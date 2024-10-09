#!/usr/bin/env guile
!#

(use-modules
  (ice-9 format)
  (ice-9 ftw) ; scandir
  (ice-9 match)
  (ice-9 textual-ports) ; get-string-all
  (srfi srfi-1) ; lists
  (srfi srfi-9)) ; records


;;; Utilities -----------------------------------------------------------------

;; String -> String -> String
;; Remove given prefix from the string (if exists).
(define (string-remove-prefix prefix str)
  (if (string-prefix? prefix str)
    (substring str (string-length prefix))
    str))


;; ListOf (ListOf T) -> ListOf T
(define (list-flatten lists)
  (apply append lists))


;; (S -> ?T) -> ListOf S -> ?T
(define (first-map fn ls)
  (fold
    (lambda (s t)
      (or t (fn s)))
    #f
    ls))


;; (try-or fail-value ...exprs)
;; Try to execute expressions.
;; Return value of the last expression or fail-value in case of exception.
(define-syntax try-or
  (syntax-rules ()
    ((_ fail-value body-expr ...)
     (with-exception-handler
       (lambda (exn) fail-value)
       (lambda () body-expr ...)
       #:unwind? #t #:unwind-for-type #t))))


;;; Path and File -------------------------------------------------------------

;; Path -> Name -> Path
;; Creates a subpath by appending given name to the path.
;; If name is mepty, do nothing.
(define (path-append basepath name)
  (if (string-null? name)
    basepath
    (string-append
      basepath
      file-name-separator-string
      name)))


;; Path -> ListOf Path
(define (path-parents p)
  (if (equal? p "/")
    (list "/")
    (cons p (path-parents (dirname p)))))


;; String -> String
(define (without-file-extension name)
  (car (string-split name #\.)))


;; File := (file Name Path StatMode)
(define-record-type <file>
  (make-file name path stat-mode)
  file?
  (name file-name)
  (path file-path)
  (stat-mode file-stat-mode))

(define (file-directory? file)
  (logtest #o040000 (file-stat-mode file)))

(define (file-regular? file)
  (logtest #o100000 (file-stat-mode file)))

(define (file-executable? file)
  (logtest #o000111 (file-stat-mode file)))

(define (file-readable? file)
  (logtest #o000444 (file-stat-mode file)))


;; Path -> ?File
(define (path->file. path)
  (try-or #f
    (let ((st (stat path)))
      (make-file
        (basename path)
        path
        (stat:mode st)))))


;; Path -> ListOf File
;; List directory content.
(define (list-dir. path)
  (filter-map
    (lambda (name)
      (and (not (member name '("." "..")))
           (path->file. (path-append path name))))
    (scandir path)))


;; Path -> ListOf String
(define (cat-file. path)
  (call-with-input-file path
    (lambda (port)
      (string-split
        (get-string-all port)
        (char-set #\linefeed #\return)))))



;;; Env -----------------------------------------------------------------------

;; Env -> String -> ?String
(define (env-value env name)
  (cdr (or (assoc name env) '("" . #f))))


;; Env -> String -> ?ListOf String
(define (env-values env name)
  (let ((value (env-value env name)))
    (and
      value
      (filter
        (lambda (s) (not (string-null? s)))
        (string-split value #\:)))))


;; String -> ?(Name . Value)
(define (envar-get. name)
  (let ((value (getenv name)))
    (and value (cons name value))))


;; () -> Env
(define (env-get-default.)
  (map
    (lambda (envar)
      (or (envar-get. (car envar))
          envar))
    '(("ENV_ENV_FILE" . ".cmd.env:.env")
      ("ENV_CMD_PREFIX" . ".cmd-:cmd-")
      ("ENV_CMD_DIR" . ".cmds"))))


;; String -> ?(Name . Value)
;; Parse line from env file.
(define (parse-envar line)
  (let* ((line (string-trim-both line))
         (at (string-index line #\=)))
    (cond
      ((string-null? line) #f)
      ((string-prefix? "#" line) #f)
      (at (cons (string-trim-right (substring line 0 at))
                (string-trim (substring line (+ 1 at)))))
      (else #f))))


;; ListOf String -> Env
;; Reads env from the env file.
;; Note: The order of envars in env (association list) should be reversed.
(define (parse-env-file lines)
  (reverse (filter-map parse-envar lines)))


;;; Context -------------------------------------------------------------------

;; Context := (ctx (AssocListOf (Path . Env)) (AssocListOf (Name . Path)))
(define-record-type <ctx>
  (make-ctx envs cmds)
  ctx?
  (envs ctx-envs)
  (cmds ctx-cmds))


;; Context -> AssocListOf (Path . Env) -> Context
(define (ctx-append-envs ctx new-envs)
  (if (eq? new-envs (ctx-envs ctx))
    ctx
    (make-ctx
      (append new-envs (ctx-envs ctx))
      (ctx-cmds ctx))))


;; Context -> AssocListOf (Name . Path) -> Context
(define (ctx-append-cmds ctx new-cmds)
  (if (eq? new-cmds (ctx-cmds ctx))
    ctx
    (make-ctx
      (ctx-envs ctx)
      (append new-cmds (ctx-cmds ctx)))))


;; Context -> Name -> Value
(define (ctx-env-values ctx name)
  (first-map
    (lambda (env)
      (env-values env name))
    (map cdr (ctx-envs ctx))))



;; Context -> ((Name . Value) -> ()) -> ()
(define (ctx-for-each-envar ctx fn)
  ;; ListOf (Name . Value) -> ()
  (define (env-apply env)
    (if (not (null? env))
      (begin
        (env-apply (cdr env))
        (fn (car env)))))
  ;; ListOf (Path . ListOf (Name . Value)) -> ()
  (define (envs-apply file-envs)
    (if (not (null? file-envs))
      (begin
        (envs-apply (cdr file-envs))
        (env-apply (cdar file-envs)))))
  (envs-apply (ctx-envs ctx)))


;; Context -> ()
(define (ctx-apply-env. ctx)
  (ctx-for-each-envar
    ctx
    (lambda (envar)
      (setenv (car envar) (cdr envar)))))


;; Context -> Name -> ?Path
(define (ctx-find-cmd ctx name)
  (let ((cmd-path (assoc name (ctx-cmds ctx))))
    (and cmd-path (cdr cmd-path))))


;; () -> Context
(define (make-default-ctx.)
  (make-ctx (list (cons #f (env-get-default.)))
            (list)))


;; Context -> ListOf File -> CatFn -> Context
;; CatFn := Path -> ListOf String
(define (load-context-envs ctx files cat)
  (define env-file-names
    (ctx-env-values ctx "ENV_ENV_FILE"))

  ;; File -> ?(Path . Env)
  (define (file->file-env f)
    (and
      (file-regular? f)
      (file-readable? f)
      (first-map
        (lambda (n)
          (and (equal? n (file-name f))
               (cons (file-path f)
                     (parse-env-file (cat (file-path f))))))
        env-file-names)))

  (define new-envs
    (fold
      (lambda (f envs)
          (let ((file-env (file->file-env f)))
            (if file-env
              (cons file-env envs)
              envs)))
      (list)
      files))

  (ctx-append-envs ctx new-envs))


;; Context -> ListOf Files -> Context
(define (load-context-cmds ctx files)
  (define cmd-prefixes
    (ctx-env-values ctx "ENV_CMD_PREFIX"))

  ;; File -> (Name . Path)
  (define (file->cmd-file f)
    (and (file-regular? f)
         (file-executable? f)
         (first-map
         (lambda (p)
           (and (string-prefix? p (file-name f))
                (cons (without-file-extension (string-remove-prefix p (file-name f)))
                      (file-path f))))
         cmd-prefixes)))

  (define new-cmds
    (fold
      (lambda (f cmds)
        (let ((cmd-file (file->cmd-file f)))
          (if cmd-file
            (cons cmd-file cmds)
            cmds)))
      (list)
      files))

  (ctx-append-cmds ctx new-cmds))


;; Context -> ListOf Files -> LoadCtxFn -> Context
;; LoadCtxFn := Context -> Path -> Context
(define (load-context-from-cmd-dirs ctx files load-ctx)
  (define cmd-dirs
    (ctx-env-values ctx "ENV_CMD_DIR"))
  ;; File -> Boolean
  (define (cmd-dir? f)
    (and (file-directory? f)
         (member (file-name f) cmd-dirs)))

  (fold
    (lambda (f ctx)
      (if (cmd-dir? f)
        (load-ctx ctx (file-path f))
        ctx))
    ctx
    files))


;; Path -> Context  -> LsFn -> CatFn -> Context
;; LsFn := Path -> ListOf File
;; CatFn := Path -> ListOf String
;; Updates hte context from files inside given directory.
(define (load-context ctx dir-path ls cat)
  (let ((files (ls dir-path)))
    (load-context-from-cmd-dirs
      (load-context-cmds
        (load-context-envs ctx files cat)
        files)
      files
      (lambda (ctx p)
        (load-context ctx p ls cat)))))


;; () -> Context
(define (load-current-context.)
  (fold
    (lambda (p ctx)
      (load-context ctx p list-dir. cat-file.))
    (make-default-ctx.)
    (path-parents (getcwd))))


;;; CLI -----------------------------------------------------------------------

(define (out. . args)
  (apply format #t args)
  (newline))

(define *prog-name* (basename (first (command-line))))

(define (log. msg . args)
  (format #t "(~a) ~?~%" *prog-name* msg args))


(define (time-it. thunk)
  (let ((start-time (get-internal-real-time)))
    (thunk)
    (let* ((end-time (get-internal-real-time))
           (duration (- end-time start-time))
           (duration-ms (* (/ duration internal-time-units-per-second) 1000)))
    (log. "Time: ~,3f ms" duration-ms))))


;; Context -> ()
(define (show-help. ctx)
  (out. "Usage: ~a <command> <args>" *prog-name*)
  (out. "")
  (let ()
    (out. "Available commands:")
    (for-each
      (lambda (cmd-path)
        (out. " - ~20a -> ~a" (car cmd-path) (cdr cmd-path)))
      (ctx-cmds ctx))
    (if (null? (ctx-cmds ctx))
      (out. " (none)")))
  (out. "")
  (let ((paths (filter-map car (ctx-envs ctx))))
    (out. "Environments:")
    (for-each
      (lambda (path)
        (out. " - ~a" path))
      paths)
    (if (null? paths)
      (out. " (none)")))
  (out. ""))


;; String -> String
(define (quote-arg str)
  (format #f "'~a'" str))


;; Context -> String -> ListOf String -> ()
(define (run-cmd. ctx path args)
  (log. "Running: ~a" path)
  (ctx-apply-env. ctx)
  (system (string-join
            (cons path (map quote-arg args))
            " ")))


(define (main.)
  (let* ((ctx (load-current-context.))
         (args (cdr (command-line)))
         (cmd-name (and (not (null? args)) (car args)))
         (cmd-args (and (not (null? args)) (cdr args)))
         (cmd-path (ctx-find-cmd ctx cmd-name)))
    (cond
      ((not cmd-name)
       ; no args was given - show help
       (show-help. ctx))
      ((and (equal? "!" cmd-name) (not (null? cmd-args)))
       ; execute the command after '!' character (in a modified environment)
       (run-cmd. ctx (car cmd-args) (cdr cmd-args)))
      ((equal? "!" cmd-name)
       ; start shell (in a modified environment)
       (run-cmd. ctx "$SHELL" '()))
      (cmd-path
       ; execute command file
       (run-cmd. ctx cmd-path cmd-args))
      (else 
       (log. "Unknown command: ~a" cmd-name)
       (show-help. ctx)))))

(time-it. main.)

