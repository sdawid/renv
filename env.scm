#!/usr/bin/env guile
!#

;;; Dev style: impure functions are marked with trailing '.'

(use-modules
  (ice-9 format)
  (ice-9 ftw) ; scandir
  (ice-9 match)
  (ice-9 textual-ports) ; get-string-all
  (srfi srfi-1) ; lists
  (srfi srfi-9)) ; records


;;; Utilities -----------------------------------------------------------------

;; Any -> ?String
(define (string-non-empty? str)
  (and (string? str)
       (not (string-null? (string-trim str)))))


;; String -> String -> String
;; Remove given prefix from the string (if exists).
(define (string-remove-prefix prefix str)
  (if (string-prefix? prefix str)
    (substring str (string-length prefix))
    str))


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
(define (remove-file-extension name)
  (car (string-split name #\.)))


;; String -> String
(define (remove-trailing-slash path)
  (if (string-suffix? "/" path)
    (substring path 0 (- (string-length path) 1))
    path))


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


;; Path -> (Port -> T) -> T
(define (read-file. reader path)
  (call-with-input-file path reader))


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
        string-non-empty?
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
    '(("ENV_FILE_NAMES" . ".cmd.env:.env")
      ("ENV_CMD_PREFIXES" . ".cmd-:cmd-")
      ("ENV_DIR_NAMES" . ".cmds:.envs"))))


;;; Env File Parser -----------------------------------------------------------

;; CharSet -> Port -> ?String
(define (read-one char-set port)
  (let ((c (lookahead-char port)))
    (and (not (eof-object? c))
         (char-set-contains? char-set c)
         (string (get-char port)))))


;; CharSet -> Port -> ?String
(define (read-many char-set port)
  (let loop ((cs '()))
    (let ((c (lookahead-char port)))
      (if (and (not (eof-object? c))
               (char-set-contains? char-set c))
        (loop (cons (get-char port) cs))
        (apply string (reverse cs))))))


(define char-set:id
  (char-set-union
    char-set:letter+digit
    (char-set #\_)))

(define char-set:eol
  (char-set #\newline #\linefeed #\page))

(define char-set:not-eol
  (char-set-complement char-set:eol))

(define char-set:whitespace-not-eol
  (char-set-difference
    char-set:whitespace
    char-set:eol))

(define char-set:not-whitespace
  (char-set-complement
    char-set:whitespace))


;; Port -> ?String
(define (read-envar-name port)
  (read-many char-set:whitespace port)
  (read-many char-set:id port))


;; Port -> ?String
(define (read-assign-symbol port)
  (read-many char-set:whitespace-not-eol port)
  (read-one (char-set #\=) port))


(define (read-quoted-string port)
  (and (read-one (char-set #\') port)
       (let loop ((cs '()))
         (let ((c (get-char port)))
           (if (or (eof-object? c) (char=? #\' c))
             (apply string (reverse cs))
             (loop (cons c cs)))))))


(define (read-double-quoted-string port)
  (and (read-one (char-set #\") port)
       (let loop ((cs '()))
         (let ((c (get-char port)))
           (cond
             ((or (eof-object? c) (char=? #\" c))
              (apply string (reverse cs)))
             ((char=? #\\ c)
              (loop (cons (get-char port) cs)))
             (else
               (loop (cons c cs))))))))


;; Port -> ?String
(define (read-envar-value port)
  (let loop ((ls '()))
    (let* ((ws (or (read-many char-set:whitespace-not-eol port) ""))
           (ls* (if (null? ls) ls (cons ws ls)))
           (c (lookahead-char port)))
      (cond
        ((or (eof-object? c)
             (char-set-contains? char-set:eol c))
         (apply string-join (list (reverse ls) "")))
        ((char=? #\# c)
         (read-many char-set:not-eol port)
         (loop ls))
        ((char=? #\' c)
         (loop (cons (read-quoted-string port) ls*)))
        ((char=? #\" c)
         (loop (cons (read-double-quoted-string port) ls*)))
        (else
         (loop (cons (read-many char-set:not-whitespace port) ls*)))))))


;; Port -> ?(Name . Value)
(define (read-envar port)
  (let* ((name (read-envar-name port))
         (assign? (read-assign-symbol port))
         (value (read-envar-value port)))
    (and assign? name value
         (cons name value))))


;; Port -> AssocListOf (Name . Value)
(define (env-file-reader port)
  (let loop ((env '()))
    (if (eof-object? (lookahead-char port))
      env
      (let ((envar (read-envar port)))
        (if envar
          (loop (cons envar env))
          (loop env))))))


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


;; Context -> Name -> ?ListOf Value
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


;; Context -> ListOf File -> ReadFn -> Context
;; ReadFn := Path -> (Port -> T) -> T
(define (load-context-envs ctx files read)
  (define env-file-names
    (ctx-env-values ctx "ENV_FILE_NAMES"))

  ;; File -> ?(Path . Env)
  (define (file->file-env f)
    (and
      (file-regular? f)
      (file-readable? f)
      (first-map
        (lambda (n)
          (and (equal? n (file-name f))
               (cons (file-path f)
                     (read env-file-reader (file-path f)))))
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


;; CmdNameFn -> Context -> ListOf Files -> Context
;; where:
;;      CmdNameFn := FileName -> ?CmdName
(define (load-context-cmds-using name-fn ctx files)
  (ctx-append-cmds
    ctx
    (fold
      (lambda (f cmds)
        (let ((name (name-fn (file-name f))))
          (if name
            (cons (cons name (file-path f))
                  cmds)
            cmds)))
      (list)
      (filter
        (lambda (f)
          (and (file-regular? f)
               (file-executable? f)))
        files))))


;; String -> ?(String . String)
(define (parse-name=value str)
  (let* ((=idx (string-contains str "="))
         (name (and =idx (string-trim-both (substring str 0 =idx))))
         (value (and =idx (string-trim-both (substring str (1+ =idx))))))
    (and (string-non-empty? name)
         (string-non-empty? value)
         (cons name value))))


;; Context -> ListOf Files -> Context
(define (load-context-cmds-from-mappings ctx files)
  (define cmd-mappings
    (filter-map
      parse-name=value
      (or (ctx-env-values ctx "ENV_CMDS") (list))))

  (define (cmd-name-from-mappings file-name)
    (first-map
      (lambda (cn-fn)
        (and (equal? file-name (cdr cn-fn))
             (car cn-fn)))
      cmd-mappings))

  (load-context-cmds-using
    cmd-name-from-mappings
    ctx files))


;; Context -> ListOf Files -> Context
(define (load-context-cmds-from-prefixes ctx files)
  (define cmd-prefixes
    (ctx-env-values ctx "ENV_CMD_PREFIXES"))

  (define (cmd-name-from-prefixes file-name)
    (first-map
      (lambda (p)
        (and (string-prefix? p file-name)
             (remove-file-extension
               (string-remove-prefix p
                 file-name))))
      cmd-prefixes))

  (load-context-cmds-using
    cmd-name-from-prefixes
    ctx files))


;; Context -> ListOf Files -> LoadCtxFn -> Context
;; LoadCtxFn := Context -> Path -> Context
(define (load-context-from-cmd-dirs ctx files load-ctx)
  (define cmd-dirs
    (filter
      string-non-empty?
      (map remove-trailing-slash
           (ctx-env-values ctx "ENV_DIR_NAMES"))))
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
;; ReadFn := Path -> (Port -> T) -> T
;; Updates hte context from files inside given directory.
(define (load-context ctx dir-path ls read)
  (let ((files (ls dir-path)))
    (load-context-from-cmd-dirs
      (load-context-cmds-from-prefixes
        (load-context-cmds-from-mappings
          (load-context-envs ctx files read)
          files)
        files)
      files
      (lambda (ctx p)
        (load-context ctx p ls read)))))


;; () -> Context
(define (load-current-context.)
  (fold
    (lambda (p ctx)
      (load-context ctx p list-dir. read-file.))
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
  (out. "Usage: ~a <command> [<args>]" *prog-name*)
  (out. "")
  (out. "Runs a local <command> in a local environment.")
  (out. "If the <command> is '!', then runs <args> shell command.")
  (out. "")
  (let ()
    (out. "Local commands:")
    (for-each
      (lambda (cmd-path)
        (out. " - ~20a -> ~a" (car cmd-path) (cdr cmd-path)))
      (ctx-cmds ctx))
    (if (null? (ctx-cmds ctx))
      (out. " (none)")))
  (out. "")
  (let ((paths (filter-map car (ctx-envs ctx))))
    (out. "Local environments:")
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
       (run-cmd. ctx (or (getenv "SHELL") "/bin/sh") '()))
      (cmd-path
       ; execute command file
       (run-cmd. ctx cmd-path cmd-args))
      (else 
       (log. "Unknown command: ~a" cmd-name)
       (show-help. ctx)))))

(time-it. main.)

