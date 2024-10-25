#!/usr/bin/env guile
!#


(define *default-configuration*
  '(("ENV_FILE_NAMES" . ".cmd.env:.env")
    ("ENV_CMD_PREFIXES" . ".cmd-:cmd-")
    ("ENV_DIR_NAMES" . ".cmds:.envs")
    ("ENV_CMDS" . "")))


;;; Dev style: impure functions are marked with trailing '.'

(use-modules
  (ice-9 format)
  (ice-9 ftw) ; scandir
  (ice-9 match)
  (ice-9 textual-ports) ; get-string-all
  (srfi srfi-1) ; lists
  (srfi srfi-9)) ; records


;;; Utilities -----------------------------------------------------------------

(define-syntax-rule (λ e ...)
  (lambda e ...))

(define-syntax-rule (when p e ...)
  (if p
      (begin e ...)
      #f))

(define some? (negate null?))

;; Any -> Boolean
(define (string-non-empty? str)
  (and (string? str)
       (not (string-null? (string-trim str)))))

(define (string-empty? str)
  (or (not str)
      (string-null? (string-trim str))))


;; String -> String -> String
;; Remove given prefix from the string (if exists).
(define (string-remove-prefix prefix str)
  (if (string-prefix? prefix str)
      (substring str (string-length prefix))
      str))


;; (S -> ?T) -> ListOf S -> ?T
(define (first-map fn ls)
  (cond ((null? ls) #f)
        (else (or (fn (car ls))
                  (first-map fn (cdr ls))))))


;; (try-or fail-value ...exprs)
;; Try to execute expressions.
;; Return value of the last expression or fail-value in case of exception.
(define-syntax try-or
  (syntax-rules ()
    ((_ fail-value body-expr ...)
     (with-exception-handler
       (λ (exn) fail-value)
       (λ () body-expr ...)
       #:unwind? #t
       #:unwind-for-type #t))))


;;; Path and File -------------------------------------------------------------

;; Path -> Name -> Path
;; Creates a subpath by appending given name to the path.
;; If name is mepty, do nothing.
(define (path-append basepath name)
  (if (string-null? name)
      basepath
      (string-append basepath
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
      (substring path 0 (1- (string-length path)))
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
    (make-file (basename path)
               path
               (stat:mode (stat path)))))


;; Path -> ListOf File
;; List directory content.
(define (list-dir. path)
  (filter-map (λ (n) (when (not (member n '("." "..")))
                       (path->file. (path-append path n))))
              (scandir path)))


;; Path -> (Port -> T) -> T
(define (read-file. reader path)
  (call-with-input-file path reader))


;;; Env -----------------------------------------------------------------------

;; Env := AListOf Envar
;; Envar := (Name . Value)
(define (make-envar name value)
  (cons name value))

(define (envar-name envar)
  (car envar))

(define (envar-value envar)
  (cdr envar))


;; String -> ListOf String
(define (split-env-value value)
  (if value
      (filter string-non-empty? (string-split value #\:))
      '()))


;; String -> ?Value
(define (env-get. name)
  (getenv name))

;; String -> String -> ()
(define (env-set. name value)
  (setenv name value))


;;; Env File Parser -----------------------------------------------------------

;; CharSet -> Port -> ?String
(define (read-one char-set port)
  (let ((c (lookahead-char port)))
    (when (and (not (eof-object? c))
               (char-set-contains? char-set c))
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
           (cond ((or (eof-object? c) (char=? #\" c))
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
      (cond ((or (eof-object? c)
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


;; Port -> ?Envar
(define (read-envar port)
  (let* ((name (read-envar-name port))
         (assign? (read-assign-symbol port))
         (value (read-envar-value port)))
    (and assign? name value
         (make-envar name value))))


;; Port -> AListOf Envar
(define (env-file-reader port)
  (let loop ((env '()))
    (if (eof-object? (lookahead-char port))
        env
        (let ((envar (read-envar port)))
          (if envar
              (loop (cons envar env))
              (loop env))))))


;;; Context -------------------------------------------------------------------

;; Context := (ctx (ListOf Path) (AListOf (Name . Path)))
(define-record-type <ctx>
  (make-ctx envs cmds)
  ctx?
  (envs ctx-envs)
  (cmds ctx-cmds))


;; Context -> Name -> ?Path
(define (ctx-find-cmd ctx name)
  (let ((cmd-path (assoc name (ctx-cmds ctx))))
    (and cmd-path (cdr cmd-path))))


;; ListOf File -> GetFn -> ListOf Path
(define (find-env-files files eget)
  (define env-file-names
    (split-env-value (eget "ENV_FILE_NAMES")))
  (define (env-file? f)
    (and (file-regular? f)
         (file-readable? f)
         (member (file-name f) env-file-names)))
  (map file-path (filter env-file? files)))


;; Path -> ReadFn -> SetFn -> ()
(define (load-env-file path read eset)
  (define (set-envar envar)
    (eset (envar-name envar) (envar-value envar)))
  (for-each set-envar
            (read env-file-reader path)))


;; String -> ?(String . String)
(define (parse-name=value str)
  (let* ((=idx (string-contains str "="))
         (name (and =idx (string-trim-both (substring str 0 =idx))))
         (value (and =idx (string-trim-both (substring str (1+ =idx))))))
    (and (string-non-empty? name)
         (string-non-empty? value)
         (cons name value))))

(define (find-cmds files eget)
  (define cmd-mappings
    (filter-map
      parse-name=value
      (split-env-value (eget "ENV_CMDS"))))

  (define (cmd-name-from-mappings file-name)
    (first-map
      (λ (m) (and (equal? file-name (cdr m))
                  (car m)))
      cmd-mappings))

  (define cmd-prefixes
    (split-env-value (eget "ENV_CMD_PREFIXES")))

  (define (cmd-name-from-prefixes file-name)
    (first-map
      (λ (p)
        (and (string-prefix? p file-name)
             (remove-file-extension
               (string-remove-prefix p file-name))))
      cmd-prefixes))

  (define (file->cmd file)
    (let ((cname (or (cmd-name-from-mappings (file-name file))
                     (cmd-name-from-prefixes (file-name file)))))
      (when cname
        (cons cname (file-path file)))))

  (filter-map file->cmd
              (filter (λ (f) (and (file-regular? f) (file-executable? f)))
                      files)))


;; ListOf File -> GetFn -> ListOf Path
(define (find-cmd-dirs files eget)
  (define cmd-dirs
    (filter string-non-empty?
            (map remove-trailing-slash
                 (split-env-value (eget "ENV_DIR_NAMES")))))
  (define (cmd-dir? f)
    (and (file-directory? f)
         (member (file-name f) cmd-dirs)))
  (map file-path
       (filter cmd-dir? files)))



;; ListOf Path  -> LsFn -> CatFn -> GetFn -> SetFn -> Context
;; LsFn := Path -> ListOf File
;; ReadFn := Path -> (Port -> T) -> T
;; GetFn := Name -> ?Value
;; SetFn := Name -> Value -> ()
;; Updates the context from files inside given directory.
(define (load-context paths ls read eget eset)
  (let loop ((paths paths))
    (if (null? paths)
        (make-ctx '() '())
        (let* ((files (ls (car paths)))
               (new-envs (find-env-files files eget))
               (new-cmds (find-cmds files eget))
               (env-dirs (find-cmd-dirs files eget)))
          (for-each (λ (path) (load-env-file path read eset)) new-envs)
          (let ((ctx (loop (append env-dirs (cdr paths)))))
            (make-ctx
              (append (ctx-envs ctx) new-envs)
              (append (ctx-cmds ctx) new-cmds)))))))



(define (load-default-configuration eget eset)
  (define (set-if-not-set default)
    (let* ((name (envar-name default))
           (old-value (eget name))
           (new-value (envar-value default)))
      (if (and (string-empty? old-value)
               (string-non-empty? new-value))
        (eset name new-value))))
  (for-each set-if-not-set *default-configuration*))


;; () -> Context
(define (load-current-context.)
  (load-default-configuration env-get. env-set.)
  (load-context (reverse (path-parents (getcwd)))
                list-dir. read-file. env-get. env-set.))


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
    (for-each (λ (c) (out. " - ~20a -> ~a" (car c) (cdr c)))
              (ctx-cmds ctx))
    (if (null? (ctx-cmds ctx))
        (out. " (none)")))
  (out. "")
  (let ((paths (ctx-envs ctx)))
    (out. "Local environments:")
    (for-each (λ (p) (out. " - ~a" p))
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
  (system (string-join
            (cons path (map quote-arg args))
            " ")))


(define (main.)
  (let* ((ctx (load-current-context.))
         (args (cdr (command-line)))
         (cmd-name (when (some? args) (car args)))
         (cmd-args (when (some? args) (cdr args)))
         (cmd-path (ctx-find-cmd ctx cmd-name)))
    (cond ((not cmd-name)
           ; no args was given - show help
           (show-help. ctx))
          ((and (equal? "!" cmd-name) (some? cmd-args))
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

