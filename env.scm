#!/usr/bin/env -S guile -e main
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
  (ice-9 textual-ports)
  (srfi srfi-1) ; lists
  (srfi srfi-9)) ; records


;;; Utilities -----------------------------------------------------------------

(define-syntax-rule (λ e ...)
  (lambda e ...))

(define-syntax when
  (syntax-rules ()
    ((_ p) p)
    ((_ p e ...) (if p (begin e ...) #f))))

;; ListOf T -> Boolean
(define some? (negate null?))

;; Any -> Boolean
(define (string-non-empty? str)
  (and (string? str)
       (not (string-null? (string-trim str)))))


;; Any -> Boolean
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

;; String -> ListOf String
(define (split-env-value value)
  (if value
      (filter string-non-empty? (string-split value #\:))
      '()))

;; String -> ?String
(define (env-get. name)
  (getenv name))

;; String -> String -> ()
(define (env-set. name value)
  (setenv name value))


;;; Env File Parser -----------------------------------------------------------

(define char-set:id
  (char-set-union char-set:letter+digit (char-set #\_)))

(define char-set:eol
  (char-set #\newline #\linefeed #\page))

(define char-set:whitespace-not-eol
  (char-set-difference char-set:whitespace char-set:eol))


;; CharSet -> Port -> ?String
(define (read-one char-set port)
  (let ((c (lookahead-char port)))
    (when (and (not (eof-object? c))
               (char-set-contains? char-set c))
      (string (get-char port)))))


;; CharSet -> Port -> ?String
(define (read-while char-set port)
  (let loop ((cs '()))
    (let ((c (lookahead-char port)))
      (if (and (not (eof-object? c))
               (char-set-contains? char-set c))
          (loop (cons (get-char port) cs))
          (when (not (null? cs))
            (apply string (reverse cs)))))))


;; CharSet -> Port -> ?String
(define (read-until char-set port)
  (read-while (char-set-complement char-set) port))


;; Port -> ?String
(define (read-quoted-string port)
  (when (read-one (char-set #\') port)
        (let ((s (read-until (char-set #\') port)))
          (get-char port)
          s)))


;; Port -> ?String
(define (read-variable port)
  (when (read-one (char-set #\$) port)
        (if (read-one (char-set #\{) port)
            (let ((s (read-until (char-set #\}) port)))
              (get-char port)
              s)
            (read-while char-set:id port))))


;; Template := ListOf (StringValue | WhitespaceValue | VariableValue)
;; StringValue := ('string . String)
;; WhitespaceValue := ('whitespace . String)
;; VariableValue := ('variable . String)

;; Template
(define empty-template '())

;; String -> Template -> Template
(define (append-string value template)
  (cons (cons 'string value) template))

;; String -> Template -> Template
(define (append-whitespace whitespace template)
  (cons (cons 'whitespace whitespace) template))

;; String -> Template -> Template
(define (append-variable name template)
  (cons (cons 'variable name) template))


;; Template -> Template
(define (trim-template template)
  (define (trim-left template)
    (drop-while (λ (v) (eq? (car v) 'whitespace)) template))
  (trim-left (reverse (trim-left (reverse template)))))


;; Template -> GetFn -> String
;; GetFn := (String -> ?String)
(define (resolve-template template get)
  (define (resolve t-value)
    (let ((type (car t-value))
          (value (cdr t-value)))
      (case type
        ((variable) (or (get value) ""))
        (else value))))
  (string-join (map resolve template) ""))


;; Port -> Template
(define (read-double-quoted-string port)
  (define (quoted-string-template)
    (let ((c (lookahead-char port)))
      (cond ((eof-object? c)
             empty-template)
            ((char=? #\" c)
             (get-char port)
             empty-template)
            ((char=? #\\ c)
             (get-char port)
             (append-string (string (get-char port))
                            (quoted-string-template)))
            ((char=? #\$ c)
             (append-variable (read-variable port)
                              (quoted-string-template)))
            (else
             (append-string (read-until (char-set #\" #\\ #\$) port)
                            (quoted-string-template))))))
  (when (read-one (char-set #\") port)
        (quoted-string-template)))


;; Port -> Template
(define (read-value port)
  (define (value-template)
    (let ((c (lookahead-char port)))
      (cond ((eof-object? c)
             empty-template)
            ((char-set-contains? char-set:eol c)
             empty-template)
            ((char=? #\# c)
             (read-until char-set:eol port)
             empty-template)
            ((char=? #\' c)
             (append-string (read-quoted-string port)
                            (value-template)))
            ((char=? #\" c)
             (append (read-double-quoted-string port)
                     (value-template)))
            ((char=? #\$ c)
             (append-variable (read-variable port)
                              (value-template)))
            ((char=? #\\ c)
             (get-char port)
             (append-string (string (get-char port))
                            (value-template)))
            ((char-set-contains? char-set:whitespace-not-eol c)
             (append-whitespace (read-while char-set:whitespace-not-eol port)
                                (value-template)))
            (else
             (append-string (read-until (char-set-union
                                          char-set:whitespace
                                          (char-set #\# #\' #\" #\$ #\\))
                                        port)
                            (value-template))))))
  (trim-template (value-template)))


;; Envar := (Name . Template)
(define (make-envar name template)
  (cond ((string? template)
         (list name (cons 'string template)))
        ((list? template)
         (cons name template))
        (else error "Expected String or List of templates")))

(define (envar-name envar)
  (car envar))

(define (envar-template envar)
  (cdr envar))


;; Port -> ?String
(define (read-envar-name port)
  (read-while char-set:whitespace port)
  (read-while char-set:id port))


;; Port -> ?String
(define (read-assign-symbol port)
  (read-while char-set:whitespace-not-eol port)
  (read-one (char-set #\=) port))


;; Port -> ?Envar
(define (read-envar port)
  (let* ((name (read-envar-name port))
         (assign? (read-assign-symbol port))
         (template (read-value port)))
    (and assign? name template
         (make-envar name template))))


;; Port -> AListOf Envar
(define (env-file-reader port)
  (if (eof-object? (lookahead-char port))
      '()
      (let ((envar (read-envar port)))
        (if envar
            (cons envar (env-file-reader port))
            (env-file-reader port)))))


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


;; Path -> ReadFn -> GetFn -> SetFn -> ()
(define (load-env-file path read eget eset)
  (define (set-envar envar)
    (eset (envar-name envar)
          (resolve-template (envar-template envar) eget)))
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
          (for-each (λ (path) (load-env-file path read eget eset)) new-envs)
          (let ((ctx (loop (append env-dirs (cdr paths)))))
            (make-ctx
              (append (ctx-envs ctx) new-envs)
              (append (ctx-cmds ctx) new-cmds)))))))


(define (load-default-configuration eget eset)
  (define (set-if-not-set default)
    (let* ((name (car default))
           (new-value (cdr default))
           (old-value (eget name)))
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
  (format (current-error-port) "(~a) ~?~%" *prog-name* msg args))


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
  (format #f "\"~a\"" str))


;; Context -> String -> ListOf String -> ()
(define (run-cmd. ctx path args)
  (log. "Running: ~a" path)
  (system (string-join
            (cons path (map quote-arg args))
            " ")))


(define (main. args)
  (let* ((ctx (load-current-context.))
         (args (cdr args))
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

(define (main args)
  (time-it. (λ () (main. args))))

