#!/bin/sh
#|
exec racket -t $0 "$@"
|#
#lang racket/base

(require racket/system)
(require racket/string)
(require racket/match)
(require racket/file)
(require racket/format)
(require racket/list)

(module+ test
  (require rackunit)
  (define p string->path)
  (define (p-list . strings) (map string->path strings)))


;; String
;; Name of this script.
(define the-script-name
  (let-values (((_base name _dir?)
                (split-path (find-system-path 'run-file))))
    (path->string name)))


;; ! -> Void
;; Main entry point.
(define (main!)
  (define args (vector->list (current-command-line-arguments)))
  (if (null? args)
      (show-help!)
      (run-command! args)))


;; ! -> Void
;; Displays helps for this command.
(define (show-help!)
  (define ctx (load-current-context!))
  (define lines
    (append
     (list (format "Usage: ~a <command> <args>" the-script-name))
     (describe-command-files (context-cmds ctx))
     (describe-environment-files (context-envs ctx))))
  (printf (string-join lines "~n" #:after-last "~n")))


;; AssocListOf String Path -> ListOf String
;; Describes available commands.
(define (describe-command-files cmds)
  (define (format-cmd cmd)
    (format " - ~a -> ~a"
            (~a (car cmd) #:min-width 20)
            (path->string (cdr cmd))))
  (append
   (list "" "Available commands:")
   (map format-cmd cmds)
   (if (null? cmds)
       (list " (none)")
       (list))))


;; ListOf Path -> ListOf String
;; Describes available environment files.
(define (describe-environment-files envs)
  (define (format-env env)
    (format " - ~a" (path->string env)))
  (append
   (list "" "Environments:")
   (map format-env envs)
   (if (null? envs)
       (list " (none)")
       (list))))


;; ! (Listof String) -> Void
;; Runs a command with a given name and optional command line arguments.
(define (run-command! args)
  (let ((command-name (first args))
        (command-args (rest args))
        (ctx (load-current-context!)))
     (match (assoc command-name (context-cmds ctx))
       ((cons _ path)
        (let ((command-line (path->command-line path command-args)))
          (load-env-variables! (context-envs ctx))
          (log! "running: ~a" command-line)
          (flush-output (current-error-port))
          (system command-line)))
       (else
        (log! "Unknown command: ~a~%" command-name)))))


;; Path -> Listof String -> String
;; Creates command line to run the file with given arguments.
(define (path->command-line path args)
  (string-join
   (cons (path->string path)
         (map (λ (a) (string-append "'" a "'")) args))))


;; Context := ((envs: Listof EnvFile) (cmds: Listof CmdFile))
;; where:
;;    EnvFile := Path
;;    CmdFile := Pairof String Path
;; Context with references to available env-vars and command files.
(struct context (envs cmds) #:transparent)


;; Context
(define empty-context
  (context '()  '()))


;; Context -> Context -> Context
(define (context-append child parent)
  (context (append (context-envs child) (context-envs parent))
           (append (context-cmds child) (context-cmds parent))))


;; ! -> Context
;; Loads a context for the current directory.
(define (load-current-context!)
  (define (list-dir dir)
    (if (directory-exists? dir)
        (map (λ (name) (build-path dir name)) (directory-list dir))
        '()))
  (load-context list-dir (current-directory)))


;; (Path -> Listof Path) -> Path -> Context
;; Loads a context for the given directory.
(define (load-context list-dir dir)
  (let ((files (append-map list-dir (context-dirs dir))))
    (context (filter env-file? files)
             (filter car (map (λ (p) (cons (cmd-file? p) p)) files)))))
(module+ test
  (define fs (list (p-list "/a/b/c" "/a/b/c/.cmds" "/a/b/c/cmd-abc.sh" "/a/b/c/.cmd-xyz" "/a/b/c/.cmd.env")
                   (p-list "/a/b/c/.cmds" "/a/b/c/.cmds/cmd-foo" "/a/b/c/.cmds/cmd.env")
                   (p-list "/a/b" "/a/b/cmd-foo")
                   (p-list "/a")
                   (p-list "/")))
  (define (list-dir path)
    (let ((found (assoc path fs)))
      (if found (cdr found) '())))
  (check-equal? (load-context list-dir (p "/a/b/c"))
                (context (p-list "/a/b/c/.cmd.env" "/a/b/c/.cmds/cmd.env")
                         `(("abc" . ,(p "/a/b/c/cmd-abc.sh"))
                           ("xyz" . ,(p "/a/b/c/.cmd-xyz"))
                           ("foo" . ,(p "/a/b/c/.cmds/cmd-foo"))
                           ("foo" . ,(p "/a/b/cmd-foo"))))))


;; Path -> Boolean
;; Check if the given file is an environment file.
(define (env-file? path)
  (let ((filename (path->string (path-last-element path))))
    (or (equal? filename ".cmd.env")
        (equal? filename "cmd.env"))))
(module+ test
  (check-true (env-file? (p ".cmd.env")))
  (check-true (env-file? (p "cmd.env")))
  (check-true (env-file? (p "/dir/.cmd.env")))
  (check-true (env-file? (p "/dir/cmd.env"))))


;; Path -> String
;; Check if the given file is a command file.
(define (cmd-file? path)
  (match (regexp-match #rx"^(\\.?)cmd-(.+?)(\\.[^.]*)?$"
                       (path->string (path-last-element path)))
    ((list _ _ name _) name)
    (else #f)))
(module+ test
  (check-equal? (cmd-file? (p "/p")) #f)
  (check-equal? (cmd-file? (p "/cmd-p.sh")) "p")
  (check-equal? (cmd-file? (p "/.cmd-p")) "p"))


;; Path -> Listof Path
;; Creates a list of context directories.
(define (context-dirs path)
  (append-map (λ (p) (list p (build-path p ".cmds")))
              (prefix-paths path)))
(module+ test
  (check-equal? (context-dirs (p "/a/b"))
                (p-list "/a/b" "/a/b/.cmds" "/a" "/a/.cmds" "/" "/.cmds")))


;; Path -> Listof Path
;; Creates a list with the given path and all its ancestors.
(define (prefix-paths path)
  (define (append-subpath path-element paths)
    (cond
      ((null? paths) (list path-element))
      (else (cons (build-path (car paths) path-element) paths))))
  (foldl append-subpath '() (explode-path path)))
(module+ test
  (check-equal? (prefix-paths (p "/"))
                (p-list "/"))
  (check-equal? (prefix-paths (p "/abc/foo/bar"))
                (p-list "/abc/foo/bar"
                        "/abc/foo"
                        "/abc"
                        "/"))
  (check-equal? (prefix-paths (p "a/b"))
                (p-list "a/b"
                        "a")))



;; Envar is (struct String String)
;; (struct name value) contains name and value of an environment variable
(struct envar (name value) #:transparent)


;; ! Envar -> Void
;; set environment variable with envar's name to envar's value
(define (envar-load! e)
  (putenv (envar-name e) (envar-value e)))


;; FileLine is (struct String Number Path)
;; (struct text number file)
;;     contains a text line together with a line number and a source file
(struct fileline (text number file) #:transparent)


; loads environment variables from `.env` files
; ! Listof Path -> Void
(define (load-env-variables! paths)
  (for-each (λ (x) (if (string? x) (log! x) (envar-load! x)))
            (read-envars file->lines paths)))


;; (Path -> Listof String) -> Listof Path -> Listof (Envar | String)
;; Parse environment variables from the given list of files.
(define (read-envars file->lines paths)
  (define (file->filelines path)
    (let* ((lines (file->lines path))
           (numbers (range 1 (add1 (length lines)))))
    (map (λ (n l) (fileline l n path)) numbers lines)))
  (filter-map parse-envar
              (append-map file->filelines paths)))
(module+ test
  (define files '(("foo" "a=a1" "" "b=b1")
                  ("bar" "a=a2" "abc")))
  (define (file->lines path)
    (let ((found (assoc (path->string path) files)))
      (if found (cdr found) found)))
  (check-equal? (read-envars file->lines (p-list "foo" "bar"))
                (list (envar "a" "a1")
                      (envar "b" "b1")
                      (envar "a" "a2")
                      "Failed to parse envar: 'abc' (bar:2)")))


;; FileLine -> Envar | String | #f
;; Parses envar from a file line.
;; Possible outputs:
;; - Envar  - environmental variable name and value
;; - String - error message describing parsing failure
;; - #f     - the line doesn't contain env var (is empty or a comment)
(define (parse-envar line)
  (cond
    ((regexp-match #rx"^ *#" (fileline-text line))
     #f) ; skip comment lines
    ((regexp-match #rx"^ *$" (fileline-text line))
     #f) ; skip empty lines
    ((regexp-match #rx"(.*?)=(.*)" (fileline-text line))
     => (λ (result)
          (match result
            ((list _ name value)
             (envar (string-trim name) (string-trim value)))
            (else #f))))
    (else
     (format "Failed to parse envar: '~a' (~a:~a)"
              (fileline-text line)
              (fileline-file line)
              (fileline-number line)))))
(module+ test
  (check-equal? (parse-envar (fileline "FOO=some value" 0 "file"))
                (envar "FOO" "some value"))
  (check-equal? (parse-envar (fileline " FOO_BAR = another value " 0 "file"))
                (envar "FOO_BAR" "another value"))
  (check-equal? (parse-envar (fileline "# a comment" 0 "file"))
                #f)
  (check-equal? "Failed to parse envar: 'foo bar' (file:0)"
                (parse-envar (fileline "foo bar" 0 "file"))))


;; ! (-> Void) -> Void
;; Executes a function and prints the exection time.
(define (measure-execution-time! fn)
  (let ((start-ms (current-milliseconds)))
    (with-handlers ((exn:break? (λ (_) (log! "user break"))))
      (fn))
    (log! "time: ~a ms" (- (current-milliseconds) start-ms))))


;; ! String -> Void
;; Logs a message to the stderr.
(define (log! message . args)
  (eprintf "(~a) ~a~n" the-script-name (format message args)))


;; Path -> Path
;; Get the last path-element or empty string if the path is the root path.
(define (path-last-element path)
  (match/values (split-path path)
    ((_ last _) last)))
(module+ test
  (check-equal? (path-last-element (p "/a/b")) (p "b"))
  (check-equal? (path-last-element (p "a/b")) (p "b"))
  (check-equal? (path-last-element (p "/")) (p "/")))                             
  

;; Execute the script.
(measure-execution-time! main!)
