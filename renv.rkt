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
  (match/values (split-path (find-system-path 'run-file))
    [(_ name _) (path->string name)]))


;; ! -> Void
;; Main entry point.
(define (main!)
  (with-handlers ((exn:break? (λ (_) (log! "user break"))))
    (define start-ms (current-milliseconds))
    (define args (vector->list (current-command-line-arguments)))
    (if (null? args)
        (show-help!)
        (exit (begin0
               (run-command! args)
               (log! "time: ~a ms" (- (current-milliseconds) start-ms)))))))


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
   (if (null? cmds) (list " (none)") (list))))


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


;; ! (Listof String) -> ErrorCode
;; Runs a command with a given name and optional command line arguments.
(define (run-command! args)
  (define command-name (first args))
  (define command-args (rest args))
  (define ctx (load-current-context!))
  (define command-line (create-command-line ctx command-name command-args))
  (if command-line
      (begin
       (load-env-variables! (context-envs ctx))
       (log! "running: ~a" command-line)
       (flush-output (current-error-port))
       (system/exit-code command-line))
      (begin
       (log! "Unknown command: ~a~%" command-name)
       1 #| error code (failure) |#)))


;; Context -> String -> Listof String -> String | #f
;; Creates a command line for a given command and it's arguments.
(define (create-command-line ctx name args)
  (define ctx-command (assoc name (context-cmds ctx)))
  (cond
    [ctx-command
     (define command-str (path->string (cdr ctx-command)))
     (define quoted-args (map (λ (a) (format "'~a'" a)) args))
     (string-join (cons command-str quoted-args))]
    [(and (equal? name "!") (empty? args))
     "$SHELL"]
    [(equal? name "!")
     (define command-str (car args))
     (define quoted-args (map (λ (a) (format "'~a'" a)) (cdr args)))
     (string-join (cons command-str quoted-args))]
    [else #f]))
(module+ test
  (define my-ctx (context (p-list)
                         `(("foo" . ,(p "foo1.sh"))
                           ("foo" . ,(p "foo2.sh")))))
  (check-equal? (create-command-line my-ctx "foo" '("a" "b" "c"))
                "foo1.sh 'a' 'b' 'c'")
  (check-equal? (create-command-line my-ctx "foo" '())
                "foo1.sh")
  (check-equal? (create-command-line my-ctx "bar" '())
                #f)
  (check-equal? (create-command-line my-ctx "!" '("ls" "-a"))
                "ls '-a'")
  (check-equal? (create-command-line my-ctx "!" '())
                "$SHELL"))


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
  (define files (append-map list-dir (context-dirs dir)))
  (context (filter env-file? files)
           (filter car (map (λ (p) (cons (cmd-file? p) p)) files))))
(module+ test
  (define fs
    (list (p-list "/a/b/c" "/a/b/c/.cmds" "/a/b/c/cmd-abc.sh" "/a/b/c/.cmd-xyz" "/a/b/c/.cmd.env")
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
  (define filename (path->string (path-last-element path)))
  (or (equal? filename ".cmd.env")
      (equal? filename "cmd.env")))
(module+ test
  (check-true (env-file? (p ".cmd.env")))
  (check-true (env-file? (p "cmd.env")))
  (check-true (env-file? (p "/dir/.cmd.env")))
  (check-true (env-file? (p "/dir/cmd.env"))))


;; Path -> String
;; Check if the given file is a command file.
(define (cmd-file? path)
  (define filename (path->string (path-last-element path)))
  (match (regexp-match #rx"^(\\.?)cmd-(.+?)(\\.[^.]*)?$" filename)
    [(list _ _ name _) name]
    [else #f]))
(module+ test
  (check-equal? (cmd-file? (p "/p")) #f)
  (check-equal? (cmd-file? (p "/cmd-p.sh")) "p")
  (check-equal? (cmd-file? (p "/.cmd-p")) "p"))


;; Path -> Listof Path
;; Creates a list of context directories.
(define (context-dirs path)
  (define (with-cmds-subdir dir)
    (list dir (build-path dir ".cmds")))
  (append-map with-cmds-subdir (prefix-paths path)))
(module+ test
  (check-equal? (context-dirs (p "/a/b"))
                (p-list "/a/b" "/a/b/.cmds" "/a" "/a/.cmds" "/" "/.cmds")))


;; Path -> Listof Path
;; Creates a list with the given path and all its ancestors.
(define (prefix-paths path)
  (define (append-subpath path-element paths)
    (cond
      [(null? paths) (list path-element)]
      [else (cons (build-path (car paths) path-element) paths)]))
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


; loads environment variables from `.env` files
; ! Listof Path -> Void
(define (load-env-variables! paths)
  (for-each
   (λ (x) (if (string? x) (log! x) (envar-load! x)))
   (read-envars file->lines paths)))


;; (Path -> Listof String) -> Listof Path -> Listof (Envar | String)
;; Parse environment variables from the given list of files.
(define (read-envars file->lines paths)
  (append-map
   (λ (p) (parse-envar-file file->lines p))
   paths))
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


;; (Path -> Listof String) -> Path -> Listof (Envar | String)
;; Parse environment variables from the file.
(define (parse-envar-file file->lines path)
  (define lines (file->lines path))
  (define numbers (range 1 (add1 (length lines))))
  (filter-map
   (λ (line number) (parse-envar line path number))
   lines
   numbers))


;; String -> Path -> Number -> Envar | String | #f
;; Parses envar from a file line.
;; Possible outputs:
;; - Envar  - environmental variable name and value
;; - String - error message describing parsing failure
;; - #f     - the line doesn't contain env var (is empty or a comment)
(define (parse-envar line path number)
  (cond
    [(regexp-match #rx"^ *#" line) #f] ; skip comment lines
    [(regexp-match #rx"^ *$" line) #f] ; skip empty lines
    [(regexp-match #rx"(.*?)=(.*)" line)
     => (λ (result)
          (match result
            [(list _ name value) (envar (string-trim name) (string-trim value))]
            [else #f]))]
    [else
     (format "Failed to parse envar: '~a' (~a:~a)" line path number)]))
(module+ test
  (check-equal? (parse-envar "FOO=some value" (p "file") 0)
                (envar "FOO" "some value"))
  (check-equal? (parse-envar " FOO_BAR = another value " (p "file") 0)
                (envar "FOO_BAR" "another value"))
  (check-equal? (parse-envar "# a comment" (p "file") 0)
                #f)
  (check-equal? (parse-envar "foo bar" (p "file") 0)
                "Failed to parse envar: 'foo bar' (file:0)"))


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
(main!)
