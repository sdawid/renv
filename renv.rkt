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
(require rash)


;; Name of the script
;; String
(define the-script-name
  (let-values (((_base name _dir?)
                (split-path (find-system-path 'run-file))))
    (path->string name)))


;; -> Void
(define (main)
  (define args (vector->list (current-command-line-arguments)))
  (if (null? args)
      (show-help)
      (run-command args))
  (void))



; === show help ===
; ! -> Void
(define (show-help)
  (define ctx (load-context (current-directory)))
  (define lines
    (append
     (describe-usage the-script-name)
     (list-command-files (context-cmds ctx))
     (list-environment-files (context-envs ctx))))
  (printf (string-join lines "~%" #:after-last "~%")))


; : String -> ListOf String
(define (describe-usage script-name)
  (list
   (format "Usage: ~a <command> <args>" script-name)))


; : AssocListOf String Path -> ListOf String
(define (list-command-files cmds)
  (define (format-command-line cmd)
    (format " - ~a -> ~a"
            (~a (car cmd) #:min-width 20)
            (path->string (cdr cmd))))
  (append
   (list "" "Available commands:")
   (map format-command-line cmds)
   (if (null? cmds)
       (list " (none)")
       (list))))


; : ListOf Path -> ListOf String
(define (list-environment-files envs)
  (define (format-environment-line env)
    (format " - ~a" (path->string env)))
  (append
   (list "" "Environments:")
   (map format-environment-line envs)
   (if (null? envs)
       (list " (none)")
       (list))))


; === run command ===
; ! (Listof String) -> Void
(define (run-command args)
  (define command-name (first args))
  (define command-args (rest args))
  (define ctx (load-context (current-directory))) 
  (load-env-files ctx)
  (match (assoc command-name (context-cmds ctx))
    ((cons _ path)
     (define command-line
       (string-join
        (cons (path->string path)
              (map (λ (a) (string-append "'" a "'")) command-args))))
     (eprintf "(~a) running: ~a~%-~%" the-script-name command-line)
     (flush-output (current-error-port))
     (system command-line))
    (else (eprintf "Unknown command: ~a~%" command-name))))



; === context ===
; Context with references to available commands and environment variables
; Context := ((envs: ListOf Path) (cmds: AssocListOf String Path))
(struct context (envs cmds) #:transparent)

; : Context
(define empty-context
  (context '()  '()))

; : Context Context -> Context
(define (context-append child parent)
  (context (append (context-envs child) (context-envs parent))
           (append (context-cmds child) (context-cmds parent))))


; ! Path -> Context
(define (load-context dir)
  (load-context-dirs (find-context-dirs dir)))

; ! Listof DirPath -> Context
(define (load-context-dirs dirs)
  (foldr (λ (dir ctx) (context-append (load-dir-context dir) ctx))
         empty-context
         dirs))

; ! DirPath -> Listof DirPath
(define (find-context-dirs dir)
  (append-map (λ (d)
                (let ((cmds-dir (build-path d ".cmds")))
                  (if (directory-exists? cmds-dir)
                      (list d cmds-dir)
                      (list d))))
              (dir->paths dir)))


; ! DirPath -> Context
(define (load-dir-context dir)
  (let ((files (directory-list dir)))
    (context (find-envs dir files)
             (find-cmds dir files))))


; : DirPath (Listof Path) -> Listof EnvFile
(define (find-envs dir files)
  (map (λ (file) (build-path dir file))
       (filter is-env-file files)))


; : Path -> Boolean : EnvFile
(define (is-env-file file)
  (let ((file (path->string file)))
    (or (equal? file ".cmd.env")
        (equal? file "cmd.env"))))


; : DirPath (Listof Path) -> Listof Command
(define (find-cmds dir files)
  (filter-map (λ (file)
                (match (regexp-match #rx"^(\\.?)cmd-(.+?)(\\.[^.]*)?$"
                                     (path->string file))
                  ((list _ _ name _)
                   (cons name (build-path dir file)))
                  (else #f)))
              files))


; directory with it's ancestors
; : Path -> Listof Path
(define (dir->paths dir)
  (foldl (λ (name paths)
           (cond
             ((null? paths)
              (list (string->path "/")))
             (else
              (cons (build-path (car paths) name)
                    paths))))
         '()
         (explode-path dir)))





; loads environment variables from `.env` files
; ! Context -> Void
(define (load-env-files ctx)
  (for-each (λ (vars) (for-each envar-load vars))
            (map parse-env-file (context-envs ctx))))


;; Envar is (struct String String)
;; (struct name value) contains name and value of an environment variable
(struct envar (name value))


;; ! Envar -> Void
;; set environment variable with envar's name to envar's value
(define (envar-load e)
  (putenv (envar-name e) (envar-value e)))


; Loads environmental variables from the given file.
;
; The file should contain lines in the following format:
;    VARIABLE_NAME=some value
;
; Empty lines and lines starting with '#' will be ignored.
;
; ! EnvFile -> Listof Envar
(define (parse-env-file path)
  (filter-map parse-envar (file->source-lines path)))


; Source line - line of text with metadata (line number, file path)
;
; SourceLine := ((text: String) (number: Integer) (file: Path))
(struct source-line (text number file) #:transparent)


; ! SourceLine -> Option Envar
(define (parse-envar line)
  (cond
    ((regexp-match #rx"^ *#" (source-line-text line))
     #f) ; skip comment lines
    ((regexp-match #rx"^ *$" (source-line-text line))
     #f) ; skip empty lines
    ((regexp-match #rx"(.*?)=(.*)" (source-line-text line))
     => (λ (result)
          (match result
            ((list _ name value)
             (envar (string-trim name) (string-trim value)))
            (else #f))))
    (else
     (eprintf "(~a) Failed to parse: ~a (line ~a in ~a)~%"
              the-script-name
              (source-line-text line)
              (source-line-number line)
              (source-line-file line))
     #f)))


; : Path -> Listof SourceLine
(define (file->source-lines path)
  (let ((lines (file->lines path)))
    (map (λ (number line) (source-line line number path))
         (range 1 (add1 (length lines)))
         lines)))




; executes given function and prints exection time
; ! (-> Void) -> Void
(define (measure-execution-time fn)
  (let ((start (current-milliseconds)))
    (with-handlers ((exn:break?
                     (λ (x) (printf "~%(~a) user break~%" the-script-name))))
      (fn))
    (eprintf "-~%(~a) time: ~a ms~%"
             the-script-name
             (- (current-milliseconds) start))))



; execute the script
(measure-execution-time main)
