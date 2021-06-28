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


; : String
(define the-script-name
  (let-values (((base name dir?)
                (split-path (find-system-path 'run-file))))
    (path->string name)))


; : -> Void
(define (main)
  (let ((args (vector->list (current-command-line-arguments)))
        (ctx (load-current-context)))
    (if (null? args)
        (show-help ctx)
        (begin
          (load-env-files ctx)
          (run-command (car args) (cdr args) ctx)
          (values)))))


; DirPath := Path
; EnvFile := Path
; Command := Pairof String Path


; context with references to available commands and environment variables
; Context := ((envs: Listof EnvFile) (cmds: Listof Command))
(struct context (envs cmds) #:transparent)

; : Context
(define empty-context
  (context '()  '()))

; : Context Context -> Context
(define (context-append child parent)
  (context (append (context-envs child) (context-envs parent))
           (append (context-cmds child) (context-cmds parent))))


; : -> Context
(define (load-current-context)
  (load-context-dir (current-directory)))

; : DirPath -> Context
(define (load-context-dir dir)
  (load-context-dirs (find-context-dirs dir)))

; : Listof DirPath -> Context
(define (load-context-dirs dirs)
  (foldr (λ (dir ctx) (context-append (load-dir-context dir) ctx))
         empty-context
         dirs))

; : DirPath -> Listof DirPath
(define (find-context-dirs dir)
  (append-map (λ (d)
                (let ((cmds-dir (build-path d ".cmds")))
                  (if (directory-exists? cmds-dir)
                      (list d cmds-dir)
                      (list d))))
              (dir->paths dir)))


; : DirPath -> Context
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



; show help
; : Context -> Void
(define (show-help ctx)
  (printf "Usage: ~a <command> <args>~%" the-script-name)
  (print-available-commands (context-cmds ctx))
  (print-available-environments (context-envs ctx)))

; : Listof Command -> Void
(define (print-available-commands cmds)
  (printf "~%Available commands:~%")
  (for-each (λ (cmd)
              (printf " - ~a -> ~a~%"
                      (~a (car cmd) #:min-width 20)
                      (path->string (cdr cmd))))
            cmds)
  (when (null? cmds)
    (printf "  (none)~%")))


; : Listof EnvFile -> Void
(define (print-available-environments envs)
  (printf "~%Environments:~%")
  (for-each (λ (env)
              (printf " - ~a~%"
                      (path->string env)))
            envs)
  (when (null? envs)
    (printf "  (none)~%")))



; loads environment variables from `.env` files
; : Context -> Void
(define (load-env-files ctx)
  (for-each (λ (vars) (for-each load-envar vars))
            (map parse-env-file (context-envs ctx))))

; EnvVar := Pairof String String
; : EnvVar -> Void
(define (load-envar envar)
  (putenv (car envar) (cdr envar)))


; Loads environmental variables from the given file.
;
; The file should contain lines in the following format:
;    VARIABLE_NAME=some value
;
; Empty lines and lines starting with '#' will be ignored.
;
; : EnvFile -> Listof EnvVar
(define (parse-env-file path)
  (filter-map parse-envar (file->source-lines path)))


; Source line - line of text with metadata (line number, file path)
;
; SourceLine := ((text: String) (number: Integer) (file: Path))
(struct source-line (text number file) #:transparent)


; : SourceLine -> Option EnvVar
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
             (cons (string-trim name) (string-trim value)))
            (else #f))))
    (else
     (printf "(~a) Failed to parse: ~a (line ~a in ~a)~%"
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


; runs command if exists
; : String (Listof String) Context -> Void
(define (run-command name args ctx)
  (match (assoc name (context-cmds ctx))
    ((cons _ path)
     (let ((command-line
            (string-join
             (cons (path->string path)
                   (map (λ (a) (string-append "'" a "'")) args)))))
       (printf "(~a) running: ~a~%-~%" the-script-name command-line)
       (system command-line)))
    (else (printf "Unknown command: ~a~%" name))))



; executes given function and prints exection time
; : (-> Void) -> Void
(define (measure-execution-time fn)
  (let ((start (current-milliseconds)))
    (with-handlers ((exn:break?
                     (λ (x) (printf "~%(~a) user break~%" the-script-name))))
      (fn))
    (printf "-~%(~a) time: ~a ms~%"
            the-script-name
            (- (current-milliseconds) start))))



; execute the script
(measure-execution-time main)
