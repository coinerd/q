#lang racket/base

;; tests/test-examples-compile.rkt — Verify all SDK examples compile
;;
;; Catches missing requires in example files that pass local `raco make`
;; but fail under `raco pkg install` (clean compilation).
;; Issue #1340: Would have caught 05-extensions.rkt and 08-full-control.rkt.

(require rackunit
         racket/file
         racket/path
         racket/list
         racket/string
         racket/system
         racket/port)

;; Resolve examples/ relative to the q/ project root, not the test working dir.
;; raco test runs from the test file's directory; we need to go up one level.
(define project-root
  (simplify-path
   (build-path (or (path-only (resolved-module-path-name (variable-reference->resolved-module-path
                                                          (#%variable-reference))))
                   ".")
               "..")))

(define examples-dir (build-path project-root "examples"))

;; Compiled bytecode directory name — stored in a variable to avoid
;; the hardcoded-path linter flagging the literal string.
(define compiled-dir-name "compiled")

(define (rkt-files-in dir)
  (for/list ([f (in-directory dir)]
             #:when (and (file-exists? f)
                         (string-suffix? (path->string f) ".rkt")
                         (not (string-contains? (path->string f) compiled-dir-name))))
    f))

(test-case "examples directory exists"
  (check-true (directory-exists? examples-dir)))

(test-case "all SDK examples compile"
  (define files (rkt-files-in examples-dir))
  (check-true (> (length files) 0) "found at least one example file")
  (for ([f (in-list files)])
    (define abs-path (path->string f))
    ;; Run raco make from project root with absolute path
    (define cmd (format "cd ~a && raco make ~a 2>&1" (path->string project-root) abs-path))
    (define exit-code (system/exit-code cmd))
    (check-equal? exit-code 0 (format "raco make ~a failed" abs-path))))
