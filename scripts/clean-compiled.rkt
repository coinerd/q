#!/usr/bin racket
#lang racket/base

;; clean-compiled.rkt — Remove stale Racket compiled bytecode (.zo) files.
;;
;; Usage: racket scripts/clean-compiled.rkt [directory]
;;   directory defaults to current directory.
;;
;; When source (.rkt) files change after `raco make`, cached .zo files
;; can reference internal variable names that no longer exist, causing
;; linklet mismatch errors at startup. This script removes all compiled/
;; directories to force recompilation from source.

(require racket/system
         racket/port
         racket/string
         racket/file)

(define args (current-command-line-arguments))

(define root
  (if (> (vector-length args) 0)
      (vector-ref args 0)
      "."))

(define (clean-compiled! root-dir)
  (define out (with-output-to-string
                (lambda ()
                  (system* "/usr/bin/find"
                           root-dir
                           "-name" "compiled"
                           "-type" "d"))))
  (define dirs
    (for/list ([line (in-list (string-split out "\n"))]
               #:when (non-empty-string? line))
      (string-trim line)))
  (for ([dir (in-list dirs)])
    (printf "  removing ~a\n" dir)
    (delete-directory/files dir))
  (if (null? dirs)
      (printf "No compiled/ directories found.\n")
      (printf "Cleaned ~a compiled/ directories.\n" (length dirs))))

(clean-compiled! root)
