#!/usr/bin racket
#lang racket/base

;; launch.rkt — Startup wrapper with bytecode staleness auto-recovery.
;;
;; When source files change after `raco make`, cached .zo files can reference
;; internal variable names that no longer exist, causing linklet mismatch
;; errors at startup. This wrapper catches that specific error, cleans
;; stale bytecode, and retries.
;;
;; Usage: racket launch.rkt [args...]
;;   (equivalent to: racket main.rkt [args...])
;;
;; For CI or scripts, use `racket main.rkt` directly — it's faster.

(require racket/system
         racket/port
         racket/string
         racket/file)

(define (clean-compiled!)
  (define out (with-output-to-string
                (lambda ()
                  (system* "/usr/bin/find" "." "-name" "compiled" "-type" "d"))))
  (define dirs
    (for/list ([line (in-list (string-split out "\n"))]
               #:when (non-empty-string? line))
      (string-trim line)))
  (unless (null? dirs)
    (for ([dir (in-list dirs)])
      (fprintf (current-error-port) "  q: removing ~a\n" dir)
      (delete-directory/files dir))
    (fprintf (current-error-port) "  q: cleaned ~a compiled/ dirs\n" (length dirs)))
  dirs)

(define (linklet-mismatch? str)
  (and (string-contains? str "instantiate-linklet: mismatch")
       (string-contains? str "possible reason: modules need to be recompiled")))

(define (run-main args)
  ;; Run main.rkt and capture stderr for linklet detection
  (define err-out (open-output-string))
  (define ok?
    (parameterize ([current-error-port err-out])
      (apply system* (find-executable-path "racket") "main.rkt" args)))
  (define err-str (get-output-string err-out))
  (values ok? err-str))

(define args (vector->list (current-command-line-arguments)))

(define-values (ok? err-str) (run-main args))

(cond
  [(linklet-mismatch? err-str)
   (fprintf (current-error-port)
            "q: stale bytecode detected — auto-cleaning and retrying...\n")
   (clean-compiled!)
   (define retry-err (open-output-string))
   (define retry-ok?
     (parameterize ([current-error-port retry-err])
       (apply system* (find-executable-path "racket") "main.rkt" args)))
   (define retry-str (get-output-string retry-err))
   (unless (string=? retry-str "")
     (display retry-str (current-error-port)))
   (exit (if retry-ok? 0 1))]
  [else
   (unless (string=? err-str "")
     (display err-str (current-error-port)))
   (exit (if ok? 0 1))])
