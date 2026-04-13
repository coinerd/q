#lang racket/base

;; util/path-helpers.rkt — shared path manipulation helpers
;;
;; Extracted from tools/builtins/write.rkt and extensions/loader.rkt
;; to eliminate duplication (QUAL-02).

(provide ;; Path utility
 path-only)

;; Extract directory portion of a path string.
;; Returns #f when the path has no directory component (i.e. is relative/simple).
(define (path-only p)
  (define-values (dir _base _must-be-dir?) (split-path p))
  (if (eq? dir 'relative) #f dir))
