#lang racket/base

;; tools/builtins/builtin-helpers.rkt — shared helpers for builtin tools
;;
;; v0.32.1 Wave 2: Extract repeated safe-mode path validation pattern
;; from edit.rkt, write.rkt, and read.rkt into a single helper.

(require racket/contract
         (only-in "../../util/safe-mode-predicates.rkt"
                  safe-mode?
                  allowed-path?
                  safe-mode-project-root))

(provide (contract-out [require-safe-path! (-> (or/c path? string?) string? (or/c #f string?))]))

;; require-safe-path! : string? string? -> (or/c #f string?)
;; Validates that a path is allowed under safe-mode constraints.
;; Returns #f if path is allowed, or an error message string if blocked.
;; Encapsulates the repeated pattern:
;;   (and (safe-mode?) (not (allowed-path? path))) → error message
(define (require-safe-path! path-str tool-name)
  (cond
    [(and (safe-mode?) (not (allowed-path? path-str)))
     (format "~a: path '~a' outside project root (~a)" tool-name path-str (safe-mode-project-root))]
    [else #f]))
