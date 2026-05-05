#lang racket/base

;; runtime/context-assembly/budgeting.rkt — config struct, token estimation, budgeting helpers
;;
;; Configuration and budget calculation for context assembly.

(require racket/contract)

(provide context-assembly-config
         context-assembly-config?
         context-assembly-config-recent-tokens
         context-assembly-config-max-catalog-entries
         context-assembly-config-max-catalog-tokens
         context-assembly-config-summary-window
         make-context-assembly-config
         context-result
         context-result?
         context-result-messages
         context-result-total-tokens
         context-result-pinned-count
         context-result-recent-count
         context-result-excluded-count
         context-result-over-budget?
         context-result-catalog
         context-result-summary)

;; Note: 0 is valid for max-catalog-* (disables catalog)
(struct context-assembly-config (recent-tokens max-catalog-entries max-catalog-tokens summary-window)
  #:guard (lambda (recent max-entries max-tokens summary _name)
            (unless (and (exact-nonnegative-integer? recent) (> recent 0))
              (raise-argument-error 'context-assembly-config "positive integer" recent))
            (unless (exact-nonnegative-integer? max-entries)
              (raise-argument-error 'context-assembly-config "non-negative integer" max-entries))
            (unless (exact-nonnegative-integer? max-tokens)
              (raise-argument-error 'context-assembly-config "non-negative integer" max-tokens))
            (unless (and (exact-nonnegative-integer? summary) (> summary 0))
              (raise-argument-error 'context-assembly-config "positive integer" summary))
            (values recent max-entries max-tokens summary))
  #:transparent)

(define (make-context-assembly-config #:recent-tokens [recent 30000]
                                      #:max-catalog-entries [max-entries 40]
                                      #:max-catalog-tokens [max-tokens 2000]
                                      #:summary-window [summary 4000])
  (context-assembly-config recent max-entries max-tokens summary))

;; Result struct — full diagnostics for observability
(struct context-result
        (messages total-tokens pinned-count recent-count excluded-count over-budget? catalog summary)
  #:transparent)
