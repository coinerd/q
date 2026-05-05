#lang typed/racket

;; runtime/context-assembly/budgeting.rkt — config struct, token estimation, budgeting helpers
;;
;; Configuration and budget calculation for context assembly.
;; v0.30.9: Migrated to Typed Racket.

(provide (struct-out context-assembly-config)
         make-context-assembly-config
         (struct-out context-result))

;; Note: 0 is valid for max-catalog-* (disables catalog)
(struct context-assembly-config
        ([recent-tokens : Positive-Integer] [max-catalog-entries : Nonnegative-Integer]
                                            [max-catalog-tokens : Nonnegative-Integer]
                                            [summary-window : Positive-Integer])
  #:transparent)

(: make-context-assembly-config
   (->* ()
        (#:recent-tokens Positive-Integer
                         #:max-catalog-entries Nonnegative-Integer
                         #:max-catalog-tokens Nonnegative-Integer
                         #:summary-window Positive-Integer)
        context-assembly-config))
(define (make-context-assembly-config #:recent-tokens [recent 30000]
                                      #:max-catalog-entries [max-entries 40]
                                      #:max-catalog-tokens [max-tokens 2000]
                                      #:summary-window [summary 4000])
  (context-assembly-config recent max-entries max-tokens summary))

;; Result struct — full diagnostics for observability
(struct context-result
        ([messages : (Listof Any)] [total-tokens : Nonnegative-Integer]
                                   [pinned-count : Nonnegative-Integer]
                                   [recent-count : Nonnegative-Integer]
                                   [excluded-count : Nonnegative-Integer]
                                   [over-budget? : Boolean]
                                   [catalog : (Listof Any)]
                                   [summary : (Option Any)])
  #:transparent)
