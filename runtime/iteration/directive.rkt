#lang racket/base

;; runtime/iteration/directive.rkt -- step directive types
;;
;; v0.35.3 (W-02): Typed union replacing on-recurse callback pattern.
;; Each iteration step returns a directive; the main loop dispatches via match.
;;
;; directive-recurse : continue with updated state
;; directive-stop    : terminal, return loop-result
;; directive-yield   : events produced, continue (for future use)

(require racket/contract)

;; Struct constructors needed as match patterns — must NOT be in contract-out
(provide directive-recurse
         directive-stop
         directive-yield
         (contract-out [directive-recurse? (-> any/c boolean?)]
                       [directive-recurse-new-ctx (-> directive-recurse? any/c)]
                       [directive-recurse-new-counters (-> directive-recurse? any/c)]
                       [directive-recurse-ws (-> directive-recurse? (or/c any/c #f))]
                       [directive-stop? (-> any/c boolean?)]
                       [directive-stop-result (-> directive-stop? any/c)]
                       [directive-yield? (-> any/c boolean?)]
                       [directive-yield-events (-> directive-yield? list?)]
                       [directive-yield-new-ctx (-> directive-yield? list?)]
                       [directive-yield-new-counters (-> directive-yield? hash?)]
                       [directive-yield-ws (-> directive-yield? (or/c any/c #f))]
                       [step-directive? (-> any/c boolean?)]))

(struct directive-recurse (new-ctx new-counters ws) #:transparent)
(struct directive-stop (result) #:transparent)
(struct directive-yield (events new-ctx new-counters ws) #:transparent)

(define (step-directive? v)
  (or (directive-recurse? v) (directive-stop? v) (directive-yield? v)))
