#lang racket/base

;; runtime/iteration/directive.rkt -- step directive types
;;
;; v0.35.3 (W-02): Typed union replacing on-recurse callback pattern.
;; Each iteration step returns a directive; the main loop dispatches via match.
;;
;; directive-recurse : continue with updated state
;; directive-stop    : terminal, return loop-result
;; directive-yield   : events produced, continue (for future use)

(provide directive-recurse
         directive-recurse?
         directive-recurse-new-ctx
         directive-recurse-new-counters
         directive-recurse-ws
         directive-stop
         directive-stop?
         directive-stop-result
         directive-yield
         directive-yield?
         directive-yield-events
         directive-yield-new-ctx
         directive-yield-new-counters
         directive-yield-ws
         step-directive?)

(struct directive-recurse (new-ctx new-counters ws) #:transparent)
(struct directive-stop (result) #:transparent)
(struct directive-yield (events new-ctx new-counters ws) #:transparent)

(define (step-directive? v)
  (or (directive-recurse? v) (directive-stop? v) (directive-yield? v)))
