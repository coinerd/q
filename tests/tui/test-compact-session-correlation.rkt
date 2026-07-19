#lang racket/base

;; @speed fast  ;; @suite tui

;; W4 A-08 (F-06): TUI compact lifecycle reducer must correlate by SESSION
;; identity, not only by request-id. Foreign-session compact events must not
;; clear pending status or append terminal transcript entries.

(require rackunit
         "../../tui/state-types.rkt"
         "../../tui/state-events/registry.rkt"
         "event-simulator.rkt")

(define (st0 #:session [sid "S1"] #:req [req #f])
  (set-compact-request-id (initial-ui-state #:session-id sid) req))

(define (compact-evt ev
                     #:session [sid "S1"]
                     #:req [req #f]
                     #:payload [extra (hash)])
  (define base (if req (hash 'request-id req) (hash)))
  (make-test-event ev (hash-set base 'session-id sid) #:session-id sid))

(test-case "foreign-session compact completed is ignored even with matching request"
  (define s0 (st0 #:req "req-1"))
  (define s1 (apply-event-to-state s0 (compact-evt "session.compact.completed"
                                                    #:session "S2" #:req "req-1")))
  ;; Pending request must survive; no terminal transcript entry added.
  (check-equal? (ui-state-compact-request-id s1) "req-1")
  (check-equal? (length (ui-state-transcript s1)) 0))

(test-case "same-session terminal without request ignored when request is pending"
  (define s0 (st0 #:req "req-1"))
  (define s1 (apply-event-to-state s0 (compact-evt "session.compact.completed" #:req #f)))
  (check-equal? (ui-state-compact-request-id s1) "req-1")
  (check-equal? (length (ui-state-transcript s1)) 0))

(test-case "automatic compact accepted only for visible session"
  (define s0 (st0 #:req #f)) ; no pending request => automatic/legacy path
  (define s1 (apply-event-to-state s0 (compact-evt "session.compact.nothing-to-compact")))
  (check-equal? (length (ui-state-transcript s1)) 1)
  ;; same shape from a foreign session is ignored
  (define s2 (apply-event-to-state s0 (compact-evt "session.compact.nothing-to-compact"
                                                    #:session "S2")))
  (check-equal? (length (ui-state-transcript s2)) 0))

(test-case "same-session matching request terminal clears pending and records"
  (define s0 (st0 #:req "req-1"))
  (define s1 (apply-event-to-state s0 (compact-evt "session.compact.completed" #:req "req-1")))
  (check-false (ui-state-compact-request-id s1))
  (check-equal? (length (ui-state-transcript s1)) 1))
