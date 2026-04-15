#lang racket

;; tests/test-session-switch.rkt — tests for Atomic Session Switching Lifecycle (#704-#707)
;;
;; Covers:
;;   - #704: Teardown old extension state on session switch
;;   - #705: Rebind extensions to new session after switch
;;   - #706: Session start event with reason (new/resume/fork)
;;   - #707: Full atomic lifecycle

(require rackunit
         "../agent/event-bus.rkt"
         "../extensions/hooks.rkt"
         "../extensions/context.rkt"
         "../extensions/api.rkt"
         "../util/protocol-types.rkt"
         "../runtime/session-switch.rkt")

;; ============================================================
;; Helpers
;; ============================================================

;; Minimal extension-registry mock for testing
(define (make-test-registry)
  (make-extension-registry))

(define (capture-events bus)
  (define b (box '()))
  (subscribe! bus (lambda (evt)
                    (set-box! b (cons evt (unbox b)))))
  b)

;; ============================================================
;; #704: Teardown old extension state
;; ============================================================

(test-case "emit-session-shutdown!: publishes session.shutdown event"
  (define bus (make-event-bus))
  (define captured (capture-events bus))
  (emit-session-shutdown! bus "old-session-1" #f #:duration 42)
  (define evts (unbox captured))
  (check-equal? (length evts) 1)
  (check-equal? (event-ev (car evts)) "session.shutdown")
  (check-equal? (hash-ref (event-payload (car evts)) 'session-id) "old-session-1")
  (check-equal? (hash-ref (event-payload (car evts)) 'duration) 42))

(test-case "emit-session-shutdown!: handles #f bus gracefully"
  (emit-session-shutdown! #f "old-session-1" #f)
  (check-true #t))

(test-case "teardown-session-extensions!: calls emit-session-shutdown"
  (define bus (make-event-bus))
  (define captured (capture-events bus))
  (teardown-session-extensions! "old-session-1" bus #f #:duration 100)
  (define evts (unbox captured))
  (check-equal? (length evts) 1)
  (check-equal? (event-ev (car evts)) "session.shutdown"))

;; ============================================================
;; #705: Rebind extensions to new session
;; ============================================================

(test-case "make-rebind-ctx: creates extension-ctx with new session data"
  (define bus (make-event-bus))
  (define reg (make-test-registry))
  (define ctx (make-rebind-ctx "new-session-1" "/tmp/sessions/new-session-1"
                                bus reg #:model-name "gpt-4o"))
  (check-true (extension-ctx? ctx))
  (check-equal? (ctx-session-id ctx) "new-session-1")
  (check-equal? (ctx-session-dir ctx) "/tmp/sessions/new-session-1")
  (check-equal? (ctx-model ctx) "gpt-4o"))

(test-case "rebind-extensions!: returns new extension-ctx"
  (define bus (make-event-bus))
  (define reg (make-test-registry))
  (define ctx (rebind-extensions! "new-session-2" "/tmp/sessions/new-session-2"
                                    bus reg))
  (check-true (extension-ctx? ctx))
  (check-equal? (ctx-session-id ctx) "new-session-2"))

(test-case "rebind-extensions!: dispatches session-rebind hook without error"
  (define bus (make-event-bus))
  (define reg (make-test-registry))
  ;; Should not crash even with empty registry
  (define ctx (rebind-extensions! "new-session-3" "/tmp/sessions/new-session-3"
                                    bus reg))
  (check-true (extension-ctx? ctx)))

;; ============================================================
;; #706: Session start event with reason
;; ============================================================

(test-case "emit-session-start!: publishes session.start with reason"
  (define bus (make-event-bus))
  (define captured (capture-events bus))
  (emit-session-start! bus "session-1" 'new)
  (define evts (unbox captured))
  (check-equal? (length evts) 1)
  (define evt (car evts))
  (check-equal? (event-ev evt) "session.start")
  (check-equal? (hash-ref (event-payload evt) 'session-id) "session-1")
  (check-equal? (hash-ref (event-payload evt) 'reason) 'new))

(test-case "emit-session-start!: includes previous-session-id for resume"
  (define bus (make-event-bus))
  (define captured (capture-events bus))
  (emit-session-start! bus "session-2" 'resume
                        #:previous-session-id "session-1")
  (define evts (unbox captured))
  (check-equal? (hash-ref (event-payload (car evts)) 'reason) 'resume)
  (check-equal? (hash-ref (event-payload (car evts)) 'previous-session-id) "session-1"))

(test-case "emit-session-start!: includes previous-session-id for fork"
  (define bus (make-event-bus))
  (define captured (capture-events bus))
  (emit-session-start! bus "session-fork-1" 'fork
                        #:previous-session-id "session-1")
  (check-equal? (hash-ref (event-payload (car (unbox captured))) 'reason) 'fork))

(test-case "emit-session-start!: rejects invalid reason"
  (check-exn exn:fail:contract?
    (lambda () (emit-session-start! (make-event-bus) "s1" 'invalid))))

(test-case "emit-session-start!: handles #f bus"
  (emit-session-start! #f "s1" 'new)
  (check-true #t))

(test-case "session-start-reason?: accepts valid reasons"
  (check-true (session-start-reason? 'new))
  (check-true (session-start-reason? 'resume))
  (check-true (session-start-reason? 'fork))
  (check-false (session-start-reason? 'invalid))
  (check-false (session-start-reason? "new")))

;; ============================================================
;; #707: Full atomic session switch lifecycle
;; ============================================================

(test-case "switch-session!: full lifecycle - new session"
  (define bus (make-event-bus))
  (define captured (capture-events bus))
  (define reg (make-test-registry))

  (define result (switch-session!
                   #:new-session-id "new-1"
                   #:new-session-dir "/tmp/new-1"
                   #:new-bus bus
                   #:new-extension-registry reg
                   #:reason 'new))

  (check-true (switch-result? result))
  (check-false (switch-result-old-session-id result))
  (check-equal? (switch-result-new-session-id result) "new-1")
  (check-equal? (switch-result-reason result) 'new)

  ;; Should have session.start event only (no teardown for new)
  (define evts (unbox captured))
  (define start-evts (filter (lambda (e) (string=? (event-ev e) "session.start")) evts))
  (define shutdown-evts (filter (lambda (e) (string=? (event-ev e) "session.shutdown")) evts))
  (check-equal? (length start-evts) 1)
  (check-equal? (length shutdown-evts) 0))

(test-case "switch-session!: full lifecycle - resume with teardown"
  (define bus (make-event-bus))
  (define captured (capture-events bus))
  (define reg (make-test-registry))

  (define result (switch-session!
                   #:old-session-id "old-1"
                   #:old-bus bus
                   #:old-extension-registry reg
                   #:old-duration 60
                   #:new-session-id "new-2"
                   #:new-session-dir "/tmp/new-2"
                   #:new-bus bus
                   #:new-extension-registry reg
                   #:reason 'resume))

  (check-equal? (switch-result-old-session-id result) "old-1")
  (check-equal? (switch-result-new-session-id result) "new-2")
  (check-equal? (switch-result-reason result) 'resume)

  ;; Should have both shutdown and start events
  (define evts (unbox captured))
  (define start-evts (filter (lambda (e) (string=? (event-ev e) "session.start")) evts))
  (define shutdown-evts (filter (lambda (e) (string=? (event-ev e) "session.shutdown")) evts))
  (check-equal? (length shutdown-evts) 1)
  (check-equal? (length start-evts) 1)

  ;; Shutdown for old session
  (check-equal? (hash-ref (event-payload (car shutdown-evts)) 'session-id) "old-1")
  ;; Start for new session with reason resume
  (check-equal? (hash-ref (event-payload (car start-evts)) 'reason) 'resume)
  (check-equal? (hash-ref (event-payload (car start-evts)) 'previous-session-id) "old-1"))

(test-case "switch-session!: full lifecycle - fork"
  (define bus (make-event-bus))
  (define captured (capture-events bus))

  (define result (switch-session!
                   #:old-session-id "parent-1"
                   #:old-bus bus
                   #:new-session-id "fork-1"
                   #:new-session-dir "/tmp/fork-1"
                   #:new-bus bus
                   #:reason 'fork))

  (check-equal? (switch-result-reason result) 'fork)
  (check-equal? (switch-result-old-session-id result) "parent-1"))

(test-case "switch-session!: rejects invalid reason"
  (check-exn exn:fail:contract?
    (lambda ()
      (switch-session!
        #:new-session-id "bad"
        #:new-session-dir "/tmp/bad"
        #:new-bus (make-event-bus)
        #:reason 'invalid))))

(test-case "switch-result struct: is transparent"
  (define sr (switch-result "old" "new" 'resume))
  (check-equal? (switch-result-old-session-id sr) "old")
  (check-equal? (switch-result-new-session-id sr) "new")
  (check-equal? (switch-result-reason sr) 'resume))
