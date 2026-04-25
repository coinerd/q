#lang racket

;; tests/test-ext-event-bus.rkt — tests for Inter-extension Event Bus (#684, #685)
;;
;; Covers:
;;   - Extension-friendly subscribe/publish/unsubscribe API
;;   - Per-extension subscription tracking
;;   - Auto-cleanup on extension unload
;;   - Filtered subscriptions
;;   - Multi-extension isolation

(require rackunit
         "../agent/event-bus.rkt"
         "../util/protocol-types.rkt"
         "../extensions/api.rkt"
         "../extensions/events.rkt"
         "../extensions/context.rkt")

;; ============================================================
;; Basic subscribe/publish
;; ============================================================

(test-case "ext-subscribe! registers handler and returns sub-id"
  (define bus (make-event-bus))
  (define received (box #f))
  (define sub-id (ext-subscribe! bus "test-ext"
                                  (lambda (evt)
                                    (set-box! received evt))))
  (check-true (exact-nonnegative-integer? sub-id))
  ;; Subscription is tracked
  (check-true (and (member sub-id (ext-subscription-ids "test-ext")) #t))
  ;; Publish and verify
  (define test-evt (make-event "test.topic" (current-seconds) "" #f (hasheq 'key "value")))
  (ext-publish! bus test-evt)
  (check-equal? (unbox received) test-evt))

(test-case "ext-unsubscribe! removes handler and tracking"
  (define bus (make-event-bus))
  (define received (box #f))
  (define sub-id (ext-subscribe! bus "unsub-ext"
                                  (lambda (evt)
                                    (set-box! received evt))))
  (ext-unsubscribe! bus "unsub-ext" sub-id)
  ;; No longer tracked
  (check-false (and (member sub-id (ext-subscription-ids "unsub-ext")) #t))
  ;; Should not receive events
  (ext-publish! bus (make-event "test.topic" (current-seconds) "" #f (hasheq)))
  (check-false (unbox received)))

(test-case "ext-publish! returns the published event"
  (define bus (make-event-bus))
  (define test-evt (make-event "test.topic" (current-seconds) "" #f (hasheq 'x 1)))
  (define result (ext-publish! bus test-evt))
  (check-equal? result test-evt))

;; ============================================================
;; Filtered subscriptions
;; ============================================================

(test-case "ext-subscribe! with #:filter only receives matching events"
  (define bus (make-event-bus))
  (define received '())
  (ext-subscribe! bus "filter-ext"
                  (lambda (evt)
                    (set! received (cons evt received)))
                  #:filter (lambda (evt)
                             (equal? (event-ev evt) "important")))
  (ext-publish! bus (make-event "noise" (current-seconds) "" #f (hasheq)))
  (ext-publish! bus (make-event "important" (current-seconds) "" #f (hasheq 'data "yes")))
  (ext-publish! bus (make-event "noise2" (current-seconds) "" #f (hasheq)))
  ;; Only the "important" event should be received
  (check-equal? (length received) 1)
  (check-equal? (event-ev (car received)) "important"))

;; ============================================================
;; Per-extension subscription tracking
;; ============================================================

(test-case "multiple subscriptions tracked per extension"
  (define bus (make-event-bus))
  (define id1 (ext-subscribe! bus "multi-ext" (lambda (evt) (void))))
  (define id2 (ext-subscribe! bus "multi-ext" (lambda (evt) (void))))
  (define id3 (ext-subscribe! bus "multi-ext" (lambda (evt) (void))))
  (define ids (ext-subscription-ids "multi-ext"))
  (check-equal? (length ids) 3)
  (check-true (and (member id1 ids) #t))
  (check-true (and (member id2 ids) #t))
  (check-true (and (member id3 ids) #t)))

(test-case "extensions tracked independently"
  (define bus (make-event-bus))
  (ext-subscribe! bus "ext-a" (lambda (evt) (void)))
  (ext-subscribe! bus "ext-b" (lambda (evt) (void)))
  (ext-subscribe! bus "ext-a" (lambda (evt) (void)))
  (check-equal? (length (ext-subscription-ids "ext-a")) 2)
  (check-equal? (length (ext-subscription-ids "ext-b")) 1))

;; ============================================================
;; Auto-cleanup on extension unload
;; ============================================================

(test-case "ext-unsubscribe-all! removes all extension subscriptions"
  (define bus (make-event-bus))
  (define received (box '()))
  (ext-subscribe! bus "cleanup-ext"
                  (lambda (evt)
                    (set-box! received (cons evt (unbox received)))))
  (ext-subscribe! bus "cleanup-ext"
                  (lambda (evt)
                    (set-box! received (cons evt (unbox received)))))
  (check-equal? (length (ext-subscription-ids "cleanup-ext")) 2)
  ;; Cleanup
  (ext-unsubscribe-all! bus "cleanup-ext")
  ;; No more tracked subscriptions
  (check-equal? (ext-subscription-ids "cleanup-ext") '())
  ;; Events should not be received
  (ext-publish! bus (make-event "test" (current-seconds) "" #f (hasheq)))
  (check-equal? (unbox received) '()))

(test-case "ext-unsubscribe-all! is idempotent"
  (define bus (make-event-bus))
  (ext-unsubscribe-all! bus "nonexistent-ext")
  (check-equal? (ext-subscription-ids "nonexistent-ext") '()))

;; ============================================================
;; Multi-extension event flow
;; ============================================================

(test-case "two extensions can communicate via event bus"
  (define bus (make-event-bus))
  (define ext-a-received (box #f))
  (define ext-b-received (box #f))
  ;; Ext A subscribes to "b.output" events
  (ext-subscribe! bus "ext-a"
                  (lambda (evt)
                    (set-box! ext-a-received evt))
                  #:filter (lambda (evt) (equal? (event-ev evt) "b.output")))
  ;; Ext B subscribes to "a.output" events
  (ext-subscribe! bus "ext-b"
                  (lambda (evt)
                    (set-box! ext-b-received evt))
                  #:filter (lambda (evt) (equal? (event-ev evt) "a.output")))
  ;; A publishes, B receives
  (ext-publish! bus (make-event "a.output" (current-seconds) "" #f (hasheq 'from "a")))
  (check-not-false (unbox ext-b-received))
  (check-equal? (hash-ref (event-payload (unbox ext-b-received)) 'from) "a")
  ;; B publishes, A receives
  (set-box! ext-a-received #f)
  (ext-publish! bus (make-event "b.output" (current-seconds) "" #f (hasheq 'from "b")))
  (check-not-false (unbox ext-a-received))
  (check-equal? (hash-ref (event-payload (unbox ext-a-received)) 'from) "b"))

(test-case "subscriber errors are isolated"
  (define bus (make-event-bus))
  (define received (box #f))
  ;; First subscriber throws
  (ext-subscribe! bus "bad-ext"
                  (lambda (evt) (error "boom")))
  ;; Second subscriber should still receive
  (ext-subscribe! bus "good-ext"
                  (lambda (evt) (set-box! received evt)))
  (ext-publish! bus (make-event "test" (current-seconds) "" #f (hasheq)))
  (check-not-false (unbox received)))

;; ============================================================
;; Extension context integration
;; ============================================================

(test-case "extension can access event bus from context"
  (define bus (make-event-bus))
  (define ext-reg (make-extension-registry))
  (define received (box #f))
  ;; Register an extension that subscribes via its hook
  (register-extension! ext-reg
    (extension "bus-user" "0.1" "1.0"
               (hasheq 'on-load
                       (lambda (ctx)
                         ;; ctx has event-bus accessor
                         (define bus-from-ctx (ctx-event-bus ctx))
                         (ext-subscribe! bus-from-ctx "bus-user"
                                         (lambda (evt) (set-box! received evt)))))))
  ;; The extension's hooks are registered but on-load isn't auto-dispatched
  ;; in this test — just verify the extension is registered
  (check-not-false (lookup-extension ext-reg "bus-user")))
