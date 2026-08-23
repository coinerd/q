#lang racket

;; @speed fast
;; @suite default
;; @boundary unit
;; BOUNDARY: integration

(require rackunit
         "../runtime/turn-orchestrator.rkt"
         "../runtime/session/session-config.rkt"
         "../util/event/event-bus.rkt"
         "../util/event/event.rkt"
         "../util/message/protocol-types.rkt"
         "../util/content/content-parts.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt")

(define (test-message id role text)
  (make-message id #f role 'message (list (make-text-part text)) 1000 (hasheq)))

(define large-context
  (list (test-message "s" 'system "system prompt")
        (test-message "u1" 'user "old user")
        (test-message "a1" 'assistant "old assistant")
        (test-message "u2" 'user "recent user")
        (test-message "a2" 'assistant "recent assistant")
        (test-message "u3" 'user "current user")))

(define (make-capturing-provider requests failure-message [failures 2])
  (define attempts 0)
  (make-provider (lambda () "adaptive-retry-test")
                 (lambda () (hash 'streaming #t))
                 (lambda (_) (make-model-response '() (hasheq) "test" 'stop))
                 (lambda (req)
                   (set! attempts (add1 attempts))
                   (set-box! requests (append (unbox requests) (list req)))
                   (if (<= attempts failures)
                       (raise (exn:fail failure-message (current-continuation-marks)))
                       (list (make-stream-chunk "ok" #f #f #f)
                             (make-stream-chunk #f #f (hasheq) #t))))))

(define (run-captured-turn ctx requests failure-message [failures 2] [events #f])
  (define bus (make-event-bus))
  (when events
    (subscribe! bus (lambda (evt) (set-box! events (append (unbox events) (list evt))))))
  (run-provider-turn ctx
                     (make-capturing-provider requests failure-message failures)
                     bus
                     #f
                     #f
                     "adaptive-session"
                     "adaptive-turn"
                     #f
                     (hash->session-config (hash 'max-tokens 1000))))

(test-case "PN-6: second retry trims oldest pair and lowers max-tokens"
  (define requests (box '()))
  (define events (box '()))
  (run-captured-turn large-context requests "connection timed out" 2 events)
  (define captured (unbox requests))
  (check-equal? (length captured) 3)
  (define first-request (first captured))
  (define first-retry-request (second captured))
  (define adaptive-request (third captured))
  (check-equal? (model-request-messages first-retry-request) (model-request-messages first-request))
  (check-equal? (model-request-settings first-retry-request) (model-request-settings first-request))
  (define first-messages (model-request-messages first-request))
  (define adaptive-messages (model-request-messages adaptive-request))
  (check-equal? (map (lambda (msg) (hash-ref msg 'role)) first-messages)
                '("system" "user" "assistant" "user" "assistant" "user"))
  (check-equal? (map (lambda (msg) (hash-ref msg 'role)) adaptive-messages)
                '("system" "user" "assistant" "user"))
  (check-equal? (hash-ref (first adaptive-messages) 'role) "system")
  (check-equal? (hash-ref (model-request-settings first-request) 'max-tokens) 1000)
  (check-equal? (hash-ref (model-request-settings adaptive-request) 'max-tokens) 750)
  (check-true (for/or ([evt (in-list (unbox events))])
                (equal? (event-ev evt) "provider.adaptive-retry"))))

(test-case "PN-6: minimum context floor preserves the remaining pair"
  (define floor-context
    (list (test-message "s" 'system "system prompt")
          (test-message "u1" 'user "only user")
          (test-message "a1" 'assistant "only assistant")
          (test-message "u2" 'user "current user")))
  (define requests (box '()))
  (define events (box '()))
  (run-captured-turn floor-context requests "connection timed out" 2 events)
  (define first-request (first (unbox requests)))
  (define adaptive-request (third (unbox requests)))
  (check-equal? (model-request-messages adaptive-request) (model-request-messages first-request))
  (check-equal? (hash-ref (model-request-settings adaptive-request) 'max-tokens) 1000)
  (define adaptive-event
    (for/first ([evt (in-list (unbox events))]
                #:when (equal? (event-ev evt) "provider.adaptive-retry"))
      evt))
  (check-not-false adaptive-event)
  (check-true (hash-ref (event-payload adaptive-event) 'floorReached)))

(test-case "PN-6: non-retryable auth errors do not adapt"
  (define requests (box '()))
  (define events (box '()))
  (check-exn exn:fail?
             (lambda () (run-captured-turn large-context requests "HTTP 401 unauthorized" 3 events)))
  (check-equal? (length (unbox requests)) 1)
  (check-false (for/or ([evt (in-list (unbox events))])
                 (equal? (event-ev evt) "provider.adaptive-retry"))))
