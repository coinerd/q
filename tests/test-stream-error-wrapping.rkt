#lang racket

;; BOUNDARY: integration

;; tests/test-stream-error-wrapping.rkt -- A6-02 + A7-02: streaming error wrapping tests

(require racket/generator
         rackunit
         rackunit/text-ui
         "../llm/provider-errors.rkt"
         "../llm/http-helpers.rkt"
         "../agent/streaming-message.rkt"
         "../util/protocol-types.rkt"
         (only-in "../util/content-parts.rkt" text-part?)
         "../agent/state.rkt"
         "../agent/event-bus.rkt"
         "../util/event-access.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt"
         (only-in "../agent/loop-stream.rkt" stream-from-provider))

(define stream-error-suite
  (test-suite "streaming error wrapping"

    ;; ── Struct contract tests (A6-02) ──

    ;; Test 1: provider-error struct is exn:fail subtype
    (test-case "provider-error is exn:fail"
      (check-exn exn:fail? (lambda () (raise-provider-error "test" 'network))))

    ;; Test 2: provider-error category is 'network for streaming
    (test-case "provider-error has network category"
      (with-handlers ([provider-error? (lambda (e)
                                         (check-equal? (provider-error-category e) 'network)
                                         (check-false (provider-error-status-code e)))])
        (raise-provider-error "network failure" 'network #f)))

    ;; Test 3: raise-provider-error defaults status-code to #f
    (test-case "raise-provider-error without status-code defaults to #f"
      (with-handlers ([provider-error? (lambda (e) (check-false (provider-error-status-code e)))])
        (raise-provider-error "no status" 'timeout)))

    ;; Test 4: provider-error roundtrip preserves message
    (test-case "provider-error preserves original message"
      (with-handlers ([provider-error?
                       (lambda (e) (check-true (string-contains? (exn-message e) "test message")))])
        (raise-provider-error "test message for roundtrip" 'network)))

    ;; Test 5: provider-error is transparent
    (test-case "provider-error is transparent struct"
      (with-handlers ([provider-error? (lambda (e) (check-true (vector? (struct->vector e))))])
        (raise-provider-error "transparency test" 'auth 401)))

    ;; ── Wrapping pattern tests (A7-02) ──

    ;; Test 6: Wrapping pattern converts raw exn:fail to provider-error
    (test-case "wrapping pattern: raw exn:fail becomes provider-error"
      (with-handlers ([provider-error? (lambda (e)
                                         (check-true (string-contains? (exn-message e) "raw error"))
                                         (check-equal? (provider-error-category e) 'network))])
        (with-handlers ([exn:fail? (lambda (e)
                                     (if (provider-error? e)
                                         (raise e)
                                         (raise (provider-error (format "Wrapped: ~a" (exn-message e))
                                                                (current-continuation-marks)
                                                                (hash)
                                                                'network
                                                                #f))))])
          (raise (exn:fail "raw error" (current-continuation-marks))))))

    ;; Test 7: Wrapping pattern passes through existing provider-error
    (test-case "wrapping pattern: provider-error passes through without re-wrapping"
      (with-handlers ([provider-error? (lambda (e)
                                         ;; Must see ORIGINAL provider-error, not re-wrapped
                                         (check-equal? (provider-error-category e) 'timeout)
                                         (check-equal? (provider-error-status-code e) 408))])
        (with-handlers ([exn:fail? (lambda (e)
                                     (if (provider-error? e)
                                         (raise e)
                                         (raise (provider-error (format "Wrapped: ~a" (exn-message e))
                                                                (current-continuation-marks)
                                                                (hash)
                                                                'network
                                                                #f))))])
          (raise (provider-error "timeout error" (current-continuation-marks) (hash) 408 'timeout)))))

    ;; Test 8: check-provider-status! wraps 4xx as provider-error
    (test-case "check-provider-status! wraps 4xx as provider-error"
      (with-handlers ([exn:fail? (lambda (e)
                                   (check-pred provider-error? e)
                                   (check-equal? (provider-error-status-code e) 400))])
        (check-provider-status! "Test" #"HTTP/1.1 400 Bad Request" #"{}")))

    ;; Test 9: check-provider-status! passes 2xx through
    (test-case "check-provider-status! passes 200 through"
      (check-equal? (check-provider-status! "Test" #"HTTP/1.1 200 OK" #"{}") (void)))

    ;; ── v0.45.10 NF2: Partial message persistence tests ──

    ;; Test 10: stream-from-provider adds partial message to loop-state on error
    (test-case "NF1: partial message persisted to loop-state on stream error"
      ;; Create a provider that streams one chunk then raises
      (define error-prov
        (make-provider (lambda () "error-mock")
                       (lambda () (hash 'streaming #t 'token-counting #t))
                       (lambda (req) (hasheq))
                       (lambda (req)
                         (generator ()
                                    (yield (make-stream-chunk "Hello partial" #f #f #f))
                                    (raise (exn:fail "stream timeout"
                                                     (current-continuation-marks)))))))
      (define bus (make-event-bus))
      (define st (make-loop-state "test-session" "test-turn"))
      ;; Subscribe to events to avoid them piling up
      (define events-received (box '()))
      (subscribe! bus (lambda (evt) (set-box! events-received (cons evt (unbox events-received)))))
      ;; stream-from-provider should raise after adding partial message
      (check-exn exn:fail?
                 (lambda ()
                   (stream-from-provider error-prov
                                         (make-model-request '() #f (hasheq))
                                         bus
                                         "test-session"
                                         "test-turn"
                                         st
                                         #f ;; no hook dispatcher
                                         #f))) ;; no cancellation token
      ;; Verify partial message was added to loop-state
      (define msgs (loop-state-messages st))
      (check-equal? (length msgs) 1)
      (define partial-msg (car msgs))
      (check-equal? (message-role partial-msg) 'assistant)
      (check-equal? (message-kind partial-msg) 'message)
      (check-true (hash-ref (message-meta partial-msg) 'partial #f))
      ;; Verify content contains the partial text
      (define content (message-content partial-msg))
      (check-true (and (list? content) (= (length content) 1)))
      (check-equal? (text-part-text (car content)) "Hello partial"))

    ;; Test 11: no partial message when stream error with no text
    (test-case "NF1: no partial message when stream error with empty text"
      ;; Create a provider that raises before emitting any content
      (define error-prov-no-text
        (make-provider (lambda () "error-mock-empty")
                       (lambda () (hash 'streaming #t 'token-counting #t))
                       (lambda (req) (hasheq))
                       (lambda (req)
                         (generator ()
                                    (raise (exn:fail "stream error before content"
                                                     (current-continuation-marks)))))))
      (define bus2 (make-event-bus))
      (define st2 (make-loop-state "test-session2" "test-turn2"))
      (subscribe! bus2 (lambda (evt) (void)))
      (check-exn exn:fail?
                 (lambda ()
                   (stream-from-provider error-prov-no-text
                                         (make-model-request '() #f (hasheq))
                                         bus2
                                         "test-session2"
                                         "test-turn2"
                                         st2
                                         #f
                                         #f)))
      ;; Verify NO message was added to loop-state
      (check-equal? (loop-state-messages st2) '()))

    (test-case "NF1: current-loop-state-for-error-recovery parameter lifecycle"
      ;; 1. Default is #f
      (check-false (current-loop-state-for-error-recovery))
      ;; 2. parameterize sets and unsets correctly
      (define st (make-loop-state "test-session" "test-turn"))
      (parameterize ([current-loop-state-for-error-recovery st])
        (check-equal? (current-loop-state-for-error-recovery) st)
        ;; 3. Messages accumulated in loop-state are visible through parameter
        (state-add-message! st (hasheq 'role 'assistant 'content "partial text"))
        (define recovered (current-loop-state-for-error-recovery))
        (check-not-false recovered)
        (define msgs (loop-state-messages recovered))
        (check-equal? (length msgs) 1)
        (check-equal? (hash-ref (car msgs) 'content) "partial text"))
      ;; 4. After parameterize exits, parameter is back to #f
      (check-false (current-loop-state-for-error-recovery)))

    (test-case "NF1: parameter with nested parameterize restores correctly"
      (define st1 (make-loop-state "s1" "t1"))
      (define st2 (make-loop-state "s2" "t2"))
      (parameterize ([current-loop-state-for-error-recovery st1])
        (check-equal? (current-loop-state-for-error-recovery) st1)
        (parameterize ([current-loop-state-for-error-recovery st2])
          (check-equal? (current-loop-state-for-error-recovery) st2))
        (check-equal? (current-loop-state-for-error-recovery) st1)))

    ;; ============================================================

    (test-case "v0.45.13 L3: thinking-only stream does NOT persist partial message"
      ;; When a stream has thinking content but no text content and errors,
      ;; the partial message is NOT persisted. This is by design: the error
      ;; handler checks streaming-message-text, not thinking.
      ;; This test documents the behavior for future reference.
      (define thinking-only-prov
        (make-provider
         (lambda () "thinking-only-mock")
         (lambda () (hash 'streaming #t 'token-counting #t))
         (lambda (req) (hasheq))
         (lambda (req)
           (generator ()
                      ;; Send a chunk with thinking but no text
                      (yield (make-stream-chunk #f #f #f #f #:delta-thinking "Deep thinking..."))
                      ;; Then error
                      (raise (exn:fail "stream timeout" (current-continuation-marks)))))))
      (define bus (make-event-bus))
      (define st (make-loop-state "test-session-t" "test-turn-t"))
      (define events-collected (box '()))
      (subscribe! bus (lambda (evt) (set-box! events-collected (cons evt (unbox events-collected)))))
      (check-exn exn:fail?
                 (lambda ()
                   (stream-from-provider thinking-only-prov
                                         (make-model-request '() #f (hasheq))
                                         bus
                                         "test-session-t"
                                         "test-turn-t"
                                         st
                                         #f
                                         #f)))
      ;; Verify thinking was actually processed: a stream-thinking event was emitted
      (define thinking-events
        (filter (lambda (e) (equal? (event-type-ref e) "model.stream.thinking"))
                (unbox events-collected)))
      (check-true (>= (length thinking-events) 1)
                  "thinking content was processed (stream-thinking event emitted)")
      ;; Verify NO message was persisted — thinking-only content is not saved
      ;; This is the current behavior. If changed, this test should be updated.
      (check-equal? (loop-state-messages st) '()))

    (test-case "v0.45.13 L3: text+thinking stream persists full message"
      ;; When a stream has BOTH text and thinking and errors,
      ;; the partial message IS persisted with the text content.
      (define both-prov
        (make-provider
         (lambda () "both-mock")
         (lambda () (hash 'streaming #t 'token-counting #t))
         (lambda (req) (hasheq))
         (lambda (req)
           (generator ()
                      ;; Send text+thinking chunk
                      (yield (make-stream-chunk "Hello" #f #f #f #:delta-thinking "Thinking..."))
                      ;; Then error
                      (raise (exn:fail "stream error" (current-continuation-marks)))))))
      (define bus2 (make-event-bus))
      (define st2 (make-loop-state "test-session-b" "test-turn-b"))
      (subscribe! bus2 (lambda (evt) (void)))
      (check-exn exn:fail?
                 (lambda ()
                   (stream-from-provider both-prov
                                         (make-model-request '() #f (hasheq))
                                         bus2
                                         "test-session-b"
                                         "test-turn-b"
                                         st2
                                         #f
                                         #f)))
      ;; Verify partial message WAS persisted — text is present
      (define msgs (loop-state-messages st2))
      (check-equal? (length msgs) 1)
      (define content (message-content (car msgs)))
      (check-true (and (list? content) (= (length content) 1)))
      (check-equal? (text-part-text (car content)) "Hello")
      ;; Verify thinking is NOT in the persisted partial message content
      ;; (only text parts are persisted, thinking is not saved as content)
      (check-true (andmap text-part? content)
                  "partial message contains only text parts, no thinking parts"))

    ;; ============================================================
    ;; v0.45.13 M1: NF1 parameter integration with stream-from-provider
    ;; ============================================================

    (test-case "v0.45.13 M1: parameter state matches state passed to stream-from-provider"
      ;; Verify that when stream-from-provider is called within a parameterize
      ;; of current-loop-state-for-error-recovery, the state object is accessible
      ;; through the parameter during error recovery.
      (define error-prov
        (make-provider (lambda () "integration-mock")
                       (lambda () (hash 'streaming #t 'token-counting #t))
                       (lambda (req) (hasheq))
                       (lambda (req)
                         (generator ()
                                    (yield (make-stream-chunk "Integration text" #f #f #f))
                                    (raise (exn:fail "integration stream error"
                                                     (current-continuation-marks)))))))
      (define bus (make-event-bus))
      (define st (make-loop-state "int-session" "int-turn"))
      (subscribe! bus (lambda (evt) (void)))
      ;; Set the parameter, then call stream-from-provider
      (parameterize ([current-loop-state-for-error-recovery st])
        ;; Verify parameter is set
        (check-equal? (current-loop-state-for-error-recovery) st)
        ;; Call stream-from-provider — it should error
        (check-exn exn:fail?
                   (lambda ()
                     (stream-from-provider error-prov
                                           (make-model-request '() #f (hasheq))
                                           bus
                                           "int-session"
                                           "int-turn"
                                           st
                                           #f
                                           #f)))
        ;; After error, verify partial message was persisted to the same state object
        (define msgs (loop-state-messages st))
        (check-equal? (length msgs) 1 "partial message persisted to loop-state")
        ;; Verify the same state is still accessible through the parameter
        (check-equal? (current-loop-state-for-error-recovery) st)
        (define recovered-msgs (loop-state-messages (current-loop-state-for-error-recovery)))
        (check-equal? (length recovered-msgs) 1 "parameter state has the partial message"))
      ;; After parameterize exits, parameter is back to #f
      (check-false (current-loop-state-for-error-recovery)))))

(run-tests stream-error-suite 'verbose)
