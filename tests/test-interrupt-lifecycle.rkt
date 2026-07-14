#lang racket

;; @speed fast  ;; @suite runtime

(require rackunit
         rackunit/text-ui
         racket/file
         "../tui/commands/runtime-control.rkt"
         "../tui/commands/context.rkt"
         "../tui/state.rkt"
         "../util/event/event.rkt"
         "../util/event/event-bus.rkt"
         "../util/cancellation.rkt"
         "../runtime/agent-session.rkt"
         "../runtime/session/session-types.rkt"
         "../runtime/session/session-mutation.rkt"
         "../runtime/session/session-interruption.rkt"
         racket/dict
         racket/async-channel
         "../llm/model.rkt"
         "../llm/provider.rkt"
         (only-in "../util/message/protocol-types.rkt" loop-result-termination-reason)
         "../tui/render-loop/watchdog.rkt"
         racket/contract
         (only-in "../util/event/event-contracts.rkt" turn-cancelled-payload/c))

(define (make-test-cctx bus state)
  (cmd-ctx (box state) (box #t) bus #f (box #f) #f (box #f) #f (box "") #f #f (box #f) (box #f)))

(define (make-test-session dir bus [token #f])
  (make-agent-session (hasheq 'session-dir
                              dir
                              'event-bus
                              bus
                              'provider
                              #f
                              'tool-registry
                              #f
                              'model-name
                              "test"
                              'system-instructions
                              '()
                              'cancellation-token
                              token)))

(define interrupt-tests
  (test-suite "functional interrupt lifecycle"

    (test-case "idle slash interrupt emits no request and reports no active turn"
      (define bus (make-event-bus))
      (define events (box '()))
      (subscribe! bus (lambda (evt) (set-box! events (cons evt (unbox events)))))
      (define state (initial-ui-state #:session-id "session-a"))
      (define cctx (make-test-cctx bus state))
      (check-equal? (handle-interrupt-command cctx state) 'continue)
      (check-equal? (unbox events) '())
      (check-true (for/or ([entry (in-list (ui-state-transcript (unbox (cmd-ctx-state-box cctx))))])
                    (regexp-match? #rx"no active turn" (transcript-entry-text entry)))))

    (test-case "busy slash interrupt publishes one targeted correlated request and waits for ack"
      (define bus (make-event-bus))
      (define events (box '()))
      (subscribe! bus (lambda (evt) (set-box! events (append (unbox events) (list evt)))))
      (define state
        (set-active-turn-id
         (set-pending-tool-name
          (set-streaming-text (set-busy (initial-ui-state #:session-id "session-a") #t) "partial")
          "bash")
         "turn-a"))
      (define cctx (make-test-cctx bus state))
      (check-equal? (handle-interrupt-command cctx state) 'continue)
      (check-equal? (length (unbox events)) 1)
      (define requested (car (unbox events)))
      (check-equal? (event-ev requested) "interrupt.requested")
      (check-equal? (event-session-id requested) "session-a")
      (check-equal? (event-turn-id requested) "turn-a")
      (check-equal? (hash-ref (event-payload requested) 'target-session-id) "session-a")
      (check-equal? (hash-ref (event-payload requested) 'target-turn-id) "turn-a")
      (check-pred string? (hash-ref (event-payload requested) 'request-id #f))
      (define waiting (unbox (cmd-ctx-state-box cctx)))
      (check-true (ui-state-busy? waiting) "request alone must not clear busy")
      (check-equal? (ui-state-streaming-text waiting) "partial")
      (check-equal? (ui-state-pending-tool-name waiting) "bash")
      (check-pred string? (ui-state-interrupt-request-id waiting)))

    (test-case "only correlated cancellation acknowledgement clears busy streaming tool state"
      (define waiting
        (set-interrupt-request-id
         (set-active-turn-id
          (set-pending-tool-name
           (set-streaming-text (set-busy (initial-ui-state #:session-id "session-a") #t) "partial")
           "bash")
          "turn-a")
         "request-a"))
      (define unrelated
        (make-event "turn.cancelled"
                    1
                    "session-a"
                    "inner-turn"
                    (hasheq 'reason "cancellation-token")))
      (define still-waiting (apply-event-to-state waiting unrelated))
      (check-true (ui-state-busy? still-waiting))
      (check-equal? (ui-state-streaming-text still-waiting) "partial")
      (define acknowledged
        (make-event "turn.cancelled"
                    2
                    "session-a"
                    "turn-a"
                    (hasheq 'request-id "request-a" 'reason "cancelled")))
      (define completed (apply-event-to-state still-waiting acknowledged))
      (check-false (ui-state-busy? completed))
      (check-false (ui-state-streaming-text completed))
      (check-false (ui-state-pending-tool-name completed))
      (check-false (ui-state-active-turn-id completed))
      (check-false (ui-state-interrupt-request-id completed))
      (check-true (for/or ([entry (in-list (ui-state-transcript completed))])
                    (string-contains? (transcript-entry-text entry) "interrupt completed"))))

    (test-case "foreign lifecycle errors and watchdog cannot clear a pending interrupt"
      (define waiting
        (set-interrupt-request-id
         (set-active-turn-id
          (set-streaming-text (set-busy (initial-ui-state #:session-id "session-a") #t) "partial")
          "turn-a")
         "request-a"))
      (define foreign-start
        (make-event "turn.started" 1 "session-b" "turn-b" (hasheq 'scope "prompt")))
      (define foreign-complete (make-event "turn.completed" 2 "session-b" "turn-b" (hash)))
      (define stale-complete (make-event "turn.completed" 2 "session-a" "stale-turn" (hash)))
      (define same-session-error
        (make-event "runtime.error" 3 "session-a" "turn-a" (hasheq 'error "provider failed")))
      (for ([evt (in-list (list foreign-start foreign-complete stale-complete same-session-error))])
        (define after (apply-event-to-state waiting evt))
        (check-true (ui-state-busy? after))
        (check-equal? (ui-state-active-turn-id after) "turn-a")
        (check-equal? (ui-state-interrupt-request-id after) "request-a")
        (check-equal? (ui-state-streaming-text after) "partial"))
      (check-false (check-busy-watchdog waiting (+ (current-inexact-milliseconds) 9999999) 1)))

    (test-case "wired request cancels the active prompt, acknowledges once, and next prompt succeeds"
      (define dir (make-temporary-file "q-interrupt-integration-~a" 'directory))
      (define bus (make-event-bus))
      (define observed (box '()))
      (subscribe! bus (lambda (evt) (set-box! observed (append (unbox observed) (list evt)))))
      (define provider-entered (make-semaphore 0))
      (define stream-count (box 0))
      (define sess-box (box #f))
      (define provider
        (make-provider
         (lambda () "interrupt-integration")
         (lambda () (hash 'streaming #t 'token-counting #t))
         (lambda (_request) (make-model-response '() (hash) "mock" 'stop))
         (lambda (_request)
           (define count (add1 (unbox stream-count)))
           (set-box! stream-count count)
           (cond
             [(= count 1)
              (semaphore-post provider-entered)
              (let wait-for-cancel ()
                (define token
                  (dict-ref (agent-session-config (unbox sess-box)) 'cancellation-token #f))
                (unless (and token (cancellation-token-cancelled? token))
                  (sleep 0.01)
                  (wait-for-cancel)))
              (list (make-stream-chunk #f #f (hasheq) #t))]
             [else
              (list (make-stream-chunk "next turn works" #f #f #f)
                    (make-stream-chunk #f #f (hasheq) #t))]))))
      (define sess
        (make-agent-session (hasheq 'session-dir
                                    dir
                                    'event-bus
                                    bus
                                    'provider
                                    provider
                                    'tool-registry
                                    #f
                                    'model-name
                                    "test"
                                    'system-instructions
                                    '())))
      (set-box! sess-box sess)
      (define result-ch (make-async-channel))
      (thread (lambda ()
                (define-values (_updated result) (run-prompt! sess "cancel this prompt"))
                (async-channel-put result-ch result)))
      (check-not-false (sync/timeout 5 provider-entered))
      (define turn-id (active-session-turn-id sess))
      (check-pred string? turn-id)
      (publish! bus
                (make-event "interrupt.requested"
                            (current-inexact-milliseconds)
                            (agent-session-session-id sess)
                            turn-id
                            (hasheq 'request-id
                                    "request-integration"
                                    'target-session-id
                                    (agent-session-session-id sess)
                                    'target-turn-id
                                    turn-id)))
      (define cancelled-result (sync/timeout 5 result-ch))
      (check-not-false cancelled-result)
      (check-equal? (loop-result-termination-reason cancelled-result) 'cancelled)
      (define correlated-acks
        (filter (lambda (evt)
                  (and (equal? (event-ev evt) "turn.cancelled")
                       (equal? (hash-ref (event-payload evt) 'request-id #f) "request-integration")))
                (unbox observed)))
      (check-equal? (length correlated-acks) 1)
      (define acknowledgement (car correlated-acks))
      (check-equal? (event-turn-id acknowledgement) turn-id)
      (check-not-exn (lambda ()
                       (contract turn-cancelled-payload/c
                                 (event-payload acknowledgement)
                                 'interrupt-producer
                                 'interrupt-consumer)))
      (define round-tripped (jsexpr->event (event->jsexpr acknowledgement)))
      (check-equal? (event-session-id round-tripped) (event-session-id acknowledgement))
      (check-equal? (event-turn-id round-tripped) turn-id)
      (check-equal? (hash-ref (event-payload round-tripped) 'request-id #f) "request-integration")
      (define lifecycle-phases
        (filter (lambda (phase)
                  (member phase '("interrupt.requested" "interrupt.accepted" "turn.cancelled")))
                (map event-ev (unbox observed))))
      (check-equal? lifecycle-phases '("interrupt.requested" "interrupt.accepted" "turn.cancelled"))
      (define-values (_next-session next-result) (run-prompt! sess "next prompt"))
      (check-equal? (loop-result-termination-reason next-result) 'completed)
      (delete-directory/files dir #:must-exist? #f))

    (test-case "accepted request rotates token even when turn finishes before signal"
      (define dir (make-temporary-file "q-interrupt-race-~a" 'directory))
      (define token (make-cancellation-token))
      (define sess (make-test-session dir (make-event-bus) token))
      (guarded-set-prompt-running! sess #t)
      (define turn-id (begin-session-turn! sess))
      (check-equal?
       (request-session-interrupt! sess (agent-session-session-id sess) turn-id "race-request")
       'accepted)
      (define-values (_finished request-id) (finish-session-turn! sess))
      (check-equal? request-id "race-request")
      (check-false (signal-session-interrupt! sess "race-request"))
      (check-false (cancellation-token-cancelled? token)
                   "removed old token must never be cancelled after finish")
      (define fresh-token (dict-ref (agent-session-config sess) 'cancellation-token #f))
      (check-false (eq? fresh-token token))
      (check-false (cancellation-token-cancelled? fresh-token))
      (guarded-set-prompt-running! sess #f)
      (delete-directory/files dir #:must-exist? #f))

    (test-case "session interruption cancels only exact active session and turn and rotates token"
      (define dir (make-temporary-file "q-interrupt-~a" 'directory))
      (define bus (make-event-bus))
      (define initial-token (make-cancellation-token))
      (define sess (make-test-session dir bus initial-token))
      (guarded-set-prompt-running! sess #t)
      (define turn-id (begin-session-turn! sess))
      (check-equal? (active-session-turn-id sess) turn-id)
      (check-equal? (request-session-interrupt! sess "other-session" turn-id "request-wrong-session")
                    'unrelated)
      (check-false (cancellation-token-cancelled? initial-token))
      (check-equal?
       (request-session-interrupt! sess (agent-session-session-id sess) "stale-turn" "r2")
       'stale)
      (check-false (cancellation-token-cancelled? initial-token))
      (check-equal?
       (request-session-interrupt! sess (agent-session-session-id sess) turn-id "request-ok")
       'accepted)
      (check-false (cancellation-token-cancelled? initial-token)
                   "acceptance is observable before cancellation is signalled")
      (check-true (signal-session-interrupt! sess "request-ok"))
      (check-true (cancellation-token-cancelled? initial-token))
      (check-equal?
       (request-session-interrupt! sess (agent-session-session-id sess) turn-id "request-duplicate")
       'already-requested)
      (define-values (finished-turn request-id) (finish-session-turn! sess))
      (check-equal? finished-turn turn-id)
      (check-equal? request-id "request-ok")
      (define next-token (dict-ref (agent-session-config sess) 'cancellation-token #f))
      (check-true (cancellation-token? next-token))
      (check-false (eq? next-token initial-token))
      (check-false (cancellation-token-cancelled? next-token))
      (guarded-set-prompt-running! sess #f)
      (delete-directory/files dir #:must-exist? #f))))

(module+ test
  (run-tests interrupt-tests))
(module+ main
  (run-tests interrupt-tests))
