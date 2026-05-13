#lang racket/base

;; BOUNDARY: integration

;; test-mid-turn-compaction-integration.rkt — TDD tests for v0.28.22 W0
;; Integration tests: mid-turn compaction wired through iteration loop

(require rackunit
         rackunit/text-ui
         racket/string
         racket/file
         "../util/message.rkt"
         "../util/content-parts.rkt"
         "../util/event.rkt"
         (only-in "../runtime/session-compaction.rkt" compact-context-mid-turn)
         "../runtime/compactor.rkt"
         "../runtime/agent-session.rkt"
         "../runtime/runtime-helpers.rkt"
         "../runtime/iteration/retry-policy.rkt"
         "../llm/token-budget.rkt"
         "../agent/event-bus.rkt")

(define integration-suite
  (test-suite "Mid-turn compaction integration"

    (test-case "check-mid-turn-budget! returns compacted context with session"
      (define bus (make-event-bus))
      (define events '())
      (subscribe! bus (lambda (evt) (set! events (cons evt events))))
      ;; Build 50-message context
      (define ctx
        (for/list ([i (in-range 50)])
          (message (format "id~a" i)
                   #f
                   'user
                   'message
                   (list (text-part "text" (format "Message ~a ~a" i (make-string 200 #\x))))
                   (current-seconds)
                   (hasheq))))
      ;; Very tight budget to trigger compaction
      (define config (hasheq 'max-context-tokens 100))
      ;; Call with session=#f — should return integer (token estimate)
      (define result-no-sess
        (check-mid-turn-budget!
         ctx
         "test-session"
         config
         #:emit-event (lambda (name payload) (emit-session-event! bus "test-session" name payload))))
      (check-true (integer? result-no-sess) "without session returns integer"))

    (test-case "compact-history returns valid result structure"
      (define ctx
        (for/list ([i (in-range 50)])
          (message (format "id~a" i)
                   #f
                   'user
                   'message
                   (list (text-part "text" (format "Message ~a ~a" i (make-string 200 #\x))))
                   (current-seconds)
                   (hasheq))))
      (define result (compact-history ctx))
      (check-true (compaction-result? result))
      (check-true (list? (compaction-result-kept-messages result))
                  "compaction result has kept messages"))

    (test-case "context.mid-turn-over-budget event emitted"
      (define bus (make-event-bus))
      (define events '())
      (subscribe! bus (lambda (evt) (set! events (cons evt events))))
      (define ctx
        (for/list ([i (in-range 50)])
          (message (format "id~a" i)
                   #f
                   'user
                   'message
                   (list (text-part "text" (format "Message ~a ~a" i (make-string 200 #\x))))
                   (current-seconds)
                   (hasheq))))
      (define config (hasheq 'max-context-tokens 100))
      (check-mid-turn-budget! ctx
                              "test-session"
                              config
                              #:emit-event (lambda (name payload)
                                             (emit-session-event! bus "test-session" name payload)))
      (check-true (> (length events) 0) "event emitted"))

    (test-case "under-budget context returns unchanged estimate"
      (define ctx
        (for/list ([i (in-range 3)])
          (message (format "id~a" i)
                   #f
                   'user
                   'message
                   (list (text-part "text" (format "Message ~a" i)))
                   (current-seconds)
                   (hasheq))))
      (define config (hasheq 'max-context-tokens 128000))
      (define result (check-mid-turn-budget! ctx #f config))
      (check-true (integer? result) "returns integer for under-budget")
      (check-true (> result 0) "positive token estimate"))

    ;; v0.28.23 W1: integration tests with split functions + mock session

    (test-case "estimate-mid-turn-tokens always returns integer"
      (define bus (make-event-bus))
      (define ctx
        (for/list ([i (in-range 50)])
          (message (format "id~a" i)
                   #f
                   'user
                   'message
                   (list (text-part "text" (format "Message ~a ~a" i (make-string 200 #\x))))
                   (current-seconds)
                   (hasheq))))
      (define config (hasheq 'max-context-tokens 128000))
      (define result
        (estimate-mid-turn-tokens
         ctx
         "test-session"
         config
         #:emit-event (lambda (name payload) (emit-session-event! bus "test-session" name payload))))
      (check-true (exact-positive-integer? result)
                  "estimate-mid-turn-tokens returns exact positive integer"))

    (test-case "estimate-mid-turn-tokens emits over-budget event when exceeded"
      (define bus (make-event-bus))
      (define events '())
      (subscribe! bus (lambda (evt) (set! events (cons evt events))))
      (define ctx
        (for/list ([i (in-range 50)])
          (message (format "id~a" i)
                   #f
                   'user
                   'message
                   (list (text-part "text" (format "Message ~a ~a" i (make-string 200 #\x))))
                   (current-seconds)
                   (hasheq))))
      (define config (hasheq 'max-context-tokens 100))
      (estimate-mid-turn-tokens ctx
                                "test-session"
                                config
                                #:emit-event (lambda (name payload)
                                               (emit-session-event! bus "test-session" name payload)))
      (define budget-events
        (filter (lambda (e)
                  (and (event? e)
                       (string? (event-ev e))
                       (string-contains? (event-ev e) "mid-turn-over-budget")))
                events))
      (check-true (> (length budget-events) 0)
                  "over-budget event emitted by estimate-mid-turn-tokens"))

    (test-case "maybe-compact-mid-turn with mock session returns message list"
      (define tmpdir (make-temporary-file "q-midturn-test-~a" 'directory))
      (define bus (make-event-bus))
      (define events '())
      (subscribe! bus (lambda (evt) (set! events (cons evt events))))
      (define sess
        (make-agent-session (hasheq 'session-dir
                                    (path->string tmpdir)
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
                                    'max-context-tokens
                                    100)))
      ;; Build large context to exceed budget
      (define ctx
        (for/list ([i (in-range 50)])
          (message (format "id~a" i)
                   #f
                   'user
                   'message
                   (list (text-part "text" (format "Message ~a ~a" i (make-string 200 #\x))))
                   (current-seconds)
                   (hasheq))))
      (define result
        (maybe-compact-mid-turn sess
                                ctx
                                "test-session"
                                (hasheq 'max-context-tokens 100)
                                #:emit-event (lambda (name payload)
                                               (emit-session-event! bus "test-session" name payload))
                                #:compact-proc (lambda (ctx) (compact-context-mid-turn sess ctx))))
      (check-true (list? result) "maybe-compact-mid-turn returns list")
      (check-true (<= (length result) (length ctx)) "compaction returns at most original count")
      (check-true (andmap message? result) "all results are messages"))

    (test-case "maybe-compact-mid-turn under-budget returns original context"
      (define tmpdir (make-temporary-file "q-midturn-test-~a" 'directory))
      (define bus (make-event-bus))
      (define sess
        (make-agent-session (hasheq 'session-dir
                                    (path->string tmpdir)
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
                                    'max-context-tokens
                                    128000)))
      ;; Small context well under budget
      (define ctx
        (for/list ([i (in-range 3)])
          (message (format "id~a" i)
                   #f
                   'user
                   'message
                   (list (text-part "text" (format "Message ~a" i)))
                   (current-seconds)
                   (hasheq))))
      (define result
        (maybe-compact-mid-turn sess
                                ctx
                                "test-session"
                                (hasheq 'max-context-tokens 128000)
                                #:emit-event (lambda (name payload)
                                               (emit-session-event! bus "test-session" name payload))
                                #:compact-proc (lambda (ctx) (compact-context-mid-turn sess ctx))))
      (check-equal? (length result) (length ctx) "under-budget returns original context unchanged"))

    ;; v0.28.24 W0: test shared token estimation helper

    (test-case "compute-mid-turn-estimate returns correct triple"
      (define ctx
        (for/list ([i (in-range 5)])
          (message (format "id~a" i)
                   #f
                   'user
                   'message
                   (list (text-part "text" (format "Msg ~a" i)))
                   (current-seconds)
                   (hasheq))))
      (define config (hasheq 'max-context-tokens 1000))
      (define-values (estimated threshold max-tok)
        (compute-mid-turn-estimate ctx config estimate-context-tokens))
      (check-true (exact-positive-integer? estimated) "estimate is positive integer")
      (check-equal? max-tok 1000 "max-tokens from config")
      (check-equal? threshold 900 "threshold is 90% of max"))))

(run-tests integration-suite)
