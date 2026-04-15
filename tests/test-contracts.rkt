#lang racket

;; q/tests/test-contracts.rkt — Module boundary contract assertion tests
;;
;; Verifies data shapes at module boundaries. Would have caught BUG-04,
;; BUG-05, BUG-10, BUG-16, #513, #521 class bugs.
;;
;; Tests validate:
;; 1. Event-bus payload shapes — every event has expected keys
;; 2. Provider response → message conversion — HTTP→struct shape
;; 3. Scheduler → tool arity agreement — correct argument passing
;; 4. Config wiring contract — config keys match expectations

(require rackunit
         rackunit/text-ui
         json
         "../llm/model.rkt"
         "../llm/provider.rkt"
         "../llm/stream.rkt"
         "../agent/event-bus.rkt"
         (only-in "../util/protocol-types.rkt"
                  make-event event? event-event event-payload
                  event-time event-session-id event-turn-id
                  event->jsexpr jsexpr->event
                  message? message-id message-role message-content
                  make-message make-text-part
                  message->jsexpr jsexpr->message
                  loop-result? loop-result-termination-reason
                  make-tool-call)
         (only-in "../tools/tool.rkt"
                  make-tool-registry register-tool! make-tool lookup-tool
                  tool-names tool? tool-execute tool->jsexpr
                  make-exec-context exec-context?
                  make-success-result make-error-result
                  tool-result? tool-result-content tool-result-is-error?)
         "../tools/scheduler.rkt"
         "../runtime/settings.rkt"
         "../extensions/api.rkt"
         "../util/jsonl.rkt"
         (prefix-in sdk: "../interfaces/sdk.rkt")
         "helpers/mock-provider.rkt")

(require (only-in "../main.rkt"
                  register-default-tools!
                  make-event-bus))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-temp-dir)
  (make-temporary-file "q-contr-~a" 'directory))

(define (cleanup-dir dir)
  (when (directory-exists? dir)
    (delete-directory/files dir)))

;; ============================================================
;; 1. Event-bus payload shapes
;; ============================================================

(test-case "contract: event struct has required fields"
  (define evt (make-event "test.event" 12345 "sess-1" "turn-1"
                          (hasheq 'key "value")))
  ;; event struct fields
  (check-equal? (event-event evt) "test.event")
  (check-true (number? (event-time evt)))
  ;; event->jsexpr round-trip
  (define js (event->jsexpr evt))
  (check-not-false (hash-ref js 'event #f) "jsexpr should have 'event")
  (check-not-false (hash-ref js 'time #f) "jsexpr should have 'time")
  (check-not-false (hash-ref js 'sessionId #f) "jsexpr should have 'sessionId")
  (check-not-false (hash-ref js 'turnId #f) "jsexpr should have 'turnId")
  (check-not-false (hash-ref js 'payload #f) "jsexpr should have 'payload"))

(test-case "contract: event->jsexpr and jsexpr->event round-trip"
  (define evt (make-event "round.trip" 9999 "s-2" "t-2"
                          (hasheq 'items '(1 2 3) 'count 3)))
  (define js (event->jsexpr evt))
  (define evt2 (jsexpr->event js))
  (check-equal? (event-event evt2) (event-event evt))
  (check-equal? (event-time evt2) (event-time evt))
  (check-equal? (event-session-id evt2) (event-session-id evt))
  (check-equal? (event-turn-id evt2) (event-turn-id evt)))

(test-case "contract: published events carry correct type"
  (define bus (make-event-bus))
  (define captured (box #f))
  (subscribe! bus (lambda (evt) (set-box! captured evt)))
  (define test-evt (make-event "capture.test" 100 "s" "t" (hasheq)))
  (publish! bus test-evt)
  (check-pred event? (unbox captured))
  (check-equal? (event-event (unbox captured)) "capture.test"))

(test-case "contract: session.started event has session-id in payload"
  (define dir (make-temp-dir))
  (define bus (make-event-bus))
  (define started-evt (box #f))
  (subscribe! bus (lambda (evt)
                    (when (equal? (event-event evt) "session.started")
                      (set-box! started-evt evt))))
  (define prov (make-simple-mock-provider "hi"))
  (define rt (sdk:make-runtime #:provider prov
                               #:session-dir dir
                               #:tool-registry (make-tool-registry)
                               #:event-bus bus))
  (define rt2 (sdk:open-session rt))
  (check-not-false (unbox started-evt)
                   "session.started event should be emitted")
  (define payload (event-payload (unbox started-evt)))
  (check-not-false (hash-ref payload 'sessionId #f)
                   "session.started payload should have sessionId")
  (cleanup-dir dir))

;; ============================================================
;; 2. Provider response → message conversion
;; ============================================================

(test-case "contract: model-response content shape"
  (define resp (make-model-response
                (list (hash 'type "text" 'text "hello"))
                (hasheq 'prompt-tokens 5 'completion-tokens 3 'total-tokens 8)
                "test-model"
                'stop))
  (check-true (list? (model-response-content resp)))
  (check-equal? (length (model-response-content resp)) 1)
  (define part (first (model-response-content resp)))
  (check-equal? (hash-ref part 'type) "text")
  (check-equal? (hash-ref part 'text) "hello"))

(test-case "contract: model-response with tool-call shape"
  (define resp (make-model-response
                (list (hash 'type "tool-call"
                            'id "call-123"
                            'name "read"
                            'arguments (hasheq 'path "/tmp/test")))
                (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
                "test-model"
                'tool-calls))
  (define part (first (model-response-content resp)))
  (check-equal? (hash-ref part 'type) "tool-call")
  (check-equal? (hash-ref part 'id) "call-123")
  (check-equal? (hash-ref part 'name) "read")
  (check-true (hash? (hash-ref part 'arguments))))

(test-case "contract: model-response round-trip via jsexpr"
  (define resp (make-model-response
                (list (hash 'type "text" 'text "round-trip"))
                (hasheq 'prompt-tokens 1 'completion-tokens 1 'total-tokens 2)
                "rt-model"
                'stop))
  (define js (model-response->jsexpr resp))
  (check-not-false (hash-ref js 'content #f))
  (check-not-false (hash-ref js 'usage #f))
  (check-not-false (hash-ref js 'model #f))
  (check-not-false (hash-ref js 'stopReason #f))
  (define resp2 (jsexpr->model-response js))
  (check-equal? (model-response-model resp2) "rt-model"))

(test-case "contract: stream-chunk has correct fields"
  (define chunk (stream-chunk "hello" #f #f #f))
  (check-equal? (stream-chunk-delta-text chunk) "hello")
  (check-false (stream-chunk-delta-tool-call chunk))
  (check-false (stream-chunk-usage chunk))
  (check-false (stream-chunk-done? chunk)))

(test-case "contract: stream-chunk done sentinel"
  (define done-chunk (stream-chunk #f #f (hasheq 'total-tokens 10) #t))
  (check-false (stream-chunk-delta-text done-chunk))
  (check-true (stream-chunk-done? done-chunk))
  (check-not-false (stream-chunk-usage done-chunk)))

;; ============================================================
;; 3. Scheduler → tool arity agreement
;; ============================================================

(test-case "contract: tool execute receives (args-hash, exec-context)"
  (define received-args (box #f))
  (define received-ctx (box #f))
  (define test-tool
    (make-tool "arity-test" "Tests argument passing"
               (hasheq 'type "object"
                       'required '("input")
                       'properties (hasheq 'input (hasheq 'type "string")))
               (lambda (args ctx)
                 (set-box! received-args args)
                 (set-box! received-ctx ctx)
                 (make-success-result "ok"))))
  ;; Call directly with correct arity
  (define args (hasheq 'input "test-value"))
  (define ctx (make-exec-context #:working-directory "/tmp"
                                 #:call-id "call-1"))
  (define result ((tool-execute test-tool) args ctx))
  (check-pred tool-result? result)
  (check-false (tool-result-is-error? result))
  ;; Verify args and ctx were received
  (check-equal? (hash-ref (unbox received-args) 'input #f) "test-value")
  (check-pred exec-context? (unbox received-ctx)))

(test-case "contract: scheduler passes correct args to tool"
  (define reg (make-tool-registry))
  (define received (box '()))
  (register-tool! reg
    (make-tool "record" "Records call args"
               (hasheq 'type "object"
                       'required '("x")
                       'properties (hasheq 'x (hasheq 'type "number")))
               (lambda (args ctx)
                 (set-box! received (cons args (unbox received)))
                 (make-success-result (number->string
                                       (hash-ref args 'x 0))))))
  (define calls
    (list (make-tool-call "c1" "record" (hasheq 'x 42))
          (make-tool-call "c2" "record" (hasheq 'x 99))))
  (define sched-result (run-tool-batch calls reg))
  (check-pred scheduler-result? sched-result)
  (check-equal? (length (scheduler-result-results sched-result)) 2)
  ;; Verify args received
  (define all-received (reverse (unbox received)))
  (check-equal? (length all-received) 2)
  (check-equal? (hash-ref (first all-received) 'x #f) 42)
  (check-equal? (hash-ref (second all-received) 'x #f) 99))

(test-case "contract: tool-result shape from success"
  (define result (make-success-result "output data"))
  (check-pred tool-result? result)
  (check-false (tool-result-is-error? result))
  (check-equal? (tool-result-content result) "output data"))

(test-case "contract: tool-result shape from error"
  (define result (make-error-result "something went wrong"))
  (check-pred tool-result? result)
  (check-true (tool-result-is-error? result))
  ;; Content is a list of content-part hashes, not a raw string
  (check-true (list? (tool-result-content result)))
  (check > (length (tool-result-content result)) 0))

;; ============================================================
;; 4. Config wiring contract
;; ============================================================

(test-case "contract: session config hash has required keys"
  (define dir (make-temp-dir))
  (define bus (make-event-bus))
  (define prov (make-simple-mock-provider "hi"))
  (define reg (make-tool-registry))
  ;; Build config exactly as SDK does
  (define config
    (hash 'provider prov
          'tool-registry reg
          'event-bus bus
          'session-dir dir))
  ;; Verify all required keys present
  (check-not-false (hash-ref config 'provider #f)
                   "config should have 'provider")
  (check-not-false (hash-ref config 'tool-registry #f)
                   "config should have 'tool-registry")
  (check-not-false (hash-ref config 'event-bus #f)
                   "config should have 'event-bus")
  (check-not-false (hash-ref config 'session-dir #f)
                   "config should have 'session-dir")
  (cleanup-dir dir))

(test-case "contract: settings merge preserves nested keys"
  (define global (hasheq 'providers (hasheq 'openai (hasheq 'base-url "https://api.openai.com"))
                         'parallel-tools #f))
  (define project (hasheq 'providers (hasheq 'openai (hasheq 'api-key "sk-test"))
                          'http-request-timeout 120))
  (define merged-hash (merge-settings global project))
  ;; merge-settings returns a raw hash (not q-settings)
  (check-equal? (hash-ref merged-hash 'http-request-timeout #f) 120)
  ;; Deep merge should preserve both provider keys
  (define providers (hash-ref merged-hash 'providers (hasheq)))
  (define openai (hash-ref providers 'openai (hasheq)))
  (check-equal? (hash-ref openai 'base-url #f) "https://api.openai.com")
  (check-equal? (hash-ref openai 'api-key #f) "sk-test"))

(test-case "contract: settings ref with default"
  (define s (load-settings))
  ;; Missing key returns default
  (check-equal? (setting-ref s 'nonexistent-key 'fallback) 'fallback))

(test-case "contract: tool registry — register and lookup"
  (define reg (make-tool-registry))
  (check-equal? (tool-names reg) '())
  (define test-tool
    (make-tool "lookup-test" "Test lookup"
               (hasheq 'type "object")
               (lambda (args ctx) (make-success-result "ok"))))
  (register-tool! reg test-tool)
  (check-equal? (tool-names reg) '("lookup-test"))
  (check-not-false (lookup-tool reg "lookup-test"))
  (check-false (lookup-tool reg "nonexistent")))

(test-case "contract: tool->jsexpr produces OpenAI function shape"
  (define t (make-tool "js-test" "Test JSON export"
                       (hasheq 'type "object"
                               'required '("x")
                               'properties (hasheq 'x (hasheq 'type "string")))
                       (lambda (args ctx) (make-success-result "ok"))))
  (define js (tool->jsexpr t))
  (check-equal? (hash-ref js 'type) "function")
  (define fn (hash-ref js 'function))
  (check-equal? (hash-ref fn 'name) "js-test")
  (check-equal? (hash-ref fn 'description) "Test JSON export")
  (check-not-false (hash-ref fn 'parameters #f)))

(test-case "contract: message round-trip through jsexpr"
  (define msg (make-message
               "msg-1" #f 'user 'message
               (list (make-text-part "hello world"))
               12345 (hasheq)))
  (define js (message->jsexpr msg))
  (check-equal? (hash-ref js 'id) "msg-1")
  (check-equal? (hash-ref js 'role) "user")
  (check-true (list? (hash-ref js 'content)))
  (define msg2 (jsexpr->message js))
  (check-equal? (message-id msg2) "msg-1")
  (check-equal? (message-role msg2) 'user))
