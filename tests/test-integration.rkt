#lang racket

;; q/tests/test-integration.rkt — end-to-end integration tests
;;
;; Exercises full flows through the SDK layer:
;;   runtime → session → provider → tools → events → session log
;; Uses mock providers exclusively. All file I/O in temp directories.

(require rackunit
         racket/generator
         rackunit/text-ui
         json
         "../llm/provider.rkt"
         (only-in "../llm/model.rkt"
                  make-model-response model-response-content
                  stream-chunk stream-chunk?
                  stream-chunk-delta-text stream-chunk-delta-tool-call
                  stream-chunk-usage stream-chunk-done?)
         (only-in "../agent/event-bus.rkt"
                  make-event-bus event-bus?
                  subscribe! unsubscribe! publish!)
         (only-in "../agent/types.rkt"
                  make-event event-event event?
                  message? message-id message-role message-content
                  make-message make-text-part
                  text-part? text-part-text
                  loop-result? loop-result-termination-reason
                  loop-result-messages loop-result-metadata
                  make-loop-result)
         (only-in "../tools/tool.rkt"
                  make-tool-registry register-tool! make-tool
                  tool-names tool?
                  make-exec-context
                  make-success-result make-error-result
                  tool-result? tool-result-content tool-result-is-error?)
         "../extensions/api.rkt"
         "../util/cancellation.rkt"
         "../util/jsonl.rkt"
         (prefix-in sdk: "../interfaces/sdk.rkt")
         (only-in "../runtime/compactor.rkt"
                  compaction-result?))

;; Import register-default-tools! from main.rkt (which wires all 9 tools)
(require (only-in "../main.rkt"
                  register-default-tools!
                  make-event-bus))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-temp-dir)
  (make-temporary-file "q-integ-~a" 'directory))

(define (cleanup-dir dir)
  (when (directory-exists? dir)
    (delete-directory/files dir)))

;; Mock provider that returns fixed text responses in sequence
(define (make-simple-mock-provider . texts)
  (define idx (box 0))
  (make-provider
   (lambda () "mock-integ")
   (lambda () (hash 'streaming #t 'token-counting #t))
   (lambda (req)
     (define i (unbox idx))
     (set-box! idx (add1 i))
     (define text (if (< i (length texts)) (list-ref texts i) "done"))
     (make-model-response
      (list (hash 'type "text" 'text text))
      (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
      "mock-model"
      'stop))
   (lambda (req)
     (define i (unbox idx))
     (set-box! idx (add1 i))
     (define text (if (< i (length texts)) (list-ref texts i) "done"))
     (list (stream-chunk text #f #f #f)
           (stream-chunk #f #f
                         (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
                         #t)))))

;; Mock provider: first call returns a tool-call via stream, second returns text
(define (make-tool-call-mock-provider tool-name tool-args response-text)
  (define call-count (box 0))
  (make-provider
   (lambda () "mock-tool-call")
   (lambda () (hash 'streaming #t 'token-counting #t))
   ;; Non-streaming send (not used by loop, but required by make-provider)
   (lambda (req)
     (set-box! call-count (add1 (unbox call-count)))
     (cond
       [(= (unbox call-count) 1)
        (make-model-response
         (list (hash 'type "tool-call"
                     'id "tc-mock-1"
                     'name tool-name
                     'arguments tool-args))
         (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
         "mock-model"
         'tool-calls)]
       [else
        (make-model-response
         (list (hash 'type "text" 'text response-text))
         (hasheq 'prompt-tokens 20 'completion-tokens 10 'total-tokens 30)
         "mock-model"
         'stop)]))
   ;; Streaming (used by loop.rkt)
   (lambda (req)
     (set-box! call-count (add1 (unbox call-count)))
     (cond
       [(<= (unbox call-count) 1)
        ;; First call: return tool-call delta via stream
        (list (stream-chunk #f
                            (hasheq 'id "tc-mock-1"
                                    'name tool-name
                                    'arguments (jsexpr->string tool-args))
                            #f #f)
              (stream-chunk #f #f
                            (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
                            #t))]
       [else
        ;; Subsequent calls: return text via stream
        (list (stream-chunk response-text #f #f #f)
              (stream-chunk #f #f
                            (hasheq 'prompt-tokens 20 'completion-tokens 10 'total-tokens 30)
                            #t))]))))

;; Build a minimal SDK runtime for testing
(define (make-test-runtime prov
                           #:session-dir [session-dir #f]
                           #:max-iterations [max-iter 10]
                           #:tool-registry [tool-reg #f]
                           #:cancellation-token [tok #f])
  (define dir (or session-dir (make-temp-dir)))
  (define reg (or tool-reg (make-tool-registry)))
  (sdk:make-runtime #:provider prov
                    #:session-dir dir
                    #:tool-registry reg
                    #:event-bus (make-event-bus)
                    #:max-iterations max-iter
                    #:cancellation-token tok))

;; ============================================================
;; 1. Full session lifecycle
;; ============================================================

(test-case "integ: create runtime, open session, run prompt"
  (define dir (make-temp-dir))
  (define prov (make-simple-mock-provider "Hello from mock!"))
  (define rt (make-test-runtime prov #:session-dir dir))
  (define rt2 (sdk:open-session rt))
  (check-true (sdk:runtime? rt2))
  (check-not-false (sdk:session-info rt2))
  (define-values (rt3 result) (sdk:run-prompt! rt2 "test prompt"))
  (check-true (sdk:runtime? rt3))
  (check-pred loop-result? result)
  (cleanup-dir dir))

(test-case "integ: session log file exists after prompt"
  (define dir (make-temp-dir))
  (define prov (make-simple-mock-provider "response"))
  (define rt (make-test-runtime prov #:session-dir dir))
  (define rt2 (sdk:open-session rt))
  (define-values (rt3 _result) (sdk:run-prompt! rt2 "hello"))
  (define info (sdk:session-info rt3))
  (check-not-false info)
  (define sid (hash-ref info 'session-id))
  (define log-path (build-path dir sid "session.jsonl"))
  (check-pred file-exists? log-path)
  (define entries (jsonl-read-all-valid log-path))
  (check >= (length entries) 2)
  ;; Verify content of entries: should have user then assistant
  (check-equal? (hash-ref (first entries) 'role) "user"
                "first entry should be user message")
  ;; Content in JSONL is a list of content-part hashes, not a raw string
  (check-true (list? (hash-ref (first entries) 'content #f))
              "user entry should have content list")
  (check-equal? (hash-ref (second entries) 'role) "assistant"
                "second entry should be assistant message")
  (cleanup-dir dir))

(test-case "integ: session-info returns correct metadata"
  (define dir (make-temp-dir))
  (define prov (make-simple-mock-provider "hi"))
  (define rt (make-test-runtime prov #:session-dir dir #:max-iterations 5))
  (define rt2 (sdk:open-session rt))
  (define info (sdk:session-info rt2))
  (check-equal? (hash-ref info 'history-length) 0)
  (check-equal? (hash-ref info 'max-iterations) 5)
  (check-true (hash-ref info 'active?))
  (cleanup-dir dir))

;; ============================================================
;; 2. Tool execution flow
;; ============================================================

(test-case "integ: tool call → scheduler executes → result appended"
  (define dir (make-temp-dir))
  (define reg (make-tool-registry))
  (register-tool! reg
    (make-tool "echo" "Echo tool"
               (hasheq 'type "object"
                       'required '("text")
                       'properties (hasheq 'text (hasheq 'type "string")))
               (lambda (args ctx)
                 (make-success-result (hash-ref args 'text "echo")))))
  (define prov (make-tool-call-mock-provider
                "echo" (hasheq 'text "hello world")
                "Got the echo result"))
  (define rt (make-test-runtime prov #:session-dir dir #:tool-registry reg))
  (define rt2 (sdk:open-session rt))
  (define-values (rt3 result) (sdk:run-prompt! rt2 "echo hello"))
  (check-equal? (loop-result-termination-reason result) 'completed)
  ;; Check log has user msg + assistant(tool-call) + tool-result + assistant(text)
  (define info (sdk:session-info rt3))
  (define sid (hash-ref info 'session-id))
  (define log-path (build-path dir sid "session.jsonl"))
  (define entries (jsonl-read-all-valid log-path))
  (check >= (length entries) 2)
  ;; Verify content: should contain user, assistant(tool-call), tool-result, assistant(text)
  (define roles (map (lambda (e) (hash-ref e 'role #f)) entries))
  (check-not-false (member "user" roles) "entries should include user role")
  (check-not-false (member "assistant" roles) "entries should include assistant role")
  (cleanup-dir dir))

(test-case "integ: unknown tool → error result"
  (define dir (make-temp-dir))
  (define reg (make-tool-registry))  ; empty registry
  (define prov (make-tool-call-mock-provider
                "nonexistent-tool" (hasheq)
                "recovered"))
  (define rt (make-test-runtime prov #:session-dir dir #:tool-registry reg))
  (define rt2 (sdk:open-session rt))
  (define-values (rt3 result) (sdk:run-prompt! rt2 "use bad tool"))
  (check-pred loop-result? result)
  (cleanup-dir dir))

;; ============================================================
;; 3. Event bus integration
;; ============================================================

(test-case "integ: events emitted during prompt"
  (define dir (make-temp-dir))
  (define prov (make-simple-mock-provider "event test"))
  (define bus (make-event-bus))
  (define reg (make-tool-registry))
  (define events-received (box '()))
  (subscribe! bus (lambda (evt)
                    (set-box! events-received
                              (append (unbox events-received)
                                      (list (event-event evt))))))
  (define rt (sdk:make-runtime #:provider prov
                               #:session-dir dir
                               #:tool-registry reg
                               #:event-bus bus))
  (define rt2 (sdk:open-session rt))
  (define-values (rt3 _result) (sdk:run-prompt! rt2 "test"))
  (define events (unbox events-received))
  (check-not-false (member "session.started" events))
  (check-not-false (member "turn.started" events))
  (check-not-false (member "turn.completed" events))
  (cleanup-dir dir))

(test-case "integ: multiple subscribers receive events"
  (define dir (make-temp-dir))
  (define prov (make-simple-mock-provider "multi sub"))
  (define bus (make-event-bus))
  (define reg (make-tool-registry))
  (define box-a (box 0))
  (define box-b (box 0))
  (subscribe! bus (lambda (evt) (set-box! box-a (add1 (unbox box-a)))))
  (subscribe! bus (lambda (evt) (set-box! box-b (add1 (unbox box-b)))))
  (define rt (sdk:make-runtime #:provider prov
                               #:session-dir dir
                               #:tool-registry reg
                               #:event-bus bus))
  (define rt2 (sdk:open-session rt))
  (define-values (rt3 _result) (sdk:run-prompt! rt2 "test"))
  (check > (unbox box-a) 0)
  (check > (unbox box-b) 0)
  (check-equal? (unbox box-a) (unbox box-b))
  (cleanup-dir dir))

(test-case "integ: unsubscribe stops events"
  (define dir (make-temp-dir))
  (define prov (make-simple-mock-provider "unsub"))
  (define bus (make-event-bus))
  (define reg (make-tool-registry))
  (define box-count (box 0))
  (define sub-id (subscribe! bus (lambda (evt)
                                   (set-box! box-count (add1 (unbox box-count))))))
  (unsubscribe! bus sub-id)
  (define rt (sdk:make-runtime #:provider prov
                               #:session-dir dir
                               #:tool-registry reg
                               #:event-bus bus))
  (define rt2 (sdk:open-session rt))
  (define-values (rt3 _result) (sdk:run-prompt! rt2 "test"))
  (check-equal? (unbox box-count) 0)
  (cleanup-dir dir))

;; ============================================================
;; 4. Session persistence and resume
;; ============================================================

(test-case "integ: resume session preserves history"
  (define dir (make-temp-dir))
  (define prov1 (make-simple-mock-provider "first response" "second response"))
  (define bus (make-event-bus))
  (define reg (make-tool-registry))
  (define rt1 (sdk:make-runtime #:provider prov1
                                #:session-dir dir
                                #:tool-registry reg
                                #:event-bus bus))
  (define rt2 (sdk:open-session rt1))
  (define info1 (sdk:session-info rt2))
  (define sid (hash-ref info1 'session-id))
  (sdk:run-prompt! rt2 "first prompt")

  ;; Resume with a new provider
  (define prov2 (make-simple-mock-provider "resumed response"))
  (define rt4 (sdk:make-runtime #:provider prov2
                                #:session-dir dir
                                #:tool-registry reg
                                #:event-bus bus))
  (define rt5 (sdk:open-session rt4 sid))
  (define info2 (sdk:session-info rt5))
  (check >= (hash-ref info2 'history-length) 2)
  (check-equal? (hash-ref info2 'session-id) sid)
  (cleanup-dir dir))

(test-case "integ: resume session → new prompt appends"
  (define dir (make-temp-dir))
  (define bus (make-event-bus))
  (define reg (make-tool-registry))
  (define prov1 (make-simple-mock-provider "first"))
  (define rt1 (sdk:make-runtime #:provider prov1
                                #:session-dir dir
                                #:tool-registry reg
                                #:event-bus bus))
  (define rt2 (sdk:open-session rt1))
  (define sid (hash-ref (sdk:session-info rt2) 'session-id))
  (sdk:run-prompt! rt2 "prompt 1")

  (define prov2 (make-simple-mock-provider "second"))
  (define rt3 (sdk:make-runtime #:provider prov2
                                #:session-dir dir
                                #:tool-registry reg
                                #:event-bus bus))
  (define rt4 (sdk:open-session rt3 sid))
  (sdk:run-prompt! rt4 "prompt 2")

  (define log-path (build-path dir sid "session.jsonl"))
  (define entries (jsonl-read-all-valid log-path))
  (check >= (length entries) 4)
  ;; Verify content: should have multiple user and assistant messages from both prompts
  (define roles (map (lambda (e) (hash-ref e 'role #f)) entries))
  (check-equal? (length (filter (lambda (r) (equal? r "user")) roles)) 2
                "should have 2 user messages from two prompts")
  (check-equal? (length (filter (lambda (r) (equal? r "assistant")) roles)) 2
                "should have 2 assistant messages from two prompts")
  (cleanup-dir dir))

(test-case "integ: resume non-existent session → error"
  (define dir (make-temp-dir))
  (define prov (make-simple-mock-provider "x"))
  (define rt (make-test-runtime prov #:session-dir dir))
  (check-exn exn:fail?
             (lambda () (sdk:open-session rt "nonexistent-session-id")))
  (cleanup-dir dir))

;; ============================================================
;; 5. Fork flow
;; ============================================================

(test-case "integ: fork session preserves entries"
  (define dir (make-temp-dir))
  (define prov (make-simple-mock-provider "fork test"))
  (define rt (make-test-runtime prov #:session-dir dir))
  (define rt2 (sdk:open-session rt))
  (sdk:run-prompt! rt2 "prompt before fork")
  (define forked (sdk:fork-session! rt2))
  (check-true (sdk:runtime? forked))
  (define orig-info (sdk:session-info rt2))
  (define fork-info (sdk:session-info forked))
  (check-equal? (hash-ref fork-info 'history-length)
                (hash-ref orig-info 'history-length))
  (cleanup-dir dir))

(test-case "integ: fork → new prompt on fork leaves original unchanged"
  (define dir (make-temp-dir))
  (define bus (make-event-bus))
  (define reg (make-tool-registry))
  (define prov (make-simple-mock-provider "original response" "fork response"))
  (define rt (sdk:make-runtime #:provider prov
                               #:session-dir dir
                               #:tool-registry reg
                               #:event-bus bus))
  (define rt2 (sdk:open-session rt))
  (sdk:run-prompt! rt2 "prompt 1")
  (define orig-history-len (hash-ref (sdk:session-info rt2) 'history-length))
  (define forked (sdk:fork-session! rt2))
  (sdk:run-prompt! forked "prompt on fork")
  (check-equal? (hash-ref (sdk:session-info rt2) 'history-length)
                orig-history-len)
  (check > (hash-ref (sdk:session-info forked) 'history-length)
         orig-history-len)
  (cleanup-dir dir))

;; ============================================================
;; 6. Compaction flow
;; ============================================================

(test-case "integ: advisory compaction returns result without modifying log"
  (define dir (make-temp-dir))
  (define prov (make-simple-mock-provider "compact test"))
  (define rt (make-test-runtime prov #:session-dir dir))
  (define rt2 (sdk:open-session rt))
  (sdk:run-prompt! rt2 "prompt")
  (define log-path (build-path dir
                               (hash-ref (sdk:session-info rt2) 'session-id)
                               "session.jsonl"))
  (define entries-before (length (jsonl-read-all-valid log-path)))
  (define-values (rt3 comp-result) (sdk:compact-session! rt2 #:persist? #f))
  (check-pred compaction-result? comp-result)
  (define entries-after (length (jsonl-read-all-valid log-path)))
  (check-equal? entries-before entries-after)
  (cleanup-dir dir))

(test-case "integ: persisting compaction appends summary entry"
  (define dir (make-temp-dir))
  (define prov (make-simple-mock-provider "persist test"))
  (define rt (make-test-runtime prov #:session-dir dir))
  (define rt2 (sdk:open-session rt))
  (sdk:run-prompt! rt2 "prompt for persist")
  (define log-path (build-path dir
                               (hash-ref (sdk:session-info rt2) 'session-id)
                               "session.jsonl"))
  (define entries-before (length (jsonl-read-all-valid log-path)))
  (define-values (rt3 comp-result) (sdk:compact-session! rt2 #:persist? #t))
  (check-pred compaction-result? comp-result)
  (define entries-after (length (jsonl-read-all-valid log-path)))
  (check >= entries-after entries-before)
  (cleanup-dir dir))

;; ============================================================
;; 7. Cancellation flow
;; ============================================================

(test-case "integ: cancelled token → prompt returns cancelled"
  (define dir (make-temp-dir))
  (define prov (make-simple-mock-provider "cancelled"))
  (define tok (make-cancellation-token))
  (cancel-token! tok)
  (define rt (make-test-runtime prov #:session-dir dir #:cancellation-token tok))
  (define rt2 (sdk:open-session rt))
  (define-values (rt3 result) (sdk:run-prompt! rt2 "should be cancelled"))
  (check-equal? (loop-result-termination-reason result) 'cancelled)
  (cleanup-dir dir))

(test-case "integ: cancellation emits turn.cancelled event"
  (define dir (make-temp-dir))
  (define prov (make-simple-mock-provider "cancel-event"))
  (define bus (make-event-bus))
  (define reg (make-tool-registry))
  (define tok (make-cancellation-token))
  (define events (box '()))
  (subscribe! bus (lambda (evt)
                    (set-box! events (append (unbox events)
                                             (list (event-event evt))))))
  (cancel-token! tok)
  (define rt (sdk:make-runtime #:provider prov
                               #:session-dir dir
                               #:tool-registry reg
                               #:event-bus bus
                               #:cancellation-token tok))
  (define rt2 (sdk:open-session rt))
  (sdk:run-prompt! rt2 "test")
  (check-not-false (member "turn.cancelled" (unbox events)))
  (cleanup-dir dir))

(test-case "integ: uncancelled token → runs normally"
  (define dir (make-temp-dir))
  (define prov (make-simple-mock-provider "normal run"))
  (define tok (make-cancellation-token))
  (define rt (make-test-runtime prov #:session-dir dir #:cancellation-token tok))
  (define rt2 (sdk:open-session rt))
  (define-values (rt3 result) (sdk:run-prompt! rt2 "normal"))
  (check-equal? (loop-result-termination-reason result) 'completed)
  (cleanup-dir dir))

;; ============================================================
;; 8. CLI / tool registration integration
;; ============================================================

(test-case "integ: register-default-tools! registers 9 tools"
  (define reg (make-tool-registry))
  (register-default-tools! reg)
  (define names (tool-names reg))
  (check-equal? (length names) 9)
  (check-not-false (member "read" names))
  (check-not-false (member "write" names))
  (check-not-false (member "edit" names))
  (check-not-false (member "bash" names))
  (check-not-false (member "grep" names))
  (check-not-false (member "find" names))
  (check-not-false (member "ls" names)))

(test-case "integ: register-default-tools! with #:only filter"
  (define reg (make-tool-registry))
  (register-default-tools! reg #:only '("read" "bash"))
  (check-equal? (sort (tool-names reg) string<?) '("bash" "read")))

(test-case "integ: register-default-tools! with empty #:only"
  (define reg (make-tool-registry))
  (register-default-tools! reg #:only '("nonexistent"))
  (check-equal? (tool-names reg) '()))

;; ============================================================
;; 9. Error / edge case handling
;; ============================================================

(test-case "integ: max-iterations=1 stops tool-call loop"
  (define dir (make-temp-dir))
  (define reg (make-tool-registry))
  (register-tool! reg
    (make-tool "ping" "ping tool"
               (hasheq 'type "object"
                       'required '()
                       'properties (hasheq))
               (lambda (args ctx) (make-success-result "pong"))))
  ;; Provider always returns tool-call via stream
  (define ever-calling-prov
    (make-provider
     (lambda () "always-tool-call")
     (lambda () (hash 'streaming #t 'token-counting #t))
     (lambda (req)
       (make-model-response
        (list (hash 'type "tool-call"
                    'id "tc-loop"
                    'name "ping"
                    'arguments (hasheq)))
        (hasheq 'prompt-tokens 5 'completion-tokens 3 'total-tokens 8)
        "mock"
        'tool-calls))
     (lambda (req)
       (list (stream-chunk
              #f
              (hasheq 'id "tc-loop"
                      'name "ping"
                      'arguments "{}")
              #f #f)
             (stream-chunk
              #f #f
              (hasheq 'prompt-tokens 5 'completion-tokens 3 'total-tokens 8)
              #t)))))
  (define rt (make-test-runtime ever-calling-prov
                                #:session-dir dir
                                #:tool-registry reg
                                #:max-iterations 1))
  (define rt2 (sdk:open-session rt))
  (define-values (rt3 result) (sdk:run-prompt! rt2 "loop"))
  (check-equal? (loop-result-termination-reason result) 'max-iterations-exceeded)
  (cleanup-dir dir))

(test-case "integ: interrupt! sets cancellation token"
  (define dir (make-temp-dir))
  (define prov (make-simple-mock-provider "before-interrupt"))
  (define tok (make-cancellation-token))
  (define rt (make-test-runtime prov #:session-dir dir #:cancellation-token tok))
  (define rt2 (sdk:open-session rt))
  (sdk:interrupt! rt2)
  (check-pred cancellation-token-cancelled? tok)
  (cleanup-dir dir))

(test-case "integ: run-prompt! without session returns no-active-session"
  (define dir (make-temp-dir))
  (define prov (make-simple-mock-provider "no session"))
  (define rt (make-test-runtime prov #:session-dir dir))
  (define-values (rt2 result) (sdk:run-prompt! rt "orphan"))
  (check-equal? result 'no-active-session))

(test-case "integ: session-info without session returns #f"
  (define dir (make-temp-dir))
  (define prov (make-simple-mock-provider "no info"))
  (define rt (make-test-runtime prov #:session-dir dir))
  (check-false (sdk:session-info rt))
  (cleanup-dir dir))

(test-case "integ: fork without session returns no-active-session"
  (define dir (make-temp-dir))
  (define prov (make-simple-mock-provider "no fork"))
  (define rt (make-test-runtime prov #:session-dir dir))
  (check-equal? (sdk:fork-session! rt) 'no-active-session)
  (cleanup-dir dir))

(test-case "integ: compact-session! without session returns no-active-session"
  (define dir (make-temp-dir))
  (define prov (make-simple-mock-provider "no compact"))
  (define rt (make-test-runtime prov #:session-dir dir))
  (check-equal? (sdk:compact-session! rt) 'no-active-session)
  (cleanup-dir dir))
