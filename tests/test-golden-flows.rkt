#lang racket

;; q/tests/test-golden-flows.rkt — end-to-end golden-path integration tests
;;
;; Validates complete flows across CLI parsing, JSON mode, SDK surface,
;; session lifecycle, event bus, fork, compaction, and cancellation.
;; All tests use mock providers — no real API calls.
;;
;; Addresses GitHub Issue #15.

(require rackunit
         racket/file
         racket/string
         racket/port
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
                  make-event event-event event-ev event?
                  event-payload
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
                  compaction-result?
                  compaction-result-removed-count
                  compaction-result-kept-messages
                  compaction-result->message-list)
         (only-in "../interfaces/cli.rkt"
                  parse-cli-args cli-config
                  cli-config-command cli-config-mode
                  cli-config-prompt cli-config-model
                  cli-config-session-id cli-config-max-turns
                  cli-config-no-tools? cli-config-tools
                  cli-config->runtime-config
                  parse-slash-command)
         (only-in "../interfaces/json-mode.rkt"
                  parse-json-intent intent-type intent-payload
                  intent? intent
                  event->json-line)
         (only-in "../runtime/session-store.rkt"
                  load-session-log))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-temp-dir)
  (make-temporary-file "q-golden-~a" 'directory))

(define (cleanup-dir dir)
  (when (directory-exists? dir)
    (delete-directory/files dir)))

;; Event collector: returns a handler proc and a getter for captured events
(define (make-event-collector)
  (define events (box '()))
  (values
   (lambda (evt)
     (set-box! events (append (unbox events) (list (event-event evt)))))
   (lambda () (unbox events))))

;; Mock provider that returns fixed text responses in sequence
(define (make-sequential-mock-provider . texts)
  (define idx (box 0))
  (make-provider
   (lambda () "mock-golden")
   (lambda () (hash 'streaming #t 'token-counting #t))
   (lambda (req)
     (define i (unbox idx))
     (set-box! idx (add1 i))
     (define text (if (< i (length texts)) (list-ref texts i) "done"))
     (make-model-response
      (list (hasheq 'type "text" 'text text))
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

;; Single-response mock provider (most common case)
(define (make-single-mock-provider [text "Mock response"])
  (apply make-sequential-mock-provider (list text)))

;; Build a minimal SDK runtime for golden tests
(define (make-golden-runtime prov
                              #:session-dir [session-dir #f]
                              #:max-iterations [max-iter 10]
                              #:tool-registry [tool-reg #f]
                              #:event-bus [bus #f]
                              #:cancellation-token [tok #f]
                              #:model-name [model-name #f]
                              #:system-instructions [instrs '()])
  (define dir (or session-dir (make-temp-dir)))
  (define reg (or tool-reg (make-tool-registry)))
  (sdk:make-runtime #:provider prov
                    #:session-dir dir
                    #:tool-registry reg
                    #:event-bus (or bus (make-event-bus))
                    #:max-iterations max-iter
                    #:cancellation-token tok
                    #:model-name model-name
                    #:system-instructions instrs))

;; ============================================================
;; 1. CLI argument parsing golden paths
;; ============================================================

(test-case "golden-cli: bare invocation → interactive chat mode"
  (define cfg (parse-cli-args '#()))
  (check-equal? (cli-config-command cfg) 'chat)
  (check-equal? (cli-config-mode cfg) 'interactive)
  (check-false (cli-config-prompt cfg))
  (check-false (cli-config-session-id cfg)))

(test-case "golden-cli: single prompt → single-shot mode"
  (define cfg (parse-cli-args '#("What is Racket?")))
  (check-equal? (cli-config-command cfg) 'prompt)
  (check-equal? (cli-config-mode cfg) 'single)
  (check-equal? (cli-config-prompt cfg) "What is Racket?"))

(test-case "golden-cli: --model flag parsed"
  (define cfg (parse-cli-args '#("--model" "gpt-4o" "hello")))
  (check-equal? (cli-config-model cfg) "gpt-4o")
  (check-equal? (cli-config-prompt cfg) "hello"))

(test-case "golden-cli: --session flag → resume mode"
  (define cfg (parse-cli-args '#("--session" "abc-123")))
  (check-equal? (cli-config-command cfg) 'resume)
  (check-equal? (cli-config-session-id cfg) "abc-123"))

(test-case "golden-cli: --json → json mode"
  (define cfg (parse-cli-args '#("--json")))
  (check-equal? (cli-config-mode cfg) 'json))

(test-case "golden-cli: --rpc → rpc mode"
  (define cfg (parse-cli-args '#("--rpc")))
  (check-equal? (cli-config-mode cfg) 'rpc))

(test-case "golden-cli: --tui → tui mode"
  (define cfg (parse-cli-args '#("--tui")))
  (check-equal? (cli-config-mode cfg) 'tui))

(test-case "golden-cli: --max-turns parsed"
  (define cfg (parse-cli-args '#("--max-turns" "5" "test")))
  (check-equal? (cli-config-max-turns cfg) 5))

(test-case "golden-cli: --no-tools flag"
  (define cfg (parse-cli-args '#("--no-tools" "do something")))
  (check-true (cli-config-no-tools? cfg)))

(test-case "golden-cli: --tool repeatable"
  (define cfg (parse-cli-args '#("--tool" "read" "--tool" "bash" "hello")))
  (check-equal? (sort (cli-config-tools cfg) string<?) '("bash" "read")))

(test-case "golden-cli: --help → help command"
  (define cfg (parse-cli-args '#("--help")))
  (check-equal? (cli-config-command cfg) 'help))

(test-case "golden-cli: --version → version command"
  (define cfg (parse-cli-args '#("--version")))
  (check-equal? (cli-config-command cfg) 'version))

(test-case "golden-cli: cli-config->runtime-config produces hash"
  (define cfg (parse-cli-args '#("--max-turns" "3" "--model" "test-model" "hi")))
  (define rt-cfg (cli-config->runtime-config cfg))
  (check-true (hash? rt-cfg))
  (check-equal? (hash-ref rt-cfg 'max-iterations) 3)
  (check-equal? (hash-ref rt-cfg 'model) "test-model"))

(test-case "golden-cli: slash-command parsing"
  (check-equal? (parse-slash-command "/help") '(help))
  (check-equal? (parse-slash-command "/quit") '(quit))
  (check-equal? (parse-slash-command "/exit") '(quit))
  (check-equal? (parse-slash-command "/compact") '(compact))
  (check-equal? (parse-slash-command "/history") '(history))
  (check-equal? (parse-slash-command "/fork abc") '(fork "abc"))
  (check-equal? (parse-slash-command "/fork") '(fork))
  (check-false (parse-slash-command "not a command"))
  (check-false (parse-slash-command "")))

;; ============================================================
;; 2. JSON mode intent parsing golden paths
;; ============================================================

(test-case "golden-json: parse prompt intent"
  (define intent-obj (parse-json-intent "{\"intent\":\"prompt\",\"text\":\"Hello\"}"))
  (check-true (intent? intent-obj))
  (check-equal? (intent-type intent-obj) 'prompt)
  (check-equal? (hash-ref (intent-payload intent-obj) 'text) "Hello"))

(test-case "golden-json: parse interrupt intent"
  (define intent-obj (parse-json-intent "{\"intent\":\"interrupt\"}"))
  (check-equal? (intent-type intent-obj) 'interrupt))

(test-case "golden-json: parse fork intent with entryId"
  (define intent-obj (parse-json-intent "{\"intent\":\"fork\",\"entryId\":\"msg-42\"}"))
  (check-equal? (intent-type intent-obj) 'fork)
  (check-equal? (hash-ref (intent-payload intent-obj) 'entryId) "msg-42"))

(test-case "golden-json: parse quit intent"
  (define intent-obj (parse-json-intent "{\"intent\":\"quit\"}"))
  (check-equal? (intent-type intent-obj) 'quit))

(test-case "golden-json: parse compact intent"
  (define intent-obj (parse-json-intent "{\"intent\":\"compact\"}"))
  (check-equal? (intent-type intent-obj) 'compact))

(test-case "golden-json: invalid JSON returns #f"
  (check-false (parse-json-intent "not json"))
  (check-false (parse-json-intent ""))
  (check-false (parse-json-intent "{\"wrong\":\"key\"}"))
  (check-false (parse-json-intent "{\"intent\":\"unknown\"}")))

(test-case "golden-json: event->json-line produces valid JSON with newline"
  (define test-evt (make-event "test.event" 1234567890 "sess-1" #f
                                (hasheq 'key "value")))
  (define json-line (event->json-line test-evt "sess-override"))
  (check-true (string-suffix? json-line "\n"))
  (define parsed (string->jsexpr (string-trim json-line)))
  (check-equal? (hash-ref parsed 'event) "test.event")
  (check-equal? (hash-ref parsed 'sessionId) "sess-override"))

;; ============================================================
;; 3. Session lifecycle golden path
;; ============================================================

(test-case "golden-session: full lifecycle — create, info, prompt, close"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define prov (make-single-mock-provider "Hello!"))
    (define rt (make-golden-runtime prov #:session-dir dir))
    ;; No session yet
    (check-false (sdk:session-info rt))
    ;; Open session
    (define rt2 (sdk:open-session rt))
    (check-true (sdk:runtime? rt2))
    (check-not-false (sdk:session-info rt2))
    ;; Session info has expected fields
    (define info (sdk:session-info rt2))
    (check-true (string? (hash-ref info 'session-id)))
    (check-true (hash-ref info 'active?))
    (check-equal? (hash-ref info 'history-length) 0)
    ;; Run prompt
    (define-values (rt3 result) (sdk:run-prompt! rt2 "Hello, world!"))
    (check-true (sdk:runtime? rt3))
    (check-true (loop-result? result))
    (check-equal? (loop-result-termination-reason result) 'completed)
    ;; History grew
    (define info3 (sdk:session-info rt3))
    (check-true (> (hash-ref info3 'history-length) 0))
    ;; Session log file exists and has valid entries
    (define sid (hash-ref info3 'session-id))
    (define log-path (build-path dir sid "session.jsonl"))
    (check-true (file-exists? log-path))
    (define entries (jsonl-read-all-valid log-path))
    (check-true (>= (length entries) 2) "log should have at least user + assistant messages")
    ;; Verify entry structure: first is user message
    (define first-entry (car entries))
    (check-equal? (hash-ref first-entry 'role) "user")
    (cleanup-dir dir)))

(test-case "golden-session: multiple prompts build history"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define prov (make-sequential-mock-provider "One" "Two" "Three"))
    (define rt (make-golden-runtime prov #:session-dir dir))
    (define rt2 (sdk:open-session rt))
    (define-values (rt3 _r1) (sdk:run-prompt! rt2 "first"))
    (define-values (rt4 _r2) (sdk:run-prompt! rt3 "second"))
    (define-values (rt5 _r3) (sdk:run-prompt! rt4 "third"))
    (define info (sdk:session-info rt5))
    ;; Should have at least 6 messages: 3 user + 3 assistant
    (check-true (>= (hash-ref info 'history-length) 6)
                "three round-trips should produce at least 6 messages")
    (cleanup-dir dir)))

(test-case "golden-session: resume preserves full history"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define bus (make-event-bus))
    (define prov1 (make-sequential-mock-provider "First reply" "Second reply"))
    (define reg (make-tool-registry))
    ;; Phase 1: create session, run two prompts
    (define rt1 (make-golden-runtime prov1 #:session-dir dir
                                     #:event-bus bus #:tool-registry reg))
    (define rt2 (sdk:open-session rt1))
    (define sid (hash-ref (sdk:session-info rt2) 'session-id))
    (define-values (rt3 _r1) (sdk:run-prompt! rt2 "prompt A"))
    (define-values (rt4 _r2) (sdk:run-prompt! rt3 "prompt B"))
    (define len-before (hash-ref (sdk:session-info rt4) 'history-length))
    ;; Phase 2: resume on fresh runtime
    (define prov2 (make-single-mock-provider "Resumed reply"))
    (define rt5 (make-golden-runtime prov2 #:session-dir dir
                                     #:event-bus bus #:tool-registry reg))
    (define rt6 (sdk:open-session rt5 sid))
    (define info-resumed (sdk:session-info rt6))
    (check-equal? (hash-ref info-resumed 'session-id) sid)
    (check-equal? (hash-ref info-resumed 'history-length) len-before)
    ;; Can run new prompt on resumed session
    (define-values (rt7 result) (sdk:run-prompt! rt6 "after resume"))
    (check-equal? (loop-result-termination-reason result) 'completed)
    (check-true (> (hash-ref (sdk:session-info rt7) 'history-length) len-before))
    (cleanup-dir dir)))

(test-case "golden-session: resume nonexistent session → error"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define prov (make-single-mock-provider))
    (define rt (make-golden-runtime prov #:session-dir dir))
    (check-exn exn:fail?
               (lambda () (sdk:open-session rt "no-such-session"))
               "resuming nonexistent session should raise")
    (cleanup-dir dir)))

;; ============================================================
;; 4. Session forking golden paths
;; ============================================================

(test-case "golden-fork: fork preserves history exactly"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define prov (make-sequential-mock-provider "Alpha" "Beta"))
    (define rt (make-golden-runtime prov #:session-dir dir))
    (define rt2 (sdk:open-session rt))
    (sdk:run-prompt! rt2 "build some history")
    (define orig-len (hash-ref (sdk:session-info rt2) 'history-length))
    (check-true (> orig-len 0) "should have history before fork")
    ;; Fork
    (define forked (sdk:fork-session! rt2))
    (check-true (sdk:runtime? forked))
    (define fork-info (sdk:session-info forked))
    (check-equal? (hash-ref fork-info 'history-length) orig-len
                  "fork should have same history as original")
    (cleanup-dir dir)))

(test-case "golden-fork: fork diverges — new prompt on fork leaves original unchanged"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define bus (make-event-bus))
    (define reg (make-tool-registry))
    (define prov (make-sequential-mock-provider "orig-1" "orig-2" "fork-only"))
    (define rt (make-golden-runtime prov #:session-dir dir
                                    #:event-bus bus #:tool-registry reg))
    (define rt2 (sdk:open-session rt))
    (sdk:run-prompt! rt2 "shared history")
    (define orig-len (hash-ref (sdk:session-info rt2) 'history-length))
    ;; Fork and add to fork only
    (define forked (sdk:fork-session! rt2))
    (define-values (fork-rt _) (sdk:run-prompt! forked "fork diverge"))
    ;; Original unchanged
    (check-equal? (hash-ref (sdk:session-info rt2) 'history-length) orig-len
                  "original session should not grow from fork activity")
    ;; Fork grew
    (check-true (> (hash-ref (sdk:session-info fork-rt) 'history-length) orig-len)
                "fork should have more history than original after divergence")
    (cleanup-dir dir)))

(test-case "golden-fork: forked session has different session-id"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define prov (make-single-mock-provider))
    (define rt (make-golden-runtime prov #:session-dir dir))
    (define rt2 (sdk:open-session rt))
    (sdk:run-prompt! rt2 "hello")
    (define orig-sid (hash-ref (sdk:session-info rt2) 'session-id))
    (define forked (sdk:fork-session! rt2))
    (define fork-sid (hash-ref (sdk:session-info forked) 'session-id))
    (check-not-equal? fork-sid orig-sid
                      "forked session must have a different ID")
    (cleanup-dir dir)))

(test-case "golden-fork: fork emits session.forked event"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define bus (make-event-bus))
    (define prov (make-single-mock-provider))
    (define-values (handler get-events) (make-event-collector))
    (subscribe! bus handler)
    (define rt (make-golden-runtime prov #:session-dir dir #:event-bus bus))
    (define rt2 (sdk:open-session rt))
    (sdk:run-prompt! rt2 "before fork")
    (define forked (sdk:fork-session! rt2))
    (define event-list (get-events))
    (check-not-false (member "session.forked" event-list)
                     "fork should emit session.forked event")
    (cleanup-dir dir)))

;; ============================================================
;; 5. Event bus flow golden paths
;; ============================================================

(test-case "golden-events: full event sequence for a prompt turn"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define bus (make-event-bus))
    (define reg (make-tool-registry))
    (define prov (make-single-mock-provider "eventful"))
    (define-values (handler get-events) (make-event-collector))
    (subscribe! bus handler)
    (define rt (make-golden-runtime prov #:session-dir dir
                                    #:event-bus bus #:tool-registry reg))
    (define rt2 (sdk:open-session rt))
    (define-values (rt3 _) (sdk:run-prompt! rt2 "trigger events"))
    (define evts (get-events))
    ;; Verify the golden event sequence
    (check-not-false (member "session.started" evts)
                     "session.started should be emitted")
    (check-not-false (member "turn.started" evts)
                     "turn.started should be emitted")
    (check-not-false (member "model.stream.delta" evts)
                     "model.stream.delta should be emitted during mock response")
    (check-not-false (member "model.stream.completed" evts)
                     "model.stream.completed should be emitted")
    (check-not-false (member "turn.completed" evts)
                     "turn.completed should be emitted")
    (check-not-false (member "session.updated" evts)
                     "session.updated should be emitted after turn")
    (cleanup-dir dir)))

(test-case "golden-events: subscriber receives all events in order"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define bus (make-event-bus))
    (define reg (make-tool-registry))
    (define prov (make-single-mock-provider "ordered"))
    (define-values (handler get-events) (make-event-collector))
    (subscribe! bus handler)
    (define rt (make-golden-runtime prov #:session-dir dir
                                    #:event-bus bus #:tool-registry reg))
    (define rt2 (sdk:open-session rt))
    (sdk:run-prompt! rt2 "order test")
    (define evts (get-events))
    ;; session.started should come before turn.started
    (define session-start-idx (index-of evts "session.started"))
    (define turn-start-idx (index-of evts "turn.started"))
    (define turn-end-idx (index-of evts "turn.completed"))
    (check-true (< session-start-idx turn-start-idx)
                "session.started should come before turn.started")
    (check-true (< turn-start-idx turn-end-idx)
                "turn.started should come before turn.completed")
    (cleanup-dir dir)))

(test-case "golden-events: filter excludes unwanted events"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define bus (make-event-bus))
    (define reg (make-tool-registry))
    (define prov (make-single-mock-provider "filtered"))
    (define filtered-events (box '()))
    ;; Only receive turn events
    (subscribe! bus
                (lambda (evt)
                  (set-box! filtered-events
                            (append (unbox filtered-events)
                                    (list (event-event evt)))))
                #:filter (lambda (evt)
                           (string-prefix? (event-event evt) "turn.")))
    (define rt (make-golden-runtime prov #:session-dir dir
                                    #:event-bus bus #:tool-registry reg))
    (define rt2 (sdk:open-session rt))
    (sdk:run-prompt! rt2 "filter test")
    (define evts (unbox filtered-events))
    ;; All received events should start with "turn."
    (for ([e (in-list evts)])
      (check-true (string-prefix? e "turn.")
                  (format "Expected turn.* event, got: ~a" e)))
    ;; But should have at least turn.started and turn.completed
    (check-not-false (member "turn.started" evts))
    (check-not-false (member "turn.completed" evts))
    (cleanup-dir dir)))

(test-case "golden-events: unsubscribe stops delivery"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define bus (make-event-bus))
    (define reg (make-tool-registry))
    (define prov (make-single-mock-provider "unsub"))
    (define events (box '()))
    (define sub-id
      (subscribe! bus
                  (lambda (evt)
                    (set-box! events
                              (append (unbox events)
                                      (list (event-event evt)))))))
    ;; Unsubscribe before any activity
    (unsubscribe! bus sub-id)
    (define rt (make-golden-runtime prov #:session-dir dir
                                    #:event-bus bus #:tool-registry reg))
    (define rt2 (sdk:open-session rt))
    (sdk:run-prompt! rt2 "unsub test")
    (check-equal? (unbox events) '()
                  "unsubscribed handler should receive no events")
    (cleanup-dir dir)))

(test-case "golden-events: multiple subscribers all receive events"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define bus (make-event-bus))
    (define reg (make-tool-registry))
    (define prov (make-single-mock-provider "multi"))
    (define box-a (box 0))
    (define box-b (box 0))
    (subscribe! bus (lambda (evt) (set-box! box-a (add1 (unbox box-a)))))
    (subscribe! bus (lambda (evt) (set-box! box-b (add1 (unbox box-b)))))
    (define rt (make-golden-runtime prov #:session-dir dir
                                    #:event-bus bus #:tool-registry reg))
    (define rt2 (sdk:open-session rt))
    (sdk:run-prompt! rt2 "multi test")
    (check-true (> (unbox box-a) 0) "subscriber A should receive events")
    (check-true (> (unbox box-b) 0) "subscriber B should receive events")
    (check-equal? (unbox box-a) (unbox box-b)
                  "both subscribers should receive same event count")
    (cleanup-dir dir)))

(test-case "golden-events: sdk subscribe-events! receives events"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define prov (make-single-mock-provider "sdk-sub"))
    (define-values (handler get-events) (make-event-collector))
    (define rt (make-golden-runtime prov #:session-dir dir))
    (define sub-id (sdk:subscribe-events! rt handler))
    (check-true (exact-nonnegative-integer? sub-id))
    (define rt2 (sdk:open-session rt))
    (sdk:run-prompt! rt2 "sdk sub test")
    (define evts (get-events))
    (check-not-false (member "session.started" evts))
    (check-not-false (member "turn.started" evts))
    (cleanup-dir dir)))

;; ============================================================
;; 6. Tool execution golden path
;; ============================================================

(test-case "golden-tools: tool call → execute → result in history"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define reg (make-tool-registry))
    (register-tool! reg
      (make-tool "greet" "Greet someone"
                 (hasheq 'type "object"
                         'required '("name")
                         'properties (hasheq 'name (hasheq 'type "string")))
                 (lambda (args ctx)
                   (make-success-result (format "Hello, ~a!" (hash-ref args 'name "world"))))))
    ;; Provider: first call returns tool-call, second returns text
    (define call-count (box 0))
    (define prov
      (make-provider
       (lambda () "mock-tool-golden")
       (lambda () (hash 'streaming #t 'token-counting #t))
       (lambda (req)
         (set-box! call-count (add1 (unbox call-count)))
         (if (= (unbox call-count) 1)
             (make-model-response
              (list (hasheq 'type "tool-call"
                            'id "tc-golden-1"
                            'name "greet"
                            'arguments (hasheq 'name "Alice")))
              (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
              "mock"
              'tool-calls)
             (make-model-response
              (list (hasheq 'type "text" 'text "Greeted Alice!"))
              (hasheq 'prompt-tokens 20 'completion-tokens 10 'total-tokens 30)
              "mock"
              'stop)))
       (lambda (req)
         (set-box! call-count (add1 (unbox call-count)))
         (if (<= (unbox call-count) 1)
             (list (stream-chunk #f
                                 (hasheq 'id "tc-golden-1"
                                         'name "greet"
                                         'arguments (jsexpr->string (hasheq 'name "Alice")))
                                 #f #f)
                   (stream-chunk #f #f
                                 (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
                                 #t))
             (list (stream-chunk "Greeted Alice!" #f #f #f)
                   (stream-chunk #f #f
                                 (hasheq 'prompt-tokens 20 'completion-tokens 10 'total-tokens 30)
                                 #t))))))
    (define rt (make-golden-runtime prov #:session-dir dir #:tool-registry reg))
    (define rt2 (sdk:open-session rt))
    (define-values (rt3 result) (sdk:run-prompt! rt2 "Say hi to Alice"))
    (check-equal? (loop-result-termination-reason result) 'completed)
    ;; Log should have: user, assistant(tool-call), tool-result, assistant(text)
    (define sid (hash-ref (sdk:session-info rt3) 'session-id))
    (define log-path (build-path dir sid "session.jsonl"))
    (define entries (jsonl-read-all-valid log-path))
    (check-true (>= (length entries) 4)
                "tool call flow should produce >= 4 log entries")
    (cleanup-dir dir)))

(test-case "golden-tools: unknown tool → error result, loop continues"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define reg (make-tool-registry))  ;; empty — no tools registered
    (define call-count (box 0))
    (define prov
      (make-provider
       (lambda () "mock-unknown-tool")
       (lambda () (hash 'streaming #t 'token-counting #t))
       (lambda (req)
         (set-box! call-count (add1 (unbox call-count)))
         (if (= (unbox call-count) 1)
             (make-model-response
              (list (hasheq 'type "tool-call"
                            'id "tc-unk"
                            'name "nonexistent"
                            'arguments (hasheq)))
              (hasheq 'prompt-tokens 5 'completion-tokens 3 'total-tokens 8)
              "mock"
              'tool-calls)
             (make-model-response
              (list (hasheq 'type "text" 'text "Handled the error"))
              (hasheq 'prompt-tokens 5 'completion-tokens 3 'total-tokens 8)
              "mock"
              'stop)))
       (lambda (req)
         (set-box! call-count (add1 (unbox call-count)))
         (if (<= (unbox call-count) 1)
             (list (stream-chunk #f
                                 (hasheq 'id "tc-unk"
                                         'name "nonexistent"
                                         'arguments "{}")
                                 #f #f)
                   (stream-chunk #f #f
                                 (hasheq 'prompt-tokens 5 'completion-tokens 3 'total-tokens 8)
                                 #t))
             (list (stream-chunk "Handled the error" #f #f #f)
                   (stream-chunk #f #f
                                 (hasheq 'prompt-tokens 5 'completion-tokens 3 'total-tokens 8)
                                 #t))))))
    (define rt (make-golden-runtime prov #:session-dir dir #:tool-registry reg))
    (define rt2 (sdk:open-session rt))
    (define-values (rt3 result) (sdk:run-prompt! rt2 "use unknown tool"))
    (check-true (loop-result? result))
    (check-equal? (loop-result-termination-reason result) 'completed)
    (cleanup-dir dir)))

;; ============================================================
;; 7. Compaction golden paths
;; ============================================================

(test-case "golden-compaction: advisory compact returns result without modifying log"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define prov (make-single-mock-provider "compact-me"))
    (define rt (make-golden-runtime prov #:session-dir dir))
    (define rt2 (sdk:open-session rt))
    (sdk:run-prompt! rt2 "build history")
    (define sid (hash-ref (sdk:session-info rt2) 'session-id))
    (define log-path (build-path dir sid "session.jsonl"))
    (define entries-before (length (jsonl-read-all-valid log-path)))
    ;; Advisory compact (default)
    (define-values (rt3 comp-res) (sdk:compact-session! rt2 #:persist? #f))
    (check-true (compaction-result? comp-res))
    ;; Log should NOT have changed
    (define entries-after (length (jsonl-read-all-valid log-path)))
    (check-equal? entries-after entries-before
                  "advisory compaction must not modify the log")
    (cleanup-dir dir)))

(test-case "golden-compaction: persist compact appends summary entry"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define prov (make-single-mock-provider "persist-me"))
    (define rt (make-golden-runtime prov #:session-dir dir))
    (define rt2 (sdk:open-session rt))
    ;; Build enough history for compaction to actually remove something
    (for ([i (in-range 25)])
      (define-values (next-rt _) (sdk:run-prompt! rt2 (format "msg ~a" i)))
      (set! rt2 next-rt))
    (define sid (hash-ref (sdk:session-info rt2) 'session-id))
    (define log-path (build-path dir sid "session.jsonl"))
    (define entries-before (length (jsonl-read-all-valid log-path)))
    ;; Persist compact
    (define-values (rt3 comp-res) (sdk:compact-session! rt2 #:persist? #t))
    (check-true (compaction-result? comp-res))
    (check-true (> (compaction-result-removed-count comp-res) 0)
                "should remove some messages with 25+ entries")
    ;; Log should have a new compaction-summary entry
    (define entries-after (jsonl-read-all-valid log-path))
    (check-true (> (length entries-after) entries-before)
                "persist compaction should add summary entry")
    (define has-summary?
      (for/or ([e (in-list entries-after)])
        (equal? (hash-ref e 'kind #f) "compaction-summary")))
    (check-true has-summary? "log should contain compaction-summary entry")
    (cleanup-dir dir)))

(test-case "golden-compaction: compact-session! without session returns 'no-active-session"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define prov (make-single-mock-provider))
    (define rt (make-golden-runtime prov #:session-dir dir))
    (check-equal? (sdk:compact-session! rt) 'no-active-session)
    (cleanup-dir dir)))

;; ============================================================
;; 8. Cancellation golden paths
;; ============================================================

(test-case "golden-cancel: pre-cancelled token → immediate cancellation"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define prov (make-single-mock-provider "cancelled"))
    (define tok (make-cancellation-token))
    (cancel-token! tok)
    (define rt (make-golden-runtime prov #:session-dir dir #:cancellation-token tok))
    (define rt2 (sdk:open-session rt))
    (define-values (rt3 result) (sdk:run-prompt! rt2 "should not run"))
    (check-equal? (loop-result-termination-reason result) 'cancelled)
    (cleanup-dir dir)))

(test-case "golden-cancel: cancel emits turn.cancelled event"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define bus (make-event-bus))
    (define reg (make-tool-registry))
    (define prov (make-single-mock-provider "cancel-event"))
    (define tok (make-cancellation-token))
    (cancel-token! tok)
    (define-values (handler get-events) (make-event-collector))
    (subscribe! bus handler)
    (define rt (make-golden-runtime prov #:session-dir dir
                                    #:event-bus bus #:tool-registry reg
                                    #:cancellation-token tok))
    (define rt2 (sdk:open-session rt))
    (sdk:run-prompt! rt2 "test")
    (check-not-false (member "turn.cancelled" (get-events))
                     "cancelled prompt should emit turn.cancelled")
    (cleanup-dir dir)))

(test-case "golden-cancel: interrupt! cancels runtime token"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define prov (make-single-mock-provider))
    (define rt (make-golden-runtime prov #:session-dir dir))
    (define rt2 (sdk:open-session rt))
    (check-false (cancellation-token-cancelled? (sdk:runtime-rt-cancellation-token rt2)))
    (sdk:interrupt! rt2)
    (check-true (cancellation-token-cancelled? (sdk:runtime-rt-cancellation-token rt2))
                "interrupt! should cancel the token")
    (cleanup-dir dir)))

(test-case "golden-cancel: uncancelled token → normal completion"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define prov (make-single-mock-provider "normal"))
    (define tok (make-cancellation-token))
    (define rt (make-golden-runtime prov #:session-dir dir #:cancellation-token tok))
    (define rt2 (sdk:open-session rt))
    (define-values (rt3 result) (sdk:run-prompt! rt2 "normal run"))
    (check-equal? (loop-result-termination-reason result) 'completed)
    (check-false (cancellation-token-cancelled? tok))
    (cleanup-dir dir)))

;; ============================================================
;; 9. Error and edge-case golden paths
;; ============================================================

(test-case "golden-error: run-prompt! without session returns 'no-active-session"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define prov (make-single-mock-provider))
    (define rt (make-golden-runtime prov #:session-dir dir))
    (define-values (rt2 result) (sdk:run-prompt! rt "orphan prompt"))
    (check-equal? result 'no-active-session)
    (cleanup-dir dir)))

(test-case "golden-error: fork without session returns 'no-active-session"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define prov (make-single-mock-provider))
    (define rt (make-golden-runtime prov #:session-dir dir))
    (check-equal? (sdk:fork-session! rt) 'no-active-session)
    (cleanup-dir dir)))

(test-case "golden-error: session-info without session returns #f"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define prov (make-single-mock-provider))
    (define rt (make-golden-runtime prov #:session-dir dir))
    (check-false (sdk:session-info rt))
    (cleanup-dir dir)))

(test-case "golden-error: max-iterations=1 stops after first tool call"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define reg (make-tool-registry))
    (register-tool! reg
      (make-tool "ping" "ping"
                 (hasheq 'type "object"
                         'required '()
                         'properties (hasheq))
                 (lambda (args ctx) (make-success-result "pong"))))
    ;; Provider always returns a tool-call
    (define prov
      (make-provider
       (lambda () "always-tool")
       (lambda () (hash 'streaming #t 'token-counting #t))
       (lambda (req)
         (make-model-response
          (list (hasheq 'type "tool-call"
                        'id "tc-loop"
                        'name "ping"
                        'arguments (hasheq)))
          (hasheq 'prompt-tokens 5 'completion-tokens 3 'total-tokens 8)
          "mock"
          'tool-calls))
       (lambda (req)
         (list (stream-chunk #f
                             (hasheq 'id "tc-loop"
                                     'name "ping"
                                     'arguments "{}")
                             #f #f)
               (stream-chunk #f #f
                             (hasheq 'prompt-tokens 5 'completion-tokens 3 'total-tokens 8)
                             #t)))))
    (define rt (make-golden-runtime prov #:session-dir dir
                                    #:tool-registry reg
                                    #:max-iterations 1))
    (define rt2 (sdk:open-session rt))
    (define-values (rt3 result) (sdk:run-prompt! rt2 "loop test"))
    (check-equal? (loop-result-termination-reason result) 'max-iterations-exceeded)
    (cleanup-dir dir)))

;; ============================================================
;; 10. System instructions golden path
;; ============================================================

(test-case "golden-system: system instructions stored and reflected in session-info"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define prov (make-single-mock-provider))
    (define instrs '("You are a helpful assistant." "Be concise."))
    (define rt (make-golden-runtime prov #:session-dir dir
                                    #:system-instructions instrs))
    (define rt2 (sdk:open-session rt))
    (define info (sdk:session-info rt2))
    (check-equal? (hash-ref info 'system-instructions) instrs)
    ;; Run a prompt — should not crash with system instructions
    (define-values (rt3 result) (sdk:run-prompt! rt2 "test with instructions"))
    (check-equal? (loop-result-termination-reason result) 'completed)
    (cleanup-dir dir)))

;; ============================================================
;; 11. Model name golden path
;; ============================================================

(test-case "golden-model: model-name stored in session-info"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define prov (make-single-mock-provider))
    (define rt (make-golden-runtime prov #:session-dir dir
                                    #:model-name "test-model-v1"))
    (define rt2 (sdk:open-session rt))
    (define info (sdk:session-info rt2))
    (check-equal? (hash-ref info 'model-name) "test-model-v1")
    (cleanup-dir dir)))

;; ============================================================
;; 12. Immutability golden paths
;; ============================================================

(test-case "golden-immutability: open-session does not mutate original"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define prov (make-single-mock-provider))
    (define rt (make-golden-runtime prov #:session-dir dir))
    (check-false (sdk:runtime-rt-session rt))
    (define rt2 (sdk:open-session rt))
    ;; Original still has no session
    (check-false (sdk:runtime-rt-session rt))
    ;; New one has session
    (check-not-false (sdk:runtime-rt-session rt2))
    (cleanup-dir dir)))

(test-case "golden-immutability: fork returns new runtime, original unchanged"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define prov (make-single-mock-provider))
    (define rt (make-golden-runtime prov #:session-dir dir))
    (define rt2 (sdk:open-session rt))
    (sdk:run-prompt! rt2 "shared")
    (define orig-sid (hash-ref (sdk:session-info rt2) 'session-id))
    (define forked (sdk:fork-session! rt2))
    ;; Original unchanged
    (check-equal? (hash-ref (sdk:session-info rt2) 'session-id) orig-sid)
    ;; Fork has different ID
    (check-not-equal? (hash-ref (sdk:session-info forked) 'session-id) orig-sid)
    (cleanup-dir dir)))

;; ============================================================
;; 13. Session log integrity golden path
;; ============================================================

(test-case "golden-log: all log entries are valid JSON"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define prov (make-sequential-mock-provider "one" "two"))
    (define rt (make-golden-runtime prov #:session-dir dir))
    (define rt2 (sdk:open-session rt))
    (sdk:run-prompt! rt2 "prompt 1")
    (sdk:run-prompt! rt2 "prompt 2")
    (define sid (hash-ref (sdk:session-info rt2) 'session-id))
    (define log-path (build-path dir sid "session.jsonl"))
    (define entries (jsonl-read-all-valid log-path))
    (check-true (>= (length entries) 4))
    ;; Every entry must have required fields
    (for ([e (in-list entries)])
      (check-true (hash-has-key? e 'id) "entry must have 'id")
      (check-true (hash-has-key? e 'role) "entry must have 'role"))
    (cleanup-dir dir)))

(test-case "golden-log: log entries have correct roles in sequence"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define prov (make-single-mock-provider "reply"))
    (define rt (make-golden-runtime prov #:session-dir dir))
    (define rt2 (sdk:open-session rt))
    (sdk:run-prompt! rt2 "hello")
    (define sid (hash-ref (sdk:session-info rt2) 'session-id))
    (define log-path (build-path dir sid "session.jsonl"))
    (define entries (jsonl-read-all-valid log-path))
    (define roles (map (lambda (e) (hash-ref e 'role)) entries))
    ;; First should be user, second should be assistant
    (check-equal? (car roles) "user")
    (check-not-false (member "assistant" roles)
                     "log should contain an assistant message")
    (cleanup-dir dir)))

;; ============================================================
;; 14. Cross-session isolation golden path
;; ============================================================

(test-case "golden-isolation: two sessions on same runtime have different IDs"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e) (cleanup-dir dir) (raise e))])
    (define prov (make-single-mock-provider))
    (define rt1 (make-golden-runtime prov #:session-dir dir))
    (define rt2 (sdk:open-session rt1))
    (define rt3 (sdk:open-session rt1))
    (define sid2 (hash-ref (sdk:session-info rt2) 'session-id))
    (define sid3 (hash-ref (sdk:session-info rt3) 'session-id))
    (check-not-equal? sid2 sid3
                      "two sessions must have different IDs")
    ;; Each should be independent
    (sdk:run-prompt! rt2 "session A message")
    (sdk:run-prompt! rt3 "session B message")
    (define log-a (build-path dir sid2 "session.jsonl"))
    (define log-b (build-path dir sid3 "session.jsonl"))
    (check-true (file-exists? log-a))
    (check-true (file-exists? log-b))
    (check-true (>= (length (jsonl-read-all-valid log-a)) 2))
    (check-true (>= (length (jsonl-read-all-valid log-b)) 2))
    (cleanup-dir dir)))
