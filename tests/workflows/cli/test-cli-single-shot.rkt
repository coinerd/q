#lang racket

;; tests/workflows/cli/test-cli-single-shot.rkt — CLI single-shot workflow tests (#168)
;;
;; Tests that a single prompt through the full SDK path:
;;   runtime → session → provider → tools → events → session log
;; produces correct output, events, session log, and side-effects.

(require rackunit
         rackunit/text-ui
         "../fixtures/mock-provider.rkt"
         "../fixtures/temp-project.rkt"
         "../fixtures/session-assert.rkt"
         "../fixtures/event-recorder.rkt"
         "../fixtures/workflow-runner.rkt"
         "../../../agent/types.rkt"
         (prefix-in sdk: "../../../interfaces/sdk.rkt"))

;; ============================================================
;; Test suite
;; ============================================================

(define suite
  (test-suite
   "CLI single-shot workflow tests"

   ;; ────────────────────────────────────────────────────────
   ;; Test 1: single-shot prompt returns text and writes session log
   ;; ────────────────────────────────────────────────────────
   (test-case "wf-cli: single-shot prompt returns text and writes session log"
     ;; Create provider that returns fixed text
     (define prov (make-scripted-provider
                   (list (text-response "The file contains hello world"))))
     ;; Run through workflow-runner
     (define wr (run-workflow prov "Read the file src/hello.rkt"))

     ;; OUTCOME: loop-result with 'completed termination
     (define output (workflow-result-output wr))
     (check-pred loop-result? output)
     (check-equal? (loop-result-termination-reason output) 'completed)

     ;; DURABLE STATE: session log exists and is valid
     (check-equal? (check-session-jsonl-valid (workflow-result-session-log wr)) #t)

     ;; DURABLE STATE: has 1 user turn and 1 assistant turn
     (check-equal? (check-session-contains-turns (workflow-result-session-log wr)
                                                  #:user-turns 1
                                                  #:assistant-turns 1)
                   #t)

     ;; SIDE-EFFECTS: events include turn.started and turn.completed
     (define recorder (workflow-result-events wr))
     (check-true (>= (length (events-of-type recorder "turn.started")) 1)
                 "expected at least one turn.started event")
     (check-true (>= (length (events-of-type recorder "turn.completed")) 1)
                 "expected at least one turn.completed event")

     ;; Also check a contiguous subsequence that IS present
     (check-equal? (event-sequence-matches? recorder
                                             '("context.built" "model.request.started"))
                   #t)

     ;; BOUNDARY: tree structure is valid
     (check-equal? (check-session-tree-structure (workflow-result-session-log wr)) #t)

     ;; Cleanup
     (cleanup-temp-project! (workflow-result-project-dir wr)
                            (workflow-result-session-dir wr)))

   ;; ────────────────────────────────────────────────────────
   ;; Test 2: session-info returns correct data after single prompt
   ;; ────────────────────────────────────────────────────────
   (test-case "wf-cli: session-info returns correct data after single prompt"
     (define prov (make-scripted-provider
                   (list (text-response "42"))))
     (define wr (run-workflow prov "What is 6 * 7?"))

     ;; Get session info from the runtime in the workflow result
     (define rt (workflow-result-runtime wr))
     (define info (sdk:session-info rt))

     ;; OUTCOME: session-info is a hash with expected keys
     (check-pred hash? info)
     (check-equal? (hash-ref info 'history-length #f) 2) ; user + assistant

     ;; DURABLE STATE: session-id matches the one in workflow result
     (check-equal? (hash-ref info 'session-id #f) (workflow-result-session-id wr))

     ;; SIDE-EFFECTS: session is active
     (check-equal? (hash-ref info 'active? #f) #t)

     ;; BOUNDARY: model-name is present (even if default)
     (check-not-false (hash-has-key? info 'model-name))

     ;; Cleanup
     (cleanup-temp-project! (workflow-result-project-dir wr)
                            (workflow-result-session-dir wr)))

   ;; ────────────────────────────────────────────────────────
   ;; Test 3: event recorder captured model.stream.delta events
   ;; ────────────────────────────────────────────────────────
   (test-case "wf-cli: event recorder captured model.stream.delta events"
     (define prov (make-scripted-provider
                   (list (text-response "Hello from the model"))))
     (define wr (run-workflow prov "Say hello"))

     ;; OUTCOME: at least one model.stream.delta event was recorded
     (define recorder (workflow-result-events wr))
     (define delta-events (events-of-type recorder "model.stream.delta"))
     (check-true (>= (length delta-events) 1)
                 (format "expected >=1 model.stream.delta, got ~a" (length delta-events)))

     ;; SIDE-EFFECTS: delta events carry actual text content
     (for ([evt (in-list delta-events)])
       (define payload (event-payload evt))
       (check-true (hash? payload)
                   "model.stream.delta payload should be a hash")
       ;; Payload should contain either 'delta (text) or 'delta-tool-call
       (check-true (or (hash-has-key? payload 'delta)
                       (hash-has-key? payload 'delta-tool-call))
                   "model.stream.delta should have 'delta or 'delta-tool-call key"))

     ;; BOUNDARY: model.stream.completed was also emitted
     (check-true (>= (length (events-of-type recorder "model.stream.completed")) 1)
                 "expected at least one model.stream.completed event")

     ;; Cleanup
     (cleanup-temp-project! (workflow-result-project-dir wr)
                            (workflow-result-session-dir wr)))

   ;; ────────────────────────────────────────────────────────
   ;; Test 4: session log has assistant message with expected text
   ;; ────────────────────────────────────────────────────────
   (test-case "wf-cli: session log has assistant message with expected text"
     (define expected-text "The answer is 42")
     (define prov (make-scripted-provider
                   (list (text-response expected-text))))
     (define wr (run-workflow prov "What is the answer?"))

     ;; DURABLE STATE: session log contains the expected text
     (define entries (workflow-session-entries wr))
     (define assistant-entries (entries-with-role entries 'assistant))
     (check-equal? (length assistant-entries) 1)

     ;; Extract text from the assistant message's content parts
     (define asst-msg (car assistant-entries))
     (define text-parts (filter text-part? (message-content asst-msg)))
     (check-true (>= (length text-parts) 1)
                 "assistant message should have at least one text part")
     (define actual-text (text-part-text (car text-parts)))
     (check-equal? actual-text expected-text)

     ;; BOUNDARY: assistant message has a valid id and timestamp
     (check-not-false (message-id asst-msg))
     (check-true (exact-positive-integer? (message-timestamp asst-msg)))

     ;; Cleanup
     (cleanup-temp-project! (workflow-result-project-dir wr)
                            (workflow-result-session-dir wr)))

   )) ;; end test-suite

;; ============================================================
;; Run
;; ============================================================

(run-tests suite)
