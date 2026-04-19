#lang racket

;; tests/workflows/tools/test-tool-bash-workflow.rkt — Bash tool workflow test
;;
;; Tests the bash tool workflow: agent runs a bash command via tool-call,
;; then reports the result. Exercises the full SDK path:
;;   runtime → session → provider → tools → events → session log.

(require rackunit
         rackunit/text-ui
         "../fixtures/mock-provider.rkt"
         "../fixtures/temp-project.rkt"
         "../fixtures/session-assert.rkt"
         "../fixtures/event-recorder.rkt"
         "../fixtures/workflow-runner.rkt"
         "../../../util/protocol-types.rkt"
         (only-in "../../../tools/tool.rkt"
                  make-tool-registry
                  register-tool!
                  make-tool
                  tool?
                  tool-execute
                  make-success-result
                  make-error-result
                  make-exec-context
                  exec-context?
                  exec-context-runtime-settings
                  tool-result?
                  tool-result-is-error?)
         (only-in "../../../runtime/settings.rkt" q-settings?))

;; ============================================================
;; Mock bash tool handler
;; ============================================================

(define (make-mock-bash-handler)
  (lambda (args ctx)
    (define cmd (hash-ref args 'command ""))
    (cond
      [(string=? cmd "") (make-error-result "command argument is empty")]
      [else (make-success-result (list (hasheq 'type "text" 'text (format "[mock] ~a" cmd))))])))

(define (make-mock-bash-tool)
  (make-tool
   "bash"
   "Run a bash command"
   (hasheq 'type "object" 'properties (hasheq 'command (hasheq 'type "string")) 'required '(command))
   (make-mock-bash-handler)))

;; ============================================================
;; Test suite
;; ============================================================

(define suite
  (test-suite "Tool-bash workflow tests"

    ;; ────────────────────────────────────────────────────────
    ;; Test 1: agent runs bash echo and reports result
    ;; ────────────────────────────────────────────────────────
    (test-case "wf-tool-bash: agent runs bash echo and reports result"
      (define prov
        (make-scripted-provider (list (tool-call-response "tc-1" "bash" (hash 'command "echo hello"))
                                      (text-response "The bash command output: hello"))))

      (define reg (make-tool-registry))
      (register-tool! reg (make-mock-bash-tool))

      (define wr (run-workflow prov "Run echo hello" #:tools reg #:files '()))

      ;; OUTCOME: loop completed
      (define output (workflow-result-output wr))
      (check-pred loop-result? output)
      (check-equal? (loop-result-termination-reason output)
                    'completed
                    "expected completed after tool-call + text response")

      ;; DURABLE STATE: session log is valid
      (check-equal? (check-session-jsonl-valid (workflow-result-session-log wr)) #t)

      ;; DURABLE STATE: 1 user turn, 2 assistant turns
      (check-equal? (check-session-contains-turns (workflow-result-session-log wr)
                                                  #:user-turns 1
                                                  #:assistant-turns 2)
                    #t
                    "expected 1 user turn and 2 assistant turns")

      ;; SIDE-EFFECTS: tool sequence is ("bash")
      (check-equal? (check-session-tool-sequence (workflow-result-session-log wr) '("bash"))
                    #t
                    "expected tool sequence (bash)")

      ;; SIDE-EFFECTS: events include tool.call.started and tool.call.completed
      (define recorder (workflow-result-events wr))
      (define ev-names (event-names recorder))
      (check-not-false (member "tool.call.started" ev-names) "expected tool.call.started in events")
      (check-not-false (member "tool.call.completed" ev-names)
                       "expected tool.call.completed in events")

      ;; BOUNDARY: output text references the command output
      (let* ([entries (workflow-session-entries wr)]
             [asst-entries (entries-with-role entries 'assistant)])
        (check-true (>= (length asst-entries) 2) "expected at least 2 assistant messages")
        (define last-asst (last asst-entries))
        (define text-parts (filter text-part? (message-content last-asst)))
        (check-true (>= (length text-parts) 1) "final assistant message should have text content")
        (define explanation (text-part-text (car text-parts)))
        (check-true (string-contains? explanation "hello")
                    (format "explanation should reference output, got: ~a" explanation)))

      ;; Cleanup
      (cleanup-temp-project! (workflow-result-project-dir wr) (workflow-result-session-dir wr)))

    ;; ────────────────────────────────────────────────────────
    ;; Test 2: bash tool handles sandbox settings via exec-context
    ;; ────────────────────────────────────────────────────────
    (test-case "wf-tool-bash: bash tool handles sandbox settings via exec-context"
      (define prov
        (make-scripted-provider (list (tool-call-response "tc-1" "bash" (hash 'command "echo test"))
                                      (text-response "Command executed successfully"))))

      ;; Mock bash that validates exec-context-runtime-settings is a q-settings?
      (define settings-validated? (box #f))
      (define reg (make-tool-registry))
      (register-tool! reg
                      (make-tool "bash"
                                 "Run a bash command"
                                 (hasheq 'type
                                         "object"
                                         'properties
                                         (hasheq 'command (hasheq 'type "string"))
                                         'required
                                         '(command))
                                 (lambda (args ctx)
                                   (when (and ctx (exec-context? ctx))
                                     (define settings (exec-context-runtime-settings ctx))
                                     (when (q-settings? settings)
                                       (set-box! settings-validated? #t)))
                                   (make-success-result (list (hasheq 'type "text" 'text "test"))))))

      (define wr (run-workflow prov "Run echo test" #:tools reg #:files '()))

      ;; OUTCOME: loop completed
      (define output (workflow-result-output wr))
      (check-pred loop-result? output)
      (check-equal? (loop-result-termination-reason output)
                    'completed
                    "expected completed after settings validation")

      ;; ASSERT: settings were validated as q-settings?
      (check-true (unbox settings-validated?)
                  "bash tool should receive exec-context with q-settings?")

      ;; DURABLE STATE: session log is valid
      (check-equal? (check-session-jsonl-valid (workflow-result-session-log wr)) #t)

      ;; Cleanup
      (cleanup-temp-project! (workflow-result-project-dir wr) (workflow-result-session-dir wr)))

    ;; ────────────────────────────────────────────────────────
    ;; Test 3: bash tool works without exec-context (backward compat)
    ;; ────────────────────────────────────────────────────────
    (test-case "wf-tool-bash: bash tool works without exec-context (backward compat)"
      (define prov
        (make-scripted-provider (list (tool-call-response "tc-1" "bash" (hash 'command "echo compat"))
                                      (text-response "Backward compat works"))))

      (define reg (make-tool-registry))
      (register-tool! reg (make-mock-bash-tool))

      ;; Full workflow path succeeds
      (define wr (run-workflow prov "Run echo compat" #:tools reg #:files '()))

      (define output (workflow-result-output wr))
      (check-pred loop-result? output)
      (check-equal? (loop-result-termination-reason output)
                    'completed
                    "expected completed for backward compat bash")

      ;; Direct invocation with #f exec-context also succeeds
      (define bash-tool (make-mock-bash-tool))
      (define result-no-ctx ((tool-execute bash-tool) (hash 'command "echo compat") #f))
      (check-true (tool-result? result-no-ctx)
                  "bash tool should return tool-result even with #f exec-context")
      (check-false (tool-result-is-error? result-no-ctx)
                   "bash tool should succeed with #f exec-context")

      ;; Cleanup
      (cleanup-temp-project! (workflow-result-project-dir wr)
                             (workflow-result-session-dir wr))))) ;; end test-suite

;; ============================================================
;; Run
;; ============================================================

(run-tests suite)
