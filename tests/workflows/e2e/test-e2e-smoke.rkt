#lang racket

;; tests/workflows/e2e/test-e2e-smoke.rkt — E2E smoke tests
;;
;; Golden path smoke tests exercising the full SDK runtime.
;; Tests that the agent can handle basic prompts through the
;; complete runtime pipeline.

(require rackunit
         rackunit/text-ui
         "../fixtures/mock-provider.rkt"
         "../fixtures/workflow-runner.rkt"
         "../fixtures/session-assert.rkt"
         "../fixtures/temp-project.rkt"
         "../../../util/protocol-types.rkt"
         (only-in "../../../tools/tool.rkt"
                  make-tool-registry
                  register-tool!
                  make-tool
                  make-success-result))

;; ============================================================
;; Test suite
;; ============================================================

(define suite
  (test-suite
   "E2E smoke tests"

   ;; ────────────────────────────────────────────────────────
   ;; Test 1: agent responds to simple prompt
   ;; ────────────────────────────────────────────────────────
   (test-case "e2e-smoke: agent responds to simple prompt"
     ;; Provider scripts single text-response
     (define prov (make-scripted-provider
                   (list (text-response "I can help"))))

     ;; Run workflow with a simple prompt
     (define wr (run-workflow prov "Hello"))

     ;; OUTCOME: loop completed
     (define output (workflow-result-output wr))
     (check-pred loop-result? output)
     (check-equal? (loop-result-termination-reason output) 'completed
                   "expected completed after single text response")

     ;; DURABLE STATE: session log is valid
     (check-equal? (check-session-jsonl-valid (workflow-result-session-log wr)) #t)

     ;; DURABLE STATE: 1 user turn + 1 assistant turn
     (check-equal? (check-session-contains-turns (workflow-result-session-log wr)
                                                  #:user-turns 1
                                                  #:assistant-turns 1)
                   #t
                   "expected 1 user turn and 1 assistant turn")

     ;; Cleanup
     (cleanup-temp-project! (workflow-result-project-dir wr)
                            (workflow-result-session-dir wr)))

   ;; ────────────────────────────────────────────────────────
   ;; Test 2: agent uses tool then responds
   ;; ────────────────────────────────────────────────────────
   (test-case "e2e-smoke: agent uses tool then responds"
     ;; Provider scripts tool-call for "list" tool, then text-response
     (define prov (make-scripted-provider
                   (list (tool-call-response "tc-1" "list"
                                             (hash 'dir "."))
                         (text-response "I found the files"))))

     ;; Register mock "list" tool
     (define reg (make-tool-registry))
     (register-tool! reg
       (make-tool "list"
                  "List files in a directory"
                  (hasheq 'type "object"
                          'properties (hasheq 'dir (hasheq 'type "string"))
                          'required '(dir))
                  (lambda (args ctx)
                    (make-success-result
                     (list (hasheq 'type "text"
                                   'text "file1.rkt\nfile2.rkt"))))))

     ;; Run workflow
     (define wr (run-workflow prov "List files"
                              #:tools reg))

     ;; OUTCOME: loop completed
     (define output (workflow-result-output wr))
     (check-pred loop-result? output)
     (check-equal? (loop-result-termination-reason output) 'completed
                   "expected completed after tool-call + text response")

     ;; DURABLE STATE: session log is valid
     (check-equal? (check-session-jsonl-valid (workflow-result-session-log wr)) #t)

     ;; SIDE-EFFECTS: tool sequence is ("list")
     (check-equal? (check-session-tool-sequence (workflow-result-session-log wr)
                                                 '("list"))
                   #t
                   "expected tool sequence (list)")

     ;; DURABLE STATE: 2 assistant turns (tool-call + final text)
     (check-equal? (check-session-contains-turns (workflow-result-session-log wr)
                                                  #:assistant-turns 2)
                   #t
                   "expected 2 assistant turns (tool-call + text)")

     ;; Cleanup
     (cleanup-temp-project! (workflow-result-project-dir wr)
                            (workflow-result-session-dir wr)))

   ;; ────────────────────────────────────────────────────────
   ;; Test 3: multi-turn conversation maintains session
   ;; ────────────────────────────────────────────────────────
   (test-case "e2e-smoke: multi-turn conversation maintains session"
     ;; Provider scripts 2 text-responses (one per turn)
     (define prov (make-scripted-provider
                   (list (text-response "First response")
                         (text-response "Second response"))))

     ;; Run multi-turn workflow with 2 prompts
     (define wr (run-workflow-multi-turn prov
                                         (list "First prompt"
                                               "Second prompt")))

     ;; DURABLE STATE: session log is valid
     (check-equal? (check-session-jsonl-valid (workflow-result-session-log wr)) #t)

     ;; DURABLE STATE: at least 2 user turns
     (check-equal? (check-session-contains-turns (workflow-result-session-log wr)
                                                  #:user-turns 2)
                   #t
                   "expected at least 2 user turns in multi-turn session")

     ;; Cleanup
     (cleanup-temp-project! (workflow-result-project-dir wr)
                            (workflow-result-session-dir wr)))

   )) ;; end test-suite

;; ============================================================
;; Run
;; ============================================================

(run-tests suite)
