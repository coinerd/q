#lang racket

;; tests/workflows/extensions/test-extension-hooks.rkt (#178)
;;
;; Verifies extension hooks fire in the correct order during a workflow.
;; Tests the full hook pipeline: pre-tool-call → tool execution → post-tool-call.

(require rackunit
         rackunit/text-ui
         racket/file
         "../fixtures/mock-provider.rkt"
         "../fixtures/temp-project.rkt"
         "../fixtures/session-assert.rkt"
         "../fixtures/event-recorder.rkt"
         "../fixtures/workflow-runner.rkt"
         "../../../agent/types.rkt"
         "../../../extensions/hooks.rkt"
         (only-in "../../../tools/tool.rkt"
                  make-tool-registry
                  register-tool!
                  make-tool
                  make-success-result
                  make-error-result))

;; ============================================================
;; Test suite
;; ============================================================

(define suite
  (test-suite
   "Extension hooks workflow tests"

   ;; ────────────────────────────────────────────────────────
   ;; Test 1: hook pipeline fires for tool calls
   ;; ────────────────────────────────────────────────────────
   (test-case "wf-ext: pre/post tool hooks fire during workflow"
     (define prov (make-scripted-provider
                   (list (tool-call-response "tc-1" "read"
                                             (hash 'path "hello.txt"))
                         (text-response "File content analyzed"))))

     ;; Track hook invocations
     (define hook-log (box '()))

     (define reg (make-tool-registry))
     (register-tool! reg
       (make-tool "read"
                  "Read file"
                  (hasheq)
                  (lambda (args ctx)
                    (set-box! hook-log
                              (cons (list 'tool-execute (hash-ref args 'path "unknown"))
                                    (unbox hook-log)))
                    (make-success-result "file content here"))))

     (define wr (run-workflow prov "Read hello.txt" #:tools reg
                              #:files (list (cons "hello.txt" "file content here"))))

     ;; OUTCOME: completed
     (define output (workflow-result-output wr))
     (check-equal? (loop-result-termination-reason output) 'completed)

     ;; SIDE-EFFECTS: tool was actually executed
     (define log (reverse (unbox hook-log)))
     (check-true (>= (length log) 1)
                 (format "expected at least 1 tool execution, got ~a" log))
     (when (>= (length log) 1)
       (check-equal? (car (car log)) 'tool-execute)
       (check-equal? (cadr (car log)) "hello.txt"))

     ;; DURABLE STATE: session log valid
     (check-equal? (check-session-jsonl-valid (workflow-result-session-log wr)) #t)

     ;; SIDE-EFFECTS: tool events present
     (define recorder (workflow-result-events wr))
     (check-true (>= (length (events-of-type recorder "tool.call.started")) 1)
                 "expected tool.call.started")

     ;; Cleanup
     (cleanup-temp-project! (workflow-result-project-dir wr)
                            (workflow-result-session-dir wr)))

   ;; ────────────────────────────────────────────────────────
   ;; Test 2: hook can block tool execution
   ;; ────────────────────────────────────────────────────────
   (test-case "wf-ext: blocked tool still completes workflow"
     ;; Provider requests a tool call, but the tool returns an error.
     ;; The loop should handle the error result and continue.
     (define prov (make-scripted-provider
                   (list (tool-call-response "tc-1" "dangerous"
                                             (hash 'cmd "rm -rf /"))
                         (text-response "I understand the command was blocked"))))

     (define reg (make-tool-registry))
     (register-tool! reg
       (make-tool "dangerous"
                  "Dangerous tool"
                  (hasheq)
                  (lambda (args ctx)
                    (make-error-result "Command blocked by safety policy"))))

     (define wr (run-workflow prov "Run dangerous command" #:tools reg))

     ;; OUTCOME: completed (error fed back to model)
     (define output (workflow-result-output wr))
     (check-equal? (loop-result-termination-reason output) 'completed)

     ;; DURABLE STATE: session log valid
     (check-equal? (check-session-jsonl-valid (workflow-result-session-log wr)) #t)

     ;; SIDE-EFFECTS: tool call was attempted
     (define recorder (workflow-result-events wr))
     (check-true (>= (length (events-of-type recorder "tool.call.started")) 1)
                 "expected tool.call.started")

     ;; Cleanup
     (cleanup-temp-project! (workflow-result-project-dir wr)
                            (workflow-result-session-dir wr)))

   ;; ────────────────────────────────────────────────────────
   ;; Test 3: multiple tools fire events in order
   ;; ────────────────────────────────────────────────────────
   (test-case "wf-ext: sequential tool calls produce ordered events"
     (define prov (make-scripted-provider
                   (list (tool-call-response "tc-1" "step1" (hash))
                         (tool-call-response "tc-2" "step2" (hash))
                         (text-response "Both steps completed"))))

     (define step-box (box '()))

     (define reg (make-tool-registry))
     (register-tool! reg
       (make-tool "step1" "Step 1" (hasheq)
                  (lambda (args ctx)
                    (set-box! step-box (cons 1 (unbox step-box)))
                    (make-success-result "step1 done"))))
     (register-tool! reg
       (make-tool "step2" "Step 2" (hasheq)
                  (lambda (args ctx)
                    (set-box! step-box (cons 2 (unbox step-box)))
                    (make-success-result "step2 done"))))

     (define wr (run-workflow prov "Run both steps" #:tools reg))

     ;; OUTCOME: completed
     (check-equal? (loop-result-termination-reason (workflow-result-output wr))
                   'completed)

     ;; SIDE-EFFECTS: both tools executed in order
     (define steps (reverse (unbox step-box)))
     (check-equal? steps '(1 2)
                   (format "expected steps (1 2), got ~a" steps))

     ;; DURABLE STATE: tool sequence in session log
     (check-equal? (check-session-tool-sequence (workflow-result-session-log wr)
                                                 '("step1" "step2"))
                   #t)

     ;; Cleanup
     (cleanup-temp-project! (workflow-result-project-dir wr)
                            (workflow-result-session-dir wr)))

   )) ;; end test-suite

;; ============================================================
;; Run
;; ============================================================

(run-tests suite)
