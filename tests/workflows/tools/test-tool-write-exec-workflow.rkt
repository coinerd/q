#lang racket

;; tests/workflows/tools/test-tool-write-exec-workflow.rkt — Multi-tool chaining (#172)
;;
;; THE KEY TEST: Tests multi-tool chaining — write file then execute.
;; Exercises the full multi-turn tool-call loop end-to-end:
;;   runtime → session → provider → tools → events → session log.
;; Proves that the iteration loop correctly handles multiple sequential
;; tool calls across turns.

(require rackunit
         rackunit/text-ui
         racket/file
         "../fixtures/mock-provider.rkt"
         "../fixtures/temp-project.rkt"
         "../fixtures/session-assert.rkt"
         "../fixtures/event-recorder.rkt"
         "../fixtures/workflow-runner.rkt"
         "../../../agent/types.rkt"
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
   "Tool-write+exec workflow tests"

   ;; ────────────────────────────────────────────────────────
   ;; Test 1: write file then execute — multi-tool chaining
   ;; ────────────────────────────────────────────────────────
   (test-case "wf-tool-write-exec: write file then run bash command"
     ;; Provider returns 3 responses in sequence: write tool-call, bash tool-call, text summary.
     (define prov (make-scripted-provider
                   (list (tool-call-response "tc-1" "write"
                                             (hash 'path "tests/test-hello.rkt"
                                                   'content "#lang racket\n(check-equal? 1 1)"))
                         (tool-call-response "tc-2" "bash"
                                             (hash 'command "raco test tests/test-hello.rkt"))
                         (text-response "Test written and executed successfully"))))

     ;; Register mock "write" and "bash" tools.
     ;; Use a box to capture where the write tool actually writes.
     (define write-dir-box (box #f))

     (define reg (make-tool-registry))
     (register-tool! reg
       (make-tool "write"
                  "Write a file"
                  (hasheq 'type "object"
                          'properties (hasheq 'path (hasheq 'type "string")
                                              'content (hasheq 'type "string"))
                          'required '(path content))
                  (lambda (args ctx)
                    (define path (hash-ref args 'path))
                    (define content (hash-ref args 'content))
                    ;; Use current-directory (set by agent loop / scheduler)
                    (define base (current-directory))
                    (set-box! write-dir-box base)
                    (define full-path (build-path base path))
                    (define-values (parent _name _must-be-dir?) (split-path full-path))
                    (when parent (make-directory* parent))
                    (call-with-output-file full-path
                      (lambda (out) (display content out))
                      #:exists 'replace)
                    (make-success-result
                     (format "Wrote ~a bytes to ~a"
                             (string-length content) path)))))

     (register-tool! reg
       (make-tool "bash"
                  "Execute a bash command"
                  (hasheq 'type "object"
                          'properties (hasheq 'command (hasheq 'type "string"))
                          'required '(command))
                  (lambda (args ctx)
                    (define cmd (hash-ref args 'command "echo noop"))
                    (make-success-result
                     (format "$ ~a\n1 test passed\n" cmd)))))

     ;; Run workflow with a project directory
     (define wr (run-workflow prov "Write a test file and run it"
                              #:tools reg
                              #:files (list (cons "tests/.gitkeep" ""))))

     ;; OUTCOME: loop completed (all 3 turns consumed)
     (define output (workflow-result-output wr))
     (check-pred loop-result? output)
     (check-equal? (loop-result-termination-reason output) 'completed
                   "expected completed after write + bash + text summary")

     ;; SIDE-EFFECTS: tool sequence is ("write" "bash")
     (check-equal? (check-session-tool-sequence (workflow-result-session-log wr)
                                                 '("write" "bash"))
                   #t
                   "expected tool sequence (write bash)")

     ;; DURABLE STATE: written file exists somewhere on disk
     ;; The write tool writes to (current-directory) at execution time.
     ;; Check the box for where it actually wrote.
     (define write-dir (unbox write-dir-box))
     (when write-dir
       (define written-file (build-path write-dir "tests/test-hello.rkt"))
       (check-true (file-exists? written-file)
                   (format "written file should exist at ~a" written-file))
       (when (file-exists? written-file)
         (define actual-content (file->string written-file))
         (check-true (string-contains? actual-content "check-equal? 1 1")
                     (format "written file should contain test content, got: ~a"
                             actual-content))))

     ;; DURABLE STATE: session log has 1 user turn and multiple assistant turns
     (check-equal? (check-session-jsonl-valid (workflow-result-session-log wr)) #t)

     ;; SIDE-EFFECTS: events include tool call events
     (define recorder (workflow-result-events wr))
     (define tc-started (events-of-type recorder "tool.call.started"))
     (define tc-completed (events-of-type recorder "tool.call.completed"))
     (check-true (>= (length tc-started) 2)
                 (format "expected 2+ tool.call.started, got ~a" (length tc-started)))
     (check-true (>= (length tc-completed) 2)
                 (format "expected 2+ tool.call.completed, got ~a" (length tc-completed)))

     ;; BOUNDARY: tree structure is valid
     (check-equal? (check-session-tree-structure (workflow-result-session-log wr)) #t)

     ;; Cleanup
     (cleanup-temp-project! (workflow-result-project-dir wr)
                            (workflow-result-session-dir wr)))

   ;; ────────────────────────────────────────────────────────
   ;; Test 2: write tool error propagates through loop
   ;; ────────────────────────────────────────────────────────
   (test-case "wf-tool-write-exec: write error is handled gracefully"
     (define prov (make-scripted-provider
                   (list (tool-call-response "tc-1" "write"
                                             (hash 'path "/forbidden/path.rkt"
                                                   'content "data"))
                         (text-response "I see the write failed"))))

     (define reg (make-tool-registry))
     (register-tool! reg
       (make-tool "write"
                  "Write a file"
                  (hasheq 'type "object"
                          'properties (hasheq 'path (hasheq 'type "string")
                                              'content (hasheq 'type "string"))
                          'required '(path content))
                  (lambda (args ctx)
                    (define path (hash-ref args 'path ""))
                    (if (string-prefix? path "/forbidden")
                        (make-error-result (format "Permission denied: ~a" path))
                        (make-success-result "ok")))))

     (define wr (run-workflow prov "Write to forbidden path"
                              #:tools reg
                              #:files '()))

     ;; OUTCOME: completed (error fed back to model, model responds)
     (define output (workflow-result-output wr))
     (check-pred loop-result? output)
     (check-equal? (loop-result-termination-reason output) 'completed)

     ;; SIDE-EFFECTS: at least one tool call was attempted
     (define recorder (workflow-result-events wr))
     (define tc-started (events-of-type recorder "tool.call.started"))
     (check-true (>= (length tc-started) 1)
                 "expected at least 1 tool.call.started")

     ;; DURABLE STATE: session log valid
     (check-equal? (check-session-jsonl-valid (workflow-result-session-log wr)) #t)

     ;; Cleanup
     (cleanup-temp-project! (workflow-result-project-dir wr)
                            (workflow-result-session-dir wr)))

   )) ;; end test-suite

;; ============================================================
;; Run
;; ============================================================

(run-tests suite)
