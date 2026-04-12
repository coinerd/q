#lang racket

;; tests/workflows/tools/test-tool-read-workflow.rkt — Read-file workflow (#171)
;;
;; Tests the read-file workflow: agent reads a file via tool-call,
;; then explains the contents. Exercises the full SDK path:
;;   runtime → session → provider → tools → events → session log.

(require rackunit
         rackunit/text-ui
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
   "Tool-read workflow tests"

   ;; ────────────────────────────────────────────────────────
   ;; Test 1: agent reads file and explains it
   ;; ────────────────────────────────────────────────────────
   (test-case "wf-tool-read: agent reads file and explains content"
     ;; Provider: first returns tool-call for "read", then text explanation
     (define prov (make-scripted-provider
                   (list (tool-call-response "tc-1" "read"
                                             (hash 'path "src/hello.rkt"))
                         (text-response "The file is a Racket program that prints hello"))))

     ;; Register a mock "read" tool that returns canned file content
     (define reg (make-tool-registry))
     (register-tool! reg
       (make-tool "read"
                  "Read a file"
                  (hasheq 'type "object"
                          'properties (hasheq 'path (hasheq 'type "string"))
                          'required '(path))
                  (lambda (args ctx)
                    (make-success-result
                     (list (hasheq 'type "text"
                                   'text "(displayln \"hello\")"))))))

     ;; Run with a project containing src/hello.rkt
     (define wr (run-workflow prov "Read src/hello.rkt"
                              #:tools reg
                              #:files (list (cons "src/hello.rkt"
                                                  "#lang racket\n(displayln \"hello\")"))))

     ;; OUTCOME: loop completed (ran both turns: tool-call + final text)
     (define output (workflow-result-output wr))
     (check-pred loop-result? output)
     (check-equal? (loop-result-termination-reason output) 'completed
                   "expected completed after tool-call + text response")

     ;; DURABLE STATE: session log is valid
     (check-equal? (check-session-jsonl-valid (workflow-result-session-log wr)) #t)

     ;; DURABLE STATE: 1 user turn, at least 1 assistant turn
     (check-equal? (check-session-contains-turns (workflow-result-session-log wr)
                                                  #:user-turns 1
                                                  #:assistant-turns 2)
                   #t
                   "expected 1 user turn and 2 assistant turns (tool-call + text)")

     ;; SIDE-EFFECTS: tool sequence is ("read")
     (check-equal? (check-session-tool-sequence (workflow-result-session-log wr)
                                                 '("read"))
                   #t
                   "expected tool sequence (read)")

     ;; SIDE-EFFECTS: events include tool.call.started and tool.call.completed
     (define recorder (workflow-result-events wr))
     (define ev-names (event-names recorder))
     (check-not-false (member "tool.call.started" ev-names)
                      "expected tool.call.started in events")
     (check-not-false (member "tool.call.completed" ev-names)
                      "expected tool.call.completed in events")

     ;; BOUNDARY: output text references the file content
     (define entries (workflow-session-entries wr))
     (define assistant-entries (entries-with-role entries 'assistant))
     ;; The second assistant message should contain the explanation text
     (check-true (>= (length assistant-entries) 2)
                 "expected at least 2 assistant messages")
     (define last-asst (last assistant-entries))
     (define text-parts (filter text-part? (message-content last-asst)))
     (check-true (>= (length text-parts) 1)
                 "final assistant message should have text content")
     (define explanation (text-part-text (car text-parts)))
     (check-true (string-contains? explanation "hello")
                 (format "explanation should reference file content, got: ~a" explanation))

     ;; BOUNDARY: tree structure is valid
     (check-equal? (check-session-tree-structure (workflow-result-session-log wr)) #t)

     ;; Cleanup
     (cleanup-temp-project! (workflow-result-project-dir wr)
                            (workflow-result-session-dir wr)))

   ;; ────────────────────────────────────────────────────────
   ;; Test 2: read tool with missing path arg returns error gracefully
   ;; ────────────────────────────────────────────────────────
   (test-case "wf-tool-read: read tool handles empty path gracefully"
     (define prov (make-scripted-provider
                   (list (tool-call-response "tc-1" "read"
                                             (hash 'path ""))
                         (text-response "The file path was empty"))))

     (define reg (make-tool-registry))
     (register-tool! reg
       (make-tool "read"
                  "Read a file"
                  (hasheq 'type "object"
                          'properties (hasheq 'path (hasheq 'type "string"))
                          'required '(path))
                  (lambda (args ctx)
                    (define path (hash-ref args 'path ""))
                    (if (string=? path "")
                        (make-error-result "path argument is empty")
                        (make-success-result
                         (list (hasheq 'type "text" 'text "file content")))))))

     (define wr (run-workflow prov "Read nothing"
                              #:tools reg
                              #:files '()))

     ;; OUTCOME: loop completed (error is fed back, then model responds)
     (define output (workflow-result-output wr))
     (check-pred loop-result? output)
     (check-equal? (loop-result-termination-reason output) 'completed)

     ;; SIDE-EFFECTS: tool sequence is ("read") even though it errored
     (check-equal? (check-session-tool-sequence (workflow-result-session-log wr)
                                                 '("read"))
                   #t)

     ;; DURABLE STATE: session log is valid
     (check-equal? (check-session-jsonl-valid (workflow-result-session-log wr)) #t)

     ;; Cleanup
     (cleanup-temp-project! (workflow-result-project-dir wr)
                            (workflow-result-session-dir wr)))

   )) ;; end test-suite

;; ============================================================
;; Run
;; ============================================================

(run-tests suite)
