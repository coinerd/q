#lang racket

;; tests/workflows/tools/test-tool-edit-workflow.rkt — Edit workflow (#173)
;;
;; Tests the edit-file workflow: agent edits file content via tool-call.
;; Exercises the full SDK path with a tool that reads, transforms, and
;; writes back: runtime → session → provider → tools → events → session log.

(require rackunit
         rackunit/text-ui
         racket/file
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
                  make-success-result
                  make-error-result))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-edit-tool)
  (make-tool "edit"
             "Edit a file by replacing text"
             (hasheq 'type "object"
                     'properties (hasheq 'path (hasheq 'type "string")
                                         'old-text (hasheq 'type "string")
                                         'new-text (hasheq 'type "string"))
                     'required '(path old-text new-text))
             (lambda (args ctx)
               (define path (hash-ref args 'path))
               (define old-text (hash-ref args 'old-text))
               (define new-text (hash-ref args 'new-text))
               (define base (current-directory))
               (define full-path (build-path base path))
               (cond
                 [(not (file-exists? full-path))
                  (make-error-result (format "File not found: ~a" path))]
                 [else
                  (define content (file->string full-path))
                  (cond
                    [(not (string-contains? content old-text))
                     (make-error-result
                      (format "Old text '~a' not found in ~a" old-text path))]
                    [else
                     (define replaced (string-replace content old-text new-text))
                     (call-with-output-file full-path
                       (lambda (out) (display replaced out))
                       #:exists 'replace)
                     (make-success-result
                      (format "Replaced '~a' with '~a' in ~a"
                              old-text new-text path))])]))))

;; ============================================================
;; Test suite
;; ============================================================

(define suite
  (test-suite
   "Tool-edit workflow tests"

   ;; ────────────────────────────────────────────────────────
   ;; Test 1: agent edits file content (port 8080 → 9090)
   ;; ────────────────────────────────────────────────────────
   (test-case "wf-tool-edit: agent edits config file content"
     (define prov (make-scripted-provider
                   (list (tool-call-response "tc-1" "edit"
                                             (hash 'path "src/config.rkt"
                                                   'old-text "8080"
                                                   'new-text "9090"))
                         (text-response "Updated the port from 8080 to 9090"))))

     (define reg (make-tool-registry))
     (register-tool! reg (make-edit-tool))

     (define wr (run-workflow prov "Change the port to 9090"
                              #:tools reg
                              #:files (list (cons "src/config.rkt"
                                                  "(define port 8080)"))))

     ;; OUTCOME: completed
     (define output (workflow-result-output wr))
     (check-pred loop-result? output)
     (check-equal? (loop-result-termination-reason output) 'completed
                   "expected completed after edit + text response")

     ;; DURABLE STATE: file content changed from 8080 to 9090
     (define project-dir (workflow-result-project-dir wr))
     (when project-dir
       (define config-file (build-path project-dir "src/config.rkt"))
       (when (file-exists? config-file)
         (define new-content (file->string config-file))
         (check-true (string-contains? new-content "9090")
                     (format "file should contain 9090, got: ~a" new-content))
         (check-false (string-contains? new-content "8080")
                      (format "file should NOT contain 8080, got: ~a" new-content))))

     ;; SIDE-EFFECTS: tool sequence is ("edit")
     (check-equal? (check-session-tool-sequence (workflow-result-session-log wr)
                                                 '("edit"))
                   #t
                   "expected tool sequence (edit)")

     ;; DURABLE STATE: session log valid
     (check-equal? (check-session-jsonl-valid (workflow-result-session-log wr)) #t)

     ;; SIDE-EFFECTS: tool call events present
     (define recorder (workflow-result-events wr))
     (check-true (>= (length (events-of-type recorder "tool.call.started")) 1)
                 "expected tool.call.started")
     (check-true (>= (length (events-of-type recorder "tool.call.completed")) 1)
                 "expected tool.call.completed")

     ;; BOUNDARY: tree structure valid
     (check-equal? (check-session-tree-structure (workflow-result-session-log wr)) #t)

     ;; Cleanup
     (cleanup-temp-project! (workflow-result-project-dir wr)
                            (workflow-result-session-dir wr)))

   ;; ────────────────────────────────────────────────────────
   ;; Test 2: edit tool on non-existent file returns error
   ;; ────────────────────────────────────────────────────────
   (test-case "wf-tool-edit: edit on missing file returns error gracefully"
     (define prov (make-scripted-provider
                   (list (tool-call-response "tc-1" "edit"
                                             (hash 'path "nonexistent.rkt"
                                                   'old-text "foo"
                                                   'new-text "bar"))
                         (text-response "The file does not exist"))))

     (define reg (make-tool-registry))
     (register-tool! reg (make-edit-tool))

     (define wr (run-workflow prov "Edit the missing file"
                              #:tools reg
                              #:files '()))

     ;; OUTCOME: completed (error returned, model responds)
     (define output (workflow-result-output wr))
     (check-pred loop-result? output)
     (check-equal? (loop-result-termination-reason output) 'completed)

     ;; SIDE-EFFECTS: tool sequence is ("edit")
     (check-equal? (check-session-tool-sequence (workflow-result-session-log wr)
                                                 '("edit"))
                   #t)

     ;; SIDE-EFFECTS: at least a tool.call.started
     (define recorder (workflow-result-events wr))
     (check-true (>= (length (events-of-type recorder "tool.call.started")) 1)
                 "expected tool.call.started")

     ;; DURABLE STATE: session log valid
     (check-equal? (check-session-jsonl-valid (workflow-result-session-log wr)) #t)

     ;; Cleanup
     (cleanup-temp-project! (workflow-result-project-dir wr)
                            (workflow-result-session-dir wr)))

   ;; ────────────────────────────────────────────────────────
   ;; Test 3: edit with old-text not found returns error result
   ;; ────────────────────────────────────────────────────────
   (test-case "wf-tool-edit: old-text not in file returns error"
     (define prov (make-scripted-provider
                   (list (tool-call-response "tc-1" "edit"
                                             (hash 'path "src/data.rkt"
                                                   'old-text "nonexistent-string"
                                                   'new-text "replacement"))
                         (text-response "The text was not found in the file"))))

     (define reg (make-tool-registry))
     (register-tool! reg (make-edit-tool))

     (define wr (run-workflow prov "Replace nonexistent text"
                              #:tools reg
                              #:files (list (cons "src/data.rkt" "(define x 42)"))))

     ;; OUTCOME: completed
     (define output (workflow-result-output wr))
     (check-pred loop-result? output)
     (check-equal? (loop-result-termination-reason output) 'completed)

     ;; DURABLE STATE: file content unchanged
     (define project-dir (workflow-result-project-dir wr))
     (when project-dir
       (define data-file (build-path project-dir "src/data.rkt"))
       (when (file-exists? data-file)
         (define content (file->string data-file))
         (check-equal? content "(define x 42)"
                       "file should be unchanged when old-text not found")))

     ;; SIDE-EFFECTS: tool call events present
     (define recorder (workflow-result-events wr))
     (check-true (>= (length (events-of-type recorder "tool.call.started")) 1)
                 "expected tool.call.started")

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
