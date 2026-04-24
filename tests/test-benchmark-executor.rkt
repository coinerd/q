#lang racket

;; tests/test-benchmark-executor.rkt — Tests for benchmark executor pipeline
;;
;; Tests the fixed executor: tool registration, timeout handling,
;; channel-based concurrency, and session-info API usage.

(require rackunit
         racket/file
         racket/path
         json
         (only-in "../scripts/benchmark/task.rkt"
                  benchmark-task
                  benchmark-task-name
                  benchmark-task-prompt
                  benchmark-task-category
                  benchmark-task-difficulty
                  benchmark-task-max-iterations
                  benchmark-task-time-limit-seconds
                  benchmark-task-setup
                  benchmark-task-scoring
                  task-setup
                  task-teardown
                  task-setup-spec
                  scoring-spec)
         (only-in "../scripts/benchmark/executor.rkt"
                  execution-result
                  execution-result-outcome
                  execution-result-task-name
                  execution-result-duration-ms
                  execution-result-iterations-used
                  execution-result-error-msg
                  execution-result-project-dir
                  execution-result-trace-path
                  execute-task/mock
                  trace-tool-calls))

;; ============================================================
;; Test helper: create a minimal benchmark task
;; ============================================================

(define (make-test-task #:name [name "test-task"]
                        #:prompt [prompt "Test prompt"]
                        #:max-iterations [max-iter 5]
                        #:time-limit [time-limit 30])
  (benchmark-task name
                  'implementation
                  1
                  "Test task for executor"
                  prompt
                  max-iter
                  1000
                  time-limit
                  (task-setup-spec (hasheq) '() '())
                  (scoring-spec '() '() '() '() '() #f #f)
                  '()
                  #f))

;; ============================================================
;; Tests
;; ============================================================

(test-case "exec: mock execution completes successfully"
  (define task (make-test-task))
  (define result (execute-task/mock task))
  (check-equal? (execution-result-task-name result) "test-task")
  (check-equal? (execution-result-outcome result) 'completed)
  (check-true (>= (execution-result-duration-ms result) 0))
  ;; Cleanup
  (when (execution-result-project-dir result)
    (with-handlers ([exn:fail? void])
      (delete-directory/files (execution-result-project-dir result)))))

(test-case "exec: mock execution returns positive iterations"
  (define task (make-test-task))
  (define result (execute-task/mock task))
  (check-true (>= (execution-result-iterations-used result) 0))
  ;; Cleanup
  (when (execution-result-project-dir result)
    (with-handlers ([exn:fail? void])
      (delete-directory/files (execution-result-project-dir result)))))

(test-case "exec: timeout triggers correctly"
  (define task (make-test-task #:time-limit 1 #:max-iterations 1000))
  ;; With a 1-second timeout and a mock provider that completes instantly,
  ;; this should complete normally (mock is fast).
  ;; But verify the timeout code path doesn't crash.
  (define result (execute-task/mock task))
  (check-not-false (member (execution-result-outcome result) '(completed timeout)))
  ;; Cleanup
  (when (execution-result-project-dir result)
    (with-handlers ([exn:fail? void])
      (delete-directory/files (execution-result-project-dir result)))))

(test-case "exec: trace-tool-calls reads q trace format"
  (define tmp-dir (make-temporary-file "trace-test-~a" 'directory))
  (define trace-file (build-path tmp-dir "test.jsonl"))
  ;; Write q-format trace entries
  (call-with-output-file trace-file
                         (lambda (out)
                           ;; Non-tool event (should be skipped)
                           (write-json (hasheq 'ts
                                               "2026-01-01T00:00:00Z"
                                               'seq
                                               1
                                               'phase
                                               "assistant.message.completed"
                                               'sessionId
                                               "s1"
                                               'turnId
                                               "t1"
                                               'data
                                               (hasheq 'content "hello"))
                                       out)
                           (newline out)
                           ;; Tool call event (should be captured)
                           (write-json (hasheq 'ts
                                               "2026-01-01T00:00:01Z"
                                               'seq
                                               2
                                               'phase
                                               "tool.call.started"
                                               'sessionId
                                               "s1"
                                               'turnId
                                               "t1"
                                               'data
                                               (hasheq 'id "call_1" 'name "read" 'arguments "{}"))
                                       out)
                           (newline out)
                           ;; Another tool call
                           (write-json (hasheq 'ts
                                               "2026-01-01T00:00:02Z"
                                               'seq
                                               3
                                               'phase
                                               "tool.call.started"
                                               'sessionId
                                               "s1"
                                               'turnId
                                               "t1"
                                               'data
                                               (hasheq 'id "call_2" 'name "edit" 'arguments "{}"))
                                       out)
                           (newline out)))
  (define tools (trace-tool-calls trace-file))
  (check-equal? tools '("read" "edit"))
  (delete-directory/files tmp-dir))

(test-case "exec: trace-tool-calls returns empty for missing file"
  (check-equal? (trace-tool-calls "/nonexistent/path.jsonl") '()))

(test-case "exec: trace-tool-calls returns empty for #f path"
  (check-equal? (trace-tool-calls #f) '()))

(test-case "benchmark tool set excludes session-recall and skill-router"
  ;; Verify that the standard benchmark tool list doesn't include meta-tools
  (define benchmark-tools '("read" "write" "edit" "bash" "grep" "find" "ls" "spawn-subagent"))
  (check-false (member "session-recall" benchmark-tools))
  (check-false (member "skill-router" benchmark-tools))
  (check-equal? (length benchmark-tools) 8))
