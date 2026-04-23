#lang racket

;; tests/workflows/dogfood/test-dogfood-execution-analysis.rkt
;; v0.18.4 Wave 2: Dogfood execution + analysis workflow tests

(require rackunit
         rackunit/text-ui
         racket/file
         racket/port
         json
         "../../../scripts/run-dogfood-session.rkt"
         "../../../scripts/capture-regression.rkt")

;; ── Helpers ──

(define (with-temp-dir proc)
  (define dir (make-temporary-file "dogfood-exec-~a" 'directory))
  (with-handlers ([exn:fail? (lambda (e)
                               (when (directory-exists? dir)
                                 (delete-directory/files dir))
                               (raise e))])
    (begin0 (proc dir)
      (when (directory-exists? dir)
        (delete-directory/files dir)))))

(define (write-trace path entries)
  (call-with-output-file path
                         (lambda (out)
                           (for ([e (in-list entries)])
                             (displayln (jsexpr->string e) out)))))

;; ── Test Suite ──

(define test-dogfood-execution-analysis
  (test-suite "Dogfood Execution + Analysis Tests (v0.18.4 Wave 2)"

    ;; Test 1: Mock execution produces valid trace
    (test-case "mock execution produces task metadata"
      (with-temp-dir (lambda (dir)
                       (define task
                         (dogfood-task "trace-test"
                                       "Test trace"
                                       "Create hello.txt"
                                       '()
                                       ""
                                       5
                                       (hasheq 'files (hasheq "hello.txt" "Hello"))
                                       '("hello.txt")))
                       (define result-dir (run-dogfood-task/mock task dir))
                       (define meta-path (build-path result-dir "task-meta.json"))
                       (check-true (file-exists? meta-path))
                       (define meta (call-with-input-file meta-path read-json))
                       (check-equal? (hash-ref meta 'task) "trace-test")
                       (check-equal? (hash-ref meta 'mode) "mock"))))

    ;; Test 2: Baseline captures tool call count
    (test-case "baseline captures tool metrics"
      (with-temp-dir (lambda (dir)
                       (define trace-path (build-path dir "trace.jsonl"))
                       (write-trace trace-path
                                    (list (hasheq 'role "user" 'content "test")
                                          (hasheq 'role
                                                  "assistant"
                                                  'content
                                                  "working"
                                                  'tool_calls
                                                  (list (hasheq 'function (hasheq 'name "read"))
                                                        (hasheq 'function (hasheq 'name "write"))))
                                          (hasheq 'role "tool" 'content "ok")
                                          (hasheq 'role "tool" 'content "done")
                                          (hasheq 'role "assistant" 'content "finished")))

                       (define baseline-path (capture-baseline trace-path))
                       (define baseline (load-baseline baseline-path))
                       (check-equal? (hash-ref baseline 'tool_call_count) 2)
                       (check-equal? (hash-ref baseline 'iteration_count) 2)
                       (check-not-false (member "read" (hash-ref baseline 'unique_tools)))
                       (check-not-false (member "write" (hash-ref baseline 'unique_tools))))))

    ;; Test 3: Regression detection - pass case
    (test-case "regression passes when metrics are similar"
      (with-temp-dir (lambda (dir)
                       (define baseline-path (build-path dir "baseline.json"))
                       (call-with-output-file baseline-path
                                              (lambda (out)
                                                (write-json (hasheq 'tool_call_count
                                                                    2
                                                                    'iteration_count
                                                                    3
                                                                    'unique_tools
                                                                    '("read" "write")
                                                                    'outcome_length
                                                                    10)
                                                            out)))
                       (define trace-path (build-path dir "trace.jsonl"))
                       (write-trace trace-path
                                    (list (hasheq 'role "user" 'content "test")
                                          (hasheq 'role
                                                  "assistant"
                                                  'content
                                                  "w"
                                                  'tool_calls
                                                  (list (hasheq 'function (hasheq 'name "read"))))
                                          (hasheq 'role "tool" 'content "ok")
                                          (hasheq 'role
                                                  "assistant"
                                                  'content
                                                  "w"
                                                  'tool_calls
                                                  (list (hasheq 'function (hasheq 'name "write"))))
                                          (hasheq 'role "tool" 'content "ok")
                                          (hasheq 'role "assistant" 'content "done")))

                       (check-true (regression-passes? trace-path baseline-path 0.5)))))

    ;; Test 4: Regression detection - fail case
    (test-case "regression fails when tool calls increase too much"
      (with-temp-dir
       (lambda (dir)
         (define baseline-path (build-path dir "baseline.json"))
         (call-with-output-file
          baseline-path
          (lambda (out)
            (write-json
             (hasheq 'tool_call_count 1 'iteration_count 2 'unique_tools '("read") 'outcome_length 10)
             out)))

         (define trace-path (build-path dir "trace.jsonl"))
         ;; 5 tool calls vs baseline 1
         (write-trace trace-path
                      (list (hasheq 'role "user" 'content "test")
                            (hasheq 'role
                                    "assistant"
                                    'content
                                    "w"
                                    'tool_calls
                                    (for/list ([i (in-range 5)])
                                      (hasheq 'function (hasheq 'name "read"))))
                            (hasheq 'role "tool" 'content "ok")
                            (hasheq 'role "tool" 'content "ok")
                            (hasheq 'role "tool" 'content "ok")
                            (hasheq 'role "tool" 'content "ok")
                            (hasheq 'role "tool" 'content "ok")
                            (hasheq 'role "assistant" 'content "done")))

         (check-false (regression-passes? trace-path baseline-path 0.25)))))

    ;; Test 5: Multi-task comparison
    (test-case "multiple tasks produce comparable baselines"
      (with-temp-dir
       (lambda (dir)
         (define task-a (dogfood-task "task-a" "First" "Do A" '() "" 5 (hasheq) '()))
         (define task-b (dogfood-task "task-b" "Second" "Do B" '() "" 5 (hasheq) '()))

         (define dir-a (run-dogfood-task/mock task-a (build-path dir "a")))
         (define dir-b (run-dogfood-task/mock task-b (build-path dir "b")))

         (check-true (directory-exists? dir-a))
         (check-true (directory-exists? dir-b))

         (define meta-a (call-with-input-file (build-path dir-a "task-meta.json") read-json))
         (define meta-b (call-with-input-file (build-path dir-b "task-meta.json") read-json))

         (check-equal? (hash-ref meta-a 'task) "task-a")
         (check-equal? (hash-ref meta-b 'task) "task-b"))))

    ;; Test 6: Baseline comparison output
    (test-case "compare-to-baseline produces detailed output"
      (with-temp-dir
       (lambda (dir)
         (define baseline-path (build-path dir "baseline.json"))
         (call-with-output-file
          baseline-path
          (lambda (out)
            (write-json
             (hasheq 'tool_call_count 1 'iteration_count 2 'unique_tools '("read") 'outcome_length 10)
             out)))
         (define trace-path (build-path dir "trace.jsonl"))
         (write-trace trace-path
                      (list (hasheq 'role "user" 'content "test")
                            (hasheq 'role "assistant" 'content "done")))

         (define cmp (compare-to-baseline trace-path baseline-path))
         (check-true (hash? cmp))
         (check-true (hash-has-key? cmp 'tool_call_count))
         (check-true (hash-has-key? cmp 'iteration_count)))))))

(run-tests test-dogfood-execution-analysis)
