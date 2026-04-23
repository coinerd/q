#lang racket

;; tests/workflows/dogfood/test-dogfood-infrastructure.rkt
;; v0.18.4 Wave 1: Dogfood infrastructure tests

(require rackunit
         rackunit/text-ui
         racket/file
         racket/port
         json
         "../../../scripts/run-dogfood-session.rkt"
         "../../../scripts/capture-regression.rkt")

;; ── Helpers ──

(define (with-temp-dir proc)
  (define dir (make-temporary-file "dogfood-test-~a" 'directory))
  (with-handlers ([exn:fail? (lambda (e)
                               (when (directory-exists? dir)
                                 (delete-directory/files dir))
                               (raise e))])
    (begin0 (proc dir)
      (when (directory-exists? dir)
        (delete-directory/files dir)))))

(define (write-task-json dir name spec)
  (define path (build-path dir (format "~a.json" name)))
  (call-with-output-file path (lambda (out) (write-json spec out)))
  path)

;; ── Test Suite ──

(define test-dogfood-infrastructure
  (test-suite "Dogfood Infrastructure Tests (v0.18.4 Wave 1)"

    (test-case "load-dogfood-task loads valid JSON"
      (with-temp-dir
       (lambda (dir)
         (define path
           (write-task-json dir
                            "valid"
                            (hasheq 'name "test-task" 'prompt "Do something" 'max_iterations 5)))
         (define task (load-dogfood-task path))
         (check-true (dogfood-task? task))
         (check-equal? (dogfood-task-name task) "test-task")
         (check-equal? (dogfood-task-prompt task) "Do something")
         (check-equal? (dogfood-task-max-iterations task) 5))))

    (test-case "validate-dogfood-task rejects missing name"
      (check-exn exn:fail? (lambda () (validate-dogfood-task (hasheq 'prompt "test")))))

    (test-case "validate-dogfood-task rejects missing prompt"
      (check-exn exn:fail? (lambda () (validate-dogfood-task (hasheq 'name "test")))))

    (test-case "validate-dogfood-task rejects non-hash"
      (check-exn exn:fail? (lambda () (validate-dogfood-task "not a hash"))))

    (test-case "setup-task-files creates files from spec"
      (with-temp-dir (lambda (dir)
                       (define task
                         (dogfood-task "file-test"
                                       ""
                                       "prompt"
                                       '()
                                       ""
                                       5
                                       (hasheq 'files
                                               (hasheq "test.txt" "hello" "sub/nested.txt" "world"))
                                       '()))
                       (setup-task-files task dir)
                       (check-true (file-exists? (build-path dir "test.txt")))
                       (check-true (file-exists? (build-path dir "sub/nested.txt")))
                       (check-equal? (file->string (build-path dir "test.txt")) "hello"))))

    (test-case "teardown-task-files removes specified files"
      (with-temp-dir (lambda (dir)
                       (define f1 (build-path dir "cleanup.txt"))
                       (call-with-output-file f1 (lambda (out) (display "temp" out)))
                       (check-true (file-exists? f1))
                       (define task
                         (dogfood-task "cleanup-test" "" "prompt" '() "" 5 (hasheq) '("cleanup.txt")))
                       (teardown-task-files task dir)
                       (check-false (file-exists? f1)))))

    (test-case "run-dogfood-task/mock produces trace directory"
      (with-temp-dir (lambda (dir)
                       (define task
                         (dogfood-task "mock-test" "Mock task" "Hello agent" '() "" 3 (hasheq) '()))
                       (define result-dir (run-dogfood-task/mock task dir))
                       (check-true (directory-exists? result-dir))
                       (check-true (file-exists? (build-path result-dir "task-meta.json"))))))

    (test-case "task->session-prompt returns prompt field"
      (define task (dogfood-task "p" "" "Hello world" '() "" 3 (hasheq) '()))
      (check-equal? (task->session-prompt task) "Hello world"))

    (test-case "capture-baseline writes metrics from trace"
      (with-temp-dir (lambda (dir)
                       (define trace-path (build-path dir "trace.jsonl"))
                       (call-with-output-file
                        trace-path
                        (lambda (out)
                          (for ([msg (list (hasheq 'role "user" 'content "test")
                                           (hasheq 'role
                                                   "assistant"
                                                   'content
                                                   "response"
                                                   'tool_calls
                                                   (list (hasheq 'function (hasheq 'name "read"))))
                                           (hasheq 'role "tool" 'content "file content")
                                           (hasheq 'role "assistant" 'content "Done"))])
                            (displayln (jsexpr->string msg) out))))
                       (define baseline-path (capture-baseline trace-path))
                       (check-true (file-exists? baseline-path))
                       (define baseline (load-baseline baseline-path))
                       (check-equal? (hash-ref baseline 'tool_call_count) 1)
                       (check-equal? (hash-ref baseline 'iteration_count) 2))))

    (test-case "regression-passes? within tolerance"
      (with-temp-dir
       (lambda (dir)
         (define trace-path (build-path dir "trace.jsonl"))
         (call-with-output-file trace-path
                                (lambda (out)
                                  (for ([msg (list (hasheq 'role "user" 'content "test")
                                                   (hasheq 'role "assistant" 'content "response")
                                                   (hasheq 'role "assistant" 'content "done"))])
                                    (displayln (jsexpr->string msg) out))))
         (define baseline-path (build-path dir "baseline.json"))
         (call-with-output-file
          baseline-path
          (lambda (out)
            (write-json
             (hasheq 'tool_call_count 0 'iteration_count 2 'unique_tools '() 'outcome_length 4)
             out)))
         (check-true (regression-passes? trace-path baseline-path)))))

    (test-case "sample tasks load successfully"
      (define tasks-dir (build-path (current-directory) "tests" "workflows" "dogfood" "tasks"))
      (when (directory-exists? tasks-dir)
        (define task-files
          (for/list ([f (in-directory tasks-dir)]
                     #:when (regexp-match? #rx"\\.json$" (path->string f)))
            f))
        (check-true (>= (length task-files) 3) "Should have at least 3 sample tasks")
        (for ([f (in-list task-files)])
          (define task (load-dogfood-task f))
          (check-true (dogfood-task? task) (format "Task ~a should load" (path->string f))))))))

(run-tests test-dogfood-infrastructure)
