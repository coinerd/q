#lang racket

;; tests/test-benchmark-task.rkt — Tests for benchmark task format

(require rackunit
         racket/file
         racket/port
         json
         (only-in "../scripts/benchmark/task.rkt"
                  benchmark-task?
                  benchmark-task-name
                  benchmark-task-category
                  benchmark-task-difficulty
                  benchmark-task-prompt
                  benchmark-task-max-iterations
                  benchmark-task-max-tokens
                  benchmark-task-time-limit-seconds
                  benchmark-task-setup
                  benchmark-task-scoring
                  benchmark-task-teardown
                  scoring-spec-files-created
                  scoring-spec-tools-used
                  task-setup-spec-files
                  task-setup-spec-extensions
                  task-setup-spec-skills
                  file-check-path
                  file-check-must-contain
                  load-benchmark-task
                  validate-benchmark-task
                  task-setup
                  task-teardown
                  all-benchmark-tasks))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-temp-task-file content)
  (define tmp (make-temporary-file "bench-task-~a.json"))
  (call-with-output-file tmp
    (lambda (out) (write-json content out))
    #:exists 'replace)
  tmp)

;; ============================================================
;; Loading valid tasks
;; ============================================================

(test-case "load valid minimal task"
  (define tmp (make-temp-task-file
               (hasheq 'name "test-task"
                       'prompt "Do something")))
  (define task (load-benchmark-task tmp))
  (check-equal? (benchmark-task-name task) "test-task")
  (check-equal? (benchmark-task-prompt task) "Do something")
  (check-equal? (benchmark-task-category task) 'implementation)
  (check-equal? (benchmark-task-difficulty task) 1)
  (delete-file tmp))

(test-case "load valid full task"
  (define tmp (make-temporary-file "bench-task-~a.json"))
  ;; Write JSON manually since write-json can't handle string keys in nested hashes
  (call-with-output-file tmp
    (lambda (out)
      (display "{\"name\":\"full-task\",\"prompt\":\"Implement X\",\"category\":\"bug-fix\",\"difficulty\":3,\"description\":\"A complex bug fix\",\"max_iterations\":20,\"max_tokens\":50000,\"time_limit_seconds\":300,\"setup\":{\"files\":{\"foo.rkt\":\"#lang racket\"},\"extensions\":[\"racket-tooling\"],\"skills\":[\"q-racket-guardrails\"]},\"scoring\":{\"files_created\":[{\"path\":\"q/foo.rkt\",\"must_contain\":[\"define\",\"provide\"]}],\"tools_used\":[\"write\",\"racket-check\"],\"max_iterations\":15},\"teardown\":[\"q/foo.rkt\"]}" out))
    #:exists 'replace)
  (define task (load-benchmark-task tmp))
  (check-equal? (benchmark-task-name task) "full-task")
  (check-equal? (benchmark-task-category task) 'bug-fix)
  (check-equal? (benchmark-task-difficulty task) 3)
  (check-equal? (benchmark-task-max-iterations task) 20)
  (check-equal? (benchmark-task-max-tokens task) 50000)
  (check-equal? (benchmark-task-time-limit-seconds task) 300)
  ;; Setup — keys come through as strings from JSON
  (check-equal? (hash-ref (task-setup-spec-files (benchmark-task-setup task)) 'foo.rkt)
                "#lang racket")
  (check-equal? (task-setup-spec-extensions (benchmark-task-setup task))
                '("racket-tooling"))
  (check-equal? (task-setup-spec-skills (benchmark-task-setup task))
                '("q-racket-guardrails"))
  ;; Scoring
  (define scoring (benchmark-task-scoring task))
  (check-equal? (length (scoring-spec-files-created scoring)) 1)
  (check-equal? (file-check-path (car (scoring-spec-files-created scoring))) "q/foo.rkt")
  (check-equal? (file-check-must-contain (car (scoring-spec-files-created scoring)))
                '("define" "provide"))
  (check-equal? (scoring-spec-tools-used scoring) '("write" "racket-check"))
  ;; Teardown
  (check-equal? (benchmark-task-teardown task) '("q/foo.rkt"))
  (delete-file tmp))

;; ============================================================
;; Rejecting invalid tasks
;; ============================================================

(test-case "reject task without name"
  (check-exn exn:fail?
             (lambda ()
               (validate-benchmark-task (hasheq 'prompt "Do something")))))

(test-case "reject task without prompt"
  (check-exn exn:fail?
             (lambda ()
               (validate-benchmark-task (hasheq 'name "test")))))

(test-case "reject task with empty name"
  (check-exn exn:fail?
             (lambda ()
               (validate-benchmark-task (hasheq 'name "" 'prompt "Do something")))))

(test-case "reject task with invalid category"
  (check-exn exn:fail?
             (lambda ()
               (validate-benchmark-task
                (hasheq 'name "test" 'prompt "x" 'category "invalid")))))

(test-case "reject task with invalid difficulty"
  (check-exn exn:fail?
             (lambda ()
               (validate-benchmark-task
                (hasheq 'name "test" 'prompt "x" 'difficulty 5)))))

(test-case "reject non-hash input"
  (check-exn exn:fail?
             (lambda ()
               (validate-benchmark-task "not a hash"))))

;; ============================================================
;; Setup and teardown
;; ============================================================

(test-case "setup creates temp directory"
  (define task (validate-benchmark-task
                (hasheq 'name "setup-test"
                        'prompt "test"
                        'setup (hasheq 'files (hasheq 'hello.rkt "#lang racket")))))
  (define tmp-dir (task-setup task))
  (check-true (directory-exists? tmp-dir))
  (check-true (file-exists? (build-path tmp-dir "hello.rkt")))
  (task-teardown task tmp-dir)
  (check-false (directory-exists? tmp-dir)))

(test-case "setup with multiple files"
  (define task (validate-benchmark-task
                (hasheq 'name "multi-file"
                        'prompt "test"
                        'setup (hasheq 'files (hasheq 'a.rkt "content-a"
                                                       'dir/b.rkt "content-b")))))
  (define tmp-dir (task-setup task))
  (check-true (file-exists? (build-path tmp-dir "a.rkt")))
  (check-true (file-exists? (build-path tmp-dir "dir" "b.rkt")))
  (task-teardown task tmp-dir))

(test-case "teardown with keep? preserves directory"
  (define task (validate-benchmark-task
                (hasheq 'name "keep-test" 'prompt "test")))
  (define tmp-dir (task-setup task))
  (task-teardown task tmp-dir #t)
  (check-true (directory-exists? tmp-dir))
  (delete-directory/files tmp-dir))

(test-case "teardown nonexistent dir is safe"
  (define task (validate-benchmark-task
                (hasheq 'name "safe-test" 'prompt "test")))
  ;; Should not raise
  (task-teardown task "/tmp/nonexistent-benchmark-dir-xyz123"))

;; ============================================================
;; Loading from directory
;; ============================================================

(test-case "all-benchmark-tasks loads from directory"
  (define tmp-dir (make-temporary-file "bench-tasks-~a" 'directory))
  (for ([name '("task-a.json" "task-b.json")])
    (call-with-output-file (build-path tmp-dir name)
      (lambda (out)
        (write-json (hasheq 'name name 'prompt (format "Prompt for ~a" name)) out))))
  (define tasks (all-benchmark-tasks tmp-dir))
  (check-equal? (length tasks) 2)
  (check-true (andmap benchmark-task? tasks))
  (delete-directory/files tmp-dir))

(test-case "all-benchmark-tasks ignores non-JSON files"
  (define tmp-dir (make-temporary-file "bench-tasks-~a" 'directory))
  (call-with-output-file (build-path tmp-dir "task.json")
    (lambda (out) (write-json (hasheq 'name "only-task" 'prompt "x") out)))
  (call-with-output-file (build-path tmp-dir "readme.md")
    (lambda (out) (display "ignore me" out)))
  (define tasks (all-benchmark-tasks tmp-dir))
  (check-equal? (length tasks) 1)
  (delete-directory/files tmp-dir))
