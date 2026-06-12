#lang racket/base

;; @speed fast
;; @suite default
;;
;; Regression tests for spawn-subagents result extraction bugs:
;; Bug 1: extract-text-summary truncated to 200 chars (now 4000)
;; Bug 2: Empty result when assistant messages have only tool calls
;; Bug 3: #hasheq in arg summary for spawn-subagents
;;
;; Related: commit 4cd0ae41 (initial #hasheq fix), this commit (empty/truncated results)

(require rackunit
         rackunit/text-ui
         racket/string
         ;; Bug 3: extract-arg-summary
         (only-in "../tui/state-types.rkt" extract-arg-summary))

;; ═══════════════════════════════════════════════════════════
;; Bug 1: Truncation limit (tested via raco test submodule)
;; ═══════════════════════════════════════════════════════════
;; extract-text-summary is a private function; tested via
;; raco test tools/builtins/spawn-subagent.rkt module+ test
;; or through tool-spawn-subagents integration.

;; ═══════════════════════════════════════════════════════════
;; Bug 3: Arg summary for spawn-subagents
;; ═══════════════════════════════════════════════════════════

(define-test-suite
 bug3-arg-summary-tests
 (test-case "spawn-subagents arg summary shows job count + task"
   (define args
     (hasheq 'jobs
             (list (hasheq 'task "Scrape URL http://example.com" 'jobId "job1"))
             'maxParallel
             2
             'aggregate
             #t))
   (define summary (extract-arg-summary args))
   ;; Should NOT contain #hasheq
   (check-false (string-contains? summary "#hasheq")
                (format "Arg summary should not contain #hasheq, got: ~a" summary))
   ;; Should contain job count
   (check-true (string-contains? summary "1 job") (format "Should mention '1 job', got: ~a" summary))
   ;; Should contain first task text
   (check-true (string-contains? summary "Scrape")
               (format "Should contain task text, got: ~a" summary)))
 (test-case "spawn-subagents with multiple jobs shows count"
   (define args
     (hasheq 'jobs
             (list (hasheq 'task "Task A" 'jobId "j1")
                   (hasheq 'task "Task B" 'jobId "j2")
                   (hasheq 'task "Task C" 'jobId "j3"))
             'maxParallel
             3))
   (define summary (extract-arg-summary args))
   (check-false (string-contains? summary "#hasheq"))
   (check-true (string-contains? summary "3 jobs") (format "Should say '3 jobs', got: ~a" summary)))
 (test-case "spawn-subagent (single) arg summary shows task"
   (define args (hasheq 'task "Read the file config.json and summarize it" 'max-turns 3))
   (define summary (extract-arg-summary args))
   (check-false (string-contains? summary "#hasheq"))
   (check-true (string-contains? summary "config.json")
               (format "Should contain task text, got: ~a" summary)))
 (test-case "spawn-subagents with no task text shows count only"
   (define args (hasheq 'jobs (list (hasheq 'jobId "j1") (hasheq 'jobId "j2")) 'maxParallel 2))
   (define summary (extract-arg-summary args))
   (check-false (string-contains? summary "#hasheq"))
   (check-true (string-contains? summary "2 jobs") (format "Should say '2 jobs', got: ~a" summary))))

;; ═══════════════════════════════════════════════════════════
;; Run all tests
;; ═══════════════════════════════════════════════════════════

(run-tests (test-suite "spawn-subagent-result-extraction"
             bug3-arg-summary-tests))
