#lang racket

;; tests/test-benchmark-scorer.rkt — Tests for scoring engine

(require rackunit
         racket/file
         racket/format
         racket/path
         json
         (only-in "../scripts/benchmark/executor.rkt" execution-result execution-result?)
         (only-in "../scripts/benchmark/task.rkt" validate-benchmark-task)
         "../scripts/benchmark/scorer.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-test-exec-result #:task-name [name "test-task"]
                               #:trace-path [trace-path #f]
                               #:session-dir
                               [session-dir (make-temporary-file "bench-session-~a" 'directory)]
                               #:duration-ms [duration-ms 5000]
                               #:iterations-used [iterations 5]
                               #:outcome [outcome 'completed]
                               #:error-msg [error-msg #f]
                               #:project-dir [project-dir #f]
                               #:raw-result [raw-result #f])
  (execution-result name
                    trace-path
                    session-dir
                    duration-ms
                    iterations
                    outcome
                    error-msg
                    project-dir
                    raw-result))

(define (make-test-task #:name [name "test-task"]
                        #:max-iterations [max-iters 20]
                        #:files-created [fc '()]
                        #:files-not-modified [fnm '()]
                        #:tools-used [tu '()]
                        #:tools-not-used [tnu '()])
  (validate-benchmark-task
   (hasheq 'name
           name
           'prompt
           "test prompt"
           'max_iterations
           max-iters
           'scoring
           (hasheq 'files_created fc 'files_not_modified fnm 'tools_used tu 'tools_not_used tnu))))

;; ============================================================
;; score->verdict
;; ============================================================

(test-case "verdict: PASS for >= 70"
  (check-equal? (score->verdict 85) 'PASS)
  (check-equal? (score->verdict 70) 'PASS))

(test-case "verdict: PARTIAL for >= 40 and < 70"
  (check-equal? (score->verdict 69) 'PARTIAL)
  (check-equal? (score->verdict 40) 'PARTIAL))

(test-case "verdict: FAIL for < 40"
  (check-equal? (score->verdict 39) 'FAIL)
  (check-equal? (score->verdict 0) 'FAIL))

;; ============================================================
;; score-efficiency
;; ============================================================

(test-case "efficiency: perfect when 0 iterations used"
  (define exec (make-test-exec-result #:iterations-used 0))
  (define task (make-test-task #:max-iterations 20))
  (let-values ([(score _) (score-efficiency exec task)])
    (check-equal? score 100)))

(test-case "efficiency: 50% when half iterations used"
  (define exec (make-test-exec-result #:iterations-used 10))
  (define task (make-test-task #:max-iterations 20))
  (let-values ([(score _) (score-efficiency exec task)])
    (check-equal? score 50)))

(test-case "efficiency: low when max iterations reached"
  (define exec (make-test-exec-result #:iterations-used 20))
  (define task (make-test-task #:max-iterations 20))
  (let-values ([(score _) (score-efficiency exec task)])
    (check-true (<= score 10))))

;; ============================================================
;; score-skill-compliance (placeholder)
;; ============================================================

(test-case "skill-compliance: always 100 (placeholder)"
  (define exec (make-test-exec-result))
  (define task (make-test-task))
  (let-values ([(score _) (score-skill-compliance exec task)])
    (check-equal? score 100)))

;; ============================================================
;; score-correctness
;; ============================================================

(test-case "correctness: 100 when no files-required and no project-dir"
  (define exec (make-test-exec-result #:project-dir #f))
  (define task (make-test-task))
  (let-values ([(score _) (score-correctness exec task)])
    (check-equal? score 100)))

(test-case "correctness: 0 when required files don't exist"
  (define tmp-dir (make-temporary-file "bench-correctness-~a" 'directory))
  (define exec (make-test-exec-result #:project-dir tmp-dir))
  (define task
    (make-test-task #:files-created
                    (list (hasheq 'path "nonexistent.rkt" 'must_contain '("define")))))
  (let-values ([(score _) (score-correctness exec task)])
    (check-true (< score 100)))
  (delete-directory/files tmp-dir))

(test-case "correctness: high when required files exist with content"
  (define tmp-dir (make-temporary-file "bench-correctness-~a" 'directory))
  (call-with-output-file (build-path tmp-dir "hello.rkt")
                         (lambda (out) (display "(define (foo) 42)" out)))
  (define exec (make-test-exec-result #:project-dir tmp-dir))
  (define task
    (make-test-task #:files-created (list (hasheq 'path "hello.rkt" 'must_contain '("define")))))
  (let-values ([(score _) (score-correctness exec task)])
    (check-true (>= score 80)))
  (delete-directory/files tmp-dir))

;; ============================================================
;; score-tool-discipline
;; ============================================================

(test-case "tool-discipline: 100 when no trace and no tool requirements"
  (define exec (make-test-exec-result #:trace-path #f))
  (define task (make-test-task))
  (let-values ([(score _) (score-tool-discipline exec task)])
    (check-equal? score 100)))

(test-case "tool-discipline: 100 when empty trace and no requirements"
  (define tmp-file (make-temporary-file "bench-trace-~a.jsonl"))
  (call-with-output-file tmp-file (lambda (out) (void)) #:exists 'replace)
  (define exec (make-test-exec-result #:trace-path tmp-file))
  (define task (make-test-task))
  (let-values ([(score _) (score-tool-discipline exec task)])
    (check-equal? score 100))
  (delete-file tmp-file))

(test-case "tool-discipline: detects tools used from trace"
  (define tmp-file (make-temporary-file "bench-trace-~a.jsonl"))
  (call-with-output-file
   tmp-file
   (lambda (out)
     (write-json (hasheq 'phase "tool.call.started" 'data (hasheq 'name "write")) out)
     (newline out)
     (write-json (hasheq 'phase "tool.call.started" 'data (hasheq 'name "read")) out)
     (newline out))
   #:exists 'replace)
  (define exec (make-test-exec-result #:trace-path tmp-file))
  (define task (make-test-task #:tools-used '("write" "read")))
  (let-values ([(score _) (score-tool-discipline exec task)])
    (check-equal? score 100))
  (delete-file tmp-file))

(test-case "tool-discipline: penalizes missing tools"
  (define tmp-file (make-temporary-file "bench-trace-~a.jsonl"))
  (call-with-output-file tmp-file
                         (lambda (out)
                           (write-json (hasheq 'phase "tool.call.started" 'data (hasheq 'name "read"))
                                       out)
                           (newline out))
                         #:exists 'replace)
  (define exec (make-test-exec-result #:trace-path tmp-file))
  (define task (make-test-task #:tools-used '("write" "read")))
  (let-values ([(score _) (score-tool-discipline exec task)])
    (check-true (< score 100)))
  (delete-file tmp-file))

(test-case "tool-discipline: penalizes forbidden tools"
  (define tmp-file (make-temporary-file "bench-trace-~a.jsonl"))
  (call-with-output-file
   tmp-file
   (lambda (out)
     (write-json (hasheq 'phase "tool.call.started" 'data (hasheq 'name "bash")) out)
     (newline out)
     (write-json (hasheq 'phase "tool.call.started" 'data (hasheq 'name "write")) out)
     (newline out))
   #:exists 'replace)
  (define exec (make-test-exec-result #:trace-path tmp-file))
  (define task (make-test-task #:tools-used '("write") #:tools-not-used '("bash")))
  (let-values ([(score _) (score-tool-discipline exec task)])
    (check-true (< score 100)))
  (delete-file tmp-file))

;; ============================================================
;; score-no-regressions
;; ============================================================

(test-case "no-regressions: 100 when no files-not-modified spec"
  (define exec (make-test-exec-result #:project-dir #f))
  (define task (make-test-task))
  (let-values ([(score _) (score-no-regressions exec task)])
    (check-equal? score 100)))

(test-case "no-regressions: 100 when no project-dir and empty not-modified list"
  (define exec (make-test-exec-result #:project-dir #f))
  (define task (make-test-task #:files-not-modified '()))
  (let-values ([(score _) (score-no-regressions exec task)])
    (check-equal? score 100)))

;; ============================================================
;; aggregate-scores
;; ============================================================

(test-case "aggregate: weighted sum produces correct total"
  (define total (aggregate-scores 100 100 100 100 100))
  (check-true (= total 100)))

(test-case "aggregate: all zeros produces zero"
  (define total (aggregate-scores 0 0 0 0 0))
  (check-true (= total 0)))

(test-case "aggregate: mixed scores produce weighted result"
  (define total (aggregate-scores 50 100 100 100 100))
  (check-true (= total 80)))

;; ============================================================
;; score-execution (integration)
;; ============================================================

(test-case "score-execution: returns score-result struct"
  (define exec (make-test-exec-result))
  (define task (make-test-task))
  (define result (score-execution exec task))
  (check-true (score-result? result))
  (check-equal? (score-result-task-name result) "test-task")
  (check-true (>= (score-result-total-score result) 0))
  (check-true (<= (score-result-total-score result) 100))
  (check-not-false (member (score-result-verdict result) '(PASS PARTIAL FAIL))))

;; ============================================================
;; v0.19.3 Wave 4: Partial credit + must_not_contain tests
;; ============================================================

(test-case "correctness: partial credit for 2 of 3 must-contain"
  (define tmp-dir (make-temporary-file "bench-correct-~a" 'directory))
  ;; Create a file with 2 of 3 required strings
  (call-with-output-file (build-path tmp-dir "output.rkt")
                         (lambda (out) (display "#lang racket\n(define (foo x) (+ x 1))\n" out)))
  (define exec (make-test-exec-result #:project-dir tmp-dir))
  (define task
    (make-test-task #:files-created (list (hasheq 'path
                                                  "output.rkt"
                                                  'must_contain
                                                  '("#lang racket" "define (foo" "MISSING-STRING")))))
  (define-values (score details) (score-correctness exec task))
  ;; 2 of 3 positive checks pass → 2/3 ≈ 67%
  (check-true (> score 50) "partial credit should be > 50")
  (check-true (< score 100) "partial credit should be < 100")
  (delete-directory/files tmp-dir))

(test-case "correctness: must_not_contain violation reduces score"
  (define tmp-dir (make-temporary-file "bench-neg-~a" 'directory))
  (call-with-output-file
   (build-path tmp-dir "output.rkt")
   (lambda (out) (display "#lang racket\n(define (foo x) (error \"not implemented\"))\n" out)))
  (define exec (make-test-exec-result #:project-dir tmp-dir))
  (define task
    (make-test-task
     #:files-created
     (list (hasheq 'path "output.rkt" 'must_contain '("#lang racket") 'must_not_contain '("error")))))
  (define-values (score details) (score-correctness exec task))
  ;; 1 positive + 0 negative pass = 1/2 = 50%
  (check-equal? score 50)
  (delete-directory/files tmp-dir))

(test-case "correctness: all must_not_contain pass gives full credit"
  (define tmp-dir (make-temporary-file "bench-neg2-~a" 'directory))
  (call-with-output-file (build-path tmp-dir "output.rkt")
                         (lambda (out) (display "#lang racket\n(define (foo x) (+ x 1))\n" out)))
  (define exec (make-test-exec-result #:project-dir tmp-dir))
  (define task
    (make-test-task
     #:files-created
     (list (hasheq 'path "output.rkt" 'must_contain '("#lang racket") 'must_not_contain '("error")))))
  (define-values (score details) (score-correctness exec task))
  (check-equal? score 100)
  (delete-directory/files tmp-dir))
