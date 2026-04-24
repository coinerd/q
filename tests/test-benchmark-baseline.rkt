#lang racket

(require rackunit
         racket/file
         json
         "../scripts/benchmark/baseline.rkt"
         "../scripts/benchmark/report.rkt"
         "../scripts/benchmark/scorer.rkt")

;; Helper: build a benchmark-report from a list of scores
(define (make-test-report scores)
  (generate-report
   (for/list ([s (in-list scores)])
     (score-result "test-task" s 'PASS s s s s s #f))))

(test-case "baseline-path joins version and dir"
  (check-equal? (path->string (baseline-path "0.19.1" "/tmp/baselines"))
                "/tmp/baselines/0.19.1.json"))

(test-case "capture-baseline creates file"
  (define tmp (make-temporary-file "baseline-test-~a" 'directory))
  (dynamic-wind
    void
    (lambda ()
      (define report (make-test-report '(85.0)))
      (capture-baseline report "0.19.0" tmp)
      (define expected-file (build-path tmp "0.19.0.json"))
      (check-true (file-exists? expected-file)
                  "capture-baseline should create a JSON file"))
    (lambda () (delete-directory/files tmp))))

(test-case "load-baseline roundtrips"
  (define tmp (make-temporary-file "baseline-test-~a" 'directory))
  (dynamic-wind
    void
    (lambda ()
      (define report (make-test-report '(90.0 75.0)))
      (capture-baseline report "0.19.0" tmp)
      (define loaded (load-baseline (baseline-path "0.19.0" tmp)))
      (check-equal? (hash-ref (benchmark-report-summary loaded) 'total-tasks)
                    2
                    "loaded baseline should preserve task count"))
    (lambda () (delete-directory/files tmp))))

(test-case "list-baselines returns sorted files"
  (define tmp (make-temporary-file "baseline-test-~a" 'directory))
  (dynamic-wind
    void
    (lambda ()
      ;; Create two baseline files manually
      (call-with-output-file (build-path tmp "0.18.0.json")
        (lambda (out) (write-json (hasheq 'version "0.18.0" 'timestamp "t" 'results '() 'summary (hasheq)) out))
        #:exists 'replace)
      (call-with-output-file (build-path tmp "0.19.0.json")
        (lambda (out) (write-json (hasheq 'version "0.19.0" 'timestamp "t" 'results '() 'summary (hasheq)) out))
        #:exists 'replace)
      (define baselines (list-baselines tmp))
      (check-equal? (length baselines) 2)
      (check-equal? (path->string (car baselines)) "0.18.0.json")
      (check-equal? (path->string (cadr baselines)) "0.19.0.json"))
    (lambda () (delete-directory/files tmp))))

(test-case "list-baselines empty dir"
  (check-equal? (list-baselines "/nonexistent/path/that/does/not/exist") '()))
