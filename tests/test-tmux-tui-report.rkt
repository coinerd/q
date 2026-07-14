#lang racket/base

;; @speed fast
;; @suite default

(require rackunit
         racket/file
         racket/port
         racket/string
         "../scripts/tmux-tui-report.rkt")

(define (with-temp-dir proc)
  (define dir (make-temporary-file "q-tmux-report-test-~a" 'directory))
  (dynamic-wind void
                (lambda () (proc dir))
                (lambda ()
                  (with-handlers ([exn:fail? (lambda (e) (void))])
                    (delete-directory/files dir)))))

(test-case "classify success dir without artifacts (GAP-6 improved classification)"
  (with-temp-dir (lambda (dir)
                   (define success-dir (build-path dir "q-tmux-art-success"))
                   (make-directory* success-dir)
                   ;; No reason file, no artifacts → success-no-artifacts
                   (check-equal? (classify-failure #f success-dir) 'success-no-artifacts)
                   (define rendered (with-output-to-string (lambda () (render-report dir))))
                   (check-true (string-contains? rendered "Category: success-no-artifacts"))
                   (check-true (string-contains? rendered "Artifacts: not expected"))
                   (check-false (string-contains? rendered "❌ MISSING")))))

(test-case "classify timeout failure with complete redacted bundle"
  (with-temp-dir (lambda (dir)
                   (define art-dir (build-path dir "q-tmux-art-timeout"))
                   (make-directory* art-dir)
                   (call-with-output-file (build-path art-dir "reason.txt")
                                          (lambda (p) (display "timeout waiting for prompt" p)))
                   (call-with-output-file (build-path art-dir "raw-capture.txt")
                                          (lambda (p) (display "safe raw" p)))
                   (call-with-output-file (build-path art-dir "normalized-capture.txt")
                                          (lambda (p) (display "safe normalized" p)))
                   (call-with-output-file (build-path art-dir "env-summary.txt")
                                          (lambda (p) (display "API_KEY=<REDACTED>" p)))
                   (check-equal? (classify-failure "timeout waiting for prompt" art-dir) 'timeout)
                   (check-true (complete-artifact-bundle? art-dir))
                   (check-equal? (check-redaction art-dir) '())
                   (define rendered (with-output-to-string (lambda () (render-report dir))))
                   (check-true (string-contains? rendered "Category: timeout"))
                   (check-true (string-contains? rendered "raw-capture.txt: ✅")))))

(test-case "redaction report detects leaked credential-like values"
  (with-temp-dir (lambda (dir)
                   (define art-dir (build-path dir "q-tmux-art-leak"))
                   (make-directory* art-dir)
                   (call-with-output-file
                    (build-path art-dir "env-summary.txt")
                    (lambda (p)
                      (display "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.real-token"
                               p)))
                   (check-not-equal? (check-redaction art-dir) '())
                   (define rendered (with-output-to-string (lambda () (render-report dir))))
                   (check-true (string-contains? rendered "Redaction: ❌ FAIL")))))

(test-case "redaction report ignores benign sk substrings and Bearer prose"
  (with-temp-dir (lambda (dir)
                   (define art-dir (build-path dir "q-tmux-art-clean"))
                   (make-directory* art-dir)
                   (call-with-output-file
                    (build-path art-dir "normalized-capture.txt")
                    (lambda (p)
                      (display "set-task-state risk-score Bearer authentication token=<REDACTED>" p)))
                   (check-equal? (check-redaction art-dir) '()))))
