#lang racket/base

;; @speed fast
;; @suite default

(require json
         rackunit
         racket/file
         racket/list
         racket/port
         racket/string
         "../scripts/tmux-tui-explore.rkt")

(define (with-temp-dir proc)
  (define dir (make-temporary-file "q-tmux-explore-test-~a" 'directory))
  (dynamic-wind void
                (lambda () (proc dir))
                (lambda ()
                  (with-handlers ([exn:fail? (lambda (_e) (void))])
                    (delete-directory/files dir)))))

(define (scenario-tags)
  (map explore-scenario-tag scenario-registry))

(test-case "scenario registry contains required v0.99.44 exploration scenarios"
  (check-equal? (scenario-tags) '("memory" "gsd" "mas" "tools" "release-audit" "durable-memory"))
  (check-equal? (length (find-scenarios #:filter "memory")) 1)
  (check-equal? (find-scenarios #:filter "missing") '()))

(test-case "scenario list renders tags and descriptions"
  (define rendered (with-output-to-string (lambda () (print-scenario-list))))
  (check-true (string-contains? rendered "memory"))
  (check-true (string-contains? rendered "release-audit"))
  (check-true (string-contains? rendered "durable-memory")))

(test-case "real-provider authorization requires all explicit gates"
  (parameterize ([current-environment-variables
                  (environment-variables-copy (current-environment-variables))])
    (putenv "Q_TMUX_TUI_TESTS" "1")
    (putenv "Q_TMUX_TUI_REAL_PROVIDER" "1")
    (putenv "Q_TMUX_TUI_REAL_PROVIDER_CONFIRM" "NOPE")
    (check-false (real-provider-authorized?))
    (check-exn #rx"real-provider mode requires" require-real-provider-authorization!)
    (putenv "Q_TMUX_TUI_REAL_PROVIDER_CONFIRM" "I_UNDERSTAND_COSTS")
    (check-true (real-provider-authorized?))
    (check-not-exn require-real-provider-authorization!)))

(test-case "redaction removes credential-like values"
  (define raw "api_key=abc123 token=xyz password=hunter2 Authorization: Bearer secret sk-testSECRET")
  (define redacted (redact-explore-text raw))
  (check-false (string-contains? redacted "abc123"))
  (check-false (string-contains? redacted "hunter2"))
  (check-false (string-contains? redacted "sk-testSECRET"))
  (check-true (string-contains? redacted "<REDACTED>")))

(test-case "mock exploration writes markdown and summary files"
  (with-temp-dir (lambda (dir)
                   (define results
                     (run-exploration #:mode 'mock #:filter "tools" #:out (path->string dir)))
                   (check-equal? (length results) 1)
                   (define result (car results))
                   (check-equal? (explore-result-tag result) "tools")
                   (check-equal? (explore-result-status result) 'pass-mock-tui)
                   (check-equal? (explore-result-classification result) 'pass)
                   (check-true (file-exists? (explore-result-report-path result)))
                   (check-true (file-exists? (build-path dir "summary.tsv")))
                   (check-true (file-exists? (build-path dir "summary.json")))
                   (define md (file->string (explore-result-report-path result)))
                   (check-true (string-contains? md "README_FIXTURE_ALPHA"))
                   (check-true (string-contains? md "credential-like values are redacted"))
                   (check-true (string-contains? md "trace.jsonl turn.completed"))
                   (define rows (call-with-input-file (build-path dir "summary.json") read-json))
                   (check-equal? (length rows) 1)
                   (check-equal? (hash-ref (car rows) 'tag) "tools"))))

(test-case "mock exploration can append central markdown log"
  (with-temp-dir (lambda (dir)
                   (define log-path (build-path dir "TMUX-Q-TUI-EXPLORATION-LOG.md"))
                   (define results
                     (run-exploration #:mode 'mock
                                      #:filter "release-audit"
                                      #:out (path->string dir)
                                      #:append-log (path->string log-path)))
                   (check-equal? (length results) 1)
                   (define log-text (file->string log-path))
                   (check-true (string-contains? log-text "Explorer run"))
                   (check-true (string-contains? log-text "release-audit"))
                   (check-true (string-contains? log-text "pass-mock-tui-partial")))))

(test-case "real exploration refuses without explicit gates before writing reports"
  (with-temp-dir
   (lambda (dir)
     (parameterize ([current-environment-variables
                     (environment-variables-copy (current-environment-variables))])
       (putenv "Q_TMUX_TUI_TESTS" "1")
       (putenv "Q_TMUX_TUI_REAL_PROVIDER" "0")
       (check-exn #rx"real-provider mode requires"
                  (lambda ()
                    (run-exploration #:mode 'real #:filter "memory" #:out (path->string dir))))
       (check-false (file-exists? (build-path dir "summary.tsv")))))))
