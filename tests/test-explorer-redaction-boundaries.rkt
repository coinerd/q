#lang racket

;; @speed fast
;; @suite security

(require rackunit
         racket/file
         racket/string
         "../scripts/tmux-tui-explore.rkt")

(test-case "explorer summary artifacts redact credential-bearing fields"
  (define root (make-temporary-file "q-explorer-redaction-~a" 'directory))
  (define result
    (explore-result "tag"
                    "access_token=explorer-access-raw"
                    'mock
                    'pass
                    'pass
                    "/tmp/client_secret=explorer-client-raw.md"
                    '()
                    "start"
                    "end"))
  (define-values (tsv-path json-path) (write-summary-files! root (list result)))
  (for ([path (in-list (list tsv-path json-path))])
    (define text (file->string path))
    (check-false (string-contains? text "explorer-access-raw"))
    (check-false (string-contains? text "explorer-client-raw"))
    (check-true (string-contains? text "<REDACTED>")))
  (delete-directory/files root))

(test-case "explorer report recursively redacts nested evidence"
  (define root (make-temporary-file "q-explorer-evidence-redaction-~a" 'directory))
  (define raw-values
    '("nested-capability-secret" "nested-capability-token" "nested-verifier" "nested-code"))
  (define evidence
    (list (cons "nested"
                (hasheq 'capability-secret
                        (list (first raw-values))
                        'capability-token
                        (vector (second raw-values))
                        'code-verifier
                        (third raw-values)
                        'authorization-code
                        (fourth raw-values)))))
  (define result (explore-result "tag" "title" 'mock 'pass 'pass #f evidence "start" "end"))
  (define written (write-explore-report! root result))
  (define report (file->string (explore-result-report-path written)))
  (for ([raw (in-list raw-values)])
    (check-false (string-contains? report raw)))
  (check-true (string-contains? report "<REDACTED>"))
  (delete-directory/files root))
