#lang racket/base
;; @speed fast
;; @suite default
;; W6 evidence manifest tests (F-03/F-08).

(require rackunit
         racket/file
         racket/string
         "../scripts/tmux-explore/evidence.rkt")

(define tmp-dir (make-temporary-file "q-evidence-test-~a" 'directory))

(dynamic-wind
 (lambda () (void))
 (lambda ()

   (test-case "build-evidence-manifest includes full SHA, version, digests"
     ;; Create a test artifact file
     (call-with-output-file (build-path tmp-dir "capture.txt")
                            (lambda (out) (display "test capture" out))
                            #:exists 'replace)
     (call-with-output-file (build-path tmp-dir "trace.jsonl")
                            (lambda (out) (display "{\"phase\":\"turn.started\"}" out))
                            #:exists 'replace)
     (define manifest
       (build-evidence-manifest #:tag "memory"
                                #:status 'pass
                                #:classification 'pass
                                #:repo-sha "abcdef1234567890abcdef1234567890abcdef12"
                                #:version "0.99.51"
                                #:output-dir tmp-dir
                                #:scenario-count 9))
     (check-equal? (hash-ref manifest 'schema-version) 1)
     (check-equal? (hash-ref manifest 'tag) "memory")
     (check-equal? (hash-ref manifest 'repo-sha) "abcdef1234567890abcdef1234567890abcdef12")
     (check-equal? (hash-ref manifest 'version) "0.99.51")
     (check-equal? (hash-ref manifest 'scenario-count) 9)
     (check-true (and (hash-ref manifest 'artifact-digest #f)
                      (not (equal? (hash-ref manifest 'artifact-digest) "ERROR")))
                 "artifact-digest must be computed")
     (check-true (and (hash-has-key? manifest 'artifacts) (pair? (hash-ref manifest 'artifacts)))
                 "artifacts list must be non-empty")
     (check-equal? (hash-ref manifest 'artifact-count) 2))

   (test-case "verify-evidence-manifest accepts valid manifest"
     (call-with-output-file (build-path tmp-dir "f2.txt")
                            (lambda (out) (display "data" out))
                            #:exists 'replace)
     (define manifest
       (build-evidence-manifest #:tag "tools"
                                #:status 'pass
                                #:classification 'pass
                                #:repo-sha "1234567890abcdef1234567890abcdef12345678"
                                #:version "0.99.51"
                                #:output-dir tmp-dir
                                #:scenario-count 9))
     (define result (verify-evidence-manifest manifest))
     (check-true (hash-ref result 'valid?) (string-join (hash-ref result 'reasons) "; ")))

   (test-case "verify-evidence-manifest rejects missing SHA"
     (define manifest
       (hasheq 'repo-sha
               "short"
               'version
               "0.99.51"
               'artifact-digest
               "x"
               'artifact-count
               1
               'scenario-count
               1
               'artifacts
               '()))
     (define result (verify-evidence-manifest manifest))
     (check-false (hash-ref result 'valid?))
     (check-true (ormap (lambda (r) (string-contains? r "repo-sha")) (hash-ref result 'reasons))))

   (test-case "verify-evidence-manifest rejects zero artifact count"
     (define manifest
       (hasheq 'repo-sha
               "1234567890abcdef1234567890abcdef12345678"
               'version
               "0.99.51"
               'artifact-digest
               "x"
               'artifact-count
               0
               'scenario-count
               1
               'artifacts
               '()))
     (define result (verify-evidence-manifest manifest))
     (check-false (hash-ref result 'valid?)))

   (test-case "verify-evidence-manifest rejects zero scenario count"
     (define manifest
       (hasheq 'repo-sha
               "1234567890abcdef1234567890abcdef12345678"
               'version
               "0.99.51"
               'artifact-digest
               "x"
               'artifact-count
               1
               'scenario-count
               0
               'artifacts
               '("f")))
     (define result (verify-evidence-manifest manifest))
     (check-false (hash-ref result 'valid?)))

   (test-case "leak-scan-passed? with clean text"
     (check-true (leak-scan-passed? "no secrets here" '())))

   (test-case "write-evidence-manifest! produces JSON"
     (define manifest
       (build-evidence-manifest #:tag "memory"
                                #:status 'pass
                                #:classification 'pass
                                #:repo-sha "abcdef1234567890abcdef1234567890abcdef12"
                                #:version "0.99.51"
                                #:output-dir tmp-dir
                                #:scenario-count 9))
     (define path (build-path tmp-dir "manifest.json"))
     (write-evidence-manifest! manifest path)
     (check-true (file-exists? path))))
 (lambda () (delete-directory/files tmp-dir #:must-exist? #f)))
