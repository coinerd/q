#lang racket/base

;; @speed fast
;; @suite testing
;; @isolation subprocess

(require rackunit
         rackunit/text-ui
         json
         racket/file
         racket/path
         racket/runtime-path
         racket/system
         "../scripts/run-tests.rkt")

(define-runtime-path here ".")
(define project-root (simplify-path (build-path here "..")))
(define runner-module `(file ,(path->string (build-path project-root "scripts/run-tests.rkt"))))

(define load-known-failure-ledger*
  (dynamic-require runner-module
                   'load-known-failure-ledger
                   (lambda () (lambda (_) (error 'load-known-failure-ledger "missing export")))))

(define summarize-ledger-results*
  (dynamic-require runner-module
                   'summarize-ledger-results
                   (lambda () (lambda _ (error 'summarize-ledger-results "missing export")))))

(define (result #:path path
                #:exit [exit-code 1]
                #:out [out "FAILURE"]
                #:failed [failed 1]
                #:total [total 1])
  (test-file-result path exit-code (string->bytes/utf-8 out) #"" 10 0 failed total))

(define (write-ledger entries)
  (define path (make-temporary-file "q-known-failures-~a.json"))
  (call-with-output-file path
                         #:exists 'truncate/replace
                         (lambda (out) (write-json (hasheq 'version 1 'entries entries) out)))
  path)

(define (delete-file/safe path)
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (when (file-exists? path)
      (delete-file path))))

(define (run/capture cmd)
  (parameterize ([current-directory project-root])
    (define out (open-output-string))
    (define err (open-output-string))
    (define code
      (parameterize ([current-output-port out]
                     [current-error-port err])
        (system/exit-code cmd)))
    (values code (get-output-string out) (get-output-string err))))

(define suite
  (test-suite "run-tests known-failure ledger"
    (test-case "parse-args accepts --ledger"
      (define-values (_jobs
                      _seq?
                      _timeout
                      _strict?
                      _suite
                      _extra
                      _repeat
                      _record?
                      _inventory?
                      _diagnose?
                      _mode
                      _json-out
                      ledger
                      _profile)
        (parse-args '("--ledger" "tests/test-suite-ledger.json")))
      (check-equal? ledger "tests/test-suite-ledger.json"))

    (test-case "known-failure ledger classifies known, new, unclassified, and resolved failures"
      (define ledger-path
        (write-ledger (list (hasheq 'file
                                    "tests/known.rkt"
                                    'category
                                    "ASSERTION_FAILURE"
                                    'owner
                                    "runtime"
                                    'first_seen
                                    "0.99.28"
                                    'release_blocking
                                    #f
                                    'issue
                                    "#9001"
                                    'notes
                                    "pre-existing assertion drift")
                            (hasheq 'file
                                    "tests/category-changed.rkt"
                                    'category
                                    "MODULE_LOAD_FAILURE"
                                    'owner
                                    "runtime"
                                    'first_seen
                                    "0.99.28"
                                    'release_blocking
                                    #t
                                    'issue
                                    "#9002"
                                    'notes
                                    "category must match")
                            (hasheq 'file
                                    "tests/resolved.rkt"
                                    'category
                                    "ASSERTION_FAILURE"
                                    'owner
                                    "runtime"
                                    'first_seen
                                    "0.99.28"
                                    'release_blocking
                                    #f
                                    'issue
                                    "#9003"
                                    'notes
                                    "should be resolved"))))
      (dynamic-wind void
                    (lambda ()
                      (define ledger (load-known-failure-ledger* ledger-path))
                      (check-equal? (length ledger) 3)
                      (define summary
                        (summarize-ledger-results* ledger
                                                   (list (result #:path "tests/known.rkt")
                                                         (result #:path "tests/new.rkt")
                                                         (result #:path "tests/category-changed.rkt"
                                                                 #:out "FAILURE"))))
                      (check-equal? (length (hash-ref summary 'known_failures)) 1)
                      (check-equal? (length (hash-ref summary 'new_failures)) 1)
                      (check-equal? (length (hash-ref summary 'unclassified_failures)) 2)
                      (check-equal? (length (hash-ref summary 'resolved_known_failures)) 1)
                      (check-equal? (hash-ref (car (hash-ref summary 'known_failures)) 'issue)
                                    "#9001"))
                    (lambda () (delete-file/safe ledger-path))))

    (test-case "CLI prints known-failure ledger summary"
      (define missing-file (make-temporary-file "test-missing-cli-known-~a.rkt"))
      (define ledger-path
        (write-ledger (list (hasheq 'file
                                    (path->string missing-file)
                                    'category
                                    "MODULE_LOAD_FAILURE"
                                    'owner
                                    "testing"
                                    'first_seen
                                    "0.99.30"
                                    'release_blocking
                                    #f
                                    'issue
                                    "#8313"
                                    'notes
                                    "synthetic missing module"))))
      (call-with-output-file missing-file
                             #:exists 'truncate/replace
                             (lambda (out)
                               (displayln "#lang racket/base" out)
                               (displayln "(require definitely/missing/module)" out)))
      (dynamic-wind
       void
       (lambda ()
         (define-values (code stdout stderr)
           (run/capture (format "racket scripts/run-tests.rkt --sequential --ledger ~a ~a"
                                ledger-path
                                missing-file)))
         (check-equal? code 1 stderr)
         (check-true (regexp-match? #rx"Known failures:[ ]+1" stdout) stdout)
         (check-true (regexp-match? #rx"New failures:[ ]+0" stdout) stdout)
         (check-true (regexp-match? #rx"Unclassified failures:[ ]+0" stdout) stdout)
         (check-true (regexp-match? #rx"Resolved known failures:[ ]+0" stdout) stdout))
       (lambda ()
         (delete-file/safe ledger-path)
         (delete-file/safe missing-file))))))

(run-tests suite)
