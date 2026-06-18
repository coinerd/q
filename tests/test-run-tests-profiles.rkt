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

(define profile-skips-test?* (dynamic-require runner-module 'profile-skips-test? (lambda () #f)))
(define make-skipped-result* (dynamic-require runner-module 'make-skipped-result (lambda () #f)))
(define classify-test-result* (dynamic-require runner-module 'classify-test-result (lambda () #f)))
(define test-result->jsexpr* (dynamic-require runner-module 'test-result->jsexpr (lambda () #f)))

(define (write-temp-test name content)
  (define dir (make-temporary-file "q-profile-test-~a" 'directory))
  (define file (build-path dir name))
  (call-with-output-file file #:exists 'replace (lambda (out) (display content out)))
  (values file dir))

(define (delete-dir/safe dir)
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (delete-directory/files dir)))

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
  (test-suite "run-tests environment profiles"

    (test-case "parse-args accepts --profile"
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
                      _json
                      _ledger
                      profile)
        (parse-args '("--profile" "vps")))
      (check-equal? profile 'vps))

    (test-case "profile rules skip required unavailable capabilities"
      (check-pred procedure? profile-skips-test?*)
      (check-true (profile-skips-test?* 'vps '("browser")))
      (check-true (profile-skips-test?* 'headless '("terminal")))
      (check-true (profile-skips-test?* 'ci '("provider-key")))
      (check-false (profile-skips-test?* 'full '("browser")))
      (check-false (profile-skips-test?* 'local '("terminal")))
      (check-false (profile-skips-test?* 'vps '("filesystem" "git"))))

    (test-case "skipped result has explicit category and is not PASS"
      (check-pred procedure? make-skipped-result*)
      (define r (make-skipped-result* "tests/needs-browser.rkt" 'vps '("browser")))
      (check-equal? (classify-test-result* r) 'SKIPPED_BY_PROFILE)
      (define js (test-result->jsexpr* r))
      (check-equal? (hash-ref js 'category) "SKIPPED_BY_PROFILE")
      (check-equal? (hash-ref js 'exit_code) 5)
      (check-equal? (hash-ref js 'total) 0))

    (test-case "CLI skips explicit @requires browser file under vps profile"
      (define-values (file dir)
        (write-temp-test
         "test-needs-browser.rkt"
         (string-append "#lang racket/base\n"
                        ";; @requires browser\n"
                        "(error 'profile-test \"should not execute when skipped\")\n")))
      (define out (build-path dir "results.json"))
      (dynamic-wind
       void
       (lambda ()
         (define-values (code stdout stderr)
           (run/capture
            (format "racket scripts/run-tests.rkt --profile vps --json-out ~a ~a" out file)))
         (check-equal? code 0 stderr)
         (check-true (regexp-match? #rx"Skipped by profile: 1" stdout))
         (check-false (regexp-match? #rx"PASS=1" stdout))
         (define js (call-with-input-file out read-json))
         (check-equal? (hash-ref js 'profile) "vps")
         (check-equal? (hash-ref (hash-ref js 'summary) 'files_skipped_by_profile) 1)
         (define fjs (car (hash-ref js 'files)))
         (check-equal? (hash-ref fjs 'category) "SKIPPED_BY_PROFILE"))
       (lambda () (delete-dir/safe dir))))))

(run-tests suite)
