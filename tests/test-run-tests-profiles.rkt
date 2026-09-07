#lang racket/base

;; @speed fast
;; @suite testing
;; @isolation process
;; @boundary integration  ;; @mutates fs

(require rackunit
         rackunit/text-ui
         json
         racket/file
         racket/path
         racket/runtime-path
         racket/string
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

(define (parse-scheduler args)
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
                  scheduler
                  _json
                  _ledger
                  _profile
                  _lint-metadata?
                  _changed-base
                  _changed-head
                  _explain?
                  _impact-dry-run?
                  _prioritize
                  _failure-history
                  _generate-covers-manifest?
                  _shard-plan
                  _durations
                  _ordering)
    (parse-args args))
  scheduler)

;; ── W4 (#9592) security-queue hold contract helpers ──

(define ci-yml-path* (build-path project-root ".github" "workflows" "ci.yml"))

(define (ci-yml-lines*)
  (file->lines ci-yml-path*))

(define (security-job-block*)
  (define all (ci-yml-lines*))
  (define start
    (for/first ([line (in-list all)]
                [i (in-naturals)]
                #:when (regexp-match? #px"^  security:\\s*$" line))
      i))
  (unless start
    (error 'security-job-block "ci.yml no longer declares a security job"))
  (cons (list-ref all start)
        (for/list ([line (in-list (list-tail all (add1 start)))]
                   #:break (regexp-match? #px"^  [A-Za-z0-9_-]+:" line))
          line)))

(define (security-suite-run-line*)
  (define candidates
    (for/list ([line (in-list (security-job-block*))]
               #:when (and (string-contains? line "run-tests.rkt --suite security")
                           (string-contains? line "run:")))
      line))
  (and (= (length candidates) 1) (car candidates)))

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
                      _scheduler
                      _json
                      _ledger
                      profile
                      _lint-metadata?
                      _changed-base
                      _changed-head
                      _explain?
                      _impact-dry-run?
                      _prioritize
                      _failure-history
                      _generate-covers-manifest?
                      _shard-plan
                      _durations
                      _ordering)
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
       (lambda () (delete-dir/safe dir))))

    (test-case "executed profile skip renders S rather than F"
      (define-values (file dir)
        (write-temp-test
         "test-runtime-skip.rkt"
         (string-append "#lang racket/base\n"
                        "(displayln \"skipped_by_profile: unavailable test capability\")\n"
                        "(exit 5)\n")))
      (dynamic-wind void
                    (lambda ()
                      (define-values (code stdout stderr)
                        (run/capture (format "racket scripts/run-tests.rkt ~a" file)))
                      (check-equal? code 0 stderr)
                      (check-true (regexp-match? #rx"(^|\n)S\n" stdout))
                      (check-false (regexp-match? #rx"(^|\n)F\n" stdout)))
                    (lambda () (delete-dir/safe dir))))

    ;; ── W2 (#9590) fast-queue hold contract ──
    ;; W1 closed cohort C1 with an explicit hold verdict for fast-queue
    ;; (zero recorded shadow attempts), so W2 records the hold instead of
    ;; activating. These pins make the hold observable: the repository
    ;; variable TEST_RUNNER_SCHEDULER stays a CI-shell-only lever and the
    ;; runner CLI keeps its batch default under every resolution.

    (test-case "hold: scheduler default remains batch while the W1 fast-queue hold stands"
      (check-equal? (parse-scheduler '()) 'batch)
      (define env-queue (make-environment-variables))
      (environment-variables-set! env-queue #"TEST_RUNNER_SCHEDULER" #"queue")
      (parameterize ([current-environment-variables env-queue])
        (check-equal? (parse-scheduler '()) 'batch))
      (define env-batch (make-environment-variables))
      (environment-variables-set! env-batch #"TEST_RUNNER_SCHEDULER" #"batch")
      (parameterize ([current-environment-variables env-batch])
        (check-equal? (parse-scheduler '()) 'batch)))

    (test-case "hold: manual --scheduler override still selects the requested scheduler"
      (check-equal? (parse-scheduler '("--scheduler" "queue")) 'queue)
      (check-equal? (parse-scheduler '("--scheduler" "batch")) 'batch))

    ;; ── W4 (#9592) security-queue hold contract ──
    ;; W1's cohort C1 decision block records security-queue . hold with zero
    ;; paired shadow samples (security/queue/fifo: 0 recorded attempts across
    ;; all 20 eligible SHAs), so the roadmap's §6 W4 security gate (milestone
    ;; of the current release)
    ;; ("Security p50 <= 240 s; permission/isolation semantics unchanged") is
    ;; not evaluable from absent evidence. W4 therefore records the hold
    ;; instead of activating. These pins make the hold observable:
    ;;  - the required security lane's workflow block carries no scheduler
    ;;    lever and its single suite command is pinned verbatim (CLI batch
    ;;    default end to end),
    ;;  - the batch env lever still forces batch everywhere, and no ci.yml
    ;;    lane may resolve a scheduler default from the repository variable.
    ;; Any silent future activation (a workflow scheduler token, a changed
    ;; suite command) breaks a pin deliberately.

    (test-case "hold: security required lane stays on batch while the W1 security-queue hold stands"
      (define block (string-join (security-job-block*) "\n"))
      (check-true (> (string-length block) 100) "security job block extraction failed")
      (check-false
       (regexp-match? #rx"TEST_RUNNER_SCHEDULER" block)
       "the security lane must not reference the scheduler repository variable while the W1 security-queue hold stands")
      (check-false (regexp-match? #rx"--scheduler" block)
                   "the security lane command must keep the CLI batch default")
      (check-equal?
       (security-suite-run-line*)
       (string-append
        "        run: STRICT_TEST_RUNNER=1 racket scripts/run-tests.rkt"
        " --suite security --jobs 4 --json-out test-results.json 2>&1 | tee test-output.log")
       "the security lane's single suite command changed; only a reviewed promote may touch it"))

    (test-case "hold: TEST_RUNNER_SCHEDULER=batch still forces batch everywhere"
      (define env-batch (make-environment-variables))
      (environment-variables-set! env-batch #"TEST_RUNNER_SCHEDULER" #"batch")
      (parameterize ([current-environment-variables env-batch])
        (check-equal? (parse-scheduler '()) 'batch)
        (check-equal? (parse-scheduler '("--scheduler" "batch")) 'batch))
      (check-false
       (regexp-match? #rx"TEST_RUNNER_SCHEDULER"
                      (string-join (filter (lambda (line) (not (regexp-match? #px"^\\s*#" line)))
                                           (ci-yml-lines*))
                                   "\n"))
       "no ci.yml lane may resolve a scheduler from the repository variable while the W1 security-queue hold stands"))))

(run-tests suite)
