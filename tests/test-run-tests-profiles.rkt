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

;; ── v1.00.27 W2 (#9590) overlap ownership helpers ──

;; Generic ci.yml job-block extraction (same idiom as security-job-block*).
(define (ci-job-block* name)
  (define all (ci-yml-lines*))
  (define start
    (for/first ([line (in-list all)]
                [i (in-naturals)]
                #:when (regexp-match?
                        (byte-regexp (bytes-append #"^  " (string->bytes/utf-8 name) #":\\s*$"))
                        line))
      i))
  (unless start
    (error 'ci-job-block "ci.yml no longer declares a ~a job" name))
  (cons (list-ref all start)
        (for/list ([line (in-list (list-tail all (add1 start)))]
                   #:break (regexp-match? #px"^  [A-Za-z0-9_-]+:" line))
          line)))

;; W0 ownership-matrix reality derivation: the authoritative per-family
;; required_gates source (same function the milestone gate derives from).
(define inventory-path* (build-path project-root "scripts" "run-tests" "inventory.rkt"))
(define (tier-ownership-rows*)
  ((dynamic-require inventory-path*
                    'tier-ownership-rows
                    (lambda ()
                      (error 'w2-overlap "inventory.rkt no longer exports tier-ownership-rows")))))

;; W2 checksummed overlap-review artifact.
(define overlap-review-path*
  (build-path project-root "artifacts" "tier-ownership" "v1.00.27-w2" "overlap-review.json"))
(define w2-sha256sums-path*
  (build-path project-root "artifacts" "tier-ownership" "v1.00.27-w2" "SHA256SUMS"))

;; read-json yields hashes with symbol keys; jref accepts either spelling.
(define (jref payload key)
  (if (hash? payload)
      (hash-ref payload (string->symbol key) (hash-ref payload key #f))
      #f))

(define sha256-hex*
  (dynamic-require (build-path project-root "scripts" "run-tests" "sha256.rkt")
                   'sha256-hex
                   (lambda () (error 'w2-overlap "sha256.rkt no longer exports sha256-hex"))))

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

(define suite-failures (run-tests suite))

;; ── v1.00.27 W2 (#9590) overlap ownership: platform/fast + security/fast ──
;; W2 scope: every test family whose W0-derived required_gates include
;; `fast` AND (`platform` or `security`) is an overlap row. Governance:
;; each such row must carry a verdict in the checksummed W2 overlap
;; review — either decision "kept" with a non-empty rationale (intentional
;; overlap: different env, isolation, or gate), or decision
;; "removed-duplicate" with same-commit equivalence evidence. An overlap
;; row with neither fails here. Delivered as flat top-level checks so every
;; form's balance is local.

;; W2 helpers over the W0 ownership-matrix reality derivation.
(define (w2-gates-of r)
  (jref r "required_gates"))

(define (w2-axis-rows axis)
  (define other (car (string-split axis "/")))
  (for/list ([r (in-list (tier-ownership-rows*))]
             #:when (and (member "fast" (w2-gates-of r)) (member other (w2-gates-of r))))
    r))

;; W0 matrix rows identify their test-family path under whichever spelling
;; inventory.rkt emits; accept all known spellings.
(define (w2-family-path r)
  (for/or ([key (in-list '("file" "test" "path" "family" "name"))])
    (define v (jref r key))
    (and (string? v) v)))

;; W2 artifact governance checks run via text-ui so their result is
;; countable; the module epilogue turns any failure into a nonzero exit
;; (rackunit's body printing alone does not fail the process).
(define w2-profiles-failures
  (run-tests
   (test-suite "W2 overlap artifact governance"
     (test-case "W2: overlap review artifact exists, is well-formed, and is checksummed"
       (check-true (file-exists? overlap-review-path*) "W2 overlap review artifact is missing")
       (define payload (call-with-input-file overlap-review-path* read-json))
       (check-equal? (jref payload "schema") "tier-ownership-overlap-review/v1")
       (check-equal? (jref payload "milestone") "v1.00.27")
       (check-equal? (jref payload "wave") "v1.00.27-w2")
       (check-equal? (jref payload "source_matrix")
                     "artifacts/tier-ownership/v1.00.27-w0/ownership-matrix.json")
       (check-equal? (jref payload "overlap_axes") '("platform/fast" "security/fast"))
       (check-true (file-exists? w2-sha256sums-path*) "W2 SHA256SUMS is missing")
       (define review-digest (sha256-hex* (open-input-file overlap-review-path*)))
       (check-true (for/or ([line (in-list (file->lines w2-sha256sums-path*))])
                     (and (regexp-match? #rx"overlap-review[.]json$" line)
                          (equal? (car (string-split line)) review-digest)))
                   "SHA256SUMS entry for overlap-review.json is missing or stale")
       ;; Governance (wave TDD step 2): every overlap row must carry a valid
       ;; verdict — decision "kept" with a non-empty rationale, or decision
       ;; "removed-duplicate" with same-commit equivalence evidence. A row
       ;; with neither fails here.
       (define rows (jref payload "rows"))
       (check-true (list? rows) "overlap review rows missing or not a list")
       (when (list? rows)
         (for ([row (in-list rows)]
               [i (in-naturals)])
           (check-true (and (member (jref row "axis") '("platform/fast" "security/fast")) #t)
                       (format "row ~a: unknown axis" i))
           (check-true (and (string? (jref row "test")) (non-empty-string? (jref row "test")))
                       (format "row ~a: missing test path" i))
           (define decision (jref row "decision"))
           (check-true (and (member decision '("kept" "removed-duplicate")) #t)
                       (format "row ~a: decision must be kept or removed-duplicate" i))
           (check-true
            (or (and (equal? decision "kept")
                     (string? (jref row "rationale"))
                     (non-empty-string? (jref row "rationale")))
                (and (equal? decision "removed-duplicate")
                     (string? (jref row "evidence"))
                     (non-empty-string? (jref row "evidence"))))
            (format
             "row ~a: kept rows need a rationale; removed-duplicate rows need equivalence evidence"
             i)))
         (check-equal?
          (length (filter (lambda (row) (equal? (jref row "decision") "removed-duplicate")) rows))
          (jref payload "removals")
          "removals count must equal the number of removed-duplicate rows"))
       ;; Coverage: every W0-derived overlap row (fast ∩ platform or fast ∩
       ;; security) must appear in the review artifact with a verdict.
       (when (list? rows)
         (for ([axis (in-list '("platform/fast" "security/fast"))])
           (define derived (w2-axis-rows axis))
           (check-true (pair? derived)
                       (format "~a: no W0 overlap rows derived — derivation is broken" axis))
           (for ([r (in-list derived)])
             (define t (w2-family-path r))
             (check-true
              (for/or ([row (in-list rows)])
                (and (equal? (jref row "axis") axis) (equal? (jref row "test") t)))
              (format "~a: overlap row ~a has no verdict row in the review artifact" axis t)))))
       (void)))))

;; A red suite or red W2 artifact check must fail the process: rackunit's
;; printing alone does not set the exit code.
(when (> (+ suite-failures w2-profiles-failures) 0)
  (exit 1))
