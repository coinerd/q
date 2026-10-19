#lang racket
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @timeout 600
(require rackunit
         json
         racket/runtime-path
         "helpers/supplemental-evidence-fixture.rkt"
         "../extensions/gsd/evidence-discovery.rkt"
         "../extensions/gsd/evidence-repair-format.rkt"
         "../extensions/gsd/evidence-runner-coverage.rkt"
         "../extensions/gsd/evidence-probe.rkt"
         "../extensions/gsd/review-proof.rkt"
         "../extensions/gsd/composition-root.rkt"
         "../extensions/gsd/verification-job.rkt"
         "../util/json/checksum.rkt")
(define-runtime-path owner "..")
(define (manifest repo)
  (list
   (hasheq 'path fixture-helper-path 'sha256 (sha256-file (build-path repo fixture-helper-path)))))
(define (discover repo baseline [approved (manifest repo)] [cmd command] [test fixture-test-path])
  (discover-evidence-probe-spec repo
                                (fixture-context repo)
                                cmd
                                #:harness-files approved
                                #:baseline-head baseline
                                #:test-path test))
(define (commit repo)
  (fixture-git repo "add" "tests" "runtime/settings-query.rkt" "scripts/run-tests.rkt")
  (fixture-git repo "commit" "--allow-empty" "-qm" "fixture variant"))
(define (structured result)
  (call-with-input-file (hash-ref (hash-ref result 'structured) 'path) read-json))
(define command "racket scripts/run-tests.rkt tests/probe.rkt")
(module+ test
  (test-case "default discovery rejects undeclared literal helper (behavioral regression)"
    (call-with-supplemental-fixture
     (lambda (repo baseline)
       (check-false (discover-evidence-probe-spec repo (fixture-context repo) command)
                    "undeclared test helper must not yield an empty-overlay spec"))))
  (test-case "approved exact helper manifest and pinned selection; invalid pins never fall back"
    (call-with-supplemental-fixture
     (lambda (repo baseline)
       (define s (discover repo baseline))
       (check-true (and s (valid-probe-spec? s)))
       (when s
         (check-equal? (hash-ref s 'harness-files) (manifest repo))
         (check-equal? (hash-ref s 'baseline-head) baseline)
         (check-equal? (hash-ref s 'test-paths) (list fixture-test-path)))
       (for ([bad (list #f "HEAD~1" (make-string 40 #\0) (hash-ref (fixture-context repo) 'head))])
         (check-false (discover repo bad) (format "baseline ~s" bad)))
       (for ([bad (list #f "tests/absent.rkt" "tests/../probe.rkt")])
         (check-false (discover repo baseline (manifest repo) command bad)))
       (check-false
        (discover repo baseline (list (hash-set (car (manifest repo)) 'sha256 (make-string 64 #\0)))))
       (check-false (discover repo baseline (append (manifest repo) (manifest repo))))
       (define test-only-base (hash-ref (fixture-context repo) 'head))
       (fixture-put repo
                    "tests/helpers/unused.rkt"
                    "#lang racket/base\n(provide unused)\n(define unused 1)\n")
       (commit repo)
       (check-false (discover repo test-only-base)
                    "pinned baseline without production difference cannot fall back")
       (define unrelated
         (fixture-git repo
                      "commit-tree"
                      (hash-ref (fixture-context repo) 'tree)
                      "-m"
                      "unrelated root"))
       (check-false (discover repo unrelated) "pinned baseline must be a strict ancestor")
       (check-false
        (discover repo
                  baseline
                  (append (manifest repo)
                          (list (hasheq 'path
                                        "tests/helpers/unused.rkt"
                                        'sha256
                                        (sha256-file (build-path repo
                                                                 "tests/helpers/unused.rkt")))))))
       (check-false
        (discover repo
                  baseline
                  (list (hasheq 'path
                                "runtime/settings-query.rkt"
                                'sha256
                                (sha256-file (build-path repo "runtime/settings-query.rkt"))))))
       ;; A real helper imported by a non-selected probe cannot authorize this one.
       (fixture-put repo "tests/other.rkt" (file->string (build-path repo fixture-test-path)))
       (fixture-put repo
                    fixture-test-path
                    (string-replace (file->string (build-path repo fixture-test-path))
                                    "(valid-tracker-binding-normalizes?)"
                                    "#t"))
       (commit repo)
       (check-false (discover repo
                              baseline
                              (manifest repo)
                              "racket scripts/run-tests.rkt tests/other.rkt"
                              fixture-test-path)))))
  (test-case "helper imports are literal, closed, regular and statically read without execution"
    (call-with-supplemental-fixture
     (lambda (repo baseline)
       (define original (file->string (build-path repo fixture-helper-path)))
       (define probe (file->string (build-path repo fixture-test-path)))
       (define marker (build-path repo "reader-executed"))
       (fixture-put
        repo
        "tests/helpers/reader.rkt"
        (format (string-append "#lang racket/base\n(provide read read-syntax)\n"
                               "(call-with-output-file ~s (lambda (out) (display 'executed out)))\n")
                (path->string marker)))
       (fixture-put repo
                    "tests/helpers/transitive.rkt"
                    "#lang racket/base\n(provide plain)\n(define plain 1)\n")
       (define reader (path->string (build-path repo "tests/helpers/reader.rkt")))
       (for ([suffix (list "\n(require \"transitive.rkt\")\n"
                           "\n(require (only-in \"transitive.rkt\" plain))\n"
                           "\n(require (for-syntax racket/base))\n"
                           "\n(local-require \"transitive.rkt\")\n"
                           "\n(dynamic-require (build-path \"transitive.rkt\") #f)\n"
                           "\n(require \"../../../outside.rkt\")\n"
                           (format "\n#reader (file ~s)\n" reader))])
         (fixture-put repo fixture-helper-path (string-append original suffix))
         (commit repo)
         (check-false (discover repo baseline) suffix)
         (check-false (file-exists? marker) "static discovery executed reader"))
       (fixture-put
        repo
        fixture-helper-path
        (string-replace original "#lang racket/base" (format "#lang reader (file ~s)" reader)))
       (commit repo)
       (check-false (discover repo baseline))
       (check-false (file-exists? marker))
       ;; Even a supported helper initializer must not execute in discovery.
       (fixture-put
        repo
        fixture-helper-path
        (string-append
         original
         (format
          "\n(define initialized (call-with-output-file ~s (lambda (out) (display 'executed out))))\n"
          (path->string marker))))
       (commit repo)
       (check-not-false (discover repo baseline))
       (check-false (file-exists? marker) "discovery loaded the helper")
       ;; Declared transitive literal imports are overlaid too, but not aliases.
       (fixture-put repo
                    fixture-helper-path
                    (string-append original "\n(require \"transitive.rkt\")\n"))
       (commit repo)
       (define transitive
         (hasheq 'path
                 "tests/helpers/transitive.rkt"
                 'sha256
                 (sha256-file (build-path repo "tests/helpers/transitive.rkt"))))
       (check-not-false (discover repo baseline (append (manifest repo) (list transitive))))
       (fixture-put repo fixture-helper-path original)
       (for ([require-form
              (list "(only-in \"helpers/tracker-evidence.rkt\" valid-tracker-binding-normalizes?)"
                    "\"../../../outside.rkt\"")])
         (fixture-put repo
                      fixture-test-path
                      (string-replace probe "\"helpers/tracker-evidence.rkt\"" require-form))
         (commit repo)
         (check-false (discover repo baseline)))
       (fixture-put repo fixture-test-path probe)
       (fixture-put repo fixture-helper-path original)
       (commit repo)
       ;; Dirty, executable, symlink file and symlink directory are all refused.
       (define approved (manifest repo))
       (fixture-put repo fixture-helper-path (string-append original "\n; drift\n"))
       (check-false (discover repo baseline approved))
       (fixture-put repo fixture-helper-path original)
       (file-or-directory-permissions (build-path repo fixture-helper-path) #o755)
       (commit repo)
       (check-equal? (fixture-git repo "status" "--porcelain" "--untracked-files=all") "")
       (check-false (discover repo baseline approved))
       (file-or-directory-permissions (build-path repo fixture-helper-path) #o644)
       (commit repo)
       (delete-file (build-path repo fixture-helper-path))
       (make-file-or-directory-link "transitive.rkt" (build-path repo fixture-helper-path))
       (commit repo)
       (check-false (discover repo baseline approved))
       (delete-file (build-path repo fixture-helper-path))
       (fixture-put repo fixture-helper-path original)
       (commit repo)
       (rename-file-or-directory (build-path repo "tests/helpers")
                                 (build-path repo "tests/real-helpers"))
       (make-file-or-directory-link "real-helpers" (build-path repo "tests/helpers"))
       (commit repo)
       (check-false (discover repo baseline approved)))))
  (test-case "real owned missing-export assertion RED and identical-byte helper/test GREEN"
    (call-with-supplemental-fixture
     (lambda (repo baseline)
       (define proofs (make-temporary-file "supplemental-owned-~a" 'directory))
       (dynamic-wind
        void
        (lambda ()
          (parameterize ([current-owned-proof-root (build-path proofs "owned")]
                         [current-gsd-verification-registry (make-verification-registry)])
            (define discovered (discover repo baseline))
            (check-not-false discovered)
            ;; The real settings-core dependency closure is intentionally kept;
            ;; allow its cold compilation within the existing schema ceiling.
            (define s (and discovered (hash-set discovered 'timeout-sec 120)))
            (when s
              (define p (prepare-baseline-probe! repo s "missing-export"))
              (define files (append (hash-ref s 'test-files) (hash-ref s 'harness-files)))
              (for ([e files])
                (for ([role '(baseline candidate)])
                  (check-equal? (sha256-file (build-path (hash-ref p role) (hash-ref e 'path)))
                                (hash-ref e 'sha256))))
              (define ref (run-baseline-probe! repo p "missing-export" #:guard (lambda () #t)))
              (unless (hash-has-key? ref 'sha256)
                (for ([job (verification-close-registry! (current-gsd-verification-registry))])
                  (eprintf "owned fixture job ~s: ~a\n"
                           (verification-job-state job)
                           (file->string (verification-job-log-path job)))))
              (check-true (hash-has-key? ref 'sha256) (format "~s" ref))
              (when (hash-has-key? ref 'sha256)
                (define m (validate-baseline-probe! repo ref (fixture-context repo)))
                (check-equal? (hash-ref m 'relation) "baseline-with-test-overlay-v1")
                (define reports (map structured (hash-ref m 'results)))
                (for ([r reports]
                      [failures '(1 0)]
                      [successes '(0 1)])
                  (check-true (hash-ref r 'compile-ok))
                  (check-true (hash-ref r 'load-ok))
                  (check-true (hash-ref r 'suite-completed))
                  (check-equal? (hash-ref r 'errors) 0)
                  (check-equal? (hash-ref r 'failures) failures)
                  (check-equal? (hash-ref r 'successes) successes)
                  (define c (car (hash-ref r 'cases)))
                  (define target (hash-ref s 'target))
                  (check-equal? (hash-ref c 'suite-path) (hash-ref target 'suite-path))
                  (check-equal? (hash-ref c 'test-case-name) (hash-ref target 'test-case-name))
                  (check-equal? (hash-ref c 'check-count) 1)
                  (define identity (car (hash-ref c 'checks)))
                  (check-equal? (hash-ref identity 'name) "check-true")
                  (check-equal? (hash-ref identity 'location) (hash-ref target 'check-location)))
                (define red (car (hash-ref (car reports) 'cases)))
                (check-equal? (hash-ref red 'failure-check) (car (hash-ref red 'checks)))))
            ;; A module setup error must propagate, never become a false-return
            ;; witness. Keep the identical probe/helper and break ONLY baseline.
            (fixture-copy repo "baseline-query.rkt" "runtime/settings-query.rkt")
            (fixture-put repo
                         "runtime/settings-query.rkt"
                         (string-append "#lang racket/base\n(provide gsd-tracker-live-binding)\n"
                                        "(define (gsd-tracker-live-binding settings) #f)\n"
                                        "(error 'fixture-setup \"broken module\")\n"))
            (commit repo)
            (define broken-baseline (fixture-git repo "rev-parse" "HEAD"))
            (fixture-copy repo "candidate-query.rkt" "runtime/settings-query.rkt")
            (commit repo)
            (define broken-discovered (discover repo broken-baseline))
            (define broken-spec (and broken-discovered (hash-set broken-discovered 'timeout-sec 120)))
            (check-not-false broken-spec)
            (when broken-spec
              (define p (prepare-baseline-probe! repo broken-spec "setup-error"))
              (define ref (run-baseline-probe! repo p "setup-error" #:guard (lambda () #t)))
              (check-equal? (hash-ref ref 'status #f) "blocked")
              (check-false (hash-has-key? ref 'sha256))
              ;; Inspect the real retained owned worker result, not fake logs.
              (define reports
                (for/list ([path (in-directory (build-path proofs "owned"))]
                           #:when (and (file-exists? path) (path-has-extension? path #".json")))
                  (call-with-input-file path read-json)))
              (define errors
                (filter (lambda (r)
                          (and (equal? (hash-ref r 'kind #f) "structured-probe-result/v1")
                               (equal? (hash-ref r 'errors #f) 1)))
                        reports))
              (check-equal? (length errors) 1)
              (when (pair? errors)
                (define telemetry (car errors))
                (check-equal? (hash-ref telemetry 'failures) 0)
                (check-equal? (hash-ref (car (hash-ref telemetry 'cases)) 'status) "error")))))
        (lambda () (delete-directory/files proofs))))))
  (test-case "extended focused prefix authenticates the installed runner dependency closure"
    (define repo (make-temporary-file "supplemental-coverage-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (check-true (parameterize ([current-output-port (open-output-nowhere)]
                                  [current-error-port (open-output-nowhere)])
                     (system* (find-executable-path "git")
                              "clone"
                              "--shared"
                              "--no-checkout"
                              (path->string owner)
                              (path->string repo))))
       (define head (fixture-git repo "rev-parse" "HEAD"))
       (define c1
         (string-append
          "raco fmt -i runtime/settings-query.rkt extensions/gsd/wave-executor.rkt && "
          "raco make runtime/settings-query.rkt extensions/gsd/wave-executor.rkt && "
          "racket scripts/run-tests.rkt tests/test-gsd-wave-executor-isolation.rkt "
          "tests/test-gsd-wave-worktree.rkt tests/test-arch-parameters.rkt tests/probe.rkt && "
          "racket scripts/run-tests.rkt --suite fast && racket scripts/lint-all.rkt"))
       (for ([path '("tests/test-gsd-wave-executor-isolation.rkt" "tests/test-gsd-wave-worktree.rkt"
                                                                  "tests/test-arch-parameters.rkt"
                                                                  "tests/probe.rkt")])
         (check-true (regression-command-covered? repo head c1 path)))
       (fixture-git repo "read-tree" "HEAD")
       (fixture-put repo "replacement.rkt" "#lang racket/base\n(void)\n")
       (define blob (fixture-git repo "hash-object" "-w" "replacement.rkt"))
       (fixture-git repo
                    "update-index"
                    "--cacheinfo"
                    (string-append "100644," blob ",scripts/run-tests/cli.rkt"))
       (define tree (fixture-git repo "write-tree"))
       (define altered
         (fixture-git repo
                      "-c"
                      "user.name=Fixture"
                      "-c"
                      "user.email=fixture@invalid"
                      "commit-tree"
                      tree
                      "-p"
                      head
                      "-m"
                      "tampered dependency"))
       (check-false (regression-command-covered? repo altered c1 fixture-test-path)))
     (lambda () (delete-directory/files repo)))))
