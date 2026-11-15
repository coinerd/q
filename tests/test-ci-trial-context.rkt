#lang racket/base

;; @speed fast
;; @suite workflows
;; @boundary integration
;; @isolation temp-dir
;; @mutates temp-dir
;; @timeout 180

;; tests/test-ci-trial-context.rkt — testing-framework refactor §11.5.
;;
;; Contract for scripts/ci/trial-context.rkt, which replaces the `python3 - <<PY`
;; heredoc in .github/workflows/ci-recovery-benchmark.yml that wrote
;; trial-context.json before any reusable CI code runs:
;;   - the test-inventory digest is sha256 over the SORTED `git ls-files tests`
;;     lines, each terminated by "\n" (pinned known-answer vector, cross-checked
;;     against `sha256sum`, and reproduced from a real temp git repository);
;;   - the derived fields keep their old semantics exactly: attempt/run-id/
;;     run-attempt ints, shard-total 4/6/8 else 3, shard-plan "0,…,n-1",
;;     created-at-utc in strict UTC `…Z` form;
;;   - the CLI writes exactly the eleven documented keys with the documented
;;     types, and refuses (exit 1, nothing written) on a malformed environment.

(require json
         rackunit
         racket/file
         racket/format
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         "../scripts/ci/trial-context.rkt"
         (only-in "../util/sha256.rkt" sha256-hex))

(define-runtime-path here "test-ci-trial-context.rkt")
(define repo-root (simplify-path (build-path (path-only here) "..")))
(define script (build-path repo-root "scripts" "ci" "trial-context.rkt"))

;; Known-answer vector for "tests/a.rkt\ntests/b.rkt\n", verified independently
;; with `sha256sum` (see the digest test below).
(define two-file-digest "b61411a8adc39fac9897071e15ff621596ba4fe52c0098e7b501d5f175f93a96")
;; Same, for tests/test-a.rkt, tests/test-b.rkt, tests/test-c.rkt.
(define three-file-digest "d236ab4b3d6fd40f65b036e1154f43d336cf7685919c13ccbac691440a380254")

;; git must not inherit the ambient repository plumbing (tests run from a
;; checkout, and GIT_DIR in the environment would retarget every command).
(define (clean-env [overrides '()])
  (define src (current-environment-variables))
  (define dst (make-environment-variables))
  (for ([name (in-list (environment-variables-names src))])
    (environment-variables-set! dst name (environment-variables-ref src name)))
  (for ([name (in-list '(#"GIT_DIR" #"GIT_INDEX_FILE" #"GIT_WORK_TREE"
                                  #"GIT_COMMON_DIR" #"GIT_OBJECT_DIRECTORY"
                                  #"GIT_ALTERNATE_OBJECT_DIRECTORIES" #"GIT_PREFIX"
                                  #"TRIAL_ID" #"TRIAL_ATTEMPT" #"TREATMENT" #"RUN_ID"
                                  #"RUN_ATTEMPT" #"DISPATCH_SHA" #"WORKFLOW_SHA"
                                  #"FAST_SHARD_COUNT"))])
    (environment-variables-set! dst name #f))
  (for ([pair (in-list overrides)])
    (environment-variables-set! dst (string->bytes/utf-8 (car pair))
                                (string->bytes/utf-8 (cdr pair))))
  dst)

;; Runs one child process with a clean environment; returns (values code output).
(define (run-process argv env-vars)
  (define out (open-output-string))
  (define code
    (parameterize ([current-output-port out]
                   [current-error-port out]
                   [current-environment-variables env-vars])
      (apply system*/exit-code argv)))
  (values code (get-output-string out)))

(define (git root . args)
  (define-values (code out)
    (run-process (append (list (find-executable-path "git")
                               "-C"
                               (path->string root))
                         args)
                 (clean-env)))
  (check-equal? code 0 (format "git ~a failed: ~a" args out))
  out)

(define (write-text! path text)
  (make-directory* (path-only path))
  (call-with-output-file path (lambda (o) (display text o)) #:exists 'truncate))

;; Run the CLI with the trial environment; returns (values code output).
(define (run-cli root out-path env-vars)
  (run-process (list (find-executable-path "racket")
                     (path->string script)
                     "--out"
                     (path->string out-path)
                     "--root"
                     (path->string root))
               (clean-env env-vars)))

(define trial-env
  (list (cons "TRIAL_ID" "trial-2026-10-30-c1")
        (cons "TRIAL_ATTEMPT" "2")
        (cons "TREATMENT" "C")
        (cons "RUN_ID" "1234567890")
        (cons "RUN_ATTEMPT" "3")
        (cons "DISPATCH_SHA" "8299409c9cf6330a85e66cbe5a9ba445bf739d27")
        (cons "WORKFLOW_SHA" "0123456789abcdef0123456789abcdef01234567")
        (cons "FAST_SHARD_COUNT" "6")))

(define trial-context-suite
  (test-suite
   "scripts/ci/trial-context.rkt"

   ;; -------------------------------------------------------------- digest

   (test-case "the inventory digest sorts its lines and terminates each with a newline"
     (check-equal? (inventory-digest '("tests/b.rkt" "tests/a.rkt")) two-file-digest)
     ;; order of the input never matters
     (check-equal? (inventory-digest '("tests/a.rkt" "tests/b.rkt")) two-file-digest))

   (test-case "the pinned digest matches an independent sha256 implementation"
     ;; `sha256sum` over the same bytes is the cross-check the collector's
     ;; `git ls-files tests | LC_ALL=C sort | sha256sum` relies on.
     (define tmp (make-temporary-file "q-trial-digest-~a.txt"))
     (dynamic-wind
      void
      (lambda ()
        (write-text! tmp "tests/a.rkt\ntests/b.rkt\n")
        (define-values (code out)
          (run-process (list (find-executable-path "sha256sum") (path->string tmp))
                       (clean-env)))
        (check-equal? code 0 out)
        (check-true (string-prefix? out two-file-digest) out))
      (lambda () (when (file-exists? tmp) (delete-file tmp)))))

   (test-case "the digest covers the trailing newline of the last line"
     ;; A missing final newline is a different byte string: the digest must not
     ;; quietly accept a concatenation that dropped it.
     (check-not-equal? (inventory-digest '("tests/a.rkt"))
                       (sha256-hex (string->bytes/utf-8 "tests/a.rkt"))))

   ;; ------------------------------------------------------- derived fields

   (test-case "shard-total keeps the documented 4/6/8-else-3 guard"
     (check-equal? (shard-total "4") 4)
     (check-equal? (shard-total "6") 6)
     (check-equal? (shard-total "8") 8)
     (check-equal? (shard-total "3") 3)
     (check-equal? (shard-total #f) 3)
     (check-equal? (shard-total "garbage") 3))

   (test-case "shard-plan is the comma-joined 0..n-1 plan"
     (check-equal? (shard-plan 3) "0,1,2")
     (check-equal? (shard-plan 4) "0,1,2,3")
     (check-equal? (shard-plan 8) "0,1,2,3,4,5,6,7"))

   (test-case "created-at-utc is strict UTC with a trailing Z"
     ;; NOTE: this repository's Racket spells counted repetition `{4}` as a
     ;; literal brace, so the grammar is pinned with explicit character
     ;; classes — a `{4}` here would silently match nothing.
     (define (utc-shaped? s)
       (and (= (string-length s) 20)
            (regexp-match?
             #rx"^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9]Z$"
             s)
            #t))
     (check-equal? (created-at-utc 0) "1970-01-01T00:00:00Z")
     (check-true (utc-shaped? (created-at-utc)))
     (check-true (utc-shaped? (created-at-utc 1767225599))))

   ;; ---------------------------------------------------------- integration

   (test-case "a real git repository yields the documented document"
     (define root (make-temporary-file "q-trial-ctx-~a" 'directory))
     (dynamic-wind
      void
      (lambda ()
        (git root "init" "-q")
        ;; Added out of order on purpose: the digest is over the sorted list.
        (write-text! (build-path root "tests" "test-c.rkt") "#lang racket/base\n")
        (write-text! (build-path root "tests" "test-a.rkt") "#lang racket/base\n")
        (write-text! (build-path root "tests" "test-b.rkt") "#lang racket/base\n")
        (git root "add" "tests")
        (define out-path (build-path root "trial-context.json"))
        (define-values (code out) (run-cli root out-path trial-env))
        (check-equal? code 0 out)
        (define doc (call-with-input-file out-path read-json))
        (check-equal? (sort (map ~a (hash-keys doc)) string<?)
                      '("attempt" "created-at-utc" "head-sha" "run-attempt" "run-id"
                        "shard-plan" "shard-total" "test-inventory-digest" "treatment"
                        "trial-id" "workflow-revision-sha"))
        (check-equal? (hash-ref doc 'trial-id) "trial-2026-10-30-c1")
        (check-equal? (hash-ref doc 'attempt) 2)
        (check-equal? (hash-ref doc 'treatment) "C")
        (check-equal? (hash-ref doc 'run-id) 1234567890)
        (check-equal? (hash-ref doc 'run-attempt) 3)
        (check-equal? (hash-ref doc 'head-sha) "8299409c9cf6330a85e66cbe5a9ba445bf739d27")
        (check-equal? (hash-ref doc 'workflow-revision-sha)
                      "0123456789abcdef0123456789abcdef01234567")
        (check-equal? (hash-ref doc 'shard-total) 6)
        (check-equal? (hash-ref doc 'shard-plan) "0,1,2,3,4,5")
        (check-equal? (hash-ref doc 'test-inventory-digest) three-file-digest)
        (check-equal? (string-length (hash-ref doc 'created-at-utc)) 20)
        (check-true (regexp-match?
                     #rx"^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9]Z$"
                     (hash-ref doc 'created-at-utc))))
      (lambda () (delete-directory/files root #:must-exist? #f))))

   (test-case "an unvalidated trial identity is refused and writes nothing"
     (define root (make-temporary-file "q-trial-ctx-~a" 'directory))
     (dynamic-wind
      void
      (lambda ()
        (git root "init" "-q")
        (write-text! (build-path root "tests" "test-a.rkt") "#lang racket/base\n")
        (git root "add" "tests")
        (define out-path (build-path root "trial-context.json"))
        ;; attempt is not a number: the old heredoc raised on int(), this must
        ;; refuse too — never publish a half-bound context.
        (define bad-env (append trial-env (list (cons "TRIAL_ATTEMPT" "not-a-number"))))
        (define-values (code out) (run-cli root out-path bad-env))
        (check-equal? code 1 out)
        (check-false (file-exists? out-path))
        ;; a missing field is refused as well
        (define missing-env
          (for/list ([pair (in-list trial-env)] #:unless (equal? (car pair) "RUN_ID")) pair))
        (define-values (code2 out2) (run-cli root out-path missing-env))
        (check-equal? code2 1 out2)
        (check-false (file-exists? out-path)))
      (lambda () (delete-directory/files root #:must-exist? #f))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests trial-context-suite))

(module+ main
  (require rackunit/text-ui)
  (exit (run-tests trial-context-suite)))
