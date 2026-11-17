#lang racket
;; @speed fast
;; @suite testing
;; @boundary integration
;; @timeout 300

;; Contract for the completed `./qt` interface table (plan §3 / §11.1):
;;   - `quick --changed BASE..HEAD` is a conservative impact selection over the
;;     AUDITED quick owners, labelled a reproduction (never a PR claim), and it
;;     fails closed on a missing/malformed range, an uncomputable base, an empty
;;     diff and an empty owner selection;
;;   - `default --file FILE` is an explicit reproduction with exactly the
;;     `full --file` label rules (no PR pass, no sharding, no gate evidence);
;;   - `release --candidate SHA` verifies the EXACT candidate: a non-SHA value,
;;     a different HEAD and a dirty checkout are refused before anything runs,
;;     and the release obligations stay unconditional;
;;   - `--lane NAME` dispatches the existing suite/shard spec from the policy
;;     table, fails closed on an unknown lane or a shard-plan lane without
;;     shards, and refuses to be combined with a second selection;
;;   - `./qt --help` lists exactly the implemented commands and options.
;; Only fail-closed paths and plan-only paths are exercised against the real
;; repository; execution and selection logic are tested against fixtures.

(require rackunit
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         json
         "../scripts/testing/full-plan.rkt")

(define-runtime-path qt "../qt")
(define-runtime-path root "..")

;; Read both pipes while the child runs; neither pipe may block the other.
(define (invoke-command timeout cwd executable . args)
  (define cust (make-custodian))
  (define proc #f)
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-custodian cust]
                                 [current-directory cwd]
                                 [subprocess-group-enabled #t])
                    (define-values (child out in err) (apply subprocess #f #f #f executable args))
                    (set! proc child)
                    (close-output-port in)
                    (define stdout #f)
                    (define stderr #f)
                    (define out-thread (thread (lambda () (set! stdout (port->string out)))))
                    (define err-thread (thread (lambda () (set! stderr (port->string err)))))
                    (define done
                      (thread (lambda ()
                                (subprocess-wait child)
                                (thread-wait out-thread)
                                (thread-wait err-thread))))
                    (unless (sync/timeout timeout done)
                      (error 'invoke-command "subprocess timed out after ~a seconds" timeout))
                    (values (subprocess-status child) stdout stderr)))
                (lambda ()
                  (when proc
                    (when (eq? (subprocess-status proc) 'running)
                      (subprocess-kill proc #t))
                    (subprocess-wait proc))
                  (custodian-shutdown-all cust))))

(define (invoke . args)
  (apply invoke-command 300 (simplify-path root) (find-executable-path "racket") qt args))

;; Run qt from an arbitrary working directory: lane selection resolves the
;; repository root from the working tree, so a fixture cwd is a real lane root.
(define (invoke-in cwd . args)
  (apply invoke-command 300 cwd (find-executable-path "racket") qt args))

(define git-exe (find-executable-path "git"))

(define (git? cwd . args)
  (parameterize ([current-directory cwd]
                 [current-output-port (open-output-nowhere)]
                 [current-error-port (open-output-nowhere)])
    (eq? #t (apply system* git-exe args))))

(define (git-line cwd . args)
  (define out (open-output-string))
  (parameterize ([current-directory cwd]
                 [current-output-port out])
    (apply system* git-exe args))
  (string-trim (get-output-string out)))

(define (write-file path text)
  (make-parent-directory* path)
  (call-with-output-file path #:exists 'truncate (lambda (out) (display text out))))

;; A disposable git fixture: one commit, then a second commit adding a file.
(define (with-git-fixture thunk)
  (define dir (make-temporary-file "qt-interface-git-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (write-file (build-path dir "info.rkt") "#lang info\n(define version \"0.0.0\")\n")
                  (git? dir "init" "-q")
                  (git? dir "add" "-A")
                  (git? dir
                        "-c"
                        "user.email=fixture@example.invalid"
                        "-c"
                        "user.name=fixture"
                        "-c"
                        "commit.gpgsign=false"
                        "commit"
                        "-q"
                        "-m"
                        "fixture baseline")
                  (write-file (build-path dir "runtime" "foo.rkt")
                              "#lang racket/base\n(provide foo)\n(define (foo) 1)\n")
                  (git? dir "add" "-A")
                  (git? dir
                        "-c"
                        "user.email=fixture@example.invalid"
                        "-c"
                        "user.name=fixture"
                        "-c"
                        "commit.gpgsign=false"
                        "commit"
                        "-q"
                        "-m"
                        "fixture change")
                  (thunk dir))
                (lambda () (delete-directory/files dir #:must-exist? #f))))

;; `raco test` supplies the RackUnit context through the `test` submodule.
(module+ test

  ;; ---------------------------------------------------------------- help
  (test-case "--help lists exactly the implemented commands"
    (define-values (status out err) (invoke "--help"))
    (check-equal? status 0)
    (check-true (string-contains? out "quick [FILE]"))
    (check-regexp-match #rx"quick --changed BASE\\.\\.HEAD" out)
    (check-regexp-match #rx"default --file FILE" out)
    (check-regexp-match #rx"release --candidate SHA" out)
    (check-regexp-match #rx"--lane NAME" out)
    ;; §3: a bare `./qt` is the default lane, and the usage table says so.
    (check-true (string-contains? out "(none)"))
    (check-true (string-contains? out "Same as `default`"))
    (check-equal? err ""))

  ;; ---------------------------------------------------- bare ./qt = default lane
  ;; §3 interface table: `./qt` runs the mandatory PR selection (`default`). The
  ;; positive case executes a real (one-file) default lane in a fixture root, so
  ;; the plan, the label and the pass line are the default lane's own.
  (define (with-bare-root thunk)
    (define dir (make-temporary-file "qt-bare-default-~a" 'directory))
    (dynamic-wind void
                  (lambda ()
                    (write-file (build-path dir "tests" "a.rkt")
                                (string-append "#lang racket/base\n"
                                               ";; @speed fast\n"
                                               "(require rackunit)\n"
                                               "(module+ test (check-true #t))\n"))
                    (thunk dir))
                  (lambda () (delete-directory/files dir #:must-exist? #f))))

  (test-case "bare ./qt runs the default lane and prints its pass line"
    (with-bare-root (lambda (dir)
                      (define-values (status out err) (invoke-in dir))
                      (check-equal? status 0 (string-append out err))
                      (check-regexp-match #rx"^default: " out)
                      (check-regexp-match
                       #rx"offline PR verification only; platform and GitHub-hosted claims remain with CI"
                       out)
                      (check-regexp-match #rx"default passed: 1 files" out)
                      (check-equal? err ""))))

  (test-case "bare ./qt --explain prints the default plan and executes nothing"
    (with-bare-root (lambda (dir)
                      (define-values (status out err) (invoke-in dir "--explain"))
                      (check-equal? status 0 (string-append out err))
                      (check-regexp-match #rx"^default: " out)
                      (check-regexp-match #rx"selection only" out)
                      (check-true (string-contains? out "tests/a.rkt"))
                      (check-false (regexp-match? #rx"default passed:" out)
                                   "--explain must never execute the lane"))))

  (test-case "bare ./qt with no tests/ fails through default selection, not 'missing command'"
    (define dir (make-temporary-file "qt-bare-empty-~a" 'directory))
    (dynamic-wind void
                  (lambda ()
                    (define-values (status out err) (invoke-in dir))
                    (check-not-equal? status 0)
                    ;; The zero-argument case is a lane dispatch now: it fails in selection
                    ;; (`qt: ...`), never with the retired missing-command refusal.
                    (check-regexp-match #rx"^qt: " err)
                    (check-false (regexp-match? #rx"missing command" err)))
                  (lambda () (delete-directory/files dir #:must-exist? #f))))

  (test-case "bare ./qt with an unsupported option still fails before selection"
    (with-bare-root (lambda (dir)
                      (define-values (status out err) (invoke-in dir "--bogus"))
                      (check-not-equal? status 0)
                      (check-regexp-match #rx"^qt: unsupported option: --bogus" err)
                      (check-false (regexp-match? #rx"default passed:" out)))))

  ;; ------------------------------------------------------- quick --changed
  (test-case "quick --changed without a range fails closed"
    (define-values (status out err) (invoke "quick" "--changed"))
    (check-not-equal? status 0)
    (check-regexp-match #rx"^qt: quick --changed requires BASE\\.\\.HEAD" err))

  (test-case "quick --changed with a malformed range fails closed"
    (define-values (status out err) (invoke "quick" "--changed" "a..b..c" "--explain"))
    (check-not-equal? status 0)
    (check-regexp-match #rx"malformed --changed range" err))

  (test-case "quick --changed with an unusable base fails closed"
    (if git-exe
        (let-values ([(status out err) (invoke "quick" "--changed" "no-such-ref..HEAD" "--explain")])
          (check-not-equal? status 0)
          (check-regexp-match #rx"cannot compute the diff" err)
          (check-regexp-match #rx"base ref no-such-ref does not resolve" err)
          (check-false (regexp-match? #rx"changed-file impact selection" out)
                       "no selection is printed when the diff fails"))
        (displayln "SKIP quick --changed base: git unavailable")))

  (define (parent-commit-available? cwd)
    (and git-exe
         (parameterize ([current-directory cwd]
                        [current-output-port (open-output-nowhere)]
                        [current-error-port (open-output-nowhere)])
           (eq? #t (system* git-exe "rev-parse" "--verify" "--quiet" "HEAD~1^{commit}")))))

  (test-case "quick --changed --explain fails closed on an empty diff"
    ;; Deterministic in every checkout, including CI's shallow (depth 1) clone
    ;; where HEAD~1 does not exist: an empty range is a failure, never a pass.
    (if git-exe
        (let-values ([(status out err) (invoke "quick" "--changed" "HEAD..HEAD" "--explain")])
          (check-not-equal? status 0)
          (check-regexp-match #rx"empty \\(zero selection is a failure, not a pass\\)" err)
          (check-false (regexp-match? #rx"changed-file impact selection" out)
                       "no selection is printed for an empty diff"))
        (displayln "SKIP quick --changed empty diff: git unavailable")))

  (test-case "quick --changed --explain renders the impact selection as a reproduction"
    ;; Needs a real parent commit (CI clones with depth 1): the positive case is
    ;; skipped there, and the fail-closed case above still runs.
    (if (parent-commit-available? (simplify-path root))
        (let-values ([(status out err) (invoke "quick" "--changed" "HEAD~1..HEAD" "--explain")])
          (check-equal? status 0)
          (check-regexp-match #rx"reproduction only, not a PR pass" out)
          (check-regexp-match #rx"changed: commit range HEAD~1\\.\\.HEAD" out)
          (check-regexp-match #rx"  tests/test-" out)
          ;; Plan only: nothing ran.
          (check-false (regexp-match? #rx"quick passed:" out)))
        (displayln "SKIP quick --changed explain: no parent commit (shallow clone)")))

  (test-case "quick --changed selection maps @covers onto the audited owners (fixture)"
    ;; Pure mapping: the owner set is the audited list, the manifest decides the
    ;; attribution, and an unattributable owner is INCLUDED (fail open).
    (define dir (make-temporary-file "qt-interface-owners-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (write-file
        (build-path dir "tests" ".coverage-manifest.json")
        (jsexpr->string
         (hash
          'schema
          "coverage-manifest/v1"
          'generated-at
          "fixture"
          'entries
          (list
           (hash 'test "tests/owner-a.rkt" 'covers (list "util/a.rkt") 'source "manual-review")
           (hash 'test "tests/owner-b.rkt" 'covers (list "util/b.rkt") 'source "manual-review")))))
       (write-file (build-path dir "util" "a.rkt") "#lang racket/base\n(provide a)\n(define (a) 1)\n")
       (write-file (build-path dir "util" "b.rkt") "#lang racket/base\n(provide b)\n(define (b) 2)\n")
       ;; owner-c carries no @covers attribution: it is INCLUDED (fail open).
       (define owners '("tests/owner-a.rkt" "tests/owner-b.rkt" "tests/owner-c.rkt"))
       (define report (quick-changed-owners dir owners '("util/a.rkt") #:base "fixture-base"))
       (check-equal? (change-report-included report) '("tests/owner-a.rkt" "tests/owner-c.rkt"))
       (check-equal? (change-report-excluded report) '("tests/owner-b.rkt"))
       (check-equal? (change-report-unattributable report) '("tests/owner-c.rkt"))
       (check-regexp-match #rx"working-tree diff against fixture-base"
                           (string-join (change-report-lines report) "\n")))
     (lambda () (delete-directory/files dir #:must-exist? #f))))

  ;; -------------------------------------------------- commits-changed truth
  (test-case "commits-changed reports the range and refuses a bad ref"
    (if git-exe
        (with-git-fixture (lambda (dir)
                            (let-values ([(rels label outside) (commits-changed "HEAD~1" "HEAD" dir)])
                              (check-equal? rels '("runtime/foo.rkt"))
                              (check-equal? label "HEAD~1..HEAD")
                              (check-equal? outside '()))
                            (check-exn #rx"does not resolve to a commit"
                                       (lambda () (commits-changed "no-such-ref" "HEAD" dir)))))
        (displayln "SKIP commits-changed: git unavailable")))

  ;; ---------------------------------------------------------- default --file
  (test-case "default --file is an explicit reproduction, never a PR pass"
    (define-values (status out err)
      (invoke "default" "--file" "tests/test-exn-recovery.rkt" "--explain"))
    (check-equal? status 0)
    (check-regexp-match #rx"default: explicit reproduction selection" out)
    (check-regexp-match #rx"label: explicit reproduction only, not a PR pass" out)
    (check-regexp-match #rx"reproduction files: 1" out)
    (check-equal? err ""))

  (test-case "default --file refuses sharding and gate evidence"
    ;; A reproduction is never a coverage claim: the CI-surface flags that
    ;; would shard it or claim a suite PASS are refused BY NAME, not ignored.
    (define-values (status-1 out-1 err-1)
      (invoke "default" "--file" "tests/test-exn-recovery.rkt" "--shard-total" "2"))
    (check-not-equal? status-1 0)
    (check-regexp-match #rx"default does not accept CI-surface option --shard-total" err-1)
    (define-values (status-2 out-2 err-2)
      (invoke "default" "--file" "tests/test-exn-recovery.rkt" "--record-gate-evidence"))
    (check-not-equal? status-2 0)
    (check-regexp-match #rx"default does not accept CI-surface option --record-gate-evidence" err-2))

  (test-case "default refuses a non-reproduction CI flag"
    (define-values (status out err) (invoke "default" "--suite" "fast"))
    (check-not-equal? status 0)
    (check-regexp-match #rx"default does not accept CI-surface option --suite" err))

  ;; ------------------------------------------------------- release --candidate
  (test-case "release --candidate rejects a ref instead of a commit SHA"
    (define-values (status out err) (invoke "release" "--candidate" "HEAD" "--explain"))
    (check-not-equal? status 0)
    (check-regexp-match #rx"--candidate must be a commit SHA" err))

  (test-case "release --candidate refuses a working tree that is not that SHA"
    (define-values (status out err)
      (invoke "release" "--candidate" "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef" "--explain"))
    (check-not-equal? status 0)
    (check-regexp-match #rx"does not match the checkout" err)
    (check-false (regexp-match? #rx"release: complete available" out)
                 "no plan is printed for a different candidate"))

  (test-case "release-candidate-check verifies the exact candidate and refuses a dirty tree"
    (if git-exe
        (with-git-fixture
         (lambda (dir)
           (define head (git-line dir "rev-parse" "HEAD"))
           (check-equal? (release-candidate-check dir head) head)
           (check-equal? (release-candidate-check dir (substring head 0 10)) head)
           (write-file (build-path dir "info.rkt") "#lang info\n(define version \"0.0.1\")\n")
           (check-exn #rx"names a dirty checkout" (lambda () (release-candidate-check dir head)))
           (git? dir "checkout" "--" "info.rkt")
           (check-exn #rx"does not match the checkout"
                      (lambda () (release-candidate-check dir (git-line dir "rev-parse" "HEAD~1"))))))
        (displayln "SKIP release candidate: git unavailable")))

  (test-case "release obligations are still reported for a verified candidate"
    ;; The release lane never prints a pass line it cannot support: with the
    ;; candidate verified, the obligation list is still printed and the exit is
    ;; non-zero. A fixture without tests/ fails at selection, so assert the
    ;; obligation text itself is unconditional (pure).
    (check-true (pair? (release-blocking (release-obligations))))
    (check-true (pair? (release-obligation-lines (release-obligations)))))

  (test-case "only release accepts --candidate"
    (define-values (status out err) (invoke "default" "--candidate" "abc1234"))
    (check-not-equal? status 0)
    (check-regexp-match #rx"default does not accept --candidate" err))

  ;; ---------------------------------------------------------------- --lane
  (test-case "an unknown --lane fails closed and lists the valid lanes"
    (define-values (status out err) (invoke "--lane" "no-such-lane" "--explain"))
    (check-not-equal? status 0)
    (check-regexp-match #rx"unknown --lane: no-such-lane" err)
    (check-regexp-match #rx"valid: fast, fast-shard" err)
    (check-equal? out ""))

  (test-case "--lane selects the policy suite and prints the lane identity"
    (define-values (status out err) (invoke "--lane" "fast" "--explain"))
    (check-equal? status 0)
    (check-regexp-match #rx"lane: fast \\(suite fast;" out)
    (check-regexp-match #rx"suite: fast \\(classify\\.rkt's existing suite predicate" out)
    (check-false (regexp-match? #rx"passed:" out))
    (check-equal? err ""))

  (test-case "--lane workflows comes from the policy table (suite, timeout, shards)"
    (define-values (status out err) (invoke "--lane" "workflows" "--explain"))
    (check-equal? status 0)
    (check-regexp-match #rx"lane: workflows \\(suite workflows; timeout 90; shard-plan #f" out)
    (check-regexp-match #rx"shard: 0 of 2" out))

  (test-case "--lane fast-shard needs more than one shard"
    (define-values (status out err) (invoke "--lane" "fast-shard" "--explain"))
    (check-not-equal? status 0)
    (check-regexp-match #rx"needs --shard-total > 1" err)
    (define-values (status-2 out-2 err-2)
      (invoke "--lane" "fast-shard" "--shard-index" "1" "--shard-total" "3" "--explain"))
    (check-equal? status-2 0)
    (check-regexp-match #rx"shard-plan: active" out-2)
    (check-regexp-match #rx"shard: 1 of 3 \\(duration-aware plan" out-2))

  (test-case "--lane refuses a second selection and the wrong lane commands"
    (define-values (status-1 out-1 err-1) (invoke "--lane" "fast" "--suite" "tui" "--explain"))
    (check-not-equal? status-1 0)
    (check-regexp-match #rx"--lane cannot be combined with --suite" err-1)
    (define-values (status-2 out-2 err-2)
      (invoke "--lane" "fast" "--file" "tests/test-exn-recovery.rkt" "--explain"))
    (check-not-equal? status-2 0)
    (check-regexp-match #rx"--lane cannot be combined with --file" err-2)
    (define-values (status-3 out-3 err-3) (invoke "default" "--lane" "fast" "--explain"))
    (check-not-equal? status-3 0)
    (check-regexp-match #rx"default does not accept --lane" err-3)
    (define-values (status-4 out-4 err-4) (invoke "release" "--lane" "fast" "--explain"))
    (check-not-equal? status-4 0)
    (check-regexp-match #rx"release does not accept --lane" err-4)))
