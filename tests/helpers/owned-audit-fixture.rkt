#lang racket/base
;; Real owned-Verify world builder for the owned-audit integration suites
;; (tests/test-gsd-owned-audit.rkt) and the production coordinator fixture
;; (tests/test-gsd-auto-review-production.rkt).
;;
;; Every command a world executes is a REAL repository-spelled command:
;; `raco fmt -i` (in-place canonical format), `raco make` (compile), the
;; fixture's honest `scripts/run-tests.rkt` / `scripts/lint-all.rkt` runners,
;; and the real frozen Verify chain executed by the owned verification
;; registry. There is no shimmed or fictional CLI anywhere here.
;;
;; Two producers are genuine, never templated:
;;   * `register-owned-red!` runs ONE declared focused stage of the frozen
;;     Verify declaration at the committed pre-fix head through the singleton
;;     registry and retains the authentic negative proof (raw failure log
;;     included);
;;   * `produce-owned-green!` runs the whole frozen chain through
;;     `verify-campaign-delivery`, so the green receipt carries a real owned
;;     proof reference.
;;
;; Worlds are built ONCE per suite and shared by that suite's cases (a world
;; build is a full owned Verify execution). They are deliberately NOT copied
;; per case: an owned manifest authenticates the absolute `execution-cwd` /
;; `execution-base` it was produced under, so a copy would either break the
;; authentication (tampering) or be a different world. Cases restore every
;; mutation they make.
;;
;; The owned proof store is kept out of the real user home through a
;; fixture-local HOME (the same discipline as
;; tests/test-gsd-auto-review-loop.rkt): `initialize-owned-home!` re-points the
;; process HOME and pins PLTADDONDIR to the real Racket user scope, so BOTH the
;; in-process capture (`current-owned-proof-root`) and the controller's
;; `gsd-binding-data.rkt` subprocesses resolve the same trusted root.
(require json
         racket/file
         racket/format
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         racket/system
         file/sha1
         openssl
         "../../extensions/gsd/review-proof.rkt"
         "../../extensions/gsd/verification-job.rkt"
         "../../extensions/gsd/composition-root.rkt"
         "../../extensions/gsd/campaign-state.rkt"
         "../../extensions/gsd/plan-snapshot.rkt"
         "../../extensions/gsd/delivery-verifier.rkt"
         "../../extensions/gsd/delivery-receipt.rkt"
         "../../extensions/gsd/evidence-readiness.rkt"
         "../../scripts/gsd-binding-data.rkt"
         "../../sandbox/subprocess.rkt")

(provide ;; fixture-local owned home / trusted proof root
         initialize-owned-home!
         owned-home
         owned-proof-root
         with-owned-proof-root
         ;; primitives
         git!
         git-raw!
         put-text!
         jstring
         put-json!
         read-json-file
         read-datum!
         write-datum!
         run-gate!
         binding-data-script
         ;; fixture file contents (the real gates the frozen chain spells)
         policy-names
         slug
         wave-branch
         owned-chain
         production-chain
         widget-buggy
         widget-fixed
         fixture-runner
         fixture-lint
         focused-test
         regression-suite
         ;; world construction
         build-campaign!
         build-git-world!
         install-installed-runner!
         build-owned-world
         ;; owned producers
         register-owned-red!
         produce-owned-green!
         owned-world-authority
         ownership-record
         ;; production coordinator fixture
         build-production-owned-review-world)

;; ============================================================
;; Constants (mirror the deleted Python fixtures byte for byte)
;; ============================================================

(define policy-names '("lint" "test (0)" "workflows (0)"))
(define slug "owner/repo")
(define wave-branch "campaign/v9.9.9-w1")
(define production-branch "campaign/production-review")

;; The frozen Verify chain spells REAL repository commands only (the same
;; grammar as the frozen W1 declaration): in-place canonical format, compile,
;; the focused positional test stage, the full fast suite through the real
;; --suite CLI flag, and the whole-repository lint entrypoint.
(define owned-chain
  (string-append "cd <project-base>/q && raco fmt -i tests/w1-fixture.rkt "
                 "src/gsd-fixture-widget.rkt && raco make tests/w1-fixture.rkt "
                 "&& racket scripts/run-tests.rkt tests/w1-fixture.rkt "
                 "&& racket scripts/run-tests.rkt --suite fast "
                 "&& racket scripts/lint-all.rkt"))

;; The production coordinator world freezes the same chain, minus the
;; regression file the production wave does not add (its focus stage is the
;; compilation of the widget itself).
(define production-chain
  (string-append "cd <project-base>/q && raco fmt -i src/gsd-fixture-widget.rkt "
                 "&& raco make src/gsd-fixture-widget.rkt "
                 "&& racket scripts/run-tests.rkt tests/w1-fixture.rkt "
                 "&& racket scripts/run-tests.rkt --suite fast "
                 "&& racket scripts/lint-all.rkt"))

(define widget-buggy
  "#lang racket/base
;; Fixture widget: deliberately wrong scoring rule (pre-fix RED state).
(provide widget-score)
(define (widget-score n)
  (* n 2))
")

(define widget-fixed
  "#lang racket/base
;; Fixture widget: correct scoring rule (post-fix green state).
(provide widget-score)
(define (widget-score n)
  (* n n))
")

;; The focused stage must FAIL at the pre-fix head (authentic behavioral RED)
;; and pass at the fixed head.
(define focused-test
  "#lang racket/base
(require rackunit \"../src/gsd-fixture-widget.rkt\")
(module+ test
  (check-equal? (widget-score 3) 9 \"widget-score must square the input\")
  (check-equal? (widget-score 0) 0 \"widget-score must square zero\"))
")

(define fixture-runner
  "#lang racket/base
;; Fixture gate runner for the owned-audit integration fixture ONLY.
;; Runs the named focused test files (or, for --suite fast, every tests/*.rkt
;; file in this fixture repository) through the REAL `raco test` runner,
;; fail-fast, propagating the real exit code. This is NOT the repository's
;; production test runner.
(require racket/list racket/system)
(define args (vector->list (current-command-line-arguments)))
(define files
  (cond
    [(null? args) (error 'run-tests \"fixture runner requires test files or --suite fast\")]
    [(equal? (car args) \"--suite\")
     (unless (equal? (cdr args) '(\"fast\"))
       (error 'run-tests \"fixture runner supports only --suite fast\"))
     (sort (for/list ([p (directory-list \"tests\")] #:when (regexp-match? #rx\"[.]rkt$\" p))
             (build-path \"tests\" p))
           path<?)]
    [else (map string->path args)]))
(for ([path (in-list files)])
  (define code (system*/exit-code (find-executable-path \"raco\") \"test\" (path->string path)))
  (unless (zero? code) (exit code)))
")

(define fixture-lint
  "#lang racket/base
;; Fixture whole-repository lint for the owned-audit integration fixture
;; ONLY: real checks (no tabs, no trailing whitespace, no line over 100
;; columns) over this fixture's Racket sources. NOT the production linter.
(require racket/file racket/list racket/string)
(define files
  (sort (append (for/list ([p (directory-list \"tests\")] #:when (regexp-match? #rx\"[.]rkt$\" p))
                  (build-path \"tests\" p))
                (for/list ([p (directory-list \"src\")] #:when (regexp-match? #rx\"[.]rkt$\" p))
                  (build-path \"src\" p)))
        path<?))
(define (file-violations path)
  (for/list ([line (in-list (file->lines path))]
             [n (in-naturals 1)]
             #:when (or (string-contains? line \"\\t\")
                        (regexp-match? #rx\"[[:space:]]$\" line)
                        (> (string-length line) 100)))
    (format \"~a:~a\" path n)))
(define bad
  (for*/fold ([acc '()]) ([path (in-list files)])
    (append (file-violations path) acc)))
(unless (null? bad)
  (eprintf \"fixture lint failures:\\n~a\\n\" (string-join bad \"\\n\"))
  (exit 1))
")

;; The production world's regression suite: a real RackUnit suite so the
;; installed runner has something honest to execute.
(define regression-suite
  "#lang racket/base
;; @speed fast
;; @suite fixture
(require rackunit \"../src/gsd-fixture-widget.rkt\")
(provide evidence-suite)
(define evidence-suite
  (test-suite \"widget\"
    (test-case \"squares input\" (check-equal? (widget-score 3) 9))
    (test-case \"squares zero\" (check-equal? (widget-score 0) 0))))
(module+ test
  (require rackunit/text-ui)
  (exit (run-tests evidence-suite)))
")

;; ============================================================
;; Fixture-local HOME + trusted owned-proof root
;; ============================================================

(define owned-home (make-temporary-file "q-owned-audit-home-~a" 'directory))
(define owned-proof-root (build-path owned-home ".q" "agent" "owned-review-proofs"))
;; Resolved BEFORE the fixture HOME is installed: the real Racket user scope
;; must stay readable to every fixture subprocess (raco fmt/make, raco test).
(define real-addon-dir (path->directory-path (find-system-path 'addon-dir)))
(define home-installed? (box #f))

(define (initialize-owned-home!)
  (unless (unbox home-installed?)
    (define env (current-environment-variables))
    (environment-variables-set! env #"HOME" (path->bytes owned-home))
    (environment-variables-set! env #"PLTADDONDIR" (path->bytes real-addon-dir))
    ;; The deleted Python fixture disabled its process budget outright
    ;; (`PROCESS_BUDGET_SEC = 10**6`). The native seam clamps the budget to
    ;; [10, 235] (`runtime-caller-budget - 5`), so a fixture world pins the
    ;; ceiling instead: the fixture measures production laws, not a loaded
    ;; host's scheduling latency.
    (environment-variables-set! env #"GSD_DELIVERY_BUDGET_SEC" #"235")
    (set-box! home-installed? #t))
  owned-proof-root)

(define (with-owned-proof-root thunk)
  (initialize-owned-home!)
  (parameterize ([current-owned-proof-root owned-proof-root])
    (thunk)))

;; ============================================================
;; Primitives
;; ============================================================

(define-runtime-path binding-data-script "../../scripts/gsd-binding-data.rkt")
(define-runtime-path wave-gate-script "../../scripts/gsd-wave-gate.rkt")
(define-runtime-path auto-review-owned-fixture "auto-review-owned-fixture.rkt")

(define git-executable (find-executable-path "git"))
(define racket-executable (find-executable-path "racket"))
(define raco-executable (find-executable-path "raco"))

(define (subprocess-failed? result)
  (or (subprocess-result-timed-out? result)
      (not (zero? (subprocess-result-exit-code result)))))

(define (git* dir . args)
  (define argv (append (if dir (list "-C" (path->string dir)) '()) args))
  (define result
    (run-subprocess (path->string git-executable) #:args argv #:timeout 120))
  (when (subprocess-failed? result)
    (error 'fixture-git "git ~s failed: ~a" argv (subprocess-result-stderr result)))
  result)

;; Trimmed stdout, like the Python fixture's `sh`.
(define (git! dir . args)
  (string-trim (subprocess-result-stdout (apply git* dir args))))

;; RAW stdout (never trimmed): content digests are computed over exact bytes.
(define (git-raw! dir . args)
  (subprocess-result-stdout (apply git* dir args)))

(define (run-command! executable dir args what)
  (define result
    (run-subprocess (path->string executable) #:args args #:directory dir #:timeout 180))
  (when (subprocess-failed? result)
    (error 'fixture
           "~a failed (~a): ~a"
           what
           (subprocess-result-exit-code result)
           (subprocess-result-stderr result)))
  result)

(define (raco-fmt-in! dir . files)
  (run-command! raco-executable dir (append (list "fmt" "-i") files) "raco fmt -i"))

(define (put-text! path text)
  (make-directory* (path-only path))
  (call-with-output-file path #:exists 'truncate (lambda (out) (display text out))))

(define (jstring value)
  (let ([out (open-output-string)])
    (write-json value out)
    (get-output-string out)))

(define (put-json! path value)
  (put-text! path (jstring value)))

(define (read-json-file path)
  (call-with-input-file path read-json))

;; One committed `.rktd` datum through the trusted reader.
(define (read-datum! path)
  (read-hash-datum (path->string path)))

;; One `.rktd` datum through the trusted writer (never string-built syntax).
(define (write-datum! datum path)
  (define payload (make-temporary-file "q-owned-datum-~a.json"))
  (dynamic-wind
   void
   (lambda ()
     (put-json! payload datum)
     (define result
       (run-subprocess (path->string racket-executable)
                       #:args (list (path->string binding-data-script)
                                    "write-datum"
                                    (path->string payload)
                                    (path->string path))
                       #:timeout 120))
     (when (subprocess-failed? result)
       (error 'fixture "write-datum failed: ~a" (subprocess-result-stderr result)))
     path)
   (lambda () (when (file-exists? payload) (delete-file payload)))))

(define (run-gate! evidence args)
  (run-subprocess (path->string racket-executable)
                  #:args (append (list (path->string wave-gate-script)
                                       (path->string evidence))
                                 args)
                  #:timeout 300))

;; ============================================================
;; Campaign fixture (real frozen snapshot through the real API)
;; ============================================================

(define (build-campaign! base
                        plan
                        wave-index
                        #:branch [branch wave-branch]
                        #:heading [heading "# Fixture plan"]
                        #:wave-override [wave-override #f]
                        #:snapshot? [snapshot? #t])
  (define root (build-path base "campaign"))
  (define planning (build-path root ".planning"))
  (define plan-text
    (format "~a\n\nSingle wave: waves/W~a-fixture.md owns the delivery.\n"
            heading
            wave-index))
  (define wave-text
    (or wave-override
        (format (string-append "# W~a: Fixture wave\n\nBranch: `~a`; GitHub issue "
                               "[#9686](https://github.com/~a/issues/9686), milestone #895.\n\n"
                               "## Files\n\n"
                               "- File: `docs/reports/gsd-wave-evidence/v9.9.9-w~a.rktd`\n"
                               "- File: `docs/reports/gsd-wave-reviews/v9.9.9-w~a.rktd`\n"
                               "- File: `docs/reports/gsd-wave-validation/v9.9.9-w~a.rktd`\n")
                wave-index branch slug wave-index wave-index wave-index)))
  (put-text! (build-path planning "PLAN.md") plan-text)
  (put-text! (build-path planning (format "waves/W~a-fixture.md" wave-index)) wave-text)
  (when snapshot?
    (make-plan-snapshot! root plan plan-text))
  root)

;; ============================================================
;; Real Git world (origin bare + work clone + subject clone)
;; ============================================================

;; The subject clone sees its `origin` as the fixture's bare remote through an
;; `insteadOf` rewrite (the logical GitHub identity the controller expects);
;; the copy is therefore always resolved locally, never over the network.
(define (build-git-world! base)
  (define origin (build-path base "origin.git"))
  (define work (build-path base "work"))
  (define subject (build-path base "subject"))
  (make-directory* base)
  (git! #f "init" "--bare" "-b" "main" (path->string origin))
  (git! origin "config" "gc.auto" "0")
  (git! #f "init" "-b" "main" (path->string work))
  (for ([dir (in-list (list work))])
    (git! dir "config" "user.email" "fixture@example.com")
    (git! dir "config" "user.name" "fixture")
    (git! dir "config" "gc.auto" "0"))
  (put-text! (build-path work "scripts/required-pr-checks.policy")
             (format "(\"~a\")" (string-join policy-names "\" \"")))
  (put-text! (build-path work "src/file.txt") "one\n")
  (git! work "add" "-A")
  (git! work "commit" "-q" "-m" "c0")
  (define c0 (git! work "rev-parse" "HEAD"))
  (git! work "push" "-q" (path->string origin) "main:main")
  (hash 'base base 'origin origin 'work work 'subject subject 'c0 c0))

(define (clone-subject! world)
  (define origin (hash-ref world 'origin))
  (define subject (hash-ref world 'subject))
  (git! #f "clone" "-q" "--branch" "main" (path->string origin) (path->string subject))
  (git! subject "remote" "set-url" "origin" (format "https://github.com/~a.git" slug))
  (git! subject "config" (format "url.~a.insteadOf" (path->string origin))
        (format "https://github.com/~a.git" slug))
  (git! subject "config" "user.email" "fixture@example.com")
  (git! subject "config" "user.name" "fixture")
  (for ([dir (in-list (list (hash-ref world 'origin) subject))])
    (git! dir "config" "gc.auto" "0")
    (git! dir "config" "maintenance.auto" "false"))
  subject)

;; Discovery authenticates the installed runner's COMPLETE dependency closure,
;; not a fixture-local CLI that merely happens to run green checks. The
;; production world installs the real `scripts/run-tests.rkt` and every
;; repository-relative `.rkt` it names.
(define-runtime-path this-helper "owned-audit-fixture.rkt")

;; The installed runner's dependency closure is read from the repository that
;; owns this helper (the same tree the runner under test lives in).
(define (install-installed-runner! repo)
  (define installed-root
    (simplify-path (build-path (path-only this-helper) 'up 'up)))
  (define seen (make-hash))
  (define (install relative)
    (define source (simplify-path (path->complete-path (build-path installed-root relative))))
    (unless (hash-has-key? seen source)
      (hash-set! seen source #t)
      (define in-tree (find-relative-path installed-root source))
      (when (absolute-path? in-tree)
        (error 'fixture "installed runner dependency escapes the install tree: ~a" relative))
      (define text (file->string source))
      (put-text! (build-path repo in-tree) text)
      (for ([dependency (in-list (regexp-match* #rx"\"([^\"\n]+[.]rkt)\"" text #:match-select cadr))])
        ;; A quoted string can spell an ABSOLUTE `.rkt` path (e.g. an
        ;; unrelated literal in a format string). Python joined it and only
        ;; installed it when the join was a real file — an absolute literal is
        ;; never one of this tree's dependencies, so it is skipped here too.
        (define candidate
          (and (not (absolute-path? (string->path dependency)))
               (simplify-path (build-path (path-only source) dependency))))
        (when (and candidate (file-exists? candidate))
          (install (find-relative-path installed-root candidate))))))
  (install (string->path "scripts/run-tests.rkt")))

;; ============================================================
;; Owned world
;; ============================================================

;; The frozen snapshot of one wave is immutable, so every world carries its
;; own plan identity; two worlds never share an owned-proof store subdirectory.
(define (fresh-plan-id seed)
  (bytes->hex-string
   (sha256-bytes (string->bytes/utf-8 (format "~a-~a-~a" seed
                                              (current-inexact-milliseconds)
                                              (random))))))

(define (ownership-record plan wave-index)
  (make-campaign-record
   plan
   (make-campaign-manifest 1 "fixture" '() '() "constraints")
   (list (make-campaign-wave wave-index
                             "fixture"
                             'awaiting-delivery
                             1
                             (campaign-attempt "attempt-5" 2 0)))
   #f
   2
   'test
   0
   0))

;; The live zero-argument guard the owned RED producer mandates: still the same
;; attempt/fence and neither cancelled nor paused.
(define (live-guard record wave-index)
  (lambda ()
    (and (not (campaign-record-cancellation record))
         (not (campaign-record-budget-pause record))
         (= (campaign-fence-token record) 2)
         (for/or ([w (in-list (campaign-record-waves record))])
           (define a (campaign-wave-current-attempt w))
           (and (= wave-index (campaign-wave-index w))
                a
                (equal? (campaign-attempt-id a) "attempt-5")
                (= (campaign-attempt-fence-token a) 2))))))

(define (committed-context repo)
  (define snapshot (committed-delivery-snapshot repo))
  (make-branch-delivery-context #:repo-root (hash-ref snapshot 'repo)
                                #:branch (hash-ref snapshot 'branch)
                                #:base-commit (hash-ref snapshot 'head)
                                #:worktree-path repo))

;; Authentic behavioral RED: a fresh registered execution of the declared
;; focused stage at the committed pre-fix head, retained with its exact source
;; identity, job facts and raw log. Returns the retained red reference (#f when
;; the job was not a genuine assertion failure).
(define (register-owned-red! campaign-root plan wave-index cwd
                             [focused-command "racket scripts/run-tests.rkt tests/w1-fixture.rkt"])
  (define record (ownership-record plan wave-index))
  (define context (committed-context cwd))
  (parameterize ([current-gsd-verification-registry (make-verification-registry)]
                 [current-gsd-delivery-verify-timeout-sec 120]
                 [current-gsd-delivery-branch-context context])
    (define-values (job red-ref)
      (run-owned-red-verification campaign-root
                                  plan
                                  wave-index
                                  "attempt-5"
                                  2
                                  cwd
                                  focused-command
                                  #:guard (live-guard record wave-index)))
    (unless red-ref
      (error 'fixture
             "actual owned fixture RED execution was not retained: state=~a exit=~a log=~a"
             (verification-job-state job)
             (verification-job-exit-code job)
             (verification-job-log-path job)))
    red-ref))

;; The whole frozen chain through the owned registry: the green receipt the
;; audit authenticates.
(define (produce-owned-green! campaign-root plan wave-index cwd)
  (define record (ownership-record plan wave-index))
  (define context (committed-context cwd))
  (parameterize ([current-gsd-verification-registry (make-verification-registry)]
                 [current-gsd-delivery-verify-timeout-sec 180])
    (define result
      (verify-campaign-delivery
       campaign-root
       plan
       wave-index
       cwd
       context
       (lambda (_)
         (define check (check-verify-command campaign-root wave-index #f))
         (delivery-verification (cadr check) (list check) "actual fixture owned Verify"))
       (lambda () record)
       #:remote-published (lambda (_repo _branch _head) #t)))
    (unless (delivery-verification-approved? result)
      (error 'fixture "actual owned fixture execution failed: ~a" result))
    result))

;; The authenticated owned-proof authority for this plan/wave (the same export
;; the controller's `gsd-binding-data.rkt owned-review` action performs).
(define (owned-world-authority campaign-root plan wave-index staging)
  (owned-review-authority campaign-root plan wave-index staging))

(define (journal-path campaign-root plan wave-index)
  (build-path campaign-root
              ".planning"
              "campaigns"
              plan
              (format "coordinator-w~a.json" wave-index)))

;; One complete owned world: buggy committed implementation + failing focused
;; test at head1 (RED), the frozen Verify chain executed at head1 (green proof
;; for the pre-fix tree would be wrong), then the same function fixed at head2
;; (green). The wave worktree lives at `<campaign>/q`, exactly like a real
;; campaign checkout.
(define (build-owned-world base
                          #:plan [plan #f]
                          #:wave [wave-index 1]
                          #:branch [branch wave-branch]
                          #:chain [chain owned-chain]
                          #:with-red? [with-red? #t])
  (define plan-id (or plan (fresh-plan-id (format "~a" base))))
  (define world (build-git-world! base))
  (define work (hash-ref world 'work))
  (define origin (hash-ref world 'origin))
  ;; The implementation branch is the wave branch: buggy committed behavior
  ;; plus the failing focused test (the authentic RED state at head1).
  (git! work "checkout" "-q" "-b" "impl")
  (put-text! (build-path work ".gitignore") "compiled/\n")
  (put-text! (build-path work "src/gsd-fixture-widget.rkt") widget-buggy)
  (put-text! (build-path work "tests/w1-fixture.rkt") focused-test)
  (put-text! (build-path work "scripts/run-tests.rkt") fixture-runner)
  (put-text! (build-path work "scripts/lint-all.rkt") fixture-lint)
  ;; Canonicalize formatting BEFORE committing: the frozen `raco fmt -i` stage
  ;; then runs for real as a genuine no-op (clean before/after).
  (raco-fmt-in! work
                "tests/w1-fixture.rkt"
                "src/gsd-fixture-widget.rkt"
                "scripts/run-tests.rkt"
                "scripts/lint-all.rkt")
  (git! work "add" "--" ".")
  (git! work "commit" "-q" "-m" "fixture: buggy widget + failing focused test")
  (define head1 (git! work "rev-parse" "HEAD"))
  (git! work "push" "-q" (path->string origin) (format "impl:refs/heads/~a" branch))
  (define subject (clone-subject! world))
  (define campaign
    (build-campaign! base
                     plan-id
                     wave-index
                     #:branch branch
                     #:heading (format "# Fixture plan\n\n## Waves\n\n- [Inbox] W~a: fixture → waves/W~a-fixture.md"
                                       wave-index
                                       wave-index)
                     #:wave-override (format (string-append "# W~a: fixture\n\nBranch: `~a`; GitHub issue "
                                                            "[#9686](https://github.com/~a/issues/9686), "
                                                            "milestone #895.\n\n## Verify\n~a\n")
                                             wave-index
                                             branch
                                             slug
                                             chain)))
  (git! subject "fetch" "-q" "origin" (format "refs/heads/~a" branch))
  (git! subject "worktree" "add" "-b" branch (path->string (build-path campaign "q")) head1)
  (define q (build-path campaign "q"))
  (when with-red?
    (register-owned-red! campaign plan-id wave-index q))
  (put-text! (build-path q "src/gsd-fixture-widget.rkt") widget-fixed)
  (raco-fmt-in! q "src/gsd-fixture-widget.rkt")
  (git! q "add" "--" "src")
  (git! q "commit" "-q" "-m" "fixture: correct widget scoring (fix)")
  (define head2 (git! q "rev-parse" "HEAD"))
  (git! q "push" "-q" (path->string origin) (format "~a:refs/heads/~a" branch branch))
  (produce-owned-green! campaign plan-id wave-index q)
  (hash 'label (path->string base)
        'base base
        'world world
        'origin origin
        'work work
        'subject subject
        'campaign campaign
        'q q
        'plan plan-id
        'wave wave-index
        'branch branch
        'chain chain
        'c0 (hash-ref world 'c0)
        'head1 head1
        'head2 head2
        'with-red? with-red?
        'store (build-path (current-owned-proof-root) plan-id)
        'staging (build-path campaign
                             ".planning"
                             "campaigns"
                             plan-id
                             (format "review-w~a-g0" wave-index))
        'journal (journal-path campaign plan-id wave-index)))

;; ============================================================
;; Production coordinator fixture
;; ============================================================

;; Port of the deleted `tests/helpers/production-owned-review-fixture.py`:
;; a real declared owned-Verify world for the production coordinator with a
;; simulated PRIOR generation receipt history (the current receipt's execution
;; proof is genuinely produced here, never mocked). Returns
;; {root, repo, receipt, readiness}.
(define (build-production-owned-review-world base plan wave-index generation)
  (define branch production-branch)
  (define world (build-git-world! base))
  (define work (hash-ref world 'work))
  (define origin (hash-ref world 'origin))
  (define subject (clone-subject! world))
  (git! subject "checkout" "-q" "-b" branch (hash-ref world 'c0))
  (install-installed-runner! subject)
  (put-text! (build-path subject "src/gsd-fixture-widget.rkt") widget-buggy)
  (git! subject "add" ".")
  (git! subject "commit" "-q" "-m" "fixture: baseline widget behavior")
  (git! subject "branch" "-f" "main" "HEAD")
  (git! subject "push" "-q" "origin" "main")
  (put-text! (build-path subject "src/gsd-fixture-widget.rkt") widget-fixed)
  (put-text! (build-path subject "tests/w1-fixture.rkt") regression-suite)
  (put-text! (build-path subject "scripts/lint-all.rkt") fixture-lint)
  (put-text! (build-path subject ".gitignore") "compiled/\n")
  (raco-fmt-in! subject
                "src/gsd-fixture-widget.rkt"
                "tests/w1-fixture.rkt"
                "scripts/lint-all.rkt")
  (git! subject "add" ".")
  (git! subject "commit" "-q" "-m" "fixture: real acceptance gates")
  (git! subject "push" "-q" "origin" branch)
  (define root
    (build-campaign! base
                     plan
                     wave-index
                     #:branch branch
                     #:heading (format "# Fixture plan\n\n## Waves\n\n- [Inbox] W~a: fixture → waves/W~a-fixture.md"
                                       wave-index
                                       wave-index)
                     #:wave-override (format (string-append "# W~a: fixture\n\nBranch: `~a`; GitHub issue "
                                                            "[#9686](https://github.com/~a/issues/9686), "
                                                            "milestone #895.\n\n## Verify\n~a\n")
                                             wave-index
                                             branch
                                             slug
                                             production-chain)
                     ;; The wave checkout IS the subject clone; the frozen
                     ;; snapshot is recreated by the real producer below.
                     #:snapshot? #f))
  (make-file-or-directory-link subject (build-path root "q"))
  (define verify
    (run-subprocess (path->string racket-executable)
                    #:args (list (path->string auto-review-owned-fixture)
                                 "verify"
                                 (path->string root)
                                 plan
                                 (number->string wave-index)
                                 (path->string subject))
                    #:timeout 300))
  (when (subprocess-failed? verify)
    (error 'fixture
           "owned Verify producer failed (~a):~n~a~n~a"
           (subprocess-result-exit-code verify)
           (subprocess-result-stdout verify)
           (subprocess-result-stderr verify)))
  (define journal-file (journal-path root plan wave-index))
  (define journal
    (with-handlers ([exn:fail?
                     (lambda (error)
                       (define status (git* subject "status" "--short" "--branch"))
                       (error 'fixture
                              (string-append "owned Verify did not persist a valid receipt at ~a: ~a~n"
                                             "producer exit=~a~nstdout:~n~a~nstderr:~n~a~ngit status:~n~a")
                              journal-file
                              (exn-message error)
                              (subprocess-result-exit-code verify)
                              (subprocess-result-stdout verify)
                              (subprocess-result-stderr verify)
                              (subprocess-result-stdout status)))])
      (read-json-file journal-file)))
  (define receipt (hash-ref journal 'receipt #f))
  (unless (hash? receipt)
    (error 'fixture "owned Verify returned without a receipt at ~a" journal-file))
  ;; Preserve the pre-existing fixture's simulated prior-generation setup; the
  ;; current receipt's execution proof is genuinely produced above.
  (define prior (journal-fixture plan wave-index (hash-ref receipt 'head) generation branch))
  (when (hash-has-key? prior 'receipt-history)
    (put-json! journal-file (hash-set journal 'receipt-history (hash-ref prior 'receipt-history))))
  (hash 'root root
        'repo subject
        'receipt receipt
        'readiness (evidence-readiness root plan wave-index)
        'branch branch))

;; The same journal shape the coordinator writes, with `history` simulated
;; prior generations (the durable receipt history the review dispatch replays).
(define (journal-fixture plan wave-index head history branch)
  (define receipt
    (hasheq 'repo
            "/repo"
            'origin
            (format "https://github.com/~a.git" slug)
            'head
            head
            'tree
            (make-string 40 #\d)
            'verified-at
            1
            'evidence
            "Verify passed"
            'branch
            branch
            'attempt-id
            "attempt-5"
            'attempt-fence
            2))
  (hasheq 'schema-version
          1
          'plan-id
          plan
          'wave
          wave-index
          'stage
          "context-ready"
          'receipt
          receipt
          'receipt-history
          (for/list ([index (in-range history)])
            (hash-set receipt
                      'head
                      (let* ([text (number->string (add1 index) 16)]
                             [padding (make-string (- 40 (string-length text)) #\0)])
                        (string-append padding text))))))

;; The production world as the JSON the coordinator fixture consumes (paths as
;; strings, exactly the shape the deleted Python module printed).
(define (production-world->json world)
  (hash 'root
        (path->string (hash-ref world 'root))
        'repo
        (path->string (hash-ref world 'repo))
        'receipt
        (hash-ref world 'receipt)
        'readiness
        (hash-ref world 'readiness)))

;; A one-shot producer so the coordinator fixture does the heavy real-Git and
;; owned-Verify work in a CHILD process, exactly like the module this replaces:
;; the caller's test process stays small, which keeps every later `subprocess`
;; fork cheap on a loaded host (a multi-hundred-MB test heap makes each fork +
;; exec of the coordinator's helpers measurably slower).
(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (unless (and (= 5 (length args)) (equal? (car args) "production-world"))
    (error 'fixture "usage: production-world <base> <plan> <wave> <generation>"))
  (write-json (production-world->json (build-production-owned-review-world (second args)
                                                                        (third args)
                                                                        (string->number (fourth args))
                                                                        (string->number (fifth args)))))
  (newline))
