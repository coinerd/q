#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers extensions/gsd/delivery-coordinator.rkt
;; @covers scripts/gsd-delivery-pr.rkt
;; @covers scripts/gsd-delivery-write.rkt
;; @covers scripts/gsd-delivery-status.rkt
;; @covers scripts/gsd-delivery-merge.rkt
;; @covers scripts/gsd-delivery-binding.rkt
;; @timeout 300
;;
;; Phase B slices S5/S6/S9 — native delivery action wiring.
;;
;; The coordinator stages that used to shell `gsd-delivery.py resolve-pr /
;; pr-ci / review` now dispatch to the native scripts/gsd-delivery-pr.rkt
;; functions (delivery-resolve-pr!, delivery-pr-ci!, delivery-review!) with the
;; exact arguments the Python invocation carried, and slice S6 moved `open-pr`
;; (scripts/gsd-delivery-write.rkt) plus the `status` / `governance` readbacks
;; (scripts/gsd-delivery-status.rkt) to native code too. Slice S9 closed the
;; ladder: the protected `merge` and `prepare` (scripts/gsd-delivery-merge.rkt)
;; and every `binding-*` action (scripts/gsd-delivery-binding.rkt) are native, so
;; NO dispatchable stage spawns python3; gsd-delivery.py survives only as the
;; fallback for an unknown/legacy action name. This file pins that wiring:
;;
;;   (a) no delivery stage spawns python3,
;;   (b) a native refusal surfaces the SAME reason string the Python exit-2
;;       {"status":"delivery-pending","reason":…} payload produced (each case
;;       compares against a RECORDED expectation constant),
;;   (c) `status` is dispatched WITH the campaign root (two-tier control case),
;;       and `merge`/`prepare` receive the RECEIPT-resolved identity — the
;;       merged/head-mismatch controls prove which value the native action was
;;       handed,
;;   (d) the Python fallback is still exercised, but ONLY for an unknown /
;;       legacy action name.
;;
;; Fixture: a real local git origin plus a per-case subject clone (so
;; `refresh`, `origin/main`, `merge-base` and the PR-head fetch are REAL
;; commands), a fake `gh` installed FIRST on PATH (S3/S4 pattern), and a
;; python3 spy that fails closed and records any invocation. The spy marker is
;; the assertion: a native stage never creates it, the legacy fallback does.

(require rackunit
         json
         racket/file
         racket/list
         racket/path
         racket/string
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         (only-in "../extensions/gsd/delivery-journal.rkt" record-delivery-receipt!)
         "../extensions/gsd/delivery-coordinator.rkt"
         (only-in "../scripts/gsd-delivery-status.rkt" delivery-status!)
         "../sandbox/subprocess.rkt")

(define slug "example/q")
(define branch "campaign/test")

;; Recorded expectations: the EXACT reason strings scripts/gsd-delivery.py's
;; exit-2 payload carried for these fixtures. The native actions carry the
;; identical strings, so the coordinator journals the same typed stop.
(define recorded-multiple-prs-reason "multiple pull requests already target branch campaign/test")
(define recorded-branch-mismatch-reason "pull request branch does not match expected branch")
(define recorded-symlinked-root-reason "symlinked campaign root refused")
;; S6 recordings (same provenance, verified against the Python action's own
;; text): the delivered `status` readback, the governance binding refusal, and
;; the refusal a `status` call produces when the campaign root is NOT passed.
(define recorded-binding-absent-reason
  "W0 verified; delivery pending: protected campaign binding is absent")
(define recorded-ambiguous-root-reason "ambiguous campaign roots; explicit --campaign-root required")

;; Recorded governance refusal for a wave with no merged binding PR.
(define (recorded-governance-unresolvable-reason plan)
  (format "governance cannot self-resolve exactly one merged binding PR for branch binding/~a-w0"
          (substring plan 0 12)))

;; Slice S9 recordings: the EXACT reason strings scripts/gsd-delivery.py's exit-2
;; payload carried for these fixtures; the native ports are byte-identical.
(define recorded-merge-head-mismatch-reason
  "implementation PR head does not match the verified expected head")
(define recorded-merge-stale-base-reason
  "implementation PR is not based on fresh origin/main; reanchor and re-verify")
(define recorded-binding-not-directory-reason "binding staging output is not a regular directory")
(define recorded-binding-not-ready-to-publish-reason "binding staging is not ready to publish")

(define (recorded-prepare-unresolvable-reason)
  (format "prepare cannot self-resolve exactly one merged implementation PR for branch ~a" branch))
(define (recorded-multiple-merged-reason)
  (format "multiple merged pull requests already target branch ~a" branch))

;; The interpreter's typed stop for a `none` resolve (the binding CI/merge
;; dispatch passes the resolve result straight through).
(define (recorded-binding-none-reason stage plan)
  (format "~a: no open pull request for branch ~a; open it for genuine independent review"
          stage
          (expected-binding-branch plan)))

;; The deterministic (generation 0) binding branch the coordinator must hand to
;; `governance` as --expected-branch.
(define (expected-binding-branch plan)
  (format "binding/~a-w0" (substring plan 0 12)))
;; ============================================================
;; Fixture helpers
;; ============================================================

(define (put path text)
  (make-directory* (path-only path))
  (display-to-file text path #:exists 'truncate))

(define (env-set! env name value)
  (environment-variables-set! env (string->bytes/utf-8 name) (string->bytes/utf-8 value)))

(define fixture-env-pairs
  `(("GIT_CONFIG_NOSYSTEM" . "1") ("GIT_CONFIG_GLOBAL" . "/dev/null")
                                  ("GIT_TERMINAL_PROMPT" . "0")
                                  ("GIT_ALLOW_PROTOCOL" . "file")
                                  ("GIT_AUTHOR_NAME" . "Test")
                                  ("GIT_AUTHOR_EMAIL" . "test@example.invalid")
                                  ("GIT_COMMITTER_NAME" . "Test")
                                  ("GIT_COMMITTER_EMAIL" . "test@example.invalid")))

(define (fixture-env root [path "/usr/bin:/bin"])
  (define env (make-environment-variables))
  (env-set! env "PATH" path)
  (env-set! env "HOME" (path->string root))
  (for ([pair (in-list fixture-env-pairs)])
    (env-set! env (car pair) (cdr pair)))
  env)

(define (git! env dir . args)
  (define result
    (run-subprocess "git"
                    #:args (cons "-C" (cons (path->string (path->complete-path dir)) args))
                    #:directory dir
                    #:environment env
                    #:timeout 60
                    #:process-group? #t))
  (unless (and (zero? (subprocess-result-exit-code result))
               (not (subprocess-result-timed-out? result)))
    (error 'fixture-git "git ~s failed: ~a" args (subprocess-result-stderr result)))
  (string-trim (subprocess-result-stdout result)))

;; The fake `gh`: logs the argument vector, recovers the route argument and
;; prints the fixture. `case` patterns are DOUBLE-QUOTED so `?`/`&` stay
;; literal. A route entry whose pattern is `*` becomes the fallback fixture,
;; for routes that cannot be spelled out up front (one embedding the campaign
;; plan hash).
(define (install-gh-shim! bin fixtures routes)
  (define fallback (findf (lambda (entry) (equal? (car entry) "*")) routes))
  (define explicit (filter (lambda (entry) (not (equal? (car entry) "*"))) routes))
  (define shim (build-path bin "gh"))
  (define lines
    (append (list "#!/bin/sh"
                  "printf '%s\\n' \"$*\" >> \"$GH_SHIM_LOG\""
                  "route="
                  "for arg in \"$@\"; do"
                  "  case \"$arg\" in"
                  "    repos/*) route=$arg ;;"
                  "  esac"
                  "done"
                  "case \"$route\" in")
            (for/list ([entry (in-list explicit)])
              (format "  ~s) /bin/cat \"$GH_SHIM_FIXTURES/~a\" ;;" (car entry) (cadr entry)))
            (list (if fallback
                      (format "  *) /bin/cat \"$GH_SHIM_FIXTURES/~a\" ;;" (cadr fallback))
                      "  *) printf 'no fixture for route %s\\n' \"$route\" >&2; exit 66 ;;")
                  "esac")))
  (put shim (string-append (string-join lines "\n") "\n"))
  (file-or-directory-permissions shim #o700)
  (for ([entry (in-list routes)])
    (put (build-path fixtures (cadr entry)) (caddr entry))))

;; A python3 spy that fails closed and records any invocation.
(define (install-python3-spy! bin marker)
  (define spy (build-path bin "python3"))
  (put spy (format "#!/bin/sh\nprintf invoked > '~a'\nexit 97\n" marker))
  (file-or-directory-permissions spy #o700))

;; The shared, immutable origin: main carries the required-check policy and
;; campaign/test is the receipt branch, published with a PR ref.
(define origin-box (box #f))

(define (shared-origin)
  (or (unbox origin-box)
      (let* ([root (make-temporary-file "ladder-wiring-origin-~a" 'directory)]
             [env (fixture-env root "/usr/bin:/bin")]
             [origin (build-path root "origin.git")]
             [work (build-path root "work")])
        (check-not-false (find-executable-path "git") "the fixture must resolve git through PATH")
        (git! env root "init" "--bare" "-b" "main" (path->string origin))
        (git! env root "init" "-b" "main" (path->string work))
        (put (build-path work "tracked") "initial\n")
        (git! env work "add" "tracked")
        (git! env work "commit" "-qm" "initial")
        (put (build-path work "scripts/required-pr-checks.policy") "(\"ci\")\n")
        (git! env work "add" "scripts/required-pr-checks.policy")
        (git! env work "commit" "-qm" "policy")
        (define main-sha (git! env work "rev-parse" "HEAD"))
        (git! env work "remote" "add" "origin" (path->string origin))
        (git! env work "push" "-q" "origin" "main")
        (git! env work "checkout" "-qb" branch main-sha)
        (put (build-path work "feature") "impl\n")
        (git! env work "add" "feature")
        (git! env work "commit" "-qm" "impl")
        (define head (git! env work "rev-parse" "HEAD"))
        (git! env work "push" "-q" "origin" branch)
        (git! env work "push" "-q" "origin" (format "~a:refs/pull/1/head" head))
        (define world (hasheq 'origin origin 'main main-sha 'head head))
        (set-box! origin-box world)
        world)))

(define (receipt-head)
  (hash-ref (shared-origin) 'head))

;; ============================================================
;; JSON fixtures (the same shapes gsd-delivery.py read)
;; ============================================================

(define (pr-hash #:number [number 1]
                 #:state [state "open"]
                 #:head [head (receipt-head)]
                 #:head-ref [head-ref branch]
                 #:head-repo [head-repo slug]
                 #:merged? [merged? #f])
  (hasheq 'number
          number
          'state
          state
          'merged
          merged?
          'head
          (hasheq 'sha head 'ref head-ref 'repo (hasheq 'full_name head-repo))
          'base
          (hasheq 'ref "main" 'repo (hasheq 'full_name slug))))

(define (json-text value)
  (let ([out (open-output-string)])
    (write-json value out)
    (get-output-string out)))

(define (open-route-for br)
  (format "repos/~a/pulls?state=open&head=example:~a" slug br))

(define (all-route-for br)
  (format "repos/~a/pulls?state=all&head=example:~a" slug br))

(define (open-route)
  (open-route-for branch))

(define (all-route)
  (all-route-for branch))

(define (pr-route [number 1])
  (format "repos/~a/pulls/~a" slug number))

;; The PR-collection route `gh api -X POST repos/<slug>/pulls` targets.
(define (create-route)
  (format "repos/~a/pulls" slug))

;; ============================================================
;; Campaign fixture: the durable receipt the coordinator reads
;; ============================================================

(define (make-campaign! subject head)
  (put (build-path subject ".planning/PLAN.md")
       (string-append "# Plan: Delivery ladder wiring\n\n## Waves\n\n"
                      "- [Inbox] W0: Test → waves/W0-test.md\n"))
  (put (build-path subject ".planning/waves/W0-test.md")
       "# Test\n\nGoal: test\n\n## Verify\n\nraco test .\n")
  (define rec (migrate-campaign! subject))
  (define w (car (campaign-record-waves rec)))
  (set-campaign-wave-status! w 'done)
  (set-campaign-wave-delivery-branch! w branch)
  (set-campaign-wave-delivery-head-sha! w head)
  (set-campaign-fence-token! rec 7)
  (persist-campaign! subject rec)
  (define plan (campaign-plan-id rec))
  (record-delivery-receipt! subject
                            plan
                            0
                            (hasheq 'repo
                                    "git@github.com:example/q.git"
                                    'branch
                                    branch
                                    'origin
                                    "https://github.com/example/q.git"
                                    'evidence
                                    (format "docs/reports/gsd-wave-evidence/~a-w0.rktd" plan)
                                    'head
                                    head
                                    'tree
                                    (make-string 40 #\b)
                                    'attempt-id
                                    "attempt-1"
                                    'attempt-fence
                                    7
                                    'verified-at
                                    1789500000))
  plan)

;; ============================================================
;; One isolated case (own clone, own gh shim, own python3 spy)
;; ============================================================

;; routes: list of (list route-pattern fixture-file-name fixture-text). PROC
;; receives a context hash inside the fixture's environment parameterization.
(define (with-ladder-case routes proc)
  (define root (make-temporary-file "ladder-wiring-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define world (shared-origin))
     (define build-env (fixture-env root "/usr/bin:/bin"))
     (define subject (build-path root "subject"))
     (git! build-env root "clone" "-q" (path->string (hash-ref world 'origin)) (path->string subject))
     (git! build-env subject "config" "user.name" "Test")
     (git! build-env subject "config" "user.email" "test@example.invalid")
     ;; CONFIGURED origin identity is the GitHub slug; the insteadOf rewrite
     ;; only transports it to the local bare origin (S3/S4 law).
     (git! build-env subject "config" "remote.origin.url" "https://github.com/example/q.git")
     (git! build-env
           subject
           "config"
           (format "url.~a.insteadOf" (path->string (hash-ref world 'origin)))
           "https://github.com/example/q.git")
     (define plan (make-campaign! subject (hash-ref world 'head)))
     (define bin (build-path root "bin"))
     (define fixtures (build-path root "fixtures"))
     (define marker (build-path root "python-invoked"))
     (make-directory* bin)
     (make-directory* fixtures)
     (install-gh-shim! bin fixtures routes)
     (install-python3-spy! bin marker)
     ;; The native actions load the coordinator journal and the campaign
     ;; snapshot through `racket scripts/gsd-binding-data.rkt`, so the
     ;; interpreter's own directory stays on PATH; only python3 is shimmed (the
     ;; spy directory comes FIRST).
     (define racket-dir (path-only (find-executable-path "racket")))
     (define env
       (fixture-env root
                    (format "~a:/usr/bin:/bin~a"
                            (path->string bin)
                            (if racket-dir
                                (format ":~a" (path->string racket-dir))
                                ""))))
     (env-set! env "GH_SHIM_LOG" (path->string (build-path root "gh.log")))
     (env-set! env "GH_SHIM_FIXTURES" (path->string fixtures))
     (parameterize ([current-environment-variables env])
       (proc (hasheq 'root
                     root
                     'subject
                     subject
                     'plan
                     plan
                     'head
                     (hash-ref world 'head)
                     'marker
                     marker
                     'log
                     (build-path root "gh.log")))))
   (lambda () (delete-directory/files root #:must-exist? #f))))

(define (stage-effect ctx stage)
  (default-delivery-controller (hash-ref ctx 'subject) (hash-ref ctx 'plan) 0 stage))

(define (gh-calls ctx)
  (define log (hash-ref ctx 'log))
  (if (file-exists? log)
      (file->lines log)
      '()))

(define (spawned-python3? ctx)
  (file-exists? (hash-ref ctx 'marker)))

;; ============================================================
;; Two-tier fixture for the delivered `status` readback
;; ============================================================

;; Campaign root <root>/ holds the journal; <root>/q/ is the nested repository
;; (the recognised two-tier workspace layout), so the coordinator resolves the
;; repo to <root>/q while the campaign root stays <root>. The nested checkout
;; carries its own EMPTY .planning/ so a status call WITHOUT the explicit
;; campaign root refuses as ambiguous — the control assertion that proves the
;; delivered stage really hands the campaign root to the native action.
;;
;; CTX keys: root, subject (<root>/q), plan, head, marker, python3-racket path.
(define (with-two-tier-case proc)
  (define root (make-temporary-file "status-wiring-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define world (shared-origin))
     (define build-env (fixture-env root "/usr/bin:/bin"))
     (define subject (build-path root "q"))
     (git! build-env root "clone" "-q" (path->string (hash-ref world 'origin)) (path->string subject))
     (git! build-env subject "config" "user.name" "Test")
     (git! build-env subject "config" "user.email" "test@example.invalid")
     (git! build-env subject "config" "remote.origin.url" "https://github.com/example/q.git")
     (git! build-env
           subject
           "config"
           (format "url.~a.insteadOf" (path->string (hash-ref world 'origin)))
           "https://github.com/example/q.git")
     (define plan (make-campaign! root (hash-ref world 'head)))
     ;; An independent .planning in the nested checkout: that is what makes a
     ;; root-less `status` ambiguous (never guessed).
     (make-directory* (build-path subject ".planning"))
     (define bin (build-path root "bin"))
     (define marker (build-path root "python-invoked"))
     (make-directory* bin)
     (install-python3-spy! bin marker)
     ;; The native status action loads the journal through `racket
     ;; scripts/gsd-binding-data.rkt`, so the interpreter's own directory stays
     ;; on PATH; only python3 is shimmed.
     (define racket-dir (path-only (find-executable-path "racket")))
     (define env
       (fixture-env root
                    (format "~a:/usr/bin:/bin~a"
                            (path->string bin)
                            (if racket-dir
                                (format ":~a" (path->string racket-dir))
                                ""))))
     (parameterize ([current-environment-variables env])
       (proc
        (hasheq 'root root 'subject subject 'plan plan 'head (hash-ref world 'head) 'marker marker))))
   (lambda () (delete-directory/files root #:must-exist? #f))))

;; ============================================================
;; Fixtures for the green pr-ci verdict (the full trusted-check chain)
;; ============================================================

(define (check-runs-json)
  (json-text (hasheq 'total_count
                     1
                     'check_runs
                     (list (hasheq 'id
                                   9
                                   'name
                                   "ci"
                                   'status
                                   "completed"
                                   'conclusion
                                   "success"
                                   'head_sha
                                   (receipt-head)
                                   'app
                                   (hasheq 'id 15368)
                                   'details_url
                                   (format "https://github.com/~a/actions/runs/555/job/1" slug))))))

(define (workflow-run-json)
  (json-text (hasheq 'status
                     "completed"
                     'conclusion
                     "success"
                     'head_sha
                     (receipt-head)
                     'event
                     "pull_request"
                     'head_branch
                     branch
                     'path
                     ".github/workflows/ci.yml"
                     'repository
                     (hasheq 'full_name slug))))

(define (protection-json)
  (json-text (hasheq 'enforce_admins
                     (hasheq 'enabled #t)
                     'required_status_checks
                     (hasheq 'strict #t 'contexts (list "ci"))
                     'allow_force_pushes
                     (hasheq 'enabled #f)
                     'allow_deletions
                     (hasheq 'enabled #f))))

;; ============================================================
;; Tests
;; ============================================================

;; (a)+(b) implementation-pr: native resolve-pr, no python3, Python reason.
(module+ test
  (test-case "implementation-pr resolves the PR natively (python3 is never spawned)"
    (with-ladder-case
     (list (list (open-route) "open.json" (json-text (list (pr-hash))))
           (list (all-route) "all.json" "[]"))
     (lambda (ctx)
       (define result (stage-effect ctx "implementation-pr"))
       (check-eq? (delivery-effect-result-kind result) 'ok)
       (define data (delivery-effect-result-data result))
       (check-equal? (hash-ref data 'status #f) "resolved")
       (check-equal? (hash-ref data 'pr #f) 1)
       (check-equal? (hash-ref data 'head #f) (hash-ref ctx 'head))
       (check-equal? (hash-ref data 'branch #f) branch)
       (check-false (spawned-python3? ctx) "the read-only resolve must not reach python3")
       ;; The resolve really interrogated the (fake) API through the route the
       ;; Python action used, so this is not a cached/no-op path.
       (check-true (for/or ([line (in-list (gh-calls ctx))])
                     (string-contains? line (format "pulls?state=open&head=example:~a" branch))))))))

(module+ test
  (test-case "implementation-pr native refusal preserves the Python reason string"
    ;; Two open PRs for one branch: the same fail-closed ambiguity the Python
    ;; action refused, with the same reason text.
    (with-ladder-case (list (list (open-route)
                                  "open.json"
                                  (json-text (list (pr-hash #:number 1) (pr-hash #:number 2))))
                            (list (all-route) "all.json" "[]"))
                      (lambda (ctx)
                        (define result (stage-effect ctx "implementation-pr"))
                        (check-eq? (delivery-effect-result-kind result) 'blocked)
                        (check-equal? (hash-ref (delivery-effect-result-data result) 'reason)
                                      recorded-multiple-prs-reason)
                        (check-false (spawned-python3? ctx)
                                     "a refused native resolve must not fall back to python3")))))

;; (a)+(b) implementation-ci: native resolve-pr + pr-ci.
(module+ test
  (test-case "implementation-ci dispatches pr-ci natively with the receipt branch"
    ;; The resolved PR head names another branch: pr-ci refuses on the branch it
    ;; was HANDED, proving --expected-branch travels from the receipt.
    (with-ladder-case
     (list (list (open-route) "open.json" (json-text (list (pr-hash))))
           (list (pr-route 1) "pr1.json" (json-text (pr-hash #:head-ref "campaign/other")))
           (list (all-route) "all.json" "[]"))
     (lambda (ctx)
       (define result (stage-effect ctx "implementation-ci"))
       (check-eq? (delivery-effect-result-kind result) 'blocked)
       (check-equal? (hash-ref (delivery-effect-result-data result) 'reason)
                     recorded-branch-mismatch-reason)
       (check-false (spawned-python3? ctx) "the read-only pr-ci must not reach python3")
       (check-true (for/or ([line (in-list (gh-calls ctx))])
                     (string-contains? line (pr-route 1)))
                   "pr-ci must read the resolved PR number, not guess one")))))

(module+ test
  (test-case "implementation-ci reports a green native pr-ci verdict as ok"
    (with-ladder-case
     (list
      (list (open-route) "open.json" (json-text (list (pr-hash))))
      (list (pr-route 1) "pr1.json" (json-text (pr-hash)))
      (list (format "repos/~a/commits/~a/check-runs?per_page=100&page=1" slug (receipt-head))
            "checks.json"
            (check-runs-json))
      (list (format "repos/~a/actions/runs/555" slug) "run.json" (workflow-run-json))
      (list (format "repos/~a/branches/main/protection" slug) "protection.json" (protection-json))
      (list (all-route) "all.json" "[]"))
     (lambda (ctx)
       (define result (stage-effect ctx "implementation-ci"))
       (check-eq? (delivery-effect-result-kind result) 'ok)
       (check-equal? (hash-ref (delivery-effect-result-data result) 'status #f) "green")
       (check-false (spawned-python3? ctx) "a green native pr-ci must not reach python3")))))

;; (a)+(b) implementation-review: the native review action.
;;
;; The stage reaches its dispatch only after the durable evidence-readiness
;; gate, whose "ready" state needs a full owned-proof fixture (covered
;; end-to-end by tests/test-gsd-lost-author-finish.rkt). The WIRING is pinned
;; directly here, plus the stage-level guarantee that the stage itself never
;; spawns python3.
(module+ test
  (test-case "implementation-review dispatches the native review action (python3 is never spawned)"
    ;; A symlinked campaign root is the native review action's first refusal,
    ;; in the same order the Python action checked it and with the same reason.
    (with-ladder-case '()
                      (lambda (ctx)
                        (define link (build-path (hash-ref ctx 'root) "campaign-link"))
                        (make-file-or-directory-link (hash-ref ctx 'subject) link)
                        (define result
                          (run-native-review link
                                             (hash-ref ctx 'plan)
                                             0
                                             "implementation-review"
                                             (hash-ref ctx 'head)
                                             branch
                                             0))
                        (check-eq? (delivery-effect-result-kind result) 'blocked)
                        (check-equal? (hash-ref (delivery-effect-result-data result) 'reason)
                                      recorded-symlinked-root-reason)
                        (check-false (spawned-python3? ctx)
                                     "the read-only review must not reach python3")))))

(module+ test
  (test-case "implementation-review runs the native review at the receipt identity"
    ;; Regression: the native review action resolves its campaign root itself,
    ;; so the coordinator must hand it the PATH (a text argument raised
    ;; `path->string: contract violation` at the snapshot helper seam).
    ;; Reaching the binding-trio refusal proves the wiring drove the real
    ;; native action (journal generation, snapshot facts, receipt-tip ancestry
    ;; and the branch-tip blob read) without python3, and that the refusal
    ;; keeps the exact Python reason the production review produced
    ;; (tests/test-gsd-lost-author-finish.rkt asserts the same text end-to-end).
    (with-ladder-case
     '()
     (lambda (ctx)
       (define head (hash-ref ctx 'head))
       (define plan (hash-ref ctx 'plan))
       (define result
         (run-native-review (hash-ref ctx 'subject) plan 0 "implementation-review" head branch 0))
       (check-eq? (delivery-effect-result-kind result) 'blocked)
       (check-equal?
        (hash-ref (delivery-effect-result-data result) 'reason)
        (format (string-append
                 "binding trio docs/reports/gsd-wave-evidence/~a-w0.rktd is absent on branch ~a "
                 "(tip ~a); a genuine independent reviewer must commit it after Verify")
                plan
                branch
                (substring head 0 12)))
       (check-false (spawned-python3? ctx) "the read-only review must not reach python3")))))

(module+ test
  (test-case "implementation-review stage never spawns python3"
    (with-ladder-case '()
                      (lambda (ctx)
                        (define result (stage-effect ctx "implementation-review"))
                        ;; Blocked by the durable evidence gate — never by a missing controller
                        ;; action — and never by spawning python3.
                        (check-eq? (delivery-effect-result-kind result) 'blocked)
                        (check-false (spawned-python3? ctx)
                                     "an unready review stage must not reach python3")))))

;; (a)+(b) implementation-pr: native open-pr (the mutating create step).
(module+ test
  (test-case "implementation-pr opens the PR natively (python3 is never spawned)"
    (with-ladder-case
     (list (list (open-route) "open.json" "[]")
           (list (all-route) "all.json" "[]")
           (list (create-route) "create.json" (json-text (pr-hash)))
           (list (pr-route 1) "pr1.json" (json-text (pr-hash))))
     (lambda (ctx)
       (define result (stage-effect ctx "implementation-pr"))
       (check-eq? (delivery-effect-result-kind result) 'ok)
       (check-equal? (hash-ref (delivery-effect-result-data result) 'status #f) "opened")
       (check-false (spawned-python3? ctx) "open-pr is native since slice S6: no python3")
       ;; The mutation really POSTed the collection route the Python action used,
       ;; so this is not a resolve-only no-op path.
       (check-true (for/or ([line (in-list (gh-calls ctx))])
                     (and (string-contains? line "-X POST") (string-contains? line (create-route))))
                   "open-pr must POST repos/<slug>/pulls")))))

(module+ test
  (test-case "open-pr native refusal preserves the Python reason string"
    ;; Two open PRs for one branch: the same fail-closed ambiguity the Python
    ;; action refused (gsd-delivery.py's resolve_existing_pr), same reason text.
    (with-ladder-case (list (list (open-route)
                                  "open.json"
                                  (json-text (list (pr-hash #:number 1) (pr-hash #:number 2))))
                            (list (all-route) "all.json" "[]"))
                      (lambda (ctx)
                        (define result
                          (run-native-open-pr (hash-ref ctx 'subject)
                                              (hash-ref ctx 'plan)
                                              0
                                              "implementation-pr"
                                              branch
                                              (hash-ref ctx 'head)))
                        (check-eq? (delivery-effect-result-kind result) 'blocked)
                        (check-equal? (hash-ref (delivery-effect-result-data result) 'reason)
                                      recorded-multiple-prs-reason)
                        (check-false (spawned-python3? ctx)
                                     "a refused native open-pr must not fall back to python3")))))

;; (a)+(b) governance: the native protected-publication readback.
(module+ test
  (test-case "governance runs natively with the deterministic binding branch (no python3)"
    ;; No merged binding PR exists: the native action refuses with the exact
    ;; Python reason, and the API route it queried carries the binding branch the
    ;; coordinator handed it (--expected-branch travelled).
    (with-ladder-case (list (list "*" "all.json" "[]"))
                      (lambda (ctx)
                        (define plan (hash-ref ctx 'plan))
                        (define result (stage-effect ctx "governance"))
                        (check-eq? (delivery-effect-result-kind result) 'blocked)
                        (check-equal? (hash-ref (delivery-effect-result-data result) 'reason)
                                      (recorded-governance-unresolvable-reason plan))
                        (check-false (spawned-python3? ctx)
                                     "governance is native since slice S6: no python3")
                        (check-true (for/or ([line (in-list (gh-calls ctx))])
                                      (string-contains? line (expected-binding-branch plan)))
                                    "governance must query the deterministic binding branch")))))

;; (a)+(b)+(c) delivered: native status, dispatched WITH the campaign root.
(module+ test
  (test-case "delivered runs status natively WITH the campaign root (no python3)"
    (with-two-tier-case
     (lambda (ctx)
       (define root (hash-ref ctx 'root))
       (define plan (hash-ref ctx 'plan))
       ;; The journal the native readback must observe lives at the CAMPAIGN ROOT
       ;; (not in the nested checkout).
       (check-true (file-exists?
                    (build-path root ".planning" "campaigns" plan "coordinator-w0.json")))
       (define result (default-delivery-controller root plan 0 "delivered"))
       (check-eq? (delivery-effect-result-kind result) 'blocked)
       (check-equal? (hash-ref (delivery-effect-result-data result) 'reason)
                     recorded-binding-absent-reason)
       (check-false (spawned-python3? ctx) "status is native since slice S6: no python3")
       ;; CONTROL: without the explicit campaign root the same repository refuses
       ;; as ambiguous, so the reason above can only come from the root argument
       ;; the delivered stage passes.
       (check-exn (lambda (e) (equal? (exn-message e) recorded-ambiguous-root-reason))
                  (lambda () (delivery-status! (hash-ref ctx 'subject) plan 0)))))))

;; (a) EVERY dispatchable stage is native. Each stage is asserted python3-free
;; by its own case below (implementation-review / -pr / -ci / -merged,
;; binding-prepared / -review / -pr / -ci / -merged, governance, delivered)
;; except the two that have no dedicated case in this file — `delivery-preflight`
;; and `sync`, swept here. tests/test-gsd-delivery-preflight.rkt and
;; tests/test-gsd-delivery-sync-native.rkt pin their production wiring.
(module+ test
  (test-case "delivery-preflight and sync dispatch natively (no python3)"
    (with-ladder-case (list (list "*" "none.json" "[]"))
                      (lambda (ctx)
                        (for ([stage (in-list '("delivery-preflight" "sync"))])
                          (when (file-exists? (hash-ref ctx 'marker))
                            (delete-file (hash-ref ctx 'marker)))
                          (stage-effect ctx stage)
                          (check-false (spawned-python3? ctx)
                                       (format "~a must be native (no python3)" stage)))))))

;; (a)+(b)+(c) implementation-merged: the native protected merge.
(module+ test
  (test-case "implementation-merged runs the native merge with the receipt-resolved head"
    ;; The PR collection the resolve reads carries the receipt head; the PR that
    ;; the merge itself reads names a DIFFERENT head. The refusal proves the
    ;; merge received the RECEIPT-resolved head (--expected-head) instead of
    ;; accepting whatever its own read returned.
    (with-ladder-case
     (list (list (open-route) "open.json" (json-text (list (pr-hash))))
           (list (pr-route 1) "pr1.json" (json-text (pr-hash #:head (make-string 40 #\c))))
           (list (all-route) "all.json" "[]"))
     (lambda (ctx)
       (define result (stage-effect ctx "implementation-merged"))
       (check-eq? (delivery-effect-result-kind result) 'blocked)
       (check-equal? (hash-ref (delivery-effect-result-data result) 'reason)
                     recorded-merge-head-mismatch-reason)
       (check-false (spawned-python3? ctx) "the protected merge is native since slice S9")
       (check-true (for/or ([line (in-list (gh-calls ctx))])
                     (string-contains? line (pr-route 1)))
                   "merge must read the resolved PR number")))))

(module+ test
  (test-case "merge's expected-head is the resolved identity (control)"
    ;; The same fixture with AGREEING heads: the exact-head contract passes, so
    ;; the refusal moves on to the fresh-origin/main base proof. Together with
    ;; the mismatch case this pins that --expected-head is the resolved head.
    (with-ladder-case (list (list (open-route) "open.json" (json-text (list (pr-hash))))
                            (list (pr-route 1) "pr1.json" (json-text (pr-hash)))
                            (list (all-route) "all.json" "[]"))
                      (lambda (ctx)
                        (define result (stage-effect ctx "implementation-merged"))
                        (check-eq? (delivery-effect-result-kind result) 'blocked)
                        (check-equal? (hash-ref (delivery-effect-result-data result) 'reason)
                                      recorded-merge-stale-base-reason)
                        (check-false (spawned-python3? ctx)
                                     "the protected merge is native since slice S9")))))

;; (a)+(b) binding-prepared: the native prepare.
(module+ test
  (test-case "binding-prepared runs the native prepare with the receipt branch (no python3)"
    ;; No merged implementation PR exists: the action's own self-resolution
    ;; refuses with the exact Python text, which echoes the branch it was
    ;; HANDED (the durable receipt branch).
    (with-ladder-case (list (list (all-route) "all.json" "[]"))
                      (lambda (ctx)
                        (define result (stage-effect ctx "binding-prepared"))
                        (check-eq? (delivery-effect-result-kind result) 'blocked)
                        (check-equal? (hash-ref (delivery-effect-result-data result) 'reason)
                                      (recorded-prepare-unresolvable-reason))
                        (check-false (spawned-python3? ctx) "prepare is native since slice S9")))))

(module+ test
  (test-case "prepare's expected-head narrows the merged-PR self-resolution (control)"
    ;; TWO merged PRs for the durable branch. Only --expected-head = the receipt
    ;; head narrows them to exactly one; without it the action refuses with the
    ;; multiple-merged reason, and with a different head with the unresolvable
    ;; one. Reaching a DEEPER proof therefore pins the head argument.
    (with-ladder-case (list (list (all-route)
                                  "all.json"
                                  (json-text (list (hash-set (pr-hash #:state "closed" #:merged? #t)
                                                             'merge_commit_sha
                                                             (make-string 40 #\d))
                                                   (hash-set (pr-hash #:number 2
                                                                      #:state "closed"
                                                                      #:merged? #t
                                                                      #:head (make-string 40 #\e))
                                                             'merge_commit_sha
                                                             (make-string 40 #\f)))))
                            (list "*" "none.json" "[]"))
                      (lambda (ctx)
                        (define result (stage-effect ctx "binding-prepared"))
                        (check-eq? (delivery-effect-result-kind result) 'blocked)
                        (define reason (hash-ref (delivery-effect-result-data result) 'reason))
                        (check-false (equal? reason (recorded-multiple-merged-reason))
                                     "the receipt head must narrow the two merged PRs to one")
                        (check-false (equal? reason (recorded-prepare-unresolvable-reason))
                                     "the receipt head must self-resolve exactly one merged PR")
                        (check-false (spawned-python3? ctx) "prepare is native since slice S9")))))

;; (a)+(b) binding-review: the native staged-trio validation.
(module+ test
  (test-case "binding-review runs the native staged-trio validation (no python3)"
    ;; A REGULAR FILE at the deterministic staging path: the native action's
    ;; structural refusal, with the exact Python reason text.
    (with-ladder-case
     '()
     (lambda (ctx)
       (define plan (hash-ref ctx 'plan))
       (put (build-path (hash-ref ctx 'subject) ".planning" "campaigns" plan "binding-w0")
            "not a directory\n")
       (define result (stage-effect ctx "binding-review"))
       (check-eq? (delivery-effect-result-kind result) 'blocked)
       (check-equal? (hash-ref (delivery-effect-result-data result) 'reason)
                     recorded-binding-not-directory-reason)
       (check-false (spawned-python3? ctx) "binding-review is native since slice S9")))))

;; (a)+(b) binding-pr: the native publication.
(module+ test
  (test-case "binding-pr publishes only a reviewed staging trio (native, no python3)"
    (with-ladder-case '()
                      (lambda (ctx)
                        (define result (stage-effect ctx "binding-pr"))
                        ;; No staging directory at all: binding-review reports
                        ;; awaiting-review, so publication refuses with the
                        ;; Python text and nothing is pushed.
                        (check-eq? (delivery-effect-result-kind result) 'blocked)
                        (check-equal? (hash-ref (delivery-effect-result-data result) 'reason)
                                      recorded-binding-not-ready-to-publish-reason)
                        (check-false (spawned-python3? ctx) "binding-pr is native since slice S9")))))

;; (a)+(b)+(c) binding-merged: the native dispatch's idempotent resume.
(module+ test
  (test-case "binding-merged resumes natively on an already-merged binding PR"
    ;; A merged binding publication PR at the deterministic branch tip: the
    ;; native resolve returns the idempotent `already-merged` identity, so the
    ;; dispatch passes it through VERBATIM instead of merging again — the whole
    ;; path is native and the branch it resolved is the one the dispatch built.
    (with-ladder-case
     '()
     (lambda (ctx)
       (define root (hash-ref ctx 'root))
       (define subject (hash-ref ctx 'subject))
       (define plan (hash-ref ctx 'plan))
       (define binding-br (expected-binding-branch plan))
       (define env (fixture-env root "/usr/bin:/bin"))
       ;; A real binding publication branch at the origin plus
       ;; its PR ref, so the fetched branch tip and
       ;; refs/pull/2/head agree with the merged PR head.
       (git! env subject "checkout" "-q" "-b" binding-br "main")
       (put (build-path subject "binding-draft") "draft\n")
       (git! env subject "add" "binding-draft")
       (git! env subject "commit" "-qm" "binding draft")
       (define tip (git! env subject "rev-parse" "HEAD"))
       (git! env subject "push" "-q" "origin" (format "~a:refs/heads/~a" tip binding-br))
       (git! env subject "push" "-q" "origin" (format "~a:refs/pull/2/head" tip))
       (git! env subject "checkout" "-q" branch)
       (define merged-pr
         (hash-set (pr-hash #:number 2 #:state "closed" #:merged? #t #:head tip #:head-ref binding-br)
                   'merge_commit_sha
                   (make-string 40 #\f)))
       ;; Routes are installed HERE because the deterministic
       ;; binding branch embeds the campaign plan hash.
       (install-gh-shim!
        (build-path root "bin")
        (build-path root "fixtures")
        (list (list (all-route-for binding-br) "ball.json" (json-text (list merged-pr)))
              (list (pr-route 2) "pr2.json" (json-text merged-pr))
              (list "*" "none.json" "[]")))
       (define result (stage-effect ctx "binding-merged"))
       (check-eq? (delivery-effect-result-kind result) 'ok)
       (check-equal? (hash-ref (delivery-effect-result-data result) 'status #f) "already-merged")
       (check-equal? (hash-ref (delivery-effect-result-data result) 'pr #f) 2)
       (check-equal? (hash-ref (delivery-effect-result-data result) 'head #f) tip)
       (check-false (spawned-python3? ctx) "the binding dispatch is native since slice S9")
       (check-true (for/or ([line (in-list (gh-calls ctx))])
                     (string-contains? line (all-route-for binding-br)))
                   (string-append "the resolve must query the deterministic " "binding branch"))))))

(module+ test
  (test-case "binding-ci and binding-merged dispatch natively through the binding resolve"
    ;; No binding PR exists: the dispatch resolves the deterministic binding
    ;; branch (the argument it hands the resolve), gets the typed `none` result
    ;; and stops with the interpreter's reason — never python3.
    (with-ladder-case
     (list (list "*" "none.json" "[]"))
     (lambda (ctx)
       (define plan (hash-ref ctx 'plan))
       (for ([stage (in-list '("binding-ci" "binding-merged"))])
         (when (file-exists? (hash-ref ctx 'marker))
           (delete-file (hash-ref ctx 'marker)))
         (define result (stage-effect ctx stage))
         (check-eq? (delivery-effect-result-kind result) 'blocked)
         (check-equal? (hash-ref (delivery-effect-result-data result) 'reason)
                       (recorded-binding-none-reason stage plan))
         (check-false (spawned-python3? ctx) (format "~a is native since slice S9" stage))
         (check-true (for/or ([line (in-list (gh-calls ctx))])
                       (string-contains? line (expected-binding-branch plan)))
                     (format "~a must resolve the deterministic binding branch" stage)))))))

;; (d) an unknown / legacy action name is a typed blocked stop, never a shell-out.
(module+ test
  (test-case "an unknown/legacy action name is a typed blocked stop, never a shell-out"
    (with-ladder-case
     (list (list "*" "none.json" "[]"))
     (lambda (ctx)
       (define subject (hash-ref ctx 'subject))
       (define plan (hash-ref ctx 'plan))
       ;; A KNOWN binding action through the same dispatch seam: native.
       (define known
         (run-native-binding-action subject
                                    plan
                                    0
                                    "binding-ci"
                                    "binding-resolve-pr"
                                    "--expected-branch"
                                    branch))
       (check-eq? (delivery-effect-result-kind known) 'blocked)
       (check-equal?
        (hash-ref (delivery-effect-result-data known) 'reason)
        (format
         "binding-ci: no open pull request for branch ~a; open it for genuine independent review"
         branch))
       (check-false (spawned-python3? ctx) "a known binding action must never reach python3")
       ;; The legacy `binding-publish` alias is served by NO surface: since the
       ;; Phase B final deletion there is no Python controller left to fall back
       ;; to, so the dispatch refuses with a typed reason and publishes nothing.
       (define legacy
         (run-native-binding-action subject
                                    plan
                                    0
                                    "binding-ci"
                                    "binding-publish"
                                    "--output"
                                    (default-binding-staging-path subject plan 0)
                                    "--campaign-root"
                                    (path->string subject)))
       (check-eq? (delivery-effect-result-kind legacy) 'blocked)
       (check-true (string-contains? (hash-ref (delivery-effect-result-data legacy) 'reason)
                                     "is not implemented by the native controller"))
       (check-false (spawned-python3? ctx)
                    "an unknown/legacy action name is refused, never shelled out")))))
