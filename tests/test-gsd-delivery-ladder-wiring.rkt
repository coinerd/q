#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers extensions/gsd/delivery-coordinator.rkt
;; @covers scripts/gsd-delivery-pr.rkt
;; @timeout 300
;;
;; Phase B slice S5 — native read-only delivery ladder wiring.
;;
;; The coordinator stages that used to shell `gsd-delivery.py resolve-pr /
;; pr-ci / review` now dispatch to the native scripts/gsd-delivery-pr.rkt
;; functions (delivery-resolve-pr!, delivery-pr-ci!, delivery-review!) with the
;; exact arguments the Python invocation carried. This file pins that wiring:
;;
;;   (a) the three read-only stages never spawn python3,
;;   (b) a native refusal surfaces the SAME reason string the Python exit-2
;;       {"status":"delivery-pending","reason":…} payload produced (each case
;;       compares against a RECORDED expectation constant),
;;   (c) every other stage still dispatches through the controller
;;       (open-pr / merge / governance are asserted to reach python3).
;;
;; Fixture: a real local git origin plus a per-case subject clone (so
;; `refresh`, `origin/main`, `merge-base` and the PR-head fetch are REAL
;; commands), a fake `gh` installed FIRST on PATH (S3/S4 pattern), and a
;; python3 spy that fails closed and records any invocation. The spy marker is
;; the assertion: a native stage never creates it, a Python stage always does.

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
         "../sandbox/subprocess.rkt")

(define slug "example/q")
(define branch "campaign/test")

;; Recorded expectations: the EXACT reason strings scripts/gsd-delivery.py's
;; exit-2 payload carried for these fixtures. The native actions carry the
;; identical strings, so the coordinator journals the same typed stop.
(define recorded-multiple-prs-reason "multiple pull requests already target branch campaign/test")
(define recorded-branch-mismatch-reason "pull request branch does not match expected branch")
(define recorded-symlinked-root-reason "symlinked campaign root refused")
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
;; literal.
(define (install-gh-shim! bin fixtures routes)
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
            (for/list ([entry (in-list routes)])
              (format "  ~s) /bin/cat \"$GH_SHIM_FIXTURES/~a\" ;;" (car entry) (cadr entry)))
            (list "  *) printf 'no fixture for route %s\\n' \"$route\" >&2; exit 66 ;;" "esac")))
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

(define (open-route)
  (format "repos/~a/pulls?state=open&head=example:~a" slug branch))

(define (all-route)
  (format "repos/~a/pulls?state=all&head=example:~a" slug branch))

(define (pr-route [number 1])
  (format "repos/~a/pulls/~a" slug number))

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
     (define env (fixture-env root (format "~a:/usr/bin:/bin" (path->string bin))))
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

;; (c) every mutating action still dispatches through the controller.
(module+ test
  (test-case "implementation-pr's missing-PR path still dispatches open-pr through the controller"
    (with-ladder-case
     (list (list (open-route) "open.json" "[]") (list (all-route) "all.json" "[]"))
     (lambda (ctx)
       (stage-effect ctx "implementation-pr")
       (check-true (spawned-python3? ctx)
                   "opening a pull request is mutating and stays on gsd-delivery.py")))))

(module+ test
  (test-case "implementation-merged still dispatches the protected merge through the controller"
    (with-ladder-case
     (list (list (open-route) "open.json" (json-text (list (pr-hash))))
           (list (all-route) "all.json" "[]"))
     (lambda (ctx)
       (stage-effect ctx "implementation-merged")
       (check-true (spawned-python3? ctx)
                   "the protected merge is mutating and stays on gsd-delivery.py")))))

(module+ test
  (test-case "governance still dispatches through the controller"
    (with-ladder-case
     '()
     (lambda (ctx)
       (stage-effect ctx "governance")
       (check-true (spawned-python3? ctx)
                   "governance is not a read-only ladder action and stays on gsd-delivery.py")))))
