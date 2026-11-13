#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers scripts/gsd-delivery-api.rkt
;; @covers scripts/gsd-delivery-git.rkt
;; @timeout 420
;; Phase B slice S3: the GitHub API READ layer of scripts/gsd-delivery.py
;; (`api`, `checks`, `trusted_check`, `protection`, `validate_merge`,
;; `fetch_head`) is ported to Racket (scripts/gsd-delivery-api.rkt) on the
;; shared bounded seam proven by the S1/S2 native ports
;; (scripts/gsd-delivery-git.rkt). The module is NOT wired into the coordinator
;; by this slice and performs no mutation.
;;
;; Parity law under test: every refusal is a typed exn:fail:delivery-action
;; carrying the EXACT Python reason string, and every positive path returns the
;; same data. A fake `gh` executable is installed FIRST on PATH: it dispatches
;; the requested `repos/<slug>/<route>` argument to a JSON fixture file and
;; appends each invocation to a log, so spawn counts (memoization), the exact
;; argument vector (no shell), and the presence/absence of `--paginate` are all
;; asserted from evidence rather than from the module's own bookkeeping.
;;
;; Infrastructure honesty: no gh subprocess is spawned through a shell — the
;; logged argument vector contains routes with `?` and `&` intact, which a
;; shell would have split. `fetch_head` runs through the same bounded runner
;; (`gh` is never involved) and its PR-number guard refuses before any command.

(require rackunit
         json
         racket/file
         racket/list
         racket/path
         racket/string
         "../sandbox/subprocess.rkt"
         "../scripts/gsd-delivery-git.rkt"
         "../scripts/gsd-delivery-api.rkt")

;; ============================================================
;; Fixture helpers
;; ============================================================

(define slug "coinerd/q")
(define head-sha (make-string 40 #\a))
(define merge-sha (make-string 40 #\b))
(define base-sha (make-string 40 #\c))

;; Start from an empty environment: no user/system Git config, credential
;; helpers, or ambient overrides. PATH is explicit.
(define (environment-set! env name value)
  (environment-variables-set! env (string->bytes/utf-8 name) (string->bytes/utf-8 value)))

(define (base-env root path)
  (define env (make-environment-variables))
  (for ([pair (in-list `(("PATH" . ,path) ("HOME" . ,(path->string root))
                                          ("GIT_CONFIG_NOSYSTEM" . "1")
                                          ("GIT_CONFIG_GLOBAL" . "/dev/null")
                                          ("GIT_TERMINAL_PROMPT" . "0")
                                          ("GIT_ALLOW_PROTOCOL" . "file")
                                          ("GIT_AUTHOR_NAME" . "Test")
                                          ("GIT_AUTHOR_EMAIL" . "test@example.invalid")
                                          ("GIT_COMMITTER_NAME" . "Test")
                                          ("GIT_COMMITTER_EMAIL" . "test@example.invalid")))])
    (environment-set! env (car pair) (cdr pair)))
  env)

(define (put path text)
  (make-directory* (path-only path))
  (call-with-output-file path #:exists 'truncate (lambda (out) (display text out))))

;; The requested API route, in the exact shape the module passes to `gh`.
(define (route slug* path)
  (format "repos/~a/~a" slug* path))

(define (checks-route sha page)
  (route slug (format "commits/~a/check-runs?per_page=100&page=~a" sha page)))

;; The fake `gh`: logs `$*`, recovers the route argument, prints the fixture.
;; `case` patterns are DOUBLE-QUOTED so `?` stays literal (no globbing), which
;; keeps the fixture dispatch exact for query-string routes.
(define (install-gh-shim! root routes)
  (define bin (build-path root "bin"))
  (make-directory* bin)
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
  shim)

;; fixtures: list of (list route-string response-body)
(define (with-gh-world fixtures proc)
  (define root (make-temporary-file "native-api-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (define bin (build-path root "bin"))
                  (define fixtures-dir (build-path root "fixtures"))
                  (define log (build-path root "gh-invocations.log"))
                  (make-directory* bin)
                  (make-directory* fixtures-dir)
                  (define routes
                    (for/list ([entry (in-list fixtures)]
                               [index (in-naturals)])
                      (define file (format "fixture-~a.json" index))
                      (put (build-path fixtures-dir file) (cadr entry))
                      (list (car entry) file)))
                  (install-gh-shim! root routes)
                  (define env (base-env root (format "~a:/usr/bin:/bin" bin)))
                  (environment-set! env "GH_SHIM_LOG" (path->string log))
                  (environment-set! env "GH_SHIM_FIXTURES" (path->string fixtures-dir))
                  (parameterize ([current-environment-variables env])
                    ;; The shim IS the executable under test; assert the resolution before
                    ;; trusting any logged evidence.
                    (define resolved (find-executable-path "gh"))
                    (check-not-false resolved "the fixture must resolve gh through PATH")
                    (clear-api-cache!)
                    (proc root)))
                (lambda () (delete-directory/files root))))

(define (gh-log root)
  (define log (build-path root "gh-invocations.log"))
  (if (file-exists? log)
      (file->lines log)
      '()))

;; A refusal is ONLY the shared typed exception, and its message is the typed
;; reason the coordinator surfaces verbatim.
(define (delivery-refusal thunk)
  (with-handlers ([exn:fail:delivery-action? exn-message])
    (thunk)
    (fail "expected a typed exn:fail:delivery-action refusal, but the call returned")))

;; ---------- JSON fixture constructors -------------------------------------

(define (checks-page runs total)
  (jsexpr->string (hasheq 'total_count total 'check_runs runs)))

(define (run-entry #:id [id 1] #:name [name "ci"])
  (hasheq 'id id 'name name))

(define default-details (format "https://github.com/~a/actions/runs/999/job/123" slug))

(define (check-run #:id [id 42]
                   #:name [name "ci"]
                   #:status [status "completed"]
                   #:conclusion [conclusion "success"]
                   #:head [head-sha* head-sha]
                   #:app [app 15368]
                   #:details [details default-details])
  (hasheq 'id
          id
          'name
          name
          'status
          status
          'conclusion
          conclusion
          'head_sha
          head-sha*
          'app
          (hasheq 'id app)
          'details_url
          details))

(define (actions-run #:status [status "completed"]
                     #:conclusion [conclusion "success"]
                     #:head [head-sha* head-sha]
                     #:event [event "pull_request"]
                     #:branch [branch "campaign/w1"]
                     #:path [path ".github/workflows/ci.yml"]
                     #:repo [repo slug])
  (hasheq 'status
          status
          'conclusion
          conclusion
          'head_sha
          head-sha*
          'event
          event
          'head_branch
          branch
          'path
          path
          'repository
          (hasheq 'full_name repo)))

(define (protection-policy #:enforce [enforce #t]
                           #:strict [strict #t]
                           #:force [force #f]
                           #:delete [delete #f]
                           #:contexts [contexts '("ci" "lint")])
  (jsexpr->string (hasheq 'enforce_admins
                          (hasheq 'enabled enforce)
                          'required_status_checks
                          (hasheq 'strict strict 'contexts contexts)
                          'allow_force_pushes
                          (hasheq 'enabled force)
                          'allow_deletions
                          (hasheq 'enabled delete))))

(define (merged-pr #:merge [merge merge-sha]
                   #:head [head head-sha]
                   #:merged [merged #t]
                   #:merged-at [merged-at "2026-01-01T00:00:00Z"]
                   #:state [state "closed"]
                   #:base-ref [base-ref "main"]
                   #:base-repo [base-repo slug]
                   #:head-repo [head-repo slug])
  (hasheq 'number
          7
          'state
          state
          'merged
          merged
          'merged_at
          merged-at
          'merge_commit_sha
          merge
          'head
          (hasheq 'sha head 'repo (hasheq 'full_name head-repo))
          'base
          (hasheq 'ref base-ref 'repo (hasheq 'full_name base-repo))))

(define (merge-commit #:sha [sha merge-sha] #:parents [parents (list (hasheq 'sha base-sha))])
  (hasheq 'sha sha 'parents parents))

;; ---------- real repository fixture (fetch-head) --------------------------

(define (git dir . args)
  (define result
    (run-subprocess "git"
                    #:args args
                    #:directory dir
                    #:environment (current-environment-variables)
                    #:timeout 20
                    #:process-group? #t))
  (unless (and (zero? (subprocess-result-exit-code result))
               (not (subprocess-result-timed-out? result)))
    (error 'fixture-git "failed ~s: ~a" args (subprocess-result-stderr result)))
  (string-trim (subprocess-result-stdout result)))

;; A bare origin that publishes both refs/heads/main and refs/pull/3/head, plus
;; a clone whose FETCH_HEAD the port must validate.
(define (with-repo-world proc)
  (define root (make-temporary-file "native-api-repo-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-environment-variables (base-env root "/usr/bin:/bin")])
                    (clear-api-cache!)
                    (define origin (build-path root "origin.git"))
                    (define work (build-path root "work"))
                    (define subject (build-path root "subject"))
                    (git root "init" "--bare" (path->string origin))
                    (git root "init" "-b" "main" (path->string work))
                    (put (build-path work "tracked") "initial\n")
                    (git work "add" "tracked")
                    (git work "commit" "-m" "initial")
                    (git work "remote" "add" "origin" (path->string origin))
                    (git work "push" "origin" "main")
                    (git work "push" "origin" "HEAD:refs/pull/3/head")
                    (git root "clone" "-b" "main" (path->string origin) (path->string subject))
                    (proc subject (git work "rev-parse" "HEAD"))))
                (lambda () (delete-directory/files root))))

(module+ test
  ;; ============================================================
  ;; api-call
  ;; ============================================================

  (test-case "api-call parses a route through the bounded gh runner (no shell, no --paginate)"
    (with-gh-world
     (list (list (route slug "pulls/7") (jsexpr->string (hasheq 'number 7 'state "closed"))))
     (lambda (root)
       (define data (api-call slug "pulls/7"))
       (check-true (hash? data))
       (check-equal? (hash-ref data 'number) 7)
       ;; Exact argument vector, appended by the shim itself: the route is ONE
       ;; literal argument (no shell split) and --paginate was never passed.
       (check-equal? (gh-log root)
                     (list (format "api --hostname github.com ~a" (route slug "pulls/7"))))
       (check-false (string-contains? (car (gh-log root)) "--paginate")
                    "the non-paginated path must not request --paginate"))))

  (test-case "api-call passes --paginate --slurp only when asked"
    (with-gh-world (list (list (route slug "pulls?state=all") "[]"))
                   (lambda (root)
                     (check-equal? (api-call slug "pulls?state=all" #:paginate? #t) '())
                     (check-equal? (gh-log root)
                                   (list (format "api --hostname github.com ~a --paginate --slurp"
                                                 (route slug "pulls?state=all")))))))

  (test-case "malformed GitHub JSON refuses with the exact route-specific reason"
    (with-gh-world (list (list (route slug "pulls/7") "not json at all"))
                   (lambda (root)
                     (check-equal? (delivery-refusal (lambda () (api-call slug "pulls/7")))
                                   "malformed GitHub response for pulls/7")
                     (check-equal? (length (gh-log root)) 1))))

  (test-case "a non-object, non-list payload refuses with the same reason"
    (for ([body (in-list '("42" "\"text\"" "null" "true"))])
      (with-gh-world (list (list (route slug "pulls/7") body))
                     (lambda (_root)
                       (check-equal? (delivery-refusal (lambda () (api-call slug "pulls/7")))
                                     "malformed GitHub response for pulls/7"
                                     body)))))

  (test-case "api-call memoizes per (slug, route, paginate) and spawns gh once"
    (with-gh-world (list (list (route slug "pulls/7") (jsexpr->string (hasheq 'number 7)))
                         (list (route slug "pulls/8") (jsexpr->string (hasheq 'number 8))))
                   (lambda (root)
                     ;; Two identical reads -> exactly one spawn.
                     (check-equal? (hash-ref (api-call slug "pulls/7") 'number) 7)
                     (check-equal? (hash-ref (api-call slug "pulls/7") 'number) 7)
                     (check-equal? (length (gh-log root)) 1)
                     ;; The cache is per route, not global.
                     (check-equal? (hash-ref (api-call slug "pulls/8") 'number) 8)
                     (check-equal? (length (gh-log root)) 2)
                     ;; The paginate flag is part of the key.
                     (api-call slug "pulls/7" #:paginate? #t)
                     (check-equal? (length (gh-log root)) 3)
                     ;; clear-api-cache! drops the memo, so the next read re-spawns.
                     (clear-api-cache!)
                     (void (api-call slug "pulls/7"))
                     (check-equal? (length (gh-log root)) 4))))

  ;; ============================================================
  ;; checks
  ;; ============================================================

  (test-case "checks paginates manually until total_count is covered"
    (with-gh-world
     (list (list (checks-route head-sha 1)
                 (checks-page (list (run-entry #:id 1 #:name "ci") (run-entry #:id 2 #:name "lint"))
                              3))
           (list (checks-route head-sha 2) (checks-page (list (run-entry #:id 3 #:name "test")) 3)))
     (lambda (root)
       (define runs (checks slug head-sha))
       (check-equal? (map (lambda (run) (hash-ref run 'id)) runs) '(1 2 3))
       ;; Exact logged argument vectors: the query string is ONE literal
       ;; argument (page=1 and page=2 are distinct routes) and no page skipped.
       (check-equal? (gh-log root)
                     (list (format "api --hostname github.com ~a" (checks-route head-sha 1))
                           (format "api --hostname github.com ~a" (checks-route head-sha 2))))
       ;; Memoized per (slug, sha): a second read spawns nothing.
       (check-equal? (length (checks slug head-sha)) 3)
       (check-equal? (length (gh-log root)) 2))))

  (test-case "checks refuses a page without a check_runs list"
    (with-gh-world (list (list (checks-route head-sha 1) (jsexpr->string (hasheq 'total_count 1))))
                   (lambda (_root)
                     (check-equal? (delivery-refusal (lambda () (checks slug head-sha)))
                                   "invalid check result"))))

  (test-case "checks refuses a non-object check run entry"
    (with-gh-world (list (list (checks-route head-sha 1) (checks-page (list (run-entry #:id 1) 7) 2)))
                   (lambda (_root)
                     (check-equal? (delivery-refusal (lambda () (checks slug head-sha)))
                                   "invalid check run entry"))))

  (test-case "checks refuses a missing or non-integer total_count"
    (for ([page (in-list (list (jsexpr->string (hasheq 'check_runs '()))
                               (jsexpr->string (hasheq 'total_count "1" 'check_runs '()))
                               (jsexpr->string (hasheq 'total_count 1.5 'check_runs '()))))])
      (with-gh-world (list (list (checks-route head-sha 1) page))
                     (lambda (_root)
                       (check-equal? (delivery-refusal (lambda () (checks slug head-sha)))
                                     "malformed check-total"
                                     page)))))

  (test-case "checks refuses an incomplete pagination (empty page below total)"
    (with-gh-world (list (list (checks-route head-sha 1) (checks-page (list (run-entry #:id 1)) 3))
                         (list (checks-route head-sha 2) (checks-page '() 3)))
                   (lambda (_root)
                     (check-equal? (delivery-refusal (lambda () (checks slug head-sha)))
                                   "check pagination incomplete"))))

  ;; ============================================================
  ;; trusted-check
  ;; ============================================================

  (define (trusted-world check run proc)
    (with-gh-world (list (list (checks-route head-sha 1) (checks-page (list check) 1))
                         (list (route slug "actions/runs/999") (jsexpr->string run)))
                   proc))

  (test-case "trusted-check accepts a completed trusted check whose Actions run matches"
    (trusted-world
     (check-run)
     (actions-run)
     (lambda (root)
       (check-equal? (trusted-check slug head-sha "ci" "pull_request" #:branch "campaign/w1") (void))
       (check-equal? (gh-log root)
                     (list (format "api --hostname github.com ~a" (checks-route head-sha 1))
                           (format "api --hostname github.com ~a"
                                   (route slug "actions/runs/999")))))))

  (test-case "trusted-check picks the highest-id candidate and refuses when it is untrusted"
    ;; max() is the FIRST maximal element: the newer failed run wins over the
    ;; older successful one, so a retry cannot hide a failure.
    (with-gh-world
     (list (list (checks-route head-sha 1)
                 (checks-page (list (check-run #:id 5) (check-run #:id 9 #:conclusion "failure")) 2)))
     (lambda (_root)
       (check-equal? (delivery-refusal (lambda () (trusted-check slug head-sha "ci" "pull_request")))
                     "pending, failed, or untrusted check: ci"))))

  (test-case "trusted-check refuses a missing check name"
    (with-gh-world
     (list (list (checks-route head-sha 1) (checks-page (list (check-run #:id 1 #:name "lint")) 1)))
     (lambda (root)
       (check-equal? (delivery-refusal (lambda () (trusted-check slug head-sha "ci" "pull_request")))
                     "missing check: ci")
       (check-equal? (length (gh-log root)) 1))))

  (test-case "trusted-check refuses a check from another app, a non-success and a pending check"
    (trusted-world
     (check-run #:app 1234)
     (actions-run)
     (lambda (_root)
       (check-equal? (delivery-refusal (lambda () (trusted-check slug head-sha "ci" "pull_request")))
                     "pending, failed, or untrusted check: ci")))
    (trusted-world
     (check-run #:conclusion "failure")
     (actions-run)
     (lambda (_root)
       (check-equal? (delivery-refusal (lambda () (trusted-check slug head-sha "ci" "pull_request")))
                     "pending, failed, or untrusted check: ci")))
    (trusted-world
     (check-run #:status "in_progress" #:conclusion 'null)
     (actions-run)
     (lambda (_root)
       (check-equal? (delivery-refusal (lambda () (trusted-check slug head-sha "ci" "pull_request")))
                     "pending, failed, or untrusted check: ci"))))

  (test-case "trusted-check refuses a details_url that names another repository"
    (trusted-world
     (check-run #:details "https://github.com/attacker/q/actions/runs/999/job/123")
     (actions-run)
     (lambda (_root)
       (check-equal? (delivery-refusal (lambda () (trusted-check slug head-sha "ci" "pull_request")))
                     "check has no authenticated Actions run: ci"))))

  (test-case "trusted-check refuses a details_url that is not an Actions job URL"
    (trusted-world
     (check-run #:details (format "https://github.com/~a/actions/runs/999" slug))
     (actions-run)
     (lambda (_root)
       (check-equal? (delivery-refusal (lambda () (trusted-check slug head-sha "ci" "pull_request")))
                     "check has no authenticated Actions run: ci"))))

  (test-case "trusted-check refuses a workflow run event/path/branch mismatch"
    (for ([run (in-list (list (actions-run #:event "push")
                              (actions-run #:path ".github/workflows/other.yml")
                              (actions-run #:branch "main")))])
      (trusted-world
       (check-run)
       run
       (lambda (_root)
         (check-equal? (delivery-refusal
                        (lambda ()
                          (trusted-check slug head-sha "ci" "pull_request" #:branch "campaign/w1")))
                       "check workflow/ref/event mismatch: ci")))))

  (test-case "trusted-check tolerates any head_branch when no branch is required"
    (trusted-world (check-run)
                   (actions-run #:branch "main")
                   (lambda (_root)
                     (check-equal? (trusted-check slug head-sha "ci" "pull_request") (void)))))

  ;; ============================================================
  ;; protection
  ;; ============================================================

  (define (protection-world policy proc)
    (with-gh-world (list (list (route slug "branches/main/protection") policy)) proc))

  (test-case "protection accepts admin-enforced, strict, push/delete-locked policy"
    (protection-world (protection-policy)
                      (lambda (root)
                        (define policy (protection slug (list "ci")))
                        (check-true (hash? policy))
                        (check-equal? (gh-log root)
                                      (list (format "api --hostname github.com ~a"
                                                    (route slug "branches/main/protection")))))))

  (test-case "protection refuses each weakened policy branch"
    (for ([policy (in-list (list (protection-policy #:enforce #f)
                                 (protection-policy #:strict #f)
                                 (protection-policy #:force #t)
                                 (protection-policy #:delete #t)))])
      (protection-world policy
                        (lambda (_root)
                          (check-equal? (delivery-refusal (lambda () (protection slug (list "ci"))))
                                        "default branch protection is absent or weakened"
                                        policy)))))

  (test-case "protection refuses a missing/non-string context list and a malformed policy"
    (for ([policy (in-list (list (protection-policy #:contexts '())
                                 (protection-policy #:contexts (list 1))))])
      (protection-world policy
                        (lambda (_root)
                          (check-equal? (delivery-refusal (lambda () (protection slug (list "ci"))))
                                        "default branch protection requires configured native checks"
                                        policy))))
    (with-gh-world (list (list (route slug "branches/main/protection") "[]"))
                   (lambda (_root)
                     (check-equal? (delivery-refusal (lambda () (protection slug (list "ci"))))
                                   "malformed branch protection response"))))

  (test-case "protection refuses a policy that misses an expected check name"
    (protection-world
     (protection-policy #:contexts '("ci"))
     (lambda (_root)
       (check-equal? (delivery-refusal (lambda () (protection slug (list "ci" "lint"))))
                     "branch protection does not require expected checks: lint")
       ;; Only the MISSING names are reported, in expected-name order.
       (check-equal? (delivery-refusal (lambda () (protection slug (list "lint" "test"))))
                     "branch protection does not require expected checks: lint, test"))))

  (test-case "protection refuses an empty expected-name list before any request"
    (protection-world (protection-policy)
                      (lambda (root)
                        (check-equal? (delivery-refusal (lambda () (protection slug '())))
                                      "invalid expected check names")
                        (check-equal? (gh-log root) '()))))

  ;; ============================================================
  ;; validate-merge
  ;; ============================================================

  (test-case "validate-merge accepts a same-repo squash merge of the verified head"
    (check-equal? (validate-merge (merged-pr) (merge-commit) slug merge-sha head-sha) (void))
    ;; The commit-association shape (`merged` null, closed + merged_at) is
    ;; authoritatively merged too.
    (check-equal? (validate-merge (merged-pr #:merged 'null) (merge-commit) slug merge-sha head-sha)
                  (void)))

  (test-case "validate-merge refuses a malformed PR/commit response"
    (check-equal? (delivery-refusal (lambda ()
                                      (validate-merge '() (merge-commit) slug merge-sha head-sha)))
                  "malformed PR/commit response")
    (check-equal? (delivery-refusal (lambda ()
                                      (validate-merge (merged-pr) '() slug merge-sha head-sha)))
                  "malformed PR/commit response"))

  (test-case "validate-merge refuses a PR that is not merged"
    (for ([pr (in-list (list (merged-pr #:merged #f #:merged-at 'null #:state "open")
                             (merged-pr #:merged 'null #:state "open")))])
      (check-equal? (delivery-refusal (lambda ()
                                        (validate-merge pr (merge-commit) slug merge-sha head-sha)))
                    "implementation PR is not merged")))

  (test-case "validate-merge refuses a merge/head SHA mismatch"
    (check-equal?
     (delivery-refusal
      (lambda ()
        (validate-merge (merged-pr #:merge base-sha) (merge-commit) slug merge-sha head-sha)))
     "implementation merge SHA mismatch")
    ;; A short SHA is never a full SHA, even when it matches the requested one.
    (check-equal? (delivery-refusal (lambda ()
                                      (validate-merge (merged-pr #:merge "abc123")
                                                      (merge-commit #:sha "abc123")
                                                      slug
                                                      "abc123"
                                                      head-sha)))
                  "implementation merge SHA mismatch")
    (check-equal?
     (delivery-refusal
      (lambda () (validate-merge (merged-pr #:head base-sha) (merge-commit) slug merge-sha head-sha)))
     "implementation head SHA mismatch")
    (check-equal? (delivery-refusal
                   (lambda () (validate-merge (merged-pr) (merge-commit) slug merge-sha "abc123")))
                  "implementation head SHA mismatch"))

  (test-case "validate-merge refuses a fork, a non-main base or another repository"
    (for ([pr (in-list (list (merged-pr #:base-ref "develop")
                             (merged-pr #:base-repo "attacker/q")
                             (merged-pr #:head-repo "attacker/q")))])
      (check-equal? (delivery-refusal (lambda ()
                                        (validate-merge pr (merge-commit) slug merge-sha head-sha)))
                    "implementation repository/default branch mismatch")))

  (test-case "validate-merge refuses non-squash provenance"
    (for ([commit (in-list (list (merge-commit #:parents (list (hasheq 'sha base-sha)
                                                               (hasheq 'sha base-sha)))
                                 (merge-commit #:parents '())
                                 (merge-commit #:sha base-sha)))])
      (check-equal? (delivery-refusal (lambda ()
                                        (validate-merge (merged-pr) commit slug merge-sha head-sha)))
                    "delivery must have single-parent squash provenance")))

  ;; ============================================================
  ;; fetch-head
  ;; ============================================================

  (test-case "fetch-head refuses a non-positive or non-integer PR number before any command"
    ;; The repository path does not exist: an actual git invocation would raise
    ;; a different (classifier) reason, so the exact message proves the guard
    ;; ran first.
    (for ([number (in-list (list 0 -1 "3" 1.0 #f))])
      (check-equal? (delivery-refusal (lambda ()
                                        (fetch-head "/nonexistent-native-api-fixture" number)))
                    "invalid implementation PR number"
                    (format "~s" number))))

  (test-case "fetch-head fetches refs/pull/<n>/head through the git seam and returns the commit"
    (with-repo-world (lambda (subject expected)
                       (check-equal? (fetch-head subject 3) expected)
                       (check-equal? (git subject "rev-parse" "FETCH_HEAD") expected)))))
