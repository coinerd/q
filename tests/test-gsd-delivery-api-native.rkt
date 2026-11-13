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
;;
;; Budget honesty (one budget per action): ONE delivery-api-session owns ONE
;; (budget-seconds, start-ms) pair and builds BOTH the gh runner and
;; fetch-head's `git -C <repo> ...` commands from it, so a gh read that spends
;; the budget makes the following fetch-head refuse with the typed
;; `budget-exhausted` cause instead of running on a fresh budget. Budgets are
;; MONOTONIC across clear-api-cache! (which drops memos only), and a
;; post-mutation proof must pass `#:refresh?` — the Racket mirror of Python's
;; `_refresh=True` — because a cached read can never prove a mutation.
;;
;; Negative fixtures: every guard below is killed by a case that ONLY that
;; guard can reject — a regexp-metacharacter slug (`coinerd/q.b` accepted,
;; `coinerd/qXb` refused), hostile details_url prefixes/suffixes (plus a
;; trailing newline no `$`-anchor lets through), a check whose head_sha is
;; another commit, an Actions run owned by another repository, an Actions run
;; that is not completed / not successful / recorded on another head SHA (each
;; otherwise fully trusted), a non-40-hex FETCH_HEAD, and total_count 0 served
;; in one page (no second request).
;;
;; Differential parity: when python3/python resolves, scripts/gsd-delivery.py is
;; IMPORTED by path and driven through the SAME gh shim fixtures, and every
;; refusal message / success payload must match exactly. Each case ALSO declares
;; its own expected outcome ('ok or the exact reason string) and both sides are
;; asserted against it, so a fixture/route drift cannot let two identical
;; transport failures masquerade as parity. A host without an interpreter is a
;; FAILED gate (never a skip) — the same discipline as
;; tests/test-gsd-delivery-sync-native.rkt.

(require rackunit
         json
         racket/file
         racket/list
         racket/match
         racket/path
         racket/runtime-path
         racket/string
         "../sandbox/subprocess.rkt"
         "../scripts/gsd-delivery-git.rkt"
         "../scripts/gsd-delivery-api.rkt")

(define-runtime-path python-controller "../scripts/gsd-delivery.py")

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

;; Rewrite the Nth installed fixture IN PLACE, so a test can prove which payload
;; a call actually served (cache hit vs a fresh spawn) rather than trusting the
;; module's own bookkeeping.
(define (set-fixture! root index body)
  (put (build-path root "fixtures" (format "fixture-~a.json" index)) body))

;; Install a custom executable in `bin`; the FIRST PATH entry resolves it, so
;; the script named gh/git IS the executable under test.
(define (install-shim! bin name script)
  (make-directory* bin)
  (define shim (build-path bin name))
  (put shim (string-append script "\n"))
  (file-or-directory-permissions shim #o700)
  shim)

;; A world where BOTH executables are shims. The gh shim consumes a fixed slice
;; of the session budget and the git shim would SUCCEED (a 40-hex head), so a
;; fetch-head that ran on a fresh budget would return a sha instead of
;; refusing: the refusal is only possible when both share ONE budget.
(define (with-budget-world gh-script git-script proc)
  (define root (make-temporary-file "native-api-budget-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define bin (build-path root "bin"))
     (define repo (build-path root "repo"))
     (make-directory* repo)
     (install-shim! bin "gh" gh-script)
     (install-shim! bin "git" git-script)
     (define env (base-env root (format "~a:/usr/bin:/bin" bin)))
     (environment-set! env "GH_SHIM_LOG" (path->string (build-path root "gh.log")))
     (environment-set! env "GIT_SHIM_LOG" (path->string (build-path root "git.log")))
     (parameterize ([current-environment-variables env])
       (check-not-false (find-executable-path "gh") "the fixture must resolve gh through PATH")
       (check-not-false (find-executable-path "git") "the fixture must resolve git through PATH")
       (clear-api-cache!)
       (proc root repo)))
   (lambda () (delete-directory/files root))))

;; A world with a fake `git` only: the session's repo-git command is the
;; executable under test, so a shim can produce a fetched head the real git
;; would never print (the repository path is never touched).
(define (with-git-shim-world script proc)
  (define root (make-temporary-file "native-api-git-shim-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (define bin (build-path root "bin"))
                  (define repo (build-path root "repo"))
                  (make-directory* repo)
                  (install-shim! bin "git" script)
                  (define env (base-env root (format "~a:/usr/bin:/bin" bin)))
                  (environment-set! env "GIT_SHIM_LOG" (path->string (build-path root "git.log")))
                  (parameterize ([current-environment-variables env])
                    (check-not-false (find-executable-path "git")
                                     "the fixture must resolve git through PATH")
                    (clear-api-cache!)
                    (proc repo)))
                (lambda () (delete-directory/files root))))

;; The Python parity driver: import the controller BY PATH (the module is
;; importable; `main()` only runs under __main__) and drive the SAME five read
;; functions with the SAME shim fixtures. It prints
;; {"ok":true,"value":...} or {"ok":false,"reason":...} so outcomes compare
;; exactly — never by scraping stderr.
(define python-driver
  #<<PY
import importlib.util, json, sys
spec = importlib.util.spec_from_file_location("gsd_delivery", sys.argv[1])
mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)
case = json.loads(sys.argv[2])
fn = case["fn"]
try:
    if fn == "api":
        value = mod.api(case["slug"], case["route"])
    elif fn == "checks":
        value = mod.checks(case["slug"], case["sha"])
    elif fn == "trusted_check":
        value = mod.trusted_check(case["slug"], case["sha"], case["name"], case["event"],
                                  case.get("branch"))
    elif fn == "protection":
        value = mod.protection(case["slug"], case["names"])
    elif fn == "validate_merge":
        value = mod.validate_merge(case["pr"], case["commit"], case["slug"], case["merge"],
                                   case["head"])
    else:
        raise SystemExit("unknown parity case: " + fn)
    print(json.dumps({"ok": True, "value": value}))
except mod.Pending as exc:
    print(json.dumps({"ok": False, "reason": str(exc)}))
PY
  )

(define (run-python-case root python spec)
  (define result
    (run-subprocess (path->string python)
                    #:args
                    (list "-c" python-driver (path->string python-controller) (jsexpr->string spec))
                    #:directory root
                    #:environment (current-environment-variables)
                    #:timeout 60
                    #:process-group? #t))
  (unless (and (zero? (subprocess-result-exit-code result))
               (not (subprocess-result-timed-out? result)))
    (error 'python-parity "python parity driver failed: ~a" (subprocess-result-stderr result)))
  (string->jsexpr (subprocess-result-stdout result)))

;; Order-insensitive jsexpr shape: hashes become sorted key/value lists, so a
;; Python dict and a Racket hash compare by CONTENT (never by key order or hash
;; equality flavour) and (void)/null compare equal.
(define (canonical value)
  (cond
    [(void? value) 'null]
    [(eq? value 'null) 'null]
    [(hash? value)
     (cons 'object
           (for/list ([key (in-list (sort (map symbol->string (hash-keys value)) string<?))])
             (cons key (canonical (hash-ref value (string->symbol key))))))]
    [(list? value) (cons 'array (map canonical value))]
    [else value]))

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

;; A bare origin that publishes refs/heads/main and refs/pull/3/head as two
;; DIFFERENT commits, plus a clone whose FETCH_HEAD the port must validate. The
;; PR ref commit differs from main on purpose: deleting the `git fetch` call
;; leaves no FETCH_HEAD at all, and a main-sha FETCH_HEAD could never equal the
;; asserted PR head.
(define (with-repo-world proc)
  (define root (make-temporary-file "native-api-repo-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-environment-variables (base-env root "/usr/bin:/bin")])
                    ;; The fixture IS a git fixture: assert the resolution before trusting it.
                    (check-not-false (find-executable-path "git")
                                     "the fixture must resolve git through PATH")
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
                    (define main-sha (git work "rev-parse" "HEAD"))
                    (put (build-path work "pr-only") "pr head\n")
                    (git work "add" "pr-only")
                    (git work "commit" "-m" "pr head")
                    (define pr-sha (git work "rev-parse" "HEAD"))
                    (git work "push" "origin" "HEAD:refs/pull/3/head")
                    (check-not-equal? pr-sha main-sha "the PR ref must carry its own commit")
                    (git root "clone" "-b" "main" (path->string origin) (path->string subject))
                    (proc subject pr-sha main-sha)))
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

  (test-case "a cached second call spawns once; #:refresh? spawns again and returns the new payload"
    ;; The fixture is REPLACED between reads, so a cache hit can only return the
    ;; stale payload and a refresh is the only way to observe the new one.
    (with-gh-world
     (list (list (route slug "pulls/7") (jsexpr->string (hasheq 'number 7))))
     (lambda (root)
       (check-equal? (hash-ref (api-call slug "pulls/7") 'number) 7)
       (check-equal? (hash-ref (api-call slug "pulls/7") 'number) 7)
       (check-equal? (length (gh-log root)) 1 "the cached second call must not spawn gh")
       ;; The remote moves on: only a refresh may see it.
       (set-fixture! root 0 (jsexpr->string (hasheq 'number 8)))
       (check-equal? (hash-ref (api-call slug "pulls/7") 'number)
                     7
                     "an unrefreshed read stays cached")
       (check-equal? (hash-ref (api-call slug "pulls/7" #:refresh? #t) 'number) 8)
       (check-equal? (length (gh-log root)) 2 "a refresh must spawn gh again")
       ;; The fresh payload is STORED: the next plain read is the
       ;; new one, and still without a third spawn.
       (check-equal? (hash-ref (api-call slug "pulls/7") 'number) 8)
       (check-equal? (length (gh-log root)) 2))))

  ;; ============================================================
  ;; session budget (ONE budget for gh AND repo git)
  ;; ============================================================

  (test-case "a session keeps its budget and caches through clear-api-cache!"
    (with-gh-world
     (list (list (route slug "pulls/7") (jsexpr->string (hasheq 'number 7))))
     (lambda (root)
       (define session (make-delivery-api-session #f))
       (define call (delivery-api-session-api-call session))
       (define start (delivery-api-session-start-ms session))
       (check-equal? (hash-ref (call slug "pulls/7") 'number) 7)
       (check-equal? (length (gh-log root)) 1)
       ;; The memo is dropped ...
       (clear-api-cache!)
       (check-equal? (hash-ref (call slug "pulls/7") 'number) 7)
       (check-equal? (length (gh-log root)) 2)
       ;; ... but the budget and its start time never moved.
       (check-equal? (delivery-api-session-start-ms session) start)
       (check-equal? (delivery-api-session-budget session) (budget-seconds))
       ;; A SPENT budget stays spent across a clear: clearing may
       ;; never buy a fresh budget.
       (define spent
         (make-delivery-api-session #f
                                    #:budget 10.0
                                    #:start-ms (- (current-inexact-monotonic-milliseconds) 60000.0)))
       (clear-api-cache!)
       (define reason
         (delivery-refusal (lambda () ((delivery-api-session-api-call spent) slug "pulls/7"))))
       (check-true (string-prefix? reason "budget-exhausted:") reason)
       (check-true (infrastructure-refusal? reason) reason))))

  (test-case "clear-api-cache! neither drops nor recreates the DEFAULT session"
    ;; The explicit-session case above proves a SESSION keeps its budget; this
    ;; one proves the module-level shims keep using the SAME default session. A
    ;; regression restoring `(set-box! default-session-box #f)` inside
    ;; clear-api-cache! would either leave no session (#f deadline) or rebuild
    ;; one with a fresh start-ms (a different deadline).
    (with-gh-world
     (list (list (route slug "pulls/7") (jsexpr->string (hasheq 'number 7))))
     (lambda (root)
       (check-equal? (hash-ref (api-call slug "pulls/7") 'number) 7)
       (define deadline-before (default-session-deadline-ms))
       (check-true (real? deadline-before) "a shimmed read must have created the default session")
       (check-equal? (length (gh-log root)) 1)
       (clear-api-cache!)
       ;; Read the deadline BEFORE any further call: the accessor is pure, so a
       ;; reset box shows up here as #f and a rebuilt session as a later
       ;; deadline (its start-ms is re-sampled).
       (check-equal? (default-session-deadline-ms)
                     deadline-before
                     "clear-api-cache! must not reset or recreate the default session")
       ;; The clear really dropped the memo, so the assertion above is not
       ;; vacuous ...
       (check-equal? (hash-ref (api-call slug "pulls/7") 'number) 7)
       (check-equal? (length (gh-log root)) 2)
       ;; ... and the re-spawn was served by the SAME budget.
       (check-equal? (default-session-deadline-ms) deadline-before))))

  (test-case "one session shares ONE budget: a gh read that spends it makes fetch-head refuse"
    ;; The gh shim consumes the rest of the session budget and the git shim WOULD
    ;; succeed, so only a shared budget can refuse here (a second runner would
    ;; hand fetch-head a fresh budget and return the shim's 40-hex head).
    ;; The seam refuses below 1.0 s remaining and clamps a command to left-0.5,
    ;; so a ~1.5 s remaining budget plus a 0.75 s gh read lands mid-window.
    (with-budget-world
     (string-append "#!/bin/sh"
                    "\nprintf '%s\\n' \"$*\" >> \"$GH_SHIM_LOG\""
                    "\nsleep 0.75"
                    "\nprintf '%s\\n' '{\"number\": 7}'")
     (string-append "#!/bin/sh"
                    "\nprintf '%s\\n' \"$*\" >> \"$GIT_SHIM_LOG\""
                    "\nprintf '%s\\n' 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'")
     (lambda (_root repo)
       (define now (current-inexact-monotonic-milliseconds))
       (define session (make-delivery-api-session repo #:budget 10.0 #:start-ms (- now 8500.0)))
       (check-equal? (hash-ref ((delivery-api-session-api-call session) slug "pulls/7") 'number) 7)
       (define reason (delivery-refusal (lambda () ((delivery-api-session-fetch-head session) 3))))
       (check-true (string-prefix? reason "budget-exhausted:") reason)
       (check-true (infrastructure-refusal? reason) reason)
       ;; The refusal happens BEFORE any git command: the shim was never run (a
       ;; fresh budget would have run it and returned a sha).
       (check-false (file-exists? (getenv "GIT_SHIM_LOG"))))))

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

  (test-case "checks returns '() for total_count 0 in a single empty page (no extra request)"
    (with-gh-world (list (list (checks-route head-sha 1) (checks-page '() 0)))
                   (lambda (root)
                     (check-equal? (checks slug head-sha) '())
                     ;; An empty page must never paginate further.
                     (check-equal? (length (gh-log root)) 1))))

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

  (test-case "a regexp-metacharacter slug authenticates only its literal repository"
    ;; `coinerd/q.b` must not match `coinerd/qXb`: without regexp-quote the `.`
    ;; is a wildcard, and a look-alike repository would authenticate.
    (define dotted "coinerd/q.b")
    (define (dotted-page details)
      (list (list (route dotted (format "commits/~a/check-runs?per_page=100&page=1" head-sha))
                  (checks-page (list (check-run #:details details)) 1))))
    (with-gh-world (dotted-page "https://github.com/coinerd/qXb/actions/runs/999/job/123")
                   (lambda (_root)
                     (check-equal? (delivery-refusal
                                    (lambda () (trusted-check dotted head-sha "ci" "pull_request")))
                                   "check has no authenticated Actions run: ci")))
    ;; The exact slug is accepted end to end (check AND its Actions run).
    (with-gh-world (append (dotted-page "https://github.com/coinerd/q.b/actions/runs/999/job/123")
                           (list (list (route dotted "actions/runs/999")
                                       (jsexpr->string (actions-run #:repo dotted)))))
                   (lambda (_root)
                     (check-equal? (trusted-check dotted head-sha "ci" "pull_request") (void)))))

  (test-case "trusted-check refuses hostile details_url prefixes and suffixes"
    (define (refused! details)
      (trusted-world (check-run #:details details)
                     (actions-run)
                     (lambda (_root)
                       (check-equal? (delivery-refusal
                                      (lambda () (trusted-check slug head-sha "ci" "pull_request")))
                                     "check has no authenticated Actions run: ci"
                                     details))))
    ;; A correct-looking URL hidden behind a hostile prefix, then hostile
    ;; suffixes on an otherwise exact URL, then a trailing newline: Racket's `$`
    ;; is end-of-input, so no suffix (newline included) is smuggled past it.
    (define hostile
      (list (format "https://evil/x/https://github.com/~a/actions/runs/999/job/123" slug)
            (format "https://github.com/~a/actions/runs/999/job/123/extra" slug)
            (format "https://github.com/~a/actions/runs/999/job/123?x=1" slug)
            (format "https://github.com/~a/actions/runs/999/job/123\n" slug)))
    (for ([details (in-list hostile)])
      (refused! details))
    ;; Both anchors asserted directly (the URL pattern and the SHA guard use
    ;; `$`, which in Racket rejects a trailing newline; `\z` would too).
    (check-false (regexp-match?
                  (string-append "^https://github\\.com/x/actions/runs/[0-9]+/job/[0-9]+$")
                  "https://github.com/x/actions/runs/1/job/2\n"))
    (check-false (full-sha? (string-append head-sha "\n"))))

  (test-case "trusted-check refuses a check recorded on another head SHA"
    (trusted-world
     (check-run #:head base-sha)
     (actions-run)
     (lambda (_root)
       (check-equal? (delivery-refusal (lambda () (trusted-check slug head-sha "ci" "pull_request")))
                     "pending, failed, or untrusted check: ci"))))

  (test-case "trusted-check refuses an Actions run owned by another repository"
    ;; The details_url names THIS repository correctly; the run itself claims
    ;; another owner, so the provenance chain is broken.
    (trusted-world
     (check-run)
     (actions-run #:repo "attacker/q")
     (lambda (_root)
       (check-equal? (delivery-refusal (lambda () (trusted-check slug head-sha "ci" "pull_request")))
                     "check workflow/ref/event mismatch: ci"))))

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

  (test-case "trusted-check refuses an Actions run whose status is not completed"
    ;; Kills the run-level `status == "completed"` guard ALONE: the check run and
    ;; every other field of the Actions run (conclusion, head_sha, event, path,
    ;; repository, head_branch) are exactly what the same query trusts, so only
    ;; this guard can produce the refusal.
    (trusted-world
     (check-run)
     (actions-run #:status "in_progress")
     (lambda (_root)
       (check-equal? (delivery-refusal
                      (lambda ()
                        (trusted-check slug head-sha "ci" "pull_request" #:branch "campaign/w1")))
                     "check workflow/ref/event mismatch: ci"))))

  (test-case "trusted-check refuses an Actions run whose conclusion is not success"
    ;; Kills the run-level `conclusion == "success"` guard ALONE: status stays
    ;; completed and every other run field matches the trusted shape.
    (trusted-world
     (check-run)
     (actions-run #:conclusion "failure")
     (lambda (_root)
       (check-equal? (delivery-refusal
                      (lambda ()
                        (trusted-check slug head-sha "ci" "pull_request" #:branch "campaign/w1")))
                     "check workflow/ref/event mismatch: ci"))))

  (test-case "trusted-check refuses an Actions run recorded on another head SHA"
    ;; Kills the run-level `head_sha == sha` guard ALONE: the CHECK is on the
    ;; queried sha and every other run field matches, so only the RUN's sha can
    ;; reject this (the check-level head_sha guard is killed by its own case).
    (trusted-world
     (check-run)
     (actions-run #:head base-sha)
     (lambda (_root)
       (check-equal? (delivery-refusal
                      (lambda ()
                        (trusted-check slug head-sha "ci" "pull_request" #:branch "campaign/w1")))
                     "check workflow/ref/event mismatch: ci"))))

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

  (test-case "fetch-head fetches refs/pull/<n>/head through the git seam and returns the PR commit"
    (with-repo-world (lambda (subject pr-sha main-sha)
                       ;; The PR ref commit is NOT main: a deleted `git fetch` (no FETCH_HEAD)
                       ;; and a main-sha FETCH_HEAD both fail here.
                       (define fetched (fetch-head subject 3))
                       (check-equal? fetched pr-sha)
                       (check-not-equal? fetched main-sha)
                       (check-equal? (git subject "rev-parse" "FETCH_HEAD") pr-sha)
                       (check-equal? (git subject "rev-parse" "HEAD") main-sha))))

  (test-case "a session fetch-head fetches the PR ref through the shared bounded runner"
    (with-repo-world (lambda (subject pr-sha main-sha)
                       (define session (make-delivery-api-session subject))
                       (define fetched ((delivery-api-session-fetch-head session) 3))
                       (check-equal? fetched pr-sha)
                       (check-not-equal? fetched main-sha)
                       (check-equal? (git subject "rev-parse" "FETCH_HEAD") pr-sha))))

  (test-case "fetch-head refuses a non-40-hex FETCH_HEAD"
    ;; The git shim answers the fetch and then prints a head no real git would
    ;; produce, so the refusal is attributable to the FETCH_HEAD guard alone.
    (with-git-shim-world (string-append "#!/bin/sh"
                                        "\nprintf '%s\\n' \"$*\" >> \"$GIT_SHIM_LOG\""
                                        "\ncase \"$*\" in"
                                        "\n  *'rev-parse FETCH_HEAD'*) printf '%s\\n' 'deadbeef' ;;"
                                        "\nesac"
                                        "\nexit 0")
                         (lambda (repo)
                           (check-equal? (delivery-refusal (lambda () (fetch-head repo 3)))
                                         "malformed fetched PR head")
                           ;; Both commands really ran (fetch, then rev-parse): the guard checked a
                           ;; fetched value, it did not skip the fetch.
                           (check-equal? (length (file->lines (getenv "GIT_SHIM_LOG"))) 2))))

  ;; ============================================================
  ;; differential parity with the imported Python controller
  ;; ============================================================

  (test-case "differential parity with the imported Python controller (same gh fixtures)"
    ;; Parity IS the contract, so a host without an interpreter is a FAILED
    ;; gate (fail closed), never a silent SKIP.
    (define python (or (find-executable-path "python3") (find-executable-path "python")))
    (unless python
      (fail
       "python3/python is required for the differential read-layer parity case; a missing interpreter is a FAILED gate, never a SKIP"))
    ;; One slug per case keeps every route (and every fixture file) distinct.
    (define (case-slug n)
      (format "coinerd/api-parity-~a" n))
    (define (page sha slug* runs total)
      (list (list (format "repos/~a/commits/~a/check-runs?per_page=100&page=1" slug* sha)
                  (checks-page runs total))))
    (define case-sha-4 (make-string 40 #\4))
    (define case-sha-5 (make-string 40 #\5))
    (define case-sha-6 (make-string 40 #\6))
    (define case-sha-7 (make-string 40 #\7))
    (define case-sha-8 (make-string 40 #\8))
    (define case-sha-9 (make-string 40 #\9))
    ;; Every entry: (label python-spec fixtures expected racket-thunk). Both
    ;; sides use the same functions, the same fixtures and the same arguments,
    ;; and BOTH outcomes must additionally match `expected` — either 'ok or the
    ;; EXACT reason string. Comparing the two sides only against EACH OTHER
    ;; would let a gh-shim fixture/route drift produce two identical
    ;; "delivery command failed (gh, exit 66)" results and call that parity.
    (define cases
      (list
       (list "api-object"
             (hasheq 'fn "api" 'slug (case-slug 1) 'route "pulls/7")
             (list (list (route (case-slug 1) "pulls/7")
                         (jsexpr->string (hasheq 'number 7 'state "closed"))))
             'ok
             (lambda () (api-call (case-slug 1) "pulls/7")))
       (list "api-malformed-json"
             (hasheq 'fn "api" 'slug (case-slug 2) 'route "pulls/8")
             (list (list (route (case-slug 2) "pulls/8") "not json at all"))
             "malformed GitHub response for pulls/8"
             (lambda () (api-call (case-slug 2) "pulls/8")))
       (list "api-scalar-payload"
             (hasheq 'fn "api" 'slug (case-slug 3) 'route "pulls/9")
             (list (list (route (case-slug 3) "pulls/9") "42"))
             "malformed GitHub response for pulls/9"
             (lambda () (api-call (case-slug 3) "pulls/9")))
       (list "checks-missing-check-runs"
             (hasheq 'fn "checks" 'slug (case-slug 4) 'sha case-sha-4)
             ;; No check_runs key at all: both sides refuse "invalid check result".
             (list (list (format "repos/~a/commits/~a/check-runs?per_page=100&page=1"
                                 (case-slug 4)
                                 case-sha-4)
                         (jsexpr->string (hasheq 'total_count 1))))
             "invalid check result"
             (lambda () (checks (case-slug 4) case-sha-4)))
       (list "checks-non-object-run"
             (hasheq 'fn "checks" 'slug (case-slug 5) 'sha case-sha-5)
             (page case-sha-5 (case-slug 5) (list 7) 2)
             "invalid check run entry"
             (lambda () (checks (case-slug 5) case-sha-5)))
       (list "checks-string-total"
             (hasheq 'fn "checks" 'slug (case-slug 6) 'sha case-sha-6)
             ;; total_count as a JSON string: Python requires isinstance(int),
             ;; Racket requires exact-integer? — both refuse.
             (page case-sha-6 (case-slug 6) '() "1")
             "malformed check-total"
             (lambda () (checks (case-slug 6) case-sha-6)))
       (list "trusted-check-foreign-details"
             (hasheq 'fn
                     "trusted_check"
                     'slug
                     (case-slug 7)
                     'sha
                     case-sha-7
                     'name
                     "ci"
                     'event
                     "pull_request")
             (page case-sha-7
                   (case-slug 7)
                   (list (check-run #:head case-sha-7
                                    #:details
                                    "https://github.com/attacker/q/actions/runs/999/job/123"))
                   1)
             "check has no authenticated Actions run: ci"
             (lambda () (trusted-check (case-slug 7) case-sha-7 "ci" "pull_request")))
       (list "trusted-check-event-mismatch"
             (hasheq 'fn
                     "trusted_check"
                     'slug
                     (case-slug 8)
                     'sha
                     case-sha-8
                     'name
                     "ci"
                     'event
                     "pull_request")
             (append (page case-sha-8
                           (case-slug 8)
                           (list (check-run #:head case-sha-8
                                            #:details
                                            (format "https://github.com/~a/actions/runs/777/job/123"
                                                    (case-slug 8))))
                           1)
                     (list (list (route (case-slug 8) "actions/runs/777")
                                 (jsexpr->string (actions-run #:head case-sha-8 #:event "push")))))
             "check workflow/ref/event mismatch: ci"
             (lambda () (trusted-check (case-slug 8) case-sha-8 "ci" "pull_request")))
       (list "trusted-check-success"
             (hasheq 'fn
                     "trusted_check"
                     'slug
                     (case-slug 9)
                     'sha
                     case-sha-9
                     'name
                     "ci"
                     'event
                     "pull_request")
             (append (page case-sha-9
                           (case-slug 9)
                           (list (check-run #:head case-sha-9
                                            #:details
                                            (format "https://github.com/~a/actions/runs/888/job/123"
                                                    (case-slug 9))))
                           1)
                     (list (list (route (case-slug 9) "actions/runs/888")
                                 (jsexpr->string (actions-run #:head case-sha-9
                                                              #:repo (case-slug 9))))))
             'ok
             (lambda () (trusted-check (case-slug 9) case-sha-9 "ci" "pull_request")))
       (list "protection-weakened"
             (hasheq 'fn "protection" 'slug (case-slug 10) 'names (list "ci"))
             (list (list (route (case-slug 10) "branches/main/protection")
                         (protection-policy #:strict #f)))
             "default branch protection is absent or weakened"
             (lambda () (protection (case-slug 10) (list "ci"))))
       (list "protection-missing-name"
             (hasheq 'fn "protection" 'slug (case-slug 11) 'names (list "ci" "lint"))
             (list (list (route (case-slug 11) "branches/main/protection")
                         (protection-policy #:contexts '("ci"))))
             "branch protection does not require expected checks: lint"
             (lambda () (protection (case-slug 11) (list "ci" "lint"))))
       (list "validate-merge-not-merged"
             (hasheq 'fn
                     "validate_merge"
                     'pr
                     (merged-pr #:merged #f #:merged-at 'null #:state "open")
                     'commit
                     (merge-commit)
                     'slug
                     slug
                     'merge
                     merge-sha
                     'head
                     head-sha)
             '()
             "implementation PR is not merged"
             (lambda ()
               (validate-merge (merged-pr #:merged #f #:merged-at 'null #:state "open")
                               (merge-commit)
                               slug
                               merge-sha
                               head-sha)))
       (list "validate-merge-success"
             (hasheq 'fn
                     "validate_merge"
                     'pr
                     (merged-pr)
                     'commit
                     (merge-commit)
                     'slug
                     slug
                     'merge
                     merge-sha
                     'head
                     head-sha)
             '()
             'ok
             (lambda () (validate-merge (merged-pr) (merge-commit) slug merge-sha head-sha)))))
    ;; The table size is FIXED: a case (or its fixtures) silently disappearing
    ;; must fail loudly instead of quietly shrinking parity coverage.
    (check-equal? (length cases) 13 "the parity table must carry exactly 13 cases")
    ;; A declared outcome is asserted against EACH side separately, so the two
    ;; sides can no longer agree on one transport failure and call it parity.
    (define (check-declared-outcome! label side outcome expected)
      (cond
        [(eq? expected 'ok)
         (check-true (hash-ref outcome 'ok) (format "~a: ~a must succeed" label side))]
        [else
         (check-false (hash-ref outcome 'ok) (format "~a: ~a must refuse" label side))
         (check-equal? (hash-ref outcome 'reason #f)
                       expected
                       (format "~a: ~a reason" label side))]))
    (with-gh-world
     (append* (for/list ([case (in-list cases)])
                (match-define (list _label _spec fixtures _expected _thunk) case)
                fixtures))
     (lambda (root)
       (for ([case (in-list cases)])
         (match-define (list label spec _fixtures expected thunk) case)
         (define python-outcome (run-python-case root python spec))
         (define racket-outcome
           (with-handlers ([exn:fail:delivery-action? (lambda (error)
                                                        (hasheq 'ok #f 'reason (exn-message error)))])
             (hasheq 'ok #t 'value (thunk))))
         (check-declared-outcome! label "racket" racket-outcome expected)
         (check-declared-outcome! label "python" python-outcome expected)
         (check-equal? (canonical racket-outcome) (canonical python-outcome) label))))))
