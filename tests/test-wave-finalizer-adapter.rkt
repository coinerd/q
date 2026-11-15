#lang racket

;; @speed fast
;; @suite testing
;; @covers scripts/github/wave-finalizer-adapter.rkt
;; @covers scripts/github/wave-finalizer.rkt
;; @covers scripts/gsd-delivery-api.rkt
;; @covers scripts/gsd-delivery-git.rkt
;;
;; Port job J2: the protected-wave finalizer's authenticated GitHub adapter
;; (scripts/github/wave-finalizer-adapter.rkt) on top of the q delivery seams.
;;
;; Evidence law: a fake `gh` executable is installed FIRST on PATH. It logs the
;; argument vector it received and then prints the fixture registered for the
;; (method, route) pair it parsed out of that vector, so every assertion below
;; is made from the child's own evidence — the exact argv (no shell: query
;; strings stay inside ONE argument), the HTTP method (-X PUT / -X PATCH /
;; graphql POST), the sorted `-f` field order, and the spawn count
;; (memoization, refresh, budget refusals).
;;
;; What the cases prove:
;;   (a) exact argv for every adapter method, including the REST mutation
;;       methods and the caller-supplied GraphQL query/mutation POST shape;
;;   (b) actions/runs pagination across two pages driven by total_count, plus
;;       the fail-closed refusals (missing workflow_runs, missing/invalid
;;       total_count, a non-object run entry, incomplete pagination, and a
;;       total_count of 0 served in exactly one request);
;;   (c) one adapter = one session = one budget window: exactly ONE runner is
;;       created per adapter and it is built from the SESSION's own
;;       (budget, start-ms) pair; a gh call that spends the window makes the
;;       next call (a read AND a mutation) refuse with `budget-exhausted`
;;       without spawning gh;
;;   (d) credential isolation: a gh shim that echoes GITHUB_TOKEN to stderr and
;;       fails never leaks the token into the typed refusal;
;;   (e) every adapter failure surfaces in the finalizer taxonomy
;;       (exn:fail:finalizer) with the expected kind and message, including the
;;       CLI's argparse-parity exit 2 when no verifier is injected.

(require rackunit
         json
         racket/file
         racket/list
         racket/path
         racket/string
         "../scripts/github/wave-finalizer.rkt"
         "../scripts/github/wave-finalizer-adapter.rkt"
         "../scripts/gsd-delivery-api.rkt"
         "../scripts/gsd-delivery-git.rkt")

(define slug "acme/widgets")
(define head-sha (make-string 40 #\a))
(define merge-sha (make-string 40 #\b))
(define pr-number 17)
(define issue-number 40)
(define get-query "QUERY:GET-FIELD")
(define update-mutation "MUTATION:UPDATE-FIELD")

;; ============================================================
;; Fixture world
;; ============================================================

(define (environment-set! env name value)
  (environment-variables-set! env (string->bytes/utf-8 name) (string->bytes/utf-8 value)))

(define (base-env root path)
  (define env (make-environment-variables))
  (for ([pair (in-list `(("PATH" . ,path) ("HOME" . ,(path->string root))
                                          ("GIT_CONFIG_NOSYSTEM" . "1")
                                          ("GIT_TERMINAL_PROMPT" . "0")))])
    (environment-set! env (car pair) (cdr pair)))
  env)

(define (put path text)
  (make-directory* (path-only path))
  (call-with-output-file path #:exists 'truncate (lambda (out) (display text out))))

;; The fake `gh`: log the argv, parse the (method, route) pair out of it, and
;; print the fixture registered for that key. `-X PUT`/`-X PATCH` are part of
;; the key; the graphql subcommand is keyed by the caller's query text, because
;; two GraphQL methods POST the same route.
(define (shim-script entries)
  (string-append
   "#!/bin/sh\n"
   "printf '%s\\n' \"$*\" >> \"$GH_SHIM_LOG\"\n"
   "route=\n"
   "method=GET\n"
   "previous=\n"
   "for argument in \"$@\"; do\n"
   "  case \"$argument\" in\n"
   "    repos/*) route=$argument ;;\n"
   "    graphql) route=graphql ;;\n"
   "    PUT|PATCH|POST) if [ \"$previous\" = \"-X\" ]; then method=$argument; fi ;;\n"
   "  esac\n"
   "  previous=$argument\n"
   "done\n"
   "key=\"$method $route\"\n"
   "case \"$*\" in\n"
   "  *\"query=QUERY:GET-FIELD\"*) key=graphql-get ;;\n"
   "  *\"query=MUTATION:UPDATE-FIELD\"*) key=graphql-update ;;\n"
   "esac\n"
   "case \"$key\" in\n"
   (string-join (for/list ([entry (in-list entries)])
                  (format "  ~s) /bin/cat \"$GH_SHIM_FIXTURES/~a\" ;;" (car entry) (cdr entry)))
                "\n")
   "\n"
   "  *) printf 'no fixture for %s\\n' \"$key\" >&2; exit 66 ;;\n"
   "esac\n"))

;; fixtures: list of (key . response-body). `script`, when given, replaces the
;; standard shim (the budget and credential-isolation worlds); the fixtures are
;; still installed so a custom script can `cat` them.
(define (with-gh-world fixtures script proc)
  (define root (make-temporary-file "wave-finalizer-adapter-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define bin (build-path root "bin"))
     (define fixtures-dir (build-path root "fixtures"))
     (define shim (build-path bin "gh"))
     (make-directory* bin)
     (make-directory* fixtures-dir)
     (define entries
       (for/list ([fixture (in-list fixtures)]
                  [index (in-naturals)])
         (define file (format "fixture-~a.json" index))
         (put (build-path fixtures-dir file) (cdr fixture))
         (cons (car fixture) file)))
     (put shim (or script (shim-script entries)))
     (file-or-directory-permissions shim #o700)
     (define env (base-env root (format "~a:/usr/bin:/bin" bin)))
     (environment-set! env "GH_SHIM_LOG" (path->string (build-path root "gh-invocations.log")))
     (environment-set! env "GH_SHIM_FIXTURES" (path->string fixtures-dir))
     (parameterize ([current-environment-variables env])
       (check-not-false (find-executable-path "gh") "the fixture must resolve gh through PATH")
       (proc root)))
   (lambda () (delete-directory/files root))))

(define (with-shim fixtures proc)
  (with-gh-world fixtures #f proc))

(define (with-script fixtures script proc)
  (with-gh-world fixtures script proc))

(define (gh-log root)
  (define log (build-path root "gh-invocations.log"))
  (if (file-exists? log)
      (file->lines log)
      '()))

;; Rewrite the Nth installed fixture IN PLACE, so a cache hit can only return the
;; stale payload and a refresh is the only way to observe the new one.
(define (set-fixture! root index body)
  (put (build-path root "fixtures" (format "fixture-~a.json" index)) body))

(define (json value)
  (jsexpr->string value))

;; ============================================================
;; Adapter helpers
;; ============================================================

(define (verify-fixture repository reference)
  (hasheq 'verified #t 'repository repository 'reference reference))

(define (make-adapter #:verify-evidence [verify-evidence verify-fixture]
                      #:get-field-query [get-field-query get-query]
                      #:update-field-mutation [update-field-mutation update-mutation]
                      #:budget [budget #f]
                      #:start-ms [start-ms #f]
                      #:make-runner [make-runner make-bounded-runner])
  (make-wave-finalizer-adapter slug
                               #:verify-evidence verify-evidence
                               #:get-field-query get-field-query
                               #:update-field-mutation update-field-mutation
                               #:budget budget
                               #:start-ms start-ms
                               #:make-runner make-runner))

;; A refusal is ONLY the finalizer taxonomy; its kind is the Python class name.
(define (finalizer-refusal thunk)
  (with-handlers ([exn:fail:finalizer?
                   (lambda (error)
                     (hasheq 'kind (exn:fail:finalizer-kind error) 'message (exn-message error)))])
    (thunk)
    (fail "expected a typed exn:fail:finalizer refusal, but the call returned")))

(define (refused! thunk kind message)
  (define refusal (finalizer-refusal thunk))
  (check-equal? (hash-ref refusal 'kind) kind (format "~a" refusal))
  (check-equal? (hash-ref refusal 'message) message (format "~a" refusal))
  refusal)

;; Fixture keys (the shim's view of the argv).
(define (pr-key)
  (format "GET repos/~a/pulls/~a" slug pr-number))
(define (protection-key ref)
  (format "GET repos/~a/branches/~a/protection" slug ref))
(define (workflow-runs-route page)
  (format "repos/~a/actions/runs?head_sha=~a&per_page=100&page=~a" slug head-sha page))
(define (workflow-runs-key page)
  (string-append "GET " (workflow-runs-route page)))
(define (checks-key)
  (format "GET repos/~a/commits/~a/check-runs?per_page=100&page=1" slug head-sha))
(define (merge-key)
  (format "PUT repos/~a/pulls/~a/merge" slug pr-number))
(define (issue-key)
  (format "GET repos/~a/issues/~a" slug issue-number))
(define (close-issue-key)
  (format "PATCH repos/~a/issues/~a" slug issue-number))

(define (poll-policy)
  (hasheq 'required_status_checks
          (hasheq 'strict #t 'checks (list (hasheq 'context "ci" 'app_id 15368)) 'contexts '())
          'enforce_admins
          (hasheq 'enabled #t)
          'required_linear_history
          (hasheq 'enabled #t)
          'allow_force_pushes
          (hasheq 'enabled #f)
          'allow_deletions
          (hasheq 'enabled #f)))

(define (open-issue)
  (json (hasheq 'number issue-number 'state "open")))
(define (closed-issue)
  (json (hasheq 'number issue-number 'state "closed")))
(define (open-pr)
  (json (hasheq 'number pr-number 'state "open" 'merged #f)))
(define (merged-pr)
  (json (hasheq 'number pr-number 'state "closed" 'merged #t 'merge_commit_sha merge-sha)))
(define (graphql-get-body)
  (json (hasheq 'data (hasheq 'node (hasheq 'field (hasheq 'value "Todo"))))))
(define (graphql-update-body)
  (json (hasheq 'data (hasheq 'node (hasheq 'field (hasheq 'value "Done"))))))

;; ============================================================
;; (a) exact argv per method
;; ============================================================

(module+ test
  (test-case "get-pr reads the pull route through the session; refresh is the only re-spawn"
    (with-shim
     (list (cons (pr-key) (open-pr)))
     (lambda (root)
       (define adapter (make-adapter))
       (check-equal? (hash-ref (wave-finalizer-adapter-get-pr adapter pr-number) 'number) pr-number)
       (check-equal? (gh-log root)
                     (list (format "api --hostname github.com repos/~a/pulls/~a" slug pr-number)))
       ;; A repeated read is served by the session memo: no second spawn.
       (check-equal? (hash-ref (wave-finalizer-adapter-get-pr adapter pr-number) 'number) pr-number)
       (check-equal? (length (gh-log root)) 1)
       ;; The remote moves on; only #:refresh? may observe it, and the
       ;; fresh payload is stored for the next unrefreshed read.
       (set-fixture! root 0 (json (hasheq 'number (+ 1 pr-number) 'state "open")))
       (check-equal? (hash-ref (wave-finalizer-adapter-get-pr adapter pr-number) 'number) pr-number)
       (check-equal? (hash-ref (wave-finalizer-adapter-get-pr adapter pr-number #:refresh? #t)
                               'number)
                     (+ 1 pr-number))
       (check-equal? (length (gh-log root)) 2)
       (check-equal? (hash-ref (wave-finalizer-adapter-get-pr adapter pr-number) 'number)
                     (+ 1 pr-number)))))

  (test-case "get-protection uses the raw branches/<ref>/protection route"
    (with-shim (list (cons (protection-key "main") (json (poll-policy))))
               (lambda (root)
                 (define adapter (make-adapter))
                 (define protection (wave-finalizer-adapter-get-protection adapter "main"))
                 (check-equal? (hash-ref protection 'enforce_admins) (hasheq 'enabled #t))
                 (check-equal?
                  (gh-log root)
                  (list (format "api --hostname github.com repos/~a/branches/main/protection"
                                slug))))))

  (test-case "merge-pr PUTs the sorted merge fields (merge_method before sha)"
    (with-shim
     (list (cons (merge-key) (merged-pr)))
     (lambda (root)
       (define adapter (make-adapter))
       (define response
         (wave-finalizer-adapter-merge-pr adapter pr-number #:sha head-sha #:merge-method "squash"))
       (check-equal? (hash-ref response 'merged) #t)
       (check-equal?
        (gh-log root)
        (list
         (format
          "api --hostname github.com -X PUT repos/~a/pulls/~a/merge -f merge_method=squash -f sha=~a"
          slug
          pr-number
          head-sha))))))

  (test-case "get-issue and close-issue use the exact issue routes and methods"
    (with-shim
     (list (cons (issue-key) (open-issue)) (cons (close-issue-key) (closed-issue)))
     (lambda (root)
       (define adapter (make-adapter))
       (check-equal? (hash-ref (wave-finalizer-adapter-get-issue adapter issue-number) 'state) "open")
       (check-equal? (hash-ref (wave-finalizer-adapter-close-issue adapter issue-number) 'state)
                     "closed")
       (check-equal?
        (gh-log root)
        (list (format "api --hostname github.com repos/~a/issues/~a" slug issue-number)
              (format "api --hostname github.com -X PATCH repos/~a/issues/~a -f state=closed"
                      slug
                      issue-number))))))

  (test-case "get-field and update-field POST the caller's GraphQL text (value passed through)"
    (with-shim
     (list (cons "graphql-get" (graphql-get-body)) (cons "graphql-update" (graphql-update-body)))
     (lambda (root)
       (define adapter (make-adapter))
       (check-equal? (wave-finalizer-adapter-get-field adapter "PVT_1" "PVTI_1" "PVTF_1") "Todo")
       (check-equal? (wave-finalizer-adapter-update-field adapter "PVT_1" "PVTI_1" "PVTF_1" "Done")
                     "Done")
       ;; A structured configured value is passed through as its JSON
       ;; text, never coerced or reordered.
       (check-equal?
        (wave-finalizer-adapter-update-field adapter "PVT_1" "PVTI_1" "PVTF_1" (list "a" 1))
        "Done")
       (check-equal?
        (gh-log root)
        (list
         (format
          "api graphql -X POST -f query=~a -f projectId=PVT_1 -f itemId=PVTI_1 -f fieldId=PVTF_1"
          get-query)
         (format
          "api graphql -X POST -f query=~a -f projectId=PVT_1 -f itemId=PVTI_1 -f fieldId=PVTF_1 -f value=\"Done\""
          update-mutation)
         (format
          "api graphql -X POST -f query=~a -f projectId=PVT_1 -f itemId=PVTI_1 -f fieldId=PVTF_1 -f value=[\"a\",1]"
          update-mutation))))))

  (test-case "list-check-runs returns the session's checks call"
    (with-shim
     (list (cons (checks-key)
                 (json (hasheq 'total_count 1 'check_runs (list (hasheq 'id 9 'name "ci"))))))
     (lambda (root)
       (define adapter (make-adapter))
       (define runs (wave-finalizer-adapter-list-check-runs adapter head-sha))
       (check-equal? (map (lambda (run) (hash-ref run 'id)) runs) '(9))
       (check-equal?
        (gh-log root)
        (list (format "api --hostname github.com repos/~a/commits/~a/check-runs?per_page=100&page=1"
                      slug
                      head-sha))))))

  ;; ============================================================
  ;; (b) actions/runs pagination and fail-closed completeness
  ;; ============================================================

  (test-case "list-workflow-runs paginates across two pages by total_count"
    (with-shim
     (list (cons (workflow-runs-key 1)
                 (json (hasheq 'total_count 3 'workflow_runs (list (hasheq 'id 1) (hasheq 'id 2)))))
           (cons (workflow-runs-key 2)
                 (json (hasheq 'total_count 3 'workflow_runs (list (hasheq 'id 3))))))
     (lambda (root)
       (define adapter (make-adapter))
       (define runs (wave-finalizer-adapter-list-workflow-runs adapter head-sha))
       (check-equal? (map (lambda (run) (hash-ref run 'id)) runs) '(1 2 3))
       (check-equal? (gh-log root)
                     (list (format "api --hostname github.com ~a" (workflow-runs-route 1))
                           (format "api --hostname github.com ~a" (workflow-runs-route 2)))))))

  (test-case "list-workflow-runs re-reads every call (no stale memo for pre-merge evidence)"
    (with-shim
     (list (cons (workflow-runs-key 1)
                 (json (hasheq 'total_count 1 'workflow_runs (list (hasheq 'id 1))))))
     (lambda (root)
       (define adapter (make-adapter))
       (check-equal? (length (wave-finalizer-adapter-list-workflow-runs adapter head-sha)) 1)
       (check-equal? (length (gh-log root)) 1)
       ;; The remote gains a run attempt: only a fresh read may see it.
       (set-fixture! root 0 (json (hasheq 'total_count 1 'workflow_runs (list (hasheq 'id 2)))))
       (define runs (wave-finalizer-adapter-list-workflow-runs adapter head-sha))
       (check-equal? (map (lambda (run) (hash-ref run 'id)) runs) '(2))
       (check-equal? (length (gh-log root))
                     2
                     "a repeated run read must not be served from the memo"))))

  (test-case "list-workflow-runs returns '() for total_count 0 in exactly one request"
    (with-shim (list (cons (workflow-runs-key 1) (json (hasheq 'total_count 0 'workflow_runs '()))))
               (lambda (root)
                 (check-equal? (wave-finalizer-adapter-list-workflow-runs (make-adapter) head-sha)
                               '())
                 (check-equal? (length (gh-log root)) 1))))

  (test-case "list-workflow-runs fails closed on a page without a workflow_runs list"
    (for ([body (in-list (list (json (hasheq 'total_count 1))
                               (json (hasheq 'total_count 1 'workflow_runs "x"))))])
      (with-shim (list (cons (workflow-runs-key 1) body))
                 (lambda (_root)
                   (refused! (lambda ()
                               (wave-finalizer-adapter-list-workflow-runs (make-adapter) head-sha))
                             "ApiUncertainty"
                             "invalid workflow run result")))))

  (test-case "list-workflow-runs fails closed on a non-object run entry"
    (with-shim
     (list (cons (workflow-runs-key 1) (json (hasheq 'total_count 1 'workflow_runs (list 7)))))
     (lambda (_root)
       (refused! (lambda () (wave-finalizer-adapter-list-workflow-runs (make-adapter) head-sha))
                 "ApiUncertainty"
                 "invalid workflow run entry"))))

  (test-case "list-workflow-runs fails closed on a missing or invalid total_count"
    (for ([body (in-list (list (json (hasheq 'workflow_runs '()))
                               (json (hasheq 'total_count "1" 'workflow_runs '()))
                               (json (hasheq 'total_count 1.5 'workflow_runs '()))
                               (json (hasheq 'total_count #t 'workflow_runs '()))))])
      (with-shim (list (cons (workflow-runs-key 1) body))
                 (lambda (_root)
                   (refused! (lambda ()
                               (wave-finalizer-adapter-list-workflow-runs (make-adapter) head-sha))
                             "ApiUncertainty"
                             "malformed workflow-run total")))))

  (test-case "list-workflow-runs fails closed on incomplete pagination"
    (with-shim (list (cons (workflow-runs-key 1)
                           (json (hasheq 'total_count 3 'workflow_runs (list (hasheq 'id 1)))))
                     (cons (workflow-runs-key 2) (json (hasheq 'total_count 3 'workflow_runs '()))))
               (lambda (_root)
                 (refused! (lambda ()
                             (wave-finalizer-adapter-list-workflow-runs (make-adapter) head-sha))
                           "ApiUncertainty"
                           "workflow run pagination incomplete"))))

  ;; ============================================================
  ;; (c) one adapter = one session = one budget window
  ;; ============================================================

  (test-case "an adapter creates exactly ONE runner, from the session's own budget window"
    (with-shim
     (list (cons (pr-key) (open-pr))
           (cons (merge-key) (merged-pr))
           (cons "graphql-get" (graphql-get-body)))
     (lambda (_root)
       (define runner-calls '())
       (define now (current-inexact-monotonic-milliseconds))
       (define start-ms (- now 1000.0))
       (define adapter
         (make-adapter #:budget 10.0
                       #:start-ms start-ms
                       #:make-runner (lambda (budget start)
                                       (set! runner-calls (cons (list budget start) runner-calls))
                                       (make-bounded-runner budget start))))
       ;; Several reads and mutations: still exactly ONE runner.
       (wave-finalizer-adapter-get-pr adapter pr-number)
       (wave-finalizer-adapter-merge-pr adapter pr-number #:sha head-sha #:merge-method "squash")
       (wave-finalizer-adapter-get-field adapter "PVT_1" "PVTI_1" "PVTF_1")
       (check-equal? (length runner-calls) 1 "the adapter must create one runner, never one per call")
       ;; The runner is built from the SESSION's pair: no fresh
       ;; budget, no fresh start time (a second budget window would
       ;; silently extend the action).
       (check-equal? (car runner-calls)
                     (list (delivery-api-session-budget (wave-finalizer-adapter-session adapter))
                           (delivery-api-session-start-ms (wave-finalizer-adapter-session adapter))))
       (check-equal? (wave-finalizer-adapter-budget adapter) 10.0)
       (check-equal? (wave-finalizer-adapter-deadline-ms adapter) (+ start-ms 10000.0)))))

  (test-case "a gh read that spends the budget makes the later mutation refuse budget-exhausted"
    ;; Budget 10 s started 8 s ago => 2 s remain. The gh shim sleeps 1.1 s and
    ;; the seam clamps that command to remaining-0.5 = 1.5 s, so the read
    ;; succeeds and leaves < 1 s. Only a SHARED window can refuse the next call;
    ;; a second budget window (or a runner with a fresh start time) would
    ;; happily run the mutation.
    (with-script (list (cons (pr-key) (open-pr)) (cons (merge-key) (merged-pr)))
                 (string-append "#!/bin/sh"
                                "\nprintf '%s\\n' \"$*\" >> \"$GH_SHIM_LOG\""
                                "\nsleep 1.1"
                                "\n/bin/cat \"$GH_SHIM_FIXTURES/fixture-0.json\"\n")
                 (lambda (root)
                   (define now (current-inexact-monotonic-milliseconds))
                   (define adapter (make-adapter #:budget 10.0 #:start-ms (- now 8000.0)))
                   (check-equal? (hash-ref (wave-finalizer-adapter-get-pr adapter pr-number) 'number)
                                 pr-number)
                   (define refusal
                     (finalizer-refusal (lambda ()
                                          (wave-finalizer-adapter-merge-pr adapter
                                                                           pr-number
                                                                           #:sha head-sha
                                                                           #:merge-method "squash"))))
                   (check-equal? (hash-ref refusal 'kind) "ApiUncertainty")
                   (check-true (string-prefix? (hash-ref refusal 'message)
                                               "GitHub API uncertainty during merge pull request: ")
                               (hash-ref refusal 'message))
                   (check-true (string-contains? (hash-ref refusal 'message) "budget-exhausted:")
                               (hash-ref refusal 'message))
                   ;; The refusal happened BEFORE any spawn: a fresh budget would have run gh.
                   (check-equal? (length (gh-log root)) 1))))

  (test-case "an already spent window refuses before spawning gh (reads and mutations)"
    (with-shim (list (cons (pr-key) (open-pr)) (cons (close-issue-key) (closed-issue)))
               (lambda (root)
                 (define now (current-inexact-monotonic-milliseconds))
                 (define adapter (make-adapter #:budget 10.0 #:start-ms (- now 60000.0)))
                 (define read-refusal
                   (finalizer-refusal (lambda () (wave-finalizer-adapter-get-pr adapter pr-number))))
                 (check-equal? (hash-ref read-refusal 'kind) "ApiUncertainty")
                 (check-true (string-prefix? (hash-ref read-refusal 'message)
                                             "GitHub API uncertainty during get pull request: "))
                 (check-true (string-contains? (hash-ref read-refusal 'message) "budget-exhausted:"))
                 (define write-refusal
                   (finalizer-refusal (lambda ()
                                        (wave-finalizer-adapter-close-issue adapter issue-number))))
                 (check-equal? (hash-ref write-refusal 'kind) "ApiUncertainty")
                 (check-true (string-contains? (hash-ref write-refusal 'message) "budget-exhausted:"))
                 (check-equal? (gh-log root) '()))))

  ;; ============================================================
  ;; (d) credential isolation
  ;; ============================================================

  (test-case "a gh shim that echoes GITHUB_TOKEN to stderr never leaks it into a refusal"
    (with-script
     '()
     (string-append "#!/bin/sh"
                    "\nprintf '%s\\n' \"$*\" >> \"$GH_SHIM_LOG\""
                    "\nprintf 'GITHUB_TOKEN=ghp_secret\\n' >&2"
                    "\nprintf 'fatal: bad credentials\\n' >&2"
                    "\nexit 1\n")
     (lambda (_root)
       (define refusal
         (finalizer-refusal (lambda () (wave-finalizer-adapter-get-pr (make-adapter) pr-number))))
       (check-equal? (hash-ref refusal 'kind) "ApiUncertainty")
       (define message (hash-ref refusal 'message))
       (check-true (string-prefix? message "GitHub API uncertainty during get pull request: ")
                   message)
       (check-false (string-contains? message "ghp_secret") message)
       (check-false (string-contains? message "GITHUB_TOKEN") message))))

  ;; ============================================================
  ;; (e) the finalizer taxonomy
  ;; ============================================================

  (test-case "a non-owner/name repository refuses with the ConfigurationError kind"
    (refused! (lambda () (make-wave-finalizer-adapter "acme"))
              "ConfigurationError"
              "repository must be owner/name")
    (refused! (lambda () (make-wave-finalizer-adapter "acme/widgets/extra"))
              "ConfigurationError"
              "repository must be owner/name"))

  (test-case "a malformed GitHub response surfaces as wrapped ApiUncertainty"
    (with-shim
     (list (cons (pr-key) "not json at all"))
     (lambda (_root)
       (refused!
        (lambda () (wave-finalizer-adapter-get-pr (make-adapter) pr-number))
        "ApiUncertainty"
        "GitHub API uncertainty during get pull request: malformed GitHub response for pulls/17"))))

  (test-case "a non-object read response refuses with the adapter-protocol message"
    (with-shim (list (cons (pr-key) "[]") (cons (protection-key "main") "[]") (cons (issue-key) "[]"))
               (lambda (_root)
                 (refused! (lambda () (wave-finalizer-adapter-get-pr (make-adapter) pr-number))
                           "ApiUncertainty"
                           "pull request response is not an object")
                 (refused! (lambda () (wave-finalizer-adapter-get-protection (make-adapter) "main"))
                           "ApiUncertainty"
                           "branch protection response is not an object")
                 (refused! (lambda () (wave-finalizer-adapter-get-issue (make-adapter) issue-number))
                           "ApiUncertainty"
                           "issue 40 response is not an object"))))

  (test-case "a malformed check-run entry surfaces as wrapped ApiUncertainty"
    (with-shim
     (list (cons (checks-key) (json (hasheq 'total_count 1 'check_runs (list 7)))))
     (lambda (_root)
       (refused! (lambda () (wave-finalizer-adapter-list-check-runs (make-adapter) head-sha))
                 "ApiUncertainty"
                 "GitHub API uncertainty during list check runs: invalid check run entry"))))

  (test-case "a malformed mutation response refuses with the exact protocol message"
    ;; Unparseable payload: the same cause, wrapped like every transport failure.
    (with-shim
     (list (cons (merge-key) "not json"))
     (lambda (_root)
       (refused!
        (lambda ()
          (wave-finalizer-adapter-merge-pr (make-adapter)
                                           pr-number
                                           #:sha head-sha
                                           #:merge-method "squash"))
        "ApiUncertainty"
        "GitHub API uncertainty during merge pull request: malformed GitHub mutation response")))
    ;; Non-object payload: the Python adapter-protocol message, unwrapped.
    (with-shim (list (cons (merge-key) "[]") (cons (close-issue-key) "[]"))
               (lambda (_root)
                 (refused! (lambda ()
                             (wave-finalizer-adapter-merge-pr (make-adapter)
                                                              pr-number
                                                              #:sha head-sha
                                                              #:merge-method "squash"))
                           "ApiUncertainty"
                           "malformed GitHub mutation response")
                 (refused! (lambda ()
                             (wave-finalizer-adapter-close-issue (make-adapter) issue-number))
                           "ApiUncertainty"
                           "malformed GitHub mutation response"))))

  (test-case "the caller must supply the GraphQL text it wants executed"
    (with-shim
     '()
     (lambda (_root)
       (refused! (lambda ()
                   (wave-finalizer-adapter-get-field (make-adapter #:get-field-query #f)
                                                     "PVT_1"
                                                     "PVTI_1"
                                                     "PVTF_1"))
                 "ConfigurationError"
                 "the caller must supply the GraphQL project field query text")
       (refused! (lambda ()
                   (wave-finalizer-adapter-update-field (make-adapter #:update-field-mutation #f)
                                                        "PVT_1"
                                                        "PVTI_1"
                                                        "PVTF_1"
                                                        "Done"))
                 "ConfigurationError"
                 "the caller must supply the GraphQL project field mutation text"))))

  (test-case "a field response without data.node.field.value is uncertainty, never a guess"
    (with-shim
     (list (cons "graphql-get" (json (hasheq 'data (hasheq 'node (hasheq))))))
     (lambda (_root)
       (refused! (lambda ()
                   (wave-finalizer-adapter-get-field (make-adapter) "PVT_1" "PVTI_1" "PVTF_1"))
                 "ApiUncertainty"
                 "read project field response is missing data.node.field.value"))))

  (test-case "verify-evidence requires the launcher's injected procedure"
    (with-shim
     '()
     (lambda (_root)
       (refused! (lambda ()
                   (wave-finalizer-adapter-verify-evidence (make-adapter #:verify-evidence #f)
                                                           slug
                                                           (hasheq 'kind "artifact" 'id 7001)))
                 "AttestationError"
                 missing-launcher-message)
       ;; The injected procedure's result is passed through, and its
       ;; shape is held to the adapter-protocol contract.
       (check-equal?
        (hash-ref (wave-finalizer-adapter-verify-evidence (make-adapter)
                                                          slug
                                                          (hasheq 'kind "artifact" 'id 7001))
                  'verified)
        #t)
       (refused! (lambda ()
                   (wave-finalizer-adapter-verify-evidence
                    (make-adapter #:verify-evidence (lambda (_repository _reference) '()))
                    slug
                    (hasheq 'kind "artifact" 'id 7001)))
                 "ApiUncertainty"
                 "attested evidence response is not an object"))))

  (test-case "the CLI refuses without an injected verifier (argparse parity: exit 2)"
    (define exit-code #f)
    (define errors (open-output-string))
    (parameterize ([current-error-port errors])
      (wave-finalizer-adapter-main (list "--repo" slug) #:exit (lambda (code) (set! exit-code code))))
    (check-equal? exit-code 2)
    (define text (get-output-string errors))
    (check-true (string-contains? text missing-launcher-message) text)
    (check-true (string-contains? text "usage:") text))

  (test-case "the CLI reports the adapter's budget window when a verifier is injected"
    (define exit-code #f)
    (define out (open-output-string))
    (parameterize ([current-output-port out])
      (wave-finalizer-adapter-main (list "--repo" slug)
                                   #:verify-evidence verify-fixture
                                   #:exit (lambda (code) (set! exit-code code))))
    (check-equal? exit-code 0)
    (define payload (string->jsexpr (get-output-string out)))
    (check-equal? (hash-ref payload 'status) "ready")
    (check-equal? (hash-ref payload 'repository) slug)
    (check-true (real? (hash-ref payload 'budget-seconds))))

  (test-case "the CLI reports a configuration refusal with exit 1"
    (define exit-code #f)
    (define errors (open-output-string))
    (parameterize ([current-error-port errors])
      (wave-finalizer-adapter-main (list "--repo" "acme")
                                   #:verify-evidence verify-fixture
                                   #:exit (lambda (code) (set! exit-code code))))
    (check-equal? exit-code 1)
    (check-true (string-contains? (get-output-string errors) "repository must be owner/name"))))
