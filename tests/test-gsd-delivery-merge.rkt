#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers scripts/gsd-delivery-merge.rkt
;; @covers scripts/gsd-delivery-write.rkt
;; @covers scripts/gsd-delivery-status.rkt
;; @covers scripts/gsd-delivery-pr.rkt
;; @timeout 420
;;
;; Phase B slice S7: the MUTATING delivery ladder actions `merge` and `prepare`
;; of scripts/gsd-delivery.py are ported to Racket in
;; scripts/gsd-delivery-merge.rkt. The module is NOT wired into the coordinator
;; by this slice, the Python controller is untouched, and every module it reuses
;; (S3 api, S4 read ladder, S5 write seam, S6 status) is read-only here.
;;
;; Fixture law (speed): each real git world is built ONCE per fixture kind
;; (`merge`, `prepare`, `impure`, `rejected`) through two bounded shell scripts,
;; then every case drives its own cheap `cp -a` copy of the subject clone. Cases
;; run as independent JOBS across a small lane pool (each job owns its own
;; subject copy, fake `gh` and log), and every assertion runs in the test thread.
;;
;; Evidence law: a fake `gh` executable is installed FIRST on PATH and logs every
;; invocation's ARGV VERBATIM (unit separator between arguments, record separator
;; after the last one), and `git` is a logging forwarder to the real binary. The
;; combined log therefore proves BOTH the exact argument vector of the merge PUT
;; and the ORDER of the reads around it. A route may carry a SECOND body served
;; from its second invocation onward, which is how one action sees a stage-1 read
;; and a post-mutation re-read of the SAME GitHub route.
;;
;; NON-IDEMPOTENT-PUT under test: a lost/ambiguous PUT response is a typed
;; refusal with EXACTLY ONE PUT in the log; the recovery is a LATER action that
;; re-reads the PR and returns `already-merged` with still exactly ONE PUT, and a
;; merge of an already merged PR spawns NO PUT at all.
;;
;; Proof-before-mutation under test: the strict trio gate, the operator
;; authorization, the non-author review, branch protection and every required
;; trusted check run BEFORE the PUT; every negative fixture (wrong head, wrong
;; merge sha, closed PR, fork identity, non-approved review, weakened protection,
;; pending/failed/missing/untrusted check, stale base, wrong branch,
;; unconfirmed/divergent merge response) ends with ZERO PUTs.
;;
;; prepare under test: the exact `pending-review` payload, the three staged
;; `.rktd` artifacts, the removed `request.json`, the idempotent
;; `already-staged` resume, and the refusal ladder — with NO GitHub mutation.

(require rackunit
         json
         openssl
         file/sha1
         racket/async-channel
         racket/file
         racket/hash
         racket/list
         racket/path
         racket/pretty
         racket/runtime-path
         racket/string
         "../sandbox/subprocess.rkt"
         "../extensions/gsd/plan-snapshot.rkt"
         "../scripts/gsd-delivery-git.rkt"
         "../scripts/gsd-delivery-api.rkt"
         "../scripts/gsd-delivery-pr.rkt"
         "../scripts/gsd-delivery-write.rkt"
         "../scripts/gsd-delivery-status.rkt"
         "../scripts/gsd-delivery-merge.rkt")

;; Resolved BEFORE any fixture PATH is installed, so the shims forward to the
;; real binaries.
(define racket-executable (or (find-executable-path "racket") "racket"))
(define racket-directory (path->string (path-only (path->complete-path racket-executable))))
(define real-git (path->string (or (find-executable-path "git") "/usr/bin/git")))
(define-runtime-path merge-module "../scripts/gsd-delivery-merge.rkt")

(define slug "coinerd/s7")
(define plan-id (make-string 64 #\a))
(define wave 1)
(define wave-branch "campaign/w1")
(define evidence-relative (format "docs/reports/gsd-wave-evidence/~a-w1.rktd" plan-id))
(define review-relative (format "docs/reports/gsd-wave-reviews/~a-w1.rktd" plan-id))
(define validation-relative (format "docs/reports/gsd-wave-validation/~a-w1.rktd" plan-id))
(define binding-relative evidence-relative)
;; The merge commit the fake GitHub reports for the `merge` world (the local git
;; world has no squash there: the PR is still open and based on fresh main).
(define api-merge-sha (make-string 40 #\e))
(define lanes 6)

;; ============================================================
;; Fixture helpers
;; ============================================================

(define (environment-set! env name value)
  (environment-variables-set! env (string->bytes/utf-8 name) (string->bytes/utf-8 value)))

(define (fixture-env root [path "/usr/bin:/bin"])
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

(define (put-datum path datum)
  (make-directory* (path-only path))
  (call-with-output-file path #:exists 'truncate (lambda (out) (pretty-write datum out))))

(define (json-text value)
  (jsexpr->string value))

;; ============================================================
;; Real schema-2 trio fixtures (the unchanged strict gate must accept them)
;; ============================================================

(define (trio-authorization head)
  (hasheq 'operator
          "release-operator"
          'wave
          "W1"
          'head
          head
          'action
          "merge"
          'source
          "recorded operator authorization for campaign W1"))

(define (trio-evidence impl digest)
  (hasheq 'schema-version
          2
          'milestone
          1
          'issue
          7
          'wave
          "W1"
          'plan-id
          plan-id
          'status
          "ready-for-merge"
          'implementation-sha
          impl
          'content-digest
          digest
          'required-checks
          '("ci")
          'wave-branch
          wave-branch
          'review-artifact
          review-relative
          'validation-artifact
          validation-relative
          'merge-authorization
          (trio-authorization impl)))

(define (trio-review impl digest)
  (hasheq
   'reviewer
   "independent-reviewer"
   'verdict
   "APPROVED"
   'timestamp
   "2026-01-01T00:00:00Z"
   'reviewed-sha
   impl
   'content-digest
   digest
   'scope
   "Independent review of the wave diff, the excluded-content digest and the strict gate"
   'report
   "The reviewer read the diff, recomputed the excluded digest and reran the strict trio gate."))

(define (trio-validation impl digest)
  (hasheq 'status
          "current"
          'milestone
          1
          'wave
          "W1"
          'issue
          7
          'branch
          wave-branch
          'implementation-sha
          impl
          'content-digest
          digest
          'red-first
          (hasheq 'command
                  "racket tests/test-fixture-gate.rkt"
                  'failure
                  "the fixture gate failed before the implementation landed")
          'focused-tests
          (hasheq 'result "passed" 'command "raco test tests/test-fixture-gate.rkt")
          'format-compile
          (hasheq 'result "passed" 'command "raco make")
          'lint
          (hasheq 'result "passed" 'command "raco make lint")
          'fast
          (hasheq 'result "passed" 'command "racket scripts/run-tests.rkt --suite fast")
          'review-artifact
          review-relative
          'remaining-items
          '()
          'planning-sync
          "current"))

;; ============================================================
;; Real git worlds (built ONCE per fixture kind)
;; ============================================================

(define script-preamble
  (list "set -e"
        "export GIT_AUTHOR_NAME=Test"
        "export GIT_AUTHOR_EMAIL=test@example.invalid"
        "export GIT_COMMITTER_NAME=Test"
        "export GIT_COMMITTER_EMAIL=test@example.invalid"
        "export GIT_CONFIG_NOSYSTEM=1"
        "export GIT_CONFIG_GLOBAL=/dev/null"
        "export GIT_ALLOW_PROTOCOL=file"
        "ROOT=$(pwd)"))

(define exclude-specs
  (list ":(exclude)docs/reports/gsd-wave-evidence/**"
        ":(exclude)docs/reports/gsd-wave-reviews/**"
        ":(exclude)docs/reports/gsd-wave-validation/**"))

(define (run-script! root name lines)
  (define path (build-path root name))
  (put path (string-append "#!/bin/sh\n" (string-join lines "\n") "\n"))
  (define result
    (run-subprocess "/bin/sh"
                    #:args (list (path->string path))
                    #:directory root
                    #:environment (current-environment-variables)
                    #:timeout 240
                    #:process-group? #t))
  (unless (and (zero? (subprocess-result-exit-code result))
               (not (subprocess-result-timed-out? result)))
    (error 'fixture-world "~a failed: ~a" name (subprocess-result-stderr result))))

;; Phase 1: the protected base, the implementation commit and the exact
;; excluded-path content digest of base..implementation.
(define (build-phase-1! root)
  (run-script!
   root
   "build-1.sh"
   (append
    script-preamble
    (list
     "git init -q --bare -b main origin.git"
     "git init -q -b main work"
     "cd work"
     "git remote add origin ../origin.git"
     "printf 'initial\\n' > tracked"
     "mkdir -p scripts"
     "printf '(\"ci\")\\n' > scripts/required-pr-checks.policy"
     "git add -A"
     "git commit -q -m initial"
     "BASE=$(git rev-parse HEAD)"
     "git checkout -q -b campaign/w1 \"$BASE\""
     "printf 'impl\\n' > feature"
     "git add feature"
     "git commit -q -m impl"
     "IMPL=$(git rev-parse HEAD)"
     "git push -q origin campaign/w1"
     (format
      "DIGEST=$(git diff --binary \"$BASE...$IMPL\" -- . '~a' '~a' '~a' | sha256sum | cut -d' ' -f1)"
      (list-ref exclude-specs 0)
      (list-ref exclude-specs 1)
      (list-ref exclude-specs 2))
     (string-append "printf 'BASE=%s\\nIMPL=%s\\nDIGEST=%s\\n' "
                    "\"$BASE\" \"$IMPL\" \"$DIGEST\" > \"$ROOT/facts-1.txt\"")
     "git checkout -q main"))))

(define (advance-main? variant)
  (memq variant '(prepare impure)))

;; Phase 2: the evidence-only trio commit, the protected squash identity, the
;; decoy PR refs and the subject clone with a GitHub-shaped configured origin.
(define (build-phase-2! root variant)
  (define copies
    (list (format "cp ../fixtures/evidence ~a" evidence-relative)
          (format "cp ../fixtures/review ~a" review-relative)
          (format "cp ../fixtures/validation ~a" validation-relative)))
  (define dirs
    (list "mkdir -p docs/reports/gsd-wave-evidence"
          "mkdir -p docs/reports/gsd-wave-reviews"
          "mkdir -p docs/reports/gsd-wave-validation"))
  (define impure
    (if (eq? variant 'impure)
        (list "printf 'drift\\n' > src-drift")
        '()))
  (define advance
    (if (advance-main? variant)
        (list "git reset -q --hard \"$MERGE\"" "git push -q origin main")
        (list "git push -q origin main")))
  (run-script!
   root
   "build-2.sh"
   (append script-preamble
           (list "cd work"
                 "BASE=$(sed -n 's/^BASE=//p' \"$ROOT/facts-1.txt\")"
                 "IMPL=$(sed -n 's/^IMPL=//p' \"$ROOT/facts-1.txt\")"
                 "git checkout -q campaign/w1")
           dirs
           copies
           impure
           (list "git add -A"
                 "git commit -q -m 'evidence trio'"
                 "HEAD_SHA=$(git rev-parse HEAD)"
                 "git push -q origin campaign/w1"
                 "git push -q origin \"$HEAD_SHA:refs/pull/1/head\""
                 "git push -q origin \"$IMPL:refs/pull/2/head\""
                 "git checkout -q main"
                 "TREE=$(git rev-parse \"$HEAD_SHA^{tree}\")"
                 "MERGE=$(git commit-tree \"$TREE\" -p \"$BASE\" -m 'squash merge')"
                 "SIDE=$(git commit-tree \"$TREE\" -p \"$IMPL\" -m 'side merge')")
           advance
           (list (string-append "printf 'HEAD=%s\\nMERGE=%s\\nSIDE=%s\\n' "
                                "\"$HEAD_SHA\" \"$MERGE\" \"$SIDE\" > \"$ROOT/facts-2.txt\"")
                 "cd \"$ROOT\""
                 "git clone -q origin.git subject"
                 "cd subject"
                 "git config user.name Test"
                 "git config user.email test@example.invalid"
                 (format "git config remote.origin.url 'https://github.com/~a'" slug)
                 (format "git config 'url.~a.insteadOf' 'https://github.com/~a'"
                         (path->string (build-path root "origin.git"))
                         slug)))))

(define (read-facts path)
  (for/fold ([acc (hash)])
            ([line (in-list (if (file-exists? path)
                                (file->lines path)
                                '()))])
    (define parts (string-split line "="))
    (hash-set acc (string->symbol (string-downcase (car parts))) (cadr parts))))

;; The committed trio varies per world so a refusal case never needs to rewrite
;; git history: 'rejected commits a non-APPROVED review verdict.
(define (world-trio variant impl digest)
  (define review
    (if (eq? variant 'rejected)
        (hash-set (trio-review impl digest) 'verdict "CHANGES_REQUESTED")
        (trio-review impl digest)))
  (values (trio-evidence impl digest) review (trio-validation impl digest)))

(define (build-world! root variant)
  (build-phase-1! root)
  (define facts-1 (read-facts (build-path root "facts-1.txt")))
  (define impl (hash-ref facts-1 'impl))
  (define digest (hash-ref facts-1 'digest))
  (define fixtures (build-path root "fixtures"))
  (define-values (evidence review validation) (world-trio variant impl digest))
  (put-datum (build-path fixtures "evidence") evidence)
  (put-datum (build-path fixtures "review") review)
  (put-datum (build-path fixtures "validation") validation)
  (build-phase-2! root variant)
  (hash-union (hash-union facts-1 (read-facts (build-path root "facts-2.txt")))
              (hasheq 'subject (build-path root "subject"))
              #:combine (lambda (a b) b)))

(define world-cache (make-hash))
(define world-lock (make-semaphore 1))

(define (shared-world variant)
  (call-with-semaphore world-lock
                       (lambda ()
                         (hash-ref! world-cache
                                    variant
                                    (lambda ()
                                      (define root
                                        (make-temporary-file "native-merge-world-~a" 'directory))
                                      (define env (fixture-env root "/usr/bin:/bin"))
                                      (parameterize ([current-environment-variables env])
                                        (check-not-false (find-executable-path "git")
                                                         "the fixture must resolve git through PATH")
                                        (build-world! root variant)))))))

;; ============================================================
;; Logging shims
;; ============================================================

(define record-separator (string #\u001E))
(define unit-separator (string #\u001F))

(define (argv-log-snippet marker)
  (format (string-append "{ printf '%s' ~a"
                         "; for a in \"$@\"; do printf '\\037%s' \"$a\"; done"
                         "; printf '\\036'; } >> \"$MERGE_SHIM_LOG\"")
          marker))

;; Each route may carry a SECOND body (`<index>.2.json`) served from the second
;; invocation onward: that is how one action sees a pre-mutation read and a
;; post-mutation re-read of the SAME GitHub route.
(define gh-shim-script
  (string-append
   "#!/bin/sh\n"
   (argv-log-snippet "gh")
   "\nroute=\n"
   "for arg in \"$@\"; do case \"$arg\" in repos/*) route=$arg ;; esac; done\n"
   "idx=$(awk -F'\t' -v r=\"$route\" '$2==r {print $1; exit}' \"$MERGE_SHIM_ROUTES\")\n"
   "if [ -z \"$idx\" ]; then printf 'no fixture for route %s\\n' \"$route\" >&2; exit 66; fi\n"
   "hits=\"$MERGE_SHIM_HITS/$idx\"\n"
   "sel=\"$idx.json\"\n"
   "if [ -f \"$MERGE_SHIM_FIXTURES/$idx.2.json\" ] && [ -f \"$hits\" ]; then sel=\"$idx.2.json\"; fi\n"
   "touch \"$hits\"\n"
   "cat \"$MERGE_SHIM_FIXTURES/$sel\"\n"))

(define (read-records log)
  (if (file-exists? log)
      (for/list ([record (in-list (string-split (file->string log) record-separator))]
                 #:unless (string=? record ""))
        (string-split record unit-separator))
      '()))

(define (invocations records program)
  (for/list ([record (in-list records)]
             #:when (equal? (car record) program))
    (cdr record)))

(define (argv-method argv)
  (define at (index-of argv "-X"))
  (and at (< (add1 at) (length argv)) (list-ref argv (add1 at))))

;; Every gh MUTATION (any invocation carrying an explicit method).
(define (mutations records)
  (for/list ([argv (in-list (invocations records "gh"))]
             #:when (argv-method argv))
    argv))

(define (route slug* path)
  (format "repos/~a/~a" slug* path))

;; Rewrite the fixture a route serves, IN PLACE, so a case can model the service
;; state changing (or probe several payloads) without reinstalling the shim.
(define (set-fixture! root route* body)
  (define line
    (for/first ([entry (in-list (file->lines (build-path root "routes.tsv")))]
                #:when (equal? (cadr (string-split entry "\t")) route*))
      entry))
  (unless line
    (error 'set-fixture! "no installed fixture for route ~a" route*))
  (define index (car (string-split line "\t")))
  (put (build-path root "fixtures" (format "~a.json" index)) body))

(define (install-shims! root fixtures)
  (define bin (build-path root "bin"))
  (define fixtures-dir (build-path root "fixtures"))
  (define hits-dir (build-path root "hits"))
  (make-directory* bin)
  (make-directory* fixtures-dir)
  (make-directory* hits-dir)
  (define routes
    (for/list ([entry (in-list fixtures)]
               [index (in-naturals)])
      (define body (cdr entry))
      (define first
        (if (pair? body)
            (car body)
            body))
      (put (build-path fixtures-dir (format "~a.json" index)) first)
      (when (pair? body)
        (put (build-path fixtures-dir (format "~a.2.json" index)) (cadr body)))
      (cons (car entry) index)))
  (put (build-path root "routes.tsv")
       (string-join (for/list ([entry (in-list routes)])
                      (format "~a\t~a" (cdr entry) (car entry)))
                    "\n"))
  (define gh (build-path bin "gh"))
  (put gh gh-shim-script)
  (file-or-directory-permissions gh #o700)
  (define env (fixture-env root (format "~a:~a:/usr/bin:/bin" bin racket-directory)))
  (environment-set! env "MERGE_SHIM_FIXTURES" (path->string fixtures-dir))
  (environment-set! env "MERGE_SHIM_HITS" (path->string hits-dir))
  (environment-set! env "MERGE_SHIM_ROUTES" (path->string (build-path root "routes.tsv")))
  (environment-set! env "MERGE_SHIM_LOG" (path->string (build-path root "shim.log")))
  env)

;; ============================================================
;; GitHub fixture constructors
;; ============================================================

(define (pr-json #:number [number 1]
                 #:state [state "open"]
                 #:merged [merged #f]
                 #:merge-sha [merge-sha #f]
                 #:head-sha [head-sha (make-string 40 #\d)]
                 #:head-ref [head-ref wave-branch]
                 #:base-sha [base-sha (make-string 40 #\c)]
                 #:base-ref [base-ref "main"]
                 #:base-repo [base-repo slug]
                 #:head-repo [head-repo slug]
                 #:author [author "pr-author"])
  (hasheq 'number
          number
          'state
          state
          'merged
          merged
          'merged_at
          (if merged "2026-01-01T00:00:00Z" 'null)
          'merge_commit_sha
          (if merge-sha merge-sha 'null)
          'user
          (hasheq 'login author)
          'head
          (hasheq 'ref head-ref 'sha head-sha 'repo (hasheq 'full_name head-repo))
          'base
          (hasheq 'ref base-ref 'sha base-sha 'repo (hasheq 'full_name base-repo))))

(define (commit-json sha parents)
  (hasheq 'sha
          sha
          'parents
          (for/list ([parent (in-list parents)])
            (hasheq 'sha parent))))

(define (protection-json #:contexts [contexts '("ci")]
                         #:enforce-admins [enforce-admins #t]
                         #:strict [strict #t])
  (hasheq 'enforce_admins
          (hasheq 'enabled enforce-admins)
          'required_status_checks
          (hasheq 'strict strict 'contexts contexts)
          'allow_force_pushes
          (hasheq 'enabled #f)
          'allow_deletions
          (hasheq 'enabled #f)))

(define (check-run-json #:sha sha
                        #:run-id [run-id 1001]
                        #:app-id [app-id actions-app-id]
                        #:status [status "completed"]
                        #:conclusion [conclusion "success"])
  (hasheq 'id
          11
          'name
          "ci"
          'status
          status
          'conclusion
          conclusion
          'head_sha
          sha
          'app
          (hasheq 'id app-id)
          'details_url
          (format "https://github.com/~a/actions/runs/~a/job/9001" slug run-id)))

(define (workflow-run-json #:sha sha
                           #:run-id [run-id 1001]
                           #:event [event "pull_request"]
                           #:branch [branch wave-branch]
                           #:status [status "completed"]
                           #:conclusion [conclusion "success"])
  (hasheq 'id
          run-id
          'status
          status
          'conclusion
          conclusion
          'head_sha
          sha
          'head_branch
          branch
          'event
          event
          'path
          ".github/workflows/ci.yml"
          'repository
          (hasheq 'full_name slug)))

(define (check-runs-json runs)
  (hasheq 'total_count (length runs) 'check_runs runs))

(define (check-runs-route sha)
  (format "commits/~a/check-runs?per_page=100&page=1" sha))

;; The happy-path fixture set for `merge`: stage-1 OPEN read, post-PUT MERGED
;; re-read of the same route, the PUT response itself, the reported merge commit,
;; branch protection and one trusted check (plus the workflow run it names).
(define (merge-fixtures world)
  (define head (hash-ref world 'head))
  (define base (hash-ref world 'base))
  (list (cons (route slug "pulls/1")
              (list (json-text (pr-json #:number 1 #:head-sha head #:base-sha base))
                    (json-text (pr-json #:number 1
                                        #:state "closed"
                                        #:merged #t
                                        #:merge-sha api-merge-sha
                                        #:head-sha head
                                        #:base-sha base))))
        (cons (route slug "pulls/1/merge") (format "{\"merged\":true,\"sha\":\"~a\"}" api-merge-sha))
        (cons (route slug (format "commits/~a" api-merge-sha))
              (json-text (commit-json api-merge-sha (list base))))
        (cons (route slug "branches/main/protection") (json-text (protection-json)))
        (cons (route slug (check-runs-route head))
              (json-text (check-runs-json (list (check-run-json #:sha head)))))
        (cons (route slug "actions/runs/1001") (json-text (workflow-run-json #:sha head)))))

;; The already-merged fixture set: one MERGED read, its merge commit, protection
;; and one trusted check.
(define (merge-fixtures-merged head base)
  (list (cons (route slug "pulls/1")
              (json-text (pr-json #:number 1
                                  #:state "closed"
                                  #:merged #t
                                  #:merge-sha api-merge-sha
                                  #:head-sha head
                                  #:base-sha base)))
        (cons (route slug (format "commits/~a" api-merge-sha))
              (json-text (commit-json api-merge-sha (list base))))
        (cons (route slug "branches/main/protection") (json-text (protection-json)))
        (cons (route slug (check-runs-route head))
              (json-text (check-runs-json (list (check-run-json #:sha head)))))
        (cons (route slug "actions/runs/1001") (json-text (workflow-run-json #:sha head)))))

;; The prepare fixture set: a merged same-repo main-base PR whose merge commit IS
;; the local squash, plus protection requiring the policy snapshot. `prepare`
;; reads no check runs at all, so no check fixtures are installed.
(define (prepare-fixtures world)
  (define head (hash-ref world 'head))
  (define base (hash-ref world 'base))
  (define merge (hash-ref world 'merge))
  (list (cons (route slug "pulls/1")
              (json-text (pr-json #:number 1
                                  #:state "closed"
                                  #:merged #t
                                  #:merge-sha merge
                                  #:head-sha head
                                  #:base-sha base)))
        (cons (route slug (format "commits/~a" merge)) (json-text (commit-json merge (list base))))
        (cons (route slug "branches/main/protection") (json-text (protection-json)))))

;; Replace (or append) one route's body in a fixture list.
(define (fixtures-with fixtures route* body)
  (define existing
    (for/first ([entry (in-list fixtures)]
                #:when (equal? (car entry) route*))
      entry))
  (cond
    [(not existing) (append fixtures (list (cons route* body)))]
    [else
     (for/list ([entry (in-list fixtures)])
       (if (equal? (car entry) route*)
           (cons route* body)
           entry))]))

;; ============================================================
;; Campaign fixtures (frozen snapshot + coordinator journal)
;; ============================================================

;; A REAL frozen campaign snapshot: PLAN.md + the referenced wave doc go through
;; the trusted plan-snapshot module, so the snapshot-facts subprocess verifies a
;; genuine manifest (never a hand-written one).
(define (write-campaign! root)
  (define plan-text
    (string-append "# Plan: Delivery Merge Test\n\n## Waves\n\n"
                   "- [Inbox] W1: Fixture → waves/W1-fixture.md\n"))
  (put (build-path root ".planning" "PLAN.md") plan-text)
  (put (build-path root ".planning" "waves" "W1-fixture.md")
       "# Wave 1\nStatus: Inbox\n\n## Files\n- File: q/feature.rkt\n\n## Verify\nraco make\n")
  (make-plan-snapshot! root plan-id plan-text))

(define (write-journal! root #:head head #:branch branch)
  (define journal
    (hasheq 'schema-version
            1
            'plan-id
            plan-id
            'wave
            wave
            'stage
            "governance"
            'receipt
            (hasheq 'repo
                    slug
                    'branch
                    branch
                    'origin
                    (format "https://github.com/~a" slug)
                    'evidence
                    evidence-relative
                    'head
                    head
                    'tree
                    (make-string 40 #\a)
                    'verified-at
                    1
                    'attempt-id
                    "attempt-7"
                    'attempt-fence
                    3)))
  (put (build-path root ".planning" "campaigns" plan-id (format "coordinator-w~a.json" wave))
       (string-append (json-text journal) "\n")))

;; ============================================================
;; Proof capture and the per-case runner
;; ============================================================

(define (delivery-refusal thunk)
  (with-handlers ([exn:fail:delivery-action? exn-message])
    (thunk)
    (fail "expected a typed exn:fail:delivery-action refusal, but the call returned")))

;; ('value v) or ('reason message) — the outcome of one action inside a job.
(define (probe thunk)
  (with-handlers ([exn:fail:delivery-action? (lambda (error) (list 'reason (exn-message error)))])
    (list 'value (thunk))))

;; Every job returns (hasheq 'result <probe> 'records <shim log>), so the test
;; thread can assert both the outcome and the mutation history. A thunk may take
;; (world root log) or just (world).
(define (apply-thunk thunk world root log)
  (if (procedure-arity-includes? thunk 3)
      (thunk world root log)
      (thunk world)))

(define (case-thunk thunk)
  (lambda (world root log)
    (hash-set (hasheq 'result (probe (lambda () (apply-thunk thunk world root log))))
              'records
              (read-records log))))

;; fixtures: list of (route . body) / (route first second) / a procedure of world.
(define (with-world variant proc #:fixtures [fixtures '()])
  (define cached (shared-world variant))
  (define root (make-temporary-file "native-merge-case-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (define repo (copy-subject! cached root))
                  (define world (hash-set cached 'subject repo))
                  (define env
                    (install-shims! root
                                    (if (procedure? fixtures)
                                        (fixtures world)
                                        fixtures)))
                  (parameterize ([current-environment-variables env])
                    (check-not-false (find-executable-path "gh")
                                     "the fixture must resolve gh through PATH")
                    (clear-api-cache!)
                    (proc world root (build-path root "shim.log"))))
                (lambda () (delete-directory/files root #:must-exist? #f))))

(define (copy-subject! cached root)
  (define dst (build-path root "repo"))
  (define result
    (run-subprocess "/bin/cp"
                    #:args (list "-a" (path->string (hash-ref cached 'subject)) (path->string dst))
                    #:environment (current-environment-variables)
                    #:timeout 120
                    #:process-group? #t))
  (unless (and (zero? (subprocess-result-exit-code result))
               (not (subprocess-result-timed-out? result)))
    (error 'fixture-copy "subject copy failed: ~a" (subprocess-result-stderr result)))
  dst)

;; ============================================================
;; Jobs (one whole action each, driven across a small lane pool)
;; ============================================================

(struct job (label variant fixtures thunk expected assertions) #:transparent)

(define (run-job a-job)
  (with-world (job-variant a-job)
              (lambda (world root log) ((job-thunk a-job) world root log))
              #:fixtures (job-fixtures a-job)))

(define (run-jobs jobs [lanes* lanes])
  (define channel (make-async-channel))
  (define buckets (make-vector lanes* '()))
  (for ([a-job (in-list jobs)]
        [index (in-naturals)])
    (define lane (modulo index lanes*))
    (vector-set! buckets lane (cons a-job (vector-ref buckets lane))))
  (define active 0)
  (for ([bucket (in-vector buckets)])
    (when (pair? bucket)
      (set! active (add1 active))
      (thread (lambda ()
                (for ([a-job (in-list (reverse bucket))])
                  (define outcome
                    (with-handlers
                        ([exn:fail?
                          (lambda (error)
                            (list 'error (format "~a: ~a" (object-name error) (exn-message error))))])
                      (list 'ran (run-job a-job))))
                  (async-channel-put channel (cons (job-label a-job) outcome)))
                (async-channel-put channel 'lane-done)))))
  (let loop ([remaining active]
             [results '()])
    (cond
      [(zero? remaining) (reverse results)]
      [else
       (define item (async-channel-get channel))
       (if (eq? item 'lane-done)
           (loop (sub1 remaining) results)
           (loop remaining (cons item results)))])))

;; Declared outcome: 'value (success), an exact reason string, or
;; (list 'prefix <prefix>) for the typed helper verdicts, or
;; (list 'mutating <reason>) for a refusal that legitimately issued exactly ONE
;; merge PUT before failing (a non-idempotent write whose response was bad).
(define (check-outcome! label result declared records)
  (cond
    [(eq? declared 'value)
     (check-equal? (car result) 'value (format "~a must succeed (got ~s)" label result))]
    [(and (pair? declared) (eq? (car declared) 'mutating))
     (check-equal? (car result)
                   'reason
                   (format "~a must refuse with a typed reason (got ~s)" label result))
     (check-equal? (cadr result) (cadr declared) label)
     (check-equal? (length (mutations records))
                   1
                   (format "~a must attempt the merge EXACTLY once" label))]
    [else
     (check-equal? (car result)
                   'reason
                   (format "~a must refuse with a typed reason (got ~s)" label result))
     (define reason (cadr result))
     (if (pair? declared)
         (check-true (string-prefix? reason (cadr declared))
                     (format "~a reason prefix (got ~s)" label reason))
         (check-equal? reason declared label))
     ;; No refusal may reach a mutation.
     (check-equal? (mutations records) '() (format "~a must not mutate" label))]))

(define (check-jobs! jobs)
  (define results (run-jobs jobs))
  (check-equal? (length results) (length jobs) "every job must report")
  (for ([item (in-list results)])
    (define label (car item))
    (define a-job
      (for/first ([candidate (in-list jobs)]
                  #:when (equal? (job-label candidate) label))
        candidate))
    (define outcome (cdr item))
    (cond
      [(not (eq? (car outcome) 'ran))
       (fail (format "~a raised unexpectedly: ~s" label (cadr outcome)))]
      [else
       (define facts (cadr outcome))
       (define result (hash-ref facts 'result))
       (check-outcome! label result (job-expected a-job) (hash-ref facts 'records))
       ((job-assertions a-job) facts label)])))

;; ============================================================
;; Shared assertion helpers
;; ============================================================

(define (check-hashes-equal? actual expected label)
  (check-equal? (sort (hash-keys actual) symbol<?) (sort (hash-keys expected) symbol<?) label)
  (for ([key (in-list (hash-keys expected))])
    (check-equal? (hash-ref actual key #f) (hash-ref expected key) (format "~a ~a" label key))))

(define no-assertions (lambda (_facts _label) (void)))

(define (ok-value facts)
  (define result (hash-ref facts 'result))
  (unless (eq? (car result) 'value)
    (fail (format "expected success, got ~s" result)))
  (cadr result))

(define (value-of facts label payload-assertions)
  (define payload (ok-value facts))
  (payload-assertions payload label)
  payload)

;; The typed, credential-free summary the shared seam produces for any failing
;; helper subprocess (the wave gate, a git ancestry probe, the snapshot reader).
(define runner-failed
  "delivery command failed (racket, exit 1); inspect authentication, repository refs and required artifacts locally")

(define (position-of records predicate what)
  (or (for/first ([record (in-list records)]
                  [index (in-naturals)]
                  #:when (predicate record))
        index)
      (fail (format "the shim log carries no ~a invocation" what))))

(define (gh-read? route* record)
  (and (equal? (car record) "gh") (member route* (cdr record)) #t))

;; ============================================================
;; Job table
;; ============================================================

(define (merge-run world number head branch)
  (delivery-merge! (hash-ref world 'subject) plan-id wave number head branch evidence-relative))

(define (default-merge-run world)
  (merge-run world 1 (hash-ref world 'head) wave-branch))

;; Every job thunk takes (world root log): this adapter keeps the common
;; "run the default merge" thunk in that shape.
(define (run-default-merge world root log)
  (default-merge-run world))

(define (prepare-run world campaign root #:output [output #f] #:number [number 1])
  (delivery-prepare! (hash-ref world 'subject)
                     plan-id
                     wave
                     number
                     evidence-relative
                     (path->string campaign)
                     (path->string (or output (build-path root "staging")))))

(define (prepare-campaign thunk)
  (lambda (world root log)
    (define campaign (build-path root "campaign"))
    (write-campaign! campaign)
    (thunk world campaign root log)))

(define (merge-job label variant fixtures expected [assertions no-assertions])
  (job label variant fixtures (case-thunk default-merge-run) expected assertions))

(define (prepare-job label variant fixtures thunk expected [assertions no-assertions])
  (job label variant fixtures (case-thunk thunk) expected assertions))

(define jobs
  (list
   ;; ----------------------------------------------------------
   ;; merge: the happy path and the exact mutation contract
   ;; ----------------------------------------------------------
   (merge-job
    "merge happy path"
    'merge
    merge-fixtures
    'value
    (lambda (facts label)
      (define world (shared-world 'merge))
      (define head (hash-ref world 'head))
      (define records (hash-ref facts 'records))
      (value-of facts
                label
                (lambda (payload label)
                  (check-equal? payload
                                (hasheq 'status
                                        "merged"
                                        'merge-sha
                                        api-merge-sha
                                        'plan-id
                                        plan-id
                                        'wave
                                        wave
                                        'head
                                        head
                                        'pr
                                        1)
                                label)))
      ;; EXACTLY one mutation, with Python's exact argv.
      (check-equal? (mutations records)
                    (list (list "api"
                                "--hostname"
                                "github.com"
                                "-X"
                                "PUT"
                                (route slug "pulls/1/merge")
                                "-f"
                                "merge_method=squash"))
                    "the merge PUT is Python's exact argv and is never repeated")
      ;; Proofs before the PUT; the fresh re-read after it.
      (define put-at
        (position-of records
                     (lambda (record)
                       (and (equal? (car record) "gh") (equal? (argv-method (cdr record)) "PUT")))
                     "PUT"))
      (define pulls-at
        (for/list ([record (in-list records)]
                   [index (in-naturals)]
                   #:when (gh-read? (route slug "pulls/1") record))
          index))
      (check-equal? (length pulls-at) 2 "one stage-1 read plus one fresh proof read")
      (check-true (< (first pulls-at) put-at) "the stage-1 read precedes the PUT")
      (check-true (< put-at (second pulls-at)) "the post-PUT proof read follows the PUT")
      (for ([route* (in-list (list (route slug (check-runs-route head))
                                   (route slug "actions/runs/1001")
                                   (route slug "branches/main/protection")))])
        (check-true (< (position-of records (lambda (record) (gh-read? route* record)) route*) put-at)
                    (format "~a must precede the PUT" route*)))))
   (merge-job "merge already merged"
              'merge
              (lambda (world) (merge-fixtures-merged (hash-ref world 'head) (hash-ref world 'base)))
              'value
              (lambda (facts label)
                (define world (shared-world 'merge))
                (value-of facts
                          label
                          (lambda (payload label)
                            (check-equal? payload
                                          (hasheq 'status
                                                  "already-merged"
                                                  'merge-sha
                                                  api-merge-sha
                                                  'plan-id
                                                  plan-id
                                                  'wave
                                                  wave
                                                  'head
                                                  (hash-ref world 'head)
                                                  'pr
                                                  1)
                                          label)))
                (check-equal? (mutations (hash-ref facts 'records))
                              '()
                              "an already merged PR is never merged twice")))
   (job
    "merge lost response then fresh re-read"
    'merge
    (lambda (world)
      (define head (hash-ref world 'head))
      (define base (hash-ref world 'base))
      (fixtures-with (fixtures-with (merge-fixtures world) (route slug "pulls/1/merge") "not-json")
                     (route slug "pulls/1")
                     (list (json-text (pr-json #:number 1 #:head-sha head #:base-sha base))
                           (json-text (pr-json #:number 1
                                               #:state "closed"
                                               #:merged #t
                                               #:merge-sha api-merge-sha
                                               #:head-sha head
                                               #:base-sha base)))))
    (case-thunk (lambda (world root log)
                  (define head (hash-ref world 'head))
                  ;; Attempt 1: the service merged, but the response is unusable.
                  (define first
                    (probe (lambda ()
                             (delivery-merge! (hash-ref world 'subject)
                                              plan-id
                                              wave
                                              1
                                              head
                                              wave-branch
                                              evidence-relative))))
                  ;; Attempt 2 (a fresh action/session) proves the merge from a
                  ;; fresh read and does not mutate again.
                  (define second
                    (delivery-merge! (hash-ref world 'subject)
                                     plan-id
                                     wave
                                     1
                                     head
                                     wave-branch
                                     evidence-relative))
                  (hasheq 'first first 'second second)))
    'value
    (lambda (facts label)
      (define payload (ok-value facts))
      (check-equal? (car (hash-ref payload 'first)) 'reason "the unusable PUT response refuses")
      (check-equal? (cadr (hash-ref payload 'first))
                    "malformed GitHub mutation response"
                    "the lost response surfaces the seam's typed reason")
      (check-equal? (hash-ref (hash-ref payload 'second) 'status) "already-merged")
      (check-equal? (length (mutations (hash-ref facts 'records)))
                    1
                    "a lost response is recovered by a re-read, never by a retry")))
   (merge-job "merge response without merged=true"
              'merge
              (lambda (world)
                (fixtures-with (merge-fixtures world)
                               (route slug "pulls/1/merge")
                               "{\"merged\":true,\"sha\":\"deadbeef\"}"))
              (list 'mutating "GitHub did not confirm the protected squash merge"))
   (merge-job
    "merge sha mismatch after refetch"
    'merge
    (lambda (world)
      (define head (hash-ref world 'head))
      (define base (hash-ref world 'base))
      (fixtures-with (fixtures-with (merge-fixtures world)
                                    (route slug (format "commits/~a" (hash-ref world 'impl)))
                                    (json-text (commit-json (hash-ref world 'impl) (list base))))
                     (route slug "pulls/1")
                     (list (json-text (pr-json #:number 1 #:head-sha head #:base-sha base))
                           (json-text (pr-json #:number 1
                                               #:state "closed"
                                               #:merged #t
                                               #:merge-sha (hash-ref world 'impl)
                                               #:head-sha head
                                               #:base-sha base)))))
    (list 'mutating "implementation merge SHA mismatch"))
   ;; ----------------------------------------------------------
   ;; merge: pre-mutation identity refusals
   ;; ----------------------------------------------------------
   (job "merge wrong expected head"
        'merge
        merge-fixtures
        (case-thunk (lambda (world root log) (merge-run world 1 (make-string 40 #\b) wave-branch)))
        "implementation PR head does not match the verified expected head"
        no-assertions)
   (job "merge wrong merge sha"
        'merge
        (lambda (world)
          (define base (hash-ref world 'base))
          (fixtures-with (fixtures-with (merge-fixtures-merged (hash-ref world 'head) base)
                                        (route slug "commits/deadbeef")
                                        (json-text (commit-json "deadbeef" (list base))))
                         (route slug "pulls/1")
                         (json-text (pr-json #:number 1
                                             #:state "closed"
                                             #:merged #t
                                             #:merge-sha "deadbeef"
                                             #:head-sha (hash-ref world 'head)))))
        (case-thunk default-merge-run)
        "implementation merge SHA mismatch"
        no-assertions)
   (job "merge closed and unmerged"
        'merge
        (lambda (world)
          (fixtures-with
           (merge-fixtures world)
           (route slug "pulls/1")
           (json-text (pr-json #:number 1 #:state "closed" #:head-sha (hash-ref world 'head)))))
        (case-thunk default-merge-run)
        "implementation PR is neither open nor already merged"
        no-assertions)
   (job "merge fork identity"
        'merge
        (lambda (world)
          (fixtures-with
           (merge-fixtures world)
           (route slug "pulls/1")
           (json-text (pr-json #:number 1 #:head-sha (hash-ref world 'head) #:head-repo "fork/q"))))
        (case-thunk default-merge-run)
        "implementation PR must be a same-repository main-base pull request"
        no-assertions)
   (job "merge fetched ref mismatch"
        'merge
        (lambda (world)
          (fixtures-with (merge-fixtures world)
                         (route slug "pulls/2")
                         (json-text (pr-json #:number 2
                                             #:head-sha (hash-ref world 'head)
                                             #:base-sha (hash-ref world 'base)))))
        (case-thunk (lambda (world root log) (merge-run world 2 (hash-ref world 'head) wave-branch)))
        "fetched implementation head does not match expected head"
        no-assertions)
   ;; ----------------------------------------------------------
   ;; merge: the approval contract
   ;; ----------------------------------------------------------
   ;; The reviewer-is-the-author clause and the verdict/head-binding clauses of
   ;; merge_authorization / reviewed_head_approval are asserted directly in the
   ;; unit tests below; here only the STRICT GATE's own refusal is driven through
   ;; the merge proof, because the gate accepts no unapproved artifact at all.
   (merge-job "merge review not approved at the exact head" 'rejected merge-fixtures runner-failed)
   ;; ----------------------------------------------------------
   ;; merge: protection and required checks
   ;; ----------------------------------------------------------
   (merge-job "merge weakened protection"
              'merge
              (lambda (world)
                (fixtures-with (merge-fixtures world)
                               (route slug "branches/main/protection")
                               (json-text (protection-json #:enforce-admins #f))))
              "default branch protection is absent or weakened")
   ;; The remaining protection/check branches are asserted DIRECTLY against the
   ;; shared S3 seams the merge proof calls (protection / trusted-check), each
   ;; through its own delivery session: driving every one of them through a full
   ;; merge proof would cost minutes on a slow host (the same documented split as
   ;; the S6 status suite).
   (job "merge proof seams refuse bad protection and checks"
        'merge
        merge-fixtures
        (case-thunk
         (lambda (world root log)
           (define head (hash-ref world 'head))
           (define (fresh-session)
             (delivery-pr-context-session (make-delivery-pr-context (hash-ref world 'subject))))
           (define (protection-probe payload)
             (set-fixture! root (route slug "branches/main/protection") payload)
             (probe (lambda () ((delivery-api-session-protection (fresh-session)) slug '("ci")))))
           (define (check-probe payload #:branch [branch wave-branch])
             (set-fixture! root (route slug (check-runs-route head)) payload)
             (probe (lambda ()
                      ((delivery-api-session-trusted-check (fresh-session)) slug
                                                                            head
                                                                            "ci"
                                                                            "pull_request"
                                                                            #:branch branch))))
           (hasheq
            'protection-empty
            (protection-probe (json-text (protection-json #:contexts '())))
            'protection-missing
            (protection-probe (json-text (protection-json #:contexts '("other"))))
            'missing
            (check-probe (json-text (check-runs-json '())))
            'pending
            (check-probe (json-text (check-runs-json (list (check-run-json #:sha head
                                                                           #:status "in_progress"
                                                                           #:conclusion 'null)))))
            'failed
            (check-probe (json-text (check-runs-json (list (check-run-json #:sha head
                                                                           #:conclusion "failure")))))
            'untrusted
            (check-probe (json-text (check-runs-json (list (check-run-json #:sha head #:app-id 42)))))
            'wrong-branch
            (check-probe (json-text (check-runs-json (list (check-run-json #:sha head))))
                         #:branch "campaign/other"))))
        'value
        (lambda (facts label)
          (define outcomes (ok-value facts))
          (define expected
            (hasheq 'protection-empty
                    "default branch protection requires configured native checks"
                    'protection-missing
                    "branch protection does not require expected checks: ci"
                    'missing
                    "missing check: ci"
                    'pending
                    "pending, failed, or untrusted check: ci"
                    'failed
                    "pending, failed, or untrusted check: ci"
                    'untrusted
                    "pending, failed, or untrusted check: ci"
                    'wrong-branch
                    "check workflow/ref/event mismatch: ci"))
          (for ([key (in-list (hash-keys expected))])
            (check-equal? (hash-ref outcomes key)
                          (list 'reason (hash-ref expected key))
                          (format "~a ~a" label key))
            (check-equal? (mutations (hash-ref facts 'records))
                          '()
                          (format "~a must not mutate" label)))))
   ;; ----------------------------------------------------------
   ;; prepare
   ;; ----------------------------------------------------------
   (job "prepare happy path"
        'prepare
        prepare-fixtures
        (case-thunk (prepare-campaign
                     (lambda (world campaign root log)
                       (define output (build-path root "staging"))
                       (hasheq 'payload
                               (prepare-run world campaign root)
                               'staged
                               (for/list ([relative (in-list (list evidence-relative
                                                                   review-relative
                                                                   validation-relative))])
                                 (file-exists? (build-path output relative)))
                               'request
                               (file-exists? (build-path output "request.json"))
                               'output
                               (path->string output)))))
        'value
        (lambda (facts label)
          (define bundle (ok-value facts))
          (check-hashes-equal? (hash-ref bundle 'payload)
                               (hasheq 'status
                                       "pending-review"
                                       'output
                                       (hash-ref bundle 'output)
                                       'binding
                                       binding-relative
                                       'source
                                       evidence-relative
                                       'next
                                       (prepare-next-message))
                               label)
          (check-equal? (hash-ref bundle 'staged) '(#t #t #t) "all three .rktd artifacts are staged")
          (check-false (hash-ref bundle 'request) "the transient request payload is removed")
          (check-equal? (mutations (hash-ref facts 'records)) '() "prepare never mutates GitHub")))
   (job "prepare staged resume"
        'prepare
        prepare-fixtures
        (case-thunk
         (prepare-campaign
          (lambda (world campaign root log)
            (define output (build-path root "staging"))
            (define (run!)
              (prepare-run world campaign root))
            (define first (run!))
            (define resumed (run!))
            (hasheq 'first first 'resumed resumed 'world world 'output (path->string output)))))
        'value
        (lambda (facts label)
          (define bundle (ok-value facts))
          (define world (hash-ref bundle 'world))
          (check-equal? (hash-ref (hash-ref bundle 'first) 'status) "pending-review")
          (check-hashes-equal? (hash-ref bundle 'resumed)
                               (hasheq 'status
                                       "already-staged"
                                       'output
                                       (hash-ref bundle 'output)
                                       'binding
                                       binding-relative
                                       'branch
                                       (binding-branch plan-id wave)
                                       'merge-sha
                                       (hash-ref world 'merge)
                                       'delivery-head-sha
                                       (hash-ref world 'head)
                                       'implementation-sha
                                       (hash-ref world 'merge))
                               "idempotent staging resume")))
   (prepare-job "prepare without pr and branch"
                'prepare
                prepare-fixtures
                (prepare-campaign (lambda (world campaign root log)
                                    (delivery-prepare! (hash-ref world 'subject)
                                                       plan-id
                                                       wave
                                                       #f
                                                       evidence-relative
                                                       (path->string campaign)
                                                       (path->string (build-path root "staging")))))
                "prepare --pr requires --expected-branch for durable PR self-resolution")
   (prepare-job "prepare malformed PR"
                'prepare
                (lambda (world) (fixtures-with (prepare-fixtures world) (route slug "pulls/1") "[]"))
                (prepare-campaign (lambda (world campaign root log)
                                    (prepare-run world campaign root)))
                "malformed PR response")
   (prepare-job "prepare unmerged PR"
                'prepare
                (lambda (world)
                  (fixtures-with (prepare-fixtures world)
                                 (route slug "pulls/1")
                                 (json-text (pr-json #:number 1
                                                     #:state "open"
                                                     #:head-sha (hash-ref world 'head)
                                                     #:base-sha (hash-ref world 'base)))))
                (prepare-campaign (lambda (world campaign root log)
                                    (prepare-run world campaign root)))
                "implementation PR must be independently reviewed and merged first")
   (prepare-job "prepare fork identity"
                'prepare
                (lambda (world)
                  (fixtures-with (prepare-fixtures world)
                                 (route slug "pulls/1")
                                 (json-text (pr-json #:number 1
                                                     #:state "closed"
                                                     #:merged #t
                                                     #:merge-sha (hash-ref world 'merge)
                                                     #:head-sha (hash-ref world 'head)
                                                     #:head-repo "fork/q"))))
                (prepare-campaign (lambda (world campaign root log)
                                    (prepare-run world campaign root)))
                "implementation PR must be a same-repository main-base pull request")
   (prepare-job "prepare fetched head mismatch"
                'prepare
                (lambda (world)
                  (fixtures-with (prepare-fixtures world)
                                 (route slug "pulls/2")
                                 (json-text (pr-json #:number 2
                                                     #:state "closed"
                                                     #:merged #t
                                                     #:merge-sha (hash-ref world 'merge)
                                                     #:head-sha (hash-ref world 'head)
                                                     #:base-sha (hash-ref world 'base)))))
                (prepare-campaign (lambda (world campaign root log)
                                    (prepare-run world campaign root #:number 2)))
                "fetched implementation head does not match PR head SHA")
   (prepare-job "prepare merge tree mismatch"
                'prepare
                (lambda (world)
                  (define base (hash-ref world 'base))
                  (define impl (hash-ref world 'impl))
                  (fixtures-with (fixtures-with (prepare-fixtures world)
                                                (route slug (format "commits/~a" impl))
                                                (json-text (commit-json impl (list base))))
                                 (route slug "pulls/1")
                                 (json-text (pr-json #:number 1
                                                     #:state "closed"
                                                     #:merged #t
                                                     #:merge-sha impl
                                                     #:head-sha (hash-ref world 'head)
                                                     #:base-sha base))))
                (prepare-campaign (lambda (world campaign root log)
                                    (prepare-run world campaign root)))
                "implementation merge tree differs from fetched PR head tree")
   (prepare-job "prepare weakened protection"
                'prepare
                (lambda (world)
                  (fixtures-with (prepare-fixtures world)
                                 (route slug "branches/main/protection")
                                 (json-text (protection-json #:strict #f))))
                (prepare-campaign (lambda (world campaign root log)
                                    (prepare-run world campaign root)))
                "default branch protection is absent or weakened")
   ;; The attempt-bound receipt rule is asserted directly in the unit tests below
   ;; (require-binding-receipt), and the declared/source output rule is asserted
   ;; against declared-outputs / active-declared-outputs, so the remaining
   ;; prepare refusals here are the ones that need the real staging pipeline.
   (prepare-job "prepare impure evidence record commit"
                'impure
                prepare-fixtures
                (prepare-campaign (lambda (world campaign root log)
                                    (prepare-run world campaign root)))
                (list 'prefix "impure-record-commit: src-drift"))))

;; ============================================================
;; Module-level unit tests (no git world needed)
;; ============================================================

(define stub-digest-git (lambda (_args) ""))

(define (unit-evidence)
  (hasheq
   'schema-version
   2
   'implementation-sha
   (make-string 40 #\a)
   'content-digest
   (bytes->hex-string (sha256-bytes #""))
   'merge-authorization
   (hasheq 'operator "ops" 'wave "W1" 'head (make-string 40 #\a) 'action "merge" 'source "recorded")))

(module+ test
  (test-case "snapshot-facts refuses without a real campaign root"
    (for ([root (in-list (list #f
                               (path->string (build-path (find-system-path 'temp-dir)
                                                         "s7-absent-campaign"))))])
      (check-equal? (delivery-refusal (lambda ()
                                        (snapshot-facts (lambda (_c _a _t) "{}") root plan-id wave)))
                    "prepare requires --campaign-root with the campaign working tree")))

  (test-case "declared-outputs: none / single / conflicting / malformed"
    (check-false (declared-outputs (hasheq 'status "ok") "/tmp/x"))
    (check-false (declared-outputs (hasheq 'declared
                                           (hasheq 'evidence '() 'review '() 'validation '()))
                                   "/tmp/x"))
    (check-hashes-equal?
     (declared-outputs (hasheq 'declared
                               (hasheq 'evidence
                                       (list (format "docs/reports/gsd-wave-evidence/~a-w1.rktd"
                                                     plan-id))
                                       'review
                                       (list review-relative)
                                       'validation
                                       (list validation-relative)))
                       "/tmp/x")
     (hasheq 'evidence evidence-relative 'review review-relative 'validation validation-relative)
     "single declared trio")
    (check-equal?
     (delivery-refusal
      (lambda ()
        (declared-outputs
         (hasheq
          'declared
          (hasheq 'evidence (list evidence-relative) 'review (list review-relative) 'validation '()))
         "/tmp/x")))
     (string-append "frozen wave document must declare exactly one evidence, review "
                    "and validation output; filename guessing is refused"))
    (check-equal? (delivery-refusal (lambda ()
                                      (declared-outputs (hasheq 'declared
                                                                (hasheq 'evidence
                                                                        (list evidence-relative
                                                                              review-relative)
                                                                        'review
                                                                        (list review-relative)
                                                                        'validation
                                                                        (list validation-relative)))
                                                        "/tmp/x")))
                  (string-append "frozen wave document must declare exactly one evidence, review "
                                 "and validation output; filename guessing is refused"))
    (check-equal? (delivery-refusal
                   (lambda ()
                     (declared-outputs (hasheq 'declared (hasheq 'evidence "not-a-list")) "/tmp/x")))
                  "malformed declared wave output for evidence"))

  (test-case "active-declared-outputs derives the -r<n> repair family"
    (define facts
      (hasheq 'declared
              (hasheq 'evidence
                      (list evidence-relative)
                      'review
                      (list review-relative)
                      'validation
                      (list validation-relative))))
    (check-hashes-equal?
     (active-declared-outputs facts "/tmp/x" plan-id wave 0)
     (hasheq 'evidence evidence-relative 'review review-relative 'validation validation-relative)
     "generation 0")
    (check-hashes-equal? (active-declared-outputs facts "/tmp/x" plan-id wave 1)
                         (hasheq 'evidence
                                 (format "docs/reports/gsd-wave-evidence/~a-w1-r1.rktd" plan-id)
                                 'review
                                 (format "docs/reports/gsd-wave-reviews/~a-w1-r1.rktd" plan-id)
                                 'validation
                                 (format "docs/reports/gsd-wave-validation/~a-w1-r1.rktd" plan-id))
                         "repair generation")
    (check-hashes-equal?
     (active-declared-outputs (hasheq) "/tmp/x" plan-id wave 0)
     (hasheq 'evidence evidence-relative 'review review-relative 'validation validation-relative)
     "zero-declared wave"))

  (test-case "merge-authorization refuses every under-specified direction"
    (define head (make-string 40 #\a))
    (check-equal? (delivery-refusal (lambda ()
                                      (merge-authorization (hasheq 'schema-version 2)
                                                           "W1"
                                                           head
                                                           (make-string 40 #\b)
                                                           stub-digest-git)))
                  "no-operator-authorization: evidence record carries no merge-authorization object")
    (for ([field (in-list (list "operator" "wave" "head" "action" "source"))])
      (define evidence
        (hash-set (unit-evidence)
                  'merge-authorization
                  (hash-remove (hash-ref (unit-evidence) 'merge-authorization)
                               (string->symbol field))))
      (check-equal?
       (delivery-refusal
        (lambda () (merge-authorization evidence "W1" head (make-string 40 #\b) stub-digest-git)))
       (string-append "no-operator-authorization: merge-authorization." field " is missing or empty")
       field))
    (check-equal?
     (delivery-refusal
      (lambda ()
        (merge-authorization (unit-evidence) "W2" head (make-string 40 #\b) stub-digest-git)))
     "no-operator-authorization: merge-authorization names wave 'W1', not 'W2'")
    (check-equal?
     (delivery-refusal (lambda ()
                         (merge-authorization (hash-set (unit-evidence) 'implementation-sha "short")
                                              "W1"
                                              head
                                              (make-string 40 #\b)
                                              stub-digest-git)))
     "no-operator-authorization: evidence record has no implementation head to authorize")
    (check-equal?
     (delivery-refusal (lambda ()
                         (merge-authorization
                          (unit-evidence)
                          "W1"
                          head
                          (make-string 40 #\b)
                          (lambda (args) (if (string-prefix? (car args) "diff") "changed" "")))))
     (string-append
      "no-operator-authorization: the merge tip no longer carries the authorized implementation content"))
    (check-true
     (hash? (merge-authorization (unit-evidence) "W1" head (make-string 40 #\b) stub-digest-git))))

  (test-case "reviewed-head-approval refuses a non-approved, anonymous or misbound review"
    (define evidence (unit-evidence))
    (define impl (hash-ref evidence 'implementation-sha))
    (define review (hasheq 'reviewer "reviewer" 'verdict "APPROVED" 'reviewed-sha impl))
    (check-equal? (delivery-refusal (lambda () (reviewed-head-approval '() evidence "author")))
                  "no-review-artifact: the review artifact is missing or malformed")
    (check-equal? (delivery-refusal (lambda () (reviewed-head-approval review evidence "")))
                  (string-append "no-review-artifact: PR author identity is missing or malformed; "
                                 "the non-author property is unverifiable"))
    (check-equal?
     (delivery-refusal
      (lambda ()
        (reviewed-head-approval (hash-set review 'verdict "CHANGES_REQUESTED") evidence "author")))
     "no-review-artifact: review verdict is 'CHANGES_REQUESTED', not APPROVED")
    (check-equal?
     (delivery-refusal (lambda ()
                         (reviewed-head-approval (hash-remove review 'reviewer) evidence "author")))
     "no-review-artifact: review has no reviewer identity")
    (check-equal?
     (delivery-refusal
      (lambda () (reviewed-head-approval (hash-set review 'reviewer "Author ") evidence "author")))
     "no-review-artifact: review has no non-author reviewer identity")
    (check-equal?
     (delivery-refusal (lambda ()
                         (reviewed-head-approval (hash-set review 'reviewed-sha (make-string 40 #\b))
                                                 evidence
                                                 "author")))
     (format (string-append "head-binding-mismatch: review reviewed-sha ~a does not bind "
                            "the verified implementation head ~a")
             (make-string 40 #\b)
             impl))
    (check-true (hash? (reviewed-head-approval review evidence "author"))))

  (test-case "require-binding-receipt pins the journal receipt"
    (check-not-exn (lambda () (require-binding-receipt #f "head" wave-branch)))
    (define head (make-string 40 #\a))
    (define (journal #:head* [head* head] #:branch [branch wave-branch])
      (hasheq 'receipt (hasheq 'head head* 'branch branch 'attempt-id "attempt-1" 'attempt-fence 2)))
    (check-not-exn (lambda () (require-binding-receipt (journal) head wave-branch)))
    (for ([data (in-list (list (journal #:head* (make-string 40 #\b))
                               (journal #:branch "campaign/other")))])
      (check-equal? (delivery-refusal (lambda () (require-binding-receipt data head wave-branch)))
                    "binding must match the current attempt-bound receipt head/branch"))
    (check-equal?
     (delivery-refusal (lambda () (require-binding-receipt (journal #:branch "main") head "main")))
     "binding must match the current attempt-bound receipt head/branch"))

  (test-case "draft-request builds Python's exact staging request"
    (define source (hasheq 'wave "W1" 'milestone 3 'issue 7))
    (define pr
      (pr-json #:number 4
               #:state "closed"
               #:merged #t
               #:merge-sha (make-string 40 #\b)
               #:head-sha (make-string 40 #\a)))
    (check-equal? (draft-request source plan-id wave pr "binding/x" '("ci") 0)
                  (hasheq 'plan-id
                          plan-id
                          'wave
                          "W1"
                          'milestone
                          3
                          'issue
                          7
                          'status
                          "pending-review"
                          'merge-sha
                          (make-string 40 #\b)
                          'delivery-pr
                          4
                          'delivery-head-sha
                          (make-string 40 #\a)
                          'wave-branch
                          wave-branch
                          'merged-at
                          "2026-01-01T00:00:00Z"
                          'branch
                          "binding/x"
                          'required-pr-checks
                          '("ci")
                          'binding-generation
                          0))
    (check-equal?
     (delivery-refusal (lambda ()
                         (draft-request (hasheq 'wave "W2") plan-id wave pr "binding/x" '("ci"))))
     "source trio belongs to another wave")
    (check-equal? (delivery-refusal (lambda ()
                                      (draft-request source plan-id wave pr "binding/x" '())))
                  "binding draft requires the exact required-check policy snapshot")
    (check-equal? (prepare-next-message)
                  (string-append "Copy trio into a dedicated fresh-main worktree; complete "
                                 "genuine binding review and gates; publish through protected "
                                 "squash PR; wait for main governance; sync and resume /go.")))

  (test-case "the S7 CLI refuses missing flags and unsupported actions"
    (check-equal? (delivery-refusal (lambda () (delivery-merge-action! '("merge"))))
                  "merge requires --repo")
    (check-equal? (delivery-refusal (lambda () (delivery-merge-action! '("merge" "--repo" "/tmp/x"))))
                  "merge requires --pr, --expected-head, --expected-branch and --evidence")
    (check-equal? (delivery-refusal (lambda ()
                                      (delivery-merge-action! '("prepare" "--repo" "/tmp/x"))))
                  "prepare requires --evidence, --output and --campaign-root")
    (check-equal? (delivery-refusal (lambda ()
                                      (delivery-merge-action! (list "prepare"
                                                                    "--repo"
                                                                    "/tmp/x"
                                                                    "--evidence"
                                                                    "e"
                                                                    "--output"
                                                                    "/tmp/out"
                                                                    "--campaign-root"
                                                                    "/tmp/c"))))
                  "prepare without --pr requires --expected-branch")
    (check-equal? (delivery-refusal
                   (lambda () (delivery-merge-action! '("status" "--repo" "/tmp/x" "--wave" "1"))))
                  "unsupported action status; this module implements merge and prepare only")
    (check-equal? (delivery-refusal
                   (lambda () (delivery-merge-action! '("merge" "--repo" "/tmp/x" "--wave" "one"))))
                  "invalid integer for --wave: one"))

  (test-case "the S7 CLI prints the typed delivery-pending JSON and exits 2"
    (define result
      (run-subprocess (path->string racket-executable)
                      #:args (list (path->string merge-module) "merge" "--repo" "/tmp/x")
                      #:timeout 120
                      #:process-group? #t))
    (check-equal? (subprocess-result-exit-code result) 2)
    (define payload (string->jsexpr (subprocess-result-stdout result)))
    (check-equal? (hash-ref payload 'status) "delivery-pending")
    (check-equal? (hash-ref payload 'reason)
                  "merge requires --pr, --expected-head, --expected-branch and --evidence"))

  (test-case "every merge/prepare job reports its declared outcome"
    (check-true (>= (length jobs) 22) "the job table must cover the whole slice")
    (check-jobs! jobs)))
