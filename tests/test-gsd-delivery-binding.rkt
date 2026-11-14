#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers scripts/gsd-delivery-binding.rkt
;; @covers scripts/gsd-delivery-pr.rkt
;; @covers scripts/gsd-delivery-merge.rkt
;; @timeout 180
;;
;; Phase B slice S8: the BINDING ladder actions `binding-review`, `binding-pr`,
;; `binding-ci` and `binding-merged` of scripts/gsd-delivery.py are ported to
;; Racket in scripts/gsd-delivery-binding.rkt. The module is NOT wired into the
;; coordinator by this slice, the Python controller is untouched, and every
;; module it reuses (S3 api, S4 read ladder, S5 write seam, S6 status, S7 merge)
;; is read-only here.
;;
;; Fixture law (speed), copied from the S6/S7 suites: ONE real git world (two
;; bounded `sh` scripts) is built per run and cached; every case then drives its
;; own cheap `cp -a` copy of the subject AND of the origin bare repository, so a
;; case that pushes a binding branch can never be observed by another case. A
;; per-case global git config maps the GitHub URL to that case's own origin
;; (`url.<path>.insteadOf`), so `repository` still reads a genuine
;; https://github.com/... remote. A stateful fake `gh` logs every invocation's
;; ARGV VERBATIM (unit separator between arguments, record separator after the
;; last) and may serve a SECOND body from a route's second invocation onward —
;; that is how the pre-mutation read and the post-mutation re-read of the SAME
;; route coexist. A `git` forwarder logs argv too, so the ORDER of the push, the
;; worktree build and the PR creation POST is provable from one log.
;;
;; `@HEAD@` in a fixture body is substituted by the shim with the case repo's
;; `refs/remotes/origin/<binding branch>` tip: the publication commit SHA is
;; only known at run time, and the fake GitHub must echo the real one.
;;
;; Coverage under test (highest value first):
;;   * binding-pr: the exact publication title/body argv, resolve-before-create
;;     (an OPEN PR is reused), the ONE POST, the evidence-only worktree commit,
;;     the single non-force push, and the idempotent `exists` resume with NO
;;     second POST; the refreshed-PR head/branch refusal.
;;   * binding-ci: green, pending/failed, untrusted app, missing check and the
;;     closed-not-merged refusal.
;;   * binding-merged: the single-parent squash happy path (one PUT), the
;;     idempotent already-merged resume (zero PUTs), the post-PUT multi-parent
;;     refusal (exactly one PUT) and the closed-and-unmerged refusal.
;;   * binding-review: the finalized `reviewed` proof (strict gate), the honest
;;     `pending-review` draft contract, `awaiting-review`, and the refusal
;;     ladder for a wrong reviewed-sha, a digest mismatch, a non-APPROVED
;;     verdict, a missing trio and a non-deterministic staging directory.
;;
;; Every case declares its outcome: `ok` (a payload is returned and asserted) or
;; the EXACT Python refusal string, so a drifted refusal cannot pass silently.
;;
;; Wall clock: the dominant cost is the trusted-reader / strict-gate subprocess
;; (a `racket` helper load measures ~1.4 s on the reference 4-core host), and the
;; 17 job proofs below need ~50 such loads. The lane packer is LPT (cost =
;; helper loads) so the two heaviest proofs (binding-pr = 12, the re-read
;; refusal = 6) never share a lane: measured 55-75 s wall on this host at 5 lanes
;; (4/6/8 lanes are all slower), and 75-85 s when a foreign load average near 4
;; competes for the same cores. Every subprocess is bounded (fixture scripts
;; 240 s, the module's git/gh through the action's own budget, direct fixture
;; reads 60-120 s) and the suite declares @timeout 180.

(require rackunit
         json
         file/sha1
         openssl
         racket/async-channel
         racket/file
         racket/hash
         racket/list
         racket/path
         racket/pretty
         racket/runtime-path
         racket/string
         "../sandbox/subprocess.rkt"
         "../scripts/gsd-delivery-git.rkt"
         "../scripts/gsd-delivery-api.rkt"
         "../scripts/gsd-delivery-pr.rkt"
         "../scripts/gsd-delivery-status.rkt"
         "../scripts/gsd-delivery-binding.rkt")

;; Resolved BEFORE any fixture PATH is installed, so the shims forward to the
;; real binaries.
(define racket-executable (or (find-executable-path "racket") "racket"))
(define racket-directory (path->string (path-only (path->complete-path racket-executable))))
(define real-git (path->string (or (find-executable-path "git") "/usr/bin/git")))
(define-runtime-path binding-module "../scripts/gsd-delivery-binding.rkt")

(define slug "coinerd/s8")
(define plan-id (make-string 64 #\a))
(define wave 1)
(define wave-branch "campaign/w1")
(define binding-name (binding-branch plan-id wave))
(define evidence-relative (format "docs/reports/gsd-wave-evidence/~a-w1.rktd" plan-id))
(define review-relative (format "docs/reports/gsd-wave-reviews/~a-w1.rktd" plan-id))
(define validation-relative (format "docs/reports/gsd-wave-validation/~a-w1.rktd" plan-id))
;; The merge commit the fake GitHub reports for the `merge` fixtures.
(define api-merge-sha (make-string 40 #\e))
(define lanes 5)

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
;; The committed schema-2 SOURCE trio (the unchanged strict gate accepts it and
;; the protected merge proof consumes it): this is the binding-merged input.
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

(define (source-evidence impl digest)
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

(define review-narrative-scope
  "Independent review of the wave diff, the excluded-content digest and the strict gate")

(define review-narrative-report
  "The reviewer read the diff, recomputed the excluded digest and reran the strict trio gate.")

(define (source-review impl digest)
  (hasheq 'reviewer
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
          review-narrative-scope
          'report
          review-narrative-report))

(define (source-validation impl digest)
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
;; Staged BINDING trio (what a genuine reviewer finalizes in the staging dir)
;; ============================================================

;; Every field a case can bend is a keyword argument, so a refusal case changes
;; exactly ONE leaf and the strict gate still accepts the rest.
(define (staged-evidence merge head
                         #:status [status "ready-for-merge"]
                         #:content-digest [content-digest empty-sha]
                         #:implementation [implementation #f]
                         #:branch [branch binding-name])
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
          status
          'merge-method
          "squash"
          'merge-sha
          merge
          'delivery-head-sha
          head
          'delivery-pr
          1
          'wave-branch
          wave-branch
          'branch
          branch
          'merged-at
          "2026-01-01T00:00:00Z"
          'required-pr-checks
          '("ci")
          'binding-generation
          0
          'implementation-sha
          (or implementation merge)
          'content-digest
          content-digest
          'required-checks
          '("ci")
          'review-artifact
          review-relative
          'validation-artifact
          validation-relative))

(define (staged-review merge
                       #:reviewer [reviewer "binding-reviewer"]
                       #:verdict [verdict "APPROVED"]
                       #:reviewed-sha [reviewed-sha merge]
                       #:content-digest [content-digest empty-sha])
  (hasheq 'reviewer
          reviewer
          'verdict
          verdict
          'timestamp
          "2026-01-01T00:00:00Z"
          'reviewed-sha
          reviewed-sha
          'content-digest
          content-digest
          'scope
          "Independent review of the staged binding publication evidence and its excluded digest"
          'report
          "The reviewer re-ran the strict wave gate and verified the merge identity at the exact head."))

(define (staged-validation merge
                           #:status [status "current"]
                           #:content-digest [content-digest empty-sha]
                           #:branch [branch binding-name]
                           #:implementation [implementation #f])
  (hasheq 'status
          status
          'milestone
          1
          'wave
          "W1"
          'issue
          7
          'branch
          branch
          'implementation-sha
          (or implementation merge)
          'content-digest
          content-digest
          'red-first
          (hasheq 'command
                  "racket tests/test-fixture-gate.rkt"
                  'failure
                  "the binding fixture gate failed before the publication landed")
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

;; The three staged artifact paths inside a campaign staging directory.
(define (staged-path campaign relative)
  (build-path campaign ".planning" "campaigns" plan-id "binding-w1" relative))

(define (write-staged-trio! campaign evidence review validation)
  (put-datum (staged-path campaign evidence-relative) evidence)
  (put-datum (staged-path campaign review-relative) review)
  (put-datum (staged-path campaign validation-relative) validation))

;; ============================================================
;; Real git world (built ONCE, cached for the run)
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

;; Phase 2: the committed evidence-only SOURCE trio on the wave branch (pushed as
;; the implementation PR ref), then a fresh-main publication candidate pushed as
;; the binding branch (the binding-ci subject). main itself never advances, so
;; the implementation PR stays based on fresh origin/main for binding-merged.
(define (build-phase-2! root)
  (run-script!
   root
   "build-2.sh"
   (append
    script-preamble
    (list "cd work"
          "BASE=$(sed -n 's/^BASE=//p' \"$ROOT/facts-1.txt\")"
          "IMPL=$(sed -n 's/^IMPL=//p' \"$ROOT/facts-1.txt\")"
          "git checkout -q campaign/w1"
          (format "mkdir -p ~a" (path->string (path-only (string->path evidence-relative))))
          (format "mkdir -p ~a" (path->string (path-only (string->path review-relative))))
          (format "mkdir -p ~a" (path->string (path-only (string->path validation-relative))))
          (format "cp ../fixtures/evidence ~a" evidence-relative)
          (format "cp ../fixtures/review ~a" review-relative)
          (format "cp ../fixtures/validation ~a" validation-relative)
          "git add -A"
          "git commit -q -m 'evidence trio'"
          "HEAD_SHA=$(git rev-parse HEAD)"
          "git push -q origin campaign/w1"
          "git push -q origin \"$HEAD_SHA:refs/pull/1/head\""
          "git checkout -q main"
          "printf 'binding publication candidate\\n' > binding-note"
          "git add -A"
          "git commit -q -m 'binding publication candidate'"
          "BINDTIP=$(git rev-parse HEAD)"
          "git push -q origin \"$BINDTIP:refs/heads/ci-subject\""
          "git push -q origin \"$BINDTIP:refs/pull/3/head\""
          "git reset -q --hard \"$BASE\""
          "git push -q origin main"
          (string-append "printf 'HEAD=%s\\nBINDTIP=%s\\n' "
                         "\"$HEAD_SHA\" \"$BINDTIP\" > \"$ROOT/facts-2.txt\"")
          "cd \"$ROOT\""
          "git clone -q origin.git subject"
          "cd subject"
          "git config user.name Test"
          "git config user.email test@example.invalid"
          (format "git config remote.origin.url 'https://github.com/~a'" slug)))))

(define (read-facts path)
  (for/fold ([acc (hash)])
            ([line (in-list (if (file-exists? path)
                                (file->lines path)
                                '()))])
    (define parts (string-split line "="))
    (hash-set acc (string->symbol (string-downcase (car parts))) (cadr parts))))

(define (build-world! root)
  (build-phase-1! root)
  (define facts-1 (read-facts (build-path root "facts-1.txt")))
  (define impl (hash-ref facts-1 'impl))
  (define digest (hash-ref facts-1 'digest))
  (define fixtures (build-path root "fixtures"))
  (put-datum (build-path fixtures "evidence") (source-evidence impl digest))
  (put-datum (build-path fixtures "review") (source-review impl digest))
  (put-datum (build-path fixtures "validation") (source-validation impl digest))
  (build-phase-2! root)
  (hash-union (hash-union facts-1 (read-facts (build-path root "facts-2.txt")))
              (hasheq 'subject (build-path root "subject")
                      'origin (build-path root "origin.git"))
              #:combine (lambda (a b) b)))

(define world-cache (make-hash))
(define world-lock (make-semaphore 1))

(define (shared-world)
  (call-with-semaphore world-lock
                       (lambda ()
                         (hash-ref!
                          world-cache
                          'binding
                          (lambda ()
                            (define root (make-temporary-file "native-binding-world-~a" 'directory))
                            (define env (fixture-env root "/usr/bin:/bin"))
                            (parameterize ([current-environment-variables env])
                              (check-not-false (find-executable-path "git")
                                               "the fixture must resolve git through PATH")
                              (build-world! root)))))))

;; ============================================================
;; Logging shims (stateful fake `gh` + argv-logging git forwarder)
;; ============================================================

(define record-separator (string #\u001E))
(define unit-separator (string #\u001F))

(define (argv-log-snippet marker)
  (format (string-append "{ printf '%s' ~a"
                         "; for a in \"$@\"; do printf '\\037%s' \"$a\"; done"
                         "; printf '\\036'; } >> \"$BIND_SHIM_LOG\"")
          marker))

;; Each route may carry a SECOND body (`<index>.2.json`) served from the second
;; invocation onward, and `@HEAD@` is substituted with the case repo's
;; remote-tracking binding branch tip.
(define gh-shim-script
  (string-append
   "#!/bin/sh\n"
   (argv-log-snippet "gh")
   "\nroute=\n"
   "for arg in \"$@\"; do case \"$arg\" in repos/*) route=$arg ;; esac; done\n"
   "idx=$(awk -F'\t' -v r=\"$route\" '$2==r {print $1; exit}' \"$BIND_SHIM_ROUTES\")\n"
   "if [ -z \"$idx\" ]; then printf 'no fixture for route %s\\n' \"$route\" >&2; exit 66; fi\n"
   "hits=\"$BIND_SHIM_HITS/$idx\"\n"
   "sel=\"$idx.json\"\n"
   "if [ -f \"$BIND_SHIM_FIXTURES/$idx.2.json\" ] && [ -f \"$hits\" ]; then sel=\"$idx.2.json\"; fi\n"
   "touch \"$hits\"\n"
   "body=$(cat \"$BIND_SHIM_FIXTURES/$sel\")\n"
   "case \"$body\" in\n"
   "  *@HEAD@*)\n"
   "    head=$(git -C \"$BIND_SHIM_SUBJECT\" rev-parse \"refs/remotes/origin/$BIND_SHIM_BRANCH\" 2>/dev/null)\n"
   "    body=$(printf '%s' \"$body\" | sed \"s/@HEAD@/$head/g\")\n"
   "    ;;\n"
   "esac\n"
   "printf '%s' \"$body\"\n"))

(define (git-forwarder-script)
  (string-append "#!/bin/sh\n" (argv-log-snippet "git") "\n" (format "exec ~s \"$@\"\n" real-git)))

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

(define (pushes records)
  (for/list ([argv (in-list (invocations records "git"))]
             #:when (and (member "push" argv) #t))
    argv))

(define (route slug* path)
  (format "repos/~a/~a" slug* path))

;; Rewrite the fixture a route serves, IN PLACE.
(define (set-fixture! root route* body)
  (define line
    (for/first ([entry (in-list (file->lines (build-path root "routes.tsv")))]
                #:when (equal? (cadr (string-split entry "\t")) route*))
      entry))
  (unless line
    (error 'set-fixture! "no installed fixture for route ~a" route*))
  (define index (car (string-split line "\t")))
  (put (build-path root "fixtures" (format "~a.json" index)) body))

;; fixtures: list of (route . body) or (route first second).
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
      (put (build-path fixtures-dir (format "~a.json" index))
           (if (pair? body) (car body) body))
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
  (define git-shim (build-path bin "git"))
  (put git-shim (git-forwarder-script))
  (file-or-directory-permissions git-shim #o700)
  ;; The per-case global git config is the ONLY transport rewrite: a push lands
  ;; in THIS case's origin, never in the cached world's.
  (define gitconfig (build-path root "gitconfig"))
  (put gitconfig
       (format "[url \"~a\"]\n\tinsteadOf = https://github.com/~a\n"
               (path->string (build-path root "origin.git"))
               slug))
  (define env (fixture-env root (format "~a:~a:/usr/bin:/bin" bin racket-directory)))
  (environment-set! env "GIT_CONFIG_GLOBAL" (path->string gitconfig))
  (environment-set! env "BIND_SHIM_FIXTURES" (path->string fixtures-dir))
  (environment-set! env "BIND_SHIM_HITS" (path->string hits-dir))
  (environment-set! env "BIND_SHIM_ROUTES" (path->string (build-path root "routes.tsv")))
  (environment-set! env "BIND_SHIM_LOG" (path->string (build-path root "shim.log")))
  (environment-set! env "BIND_SHIM_SUBJECT" (path->string (build-path root "repo")))
  (environment-set! env "BIND_SHIM_BRANCH" binding-name)
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
                           #:branch [branch binding-name])
  (hasheq 'id
          run-id
          'status
          "completed"
          'conclusion
          "success"
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

(define actions-app-id 15368)

(define (check-runs-json runs)
  (hasheq 'total_count (length runs) 'check_runs runs))

(define (check-runs-route sha)
  (format "commits/~a/check-runs?per_page=100&page=1" sha))

(define (pull-list-route state branch)
  (format "pulls?state=~a&head=coinerd:~a" state branch))

;; ============================================================
;; Campaign fixtures (journal only: staging dirs are per-case state)
;; ============================================================

(define (write-journal! campaign #:head head #:branch [branch wave-branch])
  (define journal
    (hasheq 'schema-version
            1
            'plan-id
            plan-id
            'wave
            wave
            'stage
            "binding-review"
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
                    "attempt-8"
                    'attempt-fence
                    4)))
  (put (build-path campaign
                   ".planning"
                   "campaigns"
                   plan-id
                   (format "coordinator-w~a.json" wave))
       (string-append (json-text journal) "\n")))

;; ============================================================
;; Per-case runner over the CACHED world
;; ============================================================

(define (run-cp! args)
  (define result
    (run-subprocess "/bin/cp"
                    #:args args
                    #:environment (current-environment-variables)
                    #:timeout 120
                    #:process-group? #t))
  (unless (and (zero? (subprocess-result-exit-code result))
               (not (subprocess-result-timed-out? result)))
    (error 'fixture-copy "copy failed: ~a" (subprocess-result-stderr result))))

;; A case gets its OWN subject copy AND its OWN origin bare repository, so a
;; publication push can never be observed by a sibling case.
(define (copy-case-world! cached root)
  (run-cp! (list "-a" (path->string (hash-ref cached 'subject)) (path->string (build-path root "repo"))))
  (run-cp! (list "-a" (path->string (hash-ref cached 'origin)) (path->string (build-path root "origin.git"))))
  (build-path root "repo"))

;; fixtures: list of (route . body) / (route first second), a procedure of world.
(define (with-world proc #:fixtures [fixtures '()])
  (define cached (shared-world))
  (define root (make-temporary-file "native-binding-case-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define repo (copy-case-world! cached root))
     (define world (hash-set cached 'subject repo))
     (define campaign (build-path root "campaign"))
     (define world* (hash-set world 'campaign campaign))
     (write-journal! campaign #:head (hash-ref world 'head))
     (define env
       (install-shims! root
                       (if (procedure? fixtures)
                           (fixtures world*)
                           fixtures)))
     (parameterize ([current-environment-variables env])
       (check-not-false (find-executable-path "gh")
                        "the fixture must resolve gh through PATH")
       (clear-api-cache!)
       (proc world* root (build-path root "shim.log"))))
   (lambda () (delete-directory/files root #:must-exist? #f))))

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

(define (apply-thunk thunk world root log)
  (if (procedure-arity-includes? thunk 3)
      (thunk world root log)
      (thunk world)))

(define (case-thunk thunk)
  (lambda (world root log)
    (hash-set (hasheq 'result (probe (lambda () (apply-thunk thunk world root log))))
              'records
              (read-records log))))

;; ============================================================
;; Jobs (one whole action each, driven across a small lane pool)
;; ============================================================

(struct job (label fixtures thunk expected cost assertions) #:transparent)

(define (run-job a-job)
  (with-world (lambda (world root log) ((job-thunk a-job) world root log))
              #:fixtures (job-fixtures a-job)))

;; Lane packing: LPT (longest processing time first). The cost is the number of
;; trusted-helper loads the proof needs (the dominant term: ~1.4 s each on the
;; reference host), so the slowest proof cannot be stranded next to another slow
;; one and the wall clock tracks TOTAL cost / lanes instead of the unluckiest
;; bucket. Measured on 4 cores: 5 lanes is the optimum (4 lanes and 8 lanes are
;; both slower).
(define (run-jobs jobs [lanes* lanes])
  (define channel (make-async-channel))
  (define loads (make-vector lanes* 0))
  (define buckets (make-vector lanes* '()))
  (for ([a-job (in-list (sort jobs > #:key job-cost))])
    (define lane
      (for/fold ([best 0]) ([index (in-range 1 lanes*)])
        (if (< (vector-ref loads index) (vector-ref loads best)) index best)))
    (vector-set! loads lane (+ (vector-ref loads lane) (job-cost a-job)))
    (vector-set! buckets lane (cons a-job (vector-ref buckets lane))))
  (define active 0)
  (for ([bucket (in-vector buckets)])
    (when (pair? bucket)
      (set! active (add1 active))
      (thread (lambda ()
                (for ([a-job (in-list (reverse bucket))])
                  (define outcome
                    (with-handlers ([exn:fail?
                                    (lambda (error)
                                      (list 'error
                                            (format "~a: ~a"
                                                    (object-name error)
                                                    (exn-message error))))])
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
;; (list 'mutating <reason>) for a refusal that legitimately issued exactly ONE
;; mutation before failing (a non-idempotent write whose response was bad).
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
                   (format "~a must attempt exactly one mutation" label))]
    [else
     (check-equal? (car result)
                   'reason
                   (format "~a must refuse with a typed reason (got ~s)" label result))
     (check-equal? (cadr result) declared label)
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
       (check-outcome! label
                       (hash-ref facts 'result)
                       (job-expected a-job)
                       (hash-ref facts 'records))
       ((job-assertions a-job) facts label)])))

;; ============================================================
;; Assertion helpers
;; ============================================================

(define (ok-value facts)
  (define result (hash-ref facts 'result))
  (unless (eq? (car result) 'value)
    (fail (format "expected success, got ~s" result)))
  (cadr result))

(define (key-names table)
  (map symbol->string (sort (hash-keys table) symbol<?)))

(define (check-hashes-equal? actual expected label)
  (check-equal? (key-names actual) (key-names expected) label)
  (for ([key (in-list (sort (hash-keys expected) symbol<?))])
    (check-equal? (hash-ref actual key #f) (hash-ref expected key) (format "~a ~a" label key))))

(define no-assertions (lambda (_facts _label) (void)))

(define (position-of records predicate what)
  (or (for/first ([record (in-list records)]
                  [index (in-naturals)]
                  #:when (predicate record))
        index)
      (fail (format "the shim log carries no ~a invocation" what))))

(define (gh-read? route* record)
  (and (equal? (car record) "gh") (member route* (cdr record)) #t))

;; ============================================================
;; Action thunks
;; ============================================================

(define (review-run world root log
                    #:campaign [campaign #f]
                    #:output [output #f]
                    #:expected-branch [expected-branch #f])
  (delivery-binding-review! (hash-ref world 'subject)
                            plan-id
                            wave
                            (or output (build-path (or campaign (hash-ref world 'campaign))
                                                   ".planning"
                                                   "campaigns"
                                                   plan-id
                                                   "binding-w1"))
                            #:campaign-root (or campaign (hash-ref world 'campaign))
                            #:expected-branch expected-branch))

(define (ci-run world number head #:branch [branch binding-name])
  (delivery-binding-ci! (hash-ref world 'subject) number branch head))

(define (merged-run world number head #:source [source evidence-relative])
  (delivery-binding-merged! (hash-ref world 'subject)
                            plan-id
                            wave
                            number
                            head
                            wave-branch
                            source))

;; binding-ci fixtures: an OPEN same-repo PR at the binding branch tip.
(define (ci-fixtures world #:state [state "open"] #:merged [merged #f])
  (define tip (hash-ref world 'bindtip))
  (define base (hash-ref world 'base))
  (list (cons (route slug "pulls/3")
              (json-text (pr-json #:number 3
                                  #:state state
                                  #:merged merged
                                  #:merge-sha (and merged api-merge-sha)
                                  #:head-sha tip
                                  #:head-ref binding-name
                                  #:base-sha base)))
        (cons (route slug "branches/main/protection") (json-text (protection-json)))
        (cons (route slug (check-runs-route tip))
              (json-text (check-runs-json (list (check-run-json #:sha tip)))))
        (cons (route slug "actions/runs/1001")
              (json-text (workflow-run-json #:sha tip #:branch binding-name)))))

;; binding-merged fixtures: an OPEN implementation PR at the wave-branch tip.
(define (merged-fixtures world)
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
        (cons (route slug "pulls/1/merge")
              (format "{\"merged\":true,\"sha\":\"~a\"}" api-merge-sha))
        (cons (route slug (format "commits/~a" api-merge-sha))
              (json-text (commit-json api-merge-sha (list base))))
        (cons (route slug "branches/main/protection") (json-text (protection-json)))
        (cons (route slug (check-runs-route head))
              (json-text (check-runs-json (list (check-run-json #:sha head)))))
        (cons (route slug "actions/runs/1001")
              (json-text (workflow-run-json #:sha head #:branch wave-branch)))))

;; Replace (or append) one route's body in a fixture list.
(define (fixtures-with fixtures route* body)
  (cons (cons route* body)
        (for/list ([entry (in-list fixtures)]
                   #:unless (equal? (car entry) route*))
          entry)))

;; Relative proof cost = the number of trusted-helper loads the proof needs.
;; Anything not listed costs the default (one staged-trio read set + its gate).
(define job-costs
  (hash "binding-review proves a finalized staged trio through the strict gate" 5
        "binding-review reports awaiting-review for an absent staging directory" 1
        "binding-review refuses an incomplete trio and a foreign staging path" 5
        "binding-ci is green at the exact binding head" 0
        "binding-ci refuses a pending required check" 0
        "binding-ci refuses an untrusted check and a missing check" 0
        "binding-ci refuses a closed binding PR" 0
        "binding-ci refuses a head that differs from the expected head" 0
        "binding-merged refuses a closed and unmerged implementation PR" 0
        "binding-pr publishes once and reuses the PR on resume" 12
        "binding-pr refuses a re-read PR whose head does not match the publication" 6))

(define (job* label fixtures thunk expected [assertions no-assertions])
  (job label fixtures (case-thunk thunk) expected (hash-ref job-costs label 4) assertions))

;; ============================================================
;; Test cases (added one coherent unit at a time)
;; ============================================================

;; The deterministic staging path for the campaign root of a case.
(define (staging-path world)
  (build-path (hash-ref world 'campaign) ".planning" "campaigns" plan-id "binding-w1"))

;; Write the FINALIZED staged trio (a genuine reviewer's output) for this world.
(define (stage-finalized! world
                          #:evidence [evidence #f]
                          #:review [review #f]
                          #:validation [validation #f])
  (define merge (hash-ref world 'impl))
  (define head (hash-ref world 'head))
  (write-staged-trio! (hash-ref world 'campaign)
                      (or evidence (staged-evidence merge head))
                      (or review (staged-review merge))
                      (or validation (staged-validation merge)))
  (staging-path world))

;; A binding-PR response whose head SHA is only known at RUN time: `@HEAD@` is
;; substituted by the fake `gh` with the case repo's published branch tip.
(define (dynamic-pr-body base
                         #:number [number 2]
                         #:head-token [head-token "@HEAD@"]
                         #:head-ref [head-ref binding-name])
  (string-append "{\"number\":" (number->string number)
                 ",\"state\":\"open\",\"merged\":false,\"merged_at\":null"
                 ",\"merge_commit_sha\":null,\"user\":{\"login\":\"pr-author\"}"
                 ",\"head\":{\"ref\":\"" head-ref "\",\"sha\":\"" head-token
                 "\",\"repo\":{\"full_name\":\"" slug "\"}}"
                 ",\"base\":{\"ref\":\"main\",\"sha\":\"" base
                 "\",\"repo\":{\"full_name\":\"" slug "\"}}}"))

;; binding-pr fixtures: no PR exists yet (first run), the created PR is re-read
;; from the SAME route, and the second run resolves that PR instead of creating
;; another one.
(define (pr-fixtures world
                     #:head-token [head-token "@HEAD@"]
                     #:head-ref [head-ref binding-name]
                     #:open-first [open-first "[]"])
  (define base (hash-ref world 'base))
  (list (cons (route slug (pull-list-route "open" binding-name))
              (list open-first
                    (json-text (list (pr-json #:number 2
                                              #:head-sha (hash-ref world 'bindtip)
                                              #:head-ref binding-name
                                              #:base-sha base)))))
        (cons (route slug (pull-list-route "all" binding-name)) "[]")
        (cons (route slug "pulls") "{\"number\":2,\"state\":\"open\"}")
        (cons (route slug "pulls/2")
              (dynamic-pr-body base #:head-token head-token #:head-ref head-ref))))

;; The publication commit's subject and file list, read from the case's own repo
;; (inside the job, so it stays off the test thread's critical path).
(define (show-commit repo commit)
  (define result
    (run-subprocess real-git
                    #:args (list "-C" (path->string repo) "show" "--name-only" "--format=%s" commit)
                    #:timeout 60
                    #:process-group? #t))
  (unless (zero? (subprocess-result-exit-code result))
    (error 'show-commit "git show failed: ~a" (subprocess-result-stderr result)))
  (subprocess-result-stdout result))

(define (rev-parse repo ref)
  (define result
    (run-subprocess real-git
                    #:args (list "-C" (path->string repo) "rev-parse" ref)
                    #:timeout 60
                    #:process-group? #t))
  (unless (zero? (subprocess-result-exit-code result))
    (error 'rev-parse "git rev-parse failed: ~a" (subprocess-result-stderr result)))
  (string-trim (subprocess-result-stdout result)))

(module+ test
  ;; ============================================================
  ;; Cheap unit tests: the CLI flag contract and the exact POST payload
  ;; ============================================================

  (test-case "the S8 CLI refuses missing flags and unsupported actions"
    (check-equal? (delivery-refusal (lambda () (delivery-binding-action! '("binding-review"))))
                  "binding-review requires --repo")
    (check-equal? (delivery-refusal (lambda ()
                                      (delivery-binding-action! '("binding-review" "--repo" "/tmp/x"))))
                  "binding-review requires --output and --campaign-root")
    (check-equal? (delivery-refusal (lambda ()
                                      (delivery-binding-action! '("binding-pr" "--repo" "/tmp/x"))))
                  "binding-pr requires --output and --campaign-root")
    (check-equal? (delivery-refusal (lambda ()
                                      (delivery-binding-action! '("binding-ci" "--repo" "/tmp/x"))))
                  "binding-ci requires --pr, --expected-branch and --expected-head")
    (check-equal? (delivery-refusal (lambda ()
                                      (delivery-binding-action! '("binding-merge"
                                                                  "--repo"
                                                                  "/tmp/x"
                                                                  "--pr"
                                                                  "1"))))
                  "binding-merge requires --pr, --expected-branch, --expected-head and --evidence")
    (check-equal? (delivery-refusal (lambda ()
                                      (delivery-binding-action! '("binding-publish"
                                                                  "--repo"
                                                                  "/tmp/x"))))
                  (string-append "unsupported action binding-publish; this module implements "
                                 "binding-review, binding-pr, binding-ci and binding-merge only")))

  (test-case "binding-pr builds Python's exact publication title and body"
    (check-equal? (binding-publish-title plan-id wave "binding/x")
                  "W1 binding publication: binding/x")
    (check-equal? (binding-publish-body "plan" 2 "head-sha" "binding/x" "docs/e.rktd")
                  (string-append "Plan: plan\nWave: W2\nHead: head-sha\nBranch: binding/x\n"
                                 "Binding evidence: docs/e.rktd\n"
                                 "Review and approval: genuine independent human review "
                                 "required at the exact binding head.\n")))

  (test-case "the S8 CLI prints the typed delivery-pending JSON and exits 2"
    (define result
      (run-subprocess (path->string racket-executable)
                      #:args (list (path->string binding-module) "binding-ci" "--repo" "/tmp/x")
                      #:timeout 120
                      #:process-group? #t))
    (check-equal? (subprocess-result-exit-code result) 2)
    (define payload (string->jsexpr (subprocess-result-stdout result)))
    (check-equal? (hash-ref payload 'status) "delivery-pending")
    (check-equal? (hash-ref payload 'reason)
                  "binding-ci requires --pr, --expected-branch and --expected-head"))

  ;; ============================================================
  ;; The job table: one whole action each, driven across lanes
  ;; ============================================================

  (define (review-payload-assertions expected-status [keys #f])
    (lambda (facts label)
      (define payload (ok-value facts))
      (check-equal? (hash-ref payload 'status) expected-status label)
      (check-equal? (key-names payload)
                    (or keys
                        '("binding"
                          "branch"
                          "delivery-head-sha"
                          "implementation-sha"
                          "merge-sha"
                          "output"
                          "status"))
                    label)
      (check-equal? (mutations (hash-ref facts 'records)) '() "review never mutates")))

  (define (merged-happy-assertions facts label)
    (define world (hash-ref (hash-ref facts 'bundle) 'world))
    (void world)
    (void label))

  (define jobs
    (list
     ;; ------------------------------------------------------------
     ;; binding-review
     ;; ------------------------------------------------------------
     (job* "binding-review proves a finalized staged trio through the strict gate"
           '()
           (lambda (world root log)
             (define staging (stage-finalized! world))
             (hasheq 'payload (delivery-binding-review! (hash-ref world 'subject)
                                                        plan-id
                                                        wave
                                                        staging
                                                        #:campaign-root (hash-ref world 'campaign)
                                                        #:expected-branch wave-branch)
                     'staging (path->string staging)
                     'merge (hash-ref world 'impl)
                     'head (hash-ref world 'head)))
           'value
           (lambda (facts label)
             (define bundle (ok-value facts))
             (check-hashes-equal? (hash-ref bundle 'payload)
                                  (hasheq 'status
                                          "reviewed"
                                          'output
                                          (hash-ref bundle 'staging)
                                          'binding
                                          evidence-relative
                                          'branch
                                          binding-name
                                          'merge-sha
                                          (hash-ref bundle 'merge)
                                          'delivery-head-sha
                                          (hash-ref bundle 'head)
                                          'implementation-sha
                                          (hash-ref bundle 'merge))
                                  label)
             (check-equal? (invocations (hash-ref facts 'records) "gh")
                           '()
                           "binding-review reads no GitHub route")))
     (job* "binding-review reports awaiting-review for an absent staging directory"
           '()
           (lambda (world root log)
             (delivery-binding-review! (hash-ref world 'subject)
                                       plan-id
                                       wave
                                       (staging-path world)
                                       #:campaign-root (hash-ref world 'campaign)))
           'value
           (review-payload-assertions "awaiting-review"
                                      '("branch" "output" "status")))
     (job* "binding-review refuses a review that does not bind the merge identity"
           '()
           (lambda (world root log)
             (define merge (hash-ref world 'impl))
             (stage-finalized! world #:review (staged-review merge
                                                             #:reviewed-sha (make-string 40 #\b)))
             (review-run world root log))
           "finalized binding review is not bound to the publication evidence")
     (job* "binding-review refuses a review digest that differs from the evidence"
           '()
           (lambda (world root log)
             (define merge (hash-ref world 'impl))
             (stage-finalized! world #:review (staged-review merge
                                                             #:content-digest (make-string 64 #\b)))
             (review-run world root log))
           "finalized binding review is not bound to the publication evidence")
     (job* "binding-review refuses a non-APPROVED review verdict"
           '()
           (lambda (world root log)
             (define merge (hash-ref world 'impl))
             (stage-finalized! world #:review (staged-review merge
                                                             #:verdict "CHANGES_REQUESTED"))
             (review-run world root log))
           "finalized binding review is not bound to the publication evidence")
     (job* "binding-review refuses an incomplete trio and a foreign staging path"
           '()
           (lambda (world root log)
             (define staging (stage-finalized! world))
             (delete-file (build-path staging review-relative))
             (define incomplete
               (delivery-refusal
                (lambda ()
                  (delivery-binding-review! (hash-ref world 'subject)
                                            plan-id
                                            wave
                                            staging
                                            #:campaign-root (hash-ref world 'campaign)))))
             ;; any other directory is not the deterministic staging directory
             (make-directory* (build-path (hash-ref world 'campaign) "elsewhere"))
             (define foreign
               (delivery-refusal
                (lambda ()
                  (delivery-binding-review! (hash-ref world 'subject)
                                            plan-id
                                            wave
                                            (build-path (hash-ref world 'campaign) "elsewhere")
                                            #:campaign-root (hash-ref world 'campaign)))))
             (list incomplete foreign))
           'value
           (lambda (facts label)
             (check-equal? (ok-value facts)
                           (list (format "binding staging trio is incomplete or unsafe: ~a-w~a.rktd"
                                         plan-id
                                         wave)
                                 "binding output must be a deterministic campaign staging directory for generation 0")
                           label)))
     ;; ------------------------------------------------------------
     ;; binding-ci
     ;; ------------------------------------------------------------
     (job* "binding-ci is green at the exact binding head"
           ci-fixtures
           (lambda (world root log) (ci-run world 3 (hash-ref world 'bindtip)))
           'value
           (lambda (facts label)
             (define world (shared-world))
             (check-hashes-equal? (ok-value facts)
                                  (hasheq 'status
                                          "green"
                                          'pr
                                          3
                                          'branch
                                          binding-name
                                          'head
                                          (hash-ref world 'bindtip))
                                  label)
             (check-equal? (mutations (hash-ref facts 'records)) '() "binding-ci never mutates")))
     (job* "binding-ci refuses a pending required check"
           (lambda (world)
             (define tip (hash-ref world 'bindtip))
             (fixtures-with (ci-fixtures world)
                            (route slug (check-runs-route tip))
                            (json-text (check-runs-json
                                        (list (check-run-json #:sha tip
                                                              #:status "in_progress"
                                                              #:conclusion 'null))))))
           (lambda (world root log) (ci-run world 3 (hash-ref world 'bindtip)))
           "pending, failed, or untrusted check: ci")
     (job* "binding-ci refuses an untrusted check and a missing check"
           (lambda (world)
             (define tip (hash-ref world 'bindtip))
             (fixtures-with (ci-fixtures world)
                            (route slug (check-runs-route tip))
                            (json-text (check-runs-json
                                        (list (check-run-json #:sha tip #:app-id 42))))))
           (lambda (world root log)
             (define tip (hash-ref world 'bindtip))
             (define untrusted (probe (lambda () (ci-run world 3 tip))))
             (set-fixture! root
                           (route slug (check-runs-route tip))
                           (json-text (check-runs-json '())))
             (define missing (probe (lambda () (ci-run world 3 tip))))
             (hasheq 'untrusted untrusted 'missing missing))
           'value
           (lambda (facts label)
             (define bundle (ok-value facts))
             (check-equal? (hash-ref bundle 'untrusted)
                           (list 'reason "pending, failed, or untrusted check: ci")
                           label)
             (check-equal? (hash-ref bundle 'missing) (list 'reason "missing check: ci") label)))
     (job* "binding-ci refuses a closed binding PR"
           (lambda (world) (ci-fixtures world #:state "closed"))
           (lambda (world root log) (ci-run world 3 (hash-ref world 'bindtip)))
           "binding pull request is not open")
     (job* "binding-ci refuses a head that differs from the expected head"
           ci-fixtures
           (lambda (world root log) (ci-run world 3 (make-string 40 #\b)))
           "binding pull request head does not match the verified expected head")
     ;; ------------------------------------------------------------
     ;; binding-merged (the unchanged protected merge gate)
     ;; ------------------------------------------------------------
     (job* "binding-merged runs the protected squash merge exactly once"
           merged-fixtures
           (lambda (world root log) (merged-run world 1 (hash-ref world 'head)))
           'value
           (lambda (facts label)
             (define world (shared-world))
             (define records (hash-ref facts 'records))
             (check-hashes-equal? (ok-value facts)
                                  (hasheq 'status
                                          "merged"
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
                                  label)
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
             (define put-at
               (position-of records
                            (lambda (record)
                              (and (equal? (car record) "gh")
                                   (equal? (argv-method (cdr record)) "PUT")))
                            "PUT"))
             (define pulls-at
               (for/list ([record (in-list records)]
                          [index (in-naturals)]
                          #:when (gh-read? (route slug "pulls/1") record))
                 index))
             (check-equal? (length pulls-at) 2 "one stage-1 read plus one fresh proof read")
             (check-true (< (first pulls-at) put-at) "the stage-1 read precedes the PUT")
             (check-true (< put-at (second pulls-at)) "the post-PUT proof read follows the PUT")))
     (job* "binding-merged is idempotent for an already merged binding PR"
           (lambda (world)
             (define head (hash-ref world 'head))
             (define base (hash-ref world 'base))
             (list (cons (route slug "pulls/1")
                         (json-text (pr-json #:number 1
                                             #:state "closed"
                                             #:merged #t
                                             #:merge-sha api-merge-sha
                                             #:head-sha head
                                             #:base-sha base)))
                   (cons (route slug (format "commits/~a" api-merge-sha))
                         (json-text (commit-json api-merge-sha (list base))))
                   (cons (route slug "branches/main/protection")
                         (json-text (protection-json)))
                   (cons (route slug (check-runs-route head))
                         (json-text (check-runs-json (list (check-run-json #:sha head)))))
                   (cons (route slug "actions/runs/1001")
                         (json-text (workflow-run-json #:sha head #:branch wave-branch)))))
           (lambda (world root log) (merged-run world 1 (hash-ref world 'head)))
           'value
           (lambda (facts label)
             (define world (shared-world))
             (check-equal? (hash-ref (ok-value facts) 'status) "already-merged" label)
             (check-equal? (hash-ref (ok-value facts) 'merge-sha) api-merge-sha label)
             (check-equal? (hash-ref (ok-value facts) 'head) (hash-ref world 'head) label)
             (check-equal? (mutations (hash-ref facts 'records))
                           '()
                           "an already merged PR is never merged twice")))
     (job* "binding-merged refuses a non-squash (multi-parent) merge commit"
           (lambda (world)
             (define head (hash-ref world 'head))
             (define base (hash-ref world 'base))
             (fixtures-with (merged-fixtures world)
                            (route slug (format "commits/~a" api-merge-sha))
                            (json-text (commit-json api-merge-sha (list base base)))))
           (lambda (world root log) (merged-run world 1 (hash-ref world 'head)))
           (list 'mutating "delivery must have single-parent squash provenance"))
     (job* "binding-merged refuses a closed and unmerged implementation PR"
           (lambda (world)
             (define head (hash-ref world 'head))
             (list (cons (route slug "pulls/1")
                         (json-text (pr-json #:number 1
                                             #:state "closed"
                                             #:head-sha head
                                             #:base-sha (hash-ref world 'base))))))
           (lambda (world root log) (merged-run world 1 (hash-ref world 'head)))
           "implementation PR is neither open nor already merged")
     ;; ------------------------------------------------------------
     ;; binding-pr (binding_publish): the ONE non-idempotent POST
     ;; ------------------------------------------------------------
     (job* "binding-pr publishes once and reuses the PR on resume"
           pr-fixtures
           (lambda (world root log)
             (define repo (hash-ref world 'subject))
             (define staging (stage-finalized! world))
             (define first
               (probe (lambda ()
                        (delivery-binding-pr! repo
                                              plan-id
                                              wave
                                              staging
                                              #:campaign-root (hash-ref world 'campaign)))))
             (define tip (rev-parse repo (format "refs/remotes/origin/~a" binding-name)))
             (define show (show-commit repo tip))
             (define second
               (probe (lambda ()
                        (delivery-binding-pr! repo
                                              plan-id
                                              wave
                                              staging
                                              #:campaign-root (hash-ref world 'campaign)))))
             (hasheq 'first first 'second second 'tip tip 'show show))
           'value
           (lambda (facts label)
             (define bundle (ok-value facts))
             (define records (hash-ref facts 'records))
             (define first (hash-ref bundle 'first))
             (define second (hash-ref bundle 'second))
             (define tip (hash-ref bundle 'tip))
             (check-equal? (car first) 'value (format "~a first run: ~s" label first))
             (check-equal? (car second) 'value (format "~a resume: ~s" label second))
             (check-hashes-equal? (cadr first)
                                  (hasheq 'status "opened" 'pr 2 'branch binding-name 'head tip)
                                  label)
             (check-hashes-equal? (cadr second)
                                  (hasheq 'status "exists" 'pr 2 'branch binding-name 'head tip)
                                  label)
             ;; resolve-before-create: ONE POST across both runs.
             (check-equal? (mutations records)
                           (list (list "api"
                                       "--hostname"
                                       "github.com"
                                       "-X"
                                       "POST"
                                       (route slug "pulls")
                                       "-f"
                                       "base=main"
                                       "-f"
                                       (format "body=~a"
                                               (binding-publish-body plan-id
                                                                     wave
                                                                     tip
                                                                     binding-name
                                                                     evidence-relative))
                                       "-f"
                                       (format "head=~a" binding-name)
                                       "-f"
                                       (format "title=~a"
                                               (binding-publish-title plan-id wave binding-name))))
                           "the binding PR POST is Python's exact argv, attempted once")
             ;; Order: the branch push precedes the POST; the POST precedes the
             ;; refreshed re-read of the created PR.
             (define push-at
               (position-of records
                            (lambda (record)
                              (and (equal? (car record) "git")
                                   (member "push" (cdr record))
                                   #t))
                            "push"))
             (define post-at
               (position-of records
                            (lambda (record)
                              (and (equal? (car record) "gh")
                                   (equal? (argv-method (cdr record)) "POST")))
                            "POST"))
             (check-true (< push-at post-at) "the publication push precedes the PR creation")
             (check-true (< post-at (position-of records
                                                 (lambda (record)
                                                   (gh-read? (route slug "pulls/2") record))
                                                 "pulls/2"))
                         "the created PR is re-read after the POST")
             ;; The publication commit is Python's exact subject and touches
             ;; EXACTLY the three staged evidence artifacts.
             (define lines
               (for/list ([line (in-list (string-split (hash-ref bundle 'show) "\n"))]
                          #:unless (string=? (string-trim line) ""))
                 (string-trim line)))
             (check-equal? (car lines)
                           (format "W~a binding draft: ~a-w~a" wave plan-id wave)
                           label)
             (check-equal? (sort (cdr lines) string<?)
                           (sort (list review-relative validation-relative evidence-relative)
                                 string<?)
                           "the binding publication commit is evidence-only")))
     (job* "binding-pr refuses a re-read PR whose head does not match the publication"
           (lambda (world)
             (pr-fixtures world #:head-ref "campaign/other"))
           (lambda (world root log)
             (delivery-binding-pr! (hash-ref world 'subject)
                                   plan-id
                                   wave
                                   (stage-finalized! world)
                                   #:campaign-root (hash-ref world 'campaign)))
           (list 'mutating
                 "binding pull request does not match the verified publication branch/head"))))

  (test-case "every binding action reports its declared outcome"
    (check-true (>= (length jobs) 16) "the job table must cover the whole slice")
    (check-jobs! jobs)))
