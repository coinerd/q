#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers scripts/gsd-delivery-pr.rkt
;; @covers scripts/gsd-delivery-api.rkt
;; @covers scripts/gsd-delivery-git.rkt
;; @timeout 420
;; Phase B slice S4: the READ-ONLY delivery ladder actions of
;; scripts/gsd-delivery.py (`resolve-pr`, `pr-ci`, `review`) and the helpers
;; they share (resolve_pr, resolve_merged_pr, resolve_existing_pr,
;; resolve_prs_for_branch, branch_owner, validate_pr_identity,
;; require_receipt_tip, policy_names, blob_or_none, refresh, repository,
;; EXCLUDED_PREFIXES) are ported to Racket (scripts/gsd-delivery-pr.rkt). The
;; module is NOT wired into the coordinator by this slice and mutates nothing.
;;
;; Parity law under test: every refusal is a typed exn:fail:delivery-action
;; carrying the EXACT Python reason string, and every positive path returns the
;; same JSON shape. A fake `gh` executable is installed FIRST on PATH (S3
;; pattern) and a real local git origin/subject pair backs every git read, so
;; `refresh`, `fetch_head`, `merge-base` and the policy blob read are REAL
;; commands: deleting a fetch or a rev-parse cannot hide behind a stub. That
;; git fixture is built ONCE and shared read-only across cases (the actions
;; below never mutate origin); each case still gets its OWN gh shim, fixture
;; directory and invocation log, so every spawn is attributable per case.
;;
;; The transport seam is honest: the subject's CONFIGURED origin URL is
;; https://github.com/<slug>, while `url.<origin>.insteadOf` maps it to the
;; local bare origin. That is exactly the distinction repository() must
;; respect — the configured URL defines identity, the rewrite only transports.
;;
;; Negative fixtures kill one guard at a time: a closed PR served for the open
;; query (unexpected state), a closed-but-NOT-merged PR served for the all-state
;; resume query and for pr-ci's expected-head path, a PR without a number, a
;; short head SHA, a foreign head ref, a fork head repo, a PR ref that does not
;; match the claimed head, a non-ancestor receipt head, a non-evidence drift
;; commit after the receipt, a look-alike `docs/reports/gsd-wave-evidence-x/`
;; drift path (prefix widening), five foreign drift paths (the refusal must name
;; exactly three), a malformed/duplicated/empty/blank-named policy datum, a
;; non-list pulls payload, a JSON-null candidate, a malformed PR hash, a
;; non-GitHub origin URL, and pr-ci pending/failed/missing/untrusted/
;; closed-not-merged checks.
;;
;; The REVIEW ladder is exercised end to end, never stubbed: a REAL schema-2
;; trio is committed at the wave tip of `campaign/w3`/`w4`/`w5` and the
;; unchanged strict gate (scripts/gsd-wave-gate.rkt, spawned as a subprocess)
;; really runs over materialized blobs and the real excluded-path digest. That
;; is what pins `awaiting-review`, `reviewed`, the weaker reviewed-sha clause
;; and its refusals.
;;
;; Differential parity: every case's Python outcome (success payload or refusal
;; message) was captured from scripts/gsd-delivery.py at its final revision into
;; tests/fixtures/gsd-delivery-goldens.json before the controller was deleted
;; (Phase B), driven through the SAME gh fixtures against the SAME git subject;
;; every case declares its OWN expected outcome ('ok or the exact reason), so a
;; fixture drift can never let two identical transport failures masquerade as
;; parity. Git object names and the temp subject path are tokenized in the
;; recording and substituted back from the live fixtures, and the golden loader
;; pins the case count, the label set and each case's input text, so a stale row
;; can never answer a different case. No interpreter is needed on any host.

(require rackunit
         json
         openssl
         file/sha1
         racket/file
         racket/list
         racket/path
         racket/pretty
         racket/runtime-path
         racket/string
         "../sandbox/subprocess.rkt"
         "../scripts/gsd-delivery-git.rkt"
         "../scripts/gsd-delivery-api.rkt"
         "../scripts/gsd-delivery-pr.rkt"
         "helpers/gsd-delivery-goldens.rkt")

(define-runtime-path read-ladder-module "../scripts/gsd-delivery-pr.rkt")

;; The module under test spawns `racket` for the binding-data helper and the
;; strict wave gate, and the CLI case spawns this same interpreter. Resolve it
;; once under the runner's OWN environment and put its directory on the fixture
;; PATH (after the shims, so `gh`/`git` stay the shims).
(define racket-executable (or (find-executable-path "racket") "racket"))
(define racket-directory
  (path->string (path-only (path->complete-path racket-executable))))
(define (fixture-path bin)
  (format "~a:~a:/usr/bin:/bin" bin racket-directory))

(define slug "coinerd/q")
(define plan-id (make-string 64 #\a))
(define branch "campaign/w1")

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

;; `.rktd` artifacts are ONE Racket hash datum; the trusted writer
;; (scripts/gsd-binding-data.rkt write-datum) pretty-writes a JSON hash, so a
;; fixture can commit byte-shaped equivalents without a subprocess round trip.
(define (put-datum path datum)
  (make-directory* (path-only path))
  (call-with-output-file path #:exists 'truncate (lambda (out) (pretty-write datum out))))

(define (git dir . args)
  (define result
    (run-subprocess "git"
                    #:args args
                    #:directory dir
                    #:environment (current-environment-variables)
                    #:timeout 30
                    #:process-group? #t))
  (unless (and (zero? (subprocess-result-exit-code result))
               (not (subprocess-result-timed-out? result)))
    (error 'fixture-git "failed ~s: ~a" args (subprocess-result-stderr result)))
  (string-trim (subprocess-result-stdout result)))

;; Same seam as `git`, but stdout is returned UNTRIMMED: a content digest
;; hashes the exact bytes git printed, trailing newline included.
(define (git-raw dir . args)
  (define result
    (run-subprocess "git"
                    #:args args
                    #:directory dir
                    #:environment (current-environment-variables)
                    #:timeout 30
                    #:process-group? #t))
  (unless (and (zero? (subprocess-result-exit-code result))
               (not (subprocess-result-timed-out? result)))
    (error 'fixture-git "failed ~s: ~a" args (subprocess-result-stderr result)))
  (subprocess-result-stdout result))

;; The fake `gh`: logs the argument vector, recovers the route argument and
;; prints the fixture. `case` patterns are DOUBLE-QUOTED so `?`/`&` stay literal.
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

(define (route slug* path)
  (format "repos/~a/~a" slug* path))

;; The per-case shim invocation log, from the shim's own environment variable.
(define (shim-log name)
  (define log (getenv name))
  (if (and log (file-exists? log))
      (file->lines log)
      '()))

(define (gh-log) (shim-log "GH_SHIM_LOG"))

(define (git-log) (shim-log "GIT_SHIM_LOG"))

;; ============================================================
;; Real schema-2 trio fixtures (the REVIEW ladder)
;; ============================================================

;; Byte-identical to the module's EXCLUDED_PREFIXES as git pathspec magic.
(define trio-exclude-pathspecs
  (list ":(exclude)docs/reports/gsd-wave-evidence/**"
        ":(exclude)docs/reports/gsd-wave-reviews/**"
        ":(exclude)docs/reports/gsd-wave-validation/**"))

;; Python digest(): sha256 over `git diff --binary base...head -- . <excludes>`.
;; The trio lives under the excluded directories, so this digest is the same
;; before and after the trio commit: the gate has to recompute it for real.
(define (excluded-content-digest dir base head)
  (bytes->hex-string
   (sha256-bytes
    (string->bytes/utf-8
     (apply git-raw
            dir
            (append (list "diff" "--binary" (format "~a...~a" base head) "--" ".")
                    trio-exclude-pathspecs))))))

;; One schema-2 evidence/review/validation triple that the UNCHANGED strict gate
;; accepts: status ready-for-merge, unique required checks matching the policy
;; at the ref, an APPROVED review bound to implementation-sha AND the excluded
;; content digest, and a complete validation record.
(define (trio-datums digest implementation-sha [branch* "campaign/w3"])
  (define review-relative (format "docs/reports/gsd-wave-reviews/~a-w1.rktd" plan-id))
  (define validation-relative (format "docs/reports/gsd-wave-validation/~a-w1.rktd" plan-id))
  (define evidence
    (hasheq 'schema-version
            2
            'milestone
            1
            'wave
            "W1"
            'issue
            7
            'status
            "ready-for-merge"
            'implementation-sha
            implementation-sha
            'content-digest
            digest
            'required-checks
            '("ci")
            'review-artifact
            review-relative
            'validation-artifact
            validation-relative))
  (define review
    (hasheq 'reviewer
            "independent-reviewer"
            'verdict
            "APPROVED"
            'timestamp
            "2026-01-01T00:00:00Z"
            'reviewed-sha
            implementation-sha
            'content-digest
            digest
            'scope
            "Independent review of the wave diff, the excluded-content digest and the strict gate"
            'report
            "The reviewer read the diff, recomputed the excluded digest and reran the strict trio gate."))
  (define validation
    (hasheq 'status
            "current"
            'milestone
            1
            'wave
            "W1"
            'issue
            7
            'branch
            branch*
            'implementation-sha
            implementation-sha
            'content-digest
            digest
            'red-first
            (hasheq 'command "racket tests/test-fixture-gate.rkt"
                    'failure "the fixture gate failed before the implementation landed")
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
  (values evidence review validation))

;; The test repository: a bare origin, a work checkout that publishes commits,
;; and a subject clone. Every ref a ladder action reads is published.
(define (build-repo-world root)
  (define origin (build-path root "origin.git"))
  (define work (build-path root "work"))
  (define subject (build-path root "subject"))
  (git root "init" "--bare" "-b" "main" (path->string origin))
  (git root "init" "-b" "main" (path->string work))
  (put (build-path work "tracked") "initial\n")
  (git work "add" "tracked")
  (git work "commit" "-m" "initial")
  (define pre-policy (git work "rev-parse" "HEAD"))
  (put (build-path work "scripts/required-pr-checks.policy") "(\"ci\")\n")
  (git work "add" "scripts/required-pr-checks.policy")
  (git work "commit" "-m" "policy")
  (define main (git work "rev-parse" "HEAD"))
  (git work "remote" "add" "origin" (path->string origin))
  (git work "push" "origin" "main")
  ;; campaign/w1: one implementation commit after main.
  (git work "checkout" "-b" branch main)
  (put (build-path work "feature") "impl\n")
  (git work "add" "feature")
  (git work "commit" "-m" "impl")
  (define head (git work "rev-parse" "HEAD"))
  (git work "push" "origin" branch)
  (git work "push" "origin" (format "~a:refs/pull/1/head" branch))
  ;; campaign/w2: a durable receipt head plus one evidence-only commit.
  (git work "checkout" "-b" "campaign/w2" main)
  (put (build-path work "impl-two") "impl two\n")
  (git work "add" "impl-two")
  (git work "commit" "-m" "impl two")
  (define receipt (git work "rev-parse" "HEAD"))
  (put (build-path work "docs/reports/gsd-wave-evidence" (format "~a-w0.rktd" plan-id)) "{}\n")
  (git work "add" "docs/reports/gsd-wave-evidence")
  (git work "commit" "-m" "evidence")
  (define tip (git work "rev-parse" "HEAD"))
  (git work "push" "origin" "campaign/w2")
  (git work "push" "origin" "campaign/w2:refs/pull/2/head")
  (git work "push" "origin" "campaign/w2:refs/pull/3/head")
  ;; campaign/drift: a NON-evidence commit after the receipt head.
  (git work "checkout" "-b" "campaign/drift" receipt)
  (put (build-path work "src-drift") "drift\n")
  (git work "add" "src-drift")
  (git work "commit" "-m" "drift")
  (define drift (git work "rev-parse" "HEAD"))
  (git work "push" "origin" "campaign/drift")
  (git work "push" "origin" "campaign/drift:refs/pull/4/head")
  ;; campaign/bad-policy: a duplicated, hence malformed, policy snapshot.
  (git work "checkout" "-b" "campaign/bad-policy" main)
  (put (build-path work "scripts/required-pr-checks.policy") "(\"ci\" \"ci\")\n")
  (git work "add" "scripts/required-pr-checks.policy")
  (git work "commit" "-m" "bad policy")
  (define bad-policy (git work "rev-parse" "HEAD"))
  (git work "push" "origin" "campaign/bad-policy")
  ;; campaign/empty-policy and campaign/blank-policy: the pair? and non-blank
  ;; guards of the required-check policy reader.
  (git work "checkout" "-b" "campaign/empty-policy" main)
  (put (build-path work "scripts/required-pr-checks.policy") "()\n")
  (git work "add" "scripts/required-pr-checks.policy")
  (git work "commit" "-m" "empty policy")
  (define empty-policy (git work "rev-parse" "HEAD"))
  (git work "push" "origin" "campaign/empty-policy")
  (git work "checkout" "-b" "campaign/blank-policy" main)
  (put (build-path work "scripts/required-pr-checks.policy") "(\"ci\" \"   \")\n")
  (git work "add" "scripts/required-pr-checks.policy")
  (git work "commit" "-m" "blank policy name")
  (define blank-policy (git work "rev-parse" "HEAD"))
  (git work "push" "origin" "campaign/blank-policy")
  ;; campaign/drift-x: a path that merely SHARES the excluded prefix stem. It is
  ;; not under `docs/reports/gsd-wave-evidence/`, so a widened prefix accepts it.
  (git work "checkout" "-b" "campaign/drift-x" receipt)
  (put (build-path work "docs/reports/gsd-wave-evidence-x/foo") "widened\n")
  (git work "add" "docs/reports/gsd-wave-evidence-x/foo")
  (git work "commit" "-m" "prefix widening drift")
  (define drift-x (git work "rev-parse" "HEAD"))
  (git work "push" "origin" "campaign/drift-x")
  ;; campaign/drift-many: FIVE foreign paths, so the refusal must name exactly
  ;; three of them (git sorts --name-only output, so the first three are stable).
  (git work "checkout" "-b" "campaign/drift-many" receipt)
  (define drift-many-names
    '("src-drift-a" "src-drift-b" "src-drift-c" "src-drift-d" "src-drift-e"))
  (for ([name (in-list drift-many-names)])
    (put (build-path work name) (string-append name "\n")))
  (apply git work "add" drift-many-names)
  (git work "commit" "-m" "many drift paths")
  (define drift-many (git work "rev-parse" "HEAD"))
  (git work "push" "origin" "campaign/drift-many")
  ;; campaign/w3: a REAL schema-2 trio at the wave tip (the review green path).
  (git work "checkout" "-b" "campaign/w3" main)
  (put (build-path work "impl-three") "impl three\n")
  (git work "add" "impl-three")
  (git work "commit" "-m" "impl three")
  (define review-ancestor (git work "rev-parse" "HEAD"))
  (define review-digest (excluded-content-digest work main review-ancestor))
  (define (commit-trio! implementation-sha message)
    (define-values (evidence review validation) (trio-datums review-digest implementation-sha))
    (define relative (format "docs/reports/gsd-wave-evidence/~a-w1.rktd" plan-id))
    (put-datum (build-path work relative) evidence)
    (put-datum (build-path work (hash-ref evidence 'review-artifact)) review)
    (put-datum (build-path work (hash-ref evidence 'validation-artifact)) validation)
    (git work
         "add"
         relative
         (hash-ref evidence 'review-artifact)
         (hash-ref evidence 'validation-artifact))
    (git work "commit" "-m" message)
    (git work "rev-parse" "HEAD"))
  (define review-tip (commit-trio! review-ancestor "schema-2 trio"))
  (git work "push" "origin" "campaign/w3")
  ;; campaign/w4: the same trio, but reviewed-sha (and implementation-sha) name
  ;; the FOREIGN campaign/drift tip, which is not an ancestor of the receipt
  ;; head. The trio paths are excluded from the digest, so it is unchanged.
  (git work "checkout" "-b" "campaign/w4" review-tip)
  (define review-foreign-tip (commit-trio! drift "foreign reviewed-sha"))
  (git work "push" "origin" "campaign/w4")
  ;; campaign/w5: the trio is present but the review artifact is absent.
  (git work "checkout" "-b" "campaign/w5" review-tip)
  (git work "rm" (format "docs/reports/gsd-wave-reviews/~a-w1.rktd" plan-id))
  (git work "commit" "-m" "drop review artifact")
  (define review-absent-tip (git work "rev-parse" "HEAD"))
  (git work "push" "origin" "campaign/w5")
  (git root "clone" (path->string origin) (path->string subject))
  (git subject "config" "user.name" "Test")
  (git subject "config" "user.email" "test@example.invalid")
  (hasheq 'root
          root
          'work
          work
          'origin
          origin
          'subject
          subject
          'pre-policy
          pre-policy
          'main
          main
          'head
          head
          'receipt
          receipt
          'tip
          tip
          'drift
          drift
          'bad-policy
          bad-policy
          'empty-policy
          empty-policy
          'blank-policy
          blank-policy
          'drift-x
          drift-x
          'drift-many
          drift-many
          'drift-many-names
          drift-many-names
          'review-ancestor
          review-ancestor
          'review-digest
          review-digest
          'review-tip
          review-tip
          'review-foreign-tip
          review-foreign-tip
          'review-absent-tip
          review-absent-tip
          'plan
          plan-id))

;; Point the subject's CONFIGURED origin URL at a GitHub slug while the
;; insteadOf rewrite keeps every git transport local.
(define (point-at! world slug*)
  (define subject (hash-ref world 'subject))
  (define origin (hash-ref world 'origin))
  (define url (format "https://github.com/~a" slug*))
  (git subject "config" "remote.origin.url" url)
  (git subject "config" (format "url.~a.insteadOf" (path->string origin)) url))

;; The git fixture is IMMUTABLE for the whole run (every action below is
;; read-only against origin), so it is built ONCE instead of per test-case: a
;; repo build costs ~25 bounded subprocesses, and rebuilding it for every case
;; dominated the suite wall clock. The single temp root is left to the OS temp
;; cleaner (one small directory per run, never inside the checkout).
(define repo-world-box (box #f))

(define (shared-repo-world)
  (or (unbox repo-world-box)
      (let* ([root (make-temporary-file "native-ladder-repo-~a" 'directory)]
             [env (fixture-env root "/usr/bin:/bin")])
        (define world
          (parameterize ([current-environment-variables env])
            (check-not-false (find-executable-path "git") "the fixture must resolve git through PATH")
            (build-repo-world root)))
        (set-box! repo-world-box world)
        world)))

;; fixtures: list of (list route-string response-body), or a procedure of the
;; built world returning that list. Each case gets its OWN gh shim (fixtures
;; and invocation log) over the ONE shared git fixture.
(define (with-ladder-world fixtures slug* proc)
  (define world (shared-repo-world))
  (define root (make-temporary-file "native-ladder-gh-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (define bin (build-path root "bin"))
                  (define fixtures-dir (build-path root "fixtures"))
                  (make-directory* bin)
                  (make-directory* fixtures-dir)
                  (define installed
                    (if (procedure? fixtures)
                        (fixtures world)
                        fixtures))
                  (define routes
                    (for/list ([entry (in-list installed)]
                               [index (in-naturals)])
                      (define file (format "fixture-~a.json" index))
                      (put (build-path fixtures-dir file) (cadr entry))
                      (list (car entry) file)))
                  (install-gh-shim! root routes)
                  (define env (fixture-env root (fixture-path bin)))
                  (environment-set! env "GH_SHIM_LOG" (path->string (build-path root "gh.log")))
                  (environment-set! env "GH_SHIM_FIXTURES" (path->string fixtures-dir))
                  (parameterize ([current-environment-variables env])
                    (check-not-false (find-executable-path "gh")
                                     "the fixture must resolve gh through PATH")
                    (when slug*
                      (point-at! world slug*))
                    (clear-api-cache!)
                    (proc world)))
                (lambda () (delete-directory/files root))))

;; A fake `git` only world (the executable under test is the shim).
(define (with-git-shim-world script proc)
  (define root (make-temporary-file "native-ladder-git-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (define bin (build-path root "bin"))
                  (define subject (build-path root "subject"))
                  (make-directory* bin)
                  (make-directory* subject)
                  (define shim (build-path bin "git"))
                  (put shim (string-append script "\n"))
                  (file-or-directory-permissions shim #o700)
                  (define env (fixture-env root (fixture-path bin)))
                  (environment-set! env "GIT_SHIM_LOG" (path->string (build-path root "git.log")))
                  (parameterize ([current-environment-variables env])
                    (check-not-false (find-executable-path "git")
                                     "the fixture must resolve git through PATH")
                    (check-equal? (path->complete-path (find-executable-path "git"))
                                  (path->complete-path shim)
                                  "the fixture must resolve git to the shim")
                    (clear-api-cache!)
                    (proc subject)))
                (lambda () (delete-directory/files root))))

;; PATH world with BOTH shims logging: `git` answers the repository identity
;; and `gh` records any read. Used to prove the action's git side draws on the
;; SAME budget as the gh side (a fresh runner would execute git first).
(define (with-budget-shims proc)
  (define root (make-temporary-file "native-ladder-budget-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define bin (build-path root "bin"))
     (make-directory* bin)
     (define git-shim (build-path bin "git"))
     (put git-shim
          (string-append "#!/bin/sh"
                         "\nprintf '%s\\n' \"$*\" >> \"$GIT_SHIM_LOG\""
                         "\ncase \"$*\" in"
                         "\n  *'config --get remote.origin.url'*)"
                         " printf '%s\\n' 'https://github.com/coinerd/q' ;;"
                         "\nesac"
                         "\nexit 0"))
     (file-or-directory-permissions git-shim #o700)
     (define gh-shim (build-path bin "gh"))
     (put gh-shim
          (string-append "#!/bin/sh"
                         "\nprintf '%s\\n' \"$*\" >> \"$GH_SHIM_LOG\""
                         "\nexit 0"))
     (file-or-directory-permissions gh-shim #o700)
     (define env (fixture-env root (fixture-path bin)))
     (environment-set! env "GIT_SHIM_LOG" (path->string (build-path root "git.log")))
     (environment-set! env "GH_SHIM_LOG" (path->string (build-path root "gh.log")))
     (parameterize ([current-environment-variables env])
       (check-not-false (find-executable-path "git") "the fixture must resolve git through PATH")
       (check-not-false (find-executable-path "gh") "the fixture must resolve gh through PATH")
       (clear-api-cache!)
       (proc root)))
   (lambda () (delete-directory/files root))))

;; ============================================================
;; JSON fixture constructors
;; ============================================================

(define (pr-json #:number [number 1]
                 #:state [state "open"]
                 #:head-sha [head-sha (make-string 40 #\d)]
                 #:head-ref [head-ref branch]
                 #:merged [merged 'null]
                 #:merged-at [merged-at 'null]
                 #:base-ref [base-ref "main"]
                 #:base-repo [base-repo slug]
                 #:head-repo [head-repo slug]
                 #:base-sha [base-sha (make-string 40 #\c)])
  (hasheq 'number
          number
          'state
          state
          'merged
          merged
          'merged_at
          merged-at
          'merge_commit_sha
          (make-string 40 #\b)
          'head
          (hasheq 'sha head-sha 'ref head-ref 'repo (hasheq 'full_name head-repo))
          'base
          (hasheq 'ref base-ref 'sha base-sha 'repo (hasheq 'full_name base-repo))))

(define (merged-pr-json #:number [number 3]
                        #:head-sha [head-sha (make-string 40 #\d)]
                        #:head-ref [head-ref "campaign/w2"]
                        #:head-repo [head-repo slug]
                        #:base-repo [base-repo slug])
  (pr-json #:number number
           #:state "closed"
           #:head-sha head-sha
           #:head-ref head-ref
           #:head-repo head-repo
           #:base-repo base-repo
           #:merged #t
           #:merged-at "2026-01-01T00:00:00Z"))

(define (checks-page runs total)
  (jsexpr->string (hasheq 'total_count total 'check_runs runs)))

(define (check-run #:id [id 42]
                   #:name [name "ci"]
                   #:status [status "completed"]
                   #:conclusion [conclusion "success"]
                   #:head [head* (make-string 40 #\d)]
                   #:app [app 15368]
                   #:details [details #f]
                   #:slug [slug* slug])
  (hasheq 'id
          id
          'name
          name
          'status
          status
          'conclusion
          conclusion
          'head_sha
          head*
          'app
          (hasheq 'id app)
          'details_url
          (or details (format "https://github.com/~a/actions/runs/999/job/123" slug*))))

(define (actions-run #:status [status "completed"]
                     #:conclusion [conclusion "success"]
                     #:head [head* (make-string 40 #\d)]
                     #:event [event "pull_request"]
                     #:branch [branch* branch]
                     #:path [path ".github/workflows/ci.yml"]
                     #:repo [repo slug])
  (hasheq 'status
          status
          'conclusion
          conclusion
          'head_sha
          head*
          'event
          event
          'head_branch
          branch*
          'path
          path
          'repository
          (hasheq 'full_name repo)))

(define (protection-json #:contexts [contexts '("ci")])
  (jsexpr->string (hasheq 'enforce_admins
                          (hasheq 'enabled #t)
                          'required_status_checks
                          (hasheq 'strict #t 'contexts contexts)
                          'allow_force_pushes
                          (hasheq 'enabled #f)
                          'allow_deletions
                          (hasheq 'enabled #f))))

;; ============================================================
;; Refusal helpers
;; ============================================================

(define (delivery-refusal thunk)
  (with-handlers ([exn:fail:delivery-action? exn-message])
    (thunk)
    (fail "expected a typed exn:fail:delivery-action refusal, but the call returned")))

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

(define (json-value value)
  (cond
    [(void? value) 'null]
    [(eq? value #f) 'null]
    [else value]))

;; ============================================================
;; The recorded oracle answers
;; ============================================================
(define parity-group "gsd-delivery-read-ladder")

(module+ test
  ;; ============================================================
  ;; repository / refresh / branch_owner
  ;; ============================================================

  (test-case "repository reads the configured origin URL, not the transport rewrite"
    (with-ladder-world '()
                       slug
                       (lambda (world)
                         (define ctx (make-delivery-pr-context (hash-ref world 'subject)))
                         (check-equal? ((delivery-pr-context-git ctx) (list "config"
                                                                            "remote.origin.url"))
                                       (format "https://github.com/~a\n" slug))
                         (check-equal? (repository (delivery-pr-context-git ctx)) slug))))

  (test-case "repository refuses a non-GitHub origin URL and accepts only GitHub forms"
    (with-ladder-world
     '()
     #f
     (lambda (world)
       (define subject (hash-ref world 'subject))
       (define (repository-of url)
         (git subject "config" "remote.origin.url" url)
         (repository (delivery-pr-context-git (make-delivery-pr-context subject))))
       (for ([url (in-list '("https://gitlab.com/coinerd/q"
                             "https://github.example.com/coinerd/q"
                             "ssh://git@github.com/coinerd/q"
                             "coinerd/q"))])
         (check-equal? (delivery-refusal (lambda () (repository-of url)))
                       "origin must identify a GitHub repository (no arbitrary API host)"
                       url))
       (for ([url (in-list '("https://github.com/coinerd/q"
                             "https://github.com/coinerd/q.git"
                             "git@github.com:coinerd/q"
                             "git@github.com:coinerd/q.git"))])
         (check-equal? (repository-of url) slug url))
       ;; Restore the fixture identity for later cases (and the insteadOf map).
       (point-at! world slug)
       (void))))

  (test-case "branch_owner refuses a slug that is not exactly owner/name"
    (check-equal? (branch-owner "coinerd/q") "coinerd")
    (for ([bad (in-list '("coinerd" "coinerd/" "/q" "a/b/c" ""))])
      (check-equal? (delivery-refusal (lambda () (branch-owner bad)))
                    (string-append "malformed repository slug: " bad)
                    bad)))

  ;; ============================================================
  ;; resolve-pr
  ;; ============================================================

  (test-case "resolve-pr returns the single open PR for the durable branch (owner:branch filter)"
    (define head-sha (make-string 40 #\d))
    (with-ladder-world
     (lambda (world)
       (list (list (format "repos/~a/pulls?state=open&head=coinerd:~a" slug branch)
                   (jsexpr->string (list (pr-json #:number 7 #:head-sha head-sha))))))
     slug
     (lambda (world)
       (define result (delivery-resolve-pr! (hash-ref world 'subject) plan-id 0 branch))
       (check-equal?
        result
        (hasheq 'status "resolved" 'pr 7 'plan-id plan-id 'wave 0 'branch branch 'head head-sha))
       ;; The query really used GitHub's `owner:branch` head filter and one
       ;; literal argv element (no shell split on `?`/`&`).
       (check-equal?
        (gh-log)
        (list (format "api --hostname github.com repos/~a/pulls?state=open&head=coinerd:~a"
                      slug
                      branch))))))

  (test-case "resolve-pr reports none when no candidate exists"
    (with-ladder-world
     (lambda (_world)
       (list (list (format "repos/~a/pulls?state=open&head=coinerd:campaign/none" slug) "[]")))
     slug
     (lambda (world)
       (define result (delivery-resolve-pr! (hash-ref world 'subject) plan-id 2 "campaign/none"))
       (check-equal? result (hasheq 'status "none" 'plan-id plan-id 'wave 2 'branch "campaign/none"))
       (check-false (hash-has-key? result 'pr)))))

  (test-case "resolve-pr refuses multiple candidates instead of silently reusing one"
    (with-ladder-world
     (lambda (world)
       (list (list (format "repos/~a/pulls?state=open&head=coinerd:campaign/w9" slug)
                   (jsexpr->string (list (pr-json #:number 1 #:head-sha (hash-ref world 'head))
                                         (pr-json #:number 2 #:head-sha (hash-ref world 'head)))))))
     slug
     (lambda (world)
       (check-equal?
        (delivery-refusal (lambda ()
                            (delivery-resolve-pr! (hash-ref world 'subject) plan-id 0 "campaign/w9")))
        "multiple pull requests already target branch campaign/w9"))))

  (test-case "resolve-pr resumes exactly one closed merged PR at the durable receipt head"
    (with-ladder-world
     (lambda (world)
       (list (list (format "repos/~a/pulls?state=open&head=coinerd:campaign/w2" slug) "[]")
             (list (format "repos/~a/pulls?state=all&head=coinerd:campaign/w2" slug)
                   (jsexpr->string (list (merged-pr-json #:number 3
                                                         #:head-sha (hash-ref world 'tip)))))))
     slug
     (lambda (world)
       (define subject (hash-ref world 'subject))
       (define result
         (delivery-resolve-pr! subject
                               plan-id
                               1
                               "campaign/w2"
                               #:expected-head (hash-ref world 'receipt)))
       (check-equal? result
                     (hasheq 'status
                             "resolved"
                             'pr
                             3
                             'plan-id
                             plan-id
                             'wave
                             1
                             'branch
                             "campaign/w2"
                             'head
                             (hash-ref world 'tip)))
       ;; The PR ref was really fetched: FETCH_HEAD is the tip, not main.
       (check-equal? (git subject "rev-parse" "FETCH_HEAD") (hash-ref world 'tip))
       (check-not-equal? (hash-ref world 'tip) (hash-ref world 'main)))))

  (test-case "resolve-pr refuses a closed candidate that is not the merged resume"
    ;; No expected head -> the open query returns a closed PR: the state guard
    ;; (reached only WITH an expected head) is pinned separately below.
    (with-ladder-world
     (lambda (world)
       (list (list (format "repos/~a/pulls?state=open&head=coinerd:~a" slug branch)
                   (jsexpr->string
                    (list (pr-json #:number 1 #:state "closed" #:head-sha (hash-ref world 'head)))))))
     slug
     (lambda (world)
       (check-equal? (delivery-refusal (lambda ()
                                         (delivery-resolve-pr! (hash-ref world 'subject)
                                                               plan-id
                                                               0
                                                               branch
                                                               #:expected-head
                                                               (hash-ref world 'main))))
                     "resolved pull request has an unexpected state"))))

  (test-case "resolve-pr refuses an unusable number, short head, wrong branch and a fork"
    (define head-sha (make-string 40 #\d))
    (define (run-with pr expected)
      (with-ladder-world
       (lambda (_world)
         (list (list (format "repos/~a/pulls?state=open&head=coinerd:~a" slug branch)
                     (jsexpr->string (list pr)))))
       slug
       (lambda (world)
         (check-equal? (delivery-refusal (lambda ()
                                           (delivery-resolve-pr! (hash-ref world 'subject)
                                                                 plan-id
                                                                 0
                                                                 branch
                                                                 #:expected-head
                                                                 (hash-ref world 'main))))
                       expected))))
    ;; No usable number: a PR without `number` refuses before anything else.
    (run-with (hasheq 'state "open") "resolved pull request has no usable number")
    ;; Short head SHA.
    (run-with (pr-json #:number 1 #:head-sha "deadbeef") "resolved pull request has no full head SHA")
    ;; Foreign head branch.
    (run-with (pr-json #:number 1 #:head-sha head-sha #:head-ref "campaign/other")
              "resolved pull request branch does not match expected branch")
    ;; Fork head repository.
    (run-with (pr-json #:number 1 #:head-sha head-sha #:head-repo "attacker/q")
              "implementation PR must be a same-repository main-base pull request")
    ;; Non-main base.
    (run-with (pr-json #:number 1 #:head-sha head-sha #:base-ref "develop")
              "implementation PR must be a same-repository main-base pull request"))

  (test-case "resolve-pr refuses a PR head that is not the actually-fetched PR ref"
    (with-ladder-world
     (lambda (world)
       (list (list (format "repos/~a/pulls?state=open&head=coinerd:~a" slug branch)
                   (jsexpr->string (list (pr-json #:number 1 #:head-sha (hash-ref world 'tip)))))))
     slug
     (lambda (world)
       ;; refs/pull/1/head carries the w1 head; the PR claims a different sha.
       (check-equal? (delivery-refusal (lambda ()
                                         (delivery-resolve-pr! (hash-ref world 'subject)
                                                               plan-id
                                                               0
                                                               branch
                                                               #:expected-head
                                                               (hash-ref world 'main))))
                     "resolved pull-request head does not match fetched ref"))))

  (test-case "resolve-pr accepts evidence-only drift and refuses non-evidence drift"
    (define (run-for target-branch number expected-head-key tip-key expected)
      (with-ladder-world
       (lambda (world)
         (list (list (format "repos/~a/pulls?state=open&head=coinerd:~a" slug target-branch)
                     (jsexpr->string (list (pr-json #:number number
                                                    #:head-sha (hash-ref world tip-key)
                                                    #:head-ref target-branch))))))
       slug
       (lambda (world)
         (define result
           (delivery-resolve-pr! (hash-ref world 'subject)
                                 plan-id
                                 1
                                 target-branch
                                 #:expected-head (hash-ref world expected-head-key)))
         (check-equal? result
                       (hasheq 'status
                               "resolved"
                               'pr
                               number
                               'plan-id
                               plan-id
                               'wave
                               1
                               'branch
                               target-branch
                               'head
                               (hash-ref world tip-key))))))
    ;; campaign/w2 tip differs from the receipt only in gsd-wave-evidence.
    (run-for "campaign/w2" 2 'receipt 'tip 'ok)
    ;; campaign/drift adds a source file after the receipt head.
    (with-ladder-world
     (lambda (world)
       (list (list (format "repos/~a/pulls?state=open&head=coinerd:campaign/drift" slug)
                   (jsexpr->string (list (pr-json #:number 4
                                                  #:head-sha (hash-ref world 'drift)
                                                  #:head-ref "campaign/drift"))))))
     slug
     (lambda (world)
       (define receipt (hash-ref world 'receipt))
       (define drift (hash-ref world 'drift))
       (check-equal?
        (delivery-refusal (lambda ()
                            (delivery-resolve-pr! (hash-ref world 'subject)
                                                  plan-id
                                                  1
                                                  "campaign/drift"
                                                  #:expected-head receipt)))
        (format
         "receipt head ~a is verified but later wave-branch commits touch non-evidence paths: src-drift; re-verify before merge"
         (substring receipt 0 12))
        (format "drift tip ~a" (substring drift 0 12))))))

  (test-case "resolve-pr refuses a non-list pulls payload for open and all-state queries"
    (define object-payload "{\"message\":\"Not Found\"}")
    (with-ladder-world
     (lambda (_world)
       (list (list (format "repos/~a/pulls?state=open&head=coinerd:~a" slug branch) object-payload)))
     slug
     (lambda (world)
       (check-equal? (delivery-refusal (lambda ()
                                         (delivery-resolve-pr! (hash-ref world 'subject) plan-id 0 branch)))
                     "malformed pull-request response")))
    (with-ladder-world
     (lambda (_world)
       (list (list (format "repos/~a/pulls?state=open&head=coinerd:campaign/w2" slug) "[]")
             (list (format "repos/~a/pulls?state=all&head=coinerd:campaign/w2" slug) object-payload)))
     slug
     (lambda (world)
       (check-equal? (delivery-refusal (lambda ()
                                         (delivery-resolve-pr! (hash-ref world 'subject)
                                                               plan-id
                                                               1
                                                               "campaign/w2"
                                                               #:expected-head
                                                               (hash-ref world 'receipt))))
                     "malformed pull-request response")))
    ;; A JSON-null candidate is the documented STRICTER-than-Python deviation:
    ;; Racket's 'null is truthy, so nothing is ever resolved from it.
    (with-ladder-world
     (lambda (_world)
       (list (list (format "repos/~a/pulls?state=open&head=coinerd:~a" slug branch) "[null]")
             (list (format "repos/~a/pulls?state=all&head=coinerd:~a" slug branch) "[null]")))
     slug
     (lambda (world)
       (check-equal? (delivery-refusal (lambda ()
                                         (delivery-resolve-pr! (hash-ref world 'subject)
                                                               plan-id
                                                               0
                                                               branch
                                                               #:expected-head
                                                               (hash-ref world 'main))))
                     "resolved pull request has no usable number"))))

  (test-case "resolve-merged-pr ignores a closed but NOT merged PR in the all-state query"
    (with-ladder-world
     (lambda (world)
       (list (list (format "repos/~a/pulls?state=open&head=coinerd:campaign/w2" slug) "[]")
             (list (format "repos/~a/pulls?state=all&head=coinerd:campaign/w2" slug)
                   (jsexpr->string (list (pr-json #:number 3
                                                  #:state "closed"
                                                  #:head-sha (hash-ref world 'tip)
                                                  #:head-ref "campaign/w2"))))))
     slug
     (lambda (world)
       (define receipt (hash-ref world 'receipt))
       (check-equal? (delivery-resolve-pr! (hash-ref world 'subject)
                                           plan-id
                                           1
                                           "campaign/w2"
                                           #:expected-head receipt)
                     (hasheq 'status "none" 'plan-id plan-id 'wave 1 'branch "campaign/w2"))
       (check-false
        (resolve-merged-pr (delivery-pr-context-api (make-delivery-pr-context (hash-ref world 'subject)))
                           slug
                           "campaign/w2"
                           receipt)))))

  ;; ============================================================
  ;; require-receipt-tip on real git objects
  ;; ============================================================

  (test-case "require-receipt-tip refuses a tip the receipt head is not an ancestor of"
    (with-ladder-world
     '()
     slug
     (lambda (world)
       (define ctx (make-delivery-pr-context (hash-ref world 'subject)))
       (define git* (delivery-pr-context-git ctx))
       (define receipt (hash-ref world 'receipt))
       (define main (hash-ref world 'main))
       (check-equal?
        (delivery-refusal (lambda () (require-receipt-tip git* receipt main "review")))
        (format
         "receipt head ~a is not an ancestor of tip ~a; the wave branch may have been rewritten; re-verify before review"
         (substring receipt 0 12)
         (substring main 0 12))))))

  (test-case "require-receipt-tip accepts excluded-evidence-only drift"
    (with-ladder-world '()
                       slug
                       (lambda (world)
                         (define ctx (make-delivery-pr-context (hash-ref world 'subject)))
                         (check-not-exn (lambda ()
                                          (require-receipt-tip (delivery-pr-context-git ctx)
                                                               (hash-ref world 'receipt)
                                                               (hash-ref world 'tip)
                                                               "merge"))))))

  (test-case "require-receipt-tip refuses an intervening non-evidence commit"
    (with-ladder-world
     '()
     slug
     (lambda (world)
       (define ctx (make-delivery-pr-context (hash-ref world 'subject)))
       (define receipt (hash-ref world 'receipt))
       (check-equal?
        (delivery-refusal
         (lambda ()
           (require-receipt-tip (delivery-pr-context-git ctx) receipt (hash-ref world 'drift))))
        (format
         "receipt head ~a is verified but later wave-branch commits touch non-evidence paths: src-drift; re-verify before delivery"
         (substring receipt 0 12))))))

  (test-case "require-receipt-tip refuses a look-alike evidence prefix (no widening)"
    (with-ladder-world
     '()
     slug
     (lambda (world)
       (define ctx (make-delivery-pr-context (hash-ref world 'subject)))
       (define receipt (hash-ref world 'receipt))
       (check-equal?
        (delivery-refusal
         (lambda ()
           (require-receipt-tip (delivery-pr-context-git ctx) receipt (hash-ref world 'drift-x))))
        (format
         (string-append
          "receipt head ~a is verified but later wave-branch commits touch non-evidence paths: "
          "docs/reports/gsd-wave-evidence-x/foo; re-verify before delivery")
         (substring receipt 0 12))))))

  (test-case "require-receipt-tip lists exactly three of many foreign drift paths"
    (with-ladder-world
     '()
     slug
     (lambda (world)
       (define ctx (make-delivery-pr-context (hash-ref world 'subject)))
       (define receipt (hash-ref world 'receipt))
       (check-equal?
        (delivery-refusal
         (lambda ()
           (require-receipt-tip (delivery-pr-context-git ctx) receipt (hash-ref world 'drift-many))))
        (format
         (string-append
          "receipt head ~a is verified but later wave-branch commits touch non-evidence paths: "
          "src-drift-a, src-drift-b, src-drift-c; re-verify before delivery")
         (substring receipt 0 12))))))

  (test-case "require-receipt-tip refuses a non-full expected head and ignores absence"
    (with-ladder-world
     '()
     slug
     (lambda (world)
       (define ctx (make-delivery-pr-context (hash-ref world 'subject)))
       (define git* (delivery-pr-context-git ctx))
       (check-equal?
        (delivery-refusal (lambda () (require-receipt-tip git* "deadbeef" (hash-ref world 'head))))
        "expected-head must be a full SHA")
       (check-not-exn (lambda () (require-receipt-tip git* #f (hash-ref world 'head)))))))

  ;; ============================================================
  ;; budget: one session, one budget, passed through
  ;; ============================================================

  (test-case "an action's git reads share the session's ONE budget"
    (with-budget-shims
     (lambda (root)
       ;; Positive control: the shim is on PATH and records invocations.
       (define healthy (make-delivery-pr-context root))
       (check-equal?
        (string-trim ((delivery-pr-context-git healthy) (list "config" "--get" "remote.origin.url")))
        "https://github.com/coinerd/q")
       (define pre-spend (git-log))
       (check-equal? (length pre-spend) 1 "the control call must be recorded")
       ;; The (budget, start-ms) pair the gh reads use is ALREADY spent, so the
       ;; first git read of the action must refuse before spawning anything.
       (define spent
         (make-delivery-pr-context root
                                   #:budget
                                   10.0
                                   #:start-ms
                                   (- (current-inexact-monotonic-milliseconds) 60000.0)))
       (check-equal?
        (delivery-refusal (lambda () (delivery-pr-ci! root 1 branch #:context spent)))
        "budget-exhausted: the delivery process budget (10s) is spent; rerun the action separately")
       ;; No fresh runner was created: a second make-repo-git-command would have
       ;; run `git config` and `git fetch` first, so the log would grow.
       (check-equal? (git-log) pre-spend "the shared budget must refuse before any git exec")
       (check-equal? (gh-log) '() "no gh call may happen after the budget refusal"))))

  ;; ============================================================
  ;; policy_names
  ;; ============================================================

  (test-case "policy_names reads the snapshot at a ref and refuses absence/malformation"
    (with-ladder-world
     '()
     slug
     (lambda (world)
       (define ctx (make-delivery-pr-context (hash-ref world 'subject)))
       (define git* (delivery-pr-context-git ctx))
       (check-equal? (policy-names git* (hash-ref world 'main)) '("ci"))
       (check-equal? (delivery-refusal (lambda () (policy-names git* (hash-ref world 'pre-policy))))
                     (string-append "required-check policy is absent at "
                                    (hash-ref world 'pre-policy)))
       (check-equal? (delivery-refusal (lambda () (policy-names git* (hash-ref world 'bad-policy))))
                     (string-append "malformed required-check policy at "
                                    (hash-ref world 'bad-policy))))))

  (test-case "policy_names refuses an empty and a blank-named required-check policy"
    (with-ladder-world
     '()
     slug
     (lambda (world)
       (define git* (delivery-pr-context-git (make-delivery-pr-context (hash-ref world 'subject))))
       (for ([ref (in-list (list (hash-ref world 'empty-policy) (hash-ref world 'blank-policy)))])
         (check-equal? (delivery-refusal (lambda () (policy-names git* ref)))
                       (string-append "malformed required-check policy at " ref))))))

  (test-case "blob_or_none returns #f for an absent path and the blob text otherwise"
    (with-ladder-world
     '()
     slug
     (lambda (world)
       (define ctx (make-delivery-pr-context (hash-ref world 'subject)))
       (define git* (delivery-pr-context-git ctx))
       (check-false
        (blob-or-none git* (hash-ref world 'pre-policy) "scripts/required-pr-checks.policy"))
       (check-equal? (blob-or-none git* (hash-ref world 'main) "scripts/required-pr-checks.policy")
                     "(\"ci\")\n"))))

  ;; ============================================================
  ;; pr-ci
  ;; ============================================================

  (define (green-fixtures world slug* pr)
    (define head (hash-ref pr 'head-sha))
    (list (list (route slug* "pulls/1") (jsexpr->string (pr-json #:number 1 #:head-sha head)))
          (list (route slug* (format "commits/~a/check-runs?per_page=100&page=1" head))
                (checks-page (list (check-run #:head head #:slug slug*)) 1))
          (list (route slug* "actions/runs/999")
                (jsexpr->string (actions-run #:head head #:repo slug*)))
          (list (route slug* "branches/main/protection") (protection-json))))

  (test-case "pr-ci reports green through policy + protection + trusted checks"
    (with-ladder-world
     (lambda (world) (green-fixtures world slug (hasheq 'head-sha (hash-ref world 'head))))
     slug
     (lambda (world)
       (define result (delivery-pr-ci! (hash-ref world 'subject) 1 branch))
       (check-equal? result
                     (hasheq 'status "green" 'pr 1 'branch branch 'head (hash-ref world 'head)))
       (define routes (gh-log))
       (check-equal? (length routes) 4)
       (check-equal? (list-ref routes 0) (format "api --hostname github.com repos/~a/pulls/1" slug))
       ;; protection first, then the trusted-check snapshot at the exact head.
       (check-equal? (list-ref routes 1)
                     (format "api --hostname github.com repos/~a/branches/main/protection" slug))
       (check-true (string-contains? (list-ref routes 2) "check-runs")
                   "the required-check snapshot is read at the exact head")
       (check-true (string-suffix? (list-ref routes 3) "actions/runs/999") (list-ref routes 3)))))

  (test-case "pr-ci refuses pending, failed, missing and untrusted required checks"
    (define (run-with check-run* expected)
      (with-ladder-world
       (lambda (world)
         (define head (hash-ref world 'head))
         (list (list (route slug "pulls/1") (jsexpr->string (pr-json #:number 1 #:head-sha head)))
               (list (route slug (format "commits/~a/check-runs?per_page=100&page=1" head))
                     (checks-page (if check-run*
                                      (list check-run*)
                                      '())
                                  (if check-run* 1 0)))
               (list (route slug "actions/runs/999") (jsexpr->string (actions-run #:head head)))
               (list (route slug "branches/main/protection") (protection-json))))
       slug
       (lambda (world)
         (check-equal? (delivery-refusal (lambda ()
                                           (delivery-pr-ci! (hash-ref world 'subject) 1 branch)))
                       expected))))
    (run-with (check-run #:status "in_progress" #:conclusion 'null)
              "pending, failed, or untrusted check: ci")
    (run-with (check-run #:conclusion "failure") "pending, failed, or untrusted check: ci")
    (run-with (check-run #:app 1234) "pending, failed, or untrusted check: ci")
    (run-with (check-run #:name "other") "missing check: ci"))

  (test-case "pr-ci refuses a closed PR unless it is the receipt-bound merged resume"
    (define (run-with state expected-head-key expected)
      (with-ladder-world
       (lambda (world)
         (define head (hash-ref world 'head))
         (list (list (route slug "pulls/1")
                     (jsexpr->string (pr-json #:number 1
                                              #:state state
                                              #:head-sha head
                                              #:merged (if (equal? state "closed") #t 'null)
                                              #:merged-at (if (equal? state "closed")
                                                              "2026-01-01T00:00:00Z"
                                                              'null))))
               (list (route slug (format "commits/~a/check-runs?per_page=100&page=1" head))
                     (checks-page (list (check-run #:head head)) 1))
               (list (route slug "actions/runs/999") (jsexpr->string (actions-run #:head head)))
               (list (route slug "branches/main/protection") (protection-json))))
       slug
       (lambda (world)
         (define (call)
           (delivery-pr-ci! (hash-ref world 'subject)
                            1
                            branch
                            #:expected-head (and expected-head-key
                                                 (hash-ref world expected-head-key))))
         (if (eq? expected 'ok)
             (check-equal? (call)
                           (hasheq 'status "green" 'pr 1 'branch branch 'head (hash-ref world 'head)))
             (check-equal? (delivery-refusal call) expected)))))
    (run-with "closed" #f "pull request is not open")
    (run-with "closed" 'head 'ok)
    (run-with "open" #f 'ok))

  (test-case "pr-ci refuses a closed but NOT merged PR even at the expected head"
    (with-ladder-world
     (lambda (world)
       (define head (hash-ref world 'head))
       (list (list (route slug "pulls/1")
                   (jsexpr->string (pr-json #:number 1 #:state "closed" #:head-sha head)))))
     slug
     (lambda (world)
       ;; The merged-resume tolerance is gated on pr_is_merged: a closed PR
       ;; without `merged`/`merged_at` is never the resume identity.
       (check-equal? (delivery-refusal (lambda ()
                                         (delivery-pr-ci! (hash-ref world 'subject)
                                                          1
                                                          branch
                                                          #:expected-head
                                                          (hash-ref world 'head))))
                     "pull request is not open"))))

  (test-case "pr-ci and validate-pr-identity refuse a malformed PR datum"
    (with-ladder-world
     (lambda (_world) (list (list (route slug "pulls/1") "[]")))
     slug
     (lambda (world)
       (check-equal? (delivery-refusal (lambda () (delivery-pr-ci! (hash-ref world 'subject) 1 branch)))
                     "malformed pull-request response")))
    (with-ladder-world
     (lambda (_world) (list (list (route slug "pulls/1") "null")))
     slug
     (lambda (world)
       (check-equal? (delivery-refusal (lambda () (delivery-pr-ci! (hash-ref world 'subject) 1 branch)))
                     "malformed GitHub response for pulls/1")))
    (for ([bad (in-list '(() "not-a-hash" 42))])
      (check-equal? (delivery-refusal (lambda () (validate-pr-identity bad slug)))
                    "malformed PR response"
                    (format "~s" bad))))

  (test-case "pr-ci refuses a short head, a foreign branch and a mismatched fetched ref"
    (define (run-with pr expected)
      (with-ladder-world
       (lambda (world) (list (list (route slug "pulls/1") (jsexpr->string pr))))
       slug
       (lambda (world)
         (check-equal? (delivery-refusal (lambda ()
                                           (delivery-pr-ci! (hash-ref world 'subject) 1 branch)))
                       expected))))
    ;; Short head SHA refuses before any fetch.
    (run-with (pr-json #:number 1 #:head-sha "deadbeef") "pull request has no full head SHA")
    ;; Foreign head branch.
    (run-with (pr-json #:number 1 #:head-sha (make-string 40 #\d) #:head-ref "campaign/other")
              "pull request branch does not match expected branch")
    ;; Fork head repository.
    (run-with (pr-json #:number 1 #:head-sha (make-string 40 #\d) #:head-repo "attacker/q")
              "implementation PR must be a same-repository main-base pull request"))

  (test-case "pr-ci refuses a PR head that is not the actually-fetched PR ref"
    (with-ladder-world
     (lambda (world)
       (list (list (route slug "pulls/1")
                   (jsexpr->string (pr-json #:number 1 #:head-sha (hash-ref world 'tip))))))
     slug
     (lambda (world)
       (check-equal? (delivery-refusal (lambda ()
                                         (delivery-pr-ci! (hash-ref world 'subject) 1 branch)))
                     "pull-request head does not match fetched ref"))))

  (test-case "pr-ci refuses a missing --pr/--expected-branch and a malformed origin/main"
    (define subject "/nonexistent-native-ladder-fixture")
    (for ([number (in-list (list 0 -1 "1" 1.0 #f))])
      (check-equal? (delivery-refusal (lambda () (delivery-pr-ci! subject number branch)))
                    "pr-ci requires --pr"
                    (format "~s" number)))
    (for ([expected-branch (in-list (list "" "   " #f))])
      (check-equal? (delivery-refusal (lambda () (delivery-pr-ci! subject 1 expected-branch)))
                    "pr-ci requires --expected-branch"
                    (format "~s" expected-branch)))
    ;; A git shim answers the fetch but prints an unusable origin/main.
    (with-git-shim-world
     (string-append "#!/bin/sh"
                    "\nprintf '%s\\n' \"$*\" >> \"$GIT_SHIM_LOG\""
                    "\ncase \"$*\" in"
                    "\n  *'config --get remote.origin.url'*)"
                    " printf '%s\\n' 'https://github.com/coinerd/q' ;;"
                    "\n  *'rev-parse refs/remotes/origin/main'*) printf '%s\\n' 'deadbeef' ;;"
                    "\nesac"
                    "\nexit 0")
     (lambda (subject*)
       (check-equal? (delivery-refusal (lambda () (delivery-pr-ci! subject* 1 branch)))
                     "malformed origin/main head"))))

  (test-case "pr-ci reads the policy from the git ref, not from the worktree copy"
    (with-ladder-world
     (lambda (world)
       (define head (hash-ref world 'head))
       (list (list (route slug "pulls/1") (jsexpr->string (pr-json #:number 1 #:head-sha head)))
             (list (route slug (format "commits/~a/check-runs?per_page=100&page=1" head))
                   (checks-page (list (check-run #:head head)) 1))
             (list (route slug "actions/runs/999") (jsexpr->string (actions-run #:head head)))
             (list (route slug "branches/main/protection") (protection-json))))
     slug
     (lambda (world)
       ;; The checkout's policy file is rewritten to a DIFFERENT name set. A
       ;; worktree read would now require `other` and refuse; the committed ref
       ;; object still says `ci`, so the run must stay green.
       (define worktree-policy
         (build-path (hash-ref world 'subject) "scripts/required-pr-checks.policy"))
       (put worktree-policy "(\"other\")\n")
       (check-equal? (delivery-pr-ci! (hash-ref world 'subject) 1 branch)
                     (hasheq 'status "green" 'pr 1 'branch branch 'head (hash-ref world 'head))))))

  ;; ============================================================
  ;; review
  ;; ============================================================

  (test-case "review refuses non-receipt arguments before any git read"
    (define subject "/nonexistent-native-ladder-fixture")
    (check-equal? (delivery-refusal (lambda ()
                                      (delivery-review! subject plan-id 0 "deadbeef" branch)))
                  "review requires --expected-head (the durable receipt head)")
    (check-equal? (delivery-refusal (lambda ()
                                      (delivery-review! subject plan-id 0 (make-string 40 #\a) "  ")))
                  "review requires --expected-branch (the durable receipt branch)")
    (check-equal? (delivery-refusal
                   (lambda ()
                     (delivery-review! subject plan-id 0 (make-string 40 #\a) branch #:generation 1)))
                  "review --generation requires --campaign-root for authoritative resolution"))

  (test-case "review refuses when the binding trio is absent at the receipt tip"
    (with-ladder-world
     '()
     slug
     (lambda (world)
       ;; campaign/w2 carries only excluded-evidence drift after the receipt,
       ;; so the receipt-tip rule passes and the hash-named W1 trio is simply
       ;; absent at that tip (the fixture committed a W0 path). The refusal is
       ;; the Python message verbatim — never an invented artifact.
       (define outcome
         (delivery-refusal (lambda ()
                             (delivery-review! (hash-ref world 'subject)
                                               plan-id
                                               1
                                               (hash-ref world 'receipt)
                                               "campaign/w2"))))
       (check-true
        (string-prefix?
         outcome
         (format
          "binding trio docs/reports/gsd-wave-evidence/~a-w1.rktd is absent on branch campaign/w2 (tip "
          plan-id))
        outcome)
       (check-true (string-suffix? outcome
                                   "; a genuine independent reviewer must commit it after Verify")
                   outcome))))

  (test-case "review refuses a rewritten branch before reading any artifact"
    (with-ladder-world
     '()
     slug
     (lambda (world)
       ;; The receipt head is not an ancestor of campaign/w1's tip.
       (define receipt (hash-ref world 'receipt))
       (define head (hash-ref world 'head))
       (check-equal?
        (delivery-refusal (lambda ()
                            (delivery-review! (hash-ref world 'subject) plan-id 1 receipt branch)))
        (format
         "receipt head ~a is not an ancestor of tip ~a; the wave branch may have been rewritten; re-verify before review"
         (substring receipt 0 12)
         (substring head 0 12))))))

  (test-case "review returns awaiting-review when the trio exists but the review artifact is absent"
    (with-ladder-world
     '()
     slug
     (lambda (world)
       (define tip (hash-ref world 'review-absent-tip))
       (check-equal?
        (delivery-review! (hash-ref world 'subject) plan-id 1 tip "campaign/w5")
        (hasheq
         'status
         "awaiting-review"
         'plan-id
         plan-id
         'wave
         1
         'head
         tip
         'review-artifact
         (format "docs/reports/gsd-wave-reviews/~a-w1.rktd" plan-id)
         'reason
         "review artifact absent at the receipt identity; a genuine independent non-author reviewer must produce it")))))

  (test-case "review reports reviewed for a genuine APPROVED trio bound to implementation-sha and digest"
    (with-ladder-world
     '()
     slug
     (lambda (world)
       (define work (hash-ref world 'work))
       (define main (hash-ref world 'main))
       (define ancestor (hash-ref world 'review-ancestor))
       (define result (delivery-review! (hash-ref world 'subject) plan-id 1 ancestor "campaign/w3"))
       (check-equal? result
                     (hasheq 'status
                             "reviewed"
                             'plan-id
                             plan-id
                             'wave
                             1
                             'head
                             ancestor
                             'reviewed-sha
                             ancestor
                             'review-artifact
                             (format "docs/reports/gsd-wave-reviews/~a-w1.rktd" plan-id)))
       ;; The gate accepted the recomputed excluded-content digest: the trio
       ;; commit itself lives only under excluded directories, so the digest of
       ;; the tip equals the digest of the implementation head.
       (check-equal? (excluded-content-digest work main (hash-ref world 'review-tip))
                     (hash-ref world 'review-digest)))))

  (test-case "review accepts a reviewed-sha that is an evidence-only ancestor of the receipt head"
    (with-ladder-world
     '()
     slug
     (lambda (world)
       (define tip (hash-ref world 'review-tip))
       (define ancestor (hash-ref world 'review-ancestor))
       (check-not-equal? tip ancestor)
       (check-equal? (delivery-review! (hash-ref world 'subject) plan-id 1 tip "campaign/w3")
                     (hasheq 'status
                             "reviewed"
                             'plan-id
                             plan-id
                             'wave
                             1
                             'head
                             tip
                             'reviewed-sha
                             ancestor
                             'review-artifact
                             (format "docs/reports/gsd-wave-reviews/~a-w1.rktd" plan-id))))))

  (test-case "review refuses a reviewed-sha that is neither the receipt head nor an evidence-only ancestor"
    (with-ladder-world
     '()
     slug
     (lambda (world)
       (define tip (hash-ref world 'review-foreign-tip))
       (define drift (hash-ref world 'drift))
       (check-equal?
        (delivery-refusal (lambda ()
                            (delivery-review! (hash-ref world 'subject) plan-id 1 tip "campaign/w4")))
        (format
         "receipt head ~a is not an ancestor of tip ~a; the wave branch may have been rewritten; re-verify before review"
         (substring drift 0 12)
         (substring tip 0 12))))))

  (test-case "the review CLI accepts --campaign-root as text and refuses through the typed surface"
    (with-ladder-world
     '()
     slug
     (lambda (world)
       (define campaign-root (make-temporary-file "native-ladder-campaign-~a" 'directory))
       (dynamic-wind
        void
        (lambda ()
          (define result
            (run-subprocess (path->string racket-executable)
                            #:args
                            (list (path->string read-ladder-module)
                                  "review"
                                  "--repo"
                                  (path->string (hash-ref world 'subject))
                                  "--plan"
                                  plan-id
                                  "--wave"
                                  "1"
                                  "--expected-head"
                                  (hash-ref world 'receipt)
                                  "--expected-branch"
                                  "campaign/w2"
                                  "--campaign-root"
                                  (path->string campaign-root))
                            #:directory (hash-ref world 'root)
                            #:environment (current-environment-variables)
                            #:timeout 120
                            #:process-group? #t))
          ;; A raw string campaign root used to raise a path->string contract
          ;; violation; it is normalized now, so the action reaches the
          ;; coordinator snapshot, which refuses as typed JSON + exit 2.
          (check-equal? (subprocess-result-exit-code result) 2)
          (check-equal? (string->jsexpr (string-trim (subprocess-result-stdout result)))
                        (hasheq 'status "delivery-pending" 'reason "malformed campaign snapshot facts")))
        (lambda () (delete-directory/files campaign-root))))))

  ;; ============================================================
  ;; differential parity with the recorded Python answers
  ;; ============================================================

  (test-case "differential parity with the recorded Python answers (>= 13 cases)"
    ;; Every case's Python outcome was recorded from scripts/gsd-delivery.py at
    ;; its final revision (tests/fixtures/gsd-delivery-goldens.json); the golden
    ;; group pins the case count, the label set and each case's input text.
    (define (case-slug n)
      (format "coinerd/ladder-~a"
              (if (< n 10)
                  (format "0~a" n)
                  (format "~a" n))))
    (define (open-route slug* branch*)
      (format "repos/~a/pulls?state=open&head=coinerd:~a" slug* branch*))
    (define (all-route slug* branch*)
      (format "repos/~a/pulls?state=all&head=coinerd:~a" slug* branch*))
    (with-ladder-world
     (lambda (world)
       (define head (hash-ref world 'head))
       (define tip (hash-ref world 'tip))
       (define receipt (hash-ref world 'receipt))
       (define main (hash-ref world 'main))
       (define drift (hash-ref world 'drift))
       (define (s n)
         (case-slug n))
       (append
        ;; 1 branch_owner success
        '()
        ;; 2..n fixtures built below
        ;; resolve_pr: none
        (list
         (list (open-route (s 2) "campaign/none") "[]")
         ;; resolve_pr: open
         (list (open-route (s 3) branch) (jsexpr->string (list (pr-json #:number 7 #:head-sha head))))
         ;; resolve_pr: multiple
         (list (open-route (s 4) "campaign/w9")
               (jsexpr->string (list (pr-json #:number 1 #:head-sha head)
                                     (pr-json #:number 2 #:head-sha head))))
         ;; resolve_pr: unexpected state
         (list (open-route (s 5) branch)
               (jsexpr->string (list (pr-json #:number 1 #:state "closed" #:head-sha head))))
         ;; resolve_pr: no usable number
         (list (open-route (s 6) branch) (jsexpr->string (list (hasheq 'state "open"))))
         ;; resolve_pr: no full head SHA
         (list (open-route (s 7) branch)
               (jsexpr->string (list (pr-json #:number 1 #:head-sha "deadbeef"))))
         ;; resolve_pr: branch mismatch
         (list (open-route (s 8) branch)
               (jsexpr->string
                (list (pr-json #:number 1 #:head-sha head #:head-ref "campaign/other"))))
         ;; resolve_pr: fork identity
         (list (open-route (s 9) branch)
               (jsexpr->string (list (pr-json #:number 1 #:head-sha head #:head-repo "attacker/q"))))
         ;; resolve_pr: fetched ref mismatch (PR claims the tip, ref 1 is head)
         (list (open-route (s 10) branch)
               (jsexpr->string
                (list (pr-json #:number 1 #:head-sha tip #:head-repo (s 10) #:base-repo (s 10)))))
         ;; resolve_pr: merged resume
         (list (open-route (s 11) "campaign/w2") "[]")
         (list
          (all-route (s 11) "campaign/w2")
          (jsexpr->string
           (list (merged-pr-json #:number 3 #:head-sha tip #:head-repo (s 11) #:base-repo (s 11)))))
         ;; resolve_pr: receipt drift
         (list (open-route (s 12) "campaign/drift")
               (jsexpr->string (list (pr-json #:number 4
                                              #:head-sha drift
                                              #:head-ref "campaign/drift"
                                              #:head-repo (s 12)
                                              #:base-repo (s 12)))))
         ;; resolve_merged_pr: none
         (list (all-route (s 13) "campaign/none") "[]")
         ;; pr_ci: not open
         (list (route (s 14) "pulls/1")
               (jsexpr->string (pr-json #:number 1 #:state "closed" #:head-sha head)))
         ;; pr_ci: green
         (list (route (s 15) "pulls/1")
               (jsexpr->string
                (pr-json #:number 1 #:head-sha head #:head-repo (s 15) #:base-repo (s 15))))
         (list (route (s 15) (format "commits/~a/check-runs?per_page=100&page=1" head))
               (checks-page (list (check-run #:head head #:slug (s 15))) 1))
         (list (route (s 15) "actions/runs/999")
               (jsexpr->string (actions-run #:head head #:repo (s 15))))
         (list (route (s 15) "branches/main/protection") (protection-json))
         ;; pr_ci: fetched ref mismatch
         (list (route (s 16) "pulls/1")
               (jsexpr->string
                (pr-json #:number 1 #:head-sha tip #:head-repo (s 16) #:base-repo (s 16)))))))
     #f
     (lambda (world)
       (define subject (path->string (hash-ref world 'subject)))
       (define head (hash-ref world 'head))
       (define tip (hash-ref world 'tip))
       (define receipt (hash-ref world 'receipt))
       (define main (hash-ref world 'main))
       (define drift (hash-ref world 'drift))
       (define pre-policy (hash-ref world 'pre-policy))
       (define bad-policy (hash-ref world 'bad-policy))
       (define (s n)
         (case-slug n))
       (define cases
         (list
          (list "branch-owner"
                (hasheq 'fn "branch_owner" 'slug "coinerd/q")
                'ok
                (lambda () (branch-owner "coinerd/q")))
          (list "branch-owner-malformed"
                (hasheq 'fn "branch_owner" 'slug "coinerd")
                "malformed repository slug: coinerd"
                (lambda () (branch-owner "coinerd")))
          (list
           "resolve-prs-none"
           (hasheq 'fn "resolve_prs_for_branch" 'slug (s 2) 'branch "campaign/none" 'state "open")
           'ok
           (lambda ()
             (resolve-existing-pr (delivery-pr-context-api (make-delivery-pr-context subject))
                                  (s 2)
                                  "campaign/none")))
          (list "resolve-pr-prs-multiple"
                (hasheq 'fn "resolve_prs_for_branch" 'slug (s 4) 'branch "campaign/w9" 'state "open")
                "multiple pull requests already target branch campaign/w9"
                (lambda ()
                  (resolve-existing-pr (delivery-pr-context-api (make-delivery-pr-context subject))
                                       (s 4)
                                       "campaign/w9")))
          (list "resolve-pr-none"
                (hasheq 'fn "resolve_pr" 'repo subject 'plan plan-id 'wave 0 'branch "campaign/none")
                'ok
                (lambda () (delivery-resolve-pr! subject plan-id 0 "campaign/none")))
          (list "resolve-pr-open"
                (hasheq 'fn "resolve_pr" 'repo subject 'plan plan-id 'wave 0 'branch branch)
                'ok
                (lambda () (delivery-resolve-pr! subject plan-id 0 branch)))
          (list "resolve-pr-unexpected-state"
                (hasheq 'fn
                        "resolve_pr"
                        'repo
                        subject
                        'plan
                        plan-id
                        'wave
                        0
                        'branch
                        branch
                        'expected_head
                        main)
                "resolved pull request has an unexpected state"
                (lambda () (delivery-resolve-pr! subject plan-id 0 branch #:expected-head main)))
          (list "resolve-pr-no-usable-number"
                (hasheq 'fn
                        "resolve_pr"
                        'repo
                        subject
                        'plan
                        plan-id
                        'wave
                        0
                        'branch
                        branch
                        'expected_head
                        main)
                "resolved pull request has no usable number"
                (lambda () (delivery-resolve-pr! subject plan-id 0 branch #:expected-head main)))
          (list "resolve-pr-no-head-sha"
                (hasheq 'fn
                        "resolve_pr"
                        'repo
                        subject
                        'plan
                        plan-id
                        'wave
                        0
                        'branch
                        branch
                        'expected_head
                        main)
                "resolved pull request has no full head SHA"
                (lambda () (delivery-resolve-pr! subject plan-id 0 branch #:expected-head main)))
          (list "resolve-pr-branch-mismatch"
                (hasheq 'fn
                        "resolve_pr"
                        'repo
                        subject
                        'plan
                        plan-id
                        'wave
                        0
                        'branch
                        branch
                        'expected_head
                        main)
                "resolved pull request branch does not match expected branch"
                (lambda () (delivery-resolve-pr! subject plan-id 0 branch #:expected-head main)))
          (list "resolve-pr-identity"
                (hasheq 'fn
                        "resolve_pr"
                        'repo
                        subject
                        'plan
                        plan-id
                        'wave
                        0
                        'branch
                        branch
                        'expected_head
                        main)
                "implementation PR must be a same-repository main-base pull request"
                (lambda () (delivery-resolve-pr! subject plan-id 0 branch #:expected-head main)))
          (list "resolve-pr-fetched-mismatch"
                (hasheq 'fn
                        "resolve_pr"
                        'repo
                        subject
                        'plan
                        plan-id
                        'wave
                        0
                        'branch
                        branch
                        'expected_head
                        main)
                "resolved pull-request head does not match fetched ref"
                (lambda () (delivery-resolve-pr! subject plan-id 0 branch #:expected-head main)))
          (list "resolve-pr-merged-resume"
                (hasheq 'fn
                        "resolve_pr"
                        'repo
                        subject
                        'plan
                        plan-id
                        'wave
                        1
                        'branch
                        "campaign/w2"
                        'expected_head
                        receipt)
                'ok
                (lambda ()
                  (delivery-resolve-pr! subject plan-id 1 "campaign/w2" #:expected-head receipt)))
          (list
           "resolve-pr-receipt-drift"
           (hasheq 'fn
                   "resolve_pr"
                   'repo
                   subject
                   'plan
                   plan-id
                   'wave
                   1
                   'branch
                   "campaign/drift"
                   'expected_head
                   receipt)
           (format
            "receipt head ~a is verified but later wave-branch commits touch non-evidence paths: src-drift; re-verify before merge"
            (substring receipt 0 12))
           (lambda ()
             (delivery-resolve-pr! subject plan-id 1 "campaign/drift" #:expected-head receipt)))
          (list "resolve-merged-pr-none"
                (hasheq 'fn "resolve_merged_pr" 'slug (s 13) 'branch "campaign/none")
                'ok
                (lambda ()
                  (resolve-merged-pr (delivery-pr-context-api (make-delivery-pr-context subject))
                                     (s 13)
                                     "campaign/none"
                                     #f)))
          (list "resolve-merged-pr-requires-sha"
                (hasheq 'fn
                        "resolve_merged_pr"
                        'slug
                        (s 13)
                        'branch
                        "campaign/w2"
                        'expected_head
                        "deadbeef")
                "merged resolution requires a full expected-head SHA"
                (lambda ()
                  (resolve-merged-pr (delivery-pr-context-api (make-delivery-pr-context subject))
                                     (s 13)
                                     "campaign/w2"
                                     "deadbeef")))
          (list "validate-pr-identity-ok"
                (hasheq 'fn "validate_pr_identity" 'slug slug 'pr (pr-json))
                'ok
                (lambda () (validate-pr-identity (pr-json) slug)))
          (list "validate-pr-identity-fork"
                (hasheq 'fn "validate_pr_identity" 'slug slug 'pr (pr-json #:head-repo "attacker/q"))
                "implementation PR must be a same-repository main-base pull request"
                (lambda () (validate-pr-identity (pr-json #:head-repo "attacker/q") slug)))
          (list "policy-names-ok"
                (hasheq 'fn "policy_names" 'repo subject 'ref main)
                'ok
                (lambda ()
                  (policy-names (delivery-pr-context-git (make-delivery-pr-context subject)) main)))
          (list "policy-names-absent"
                (hasheq 'fn "policy_names" 'repo subject 'ref pre-policy)
                (string-append "required-check policy is absent at " pre-policy)
                (lambda ()
                  (policy-names (delivery-pr-context-git (make-delivery-pr-context subject))
                                pre-policy)))
          (list "policy-names-malformed"
                (hasheq 'fn "policy_names" 'repo subject 'ref bad-policy)
                (string-append "malformed required-check policy at " bad-policy)
                (lambda ()
                  (policy-names (delivery-pr-context-git (make-delivery-pr-context subject))
                                bad-policy)))
          (list
           "require-receipt-tip-not-ancestor"
           (hasheq 'fn
                   "require_receipt_tip"
                   'repo
                   subject
                   'expected_head
                   receipt
                   'tip
                   main
                   'action
                   "review")
           (format
            "receipt head ~a is not an ancestor of tip ~a; the wave branch may have been rewritten; re-verify before review"
            (substring receipt 0 12)
            (substring main 0 12))
           (lambda ()
             (require-receipt-tip (delivery-pr-context-git (make-delivery-pr-context subject))
                                  receipt
                                  main
                                  "review")))
          (list "require-receipt-tip-evidence-ok"
                (hasheq 'fn
                        "require_receipt_tip"
                        'repo
                        subject
                        'expected_head
                        receipt
                        'tip
                        tip
                        'action
                        "review")
                'ok
                (lambda ()
                  (require-receipt-tip (delivery-pr-context-git (make-delivery-pr-context subject))
                                       receipt
                                       tip
                                       "review")))
          (list
           "require-receipt-tip-drift"
           (hasheq 'fn
                   "require_receipt_tip"
                   'repo
                   subject
                   'expected_head
                   receipt
                   'tip
                   drift
                   'action
                   "review")
           (format
            "receipt head ~a is verified but later wave-branch commits touch non-evidence paths: src-drift; re-verify before review"
            (substring receipt 0 12))
           (lambda ()
             (require-receipt-tip (delivery-pr-context-git (make-delivery-pr-context subject))
                                  receipt
                                  drift
                                  "review")))
          (list "pr-ci-not-open"
                (hasheq 'fn "pr_ci" 'repo subject 'number 1 'branch branch)
                "pull request is not open"
                (lambda () (delivery-pr-ci! subject 1 branch)))
          (list "pr-ci-green"
                (hasheq 'fn "pr_ci" 'repo subject 'number 1 'branch branch)
                'ok
                (lambda () (delivery-pr-ci! subject 1 branch)))
          (list "pr-ci-fetched-mismatch"
                (hasheq 'fn "pr_ci" 'repo subject 'number 1 'branch branch)
                "pull-request head does not match fetched ref"
                (lambda () (delivery-pr-ci! subject 1 branch)))))
       (define parity-tokens
         (list (cons "head" head)
               (cons "tip" tip)
               (cons "receipt" receipt)
               (cons "main" main)
               (cons "drift" drift)
               (cons "pre-policy" pre-policy)
               (cons "bad-policy" bad-policy)
               (cons "repo" subject)))
       (check-true (>= (length cases) 13) "the parity table must carry at least 13 cases")
       ;; Each case owns ONE slug: the gh fixtures above are keyed by it, and
       ;; `point-at!` makes both sides read exactly that slug from the subject's
       ;; configured origin URL. A missing/extra slug is a loud failure.
       (define case-slugs
         (list (s 1)
               (s 1)
               (s 2)
               (s 4)
               (s 2)
               (s 3)
               (s 5)
               (s 6)
               (s 7)
               (s 8)
               (s 9)
               (s 10)
               (s 11)
               (s 12)
               (s 13)
               (s 13)
               (s 1)
               (s 1)
               (s 1)
               (s 1)
               (s 1)
               (s 1)
               (s 1)
               (s 1)
               (s 14)
               (s 15)
               (s 16)))
       (check-equal? (length case-slugs)
                     (length cases)
                     "every parity case must declare exactly one slug")
       ;; ...and the recorded table must carry EXACTLY these labels.
       (check-equal? (sort (map car cases) string<?)
                     (golden-labels parity-group)
                     "the golden table must match the parity table exactly")
       (define (check-declared-outcome! label side outcome expected)
         (cond
           [(eq? expected 'ok)
            (check-true (hash-ref outcome 'ok) (format "~a: ~a must succeed" label side))]
           [else
            (check-false (hash-ref outcome 'ok) (format "~a: ~a must refuse" label side))
            (check-equal? (hash-ref outcome 'reason #f)
                          expected
                          (format "~a: ~a reason" label side))]))
       (for ([case (in-list cases)]
             [case-slug (in-list case-slugs)])
         (define label (car case))
         (define spec (cadr case))
         (define expected (caddr case))
         (define thunk (cadddr case))
         (point-at! world case-slug)
         (define oracle-outcome (golden-outcome parity-group parity-tokens label spec))
         (define racket-outcome
           (with-handlers ([exn:fail:delivery-action? (lambda (error)
                                                        (hasheq 'ok #f 'reason (exn-message error)))])
             (hasheq 'ok #t 'value (json-value (thunk)))))
         (check-declared-outcome! label "racket" racket-outcome expected)
         (check-declared-outcome! label "recorded" oracle-outcome expected)
         (check-equal? (canonical racket-outcome) (canonical oracle-outcome) label))))))
