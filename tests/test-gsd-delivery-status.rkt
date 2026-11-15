#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers scripts/gsd-delivery-status.rkt
;; @covers scripts/gsd-delivery-pr.rkt
;; @covers scripts/gsd-delivery-api.rkt
;; @covers scripts/gsd-delivery-git.rkt
;; @timeout 300
;; Phase B slice S6: the delivery-proof `status` and the read-only `governance`
;; actions of scripts/gsd-delivery.py are ported to scripts/gsd-delivery-status.rkt.
;; The module is NOT wired into the coordinator by this slice and mutates nothing.
;;
;; Parity law under test: every refusal is a typed exn:fail:delivery-action
;; carrying the EXACT Python reason string and every positive path returns the
;; same JSON object, so a coordinator reason chain never changes.
;;
;; Fixture law (speed): every git world is built ONCE per fixture kind and
;; cached for the whole run (a build is TWO bounded `sh` scripts, not ~20
;; subprocess round trips); each case only installs its OWN fake `gh` (route
;; fixtures + argv log) over a `cp -a` copy of that cached subject, and the
;; campaign journal lives in the case's own temporary root. No case re-creates a
;; git world. Independent refusal proofs then run in PARALLEL lanes (each with
;; its own subject copy, fake `gh` and journal), so the wall clock of the suite
;; stays far below the cost of the proofs it runs.
;;
;; A REAL git origin/work/subject world backs every git read, so refresh,
;; fetch_head, rev-list, diff-tree, ls-tree, show, merge-base and the tree reads
;; are REAL commands: deleting one cannot hide behind a stub. The status proof
;; is exercised end to end: a REAL schema-2 trio is committed as the
;; single-parent protected binding publication and the unchanged strict gate
;; (scripts/gsd-wave-gate.rkt, spawned as a subprocess) really runs over
;; materialized blobs and the real excluded-path digest.
;;
;; Negative fixtures kill one guard at a time, end to end through `status`:
;; missing/mismatched journal, an unbound receipt, malformed origin/main, an
;; absent publication, a two-parent publication, receipt-head authentication,
;; campaign/wave/merge identity, implementation-sha vs merge-sha, a fetched head
;; that is not the binding head, a merge tree that is not the PR head tree, a
;; publication that changes more evidence than the current binding, trio bytes
;; changed on main, trio bytes diverged locally, and the merge/publication
;; ancestry failure. `governance` is pinned end to end for the deterministic
;; branch family (base and -r<n>), an unresolvable/ambiguous/incomplete merged
;; PR, a non-squash merge, a foreign base, a merge that never reached main and
;; its ONE trusted governance push check.
;;
;; Documented coverage split: the required-check name snapshot and branch-
;; protection enforcement are the SHARED S3 functions `status` calls (protection,
;; trusted-check); their weakened/empty/pending/untrusted/foreign-event refusals
;; are asserted here directly against one delivery-api session with its own fake
;; `gh`, because driving each of them through a full `status` proof would cost
;; minutes on a slow host. A fork/foreign PR identity refusal is likewise pinned
;; by validate_pr_identity in the S4 read-ladder suite. There is no Python
;; differential driver in this file (see the slice notes).

(require rackunit
         json
         openssl
         file/sha1
         racket/file
         racket/hash
         racket/list
         racket/path
         racket/pretty
         racket/runtime-path
         racket/string
         racket/async-channel
         "../sandbox/subprocess.rkt"
         "../scripts/gsd-delivery-git.rkt"
         "../scripts/gsd-delivery-api.rkt"
         "../scripts/gsd-delivery-pr.rkt"
         "../scripts/gsd-delivery-status.rkt")

;; Resolve the interpreter and the real git BEFORE any fixture PATH is
;; installed, so the shims can forward to the real binary.
(define racket-executable (or (find-executable-path "racket") "racket"))
(define racket-directory (path->string (path-only (path->complete-path racket-executable))))
(define real-git (path->string (or (find-executable-path "git") "/usr/bin/git")))

(define slug "coinerd/s6")
(define plan-id (make-string 64 #\a))
(define wave 1)
(define wave-branch (format "campaign/~a/w1" (substring plan-id 0 8)))
(define binding-name (format "binding/~a-w1" (substring plan-id 0 12)))
(define evidence-relative (format "docs/reports/gsd-wave-evidence/~a-w1.rktd" plan-id))
(define review-relative (format "docs/reports/gsd-wave-reviews/~a-w1.rktd" plan-id))
(define validation-relative (format "docs/reports/gsd-wave-validation/~a-w1.rktd" plan-id))
(define policy-relative "scripts/required-pr-checks.policy")
(define empty-digest (bytes->hex-string (sha256-bytes #"")))
(define governance-check "gsd-governance")
(define actions-app-id 15368)

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

;; `.rktd` artifacts are ONE Racket hash datum: the trusted writer
;; (scripts/gsd-binding-data.rkt) pretty-writes a JSON hash, so a fixture can
;; commit byte-shaped equivalents without a subprocess round trip.
(define (put-datum path datum)
  (make-directory* (path-only path))
  (call-with-output-file path #:exists 'truncate (lambda (out) (pretty-write datum out))))

(define (json-text value)
  (jsexpr->string value))

(define (git dir . args)
  (define result
    (run-subprocess "git"
                    #:args args
                    #:directory dir
                    #:environment (current-environment-variables)
                    #:timeout 60
                    #:process-group? #t))
  (unless (and (zero? (subprocess-result-exit-code result))
               (not (subprocess-result-timed-out? result)))
    (error 'fixture-git "failed ~s: ~a" args (subprocess-result-stderr result)))
  (string-trim (subprocess-result-stdout result)))

;; ============================================================
;; Real schema-2 trio fixtures (the unchanged strict gate must accept them)
;; ============================================================

(define (trio-evidence #:merge merge #:head head #:implementation implementation #:method method)
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
          'merge-method
          method
          'merge-sha
          merge
          'delivery-head-sha
          head
          'delivery-pr
          1
          'wave-branch
          wave-branch
          'branch
          wave-branch
          'merged-at
          "2026-01-01T00:00:00Z"
          'required-pr-checks
          '("ci")
          'binding-generation
          0
          'implementation-sha
          implementation
          'content-digest
          empty-digest
          'required-checks
          '("ci")
          'review-artifact
          review-relative
          'validation-artifact
          validation-relative))

(define (trio-review implementation)
  (hasheq
   'reviewer
   "independent-reviewer"
   'verdict
   "APPROVED"
   'timestamp
   "2026-01-01T00:00:00Z"
   'reviewed-sha
   implementation
   'content-digest
   empty-digest
   'scope
   "Independent review of the wave diff, the excluded-content digest and the strict gate"
   'report
   "The reviewer read the diff, recomputed the excluded digest and reran the strict trio gate."))

(define (trio-validation implementation)
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
          implementation
          'content-digest
          empty-digest
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

(define (trio-datums #:merge merge
                     #:head head
                     #:implementation implementation
                     #:method [method "squash"])
  (values (trio-evidence #:merge merge #:head head #:implementation implementation #:method method)
          (trio-review implementation)
          (trio-validation implementation)))

;; ============================================================
;; The real git worlds (ONE per fixture kind, cached for the run)
;; ============================================================

;; variants:
;;   'happy                     trio published as the single-parent binding
;;   'no-publication            the binding was never published on main
;;   'merge-publication         the publication is a two-parent merge commit
;;   'extra-evidence            the publication also adds another evidence file
;;   'merge-identity            evidence merge-method is not squash
;;   'implementation-head       evidence implementation-sha is the PR head
;;   'fetched-mismatch          refs/pull/1/head is not the binding head
;;   'tree-mismatch             the merge commit tree differs from the PR head tree
;;   'merge-not-ancestor        the claimed merge is not an ancestor of the publication
;;   'main-drift                main advances with a review edit after publication
;;   'local-drift               the subject's local evidence file is overwritten
;;
;; Variant-specific fixture files: the trio datums are computed from the phase-1
;; facts (base/head/squash), so they are written BETWEEN the two build scripts.
(define (write-fixtures! root variant facts)
  (define head (hash-ref facts 'head))
  (define base (hash-ref facts 'base))
  (define squash (hash-ref facts 'squash))
  (define claimed (if (eq? variant 'merge-not-ancestor) head squash))
  (define implementation (if (eq? variant 'implementation-head) head claimed))
  (define-values (evidence review validation)
    (trio-datums #:merge claimed
                 #:head head
                 #:implementation implementation
                 #:method (if (eq? variant 'merge-identity) "rebase" "squash")))
  (define dir (build-path root "fixtures"))
  (make-directory* dir)
  (put-datum (build-path dir "evidence") evidence)
  (put-datum (build-path dir "review") review)
  (put-datum (build-path dir "validation") validation)
  (put-datum (build-path dir "evidence-extra") (hasheq 'schema-version 2 'wave "W0"))
  (put-datum (build-path dir "review-drifted") (hash-set review 'note "later edit"))
  (hasheq 'evidence
          evidence
          'review
          review
          'validation
          validation
          'base
          base
          'head
          head
          'squash
          squash
          'claimed-merge
          claimed))

(define (script-text lines)
  (string-append "#!/bin/sh\nset -e\n" (string-join lines "\n") "\n"))

(define (run-script! root name lines)
  (define path (build-path root name))
  (put path (script-text lines))
  (define result
    (run-subprocess "/bin/sh"
                    #:args (list (path->string path))
                    #:directory root
                    #:environment (current-environment-variables)
                    #:timeout 180
                    #:process-group? #t))
  (unless (and (zero? (subprocess-result-exit-code result))
               (not (subprocess-result-timed-out? result)))
    (error 'fixture-world "~a failed: ~a" name (subprocess-result-stderr result))))

(define (fact-key name)
  (case (string-downcase name)
    [("pub") 'publication]
    [else (string->symbol (string-downcase name))]))

(define (read-facts path)
  (define lines
    (if (file-exists? path)
        (file->lines path)
        '()))
  (for/fold ([acc (hash)]) ([line (in-list lines)])
    (define parts (string-split line "="))
    (hash-set acc (fact-key (car parts)) (cadr parts))))

(define script-preamble
  (list "export GIT_AUTHOR_NAME=Test"
        "export GIT_AUTHOR_EMAIL=test@example.invalid"
        "export GIT_COMMITTER_NAME=Test"
        "export GIT_COMMITTER_EMAIL=test@example.invalid"
        "export GIT_CONFIG_NOSYSTEM=1"
        "export GIT_CONFIG_GLOBAL=/dev/null"
        "export GIT_ALLOW_PROTOCOL=file"
        "ROOT=$(pwd)"))

;; Phase 1: origin + the implementation branch + the protected squash merge.
;; ONE bounded subprocess runs every git command, so a whole world costs one
;; harness round trip instead of ~20 (the suite's dominant cost).
(define (build-world-phase-1! root variant)
  (run-script!
   root
   "build-1.sh"
   (append
    script-preamble
    (list
     "git init -q --bare -b main origin.git"
     "git init -q -b main work"
     "cd work"
     "printf 'initial\\n' > tracked"
     "mkdir -p scripts"
     "printf '(\"ci\")\\n' > scripts/required-pr-checks.policy"
     "git add -A"
     "git commit -q -m 'initial policy'"
     "BASE=$(git rev-parse HEAD)"
     "git remote add origin ../origin.git"
     (format "git checkout -q -b ~a \"$BASE\"" wave-branch)
     "printf 'impl\\n' > feature"
     "git add feature"
     "git commit -q -m impl"
     "HEAD_SHA=$(git rev-parse HEAD)"
     (format "git push -q origin ~a" wave-branch)
     (format "git push -q origin \"~a:refs/pull/1/head\""
             (if (eq? variant 'fetched-mismatch) "$BASE" "$HEAD_SHA"))
     "git checkout -q main"
     (if (eq? variant 'tree-mismatch)
         (string-join
          (list "TREE=$(git rev-parse \"$BASE^{tree}\")"
                "git reset -q --hard \"$(git commit-tree \"$TREE\" -p \"$BASE\" -m 'squash merge')\"")
          "\n")
         (string-join (list (format "git merge -q --squash ~a" wave-branch)
                            "git commit -q -m 'squash merge'")
                      "\n"))
     "SQUASH=$(git rev-parse HEAD)"
     "HEAD_TREE=$(git rev-parse \"$HEAD_SHA^{tree}\")"
     (string-append "printf 'BASE=%s\\nHEAD=%s\\nSQUASH=%s\\nHEAD-TREE=%s\\n' "
                    "\"$BASE\" \"$HEAD_SHA\" \"$SQUASH\" \"$HEAD_TREE\" > \"$ROOT/facts-1.txt\"")))))

;; Phase 2: the binding publication, any topology variant, and the subject
;; clone with the configured GitHub origin URL (insteadOf keeps transport local).
(define (build-world-phase-2! root variant)
  (define evidence-file "../fixtures/evidence")
  (define copy-trio
    (list (format "cp ~a ~a" evidence-file evidence-relative)
          (format "cp ../fixtures/review ~a" review-relative)
          (format "cp ../fixtures/validation ~a" validation-relative)))
  (define mkdirs
    (list (format "mkdir -p ~a" (path->string (path-only (string->path evidence-relative))))
          (format "mkdir -p ~a" (path->string (path-only (string->path review-relative))))
          (format "mkdir -p ~a" (path->string (path-only (string->path validation-relative))))))
  (define extra-evidence
    (if (eq? variant 'extra-evidence)
        (list (format "cp ../fixtures/evidence-extra docs/reports/gsd-wave-evidence/~a-w0.rktd"
                      plan-id))
        '()))
  (define publication-lines
    (cond
      [(eq? variant 'no-publication) '("true")]
      [(eq? variant 'merge-publication)
       (append (list "git checkout -q -b side \"$SQUASH\""
                     "printf 'side\\n' > side-file"
                     "git add -A"
                     "git commit -q -m side"
                     "git checkout -q main"
                     "git merge -q --no-ff --no-commit side")
               mkdirs
               copy-trio
               (list "git add -A" "git commit -q -m 'publication merge'"))]
      [else
       (append mkdirs
               copy-trio
               extra-evidence
               (list "git add -A" "git commit -q -m 'binding trio'"))]))
  (define drift-lines
    (if (eq? variant 'main-drift)
        (list (format "cp ../fixtures/review-drifted ~a" review-relative)
              "git add -A"
              "git commit -q -m 'later review edit'")
        '()))
  (run-script!
   root
   "build-2.sh"
   (append
    script-preamble
    (list "cd work"
          "BASE=$(sed -n 's/^BASE=//p' \"$ROOT/facts-1.txt\")"
          "SQUASH=$(git rev-parse main)")
    publication-lines
    (list "PUB=$(git rev-parse HEAD)")
    drift-lines
    (list "git push -q origin main"
          "cd \"$ROOT\""
          "git clone -q origin.git subject"
          "cd subject"
          "git config user.name Test"
          "git config user.email test@example.invalid"
          (format "git config remote.origin.url 'https://github.com/~a'" slug)
          (format "git config 'url.~a.insteadOf' 'https://github.com/~a'"
                  (path->string (build-path root "origin.git"))
                  slug)
          "printf 'PUB=%s\\n' \"$PUB\" > \"$ROOT/facts-2.txt\""))))

(define (build-world! root variant)
  (build-world-phase-1! root variant)
  (define facts-1 (read-facts (build-path root "facts-1.txt")))
  (define datums (write-fixtures! root variant facts-1))
  (build-world-phase-2! root variant)
  (define facts-2 (read-facts (build-path root "facts-2.txt")))
  (define subject (build-path root "subject"))
  (when (eq? variant 'local-drift)
    (put (build-path subject evidence-relative) "{}\n"))
  (define facts
    (hash-union (hash-union datums facts-1 #:combine (lambda (a b) b))
                facts-2
                #:combine (lambda (a b) b)))
  (hash-set (hash-set facts 'work (build-path root "work")) 'subject subject))

(define world-cache (make-hash))

(define world-lock (make-semaphore 1))

;; A world is built ONCE per fixture kind; the lock keeps concurrent lanes from
;; racing the cache (a build is one bounded subprocess per phase).
(define (shared-world variant)
  (call-with-semaphore world-lock
                       (lambda ()
                         (hash-ref! world-cache
                                    variant
                                    (lambda ()
                                      (define root
                                        (make-temporary-file "native-status-world-~a" 'directory))
                                      (define env (fixture-env root "/usr/bin:/bin"))
                                      (parameterize ([current-environment-variables env])
                                        (check-not-false (find-executable-path "git")
                                                         "the fixture must resolve git through PATH")
                                        (build-world! root variant)))))))

;; ============================================================
;; Journal fixture
;; ============================================================

(define (write-journal! root world mode)
  (define receipt
    (hasheq 'repo
            slug
            'branch
            (if (eq? mode 'receipt-branch) "campaign/foreign" wave-branch)
            'origin
            (format "https://github.com/~a" slug)
            'evidence
            evidence-relative
            'head
            (if (eq? mode 'receipt-head)
                (hash-ref world 'base)
                (hash-ref world 'head))
            'tree
            (hash-ref world 'head-tree)
            'verified-at
            1
            'attempt-id
            (if (eq? mode 'no-attempt) "" "attempt-7")
            'attempt-fence
            3))
  (define journal
    (hasheq 'schema-version
            1
            'plan-id
            (if (eq? mode 'mismatched)
                (make-string 64 #\b)
                plan-id)
            'wave
            wave
            'stage
            "governance"
            'receipt
            receipt))
  (put (build-path root ".planning" "campaigns" plan-id "coordinator-w1.json")
       (string-append (json-text journal) "\n")))

;; ============================================================
;; GitHub fixtures
;; ============================================================

(define (route slug* path)
  (format "repos/~a/~a" slug* path))

(define (check-runs-route sha)
  (format "commits/~a/check-runs?per_page=100&page=1" sha))

(define (merged-pr-json #:number number
                        #:merge merge
                        #:head-sha head-sha
                        #:head-ref head-ref
                        #:slug slug*
                        #:state [state "closed"]
                        #:head-repo [head-repo slug*]
                        #:base-repo [base-repo slug*])
  (hasheq 'number
          number
          'state
          state
          'merged
          #t
          'merged_at
          "2026-01-01T00:00:00Z"
          'merge_commit_sha
          merge
          'head
          (hasheq 'ref head-ref 'sha head-sha 'repo (hasheq 'full_name head-repo))
          'base
          (hasheq 'ref "main" 'sha (make-string 40 #\c) 'repo (hasheq 'full_name base-repo))))

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

(define (check-run-json #:name name
                        #:sha sha
                        #:run-id run-id
                        #:slug slug*
                        #:app-id [app-id actions-app-id]
                        #:status [status "completed"]
                        #:conclusion [conclusion "success"]
                        #:path [path ".github/workflows/ci.yml"])
  (hasheq 'id
          11
          'name
          name
          'status
          status
          'conclusion
          conclusion
          'head_sha
          sha
          'app
          (hasheq 'id app-id)
          'details_url
          (format "https://github.com/~a/actions/runs/~a/job/9001" slug* run-id)
          'workflow-path
          path))

(define (workflow-run-json #:id id
                           #:sha sha
                           #:slug slug*
                           #:event event
                           #:branch branch
                           #:status [status "completed"]
                           #:conclusion [conclusion "success"])
  (hasheq 'id
          id
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
          (hasheq 'full_name slug*)))

(define (check-runs-json runs)
  (hasheq 'total_count (length runs) 'check_runs runs))

;; The complete happy-path fixture set for the status proof.
(define (status-fixtures world)
  (define head (hash-ref world 'head))
  (define claimed (hash-ref world 'claimed-merge))
  (define publication (hash-ref world 'publication))
  (define base
    (list
     (cons (route slug "pulls/1")
           (json-text (merged-pr-json #:number 1
                                      #:merge claimed
                                      #:head-sha head
                                      #:head-ref wave-branch
                                      #:slug slug)))
     (cons (route slug (format "commits/~a" claimed))
           (json-text (commit-json claimed (list (hash-ref world 'base)))))
     (cons (route slug "branches/main/protection") (json-text (protection-json)))
     (cons (route slug (check-runs-route head))
           (json-text (check-runs-json
                       (list (check-run-json #:name "ci" #:sha head #:run-id 1001 #:slug slug)))))
     (cons (route slug "actions/runs/1001")
           (json-text (workflow-run-json #:id 1001
                                         #:sha head
                                         #:slug slug
                                         #:event "pull_request"
                                         #:branch wave-branch)))))
  (if publication
      (append base
              (list (cons (route slug (format "commits/~a/pulls" publication))
                          (json-text (list (merged-pr-json #:number 2
                                                           #:merge publication
                                                           #:head-sha head
                                                           #:head-ref binding-name
                                                           #:slug slug))))
                    (cons (route slug (check-runs-route publication))
                          (json-text (check-runs-json (list (check-run-json #:name governance-check
                                                                            #:sha publication
                                                                            #:run-id 2002
                                                                            #:slug slug)))))
                    (cons (route slug "actions/runs/2002")
                          (json-text (workflow-run-json #:id 2002
                                                        #:sha publication
                                                        #:slug slug
                                                        #:event "push"
                                                        #:branch "main")))))
      base))

;; The complete happy-path fixture set for the governance proof (the governance
;; check runs on the MERGE commit; governance reads no trio at all).
(define (governance-fixtures world [branch* binding-name])
  (define head (hash-ref world 'head))
  (define squash (hash-ref world 'squash))
  (list
   (cons
    (format "repos/~a/pulls?state=all&head=coinerd:~a" slug branch*)
    (json-text
     (list
      (merged-pr-json #:number 2 #:merge squash #:head-sha head #:head-ref branch* #:slug slug))))
   (cons (route slug (format "commits/~a" squash))
         (json-text (commit-json squash (list (hash-ref world 'base)))))
   (cons (route slug (check-runs-route squash))
         (json-text
          (check-runs-json
           (list (check-run-json #:name governance-check #:sha squash #:run-id 3003 #:slug slug)))))
   (cons (route slug "actions/runs/3003")
         (json-text
          (workflow-run-json #:id 3003 #:sha squash #:slug slug #:event "push" #:branch "main")))))

;; ============================================================
;; Fake `gh` (and optional logging `git` forwarder)
;; ============================================================

(define record-separator (string #\u001E))
(define unit-separator (string #\u001F))

(define (argv-log-snippet marker)
  (format (string-append "{ printf '%s' ~a"
                         "; for a in \"$@\"; do printf '\\037%s' \"$a\"; done"
                         "; printf '\\036'; } >> \"$STATUS_SHIM_LOG\"")
          marker))

(define (gh-shim-script routes)
  (string-append
   "#!/bin/sh\n"
   (argv-log-snippet "gh")
   "\nroute=\n"
   "for arg in \"$@\"; do case \"$arg\" in repos/*) route=$arg ;; esac; done\n"
   "case \"$route\" in\n"
   (string-join
    (append (for/list ([entry (in-list routes)])
              (format "  ~s) /bin/cat \"$GH_SHIM_FIXTURES/~a\" ;;" (car entry) (cdr entry)))
            (list "  *) printf 'no fixture for route %s\\n' \"$route\" >&2; exit 66 ;;"))
    "\n")
   "\nesac\n"))

;; A real-git forwarder that records the argv first. GIT_OVERRIDE_MAIN is the
;; fault-injection seam for the defensive "malformed origin/main head" guard.
(define (git-forwarder-script)
  (string-append "#!/bin/sh\n"
                 (argv-log-snippet "git")
                 "\nif [ -n \"$GIT_OVERRIDE_MAIN\" ]; then\n"
                 "  case \"$*\" in\n"
                 "    *\"rev-parse refs/remotes/origin/main\"*)"
                 " printf '%s\\n' \"$GIT_OVERRIDE_MAIN\"; exit 0 ;;\n"
                 "  esac\n"
                 "fi\n"
                 (format "exec ~s \"$@\"\n" real-git)))

(define (read-records log)
  (if (file-exists? log)
      (for/list ([record (in-list (string-split (file->string log) record-separator))]
                 #:unless (string=? record ""))
        (string-split record unit-separator))
      '()))

(define (invocations log [program #f])
  (for/list ([record (in-list (read-records log))]
             #:when (or (not program) (equal? (car record) program)))
    (cdr record)))

(define mutating-git-subcommands
  '("push" "commit" "commit-tree" "add" "reset" "merge" "checkout" "switch" "rm" "tag"))

;; Any state-changing invocation the actions could possibly have made.
(define (mutations log)
  (for/list ([record (in-list (read-records log))]
             #:when (let ([program (car record)]
                          [argv (cdr record)])
                      (cond
                        [(equal? program "gh") (and (member "-X" argv) #t)]
                        [(equal? program "git")
                         (for/or ([arg (in-list argv)])
                           (and (member arg mutating-git-subcommands) #t))]
                        [else #f])))
    record))

(define (install-shims! root fixtures #:log-git? [log-git? #f] #:override-main [override-main #f])
  (define bin (build-path root "bin"))
  (define fixtures-dir (build-path root "fixtures"))
  (make-directory* bin)
  (make-directory* fixtures-dir)
  (define routes
    (for/list ([entry (in-list fixtures)]
               [index (in-naturals)])
      (define file (format "fixture-~a.json" index))
      (put (build-path fixtures-dir file) (cdr entry))
      (cons (car entry) file)))
  (define gh (build-path bin "gh"))
  (put gh (gh-shim-script routes))
  (file-or-directory-permissions gh #o700)
  (when (or log-git? override-main)
    (define git-shim (build-path bin "git"))
    (put git-shim (git-forwarder-script))
    (file-or-directory-permissions git-shim #o700))
  (define env (fixture-env root (format "~a:~a:/usr/bin:/bin" bin racket-directory)))
  (environment-set! env "GH_SHIM_FIXTURES" (path->string fixtures-dir))
  (environment-set! env "STATUS_SHIM_LOG" (path->string (build-path root "shim.log")))
  (when override-main
    (environment-set! env "GIT_OVERRIDE_MAIN" override-main))
  env)

;; ============================================================
;; Per-case runner over a CACHED world
;; ============================================================

;; The cached world's subject is IMMUTABLE, but a status/governance proof runs
;; `git fetch` (which writes FETCH_HEAD and the tracking ref), so every case
;; drives its own `cp -a` copy of the subject. That keeps cases independent (and
;; makes running several cases concurrently safe) for one cheap subprocess.
(define (copy-subject! world root)
  (define dst (build-path root "repo"))
  (define result
    (run-subprocess "/bin/cp"
                    #:args (list "-a" (path->string (hash-ref world 'subject)) (path->string dst))
                    #:environment (current-environment-variables)
                    #:timeout 120
                    #:process-group? #t))
  (unless (and (zero? (subprocess-result-exit-code result))
               (not (subprocess-result-timed-out? result)))
    (error 'fixture-copy "subject copy failed: ~a" (subprocess-result-stderr result)))
  dst)

;; `fixtures` is a list of (route . body), a procedure of the world, or #f for
;; the default status set. The journal (and any local drift) is per-case state in
;; the case's own temporary root, never in the cached world.
(define (with-world variant
                    proc
                    #:fixtures [fixtures #f]
                    #:journal [journal 'valid]
                    #:override-main [override-main #f]
                    #:log-git? [log-git? #f])
  (define cached (shared-world variant))
  (define root (make-temporary-file "native-status-case-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define world (hash-set cached 'subject (copy-subject! cached root)))
     (unless (eq? journal 'none)
       (write-journal! root world journal))
     (define installed
       (cond
         [(procedure? fixtures) (fixtures world)]
         [fixtures fixtures]
         [else (status-fixtures world)]))
     (define env (install-shims! root installed #:log-git? log-git? #:override-main override-main))
     (parameterize ([current-environment-variables env])
       (check-not-false (find-executable-path "gh") "the fixture must resolve gh through PATH")
       (proc world root (build-path root "shim.log"))))
   (lambda () (delete-directory/files root))))

;; ============================================================
;; Refusal helpers
;; ============================================================

(define (delivery-refusal thunk)
  (with-handlers ([exn:fail:delivery-action? exn-message])
    (thunk)
    (fail "expected a typed exn:fail:delivery-action refusal, but the call returned")))

(define (status-refusal world root)
  (delivery-refusal
   (lambda () (delivery-status! (hash-ref world 'subject) plan-id wave #:campaign-root root))))

(define (governance-refusal world [branch* binding-name])
  (delivery-refusal (lambda ()
                      (delivery-governance! (hash-ref world 'subject) plan-id wave branch*))))

;; ============================================================
;; Parallel refusal jobs (each job is one whole status/governance proof)
;; ============================================================

;; A proof run spawns ~50 bounded subprocesses and waits on them, so several
;; INDEPENDENT refusal cases are driven concurrently over their own subject copy
;; and their own fake `gh`. Assertions still run in the test thread: a job only
;; returns its outcome (value, typed refusal, or unexpected error).
(struct job (label variant run fixtures journal override-main expected) #:transparent)

(define (job* label
              variant
              expected
              run
              #:fixtures [fixtures #f]
              #:journal [journal 'valid]
              #:override-main [override-main #f])
  (job label variant run fixtures journal override-main expected))

(define (capture thunk)
  (with-handlers ([exn:fail:delivery-action? (lambda (error) (cons 'refusal (exn-message error)))]
                  [exn:fail? (lambda (error) (cons 'error (exn-message error)))])
    (cons 'value (thunk))))

(define (run-job a-job)
  (with-world (job-variant a-job)
              (lambda (world root log) (capture (lambda () ((job-run a-job) world root log))))
              #:fixtures (job-fixtures a-job)
              #:journal (job-journal a-job)
              #:override-main (job-override-main a-job)))

(define (run-jobs jobs [lanes 6])
  (define channel (make-async-channel))
  (define buckets (make-vector lanes '()))
  (for ([a-job (in-list jobs)]
        [index (in-naturals)])
    (define lane (modulo index lanes))
    (vector-set! buckets lane (cons a-job (vector-ref buckets lane))))
  (define active 0)
  (for ([bucket (in-vector buckets)])
    (when (pair? bucket)
      (set! active (add1 active))
      (thread (lambda ()
                (for ([a-job (in-list (reverse bucket))])
                  (define outcome
                    (with-handlers ([exn:fail? (lambda (error) (cons 'error (exn-message error)))])
                      (run-job a-job)))
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

;; Assert every job's DECLARED outcome: 'ok, or the exact Python reason string.
(define (check-jobs! jobs)
  (define results (run-jobs jobs))
  (check-equal? (length results) (length jobs) "every job must report")
  (for ([result (in-list results)])
    (define label (car result))
    (define a-job
      (for/first ([candidate (in-list jobs)]
                  #:when (equal? (job-label candidate) label))
        candidate))
    (define outcome (cdr result))
    (define expected (and a-job (job-expected a-job)))
    (cond
      [(and a-job (eq? expected 'ok))
       (check-equal? (car outcome) 'value (format "~a must succeed: ~s" label outcome))]
      [else
       (check-equal? outcome
                     (cons 'refusal expected)
                     (format "~a must refuse with the exact reason (got ~s)" label outcome))])))

;; ============================================================
;; Fixture-set overrides and the two actions as job thunks
;; ============================================================

;; Replace (or append) one route's payload in an installed fixture set.
(define (fixtures-with old route* body)
  (cons (cons route* body)
        (for/list ([entry (in-list old)]
                   #:unless (equal? (car entry) route*))
          entry)))

(define (governance-fixtures-with world route* body [branch* binding-name])
  (fixtures-with (governance-fixtures world branch*) route* body))

;; The job thunks: a refusal propagates to `capture`, a success returns the
;; payload, so one table can declare both kinds of outcome.
(define (status-run world root log)
  (delivery-status! (hash-ref world 'subject) plan-id wave #:campaign-root root))

(define (governance-run world root log)
  (delivery-governance! (hash-ref world 'subject) plan-id wave binding-name))

(define (governance-run-fixtures world payload)
  (governance-fixtures-with world (route slug "actions/runs/3003") (json-text payload)))

;; A `gh`-only case root: the shared S3 gates are exercised directly against
;; their own fake-`gh` fixtures (no repository read at all).
(define (with-gh-only fixtures proc)
  (define root (make-temporary-file "native-status-gh-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (define env (install-shims! root fixtures))
                  (parameterize ([current-environment-variables env])
                    (proc (make-delivery-api-session #f) root)))
                (lambda () (delete-directory/files root))))

(module+ test
  ;; ============================================================
  ;; status: the delivery proof
  ;; ============================================================

  (test-case "status proves delivery with the exact payload and reads the proof"
    (with-world
     'happy
     (lambda (world root log)
       (define result (delivery-status! (hash-ref world 'subject) plan-id wave #:campaign-root root))
       (check-equal? result
                     (hasheq 'status
                             "delivered"
                             'merge-sha
                             (hash-ref world 'squash)
                             'publication-sha
                             (hash-ref world 'publication)
                             'plan-id
                             plan-id
                             'wave
                             wave
                             'delivery-head-sha
                             (hash-ref world 'head)
                             'delivery-branch
                             wave-branch
                             'binding-generation
                             0
                             'attempt-id
                             "attempt-7"
                             'attempt-fence
                             3))
       ;; the payload keys are EXACTLY the Python ones
       (check-equal? (sort (map symbol->string (hash-keys result)) string<?)
                     '("attempt-fence" "attempt-id"
                                       "binding-generation"
                                       "delivery-branch"
                                       "delivery-head-sha"
                                       "merge-sha"
                                       "plan-id"
                                       "publication-sha"
                                       "status"
                                       "wave"))
       ;; the refreshed main, the fetched PR ref and rev-list/pr reads
       ;; really ran (a stub could not fabricate these)
       (define git-reads (invocations log "git"))
       ;; a record is ("git" "-C" <repo> <args> ...)
       (check-not-false (for/or ([argv (in-list git-reads)])
                          (member "refs/heads/main:refs/remotes/origin/main" argv)))
       (check-not-false (for/or ([argv (in-list git-reads)])
                          (member "refs/pull/1/head" argv)))
       (check-not-false
        (for/or ([argv (in-list git-reads)])
          (equal? (drop argv 2)
                  (list "rev-list" "--parents" "-n" "1" (hash-ref world 'publication)))))
       ;; one trusted check was read for the head and one for the
       ;; publication push
       (define gh-reads (invocations log "gh"))
       (check-not-false (for/or ([argv (in-list gh-reads)])
                          (equal? (last argv)
                                  (route slug (check-runs-route (hash-ref world 'head))))))
       (check-not-false (for/or ([argv (in-list gh-reads)])
                          (equal? (last argv)
                                  (route slug (check-runs-route (hash-ref world 'publication))))))
       (check-equal? (mutations log) '() "status must never mutate"))
     #:log-git? #t))

  (test-case "governance verifies the merged binding publication without mutating"
    (with-world
     'happy
     (lambda (world root log)
       (check-equal? (delivery-governance! (hash-ref world 'subject) plan-id wave binding-name)
                     (hasheq 'status
                             "governed"
                             'pr
                             2
                             'branch
                             binding-name
                             'publication-sha
                             (hash-ref world 'squash)))
       (check-equal? (mutations log) '() "governance must never mutate"))
     #:fixtures (lambda (world) (governance-fixtures world))
     #:log-git? #t)))

;; ============================================================
;; status refusals (parallel independent proofs, one cached world each)
;; ============================================================

(test-case "status refuses a missing, mismatched or unbound campaign journal"
  (check-jobs! (list (job* "journal-missing"
                           'happy
                           "delivery proof requires the authoritative coordinator journal"
                           status-run
                           #:journal 'none)
                     (job* "journal-mismatched"
                           'happy
                           (string-append
                            "delivery command failed (racket, exit 1); "
                            "inspect authentication, repository refs and required artifacts locally")
                           status-run
                           #:journal 'mismatched)
                     (job* "journal-no-attempt"
                           'happy
                           "delivery proof requires an attempt-bound Verify receipt"
                           status-run
                           #:journal 'no-attempt))))

(test-case "status refuses a malformed origin/main head and an absent or non-single-parent publication"
  (check-jobs!
   (list (job* "malformed-main"
               'happy
               "malformed origin/main head"
               status-run
               #:override-main "not-a-sha")
         (job* "absent-publication"
               'no-publication
               (format "W~a verified; delivery pending: protected campaign binding is absent" wave)
               status-run)
         (job* "non-single-parent"
               'merge-publication
               "binding publication is not a single-parent protected commit"
               status-run))))

(test-case "status refuses receipt, campaign, implementation, publication and trio mismatches"
  (define ancestry-refusal
    (string-append "delivery command failed (git, exit 1); "
                   "inspect authentication, repository refs and required artifacts locally"))
  (check-jobs!
   (list (job* "receipt-head"
               'happy
               "published binding does not authenticate the current receipt head/branch"
               status-run
               #:journal 'receipt-head)
         (job* "merge-identity" 'merge-identity "campaign/wave/merge identity mismatch" status-run)
         (job* "implementation-sha"
               'implementation-head
               "binding implementation-sha differs from merge-sha (squash semantics violated)"
               status-run)
         (job* "fetched-mismatch"
               'fetched-mismatch
               "fetched implementation head does not match binding delivery-head-sha"
               status-run)
         (job* "tree-mismatch"
               'tree-mismatch
               "implementation merge tree differs from PR head tree (not a clean squash)"
               status-run)
         (job* "extra-paths"
               'extra-evidence
               (string-append "publication must change exactly the current binding evidence "
                              "(possible governance green-skip refused)")
               status-run)
         (job* "trio-changed-on-main"
               'main-drift
               "binding trio changed since governance publication"
               status-run)
         (job* "trio-changed-locally"
               'local-drift
               "delivery sync pending: local artifact differs from committed proof"
               status-run)
         (job* "merge-not-ancestor" 'merge-not-ancestor ancestry-refusal status-run))))

(test-case "the protection and trusted-check gates status calls refuse weakened coverage"
  ;; These are the SHARED S3 gates `status` invokes with the binding's own
  ;; name snapshot / head / branch / publication push; `status` itself only
  ;; wires them (see the happy path's per-name trusted-check reads).
  (define sha (make-string 40 #\d))
  (define (refusal-with fixtures thunk)
    (with-gh-only fixtures (lambda (session root) (delivery-refusal (lambda () (thunk session))))))
  (define (protection-refusal payload)
    (refusal-with (list (cons (route slug "branches/main/protection") (json-text payload)))
                  (lambda (session) ((delivery-api-session-protection session) slug '("ci")))))
  (check-equal? (protection-refusal (protection-json #:enforce-admins #f))
                "default branch protection is absent or weakened")
  (check-equal? (protection-refusal (protection-json #:strict #f))
                "default branch protection is absent or weakened")
  (check-equal? (protection-refusal (protection-json #:contexts '()))
                "default branch protection requires configured native checks")
  (check-equal? (protection-refusal (protection-json #:contexts '("other")))
                "branch protection does not require expected checks: ci")
  (define (trusted-check-refusal check-run run)
    (refusal-with (append (list (cons (route slug (check-runs-route sha))
                                      (json-text (check-runs-json (list check-run)))))
                          (if run
                              (list (cons (route slug "actions/runs/11") (json-text run)))
                              '()))
                  (lambda (session)
                    ((delivery-api-session-trusted-check session) slug
                                                                  sha
                                                                  governance-check
                                                                  "push"
                                                                  #:branch "main"))))
  (check-equal? (refusal-with (list (cons (route slug (check-runs-route sha))
                                          (json-text (check-runs-json '()))))
                              (lambda (session)
                                ((delivery-api-session-trusted-check session) slug
                                                                              sha
                                                                              governance-check
                                                                              "push"
                                                                              #:branch "main")))
                "missing check: gsd-governance")
  (check-equal? (trusted-check-refusal (check-run-json #:name governance-check
                                                       #:sha sha
                                                       #:run-id 11
                                                       #:slug slug
                                                       #:status "in_progress"
                                                       #:conclusion #f)
                                       #f)
                "pending, failed, or untrusted check: gsd-governance")
  (check-equal?
   (trusted-check-refusal
    (check-run-json #:name governance-check #:sha sha #:run-id 11 #:slug slug #:app-id 999)
    #f)
   "pending, failed, or untrusted check: gsd-governance")
  ;; the workflow run behind the check must be a push run on main
  (check-equal?
   (trusted-check-refusal
    (check-run-json #:name governance-check #:sha sha #:run-id 11 #:slug slug)
    (workflow-run-json #:id 11 #:sha sha #:slug slug #:event "pull_request" #:branch "main"))
   "check workflow/ref/event mismatch: gsd-governance")
  (check-equal?
   (trusted-check-refusal
    (check-run-json #:name governance-check #:sha sha #:run-id 11 #:slug slug)
    (workflow-run-json #:id 11 #:sha sha #:slug slug #:event "push" #:branch wave-branch))
   "check workflow/ref/event mismatch: gsd-governance"))

;; ============================================================
;; governance refusals (read-only, cheap: no trio gate)
;; ============================================================

(test-case "governance refuses a branch that is not the deterministic binding branch"
  (for ([branch* (in-list (list "campaign/w1" (format "~a-r0" binding-name)))])
    (check-equal?
     (delivery-refusal (lambda ()
                         (delivery-governance! "/nonexistent-status-fixture" plan-id wave branch*)))
     "governance branch does not match the deterministic binding branch"))
  (check-equal?
   (delivery-refusal (lambda () (delivery-governance! "/nonexistent-status-fixture" plan-id wave "")))
   "governance requires --expected-branch"))

(test-case "governance accepts every -r<n> republication branch"
  (define results
    (run-jobs (for/list ([branch* (in-list (list (format "~a-r1" binding-name)
                                                 (format "~a-r27" binding-name)))])
                (job* (format "republication ~a" branch*)
                      'happy
                      'ok
                      (lambda (world root log)
                        (delivery-governance! (hash-ref world 'subject) plan-id wave branch*))
                      #:fixtures (lambda (world) (governance-fixtures world branch*))))
              2))
  (for ([result (in-list results)])
    (check-equal? (car (cdr result)) 'value (format "~a" result))))

(test-case "governance refuses an unresolvable, ambiguous or incomplete merged PR"
  (define query (format "repos/~a/pulls?state=all&head=coinerd:~a" slug binding-name))
  (check-jobs!
   (list
    (job* "unresolvable"
          'happy
          (string-append "governance cannot self-resolve exactly one merged binding PR for branch "
                         binding-name)
          governance-run
          #:fixtures (lambda (world) (governance-fixtures-with world query "[]")))
    (job* "ambiguous"
          'happy
          (string-append "multiple merged pull requests already target branch " binding-name)
          governance-run
          #:fixtures
          (lambda (world)
            (define pr
              (merged-pr-json #:number 2
                              #:merge (hash-ref world 'squash)
                              #:head-sha (hash-ref world 'head)
                              #:head-ref binding-name
                              #:slug slug))
            (governance-fixtures-with world query (json-text (list pr (hash-set pr 'number 3))))))
    (job* "incomplete"
          'happy
          "merged binding PR has incomplete identity"
          governance-run
          #:fixtures (lambda (world)
                       (governance-fixtures-with
                        world
                        query
                        (json-text (list (merged-pr-json #:number 2
                                                         #:merge "deadbeef"
                                                         #:head-sha (hash-ref world 'head)
                                                         #:head-ref binding-name
                                                         #:slug slug)))))))))

(test-case "governance refuses a non-squash merge, a foreign base and broken ancestry"
  (define query (format "repos/~a/pulls?state=all&head=coinerd:~a" slug binding-name))
  (check-jobs!
   (list
    (job* "two-parent-merge"
          'happy
          "delivery must have single-parent squash provenance"
          governance-run
          #:fixtures
          (lambda (world)
            (governance-fixtures-with world
                                      (route slug (format "commits/~a" (hash-ref world 'squash)))
                                      (json-text (commit-json (hash-ref world 'squash)
                                                              (list (hash-ref world 'base)
                                                                    (hash-ref world 'base)))))))
    (job* "foreign-base"
          'happy
          "implementation repository/default branch mismatch"
          governance-run
          #:fixtures (lambda (world)
                       (governance-fixtures-with
                        world
                        query
                        (json-text (list (merged-pr-json #:number 2
                                                         #:merge (hash-ref world 'squash)
                                                         #:head-sha (hash-ref world 'head)
                                                         #:head-ref binding-name
                                                         #:slug slug
                                                         #:base-repo "other/q"))))))
    (job* "merge-not-on-main"
          'happy
          (string-append "delivery command failed (git, exit 1); "
                         "inspect authentication, repository refs and required artifacts locally")
          governance-run
          #:fixtures
          (lambda (world)
            (define orphan (hash-ref world 'head))
            (fixtures-with
             (governance-fixtures-with world
                                       query
                                       (json-text (list (merged-pr-json #:number 2
                                                                        #:merge orphan
                                                                        #:head-sha orphan
                                                                        #:head-ref binding-name
                                                                        #:slug slug))))
             (route slug (format "commits/~a" orphan))
             (json-text (commit-json orphan (list (hash-ref world 'base)))))))))

  (test-case "governance requires one trusted governance push check on the merge commit"
    (check-jobs!
     (list (job* "check-missing"
                 'happy
                 "missing check: gsd-governance"
                 governance-run
                 #:fixtures
                 (lambda (world)
                   (governance-fixtures-with world
                                             (route slug (check-runs-route (hash-ref world 'squash)))
                                             (json-text (hasheq 'total_count 0 'check_runs '())))))
           (job* "check-failed"
                 'happy
                 "pending, failed, or untrusted check: gsd-governance"
                 governance-run
                 #:fixtures
                 (lambda (world)
                   (governance-fixtures-with
                    world
                    (route slug (check-runs-route (hash-ref world 'squash)))
                    (json-text (check-runs-json (list (check-run-json #:name governance-check
                                                                      #:sha (hash-ref world 'head)
                                                                      #:run-id 3003
                                                                      #:slug slug
                                                                      #:conclusion "failure")))))))
           (job* "check-not-push"
                 'happy
                 "check workflow/ref/event mismatch: gsd-governance"
                 governance-run
                 #:fixtures
                 (lambda (world)
                   (governance-run-fixtures world
                                            (workflow-run-json #:id 3003
                                                               #:sha (hash-ref world 'squash)
                                                               #:slug slug
                                                               #:event "pull_request"
                                                               #:branch "main")))))))

  ;; ============================================================
  ;; CLI surface (pure: no repository or network read)
  ;; ============================================================

  (test-case "the status CLI refuses unsupported actions and missing flags"
    (check-equal?
     (delivery-refusal (lambda () (delivery-status-action! '("merge" "--repo" "/tmp/x"))))
     (string-append "unsupported action merge; this module implements status " "and governance only"))
    (check-equal? (delivery-refusal (lambda () (delivery-status-action! '("status"))))
                  "status requires --repo")
    (check-equal? (delivery-refusal (lambda ()
                                      (delivery-status-action! '("status" "--repo" "/tmp/x"))))
                  "status requires --campaign-root")
    (check-equal?
     (delivery-refusal (lambda ()
                         (delivery-status-action! '("governance" "--repo" "/tmp/x" "--wave" "1"))))
     "governance requires --expected-branch")
    (check-equal?
     (delivery-refusal (lambda ()
                         (delivery-status-action!
                          '("status" "--repo" "/tmp/x" "--wave" "one" "--campaign-root" "/tmp/x"))))
     "invalid integer for --wave: one")))
