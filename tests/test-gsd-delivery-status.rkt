#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers scripts/gsd-delivery-status.rkt
;; @covers scripts/gsd-delivery-pr.rkt
;; @covers scripts/gsd-delivery-api.rkt
;; @covers scripts/gsd-delivery-git.rkt
;; @timeout 900
;; Phase B slice S6: the delivery-proof `status` and the read-only `governance`
;; actions of scripts/gsd-delivery.py are ported to Racket
;; (scripts/gsd-delivery-status.rkt). The module is NOT wired into the
;; coordinator by this slice, the Python controller is untouched, and the S3/S4
;; modules it reuses are read-only here.
;;
;; Parity law under test: every refusal is a typed
;; exn:fail:delivery-action carrying the EXACT Python reason string and every
;; positive path returns the same JSON object, so a coordinator reason chain
;; never changes. A fake `gh` executable is installed FIRST on PATH (S3 pattern)
;; and logs every invocation's argv (unit-separator between elements), while
;; `git` is a logging FORWARDER that execs the real binary (plus an optional
;; `GIT_OVERRIDE_MAIN` fault injection so the defensive "malformed origin/main
;; head" guard is reachable). A REAL git origin/work/subject world backs every
;; git command, so refresh, fetch_head, rev-list, diff-tree, ls-tree, show,
;; merge-base and the tree reads are REAL commands: deleting one cannot hide
;; behind a stub.
;;
;; The status proof is exercised end to end, never stubbed: a REAL schema-2
;; trio is committed as the single-parent protected binding publication, and the
;; unchanged strict gate (scripts/gsd-wave-gate.rkt, spawned as a subprocess)
;; really runs over materialized blobs and the real excluded-path digest.
;;
;; Negative fixtures kill one guard at a time: missing/mismatched journal,
;; malformed origin/main, absent publication, two-parent publication, a
;; publication that changes more evidence than the current binding,
;; campaign/wave/merge identity, receipt head/branch, implementation-sha vs
;; merge-sha, a fork PR, a foreign publication PR, a fetched head that is not
;; the binding head, a merge tree that is not the PR head tree, weakened/empty
;; protection, pending/untrusted/missing/foreign checks, a governance check that
;; is not a push run on main, trio bytes changed on main / missing from HEAD /
;; differing locally, and both ancestry failures (merge not an ancestor of the
;; publication, publication not an ancestor of HEAD).
;;
;; Read-only law under test: both actions are driven to completion and the shim
;; log must contain NO mutation (`gh -X PUT/POST`, `git push`, `git commit`), and
;; both refuse with the typed `budget-exhausted` cause BEFORE spawning anything
;; when the one session budget is already spent.
;;
;; Differential parity: scripts/gsd-delivery.py is IMPORTED by path and driven
;; through the SAME gh fixtures against the SAME git world and journal; every
;; case declares its OWN expected outcome ('ok or the exact reason), so a
;; fixture drift cannot let two identical transport failures masquerade as
;; parity. A host without a python interpreter is a FAILED gate, never a skip.

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
         "../scripts/gsd-delivery-status.rkt")

(define-runtime-path python-controller "../scripts/gsd-delivery.py")
(define-runtime-path status-module "../scripts/gsd-delivery-status.rkt")

(define racket-executable (or (find-executable-path "racket") "racket"))
(define racket-directory
  (path->string (path-only (path->complete-path racket-executable))))
(define real-git (path->string (or (find-executable-path "git") "/usr/bin/git")))

(define slug "coinerd/s6")
(define plan-id (make-string 64 #\a))
(define wave 1)
(define wave-branch (format "campaign/~a/w1" (substring plan-id 0 8)))
(define binding-branch-name (format "binding/~a-w1" (substring plan-id 0 12)))
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
  (for ([pair (in-list `(("PATH" . ,path)
                         ("HOME" . ,(path->string root))
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

(define record-separator (string #\u001E))
(define unit-separator (string #\u001F))

(define (argv-log-snippet marker)
  (format (string-append "{ printf '%s' ~a"
                         "; for a in \"$@\"; do printf '\\037%s' \"$a\"; done"
                         "; printf '\\036'; } >> \"$STATUS_SHIM_LOG\"")
          marker))

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

;; Any state-changing invocation in the log: a `gh` mutation (-X PUT/POST/…) or
;; a git command that writes objects, refs or the worktree. Used by the
;; read-only law tests.
(define mutating-git-subcommands
  '("push" "commit" "commit-tree" "add" "reset" "merge" "checkout" "switch" "rm" "tag"))

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

(define (route slug* path)
  (format "repos/~a/~a" slug* path))

(define (check-runs-route sha)
  (format "commits/~a/check-runs?per_page=100&page=1" sha))

;; ============================================================
;; Shim scripts
;; ============================================================

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

;; A real-git forwarder that records the argv first. GIT_OVERRIDE_MAIN is a
;; FAULT INJECTION seam for the otherwise unreachable defensive
;; "malformed origin/main head" guard: it answers only the origin/main rev-parse.
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

;; ============================================================
;; Real schema-2 trio fixtures (the unchanged strict gate must accept them)
;; ============================================================

(define (base-evidence merge head implementation merge-method wave-label plan)
  (hasheq 'schema-version
          2
          'milestone
          1
          'issue
          7
          'wave
          wave-label
          'status
          "ready-for-merge"
          'plan-id
          plan
          'merge-method
          merge-method
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

;; One schema-2 trio the UNCHANGED strict gate accepts. The publication commit
;; only touches excluded evidence directories, so the excluded-path digest of
;; `squash...publication` is the sha256 of an empty diff.
(define (trio-datums #:merge [merge ""]
                     #:head [head ""]
                     #:implementation [implementation #f]
                     #:merge-method [merge-method "squash"]
                     #:wave-label [wave-label "W1"]
                     #:plan [plan plan-id])
  (define effective (or implementation merge))
  (values (base-evidence merge head effective merge-method wave-label plan)
          (trio-review effective)
          (trio-validation effective)))

;; ============================================================
;; The real git world
;; ============================================================

;; variants:
;;   'happy                     trio published as the single-parent binding
;;   'no-publication            the binding was never published on main
;;   'merge-publication         the publication is a two-parent merge commit
;;   'extra-evidence            the publication also adds another evidence file
;;   'tree-mismatch             the merge commit tree differs from the PR head tree
;;   'not-ancestor-publication  main carries the binding on a line without the merge
;;   'implementation-head       evidence implementation-sha is the PR head
;;   'merge-identity            evidence merge-method is not squash
;;   'fetched-mismatch          refs/pull/1/head is not the binding head
;;   'sibling-trio              an extra branch carries the SAME trio bytes
;;   'divergent-trio            an extra branch carries DIFFERENT trio bytes
(define (build-world! root variant)
  (define origin (build-path root "origin.git"))
  (define work (build-path root "work"))
  (define subject (build-path root "subject"))
  (git root "init" "--bare" "-b" "main" (path->string origin))
  (git root "init" "-b" "main" (path->string work))
  (put (build-path work "tracked") "initial\n")
  (put (build-path work policy-relative) "(\"ci\")\n")
  (git work "add" "-A")
  (git work "commit" "-m" "initial policy")
  (define base (git work "rev-parse" "HEAD"))
  (git work "remote" "add" "origin" (path->string origin))
  ;; the implementation branch plus the PR ref
  (git work "checkout" "-b" wave-branch base)
  (put (build-path work "feature") "impl\n")
  (git work "add" "feature")
  (git work "commit" "-m" "impl")
  (define head (git work "rev-parse" "HEAD"))
  (git work "push" "origin" wave-branch)
  (git work
       "push"
       "origin"
       (if (eq? variant 'fetched-mismatch)
           (format "~a:refs/pull/1/head" base)
           (format "~a:refs/pull/1/head" wave-branch)))
  ;; the protected squash merge
  (git work "checkout" "main")
  (cond
    [(eq? variant 'tree-mismatch)
     ;; A squash commit whose TREE is the pre-merge tree: the merge tree then
     ;; differs from the PR head tree (not a clean squash).
     (define squash-tree (git work "rev-parse" (format "~a^{tree}" base)))
     (define squashed (git work "commit-tree" squash-tree "-p" base "-m" "squash merge"))
     (git work "reset" "--hard" squashed)]
    [else
     (git work "merge" "--squash" wave-branch)
     (git work "commit" "-m" "squash merge")])
  (define squash (git work "rev-parse" "HEAD"))
  ;; The merge commit the binding claims to be delivered: normally the squash
  ;; commit; for 'merge-not-ancestor the PR head itself (which then exists in the
  ;; subject only after fetch_head, so the final ancestry check is the failure).
  (define claimed-merge
    (if (eq? variant 'merge-not-ancestor)
        head
        squash))
  (define implementation
    (cond
      [(eq? variant 'implementation-head) head]
      [else claimed-merge]))
  (define-values (evidence review validation)
    (trio-datums #:merge claimed-merge
                 #:head head
                 #:implementation implementation
                 #:merge-method (if (eq? variant 'merge-identity) "rebase" "squash")))
  (define (write-trio! evidence* review* validation*)
    (put-datum (build-path work evidence-relative) evidence*)
    (put-datum (build-path work review-relative) review*)
    (put-datum (build-path work validation-relative) validation*))
  (define (sibling-trio-commit! branch* evidence* review* validation* message)
    (git work "checkout" "-b" branch* base)
    (write-trio! evidence* review* validation*)
    (git work "add" "-A")
    (git work "commit" "-m" message)
    (define sha (git work "rev-parse" "HEAD"))
    (git work "push" "origin" (format "~a" branch*))
    (git work "checkout" "main")
    sha)
  (define extra-commit
    (cond
      [(eq? variant 'sibling-trio)
       (sibling-trio-commit! "head-other" evidence review validation "sibling trio")]
      [(eq? variant 'divergent-trio)
       (sibling-trio-commit! "head-different"
                             (hash-set evidence 'note "different")
                             review
                             validation
                             "divergent trio")]
      [else #f]))
  (define publication
    (cond
      [(eq? variant 'no-publication) #f]
      [(eq? variant 'merge-publication)
       ;; A two-parent publication: neither parent carries the binding, so the
       ;; merge commit itself introduces it and path-limited history cannot
       ;; simplify it away — it IS the publication commit of the binding.
       (git work "checkout" "-b" "side" squash)
       (put (build-path work "side-file") "side\n")
       (git work "add" "-A")
       (git work "commit" "-m" "side")
       (git work "checkout" "main")
       (git work "merge" "--no-ff" "--no-commit" "side")
       (write-trio! evidence review validation)
       (git work "add" "-A")
       (git work "commit" "-m" "publication merge")
       (git work "rev-parse" "HEAD")]
      [(eq? variant 'extra-evidence)
       (write-trio! evidence review validation)
       (put-datum (build-path work (format "docs/reports/gsd-wave-evidence/~a-w0.rktd" plan-id))
                  (hasheq 'schema-version 2 'wave "W0"))
       (git work "add" "-A")
       (git work "commit" "-m" "binding trio plus another evidence file")
       (git work "rev-parse" "HEAD")]
      [else
       (write-trio! evidence review validation)
       (git work "add" "-A")
       (git work "commit" "-m" "binding trio")
       (git work "rev-parse" "HEAD")]))
  (git work "push" "origin" "main")
  (git root "clone" (path->string origin) (path->string subject))
  (git subject "config" "user.name" "Test")
  (git subject "config" "user.email" "test@example.invalid")
  (hasheq 'root
          root
          'origin
          origin
          'work
          work
          'subject
          subject
          'slug
          slug
          'base
          base
          'head
          head
          'head-tree
          (git subject "rev-parse" (format "~a^{tree}" head))
          'squash
          squash
          'claimed-merge
          claimed-merge
          'publication
          publication
          'extra-commit
          extra-commit
          'wave-branch
          wave-branch
          'binding-branch
          binding-branch-name
          'evidence
          evidence
          'review
          review
          'validation
          validation))

;; Point the subject's CONFIGURED origin URL at a GitHub slug while the
;; insteadOf rewrite keeps every git transport local.
(define (point-at! world slug*)
  (define subject (hash-ref world 'subject))
  (define origin (hash-ref world 'origin))
  (define url (format "https://github.com/~a" slug*))
  (git subject "config" "remote.origin.url" url)
  (git subject "config" (format "url.~a.insteadOf" (path->string origin)) url))

;; ============================================================
;; Journal fixture
;; ============================================================

(define (journal-path root)
  (build-path root ".planning" "campaigns" plan-id "coordinator-w1.json"))

;; journal modes: 'valid, 'none (absent), 'mismatched (another plan id),
;; 'receipt-head, 'receipt-branch, 'no-attempt
(define (write-journal! root world mode)
  (define receipt
    (hasheq 'repo
            slug
            'branch
            (if (eq? mode 'receipt-branch) "campaign/foreign" wave-branch)
            'origin
            "https://github.com/coinerd/s6"
            'evidence
            evidence-relative
            'head
            (if (eq? mode 'receipt-head) (hash-ref world 'base) (hash-ref world 'head))
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
            (if (eq? mode 'mismatched) (make-string 64 #\b) plan-id)
            'wave
            wave
            'stage
            "governance"
            'receipt
            receipt))
  (put (journal-path root) (string-append (json-text journal) "\n")))

;; ============================================================
;; GitHub fixtures
;; ============================================================

(define (merged-pr-json #:number number
                        #:merge merge
                        #:head-sha head-sha
                        #:head-ref head-ref
                        #:slug slug*
                        #:state [state "closed"]
                        #:head-repo [head-repo slug*]
                        #:base-repo [base-repo slug*]
                        #:base-ref [base-ref "main"])
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
          (hasheq 'ref base-ref 'sha (make-string 40 #\c) 'repo (hasheq 'full_name base-repo))))

(define (commit-json sha parents)
  (hasheq 'sha sha 'parents (for/list ([parent (in-list parents)]) (hasheq 'sha parent))))

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
                        #:id [id 11]
                        #:details [details #f])
  (hasheq 'id
          id
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
          (or details (format "https://github.com/~a/actions/runs/~a/job/9001" slug* run-id))))

(define (workflow-run-json #:id id
                           #:sha sha
                           #:slug slug*
                           #:event event
                           #:branch branch
                           #:status [status "completed"]
                           #:conclusion [conclusion "success"]
                           #:path [path ".github/workflows/ci.yml"])
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
          path
          'repository
          (hasheq 'full_name slug*)))

;; The complete happy-path fixture set for the status proof.
(define (status-fixtures world slug*)
  (define head (hash-ref world 'head))
  (define claimed (hash-ref world 'claimed-merge))
  (define publication (hash-ref world 'publication))
  (append
   (list
    (cons (route slug* "pulls/1")
          (json-text (merged-pr-json #:number 1
                                     #:merge claimed
                                     #:head-sha head
                                     #:head-ref (hash-ref world 'wave-branch)
                                     #:slug slug*)))
    (cons (route slug* (format "commits/~a" claimed))
          (json-text (commit-json claimed (list (hash-ref world 'base)))))
    (cons (route slug* "branches/main/protection") (json-text (protection-json)))
    (cons (route slug* (check-runs-route head))
          (json-text (hasheq 'total_count
                             1
                             'check_runs
                             (list (check-run-json #:name "ci" #:sha head #:run-id 1001 #:slug slug*)))))
    (cons (route slug* "actions/runs/1001")
          (json-text (workflow-run-json #:id
                                        1001
                                        #:sha
                                        head
                                        #:slug
                                        slug*
                                        #:event
                                        "pull_request"
                                        #:branch
                                        (hash-ref world 'wave-branch)))))
   (if publication
       (list (cons (route slug* (format "commits/~a/pulls" publication))
                   (json-text
                    (list (merged-pr-json #:number
                                          2
                                          #:merge
                                          publication
                                          #:head-sha
                                          head
                                          #:head-ref
                                          binding-branch-name
                                          #:slug
                                          slug*))))
             (cons (route slug* (check-runs-route publication))
                   (json-text
                    (hasheq 'total_count
                            1
                            'check_runs
                            (list (check-run-json #:name
                                                  governance-check
                                                  #:sha
                                                  publication
                                                  #:run-id
                                                  2002
                                                  #:slug
                                                  slug*)))))
             (cons (route slug* "actions/runs/2002")
                   (json-text (workflow-run-json #:id
                                                 2002
                                                 #:sha
                                                 publication
                                                 #:slug
                                                 slug*
                                                 #:event
                                                 "push"
                                                 #:branch
                                                 "main"))))
       '())))

;; The complete happy-path fixture set for the governance proof (the governance
;; check runs on the MERGE commit, and no trio read is needed at all).
(define (governance-fixtures world slug* [branch* binding-branch-name])
  (define head (hash-ref world 'head))
  (define squash (hash-ref world 'squash))
  (list (cons (format "repos/~a/pulls?state=all&head=coinerd:~a" slug* branch*)
              (json-text
               (list (merged-pr-json #:number
                                     2
                                     #:merge
                                     squash
                                     #:head-sha
                                     head
                                     #:head-ref
                                     branch*
                                     #:slug
                                     slug*))))
        (cons (route slug* (format "commits/~a" squash))
              (json-text (commit-json squash (list (hash-ref world 'base)))))
        (cons (route slug* (check-runs-route squash))
              (json-text
               (hasheq 'total_count
                       1
                       'check_runs
                       (list (check-run-json #:name
                                             governance-check
                                             #:sha
                                             squash
                                             #:run-id
                                             3003
                                             #:slug
                                             slug*)))))
        (cons (route slug* "actions/runs/3003")
              (json-text (workflow-run-json #:id 3003 #:sha squash #:slug slug* #:event "push" #:branch "main")))))

;; ============================================================
;; Per-case world: shared or fresh git fixture + fresh shims/log
;; ============================================================

(define (install-shims! root fixtures)
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
  ;; route -> fixture index, so one case can swap the payload a route serves.
  (put (build-path root "routes.tsv")
       (string-join (for/list ([entry (in-list routes)]
                               [index (in-naturals)])
                      (format "~a\t~a" index (car entry)))
                    "\n"))
  (define gh (build-path bin "gh"))
  (put gh (gh-shim-script routes))
  (file-or-directory-permissions gh #o700)
  (define git-shim (build-path bin "git"))
  (put git-shim (git-forwarder-script))
  (file-or-directory-permissions git-shim #o700)
  (define env (fixture-env root (format "~a:~a:/usr/bin:/bin" bin racket-directory)))
  (environment-set! env "GH_SHIM_FIXTURES" (path->string fixtures-dir))
  (environment-set! env "STATUS_SHIM_LOG" (path->string (build-path root "shim.log")))
  env)

;; Rewrite the fixture a route serves, IN PLACE.
(define (set-fixture! root route-text body)
  (define line
    (for/first ([entry (in-list (file->lines (build-path root "routes.tsv")))]
                #:when (equal? (cadr (string-split entry "\t")) route-text))
      entry))
  (unless line
    (error 'set-fixture! "no installed fixture for route ~a" route-text))
  (define index (car (string-split line "\t")))
  (put (build-path root "fixtures" (format "fixture-~a.json" index)) body))

;; Run `proc` against a FRESH git world for `variant`. `fixtures` is a list of
;; (route . body) or a procedure of the built world; `journal` selects the
;; journal fixture; `override-main` injects the malformed-origin/main fault.
(define (with-world variant
                    proc
                    #:fixtures [fixtures #f]
                    #:journal [journal 'valid]
                    #:override-main [override-main #f]
                    #:slug [slug* slug])
  (define root (make-temporary-file "native-status-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define build-env (fixture-env root "/usr/bin:/bin"))
     (define world
       (parameterize ([current-environment-variables build-env])
         (check-not-false (find-executable-path "git") "the fixture must resolve git through PATH")
         (build-world! root variant)))
     (when (not (eq? journal 'none))
       (write-journal! root world journal))
     (parameterize ([current-environment-variables build-env])
       (point-at! world slug*))
     (define installed
       (cond
         [(procedure? fixtures) (fixtures world)]
         [fixtures fixtures]
         [else (status-fixtures world slug*)]))
     (define env (install-shims! root installed))
     (when override-main
       (environment-set! env "GIT_OVERRIDE_MAIN" override-main))
     (parameterize ([current-environment-variables env])
       (check-not-false (find-executable-path "gh") "the fixture must resolve gh through PATH")
       (check-equal? (path->complete-path (find-executable-path "git"))
                     (path->complete-path (build-path root "bin" "git"))
                     "the fixture must resolve git to the logging forwarder")
       (check-not-false (find-executable-path "racket") "the helper/gate must resolve racket")
       (clear-api-cache!)
       (proc world root (build-path root "shim.log"))))
   (lambda () (delete-directory/files root))))

;; ONE immutable happy world shared by every fixture-only case (a repo build
;; costs ~20 bounded subprocesses); every case still gets its OWN shims and log.
(define happy-world-box (box #f))

(define (happy-world)
  (or (unbox happy-world-box)
      (let* ([root (make-temporary-file "native-status-base-~a" 'directory)]
             [env (fixture-env root "/usr/bin:/bin")]
             [world (parameterize ([current-environment-variables env])
                      (build-world! root 'happy))])
        (parameterize ([current-environment-variables env])
          (point-at! world slug)
          (write-journal! root world 'valid))
        (set-box! happy-world-box world)
        world)))

;; A case over the SHARED happy world: fresh shims, fixtures and log. `fixtures`
;; is a list of (route . body), a procedure of the world, or #f for the default
;; happy-path set.
(define (with-happy-world fixtures
                          proc
                          #:journal [journal #f]
                          #:override-main [override-main #f])
  (define world (happy-world))
  (define root (make-temporary-file "native-status-case-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     ;; The campaign root of THIS case is its own temporary root: the journal is
     ;; per-case state, never shared fixture state.
     (write-journal! root world (or journal 'valid))
     (define installed
       (cond
         [(procedure? fixtures) (fixtures world)]
         [fixtures fixtures]
         [else (status-fixtures world slug)]))
     (define env (install-shims! root installed))
     (when override-main
       (environment-set! env "GIT_OVERRIDE_MAIN" override-main))
     (parameterize ([current-environment-variables env])
       (clear-api-cache!)
       (proc world root (build-path root "shim.log"))))
   (lambda () (delete-directory/files root))))

;; ============================================================
;; Refusal / payload helpers
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

(define (refusal-prefix thunk prefix)
  (define reason (delivery-refusal thunk))
  (check-true (string-prefix? reason prefix) reason)
  reason)

;; ============================================================
;; Python parity driver
;; ============================================================

(define python-driver
  #<<PY
import importlib.util, json, sys
spec = importlib.util.spec_from_file_location("gsd_delivery", sys.argv[1])
mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)
case = json.loads(sys.argv[2])
fn = case["fn"]
try:
    if fn == "status":
        value = mod.status(case["repo"], case["plan"], case["wave"], case["campaign_root"])
    elif fn == "governance":
        value = mod.governance(case["repo"], case["plan"], case["wave"], case["expected_branch"])
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
                    #:timeout 180
                    #:process-group? #t))
  (unless (and (zero? (subprocess-result-exit-code result))
               (not (subprocess-result-timed-out? result)))
    (error 'python-parity "python parity driver failed: ~a" (subprocess-result-stderr result)))
  (string->jsexpr (subprocess-result-stdout result)))

(module+ test
  ;; ============================================================
  ;; status: the delivery proof
  ;; ============================================================

  (test-case "status proves delivery with the exact payload"
    (with-happy-world #f
                      (lambda (world root log)
                        (define result
                          (delivery-status! (hash-ref world 'subject) plan-id wave #:campaign-root root))
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
                        ;; The proof really read the strict gate, the PR ref and
                        ;; the trusted checks (no stub could produce these).
                        (define git-reads (invocations log "git"))
                        (check-true (for/or ([argv (in-list git-reads)])
                                      (member "refs/heads/main:refs/remotes/origin/main" argv)))
                        (check-true (for/or ([argv (in-list git-reads)])
                                      (member "refs/pull/1/head" argv)))
                        (check-true (for/or ([argv (in-list git-reads)])
                                      (equal? argv
                                              (list "-C"
                                                    (path->string (hash-ref world 'subject))
                                                    "rev-list"
                                                    "--parents"
                                                    "-n"
                                                    "1"
                                                    (hash-ref world 'publication)))))
                        (check-true (for/or ([argv (in-list (invocations log "gh"))])
                                      (equal? argv
                                              (list "api"
                                                    "--hostname"
                                                    "github.com"
                                                    (route slug (check-runs-route (hash-ref world 'head)))))))
                        (check-true (for/or ([argv (in-list (invocations log "gh"))])
                                      (equal? argv
                                              (list "api"
                                                    "--hostname"
                                                    "github.com"
                                                    (route slug
                                                           (check-runs-route (hash-ref world
                                                                                       'publication))))))))))

  (test-case "status refuses a missing, mismatched or unbound journal"
    ;; missing journal: the generation read sees no journal, the proof refuses
    (with-happy-world #f
                      #:journal 'none
                      (lambda (world root log)
                        (check-equal? (delivery-refusal (lambda ()
                                                          (delivery-status! (hash-ref world 'subject)
                                                                            plan-id
                                                                            wave
                                                                            #:campaign-root root)))
                                      "delivery proof requires the authoritative coordinator journal")))
    ;; a journal the trusted loader rejects fails the helper, and the seam's
    ;; typed cause surfaces (never a fabricated receipt)
    (with-happy-world #f
                      #:journal 'mismatched
                      (lambda (world root log)
                        (check-equal?
                         (delivery-refusal
                          (lambda ()
                            (delivery-status! (hash-ref world 'subject)
                                              plan-id
                                              wave
                                              #:campaign-root root)))
                         "delivery command failed (racket, exit 1); inspect authentication, repository refs and required artifacts locally")))
    ;; a receipt whose head or branch is not the published binding's identity
    ;; passes the attempt-bound SHAPE and is refused by the authentication step
    (for ([mode (in-list (list 'receipt-head 'receipt-branch))])
      (with-happy-world #f
                        #:journal mode
                        (lambda (world root log)
                          (check-equal? (delivery-refusal (lambda ()
                                                            (delivery-status! (hash-ref world
                                                                                        'subject)
                                                                              plan-id
                                                                              wave
                                                                              #:campaign-root
                                                                              root)))
                                        "published binding does not authenticate the current receipt head/branch"
                                        (format "~a" mode)))))
    ;; a receipt with no attempt identity is not a Verify receipt at all
    (with-happy-world #f
                      #:journal 'no-attempt
                      (lambda (world root log)
                        (check-equal? (delivery-refusal (lambda ()
                                                          (delivery-status! (hash-ref world 'subject)
                                                                            plan-id
                                                                            wave
                                                                            #:campaign-root root)))
                                      "delivery proof requires an attempt-bound Verify receipt"))))

  (test-case "status refuses a malformed origin/main and an absent publication"
    (with-happy-world #f
                      #:override-main "not-a-sha"
                      (lambda (world root log)
                        (check-equal? (delivery-refusal (lambda ()
                                                          (delivery-status! (hash-ref world 'subject)
                                                                            plan-id
                                                                            wave
                                                                            #:campaign-root root)))
                                      "malformed origin/main head")))
    (with-world 'no-publication
                (lambda (world root log)
                  (check-equal? (delivery-refusal (lambda ()
                                                    (delivery-status! (hash-ref world 'subject)
                                                                      plan-id
                                                                      wave
                                                                      #:campaign-root root)))
                                (format "W~a verified; delivery pending: protected campaign binding is absent"
                                        wave)))))

  (test-case "status refuses a non-single-parent publication"
    (with-world 'merge-publication
                (lambda (world root log)
                  (check-equal? (delivery-refusal (lambda ()
                                                    (delivery-status! (hash-ref world 'subject)
                                                                      plan-id
                                                                      wave
                                                                      #:campaign-root root)))
                                "binding publication is not a single-parent protected commit"))))

  (test-case "status refuses a publication that changes more than the binding"
    (with-world 'extra-evidence
                (lambda (world root log)
                  (check-equal? (delivery-refusal (lambda ()
                                                    (delivery-status! (hash-ref world 'subject)
                                                                      plan-id
                                                                      wave
                                                                      #:campaign-root root)))
                                (string-append "publication must change exactly the current binding evidence "
                                               "(possible governance green-skip refused)")))))

  (test-case "status refuses campaign/wave/merge identity mismatches"
    (with-world 'merge-identity
                (lambda (world root log)
                  (check-equal? (delivery-refusal (lambda ()
                                                    (delivery-status! (hash-ref world 'subject)
                                                                      plan-id
                                                                      wave
                                                                      #:campaign-root root)))
                                "campaign/wave/merge identity mismatch"))))

  (test-case "status refuses implementation-sha that is not the merge-sha"
    (with-world 'implementation-head
                (lambda (world root log)
                  (check-equal? (delivery-refusal (lambda ()
                                                    (delivery-status! (hash-ref world 'subject)
                                                                      plan-id
                                                                      wave
                                                                      #:campaign-root root)))
                                "binding implementation-sha differs from merge-sha (squash semantics violated)"))))

  (test-case "status refuses a fork / foreign implementation PR"
    (with-happy-world (lambda (world)
                        (define head (hash-ref world 'head))
                        (define squash (hash-ref world 'squash))
                        (list (cons (route slug "pulls/1")
                                    (json-text (merged-pr-json #:number
                                                               1
                                                               #:merge
                                                               squash
                                                               #:head-sha
                                                               head
                                                               #:head-ref
                                                               wave-branch
                                                               #:slug
                                                               slug
                                                               #:head-repo
                                                               "fork/q")))
                              (cons (route slug (format "commits/~a" squash))
                                    (json-text (commit-json squash (list (hash-ref world 'base)))))
                              (cons (route slug "branches/main/protection")
                                    (json-text (protection-json)))
                              (cons (route slug (check-runs-route head))
                                    (json-text
                                     (hasheq 'total_count
                                             1
                                             'check_runs
                                             (list (check-run-json #:name "ci" #:sha head #:run-id 1001 #:slug slug)))))))
                      (lambda (world root log)
                        (check-equal? (delivery-refusal (lambda ()
                                                          (delivery-status! (hash-ref world 'subject)
                                                                            plan-id
                                                                            wave
                                                                            #:campaign-root root)))
                                      "implementation repository/default branch mismatch"))))

  (test-case "status refuses a publication not associated with a merged same-repo PR"
    (with-happy-world (lambda (world)
                        (define installed (status-fixtures world slug))
                        (cons (cons (route slug (format "commits/~a/pulls" (hash-ref world 'publication)))
                                    "[]")
                              installed))
                      (lambda (world root log)
                        (check-equal? (delivery-refusal (lambda ()
                                                          (delivery-status! (hash-ref world 'subject)
                                                                            plan-id
                                                                            wave
                                                                            #:campaign-root root)))
                                      "binding publication is not associated with a PR")))))

  (test-case "status refuses a fetched head and a merge tree that do not match the binding"
    (with-world 'fetched-mismatch
                (lambda (world root log)
                  (check-equal? (delivery-refusal (lambda ()
                                                    (delivery-status! (hash-ref world 'subject)
                                                                      plan-id
                                                                      wave
                                                                      #:campaign-root root)))
                                "fetched implementation head does not match binding delivery-head-sha")))
    (with-world 'tree-mismatch
                (lambda (world root log)
                  (check-equal? (delivery-refusal (lambda ()
                                                    (delivery-status! (hash-ref world 'subject)
                                                                      plan-id
                                                                      wave
                                                                      #:campaign-root root)))
                                "implementation merge tree differs from PR head tree (not a clean squash)"))))

  (test-case "status refuses weakened, empty and non-covering branch protection"
    (for ([entry (in-list (list (list (protection-json #:enforce-admins #f)
                                      "default branch protection is absent or weakened")
                                (list (protection-json #:strict #f)
                                      "default branch protection is absent or weakened")
                                (list (protection-json #:contexts '())
                                      "default branch protection requires configured native checks")
                                (list (protection-json #:contexts '("other"))
                                      "branch protection does not require expected checks: ci")))]
          [index (in-naturals)])
      (define payload (car entry))
      (define expected (cadr entry))
      (with-happy-world (lambda (world)
                          (define installed (status-fixtures world slug))
                          (cons (cons (route slug "branches/main/protection") (json-text payload))
                                installed))
                        (lambda (world root log)
                          (check-equal? (delivery-refusal (lambda ()
                                                            (delivery-status! (hash-ref world
                                                                                        'subject)
                                                                              plan-id
                                                                              wave
                                                                              #:campaign-root
                                                                              root)))
                                        expected
                                        (format "~a" index))))))

  (test-case "status refuses missing, pending, untrusted and unauthenticated checks"
    (define (check-runs-payload head kind)
      (case kind
        [(empty) (hasheq 'total_count 0 'check_runs '())]
        [(pending)
         (hasheq 'total_count
                 1
                 'check_runs
                 (list (check-run-json #:name
                                       "ci"
                                       #:sha
                                       head
                                       #:run-id
                                       1001
                                       #:slug
                                       slug
                                       #:status
                                       "in_progress"
                                       #:conclusion
                                       #f)))]
        [(untrusted)
         (hasheq 'total_count
                 1
                 'check_runs
                 (list (check-run-json #:name
                                       "ci"
                                       #:sha
                                       head
                                       #:run-id
                                       1001
                                       #:slug
                                       slug
                                       #:app-id
                                       999)))]
        [(foreign)
         (hasheq 'total_count
                 1
                 'check_runs
                 (list (check-run-json #:name
                                       "ci"
                                       #:sha
                                       head
                                       #:run-id
                                       1001
                                       #:slug
                                       slug
                                       #:details
                                       "https://github.com/other/q/actions/runs/1/job/2")))]))
    (for ([entry (in-list (list (list 'empty "missing check: ci")
                                (list 'pending "pending, failed, or untrusted check: ci")
                                (list 'untrusted "pending, failed, or untrusted check: ci")
                                (list 'foreign "check has no authenticated Actions run: ci")))]
          [index (in-naturals)])
      (define kind (car entry))
      (define expected (cadr entry))
      (with-happy-world (lambda (world)
                          (define installed (status-fixtures world slug))
                          (cons (cons (route slug (check-runs-route (hash-ref world 'head)))
                                      (json-text (check-runs-payload (hash-ref world 'head) kind)))
                                installed))
                        (lambda (world root log)
                          (check-equal? (delivery-refusal (lambda ()
                                                            (delivery-status! (hash-ref world
                                                                                        'subject)
                                                                              plan-id
                                                                              wave
                                                                              #:campaign-root
                                                                              root)))
                                        expected
                                        (format "~a" index))))))

  (test-case "status refuses a governance check that is not a push run on main"
    (for ([entry (in-list (list (list "pull_request" "main" "check workflow/ref/event mismatch: gsd-governance")
                                (list "push" wave-branch "check workflow/ref/event mismatch: gsd-governance")))]
          [index (in-naturals)])
      (define event (car entry))
      (define branch* (cadr entry))
      (define expected (caddr entry))
      (with-happy-world (lambda (world)
                          (define publication (hash-ref world 'publication))
                          (define installed (status-fixtures world slug))
                          (append
                           (list (cons (route slug (check-runs-route publication))
                                       (json-text
                                        (hasheq 'total_count
                                                1
                                                'check_runs
                                                (list (check-run-json #:name
                                                                      governance-check
                                                                      #:sha
                                                                      publication
                                                                      #:run-id
                                                                      2002
                                                                      #:slug
                                                                      slug)))))
                                 (cons (route slug "actions/runs/2002")
                                       (json-text (workflow-run-json #:id
                                                                     2002
                                                                     #:sha
                                                                     publication
                                                                     #:slug
                                                                     slug
                                                                     #:event
                                                                     event
                                                                     #:branch
                                                                     branch*))))
                           installed))
                        (lambda (world root log)
                          (check-equal? (delivery-refusal (lambda ()
                                                            (delivery-status! (hash-ref world
                                                                                        'subject)
                                                                              plan-id
                                                                              wave
                                                                              #:campaign-root
                                                                              root)))
                                        expected
                                        (format "~a" index))))))

  (test-case "status refuses trio bytes that changed on main or locally"
    ;; main advances with a REVIEW-artifact change after the publication, so the
    ;; published binding path is unchanged but the trio bytes are not.
    (with-world 'happy
                (lambda (world root log)
                  (define work (hash-ref world 'work))
                  (git work "checkout" "main")
                  (put-datum (build-path work review-relative)
                             (hash-set (hash-ref world 'review) 'note "different"))
                  (git work "add" "-A")
                  (git work "commit" "-m" "later review edit")
                  (git work "push" "origin" "main")
                  (check-equal? (delivery-refusal (lambda ()
                                                    (delivery-status! (hash-ref world 'subject)
                                                                      plan-id
                                                                      wave
                                                                      #:campaign-root root)))
                                "binding trio changed since governance publication")))
    ;; the local artifact differs from the committed proof
    (with-world 'happy
                (lambda (world root log)
                  (define local (build-path (hash-ref world 'subject) evidence-relative))
                  (put local "{}\n")
                  (check-equal? (delivery-refusal (lambda ()
                                                    (delivery-status! (hash-ref world 'subject)
                                                                      plan-id
                                                                      wave
                                                                      #:campaign-root root)))
                                "delivery sync pending: local artifact differs from committed proof")))
    ;; HEAD carries the same paths with different bytes
    (with-world 'divergent-trio
                (lambda (world root log)
                  (git (hash-ref world 'subject) "fetch" "--no-tags" "origin" "head-different")
                  (git (hash-ref world 'subject) "checkout" "--detach" "FETCH_HEAD")
                  (check-equal? (delivery-refusal (lambda ()
                                                    (delivery-status! (hash-ref world 'subject)
                                                                      plan-id
                                                                      wave
                                                                      #:campaign-root root)))
                                "delivery sync pending: current checkout does not contain published trio"))))

  (test-case "status refuses both ancestry failures"
    ;; the merge is not an ancestor of the publication (the claimed merge is the
    ;; PR head, which the subject knows only after fetch_head)
    (with-world 'merge-not-ancestor
                (lambda (world root log)
                  (check-equal? (refusal-prefix (lambda ()
                                                  (delivery-status! (hash-ref world 'subject)
                                                                    plan-id
                                                                    wave
                                                                    #:campaign-root root))
                                                "delivery command failed (git, exit ")
                                "delivery command failed (git, exit 1); inspect authentication, repository refs and required artifacts locally")))
    ;; the publication is not an ancestor of HEAD, even though HEAD carries the
    ;; very same trio bytes (so every earlier gate passed)
    (with-world 'sibling-trio
                (lambda (world root log)
                  (git (hash-ref world 'subject) "fetch" "--no-tags" "origin" "head-other")
                  (git (hash-ref world 'subject) "checkout" "--detach" "FETCH_HEAD")
                  (check-equal? (refusal-prefix (lambda ()
                                                  (delivery-status! (hash-ref world 'subject)
                                                                    plan-id
                                                                    wave
                                                                    #:campaign-root root))
                                                "delivery command failed (git, exit ")
                                "delivery command failed (git, exit 1); inspect authentication, repository refs and required artifacts locally"))))

  ;; ============================================================
  ;; governance (read-only)
  ;; ============================================================

  (test-case "governance verifies a merged binding publication without mutating"
    (with-happy-world (lambda (world) (governance-fixtures world slug))
                      (lambda (world root log)
                        (check-equal?
                         (delivery-governance! (hash-ref world 'subject)
                                               plan-id
                                               wave
                                               binding-branch-name)
                         (hasheq 'status
                                 "governed"
                                 'pr
                                 2
                                 'branch
                                 binding-branch-name
                                 'publication-sha
                                 (hash-ref world 'squash)))
                        (check-equal? (mutations log) '() "governance must never mutate"))))

  (test-case "governance refuses a branch that is not the deterministic binding branch"
    (for ([branch* (in-list (list "campaign/w1" (format "~a-r0" binding-branch-name)))])
      (check-equal? (delivery-refusal (lambda ()
                                        (delivery-governance! "/nonexistent-status-fixture"
                                                              plan-id
                                                              wave
                                                              branch*)))
                    "governance branch does not match the deterministic binding branch"
                    (format "~s" branch*)))
    ;; a blank branch is refused before the branch identity is even built
    (check-equal? (delivery-refusal (lambda ()
                                      (delivery-governance! "/nonexistent-status-fixture"
                                                            plan-id
                                                            wave
                                                            "")))
                  "governance requires --expected-branch"))

  (test-case "governance accepts every -r<n> republication branch"
    (for ([branch* (in-list (list binding-branch-name
                                  (format "~a-r1" binding-branch-name)
                                  (format "~a-r27" binding-branch-name)))])
      (with-happy-world (lambda (world) (governance-fixtures world slug branch*))
                        (lambda (world root log)
                          (check-equal? (hash-ref (delivery-governance! (hash-ref world 'subject)
                                                                        plan-id
                                                                        wave
                                                                        branch*)
                                                  'branch)
                                        branch*)))))

  (test-case "governance refuses an unresolvable, ambiguous or incomplete merged PR"
    (with-happy-world (lambda (world)
                        (define installed (governance-fixtures world slug))
                        (cons (cons (format "repos/~a/pulls?state=all&head=coinerd:~a"
                                            slug
                                            binding-branch-name)
                                    "[]")
                              installed))
                      (lambda (world root log)
                        (check-equal?
                         (delivery-refusal (lambda ()
                                             (delivery-governance! (hash-ref world 'subject)
                                                                   plan-id
                                                                   wave
                                                                   binding-branch-name)))
                         (string-append "governance cannot self-resolve exactly one merged binding PR for branch "
                                        binding-branch-name))))
    (with-happy-world (lambda (world)
                        (define installed (governance-fixtures world slug))
                        (define pr (merged-pr-json #:number
                                                   2
                                                   #:merge
                                                   (hash-ref world 'squash)
                                                   #:head-sha
                                                   (hash-ref world 'head)
                                                   #:head-ref
                                                   binding-branch-name
                                                   #:slug
                                                   slug))
                        (cons (cons (format "repos/~a/pulls?state=all&head=coinerd:~a"
                                            slug
                                            binding-branch-name)
                                    (json-text (list pr (hash-set pr 'number 3))))
                              installed))
                      (lambda (world root log)
                        (check-equal?
                         (delivery-refusal (lambda ()
                                             (delivery-governance! (hash-ref world 'subject)
                                                                   plan-id
                                                                   wave
                                                                   binding-branch-name)))
                         (string-append "multiple merged pull requests already target branch "
                                        binding-branch-name))))
    (with-happy-world (lambda (world)
                        (define installed (governance-fixtures world slug))
                        (cons (cons (format "repos/~a/pulls?state=all&head=coinerd:~a"
                                            slug
                                            binding-branch-name)
                                    (json-text
                                     (list (merged-pr-json #:number
                                                           2
                                                           #:merge
                                                           "deadbeef"
                                                           #:head-sha
                                                           (hash-ref world 'head)
                                                           #:head-ref
                                                           binding-branch-name
                                                           #:slug
                                                           slug))))
                              installed))
                      (lambda (world root log)
                        (check-equal?
                         (delivery-refusal (lambda ()
                                             (delivery-governance! (hash-ref world 'subject)
                                                                   plan-id
                                                                   wave
                                                                   binding-branch-name)))
                         "merged binding PR has incomplete identity"))))

  (test-case "governance refuses a non-squash merge, a foreign base and broken ancestry"
    ;; two-parent merge commit
    (with-happy-world (lambda (world)
                        (define installed (governance-fixtures world slug))
                        (cons (cons (route slug (format "commits/~a" (hash-ref world 'squash)))
                                    (json-text (commit-json (hash-ref world 'squash)
                                                            (list (hash-ref world 'base)
                                                                  (hash-ref world 'base)))))
                              installed))
                      (lambda (world root log)
                        (check-equal? (delivery-refusal (lambda ()
                                                          (delivery-governance!
                                                           (hash-ref world 'subject)
                                                           plan-id
                                                           wave
                                                           binding-branch-name)))
                                      "delivery must have single-parent squash provenance")))
    ;; a PR whose base is not main of this repository
    (with-happy-world (lambda (world)
                        (define installed (governance-fixtures world slug))
                        (cons (cons (format "repos/~a/pulls?state=all&head=coinerd:~a"
                                            slug
                                            binding-branch-name)
                                    (json-text
                                     (list (merged-pr-json #:number
                                                           2
                                                           #:merge
                                                           (hash-ref world 'squash)
                                                           #:head-sha
                                                           (hash-ref world 'head)
                                                           #:head-ref
                                                           binding-branch-name
                                                           #:slug
                                                           slug
                                                           #:base-repo
                                                           "other/q"))))
                              installed))
                      (lambda (world root log)
                        (check-equal? (delivery-refusal (lambda ()
                                                          (delivery-governance!
                                                           (hash-ref world 'subject)
                                                           plan-id
                                                           wave
                                                           binding-branch-name)))
                                      "implementation repository/default branch mismatch")))
    ;; the merge is not an ancestor of origin/main (an existing commit that the
    ;; protected main never received)
    (with-happy-world (lambda (world)
                        (define orphan (hash-ref world 'head))
                        (define installed (governance-fixtures world slug))
                        (cons (cons (format "repos/~a/pulls?state=all&head=coinerd:~a"
                                            slug
                                            binding-branch-name)
                                    (json-text
                                     (list (merged-pr-json #:number
                                                           2
                                                           #:merge
                                                           orphan
                                                           #:head-sha
                                                           (hash-ref world 'head)
                                                           #:head-ref
                                                           binding-branch-name
                                                           #:slug
                                                           slug))))
                              (cons (cons (route slug (format "commits/~a" orphan))
                                          (json-text (commit-json orphan
                                                                  (list (hash-ref world 'base)))))
                                    installed)))
                      (lambda (world root log)
                        (check-equal?
                         (refusal-prefix
                          (lambda ()
                            (delivery-governance!
                             (hash-ref world 'subject)
                             plan-id
                             wave
                             binding-branch-name))
                          "delivery command failed (git, exit ")
                         "delivery command failed (git, exit 1); inspect authentication, repository refs and required artifacts locally"))))

  (test-case "governance requires its trusted check on the merge commit"
    (for ([entry (in-list (list (list (hasheq 'total_count 0 'check_runs '())
                                      "missing check: gsd-governance")
                                (list (hasheq 'total_count
                                              1
                                              'check_runs
                                              (list (check-run-json #:name
                                                                    governance-check
                                                                    #:sha
                                                                    (make-string 40 #\d)
                                                                    #:run-id
                                                                    3003
                                                                    #:slug
                                                                    slug
                                                                    #:conclusion
                                                                    "failure")))
                                      "pending, failed, or untrusted check: gsd-governance")))]
          [index (in-naturals)])
      (define payload (car entry))
      (define expected (cadr entry))
      (with-happy-world (lambda (world)
                          (define installed (governance-fixtures world slug))
                          (cons (cons (route slug (check-runs-route (hash-ref world 'squash)))
                                      (json-text payload))
                                installed))
                        (lambda (world root log)
                          (check-equal? (delivery-refusal (lambda ()
                                                            (delivery-governance!
                                                             (hash-ref world 'subject)
                                                             plan-id
                                                             wave
                                                             binding-branch-name)))
                                        expected
                                        (format "~a" index)))))
    ;; the run behind the check must be a push run on main
    (with-happy-world (lambda (world)
                        (define installed (governance-fixtures world slug))
                        (define squash (hash-ref world 'squash))
                        (cons (cons (route slug "actions/runs/3003")
                                    (json-text (workflow-run-json #:id
                                                                  3003
                                                                  #:sha
                                                                  squash
                                                                  #:slug
                                                                  slug
                                                                  #:event
                                                                  "pull_request"
                                                                  #:branch
                                                                  "main")))
                              installed))
                      (lambda (world root log)
                        (check-equal? (delivery-refusal (lambda ()
                                                          (delivery-governance!
                                                           (hash-ref world 'subject)
                                                           plan-id
                                                           wave
                                                           binding-branch-name)))
                                      "check workflow/ref/event mismatch: gsd-governance"))))

  ;; ============================================================
  ;; read-only law, one budget, CLI surface
  ;; ============================================================

  (test-case "status and governance spawn no mutation"
    (with-happy-world (lambda (world) (status-fixtures world slug))
                      (lambda (world root log)
                        (delivery-status! (hash-ref world 'subject) plan-id wave #:campaign-root root)
                        (check-equal? (mutations log) '() "status must never mutate")))
    (with-happy-world (lambda (world) (governance-fixtures world slug))
                      (lambda (world root log)
                        (delivery-governance! (hash-ref world 'subject)
                                              plan-id
                                              wave
                                              binding-branch-name)
                        (check-equal? (mutations log) '() "governance must never mutate"))))

  (test-case "each action spends the session's ONE budget before spawning anything"
    (with-happy-world (lambda (world)
                        (status-fixtures world slug))
                      (lambda (world root log)
                        (define ctx
                          (make-delivery-pr-context (hash-ref world 'subject)
                                                    #:budget
                                                    1.0
                                                    #:start-ms
                                                    (current-inexact-monotonic-milliseconds)))
                        (define reason
                          (delivery-refusal (lambda ()
                                              (delivery-status! (hash-ref world 'subject)
                                                                plan-id
                                                                wave
                                                                #:campaign-root
                                                                root
                                                                #:context
                                                                ctx))))
                        (check-true (string-prefix? reason "budget-exhausted:") reason)
                        (check-equal? (invocations log) '() "a spent budget must refuse before spawning")
                        (check-equal? (delivery-refusal (lambda ()
                                                          (delivery-governance!
                                                           (hash-ref world 'subject)
                                                           plan-id
                                                           wave
                                                           binding-branch-name
                                                           #:context
                                                           ctx)))
                                      reason))))

  (test-case "the status CLI refuses unsupported actions and missing flags"
    (check-equal? (delivery-refusal (lambda () (delivery-status-action! '("merge" "--repo" "/tmp/x"))))
                  "unsupported action merge; this module implements status and governance only")
    (check-equal? (delivery-refusal (lambda () (delivery-status-action! '("status"))))
                  "status requires --repo")
    (check-equal? (delivery-refusal (lambda () (delivery-status-action! '("status" "--repo" "/tmp/x"))))
                  "status requires --campaign-root")
    (check-equal? (delivery-refusal
                   (lambda ()
                     (delivery-status-action!
                      '("governance" "--repo" "/tmp/x" "--wave" "1"))))
                  "governance requires --expected-branch")
    (check-equal? (delivery-refusal
                   (lambda ()
                     (delivery-status-action!
                      '("status" "--repo" "/tmp/x" "--wave" "one" "--campaign-root" "/tmp/x"))))
                  "invalid integer for --wave: one"))

  (test-case "the status CLI prints the typed delivery-pending JSON and exits 2"
    (define result
      (run-subprocess (path->string racket-executable)
                      #:args (list (path->string status-module) "merge" "--repo" "/tmp/x")
                      #:timeout 180
                      #:process-group? #t))
    (check-equal? (subprocess-result-exit-code result) 2)
    (define payload (string->jsexpr (subprocess-result-stdout result)))
    (check-equal? (hash-ref payload 'status) "delivery-pending")
    (check-equal? (hash-ref payload 'reason)
                  "unsupported action merge; this module implements status and governance only"))

  ;; ============================================================
  ;; differential parity with the imported Python controller
  ;; ============================================================

  (define (parity-case label variant expected spec [journal 'valid] [fixtures-fn #f] [override-main #f])
    (hasheq 'label
            label
            'variant
            variant
            'expected
            expected
            'spec
            spec
            'journal
            journal
            'fixtures-fn
            fixtures-fn
            'override-main
            override-main))

  (define status-spec
    (hasheq 'fn "status" 'plan plan-id 'wave wave 'campaign_root 'PLACEHOLDER))
  (define governance-spec
    (hasheq 'fn "governance" 'plan plan-id 'wave wave 'expected_branch binding-branch-name))

  (define parity-cases
    (list
     (parity-case "status-happy" 'happy 'ok status-spec)
     (parity-case "status-journal-missing"
                  'happy
                  "delivery proof requires the authoritative coordinator journal"
                  status-spec
                  'none)
     (parity-case "status-journal-mismatched"
                  'happy
                  "delivery command failed (racket, exit 1); inspect authentication, repository refs and required artifacts locally"
                  status-spec
                  'mismatched)
     (parity-case "status-receipt-head"
                  'happy
                  "published binding does not authenticate the current receipt head/branch"
                  status-spec
                  'receipt-head)
     (parity-case "status-receipt-no-attempt"
                  'happy
                  "delivery proof requires an attempt-bound Verify receipt"
                  status-spec
                  'no-attempt)
     (parity-case "status-no-publication"
                  'no-publication
                  (format "W~a verified; delivery pending: protected campaign binding is absent" wave)
                  status-spec)
     (parity-case "status-merge-publication"
                  'merge-publication
                  "binding publication is not a single-parent protected commit"
                  status-spec)
     (parity-case "status-extra-evidence"
                  'extra-evidence
                  (string-append "publication must change exactly the current binding evidence "
                                 "(possible governance green-skip refused)")
                  status-spec)
     (parity-case "status-merge-identity"
                  'merge-identity
                  "campaign/wave/merge identity mismatch"
                  status-spec)
     (parity-case "status-implementation-head"
                  'implementation-head
                  "binding implementation-sha differs from merge-sha (squash semantics violated)"
                  status-spec)
     (parity-case "status-fetched-mismatch"
                  'fetched-mismatch
                  "fetched implementation head does not match binding delivery-head-sha"
                  status-spec)
     (parity-case "status-tree-mismatch"
                  'tree-mismatch
                  "implementation merge tree differs from PR head tree (not a clean squash)"
                  status-spec)
     (parity-case "status-protection-weakened"
                  'happy
                  "default branch protection is absent or weakened"
                  status-spec
                  'valid
                  (lambda (world)
                    (cons (cons (route slug "branches/main/protection")
                                (json-text (protection-json #:enforce-admins #f)))
                          (status-fixtures world slug))))
     (parity-case "status-check-missing"
                  'happy
                  "missing check: ci"
                  status-spec
                  'valid
                  (lambda (world)
                    (cons (cons (route slug (check-runs-route (hash-ref world 'head)))
                                (json-text (hasheq 'total_count 0 'check_runs '())))
                          (status-fixtures world slug))))
     (parity-case "status-governance-check-event"
                  'happy
                  "check workflow/ref/event mismatch: gsd-governance"
                  status-spec
                  'valid
                  (lambda (world)
                    (define publication (hash-ref world 'publication))
                    (append (list (cons (route slug "actions/runs/2002")
                                        (json-text (workflow-run-json #:id
                                                                      2002
                                                                      #:sha
                                                                      publication
                                                                      #:slug
                                                                      slug
                                                                      #:event
                                                                      "pull_request"
                                                                      #:branch
                                                                      "main"))))
                            (status-fixtures world slug))))
     (parity-case "status-fork-pr"
                  'happy
                  "implementation repository/default branch mismatch"
                  status-spec
                  'valid
                  (lambda (world)
                    (define installed (status-fixtures world slug))
                    (cons (cons (route slug "pulls/1")
                                (json-text (merged-pr-json #:number
                                                           1
                                                           #:merge
                                                           (hash-ref world 'squash)
                                                           #:head-sha
                                                           (hash-ref world 'head)
                                                           #:head-ref
                                                           wave-branch
                                                           #:slug
                                                           slug
                                                           #:head-repo
                                                           "fork/q")))
                          installed)))
     (parity-case "status-publication-not-associated"
                  'happy
                  "binding publication is not associated with a PR"
                  status-spec
                  'valid
                  (lambda (world)
                    (cons (cons (route slug (format "commits/~a/pulls" (hash-ref world 'publication)))
                                "[]")
                          (status-fixtures world slug))))
     (parity-case "status-merge-not-ancestor"
                  'merge-not-ancestor
                  "delivery command failed (git, exit 1); inspect authentication, repository refs and required artifacts locally"
                  status-spec)
     (parity-case "governance-happy"
                  'happy
                  'ok
                  governance-spec
                  'valid
                  (lambda (world) (governance-fixtures world slug)))
     (parity-case "governance-branch"
                  'happy
                  "governance branch does not match the deterministic binding branch"
                  (hash-set governance-spec 'expected_branch "campaign/w1"))
     (parity-case "governance-unresolvable"
                  'happy
                  (string-append "governance cannot self-resolve exactly one merged binding PR for branch "
                                 binding-branch-name)
                  governance-spec
                  'valid
                  (lambda (world)
                    (cons (cons (format "repos/~a/pulls?state=all&head=coinerd:~a"
                                        slug
                                        binding-branch-name)
                                "[]")
                          (governance-fixtures world slug))))
     (parity-case "governance-check-missing"
                  'happy
                  "missing check: gsd-governance"
                  governance-spec
                  'valid
                  (lambda (world)
                    (cons (cons (route slug (check-runs-route (hash-ref world 'squash)))
                                (json-text (hasheq 'total_count 0 'check_runs '())))
                          (governance-fixtures world slug))))))

  (test-case "differential parity with the imported Python controller (>= 10 cases)"
    (define python (or (find-executable-path "python3") (find-executable-path "python")))
    (unless python
      (fail (string-append "python3/python is required for the differential status parity case; "
                           "a missing interpreter is a FAILED gate, never a SKIP")))
    (check-true (>= (length parity-cases) 10) "the parity table must carry at least 10 cases")
    (define (check-declared-outcome! label side outcome expected)
      (cond
        [(eq? expected 'ok)
         (check-true (hash-ref outcome 'ok #f)
                     (format "~a: ~a must succeed, got ~s" label side outcome))]
        [else
         (check-false (hash-ref outcome 'ok #t)
                      (format "~a: ~a must refuse, got ~s" label side outcome))
         (check-equal? (hash-ref outcome 'reason #f) expected (format "~a: ~a reason" label side))]))
    (for ([case (in-list parity-cases)])
      (define label (hash-ref case 'label))
      (define fixtures-fn (hash-ref case 'fixtures-fn))
      (with-world (hash-ref case 'variant)
                  (lambda (world root log)
                    (define subject (hash-ref world 'subject))
                    (define expected (hash-ref case 'expected))
                    (define request-spec
                      (for/fold ([acc (hash-ref case 'spec)]) ([(key value) (in-hash (hash-ref case 'spec))])
                        (if (eq? value 'PLACEHOLDER)
                            (hash-set acc key (path->string root))
                            acc)))
                    (define request-spec* (hash-set request-spec 'repo (path->string subject)))
                    (define python-outcome (run-python-case root python request-spec*))
                    (define racket-outcome
                      (with-handlers ([exn:fail:delivery-action?
                                       (lambda (error)
                                         (hasheq 'ok #f 'reason (exn-message error)))])
                        (hasheq 'ok
                                #t
                                'value
                                (json-value
                                 (cond
                                   [(equal? (hash-ref request-spec* 'fn) "status")
                                    (delivery-status! subject plan-id wave #:campaign-root root)]
                                   [else
                                    (delivery-governance! subject
                                                          plan-id
                                                          wave
                                                          (hash-ref request-spec*
                                                                    'expected_branch))])))))
                    (check-declared-outcome! label "racket" racket-outcome expected)
                    (check-declared-outcome! label "python" python-outcome expected)
                    (check-equal? (canonical racket-outcome) (canonical python-outcome) label))
                  #:journal (hash-ref case 'journal)
                  #:fixtures (and fixtures-fn (lambda (world) (fixtures-fn world)))
                  #:override-main (hash-ref case 'override-main))))
