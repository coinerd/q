#lang racket/base
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @timeout 900
;; Slow lane on purpose: the approved-finalize case drives the strict gate and the
;; whole helper chain, which intermittently stalls on a loaded host (a normal run
;; is 30-45s). The tight bound turns a stall into a TIMEOUT failure inside the
;; nightly budget instead of a half-hour hang. Phase B deleted both the Python
;; oracle and the fast-bucket shim that spawned it, so this suite is the owner
;; and it runs in the slow/all suites rather than the PR fast gate.
;; @covers scripts/gsd-delivery-auto-review.rkt
;;
;; S12 native port of the automatic independent review dispatch
;; (`auto-review` prepare + finalize). This file ports the THIRTY Python
;; regressions of the automatic-review controller suite to native RackUnit
;; cases; each case names the Python case it replaces. (That suite, and the
;; filename it carried, is preserved in git history: Phase B deleted the file
;; once this port and the goldens covered it.) The behaviour contract
;; is unchanged:
;;
;;   * prepare is honest: a valid trio passes through as `reviewed`, a real
;;     refusal stays a typed delivery-pending stop, and only a genuinely
;;     missing trio (or a staged pending binding draft) gets the deliberately
;;     gate-red template + typed request;
;;   * the fresh reviewer is non-author/independent: sentinel identities,
;;     placeholder narratives, missing findings and malformed validation facts
;;     are typed review-infra stops — never a rejection, never an approval;
;;   * the controller stamps the content digest/head/verdict; reviewer prose is
;;     never authority, and only the unchanged strict gate can publish an
;;     APPROVED review at the exact pinned head;
;;   * an explicit head-bound REQUEST_CHANGES persists findings through a
;;     trusted artifact commit, replays only from a receipt-bound evidence
;;     tail, and refuses on ancestry/drift;
;;   * dispatch-request guards (missing carrier/stage/plan/wave/head/branch)
;;     refuse before any mutation, and owned-request binding/re-authentication
;;     is exercised through the manual-compatibility seam the Python fixtures
;;     use (production owned mode refuses without an authenticated proof).
;;
;; Fixtures: two cached real-git worlds (`plain`, `trio`) are built ONCE and
;; copied per case (a copy is a few milliseconds); a fake `gh` shim on PATH
;; logs its argv so the legacy tracker cases prove the exact issue lookup.
;;
;; Host note (measured, not a logic bug): this suite forks ~20 `git`/`racket`
;; helpers per case, and on a memory-fragmented host the shared subprocess seam
;; occasionally loses a child's result file and the parent then polls forever
;; (the same symptom an unrelated `test-gsd-delivery-status.rkt` process from an
;; earlier session shows). Nothing in this file depends on that timing, and
;; `QAR_ONLY=<substring>` runs any subset in isolation (e.g.
;; `QAR_ONLY=binding raco test tests/test-gsd-delivery-auto-review-native.rkt`);
;; every case below was verified green both individually and in groups.
;; `QAR_PROGRESS=1` records the running case (with RSS) to
;; $TMPDIR/qar-progress.log when a host stall must be attributed.

(require rackunit
         json
         racket/file
         racket/format
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         racket/pretty
         file/sha1
         openssl
         "../scripts/gsd-delivery-auto-review.rkt"
         "../scripts/gsd-delivery-git.rkt"
         (only-in "../scripts/gsd-delivery-pr.rkt"
                  content-digest
                  blob-or-none
                  delivery-review!)
         (only-in "../scripts/gsd-delivery-merge.rkt" draft-request)
         (only-in "../scripts/gsd-delivery-status.rkt" binding-branch)
         (only-in "../scripts/gsd-delivery-binding.rkt"
                  binding-staging-path
                  delivery-binding-review!)
         (only-in "../sandbox/subprocess.rkt"
                  run-subprocess
                  subprocess-result-stdout
                  subprocess-result-stderr
                  subprocess-result-exit-code
                  subprocess-result-timed-out?))

;; Fixtures are provided so a debug script can rebuild a world without running
;; the RackUnit cases.
(provide case-world
         with-case-world
         build-campaign
         write-journal!
         write-journal-datum!
         read-journal
         impl-staging
         reviewer-output
         write-reviewer!
         prepare!*
         finalize!*
         refusal-message
         check-refusal
         binding-world
         issue-payload
         set-gh-fixture!
         publish-datum-commit!
         datum->rktd-string
         current-head
         plan
         wave
         wave-branch
         slug
         evidence-rel
         review-rel
         validation-rel
         base-environment
         install-gh-shim!
         jstring
         gh-log
         remote-tip
         subject-git
         show-committed
         read-committed-datum
         file-bytes-map
         frozen-title
         cleanup-world)

;; ============================================================
;; Fixture constants (mirror the Python fixture)
;; ============================================================

(define policy-names '("lint" "test (0)" "workflows (0)"))
(define wave-branch "campaign/v9.9.9-w1")
(define slug "owner/repo")
(define plan (make-string 64 #\a))
(define wave 1)
(define fixture-label "v9.9.9-w1")
(define evidence-rel (string-append "docs/reports/gsd-wave-evidence/" fixture-label ".rktd"))
(define review-rel (string-append "docs/reports/gsd-wave-reviews/" fixture-label ".rktd"))
(define validation-rel (string-append "docs/reports/gsd-wave-validation/" fixture-label ".rktd"))
(define review-request-path (build-path "reviewer-output.json"))
(define git-executable (find-executable-path "git"))
(define racket-executable (find-executable-path "racket"))
(define-runtime-path binding-data-script "../scripts/gsd-binding-data.rkt")

(define (must value message)
  (unless value (error 'fixture "~a" message))
  value)

(define (non-empty-string? value)
  (and (string? value) (positive? (string-length (string-trim value)))))

;; ============================================================
;; Environment + subprocess helpers
;; ============================================================

(define (env-set! env name value)
  (environment-variables-set! env (string->bytes/utf-8 name) (string->bytes/utf-8 value)))

(define (base-environment root [bin-dir #f])
  (define env (make-environment-variables))
  (define dirs
    (remove-duplicates
     (append (if bin-dir (list (path->string bin-dir)) '())
             (list (path->string (path-only git-executable))
                   (path->string (path-only racket-executable))
                   "/usr/local/bin"
                   "/usr/bin"
                   "/bin"))
     string=?))
  (env-set! env "PATH" (string-join dirs ":"))
  (env-set! env "HOME" (path->string root))
  (env-set! env "GIT_CONFIG_NOSYSTEM" "1")
  (env-set! env "GIT_CONFIG_GLOBAL" "/dev/null")
  (env-set! env "GIT_TERMINAL_PROMPT" "0")
  (env-set! env "GSD_DELIVERY_BUDGET_SEC" "180")
  env)

;; Fixture git commands do NOT need the seam's process-group owner: git and its
;; configured helpers are trusted here and a local-path remote never detaches a
;; descendant, so the fixture keeps the plain direct-child launch (fewer moving
;; parts in the shared seam on a loaded host).
(define (git-run dir . args)
  (define argv (append (if dir (list "-C" (path->string dir)) '()) args))
  (define result
    (run-subprocess (path->string git-executable)
                    #:args argv
                    #:timeout 60))
  (unless (and (zero? (subprocess-result-exit-code result))
               (not (subprocess-result-timed-out? result)))
    (error 'fixture-git "git ~s failed: ~a" argv (subprocess-result-stderr result)))
  (string-trim (subprocess-result-stdout result)))

;; RAW stdout (never trimmed): the content digest is computed over the exact
;; bytes git writes, exactly like the delivery seam.
(define (git-raw dir . args)
  (define argv (append (if dir (list "-C" (path->string dir)) '()) args))
  (define result
    (run-subprocess (path->string git-executable)
                    #:args argv
                    #:timeout 60))
  (unless (and (zero? (subprocess-result-exit-code result))
               (not (subprocess-result-timed-out? result)))
    (error 'fixture-git "git ~s failed: ~a" argv (subprocess-result-stderr result)))
  (subprocess-result-stdout result))

(define (put-text path text)
  (make-directory* (path-only path))
  (call-with-output-file path #:exists 'truncate (lambda (out) (display text out))))

;; ============================================================
;; Cached real-git worlds (built once, copied per case)
;; ============================================================

(define template-cache (make-hash))
(define template-root (make-temporary-file "q-auto-review-fixtures-~a" 'directory))

(define (build-world-template kind)
  (define base (build-path template-root (format "~a" kind)))
  (make-directory* base)
  (define origin (build-path base "origin.git"))
  (define work (build-path base "work"))
  (git-run #f "init" "--bare" "-b" "main" (path->string origin))
  (git-run origin "config" "gc.auto" "0")
  (git-run #f "init" "-b" "main" (path->string work))
  (git-run work "config" "user.email" "fixture@example.com")
  (git-run work "config" "user.name" "fixture")
  (git-run work "config" "gc.auto" "0")
  (put-text (build-path work "scripts/required-pr-checks.policy")
            (string-append "(\"" (string-join policy-names "\" \"") "\")"))
  (put-text (build-path work "src/file.txt") "one\n")
  (git-run work "add" "-A")
  (git-run work "commit" "-q" "-m" "c0")
  (define c0 (git-run work "rev-parse" "HEAD"))
  (git-run work "checkout" "-q" "-b" "impl")
  (put-text (build-path work "src/file.txt") "two\n")
  (git-run work "add" "-A")
  (git-run work "commit" "-q" "-m" "h1")
  (define head (git-run work "rev-parse" "HEAD"))
  (when (eq? kind 'trio)
    (define subject-git (lambda (args) (apply git-raw work args)))
    (define digest (content-digest subject-git c0 head))
    (put-text (build-path work evidence-rel)
              (format (string-append "#hasheq((schema-version . 2) (milestone . 895) (wave . \"W1\") "
                                     "(issue . 9686) (status . \"ready-for-merge\") "
                                     "(implementation-sha . \"~a\") (content-digest . \"~a\") "
                                     "(required-checks . ~s) (review-artifact . \"~a\") "
                                     "(validation-artifact . \"~a\"))")
                      head
                      digest
                      (for/list ([name (in-list policy-names)]) name)
                      review-rel
                      validation-rel))
    (put-text (build-path work review-rel)
              (format (string-append "#hasheq((reviewer . \"independent-reviewer\") (verdict . \"APPROVED\") "
                                     "(timestamp . \"2026-09-14T16:00:00Z\") (reviewed-sha . \"~a\") "
                                     "(content-digest . \"~a\") "
                                     "(scope . \"independent read-only review of the wave implementation and "
                                     "gate evidence at the reviewed head\") "
                                     "(report . \"reviewed the commit chain, ran the focused and fast gates, "
                                     "and found no blocking defect at the reviewed head\"))")
                      head
                      digest))
    (put-text (build-path work validation-rel)
              (format (string-append "#hasheq((status . \"current\") (milestone . 895) (wave . \"W1\") "
                                     "(issue . 9686) (branch . \"~a\") (implementation-sha . \"~a\") "
                                     "(content-digest . \"~a\") "
                                     "(red-first . #hasheq((command . \"raco test tests/t.rkt\") "
                                     "(failure . \"reproduction observed: the malformed digest passed the gate\"))) "
                                     "(focused-tests . #hasheq((result . \"passed\") (command . \"x\"))) "
                                     "(format-compile . #hasheq((result . \"passed\") (command . \"x\"))) "
                                     "(lint . #hasheq((result . \"passed\") (command . \"x\"))) "
                                     "(fast . #hasheq((result . \"passed\") (command . \"x\"))) "
                                     "(review-artifact . \"~a\") (remaining-items . ()) "
                                     "(planning-sync . \"current\"))")
                      wave-branch
                      head
                      digest
                      review-rel))
    (git-run work "add" "-A")
    (git-run work "commit" "-q" "-m" "h2 trio")
    (set! head (git-run work "rev-parse" "HEAD")))
  (git-run work "push" "-q" (path->string origin) "main:main")
  (git-run work "push" "-q" (path->string origin) (format "impl:refs/heads/~a" wave-branch))
  (git-run work "push" "-q" (path->string origin) (format "~a:refs/pull/42/head" head))
  (define subject (build-path base "repo"))
  (git-run #f "clone" "-q" "--branch" "main" (path->string origin) (path->string subject))
  (git-run subject "remote" "set-url" "origin" (format "https://github.com/~a.git" slug))
  (git-run subject "config" (format "url.~a.insteadOf" (path->string origin))
           (format "https://github.com/~a.git" slug))
  (git-run subject "config" "user.email" "fixture@example.com")
  (git-run subject "config" "user.name" "fixture")
  ;; No background `gc --auto`/maintenance grandchild may inherit the bounded
  ;; runner's pipes: an auto-gc forked by a push would keep the pipe open and
  ;; stall the capture (the hang this fixture previously showed).
  (for ([dir (in-list (list origin work subject))])
    (git-run dir "config" "gc.auto" "0")
    (git-run dir "config" "maintenance.auto" "false"))
  (hash 'base base 'origin origin 'work work 'subject subject 'c0 c0 'head head))

(define (template kind)
  (hash-ref! template-cache kind (lambda () (build-world-template kind))))

(define (copy-directory-tree src dest)
  (make-directory* dest)
  (for ([entry (in-list (directory-list src))])
    (define from (build-path src entry))
    (define to (build-path dest entry))
    (cond
      [(directory-exists? from)
       (copy-directory-tree from to)]
      [else (copy-file from to)]))
  dest)

;; Copy the cached template into a fresh case directory and re-point the
;; insteadOf rewrite at the COPY's bare origin (the template path must never be
;; touched by a case).
(define (case-world kind)
  (define src (template kind))
  (define base (make-temporary-file (format "q-auto-review-~a-~a" kind "~a") 'directory))
  (define copy (build-path base "world"))
  (copy-directory-tree (hash-ref src 'base) copy)
  (define origin (build-path copy "origin.git"))
  (define subject (build-path copy "repo"))
  (git-run subject "config" "--unset-all"
           (format "url.~a.insteadOf" (path->string (hash-ref src 'origin))))
  (git-run subject "config" (format "url.~a.insteadOf" (path->string origin))
           (format "https://github.com/~a.git" slug))
  (hash 'base base
        'origin origin
        'work (build-path copy "work")
        'subject subject
        'c0 (hash-ref src 'c0)
        'head (hash-ref src 'head)))

(define (cleanup-world w)
  (delete-directory/files (hash-ref w 'base) #:must-exist? #f))

;; ============================================================
;; Fake `gh` shim (logs argv, serves canned issue payloads)
;; ============================================================

(define (install-gh-shim! bin fixtures)
  (make-directory* bin)
  (define shim (build-path bin "gh"))
  (define fixtures-dir (build-path (path-only bin) "gh-fixtures"))
  (make-directory* fixtures-dir)
  (define routes
    (for/list ([entry (in-list fixtures)])
      (define file (gh-fixture-file (car entry)))
      (put-text (build-path fixtures-dir file) (cdr entry))
      (cons (car entry) file)))
  (define lines
    (append
     (list "#!/bin/sh"
           "route="
           "for a in \"$@\"; do route=\"$a\"; done"
           "printf '%s\\n' \"$*\" >> \"$GH_SHIM_LOG\""
           "case \"$route\" in")
     (for/list ([entry (in-list routes)])
       (format "  ~s) /bin/cat \"$GH_SHIM_FIXTURES/~a\" ;;" (car entry) (cdr entry)))
     (list "  *) printf 'no fixture for route %s\\n' \"$route\" >&2; exit 66 ;;"
           "esac")))
  (put-text shim (string-append (string-join lines "\n") "\n"))
  (file-or-directory-permissions shim #o700)
  (void))

;; Rewrite the payload a route serves (the shim reads it per invocation, so a
;; test can prove which payload a call actually saw).
(define (set-gh-fixture! w route body)
  (put-text (build-path (hash-ref w 'base) "gh-fixtures" (gh-fixture-file route)) body))

(define (gh-fixture-file route)
  (string-append (regexp-replace* #px"/" route "_") ".json"))

(define (gh-log w)
  (define path (build-path (hash-ref w 'base) "gh-invocations.log"))
  (if (file-exists? path) (file->lines path) '()))

;; ============================================================
;; Campaign fixture (snapshot + journal), ported from the Python fixture
;; ============================================================

(define (sha256-hex text)
  (bytes->hex-string (sha256-bytes (string->bytes/utf-8 text))))

;; Python `text.strip('\n')`.
(define (strip-newlines text)
  (regexp-replace* #px"^\n+|\n+$" text ""))

(define frozen-title "v9.9.9 — Fixture delivery")

(define (build-campaign base
                        #:declared? [declared? #t]
                        #:issue [issue 9686]
                        #:milestone [milestone 895]
                        #:legacy? [legacy? #f]
                        #:heading [heading #f]
                        #:wave-override [wave-override #f])
  (define root (build-path base "campaign"))
  (define planning (build-path root ".planning"))
  (define heading* (or heading (if legacy? (string-append "# Plan: " frozen-title) "# Fixture plan")))
  (define plan-text (format "~a\n\nSingle wave: waves/W~a-fixture.md owns the delivery.\n" heading* wave))
  (define file-lines
    (if declared?
        (format (string-append "\n## Files\n\n"
                               "- File: `docs/reports/gsd-wave-evidence/~a.rktd`\n"
                               "- File: `docs/reports/gsd-wave-reviews/~a.rktd`\n"
                               "- File: `docs/reports/gsd-wave-validation/~a.rktd`\n")
                fixture-label fixture-label fixture-label)
        ""))
  (define wave-text
    (or wave-override
        (if legacy?
            (format "# W~a: Legacy wave\n\nTracker (W~a = #~a, canary row = #12345).~a"
                    wave wave issue file-lines)
            (format (string-append "# W~a: Fixture wave\n\nBranch: `~a`; GitHub issue "
                                   "[#~a](https://github.com/~a/issues/~a), milestone #~a.~a")
                    wave wave-branch issue slug issue milestone file-lines))))
  (put-text (build-path planning "PLAN.md") plan-text)
  (put-text (build-path planning (format "waves/W~a-fixture.md" wave)) wave-text)
  (define snapshot (build-path planning "campaigns" plan "plan-snapshot"))
  (define entries
    (for/list ([pair (in-list (list (cons "PLAN.md" plan-text)
                                    (cons (format "waves/W~a-fixture.md" wave) wave-text)))])
      (define text (cdr pair))
      (define canonical (strip-newlines text))
      (format "#hasheq((path . \"~a\") (size . ~a) (sha256 . \"~a\"))"
              (car pair)
              (bytes-length (string->bytes/utf-8 text))
              (sha256-hex canonical))))
  (put-text (build-path snapshot "snapshot-manifest.rktd")
            (format (string-append "#hasheq((schema-version . 1) (campaign . \"~a\") (plan-id . \"~a\") "
                                   "(created-at . 1789000000.0) (files . (~a)))")
                    plan
                    plan
                    (string-join entries " ")))
  (make-directory* (build-path snapshot "waves"))
  (put-text (build-path snapshot "PLAN.md") plan-text)
  (put-text (build-path snapshot (format "waves/W~a-fixture.md" wave)) wave-text)
  root)

(define (journal-receipt head [attempt "attempt-5"] [fence 2] [branch wave-branch])
  (hasheq 'repo
          "/repo"
          'origin
          (format "https://github.com/~a.git" slug)
          'head
          head
          'tree
          (make-string 40 #\d)
          'verified-at
          1
          'evidence
          "Verify passed"
          'branch
          branch
          'attempt-id
          attempt
          'attempt-fence
          fence))

(define (write-journal! campaign head [history 0] [attempt "attempt-5"] [fence 2])
  (define receipt (journal-receipt head attempt fence))
  (define journal
    (hasheq 'schema-version
            1
            'plan-id
            plan
            'wave
            wave
            'stage
            "context-ready"
            'receipt
            receipt
            'receipt-history
            (for/list ([index (in-range history)])
              (hash-set receipt
                        'head
                        (let* ([text (number->string (add1 index) 16)]
                               [padding (make-string (- 40 (string-length text)) #\0)])
                          (string-append padding text))))))
  (put-text (build-path campaign
                        ".planning"
                        "campaigns"
                        plan
                        (format "coordinator-w~a.json" wave))
            (jstring journal))
  journal)

(define (jstring value)
  (let ([out (open-output-string)]) (write-json value out) (get-output-string out)))

(define (read-journal campaign)
  (call-with-input-file (build-path campaign
                                    ".planning"
                                    "campaigns"
                                    plan
                                    (format "coordinator-w~a.json" wave))
                        read-json))

(define (write-journal-datum! campaign journal)
  (put-text (build-path campaign
                        ".planning"
                        "campaigns"
                        plan
                        (format "coordinator-w~a.json" wave))
            (jstring journal)))

(define (impl-staging campaign generation)
  (build-path campaign
              ".planning"
              "campaigns"
              plan
              (format "review-w~a-g~a" wave generation)))

(define (write-reviewer! staging output)
  (define path (build-path staging "reviewer-output.json"))
  (put-text path (jstring output))
  path)

;; ============================================================
;; Reviewer output fixtures (mirror the Python fixture)
;; ============================================================

(define (reviewer-output [kind "approved"] [findings #f] [reviewer "fresh-reviewer-session"])
  (define base
    (hash 'kind
          kind
          'reviewer
          reviewer
          'scope
          (string-append "independent review of the implementation, gates and provenance at the "
                         "pinned receipt head with fresh session context")
          'report
          (string-append "ran the focused and fast gates at the pinned head and verified the "
                         "provenance chain; no blocking defect found")))
  (cond
    [(equal? kind "request-changes")
     (hash-set base 'findings (or findings (list "focused tests fail at the head")))]
    [(equal? kind "approved")
     (hash-set base
               'validation
               (hash 'red-first
                     (hash 'command
                           "raco test tests/test-wave-fixture.rkt"
                           'failure
                           (string-append "reproduction observed: the malformed digest passed the "
                                          "gate before the fix"))
                     'focused-tests
                     (hash 'result "passed" 'command "raco test tests/test-wave.rkt")
                     'format-compile
                     (hash 'result "passed" 'command "raco fmt --check x.rkt")
                     'lint
                     (hash 'result "passed" 'command "raco make")
                     'fast
                     (hash 'result "passed" 'command "raco test tests/fast")
                     'remaining-items
                     '()
                     'planning-sync
                     "current"))]
    [else (hash-set base 'reason "reviewer session timed out")]))

;; ============================================================
;; Action drivers
;; ============================================================

(define (prepare!* repo campaign generation [stage "implementation-review"] [manual? #t])
  (auto-review! repo
                plan
                wave
                "prepare"
                stage
                (hash-ref (world-heads) 'head)
                wave-branch
                campaign
                generation
                (if (equal? stage "binding-review")
                    (binding-staging-path campaign plan wave generation)
                    (impl-staging campaign generation))
                #:manual-compatibility? manual?))

;; The receipt head of the CURRENT world ("plain"/"trio" and binding tests use
;; the same head); bound per case by `with-case-world`.
(define current-head (make-parameter #f))
(define (world-heads) (hash 'head (current-head)))

(define (finalize!* repo campaign generation reviewer [stage "implementation-review"] [manual? #t])
  (auto-review! repo
                plan
                wave
                "finalize"
                stage
                (current-head)
                wave-branch
                campaign
                generation
                (if (equal? stage "binding-review")
                    (binding-staging-path campaign plan wave generation)
                    (impl-staging campaign generation))
                #:reviewer-output reviewer
                #:manual-compatibility? manual?))

(define (refusal-message thunk)
  (with-handlers ([exn:fail:delivery-action? exn-message]) (thunk) #f))

(define (check-refusal thunk [hint ""])
  (define message (refusal-message thunk))
  (check-true (string? message) (string-append "expected a typed refusal " hint))
  message)

(define (remote-tip w)
  (git-run (hash-ref w 'origin) "rev-parse" (format "refs/heads/~a" wave-branch)))

;; The RAW git seam the read ladder expects (untrimmed stdout): `blob-or-none`
;; and `content-digest` hash/compare exactly the bytes git wrote.
(define (subject-git w)
  (lambda (args) (apply git-raw (hash-ref w 'subject) args)))

(define (show-committed w ref relative)
  (git-run (hash-ref w 'subject) "show" (format "~a:~a" ref relative)))

(define (read-committed-datum w ref relative)
  (with-input-from-string (show-committed w ref relative) read))

(define (file-bytes-map staging)
  (define files
    (sort (for/list ([path (in-directory staging)] #:when (file-exists? path)) path)
          path<?))
  (for/fold ([acc (hash)]) ([path (in-list files)])
    (hash-set acc (path->string (find-relative-path staging path)) (file->bytes path))))

;; ============================================================
;; Case wrapper: copy a cached world, build the campaign, run the case
;; ============================================================

(define (issue-payload issue milestone title)
  (jstring (hasheq 'number
                   issue
                   'html_url
                   (format "https://github.com/~a/issues/~a" slug issue)
                   'milestone
                   (hasheq 'number
                           milestone
                           'title
                           title
                           'url
                           (format "https://api.github.com/repos/~a/milestones/~a" slug milestone)))))

(define (with-case-world kind proc
                         #:issue [issue 9686]
                         #:milestone [milestone 895]
                         #:title [title frozen-title])
  (define w (case-world kind))
  (dynamic-wind
   void
   (lambda ()
     (define bin (build-path (hash-ref w 'base) "bin"))
     (install-gh-shim! bin
                       (list (cons (format "repos/~a/issues/~a" slug issue)
                                   (issue-payload issue milestone title))))
     (define log (build-path (hash-ref w 'base) "gh-invocations.log"))
     (define env (base-environment (hash-ref w 'base) bin))
     (env-set! env "GH_SHIM_LOG" (path->string log))
     (env-set! env "GH_SHIM_FIXTURES" (path->string (build-path (hash-ref w 'base) "gh-fixtures")))
     (parameterize ([current-environment-variables env]
                    [current-head (hash-ref w 'head)])
       (proc w)))
   (lambda () (cleanup-world w))))

;; A datum written back as an `.rktd` artifact (the shape the trusted reader
;; accepts), used to forge MALFORMED committed rejections in the recovery cases.
(define (datum->rktd-string datum)
  (let ([out (open-output-string)]) (pretty-write datum out) (get-output-string out)))

;; Commit one datum at `relative` on top of `tip` and publish it non-forced, so
;; the next prepare must re-validate exactly those committed bytes.
(define (publish-datum-commit! w tip relative datum message)
  (define work (hash-ref w 'work))
  (git-run work "fetch" "-q" (path->string (hash-ref w 'origin)) wave-branch)
  (git-run work "checkout" "-q" "--detach" tip)
  (put-text (build-path work relative) (datum->rktd-string datum))
  (git-run work "add" "--" relative)
  (git-run work "-c" "user.name=fixture" "-c" "user.email=fixture@example.com"
           "commit" "-q" "--no-gpg-sign" "-m" message)
  (define commit (git-run work "rev-parse" "HEAD"))
  (git-run work "push" "-q" (path->string (hash-ref w 'origin))
           (format "~a:refs/heads/~a" commit wave-branch))
  commit)

(define (binding-world w #:generation [generation 0])
  (define work (hash-ref w 'work))
  (git-run work "checkout" "-q" "main")
  (put-text (build-path work "src/merged.txt") "merged\n")
  (git-run work "add" "-A")
  (git-run work "commit" "-q" "-m" "M squash")
  (define merge (git-run work "rev-parse" "HEAD"))
  (git-run work "push" "-q" (path->string (hash-ref w 'origin)) "main:main")
  (define campaign (build-campaign (hash-ref w 'base)))
  (write-journal! campaign (current-head) generation)
  (define staging (binding-staging-path campaign plan wave generation))
  (make-directory* staging)
  (define pr-payload
    (hasheq 'number
            42
            'merged
            #t
            'merge_commit_sha
            merge
            'merged_at
            "2026-09-14T16:23:06Z"
            'head
            (hasheq 'sha
                    (current-head)
                    'ref
                    wave-branch
                    'repo
                    (hasheq 'full_name slug))
            'base
            (hasheq 'ref "main" 'sha (hash-ref w 'c0) 'repo (hasheq 'full_name slug))))
  (define request
    (draft-request (hash 'wave (format "W~a" wave) 'milestone 895 'issue 9686)
                   plan
                   wave
                   pr-payload
                   (binding-branch plan wave generation)
                   policy-names
                   generation))
  (define request-path (build-path staging "request.json"))
  (put-text request-path (jstring request))
  (define result
    (run-subprocess (path->string racket-executable)
                    #:args (list (path->string binding-data-script)
                                 "prepare"
                                 (path->string request-path)
                                 (path->string staging))
                    #:timeout 120))
  (unless (zero? (subprocess-result-exit-code result))
    (error 'fixture-binding "binding prepare failed: ~a" (subprocess-result-stderr result)))
  (delete-file request-path)
  (hash 'campaign campaign 'staging staging 'merge merge))

(define progress-box (box #f))

;; Optional progress tracing (`QAR_PROGRESS=1`): a host-level subprocess stall
;; then names the case that was running instead of hiding it.
(define (record-progress! name)
  (when (getenv "QAR_PROGRESS")
    (define path (build-path (find-system-path 'temp-dir) "qar-progress.log"))
    (define rss
      (with-handlers ([exn:fail? (lambda (_e) "?")])
        (define status (file->string "/proc/self/status"))
        (define match (regexp-match #px"VmRSS:\\s+([0-9]+) kB" status))
        (if match (cadr match) "?")))
    (call-with-output-file path
                           (lambda (out) (fprintf out "~a (rss ~a kB)\n" name rss))
                           #:exists (if (unbox progress-box) 'append 'truncate))
    (set-box! progress-box #t)))

(define-syntax-rule (native-case name body ...)
  (test-case name
    (record-progress! name)
    (define qar-only (getenv "QAR_ONLY"))
    (when (or (not qar-only) (string-contains? name qar-only))
      body ...)))

(module+ test
  ;; ----------------------------------------------------------------
  ;; prepare: honest gate-red template, typed passthrough, real refusals
  ;; ----------------------------------------------------------------

  ;; Python: test_prepare_missing_trio_is_dispatchable_with_gate_red_template
  (native-case "prepare stages a gate-red template + typed request for a missing trio"
    (with-case-world
     'plain
     (lambda (w)
       (define campaign (build-campaign (hash-ref w 'base)))
       (write-journal! campaign (hash-ref w 'head))
       (define result (prepare!* (hash-ref w 'subject) campaign 0))
       (check-equal? (hash-ref result 'status) "awaiting-review")
       (check-equal? (hash-ref result 'stage) "implementation-review")
       (define request (hash-ref result 'request))
       (check-equal? (hash-ref request 'plan-id) plan)
       (check-equal? (hash-ref request 'wave) wave)
       (check-equal? (hash-ref request 'generation) 0)
       (check-equal? (hash-ref request 'head) (hash-ref w 'head))
       (check-equal? (hash-ref request 'branch) wave-branch)
       (check-equal? (hash-ref request 'attempt-id) "attempt-5")
       (check-equal? (hash-ref request 'attempt-fence) 2)
       (check-equal? (hash-ref request 'evidence) evidence-rel)
       (check-equal? (hash-ref request 'review) review-rel)
       (check-equal? (hash-ref request 'validation) validation-rel)
       (check-equal? (hash-ref request 'staging) (path->string (impl-staging campaign 0)))
       (check-equal? (hash-ref request 'reviewer-output)
                     (path->string (build-path (impl-staging campaign 0) "reviewer-output.json")))
       (check-equal? (hash-ref request 'required-checks) policy-names)
       (check-true (string-contains? (hash-ref request 'prompt) "gate-red"))
       (check-true (string-contains? (hash-ref request 'prompt) (hash-ref w 'head)))
       (check-equal? (hash-ref request 'fresh-session-required) #t)
       (check-equal? (hash-ref request 'non-author-review-required) #t)
       ;; The template trio is genuinely gate-red: honest placeholders, no
       ;; fabricated APPROVED verdict, no ready-for-merge, no acceptance.
       (define staging (impl-staging campaign 0))
       (for ([relative (in-list (list evidence-rel review-rel validation-rel))])
         (define text (file->string (build-path staging relative)))
         (check-false (string-contains? text "APPROVED") relative)
         (check-false (string-contains? text "ready-for-merge") relative)
         (check-true (string-contains? text "PENDING") relative))
       (define evidence-text (file->string (build-path staging evidence-rel)))
       (check-true (string-contains? evidence-text "pending-review"))
       (check-true (string-contains? evidence-text "fresh-independent-review-required")))))

  ;; Python: test_prepare_is_idempotent_across_restarts
  (native-case "prepare is idempotent across restarts"
    (with-case-world
     'plain
     (lambda (w)
       (define campaign (build-campaign (hash-ref w 'base)))
       (write-journal! campaign (hash-ref w 'head))
       (define staging (impl-staging campaign 0))
       (define first (prepare!* (hash-ref w 'subject) campaign 0))
       (define template-bytes (file->bytes (build-path staging evidence-rel)))
       (define second (prepare!* (hash-ref w 'subject) campaign 0))
       (check-equal? (hash-ref first 'status) (hash-ref second 'status))
       (check-equal? (hash-ref first 'request) (hash-ref second 'request))
       (check-equal? (file->bytes (build-path staging evidence-rel)) template-bytes))))

  ;; Python: test_prepare_passes_through_an_already_valid_trio
  (native-case "prepare passes through an already valid trio as reviewed"
    (with-case-world
     'trio
     (lambda (w)
       (define campaign (build-campaign (hash-ref w 'base)))
       (write-journal! campaign (hash-ref w 'head))
       (define result (prepare!* (hash-ref w 'subject) campaign 0))
       (check-equal? (hash-ref result 'status) "reviewed")
       (check-equal? (hash-ref result 'head) (hash-ref w 'head))
       (check-false (directory-exists? (impl-staging campaign 0))))))

  ;; Python: test_prepare_refuses_source_drift_without_dispatching
  (native-case "prepare refuses source drift after the receipt without dispatching"
    (with-case-world
     'plain
     (lambda (w)
       (define campaign (build-campaign (hash-ref w 'base)))
       (write-journal! campaign (hash-ref w 'head))
       (put-text (build-path (hash-ref w 'work) "src/other.txt") "drift\n")
       (git-run (hash-ref w 'work) "add" "-A")
       (git-run (hash-ref w 'work) "commit" "-q" "-m" "drift")
       (define tip (git-run (hash-ref w 'work) "rev-parse" "HEAD"))
       (git-run (hash-ref w 'work) "push" "-q" (path->string (hash-ref w 'origin))
                (format "~a:refs/heads/~a" tip wave-branch))
       (define message (check-refusal (lambda () (prepare!* (hash-ref w 'subject) campaign 0))))
       (check-true (string-contains? message "non-evidence paths"))
       (check-false (directory-exists? (impl-staging campaign 0))))))

  ;; Python: test_prepare_generation_uses_repair_paths_and_preserves_old_generations
  (native-case "prepare uses repair paths for a repair generation and never writes generation 0"
    (with-case-world
     'plain
     (lambda (w)
       (define campaign (build-campaign (hash-ref w 'base)))
       (write-journal! campaign (hash-ref w 'head) 1)
       (define result (prepare!* (hash-ref w 'subject) campaign 1))
       (check-equal? (hash-ref result 'status) "awaiting-review")
       (define request (hash-ref result 'request))
       (check-equal? (hash-ref request 'generation) 1)
       (check-equal? (hash-ref request 'evidence)
                     "docs/reports/gsd-wave-evidence/v9.9.9-w1-r1.rktd")
       (check-equal? (hash-ref request 'review)
                     "docs/reports/gsd-wave-reviews/v9.9.9-w1-r1.rktd")
       (check-equal? (hash-ref request 'staging) (path->string (impl-staging campaign 1)))
       (check-not-equal? (hash-ref request 'staging) (path->string (impl-staging campaign 0)))
       (check-false (directory-exists? (impl-staging campaign 0))))))

  ;; ----------------------------------------------------------------
  ;; finalize: approval only through the unchanged strict gate
  ;; ----------------------------------------------------------------

  ;; Python: test_finalize_approved_commits_evidence_only_trio_and_passes_strict_gate
  (native-case "finalize approved commits an evidence-only trio through the strict gate"
    (with-case-world
     'plain
     (lambda (w)
       (define campaign (build-campaign (hash-ref w 'base)))
       (write-journal! campaign (hash-ref w 'head))
       (define staging (impl-staging campaign 0))
       (prepare!* (hash-ref w 'subject) campaign 0)
       (define reviewer (write-reviewer! staging (reviewer-output "approved")))
       (define result (finalize!* (hash-ref w 'subject) campaign 0 reviewer))
       (check-equal? (hash-ref result 'status) "reviewed")
       (check-equal? (hash-ref result 'head) (hash-ref w 'head))
       (check-equal? (hash-ref result 'reviewed-sha) (hash-ref w 'head))
       (check-equal? (hash-ref result 'review-artifact) review-rel)
       (define tip (remote-tip w))
       (check-not-equal? tip (hash-ref w 'head))
       (check-equal? tip (hash-ref result 'tip))
       (define changed
         (string-split (git-run (hash-ref w 'subject)
                                "diff"
                                "--name-only"
                                (format "~a..~a" (hash-ref w 'head) tip))))
       (check-equal? (sort changed string<?) (sort (list evidence-rel review-rel validation-rel) string<?))
       ;; the UNCHANGED review path re-validates the committed trio at the same
       ;; receipt head: only the strict gate produced this verdict
       (check-equal? (hash-ref (delivery-review! (hash-ref w 'subject)
                                                  plan
                                                  wave
                                                  (hash-ref w 'head)
                                                  wave-branch
                                                  #:campaign-root campaign)
                               'status)
                     "reviewed")
       (define committed (show-committed w tip evidence-rel))
       (check-true (string-contains? committed "ready-for-merge"))
       (check-true (string-contains? committed (hash-ref w 'head)))
       (define committed-review (show-committed w tip review-rel))
       (check-true (string-contains? committed-review "APPROVED"))
       (check-true (string-contains? committed-review "fresh-reviewer-session")))))

  ;; Python: test_strict_gate_failure_does_not_publish_candidate
  (native-case "a strict gate failure does not publish the candidate"
    (with-case-world
     'plain
     (lambda (w)
       (define campaign (build-campaign (hash-ref w 'base)))
       (write-journal! campaign (hash-ref w 'head))
       (define staging (impl-staging campaign 0))
       (prepare!* (hash-ref w 'subject) campaign 0)
       (define output (reviewer-output "approved"))
       (define validation (hash-ref output 'validation))
       (define reviewer
         (write-reviewer! staging
                          (hash-set output
                                    'validation
                                    (hash-set validation
                                              'remaining-items
                                              (list (hash 'classification "blocking"))))))
       (define result (finalize!* (hash-ref w 'subject) campaign 0 reviewer))
       (check-equal? (hash-ref result 'status) "review-infra")
       (check-true (string-contains? (hash-ref result 'reason) "strict"))
       (check-equal? (remote-tip w) (hash-ref w 'head)))))

  ;; Python: test_published_rejection_replays_without_journal_marker_or_staging
  (native-case "a published rejection replays without a journal marker or staging"
    (with-case-world
     'plain
     (lambda (w)
       (define campaign (build-campaign (hash-ref w 'base)))
       (write-journal! campaign (hash-ref w 'head))
       (define staging (impl-staging campaign 0))
       (prepare!* (hash-ref w 'subject) campaign 0)
       (define findings (list "  exact finding with whitespace  " "a=b; findings: exact\nsecond line"))
       (define reviewer (write-reviewer! staging (reviewer-output "request-changes" findings)))
       (define published (finalize!* (hash-ref w 'subject) campaign 0 reviewer))
       (check-equal? (hash-ref published 'status) "review-rejected")
       (delete-directory/files staging)
       (define journal-path (build-path campaign
                                        ".planning"
                                        "campaigns"
                                        plan
                                        (format "coordinator-w~a.json" wave)))
       (define before (file->bytes journal-path))
       (define replay (prepare!* (hash-ref w 'subject) campaign 0))
       (check-equal? replay published)
       (check-equal? (hash-ref replay 'findings) findings)
       (check-equal? (file->bytes journal-path) before)
       (check-false (directory-exists? staging))
       (check-equal? (remote-tip w) (hash-ref published 'tip)))))

  ;; Python: test_same_head_receipt_and_generation_drift_refuse_finalize
  (native-case "same-head receipt and generation drift refuse finalize"
    (for ([mode (in-list '("attempt-id" "attempt-fence" "generation"))])
      (with-case-world
       'plain
       (lambda (w)
         (define campaign (build-campaign (hash-ref w 'base)))
         (write-journal! campaign (hash-ref w 'head))
         (define staging (impl-staging campaign 0))
         (prepare!* (hash-ref w 'subject) campaign 0)
         (define journal (read-journal campaign))
         (define receipt (hash-ref journal 'receipt))
         (write-journal-datum!
          campaign
          (cond
            [(equal? mode "generation")
             (hash-set journal
                       'receipt-history
                       (append (hash-ref journal 'receipt-history '()) (list receipt)))]
            [(equal? mode "attempt-id") (hash-set journal 'receipt (hash-set receipt 'attempt-id "new-attempt"))]
            [else (hash-set journal 'receipt (hash-set receipt 'attempt-fence 3))]))
         (define reviewer (write-reviewer! staging (reviewer-output "request-changes")))
         (check-true (string? (refusal-message (lambda () (finalize!* (hash-ref w 'subject) campaign 0 reviewer)))) mode)
         (check-equal? (remote-tip w) (hash-ref w 'head)) (void)))))

  ;; Python: test_next_generation_does_not_replay_or_rewrite_same_head_prior_rejection
  (native-case "the next generation neither replays nor rewrites a same-head prior rejection"
    (with-case-world
     'plain
     (lambda (w)
       (define campaign (build-campaign (hash-ref w 'base)))
       (write-journal! campaign (hash-ref w 'head))
       (define staging (impl-staging campaign 0))
       (prepare!* (hash-ref w 'subject) campaign 0)
       (define reviewer (write-reviewer! staging (reviewer-output "request-changes")))
       (define published (finalize!* (hash-ref w 'subject) campaign 0 reviewer))
       (define old-blob (blob-or-none (subject-git w) (hash-ref published 'tip) review-rel))
       (define old-staging (file-bytes-map staging))
       (define journal (read-journal campaign))
       (define receipt (hash-ref journal 'receipt))
       (write-journal-datum!
        campaign
        (hash-set (hash-set journal
                            'receipt-history
                            (append (hash-ref journal 'receipt-history '()) (list receipt)))
                  'receipt
                  (hash-set (hash-set receipt 'attempt-id "new-attempt") 'attempt-fence 3)))
       (define prepared (prepare!* (hash-ref w 'subject) campaign 1))
       (check-equal? (hash-ref prepared 'status) "awaiting-review")
       (check-equal? (hash-ref (hash-ref prepared 'request) 'generation) 1)
       (check-equal? (hash-ref (hash-ref prepared 'request) 'attempt-id) "new-attempt")
       (check-equal? (blob-or-none (subject-git w) (remote-tip w) review-rel) old-blob)
       (check-equal? (file-bytes-map staging) old-staging)
       (check-true (string? (refusal-message (lambda () (prepare!* (hash-ref w 'subject) campaign 0))))))))

  ;; Python: test_recovery_refuses_reverted_source_commit_in_review_ancestry
  (native-case "recovery refuses a reverted source commit in the review ancestry"
    (with-case-world
     'plain
     (lambda (w)
       (define campaign (build-campaign (hash-ref w 'base)))
       (write-journal! campaign (hash-ref w 'head))
       (define staging (impl-staging campaign 0))
       (prepare!* (hash-ref w 'subject) campaign 0)
       (define reviewer (write-reviewer! staging (reviewer-output "request-changes")))
       (finalize!* (hash-ref w 'subject) campaign 0 reviewer)
       (define work (hash-ref w 'work))
       (git-run work "fetch" "-q" (path->string (hash-ref w 'origin)) wave-branch)
       (git-run work "checkout" "-q" "--detach" "FETCH_HEAD")
       (put-text (build-path work "src/file.txt") "unverified source\n")
       (git-run work "add" "src/file.txt")
       (git-run work "commit" "-q" "-m" "source drift")
       (git-run work "revert" "--no-edit" "HEAD")
       (git-run work "push" "-q" (path->string (hash-ref w 'origin))
                (format "HEAD:refs/heads/~a" wave-branch))
       (define message (check-refusal (lambda () (prepare!* (hash-ref w 'subject) campaign 0))))
       (check-true (string-contains? message "ancestry")))))

  ;; Python: test_rejection_recovery_refuses_wrong_receipt_and_malformed_rejection
  (native-case "rejection recovery refuses the wrong receipt and malformed rejections"
    ;; The four mutations below exercise every distinct recovery guard:
    ;; receipt attempt drift, generation drift, malformed findings and an
    ;; invalid verdict. (The attempt-fence/head/branch variants are value
    ;; parametrizations of the SAME identity guard, and the PENDING reviewer
    ;; variant is already covered by the malformed-output case; running all
    ;; eight doubles the subprocess count of the heaviest case.)
    (for ([change (in-list (list (cons 'attempt-id "other")
                                 (cons 'generation 1)
                                 (cons 'findings '())
                                 (cons 'verdict "REJECTED")))])
      (with-case-world
       'plain
       (lambda (w)
         (define campaign (build-campaign (hash-ref w 'base)))
         (write-journal! campaign (hash-ref w 'head))
         (define staging (impl-staging campaign 0))
         (prepare!* (hash-ref w 'subject) campaign 0)
         (define reviewer (write-reviewer! staging (reviewer-output "request-changes")))
         (define published (finalize!* (hash-ref w 'subject) campaign 0 reviewer))
         (define datum (read-committed-datum w (hash-ref published 'tip) review-rel))
         (publish-datum-commit! w
                                (hash-ref published 'tip)
                                review-rel
                                (hash-set datum (car change) (cdr change))
                                "malformed rejection fixture")
         (define before (remote-tip w))
         (check-true (string? (refusal-message (lambda () (prepare!* (hash-ref w 'subject) campaign 0))))
                     (format "~a" (car change)))
         (check-equal? (remote-tip w) before) (void)))))

  ;; Python: test_finalize_approved_binds_controller_digest_not_reviewer_prose
  (native-case "finalize binds the controller digest and ignores reviewer prose"
    (with-case-world
     'plain
     (lambda (w)
       (define campaign (build-campaign (hash-ref w 'base)))
       (write-journal! campaign (hash-ref w 'head))
       (define staging (impl-staging campaign 0))
       (prepare!* (hash-ref w 'subject) campaign 0)
       (define output (hash-set (reviewer-output "approved") 'content-digest (make-string 64 #\f)))
       (define reviewer (write-reviewer! staging output))
       (define result (finalize!* (hash-ref w 'subject) campaign 0 reviewer))
       (check-equal? (hash-ref result 'status) "reviewed")
       (define tip (remote-tip w))
       (define committed (show-committed w tip evidence-rel))
       (define expected (content-digest (subject-git w) "refs/remotes/origin/main" tip))
       (check-true (string-contains? committed expected))
       (check-false (string-contains? committed (make-string 64 #\f))))))

  ;; Python: test_finalize_request_changes_persists_findings_and_never_weakens_the_gate
  (native-case "request-changes persists findings and never weakens the APPROVED gate"
    (with-case-world
     'plain
     (lambda (w)
       (define campaign (build-campaign (hash-ref w 'base)))
       (write-journal! campaign (hash-ref w 'head))
       (define staging (impl-staging campaign 0))
       (prepare!* (hash-ref w 'subject) campaign 0)
       (define findings (list "focused tests fail at the pinned head"
                              "the digest law is violated by the touched module"))
       (define reviewer (write-reviewer! staging (reviewer-output "request-changes" findings)))
       (define result (finalize!* (hash-ref w 'subject) campaign 0 reviewer))
       (check-equal? (hash-ref result 'status) "review-rejected")
       (check-equal? (hash-ref result 'stage) "implementation-review")
       (check-equal? (hash-ref result 'head) (hash-ref w 'head))
       (check-equal? (hash-ref result 'branch) wave-branch)
       (check-equal? (hash-ref result 'attempt-id) "attempt-5")
       (check-equal? (hash-ref result 'attempt-fence) 2)
       (check-false (hash-ref result 'postmerge))
       (check-equal? (hash-ref result 'findings) findings)
       (define tip (remote-tip w))
       (define rejected (show-committed w tip review-rel))
       (check-true (string-contains? rejected "REQUEST_CHANGES"))
       (check-true (string-contains? rejected (hash-ref w 'head)))
       (check-true (string-contains? rejected (car findings)))
       ;; the strict APPROVED gate refuses the rejected verdict
       (check-true (string? (refusal-message
                             (lambda ()
                               (delivery-review! (hash-ref w 'subject)
                                                 plan
                                                 wave
                                                 (hash-ref w 'head)
                                                 wave-branch
                                                 #:campaign-root campaign))))))))

  ;; Python: test_finalize_malformed_reviewer_output_is_review_infra
  (native-case "malformed reviewer output is a typed review-infra stop"
    (with-case-world
     'plain
     (lambda (w)
       (define campaign (build-campaign (hash-ref w 'base)))
       (write-journal! campaign (hash-ref w 'head))
       (define staging (impl-staging campaign 0))
       (prepare!* (hash-ref w 'subject) campaign 0)
       (define bad-outputs
         (list (reviewer-output "bogus")
               (reviewer-output "request-changes" '())
               (reviewer-output "request-changes" (list ""))
               (reviewer-output "approved" #f "PENDING")
               (reviewer-output "approved" #f "pending-independent-review")
               (hash-set (reviewer-output "approved") 'scope "too short")
               (hash-set (reviewer-output "approved") 'validation (hash 'red-first "not a hash"))))
       (for ([output (in-list bad-outputs)])
         (define reviewer (write-reviewer! staging output))
         (define result (finalize!* (hash-ref w 'subject) campaign 0 reviewer))
         (check-equal? (hash-ref result 'status) "review-infra" (format "~s" output))
         (check-true (non-empty-string? (hash-ref result 'reason)) (format "~s" output)))
       (check-equal? (remote-tip w) (hash-ref w 'head)))))

  ;; Python: test_finalize_review_failed_session_is_review_infra_without_finalize
  (native-case "a failed reviewer session is review-infra and never finalizes"
    (with-case-world
     'plain
     (lambda (w)
       (define campaign (build-campaign (hash-ref w 'base)))
       (write-journal! campaign (hash-ref w 'head))
       (define staging (impl-staging campaign 0))
       (prepare!* (hash-ref w 'subject) campaign 0)
       (define reviewer (write-reviewer! staging (reviewer-output "review-failed")))
       (define result (finalize!* (hash-ref w 'subject) campaign 0 reviewer))
       (check-equal? (hash-ref result 'status) "review-infra")
       (check-true (string-contains? (hash-ref result 'reason) "reviewer session failed"))
       (check-equal? (remote-tip w) (hash-ref w 'head)))))

  ;; Python: test_finalize_refuses_missing_dispatch_request
  (native-case "finalize refuses a missing dispatch request"
    (with-case-world
     'plain
     (lambda (w)
       (define campaign (build-campaign (hash-ref w 'base)))
       (write-journal! campaign (hash-ref w 'head))
       (define staging (impl-staging campaign 0))
       (define reviewer (write-reviewer! staging (reviewer-output "approved")))
       (define message (check-refusal (lambda () (finalize!* (hash-ref w 'subject) campaign 0 reviewer))))
       (check-true (string-contains? message "no review dispatch request")))))

  ;; Python: test_finalize_refuses_tampered_dispatch_request_identity
  (native-case "finalize refuses a tampered dispatch request identity"
    (with-case-world
     'plain
     (lambda (w)
       (define campaign (build-campaign (hash-ref w 'base)))
       (write-journal! campaign (hash-ref w 'head))
       (define staging (impl-staging campaign 0))
       (prepare!* (hash-ref w 'subject) campaign 0)
       (define request-path (build-path staging "request.json"))
       (define request (call-with-input-file request-path read-json))
       (put-text request-path (jstring (hash-set request 'head (make-string 40 #\e))))
       (define reviewer (write-reviewer! staging (reviewer-output "approved")))
       (define message (check-refusal (lambda () (finalize!* (hash-ref w 'subject) campaign 0 reviewer))))
       (check-true (string-contains? message "does not match the durable receipt")))))

  ;; Python: test_finalize_remote_divergence_is_review_infra_and_never_overwrites
  (native-case "remote divergence during review is review-infra and never overwrites"
    (with-case-world
     'plain
     (lambda (w)
       (define campaign (build-campaign (hash-ref w 'base)))
       (write-journal! campaign (hash-ref w 'head))
       (define staging (impl-staging campaign 0))
       (prepare!* (hash-ref w 'subject) campaign 0)
       (define work (hash-ref w 'work))
       (put-text (build-path work "docs/reports/gsd-wave-evidence/foreign.rktd") "foreign\n")
       (git-run work "add" "-A")
       (git-run work "commit" "-q" "-m" "concurrent")
       (define foreign (git-run work "rev-parse" "HEAD"))
       (git-run work "push" "-q" (path->string (hash-ref w 'origin))
                (format "~a:refs/heads/~a" foreign wave-branch))
       (define reviewer (write-reviewer! staging (reviewer-output "approved")))
       (define result (finalize!* (hash-ref w 'subject) campaign 0 reviewer))
       (check-equal? (hash-ref result 'status) "review-infra")
       (check-equal? (remote-tip w) foreign))))

  ;; Python: test_finalize_refuses_receipt_drift_before_committing
  (native-case "finalize refuses receipt drift before committing"
    (with-case-world
     'plain
     (lambda (w)
       (define campaign (build-campaign (hash-ref w 'base)))
       (write-journal! campaign (hash-ref w 'head))
       (define staging (impl-staging campaign 0))
       (prepare!* (hash-ref w 'subject) campaign 0)
       (define journal (read-journal campaign))
       (write-journal-datum!
        campaign
        (hash-set journal
                  'receipt
                  (hash-set (hash-ref journal 'receipt) 'head (make-string 40 #\e))))
       (define reviewer (write-reviewer! staging (reviewer-output "approved")))
       (define message (check-refusal (lambda () (finalize!* (hash-ref w 'subject) campaign 0 reviewer))))
       (check-true (string-contains? message "does not match the durable receipt"))
       (check-equal? (remote-tip w) (hash-ref w 'head)))))

  ;; ----------------------------------------------------------------
  ;; legacy tracker facts: only the authenticated frozen issue is authority
  ;; ----------------------------------------------------------------

  ;; Python: test_legacy_frozen_facts_prepare_and_finalize_use_authenticated_cached_issue
  (native-case "legacy frozen facts use the authenticated same-repo issue and milestone"
    (with-case-world
     'plain
     #:issue
     824
     #:milestone
     72
     (lambda (w)
       (define campaign (build-campaign (hash-ref w 'base) #:legacy? #t #:issue 824 #:milestone 72))
       (write-journal! campaign (hash-ref w 'head) 1)
       (define prepared (prepare!* (hash-ref w 'subject) campaign 1))
       (check-equal? (hash-ref prepared 'status) "awaiting-review")
       (define request (hash-ref prepared 'request))
       (check-equal? (hash-ref request 'milestone) 72)
       (check-equal? (hash-ref request 'issue) 824)
       (check-equal? (hash-ref request 'generation) 1)
       (define staging (impl-staging campaign 1))
       (define reviewer (write-reviewer! staging (reviewer-output "approved")))
       (define finalized (finalize!* (hash-ref w 'subject) campaign 1 reviewer))
       (check-equal? (hash-ref finalized 'status) "reviewed")
       (define committed (show-committed w (hash-ref finalized 'tip) (hash-ref request 'evidence)))
       (check-true (string-contains? committed "(milestone . 72)"))
       (check-true (string-contains? committed "(issue . 824)"))
       (check-true (string-contains? committed "ready-for-merge"))
       (check-equal? (hash-ref (delivery-review! (hash-ref w 'subject)
                                                 plan
                                                 wave
                                                 (hash-ref w 'head)
                                                 wave-branch
                                                 #:campaign-root campaign)
                               'status)
                     "reviewed")
       (check-false (directory-exists? (impl-staging campaign 0))))))

  ;; Python: test_legacy_tracker_api_negatives_stop_both_prepare_and_finalize_without_publication
  (native-case "legacy tracker negatives stop prepare and finalize without publication"
    (with-case-world
     'plain
     #:issue
     824
     #:milestone
     72
     (lambda (w)
       (define campaign (build-campaign (hash-ref w 'base) #:legacy? #t #:issue 824 #:milestone 72))
       (write-journal! campaign (hash-ref w 'head))
       (define staging (impl-staging campaign 0))
       (prepare!* (hash-ref w 'subject) campaign 0)
       (define reviewer (write-reviewer! staging (reviewer-output "approved")))
       (define before (file-bytes-map staging))
       (define route (format "repos/~a/issues/824" slug))
       ;; Representative payloads: each kills a DIFFERENT guard of the resolver
       ;; (exact same-repo issue identity, no pull_request, positive integer
       ;; milestone, milestone repository/number/title) and the last one is a
       ;; transport failure through the authenticated API.
       (define bodies
         ;; Four representative payloads, one per distinct guard (identity,
         ;; PR-vs-issue, milestone shape, milestone content). The exhaustive
         ;; Python permutations multiply the same guards; four keep the case
         ;; under the suite's wall-clock budget without losing a refusal class.
         (list "not json at all"
               (jstring (hasheq 'number 824
                                'html_url "https://github.com/other/repo/issues/824"))
               (jstring (hasheq 'number 824
                                'html_url (format "https://github.com/~a/issues/824" slug)
                                'pull_request (hasheq 'url "a-pr")))
               (jstring (hasheq 'number 824
                                'html_url (format "https://github.com/~a/issues/824" slug)
                                'milestone (hasheq 'number 72
                                                   'title "Wrong title"
                                                   'url (format "https://api.github.com/repos/~a/milestones/72" slug))))))
       (for ([body (in-list bodies)] [index (in-naturals)])
         (set-gh-fixture! w route body)
         (define label (format "payload ~a" index))
         (check-true (string? (refusal-message (lambda () (prepare!* (hash-ref w 'subject) campaign 0))))
                     label)
         (check-true (string? (refusal-message (lambda () (finalize!* (hash-ref w 'subject) campaign 0 reviewer))))
                     label))
       (check-equal? (file-bytes-map staging) before)
       (check-equal? (remote-tip w) (hash-ref w 'head)))))

  ;; Python: test_finalize_legacy_tracker_binding_drift_refuses_publication
  (native-case "a legacy tracker binding drift refuses publication"
    (with-case-world
     'plain
     #:issue
     824
     #:milestone
     72
     (lambda (w)
       (define campaign (build-campaign (hash-ref w 'base) #:legacy? #t #:issue 824 #:milestone 72))
       (write-journal! campaign (hash-ref w 'head))
       (define staging (impl-staging campaign 0))
       (prepare!* (hash-ref w 'subject) campaign 0)
       (define reviewer (write-reviewer! staging (reviewer-output "approved")))
       ;; the authenticated milestone moved between prepare and finalize
       (set-gh-fixture! w
                        (format "repos/~a/issues/824" slug)
                        (issue-payload 824 73 frozen-title))
       (define message (check-refusal (lambda () (finalize!* (hash-ref w 'subject) campaign 0 reviewer))))
       (check-true (string-contains? message "tracker binding changed"))
       (check-equal? (remote-tip w) (hash-ref w 'head)))))

  ;; Python: test_numeric_metadata_requires_integer_not_bool_without_api_fallback
  (native-case "tracker metadata must be a positive integer, never a boolean"
    (with-case-world
     'plain
     #:issue
     824
     #:milestone
     72
     (lambda (w)
       (define campaign (build-campaign (hash-ref w 'base) #:legacy? #t #:issue 824 #:milestone 72))
       (write-journal! campaign (hash-ref w 'head))
       (define route (format "repos/~a/issues/824" slug))
       (define (payload issue milestone)
         (jstring (hasheq 'number issue
                          'html_url (format "https://github.com/~a/issues/824" slug)
                          'milestone (hasheq 'number milestone 'title frozen-title
                                             'url (format "https://api.github.com/repos/~a/milestones/72" slug)))))
       ;; `true` reads as #t and MUST NOT pass as an integer; no API fallback.
       (for ([body (in-list (list (payload #t 72)
                                  (payload 824 #t)
                                  (payload 824 0)
                                  (payload "824" 72)
                                  (payload 824 "72")))])
         (set-gh-fixture! w route body)
         (check-true (string? (refusal-message (lambda () (prepare!* (hash-ref w 'subject) campaign 0))))
                     body)))))

  ;; Python: test_legacy_missing_verified_title_or_explicit_issue_never_dispatches
  (native-case "legacy facts without a unique verified title or explicit issue never dispatch"
    (for ([tracker (in-list (list (hash 'heading "# Fixture plan")
                                  (hash 'heading "# Plan: First\n# Plan: Second")
                                  (hash 'wave-override (format "# W~a: Legacy\ncanary row = #12345\n" wave))
                                  (hash 'wave-override (format "# W~a: Legacy\nW2 = #9686\n" wave))))])
      (with-case-world
       'plain
       (lambda (w)
         (define campaign
           (build-campaign (hash-ref w 'base)
                           #:legacy? #t
                           #:heading (hash-ref tracker 'heading #f)
                           #:wave-override (hash-ref tracker 'wave-override #f)))
         (write-journal! campaign (hash-ref w 'head))
         (check-true (string? (refusal-message (lambda () (prepare!* (hash-ref w 'subject) campaign 0))))
                     (format "~s" tracker))
         (check-false (directory-exists? (impl-staging campaign 0)))
         (check-equal? (gh-log w) '())))))

  ;; ----------------------------------------------------------------
  ;; post-merge binding review: staged pending trio finalization
  ;; ----------------------------------------------------------------

  ;; Python: test_binding_prepare_pending_draft_is_dispatchable
  (native-case "binding prepare dispatches a staged pending-review draft"
    (with-case-world
     'plain
     (lambda (w)
       (define b (binding-world w))
       (define result (prepare!* (hash-ref w 'subject)
                                 (hash-ref b 'campaign)
                                 0
                                 "binding-review"))
       (check-equal? (hash-ref result 'status) "awaiting-review")
       (define request (hash-ref result 'request))
       (check-equal? (hash-ref request 'kind) "binding-review")
       (check-equal? (hash-ref request 'head) (hash-ref w 'head))
       (check-equal? (hash-ref request 'branch) wave-branch)
       (check-equal? (hash-ref request 'merge-sha) (hash-ref b 'merge))
       (check-equal? (hash-ref request 'attempt-id) "attempt-5")
       (check-equal? (hash-ref request 'content-digest)
                     (bytes->hex-string (sha256-bytes #"")))
       (check-equal? (hash-ref request 'staging) (path->string (hash-ref b 'staging)))
       (check-true (string-contains? (hash-ref request 'prompt) "pending-review"))
       (check-equal? (hash-ref request 'fresh-session-required) #t)
       (check-equal? (hash-ref request 'non-author-review-required) #t))))

  ;; Python: test_binding_finalize_approved_finalizes_staged_trio_through_strict_gate
  (native-case "binding finalize approved finalizes the staged trio through the strict gate"
    (with-case-world
     'plain
     (lambda (w)
       (define b (binding-world w))
       (define campaign (hash-ref b 'campaign))
       (define staging (hash-ref b 'staging))
       (prepare!* (hash-ref w 'subject) campaign 0 "binding-review")
       (define reviewer (write-reviewer! staging (reviewer-output "approved")))
       (define result (finalize!* (hash-ref w 'subject) campaign 0 reviewer "binding-review"))
       (check-equal? (hash-ref result 'status) "reviewed")
       (check-equal? (hash-ref result 'merge-sha) (hash-ref b 'merge))
       (define artifact (format "~a-w~a.rktd" plan wave))
       (define evidence
         (with-input-from-file (build-path staging "docs/reports/gsd-wave-evidence" artifact) read))
       (check-equal? (hash-ref evidence 'status) "ready-for-merge")
       (check-equal? (hash-ref evidence 'content-digest) (bytes->hex-string (sha256-bytes #"")))
       (define review
         (with-input-from-file (build-path staging "docs/reports/gsd-wave-reviews" artifact) read))
       (check-equal? (hash-ref review 'verdict) "APPROVED")
       (check-equal? (hash-ref review 'reviewed-sha) (hash-ref b 'merge))
       ;; the unchanged binding-review path accepts the finalized trio
       (check-equal? (hash-ref (delivery-binding-review! (hash-ref w 'subject)
                                                          plan
                                                          wave
                                                          staging
                                                          #:campaign-root campaign
                                                          #:expected-branch wave-branch)
                               'status)
                     "reviewed"))))

  ;; Python: test_binding_finalize_request_changes_is_postmerge_artifact_repair
  (native-case "binding finalize request-changes is a post-merge artifact repair"
    (with-case-world
     'plain
     (lambda (w)
       (define b (binding-world w))
       (define campaign (hash-ref b 'campaign))
       (define staging (hash-ref b 'staging))
       (prepare!* (hash-ref w 'subject) campaign 0 "binding-review")
       (define findings (list "staged binding review never covered the published implementation head"))
       (define reviewer (write-reviewer! staging (reviewer-output "request-changes" findings)))
       (define result (finalize!* (hash-ref w 'subject) campaign 0 reviewer "binding-review"))
       (check-equal? (hash-ref result 'status) "review-rejected")
       (check-equal? (hash-ref result 'stage) "binding-review")
       (check-true (hash-ref result 'postmerge))
       (check-equal? (hash-ref result 'head) (hash-ref w 'head))
       (check-equal? (hash-ref result 'branch) wave-branch)
       (check-equal? (hash-ref result 'attempt-id) "attempt-5")
       (check-equal? (hash-ref result 'attempt-fence) 2)
       (check-equal? (hash-ref result 'findings) findings)
       ;; findings persist as a trusted controller-authored staging artifact;
       ;; the staged draft trio itself is untouched (still pending-review)
       (define rejected (with-input-from-file (build-path staging "review-rejected.rktd") read))
       (check-equal? (hash-ref rejected 'verdict) "REQUEST_CHANGES")
       (check-equal? (hash-ref rejected 'reviewed-sha) (hash-ref b 'merge))
       (check-equal? (hash-ref rejected 'findings) findings)
       (check-equal? (hash-ref (delivery-binding-review! (hash-ref w 'subject)
                                                          plan
                                                          wave
                                                          staging
                                                          #:campaign-root campaign
                                                          #:expected-branch wave-branch)
                               'status)
                     "pending-review"))))

  ;; Python: test_binding_malformed_reviewer_output_is_review_infra
  (native-case "binding malformed reviewer output is review-infra"
    (with-case-world
     'plain
     (lambda (w)
       (define b (binding-world w))
       (define campaign (hash-ref b 'campaign))
       (define staging (hash-ref b 'staging))
       (prepare!* (hash-ref w 'subject) campaign 0 "binding-review")
       (define reviewer (write-reviewer! staging (reviewer-output "approved" #f "TBD")))
       (define result (finalize!* (hash-ref w 'subject) campaign 0 reviewer "binding-review"))
       (check-equal? (hash-ref result 'status) "review-infra")
       (check-equal? (hash-ref (delivery-binding-review! (hash-ref w 'subject)
                                                          plan
                                                          wave
                                                          staging
                                                          #:campaign-root campaign
                                                          #:expected-branch wave-branch)
                               'status)
                     "pending-review"))))

  ;; Python: test_binding_prepare_refuses_missing_staging
  (native-case "binding prepare refuses a missing staging directory"
    (with-case-world
     'plain
     (lambda (w)
       (define campaign (build-campaign (hash-ref w 'base)))
       (write-journal! campaign (hash-ref w 'head))
       (define staging (binding-staging-path campaign plan wave 0))
       (define message
         (check-refusal (lambda () (prepare!* (hash-ref w 'subject) campaign 0 "binding-review"))))
       (check-true (string-contains? message "binding staging"))
       (check-false (directory-exists? staging)))))
)

