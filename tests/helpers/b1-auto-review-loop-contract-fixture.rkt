#lang racket/base
;; B1-owned real-boundary fixture for
;; tests/test-gsd-auto-review-loop-contract.rkt (milestone #899 wave B1).
;;
;; This helper is DELIBERATELY self-contained: shared helpers
;; (tests/helpers/owned-audit-fixture.rkt, auto-review-owned-fixture.rkt, …)
;; are owned by other waves, so this module shares no mutable surface with
;; them. Everything it builds is REAL:
;;
;;   * real Git repositories (bare origin + subject clone with the logical
;;     GitHub identity re-pointed at the local bare through `insteadOf`, the
;;     same discipline as the native auto-review fixture);
;;   * a real frozen plan snapshot through the production
;;     `make-plan-snapshot!` API (the same immutable snapshot the owned
;;     worlds freeze);
;;   * the REAL native review controller invoked through
;;     `default-review-python` -> `auto-review-seam-run!` (the production
;;     dispatch seam, in the controller's documented manual-compatibility
;;     mode for the rejection/refusal wiring cases — the two-full-Verify-chain
;;     end-to-end path stays owned by tests/test-gsd-auto-review-loop.rkt);
;;   * the REAL production journal writer `record-delivery-receipt!` and the
;;     REAL rejection checkpoint `resolve-review-checkpoint!`.
;;
;; Honesty boundaries (what is simulated, and why it is not a security
;; claim):
;;   * `gh` is a PATH shim serving one canned issue payload. The tracker read
;;     is a NETWORK boundary (the GitHub REST read gh performs); the shim is
;;     the same technique the native controller regression suite uses. No
;;     security decision is made by gh: the controller's strict gates run for
;;     real either way.
;;   * the seeded receipt-history entry is the same documented
;;     "simulated prior generation" journal fixture the production suite
;;     itself uses; the CURRENT receipt is written by the real production
;;     journal writer, never by hand.
;;
;; The in-process require of scripts/gsd-binding-data.rkt is a warm-up, not a
;; dependency: the controller shells that module as a subprocess under a 45 s
;; per-command cap, and compiling it here (into the shared compiled/ cache)
;; keeps every controller subprocess inside the cap. It exports nothing this
;; helper uses.
(require racket/file
         racket/format
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         json
         file/sha1
         openssl
         "../../sandbox/subprocess.rkt"
         (only-in "../../extensions/gsd/plan-snapshot.rkt" make-plan-snapshot!)
         (only-in "../../extensions/gsd/review-dispatch.rkt" default-review-python)
         (only-in "../../scripts/gsd-delivery-auto-review.rkt" auto-review-seam-run!)
         (only-in "../../scripts/gsd-binding-data.rkt"))

(provide b1-slug
         b1-wave-branch
         b1-fixture-label
         b1-issue
         b1-milestone
         b1-plan-id
         b1-put-text!
         b1-git!
         b1-git-raw!
         b1-copy-review-world!
         b1-cleanup-world!
         b1-generation-rel
         b1-campaign-root
         b1-repo-link!
         b1-write-receipt!
         b1-journal-path
         b1-evidence-rel
         b1-review-rel
         b1-install-gh-shim!
         b1-controller-environment
         b1-manual-controller-seam
         b1-owned-controller-seam
         b1-gate-red-findings
         b1-approved-validation
         b1-rejected-runner-result
         b1-remote-tip
         b1-red-probe-branch
         build-red-probe-world!
         b1-probe-guard
         b1-warm-review-template!)

;; ============================================================
;; Constants (the same frozen fixture identity the native suite pins)
;; ============================================================

(define b1-slug "owner/b1repo")
(define b1-wave-branch "campaign/v9.9.9-b1")
(define b1-fixture-label "v9.9.9-b1")
(define b1-issue 9686)
(define b1-milestone 895)
(define policy-names '("lint" "test (0)" "workflows (0)"))
(define b1-evidence-rel (string-append "docs/reports/gsd-wave-evidence/" b1-fixture-label ".rktd"))
(define b1-review-rel (string-append "docs/reports/gsd-wave-reviews/" b1-fixture-label ".rktd"))
(define b1-validation-rel
  (string-append "docs/reports/gsd-wave-validation/" b1-fixture-label ".rktd"))
(define git-executable (find-executable-path "git"))

;; A fresh 64-hex campaign identity per world (the same discipline as the
;; owned fixture's fresh-plan-id: two worlds never share a plan identity).
(define (b1-plan-id seed)
  (bytes->hex-string (sha256-bytes (string->bytes/utf-8 (format "b1-contract-~a-~a"
                                                                seed
                                                                (current-inexact-milliseconds))))))

;; ============================================================
;; Primitives
;; ============================================================

(define (b1-put-text! path text)
  (make-directory* (path-only path))
  (call-with-output-file path #:exists 'truncate (lambda (out) (display text out))))

(define (subprocess-failed? result)
  (or (subprocess-result-timed-out? result) (not (zero? (subprocess-result-exit-code result)))))

(define (b1-git* dir args)
  (define argv
    (append (if dir
                (list "-C" (path->string dir))
                '())
            args))
  (define result (run-subprocess (path->string git-executable) #:args argv #:timeout 60))
  (when (subprocess-failed? result)
    (error 'b1-fixture "git ~s failed: ~a" argv (subprocess-result-stderr result)))
  result)

;; Trimmed stdout for identities; RAW stdout when exact bytes matter.
(define (b1-git! dir . args)
  (string-trim (subprocess-result-stdout (b1-git* dir args))))

(define (b1-git-raw! dir . args)
  (subprocess-result-stdout (b1-git* dir args)))

(define (b1-jstring value)
  (let ([out (open-output-string)])
    (write-json value out)
    (get-output-string out)))

;; ============================================================
;; The review world template (cached; copied per case)
;; ============================================================
;;
;; Shape (mirrors the deleted Python fixture both the native and production
;; suites port): a bare origin, a work clone used only to build history, and
;; a subject clone whose `origin` is the logical GitHub URL rewritten to the
;; local bare. `campaign/` is NOT part of the template: each case builds its
;; own campaign root + journal so the receipt identity is bound per case.

(define template-cache (make-hash))
(define template-root (make-temporary-file "q-b1-contract-templates-~a" 'directory))

(define (build-campaign-files! root plan wave-index)
  (define planning (build-path root ".planning"))
  (define plan-text
    (format "# Fixture plan\n\nSingle wave: waves/W~a-fixture.md owns the delivery.\n" wave-index))
  (define wave-text
    (format (string-append "# W~a: Fixture wave\n\nBranch: `~a`; GitHub issue "
                           "[#~a](https://github.com/~a/issues/~a), milestone #~a.\n\n"
                           "## Files\n\n"
                           "- File: `docs/reports/gsd-wave-evidence/~a.rktd`\n"
                           "- File: `docs/reports/gsd-wave-reviews/~a.rktd`\n"
                           "- File: `docs/reports/gsd-wave-validation/~a.rktd`\n")
            wave-index
            b1-wave-branch
            b1-issue
            b1-slug
            b1-issue
            b1-milestone
            b1-fixture-label
            b1-fixture-label
            b1-fixture-label))
  (b1-put-text! (build-path planning "PLAN.md") plan-text)
  (b1-put-text! (build-path planning (format "waves/W~a-fixture.md" wave-index)) wave-text)
  (make-plan-snapshot! root plan plan-text)
  root)

(define (build-review-template! kind)
  (define base (build-path template-root (format "~a" kind)))
  (make-directory* base)
  (define origin (build-path base "origin.git"))
  (define work (build-path base "work"))
  (b1-git! #f "init" "--bare" "-b" "main" (path->string origin))
  (b1-git! origin "config" "gc.auto" "0")
  (b1-git! #f "init" "-b" "main" (path->string work))
  (b1-git! work "config" "user.email" "fixture@example.com")
  (b1-git! work "config" "user.name" "fixture")
  (b1-git! work "config" "gc.auto" "0")
  (b1-git! work "config" "maintenance.auto" "false")
  (b1-put-text! (build-path work "scripts/required-pr-checks.policy")
                (format "(\"~a\")" (string-join policy-names "\" \"")))
  (b1-put-text! (build-path work "src/file.txt") "one\n")
  (b1-git! work "add" "-A")
  (b1-git! work "commit" "-q" "-m" "c0")
  (define c0 (b1-git! work "rev-parse" "HEAD"))
  (b1-git! work "checkout" "-q" "-b" "impl")
  (b1-put-text! (build-path work "src/file.txt") "two\n")
  (b1-git! work "add" "-A")
  (b1-git! work "commit" "-q" "-m" "h1")
  (define head (b1-git! work "rev-parse" "HEAD"))
  (b1-git! work "push" "-q" (path->string origin) "main:main")
  (b1-git! work "push" "-q" (path->string origin) (format "impl:refs/heads/~a" b1-wave-branch))
  (define subject (build-path base "repo"))
  (b1-git! #f "clone" "-q" "--branch" "main" (path->string origin) (path->string subject))
  (b1-git! subject "remote" "set-url" "origin" (format "https://github.com/~a.git" b1-slug))
  (b1-git! subject
           "config"
           (format "url.~a.insteadOf" (path->string origin))
           (format "https://github.com/~a.git" b1-slug))
  (b1-git! subject "config" "user.email" "fixture@example.com")
  (b1-git! subject "config" "user.name" "fixture")
  (b1-git! subject "config" "gc.auto" "0")
  (b1-git! subject "config" "maintenance.auto" "false")
  (hash 'base base 'origin origin 'work work 'subject subject 'c0 c0 'head head))

;; Copy the cached template into a fresh case directory and re-point the
;; insteadOf rewrite at the COPY's bare origin (the template path must never
;; be touched by a case). Returns the case world hash.
(define (copy-directory-tree src dest)
  (make-directory* dest)
  (for ([entry (in-list (directory-list src))])
    (define from (build-path src entry))
    (define to (build-path dest entry))
    (cond
      [(directory-exists? from) (copy-directory-tree from to)]
      [else (copy-file from to)]))
  dest)

;; One immutable template serves every case: each case gets its own copied
;; Git world, campaign root, plan identity and journal, so sharing the template
;; changes no claim while avoiding three identical Git-history builds.
(define (b1-warm-review-template!)
  (hash-ref! template-cache 'review (lambda () (build-review-template! 'review)))
  (void))

(define (b1-copy-review-world! kind)
  (define src (hash-ref! template-cache 'review (lambda () (build-review-template! 'review))))
  (define case-base (make-temporary-file (format "q-b1-contract-~a-~~a" kind) 'directory))
  (define copy (build-path case-base "world"))
  (copy-directory-tree (hash-ref src 'base) copy)
  (define origin (build-path copy "origin.git"))
  (define subject (build-path copy "repo"))
  (b1-git! subject
           "config"
           "--unset-all"
           (format "url.~a.insteadOf" (path->string (hash-ref src 'origin))))
  (b1-git! subject
           "config"
           (format "url.~a.insteadOf" (path->string origin))
           (format "https://github.com/~a.git" b1-slug))
  ;; The reviewer opens an EXISTING clean checkout on the exact receipt
  ;; branch (resolve-review-checkout), never a freshly cloned shortcut.
  ;; Register it per case so the copied Git world never points at the
  ;; immutable template's worktree metadata.
  (define review-worktree (build-path case-base "review-worktree"))
  (b1-git! subject
           "worktree"
           "add"
           "--track"
           "-b"
           b1-wave-branch
           (path->string review-worktree)
           (format "origin/~a" b1-wave-branch))
  (hash 'base
        case-base
        'origin
        origin
        'subject
        subject
        'c0
        (hash-ref src 'c0)
        'head
        (hash-ref src 'head)
        'review-worktree
        review-worktree))

(define (b1-cleanup-world! w)
  (delete-directory/files (hash-ref w 'base) #:must-exist? #f))

;; The declared artifact path at a binding generation: the controller
;; derives repair-generation output names by inserting -r<N> before .rktd
;; (scripts/gsd-delivery-pr.rkt repair-generation-path); the Git-boundary
;; canaries read exactly the derived path the controller published.
(define (b1-generation-rel relative generation)
  (if (= generation 0)
      relative
      (string-append (substring relative 0 (- (string-length relative) 5))
                     (format "-r~a.rktd" generation))))

;; The campaign root of a case + its `q` repository link (the layout-aware
;; coordinator root: base-dir has no .git, so the controller resolves the
;; repo through the same `<base>/q` link the production world uses).
(define (b1-campaign-root w plan wave-index)
  (define root (build-path (hash-ref w 'base) "campaign"))
  (build-campaign-files! root plan wave-index)
  root)

(define (b1-repo-link! w root)
  (make-file-or-directory-link (hash-ref w 'subject) (build-path root "q"))
  root)

;; ============================================================
;; The durable receipt (real production journal writer)
;; ============================================================

(define (b1-journal-path root plan wave-index)
  (build-path root ".planning" "campaigns" plan (format "coordinator-w~a.json" wave-index)))

(define (b1-write-receipt! record-receipt!
                           root
                           plan
                           wave-index
                           w
                           #:attempt-id [attempt-id "attempt-5"]
                           #:fence [fence 2]
                           #:history [history '()])
  ;; The CURRENT receipt through the real production writer (schema-checked;
  ;; tree/head are the subject clone's actual committed identity).
  (define subject (hash-ref w 'subject))
  (define head (hash-ref w 'head))
  (define receipt
    (hasheq 'repo
            (path->string subject)
            'origin
            (format "https://github.com/~a.git" b1-slug)
            'head
            head
            'tree
            (b1-git! subject "rev-parse" "HEAD^{tree}")
            'verified-at
            (current-seconds)
            'evidence
            b1-evidence-rel
            'branch
            b1-wave-branch
            'attempt-id
            attempt-id
            'attempt-fence
            fence))
  (record-receipt! root plan wave-index receipt)
  (unless (null? history)
    ;; The documented "simulated prior generation" seed (same journal shape
    ;; the production fixture writes): valid receipts only — the journal
    ;; loader refuses malformed history, so this seed is load-bearing.
    (define path (b1-journal-path root plan wave-index))
    (define journal (call-with-input-file path read-json))
    (b1-put-text! path (b1-jstring (hash-set journal 'receipt-history history))))
  receipt)

;; ============================================================
;; gh shim + controller environment (the network boundary fixture)
;; ============================================================

(define (issue-payload)
  (b1-jstring
   (hasheq 'number
           b1-issue
           'html_url
           (format "https://github.com/~a/issues/~a" b1-slug b1-issue)
           'milestone
           (hasheq 'number
                   b1-milestone
                   'title
                   "v9.9.9 — Fixture milestone"
                   'url
                   (format "https://api.github.com/repos/~a/milestones/~a" b1-slug b1-milestone)))))

(define (b1-install-gh-shim! w)
  (define bin (build-path (hash-ref w 'base) "bin"))
  (make-directory* bin)
  (define fixtures-dir (build-path (hash-ref w 'base) "gh-fixtures"))
  (make-directory* fixtures-dir)
  (define route (format "repos/~a/issues/~a" b1-slug b1-issue))
  (b1-put-text! (build-path fixtures-dir "issue.json") (issue-payload))
  (define shim (build-path bin "gh"))
  (b1-put-text!
   shim
   (string-join (list "#!/bin/sh"
                      "route="
                      "for a in \"$@\"; do route=\"$a\"; done"
                      "printf '%s\\n' \"$*\" >> \"$GH_SHIM_LOG\""
                      "case \"$route\" in"
                      (format "  ~a) /bin/cat \"$GH_SHIM_FIXTURES/issue.json\" ;;" route)
                      "  *) printf 'no fixture for route %s\\n' \"$route\" >&2; exit 66 ;;"
                      "esac")
                "\n"))
  (file-or-directory-permissions shim #o700)
  bin)

(define (b1-controller-environment w bin)
  ;; The controller's subprocesses cross `controller-environment` =
  ;; `sanitize-env` (secrets scrubbed, PATH preserved), so a parameterized
  ;; process environment with the shim dir prepended is the ONLY fixture
  ;; hook — no production seam is overridden.
  (define env (make-environment-variables))
  (define incoming (current-environment-variables))
  (for ([name (in-list (environment-variables-names incoming))])
    (unless (regexp-match? #rx"^(?:GH_TOKEN|GITHUB_TOKEN)$" name)
      (environment-variables-set! env name (environment-variables-ref incoming name))))
  (define current-path
    (or (let ([p (environment-variables-ref incoming #"PATH")]) (and p (bytes->string/utf-8 p)))
        "/usr/local/bin:/usr/bin:/bin"))
  (environment-variables-set! env
                              #"PATH"
                              (string->bytes/utf-8 (string-join (list (path->string bin) current-path)
                                                                ":")))
  (environment-variables-set! env #"HOME" (string->bytes/utf-8 (path->string (hash-ref w 'base))))
  (environment-variables-set! env #"GIT_CONFIG_NOSYSTEM" #"1")
  (environment-variables-set! env #"GIT_CONFIG_GLOBAL" #"/dev/null")
  (environment-variables-set! env #"GIT_TERMINAL_PROMPT" #"0")
  env)

;; ============================================================
;; Controller seams
;; ============================================================
;;
;; The REAL production dispatch seam (default-review-python ->
;; auto-review-seam-run!), with the controller's own documented
;; manual-compatibility flag for the wiring cases, and the UNMODIFIED
;; production seam (owned mode) for the prohibited-publication refusal case.

(define (b1-manual-controller-seam base-dir plan wave)
  ;; Mirrors default-review-python's exact repo resolution + flag append
  ;; (layout-aware: base-dir has no .git, so the controller repo is the
  ;; <base-dir>/q link) and adds the controller's OWN documented
  ;; manual-compatibility switch. The CLI parser is strict flag/value PAIRS
  ;; (a trailing bare flag would refuse the whole argv) and `flag-true?`
  ;; reads the value, so the switch is spelled as an explicit pair — the
  ;; same spelling the controller's own CLI accepts.
  (define repo
    (if (or (directory-exists? (build-path base-dir ".git"))
            (file-exists? (build-path base-dir ".git")))
        base-dir
        (build-path base-dir "q")))
  (lambda (args)
    (auto-review-seam-run! (append args
                                   (list "--repo"
                                         (path->string (path->complete-path repo))
                                         "--plan"
                                         plan
                                         "--wave"
                                         (number->string wave)
                                         "--manual-review-compatibility"
                                         "1")))))

(define (b1-owned-controller-seam base-dir plan wave)
  (default-review-python base-dir plan wave))

;; ============================================================
;; The red-probe world (one genuine Verify-command registry execution)
;; ============================================================
;;
;; A minimal REAL Git checkout (clean, committed, GitHub-identity origin,
;; non-main branch) + a real frozen plan snapshot whose Verify declaration
;; declares the three probe stages. The probes run through the PRODUCTION
;; owned-RED producer (`run-owned-red-verification`): the real singleton
;; verification registry, real setsid process ownership, real raw-log
;; retention — one focused stage of the frozen declaration, never the whole
;; Verify campaign.

(define b1-red-probe-branch "campaign/b1-contract-red")

(define b1-red-probe
  "#lang racket/base
(require rackunit)
(module+ test
  (check-equal? (add1 1) 3 \"b1 red probe: deliberate behavioral failure\"))
")

(define b1-green-probe
  "#lang racket/base
(require rackunit)
(module+ test
  (check-equal? (add1 1) 2 \"b1 green probe must pass\"))
")

(define b1-slow-probe
  "#lang racket/base
(module+ test
  (displayln \"b1 slow probe: sleeping past the verification deadline\")
  (sleep 60))
")

;; One frozen Verify declaration declaring all three probe stages (legacy
;; single-line shell contract; every probe stage is an accepted focused
;; whole-file runner spelling).
(define b1-red-probe-chain
  (string-append "cd <project-base>/q"
                 " && raco test tests/b1-red-probe.rkt"
                 " && raco test tests/b1-green-probe.rkt"
                 " && raco test tests/b1-slow-probe.rkt"))

(define (build-red-probe-world! base plan)
  (make-directory* base)
  (define repo (build-path base "repo"))
  (b1-git! #f "init" "-b" "main" (path->string repo))
  (b1-git! repo "config" "user.email" "fixture@example.com")
  (b1-git! repo "config" "user.name" "fixture")
  (b1-git! repo "config" "gc.auto" "0")
  (b1-put-text! (build-path repo ".gitignore") "compiled/\n")
  (b1-put-text! (build-path repo "src/b1-probe-widget.rkt")
                "#lang racket/base\n(provide b1-probe-sq)\n(define (b1-probe-sq n) (* n n))\n")
  (b1-put-text! (build-path repo "tests" "b1-red-probe.rkt") b1-red-probe)
  (b1-put-text! (build-path repo "tests" "b1-green-probe.rkt") b1-green-probe)
  (b1-put-text! (build-path repo "tests" "b1-slow-probe.rkt") b1-slow-probe)
  (b1-git! repo "add" "-A")
  (b1-git! repo "commit" "-q" "-m" "b1 probe world baseline")
  (b1-git! repo "branch" "-m" "main" b1-red-probe-branch)
  ;; The logical GitHub identity the owned binding authenticates (never
  ;; fetched: the probes execute locally, the URL is the binding fact).
  (b1-git! repo "remote" "add" "origin" (format "https://github.com/~a.git" b1-slug))
  (define head (b1-git! repo "rev-parse" "HEAD"))
  (define tree (b1-git! repo "rev-parse" "HEAD^{tree}"))
  ;; The campaign root: real PLAN + wave doc + immutable snapshot with the
  ;; frozen Verify declaration.
  (define root (build-path base "campaign"))
  (define planning (build-path root ".planning"))
  (define plan-text (format "# Probe plan\n\nSingle wave: waves/W0-fixture.md owns the delivery.\n"))
  (define wave-text
    (format (string-append "# W0: Probe wave\n\nBranch: `~a`; GitHub issue "
                           "[#~a](https://github.com/~a/issues/~a), milestone #~a.\n\n"
                           "## Verify\n~a\n")
            b1-red-probe-branch
            b1-issue
            b1-slug
            b1-issue
            b1-milestone
            b1-red-probe-chain))
  (b1-put-text! (build-path planning "PLAN.md") plan-text)
  (b1-put-text! (build-path planning "waves" "W0-fixture.md") wave-text)
  (make-plan-snapshot! root plan plan-text)
  ;; The trusted owned-proof store: OUTSIDE the execution checkout (the
  ;; producer refuses an execution-inside-store layout).
  (define proof-root (build-path base "owned-proofs"))
  (hash 'base
        base
        'repo
        repo
        'campaign
        root
        'plan
        plan
        'branch
        b1-red-probe-branch
        'head
        head
        'tree
        tree
        'proof-root
        proof-root
        'red-focused
        "raco test tests/b1-red-probe.rkt"
        'green-focused
        "raco test tests/b1-green-probe.rkt"
        'slow-focused
        "raco test tests/b1-slow-probe.rkt"))

;; One live zero-argument guard consulted by the producer (same law as the
;; owned fixture: the attempt identity/fence the caller pins is live).
(define (b1-probe-guard consulted)
  (lambda ()
    (set-box! consulted #t)
    #t))

;; ============================================================
;; Reviewer results (typed output the strict gates accept)
;; ============================================================

(define b1-gate-red-findings
  (list "  preserve exact whitespace  " "a=b; findings: exact\nsecond line"))

(define (b1-rejected-runner-result)
  (hasheq 'kind
          "request-changes"
          'reviewer
          "independent-contract-reviewer-session"
          'scope
          (make-string 70 #\s)
          'report
          (make-string 70 #\r)
          'findings
          b1-gate-red-findings))

(define (b1-approved-validation)
  (hasheq 'red-first
          (hasheq 'command
                  "raco test tests/b1-contract-probe.rkt"
                  'failure
                  "reproduction observed: the malformed digest passed the gate before the fix")
          'focused-tests
          (hasheq 'result "passed" 'command "raco test tests/b1-contract.rkt")
          'format-compile
          (hasheq 'result "passed" 'command "raco fmt --check src/file.txt")
          'lint
          (hasheq 'result "passed" 'command "raco make src/file.txt")
          'fast
          (hasheq 'result "passed" 'command "racket scripts/run-tests.rkt --suite fast")
          'remaining-items
          '()
          'planning-sync
          "current"))

(define (b1-remote-tip w)
  (b1-git! (hash-ref w 'origin) "rev-parse" (format "refs/heads/~a" b1-wave-branch)))
