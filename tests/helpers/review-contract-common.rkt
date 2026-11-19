#lang racket/base
;; Shared stateless construction library for the two auto-review contract
;; fixtures (milestone #900 wave W4a; consolidates the duplicated B1/B2
;; helper bodies flagged on #9952):
;;
;;   * tests/helpers/b1-auto-review-loop-contract-fixture.rkt
;;     (consumer: tests/test-gsd-auto-review-loop-contract.rkt), and
;;   * tests/helpers/b2-review-contract-fixture.rkt
;;     (consumer: tests/test-gsd-auto-review-production-contract.rkt).
;;
;; The two owners previously carried near-byte-identical copies of the
;; real Git/receipt/Verify/environment construction (the normalized diff
;; showed exactly ONE structural difference: B1's per-case review-worktree
;; registration). This module holds that ONE construction. It is a
;; parameterized library, NOT a fixture lifecycle framework: no setup/
;; teardown protocol, no dynamic scope, no discovery. Each owner binds a
;; frozen `review-fixture-config` in its own thin module and re-provides
;; its historical names, so every consumer keeps its exact public surface,
;; its own private Git worlds and its own unique cases.
;;
;; What stays deliberately per-owner (genuinely different, not shared):
;;   * the frozen identity (slug, wave branch, fixture label, issue,
;;     milestone) and the `tag` namespace (plan-id seeds, temp-dir
;;     prefixes, error tags, probe file names, reviewer probe commands);
;;   * B1's review-worktree registration — the reviewer opens an EXISTING
;;     clean checkout on the exact receipt branch (resolve-review-checkout);
;;     B2 declares no such step;
;;   * both owners' unique CASES and every consumer-side assertion.
;;
;; Everything built here is REAL (same construction as before the
;; consolidation):
;;   * real Git repositories (bare origin + subject clone with the logical
;;     GitHub identity re-pointed at the local bare through `insteadOf`, the
;;     same discipline as the native auto-review fixture);
;;   * a real frozen plan snapshot through the production
;;     `make-plan-snapshot!` API (the same immutable snapshot the owned
;;     worlds freeze);
;;   * the REAL production dispatch seam (`default-review-python` ->
;;     `auto-review-seam-run!`) and the REAL journal writer inputs — the
;;     current receipt is always written by the production writer, never by
;;     hand.
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
;; module uses.
(require racket/file
         racket/format
         racket/list
         racket/path
         racket/string
         json
         file/sha1
         openssl
         "../../sandbox/subprocess.rkt"
         (only-in "../../extensions/gsd/plan-snapshot.rkt" make-plan-snapshot!)
         (only-in "../../extensions/gsd/review-dispatch.rkt" default-review-python)
         (only-in "../../scripts/gsd-delivery-auto-review.rkt" auto-review-seam-run!)
         (only-in "../../scripts/gsd-binding-data.rkt"))

(provide (struct-out review-fixture-config)
         fixture-evidence-rel
         fixture-review-rel
         fixture-validation-rel
         fixture-red-probe-branch
         fixture-plan-id
         put-text!
         git!
         git-raw!
         generation-rel
         cleanup-world!
         warm-review-template!
         copy-review-world!
         campaign-root
         repo-link!
         journal-path
         write-receipt!
         install-gh-shim!
         controller-environment
         manual-controller-seam
         owned-controller-seam
         remote-tip
         build-red-probe-world!
         probe-guard
         gate-red-findings
         rejected-runner-result
         approved-validation)

;; ============================================================
;; The frozen fixture identity (the only meaningful per-owner differences)
;; ============================================================
;;
;; tag: short namespace ("b1" / "b2") naming plan-id seeds, temp-dir
;; prefixes, error tags, probe file names and reviewer probe commands.
;; with-review-worktree?: B1 #t (the reviewer opens the registered clean
;; receipt-branch checkout), B2 #f.

(struct review-fixture-config
        (tag slug wave-branch fixture-label issue milestone with-review-worktree?)
  #:transparent)

(define (fixture-evidence-rel cfg)
  (string-append "docs/reports/gsd-wave-evidence/" (review-fixture-config-fixture-label cfg) ".rktd"))

(define (fixture-review-rel cfg)
  (string-append "docs/reports/gsd-wave-reviews/" (review-fixture-config-fixture-label cfg) ".rktd"))

(define (fixture-validation-rel cfg)
  (string-append "docs/reports/gsd-wave-validation/"
                 (review-fixture-config-fixture-label cfg)
                 ".rktd"))

(define (fixture-red-probe-branch cfg)
  (format "campaign/~a-contract-red" (review-fixture-config-tag cfg)))

(define (fixture-error-tag cfg)
  (string->symbol (format "~a-fixture" (review-fixture-config-tag cfg))))

;; A fresh 64-hex campaign identity per world (the same discipline as the
;; owned fixture's fresh-plan-id: two worlds never share a plan identity).
(define (fixture-plan-id cfg seed)
  (bytes->hex-string (sha256-bytes (string->bytes/utf-8 (format "~a-contract-~a-~a"
                                                                (review-fixture-config-tag cfg)
                                                                seed
                                                                (current-inexact-milliseconds))))))

;; ============================================================
;; Primitives
;; ============================================================

(define (put-text! path text)
  (make-directory* (path-only path))
  (call-with-output-file path #:exists 'truncate (lambda (out) (display text out))))

(define git-executable (find-executable-path "git"))

(define (subprocess-failed? result)
  (or (subprocess-result-timed-out? result) (not (zero? (subprocess-result-exit-code result)))))

(define (git* cfg dir args)
  (define argv
    (append (if dir
                (list "-C" (path->string dir))
                '())
            args))
  (define result (run-subprocess (path->string git-executable) #:args argv #:timeout 60))
  (when (subprocess-failed? result)
    (error (fixture-error-tag cfg) "git ~s failed: ~a" argv (subprocess-result-stderr result)))
  result)

;; Trimmed stdout for identities; RAW stdout when exact bytes matter.
(define (git! cfg dir . args)
  (string-trim (subprocess-result-stdout (git* cfg dir args))))

(define (git-raw! cfg dir . args)
  (subprocess-result-stdout (git* cfg dir args)))

(define (jstring value)
  (let ([out (open-output-string)])
    (write-json value out)
    (get-output-string out)))

;; ============================================================
;; The review world template (cached per owner; copied per case)
;; ============================================================
;;
;; Shape (mirrors the deleted Python fixture both the native and production
;; suites port): a bare origin, a work clone used only to build history, and
;; a subject clone whose `origin` is the logical GitHub URL rewritten to the
;; local bare. `campaign/` is NOT part of the template: each case builds its
;; own campaign root + journal so the receipt identity is bound per case.
;; The cache is keyed by the owner's config struct, so B1 and B2 keep
;; separate private template repositories (different slug, branch, history)
;; exactly as when each owner module held its own cache.

(define template-cache (make-hash))
(define template-root (make-temporary-file "q-review-contract-templates-~a" 'directory))

(define (build-campaign-files! cfg root plan wave-index)
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
            (review-fixture-config-wave-branch cfg)
            (review-fixture-config-issue cfg)
            (review-fixture-config-slug cfg)
            (review-fixture-config-issue cfg)
            (review-fixture-config-milestone cfg)
            (review-fixture-config-fixture-label cfg)
            (review-fixture-config-fixture-label cfg)
            (review-fixture-config-fixture-label cfg)))
  (put-text! (build-path planning "PLAN.md") plan-text)
  (put-text! (build-path planning (format "waves/W~a-fixture.md" wave-index)) wave-text)
  (make-plan-snapshot! root plan plan-text)
  root)

(define (build-review-template! cfg)
  (define slug (review-fixture-config-slug cfg))
  (define wave-branch (review-fixture-config-wave-branch cfg))
  (define base (build-path template-root (review-fixture-config-tag cfg) "review"))
  (make-directory* base)
  (define origin (build-path base "origin.git"))
  (define work (build-path base "work"))
  (git! cfg #f "init" "--bare" "-b" "main" (path->string origin))
  (git! cfg origin "config" "gc.auto" "0")
  (git! cfg #f "init" "-b" "main" (path->string work))
  (git! cfg work "config" "user.email" "fixture@example.com")
  (git! cfg work "config" "user.name" "fixture")
  (git! cfg work "config" "gc.auto" "0")
  (git! cfg work "config" "maintenance.auto" "false")
  (put-text! (build-path work "scripts/required-pr-checks.policy")
             (format "(\"~a\")" (string-join '("lint" "test (0)" "workflows (0)") "\" \"")))
  (put-text! (build-path work "src/file.txt") "one\n")
  (git! cfg work "add" "-A")
  (git! cfg work "commit" "-q" "-m" "c0")
  (define c0 (git! cfg work "rev-parse" "HEAD"))
  (git! cfg work "checkout" "-q" "-b" "impl")
  (put-text! (build-path work "src/file.txt") "two\n")
  (git! cfg work "add" "-A")
  (git! cfg work "commit" "-q" "-m" "h1")
  (define head (git! cfg work "rev-parse" "HEAD"))
  (git! cfg work "push" "-q" (path->string origin) "main:main")
  (git! cfg work "push" "-q" (path->string origin) (format "impl:refs/heads/~a" wave-branch))
  (define subject (build-path base "repo"))
  (git! cfg #f "clone" "-q" "--branch" "main" (path->string origin) (path->string subject))
  (git! cfg subject "remote" "set-url" "origin" (format "https://github.com/~a.git" slug))
  (git! cfg
        subject
        "config"
        (format "url.~a.insteadOf" (path->string origin))
        (format "https://github.com/~a.git" slug))
  (git! cfg subject "config" "user.email" "fixture@example.com")
  (git! cfg subject "config" "user.name" "fixture")
  (git! cfg subject "config" "gc.auto" "0")
  (git! cfg subject "config" "maintenance.auto" "false")
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

;; One immutable template per owner serves every case: each case gets its own
;; copied Git world, campaign root, plan identity and journal, so sharing the
;; template changes no claim while avoiding three identical Git-history
;; builds.
(define (warm-review-template! cfg)
  (hash-ref! template-cache cfg (lambda () (build-review-template! cfg)))
  (void))

(define (copy-review-world! cfg kind)
  (define src (hash-ref! template-cache cfg (lambda () (build-review-template! cfg))))
  (define case-base
    (make-temporary-file (format "q-~a-contract-~a-~~a" (review-fixture-config-tag cfg) kind)
                         'directory))
  (define copy (build-path case-base "world"))
  (copy-directory-tree (hash-ref src 'base) copy)
  (define origin (build-path copy "origin.git"))
  (define subject (build-path copy "repo"))
  (git! cfg
        subject
        "config"
        "--unset-all"
        (format "url.~a.insteadOf" (path->string (hash-ref src 'origin))))
  (git! cfg
        subject
        "config"
        (format "url.~a.insteadOf" (path->string origin))
        (format "https://github.com/~a.git" (review-fixture-config-slug cfg)))
  ;; B1-only step: the reviewer opens an EXISTING clean checkout on the exact
  ;; receipt branch (resolve-review-checkout), never a freshly cloned
  ;; shortcut. Register it per case so the copied Git world never points at
  ;; the immutable template's worktree metadata. B2 declares no such step.
  (define review-worktree
    (if (review-fixture-config-with-review-worktree? cfg)
        (let ([rw (build-path case-base "review-worktree")])
          (git! cfg
                subject
                "worktree"
                "add"
                "--track"
                "-b"
                (review-fixture-config-wave-branch cfg)
                (path->string rw)
                (format "origin/~a" (review-fixture-config-wave-branch cfg)))
          rw)
        #f))
  (define world
    (hash 'base
          case-base
          'origin
          origin
          'subject
          subject
          'c0
          (hash-ref src 'c0)
          'head
          (hash-ref src 'head)))
  (if (review-fixture-config-with-review-worktree? cfg)
      (hash-set world 'review-worktree review-worktree)
      world))

(define (cleanup-world! w)
  (delete-directory/files (hash-ref w 'base) #:must-exist? #f))

;; The declared artifact path at a binding generation: the controller
;; derives repair-generation output names by inserting -r<N> before .rktd
;; (scripts/gsd-delivery-pr.rkt repair-generation-path); the Git-boundary
;; canaries read exactly the derived path the controller published.
(define (generation-rel relative generation)
  (if (= generation 0)
      relative
      (string-append (substring relative 0 (- (string-length relative) 5))
                     (format "-r~a.rktd" generation))))

;; The campaign root of a case + its `q` repository link (the layout-aware
;; coordinator root: base-dir has no .git, so the controller resolves the
;; repo through the same `<base>/q` link the production world uses).
(define (campaign-root cfg w plan wave-index)
  (define root (build-path (hash-ref w 'base) "campaign"))
  (build-campaign-files! cfg root plan wave-index)
  root)

(define (repo-link! w root)
  (make-file-or-directory-link (hash-ref w 'subject) (build-path root "q"))
  root)

;; ============================================================
;; The durable receipt (real production journal writer)
;; ============================================================

(define (journal-path root plan wave-index)
  (build-path root ".planning" "campaigns" plan (format "coordinator-w~a.json" wave-index)))

(define (write-receipt! cfg
                        record-receipt!
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
            (format "https://github.com/~a.git" (review-fixture-config-slug cfg))
            'head
            head
            'tree
            (git! cfg subject "rev-parse" "HEAD^{tree}")
            'verified-at
            (current-seconds)
            'evidence
            (fixture-evidence-rel cfg)
            'branch
            (review-fixture-config-wave-branch cfg)
            'attempt-id
            attempt-id
            'attempt-fence
            fence))
  (record-receipt! root plan wave-index receipt)
  (unless (null? history)
    ;; The documented "simulated prior generation" seed (same journal shape
    ;; the production fixture writes): valid receipts only — the journal
    ;; loader refuses malformed history, so this seed is load-bearing.
    (define path (journal-path root plan wave-index))
    (define journal (call-with-input-file path read-json))
    (put-text! path (jstring (hash-set journal 'receipt-history history))))
  receipt)

;; ============================================================
;; gh shim + controller environment (the network boundary fixture)
;; ============================================================

(define (issue-payload cfg)
  (jstring (hasheq 'number
                   (review-fixture-config-issue cfg)
                   'html_url
                   (format "https://github.com/~a/issues/~a"
                           (review-fixture-config-slug cfg)
                           (review-fixture-config-issue cfg))
                   'milestone
                   (hasheq 'number
                           (review-fixture-config-milestone cfg)
                           'title
                           "v9.9.9 — Fixture milestone"
                           'url
                           (format "https://api.github.com/repos/~a/milestones/~a"
                                   (review-fixture-config-slug cfg)
                                   (review-fixture-config-milestone cfg))))))

(define (install-gh-shim! cfg w)
  (define issue (review-fixture-config-issue cfg))
  (define bin (build-path (hash-ref w 'base) "bin"))
  (make-directory* bin)
  (define fixtures-dir (build-path (hash-ref w 'base) "gh-fixtures"))
  (make-directory* fixtures-dir)
  (define route (format "repos/~a/issues/~a" (review-fixture-config-slug cfg) issue))
  (put-text! (build-path fixtures-dir "issue.json") (issue-payload cfg))
  (define shim (build-path bin "gh"))
  (put-text! shim
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

(define (controller-environment w bin)
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

(define (manual-controller-seam base-dir plan wave)
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

(define (owned-controller-seam base-dir plan wave)
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
;; Verify campaign. Probe file names and the frozen declaration are
;; namespaced by the owner tag.

(define (red-probe-source cfg)
  (format
   "#lang racket/base\n(require rackunit)\n(module+ test\n  (check-equal? (add1 1) 3 \"~a red probe: deliberate behavioral failure\"))\n"
   (review-fixture-config-tag cfg)))

(define (green-probe-source cfg)
  (format
   "#lang racket/base\n(require rackunit)\n(module+ test\n  (check-equal? (add1 1) 2 \"~a green probe must pass\"))\n"
   (review-fixture-config-tag cfg)))

;; Class-F source literal (wait-audit): the sleep lives inside the generated
;; VERIFY PROBE PROGRAM source (string constant) used to prove a genuine
;; timeout and exit 124; the q test process never sleeps.
(define (slow-probe-source cfg)
  (format
   "#lang racket/base\n(module+ test\n  (displayln \"~a slow probe: sleeping past the verification deadline\")\n  (sleep 60))\n"
   (review-fixture-config-tag cfg)))

;; One frozen Verify declaration declaring all three probe stages (legacy
;; single-line shell contract; every probe stage is an accepted focused
;; whole-file runner spelling).
(define (red-probe-chain cfg)
  (string-append "cd <project-base>/q"
                 (format " && raco test tests/~a-red-probe.rkt" (review-fixture-config-tag cfg))
                 (format " && raco test tests/~a-green-probe.rkt" (review-fixture-config-tag cfg))
                 (format " && raco test tests/~a-slow-probe.rkt" (review-fixture-config-tag cfg))))

(define (build-red-probe-world! cfg base plan)
  (define tag (review-fixture-config-tag cfg))
  (define branch (fixture-red-probe-branch cfg))
  (make-directory* base)
  (define repo (build-path base "repo"))
  (git! cfg #f "init" "-b" "main" (path->string repo))
  (git! cfg repo "config" "user.email" "fixture@example.com")
  (git! cfg repo "config" "user.name" "fixture")
  (git! cfg repo "config" "gc.auto" "0")
  (put-text! (build-path repo ".gitignore") "compiled/\n")
  (put-text!
   (build-path repo "src" (format "~a-probe-widget.rkt" tag))
   (format "#lang racket/base\n(provide ~a-probe-sq)\n(define (~a-probe-sq n) (* n n))\n" tag tag))
  (put-text! (build-path repo "tests" (format "~a-red-probe.rkt" tag)) (red-probe-source cfg))
  (put-text! (build-path repo "tests" (format "~a-green-probe.rkt" tag)) (green-probe-source cfg))
  (put-text! (build-path repo "tests" (format "~a-slow-probe.rkt" tag)) (slow-probe-source cfg))
  (git! cfg repo "add" "-A")
  (git! cfg repo "commit" "-q" "-m" (format "~a probe world baseline" tag))
  (git! cfg repo "branch" "-m" "main" branch)
  ;; The logical GitHub identity the owned binding authenticates (never
  ;; fetched: the probes execute locally, the URL is the binding fact).
  (git! cfg
        repo
        "remote"
        "add"
        "origin"
        (format "https://github.com/~a.git" (review-fixture-config-slug cfg)))
  (define head (git! cfg repo "rev-parse" "HEAD"))
  (define tree (git! cfg repo "rev-parse" "HEAD^{tree}"))
  ;; The campaign root: real PLAN + wave doc + immutable snapshot with the
  ;; frozen Verify declaration.
  (define root (build-path base "campaign"))
  (define planning (build-path root ".planning"))
  (define plan-text (format "# Probe plan\n\nSingle wave: waves/W0-fixture.md owns the delivery.\n"))
  (define wave-text
    (format (string-append "# W0: Probe wave\n\nBranch: `~a`; GitHub issue "
                           "[#~a](https://github.com/~a/issues/~a), milestone #~a.\n\n"
                           "## Verify\n~a\n")
            branch
            (review-fixture-config-issue cfg)
            (review-fixture-config-slug cfg)
            (review-fixture-config-issue cfg)
            (review-fixture-config-milestone cfg)
            (red-probe-chain cfg)))
  (put-text! (build-path planning "PLAN.md") plan-text)
  (put-text! (build-path planning "waves" "W0-fixture.md") wave-text)
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
        branch
        'head
        head
        'tree
        tree
        'proof-root
        proof-root
        'red-focused
        (format "raco test tests/~a-red-probe.rkt" tag)
        'green-focused
        (format "raco test tests/~a-green-probe.rkt" tag)
        'slow-focused
        (format "raco test tests/~a-slow-probe.rkt" tag)))

;; One live zero-argument guard consulted by the producer (same law as the
;; owned fixture: the attempt identity/fence the caller pins is live).
(define (probe-guard consulted)
  (lambda ()
    (set-box! consulted #t)
    #t))

;; ============================================================
;; Reviewer results (typed output the strict gates accept)
;; ============================================================

(define gate-red-findings (list "  preserve exact whitespace  " "a=b; findings: exact\nsecond line"))

(define (rejected-runner-result)
  (hasheq 'kind
          "request-changes"
          'reviewer
          "independent-contract-reviewer-session"
          'scope
          (make-string 70 #\s)
          'report
          (make-string 70 #\r)
          'findings
          gate-red-findings))

(define (approved-validation cfg)
  (define tag (review-fixture-config-tag cfg))
  (hasheq 'red-first
          (hasheq 'command
                  (format "raco test tests/~a-contract-probe.rkt" tag)
                  'failure
                  "reproduction observed: the malformed digest passed the gate before the fix")
          'focused-tests
          (hasheq 'result "passed" 'command (format "raco test tests/~a-contract.rkt" tag))
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

(define (remote-tip cfg w)
  (git! cfg
        (hash-ref w 'origin)
        "rev-parse"
        (format "refs/heads/~a" (review-fixture-config-wave-branch cfg))))
