#lang racket/base

;; q/scripts/testing/full-plan.rkt — selection and execution for `./qt full`
;;
;; `./qt full` is the complete AVAILABLE Racket regression suite on this host:
;; the same `collect-test-files 'all` selection the repository's own discovery
;; produces, executed through the structured-evidence quick runner (fresh
;; Racket child per file, owned process group, bounded workers, RackUnit
;; counts written to a private evidence file).
;;
;; Honest labeling: a local Linux pass of this selection is NOT PR/CI
;; verification. `full-obligations` lists what a pass does not prove,
;; `full-not-provided` lists the counted obligations this command does not run
;; at all, and `--explain` prints both lists without executing or cleaning
;; anything.
;;
;; Lanes (one selection machinery, three selections):
;;   - `full`    — every discovered test file (unchanged behavior);
;;   - `default` — the required-offline PR lane (final plan §6 E1): the union
;;                 of the suites the required CI test jobs select, named by
;;                 the qt-lanes table's `required-offline` entry (no second
;;                 registry, no YAML parsing). A member whose declared
;;                 `@requires` this host cannot satisfy is `unavailable-here`:
;;                 it is LISTED, never verified, and never blocks the offline
;;                 verdict — CI stays authoritative for it;
;;   - `release` — `full` plus the release obligation list, printed whether or
;;                 not the suite passed, with a non-zero exit while any
;;                 obligation stays unverified. It never prints a pass line it
;;                 cannot support;
;;   - `--changed [BASE]` on `full`/`default` — the working-tree diff against
;;                 BASE (origin/main, then HEAD) mapped onto the lane through
;;                 the repository's `@covers` manifest and the existing impact
;;                 machinery (`scripts/testing/changed-selection.rkt`, required
;;                 lazily so `full` gains no `json` dependency). An uncomputable
;;                 or empty diff fails closed; a test file with no `@covers`
;;                 attribution is INCLUDED (fail open) and reported as
;;                 unattributable.
;;
;; Fail-closed contract (never a false green):
;;   - zero selection is an error, not an empty pass;
;;   - the runner must return exactly one result per selected file;
;;   - a result is valid only with exit 0, zero failures, a positive test
;;     count, and no timeout/truncation/identity mismatch;
;;   - worker death or missing results raise out of `run-quick` and are
;;     reported as failure by the caller;
;;   - a managed repository surface that changed during the run fails the
;;     command (the change is REPORTED, never reverted).
;;
;; Scheduling:
;;   - ONE work-conserving dynamic queue (scripts/testing/scheduler.rkt, final
;;     plan §2) schedules every file. Mutation owners (`mutating-file?` or a
;;     declared `@mutates` other than none) take the `repository-exclusive`
;;     resource and dispatch FIRST; while the lock is held, only files whose
;;     declared `@isolation` is `temp-dir` or `sandboxed-home` co-run (they run
;;     outside the checkout and cannot observe a managed-surface mutation);
;;     every other file is eligible once the mutation set drains. This is
;;     never slower than the former serial-then-parallel barrier and never
;;     co-runs conflicting owners;
;;   - two honest execution adapters remain (never the legacy runner): the
;;     audited quick child per file, so each per-file result stays
;;     attributable to exactly one file;
;;   - each file's wall-clock bound comes from its `@timeout` metadata, with
;;     `full-default-timeout` as the fallback.
;;
;; Root resolution: with no explicit root the repository walk is used, which
;; resolves through the launch tree exactly like every other q test tool
;; (classify.rkt's `base-dir`). An explicit root is for fixture trees
;; only, and must contain a `tests/` directory.
;;
;; No repository surface is written or cleaned here: selection is read-only,
;; `--explain` never reaches the execution layer, and surface detection only
;; compares snapshots.
;;
;; STABILITY: internal (developer test CLI)

(require racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         racket/system
         "../run-tests/classify.rkt"
         "quick-runner.rkt"
         "scheduler.rkt")

(provide full-label
         full-obligations
         full-not-provided
         full-default-timeout
         ;; Lanes (`./qt default` / `./qt release`)
         full-lanes
         default-label
         release-label
         lane-relative
         lane-selection
         lane-filter-lines
         ;; Required-offline lane (final plan §6 E1): the union of the suites
         ;; the required CI test jobs select, kept honest by a pinning test
         (struct-out required-producer)
         required-ci-producers
         required-offline-suites
         ;; Host capabilities + the `unavailable-here` obligation state
         (struct-out unavailable-here)
         host-capability-available?
         unavailable-here-members
         ;; Pure lane decision, mapped to messages/exit codes by the CLI after
         ;; all output has been written
         lane-verdict
         ;; Changed-file selection (`--changed [BASE]`, `--changed BASE..HEAD`)
         (struct-out change-report)
         working-tree-changed
         commits-changed
         changed-selection
         quick-changed-owners
         change-report-lines
         ;; CI lanes (`--lane NAME`): one policy table, no YAML numbers
         (struct-out qt-lane)
         qt-lanes
         qt-lane-ref
         ;; Single-suite compatibility accessor (returns the suite set)
         qt-lane-suite
         ;; Release candidate verification (`./qt release --candidate SHA`)
         release-candidate-check
         ;; Resolved plan shared by `--explain` and execution
         (struct-out lane-plan)
         resolve-lane-plan
         plan-explain-lines
         plan-preamble
         plan-label
         default-pass-pattern
         default-pass-line
         release-pass-pattern
         release-pass-line
         run-files
         run-plan
         ;; Release obligations
         (struct-out release-obligation)
         release-obligations
         release-blocking
         release-obligation-lines
         release-unverified-message
         full-execution-supported?
         full-selection
         full-file-timeout
         full-timeouts
         full-explain-lines
         run-full
         (struct-out full-run)
         full-run-valid?
         full-failure-lines
         full-result-reason
         full-surface-violation-lines
         full-pass-line
         full-pass-pattern
         ;; CI surface (`./qt full --json-out/--suite/--shard/--file/...`)
         (struct-out qt-ci-spec)
         (struct-out qt-ci-plan)
         (struct-out qt-ci-outcome)
         qt-strict-test-runner?
         qt-shard-plan-modes
         qt-effective-mode
         qt-suite-relative
         qt-suite-files
         qt-shard-partition
         qt-reproduction-file
         resolve-qt-ci-plan
         qt-ci-run-files
         qt-ci-explain-lines
         qt-ci-pass-line
         qt-ci-pass-pattern
         qt-ci-suite-label
         qt-ci-marker
         ;; Phase D step 1: ONE execution path, two CLI contracts (qt + the
         ;; scripts/run-tests.rkt compatibility adapter).
         (struct-out qt-ci-exec-options)
         qt-ci-default-options
         qt-ci-compat-options
         qt-ci-presentation
         run-qt-ci-plan)

;; One-line honest label reused by every full output summary.
(define full-label "full Racket suite only, not PR/CI verification")

;; Default per-file wall-clock bound (seconds) when a file declares no
;; `@timeout` metadata. Mirrors the quick runner's own `#:timeout` default.
(define full-default-timeout 120)

;; The named obligation claim texts, shared verbatim by the `full` honesty
;; list and the release candidate obligations (final plan §6 E2): one string
;; per claim, so a wording drift cannot create a second source of truth. The
;; platform line stays in the `full` honesty list (a local Linux pass still
;; does not prove macOS behavior), but it is NOT a release candidate
;; obligation: the local release command claims no platform coverage — CI's
;; `test-platform` required context stays its owner, unchanged.
(define governance-parity-obligation-text
  (string-append "governance-controller suite parity: the Racket finalizer port"
                 " (tests/test-wave-finalizer-{model,adapter,controller,native}.rkt) runs here,"
                 " pinned to goldens captured from the original Python governance-controller suite"
                 " (scripts/github/wave-finalizer.py + test_wave_finalizer.py, since deleted);"
                 " no live oracle is left to diff against, so this command is the authoritative"
                 " parity run"))
(define platform-obligation-text
  "macOS/platform lane (platform-specific behavior of this Linux host)")
(define github-branch-protection-text
  "GitHub branch protection and required status checks")
(define release-provenance-obligation-text
  "release provenance and exact-candidate verification (run `racket scripts/release-preflight.rkt`)")
(define ci-verification-obligation-text
  "PR/CI verification of this commit (counted by the GitHub pipeline)")

;; What a local pass of this selection cannot prove. Printed verbatim by
;; `--explain` so the absence of these checks is never mistaken for
;; verification.
(define full-obligations
  (list
   governance-parity-obligation-text
   platform-obligation-text
   github-branch-protection-text
   release-provenance-obligation-text))

;; Counted obligations this command does NOT provide: they are counted (and
;; required) by other gates, so a green `./qt full` must never be read as a
;; green PR. Kept explicit so the gap is visible instead of implied.
;;
;; The one item whose honest wording differs per lane: `full` runs every
;; selected file; `default` does not run (and cannot verify) members whose
;; declared `@requires` this host cannot satisfy. The constant is shared so a
;; wording drift cannot reintroduce a second source of truth.
(define full-requires-obligation-text
  "`@requires` gating: every selected file runs, including live-provider, terminal and browser tests")

(define full-not-provided
  (list
   ci-verification-obligation-text
   "Racket dependency, packaging, scribble-doc and release-readiness gates (counted by `racket scripts/release-preflight.rkt`)"
   (string-append "the live benchmark suite and the CI latency-budget guard "
                  "(benchmark.yml / pr-latency-guard.yml; the latency guard's automatic "
                  "trigger is disabled as of 2026-11-16 — it can only fail closed until "
                  "a trusted collector is bound, and it is not a required check)")
   full-requires-obligation-text
   (string-append "protection of repository files: a test that rewrites "
                  "info.rkt/README.md/CHANGELOG.md is DETECTED and fails the command, "
                  "but the bytes are not restored")
   "sandboxing of audited test code: tests run TRUSTED, not sandboxed"))

;; ============================================================
;; Host execution support (informational; run-quick re-checks it)
;; ============================================================

;; Same precondition the quick runner enforces before it starts a child.
;; Printed by `--explain` so the plan is honest on a host that cannot execute;
;; the authoritative check stays inside `run-quick`.
(define (full-execution-supported?)
  (and (eq? (system-type 'os*) 'linux)
       (directory-exists? "/proc/self")
       (and (find-executable-path "setsid") #t)))

;; `command` names the lane whose plan is being printed; the default keeps the
;; frozen `./qt full` wording.
(define (full-execution-support-line [command "./qt full"])
  (if (full-execution-supported?)
      "execution support: available (Linux /proc plus setsid; the runner re-checks before running)"
      (format
       "execution support: UNAVAILABLE on this host; ~a fails closed, only this plan is available"
       command)))

;; ============================================================
;; Selection
;; ============================================================

(define (selection-root root)
  ;; Directory form on purpose: the printed plan must not depend on which
  ;; launch-tree candidate supplied the root.
  (path->directory-path (simplify-path (path->complete-path (cond
                                                              [root
                                                               (if (path? root)
                                                                   root
                                                                   (string->path root))]
                                                              [else base-dir]))
                                       #f)))

;; Root-relative normalized path strings, lexicographically sorted so the
;; printed plan is byte-identical across runs and filesystems.
(define (selection-relative root)
  (define rels
    (sort (cond
            [root (collect-test-files 'all #:root (selection-root root))]
            [else (collect-test-files 'all)])
          string<?))
  (when (null? rels)
    (error 'full-plan
           "full selection is empty under ~a (zero files is a failure, not a pass)"
           (path->string (selection-root root))))
  ;; `check-duplicates` returns a duplicate ELEMENT (or #f), never a list;
  ;; the guard below tests the returned value itself.
  (selection-duplicate-guard rels))

;; Two entries for one file would run and count it twice, so duplicates are a
;; hard error. Exposed through `test-support` so the `#f`-returning guard has a
;; direct regression test (collection never produces duplicates by itself).
(define (selection-duplicate-guard rels)
  (define dups (check-duplicates rels))
  (when dups
    (error 'full-plan "full selection contains duplicate entries: ~a" dups))
  rels)

(module+ test-support
  (provide selection-duplicate-guard))

;; Absolute path strings in the same deterministic order: what the runner
;; executes, and the identity each result must match.
(define (full-selection [root #f])
  (define base (selection-root root))
  (for/list ([rel (in-list (selection-relative root))])
    (path->string (simplify-path (build-path base (string->path rel)) #f))))

;; ============================================================
;; Per-file timeouts
;; ============================================================

;; `@timeout <seconds>` metadata when declared, otherwise the caller default.
;; Non-positive/absent metadata falls back, so the runner never receives a
;; bound it would reject.
(define (full-file-timeout f [default full-default-timeout])
  (define declared (hash-ref (get-file-metadata f) 'timeout #f))
  (if (exact-positive-integer? declared) declared default))

;; Hash normalized absolute path -> seconds, exactly the `#:timeouts` shape
;; the quick runner accepts.
(define (full-timeouts files #:timeout [timeout full-default-timeout])
  (for/hash ([f (in-list files)])
    (values f (full-file-timeout f timeout))))

(define (full-timeout-policy-line files #:timeout [timeout full-default-timeout])
  (define bounds (full-timeouts files #:timeout timeout))
  (define declared (count (lambda (f) (hash-ref (get-file-metadata f) 'timeout #f)) files))
  (format
   "timeouts: @timeout metadata when declared, else default ~as (declared ~a of ~a files, max ~as)"
   timeout
   declared
   (length files)
   (if (null? files)
       timeout
       (apply max (cons timeout (hash-values bounds))))))

(define (full-mutation-policy-line files)
  (define owners (filter repository-exclusive-file? files))
  (define co-runs (filter co-runs-under-exclusive-file? files))
  (format
   (string-append
    "resource isolation: ~a mutation-owning file(s) hold repository-exclusive (dispatched first; "
    "~a declared temp-dir/sandboxed-home file(s) co-run; ~a file(s) join after owners drain)")
   (length owners)
   (length co-runs)
   (- (length files) (length owners))))

;; ============================================================
;; Lanes (`full` / `default` / `release`)
;; ============================================================

(define full-lanes '(full default release))

;; Lane labels. Neither one may ever be read as PR/CI or candidate
;; verification; the pass lines embed them verbatim. `default` establishes the
;; OFFLINE PR claims completely and says exactly what stays with CI.
(define default-label
  "offline PR verification only; platform and GitHub-hosted claims remain with CI")
(define release-label "release review only, not candidate verification")

;; ============================================================
;; Host capabilities + the `unavailable-here` state (final plan §6 E1)
;; ============================================================

;; The third obligation state between "verified" and "not provided": a
;; required-offline member whose declared `@requires` THIS HOST cannot satisfy.
;; It is listed, never counted as verified, and never blocks the offline
;; verdict: a local pass cannot establish it and CI stays authoritative for it.
;; Without this state the only options would be "default can never pass" or
;; "silently drop required tests", and both are forbidden by the final plan.
(struct unavailable-here (file requirements) #:transparent)

;; One capability model, deliberately: the runner's own profile policy
;; (`profile-unavailable-requirements` in classify.rkt), not a second registry.
;; A `local` host (the profile every qt command runs under) declares live
;; provider credentials unavailable; the rest of the classifier's `@requires`
;; vocabulary executes under the local profile contract exactly like CI does
;; (the required CI jobs run the same files with the same profile default), so
;; no flaky host probing is invented here. An unknown token is never claimed
;; (conservative: the member is listed unavailable, never silently run).
(define (host-capability-available? token)
  (define normalized (normalize-requirement token))
  (cond
    [(member normalized
             (map normalize-requirement (profile-unavailable-requirements 'local)))
     #f]
    [(member normalized schema-requires-values)
     (cond
       [(equal? normalized "git") (and (find-executable-path "git") #t)]
       ;; fs is implied by local execution; network/terminal/browser files run
       ;; under the local profile contract (a real outage or missing helper
       ;; fails the file honestly — it is never pre-declared unavailable).
       [else #t])]
    [else #f]))

;; The declared `@requires` tokens of one file that this host cannot satisfy.
;; `none` and an absent declaration never make a file unavailable.
(define (unsatisfied-requirements f)
  (filter (lambda (tok)
            (and (not (equal? tok "none"))
                 (not (host-capability-available? tok))))
          (map normalize-requirement (hash-ref (get-file-metadata f) 'requires '()))))

;; Partition root-relative lane members into executable evidence and the
;; `unavailable-here` list. The unavailable members keep their root-relative
;; identity (the same form the printed selection uses) so the explain output
;; names exactly the files the run will not execute.
(define (unavailable-here-members base rels)
  (filter values
          (for/list ([rel (in-list rels)])
            (define unsat (unsatisfied-requirements (root-relative-path-string base rel)))
            (and (pair? unsat) (unavailable-here rel unsat)))))

(define (root-relative-path base rel)
  (build-path base (string->path rel)))

;; Metadata helpers (classify.rkt) are string-parameterized; feed them the
;; normalized absolute path string, exactly like `full-timeouts` does.
(define (root-relative-path-string base rel)
  (path->string (root-relative-path base rel)))

;; Lane filter over an explicit root-relative list: the ONE place the default
;; lane's selection is computed, so `full` and `default` share every other
;; selection step. `default` is the required-offline UNION of the suites the
;; required CI test jobs select (final plan §6 E1): a file joins when ANY
;; member suite selects it, through classify.rkt's own suite dispatch — no
;; second annotation scheme, no hand-maintained list. The member lanes keep
;; their own timeout/shard policies (see `qt-lanes`); this lane's union has
;; its own entry there. An empty lane is a failure, never an empty pass.
(define (suite-select-rels suite base rels)
  ;; Metadata identity is the ABSOLUTE path (the form selection-relative's walk
  ;; cached), so fixture roots resolve exactly the metadata the universe used.
  (define abs->rel
    (for/hash ([rel (in-list rels)])
      (values (root-relative-path-string base rel) rel)))
  (for/list ([abs (in-list (collect-classify-file-list suite (hash-keys abs->rel)))])
    (hash-ref abs->rel abs)))

(define (lane-filter lane base rels)
  (case lane
    [(full release) rels]
    [(default)
     (define kept
       (sort (remove-duplicates
              (append-map
               (lambda (suite) (suite-select-rels suite base rels))
               (qt-lane-suites (qt-lane-ref 'required-offline))))
             string<?))
     (when (null? kept)
       (error 'full-plan
              "required-offline selection is empty under ~a (zero files is a failure, not a pass)"
              (path->string base)))
     kept]
    [else (error 'full-plan "unknown lane: ~a" lane)]))

;; Root-relative lane selection, in the same deterministic (lexicographic)
;; order as `full`.
(define (lane-relative lane [root #f])
  (lane-filter lane (selection-root root) (selection-relative root)))

(define (lane-selection lane [root #f])
  (define base (selection-root root))
  (for/list ([rel (in-list (lane-relative lane root))])
    (path->string (simplify-path (root-relative-path base rel) #f))))

;; Honest per-lane description for `--explain`: what the filter did, counted
;; BY SUITE, so the plan can never imply more or less than the selection
;; actually is. `base` and `universe` come from the resolved plan itself, so
;; the description cannot drift from the selection the plan will run.
;; The default lane additionally prints the claim → lane → suite mapping and
;; the detected capabilities (see `plan-explain-lines`).
(define (lane-filter-lines lane base universe)
  (case lane
    [(full release) (list "lane filter: none (the complete available selection)")]
    [(default)
     (define counts (required-offline-suite-counts base universe))
     (define union-count (length (lane-filter 'default base universe)))
     (append
      (list "lane selection: required-offline — the union of the suites the required CI test jobs select"
            "  (slow mandatory owners are INCLUDED through their suite; a declared @requires this host"
            "   cannot satisfy does not exclude a file — it becomes `unavailable-here` below)")
      (for/list ([entry (in-list counts)])
        (format "  suite ~a: ~a file(s)"
                (car entry) (cdr entry)))
      (list (format "  union: ~a file(s) (suites overlap, so the per-suite counts do not sum)"
                    union-count)))]
    [else (error 'full-plan "unknown lane: ~a" lane)]))

;; Per-producer suite sizes for the claim mapping. Classification goes through
;; classify.rkt's own dispatch (`suite-select-rels`) over the same universe the
;; selection used — no second suite registry, and the counts cannot drift from
;; the selection.
(define (required-offline-suite-counts base universe)
  (for/list ([producer (in-list required-ci-producers)])
    (define lane-name (required-producer-lane producer))
    (cons lane-name
          (length (remove-duplicates
                   (append-map (lambda (suite) (suite-select-rels suite base universe))
                               (qt-lane-suites (qt-lane-ref lane-name))))))))

;; ============================================================
;; Changed-file selection (`--changed [BASE]`)
;; ============================================================

;; The repository's existing impact machinery, loaded ON DEMAND: `full`,
;; `default` and `release` must not inherit the `json` dependency that
;; `scripts/testing/changed-selection.rkt` carries (classify.rkt deliberately
;; avoids it for minimal installations). A load failure is a selection failure.
(define-runtime-path impact-module "changed-selection.rkt")

(define (impact-procedure name)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (error 'full-plan
                            "changed selection unavailable: cannot load ~a (~a); fail closed"
                            impact-module
                            (exn-message e)))])
    (let ([binding (dynamic-require impact-module name)])
      (unless (procedure? binding)
        (error 'full-plan
               "changed selection unavailable: ~a in ~a is not a procedure"
               name
               impact-module))
      binding)))

;; Run git in `root`: (values ok? stdout). stderr is discarded; the caller
;; decides what a failure means.
(define (git-run root args)
  (define git (find-executable-path "git"))
  (if (not git)
      (values #f "")
      (with-handlers ([exn:fail? (lambda (_) (values #f ""))])
        (define out (open-output-string))
        ;; `system*` yields #t on a zero exit and #f otherwise; anything that is
        ;; not #t is a failure (fail closed).
        (define ok?
          (parameterize ([current-directory root]
                         [current-output-port out]
                         [current-error-port (open-output-string)])
            (apply system* git args)))
        (values (eq? ok? #t) (get-output-string out)))))

(define (git-lines text)
  (filter non-empty-string? (map string-trim (string-split text "\n"))))

(define (git-ok? root . args)
  (let-values ([(ok? _) (git-run root args)])
    ok?))

;; Working-tree diff against BASE (untracked files count: a new test file is
;; part of the working tree), mapped from repository-root-relative to
;; selection-root-relative paths.
;; Returns (values changed-rels base-label outside-rels). Raises when the diff
;; cannot be computed: an uncomputable diff must never look like "nothing
;; changed".
(define (working-tree-changed base root)
  (unless (find-executable-path "git")
    (error 'full-plan
           "cannot compute the working-tree diff under ~a: git is not available (fail closed)"
           (path->string root)))
  (define requested
    (and base
         (if (path? base)
             (path->string base)
             base)))
  (define resolved
    (or requested
        (for/first ([candidate (in-list '("origin/main" "HEAD"))]
                    #:when (git-ok? root
                                    "rev-parse"
                                    "--verify"
                                    "--quiet"
                                    (string-append candidate "^{commit}")))
          candidate)))
  (unless resolved
    (error
     'full-plan
     "cannot compute the working-tree diff under ~a: neither origin/main nor HEAD resolves (fail closed)"
     (path->string root)))
  (define-values (prefix-ok? prefix-text) (git-run root (list "rev-parse" "--show-prefix")))
  (define prefix
    (if prefix-ok?
        (string-trim prefix-text)
        ""))
  (define-values (diff-ok? diff-text)
    (git-run root (list "diff" "--name-only" "--no-renames" resolved)))
  (define-values (untracked-ok? untracked-text)
    (git-run root (list "ls-files" "--others" "--exclude-standard")))
  (unless (and diff-ok? untracked-ok?)
    (error 'full-plan
           "cannot compute the working-tree diff against ~a under ~a: git failed (fail closed)"
           resolved
           (path->string root)))
  (define all (remove-duplicates (append (git-lines diff-text) (git-lines untracked-text))))
  (define-values (rels outside) (diff-paths->rels all prefix))
  (values rels resolved outside))

;; Repository-root-relative diff paths -> selection-root-relative paths. Paths
;; outside the selection root are returned separately (never silently dropped).
(define (diff-paths->rels all prefix)
  (define mapped
    (for/list ([p (in-list all)])
      (cond
        [(string=? prefix "") (cons p #f)]
        [(string-prefix? p prefix) (cons (substring p (string-length prefix)) #f)]
        [else (cons p 'outside)])))
  (values (sort (for/list ([m (in-list mapped)]
                           #:unless (eq? (cdr m) 'outside))
                  (car m))
                string<?)
          (sort (for/list ([m (in-list mapped)]
                           #:when (eq? (cdr m) 'outside))
                  (car m))
                string<?)))

;; Diff between two commits (`BASE..HEAD`). Both refs must resolve to
;; commits: a typo'd base is a selection failure, never "nothing changed".
;; Returns (values changed-rels label outside-rels).
(define (commits-changed base head root)
  (unless (find-executable-path "git")
    (error 'full-plan
           "cannot compute the diff ~a..~a under ~a: git is not available (fail closed)"
           base
           head
           (path->string root)))
  (define (resolves? ref name)
    (define resolved?
      (and (string? ref)
           (positive? (string-length ref))
           (git-ok? root "rev-parse" "--verify" "--quiet" (string-append ref "^{commit}"))))
    (unless resolved?
      (error
       'full-plan
       "cannot compute the diff ~a..~a under ~a: ~a ref ~a does not resolve to a commit (fail closed)"
       base
       head
       (path->string root)
       name
       ref))
    resolved?)
  (resolves? base "base")
  (resolves? head "head")
  (define-values (prefix-ok? prefix-text) (git-run root (list "rev-parse" "--show-prefix")))
  (define prefix
    (if prefix-ok?
        (string-trim prefix-text)
        ""))
  (define-values (ok? text) (git-run root (list "diff" "--name-only" "--no-renames" base head)))
  (unless ok?
    (error 'full-plan
           "cannot compute the diff ~a..~a under ~a: git failed (fail closed)"
           base
           head
           (path->string root)))
  (define-values (rels outside) (diff-paths->rels (remove-duplicates (git-lines text)) prefix))
  (values rels (format "~a..~a" base head) outside))

;; `./qt quick --changed`: conservative impact selection over the audited quick
;; owners, through the SAME @covers/impact machinery as the full/default lane.
;; Unattributable owners are INCLUDED (fail open) and any escalation includes
;; every owner, so the selection can only widen; it is a reproduction, never a
;; PR/CI claim.
(define (quick-changed-owners root
                              owner-rels
                              changed
                              #:base [base "working tree"]
                              #:outside [outside '()])
  (changed-selection root owner-rels changed #:base base #:outside outside))

;; What the working-tree diff means for one lane. `included` is exactly what
;; the command would run; `excluded` is what it would skip and why.
(struct change-report (base changed included excluded unattributable escalations covers-count outside)
  #:transparent)

(define (change-report-escalation-text escalation)
  (format "~a ~a: ~a"
          (hash-ref escalation 'code)
          (or (hash-ref escalation 'changed-file #f) "-")
          (or (hash-ref escalation 'detail #f) "")))

;; Pure (no git, no execution): attribution comes from the repository's
;; `@covers` manifest plus `compute-impact-selection`. A test file with no
;; `@covers` attribution is INCLUDED (fail open) and reported as
;; unattributable; any escalation widens the selection to the whole lane.
(define (changed-selection root rels changed #:base [base "working tree"] #:outside [outside '()])
  (define load-manifest (impact-procedure 'load-coverage-manifest))
  (define compute (impact-procedure 'compute-impact-selection))
  (define selected* (impact-procedure 'selection-selected))
  (define escalated** (impact-procedure 'selection-escalated?))
  (define escalations* (impact-procedure 'selection-escalations))
  (define changed* (sort (remove-duplicates changed) string<?))
  (define-values (covers sources status) (load-manifest root))
  (define selection (compute root changed* covers sources rels #:manifest-status status))
  (define chosen (selected* selection))
  (define escalated? (and (escalated** selection) #t))
  (define escalations (map change-report-escalation-text (escalations* selection)))
  (define (attributed? rel)
    (pair? (hash-ref covers rel '())))
  (define unattributable (filter-not attributed? rels))
  (define (included? rel)
    (or escalated? (and (member rel chosen) #t) (not (attributed? rel))))
  (change-report base
                 changed*
                 (filter included? rels)
                 (filter-not included? rels)
                 unattributable
                 escalations
                 (hash-count covers)
                 (sort (remove-duplicates outside) string<?)))

;; One line per attribution decision, so `--explain` states WHY each file is in
;; or out of the changed selection.
;; The changed-selection header: a commit range and a working-tree diff are
;; DIFFERENT inputs (a range sees no untracked files), and the report must say
;; which one produced the selection.
(define (change-report-source-line report)
  (define base (change-report-base report))
  (if (and (string? base) (string-contains? base ".."))
      (format "changed: commit range ~a (git diff --name-only BASE HEAD)" base)
      (format "changed: working-tree diff against ~a (git diff --name-only BASE plus untracked files)"
              base)))

(define (change-report-lines report)
  (define unattributable (change-report-unattributable report))
  (define escalations (change-report-escalations report))
  (append
   (list
    (change-report-source-line report)
    (format "changed files: ~a" (length (change-report-changed report)))
    (format
     "changed mapping: @covers manifest (~a attributed test files) plus the impact dependency graph"
     (change-report-covers-count report))
    (format
     "changed attribution: ~a of ~a selected files carry no @covers attribution and are INCLUDED (fail open)"
     (length unattributable)
     (+ (length (change-report-included report)) (length (change-report-excluded report))))
    (if (null? escalations)
        "changed escalations: none"
        (format "changed escalations: ~a (the whole lane is included, fail open)"
                (length escalations))))
   (for/list ([rel (in-list (change-report-outside report))])
     (format "  changed outside this root (ignored): ~a" rel))
   (for/list ([line (in-list escalations)])
     (format "  escalation: ~a" line))
   (for/list ([rel (in-list (change-report-changed report))])
     (format "  changed: ~a" rel))
   (for/list ([rel (in-list (change-report-included report))])
     (format "  included: ~a (~a)"
             rel
             (cond
               [(member rel unattributable) "no @covers attribution; fail open"]
               [(pair? escalations) "whole lane widened by escalation; fail open"]
               [else "attributed to a changed file"])))
   (for/list ([rel (in-list (change-report-excluded report))])
     (format "  excluded: ~a (attributed to none of the changed files)" rel))))

;; ============================================================
;; Release obligations
;; ============================================================

;; One candidate-stage obligation: a stable slug (the claim's identity —
;; adding, removing or verifying one entry never renumbers the others, final
;; plan §6 E2 rev 3), the claim text, and whether THIS command verified it.
(struct release-obligation (slug text verified?) #:transparent)

;; The candidate obligation table: stable slugs over the shared claim texts.
;;   - governance-parity runs locally (the finalizer owners are part of the
;;     release selection);
;;   - ci-verification, branch-protection and release-provenance are satisfied
;;     only by explicit evidence bound to the exact candidate SHA (--evidence,
;;     final plan §6 E2) — never inferred from a green suite run and never
;;     fetched from the network (the release path stays offline).
;; The macOS/platform claim is NOT a candidate obligation (CI's test-platform
;; required context is unchanged; the local release command claims no platform
;; coverage) — it remains in the `full` honesty list above.
(define release-candidate-obligations
  (list
   (list 'governance-parity governance-parity-obligation-text)
   (list 'ci-verification ci-verification-obligation-text)
   (list 'branch-protection github-branch-protection-text)
   (list 'release-provenance release-provenance-obligation-text)))

(define (release-obligations)
  (for/list ([entry (in-list release-candidate-obligations)])
    (release-obligation (car entry) (cadr entry) #f)))

;; Every obligation still unverified. A non-empty result blocks the release
;; pass line; an empty one is the only path to a release pass.
(define (release-blocking obligations)
  (filter-not release-obligation-verified? obligations))

(define (release-obligation-lines obligations)
  (for/list ([o (in-list obligations)])
    (format "RELEASE OBLIGATION ~a [~a] ~a"
            (release-obligation-slug o)
            (if (release-obligation-verified? o) "verified" "NOT VERIFIED")
            (release-obligation-text o))))

(define (release-unverified-message blocking)
  (format
   "release unverified: ~a release obligation(s) not verified by this command (~a); a release requires a verified candidate"
   (length blocking)
   (string-join (map (lambda (o) (format "~a" (release-obligation-slug o))) blocking) ", ")))

;; ============================================================
;; Resolved plan (computed once; shared by `--explain` and execution)
;; ============================================================

(struct lane-plan (lane root rels files universe changed unavailable) #:transparent)

(define (lane-name lane)
  (case lane
    [(full) "full"]
    [(default) "default"]
    [(release) "release"]
    [else (error 'full-plan "unknown lane: ~a" lane)]))

(define (lane-command lane)
  (format "./qt ~a" (lane-name lane)))

;; Resolve exactly what a command would run, or raise. `--explain` and execution
;; share this, so a plan can never describe different work than the run.
(define (resolve-lane-plan lane [root #f] #:changed? [changed? #f] #:base [base #f])
  (unless (memq lane full-lanes)
    (error 'full-plan "unknown lane: ~a" lane))
  (define root* (selection-root root))
  ;; `universe` is every discovered file (what the lane filter reads);
  ;; `lane-rels` is the lane filter's result over that same list, so the plan
  ;; costs exactly one discovery walk.
  (define universe (selection-relative root))
  (define lane-rels (lane-filter lane root* universe))
  (define report
    (and
     changed?
     (let-values ([(changed-files label outside) (working-tree-changed base root*)])
       (when (null? changed-files)
         (error
          'full-plan
          "the working-tree diff against ~a is empty under ~a (zero selection is a failure, not a pass)"
          label
          (path->string root*)))
       (changed-selection root* lane-rels changed-files #:base label #:outside outside))))
  (define rels
    (if report
        (change-report-included report)
        lane-rels))
  ;; `unavailable-here` (final plan §6 E1): required-offline members whose
  ;; declared `@requires` this host cannot satisfy. They are listed, never
  ;; executed, never verified, and never block the offline verdict — CI stays
  ;; authoritative for them. Every other lane keeps the contract that every
  ;; selected file runs.
  (define unavailable
    (if (eq? lane 'default)
        (unavailable-here-members root* rels)
        '()))
  (define runnable-rels
    (if (eq? lane 'default)
        (filter (lambda (rel)
                  (not (for/or ([u (in-list unavailable)])
                         (string=? (unavailable-here-file u) rel))))
                rels)
        rels))
  (cond
    [(and report (null? rels))
     (error
      'full-plan
      "changed selection is empty under ~a (the diff against ~a affects none of the ~a lane files)"
      (path->string root*)
      (change-report-base report)
      (length lane-rels))]
    ;; DEFENSIVE and untestable: lane-filter/selection-relative already fail closed on an empty selection.
    [(null? rels)
     (error 'full-plan
            "lane selection is empty under ~a (zero files is a failure, not a pass)"
            (path->string root*))]
    [(null? runnable-rels)
     (error 'full-plan
            (string-append
             "required-offline selection under ~a has ~a member(s) and ALL are unavailable-here; "
             "a run that can execute no required check is a failure, not a pass")
            (path->string root*)
            (length rels))]
    [else (void)])
  (lane-plan lane
             root*
             rels
             (for/list ([rel (in-list runnable-rels)])
               (path->string (simplify-path (root-relative-path root* rel) #f)))
             universe
             report
             unavailable))

(define (plan-label plan)
  (case (lane-plan-lane plan)
    [(full) full-label]
    [(default) default-label]
    [(release) release-label]
    [else (error 'full-plan "unknown lane: ~a" (lane-plan-lane plan))]))

(define (plan-header-line plan)
  (define root (path->string (lane-plan-root plan)))
  (define changed (lane-plan-changed plan))
  (case (lane-plan-lane plan)
    [(default)
     (if changed
         (format
          "default: changed-file selection of the offline PR verification lane under ~a (diff against ~a)"
          root
          (change-report-base changed))
         (format
          "default: offline PR verification (required-offline lane) over the complete available Racket suite under ~a"
          root))]
    [(full)
     ;; `changed` is #f for plain `./qt full`; the header must never dereference
     ;; a report that was not requested. DEFENSIVE/untestable: plain full renders
     ;; through the frozen explain body and never reaches this #f arm.
     (if changed
         (format
          "full: changed-file selection of the complete available Racket suite under ~a (diff against ~a)"
          root
          (change-report-base changed))
         (format "full: complete available Racket suite under ~a" root))]
    [(release)
     (format "release: complete available Racket suite under ~a plus the release obligation list"
             root)]
    [else (error 'full-plan "unknown lane: ~a" (lane-plan-lane plan))]))

;; What a plan would run, in the order the command would run it, plus why.
;; The default lane renders its own body (claim → lane → suite mapping,
;; detected capabilities, and the `unavailable-here` list); `full` keeps the
;; frozen body, and `release` extends it with the obligation list.
(define (default-explain-lines plan command rels changed)
  (define files (lane-plan-files plan))
  (define unavailable (lane-plan-unavailable plan))
  (append
   (list (plan-header-line plan)
         (format "label: ~a" (plan-label plan))
         (format "selection: ~a files (selection only; nothing executed, nothing cleaned)"
                 (length files)))
   (lane-filter-lines 'default (lane-plan-root plan) (lane-plan-universe plan))
   (default-claim-mapping-lines command)
   (host-capability-lines command)
   (if (null? unavailable)
       (list "unavailable-here: none — every required-offline member is executable here")
       (append
        (list (format (string-append
                       "unavailable-here: ~a required-offline member(s) this host cannot verify "
                       "(LISTED below, never executed, never verified, never blocking; CI stays authoritative)")
                      (length unavailable)))
        (for/list ([u (in-list unavailable)])
          (format "  unavailable-here: ~a (@requires ~a)"
                  (unavailable-here-file u)
                  (string-join (unavailable-here-requirements u) ", ")))))
   (if changed
       (change-report-lines changed)
       '())
   (list (full-timeout-policy-line files)
         (full-mutation-policy-line files)
         (full-execution-support-line command))
   rels
   (list "")
   (list (format "NOT VERIFIED by ~a:" command))
   (for/list ([item (in-list full-obligations)])
     (format "  - ~a" item))
   (list "")
   (list (format "NOT PROVIDED by ~a (counted by other gates):" command))
   ;; The @requires item is stated per lane: `default` lists unavailable-here
   ;; members instead of running every selected file.
   (for/list ([item (in-list full-not-provided)])
     (format "  - ~a"
             (if (equal? item full-requires-obligation-text)
                 (string-append
                  "`@requires` gating: executable members run; a member whose declared @requires this "
                  "host cannot satisfy is unavailable-here (listed above; CI stays authoritative)")
                 item)))))

;; The claim → lane → suite mapping: which required CI test job each member
;; lane satisfies. The suite → test mapping is the printed selection itself.
(define (default-claim-mapping-lines command)
  (append
   (list (format "claim mapping for ~a (required CI test job -> lane -> suite):" command))
   (for/list ([producer (in-list required-ci-producers)])
     (define suites (qt-lane-suites (qt-lane-ref (required-producer-lane producer))))
     (format "  ~a -> lane ~a -> suite~a ~a"
             (string-join (required-producer-contexts producer) ", ")
             (required-producer-lane producer)
             (if (= (length suites) 1) "" "s")
             (string-join (map symbol->string suites) ", ")))
   (list (format "  required-offline (this command) -> union of those suites: ~a"
                 (string-join (map symbol->string required-offline-suites) ", ")))))

;; The detected capability table: ONE model, the runner's own local profile
;; policy, extended with the cheap real probes (git). This is what the
;; `unavailable-here` decisions above are derived from.
(define (host-capability-lines command)
  (append
   (list (format "capabilities detected for ~a (one model: the runner's own local profile policy):" command))
   (for/list ([tok (in-list schema-requires-values)])
     (format "  ~a: ~a"
             tok
             (if (host-capability-available? tok)
                 "available"
                 "NOT available locally")))
   (list (string-append
          "  (provider-key follows the local profile policy, which declares live provider credentials "
          "unavailable; a network/terminal/browser file that cannot really run FAILS honestly instead)")
         (string-append
          "  (external claims — platform lane and GitHub-hosted checks — are never established locally; "
          "CI stays authoritative for them)"))))

(define (plan-explain-lines plan)
  (define lane (lane-plan-lane plan))
  (define changed (lane-plan-changed plan))
  (define rels (lane-plan-rels plan))
  (define files (lane-plan-files plan))
  (define command (lane-command lane))
  ;; Plain `./qt full --explain` is frozen byte-for-byte: it renders through the
  ;; same body as `full-explain-lines`, from the plan's own selection, so the
  ;; lane work cannot alter an existing command's plan output.
  (cond
    [(and (eq? lane 'full) (not changed))
     (full-explain-lines/selection (lane-plan-root plan)
                                   (lane-plan-rels plan)
                                   (lane-plan-files plan))]
    [(eq? lane 'default)
     (default-explain-lines plan command rels changed)]
    [else
     (append
      (list (plan-header-line plan)
            (format "label: ~a" (plan-label plan))
            (format "selection: ~a files (selection only; nothing executed, nothing cleaned)"
                    (length rels)))
      (lane-filter-lines lane (lane-plan-root plan) (lane-plan-universe plan))
      (if changed
          (change-report-lines changed)
          '())
      (list (full-timeout-policy-line files)
            (full-mutation-policy-line files)
            (full-execution-support-line command))
      rels
      (list "")
      (list (format "NOT VERIFIED by ~a:" command))
      (for/list ([item (in-list full-obligations)])
        (format "  - ~a" item))
      (list "")
      (list (format "NOT PROVIDED by ~a (counted by other gates):" command))
      (for/list ([item (in-list full-not-provided)])
        (format "  - ~a" item))
      (if (eq? lane 'release)
          (append (list "")
                  (list "RELEASE OBLIGATIONS (unverified means a non-zero exit, never a pass line):")
                  (release-obligation-lines (release-obligations)))
          '()))]))

;; The frozen plain-`./qt full` preamble (frozen wording, updated for the
;; single-queue dispatch); every other plan states its own selection.
(define (plain-full-preamble jobs)
  (format (string-append
           "full: starting the complete available Racket suite (work-conserving queue, jobs ~a; "
           "mutation owners dispatch first); ~a")
          jobs
          full-label))

(define (plan-selection-noun plan)
  (case (lane-plan-lane plan)
    [(full) "complete available Racket suite"]
    [(default) "offline PR verification lane (required-offline)"]
    [(release) "complete available Racket suite (release review)"]
    [else (error 'full-plan "unknown lane: ~a" (lane-plan-lane plan))]))

(define (plan-preamble plan jobs)
  (define changed (lane-plan-changed plan))
  (if (and (eq? (lane-plan-lane plan) 'full) (not changed))
      (plain-full-preamble jobs)
      (format (string-append
               "~a: starting the ~a selection of the ~a (~a files; work-conserving queue, jobs ~a; "
               "mutation owners dispatch first); ~a")
              (lane-name (lane-plan-lane plan))
              (if changed "changed-file" "lane")
              (plan-selection-noun plan)
              (length (lane-plan-files plan))
              jobs
              (plan-label plan))))

;; The single honest pass line per lane, and its pinned pattern: the honest
;; label is part of the contract, so it cannot be dropped from the output.
;; `unavailable-here` members are never executed or verified; their COUNT is
;; part of the pass line so the offline verdict cannot silently swallow them
;; (the final plan §9 case: listed, not verified, not silently dropped).
(define default-pass-pattern
  (regexp
   (string-append
    "^default passed: [0-9]+ files, [0-9]+ tests \\(offline PR verification; [0-9]+ unavailable-here "
    "listed for CI; offline PR verification only; platform and GitHub-hosted claims remain with CI\\)$")))

(define (default-pass-line run unavailable-count)
  (unless (exact-nonnegative-integer? unavailable-count)
    (error 'default-pass-line
           "unavailable-count must be a non-negative integer, got: ~a"
           unavailable-count))
  (format
   (string-append
    "default passed: ~a files, ~a tests (offline PR verification; ~a unavailable-here listed for CI; ~a)")
   (length (full-run-files run))
   (apply + (map quick-result-total (full-run-results run)))
   unavailable-count
   default-label))

(define release-pass-pattern
  #rx"^release passed: [0-9]+ files, [0-9]+ tests \\(release review only, not candidate verification\\)$")

(define (release-pass-line run)
  (format "release passed: ~a files, ~a tests (~a)"
          (length (full-run-files run))
          (apply + (map quick-result-total (full-run-results run)))
          release-label))

;; ============================================================
;; Explain
;; ============================================================

(define (full-explain-lines [root #f])
  (full-explain-lines/selection (selection-root root)
                                (selection-relative root)
                                (full-selection root)))

;; The FROZEN `./qt full --explain` body: header, label, selection count, the
;; three policy lines, the selection, then both honesty lists. Both
;; `full-explain-lines` and the lane planner render through this one function,
;; so the frozen output cannot drift while other lanes add their own lines.
(define (full-explain-lines/selection base rels files)
  (append (list (format "full: complete available Racket suite under ~a" (path->string base))
                (format "label: ~a" full-label)
                (format "selection: ~a files (selection only; nothing executed, nothing cleaned)"
                        (length rels))
                (full-timeout-policy-line files)
                (full-mutation-policy-line files)
                (full-execution-support-line))
          rels
          (list "")
          (list "NOT VERIFIED by ./qt full:")
          (for/list ([item (in-list full-obligations)])
            (format "  - ~a" item))
          (list "")
          (list "NOT PROVIDED by ./qt full (counted by other gates):")
          (for/list ([item (in-list full-not-provided)])
            (format "  - ~a" item))))

(module+ test-support
  (provide full-explain-lines/selection))

;; ============================================================
;; Managed repository surfaces (detection only; never restored)
;; ============================================================

;; Same surfaces the legacy runner guards (info.rkt, README.md, CHANGELOG.md).
;; The snapshot covers BYTES and permission BITS. A change is reported by name
;; and fails the command after the per-file evidence is printed. Nothing is
;; ever restored (no git checkout, no saved bytes): restoring would destroy
;; pre-existing or concurrent user edits.
(define (managed-surface-state path)
  (cond
    [(file-exists? path) (cons (file->bytes path) (file-or-directory-permissions path 'bits))]
    [else #f]))

(define (managed-surface-path root name)
  (build-path root (string->path name)))

(define (managed-surface-snapshot root)
  (for/list ([name (in-list repo-surface-files)])
    (cons name (managed-surface-state (managed-surface-path root name)))))

(define (managed-surface-violations snapshot root)
  (for/list ([entry (in-list snapshot)]
             #:unless (equal? (cdr entry)
                              (managed-surface-state (managed-surface-path root (car entry)))))
    (car entry)))

;; ============================================================
;; Execution
;; ============================================================

(struct full-run (files results surface-violations) #:transparent)

;; One trustworthy outcome per selected file, and nothing else, in the
;; selected identity set. Any shortfall, extra result, or duplicate invalidates
;; the run. Surface violations are separate: they invalidate the COMMAND, never
;; the per-file evidence.
(define (full-run-valid? run)
  (and (full-run? run)
       (pair? (full-run-files run))
       (equal? (length (full-run-files run)) (length (full-run-results run)))
       (equal? (sort (map quick-result-path (full-run-results run)) string<?)
               (sort (full-run-files run) string<?))
       (andmap quick-result-valid? (full-run-results run))))

;; Raise (never return a partial pass) when selection is empty, when the
;; runner loses results/workers, or when an argument is out of contract.
;;
;; One execution path for every lane and every `--changed` selection: ONE
;; shared work-conserving queue (scripts/testing/scheduler.rkt) schedules
;; every file — mutation owners take repository-exclusive and dispatch first,
;; declared temp-dir/sandboxed-home isolation co-runs, the rest joins after
;; the mutation set drains — while per-file timeouts, managed-surface
;; detection and exact result multiplicity live here exactly once, so an
;; evidence rule cannot drift between commands.
(define (run-files base files jobs #:timeout [timeout full-default-timeout])
  (unless (and (exact-integer? jobs) (positive? jobs))
    (error 'run-full "jobs must be a positive integer, got ~s" jobs))
  (unless (and (exact-positive-integer? timeout) (positive? timeout))
    (error 'run-full "timeout must be a positive integer, got ~s" timeout))
  (when (null? files)
    (error 'run-files "refusing to run an empty selection (zero files is a failure, not a pass)"))
  (define bounds (full-timeouts files #:timeout timeout))
  (define snapshot (managed-surface-snapshot base))
  ;; Every audited owner runs in its own child (the quick adapter); the
  ;; repository-exclusive resource and the co-run rule come from classify.rkt's
  ;; resource classes — the same metadata the runner reads. Mutation owners
  ;; sort first (priority 0), so the lock is acquired once at the start of the
  ;; run instead of stalling the queue repeatedly mid-run.
  (define results
    (run-scheduled (for/list ([f (in-list files)]
                              [i (in-naturals)])
                     (sched-task f
                                 (lambda (path file-timeout) (run-quick-file path file-timeout))
                                 (hash-ref bounds f timeout)
                                 (repository-exclusive-file? f)
                                 (co-runs-under-exclusive-file? f)
                                 (if (repository-exclusive-file? f) 0 1)
                                 i))
                   jobs))
  (full-run files results (managed-surface-violations snapshot base)))

(define (run-full root jobs #:timeout [timeout full-default-timeout])
  (run-files (selection-root root) (full-selection root) jobs #:timeout timeout))

;; Execute a resolved plan: exactly the files `--explain` described, under the
;; same root (fixture roots included), with the same evidence rules.
(define (run-plan plan jobs #:timeout [timeout full-default-timeout])
  (run-files (lane-plan-root plan) (lane-plan-files plan) jobs #:timeout timeout))

;; ============================================================
;; Lane verdict (PURE: no ports, no output, no execution)
;; ============================================================

;; The single decision a lane run yields, computed from the evidence alone: a
;; closed/failing stdout can never reach it, because the CLI computes this
;; BEFORE printing and only maps the symbol to a message and exit code AFTER
;; printing. The release lane is the only lane with blocking obligations, and
;; an unverified obligation is a failure, never a pass.
;;
;;   'invalid    — the run produced missing or invalid per-file evidence
;;   'surface    — a managed repository surface changed during the run
;;   'unverified — release lane with at least one unverified obligation
;;   'ok         — the lane's evidence supports its pass line
;;
;; An obligation list on a non-release lane is a caller error; it fails closed
;; (`'invalid`) instead of being silently ignored, so a green is never produced
;; from an inconsistent input.
(define (lane-verdict lane valid? violations blocking)
  (cond
    [(not valid?) 'invalid]
    [(pair? violations) 'surface]
    [(and (eq? lane 'release) (pair? blocking)) 'unverified]
    [(pair? blocking) 'invalid] ; only the release lane may carry obligations
    [else 'ok]))

;; Coarse, honest classification of why a result is not trustworthy.
(define (full-result-reason result)
  (define reason (quick-result-reason result))
  (cond
    [(memq reason '(timeout truncated identity no-evidence exit)) reason]
    [(and (exact-nonnegative-integer? (quick-result-failed result))
          (positive? (quick-result-failed result)))
     'assertion]
    [else 'unknown]))

;; Concise per-file failure blocks with the reason and the exact rerun command.
(define (full-failure-lines run)
  (for/list ([result (in-list (full-run-results run))]
             #:unless (quick-result-valid? result))
    (format
     "FAILED: ~a (reason: ~a, exit ~a, ~a failures, ~a tests)\n~a\n~aReproduce (whole-suite rerun): ./qt full"
     (quick-result-path result)
     (full-result-reason result)
     (quick-result-exit result)
     (quick-result-failed result)
     (quick-result-total result)
     (quick-result-stdout result)
     (quick-result-stderr result))))

(define (full-surface-violation-lines run)
  (for/list ([name (in-list (full-run-surface-violations run))])
    (format
     "SURFACE MODIFIED: ~a (managed repository surface changed by the suite; detected, not restored)"
     name)))

;; The single honest pass line, and the pinned pattern for it: the honest
;; label is part of the contract, so it cannot be dropped from the output.
(define full-pass-pattern
  #rx"^full passed: [0-9]+ files, [0-9]+ tests \\(full Racket suite only, not PR/CI verification\\)$")

(define (full-pass-line run)
  (format "full passed: ~a files, ~a tests (~a)"
          (length (full-run-files run))
          (apply + (map quick-result-total (full-run-results run)))
          full-label))

;; ============================================================
;; CI surface (`./qt full --json-out/--suite/--shard/--file/...`)
;; ============================================================
;;
;; Phase D prerequisite: `./qt full` can express the invocations the CI jobs
;; use, so CI can eventually call qt instead of scripts/run-tests.rkt. Every
;; flag is FORWARDED to the existing runner internals, never reimplemented:
;;   - selection goes through classify.rkt's own suite predicates
;;     (`collect-test-files`) and its `shard-files` semantics;
;;   - execution goes through runner.rkt's `run-suite-once` (mode, profile
;;     skips, mutation serialization, managed-surface detection, strict
;;     zero-test detection);
;;   - evidence goes through reporting.rkt's `write-json-results!` (the exact
;;     runner schema) and is validated with
;;     `validate-run-summary-json` before the command is allowed to pass.
;; The runner modules are loaded ON DEMAND: plain `./qt full --explain`,
;; `./qt default`, `./qt release` and `./qt quick` keep their current
;; dependency-free plan path (no `json`, no runner state).
;;
;; Explicit reproduction: `./qt full --file FILE...` is the ONLY spelling for
;; a reproduction run. It is labelled an explicit reproduction (never a PR
;; pass, never full coverage) in the pass line and marked in the JSON
;; (`extra.qt.selection_mode` = "explicit-reproduction", `.reproduction` =
;; true). Positional file arguments are rejected on purpose: they would let a
;; reproduction run look like a full-suite pass. Because a reproduction is not
;; a coverage claim it may not be sharded, mixed with `--suite`, or record
;; gate evidence.
;;
;; Fail closed: an unsupported value, an unsupported flag and a combination
;; that cannot be honest raise an error naming the flag. Nothing is ignored
;; silently. `--ledger` must name an existing file: a missing path is a typo,
;; not evidence that no known failures exist.

(define-runtime-path rt-cli "../run-tests/runner.rkt")
(define-runtime-path rt-runner "../run-tests/runner.rkt")
(define-runtime-path rt-reporting "../run-tests/reporting.rkt")
;; Aliases kept so the lazy-load call sites below name their original owner;
;; the bindings now live in the surviving modules (Phase C fold, §11.3).
(define-runtime-path rt-ledger "../run-tests/reporting.rkt")
(define-runtime-path rt-gate-evidence "../run-tests/reporting.rkt")
(define-runtime-path rt-inventory "../run-tests/inventory.rkt")
(define-runtime-path rt-shard-plan "../run-tests/shard-plan.rkt")
(define-runtime-path rt-profiles "../run-tests/classify.rkt")
(define-runtime-path rt-parse "../run-tests/reporting.rkt")

;; One cache for lazily-loaded runner bindings, so the 1500-file full suite
;; does not re-resolve an accessor per result.
(define ci-binding-cache (make-hash))

(define (ci-load module label name)
  (hash-ref!
   ci-binding-cache
   (list module name)
   (lambda ()
     (with-handlers ([exn:fail? (lambda (e)
                                  (error 'full-plan
                                         "CI surface unavailable: cannot load ~a (~a); fail closed"
                                         label
                                         (exn-message e)))])
       (dynamic-require module name)))))

;; STRICT_TEST_RUNNER=1 is the CI setting that turns the runner's strict
;; zero-parsed-test detection on. qt additionally refuses ANY run that executed
;; zero tests (see `run-qt-ci-tests`), which is stricter than the runner default
;; and never a false green.
(define (qt-strict-test-runner?)
  (equal? (getenv "STRICT_TEST_RUNNER") "1"))

;; Mirror of runner.rkt's inline `--shard-plan` validation. Only the
;; deterministic `active` plan survives (§11.3); `report`/`measure` are gone.
(define (qt-shard-plan-modes)
  '("active"))

;; ============================================================
;; Spec (raw CLI values) and validation
;; ============================================================

;; Raw values from the CLI. Selection is resolved from `suite`/`files` (both #f
;; means the complete available suite, exactly like plain `./qt full`).
(struct qt-ci-spec
        (suite files
               shard-index
               shard-total
               timeout
               profile
               ledger-path
               json-out
               record-gate?
               repeat
               mode
               shard-plan
               durations
               argv)
  #:transparent)

(define (qt-known-suites)
  (ci-load rt-cli "scripts/run-tests/runner.rkt" 'known-suites))

(define (qt-known-list module label name)
  (ci-load module label name))

(define (qt-symbol-choices module label name)
  (map symbol->string (qt-known-list module label name)))

;; `contract` names the CLI whose raw values these are, so ONE resolution
;; implementation serves both front-ends:
;;   - 'qt     — the `./qt full` CI surface: fail closed on combinations a mode
;;               cannot honour, and on a `--ledger` path that does not exist
;;               (a typo is not evidence that no known failures exist);
;;   - 'compat — the long-standing `scripts/run-tests.rkt` CLI (the Phase D
;;               compatibility adapter). That CLI has always ACCEPTED these
;;               combinations with defined behavior (a missing ledger file is
;;               an empty ledger, `--file` may be sharded and may record gate
;;               evidence, plan-only modes ignore execution flags), so leaving
;;               them accepted is what keeps it byte-identical while sharing
;;               this module's selection.
(define (validate-qt-ci-spec! spec #:contract [contract 'qt])
  (define qt-contract? (eq? contract 'qt))
  (define suite (qt-ci-spec-suite spec))
  (define files (qt-ci-spec-files spec))
  (define index (qt-ci-spec-shard-index spec))
  (define total (qt-ci-spec-shard-total spec))
  (define repeat (qt-ci-spec-repeat spec))
  (define timeout (qt-ci-spec-timeout spec))
  (define profile (qt-ci-spec-profile spec))
  (define mode (qt-ci-spec-mode spec))
  (define shard-plan (qt-ci-spec-shard-plan spec))
  (define json-out (qt-ci-spec-json-out spec))
  (define ledger-path (qt-ci-spec-ledger-path spec))
  (define record-gate? (qt-ci-spec-record-gate? spec))
  (when suite
    (unless (memq suite (qt-known-suites))
      (error 'full-plan
             "unsupported --suite: ~a (valid: ~a)"
             suite
             (string-join (qt-symbol-choices rt-cli "scripts/run-tests/runner.rkt" 'known-suites)
                          ", "))))
  (unless (and (exact-integer? total) (positive? total))
    (error 'full-plan "--shard-total must be a positive integer, got ~s" total))
  (unless (and (exact-integer? index) (>= index 0) (< index total))
    (error 'full-plan "--shard-index must be an integer in [0, ~a), got ~s" total index))
  (unless (and (exact-integer? repeat) (positive? repeat))
    (error 'full-plan "--repeat must be a positive integer, got ~s" repeat))
  (when timeout
    (unless (and (real? timeout) (positive? timeout))
      (error 'full-plan "--timeout must be a positive number of seconds, got ~s" timeout)))
  (unless (memq profile (qt-known-list rt-profiles "scripts/run-tests/classify.rkt" 'known-profiles))
    (error 'full-plan
           "unsupported --profile: ~a (valid: ~a)"
           profile
           (string-join
            (qt-symbol-choices rt-profiles "scripts/run-tests/classify.rkt" 'known-profiles)
            ", ")))
  (unless (memq mode (qt-known-list rt-cli "scripts/run-tests/runner.rkt" 'known-modes))
    (error 'full-plan
           "unsupported --mode: ~a (valid: ~a)"
           mode
           (string-join (qt-symbol-choices rt-cli "scripts/run-tests/runner.rkt" 'known-modes) ", ")))
  (when (and ledger-path qt-contract?)
    (unless (file-exists? (path->complete-path (string->path ledger-path)))
      (error 'full-plan "--ledger file does not exist: ~a" ledger-path)))
  (when shard-plan
    (unless (member shard-plan (qt-shard-plan-modes))
      (error 'full-plan
             "unsupported --shard-plan: ~a (valid: ~a)"
             shard-plan
             (string-join (qt-shard-plan-modes) ", "))))
  (when (and (pair? files) suite)
    (error
     'full-plan
     "cannot combine --suite with --file: --file is explicit reproduction, --suite is a suite selection"))
  (when (and (pair? files) (> total 1) qt-contract?)
    (error
     'full-plan
     "--file (explicit reproduction) cannot be sharded (--shard-total ~a); reproduction is never a coverage claim"
     total))
  (when (and (pair? files) record-gate? qt-contract?)
    (error
     'full-plan
     "--file (explicit reproduction) cannot --record-gate-evidence (gate evidence claims a suite PASS)"))
  (void))

;; ============================================================
;; Selection (classify.rkt predicates + shard-files semantics)
;; ============================================================

;; Root-relative selection for one of the runner's OWN suite names, through
;; `collect-test-files` (no second registry, no hand-maintained list). Sorted
;; so shard assignment and the printed plan are deterministic.
(define (qt-suite-relative suite root)
  (sort (if root
            (collect-test-files suite #:root (selection-root root))
            (collect-test-files suite))
        string<?))

(define (qt-suite-files suite [root #f])
  (define base (selection-root root))
  (for/list ([rel (in-list (qt-suite-relative suite root))])
    (path->string (simplify-path (root-relative-path base rel) #f))))

;; One `--file` argument as a root-relative identity string. Fail closed when
;; the file is missing, not a `.rkt` file, or outside the selection root.
(define (qt-reproduction-file f base)
  (define raw
    (if (path? f)
        (path->string f)
        f))
  (define abs (simplify-path (path->complete-path (string->path raw) base) #f))
  (unless (file-exists? abs)
    (error 'full-plan "--file is not an existing test file: ~a" raw))
  (unless (string-suffix? (path->string abs) ".rkt")
    (error 'full-plan "--file must name a .rkt test file: ~a" raw))
  (define rel (find-relative-path base abs))
  (when (absolute-path? rel)
    (error 'full-plan "--file must live under the selection root ~a: ~a" (path->string base) raw))
  (path->string rel))

(define (qt-reproduction-files files root)
  (define base (selection-root root))
  (define rels
    (for/list ([f (in-list files)])
      (qt-reproduction-file f base)))
  (define dup (check-duplicates rels))
  (when dup
    (error 'full-plan "--file listed more than once: ~a" dup))
  rels)

;; Exactly classify.rkt's round-robin shard semantics, with qt's fail-closed
;; argument checks. A shard that selects no files is refused by the caller
;; (`run-qt-ci-tests`), matching the runner's own zero-file refusal.
(define (qt-shard-partition files index total)
  (unless (and (exact-integer? total) (positive? total))
    (error 'full-plan "--shard-total must be a positive integer, got ~s" total))
  (unless (and (exact-integer? index) (>= index 0) (< index total))
    (error 'full-plan "--shard-index must be an integer in [0, ~a), got ~s" total index))
  (shard-files files index total))

;; ============================================================
;; Resolved CI plan (computed once; shared by --explain and execution)
;; ============================================================

;; `mode`: 'full-suite | 'explicit-suite | 'explicit-reproduction. `rels` is
;; the UNsharded selection in the runner's own path form (root-relative strings
;; from classify.rkt), so duration snapshots and gate evidence keep matching.
(struct qt-ci-plan (root mode suite rels shard-index shard-total shard-plan spec) #:transparent)

(define (resolve-qt-ci-plan root spec #:contract [contract 'qt])
  (validate-qt-ci-spec! spec #:contract contract)
  (define base (selection-root root))
  (define suite (qt-ci-spec-suite spec))
  (define raw-files (qt-ci-spec-files spec))
  (define mode
    (cond
      [(pair? raw-files) 'explicit-reproduction]
      [suite 'explicit-suite]
      [else 'full-suite]))
  (define rels
    (case mode
      ;; The compat CLI has always passed positional/normalized reproduction
      ;; paths straight to the runner (absolute paths stay absolute, no
      ;; existence/.rkt validation, no de-duplication), so its results keep the
      ;; exact `path` identity they had before this adapter existed.
      [(explicit-reproduction)
       (if (eq? contract 'compat)
           (map normalize-test-path raw-files)
           (qt-reproduction-files raw-files root))]
      [(explicit-suite) (qt-suite-relative suite root)]
      [else (lane-relative 'full root)]))
  ;; qt fails closed on an empty selection. The compat CLI keeps its own
  ;; long-standing `No test files matched the selected suite.` (exit 1), so it
  ;; never reaches the execution layer with an empty selection either.
  (when (and (eq? contract 'qt) (null? rels))
    (error 'full-plan
           "CI selection is empty under ~a (zero files is a failure, not a pass)"
           (path->string base)))
  (qt-ci-plan base
              mode
              suite
              rels
              (qt-ci-spec-shard-index spec)
              (qt-ci-spec-shard-total spec)
              (qt-ci-spec-shard-plan spec)
              spec))

;; Duration-aware shard plan over the resolved selection, using shard-plan.rkt's
;; own planner and duration loader (`--shard-plan active`).
(define (qt-ci-build-shard-plan plan total)
  (define spec (qt-ci-plan-spec plan))
  (define durations (qt-ci-spec-durations spec))
  (define profile (qt-ci-spec-profile spec))
  (define load-dur (ci-load rt-shard-plan "scripts/run-tests/shard-plan.rkt" 'load-duration-snapshot))
  (define build (ci-load rt-shard-plan "scripts/run-tests/shard-plan.rkt" 'build-shard-plan/safe))
  (define skips (ci-load rt-profiles "scripts/run-tests/classify.rkt" 'profile-skips-test?))
  (define-values (dur dur-status) (load-dur durations))
  (when (qt-ci-presentation)
    (printf ";; qt: shard-plan durations=~a status=~a known=~a~n"
            (or durations "<none>")
            dur-status
            (hash-count dur)))
  (build (qt-ci-plan-rels plan)
         total
         #:durations dur
         #:profile-skips? (lambda (f) (skips profile (hash-ref (get-file-metadata f) 'requires '())))
         #:duration-source durations))

;; The exact files this plan would execute. `active` consumes the duration-aware
;; plan; every other configuration uses classify.rkt's round-robin `shard-files`.
(define (qt-ci-run-files plan)
  (define rels (qt-ci-plan-rels plan))
  (define total (qt-ci-plan-shard-total plan))
  (define index (qt-ci-plan-shard-index plan))
  (cond
    [(equal? (qt-ci-plan-shard-plan plan) "active")
     (if (> total 1)
         ((ci-load rt-shard-plan "scripts/run-tests/shard-plan.rkt" 'plan-shard-files)
          (qt-ci-build-shard-plan plan total)
          index)
         rels)]
    [(> total 1) (qt-shard-partition rels index total)]
    [else rels]))

(define (qt-ci-suite-label plan)
  (case (qt-ci-plan-mode plan)
    [(explicit-suite) (symbol->string (qt-ci-plan-suite plan))]
    [(explicit-reproduction) "reproduction"]
    [else "full"]))

(define (qt-ci-selection-noun plan)
  (case (qt-ci-plan-mode plan)
    [(explicit-suite) (format "~a suite selection" (qt-ci-plan-suite plan))]
    [(explicit-reproduction) "explicit reproduction selection"]
    [else "complete available Racket suite"]))

;; The honest label embedded in the pass line; the reproduction label is part
;; of the contract (a reproduction is never a PR pass).
(define (qt-ci-label plan)
  (case (qt-ci-plan-mode plan)
    [(explicit-suite) (format "~a suite only, not PR/CI verification" (qt-ci-plan-suite plan))]
    [(explicit-reproduction) "explicit reproduction only, not a PR pass"]
    [else full-label]))

(define (qt-ci-coverage-line plan)
  (define total (qt-ci-plan-shard-total plan))
  (define index (qt-ci-plan-shard-index plan))
  (cond
    [(> total 1) (format "shard ~a of ~a; partial coverage (never a full-suite claim)" index total)]
    [(eq? (qt-ci-plan-mode plan) 'explicit-reproduction)
     (format "explicit --file selection (~a files)" (length (qt-ci-plan-rels plan)))]
    [(eq? (qt-ci-plan-mode plan) 'explicit-suite) "explicit --suite selection"]
    [else "complete available Racket suite"]))

;; ============================================================
;; JSON marker (schema-additive; validate-run-summary-json tolerates it)
;; ============================================================

(define (qt-ci-marker plan run-files)
  (define mode (qt-ci-plan-mode plan))
  (define total (qt-ci-plan-shard-total plan))
  (define index (qt-ci-plan-shard-index plan))
  (hasheq 'command
          "qt full"
          'selection_mode
          (symbol->string mode)
          'reproduction
          (eq? mode 'explicit-reproduction)
          'full_coverage
          (and (eq? mode 'full-suite) (= total 1))
          'suite
          (if (qt-ci-plan-suite plan)
              (symbol->string (qt-ci-plan-suite plan))
              "")
          'shard_index
          index
          'shard_total
          total
          'shard_plan
          (or (qt-ci-plan-shard-plan plan) "none")
          'selection_file_count
          (length (qt-ci-plan-rels plan))
          'run_file_count
          (length run-files)
          ;; Explicit selections carry their identity in the marker; the full
          ;; suite already has every path in the top-level `files` array.
          'files
          (if (memq mode '(explicit-reproduction explicit-suite))
              (qt-ci-plan-rels plan)
              '())))

(define (qt-ci-read-json path)
  (define read-json (ci-load 'json "json" 'read-json))
  (with-handlers
      ([exn:fail?
        (lambda (e)
          (error 'full-plan "--json-out document at ~a is unreadable (~a)" path (exn-message e)))])
    (call-with-input-file path read-json)))

;; Merge the qt selection marker into the runner-written document and write it
;; back. The document itself is written (and its schema owned) by
;; reporting.rkt's `write-json-results!`; this only adds the schema-additive
;; `extra.qt` object.
(define (merge-qt-ci-marker! path plan run-files)
  (define write-json (ci-load 'json "json" 'write-json))
  (define doc (qt-ci-read-json path))
  (unless (hash? doc)
    (error 'full-plan "--json-out document at ~a is not a run-summary object" path))
  (define extra (hash-ref doc 'extra #f))
  (define extra*
    (if (hash? extra)
        (hash-copy extra)
        (make-hasheq)))
  (hash-set! extra* 'qt (qt-ci-marker plan run-files))
  (define doc* (hash-set doc 'extra extra*))
  (call-with-output-file path
                         #:exists 'truncate/replace
                         (lambda (port)
                           (write-json doc* port)
                           (newline port))))

(define (validate-qt-json-out! path)
  (define validate
    (ci-load rt-reporting "scripts/run-tests/reporting.rkt" 'validate-run-summary-json))
  (define doc (qt-ci-read-json path))
  (define problems (validate doc))
  (unless (null? problems)
    (error 'full-plan
           "--json-out document at ~a failed validate-run-summary-json: ~a"
           path
           (string-join problems "; ")))
  doc)

(define (qt-result-total r)
  ((ci-load rt-parse "scripts/run-tests/reporting.rkt" 'test-file-result-total) r))

;; The runner's own `effective-mode` mapping: `auto` and `subprocess` are the
;; only surviving modes and both execute one subprocess per test file, so
;; run-suite-once always receives an effective mode.
(define (qt-effective-mode requested suite-label)
  (if (eq? requested 'auto) 'subprocess requested))

(struct qt-ci-outcome (ran? exit-code results) #:transparent)

(define (qt-record-gate-evidence! plan run-files results jobs suite-label)
  (define spec (qt-ci-plan-spec plan))
  (define index (qt-ci-plan-shard-index plan))
  (define total (qt-ci-plan-shard-total plan))
  (define refusal (ci-load rt-gate-evidence "scripts/run-tests/reporting.rkt" 'gate-evidence-refusal))
  (define reason (refusal index total))
  (when reason
    (error 'full-plan "cannot record gate evidence: ~a" reason))
  (define record
    (ci-load rt-gate-evidence "scripts/run-tests/gate-evidence.rkt" 'record-gate-evidence!))
  (define inv-hash (ci-load rt-inventory "scripts/run-tests/inventory.rkt" 'compute-inventory-hash))
  (record suite-label
          #:results results
          #:args (qt-ci-spec-argv spec)
          #:jobs jobs
          #:timeout (qt-ci-spec-timeout spec)
          #:repeat (qt-ci-spec-repeat spec)
          #:file-count (length run-files)
          #:inventory-hash (inv-hash run-files)
          #:shard-index index
          #:shard-total total))

;; ============================================================
;; Shared execution options — ONE execution path, two CLI contracts
;; ============================================================
;; The engine below is the single implementation of execution, repeat handling,
;; JSON evidence and gate-evidence recording. It has exactly two callers:
;;   - `./qt full` (`qt-ci-default-options`) — the qt contract: the qt
;;     presentation (`;; qt:` chatty lines plus the schema-additive `extra.qt`
;;     JSON marker) and qt's fail-closed refusals (zero executed tests, and a
;;     profile that skips every selected file);
;;   - `scripts/run-tests.rkt` (`qt-ci-compat-options`) — the long-standing
;;     runner CLI, whose presentation, strict zero-parsed-test default and
;;     zero-test behavior predate the CI surface and must stay identical: the
;;     document is written by reporting.rkt alone (no `extra.qt`), the strict
;;     flag comes from the caller, and a zero-test run is reported by the
;;     runner itself instead of being refused here.
;; `suite-label` (a string) overrides the label derived from the plan, because
;; the compat CLI keeps its own suite naming ("all" for an explicit file list,
;; "impact" for a change-impact run).
(struct qt-ci-exec-options
        (suite-label strict? refuse-zero-tests? refuse-all-skipped? phases on-run-start)
  #:transparent)

;; Presentation switch for the SHARED helpers (`qt-ci-build-shard-plan`,
;; `qt-ci-run-files`) and for the engine's JSON marker: `#t` is the qt contract
;; (`;; qt:` chatty lines plus the schema-additive `extra.qt` marker). The compat
;; adapter binds it to `#f` for its whole run, so its stdout stays the
;; runner's own and the document stays exactly what reporting.rkt wrote.
(define qt-ci-presentation (make-parameter #t))

(define (qt-ci-default-options)
  (qt-ci-exec-options #f (qt-strict-test-runner?) #t #t (hasheq) void))

(define (qt-ci-compat-options suite-label strict? phases on-run-start)
  (qt-ci-exec-options suite-label strict? #f #f phases on-run-start))

(define (run-qt-ci-tests plan jobs #:options [options (qt-ci-default-options)])
  (define spec (qt-ci-plan-spec plan))
  (define total (qt-ci-plan-shard-total plan))
  (define files (qt-ci-run-files plan))
  (when (null? files)
    (error
     'full-plan
     "the selected CI run covers zero files; a shard that executes nothing is a failure, not a pass"))
  ;; Refuse an impossible gate-evidence recording BEFORE running, exactly like
  ;; the runner's own main (a shard must never emit/overwrite a full PASS).
  (when (qt-ci-spec-record-gate? spec)
    (define refusal
      (ci-load rt-gate-evidence "scripts/run-tests/gate-evidence.rkt" 'gate-evidence-refusal))
    (define reason (refusal (qt-ci-plan-shard-index plan) total))
    (when reason
      (error 'full-plan "cannot record gate evidence: ~a" reason)))
  ;; A profile that skips EVERY selected file would make the runner print its
  ;; own PASS verdict for a run that executed zero tests. Fail before starting
  ;; so that log can never look green (the post-run zero-test check stays as a
  ;; second line of defense).
  (define profile (qt-ci-spec-profile spec))
  (define skip? (ci-load rt-profiles "scripts/run-tests/classify.rkt" 'profile-skips-test?))
  (define skipped
    (filter (lambda (f) (skip? profile (hash-ref (get-file-metadata f) 'requires '()))) files))
  (when (and (qt-ci-exec-options-refuse-all-skipped? options) (= (length skipped) (length files)))
    (error 'full-plan
           (string-append "zero tests executed: all ~a selected file(s) are skipped by --profile ~a"
                          "~a; a pass with zero tests is never reported")
           (length files)
           profile
           (if (qt-ci-exec-options-strict? options) " [STRICT_TEST_RUNNER=1]" "")))
  (define requested (qt-ci-spec-mode spec))
  (define suite-label (or (qt-ci-exec-options-suite-label options) (qt-ci-suite-label plan)))
  (define mode (qt-effective-mode requested suite-label))
  (define ledger-path (qt-ci-spec-ledger-path spec))
  (define ledger
    (and ledger-path
         ((ci-load rt-ledger "scripts/run-tests/reporting.rkt" 'load-known-failure-ledger)
          ledger-path)))
  (define timeout-ms (and (qt-ci-spec-timeout spec) (* (qt-ci-spec-timeout spec) 1000)))
  (define repeat (qt-ci-spec-repeat spec))
  (define json-out (qt-ci-spec-json-out spec))
  (define shard (and (> total 1) (cons (qt-ci-plan-shard-index plan) total)))
  (define run-suite-once (ci-load rt-runner "scripts/run-tests/runner.rkt" 'run-suite-once))
  (when (and (qt-ci-presentation) (equal? (qt-ci-plan-shard-plan plan) "active") (= total 1))
    (printf ";; qt: --shard-plan active ignored — --shard-total=1 (unsharded)~n"))
  ;; STRICT_TEST_RUNNER=1 enables the runner's strict zero-parsed-test check
  ;; (the CI setting, and the qt contract's default). qt additionally refuses a
  ;; run that executed zero tests at all; the compat CLI leaves that to the
  ;; runner's own strict handling.
  (define strict? (qt-ci-exec-options-strict? options))
  (define last-results (box '()))
  (define exit-code (box 0))
  (for ([run-num (in-range 1 (add1 repeat))]
        #:break (not (zero? (unbox exit-code))))
    ;; Presentation hook: the compat CLI prints its `── Run n/N ──` header
    ;; between repeats; qt prints nothing here.
    ((qt-ci-exec-options-on-run-start options) run-num repeat)
    (define-values (code results)
      (run-suite-once files
                      jobs
                      timeout-ms
                      strict?
                      suite-label
                      run-num
                      repeat
                      mode
                      json-out
                      ledger
                      (qt-ci-spec-profile spec)
                      #:shard shard
                      #:phases (qt-ci-exec-options-phases options)))
    (set-box! last-results results)
    (set-box! exit-code code))
  (define results (unbox last-results))
  ;; Evidence first (the runner already wrote the document): under the qt
  ;; presentation add the selection marker, then validate the exact runner
  ;; schema. The compat CLI's document stays exactly what reporting.rkt wrote.
  (when json-out
    (when (qt-ci-presentation)
      (merge-qt-ci-marker! json-out plan files))
    (validate-qt-json-out! json-out))
  (define total-tests (for/sum ([r (in-list results)]) (qt-result-total r)))
  (when (and (qt-ci-exec-options-refuse-zero-tests? options) (zero? total-tests))
    (error 'full-plan
           (string-append
            "zero tests executed (~a selected files); a pass with zero tests is never reported"
            "~a")
           (length files)
           (if strict? " [STRICT_TEST_RUNNER=1]" "")))
  (when (and (zero? (unbox exit-code)) (qt-ci-spec-record-gate? spec))
    ;; The recorded label is the label the run REPORTED (a compat run keeps its
    ;; own suite naming), never a plan-derived substitute.
    (qt-record-gate-evidence! plan files results jobs suite-label))
  (qt-ci-outcome #t (unbox exit-code) results))

;; One entry point: every surviving mode executes through the runner's
;; `run-suite-once` (the shard-plan report/measure sub-modes are deleted).
(define (run-qt-ci-plan plan jobs #:options [options (qt-ci-default-options)])
  (run-qt-ci-tests plan jobs #:options options))

;; ============================================================
;; Explain (nothing executed) and the honest pass line
;; ============================================================

;; CI-surface plan: the same honesty lists as plain `./qt full`, plus the
;; selection identity (suite / shard / explicit reproduction), the explicit
;; reproduction spelling, and the shard-plan mode. Plain `./qt full --explain`
;; never reaches this function, so its output stays byte-identical.
;; ============================================================
;; CI lanes (`./qt --lane NAME`)
;; ============================================================
;; One table is the policy metadata CI reads instead of hard-coding suite,
;; profile and timeout in YAML. A lane names EXISTING runner suites (never a
;; second selection scheme) plus the shard-plan/timeout policy the engine
;; already implements. `shard-total` is a lane default: a workflow matrix that
;; enumerates shards still owns the index/total it instantiates and may pass
;; them explicitly. Unknown lanes fail closed and list the valid names.
;;
;; Suite set (final plan §6 E1): a lane carries a LIST of suites. Every
;; pre-E1 entry keeps exactly one suite, and `qt-lane-suite` stays as the
;; single-suite compatibility accessor — it now returns the suite set, a
;; ONE-ELEMENT list for every legacy lane. The CI surface still runs one
;; suite per job; a multi-suite lane (only `required-offline` today) resolves
;; through `./qt default` and is refused on the CI surface.
(struct qt-lane (name suites timeout shard-total shard-plan description) #:transparent)

;; Single-suite compatibility accessor: every legacy caller reads the suite
;; set through it; for each pre-E1 lane that is a one-element list.
(define (qt-lane-suite lane)
  (qt-lane-suites lane))

;; The required CI test producers and the qt-lane each one selects (final plan
;; §6 E1). Contexts are the exact names in scripts/required-pr-checks.policy;
;; the pinning test (tests/test-qt-lanes.rkt) asserts that this table tracks
;; the policy file and that the `required-offline` lane's suite list is exactly
;; the union of these lanes' suites, so drift fails in CI — without parsing
;; YAML (the mapping lives in the lane table, not in a parsed workflow).
(struct required-producer (contexts lane) #:transparent)

(define required-ci-producers
  (list
   (required-producer (list "test (0)" "test (1)" "test (2)") 'fast)
   (required-producer (list "security") 'security)
   (required-producer (list "workflows (0)" "workflows (1)") 'workflows)
   (required-producer (list "smoke (ubuntu-latest)") 'smoke)
   (required-producer (list "test-platform") 'platform)))

;; The required-offline suite list, in the final plan §6's own order. PINNED
;; against `required-ci-producers` (the producers' lanes' suites) — change it
;; only in the same commit that changes the required CI test jobs.
(define required-offline-suites '(fast security workflows smoke platform))

(define qt-lanes
  (list
   (qt-lane 'fast (list 'fast) #f 1 #f "fast PR regression tier")
   (qt-lane 'fast-shard (list 'fast) #f 1 "active" "duration-aware fast tier (requires --shard-total > 1)")
   (qt-lane 'platform (list 'platform) #f 1 #f "curated platform-cross subset")
   (qt-lane 'security (list 'security) #f 1 #f "security suite")
   (qt-lane 'workflows (list 'workflows) 90 2 #f "workflow/contract suite (two shards)")
   (qt-lane 'arch (list 'arch) #f 1 #f "architecture suite")
   (qt-lane 'tui (list 'tui) 60 1 #f "TUI suite")
   (qt-lane 'smoke (list 'smoke) #f 1 #f "smoke suite")
   (qt-lane 'release-smoke (list 'release-smoke) 120 1 #f "release smoke suite")
   (qt-lane 'unit-fast (list 'unit-fast) #f 1 #f "unit-fast suite")
   (qt-lane 'slow (list 'slow) #f 1 #f "slow suite")
   (qt-lane 'all (list 'all) #f 1 #f "complete discovered suite")
   (qt-lane 'required-offline
            required-offline-suites
            #f
            1
            #f
            "offline PR verification: union of the required CI test jobs' suites (resolves through ./qt default)")))

;; Resolve a lane name to its policy entry. An unknown name is an error that
;; NAMES every valid lane (fail closed, never a silent fallback to `fast`).
(define (qt-lane-ref name)
  (define wanted
    (if (symbol? name)
        name
        (string->symbol name)))
  (or (for/first ([lane (in-list qt-lanes)]
                  #:when (eq? (qt-lane-name lane) wanted))
        lane)
      (error 'full-plan
             "unknown --lane: ~a (valid: ~a)"
             wanted
             (string-join (map (lambda (l) (symbol->string (qt-lane-name l))) qt-lanes) ", "))))

;; ============================================================
;; Release candidate verification (`./qt release --candidate SHA`)
;; ============================================================
;; A release entry point that accepts a candidate must verify the EXACT
;; candidate it was handed: a different HEAD, or a checkout with uncommitted
;; changes, is refused before anything runs (release obligations stay
;; unconditional). `sha` must be a commit id (hex), never a moving ref.
(define (release-candidate-check root sha)
  (define text
    (and sha
         (if (path? sha)
             (path->string sha)
             sha)))
  (unless (and (string? text) (regexp-match? #px"^[0-9a-f]{7,40}$" text))
    (error 'full-plan
           "--candidate must be a commit SHA (7-40 lowercase hex characters), got: ~a"
           (or text "<missing>")))
  (unless (find-executable-path "git")
    (error 'full-plan
           "cannot verify --candidate ~a under ~a: git is not available (fail closed)"
           text
           (path->string root)))
  (define-values (head-ok? head-text) (git-run root (list "rev-parse" "--verify" "--quiet" "HEAD")))
  (unless head-ok?
    (error 'full-plan
           "cannot verify --candidate ~a under ~a: HEAD does not resolve (fail closed)"
           text
           (path->string root)))
  (define head (string-trim head-text))
  (unless (string-prefix? head text)
    (error
     'full-plan
     "--candidate ~a does not match the checkout at ~a (HEAD is ~a); refusing to verify a different tree"
     text
     (path->string root)
     head))
  (define-values (status-ok? status-text)
    (git-run root (list "status" "--porcelain" "--untracked-files=no")))
  (unless status-ok?
    (error 'full-plan
           "cannot verify --candidate ~a under ~a: git status failed (fail closed)"
           text
           (path->string root)))
  (define dirty (git-lines status-text))
  (when (pair? dirty)
    (error
     'full-plan
     "--candidate ~a names a dirty checkout at ~a (~a tracked change(s)); refusing to verify uncommitted bytes"
     text
     (path->string root)
     (length dirty)))
  ;; The verified identity is the resolved commit, never the caller's spelling.
  head)

(define (qt-ci-explain-lines plan #:command [command "./qt full"])
  (define spec (qt-ci-plan-spec plan))
  (define mode (qt-ci-plan-mode plan))
  (define suite (qt-ci-plan-suite plan))
  (define total (qt-ci-plan-shard-total plan))
  (define index (qt-ci-plan-shard-index plan))
  (define shard-plan (qt-ci-plan-shard-plan plan))
  (define files (qt-ci-run-files plan))
  (append
   (list
    (format "~a: ~a under ~a"
            (if (equal? command "./qt full") "full" command)
            (qt-ci-selection-noun plan)
            (path->string (qt-ci-plan-root plan)))
    (format "label: ~a" (qt-ci-label plan))
    (format "selection: ~a files (selection only; nothing executed, nothing cleaned)" (length files))
    (format "coverage: ~a" (qt-ci-coverage-line plan))
    (string-append "reproduction spelling: ./qt full --file FILE... runs an EXPLICIT REPRODUCTION "
                   "only (never a PR pass, never full coverage)"))
   (if suite
       (list (format "suite: ~a (classify.rkt's existing suite predicate; not the complete suite)"
                     suite))
       '())
   (if (> total 1)
       (list (format
              "shard: ~a of ~a (~a semantics)"
              index
              total
              (if (equal? shard-plan "active") "duration-aware plan" "round-robin --shard-files")))
       '())
   (if shard-plan
       (list
        (format "shard-plan: ~a (durations ~a)" shard-plan (or (qt-ci-spec-durations spec) "<none>")))
       '())
   (if (eq? mode 'explicit-reproduction)
       (list (format "reproduction files: ~a (explicit --file selection)"
                     (length (qt-ci-plan-rels plan))))
       '())
   (list (full-timeout-policy-line files)
         (full-mutation-policy-line files)
         (full-execution-support-line command))
   files
   (list "")
   (list (format "NOT VERIFIED by ~a:" command))
   (for/list ([item (in-list full-obligations)])
     (format "  - ~a" item))
   (list "")
   (list (format "NOT PROVIDED by ~a (counted by other gates):" command))
   (for/list ([item (in-list full-not-provided)])
     (format "  - ~a" item))))

(define qt-ci-pass-pattern #rx"^full passed: [0-9]+ files, [0-9]+ tests \\([^)]+\\)$")

(define (qt-ci-pass-line plan results #:command [command "full"])
  (define tests (for/sum ([r (in-list results)]) (qt-result-total r)))
  (format "~a passed: ~a files, ~a tests (~a; ~a)"
          command
          (length results)
          tests
          (qt-ci-label plan)
          (qt-ci-coverage-line plan)))
