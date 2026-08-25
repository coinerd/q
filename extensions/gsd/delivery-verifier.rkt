#lang racket/base

;; extensions/gsd/delivery-verifier.rkt — Structured delivery verification
;; for the /go campaign coordinator.
;;
;; STABILITY: evolving
;;
;; Replaces the hardcoded fail-closed `#f` verifier in prepare-go-campaign.
;; A wave may only cross the durable DONE commit point when REAL delivery
;; evidence exists, not merely because the executor returned a normal
;; response. Evidence checks (all fail closed):
;;   1. git repository reachable from base-dir;
;;   2. current branch matches the wave's expected feature/issue-<N>-wave
;;      (issue number resolved from .planning/STATE.md wave table);
;;   3. at least one wave target file changed vs HEAD (or untracked-new);
;;   4. a bounded verify command exits 0 (compile gate by default).
;;
;; v1.00.17 W7 (#9512b) — branch-based delivery: when the coordinator binds
;; `current-gsd-delivery-branch-context` (worktree isolation ON), checks 2–4
;; evaluate the wave BRANCH instead of any working tree: the changed set is
;; the committed diff of the branch vs the base commit captured at attempt
;; start, the expected branch is the recorded campaign branch, and the
;; verify command runs from the wave worktree. Uncommitted dirt can never
;; satisfy delivery. With the context unbound (#f) the legacy shared-tree
;; behavior above is byte-identical to pre-W7 (characterized in W0).
;;
;; Returns a `delivery-verification` struct whose approved?/message the
;; coordinator surfaces on rejection. This is the only result that may
;; cross the durable DONE commit point.

(require racket/format
         racket/path
         racket/port
         racket/set
         racket/string
         racket/system
         "plan-types.rkt"
         "wave-docs.rkt"
         (only-in "plan-context-builder.rkt" find-git-root-dir))

(provide delivery-verification
         delivery-verification?
         delivery-verification-approved?
         delivery-verification-evidence
         delivery-verification-message
         make-delivery-verifier
         run-delivery-verification
         current-gsd-delivery-verify-command
         current-gsd-delivery-verify-timeout-sec
         current-gsd-delivery-branch-context
         make-branch-delivery-context
         branch-delivery-context?
         branch-delivery-context-ref
         committed-branch-changed-files
         check-git-available
         check-branch-matches
         check-wave-files-changed
         check-verify-command)

;; ============================================================
;; Structured verification result
;; ============================================================

(struct delivery-verification (approved? evidence message) #:transparent)
;; evidence: list of (cons check-name (cons ok? detail))

;; ============================================================
;; Policy knobs
;; ============================================================

;; Optional override for the verify command. When #f, a compile gate is
;; derived from the wave's target files. Used by tests to force failure.
(define current-gsd-delivery-verify-command
  (make-parameter
   #f
   (lambda (v)
     (cond
       [(not v) v]
       [(string? v) v]
       [else (raise-argument-error 'current-gsd-delivery-verify-command "(or/c #f string?)" v)]))))

;; Bounded runtime for the verify command (seconds).
(define current-gsd-delivery-verify-timeout-sec
  (make-parameter
   300
   (lambda (v)
     (if (and (real? v) (positive? v))
         v
         (raise-argument-error 'current-gsd-delivery-verify-timeout-sec "positive-real?" v)))))

;; ============================================================
;; v1.00.17 W7 (#9512b): branch-based delivery context
;; ============================================================

;; When worktree isolation is active, the campaign coordinator binds this
;; parameter around the verifier call. Delivery evidence then comes ONLY
;; from the COMMITTED diff of the wave branch vs the base commit captured
;; at attempt start — never from any working tree. Consequences (the W7
;; root cause being fixed):
;;   * uncommitted mutations in the shared checkout (or in the wave
;;     worktree) do NOT count as delivered;
;;   * a `git checkout .` in the shared checkout can no longer destroy
;;     "done" work — approved delivery lives on the recorded branch;
;;   * wave ownership of changes is exact: the branch diff IS the wave.
;; #f (the default) selects the legacy shared-checkout verification whose
;; behavior is byte-identical to the pre-W7 verifier (characterized in W0
;; and pinned by test-gsd-executor-retry-characterization.rkt).
;;
;; Context keys:
;;   repo-root     — git root holding the campaign branch (string/path)
;;   branch        — wave branch name, "campaign/<hash8>/w<N>"
;;   base-commit   — base commit SHA captured at attempt start
;;   worktree-path — #f or the worktree checkout of the branch; the verify
;;                   command runs with this as cwd so gates exercise the
;;                   delivered (committed) tree
(define (make-branch-delivery-context #:repo-root repo-root
                                      #:branch branch
                                      #:base-commit base-commit
                                      #:worktree-path [worktree-path #f])
  (hasheq 'repo-root repo-root 'branch branch 'base-commit base-commit 'worktree-path worktree-path))

(define (branch-delivery-context? v)
  (and (hash? v) (hash-ref v 'repo-root #f) (hash-ref v 'branch #f) (hash-ref v 'base-commit #f) #t))

(define (branch-delivery-context-ref ctx key)
  (hash-ref ctx key #f))

(define current-gsd-delivery-branch-context
  (make-parameter #f
                  (lambda (v)
                    (cond
                      [(not v) v]
                      [(branch-delivery-context? v) v]
                      [else
                       (raise-argument-error 'current-gsd-delivery-branch-context
                                             "(or/c #f branch-delivery-context?)"
                                             v)]))))

;; Effective git root for git evidence: under an active branch context the
;; campaign branch's repo; otherwise the base-dir checkout.
(define (evidence-git-root base-dir)
  (define ctx (current-gsd-delivery-branch-context))
  (or (and ctx (branch-delivery-context-ref ctx 'repo-root)) (git-root-for base-dir)))

;; Effective cwd for the verify command: under an active branch context the
;; wave worktree (the delivered tree as committed); otherwise the git root.
(define (verify-command-cwd base-dir git-root)
  (define ctx (current-gsd-delivery-branch-context))
  (or (and ctx (branch-delivery-context-ref ctx 'worktree-path)) git-root))

;; The committed delivery diff of the wave branch: names changed between
;; the base commit (captured at attempt start) and the wave branch tip.
;; Three-dot semantics (merge-base...branch) keep the diff exact even if
;; origin/main moved since the attempt started. Fail closed: any git
;; failure — or a missing branch context — yields the empty set, so an
;; unverifiable branch can never satisfy delivery. The result is an
;; equal-based set: membership must compare path STRINGS by value, since
;; git output and find-relative-path produce fresh (never eq?) strings.
(define (committed-branch-changed-files)
  (define ctx (current-gsd-delivery-branch-context))
  (if (not ctx)
      (set)
      (let ()
        (define root (branch-delivery-context-ref ctx 'repo-root))
        (define result
          (run-git* root
                    (list "diff"
                          "--name-only"
                          (format "~a...~a"
                                  (branch-delivery-context-ref ctx 'base-commit)
                                  (branch-delivery-context-ref ctx 'branch)))))
        (define out (and (car result) (eq? (car result) 0) (string-split (cadr result) "\n")))
        (for/set ([p (in-list (or out '()))]
                  #:when (not (string=? (string-trim p) "")))
          (string-trim p)))))

;; ============================================================
;; Git helpers
;; ============================================================

(define (git-root-for base-dir)
  (find-git-root-dir base-dir))

(define (run-git* git-root args)
  ;; returns (list exit-code stdout) — exit-code #f on exception
  (define git (find-executable-path "git"))
  (define stdout (open-output-string))
  (define stderr (open-output-string))
  (define exit-code
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (parameterize ([current-output-port stdout]
                     [current-error-port stderr])
        (if git
            (apply system*/exit-code git "-C" git-root args)
            #f))))
  (list exit-code (get-output-string stdout) (get-output-string stderr)))

(define (git-exit-ok? result)
  (and result (eq? (car result) 0)))

(define (git-stdout result)
  (if result
      (cadr result)
      ""))

(define (git-available? base-dir)
  (define root (git-root-for base-dir))
  (and root
       (directory-exists? root)
       (git-exit-ok? (run-git* root (list "rev-parse" "--is-inside-work-tree")))))

(define (current-branch git-root)
  (define result (run-git* git-root (list "rev-parse" "--abbrev-ref" "HEAD")))
  (and (git-exit-ok? result)
       (let ([b (string-trim (git-stdout result))]) (and (not (string=? b "")) b))))

;; ============================================================
;; Issue → expected branch resolution (from STATE.md wave table)
;; ============================================================

(define wave-table-rx #rx"^\\| *W([0-9]+) *\\| *#([0-9]+) *\\|")
(define (wave-issue-number base-dir wave-idx)
  ;; Issue-less campaigns (no per-wave GitHub issues) return #f. A STATE.md
  ;; row is only honored when it references the CURRENT plan's wave doc for
  ;; this index: stale rows left by a previous campaign (whose wave docs were
  ;; replaced) must not be misread as issue mappings. When the campaign has no
  ;; issues at all, the branch check degrades to "no expected branch" (pass).
  (define state-path (build-path base-dir ".planning" "STATE.md"))
  (define current-slug (wave-slug base-dir wave-idx))
  (define expected-doc (and current-slug (format "waves/W~a-~a.md" wave-idx current-slug)))
  (cond
    [(or (not (file-exists? state-path)) (not expected-doc)) #f]
    [else
     (define text (call-with-input-file state-path port->string))
     (for/first ([line (in-list (string-split text "\n"))]
                 #:when
                 (let ([m (regexp-match wave-table-rx line)])
                   (and
                    m
                    (= (string->number (cadr m)) wave-idx)
                    ;; The row must link the current plan's wave doc.
                    (string-contains?
                     line
                     (string-append "waves/W" (number->string wave-idx) "-" current-slug ".md")))))
       (define m (regexp-match wave-table-rx line))
       (cadr (cdr m)))]))

(define (expected-wave-branch base-dir wave-idx)
  (define issue (wave-issue-number base-dir wave-idx))
  (and issue (format "feature/issue-~a-wave" issue)))

;; ============================================================
;; Individual checks
;; ============================================================

(define (check-git-available base-dir)
  (cons "git"
        (if (git-available? base-dir)
            (cons #t "git repository reachable")
            (cons #f "no git repository reachable"))))

(define (check-branch-matches base-dir wave-idx)
  (define ctx (current-gsd-delivery-branch-context))
  (cond
    ;; W7 (#9512b) isolated delivery: the expected branch is the recorded
    ;; wave branch (campaign/<hash8>/w<N>), and it must resolve as a ref in
    ;; the campaign repo. The shared checkout's current branch is
    ;; irrelevant — delivery is evaluated on the branch, not any checkout.
    [ctx
     (define root (branch-delivery-context-ref ctx 'repo-root))
     (define expected (branch-delivery-context-ref ctx 'branch))
     (define branch
       (and (git-exit-ok? (run-git* root (list "rev-parse" "--verify" expected))) expected))
     (define detail (format "branch=~a expected=~a (isolated)" (or branch #f) expected))
     (cons "branch"
           (if branch
               (cons #t detail)
               (cons #f detail)))]
    [else
     (define root (git-root-for base-dir))
     (define branch (and root (current-branch root)))
     (define expected (expected-wave-branch base-dir wave-idx))
     (define detail (format "branch=~a expected=~a" branch (or expected "?")))
     (cons
      "branch"
      (cond
        ;; Issue-less campaign: no per-wave GitHub issue table in STATE.md,
        ;; so no expected feature branch exists (work happens on main or the
        ;; current branch). Only enforce the branch match when the campaign
        ;; actually declares per-wave issues.
        [(not expected) (cons #t (string-append detail " (issue-less campaign: no expected branch)"))]
        [(and branch (string=? branch expected)) (cons #t detail)]
        [else (cons #f detail)]))]))

(define (wave-file->git-relative base-dir git-root wave-file)
  ;; Wave files may be declared either repo-root-relative (e.g.
  ;; "q/ui-core/preferences.rkt" when git root is <base>/q) or
  ;; git-root-relative (e.g. ".github/workflows/ci.yml" when git root is
  ;; <base>/q). Prefer the repo-root mapping; when it escapes the git root
  ;; ("../..."), the wave file was declared git-root-relative, so use it
  ;; verbatim. This makes the verifier tolerant of BOTH conventions: the
  ;; canonical repo-root-relative form and the git-root-relative form that
  ;; wave authors commonly use for CI/workflow paths.
  ;; BUG-0025 (v1.00.18 W1): normalize the declared path FIRST (strip
  ;; backticks and trailing "[NEW]"-style annotation prose). The parser
  ;; already cleans at parse time, but the verifier also receives manually
  ;; constructed plans, so defense in depth: never let annotation prose
  ;; reach the git mapping.
  (define f (clean-file-path wave-file))
  (define abs (path->complete-path (build-path base-dir f)))
  (define rel (find-relative-path git-root abs))
  (define rel-str (path->string rel))
  ;; The repo-root mapping escapes the git root ("../...") when the wave
  ;; file was declared git-root-relative. Detect the escape by prefix —
  ;; string-prefix? is clearer than a char-class regexp here.
  (if (or (string-prefix? rel-str "../") (string-prefix? rel-str "..\\"))
      (string-trim f)
      rel-str))

(define (base-branch-ref git-root)
  ;; The branch the current branch diverged from, used to attribute committed
  ;; delivery changes. Prefers origin/main (the integration branch); falls
  ;; back to local main. Returns #f when neither resolves.
  (for/or ([ref (in-list '("origin/main" "main"))]
           #:when (git-exit-ok? (run-git* git-root (list "rev-parse" "--verify" ref))))
    ref))

(define (changed-files-set base-dir git-root [campaign-created-at #f])
  ;; Returns a set of git-relative paths that constitute delivery evidence:
  ;; uncommitted working-tree changes vs HEAD, untracked-new files, commits on
  ;; the current branch relative to its base (waves that commit + push + open a
  ;; PR per their wave doc deliver their work as commits), and — when the
  ;; campaign's creation time is known — files changed in commits since the
  ;; campaign base (merged-to-main delivery: once a wave's PR is merged, HEAD
  ;; == origin/main so the base-relative diff is empty, but the target files
  ;; DID change during the campaign).
  (define diff-result (run-git* git-root (list "diff" "--name-only" "HEAD")))
  (define untracked-result (run-git* git-root (list "ls-files" "--others" "--exclude-standard")))
  (define committed-result
    (let ([base (base-branch-ref git-root)])
      (if base
          ;; Three-dot diff: changes introduced on HEAD since diverging from base.
          (run-git* git-root (list "diff" "--name-only" (format "~a...HEAD" base)))
          (list 1 "" ""))))
  (define campaign-result
    (if (and campaign-created-at (exact-integer? campaign-created-at))
        ;; The campaign base = the last commit reachable from HEAD that is
        ;; strictly older than the campaign's creation time. Files changed in
        ;; commits between that base and HEAD are delivery evidence for waves
        ;; whose work was merged to main.
        (let ([base-commit
               (run-git*
                git-root
                (list "rev-list" "-1" "--before" (number->string campaign-created-at) "HEAD"))])
          (if (git-exit-ok? base-commit)
              (run-git* git-root
                        (list "diff" "--name-only" (string-trim (git-stdout base-commit)) "HEAD"))
              (list 1 "" "")))
        (list 1 "" "")))
  (define paths
    (append (string-split (git-stdout diff-result) "\n")
            (string-split (git-stdout untracked-result) "\n")
            (string-split (git-stdout committed-result) "\n")
            (string-split (git-stdout campaign-result) "\n")))
  (for/set ([p (in-list paths)]
            #:when (not (string=? (string-trim p) "")))
    (string-trim p)))

;; Does the changed-file set satisfy this wave target?
;; - exact path match (existing behavior): a wave target that is a concrete
;;   file is satisfied only when that file changed;
;; - directory match: a wave target ending in "/" (e.g. "q/tests/memory/") is
;;   satisfied when ANY changed file lives under that directory. git diff
;;   lists files, never directories, so a directory target must be matched
;;   by prefix. This supports wave docs that scope a target to a directory
;;   ("existing tests + new focused tests under q/tests/memory/").
(define (wave-file-changed? changed git-relative wave-file)
  (cond
    [(set-member? changed git-relative) #t]
    ;; directory target: original wave-file ends with "/" (find-relative-path
    ;; strips the trailing slash from git-relative, so detect via wave-file)
    [(and (string? wave-file) (string-suffix? (string-trim wave-file) "/"))
     (define dir
       (if (string-suffix? git-relative "/")
           git-relative
           (string-append git-relative "/")))
     (for/or ([c (in-set changed)])
       (string-prefix? c dir))]
    [else #f]))

(define (check-wave-files-changed base-dir wave-idx plan [campaign-created-at #f])
  (define wave (and plan (plan-wave-ref plan wave-idx)))
  (define files
    (if wave
        ;; BUG-0025 (v1.00.18 W1): normalize declared paths — strip backticks
        ;; and trailing "[NEW]"-style annotation prose — before matching and
        ;; before building diagnostics. The parser cleans at parse time, but
        ;; manually constructed plans (and any future declaration route) must
        ;; not let annotation prose defeat file matching; the failure message
        ;; must show computed paths, never raw annotated declarations.
        (map clean-file-path (gsd-wave-files wave))
        '()))
  (define root (evidence-git-root base-dir))
  (cond
    [(not root) (cons "files" (cons #f "no git root"))]
    [(null? files) (cons "files" (cons #f "wave declares no target files"))]
    [else
     ;; W7 (#9512b): under an active branch context the changed set is the
     ;; COMMITTED branch diff ONLY — uncommitted worktree dirt (shared
     ;; checkout or wave worktree) never satisfies delivery. The failure
     ;; message is byte-identical to the legacy path.
     (let* ([changed (if (current-gsd-delivery-branch-context)
                         (committed-branch-changed-files)
                         (changed-files-set base-dir root campaign-created-at))]
            [changed-wave-files
             (for/list ([f (in-list files)]
                        #:when
                        (wave-file-changed? changed (wave-file->git-relative base-dir root f) f))
               f)])
       (cons "files"
             (if (pair? changed-wave-files)
                 (cons #t (format "changed: ~a" (string-join changed-wave-files ", ")))
                 ;; BUG-0025 (v1.00.18 W1): show the computed git-relative
                 ;; mapping per unmatched declared file so path mismatches
                 ;; (e.g. annotation prose surviving into the declared path,
                 ;; or a wrong path convention) are diagnosable from the
                 ;; rejection message alone.
                 (cons #f
                       (format "no wave target files changed: ~a~a"
                               (string-join files ", ")
                               (string-append
                                "\n"
                                (string-join
                                 (for/list ([f (in-list files)])
                                   (format "  ~a -> ~a" f (wave-file->git-relative base-dir root f)))
                                 "\n")))))))]))

;; ============================================================
;; Verify command (bounded)
;; ============================================================

(define (build-compile-gate base-dir git-root wave-idx plan)
  ;; `raco make` on the wave's changed Racket target files from the git root.
  ;; Non-Racket artifacts (ci.yml, .md docs, etc.) are excluded because
  ;; `raco make` fails on them and they carry no compile evidence.
  (define wave (and plan (plan-wave-ref plan wave-idx)))
  (define files
    (if wave
        ;; BUG-0025 (v1.00.18 W1): normalize declared paths (strip
        ;; annotations/backticks) so annotated "[NEW]" declarations are
        ;; still recognized as Racket sources and matched against the
        ;; changed set.
        (map clean-file-path (gsd-wave-files wave))
        '()))
  ;; W7 (#9512b): under an active branch context the "changed" set is the
  ;; committed branch diff; the gate compiles the wave's delivered sources
  ;; (targets are git-relative and layout-identical across checkouts).
  (define changed
    (if (current-gsd-delivery-branch-context)
        (committed-branch-changed-files)
        (changed-files-set base-dir git-root)))
  (define (racket-source? p)
    (regexp-match? #rx"[.]rktl?$" p))
  (define targets
    (for/list ([f (in-list files)]
               #:when (and (racket-source? f)
                           (set-member? changed (wave-file->git-relative base-dir git-root f))))
      (wave-file->git-relative base-dir git-root f)))
  (if (null? targets)
      #f
      (string-join (cons "raco make" targets) " ")))

(define (run-verify-command command git-root timeout-sec)
  ;; Run a shell command bounded by timeout-sec from git-root.
  ;; Returns (list exit-code stdout) where exit-code is #f on timeout.
  (define-values (sp out in err)
    (parameterize ([current-directory git-root])
      (subprocess #f #f #f "/bin/sh" "-c" command)))
  (define deadline (+ (current-inexact-milliseconds) (* timeout-sec 1000.0)))
  (let loop ()
    (define status (subprocess-status sp))
    (cond
      [(eq? status 'running)
       (if (>= (current-inexact-milliseconds) deadline)
           (begin
             (subprocess-kill sp #t)
             (list #f "" ""))
           (begin
             (sleep 0.01)
             (loop)))]
      [else
       (subprocess-wait sp)
       (list status
             (if (and out (input-port? out))
                 (port->string out)
                 "")
             (if (and err (input-port? err))
                 (port->string err)
                 ""))])))

(define (check-verify-command base-dir wave-idx plan)
  (define root (git-root-for base-dir))
  (define explicit (current-gsd-delivery-verify-command))
  (define gate (and root (not explicit) (build-compile-gate base-dir root wave-idx plan)))
  ;; W7 (#9512b): under an active branch context the command runs from the
  ;; wave WORKTREE (the delivered tree as committed), not the shared
  ;; checkout — the shared tree does not contain the wave's changes.
  (define run-cwd (verify-command-cwd base-dir root))
  (define (run-cmd command)
    (let* ([result (run-verify-command command run-cwd (current-gsd-delivery-verify-timeout-sec))]
           [exit-code (car result)]
           [detail (format "cmd=~a exit=~a" command exit-code)])
      (cons "verify"
            (if (eq? exit-code 0)
                (cons #t detail)
                (cons #f detail)))))
  (cond
    [(not root) (cons "verify" (cons #f "no git root"))]
    [explicit (run-cmd explicit)]
    [gate (run-cmd gate)]
    ;; No Racket targets changed/derivable (docs-only or CI-only wave). The
    ;; file-changed check already guarantees delivery evidence; there is no
    ;; compile evidence to gate on.
    [else (cons "verify" (cons #t "no Racket targets to compile (docs/CI-only change)"))]))

;; ============================================================
;; Composition
;; ============================================================

(define (run-delivery-verification base-dir plan wave-idx [campaign-created-at #f])
  ;; Run all evidence checks. A wave is approved only when every check passes.
  (define checks
    (list (check-git-available base-dir)
          (check-branch-matches base-dir wave-idx)
          (check-wave-files-changed base-dir wave-idx plan campaign-created-at)
          (check-verify-command base-dir wave-idx plan)))
  (define failed
    (for/list ([c (in-list checks)]
               #:when (not (car (cdr c))))
      (format "~a: ~a" (car c) (cdr (cdr c)))))
  (if (null? failed)
      (delivery-verification #t checks "delivery verified")
      (delivery-verification #f
                             checks
                             (string-join (cons "delivery verification failed:" failed) "\n"))))

;; Verifier callback for the campaign coordinator: (lambda (wave-idx) ...)
;; returning a `delivery-verification` struct.
(define (make-delivery-verifier base-dir plan [campaign-created-at #f])
  (lambda (wave-idx) (run-delivery-verification base-dir plan wave-idx campaign-created-at)))
