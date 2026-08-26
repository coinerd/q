#lang racket/base

;; extensions/gsd/wave-executor.rkt — Wave execution engine with error recovery
;; STABILITY: evolving
;;
;; Wave 2b of v0.21.0: Tracks wave status through execution lifecycle.
;; DD-5: Wave-level error recovery — failed waves skip, don't abort.
;;
;; Lifecycle: pending → in-progress → completed | failed | skipped
;; Failed waves do NOT block subsequent waves.
;;
;; v1.00.18 W5 (#9513): Mutation-stall watchdog accounting.
;; A "mutation" is a file-mutating tool call: write | edit | racket_edit |
;; racket_codemod (with write=true) | planning-write. Reads, greps, finds and
;; bash (even mutating bash) do NOT count — during implementation the file
;; tools are the deliverable signal.
;;   STALL-SOFT-LIMIT-DEFAULT = 25 calls without a mutation → one steering
;;     injection into the live executor session ("begin the first edit now").
;;   STALL-HARD-LIMIT-DEFAULT = 60 calls without a mutation → the attempt is
;;     failed with an explicit stall cause so verification/retry see it.
;; Both limits are keyword-configurable on run-campaign-wave; #f disables.
;; Pure accounting lives here; injection/termination live in go-orchestrator.
;;
;; v1.00.17 W6 (#9512a): Wave worktree isolation lifecycle.
;; make-wave-worktree!/cleanup-wave-worktree!/reclaim-orphaned-worktrees!
;; give each wave attempt its own git worktree + campaign branch, sibling of
;; the project root, so the shared checkout is never mutated by an in-flight
;; attempt. Gated behind the `gsd.worktree-isolation` flag (default ON since
;; the v1.00.17 W8 integration bake); `#:isolate? #f` selects the legacy
;; shared-checkout path. Path placement is LOAD-BEARING: see
;; docs/reports/GSD-WORKTREE-ISOLATION-v1.00.17.md.

(require racket/format
         racket/string
         racket/file
         racket/path
         racket/system
         racket/port
         "plan-types.rkt"
         (only-in "../../runtime/settings-query.rkt" gsd-worktree-isolation-enabled?)
         "../gsd/wave-docs.rkt"
         (only-in "shared.rkt" extract-plan-title)
         (only-in "state-machine.rkt"
                  gsd-wave-gate-counter
                  gsd-wave-gate-interval
                  gsd-wave-gate-blocked?
                  gsd-wave-gate-increment!)
         (only-in "campaign-state.rkt"
                  campaign-record-waves
                  campaign-wave-index
                  campaign-wave-status
                  campaign-wave-attempt-count))

(provide wave-status
         wave-status?
         wave-status-index
         wave-status-state
         wave-status-error-message
         wave-status-attempt-count
         wave-status-timestamp
         make-wave-executor
         make-wave-executor-from-validated
         make-wave-executor-from-campaign
         load-plan-from-index
         wave-start!
         wave-complete!
         wave-fail!
         wave-skip!
         next-pending-wave
         wave-summary
         all-waves-done?
         ;; Exposed for testing
         wave-executor-statuses
         wave-executor-plan
         compute-next-wave-statuses
         ;; Wave gate
         wave-gate-interval
         wave-gate-counter
         wave-gate-blocked?
         wave-gate-clear!
         ;; v1.00.18 W5 (#9513): mutation-stall watchdog
         STALL-SOFT-LIMIT-DEFAULT
         STALL-HARD-LIMIT-DEFAULT
         stall-limit?
         mutation-tool-call?
         mutation-tool-name?
         stall-state
         make-stall-watchdog
         stall-watchdog?
         stall-watchdog-soft-limit
         stall-watchdog-hard-limit
         stall-watchdog-snapshot
         stall-watchdog-observe!
         ;; v1.00.17 W6 (#9512a): wave worktree isolation
         WORKTREE-ISOLATION-SETTING-NAME
         WORKTREE-DIRNAME-PREFIX
         current-gsd-worktree-isolation
         worktree-isolation-enabled?
         resolve-worktree-isolation
         apply-worktree-isolation-setting!
         worktree-isolation-banner
         worktree-hash8
         wave-worktree-dirname
         wave-worktree-dir
         wave-worktree-branch-name
         wave-worktree-add-args
         git-result
         git-result-code
         git-result-stdout
         git-result-stderr
         default-run-git
         find-repo-root
         wave-worktree
         wave-worktree?
         wave-worktree-repo-root
         wave-worktree-path
         wave-worktree-branch
         wave-worktree-base-ref
         wave-worktree-planning-dir
         wave-worktree-cwd
         make-wave-worktree!
         cleanup-wave-worktree!
         release-wave-worktree!
         reclaim-orphaned-worktrees!
         ;; v1.00.20 W4 (BUG-0030): mid-wave checkpoint commits
         CHECKPOINT-COMMIT-PREFIX
         wave-checkpoint-commit-message
         checkpoint-commit-message?
         commit-wave-checkpoint!
         checkpoint-contract-lines)

;; ============================================================
;; Wave status struct
;; ============================================================

(struct wave-status (index state error-message attempt-count timestamp) #:transparent)

;; ============================================================
;; Wave executor (mutable struct with plan + statuses)
;; ============================================================

;; Wave gate — re-exported from state-machine.rkt (canonical source, AXIS1-F28)
(define wave-gate-interval gsd-wave-gate-interval)
(define wave-gate-counter gsd-wave-gate-counter)

(struct wave-executor (plan statuses) #:transparent #:mutable)

;; W-05: These accessors read/write mutable state without locking.
;; Safe because GSD wave execution is single-threaded (one wave at a time,
;; enforced by the state machine). If multi-wave concurrency is added,
;; these need synchronization.

;; wave-executor-statuses is now auto-generated by #:mutable struct

(define (set-executor-statuses! exec statuses)
  (set-wave-executor-statuses! exec statuses))

;; ============================================================
;; Constructor
;; ============================================================

(define (make-wave-executor plan)
  (define waves (gsd-plan-waves plan))
  (define initial-statuses
    (for/list ([w waves])
      (wave-status (gsd-wave-index w) 'pending #f 0 (current-seconds))))
  (wave-executor plan initial-statuses))

;; v0.24.2: Constructor from validated normalized plan.
(define (make-wave-executor-from-validated vp)
  (define norm-plan (gsd-validated-plan-plan vp))
  (define norm-waves (gsd-normalized-plan-waves norm-plan))
  (define initial-statuses
    (for/list ([w norm-waves])
      (wave-status (gsd-normalized-wave-index w) 'pending #f 0 (current-seconds))))
  ;; Reconstruct a gsd-plan for backward compatibility with wave-executor struct
  (define compat-waves
    (for/list ([w norm-waves])
      (gsd-wave (gsd-normalized-wave-index w)
                (gsd-normalized-wave-title w)
                'pending
                ""
                (gsd-normalized-wave-files w)
                '()
                (gsd-normalized-wave-verify-command w)
                (gsd-normalized-wave-done-criteria w))))
  (define compat-plan (gsd-plan compat-waves #f '() '()))
  (wave-executor compat-plan initial-statuses))

;; v0.99.80 W0: Constructor from durable campaign record (GC-2).
;; Maps campaign-wave statuses to executor statuses for restart reconstruction.
(define (make-wave-executor-from-campaign rec)
  (define statuses
    (for/list ([w (campaign-record-waves rec)])
      (wave-status (campaign-wave-index w)
                   (case (campaign-wave-status w)
                     [(done) 'completed]
                     [(deferred) 'skipped]
                     [(failed) 'failed]
                     [else 'pending])
                   #f
                   (campaign-wave-attempt-count w)
                   (current-seconds))))
  (wave-executor #f statuses))

;; ============================================================
;; Status transitions
;; ============================================================

;; Pure function: compute next wave statuses without mutation
(define (compute-next-wave-statuses statuses idx update-fn)
  (for/list ([s statuses])
    (if (= (wave-status-index s) idx)
        (update-fn s)
        s)))

;; Effect wrapper: mutates the executor's statuses field
(define (update-status! exec idx update-fn)
  (define statuses (wave-executor-statuses exec))
  (define new-statuses (compute-next-wave-statuses statuses idx update-fn))
  (set-executor-statuses! exec new-statuses))

(define (wave-gate-blocked?)
  (gsd-wave-gate-blocked?))

(define (wave-gate-clear!)
  (gsd-wave-gate-counter 0))

(define (wave-start! exec idx)
  (when (gsd-wave-gate-blocked?)
    (error 'wave-start!
           "wave gate blocked: ~a consecutive waves without broad-gate. Clear with wave-gate-clear!"
           (gsd-wave-gate-counter)))
  (gsd-wave-gate-increment!)
  (update-status!
   exec
   idx
   (lambda (s)
     (wave-status idx 'in-progress #f (add1 (wave-status-attempt-count s)) (current-seconds)))))

(define (wave-complete! exec idx)
  (update-status! exec
                  idx
                  (lambda (s)
                    (wave-status idx 'completed #f (wave-status-attempt-count s) (current-seconds)))))

(define (wave-fail! exec idx error-message)
  (update-status!
   exec
   idx
   (lambda (s)
     (wave-status idx 'failed error-message (wave-status-attempt-count s) (current-seconds)))))

(define (wave-skip! exec idx)
  (update-status! exec
                  idx
                  (lambda (s)
                    (wave-status idx 'skipped #f (wave-status-attempt-count s) (current-seconds)))))

;; ============================================================
;; Queries
;; ============================================================

(define (next-pending-wave exec)
  (define statuses (wave-executor-statuses exec))
  (define pending (filter (lambda (s) (eq? (wave-status-state s) 'pending)) statuses))
  (if (null? pending)
      #f
      (wave-status-index (car pending))))

(define (all-waves-done? exec)
  (define statuses (wave-executor-statuses exec))
  (for/and ([s statuses])
    (and (memq (wave-status-state s) '(completed failed skipped)) #t)))

(define (wave-summary exec)
  (define statuses (wave-executor-statuses exec))
  (define by-state
    (for/fold ([acc (hasheq)]) ([s statuses])
      (hash-update acc (wave-status-state s) add1 0)))
  (define n-completed (hash-ref by-state 'completed 0))
  (define n-failed (hash-ref by-state 'failed 0))
  (define n-skipped (hash-ref by-state 'skipped 0))
  (define n-pending (hash-ref by-state 'pending 0))
  (define n-in-progress (hash-ref by-state 'in-progress 0))
  (define total (length statuses))
  (define parts
    (filter values
            (list (format "Total: ~a waves" total)
                  (if (> n-completed 0)
                      (format "✅ Completed: ~a" n-completed)
                      #f)
                  (if (> n-failed 0)
                      (format "❌ Failed: ~a" n-failed)
                      #f)
                  (if (> n-skipped 0)
                      (format "⏭  Skipped: ~a" n-skipped)
                      #f)
                  (if (> n-pending 0)
                      (format "⏳ Pending: ~a" n-pending)
                      #f)
                  (if (> n-in-progress 0)
                      (format "🔄 In Progress: ~a" n-in-progress)
                      #f))))
  (string-join parts "\n"))

;; ============================================================
;; Load plan from disk (PLAN.md index + wave docs)
;; ============================================================

;; missing-wave-docs-error-text : (listof wave-index-entry?) (listof string?) -> string?
;; BUG-0023 (W2): error text naming each missing wave doc plus the expected
;; filename convention, so authors can comply immediately.
(define (missing-wave-docs-error-text entries missing)
  (format (string-append "Plan index has ~a wave entr~a but ~a wave doc~a missing on disk: ~a\n"
                         "Expected filename convention: waves/W<idx>-<slug>.md "
                         "(e.g. .planning/waves/W0-title-slug.md). "
                         "Create the missing file(s) or fix the index target path.")
          (length entries)
          (if (= (length entries) 1) "y" "ies")
          (length missing)
          (if (= (length missing) 1) " is" "s are")
          (string-join missing ", ")))

(define (load-plan-from-index base-dir)
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (if (not (file-exists? plan-path))
      #f
      (let* ([text (call-with-input-file plan-path port->string)]
             [entries (parse-plan-index text)])
        (if (null? entries)
            #f
            ;; BUG-0023 (W2): strict index validation. An index entry whose
            ;; target wave doc does not exist is a load ERROR naming the
            ;; expected path — previously read-wave-doc returned #f and the
            ;; wave loaded with silent empty content.
            (let ([missing (missing-index-doc-paths base-dir entries)])
              (if (pair? missing)
                  (raise (exn:fail (missing-wave-docs-error-text entries missing)
                                   (current-continuation-marks)))
                  (let* ([title (extract-plan-title text)]
                         [waves (for/list ([e entries])
                                  (define idx (wave-index-entry-idx e))
                                  (define slug (wave-index-entry-slug e))
                                  (define wave-data (read-wave-doc base-dir idx slug))
                                  (define wave-content
                                    (if wave-data
                                        (hash-ref wave-data 'content)
                                        ""))
                                  (gsd-wave idx
                                            (wave-index-entry-title e)
                                            (string->wave-status-from-entry e)
                                            wave-content
                                            (extract-files-from-content wave-content)
                                            '()
                                            (extract-verify-from-content wave-content)
                                            '()))])
                    (gsd-plan waves #f '() '()))))))))

;; extract-plan-title: imported from shared.rkt (v0.32.1 Wave 1 DRY)

(define (string->wave-status-from-entry e)
  (define s (wave-index-entry-status e))
  (cond
    [(string=? s "DONE") 'completed]
    [(string=? s "FAILED") 'failed]
    [(string=? s "DEFERRED") 'skipped]
    [(string=? s "In-Progress") 'in-progress]
    [else 'pending]))

(define (extract-files-from-content content)
  (hash-ref (parse-wave-content content) 'files '()))

(define (extract-verify-from-content content)
  (hash-ref (parse-wave-content content) 'verify ""))
;; v0.31.5 W0: pure function placeholder

;; ============================================================
;; Mutation-stall watchdog accounting (v1.00.18 W5 — #9513)
;;
;; v1.00.16 W3 attempt-2 made 92 tool calls — all read-only — and never
;; edited a file. Nothing observed it until delivery verification. This
;; module provides the PURE accounting half: a fold over executor
;; tool-call records that counts calls since the last file mutation.
;; ============================================================

;; Default thresholds, documented here per the wave Done criteria.
;; 25 exploring calls ≈ several minutes of read-only loops; 60 is well past
;; "gathering context" and into "not implementing".
(define STALL-SOFT-LIMIT-DEFAULT 25)
(define STALL-HARD-LIMIT-DEFAULT 60)

;; A limit is either disabled (#f) or a positive integer.
(define (stall-limit? v)
  (or (eq? v #f) (and (exact-positive-integer? v))))

;; File-mutating tool names (mutations reset the stall counter).
(define MUTATION-TOOL-NAMES '(write edit racket_edit planning-write))

;; Normalize a tool name from the wire (symbol or string) to a symbol.
(define (normalize-tool-name v)
  (cond
    [(symbol? v) v]
    [(string? v) (string->symbol v)]
    [else #f]))

;; Is this tool name a file-mutating tool? racket_codemod is conditional:
;; it mutates only when invoked with write=true (a dry-run codemod is a
;; read). racket_edit/write/edit/planning-write always mutate.
(define (mutation-tool-name? name)
  (define n (normalize-tool-name name))
  (cond
    [(memq n MUTATION-TOOL-NAMES) #t]
    [(eq? n 'racket_codemod) 'needs-arguments]
    [else #f]))

;; A tool-call record is a hash with 'name (symbol or string) and an
;; optional 'arguments hash (only racket_codemod needs it).
(define (mutation-tool-call? rec)
  (and (hash? rec)
       (let ([kind (mutation-tool-name? (hash-ref rec 'name #f))])
         (cond
           [(eq? #t kind) #t]
           [(eq? 'needs-arguments kind)
            (define args (hash-ref rec 'arguments #f))
            (and (hash? args)
                 (let ([w (hash-ref args 'write #f)])
                   ;; write=true (boolean #t or the string "true")
                   (and (not (eq? w #f)) (not (equal? w "false")))))]
           [else #f]))))

;; Pure fold: sequence of tool-call records → stall snapshot
;;   'calls-since-mutation — the watchdog signal (0 right after a mutation)
;;   'total-calls           — every observed call
;;   'mutations             — number of file mutations observed
;; No I/O, no state. Pure so tests can drive synthetic call sequences.
(define (stall-state records)
  (for/fold ([since 0]
             [total 0]
             [mutations 0]
             #:result (hasheq 'calls-since-mutation since 'total-calls total 'mutations mutations))
            ([rec (in-list (if (list? records)
                               records
                               '()))])
    (if (mutation-tool-call? rec)
        (values 0 (add1 total) (add1 mutations))
        (values (add1 since) (add1 total) mutations))))

;; Stateful watchdog over a single attempt/session. One soft injection per
;; session (not per call): the 'soft-stall? flag latches once tripped.
;; The host (go-orchestrator) observes tool batches through
;; stall-watchdog-observe! and receives 'ok | 'soft-stall | 'hard-stall.
(struct stall-watchdog (soft-limit hard-limit state) #:transparent)

(define (make-stall-watchdog #:soft-limit [soft-limit STALL-SOFT-LIMIT-DEFAULT]
                             #:hard-limit [hard-limit STALL-HARD-LIMIT-DEFAULT])
  (unless (stall-limit? soft-limit)
    (raise-argument-error 'make-stall-watchdog "(or/c #f exact-positive-integer?)" soft-limit))
  (unless (stall-limit? hard-limit)
    (raise-argument-error 'make-stall-watchdog "(or/c #f exact-positive-integer?)" hard-limit))
  (stall-watchdog soft-limit
                  hard-limit
                  (box (hasheq 'calls-since-mutation 0 'total-calls 0 'mutations 0 'soft-sent? #f))))

(define (stall-watchdog-snapshot wd)
  (unbox (stall-watchdog-state wd)))

;; Fold one batch of records into the watchdog and classify the outcome.
;;   'hard-stall — hard limit crossed (checked FIRST: an exploring executor
;;                 deep past both limits must fail, not be re-steered)
;;   'soft-stall — soft limit crossed for the first time (latched: exactly
;;                 one injection per session)
;;   'ok         — keep going
;; When both limits are #f the watchdog is inert (always 'ok).
(define (stall-watchdog-observe! wd records)
  (define st0 (stall-watchdog-snapshot wd))
  (define st1
    (for/fold ([st st0])
              ([rec (in-list (if (list? records)
                                 records
                                 '()))])
      (if (mutation-tool-call? rec)
          (hasheq 'calls-since-mutation
                  0
                  'total-calls
                  (add1 (hash-ref st 'total-calls))
                  'mutations
                  (add1 (hash-ref st 'mutations))
                  'soft-sent?
                  (hash-ref st 'soft-sent?))
          (hasheq 'calls-since-mutation
                  (add1 (hash-ref st 'calls-since-mutation))
                  'total-calls
                  (add1 (hash-ref st 'total-calls))
                  'mutations
                  (hash-ref st 'mutations)
                  'soft-sent?
                  (hash-ref st 'soft-sent?)))))
  (define hard (stall-watchdog-hard-limit wd))
  (define soft (stall-watchdog-soft-limit wd))
  (define since (hash-ref st1 'calls-since-mutation))
  (cond
    [(and hard (>= since hard))
     (set-box! (stall-watchdog-state wd) (hash-set st1 'soft-sent? #t))
     'hard-stall]
    [(and soft (>= since soft) (not (hash-ref st1 'soft-sent?)))
     (set-box! (stall-watchdog-state wd) (hash-set st1 'soft-sent? #t))
     'soft-stall]
    [else
     (set-box! (stall-watchdog-state wd) st1)
     'ok]))

;; ============================================================
;; Wave worktree isolation (v1.00.17 W6 — #9512a)
;;
;; Root cause being addressed: wave executors ran directly in the shared
;; project checkout, so "done" waves existed only as uncommitted working-tree
;; mutations, later waves ran baselines against a tree they did not own, and
;; concurrent campaigns contaminated each other's diffs and gates.
;;
;; Lifecycle (one concern, isolated):
;;   campaign start   → reclaim-orphaned-worktrees! (crash recovery)
;;   wave attempt N   → make-wave-worktree!  (worktree + campaign branch)
;;   terminal outcome → cleanup-wave-worktree! (remove worktree + branch)
;;
;; PLACEMENT CONSTRAINT (hard): the worktree is created as a SIBLING of the
;; git toplevel (the q/ checkout), i.e. at
;;   <project-parent>/wt-campaign-<hash8>-w<N>
;; NEVER under /tmp and NEVER inside the repo. Reason: scripts in this repo
;; resolve the q checkout as `<cwd>/../q`; a sibling worktree preserves that
;; path shape (worktree-root/../q → the shared checkout), which is
;; load-bearing for CI parity. See
;; docs/reports/GSD-WORKTREE-ISOLATION-v1.00.17.md.
;; ============================================================

;; User-facing setting name (documented in the design record). Default ON
;; since the v1.00.17 W8 integration bake (dogfooded on this campaign:
;; waves ran worktree-isolated end-to-end; see
;; docs/reports/GSD-EXECUTOR-HARDENING-BAKE-v1.00.17.md). The `#:isolate? #f`
;; keyword on run-campaign-wave / run-wave remains the explicit disable
;; switch for tests and operators.
(define WORKTREE-ISOLATION-SETTING-NAME "gsd.worktree-isolation")
(define WORKTREE-DIRNAME-PREFIX "wt-campaign-")

;; Feature flag, mirroring the policy.rkt parameter pattern. `#:isolate?`
;; keyword arguments (tests, explicit operator override) take precedence.
;; BUG-0028 hotfix (v1.00.17): default rolled back to #f — with isolation ON,
;; per-attempt worktrees invalidate the worker's captured allowed-roots
;; (current-allowed-roots = cwd at worker start, never refreshed), so executors
;; cannot edit ANY path and fall back to raw shell mutation. Isolation returns
;; as a default only after worker allowed-roots track worktree lifecycle.
(define current-gsd-worktree-isolation (make-parameter #f (lambda (v) (and v #t))))

;; 'auto sentinel (not #f) so an EXPLICIT #:isolate? #f is honored as the
;; documented disable switch instead of falling through to the parameter —
;; W8 bake fix, v1.00.17.
(define (worktree-isolation-enabled? #:isolate? (override 'auto))
  (if (eq? override 'auto)
      (current-gsd-worktree-isolation)
      (and override #t)))

;; BUG-0028 S1 (v1.00.19 W2): settings wiring. The gsd.worktree-isolation key
;; is declared in the settings surface; resolve-worktree-isolation connects it
;; to the runtime flag at the composition root (go-orchestrator
;; run-campaign-wave, which calls apply-worktree-isolation-setting!).
;;
;; Precedence, highest first:
;;   1. EXPLICIT #:isolate? argument ('auto = not given) — operator override,
;;      honored in both directions (#t forces ON, #f forces OFF).
;;   2. gsd.worktree-isolation project-settings key (settings may be #f when
;;      no project settings could be loaded ⇒ key absent).
;;   3. current-gsd-worktree-isolation parameter default (OFF — the BUG-0028
;;      hotfix rollback stands until the W6 bake proves zero denials).
(define (resolve-worktree-isolation settings #:isolate? (override 'auto))
  (cond
    [(not (eq? override 'auto)) (and override #t)]
    [(and settings (gsd-worktree-isolation-enabled? settings)) #t]
    [else (current-gsd-worktree-isolation)]))

;; Composition-root application: resolve per the precedence above and leave
;; the parameter consistent with the outcome so downstream
;; worktree-isolation-enabled? (default 'auto) readers agree. Returns the
;; effective flag.
(define (apply-worktree-isolation-setting! settings #:isolate? (override 'auto))
  (define effective (resolve-worktree-isolation settings #:isolate? override))
  (current-gsd-worktree-isolation effective)
  effective)

;; BUG-0028 S2 (v1.00.19 W2): /doctor-style one-liner emitted at executor
;; start when isolation is ON — active worktree + resolved allowed roots, so
;; future staleness is visible immediately instead of via failed edits.
;; Pure; callers log it.
(define (worktree-isolation-banner worktree-path allowed-roots)
  (define (->s p)
    (if (string? p)
        p
        (path->string p)))
  (format "gsd worktree isolation ON — active worktree: ~a; allowed roots: ~a"
          (->s worktree-path)
          (string-join (map ->s allowed-roots) ", ")))

;; ---- Pure naming ----------------------------------------------------------

(define (->path p)
  (if (string? p)
      (string->path p)
      p))

;; First 8 hex chars of the campaign plan-id (SHA-256). Lenient: accepts any
;; string, uses what is there. make-wave-worktree! enforces the >= 8 contract.
(define (worktree-hash8 campaign-id)
  (substring (string-downcase campaign-id) 0 (min 8 (string-length campaign-id))))

(define (wave-worktree-dirname campaign-id wave-index)
  (format "~a~a-w~a" WORKTREE-DIRNAME-PREFIX (worktree-hash8 campaign-id) wave-index))

(define (wave-worktree-branch-name campaign-id wave-index)
  (format "campaign/~a/w~a" (worktree-hash8 campaign-id) wave-index))

;; Sibling placement: parent of the git toplevel, derived — never /tmp,
;; never inside the repo.
(define (wave-worktree-dir repo-root campaign-id wave-index)
  (define repo (->path repo-root))
  (define parent (path-only (path->complete-path repo)))
  (build-path parent (wave-worktree-dirname campaign-id wave-index)))

;; Full `git worktree add` argument vector (after "-C <repo>"), pure so the
;; command shape is pinnable without a git binary.
(define (wave-worktree-add-args dir branch base-ref)
  (list "worktree"
        "add"
        "-b"
        branch
        (if (string? dir)
            dir
            (path->string dir))
        base-ref))

;; ---- Git invocation seam --------------------------------------------------

(struct git-result (code stdout stderr) #:transparent)

(define (default-run-git repo-root args)
  (define git (find-executable-path "git"))
  (unless git
    (raise (exn:fail "make-wave-worktree!: git executable not found on PATH"
                     (current-continuation-marks))))
  (define out (open-output-string))
  (define err (open-output-string))
  (define repo (->path repo-root))
  (define code
    (parameterize ([current-output-port out]
                   [current-error-port err])
      (apply system*/exit-code git "-C" (path->string repo) args)))
  (git-result code (get-output-string out) (get-output-string err)))

;; Pure marker check (no subprocess): the supported layouts are
;; base-dir itself being a work tree, or base-dir/q being one — the same
;; precedence as go-orchestrator's find-git-root.
(define (git-marker-dir? d)
  (define g (build-path d ".git"))
  (or (directory-exists? g) (file-exists? g)))

(define (find-repo-root base-dir)
  (define base (->path base-dir))
  (define q-sub (build-path base "q"))
  (cond
    [(git-marker-dir? base) base]
    [(and (directory-exists? q-sub) (git-marker-dir? q-sub)) q-sub]
    [else #f]))

;; ---- Worktree record ------------------------------------------------------

;; path         — the worktree checkout (= executor session cwd)
;; planning-dir — .planning/ of the REAL project root: campaign state stays
;;                canonical/shared, never per-worktree
(struct wave-worktree (repo-root path branch base-ref planning-dir) #:transparent)

(define (wave-worktree-cwd wt)
  (wave-worktree-path wt))

;; ---- Lifecycle ------------------------------------------------------------

;; Create the wave worktree on a fresh branch off the current base ref
;; (origin/main by default — no network access, the local tracking ref).
;; Idempotent across crashes: campaign-scoped orphans are reclaimed first.
;; Raises exn:fail (with captured stderr) when git fails or the worktree
;; directory does not exist afterwards.
(define (make-wave-worktree! base-dir
                             #:campaign-id campaign-id
                             #:wave-index wave-index
                             #:repo-root [repo-root-in #f]
                             #:base-ref [base-ref "origin/main"]
                             #:run-git [run-git default-run-git])
  (define base (->path base-dir))
  (define repo (or (and repo-root-in (->path repo-root-in)) (find-repo-root base)))
  (unless repo
    (raise-argument-error 'make-wave-worktree!
                          "base-dir containing a git work tree (base-dir or base-dir/q)"
                          base-dir))
  (unless (and (string? campaign-id) (>= (string-length campaign-id) 8))
    (raise-argument-error 'make-wave-worktree!
                          "campaign-id string of at least 8 characters"
                          campaign-id))
  (unless (exact-nonnegative-integer? wave-index)
    (raise-argument-error 'make-wave-worktree! "exact-nonnegative-integer?" wave-index))
  ;; Crash recovery: reclaim THIS wave's stale worktree/branch first so a
  ;; leftover from a crashed attempt of the same (campaign, wave) can never
  ;; wedge creation. Scoped to this wave-index only — sibling waves of the
  ;; same campaign that are still live stay untouched.
  (reclaim-orphaned-worktrees! repo
                               #:campaign-id campaign-id
                               #:wave-index wave-index
                               #:run-git run-git)
  (define dir (wave-worktree-dir repo campaign-id wave-index))
  (define branch (wave-worktree-branch-name campaign-id wave-index))
  (define r (run-git repo (wave-worktree-add-args dir branch base-ref)))
  (unless (and (zero? (git-result-code r)) (directory-exists? dir))
    (raise (exn:fail (format "make-wave-worktree!: git worktree add failed (~a):\n  ~a"
                             (git-result-code r)
                             (string-trim (git-result-stderr r)))
                     (current-continuation-marks))))
  ;; BUG-0028 S2 (v1.00.19 W2): executor-start diagnostic — active worktree +
  ;; resolved allowed roots, one line, so staleness is visible immediately.
  (define wt (wave-worktree repo dir branch base-ref (build-path base ".planning")))
  (log-info (worktree-isolation-banner (wave-worktree-path wt)
                                       (list (wave-worktree-path wt)
                                             (wave-worktree-planning-dir wt))))
  wt)

;; Best-effort, NEVER raises, never masks the terminal outcome: remove the
;; worktree, then delete the branch (order matters — branch -D refuses while
;; the branch is checked out in a live worktree). Failure of either step is
;; logged and reported in the returned status hash.
(define (cleanup-wave-worktree! wt #:run-git [run-git default-run-git])
  (define (safe-run repo args)
    (with-handlers ([exn:fail? (lambda (e)
                                 (log-warning "cleanup-wave-worktree!: git invocation failed: ~a"
                                              (exn-message e))
                                 (git-result 127 "" (exn-message e)))])
      (run-git repo args)))
  (define repo (wave-worktree-repo-root wt))
  (define dir (wave-worktree-path wt))
  (define dir-str
    (if (string? dir)
        dir
        (path->string dir)))
  (define branch (wave-worktree-branch wt))
  (define r1
    (if (directory-exists? dir)
        (safe-run repo (list "worktree" "remove" "--force" dir-str))
        (git-result 0 "" ""))) ; already gone — nothing to remove
  (unless (zero? (git-result-code r1))
    (log-warning "cleanup-wave-worktree!: worktree remove failed for ~a: ~a"
                 dir-str
                 (string-trim (git-result-stderr r1)))
    (safe-run repo (list "worktree" "prune")))
  (define r2 (safe-run repo (list "branch" "-D" branch)))
  (unless (zero? (git-result-code r2))
    (log-warning "cleanup-wave-worktree!: branch delete failed for ~a: ~a"
                 branch
                 (string-trim (git-result-stderr r2))))
  (hasheq 'ok?
          (and (zero? (git-result-code r1)) (zero? (git-result-code r2)))
          'path
          dir-str
          'branch
          branch
          'removed-worktree?
          (zero? (git-result-code r1))
          'removed-branch?
          (zero? (git-result-code r2))))

;; v1.00.17 W7 (#9512b): delivered-wave release. The campaign branch IS the
;; delivery evidence — it is recorded in the campaign record (branch + head
;; SHA) and must SURVIVE the attempt so the operator or wave-finish flow can
;; merge/open a PR (no silent auto-merge in v1.00.17). Therefore: remove the
;; worktree checkout (nothing writes to it anymore) but KEEP the branch.
;; Best-effort like cleanup-wave-worktree!: never raises, never masks the
;; campaign outcome; failures are logged and reported in the status hash.
(define (release-wave-worktree! wt #:run-git [run-git default-run-git])
  (define (safe-run repo args)
    (with-handlers ([exn:fail? (lambda (e)
                                 (log-warning "release-wave-worktree!: git invocation failed: ~a"
                                              (exn-message e))
                                 (git-result 127 "" (exn-message e)))])
      (run-git repo args)))
  (define repo (wave-worktree-repo-root wt))
  (define dir (wave-worktree-path wt))
  (define dir-str
    (if (string? dir)
        dir
        (path->string dir)))
  (define branch (wave-worktree-branch wt))
  (define r1
    (if (directory-exists? dir)
        (safe-run repo (list "worktree" "remove" "--force" dir-str))
        (git-result 0 "" ""))) ; already gone — nothing to remove
  (unless (zero? (git-result-code r1))
    (log-warning "release-wave-worktree!: worktree remove failed for ~a: ~a"
                 dir-str
                 (string-trim (git-result-stderr r1)))
    (safe-run repo (list "worktree" "prune")))
  (hasheq 'ok?
          (zero? (git-result-code r1))
          'path
          dir-str
          'branch
          branch
          'removed-worktree?
          (zero? (git-result-code r1))
          'kept-branch?
          #t))

;; Campaign-start crash recovery: prune stale worktree registrations, then
;; remove every remaining worktree whose basename matches
;; wt-campaign-<hash8>-w<N> for this campaign (or ANY wt-campaign-* when no
;; campaign-id is given). With #:wave-index the sweep narrows to exactly that
;; wave — make-wave-worktree! uses this to self-heal a crashed attempt of the
;; SAME wave without disturbing live sibling waves. Non-campaign worktrees
;; (including the main checkout and unrelated branches) are never touched.
;; #:spare-branches names campaign branches whose worktree+branch must
;; SURVIVE the sweep — the coordinator passes the delivery branches already
;; recorded as approved in the campaign record (W7 #9512b: a delivered
;; branch is durable merge evidence and must never be crash-reclaimed).
;; Returns the reclaimed paths.
(define (reclaim-orphaned-worktrees! repo-root
                                     #:campaign-id [campaign-id #f]
                                     #:wave-index [wave-index #f]
                                     #:spare-branches [spare-branches '()]
                                     #:run-git [run-git default-run-git])
  (define repo (->path repo-root))
  (define (safe-run args)
    (with-handlers ([exn:fail? (lambda (e)
                                 (log-warning "reclaim-orphaned-worktrees!: git failed: ~a"
                                              (exn-message e))
                                 (git-result 127 "" (exn-message e)))])
      (run-git repo args)))
  (safe-run (list "worktree" "prune"))
  (define listing (safe-run (list "worktree" "list" "--porcelain")))
  (define-values (wt-paths live-branches)
    ;; one pass over the porcelain listing: collect worktree paths AND the
    ;; refs currently checked out in them (needed to spare live branches in
    ;; the orphaned-branch sweep below).
    (for/fold ([paths '()]
               [branches '()])
              ([line (in-list (string-split (git-result-stdout listing) "\n"))])
      (cond
        [(regexp-match? #rx"^worktree " line)
         (values (cons (string-trim (substring line (string-length "worktree "))) paths) branches)]
        [(regexp-match? #rx"^branch " line)
         (values paths (cons (string-trim (substring line (string-length "branch "))) branches))]
        [else (values paths branches)])))
  ;; A wave-index narrows the scope to exactly that wave (used by
  ;; make-wave-worktree! so re-creating wave N never destroys a sibling
  ;; wave N-1 worktree that is still live).
  (define suffix-rx
    (if wave-index
        (format "-w~a$" wave-index)
        "-w[0-9]+$"))
  (define wanted
    (if campaign-id
        (regexp (string-append "^" WORKTREE-DIRNAME-PREFIX (worktree-hash8 campaign-id) suffix-rx))
        (regexp (string-append "^" WORKTREE-DIRNAME-PREFIX "[^-]+" suffix-rx))))
  (define reclaimed-paths
    (for/fold ([reclaimed '()]) ([p (in-list wt-paths)])
      (define basename
        (let-values ([(_parent name _dir?) (split-path (string->path p))])
          (and (path? name) (path->string name))))
      (define m (and basename (regexp-match wanted basename)))
      (cond
        [(not m) reclaimed]
        [else
         (define branch
           (let ([mm (regexp-match #rx"^(.*)-w([0-9]+)$" basename)])
             (and mm
                  (format "campaign/~a/w~a"
                          (substring (cadr mm) (string-length WORKTREE-DIRNAME-PREFIX))
                          (caddr mm)))))
         (cond
           [(and branch (member branch spare-branches))
            ;; Delivered branch still holding a live worktree (crash between
            ;; approval and release): the branch is durable merge evidence.
            (log-info "reclaim-orphaned-worktrees!: sparing delivered branch ~a (~a)" branch p)
            reclaimed]
           [else
            (log-info "reclaim-orphaned-worktrees!: reclaiming ~a" p)
            (cleanup-wave-worktree!
             (wave-worktree repo (string->path p) (or branch "campaign/unknown/w0") #f #f)
             #:run-git run-git)
            (cons p reclaimed)])])))
  ;; Pass 2 — orphaned BRANCHES: the worktree directory was removed
  ;; out-of-band (crash, rm -rf), `prune` already dropped its registration,
  ;; but the campaign branch survived. Any campaign branch that is NOT
  ;; checked out in a live worktree is dead weight from a crashed attempt:
  ;; delete the branch and any leftover sibling directory. Scoped to the
  ;; campaign exactly like pass 1.
  (define branch-rx
    (if campaign-id
        (regexp (string-append "^refs/heads/campaign/"
                               (worktree-hash8 campaign-id)
                               (if wave-index
                                   (format "/w~a$" wave-index)
                                   "/w[0-9]+$")))
        (string-append "^refs/heads/campaign/[^/]+"
                       (if wave-index
                           (format "/w~a$" wave-index)
                           "/w[0-9]+$"))))
  (define refs (safe-run (list "for-each-ref" "--format=%(refname)" "refs/heads/campaign")))
  (for/fold ([reclaimed reclaimed-paths])
            ([ref (in-list (string-split (git-result-stdout refs) "\n"))]
             #:when (and (non-empty-string? ref) (regexp-match? branch-rx ref)))
    (cond
      [(member ref live-branches) reclaimed] ; still checked out somewhere
      [(member (substring ref (string-length "refs/heads/")) spare-branches)
       ;; Orphaned only in the sense of "no live worktree": the campaign
       ;; record marks this branch as delivered — keep it for merge (W7).
       (log-info "reclaim-orphaned-worktrees!: sparing delivered branch ~a"
                 (substring ref (string-length "refs/heads/")))
       reclaimed]
      [else
       (define short (substring ref (string-length "refs/heads/")))
       (define m (regexp-match #rx"^campaign/(.+)/w([0-9]+)$" short))
       (define dir
         (and m
              (build-path
               (let-values ([(parent _name _dir?) (split-path repo)])
                 parent)
               (format "~a~a-w~a" WORKTREE-DIRNAME-PREFIX (worktree-hash8 (cadr m)) (caddr m)))))
       (log-info "reclaim-orphaned-worktrees!: reclaiming orphaned branch ~a" short)
       (when (and dir (directory-exists? dir))
         (with-handlers ([exn:fail? (lambda (e)
                                      (log-warning
                                       "reclaim-orphaned-worktrees!: rmdir failed for ~a: ~a"
                                       (and dir (path->string dir))
                                       (exn-message e)))])
           (delete-directory/files dir)))
       (safe-run (list "branch" "-D" short))
       (if dir
           (cons (path->string dir) reclaimed)
           reclaimed)])))

;; ============================================================
;; Mid-wave checkpoint commits (v1.00.20 W4 — BUG-0030)
;;
;; Root cause: executor edits lived only as uncommitted working-tree
;; state until wave completion, so any infra stop mid-wave (observed
;; every 30-50 min during the v1.00.18 bake) stranded the work as
;; unreviewed residue in whatever checkout the executor used. Contract
;; change: the executor commits after EACH completed implementation step
;; with green tests, to the delivery branch, with the deterministic
;; `checkpoint: <step summary>` message.
;;
;; Checkpoints are NORMAL COMMITS: they carry recoverable progress, they
;; do NOT trigger delivery verification (the coordinator still verifies
;; the wave's FILES/TARGETS against the branch diff, never commit count),
;; and they are NOT the wave completion signal.
;; ============================================================

(define CHECKPOINT-COMMIT-PREFIX "checkpoint: ")

;; Pure: step summary → deterministic checkpoint commit message.
(define (wave-checkpoint-commit-message step-summary)
  (string-append CHECKPOINT-COMMIT-PREFIX (string-trim (if (string? step-summary) step-summary ""))))

;; Pure: is this commit message a checkpoint commit? Distinguishes
;; progress checkpoints (CHECKPOINT-COMMIT-PREFIX) from the final
;; delivery commit ("feat(<hash8>/w<N>): ..."), so consumers can count
;; checkpoints without confusing them with the delivery.
(define (checkpoint-commit-message? message)
  (and (string? message)
       (>= (string-length message) (string-length CHECKPOINT-COMMIT-PREFIX))
       (string-prefix? message CHECKPOINT-COMMIT-PREFIX)))

;; Commit any uncommitted state in `dir` (the wave worktree/checkout the
;; executor ran in) as a checkpoint commit: git add -A + git commit with
;; a hermetic identity (no global git config required) and the
;; deterministic checkpoint message. NEVER raises — a failed checkpoint
;; must never kill the attempt that made the progress: git failures are
;; logged and reported as #f. "Nothing to commit" is a successful no-op
;; (#t): the contract fires after green steps, and a step that produced
;; no diff has nothing to checkpoint.
(define (commit-wave-checkpoint! dir step-summary #:run-git [run-git default-run-git])
  (define dir-path (->path dir))
  (define dir-str (path->string dir-path))
  (define (safe-run args)
    (with-handlers ([exn:fail? (lambda (e)
                                 (log-warning "commit-wave-checkpoint!: git invocation failed: ~a"
                                              (exn-message e))
                                 (git-result 127 "" (exn-message e)))])
      (run-git dir-path args)))
  (cond
    [(not (directory-exists? dir-path)) #f]
    [else
     (define r-status (safe-run (list "status" "--porcelain")))
     (define clean?
       (and (zero? (git-result-code r-status))
            (string=? (string-trim (git-result-stdout r-status)) "")))
     (cond
       [clean? #t]
       [else
        (define r-add (safe-run (list "add" "-A")))
        (unless (zero? (git-result-code r-add))
          (log-warning "commit-wave-checkpoint!: git add -A failed in ~a: ~a"
                       dir-str
                       (string-trim (git-result-stderr r-add))))
        (define r-commit
          (safe-run (list "-c"
                          "user.name=gsd-checkpoint"
                          "-c"
                          "user.email=checkpoint@gsd.local"
                          "commit"
                          "-m"
                          (wave-checkpoint-commit-message step-summary))))
        (cond
          [(zero? (git-result-code r-commit)) #t]
          [(regexp-match? #rx"nothing to commit"
                          (string-append (git-result-stdout r-commit) (git-result-stderr r-commit)))
           #t]
          [else
           (log-warning "commit-wave-checkpoint!: git commit failed in ~a: ~a"
                        dir-str
                        (string-trim (git-result-stderr r-commit)))
           #f])])]))

;; Pure: executor-contract lines embedded verbatim in the wave prompt
;; environment block (command-handlers). Cadence is "after each completed
;; implementation step with green tests" so an infra stop mid-wave always
;; finds committed, discoverable progress on the delivery branch instead
;; of working-tree residue.
(define (checkpoint-contract-lines)
  (list
   "## Mid-Wave Checkpoint Contract (BUG-0030)\n"
   "- After EACH completed implementation step with green tests, commit to the delivery branch:\n"
   "  `git add -A && git commit -m \"checkpoint: <step summary>\"`\n"
   "- Checkpoints are normal commits: they do NOT trigger delivery verification, do NOT mark the\n"
   "  wave DONE, and never replace the final completion flow (run the wave's verify command, then return).\n"
   "- Keep checkpointing even if you expect to finish the wave: an infra stop mid-wave must find\n"
   "  committed progress, not uncommitted residue.\n\n"))
