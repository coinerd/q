#lang racket/base

;; runtime/task-memory/projection.rkt
;; STABILITY: evolving
;;
;; W4 (#8941): Projection — derives a coherent active-task-checkpoint from
;; task-ledger-events. This is the "always-on" memory that gets injected
;; into every provider request WITHOUT requiring voluntary record_conclusion.
;;
;; The projection is a PURE FUNCTION: ledger events → checkpoint struct.
;; It does not perform I/O and does not depend on mutable state.
;;
;; Security posture (per W4 issue):
;;   - Objective/constraints derived from runtime-observed events, not assertions
;;   - References evidence (sha/commit/path) rather than copying raw tool output
;;   - Owned paths come from artifact-modified/created events only
;;   - Verification state reflects the latest observed test outcome
;;   - Blockers are unresolved failures occurring AFTER the last pass
;;
;; Layering: RUNTIME module importing types (ledger) only. No downstream deps.

(require racket/contract
         racket/list
         racket/match
         "types.rkt")

(provide active-task-checkpoint
         active-task-checkpoint?
         active-task-checkpoint-objective
         active-task-checkpoint-constraints
         active-task-checkpoint-current-phase
         active-task-checkpoint-gsd-wave
         active-task-checkpoint-owned-paths
         active-task-checkpoint-completed-work
         active-task-checkpoint-verification-state
         active-task-checkpoint-workspace-revision
         active-task-checkpoint-blockers
         active-task-checkpoint-next-action
         active-task-checkpoint-next-trigger
         active-task-checkpoint-event-count
         blocker
         blocker?
         blocker-kind
         blocker-message
         blocker-source
         work-evidence
         work-evidence?
         work-evidence-kind
         work-evidence-ref
         work-evidence-summary
         project-active-task-checkpoint
         extract-objective
         extract-owned-paths
         extract-verification-state
         extract-blockers
         extract-current-phase
         extract-gsd-wave
         extract-completed-work
         derive-next-action
         derive-next-trigger)

;; ============================================================
;; Result structs
;; ============================================================

(struct blocker (kind message source) #:transparent)
(struct work-evidence (kind ref summary) #:transparent)

(struct active-task-checkpoint
        (objective ; string | #f
         constraints ; (listof string)
         current-phase ; string | #f
         gsd-wave ; any/c | #f  (the latest wave-started event)
         owned-paths ; (listof string) — newest-first, deduped
         completed-work ; (listof work-evidence)
         verification-state ; 'passing | 'failing | 'unknown
         workspace-revision ; any/c | #f
         blockers ; (listof blocker)
         next-action ; string | #f
         next-trigger ; symbol | #f
         event-count ; integer
         )
  #:transparent)

;; ============================================================
;; Event helpers
;; ============================================================

;; Sort events by session-seq ascending (stable for equal seqs).
(define (sort-events evs)
  (sort evs < #:key task-ledger-event-session-seq))

;; Predicate helpers by event-kind.
(define (objective-set-ev? e)
  (eq? (task-ledger-event-event-kind e) 'objective-set))
(define (artifact-modified-ev? e)
  (memq (task-ledger-event-event-kind e) '(artifact-modified artifact-created)))
(define (verification-ev? e)
  (memq (task-ledger-event-event-kind e) '(verification-passed verification-failed)))
(define (phase-ev? e)
  (memq (task-ledger-event-event-kind e) '(phase-changed task-started)))
(define (error-ev? e)
  (eq? (task-ledger-event-event-kind e) 'error-occurred))
(define (commit-ev? e)
  (eq? (task-ledger-event-event-kind e) 'commit-created))
(define (wave-started-ev? e)
  (eq? (task-ledger-event-event-kind e) 'wave-started))
(define (constraint-ev? e)
  (eq? (task-ledger-event-event-kind e) 'constraint-set))

;; ============================================================
;; Objective extraction
;; ============================================================

;; The objective is the summary of the NEWEST objective-set event.
(define (extract-objective evs)
  (define sorted (sort-events (filter objective-set-ev? evs)))
  (if (null? sorted)
      #f
      (hash-ref (task-ledger-event-payload (last sorted)) 'summary #f)))

;; ============================================================
;; Constraints extraction
;; ============================================================

(define (extract-constraints evs)
  (define sorted (sort-events (filter constraint-ev? evs)))
  (for/list ([e (in-list sorted)])
    (hash-ref (task-ledger-event-payload e) 'text "")))

;; ============================================================
;; Owned paths extraction (dedup, newest position)
;; ============================================================

(define (extract-owned-paths evs)
  (define sorted (sort-events (filter artifact-modified-ev? evs)))
  ;; Walk newest-first, collecting unseen paths.
  (define reversed (reverse sorted))
  (let loop ([evs reversed]
             [seen '()]
             [acc '()])
    (cond
      [(null? evs) acc]
      [else
       (define e (car evs))
       (define p (hash-ref (task-ledger-event-payload e) 'path #f))
       (cond
         [(and p (not (member p seen))) (loop (cdr evs) (cons p seen) (append acc (list p)))]
         [else (loop (cdr evs) seen acc)])])))

;; ============================================================
;; Verification state extraction
;; ============================================================

(define (extract-verification-state evs)
  (define sorted (sort-events (filter verification-ev? evs)))
  (if (null? sorted)
      'unknown
      (let ([last-ev (last sorted)])
        (if (eq? (task-ledger-event-event-kind last-ev) 'verification-passed) 'passing 'failing))))

;; ============================================================
;; Blockers extraction
;; ============================================================

;; Blockers are unresolved failures: error-occurred or verification-failed
;; events that occur AFTER the most recent verification-passed event.
(define (extract-blockers evs)
  (define sorted (sort-events evs))
  ;; Find the index of the last verification-passed event.
  (define pass-idx
    (for/last ([e (in-list sorted)]
               [i (in-naturals)]
               #:when (eq? (task-ledger-event-event-kind e) 'verification-passed))
      i))
  (define cutoff
    (if pass-idx
        (add1 pass-idx)
        0))
  (define candidates (drop sorted cutoff))
  (define result '())
  (let loop ([evs candidates]
             [acc '()])
    (cond
      [(null? evs) (reverse acc)]
      [else
       (define e (car evs))
       (define k (task-ledger-event-event-kind e))
       (define bl
         (cond
           [(eq? k 'error-occurred)
            (blocker 'error
                     (hash-ref (task-ledger-event-payload e) 'message "error")
                     (hash-ref (task-ledger-event-payload e) 'tool #f))]
           [(eq? k 'verification-failed)
            (blocker 'test-failure
                     (hash-ref (task-ledger-event-payload e) 'summary "tests failed")
                     (hash-ref (task-ledger-event-payload e) 'path #f))]
           [else #f]))
       (loop (cdr evs)
             (if bl
                 (cons bl acc)
                 acc))])))

;; ============================================================
;; Current phase extraction
;; ============================================================

(define (extract-current-phase evs)
  (define sorted (sort-events (filter phase-ev? evs)))
  (if (null? sorted)
      #f
      (hash-ref (task-ledger-event-payload (last sorted)) 'to-state #f)))

;; ============================================================
;; GSD wave extraction
;; ============================================================

(define (extract-gsd-wave evs)
  (define sorted (sort-events (filter wave-started-ev? evs)))
  (if (null? sorted)
      #f
      (last sorted)))

;; ============================================================
;; Completed work extraction
;; ============================================================

(define (extract-completed-work evs)
  (define sorted (sort-events (filter commit-ev? evs)))
  (for/list ([e (in-list sorted)])
    (define pl (task-ledger-event-payload e))
    (work-evidence 'commit (hash-ref pl 'sha #f) (hash-ref pl 'summary #f))))

;; ============================================================
;; Next action derivation
;; ============================================================

;; Derive the next intended action from the current phase.
;; This is a heuristic mapping, not an assertion.
(define (derive-next-action phase)
  (cond
    [(not phase) "continue"]
    [(string=? phase "exploration") "plan"]
    [(string=? phase "planning") "execute"]
    [(string=? phase "implementation") "verify"]
    [(string=? phase "verification") "review-or-rework"]
    [(string=? phase "debugging") "fix-and-retest"]
    [else "continue"]))

(define (derive-next-trigger phase)
  (cond
    [(not phase) #f]
    [(string=? phase "exploration") 'plan]
    [(string=? phase "planning") 'execute]
    [(string=? phase "implementation") 'verify]
    [(string=? phase "verification") 'done]
    [(string=? phase "debugging") 'retest]
    [else #f]))

;; ============================================================
;; Full projection
;; ============================================================

(define (project-active-task-checkpoint evs)
  (define phase (extract-current-phase evs))
  (active-task-checkpoint (extract-objective evs)
                          (extract-constraints evs)
                          phase
                          (extract-gsd-wave evs)
                          (extract-owned-paths evs)
                          (extract-completed-work evs)
                          (extract-verification-state evs)
                          #f ; workspace-revision (injected by caller, not derived from events)
                          (extract-blockers evs)
                          (derive-next-action phase)
                          (derive-next-trigger phase)
                          (length evs)))
