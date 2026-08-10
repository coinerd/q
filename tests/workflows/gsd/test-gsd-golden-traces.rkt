#lang racket/base

;; @speed fast  ;; @suite workflows

;; BOUNDARY: integration

;; tests/workflows/gsd/test-gsd-golden-traces.rkt — v0.99.89 W0
;;
;; Deterministic semantic golden trace matrix for GSD workflows. Pins the
;; semantics of plan creation, /go, wave success/failure/interruption,
;; retry, replan, campaign resume and milestone close, plus failure
;; injection for a crash between commit and projection.
;;
;; Every trace covers: commands, FSM transitions, campaign record, PLAN/
;; STATE/wave projections, completion outbox, campaign result and event
;; order. Nondeterministic fields are excluded by construction (see
;; helpers/gsd-golden-trace.rkt), so each scenario yields one golden trace.
;; This matrix is the refactoring oracle for W1–W4: behavioral equivalence
;; is proven by trace comparison.

(require rackunit
         racket/string
         racket/list
         racket/match
         "../../helpers/gsd-golden-trace.rkt"
         (only-in "../../../extensions/gsd/campaign-state.rkt"
                  migrate-campaign!
                  load-campaign-record
                  campaign-plan-id))

;; ============================================================
;; Assertion helpers
;; ============================================================

(define (trace-wave trace idx)
  (for/first ([w (hash-ref (golden-trace-record trace) 'waves)]
              #:when (= (car w) idx))
    w))

(define (trace-projection trace key idx)
  (for/first ([p (hash-ref (golden-trace-projections trace) key)]
              #:when (= (car p) idx))
    p))

(define (trace-event-names trace)
  (map car (golden-trace-events trace)))

(define (transition-succeeded-payloads trace)
  (for/list ([e (golden-trace-events trace)]
             #:when (eq? (car e) 'gsd.transition.succeeded))
    (cdr e)))

(define (hex-plan-id? s)
  (and (= (string-length s) 64) (regexp-match? #rx"^[0-9a-f]+$" s)))

;; ============================================================
;; plan creation
;; ============================================================

(test-case "golden plan-creation: durable output of a planning turn seeds a campaign"
  (define trace (with-golden-trace 'plan-creation '((/plan)) scenario-plan-creation))
  (define rec (golden-trace-record trace))
  (check-equal? (golden-trace-commands trace) '((/plan)))
  (check-true (hex-plan-id? (hash-ref rec 'plan-id)))
  (check-equal? (hash-ref rec 'fence) 0)
  (check-equal? (hash-ref rec 'provenance) 'plan-and-state)
  (check-false (hash-ref rec 'cancellation))
  (check-equal? (hash-ref rec 'waves) '((0 pending 0 #f) (1 pending 0 #f)))
  (check-equal? (hash-ref (golden-trace-projections trace) 'plan-index) '((0 pending) (1 pending)))
  (check-equal? (hash-ref (golden-trace-projections trace) 'wave-docs) '((0 pending) (1 pending)))
  (check-equal? (hash-ref (golden-trace-projections trace) 'state-table) '((0 pending) (1 pending)))
  (check-equal? (hash-ref (golden-trace-projections trace) 'validation-table)
                '((0 pending) (1 pending)))
  (check-equal? (hash-ref (golden-trace-projections trace) 'plan-overall) 'in-progress)
  (check-equal? (golden-trace-outbox trace) '())
  (check-equal? (golden-trace-fsm trace) '())
  (check-equal? (golden-trace-final-mode trace) 'idle)
  (check-equal? (golden-trace-events trace) '()))

(test-case "golden plan-creation: trace is deterministic across runs"
  (check-equal? (with-golden-trace 'plan-creation '((/plan)) scenario-plan-creation)
                (with-golden-trace 'plan-creation '((/plan)) scenario-plan-creation)))

;; ============================================================
;; /go — wave success
;; ============================================================

(test-case "golden go-success: both waves complete, campaign-complete"
  (define trace (with-golden-trace 'go-success '((/plan) (/go)) scenario-go-success))
  (check-equal? (hash-ref (golden-trace-result trace) 'status) 'campaign-complete)
  (check-equal? (hash-ref (golden-trace-result trace) 'completed) '(0 1))
  (check-equal? (trace-wave trace 0) '(0 done 1 ("attempt-1" 1)))
  (check-equal? (trace-wave trace 1) '(1 done 1 ("attempt-1" 2)))
  (check-equal? (hash-ref (golden-trace-projections trace) 'plan-index) '((0 done) (1 done)))
  (check-equal? (hash-ref (golden-trace-projections trace) 'wave-docs) '((0 done) (1 done)))
  (check-equal? (hash-ref (golden-trace-projections trace) 'state-table) '((0 done) (1 done)))
  (check-equal? (hash-ref (golden-trace-projections trace) 'validation-table)
                '((0 pending) (1 pending)))
  (check-equal? (hash-ref (golden-trace-projections trace) 'plan-overall) 'all-done)
  (check-equal? (length (golden-trace-outbox trace)) 2)
  (for ([event-id (in-list (golden-trace-outbox trace))])
    (check-regexp-match #rx"^campaign/[0-9a-f]+/wave/[01]/attempt/attempt-[0-9]+/completed$"
                        event-id)))

(test-case "golden go-success: FSM auto-routes through exploring to executing/verifying"
  (define trace (with-golden-trace 'go-success '((/plan) (/go)) scenario-go-success))
  (check-equal? (golden-trace-fsm trace)
                '((idle exploring) (exploring plan-written)
                                   (plan-written executing)
                                   (executing verifying)
                                   (verifying executing)
                                   (executing verifying)))
  (check-equal? (golden-trace-final-mode trace) 'verifying))

(test-case "golden go-success: event order interleaves attempted/succeeded per hop"
  (define trace (with-golden-trace 'go-success '((/plan) (/go)) scenario-go-success))
  (define hop-pattern '(gsd.transition.attempted gsd.transition.succeeded))
  (check-equal? (trace-event-names trace)
                (append hop-pattern hop-pattern hop-pattern hop-pattern hop-pattern hop-pattern))
  ;; Succeeded payloads (from to) equal the FSM transitions.
  (check-equal? (transition-succeeded-payloads trace) (golden-trace-fsm trace)))

(test-case "golden go-success: trace is deterministic across runs"
  (check-equal? (with-golden-trace 'go-success '((/plan) (/go)) scenario-go-success)
                (with-golden-trace 'go-success '((/plan) (/go)) scenario-go-success)))

;; ============================================================
;; wave failure (verifier-first + runner error)
;; ============================================================

(test-case "golden go-verifier-reject: DONE never committed without approval"
  (define trace (with-golden-trace 'go-verifier-reject '((/plan) (/go)) scenario-go-verifier-reject))
  (check-equal? (hash-ref (golden-trace-result trace) 'status) 'wave-failed)
  (check-equal? (hash-ref (golden-trace-result trace) 'completed) '())
  (check-equal? (trace-wave trace 0) '(0 failed 1 ("attempt-1" 1)))
  (check-equal? (trace-wave trace 1) '(1 pending 0 #f))
  (check-equal? (golden-trace-outbox trace) '())
  (check-equal? (hash-ref (golden-trace-projections trace) 'plan-index) '((0 failed) (1 pending)))
  (check-equal? (hash-ref (golden-trace-projections trace) 'state-table) '((0 failed) (1 pending)))
  (check-equal? (hash-ref (golden-trace-projections trace) 'plan-overall) 'partly-done))

(test-case "golden go-failure: runner error fails the wave and stops advancement"
  (define trace (with-golden-trace 'go-failure '((/plan) (/go)) scenario-go-failure))
  (check-equal? (hash-ref (golden-trace-result trace) 'status) 'wave-failed)
  (check-equal? (hash-ref (golden-trace-result trace) 'completed) '())
  (check-equal? (trace-wave trace 0) '(0 failed 1 ("attempt-1" 1)))
  (check-equal? (trace-wave trace 1) '(1 pending 0 #f))
  (check-equal? (hash-ref (golden-trace-projections trace) 'wave-docs) '((0 failed) (1 pending)))
  (check-equal? (golden-trace-outbox trace) '())
  ;; No verifier ran: FSM stops in executing after the prompt.
  (check-equal? (golden-trace-fsm trace)
                '((idle exploring) (exploring plan-written) (plan-written executing)))
  (check-equal? (golden-trace-final-mode trace) 'executing))

;; ============================================================
;; wave interruption
;; ============================================================

(test-case "golden go-interruption: interrupted wave is durable-only, projections stay pending"
  (define trace (with-golden-trace 'go-interruption '((/plan) (/go)) scenario-go-interruption))
  (check-equal? (hash-ref (golden-trace-result trace) 'status) 'wave-cancelled)
  (check-equal? (hash-ref (golden-trace-result trace) 'completed) '())
  ;; Record: interrupted with a real attempt.
  (check-equal? (trace-wave trace 0) '(0 interrupted 1 ("attempt-1" 1)))
  (check-equal? (trace-wave trace 1) '(1 pending 0 #f))
  ;; Projections were never touched: still pre-completion.
  (check-equal? (hash-ref (golden-trace-projections trace) 'plan-index) '((0 pending) (1 pending)))
  (check-equal? (hash-ref (golden-trace-projections trace) 'wave-docs) '((0 pending) (1 pending)))
  (check-equal? (hash-ref (golden-trace-projections trace) 'state-table) '((0 pending) (1 pending)))
  (check-equal? (hash-ref (golden-trace-projections trace) 'validation-table)
                '((0 pending) (1 pending)))
  (check-equal? (golden-trace-outbox trace) '())
  (check-equal? (golden-trace-final-mode trace) 'executing))

;; ============================================================
;; retry
;; ============================================================

(test-case "golden retry-interrupted: new attempt on interrupted wave, fence advances"
  (define trace
    (with-golden-trace 'retry-interrupted '((/plan) (/go) (/go)) scenario-retry-interrupted))
  (check-equal? (hash-ref (golden-trace-result trace) 'status) 'campaign-complete)
  (check-equal? (hash-ref (golden-trace-result trace) 'completed) '(0 1))
  ;; W0 retried: attempt-2 under fence-2; W1 attempted once under fence-3.
  (check-equal? (trace-wave trace 0) '(0 done 2 ("attempt-2" 2)))
  (check-equal? (trace-wave trace 1) '(1 done 1 ("attempt-1" 3)))
  (check-equal? (hash-ref (golden-trace-projections trace) 'plan-index) '((0 done) (1 done)))
  (check-equal? (length (golden-trace-outbox trace)) 2))

;; ============================================================
;; campaign resume
;; ============================================================

(test-case "golden campaign-resume: durable record carries truth across processes"
  (define trace (with-golden-trace 'campaign-resume '((/plan) (/go) (/go)) scenario-campaign-resume))
  (check-equal? (hash-ref (golden-trace-result trace) 'status) 'campaign-complete)
  ;; Each process reports the waves it completed in this run (resume run
  ;; completed only W1).
  (check-equal? (hash-ref (golden-trace-result trace) 'completed) '(1))
  ;; W0 completed in the first process (attempt-1 fence-1); W1 was failed and
  ;; retried in the second process (attempt-2 fence-3).
  (check-equal? (trace-wave trace 0) '(0 done 1 ("attempt-1" 1)))
  (check-equal? (trace-wave trace 1) '(1 done 2 ("attempt-2" 3)))
  (check-equal? (hash-ref (golden-trace-projections trace) 'plan-index) '((0 done) (1 done)))
  (check-equal? (length (golden-trace-outbox trace)) 2)
  ;; FSM accumulates across both processes (first run stops in executing after
  ;; W1's failed prompt; second run continues to verifying).
  (check-equal? (golden-trace-fsm trace)
                '((idle exploring) (exploring plan-written)
                                   (plan-written executing)
                                   (executing verifying)
                                   (verifying executing)
                                   (executing verifying))))

;; ============================================================
;; replan
;; ============================================================

(test-case "golden replan: new plan seeds a fresh campaign identity"
  (define trace (with-golden-trace 'replan '((/plan) (/go) (/replan)) scenario-replan))
  (define rec (golden-trace-record trace))
  (define old-id (hash-ref (golden-trace-result trace) 'old-plan-id))
  (check-true (hex-plan-id? old-id))
  (check-not-equal? (hash-ref rec 'plan-id) old-id)
  (check-equal? (hash-ref rec 'provenance) 'plan-and-state)
  (check-equal? (hash-ref rec 'waves) '((0 pending 0 #f)))
  (check-equal? (hash-ref (golden-trace-projections trace) 'plan-index) '((0 pending)))
  (check-equal? (hash-ref (golden-trace-projections trace) 'plan-overall) 'in-progress)
  (check-equal? (golden-trace-outbox trace) '())
  ;; The old campaign record file is preserved on disk (replan never deletes).
  (check-true (hash-ref (golden-trace-result trace) 'old-record-preserved)))

;; ============================================================
;; milestone close
;; ============================================================

(test-case "golden milestone-close: /done archives the completed plan and resets the FSM"
  (define trace
    (with-golden-trace 'milestone-close '((/plan) (/go) (/done)) scenario-milestone-close))
  (define result (golden-trace-result trace))
  ;; The campaign ran to completion first.
  (check-equal? (hash-ref (hash-ref result 'campaign) 'status) 'campaign-complete)
  (check-equal? (hash-ref (hash-ref result 'campaign) 'completed) '(0 1))
  ;; The production archive path succeeded and moved the projections.
  (check-true (hash-ref result 'archive-success))
  (check-true (hash-ref result 'archive-dir-exists))
  (check-true (hash-ref result 'archive-moved-plan))
  (check-true (hash-ref result 'projections-cleared))
  ;; The durable record and outbox survive the archive.
  (check-equal? (trace-wave trace 0) '(0 done 1 ("attempt-1" 1)))
  (check-equal? (trace-wave trace 1) '(1 done 1 ("attempt-1" 2)))
  (check-equal? (length (golden-trace-outbox trace)) 2)
  ;; reset-gsd-after-archive! clears the FSM to a fresh idle (history cleared).
  (check-equal? (golden-trace-fsm trace) '())
  (check-equal? (golden-trace-final-mode trace) 'idle)
  ;; The archived event is last in the event order.
  (check-equal? (last (trace-event-names trace)) 'gsd.plan.archived))

;; ============================================================
;; failure injection: crash between commit and projection
;; ============================================================

(test-case "golden crash-between-commit-and-projection: durable commit with stale projections"
  (define trace
    (with-golden-trace 'crash-between-commit-and-projection
                       '((/plan) (/go))
                       scenario-crash-between-commit-and-projection))
  (define rec (golden-trace-record trace))
  ;; Durable truth: W0 committed done (attempt-1 fence-1) + outbox event;
  ;; W1 failed (attempt-1 fence-2).
  (check-equal? (trace-wave trace 0) '(0 done 1 ("attempt-1" 1)))
  (check-equal? (trace-wave trace 1) '(1 failed 1 ("attempt-1" 2)))
  (check-equal? (length (golden-trace-outbox trace)) 1)
  ;; Projections: crash happened before the projection update — stale.
  (check-equal? (hash-ref (golden-trace-projections trace) 'plan-index) '((0 pending) (1 pending)))
  (check-equal? (hash-ref (golden-trace-projections trace) 'wave-docs) '((0 pending) (1 pending)))
  (check-equal? (hash-ref (golden-trace-projections trace) 'state-table) '((0 pending) (1 pending))))

(test-case "golden crash resume: W0 projections remain stale after recovery"
  ;; Simulate the crash, then resume in a fresh process: W1 completes but the
  ;; stale W0 projections are NOT repaired by today's resume path.
  (define crash
    (with-golden-trace 'crash-between-commit-and-projection
                       '((/plan) (/go))
                       scenario-crash-between-commit-and-projection))
  (define trace
    (with-golden-trace 'crash-resume
                       '((/plan) (/go))
                       (lambda (dir ctx)
                         (define rec (migrate-campaign! dir))
                         ;; commit + crash (as above)
                         (run-golden-request! dir rec ctx #:outcomes '(ok error))
                         (seed-golden-project! dir)
                         ;; resume in a fresh process
                         (define rec2 (load-campaign-record dir (campaign-plan-id rec)))
                         (define result (run-golden-request! dir rec2 ctx #:outcomes '(ok)))
                         (values rec2 result))))
  (check-equal? (hash-ref (golden-trace-result trace) 'status) 'campaign-complete)
  ;; The resume process completed only W1; W0 was done before the crash.
  (check-equal? (hash-ref (golden-trace-result trace) 'completed) '(1))
  (check-equal? (trace-wave trace 0) '(0 done 1 ("attempt-1" 1)))
  (check-equal? (trace-wave trace 1) '(1 done 2 ("attempt-2" 3)))
  ;; W1 projection updated by resume; W0 still stale (pending) — no repair.
  (check-equal? (hash-ref (golden-trace-projections trace) 'plan-index) '((0 pending) (1 done)))
  (check-equal? (hash-ref (golden-trace-projections trace) 'wave-docs) '((0 pending) (1 done)))
  (check-equal? (hash-ref (golden-trace-projections trace) 'state-table) '((0 pending) (1 done)))
  ;; Durable record is coherent (both done) but the stale W0 projection keeps
  ;; the plan overall at partly-done — that is the current crash semantic.
  (check-equal? (hash-ref (golden-trace-projections trace) 'plan-overall) 'partly-done)
  (check-equal? (length (golden-trace-outbox trace)) 2)
  ;; Sanity: the crash trace (pre-resume) showed W0 stale too.
  (check-equal? (hash-ref (golden-trace-projections crash) 'plan-index) '((0 pending) (1 pending))))
