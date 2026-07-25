#lang racket/base

;; tests/test-task-memory-projection.rkt
;; W4 (#8941): Projection — derives a coherent active-task-checkpoint from
;; task-ledger-events. This is the "always-on" memory that gets injected
;; into every provider request without requiring voluntary record_conclusion.

(require rackunit
         rackunit/text-ui
         racket/list
         "../runtime/task-memory/types.rkt"
         "../runtime/task-memory/projection.rkt")

;; ── Helpers ──

(define base-args
  (list 1 ; schema-version
        1 ; session-seq
        "ev-1" ; event-id
        "sess-1" ; session-id
        "proj-1" ; project-id
        "task-1" ; task-id
        #f ; parent-task-id
        "branch-1" ; branch-id
        "turn-1" ; turn-id
        "req-1" ; request-id
        "asm-1" ; assembly-id
        "corr-1" ; correlation-id
        #f ; causation-id
        'runtime-observed ; source-class
        ))

(define (make-ev kind payload #:evidence [evidence '()] #:digest [digest "d"])
  (apply make-task-ledger-event (append base-args (list kind payload 1000 evidence digest))))

(define (make-ev-seq seq kind payload)
  (apply make-task-ledger-event
         (list 1
               seq
               (string-append "ev-" (number->string seq))
               "sess-1"
               "proj-1"
               "task-1"
               #f
               "branch-1"
               "turn-1"
               "req-1"
               "asm-1"
               "corr-1"
               #f
               'runtime-observed
               kind
               payload
               (* 1000 seq)
               '()
               "d")))

(define (h . kvs)
  (for/hasheq ([i (in-range 0 (length kvs) 2)])
    (values (list-ref kvs i) (list-ref kvs (add1 i)))))

(define-test-suite
 projection-suite
 ;; ── Struct basics ──
 (test-case "project-empty-events produces empty checkpoint"
   (define cp (project-active-task-checkpoint '()))
   (check-true (active-task-checkpoint? cp))
   (check-false (active-task-checkpoint-objective cp))
   (check-false (active-task-checkpoint-current-phase cp))
   (check-equal? (active-task-checkpoint-owned-paths cp) '())
   (check-equal? (active-task-checkpoint-blockers cp) '()))
 ;; ── Objective extraction ──
 (test-case "extract-objective from objective-set payload"
   (define ev (make-ev 'objective-set (h 'summary "Ship feature X")))
   (define obj (extract-objective (list ev)))
   (check-equal? obj "Ship feature X"))
 (test-case "extract-objective picks newest when multiple"
   (define evs
     (list (make-ev-seq 1 'objective-set (h 'summary "Old goal"))
           (make-ev-seq 2 'objective-set (h 'summary "New goal"))))
   (check-equal? (extract-objective evs) "New goal"))
 (test-case "extract-objective returns #f when none"
   (check-false (extract-objective '())))
 ;; ── Owned paths extraction ──
 (test-case "extract-owned-paths dedups keeping newest"
   (define evs
     (list (make-ev-seq 1 'artifact-modified (h 'path "src/foo.rkt"))
           (make-ev-seq 2 'artifact-modified (h 'path "src/bar.rkt"))
           (make-ev-seq 3 'artifact-modified (h 'path "src/foo.rkt"))))
   (define paths (extract-owned-paths evs))
   ;; foo.rkt appears twice; dedup keeps the latest occurrence position
   (check-equal? (length paths) 2)
   (check-not-false (member "src/foo.rkt" paths))
   (check-not-false (member "src/bar.rkt" paths)))
 (test-case "extract-owned-paths includes artifact-created"
   (define evs (list (make-ev-seq 1 'artifact-created (h 'path "new.rkt"))))
   (check-equal? (extract-owned-paths evs) '("new.rkt")))
 ;; ── Verification state extraction ──
 (test-case "extract-verification-state: no tests → unknown"
   (check-equal? (extract-verification-state '()) 'unknown))
 (test-case "extract-verification-state: last pass → passing"
   (define evs
     (list (make-ev-seq 1 'verification-failed (h 'path "t1.rkt"))
           (make-ev-seq 2 'verification-passed (h 'path "t1.rkt"))))
   (check-equal? (extract-verification-state evs) 'passing))
 (test-case "extract-verification-state: last fail → failing"
   (define evs
     (list (make-ev-seq 1 'verification-passed (h 'path "t1.rkt"))
           (make-ev-seq 2 'verification-failed (h 'path "t1.rkt"))))
   (check-equal? (extract-verification-state evs) 'failing))
 ;; ── Blockers extraction ──
 (test-case "extract-blockers from error-occurred"
   (define evs (list (make-ev-seq 1 'error-occurred (h 'message "OOM" 'tool "bash"))))
   (define bl (extract-blockers evs))
   (check-equal? (length bl) 1)
   (check-equal? (blocker-message (first bl)) "OOM"))
 (test-case "extract-blockers from verification-failed"
   (define evs (list (make-ev-seq 1 'verification-failed (h 'path "t.rkt" 'summary "2 failures"))))
   (define bl (extract-blockers evs))
   (check-equal? (length bl) 1))
 (test-case "extract-blockers clears after a passing test"
   (define evs
     (list (make-ev-seq 1 'verification-failed (h 'path "t.rkt"))
           (make-ev-seq 2 'error-occurred (h 'message "err"))
           (make-ev-seq 3 'verification-passed (h 'path "t.rkt"))))
   ;; A pass clears blockers from BEFORE it; errors after the pass remain.
   (define bl (extract-blockers evs))
   ;; The error-occurred is seq 2, before the pass at seq 3 → cleared
   (check-equal? bl '()))
 (test-case "extract-blockers keeps errors after last pass"
   (define evs
     (list (make-ev-seq 1 'verification-passed (h 'path "t.rkt"))
           (make-ev-seq 2 'error-occurred (h 'message "late err"))))
   (define bl (extract-blockers evs))
   (check-equal? (length bl) 1)
   (check-equal? (blocker-message (first bl)) "late err"))
 ;; ── Current phase extraction ──
 (test-case "extract-current-phase from latest phase-changed"
   (define evs
     (list (make-ev-seq 1 'phase-changed (h 'to-state "exploration"))
           (make-ev-seq 2 'phase-changed (h 'to-state "implementation"))))
   (check-equal? (extract-current-phase evs) "implementation"))
 (test-case "extract-current-phase from task-started"
   (define evs (list (make-ev-seq 1 'task-started (h 'to-state "exploration"))))
   (check-equal? (extract-current-phase evs) "exploration"))
 (test-case "extract-current-phase #f when none"
   (check-false (extract-current-phase '())))
 ;; ── GSD wave extraction ──
 (test-case "extract-gsd-wave from wave-started"
   (define evs (list (make-ev-seq 1 'wave-started (h 'from-state "plan-written"))))
   (check-not-false (extract-gsd-wave evs)))
 (test-case "extract-gsd-wave #f when none"
   (check-false (extract-gsd-wave '())))
 ;; ── Completed work extraction ──
 (test-case "extract-completed-work from commits"
   (define evs (list (make-ev-seq 1 'commit-created (h 'sha "abc123" 'summary "fix X"))))
   (define work (extract-completed-work evs))
   (check-equal? (length work) 1)
   (check-equal? (work-evidence-kind (first work)) 'commit))
 ;; ── Next action derivation ──
 (test-case "derive-next-action: implementation phase → verify"
   (check-equal? (derive-next-action "implementation") "verify"))
 (test-case "derive-next-action: exploration phase → plan"
   (check-equal? (derive-next-action "exploration") "plan"))
 (test-case "derive-next-action: unknown → continue"
   (check-equal? (derive-next-action "weird") "continue"))
 (test-case "derive-next-action: #f phase → continue"
   (check-equal? (derive-next-action #f) "continue"))
 ;; ── Full projection ──
 (test-case "project-active-task-checkpoint integrates all fields"
   (define evs
     (list (make-ev-seq 1 'objective-set (h 'summary "Ship W4"))
           (make-ev-seq 2 'task-started (h 'to-state "exploration"))
           (make-ev-seq 3 'phase-changed (h 'to-state "implementation"))
           (make-ev-seq 4 'artifact-modified (h 'path "src/a.rkt"))
           (make-ev-seq 5 'commit-created (h 'sha "abc" 'summary "w4 impl"))
           (make-ev-seq 6 'verification-failed (h 'path "t.rkt"))
           (make-ev-seq 7 'verification-passed (h 'path "t.rkt"))))
   (define cp (project-active-task-checkpoint evs))
   (check-equal? (active-task-checkpoint-objective cp) "Ship W4")
   (check-equal? (active-task-checkpoint-current-phase cp) "implementation")
   (check-equal? (active-task-checkpoint-owned-paths cp) '("src/a.rkt"))
   (check-equal? (active-task-checkpoint-verification-state cp) 'passing)
   (check-equal? (active-task-checkpoint-blockers cp) '())
   (check-equal? (length (active-task-checkpoint-completed-work cp)) 1)
   (check-equal? (active-task-checkpoint-event-count cp) 7)
   (check-equal? (active-task-checkpoint-next-action cp) "verify"))
 ;; ── Ordering: events sorted by session-seq ──
 (test-case "project handles unsorted events"
   (define evs
     (list (make-ev-seq 3 'phase-changed (h 'to-state "implementation"))
           (make-ev-seq 1 'objective-set (h 'summary "goal"))
           (make-ev-seq 2 'task-started (h 'to-state "exploration"))))
   (define cp (project-active-task-checkpoint evs))
   (check-equal? (active-task-checkpoint-objective cp) "goal")
   (check-equal? (active-task-checkpoint-current-phase cp) "implementation")))

(run-tests projection-suite)
