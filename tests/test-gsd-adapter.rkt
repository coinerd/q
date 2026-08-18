#lang racket/base

;; @speed fast  ;; @suite extensions
;; @boundary unit
;; tests/test-gsd-adapter.rkt
;; W3C (#8940): GSD adapter — translates GSD state-machine transitions,
;; task-state transitions, conclusions, and checkpoints into
;; task-ledger-events.
;;
;; This is the "state-aware" capture: it observes GSD lifecycle signals
;; (not tool outcomes) and emits durable ledger events that record the
;; agent's workflow progression.

(require rackunit
         rackunit/text-ui
         racket/list
         file/sha1
         "../util/ids.rkt"
         "../runtime/task-memory/types.rkt"
         "../runtime/task-memory/outcome-capture.rkt"
         "../runtime/task-memory/gsd-adapter.rkt")

(define ctx
  (make-capture-context #:session-id "sess-1"
                        #:project-id "proj-1"
                        #:task-id "task-1"
                        #:parent-task-id #f
                        #:branch-id "branch-1"
                        #:turn-id "turn-1"
                        #:request-id "req-1"
                        #:assembly-id "asm-1"
                        #:session-seq 3))

(define-test-suite
 gsd-adapter-suite
 ;; ── GSD transition → event-kind mapping ──
 (test-case "idle → exploring → phase-changed"
   (check-equal? (gsd-transition->event-kind 'idle 'exploring 'explore) 'phase-changed))
 (test-case "exploring → plan-written → objective-set"
   (check-equal? (gsd-transition->event-kind 'exploring 'plan-written 'plan) 'objective-set))
 (test-case "exploring → idle (cancel) → cancelled"
   (check-equal? (gsd-transition->event-kind 'exploring 'idle 'cancel) 'cancelled))
 (test-case "plan-written → executing → wave-started"
   (check-equal? (gsd-transition->event-kind 'plan-written 'executing 'execute) 'wave-started))
 (test-case "plan-written → idle (cancel) → cancelled"
   (check-equal? (gsd-transition->event-kind 'plan-written 'idle 'cancel) 'cancelled))
 (test-case "executing → verifying → phase-changed"
   (check-equal? (gsd-transition->event-kind 'executing 'verifying 'verify) 'phase-changed))
 (test-case "executing → idle (cancel) → cancelled"
   (check-equal? (gsd-transition->event-kind 'executing 'idle 'cancel) 'cancelled))
 (test-case "verifying → idle (done) → wave-completed"
   (check-equal? (gsd-transition->event-kind 'verifying 'idle 'done) 'wave-completed))
 (test-case "verifying → executing (rework) → phase-changed"
   (check-equal? (gsd-transition->event-kind 'verifying 'executing 'rework) 'phase-changed))
 ;; ── Task-state transition → event-kind mapping ──
 (test-case "task idle → exploration → task-started"
   (check-equal? (task-state-transition->event-kind 'idle 'exploration) 'task-started))
 (test-case "task exploration → planning → phase-changed"
   (check-equal? (task-state-transition->event-kind 'exploration 'planning) 'phase-changed))
 (test-case "task planning → implementation → phase-changed"
   (check-equal? (task-state-transition->event-kind 'planning 'implementation) 'phase-changed))
 (test-case "task implementation → verification → phase-changed"
   (check-equal? (task-state-transition->event-kind 'implementation 'verification) 'phase-changed))
 (test-case "task verification → debugging → phase-changed"
   (check-equal? (task-state-transition->event-kind 'verification 'debugging) 'phase-changed))
 (test-case "task any → idle (task-complete) → phase-changed"
   (check-equal? (task-state-transition->event-kind 'implementation 'idle) 'phase-changed))
 (test-case "task any → exploration (revisit) → phase-changed"
   (check-equal? (task-state-transition->event-kind 'implementation 'exploration) 'phase-changed))
 ;; ── capture-gsd-transition: full event ──
 (test-case "capture-gsd-transition produces wave-started event"
   (define e
     (capture-gsd-transition ctx
                             #:from-state 'plan-written
                             #:to-state 'executing
                             #:trigger-event 'execute
                             #:correlation-id "corr-1"))
   (check-equal? (task-ledger-event-event-kind e) 'wave-started)
   (check-equal? (task-ledger-event-correlation-id e) "corr-1")
   (check-equal? (task-ledger-event-source-class e) 'runtime-observed)
   (check-equal? (task-ledger-event-session-seq e) 3))
 (test-case "capture-gsd-transition payload records from/to/trigger"
   (define e
     (capture-gsd-transition ctx
                             #:from-state 'idle
                             #:to-state 'exploring
                             #:trigger-event 'explore
                             #:correlation-id "corr-2"))
   (define pl (task-ledger-event-payload e))
   (check-equal? (hash-ref pl 'from-state) "idle")
   (check-equal? (hash-ref pl 'to-state) "exploring")
   (check-equal? (hash-ref pl 'trigger) "explore"))
 (test-case "capture-gsd-transition generates event-id"
   (define e
     (capture-gsd-transition ctx
                             #:from-state 'idle
                             #:to-state 'exploring
                             #:trigger-event 'explore
                             #:correlation-id "c"))
   (check-true (and (string? (task-ledger-event-event-id e))
                    (positive? (string-length (task-ledger-event-event-id e))))))
 ;; ── capture-task-state-transition: full event ──
 (test-case "capture-task-state-transition produces task-started"
   (define e
     (capture-task-state-transition ctx
                                    #:from-state 'idle
                                    #:to-state 'exploration
                                    #:correlation-id "corr-3"))
   (check-equal? (task-ledger-event-event-kind e) 'task-started))
 (test-case "capture-task-state-transition payload records states"
   (define e
     (capture-task-state-transition ctx
                                    #:from-state 'planning
                                    #:to-state 'implementation
                                    #:correlation-id "c"))
   (define pl (task-ledger-event-payload e))
   (check-equal? (hash-ref pl 'from-state) "planning")
   (check-equal? (hash-ref pl 'to-state) "implementation"))
 ;; ── capture-conclusion: full event ──
 (test-case "capture-conclusion produces objective-set"
   (define e
     (capture-conclusion ctx
                         #:category 'insight
                         #:summary "Tests pass after fix"
                         #:correlation-id "corr-4"))
   (check-equal? (task-ledger-event-event-kind e) 'objective-set)
   (check-equal? (task-ledger-event-correlation-id e) "corr-4"))
 (test-case "capture-conclusion payload records category and summary"
   (define e
     (capture-conclusion ctx #:category 'risk #:summary "Boundary X is fragile" #:correlation-id "c"))
   (define pl (task-ledger-event-payload e))
   (check-equal? (hash-ref pl 'category) "risk")
   (check-equal? (hash-ref pl 'summary) "Boundary X is fragile"))
 (test-case "capture-conclusion with tags records them"
   (define e
     (capture-conclusion ctx
                         #:category 'insight
                         #:summary "s"
                         #:tags '(arch testing)
                         #:correlation-id "c"))
   (define pl (task-ledger-event-payload e))
   (check-equal? (hash-ref pl 'tags) '("arch" "testing")))
 ;; ── capture-checkpoint: full event ──
 (test-case "capture-checkpoint produces checkpoint-created"
   (define e
     (capture-checkpoint ctx
                         #:checkpoint-id "ckpt-1"
                         #:reason "wave-complete"
                         #:correlation-id "corr-5"))
   (check-equal? (task-ledger-event-event-kind e) 'checkpoint-created)
   (define pl (task-ledger-event-payload e))
   (check-equal? (hash-ref pl 'checkpoint-id) "ckpt-1")
   (check-equal? (hash-ref pl 'reason) "wave-complete"))
 ;; ── content-digest is present and varies ──
 (test-case "capture-conclusion produces non-trivial content-digest"
   (define e1 (capture-conclusion ctx #:category 'insight #:summary "summary A" #:correlation-id "c"))
   (define e2 (capture-conclusion ctx #:category 'insight #:summary "summary B" #:correlation-id "c"))
   (check-not-equal? (task-ledger-event-content-digest e1) (task-ledger-event-content-digest e2)))
 ;; ── capture-gsd-transitions: batch (transition history) ──
 (test-case "capture-gsd-transitions translates a history"
   (define transitions
     (list (list 'idle 'exploring 'explore)
           (list 'exploring 'plan-written 'plan)
           (list 'plan-written 'executing 'execute)))
   (define events (capture-gsd-transitions ctx transitions #:correlation-id "batch"))
   (check-equal? (length events) 3)
   (check-equal? (task-ledger-event-event-kind (first events)) 'phase-changed)
   (check-equal? (task-ledger-event-event-kind (second events)) 'objective-set)
   (check-equal? (task-ledger-event-event-kind (third events)) 'wave-started))
 ;; ── evidence-refs propagation ──
 (test-case "capture-gsd-transition accepts evidence-refs"
   (define e
     (capture-gsd-transition ctx
                             #:from-state 'idle
                             #:to-state 'exploring
                             #:trigger-event 'explore
                             #:correlation-id "c"
                             #:evidence-refs '("ev-1" "ev-2")))
   (check-equal? (task-ledger-event-evidence-refs e) '("ev-1" "ev-2")))
 ;; ── valid event-kinds only ──
 (test-case "all gsd-transition event-kinds are valid"
   (define kinds
     (list (gsd-transition->event-kind 'idle 'exploring 'explore)
           (gsd-transition->event-kind 'exploring 'plan-written 'plan)
           (gsd-transition->event-kind 'exploring 'idle 'cancel)
           (gsd-transition->event-kind 'plan-written 'executing 'execute)
           (gsd-transition->event-kind 'plan-written 'idle 'cancel)
           (gsd-transition->event-kind 'executing 'verifying 'verify)
           (gsd-transition->event-kind 'executing 'idle 'cancel)
           (gsd-transition->event-kind 'verifying 'idle 'done)
           (gsd-transition->event-kind 'verifying 'executing 'rework)))
   (for ([k (in-list kinds)])
     (check-true (valid-event-kind? k) (format "kind ~a should be valid" k)))))

(run-tests gsd-adapter-suite)
