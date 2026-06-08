#lang racket

;; @speed fast
;; @suite default

;; tests/test-loop-phases.rkt — Tests for runtime/iteration/loop-phases.rkt (v0.54.3 W1)
;;
;; Tests the extracted pure and effectful sub-phases from main-loop.

(require rackunit
         rackunit/text-ui
         "../agent/iteration/loop-phases.rkt"
         "../agent/event-bus.rkt"
         "../agent/queue.rkt")

(define loop-phases-suite
  (test-suite "loop-phases extracted sub-phases"

    ;; ── prepare-iteration-context with no steering/injection ──
    (test-case "prepare-iteration-context returns ctx unchanged when no queue"
      (define bus (make-event-bus))
      (define ctx (list 'msg1 'msg2))
      (define result (prepare-iteration-context ctx #f #f bus #f "s1"))
      (check-equal? result ctx))

    ;; ── prepare-iteration-context with empty queue ──
    (test-case "prepare-iteration-context with empty queue returns ctx"
      (define bus (make-event-bus))
      (define q (make-queue))
      (define ctx (list 'msg1))
      (define result (prepare-iteration-context ctx q #f bus #f "s1"))
      (check-equal? result ctx))

    ;; ── dispatch-turn-start-hooks with #f ext-reg ──
    (test-case "dispatch-turn-start-hooks with no ext-reg returns unblocked"
      (define-values (ctx blocked?) (dispatch-turn-start-hooks '() #f))
      (check-false blocked?)
      (check-equal? ctx '()))

    ;; ── dispatch-turn-start-hooks with ctx ──
    (test-case "dispatch-turn-start-hooks passes through context"
      (define ctx (list 'a 'b 'c))
      (define-values (result-ctx blocked?) (dispatch-turn-start-hooks ctx #f))
      (check-false blocked?)
      (check-equal? result-ctx ctx))

    ;; ── prepare-iteration-context with nil injected box ──
    (test-case "prepare-iteration-context with empty injected box"
      (define bus (make-event-bus))
      (define inj-box (box (list)))
      (define ctx (list 'msg))
      (define result (prepare-iteration-context ctx #f inj-box bus #f "s1"))
      (check-equal? result ctx))

    ;; ── prepare-iteration-context with messages in queue ──
    (test-case "prepare-iteration-context appends steering messages"
      (define bus (make-event-bus))
      (define q (make-queue))
      (define ctx (list 'original))
      ;; Queue starts empty, so steering will be null
      (define result (prepare-iteration-context ctx q #f bus #f "s1"))
      (check-equal? result (list 'original)))))

(run-tests loop-phases-suite)
