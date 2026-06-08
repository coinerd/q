#lang racket/base

;; @speed fast  ;; @suite extensions

;; tests/test-gsd-contract-precision.rkt — Contract precision tests for session-state
;;
;; Tests that tightened contracts in gsd/session-state.rkt correctly
;; reject wrong-type arguments and accept valid ones.

(require rackunit
         rackunit/text-ui
         "../extensions/gsd/session-state.rkt"
         "../extensions/gsd/runtime-state-types.rkt")

(define (run-contract-tests)
  ;; ── make-gsd-context ──────────────────────────────────
  (test-case "make-gsd-context returns gsd-session-ctx?"
    (define ctx (make-gsd-context))
    (check-pred gsd-session-ctx? ctx))

  ;; ── gsd-ctx-state / gsd-ctx-set-state! ────────────────
  (test-case "gsd-ctx-state returns gsd-runtime-state?"
    (define ctx (make-gsd-context))
    (check-pred gsd-runtime-state? (gsd-ctx-state ctx)))

  (test-case "gsd-ctx-set-state! accepts gsd-runtime-state?"
    (define ctx (make-gsd-context))
    (define new-state (make-initial-gsd-state))
    (gsd-ctx-set-state! ctx new-state)
    (check-equal? (gsd-ctx-state ctx) new-state))

  (test-case "gsd-ctx-set-state! rejects non-gsd-runtime-state"
    (define ctx (make-gsd-context))
    (check-exn exn:fail:contract? (lambda () (gsd-ctx-set-state! ctx "not-a-state"))))

  ;; ── gsd-ctx-mode ──────────────────────────────────────
  (test-case "gsd-ctx-mode returns symbol?"
    (define ctx (make-gsd-context))
    (check-pred symbol? (gsd-ctx-mode ctx)))

  ;; ── gsd-ctx-wave-number ───────────────────────────────
  (test-case "gsd-ctx-wave-number returns exact-nonnegative-integer?"
    (define ctx (make-gsd-context))
    (check-pred exact-nonnegative-integer? (gsd-ctx-wave-number ctx)))

  ;; ── gsd-ctx-plan / gsd-ctx-set-plan! ──────────────────
  (test-case "gsd-ctx-plan returns any (plan data)"
    (define ctx (make-gsd-context))
    ;; Plan can be #f or any data
    (check-equal? (gsd-ctx-plan ctx) #f))

  (test-case "gsd-ctx-set-plan! accepts any value"
    (define ctx (make-gsd-context))
    (gsd-ctx-set-plan! ctx "some-plan")
    (check-equal? (gsd-ctx-plan ctx) "some-plan"))

  ;; ── gsd-ctx-pinned-dir / gsd-ctx-set-pinned-dir! ──────
  (test-case "gsd-ctx-pinned-dir returns string or #f"
    (define ctx (make-gsd-context))
    (check-false (gsd-ctx-pinned-dir ctx)))

  (test-case "gsd-ctx-set-pinned-dir! accepts string"
    (define ctx (make-gsd-context))
    (gsd-ctx-set-pinned-dir! ctx "/tmp")
    (check-equal? (gsd-ctx-pinned-dir ctx) "/tmp"))

  (test-case "gsd-ctx-set-pinned-dir! rejects non-string-non-false"
    (define ctx (make-gsd-context))
    (check-exn exn:fail:contract? (lambda () (gsd-ctx-set-pinned-dir! ctx 42))))

  ;; ── gsd-ctx-edit-limit / gsd-ctx-set-edit-limit! ──────
  (test-case "gsd-ctx-edit-limit returns exact-positive-integer?"
    (define ctx (make-gsd-context))
    (check-pred exact-positive-integer? (gsd-ctx-edit-limit ctx)))

  (test-case "gsd-ctx-set-edit-limit! accepts exact-positive-integer"
    (define ctx (make-gsd-context))
    (gsd-ctx-set-edit-limit! ctx 100)
    (check-equal? (gsd-ctx-edit-limit ctx) 100))

  (test-case "gsd-ctx-set-edit-limit! rejects non-positive"
    (define ctx (make-gsd-context))
    (check-exn exn:fail:contract? (lambda () (gsd-ctx-set-edit-limit! ctx -1))))

  ;; ── gsd-ctx-history / gsd-ctx-set-history! ────────────
  (test-case "gsd-ctx-history returns list?"
    (define ctx (make-gsd-context))
    (check-pred list? (gsd-ctx-history ctx)))

  (test-case "gsd-ctx-set-history! accepts list"
    (define ctx (make-gsd-context))
    (gsd-ctx-set-history! ctx '(1 2 3))
    (check-equal? (gsd-ctx-history ctx) '(1 2 3)))

  (test-case "gsd-ctx-set-history! rejects non-list"
    (define ctx (make-gsd-context))
    (check-exn exn:fail:contract? (lambda () (gsd-ctx-set-history! ctx "not-a-list"))))

  ;; ── gsd-ctx-correlation-id / gsd-ctx-set-correlation-id!
  (test-case "gsd-ctx-correlation-id returns string or #f"
    (define ctx (make-gsd-context))
    (check-false (gsd-ctx-correlation-id ctx)))

  (test-case "gsd-ctx-set-correlation-id! accepts string"
    (define ctx (make-gsd-context))
    (gsd-ctx-set-correlation-id! ctx "corr-123")
    (check-equal? (gsd-ctx-correlation-id ctx) "corr-123"))

  (test-case "gsd-ctx-set-correlation-id! rejects non-string-non-false"
    (define ctx (make-gsd-context))
    (check-exn exn:fail:contract? (lambda () (gsd-ctx-set-correlation-id! ctx 42))))

  ;; ── with-gsd-transaction ──────────────────────────────
  (test-case "with-gsd-transaction accepts ctx and thunk"
    (define ctx (make-gsd-context))
    (define result (with-gsd-transaction ctx (lambda () 42)))
    (check-equal? result 42))

  (test-case "with-gsd-transaction rejects non-ctx"
    (check-exn exn:fail:contract? (lambda () (with-gsd-transaction "not-ctx" (lambda () 1)))))

  ;; ── ctx-read / ctx-write! / ctx-update! ───────────────
  (test-case "ctx-read rejects non-ctx"
    (check-exn exn:fail:contract? (lambda () (ctx-read "not-ctx" gsd-session-ctx-state-box))))

  (test-case "ctx-write! rejects non-ctx"
    (check-exn exn:fail:contract? (lambda () (ctx-write! "not-ctx" gsd-session-ctx-state-box 42))))

  (test-case "ctx-update! rejects non-ctx"
    (check-exn exn:fail:contract?
               (lambda () (ctx-update! "not-ctx" gsd-session-ctx-state-box add1)))))

(run-tests (test-suite "gsd-session-state contract precision"
             (run-contract-tests)))
