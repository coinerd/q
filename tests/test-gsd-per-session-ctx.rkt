#lang racket/base

;; tests/test-gsd-per-session-ctx.rkt — Per-session GSD context isolation (C-01, v0.35.1)
;;
;; Tests that gsd-session-ctx instances don't share state,
;; that per-session accessors work, and that with-gsd-transaction is isolated.

(require rackunit
         "../extensions/gsd/session-state.rkt"
         "../extensions/gsd/runtime-state-types.rkt"
         "../extensions/context.rkt")

;; ============================================================
;; Test: two independent contexts don't share state
;; ============================================================

(test-case "two gsd-session-ctx instances don't share state"
  (define ctx-a (make-gsd-context))
  (define ctx-b (make-gsd-context))
  (check-not-eq? ctx-a ctx-b)
  ;; Write to A
  (gsd-ctx-set-state! ctx-a (make-initial-gsd-state))
  ;; Write different state to B
  (gsd-ctx-set-state! ctx-b
                      (struct-copy gsd-runtime-state (make-initial-gsd-state) [mode 'implementing]))
  ;; Verify A unchanged
  (check-eq? (gsd-runtime-state-mode (gsd-ctx-state ctx-a)) 'idle)
  ;; Verify B has its own state
  (check-eq? (gsd-runtime-state-mode (gsd-ctx-state ctx-b)) 'implementing))

(test-case "plan isolation between contexts"
  (define ctx-a (make-gsd-context))
  (define ctx-b (make-gsd-context))
  (gsd-ctx-set-plan! ctx-a '(wave 1))
  (gsd-ctx-set-plan! ctx-b '(wave 2))
  (check-equal? (gsd-ctx-plan ctx-a) '(wave 1))
  (check-equal? (gsd-ctx-plan ctx-b) '(wave 2)))

(test-case "pinned-dir isolation between contexts"
  (define ctx-a (make-gsd-context))
  (define ctx-b (make-gsd-context))
  (gsd-ctx-set-pinned-dir! ctx-a "/tmp/a")
  (gsd-ctx-set-pinned-dir! ctx-b "/tmp/b")
  (check-equal? (gsd-ctx-pinned-dir ctx-a) "/tmp/a")
  (check-equal? (gsd-ctx-pinned-dir ctx-b) "/tmp/b"))

(test-case "edit-limit isolation between contexts"
  (define ctx-a (make-gsd-context))
  (define ctx-b (make-gsd-context))
  (gsd-ctx-set-edit-limit! ctx-a 100)
  (gsd-ctx-set-edit-limit! ctx-b 200)
  (check-equal? (gsd-ctx-edit-limit ctx-a) 100)
  (check-equal? (gsd-ctx-edit-limit ctx-b) 200))

(test-case "history isolation between contexts"
  (define ctx-a (make-gsd-context))
  (define ctx-b (make-gsd-context))
  (gsd-ctx-set-history! ctx-a '((a)))
  (gsd-ctx-set-history! ctx-b '((b)))
  (check-equal? (gsd-ctx-history ctx-a) '((a)))
  (check-equal? (gsd-ctx-history ctx-b) '((b))))

;; ============================================================
;; Test: with-gsd-transaction isolation
;; ============================================================

(test-case "with-gsd-transaction uses explicit ctx"
  (define ctx (make-gsd-context))
  ;; Use direct box access inside transaction (per-session accessors also acquire sem)
  (define result (with-gsd-transaction ctx (lambda () (unbox (gsd-session-ctx-state-box ctx)))))
  (check-true (gsd-runtime-state? result)))

(test-case "gsd-ctx-state-update! uses explicit ctx"
  (define ctx (make-gsd-context))
  (gsd-ctx-state-update! ctx (lambda (s) (struct-copy gsd-runtime-state s [mode 'planning])))
  (check-eq? (gsd-ctx-mode ctx) 'planning))

(test-case "gsd-ctx-history-update! uses explicit ctx"
  (define ctx (make-gsd-context))
  (gsd-ctx-history-update! ctx (lambda (h) (cons 'event h)))
  (check-equal? (gsd-ctx-history ctx) '(event)))

;; ============================================================
;; Test: extension-ctx integration
;; ============================================================

(test-case "extension-ctx accepts gsd-ctx field"
  (define gsd (make-gsd-context))
  (define ext-ctx
    (make-extension-ctx #:session-id "test"
                        #:session-dir "/tmp"
                        #:event-bus #f
                        #:extension-registry #f
                        #:gsd-ctx gsd))
  (check-equal? (ctx-gsd-ctx ext-ctx) gsd))

(test-case "extension-ctx gsd-ctx defaults to #f"
  (define ext-ctx
    (make-extension-ctx #:session-id "test"
                        #:session-dir "/tmp"
                        #:event-bus #f
                        #:extension-registry #f))
  (check-false (ctx-gsd-ctx ext-ctx)))

(test-case "gsd-ctx through extension-ctx is isolated"
  (define gsd-a (make-gsd-context))
  (define gsd-b (make-gsd-context))
  (gsd-ctx-set-plan! gsd-a '(plan-a))
  (gsd-ctx-set-plan! gsd-b '(plan-b))
  (define ext-ctx-a
    (make-extension-ctx #:session-id "a"
                        #:session-dir "/tmp"
                        #:event-bus #f
                        #:extension-registry #f
                        #:gsd-ctx gsd-a))
  (define ext-ctx-b
    (make-extension-ctx #:session-id "b"
                        #:session-dir "/tmp"
                        #:event-bus #f
                        #:extension-registry #f
                        #:gsd-ctx gsd-b))
  (check-equal? (gsd-ctx-plan (ctx-gsd-ctx ext-ctx-a)) '(plan-a))
  (check-equal? (gsd-ctx-plan (ctx-gsd-ctx ext-ctx-b)) '(plan-b)))

;; ============================================================
;; Test: default context backward compat
;; ============================================================

(test-case "gsd-default-ctx is a valid gsd-session-ctx"
  (check-true (gsd-session-ctx? gsd-default-ctx)))

(test-case "snapshot/update accessors use default ctx"
  ;; Verify snapshot/update accessors work (M-07: gsd-state-snapshot/gsd-state-update! removed)
  (check-true (gsd-runtime-state? (gsd-state-snapshot)))
  (check-true (procedure? gsd-state-update!)))
