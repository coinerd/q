#lang racket

;; @speed fast  ;; @suite extensions

;; BOUNDARY: integration

;; tests/test-gsd-planning-state.rkt — Semaphore-protected state module tests
;;
;; Wave 0 of v0.20.3: Tests for gsd-planning-state.rkt.
;; Covers: semaphore-protected concurrent access, atomic reset,
;; pinned-dir cross-thread visibility, edit-limit, budget, read-counts.

(require rackunit
         "../extensions/gsd/state-machine.rkt"
         "../extensions/gsd/session-state.rkt"
         (only-in "../extensions/gsd/core.rkt" reset-all-gsd-state!)
         (only-in "../extensions/gsd/runtime-state-types.rkt" gsd-runtime-state-current-wave))

;; ============================================================

;; Legacy mode wrappers (DEBT-01)
(define (gsd-mode)
  (let ([s (gsm-current)])
    (cond
      [(eq? s 'idle) #f]
      [(eq? s 'exploring) 'planning]
      [else s])))

(define (gsd-mode? v)
  (eq? (gsd-mode) v))

(define (set-gsd-mode! v)
  (cond
    [(not v) (gsm-reset!)]
    [(eq? v 'planning) (gsm-transition-to! 'exploring)]
    [(eq? v 'plan-written) (gsm-transition-to! 'plan-written)]
    [(eq? v 'executing) (gsm-transition-to! 'executing)]
    [else (gsm-transition! v)]))

;; gsd-mode tests
;; ============================================================

(test-case "gsd-planning-state: gsd-mode defaults to #f"
  (reset-all-gsd-state!)
  (check-equal? (gsd-mode) #f))

(test-case "set-gsd-mode! sets and reads back"
  (reset-all-gsd-state!)
  (set-gsd-mode! 'planning)
  (check-eq? (gsd-mode) 'planning)
  (set-gsd-mode! 'executing)
  (check-eq? (gsd-mode) 'executing))

(test-case "gsd-mode? checks current mode"
  (reset-all-gsd-state!)
  (set-gsd-mode! 'planning)
  (check-true (gsd-mode? 'planning))
  (check-false (gsd-mode? 'executing)))

;; ============================================================
;; pinned-planning-dir tests
;; ============================================================

(test-case "pinned-planning-dir defaults to #f"
  (reset-all-gsd-state!)
  (check-false (current-pinned-dir)))

(test-case "set-pinned-planning-dir! sets and reads back"
  (reset-all-gsd-state!)
  (set-pinned-dir! "/tmp/test")
  (check-equal? (current-pinned-dir) "/tmp/test"))

(test-case "pinned-planning-dir visible across threads"
  (reset-all-gsd-state!)
  (set-pinned-dir! "/tmp/cross-thread")
  (define result-box (box #f))
  (thread (lambda () (set-box! result-box (current-pinned-dir))))
  (sync (system-idle-evt))
  (check-equal? (unbox result-box) "/tmp/cross-thread"))

;; ============================================================
;; go-read-budget tests
;; ============================================================

;; ============================================================
;; current-max-old-text-len tests
;; ============================================================

(test-case "gsd-planning-state: current-max-old-text-len defaults to 500"
  (reset-all-gsd-state!)
  (check-equal? (current-edit-limit) 500))

(test-case "set-current-max-old-text-len! sets and reads back"
  (reset-all-gsd-state!)
  (set-edit-limit! 1200)
  (check-equal? (current-edit-limit) 1200))

(test-case "current-max-old-text-len visible across threads"
  (reset-all-gsd-state!)
  (set-edit-limit! 999)
  (define result-box (box #f))
  (thread (lambda () (set-box! result-box (current-edit-limit))))
  (sync (system-idle-evt))
  (check-equal? (unbox result-box) 999))

;; ============================================================
;; read-counts tests
;; ============================================================

;; ============================================================
;; reset-all-gsd-state! tests
;; ============================================================

;; ============================================================
;; Concurrent access tests
;; ============================================================

(test-case "concurrent gsd-mode writes — last writer wins"
  (reset-all-gsd-state!)
  (define threads
    (for/list ([i (in-range 100)])
      (thread (lambda () (set-gsd-mode! (if (even? i) 'planning 'executing))))))
  (for-each sync threads)
  ;; Value must be one of the two valid modes
  (check-true (or (eq? (gsd-mode) 'planning) (eq? (gsd-mode) 'executing))
              "mode must be a valid symbol after concurrent writes"))

;; ============================================================

;; Wave tracking tests (v0.20.4 W0)
;; ============================================================

(test-case "completed-waves starts empty"
  (reset-all-gsd-state!)
  (check-equal? (set-count (gsm-completed-waves)) 0))

(test-case "total-waves defaults to 0"
  (reset-all-gsd-state!)
  (check-equal? (gsm-total-waves) 0))

(test-case "set-total-waves! sets and reads back"
  (reset-all-gsd-state!)
  (gsm-set-total-waves! 5)
  (check-equal? (gsm-total-waves) 5))

(test-case "mark-wave-complete! adds wave to completed set"
  (reset-all-gsd-state!)
  (gsm-set-total-waves! 3)
  (gsm-mark-wave-complete! 0)
  (check-true (gsm-wave-complete? 0))
  (check-false (gsm-wave-complete? 1))
  (gsm-mark-wave-complete! 2)
  (check-true (gsm-wave-complete? 2)))

(test-case "next-pending-wave returns first incomplete wave"
  (reset-all-gsd-state!)
  (gsm-set-total-waves! 4)
  (check-equal? (gsm-next-pending-wave) 0)
  (gsm-mark-wave-complete! 0)
  (check-equal? (gsm-next-pending-wave) 1)
  (gsm-mark-wave-complete! 1)
  (gsm-mark-wave-complete! 2)
  (check-equal? (gsm-next-pending-wave) 3)
  (gsm-mark-wave-complete! 3)
  (check-equal? (gsm-next-pending-wave) #f))

(test-case "next-pending-wave returns #f when total-waves is 0"
  (reset-all-gsd-state!)
  (check-equal? (gsm-next-pending-wave) #f))

(test-case "current-wave-index defaults to 0"
  (reset-all-gsd-state!)
  (check-equal? (gsm-current-wave) 0))

(test-case "set-current-wave-index! sets and reads back"
  (reset-all-gsd-state!)
  (gsm-set-current-wave! 2)
  (check-equal? (gsm-current-wave) 2))

(test-case "current-wave-index included in snapshot"
  (reset-all-gsd-state!)
  (gsm-set-current-wave! 3)
  (define snap (gsm-snapshot))
  (check-equal? (gsd-runtime-state-current-wave snap) 3))

;; ============================================================
;; Plan tool budget tests (v0.20.4 W0)
;; ============================================================

;; ============================================================
;; reset-all-gsd-state! resets wave + budget state (v0.20.4 W0)
;; ============================================================

(test-case "reset-all-gsd-state! resets wave tracking"
  (reset-all-gsd-state!)
  (gsm-set-total-waves! 5)
  (gsm-mark-wave-complete! 0)
  (gsm-mark-wave-complete! 3)
  (reset-all-gsd-state!)
  (check-equal? (gsm-total-waves) 0)
  (check-equal? (set-count (gsm-completed-waves)) 0)
  (check-false (gsm-next-pending-wave)))

;; ============================================================
;; Concurrent wave tracking tests
;; ============================================================

(test-case "concurrent mark-wave-complete! has no lost updates"
  (reset-all-gsd-state!)
  (gsm-set-total-waves! 50)
  (define threads
    (for/list ([i (in-range 50)])
      (thread (lambda () (gsm-mark-wave-complete! i)))))
  (for-each sync threads)
  ;; All 50 waves should be complete
  (for ([i (in-range 50)])
    (check-true (gsm-wave-complete? i) (format "wave ~a should be complete" i))))
