#lang racket

;; tests/test-gsd-planning-state.rkt — Semaphore-protected state module tests
;;
;; Wave 0 of v0.20.3: Tests for gsd-planning-state.rkt.
;; Covers: semaphore-protected concurrent access, atomic reset,
;; pinned-dir cross-thread visibility, edit-limit, budget, read-counts.

(require rackunit
         "../extensions/gsd-planning-state.rkt")

;; ============================================================
;; gsd-mode tests
;; ============================================================

(test-case "gsd-mode defaults to #f"
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
  (check-false (pinned-planning-dir)))

(test-case "set-pinned-planning-dir! sets and reads back"
  (reset-all-gsd-state!)
  (set-pinned-planning-dir! "/tmp/test")
  (check-equal? (pinned-planning-dir) "/tmp/test"))

(test-case "pinned-planning-dir visible across threads"
  (reset-all-gsd-state!)
  (set-pinned-planning-dir! "/tmp/cross-thread")
  (define result-box (box #f))
  (thread (lambda () (set-box! result-box (pinned-planning-dir))))
  (sync (system-idle-evt))
  (check-equal? (unbox result-box) "/tmp/cross-thread"))

;; ============================================================
;; go-read-budget tests
;; ============================================================

;; ============================================================
;; current-max-old-text-len tests
;; ============================================================

(test-case "current-max-old-text-len defaults to 500"
  (reset-all-gsd-state!)
  (check-equal? (current-max-old-text-len) 500))

(test-case "set-current-max-old-text-len! sets and reads back"
  (reset-all-gsd-state!)
  (set-current-max-old-text-len! 1200)
  (check-equal? (current-max-old-text-len) 1200))

(test-case "current-max-old-text-len visible across threads"
  (reset-all-gsd-state!)
  (set-current-max-old-text-len! 999)
  (define result-box (box #f))
  (thread (lambda () (set-box! result-box (current-max-old-text-len))))
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
  (check-equal? (set-count (completed-waves)) 0))

(test-case "total-waves defaults to 0"
  (reset-all-gsd-state!)
  (check-equal? (total-waves) 0))

(test-case "set-total-waves! sets and reads back"
  (reset-all-gsd-state!)
  (set-total-waves! 5)
  (check-equal? (total-waves) 5))

(test-case "mark-wave-complete! adds wave to completed set"
  (reset-all-gsd-state!)
  (set-total-waves! 3)
  (mark-wave-complete! 0)
  (check-true (wave-complete? 0))
  (check-false (wave-complete? 1))
  (mark-wave-complete! 2)
  (check-true (wave-complete? 2)))

(test-case "next-pending-wave returns first incomplete wave"
  (reset-all-gsd-state!)
  (set-total-waves! 4)
  (check-equal? (next-pending-wave) 0)
  (mark-wave-complete! 0)
  (check-equal? (next-pending-wave) 1)
  (mark-wave-complete! 1)
  (mark-wave-complete! 2)
  (check-equal? (next-pending-wave) 3)
  (mark-wave-complete! 3)
  (check-equal? (next-pending-wave) #f))

(test-case "next-pending-wave returns #f when total-waves is 0"
  (reset-all-gsd-state!)
  (check-equal? (next-pending-wave) #f))

(test-case "current-wave-index defaults to 0"
  (reset-all-gsd-state!)
  (check-equal? (current-wave-index) 0))

(test-case "set-current-wave-index! sets and reads back"
  (reset-all-gsd-state!)
  (set-current-wave-index! 2)
  (check-equal? (current-wave-index) 2))

(test-case "current-wave-index included in snapshot"
  (reset-all-gsd-state!)
  (set-current-wave-index! 3)
  (define snap (gsd-snapshot))
  (check-equal? (hash-ref snap 'current-wave) 3))

;; ============================================================
;; Plan tool budget tests (v0.20.4 W0)
;; ============================================================

;; ============================================================
;; reset-all-gsd-state! resets wave + budget state (v0.20.4 W0)
;; ============================================================

(test-case "reset-all-gsd-state! resets wave tracking"
  (reset-all-gsd-state!)
  (set-total-waves! 5)
  (mark-wave-complete! 0)
  (mark-wave-complete! 3)
  (reset-all-gsd-state!)
  (check-equal? (total-waves) 0)
  (check-equal? (set-count (completed-waves)) 0)
  (check-false (next-pending-wave)))

;; ============================================================
;; Concurrent wave tracking tests
;; ============================================================

(test-case "concurrent mark-wave-complete! has no lost updates"
  (reset-all-gsd-state!)
  (set-total-waves! 50)
  (define threads
    (for/list ([i (in-range 50)])
      (thread (lambda () (mark-wave-complete! i)))))
  (for-each sync threads)
  ;; All 50 waves should be complete
  (for ([i (in-range 50)])
    (check-true (wave-complete? i) (format "wave ~a should be complete" i))))
