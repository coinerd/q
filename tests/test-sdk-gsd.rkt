#lang racket

;; tests/test-sdk-gsd.rkt — tests for GSD observability via SDK (v0.20.4 W3)
;;
;; Covers:
;;   - gsd-snapshot returns immutable hash with expected keys
;;   - gsd-snapshot doesn't expose internal boxes
;;   - gsd-status with no session → 'no-active-session
;;   - gsd-status with active session → hash with expected keys

(require rackunit
         racket/set
         (only-in "../extensions/gsd-planning-state.rkt"
                  gsd-snapshot
                  set-gsd-mode!
                  set-total-waves!
                  mark-wave-complete!
                  reset-plan-budget!
                  decrement-plan-budget!
                  decrement-budget!
                  set-go-read-budget!
                  reset-all-gsd-state!)
         (only-in "../interfaces/sdk.rkt" gsd-status))

;; ============================================================
;; gsd-snapshot tests
;; ============================================================

(test-case "gsd-snapshot returns immutable hash"
  (reset-all-gsd-state!)
  (define snap (gsd-snapshot))
  (check-pred hash? snap)
  ;; Immutable? Writing should fail
  (check-exn exn:fail? (lambda () (hash-set! snap 'foo 'bar))))

(test-case "gsd-snapshot has expected keys"
  (reset-all-gsd-state!)
  (define snap (gsd-snapshot))
  (for ([key '(mode pinned-dir
                    go-read-budget
                    edit-limit
                    read-counts
                    completed-waves
                    total-waves
                    last-edit-wave
                    plan-tool-budget)])
    (check-true (hash-has-key? snap key) (format "missing key: ~a" key))))

(test-case "gsd-snapshot doesn't expose internal boxes"
  (reset-all-gsd-state!)
  (define snap (gsd-snapshot))
  ;; All values should be plain data, not boxes
  (check-false (box? (hash-ref snap 'mode)))
  (check-false (box? (hash-ref snap 'go-read-budget)))
  (check-false (box? (hash-ref snap 'edit-limit)))
  (check-false (box? (hash-ref snap 'total-waves)))
  (check-false (box? (hash-ref snap 'plan-tool-budget))))

(test-case "gsd-snapshot reflects state changes"
  (reset-all-gsd-state!)
  (set-gsd-mode! 'executing)
  (set-total-waves! 5)
  (mark-wave-complete! 0)
  (mark-wave-complete! 1)
  (reset-plan-budget!)
  (define snap (gsd-snapshot))
  (check-equal? (hash-ref snap 'mode) 'executing)
  (check-equal? (hash-ref snap 'total-waves) 5)
  (check-true (set=? (hash-ref snap 'completed-waves) (set 0 1)))
  (check-equal? (hash-ref snap 'plan-tool-budget) 30)
  ;; Clean up
  (reset-all-gsd-state!))

;; ============================================================
;; gsd-status tests
;; ============================================================

(test-case "gsd-status returns 'no-active-session when no GSD mode"
  (reset-all-gsd-state!)
  (check-equal? (gsd-status) 'no-active-session))

(test-case "gsd-status returns hash when GSD mode is active"
  (reset-all-gsd-state!)
  (set-gsd-mode! 'planning)
  (define status (gsd-status))
  (check-pred hash? status)
  (check-equal? (hash-ref status 'mode) 'planning)
  ;; Clean up
  (reset-all-gsd-state!))

(test-case "gsd-status returns 'no-active-session after reset"
  (reset-all-gsd-state!)
  (set-gsd-mode! 'executing)
  (reset-all-gsd-state!)
  (check-equal? (gsd-status) 'no-active-session))

;; ============================================================
;; v0.20.4 W4: SDK integration tests
;; ============================================================

(test-case "gsd-snapshot reflects wave progress during execution"
  (reset-all-gsd-state!)
  (set-gsd-mode! 'executing)
  (set-total-waves! 3)
  ;; Simulate completing waves 0 and 1
  (mark-wave-complete! 0)
  (mark-wave-complete! 1)
  (define snap (gsd-snapshot))
  (check-equal? (hash-ref snap 'mode) 'executing)
  (check-equal? (hash-ref snap 'total-waves) 3)
  (check-true (set=? (hash-ref snap 'completed-waves) (set 0 1)))
  (check-equal? (gsd-status) snap) ;; gsd-status matches snapshot when mode active
  (reset-all-gsd-state!))

(test-case "gsd-snapshot reflects budget depletion"
  (reset-all-gsd-state!)
  (set-gsd-mode! 'executing)
  (set-go-read-budget! 5)
  (decrement-budget!)
  (decrement-budget!)
  (define snap (gsd-snapshot))
  (check-equal? (hash-ref snap 'go-read-budget) 3)
  (reset-all-gsd-state!))

(test-case "gsd-snapshot reflects plan-tool-budget depletion"
  (reset-all-gsd-state!)
  (set-gsd-mode! 'planning)
  (reset-plan-budget!) ;; sets to EXPLORATION-BUDGET (30)
  (decrement-plan-budget!)
  (decrement-plan-budget!)
  (decrement-plan-budget!)
  (define snap (gsd-snapshot))
  (check-equal? (hash-ref snap 'plan-tool-budget) 27)
  (reset-all-gsd-state!))

(test-case "gsd-snapshot reflects mode transitions"
  (reset-all-gsd-state!)
  ;; Start: no mode
  (check-equal? (hash-ref (gsd-snapshot) 'mode) #f)
  (check-equal? (gsd-status) 'no-active-session)
  ;; Transition to planning
  (set-gsd-mode! 'planning)
  (check-equal? (hash-ref (gsd-snapshot) 'mode) 'planning)
  (check-pred hash? (gsd-status))
  ;; Transition to plan-written
  (set-gsd-mode! 'plan-written)
  (check-equal? (hash-ref (gsd-snapshot) 'mode) 'plan-written)
  ;; Transition to executing
  (set-gsd-mode! 'executing)
  (check-equal? (hash-ref (gsd-snapshot) 'mode) 'executing)
  ;; Reset
  (reset-all-gsd-state!)
  (check-equal? (hash-ref (gsd-snapshot) 'mode) #f))
