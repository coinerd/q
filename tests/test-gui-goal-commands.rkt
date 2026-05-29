#lang racket/base

;; tests/test-gui-goal-commands.rkt — GUI /goal handler tests

(require rackunit
         racket/string
         "../gui/gui-types.rkt")

;; ============================================================
;; gui-state active-goal field
;; ============================================================

(let ()
  (define gs (make-gui-state))
  (check-false (gui-state-active-goal gs) "initial state has no active goal"))

(let ()
  (define gs
    (make-gui-state #:active-goal
                    (hasheq 'goal-text "test" 'turns-used 0 'max-turns 8 'status 'active)))
  (check-not-false (gui-state-active-goal gs) "can set active-goal in constructor"))

;; ============================================================
;; gui-state-set-active-goal
;; ============================================================

(let ()
  (define gs (make-gui-state))
  (define goal (hasheq 'goal-text "tests pass" 'turns-used 3 'max-turns 8 'status 'active))
  (define gs2 (gui-state-set-active-goal gs goal))
  (check-equal? (hash-ref (gui-state-active-goal gs2) 'goal-text) "tests pass")
  ;; Clear
  (define gs3 (gui-state-set-active-goal gs2 #f))
  (check-false (gui-state-active-goal gs3)))

;; ============================================================
;; Hash round-trip preserves active-goal
;; ============================================================

(let ()
  (define goal (hasheq 'goal-text "hello" 'turns-used 1 'max-turns 4 'status 'active))
  (define gs (make-gui-state #:active-goal goal))
  (define h (gui-state->hash gs))
  (define gs2 (hash->gui-state h))
  (check-not-false (gui-state-active-goal gs2))
  (check-equal? (hash-ref (gui-state-active-goal gs2) 'goal-text) "hello"))

(let ()
  (define gs (make-gui-state))
  (define h (gui-state->hash gs))
  (define gs2 (hash->gui-state h))
  (check-false (gui-state-active-goal gs2)))

(displayln "All GUI goal-command tests passed.")
