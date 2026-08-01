#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-gui-goal-commands.rkt — GUI /goal handler tests

(require rackunit
         racket/string
         "../gui/gui-types.rkt"
         (only-in "../runtime/session/session-config.rkt" current-goal-loop-enabled?)
         "../gui/slash-commands.rkt")

;; ============================================================
;; gui-state active-goal field
;; ============================================================

(test-case "gui-goal-commands: block 1"
  (let ()
    (define gs (make-gui-state))
    (check-false (gui-state-active-goal gs) "initial state has no active goal")))

(test-case "gui-goal-commands: block 2"
  (let ()
    (define gs
      (make-gui-state #:active-goal
                      (hasheq 'goal-text "test" 'turns-used 0 'max-turns 8 'status 'active)))
    (check-not-false (gui-state-active-goal gs) "can set active-goal in constructor")))

;; ============================================================
;; gui-state-set-active-goal
;; ============================================================

(test-case "gui-goal-commands: block 3"
  (let ()
    (define gs (make-gui-state))
    (define goal (hasheq 'goal-text "tests pass" 'turns-used 3 'max-turns 8 'status 'active))
    (define gs2 (gui-state-set-active-goal gs goal))
    (check-equal? (hash-ref (gui-state-active-goal gs2) 'goal-text) "tests pass")
    ;; Clear
    (define gs3 (gui-state-set-active-goal gs2 #f))
    (check-false (gui-state-active-goal gs3))))

;; ============================================================
;; Hash round-trip preserves active-goal
;; ============================================================

(test-case "gui-goal-commands: block 4"
  (let ()
    (define goal (hasheq 'goal-text "hello" 'turns-used 1 'max-turns 4 'status 'active))
    (define gs (make-gui-state #:active-goal goal))
    (define h (gui-state->hash gs))
    (define gs2 (hash->gui-state h))
    (check-not-false (gui-state-active-goal gs2))
    (check-equal? (hash-ref (gui-state-active-goal gs2) 'goal-text) "hello")))

(test-case "gui-goal-commands: block 5"
  (let ()
    (define gs (make-gui-state))
    (define h (gui-state->hash gs))
    (define gs2 (hash->gui-state h))
    (check-false (gui-state-active-goal gs2))))

;; Feature flag guard: /goal rejected when loop disabled (default)
(test-case "gui-goal-commands: block 6"
  (let ()
    (define state-box (box (make-gui-state)))
    (define lock (make-semaphore 1))
    (define notified #f)
    (define (notify!)
      (set! notified #t))
    ;; Without enabling the feature flag, goal should be rejected
    (parameterize ([current-goal-loop-enabled? #f])
      (define handler (make-slash-command-handler #f state-box lock notify!))
      (handler "/goal test goal"))
    ;; Should NOT have set an active goal
    (check-false (gui-state-active-goal (unbox state-box))
                 "goal not set when feature flag disabled")))

;; GUI must not create a phantom goal without a live session.
(test-case "gui-goal-commands: no session rejects goal"
  (let ()
    (define state-box (box (make-gui-state)))
    (define lock (make-semaphore 1))
    (parameterize ([current-goal-loop-enabled? #t])
      (define handler (make-slash-command-handler #f state-box lock))
      (handler "/goal \"make tests pass\""))
    (check-false (gui-state-active-goal (unbox state-box)))))

;; /goal clear must signal the background goal loop, not only clear display state.
(test-case "gui-goal-commands: clear sets cancellation flag"
  (let ()
    (define state-box
      (box (make-gui-state #:active-goal
                           (hasheq 'goal-text "old" 'turns-used 0 'max-turns 8 'status 'active))))
    (define lock (make-semaphore 1))
    (define cancel-box (box #f))
    (parameterize ([current-goal-loop-enabled? #t])
      (define handler (make-slash-command-handler #f state-box lock #:goal-cancel-box cancel-box))
      (handler "/goal clear"))
    (check-true (unbox cancel-box))
    (check-false (gui-state-active-goal (unbox state-box)))))

;; A terminal recorded goal is not a live goal and must not block a new one.
(test-case "gui-goal-commands: terminal goal does not guard concurrency"
  (let ()
    (define state-box
      (box (make-gui-state #:active-goal
                           (hasheq 'goal-text "old" 'turns-used 2 'max-turns 8 'status 'failed))))
    (define lock (make-semaphore 1))
    (parameterize ([current-goal-loop-enabled? #t])
      (define handler (make-slash-command-handler #f state-box lock))
      (handler "/goal \"new goal\""))
    ;; No session, so the new goal is rejected without replacing state; the
    ;; key assertion is that the response was not the active-goal rejection.
    (define text (string-join (map gui-message-text (gui-state-messages (unbox state-box))) "\n"))
    (check-false (string-contains? text "already active"))))
