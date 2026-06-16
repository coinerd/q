#lang racket

;; @speed fast  ;; @suite default

;; tests/test-blackboard-deployment-gate.rkt
;; v0.99.14 W0: Characterization & Pre-Implementation Safety Net
;;
;; These tests lock in the CURRENT default-off behavior of the blackboard
;; before the default flip in W2. After W2, tests #1 and #2 will be updated.
;;
;; Test Cases:
;;   1. blackboard-enabled? returns #f for default settings (characterize current)
;;   2. blackboard-enabled? returns #t when explicitly enabled
;;   3. blackboard-enabled? returns #f when explicitly disabled
;;   4. build-blackboard-context-snippet returns #f for empty blackboard
;;   5. build-blackboard-context-snippet returns non-#f for populated state
;;   6. stop-blackboard-subscriber! is idempotent (safe when not subscribed)

(require rackunit
         rackunit/text-ui
         (only-in "../runtime/settings-query.rkt" blackboard-enabled?)
         (only-in "../runtime/settings-core.rkt" q-settings)
         (only-in "../agent/blackboard.rkt"
                  make-blackboard
                  current-blackboard
                  empty-blackboard
                  blackboard-state
                  blackboard-state-wave-status)
         (only-in "../agent/blackboard-subscriber.rkt"
                  stop-blackboard-subscriber!
                  current-subscription)
         (only-in "../runtime/context-assembly/blackboard-context.rkt"
                  build-blackboard-context-snippet
                  MAX-BLACKBOARD-SNIPPET-LEN)
         (only-in "../runtime/context-assembly/state-aware-builder.rkt"
                  current-blackboard-injection-enabled))

;; ── Helper: build a q-settings with a given merged hash ──
(define (make-settings merged-hash)
  (q-settings (hash) (hash) merged-hash))

(define deployment-gate-suite
  (test-suite "Blackboard Deployment Gate (v0.99.14 W0)"

    ;; ── Test 1: Default is now #t (v0.99.14 W2 flip) ──
    (test-case "blackboard-enabled? returns #t for default settings"
      (define settings (make-settings (hash)))
      (check-true (blackboard-enabled? settings) "default should be #t after W2 flip"))

    ;; ── Test 2: Explicit #t ──
    (test-case "blackboard-enabled? returns #t when mas.blackboard.enabled = #t"
      (define settings (make-settings (hash 'mas (hash 'blackboard (hash 'enabled #t)))))
      (check-true (blackboard-enabled? settings)))

    ;; ── Test 3: Explicit #f ──
    (test-case "blackboard-enabled? returns #f when mas.blackboard.enabled = #f"
      (define settings (make-settings (hash 'mas (hash 'blackboard (hash 'enabled #f)))))
      (check-false (blackboard-enabled? settings)))

    ;; ── Test 4: Empty blackboard snippet is #f ──
    (test-case "build-blackboard-context-snippet returns #f for empty state"
      (define state empty-blackboard)
      (check-false (build-blackboard-context-snippet state)))

    ;; ── Test 5: Populated blackboard snippet is non-#f ──
    (test-case "build-blackboard-context-snippet returns non-#f for populated state"
      ;; Build a state with wave-status
      (define state
        (struct-copy blackboard-state
                     empty-blackboard
                     [wave-status (hash "W0" "completed" "W1" "in-progress")]))
      (define snippet (build-blackboard-context-snippet state))
      (check-not-false snippet)
      (check-true (string-contains? snippet "[Blackboard]"))
      (check-true (string-contains? snippet "Waves:")))

    ;; ── Test 6: stop-blackboard-subscriber! is idempotent ──
    (test-case "stop-blackboard-subscriber! is safe when not subscribed"
      ;; current-subscription should be #f initially
      (set-box! current-subscription #f)
      ;; Calling stop when no subscription is active must not throw
      (check-not-exn (lambda () (stop-blackboard-subscriber!)))
      ;; Calling again is still safe
      (check-not-exn (lambda () (stop-blackboard-subscriber!)))
      (check-false (unbox current-subscription)))

    ;; ── v0.99.14 W3: Integration Tests ──

    (test-case "default settings enable blackboard (subscriber would start)"
      (define settings (make-settings (hash)))
      (check-true (blackboard-enabled? settings) "blackboard should be enabled by default"))

    (test-case "explicit #f disables blackboard (subscriber would not start)"
      (define settings (make-settings (hash 'mas (hash 'blackboard (hash 'enabled #f)))))
      (check-false (blackboard-enabled? settings)
                   "blackboard should be disabled when explicitly set to #f"))

    (test-case "injection only when blackboard is non-empty"
      (check-false (build-blackboard-context-snippet empty-blackboard))
      (define populated-state
        (struct-copy blackboard-state empty-blackboard [wave-status (hash "W0" "completed")]))
      (define snippet (build-blackboard-context-snippet populated-state))
      (check-not-false snippet)
      (check-true (string-contains? snippet "[Blackboard]")))

    (test-case "token budget: large blackboard state truncated to 500 chars"
      (define big-wave-status
        (for/hash ([i (in-range 60)])
          (values (format "Wave~a" i) (if (even? i) "completed" "in-progress"))))
      (define state (struct-copy blackboard-state empty-blackboard [wave-status big-wave-status]))
      (define snippet (build-blackboard-context-snippet state))
      (check-not-false snippet)
      (check-true (string? snippet))
      (check-true (<= (string-length snippet) MAX-BLACKBOARD-SNIPPET-LEN)
                  "snippet must not exceed 500 chars")
      (check-true (string-suffix? snippet "...") "truncated snippet must end with ..."))))

(run-tests deployment-gate-suite)
