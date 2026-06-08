#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: unit
;; @suite runtime
;; @boundary unit
;; @speed fast
;; @mutates none
;; tests/test-goal-loop-scenarios.rkt — Goal loop scenario tests (v0.83.6 W0)
;;
;; Tests the goal scenario harness and verifies goal-run! behavior
;; with mock providers and fake run-prompt functions.

(require rackunit
         racket/list
         racket/string
         "../runtime/goal/goal-types.rkt"
         "../runtime/goal/goal-runner.rkt"
         "../llm/model.rkt"
         "../llm/provider.rkt"
         "../util/loop-result.rkt"
         "helpers/goal-scenarios.rkt"
         "helpers/provider-scenarios.rkt")

;; ---------------------------------------------------------------------------
;; Capture infrastructure
;; ---------------------------------------------------------------------------

(test-case "make-goal-capture: fresh capture is empty"
  (define cap (make-goal-capture))
  (check-equal? (goal-capture-events cap) '())
  (check-equal? (goal-capture-statuses cap) '())
  (check-false (goal-capture-state cap)))

(test-case "goal-capture-reset! clears all state"
  (define cap (make-goal-capture))
  ((make-on-event cap) 'test (hash 'a 1))
  ((make-on-status cap) "status msg")
  (check-equal? (length (goal-capture-events cap)) 1)
  (goal-capture-reset! cap)
  (check-equal? (goal-capture-events cap) '())
  (check-equal? (goal-capture-statuses cap) '()))

(test-case "make-on-event captures events in order"
  (define cap (make-goal-capture))
  (define on-evt (make-on-event cap))
  (on-evt 'a (hash))
  (on-evt 'b (hash))
  (on-evt 'c (hash))
  (define evts (goal-capture-events cap))
  (check-equal? (length evts) 3)
  (check-equal? (car (car evts)) 'a))

(test-case "make-on-status captures in order"
  (define cap (make-goal-capture))
  (define on-status (make-on-status cap))
  (on-status "first")
  (on-status "second")
  (check-equal? (goal-capture-statuses cap) '("first" "second")))

;; ---------------------------------------------------------------------------
;; Fake run-prompt
;; ---------------------------------------------------------------------------

(test-case "make-fake-run-prompt returns loop-result"
  (define fake-text (scenario-text "hello"))
  (define resp (make-loop-result (list (hash 'text "hello")) 'stop (hash)))
  (define fake-run (make-fake-run-prompt (list resp)))
  ;; fake-run takes (sess msg) and returns (values sess loop-result)
  (define-values (sess result) (fake-run #f "test"))
  (check-true (loop-result? result)))

;; ---------------------------------------------------------------------------
;; Goal provider factories
;; ---------------------------------------------------------------------------

(test-case "make-goal-provider returns valid provider"
  (define prov (make-goal-provider))
  (check-true (provider? prov)))

(test-case "make-goal-provider-no-progress returns valid provider"
  (define prov (make-goal-provider-no-progress))
  (check-true (provider? prov)))

(test-case "make-goal-provider-tool-timeout returns valid provider"
  (define prov (make-goal-provider-tool-timeout))
  (check-true (provider? prov)))

;; ---------------------------------------------------------------------------
;; Shutdown helpers
;; ---------------------------------------------------------------------------

(test-case "make-fake-shutdown-check returns #f"
  (define-values (check count bump) (make-fake-shutdown-check))
  (check-false (check)))

(test-case "make-immediate-shutdown returns #t"
  (define shutdown (make-immediate-shutdown))
  (check-true (shutdown)))

;; ---------------------------------------------------------------------------
;; goal-run! with mock components
;; ---------------------------------------------------------------------------

(test-case "goal-run! terminates with shutdown check"
  ;; Test that goal-run! respects shutdown-check
  (define cap (make-goal-capture))
  (define prov (make-goal-provider))
  ;; Use immediate shutdown so the loop exits immediately
  (define result (goal-run!
                  "test goal"
                  prov
                  "test-model"
                  (lambda (prompt)
                    (values #f (make-loop-result '() 'stop (hash))))
                  #:max-turns 2
                  #:on-event (make-on-event cap)
                  #:on-status (make-on-status cap)
                  #:shutdown-check (make-immediate-shutdown)))
  (check-true (goal-state? result)))

(test-case "goal-run! emits goal-started event"
  (define cap (make-goal-capture))
  (define prov (make-goal-provider))
  (define result (goal-run!
                  "test goal"
                  prov
                  "test-model"
                  (lambda (prompt)
                    (values #f (make-loop-result '() 'stop (hash))))
                  #:max-turns 1
                  #:on-event (make-on-event cap)
                  #:on-status (make-on-status cap)
                  #:shutdown-check (make-immediate-shutdown)))
  (define evts (goal-capture-events cap))
  (check-true (>= (length evts) 1))
  (check-equal? (car (car evts)) 'goal-started))

(test-case "goal-run! respects max-turns cap"
  (define cap (make-goal-capture))
  (define prov (make-goal-provider))
  (define turn-count (box 0))
  (define result (goal-run!
                  "test goal"
                  prov
                  "test-model"
                  (lambda (prompt)
                    (set-box! turn-count (add1 (unbox turn-count)))
                    (values #f (make-loop-result '() 'stop (hash))))
                  #:max-turns 3
                  #:on-event (make-on-event cap)
                  #:on-status (make-on-status cap)
                  #:shutdown-check (lambda () #f)))
  ;; Should have stopped due to max-turns
  (check-true (<= (goal-state-turns-used result) 3)))

(test-case "goal-run! status messages include goal text"
  (define cap (make-goal-capture))
  (define prov (make-goal-provider))
  (define result (goal-run!
                  "build a test"
                  prov
                  "test-model"
                  (lambda (prompt)
                    (values #f (make-loop-result '() 'stop (hash))))
                  #:max-turns 1
                  #:on-event (make-on-event cap)
                  #:on-status (make-on-status cap)
                  #:shutdown-check (make-immediate-shutdown)))
  (define statuses (goal-capture-statuses cap))
  (check-true (>= (length statuses) 1))
  (check-true (ormap (lambda (s) (string-contains? s "build a test")) statuses)))

;; ---------------------------------------------------------------------------
;; goal-state construction
;; ---------------------------------------------------------------------------

(test-case "make-goal-state creates valid state"
  (define st (make-goal-state #:goal-text "test goal" #:max-turns 5))
  (check-equal? (goal-state-goal-text st) "test goal")
  (check-equal? (goal-state-max-turns st) 5)
  (check-equal? (goal-state-status st) 'active)
  (check-equal? (goal-state-turns-used st) 0))

;; ---------------------------------------------------------------------------
;; make-goal-provider-per-turn-cap (F3)
;; ---------------------------------------------------------------------------

(test-case "per-turn-cap: provider returns responses without converging"
  (define prov (make-goal-provider-per-turn-cap #:iterations 5))
  ;; Each send should return a text response
  (for ([i (in-range 5)])
    (define resp (provider-send prov (make-model-request '() '() (hash))))
    (check-true (model-response? resp))))

(test-case "per-turn-cap: provider exhausts after max iterations"
  (define prov (make-goal-provider-per-turn-cap #:iterations 3))
  ;; Consume all 3 responses
  (for ([i (in-range 3)])
    (provider-send prov (make-model-request '() '() (hash))))
  ;; 4th call returns last response again (scenario provider wraps around)
  (define resp (provider-send prov (make-model-request '() '() (hash))))
  (check-true (model-response? resp)))
