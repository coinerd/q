#lang racket/base

;; q/tests/test-goal-runner-timeout.rkt — W0 v0.99.78 (G-4): wall-clock turn cap
;;
;; Contract:
;;  - A goal turn NEVER exceeds #:turn-timeout-secs + grace, even when the
;;    tool call blocks forever (semaphore that never releases).
;;  - A timed-out turn records a synthetic evaluation on the goal state:
;;    achieved? #f, reason containing "timeout" — so the loop can evaluate
;;    and continue instead of stalling.
;;  - A SIGSTOP'd subprocess spawned by the blocking tool call is killed via
;;    the v0.99.77 SIGKILL-to-process-group primitive when the cap fires:
;;    run-subprocess returns (nonzero exit), and no stopped process survives.
;;  - Normal (non-blocking) turns are unaffected and never falsely time out.

(require rackunit
         racket/list
         racket/string
         "../llm/provider.rkt"
         "../sandbox/subprocess.rkt"
         (only-in "../runtime/goal/goal-state.rkt"
                  make-goal-state
                  goal-state-status
                  goal-state-turns-used
                  goal-state-last-evaluation
                  evaluation-result?
                  evaluation-result-achieved?
                  evaluation-result-reason)
         (only-in "../runtime/goal/goal-runner.rkt" goal-loop-step))

;; ------------------------------------------------------------
;; Mock provider — never actually reached on the timeout path.
;; ------------------------------------------------------------

(define (make-mock-provider)
  (make-provider (lambda () "mock-eval")
                 (lambda () (hash 'streaming #f 'token-counting #t))
                 (lambda (req) (hash 'role "assistant" 'content "mock"))
                 (lambda (req) (hash 'role "assistant" 'content "mock"))))

;; ------------------------------------------------------------
;; Prompt fns
;; ------------------------------------------------------------

(define never-release (make-semaphore 0))

;; Blocks forever — simulates a tool call that never returns.
(define (blocking-prompt! prompt)
  (semaphore-wait never-release)
  (values #f '()))

;; Returns immediately — the normal (non-blocking) path.
(define (immediate-prompt! prompt)
  (values #f (hash 'messages (list (hasheq 'role "assistant" 'content "All tests pass now.")))))

;; ------------------------------------------------------------
;; Event/status capture
;; ------------------------------------------------------------

(define (record-events)
  (define events '())
  (define statuses '())
  (values (lambda (type data) (set! events (cons (cons type data) events)))
          (lambda (msg) (set! statuses (cons msg statuses)))
          (lambda () events)
          (lambda () statuses)))

;; ------------------------------------------------------------
;; Test 1: a blocking tool call times out; the turn is recorded
;; ------------------------------------------------------------

(let-values ([(on-event on-status get-events get-statuses) (record-events)])
  (define st (make-goal-state #:goal-text "do the thing" #:max-turns 4))
  (define start-ms (current-inexact-milliseconds))
  (define result-st
    (goal-loop-step st
                    (make-mock-provider)
                    "mock-eval"
                    blocking-prompt!
                    on-event
                    on-status
                    #:turn-timeout-secs 1))
  (define elapsed (- (current-inexact-milliseconds) start-ms))
  (check-true (< elapsed 6000)
              (format "turn returned within cap + grace (elapsed ~a ms)" (exact->inexact elapsed)))
  (check-equal? (goal-state-turns-used result-st) 1 "timed-out turn still counts as a turn")
  (define le (goal-state-last-evaluation result-st))
  (check-true (evaluation-result? le) "synthetic evaluation recorded on timeout")
  (check-false (evaluation-result-achieved? le) "timed-out turn evaluates as not-achieved")
  (check-true (string-contains? (evaluation-result-reason le) "timeout")
              (format "timeout reason recorded (got ~s)" (evaluation-result-reason le)))
  (check-not-false (assoc 'goal-turn-timed-out (get-events)) "goal-turn-timed-out event emitted")
  (check-equal? (goal-state-status result-st) 'active "timed-out turn leaves goal active"))

;; ------------------------------------------------------------
;; Test 2: the normal (non-blocking) path is unaffected
;; ------------------------------------------------------------

(let-values ([(on-event on-status get-events get-statuses) (record-events)])
  (define st (make-goal-state #:goal-text "do the thing" #:max-turns 4))
  (define start-ms (current-inexact-milliseconds))
  (define result-st
    (goal-loop-step st
                    (make-mock-provider)
                    "mock-eval"
                    immediate-prompt!
                    on-event
                    on-status
                    #:turn-timeout-secs 10))
  (define elapsed (- (current-inexact-milliseconds) start-ms))
  (check-true (< elapsed 5000)
              (format "normal turn completes quickly (elapsed ~a ms)" (exact->inexact elapsed)))
  (check-equal? (goal-state-turns-used result-st) 1 "normal turn increments turns")
  (check-equal? (goal-state-status result-st) 'active)
  (check-true (evaluation-result? (goal-state-last-evaluation result-st)))
  (check-not-false (assoc 'goal-evaluated (get-events)) "goal-evaluated emitted")
  (check-false (assoc 'goal-turn-timed-out (get-events)) "no false timeout on normal path"))

;; ------------------------------------------------------------
;; Test 3: a SIGSTOP'd subprocess is killed when the cap fires
;; ------------------------------------------------------------

(define stop-marker "goal-stop-3.14159")
(define result-box (box #f))

;; Spawns a SIGSTOP'd setsid process group, then blocks forever.
(define (blocking-subprocess-prompt! prompt)
  (set-box! result-box
            (run-subprocess "bash"
                            #:args (list "-c" (format "kill -STOP $$; sleep ~a" stop-marker))
                            #:timeout 60
                            #:process-group? #t))
  (semaphore-wait never-release)
  (values #f '()))

(define setsid-available? (and (eq? (system-type) 'linux) (find-executable-path "setsid")))

(define (lingering-count marker)
  (define res
    (run-subprocess "bash"
                    #:args
                    (list "-c" (format "ps -eo args | grep -F '~a' | grep -v grep | wc -l" marker))))
  (string->number (string-trim (subprocess-result-stdout res))))

(when setsid-available?
  (test-case "timeout kills SIGSTOP'd process group"
    (define st (make-goal-state #:goal-text "do the thing" #:max-turns 4))
    (define start-ms (current-inexact-milliseconds))
    (define result-st
      (goal-loop-step st
                      (make-mock-provider)
                      "mock-eval"
                      blocking-subprocess-prompt!
                      void
                      void
                      #:turn-timeout-secs 1))
    (define elapsed (- (current-inexact-milliseconds) start-ms))
    (check-true (< elapsed 8000)
                (format "cap + grace bounded with blocked subprocess (elapsed ~a ms)"
                        (exact->inexact elapsed)))
    (define le (goal-state-last-evaluation result-st))
    (check-true (evaluation-result? le))
    (check-false (evaluation-result-achieved? le))
    (check-not-false (unbox result-box) "blocked run-subprocess returned once the group was killed")
    (define sp-res (unbox result-box))
    (check-not-equal? (subprocess-result-exit-code sp-res)
                      0
                      (format "group-killed child exits nonzero (got ~a)"
                              (subprocess-result-exit-code sp-res)))
    (check-equal? (lingering-count stop-marker)
                  0
                  "no surviving stopped subprocess after the cap fires")))
