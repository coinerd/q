#lang racket/base

;; q/runtime/goal-runner.rkt — Main goal loop orchestration
;;
;; Orchestrates the autonomous goal loop:
;; 1. User sets goal via /goal command
;; 2. Runner loops: run-prompt! → evaluate → continue/stop
;; 3. Enforces max-turns, shutdown checks, no-progress detection
;; 4. Emits typed events at each phase

(require racket/contract
         racket/match
         racket/format
         racket/string
         racket/list
         racket/port
         racket/system
         racket/function
         (except-in "goal-state.rkt" NO-PROGRESS-THRESHOLD)
         (only-in "goal-state.rkt"
                  NO-PROGRESS-THRESHOLD
                  make-evaluation-result
                  DEFAULT-GOAL-TURN-TIMEOUT-SECS
                  evaluation-result?
                  evaluation-result-achieved?
                  evaluation-result-reason
                  evaluation-result-token-cost
                  check-result?
                  check-result-label
                  check-result-exit-code
                  check-result-timed-out?
                  check-result-stdout
                  check-result-stderr
                  goal-check?)
         "goal-evaluator.rkt"
         "goal-agent-evaluator.rkt"
         "goal-evidence.rkt"
         "goal-checks.rkt"
         "../../llm/provider.rkt"
         (only-in "../../util/time.rkt" now-epoch-ms)
         (only-in "../../util/loop-result.rkt" loop-result? loop-result-messages make-loop-result)
         ;; v0.99.78 W1 (G-8): persist evaluator decisions to the session log
         (only-in "../session/session-store-goal-task.rkt" append-evaluation-result!)
         ;; v0.99.78 W2 (G-9): persist goal-state snapshots on every mutation
         (only-in "../session/session-store-goal-task.rkt" append-goal-state-snapshot!)
         ;; v0.99.78 W3 (G-5): evidence provenance — load persisted evidence
         ;; so the loop can reject stale results after a base/tree change.
         (only-in "../session/session-store-goal-task.rkt" load-goal-evidence)
         (only-in "../../util/ids.rkt" generate-id)
         (only-in "../session/session-store-goal-task.rkt" append-evidence-result!))

;; v0.99.78: evaluator wall-clock timeout (overridable for tests)
(define current-eval-timeout-secs (make-parameter 60))

;; ============================================================
;; Provides
;; ============================================================

(provide goal-run!
         goal-run-simulated!
         goal-loop-step
         build-continuation-prompt
         current-eval-timeout-secs
         collect-evaluations
         execute-checks-for-goal
         extract-transcript-from-result
         current-goal-session-log-path
         current-simulated-prompt-sink
         current-repo-base-sha
         current-working-tree-hash)

;; v0.99.78 W6: test instrumentation for simulated prompts. Default is a
;; no-op; production behavior is unchanged.
(define current-simulated-prompt-sink (make-parameter void))

;; v0.99.78 W1 (G-8): session log path for persisting evaluator decisions.
;; When set, each evaluation is appended to the session log as a structured
;; `goal.evaluation` entry (payload identical to the `goal.evaluated` event).
(define current-goal-session-log-path (make-parameter #f))

;; v0.99.78 W2 (G-9): write a JSON-safe goal-state snapshot (kind `goal.state`)
;; whenever the session log path is set. Called after every goal-state
;; mutation so the persisted snapshot and the live state cannot diverge.
(define (persist-goal-state-snapshot! goal-st)
  (define log-path (current-goal-session-log-path))
  (when log-path
    (append-goal-state-snapshot! log-path goal-st)))

;; ============================================================
;; v0.99.78 W3 (G-5): evidence provenance helpers
;; ============================================================

;; Current base commit SHA (git rev-parse HEAD). #f if not a repo.
(define (current-repo-base-sha)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (define out (with-output-to-string (lambda () (system "git rev-parse HEAD 2>/dev/null"))))
    (define trimmed (string-trim out))
    (if (string=? trimmed "") #f trimmed)))

;; Working-tree hash: `git stash create` yields a commit object for the
;; current working tree without mutating it; if the tree is clean it
;; returns #f and we fall back to the base SHA (tree == base).
(define (current-working-tree-hash)
  (define base (current-repo-base-sha))
  (with-handlers ([exn:fail? (lambda (e) base)])
    (define out (with-output-to-string (lambda () (system "git stash create 2>/dev/null"))))
    (define trimmed (string-trim out))
    (if (or (string=? trimmed "") (string=? trimmed "false")) base trimmed)))

;; Inspect persisted `goal.evidence` entries against the current base SHA
;; and working-tree hash. Returns a list of re-verify instruction strings
;; for every STALE entry (base or tree moved since capture). Empty when
;; the session log path is unset, the store is empty, or all evidence is
;; current. Injected into the next turn prompt so old results are never
;; accepted after a base change (G-5).
(define (stale-evidence-instructions)
  (define log-path (current-goal-session-log-path))
  (define base (current-repo-base-sha))
  (define tree (current-working-tree-hash))
  (cond
    [(or (not log-path) (not base)) '()]
    [else
     (for/list ([ev (in-list (load-goal-evidence log-path))]
                #:when (evidence-stale? ev base tree))
       (reverify-instruction ev base tree))]))

;; Collect all evaluations from goal-state. The evaluations field is
;; authoritative; last-evaluation is appended only for legacy states with no
;; evaluations list, avoiding a double-count that could report no-progress
;; after two actual turns.
(define (collect-evaluations goal-st)
  (define from-field (goal-state-evaluations goal-st))
  (define last-eval (goal-state-last-evaluation goal-st))
  (if (and last-eval (null? from-field))
      (list last-eval)
      from-field))

;; Persist deterministic-check evidence with provenance captured around the
;; verification command. Base/tree bind the result to the exact code state.
(define (persist-check-evidence! check-results)
  (define log-path (current-goal-session-log-path))
  (define base (current-repo-base-sha))
  (define tree (current-working-tree-hash))
  (when (and log-path base tree (pair? check-results))
    (append-evidence-result! log-path
                             (make-evidence-provenance #:evidence-id (generate-id)
                                                       #:kind 'check
                                                       #:base-sha base
                                                       #:tree-hash tree
                                                       #:captured-at (now-epoch-ms)
                                                       #:result check-results))))

;; Execute all deterministic checks from goal-state and return results.
;; If no checks defined, returns empty list.
(define (execute-checks-for-goal goal-st)
  (define checks (goal-state-checks goal-st))
  (if (and (pair? checks) (goal-check? (car checks)))
      (execute-all-checks checks #:timeout 30)
      '()))

;; Return a failure reason when any deterministic check missed its expected
;; exit or timed out; #f when checks pass or no checks exist.
(define (failed-check-summary check-results)
  (for/first ([cr (in-list check-results)]
              #:when (or (check-result-timed-out? cr) (not (zero? (check-result-exit-code cr)))))
    (if (check-result-timed-out? cr)
        (format "check ~a timed out" (check-result-label cr))
        (format "check ~a failed with exit ~a" (check-result-label cr) (check-result-exit-code cr)))))

;; ============================================================
;; Main entry point
;; ============================================================

(define/contract (goal-run! goal-text
                            provider
                            evaluator-model
                            run-prompt-fn!
                            #:max-turns [max-turns 8]
                            #:evaluator-mode [evaluator-mode 'transcript]
                            #:checks [checks '()]
                            #:on-event [on-event void]
                            #:on-status [on-status void]
                            #:shutdown-check [shutdown-check (lambda () #f)]
                            #:turn-timeout-secs [turn-timeout-secs DEFAULT-GOAL-TURN-TIMEOUT-SECS])
  (->* (string? provider? string? procedure?)
       (#:max-turns exact-nonnegative-integer?
                    #:evaluator-mode symbol?
                    #:checks (listof goal-check?)
                    #:on-event procedure?
                    #:on-status procedure?
                    #:shutdown-check procedure?
                    #:turn-timeout-secs (or/c #f exact-positive-integer?))
       goal-state?)
  ;; Initialize goal state
  (define goal-st
    (make-goal-state #:goal-text goal-text
                     #:max-turns max-turns
                     #:evaluator-model evaluator-model
                     #:evaluator-mode evaluator-mode
                     #:checks checks))

  ;; Emit goal.started
  (on-event 'goal-started
            (hasheq 'goal-text goal-text 'max-turns max-turns 'checks (goal-state-checks goal-st)))
  (on-status (format "Goal set: ~a (max ~a turns)" goal-text max-turns))
  (persist-goal-state-snapshot! goal-st)

  ;; Run the loop
  (run-goal-loop goal-st
                 provider
                 evaluator-model
                 run-prompt-fn!
                 on-event
                 on-status
                 shutdown-check
                 #:turn-timeout-secs turn-timeout-secs))

;; ============================================================
;; Internal loop
;; ============================================================

(define (run-goal-loop goal-st
                       provider
                       evaluator-model
                       run-prompt-fn!
                       on-event
                       on-status
                       shutdown-check
                       #:turn-timeout-secs [turn-timeout-secs DEFAULT-GOAL-TURN-TIMEOUT-SECS])
  ;; Shutdown check — first priority
  (cond
    [(shutdown-check)
     (define final-st
       (struct-copy goal-state goal-st [status 'cancelled] [updated-at (now-epoch-ms)]))
     (on-event 'goal-failed
               (hasheq 'goal-text
                       (goal-state-goal-text final-st)
                       'reason
                       "cancelled"
                       'turns-used
                       (goal-state-turns-used final-st)))
     (on-status "Goal cancelled by user")
     (persist-goal-state-snapshot! final-st)
     final-st]

    ;; Max turns reached
    [(>= (goal-state-turns-used goal-st) (goal-state-max-turns goal-st))
     (define final-st (struct-copy goal-state goal-st [status 'failed] [updated-at (now-epoch-ms)]))
     (on-event 'goal-failed
               (hasheq 'goal-text
                       (goal-state-goal-text final-st)
                       'reason
                       "max turns reached"
                       'turns-used
                       (goal-state-turns-used final-st)))
     (on-status (format "Goal failed: max turns (~a) reached" (goal-state-max-turns goal-st)))
     (persist-goal-state-snapshot! final-st)
     final-st]

    [(detect-no-progress (collect-evaluations goal-st))
     (define final-st (struct-copy goal-state goal-st [status 'failed] [updated-at (now-epoch-ms)]))
     (on-event 'goal-failed
               (hasheq 'goal-text
                       (goal-state-goal-text final-st)
                       'reason
                       "no progress"
                       'turns-used
                       (goal-state-turns-used final-st)))
     (on-status "Goal failed: no progress — 3 consecutive same-reason evaluations")
     (persist-goal-state-snapshot! final-st)
     final-st]

    [else
     ;; Run one step
     (define updated-st
       (goal-loop-step goal-st
                       provider
                       evaluator-model
                       run-prompt-fn!
                       on-event
                       on-status
                       #:turn-timeout-secs turn-timeout-secs))
     (cond
       [(eq? (goal-state-status updated-st) 'achieved)
        (on-event 'goal-achieved
                  (hasheq 'goal-text
                          (goal-state-goal-text updated-st)
                          'turns-used
                          (goal-state-turns-used updated-st)
                          'total-token-cost
                          (goal-state-total-token-cost updated-st)))
        (on-status (format "Goal achieved in ~a turns!" (goal-state-turns-used updated-st)))
        (persist-goal-state-snapshot! updated-st)
        updated-st]
       [(eq? (goal-state-status updated-st) 'failed)
        (on-event 'goal-failed
                  (hasheq 'goal-text
                          (goal-state-goal-text updated-st)
                          'reason
                          (goal-state-last-evaluation-reason updated-st)
                          'turns-used
                          (goal-state-turns-used updated-st)))
        (on-status (format "Goal failed: ~a" (goal-state-last-evaluation-reason updated-st)))
        (persist-goal-state-snapshot! updated-st)
        updated-st]
       [else
        ;; Active — recurse with updated state
        (run-goal-loop updated-st
                       provider
                       evaluator-model
                       run-prompt-fn!
                       on-event
                       on-status
                       shutdown-check
                       #:turn-timeout-secs turn-timeout-secs)])]))

;; Extract reason from last evaluation safely
(define (goal-state-last-evaluation-reason gs)
  (define le (goal-state-last-evaluation gs))
  (if le
      (evaluation-result-reason le)
      "unknown"))

;; Sum token costs from all evaluations

;; ============================================================
;; Single step
;; ============================================================

(define/contract (goal-loop-step goal-st
                                 provider
                                 evaluator-model
                                 run-prompt-fn!
                                 on-event
                                 on-status
                                 #:turn-timeout-secs
                                 [turn-timeout-secs DEFAULT-GOAL-TURN-TIMEOUT-SECS])
  (->* (goal-state? provider? string? procedure? procedure? procedure?)
       (#:turn-timeout-secs (or/c #f exact-positive-integer?))
       goal-state?)
  (define turns (add1 (goal-state-turns-used goal-st)))
  (define goal-text (goal-state-goal-text goal-st))

  ;; Emit turn-started
  (on-event 'goal-turn-started (hasheq 'turn-number turns 'goal-text goal-text))
  (on-status (format "Goal turn ~a/~a: working..." turns (goal-state-max-turns goal-st)))

  ;; Build the prompt from the same instruction source covered by the W4
  ;; operating-rules contract, not a hand-assembled prompt that can omit them.
  (define sys-instructions
    (string-append
     (string-join (goal-system-instructions goal-st) "\n\n")
     "\n\n"
     (format
      "You are in an autonomous goal loop (turn ~a/~a). Goal: ~a\nProvide specific evidence: run commands, check files, show outputs."
      turns
      (goal-state-max-turns goal-st)
      goal-text)))
  (define prompt
    (if (= turns 1)
        (string-append sys-instructions "\n\n" goal-text)
        (string-append sys-instructions
                       "\n\n"
                       (evidence-prompt-for-goal goal-text (goal-state-last-evaluation goal-st)))))

  ;; v0.99.78 W3 (G-5): reject stale evidence in the loop. If any persisted
  ;; verification result is no longer current (base SHA or working-tree hash
  ;; moved since capture), inject a re-verify instruction into this turn so
  ;; the agent re-runs verification on the current code state instead of
  ;; accepting the old result.
  (define prompt-with-evidence-guidance
    (let ()
      (define stale-instrs (stale-evidence-instructions))
      (if (null? stale-instrs)
          prompt
          (string-append prompt "\n\n" (string-join stale-instrs "\n")))))
  ;; Run the prompt through the agent, bounded by the wall-clock turn cap
  ;; (G-4 v0.99.78). The prompt fn runs in a worker thread under a private
  ;; custodian so a blocking tool call cannot stall the loop forever: on
  ;; timeout we shut the custodian down (killing any subprocesses it spawned
  ;; — those launched via run-subprocess inherit the W1 v0.99.77 kill-after /
  ;; SIGKILL-to-process-group semantics) and kill the worker thread.
  (define turn-custodian (make-custodian))
  (define turn-ch (make-channel))
  (define (bounded-prompt-run!)
    (parameterize ([current-custodian turn-custodian])
      (with-handlers ([exn:fail? (lambda (e) (channel-put turn-ch (cons 'error e)))])
        (define-values (sess res) (run-prompt-fn! prompt-with-evidence-guidance))
        (channel-put turn-ch (cons 'ok (cons sess res))))))
  (define turn-worker (thread bounded-prompt-run!))
  (define turn-result (sync/timeout turn-timeout-secs turn-ch))
  (define turn-timed-out? (not turn-result))
  (when turn-timed-out?
    ;; Kill-after: shut down the custodian first (subprocesses under it get
    ;; SIGKILL'd), then kill the worker thread itself.
    (custodian-shutdown-all turn-custodian)
    (kill-thread turn-worker)
    (on-status
     (format "Goal turn ~a timed out after ~a secs (wall-clock cap)" turns turn-timeout-secs))
    (on-event 'goal-turn-timed-out (hasheq 'turn-number turns 'timeout-secs turn-timeout-secs)))
  (define-values (turn-error updated-sess loop-result)
    (match turn-result
      [(cons 'error e)
       (custodian-shutdown-all turn-custodian)
       (values e #f #f)]
      [(cons 'ok (cons sess res))
       (custodian-shutdown-all turn-custodian)
       (values #f sess res)]
      [#f (values #f #f #f)]))

  ;; Execute deterministic checks if the prompt completed, persist provenance,
  ;; and make their outcome authoritative: a passing transcript evaluator can
  ;; never override a failed/timed-out deterministic check.
  (define check-results
    (if (or turn-timed-out? turn-error)
        '()
        (execute-checks-for-goal goal-st)))
  (persist-check-evidence! check-results)

  ;; Emit goal-check-completed for each check result
  (for ([cr (in-list check-results)])
    (on-event 'goal-check-completed
              (hasheq 'label
                      (check-result-label cr)
                      'exit-code
                      (check-result-exit-code cr)
                      'timed-out?
                      (check-result-timed-out? cr)
                      'stdout
                      (check-result-stdout cr)
                      'stderr
                      (check-result-stderr cr))))

  ;; Evaluate the result
  ;; Extract transcript from loop-result for evaluation
  (define transcript (extract-transcript-from-result loop-result))
  ;; v0.99.78 FIX: bound the evaluator LLM request with a wall-clock cap.
  ;; The evaluator calls provider-send (non-streaming) which can stall on a
  ;; held request (deepseek returns 200 but never sends the body). Without a
  ;; bound the goal-loop froze for 600s+ per evaluation (observed live).
  ;; G-4 (W0): on turn timeout, record a synthetic not-achieved evaluation
  ;; instead of evaluating an empty transcript.
  (define eval-result
    (cond
      [turn-timed-out?
       (make-evaluation-result #:achieved? #f
                               #:reason (format "turn timeout after ~a secs (wall-clock cap)"
                                                turn-timeout-secs)
                               #:model-used evaluator-model
                               #:token-cost 0)]
      [turn-error
       (make-evaluation-result #:achieved? #f
                               #:reason (format "Worker error: ~a" (exn-message turn-error))
                               #:model-used evaluator-model
                               #:token-cost 0)]
      [(failed-check-summary check-results)
       =>
       (lambda (reason)
         (make-evaluation-result #:achieved? #f
                                 #:reason reason
                                 #:check-results check-results
                                 #:model-used evaluator-model
                                 #:token-cost 0))]
      [else
       (let ()
         (define eval-timeout-secs (current-eval-timeout-secs))
         (define eval-ch (make-channel))
         (define eval-worker
           (thread (lambda ()
                     (with-handlers ([exn:fail? (lambda (e) (channel-put eval-ch (cons 'error e)))])
                       (define result
                         (if (eq? (goal-state-evaluator-mode goal-st) 'agent)
                             (evaluate-with-agent goal-text
                                                  transcript
                                                  provider
                                                  evaluator-model
                                                  #:check-results check-results)
                             (evaluate-transcript goal-text
                                                  transcript
                                                  provider
                                                  evaluator-model
                                                  #:check-results check-results)))
                       (channel-put eval-ch (cons 'ok result))))))
         (match (sync/timeout eval-timeout-secs eval-ch)
           [(cons 'ok r) r]
           [(cons 'error e)
            (make-evaluation-result #:achieved? #f
                                    #:reason (format "Evaluator error: ~a" (exn-message e))
                                    #:model-used evaluator-model)]
           [#f
            (kill-thread eval-worker)
            (on-status (format "Goal turn ~a evaluator timed out after ~a s" turns eval-timeout-secs))
            (make-evaluation-result #:achieved? #f
                                    #:reason (format "evaluator timeout after ~a s" eval-timeout-secs)
                                    #:model-used evaluator-model)]))]))

  ;; Emit goal.evaluated
  (on-event 'goal-evaluated
            (hasheq 'achieved?
                    (evaluation-result-achieved? eval-result)
                    'reason
                    (evaluation-result-reason eval-result)
                    'turn-number
                    turns
                    'token-cost
                    (evaluation-result-token-cost eval-result)))

  ;; v0.99.78 W1 (G-8): persist the evaluator decision to the session log.
  ;; The persisted payload is built from the same evaluation-result as the
  ;; goal.evaluated event, so UI and log cannot diverge (event parity).
  (define session-log-path (current-goal-session-log-path))
  (when session-log-path
    (append-evaluation-result! session-log-path eval-result turns))

  ;; Update goal state. Worker exceptions and deterministic check failures are
  ;; terminal for the current goal: retrying the same failed command would
  ;; burn turns without new evidence. Evaluator timeouts/errors remain retryable.
  (define now (now-epoch-ms))
  (define terminal-worker-failure? (or turn-error (failed-check-summary check-results)))
  (define new-status
    (cond
      [(evaluation-result-achieved? eval-result) 'achieved]
      [terminal-worker-failure? 'failed]
      [else 'active]))

  (define updated-st
    (struct-copy goal-state
                 goal-st
                 [turns-used turns]
                 [status new-status]
                 [evaluations (append (goal-state-evaluations goal-st) (list eval-result))]
                 [last-evaluation eval-result]
                 [updated-at now]))
  ;; G-9 (W2): persist the goal-state snapshot on every mutation so the
  ;; session store is externally greppable without the live process.
  (persist-goal-state-snapshot! updated-st)
  ;; Worker exceptions are programming/configuration defects, not evidence
  ;; that the goal failed. Surface them to the caller after events/state are
  ;; persisted instead of waiting for a timeout or converting to an evaluation.
  (when turn-error
    (raise turn-error))
  updated-st)

;; ============================================================
;; Helpers
;; ============================================================

(define (build-continuation-prompt goal-text last-eval)
  (define reason
    (if last-eval
        (evaluation-result-reason last-eval)
        "unknown"))
  (format "The goal is not yet achieved. Reason: ~a. Continue working toward: ~a" reason goal-text))

;; Extract transcript from loop-result for evaluator
;; loop-result is a struct from the iteration loop
(define (extract-transcript-from-result loop-result)
  ;; Try to extract messages from the loop result
  ;; The loop-result may have an 'assistant-response or 'messages field
  (cond
    [(loop-result? loop-result) (loop-result-messages loop-result)]
    [(hash? loop-result)
     (define messages (hash-ref loop-result 'messages '()))
     (if (list? messages)
         messages
         '())]
    [(list? loop-result) loop-result]
    [else '()]))

;; ============================================================
;; Test helper: run a simulated goal loop (no real run-prompt!)
;; ============================================================

(define/contract (goal-run-simulated! goal-text
                                      provider
                                      evaluator-model
                                      turn-responses
                                      #:max-turns [max-turns 8]
                                      #:evaluator-mode [evaluator-mode 'transcript]
                                      #:shutdown-check [shutdown-check (lambda () #f)])
  (->* (string? provider? string? list?)
       (#:max-turns exact-nonnegative-integer? #:evaluator-mode symbol? #:shutdown-check procedure?)
       goal-state?)
  (when (null? turn-responses)
    (raise-argument-error 'goal-run-simulated! "non-empty list of turn responses" turn-responses))
  ;; Simulated run-prompt! that returns predefined responses
  (define turn-idx (box 0))
  (define (sim-run-prompt! prompt)
    (define resp
      (if (< (unbox turn-idx) (length turn-responses))
          (list-ref turn-responses (unbox turn-idx))
          (last turn-responses)))
    (set-box! turn-idx (add1 (unbox turn-idx)))
    (when (hash-has-key? resp 'explode)
      (raise (error (hash-ref resp 'explode))))
    ((current-simulated-prompt-sink) prompt)
    (values #f resp))

  (goal-run! goal-text
             provider
             evaluator-model
             sim-run-prompt!
             #:max-turns max-turns
             #:evaluator-mode evaluator-mode
             #:shutdown-check shutdown-check))
