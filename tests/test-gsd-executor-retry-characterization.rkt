#lang racket

;; W0 characterization baseline: executor plane retry / completion semantics.
;;
;; These tests FREEZE current behavior. They are not endorsements:
;;  * 'approved-empty counting as a completed run (no retry) is pinned as-is;
;;  * the health-gated retry path (call-with-provider-retry) stopping after
;;    attempts=2 even when #:health-failure-threshold is 3 is a KNOWN BUG
;;    (BUG-0022) pinned here so W1B/W2B must flip it in a reviewed diff
;;    against this file.
;;
;; Verify: racket q/tests/test-gsd-executor-retry-characterization.rkt

(require rackunit
         rackunit/text-ui
         racket/file
         racket/tcp
         racket/runtime-path
         (only-in racket/string string-contains?)
         (only-in "../extensions/gsd/prompts.rkt" planning-implement-prompt executor-reanchor-prompt)
         (prefix-in eb: "../util/event/event-bus.rkt")
         "../runtime/auto-retry.rkt"
         "../runtime/provider-retry.rkt"
         "../runtime/provider-health.rkt"
         "../llm/provider-errors.rkt"
         "../llm/conn-pool.rkt"
         "../extensions/gsd/go-orchestrator.rkt")

;; this file lives at <repo>/q/tests/ — two levels up is the repo root.
(define-runtime-path here-dir ".")
(define repo-root (simplify-path (build-path here-dir 'up 'up)))
(define (src mod-rel)
  (file->string (build-path repo-root mod-rel)))

(define prompts-src (src (build-path "q" "extensions" "gsd" "prompts.rkt")))
(define verifier-src (src (build-path "q" "extensions" "gsd" "delivery-verifier.rkt")))
(define orchestrator-src (src (build-path "q" "extensions" "gsd" "go-orchestrator.rkt")))
(define helpers-src (src (build-path "q" "tools" "builtins" "spawn-subagent-helpers.rkt")))
(define retry-src (src (build-path "q" "runtime" "auto-retry.rkt")))
(define provider-retry-src (src (build-path "q" "runtime" "provider-retry.rkt")))
(define conn-pool-src (src (build-path "q" "llm" "conn-pool.rkt")))

;; The transient network failure shape whose retry budget BUG-0022 truncates.
(define no-status-line-msg "no response status line from peer")

;; A provider network error of the exact shape raised by llm/conn-pool.rkt
;; when the first request on a server-closed idle entry surfaces.
(define (net-err)
  (provider-error (format "llm/conn-pool: ~a" no-status-line-msg)
                  (current-continuation-marks)
                  (hasheq)
                  'network
                  #f))

(define (contains? haystack needle)
  (check-true (string-contains? haystack needle) (format "expected source to contain: ~s" needle)))

;; ============================================================
;; Suite
;; ============================================================

(define (executor-retry-characterization-suite)
  (test-suite "gsd-executor-retry-characterization (W0 baseline)"

    ;; ── 1. Executor prompt template invariants (prompts.rkt) ──
    (test-case "executor prompt template pins"
      (check-pred string? planning-implement-prompt)
      (check-true (string-contains? planning-implement-prompt
                                    "[gsd-planning] EXECUTE the plan below"))
      (check-true (string-contains? planning-implement-prompt "CRITICAL RULES:"))
      (for ([rule '("1. Do NOT re-read the plan."
                    "2. Do NOT write a new plan."
                    "3. Do NOT use planning-write during implementation."
                    "4. Read each target file BEFORE editing it."
                    "5. After reading, apply the edits specified in the wave doc actions."
                    "6. After completing the assigned wave, run its verify command."
                    "7. Do NOT call /wave-done")])
        (check-true (string-contains? planning-implement-prompt rule)
                    (format "missing CRITICAL RULE: ~a" rule)))
      ;; The template is a PREFIX: the wave/plan body is appended after it.
      (check-true (string-contains? planning-implement-prompt
                                    "The plan follows. Start implementing immediately."))
      (check-false (string-contains? planning-implement-prompt "W1")
                   "template must be wave-agnostic (prefix only)")
      ;; And the composed prompt appends the caller-supplied plan body after
      ;; that prefix (planning-prompt is the composing function).
      (contains? prompts-src "(define (planning-prompt user-request)"))

    ;; ── 2. Delivery-verifier "no wave target files changed" shape ──
    (test-case "delivery-verifier unchanged-targets message shape"
      ;; delivery-verifier.rkt:288 — when no declared target changed, the
      ;; message lists ALL declared targets verbatim, comma-space-joined.
      (contains? verifier-src "(format \"no wave target files changed: ~a\"")
      (contains? verifier-src "(string-join files \", \")")
      ;; The pass branch is the positive counterpart.
      (contains? verifier-src "(format \"changed: ~a\"")
      ;; The verifier exports its verdict type for the orchestrator.
      (contains? verifier-src "delivery-verification"))

    ;; ── 3. run-campaign-wave: verifier rejection => 'wave-failed ──
    (test-case "go-orchestrator maps verifier rejection to wave-failed"
      (contains? orchestrator-src "'wave-failed")
      ;; try-complete-wave! is the integration point that consults the
      ;; delivery verifier before declaring a wave complete.
      (contains? orchestrator-src "try-complete-wave!")
      ;; The verifier's message is preserved verbatim into the recorded
      ;; outcome: the failure branch re-uses delivery-verification-message
      ;; and only falls back to "verifier rejected" when it is empty.
      (contains? orchestrator-src "(delivery-verification-message verifier-result)")
      (contains? orchestrator-src "\"verifier rejected\"")
      (contains? orchestrator-src "\"unexpected completion state\"")
      ;; campaign-result is the transparent outcome struct.
      (contains? orchestrator-src
                 "(struct campaign-result (status completed-waves message) #:transparent)"))

    ;; ── 4. Empty-result mapping: 'complete + no content => 'approved-empty ──
    (test-case "spawn-subagent-helpers empty-result mapping"
      (contains? helpers-src
                 "[(eq? raw-status 'complete) (if has-content? 'completed 'approved-empty)]")
      ;; ...and 'approved-empty is a TERMINAL-COMPLETED status today: no retry.
      (contains? helpers-src "(memq terminal-status '(completed approved-empty))"))

    ;; ── 5. Retry budget (behavioral pins; BUG-0022 prerequisite) ──
    (test-case "BEHAVIORAL: with-auto-retry alone honors #:max-retries 5"
      ;; The bare retry loop is CORRECT: a network provider-error of the
      ;; "no response status line from peer" shape gets the full budget —
      ;; 1 initial call + 5 retries — then surfaces retry-exhausted with the
      ;; last error type preserved. This isolates the health gate (next case)
      ;; as the truncator, not the loop itself.
      (parameterize ([current-auto-retry-sleep-scale 0.0])
        (define calls 0)
        (define exn
          (with-handlers ([retry-exhausted? values]
                          [exn:fail? (lambda (e)
                                       (fail-check (format "expected retry-exhausted, got: ~a"
                                                           (exn-message e)))
                                       e)])
            (with-auto-retry (lambda ()
                               (set! calls (add1 calls))
                               (raise (net-err)))
                             #:max-retries 5
                             #:on-retry (lambda args (void)))
            #f))
        (check-pred retry-exhausted? exn)
        (check-eq? (retry-exhausted-attempts exn)
                   5
                   "full budget: 5 retries under with-auto-retry alone")
        (check-eq? (retry-exhausted-last-error-type exn) 'network)
        (check-equal? calls 6 "1 initial attempt + 5 retries")))

    (test-case "KNOWN-BUG pin (BUG-0022): call-with-provider-retry truncates at attempts=2 below threshold 3"
      ;; BEHAVIORAL, frozen as-is: with a LIVE health tracker whose failure
      ;; threshold is 3, call-with-provider-retry raises retry-exhausted after
      ;; attempts=2 for the same transient shape that with-auto-retry (above)
      ;; retries 5 times. The wrapped budget is truncated by the health-gated
      ;; path, not by the loop. W2B must flip this pin to attempts=5.
      (parameterize ([current-auto-retry-sleep-scale 0.0])
        (define bus (eb:make-event-bus))
        (define tracker (make-provider-health))
        (define exn
          (with-handlers ([retry-exhausted? values]
                          [exn:fail? (lambda (e)
                                       (fail-check (format "expected retry-exhausted, got: ~a"
                                                           (exn-message e)))
                                       e)])
            ;; attempt-proc is re-invoked per attempt with (context settings)
            (call-with-provider-retry (lambda args (raise (net-err)))
                                      '()
                                      (hasheq)
                                      bus
                                      "w0-characterization-sess"
                                      "w0-characterization-turn"
                                      60
                                      #:health-tracker tracker
                                      #:health-failure-threshold 3)
            #f))
        (check-pred retry-exhausted? exn)
        (check-eq? (retry-exhausted-attempts exn)
                   2
                   "KNOWN BUG (BUG-0022): budget truncated to 2 retries today; W2B flips this")))

    (test-case "retry plumbing pins"
      (contains? retry-src "with-auto-retry")
      (contains? retry-src "#:max-retries")
      (contains? provider-retry-src "call-with-provider-retry")
      (check-equal? no-status-line-msg "no response status line from peer")
      (contains? retry-src "provider-errors.rkt"))

    ;; ── 5b. W3 (#9514/#9515): retry-prompt hardening pins ──
    (test-case "BEHAVIORAL: executor-reanchor-prompt anchors role + order"
      (define prompt
        (executor-reanchor-prompt "W3"
                                  "campaign-9514"
                                  "W3: harden retry prompts"
                                  "(read: go-orchestrator.rkt)"))
      (check-pred string? prompt)
      (check-true (string-contains? prompt "W3"))
      (check-true (string-contains? prompt "campaign-9514"))
      (check-true (string-contains? prompt "W3: harden retry prompts"))
      ;; Imperative continue instruction — the v1.00.16 W3 attempt-2
      ;; failure mode was asking "What would you like to do next?".
      (check-true (or (string-contains? prompt "continue")
                      (string-contains? prompt "Continue")
                      (string-contains? prompt "CONTINUE"))
                  "re-anchor prompt must order continuation")
      ;; Role re-anchor: verbatim executor role line (case-insensitive:
      ;; the template header is "EXECUTOR RE-ANCHOR").
      (define prompt-lc (string-downcase prompt))
      (check-true (string-contains? prompt-lc "executor")
                  "re-anchor prompt must restate the executor role")
      ;; Last tool-result excerpt is carried so the model retains context.
      (check-true (string-contains? prompt "(read: go-orchestrator.rkt)"))
      ;; Pure function: same args => same prompt (no I/O, no gensym).
      (check-equal? prompt
                    (executor-reanchor-prompt "W3"
                                              "campaign-9514"
                                              "W3: harden retry prompts"
                                              "(read: go-orchestrator.rkt)")))

    (test-case "BEHAVIORAL: no-change rejection classification"
      ;; Only the exact verifier shape "no wave target files changed: ..."
      ;; arms the #9515 failure-context retry.
      (check-true (no-change-rejection? "no wave target files changed: a.rkt, b.rkt"))
      (check-false (no-change-rejection? "verifier rejected"))
      (check-false (no-change-rejection? ""))
      ;; Bare outcome text (no file list) still classifies: the predicate
      ;; is prefix-based and the verifier always emits the files suffix.
      (check-true (no-change-rejection? "no wave target files changed"))
      (check-equal? (no-change-target-files "no wave target files changed: a.rkt, b.rkt")
                    '("a.rkt" "b.rkt")))

    (test-case "W3 structural pins: exactly-once no-change retry"
      ;; run-campaign-wave takes the bounded retry budget (default 1 from the
      ;; current-gsd-wave-no-change-retries parameter).
      (contains? orchestrator-src
                 "#:no-change-retries [no-change-retries (current-gsd-wave-no-change-retries)]")
      (contains? orchestrator-src "(> no-change-retries-left 0)")
      ;; Exactly-once: the retry consumes the budget.
      ;; W7 campaign: run-once was renamed run-once* (worktree boxes added);
      ;; the exactly-once decremented re-entry invariant is unchanged.
      (contains? orchestrator-src "(run-once* (sub1 no-change-retries-left)")
      ;; At-least-once semantics preserved: the wave is reset to pending and
      ;; persisted BEFORE the re-run, so a crash leaves it re-attemptable.
      (contains? orchestrator-src "(set-campaign-wave-status! retry-wave 'pending)")
      (contains? orchestrator-src "(persist-campaign! base-dir retry-rec)")
      ;; The retry carries the failure context block into the executor prompt.
      (contains? orchestrator-src "current-gsd-wave-failure-context")
      ;; Other verifier rejection messages fall through to permanent failure
      ;; with the verbatim verifier message preserved.
      (contains? orchestrator-src "\"verifier rejected\""))

    (test-case "W3 structural pins: re-anchor wiring into the session layer"
      ;; command-handlers launches the wave-executor session with the
      ;; re-anchor prompt installed as the empty-response nudge, so a
      ;; reasoning-only turn retries as the implementation executor.
      (contains? (src (build-path "q" "extensions" "gsd" "command-handlers.rkt"))
                 "[current-empty-response-nudge reanchor]")
      ;; step-executor consumes the parameter: the nudge is role-anchored
      ;; when set, generic otherwise (interactive sessions unaffected).
      (contains? (src (build-path "q" "runtime" "iteration" "step-executor.rkt"))
                 "(current-empty-response-nudge)")
      ;; prompts.rkt declares the pure constructor.
      (contains? prompts-src "(define (executor-reanchor-prompt"))

    ;; ── 6. Pool: server-closed idle entry surfaces today (W1B flips) ──
    (test-case "BEHAVIORAL: pool hands back a server-closed idle entry without liveness probe"
      ;; Today the pool performs NO liveness validation on the acquire path:
      ;; an idle entry whose peer has already closed the socket is handed out
      ;; again as a hit, so the FIRST REQUEST on it surfaces the
      ;; "llm/conn-pool: no response status line from peer" error to the
      ;; caller (raise site pinned below). W1B flips this to a transparent
      ;; retry — this test must be updated in that reviewed diff.
      (define listener-box (box #f))
      (define listener-port
        (let probe ([candidate 41731])
          (with-handlers ([exn:fail:network? (lambda (_) (probe (add1 candidate)))])
            (begin0 candidate
              (set-box! listener-box (tcp-listen candidate 16 #t "127.0.0.1"))))))
      (define listener (unbox listener-box))
      ;; Server: accept every connection and close it immediately, so any
      ;; pooled entry is server-closed the moment it goes idle.
      (define server-thread
        (thread (lambda ()
                  (with-handlers ([exn:fail? void])
                    (let loop ()
                      (define-values (in out) (tcp-accept listener))
                      (close-input-port in)
                      (close-output-port out)
                      (loop))))))
      (define pool (make-conn-pool #:idle-ttl-secs 60))
      (define c1 (pool-acquire! pool "127.0.0.1" listener-port #f))
      (check-pred pooled-connection? c1)
      ;; Mirror what a cleanly parsed response head does inside
      ;; pool-send-request! (see mark-pool-reusable! docstring): after a
      ;; successful request this entry is reusable, so release checks it in.
      (mark-pool-reusable! c1)
      (pool-release! pool c1) ; checked in as idle; peer closes it immediately
      (define c2 (pool-acquire! pool "127.0.0.1" listener-port #f))
      ;; The server-closed idle entry is handed straight back out (same
      ;; underlying socket, check-in shares the ports): no liveness probe,
      ;; no eviction on acquire. The failure can only surface on the first
      ;; request — exactly what W1B will turn into a transparent retry.
      (check-pred pooled-connection? c2)
      (check-eq? (pooled-connection-in c2)
                 (pooled-connection-in c1)
                 "server-closed idle entry is re-acquired verbatim (no probe/eviction)")
      (check-true (pooled-connection-reusable? c2))
      (mark-pool-fault! c2)
      (pool-release! pool c2 #:outcome 'fault)
      (pool-shutdown! pool)
      (kill-thread server-thread)
      (tcp-close listener)
      ;; Raise site for the first-request error that surfaces today.
      (contains? conn-pool-src "\"llm/conn-pool: no response status line from peer\""))))

(module+ main
  (exit (run-tests (executor-retry-characterization-suite))))
