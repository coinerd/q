#lang racket/base

;; runtime/iteration.rkt — agent iteration loop
;; STABILITY: evolving
;;
;; Handles the multi-turn tool-call iteration loop: run agent turn → check
;; for tool calls → execute tools → feed results back → repeat.
;;
;; Also provides shared helpers used by agent-session.rkt:
;;   now-seconds, emit-session-event!, maybe-dispatch-hooks
;;
;; Provider turn execution and context assembly live in turn-orchestrator.rkt.
;; This module re-exports those symbols for backward compatibility.
;;
;; ── LAYER NOTE ────────────────────────────────────────────────
;; Since v0.22.4 (MOD-01): Upward imports limited to a single constant from
;; extensions/message-inject.rkt. All behavioral upward imports moved to
;; turn-orchestrator.rkt. Listed as known exception in arch-boundaries tests.
;; ───────────────────────────────────────────────────────────────

(require racket/contract
         racket/list
         racket/set
         racket/path
         json
         (only-in racket/string string-trim string-join string-contains?)
         (only-in "../util/json-helpers.rkt" ensure-hash-args)
         "iteration/loop-state.rkt"
         "iteration/retry-policy.rkt"
         "iteration/tool-turn-bridge.rkt"
         "iteration/transition-logic.rkt"
         (only-in "../util/protocol-types.rkt"
                  message?
                  message-id
                  message-role
                  message-content
                  make-message
                  tool-result-part?
                  tool-result-part-is-error?
                  make-text-part
                  text-part?
                  text-part-text
                  tool-call-name
                  tool-call-arguments
                  make-loop-result
                  loop-result-termination-reason
                  loop-result-messages
                  loop-result-metadata
                  make-event
                  ;; FEAT-61: needed by injected-message collector
                  event-ev
                  event-payload)
         "../agent/event-bus.rkt"
         (only-in "../agent/queue.rkt"
                  dequeue-steering!
                  dequeue-followup!
                  dequeue-all-followups!
                  queue-status)
         "../agent/loop.rkt"
         ;; Settings struct for exec-context runtime-settings (#1240)
         (only-in "../runtime/settings.rkt" make-minimal-settings)
         "../runtime/session-store.rkt"
         "../runtime/tool-coordinator.rkt"
         (only-in "../runtime/compactor.rkt"
                  compaction-result-removed-count
                  compaction-result-kept-messages)
         ;; tool-coordinator re-exports these:
         ;;   extract-tool-calls-from-messages, make-tool-result-messages,
         ;;   handle-tool-calls-pending
         (only-in "../runtime/tool-coordinator.rkt"
                  handle-tool-calls-pending
                  extract-tool-calls-from-messages)
         "../util/ids.rkt"
         (only-in "../util/cancellation.rkt" cancellation-token? cancellation-token-cancelled?)
         ;; R2-6: Import hook-result accessors
         (only-in "../util/hook-types.rkt" hook-result-action hook-result-payload hook-result?)
         (only-in "../util/event-contracts.rkt"
                  budget-payload/c
                  compact-result-payload/c
                  error-detail-payload/c
                  injection-count-payload/c
                  iteration-decision-payload/c
                  reason-payload/c
                  turn-cancelled-payload/c)
         (only-in "../runtime/auto-retry.rkt" context-overflow-error?)
         ;; v0.14.1: mid-turn token budget check
         ;; estimate-context-tokens imported via DI parameter (see below)
         ;; QUAL-01 (v0.22.0): shared runtime helpers
         (only-in "runtime-helpers.rkt" emit-session-event! maybe-dispatch-hooks)
         ;; v0.22.4 (MOD-01): provider turn + context assembly + extension registration
         (only-in "turn-orchestrator.rkt"
                  run-provider-turn
                  build-assembled-context
                  register-session-extensions!)
         "working-set.rkt"
         (only-in "context-policy.rkt" estimate-message-tokens))

(provide run-iteration-loop
         ;; emit-session-event! and maybe-dispatch-hooks re-exported from runtime-helpers
         emit-session-event!
         maybe-dispatch-hooks
         ensure-hash-args ;; for testing
         ;; v0.18.0: steering helpers for testing
         extract-tool-target-path
         update-seen-paths
         ;; FEAT-61: message injection support
         make-injected-collector!
         drain-injected-messages!
         ;; FEAT-66: overflow recovery (for testing)
         call-with-overflow-recovery
         ;; v0.14.1: mid-turn token budget check (for testing)
         check-mid-turn-budget!
         ;; v0.20.5 W3: pre-registration on session open (now from turn-orchestrator)
         register-session-extensions!
         ;; DI parameters for testability (DI-01, v0.22.7)
         current-compact-proc
         current-estimate-tokens
         current-inject-topic
         ;; DI resolve functions (LOW-05, v0.22.8)
         resolve-compact-proc
         resolve-estimate-tokens
         resolve-inject-topic)

;; ============================================================
;; DI parameters (DI-01, v0.22.7; DI-04, v0.22.8)
;;
;; These parameters decouple iteration.rkt from concrete imports.
;; Callers (agent-session.rkt) set them at session init time.
;; Tests can override them with mocks.
;;
;; THREAD-LOCAL WARNING: These parameters are thread-local. They are
;; set by make-agent-session and resume-agent-session in the calling
;; thread. If run-iteration-loop is called from a different thread,
;; the parameters will be #f and fall through to lazy-require defaults
;; (for procedures) or direct imports (for values). This is safe for
;; TUI/CLI where iteration runs in the session thread. For SDK use
;; across threads, callers must set parameters before dispatching.
;; ============================================================

;; DI parameters imported from iteration/loop-state.rkt

;; ============================================================
;; Event payload contract assertions (v0.22.9 W0 T1)
;;
;; Lightweight inline validation for high-value events consumed
;; by extensions and TUI. Only applied to top events.
;; ============================================================

(define (assert-payload topic-name payload ctrct)
  (unless (ctrct payload)
    (raise-argument-error topic-name "valid event payload" payload))
  payload)

;; ============================================================
;; Shared helpers (QUAL-01: re-exported from runtime-helpers.rkt)
;; ============================================================

;; check-mid-turn-budget!: delegated to iteration/retry-policy.rkt

;; ============================================================
;; Iteration-loop helpers (private)
;; ============================================================

;; Extract tool-call structs from assistant messages' tool-call-parts
;; extract-tool-calls-from-messages: moved to runtime/tool-coordinator.rkt
;; make-tool-result-messages: moved to runtime/tool-coordinator.rkt

;; ============================================================
;; Queue helpers
;; ============================================================

;; dequeue-all-steering! moved to iteration/tool-turn-bridge.rkt

;; ============================================================
;; FEAT-66: Context overflow recovery
;; ============================================================

;; call-with-overflow-recovery moved to iteration/retry-policy.rkt

;; ============================================================
;; FEAT-61: Message injection drain
;; ============================================================

;; drain-injected-messages! moved to iteration/tool-turn-bridge.rkt

;; make-injected-collector! moved to iteration/tool-turn-bridge.rkt

;; ============================================================
;; v0.15.2 (BUG-INTENT-WITH-ACTION): Intent-without-action detection
;; ============================================================

;; v0.21.3: Intent detection removed. The prompt is self-correcting by design.
;; detect-intent-pattern, INTENT-ACTION-RX, detect-stall?, MAX-STALL-RETRIES all removed.

;; extract-last-assistant-text moved to iteration/tool-turn-bridge.rkt

;; ============================================================
;; Iteration loop
;; ============================================================
;; v0.18.0: Context-aware exploration steering helpers
;; ============================================================

;; extract-tool-target-path moved to iteration/tool-turn-bridge.rkt

;; Compute the new seen-paths set and whether to increment the counter.
;; Returns (values new-seen-paths should-increment?)
;; take-at-most moved to iteration/tool-turn-bridge.rkt

;; update-seen-paths moved to iteration/tool-turn-bridge.rkt)

;; ============================================================

(define (run-iteration-loop context
                            prov
                            bus
                            reg
                            ext-reg
                            log-path
                            session-id
                            max-iterations
                            #:cancellation-token [token #f]
                            #:config [config (hash)]
                            #:queue [steering-queue #f]
                            #:follow-up-delivery-mode [follow-up-mode 'all]
                            ;; FEAT-61: injected message collector box (created by caller)
                            #:injected-box [injected-box #f]
                            ;; #1158: shutdown check thunks (avoid circular dep on agent-session)
                            #:shutdown-check [shutdown-check #f]
                            #:force-shutdown-check [force-shutdown-check #f]
                            ;; MOD-02 (v0.22.6 W4): DI keyword args for testability
                            #:compact-proc [compact-proc (resolve-compact-proc)]
                            #:estimate-tokens [estimate-tokens (resolve-estimate-tokens)]
                            #:inject-topic [inject-topic (resolve-inject-topic)]
                            ;; v0.26.0: working set memory
                            #:working-set [initial-ws #f]
                            ;; v0.28.22 W0: session for mid-turn compaction
                            #:session [sess #f])
  ;; v0.14.1: Soft limit = max-iterations (warn), Hard limit = max-iterations-hard (stop)
  ;; v0.14.4 Wave 1: Default hard limit = max(max-iterations * 1.6, 80) for slow models
  (define max-iterations-hard
    (hash-ref config 'max-iterations-hard (max (inexact->exact (floor (* max-iterations 1.6))) 80)))
  ;; v0.26.0: Initialize working set
  (define ws (or initial-ws (make-working-set)))
  ;; Dispatch 'before-agent-start hook — extensions can block or modify config
  (define agent-start-payload
    (hasheq 'session-id
            session-id
            'max-iterations
            max-iterations
            'context-message-count
            (length context)))
  (define-values (amended-start start-hook-res)
    (maybe-dispatch-hooks ext-reg 'before-agent-start agent-start-payload))
  (if (and start-hook-res (eq? (hook-result-action start-hook-res) 'block))
      (begin
        (emit-session-event!
         bus
         session-id
         "agent.blocked"
         (assert-payload "agent.blocked"
                         (hasheq 'reason "extension-block" 'hook 'before-agent-start)
                         reason-payload/c))
        (make-loop-result '() 'completed (hasheq 'reason "extension-block")))
      (let loop ([ctx context]
                 [iteration 0]
                 [consecutive-tool-count 0]
                 [seen-paths (set)] ;; v0.18.0: same-file dedup tracking
                 [ws ws] ;; v0.26.0: working set memory
                 [intent-retry-count 0]
                 [consecutive-error-count 0] ;; v0.19.10: spiral breaker
                 [recent-tool-names '()] ;; v0.19.10: bash-only streak detection
                 [explore-count 0] ;; v0.19.11: cumulative explore budget
                 [implement-count 0] ;; v0.19.11: cumulative implement budget
                 [stall-retry-count 0]) ;; v0.20.4 W2: stall detection

        ;; ── Cooperative cancellation check (between iterations) ──
        ;; R2-5 + FEAT-61: Check steering queue + drain injected messages.
        (define ctx-with-steering
          (if steering-queue
              (let ([steering-msgs (dequeue-all-steering! steering-queue)])
                ;; #664: Emit queue-status event for TUI status bar
                (let ([qs (queue-status steering-queue)])
                  (when (or (> (hash-ref qs 'steering 0) 0) (> (hash-ref qs 'followup 0) 0))
                    (emit-session-event! bus session-id "queue.status-update" qs)))
                (if (null? steering-msgs)
                    ctx
                    (append ctx steering-msgs)))
              ctx))

        ;; FEAT-61: Drain any extension-injected messages
        (define ctx-with-injected
          (if injected-box
              (let ([injected (drain-injected-messages! bus injected-box session-id)])
                (if (null? injected)
                    ctx-with-steering
                    (begin
                      (emit-session-event! bus
                                           session-id
                                           "message.injected.drain"
                                           (assert-payload "message.injected.drain"
                                                           (hasheq 'count (length injected))
                                                           injection-count-payload/c))
                      (append ctx-with-steering injected))))
              ctx-with-steering))

        (cond
          ;; #1158: Forced shutdown (double Ctrl+C) — immediate abort
          [(and force-shutdown-check (force-shutdown-check))
           (emit-session-event! bus
                                session-id
                                "turn.cancelled"
                                (assert-payload "turn.cancelled"
                                                (hasheq 'reason "force-shutdown" 'iteration iteration)
                                                turn-cancelled-payload/c))
           (make-loop-result (append ctx-with-injected '())
                             'cancelled
                             (hasheq 'reason "force-shutdown" 'iteration iteration))]

          [(and token (cancellation-token-cancelled? token))
           (emit-session-event!
            bus
            session-id
            "turn.cancelled"
            (assert-payload "turn.cancelled"
                            (hasheq 'reason "cancellation-token" 'iteration iteration)
                            turn-cancelled-payload/c))
           (make-loop-result (append ctx-with-injected '())
                             'cancelled
                             (hasheq 'reason "cancellation-token" 'iteration iteration))]

          ;; #1158: Graceful shutdown — finish after current iteration
          [(and shutdown-check (shutdown-check))
           (emit-session-event!
            bus
            session-id
            "turn.cancelled"
            (assert-payload "turn.cancelled"
                            (hasheq 'reason "graceful-shutdown" 'iteration iteration)
                            turn-cancelled-payload/c))
           (make-loop-result (append ctx-with-injected '())
                             'completed
                             (hasheq 'reason "graceful-shutdown" 'iteration iteration))]

          [else
           (define turn-id (generate-id))

           ;; Dispatch 'turn-start hook — extensions can amend context or block
           (define-values (ctx-to-use turn-blocked?)
             (let-values ([(amended-ctx hook-res)
                           (maybe-dispatch-hooks ext-reg 'turn-start ctx-with-injected)])
               (if (and hook-res (eq? (hook-result-action hook-res) 'block))
                   (values ctx-with-injected #t)
                   (values amended-ctx #f))))

           (cond
             ;; ── Turn blocked by extension ──
             [turn-blocked?
              (emit-session-event!
               bus
               session-id
               "turn.blocked"
               (assert-payload "turn.blocked" (hasheq 'reason "extension-block") reason-payload/c))
              (make-loop-result '() 'completed (hasheq 'reason "extension-block"))]

             [else
              ;; Build assembled context (tiered + hooks) — from turn-orchestrator.rkt
              ;; v0.28.21: Handle both mutable and immutable config hashes
              ;; (build-runtime-from-cli uses hash-set!, making config mutable)
              (define config-with-ws
                (if (immutable? config)
                    (hash-set config 'working-set ws)
                    (begin
                      (hash-set! config 'working-set ws)
                      config)))
              (define ctx-final
                (build-assembled-context ctx-to-use config-with-ws ext-reg bus session-id iteration))

              ;; Run provider turn — from turn-orchestrator.rkt
              (define result
                (run-provider-turn ctx-final prov bus reg ext-reg session-id turn-id token config))

              (define termination (loop-result-termination-reason result))
              (define new-msgs (loop-result-messages result))

              ;; v0.15.0 Wave 1: emit iteration.decision for trace logging
              (emit-session-event! bus
                                   session-id
                                   "iteration.decision"
                                   (assert-payload "iteration.decision"
                                                   (hasheq 'iteration
                                                           (add1 iteration)
                                                           'termination
                                                           termination
                                                           'consecutive_tools
                                                           consecutive-tool-count
                                                           'max_iterations
                                                           max-iterations
                                                           'max_iterations_hard
                                                           max-iterations-hard)
                                                   iteration-decision-payload/c))

              (cond
                ;; ── Completed: append and return ──
                [(eq? termination 'completed)
                 (append-entries! log-path new-msgs)
                 ;; Dispatch 'turn-end hook
                 (let-values ([(amended-result after-hook-res)
                               (maybe-dispatch-hooks ext-reg 'turn-end result)])
                   (let ([effective-result (if (and after-hook-res
                                                    (eq? (hook-result-action after-hook-res) 'amend))
                                               amended-result
                                               result)])
                     ;; #662: Drain follow-up queue — if follow-ups exist,
                     ;; inject as user messages and continue the loop.
                     ;; #763: Support 'all (drain all) and 'one-at-a-time modes.
                     (define followup-continued?
                       (and steering-queue
                            (let ([followups (if (eq? follow-up-mode 'one-at-a-time)
                                                 (let ([one (dequeue-followup! steering-queue)])
                                                   (if one
                                                       (list one)
                                                       '()))
                                                 (dequeue-all-followups! steering-queue))])
                              (if (null? followups)
                                  #f
                                  (let ([followup-msgs (for/list ([fu (in-list followups)])
                                                         (make-message (generate-id)
                                                                       #f
                                                                       'user
                                                                       'text
                                                                       (list (make-text-part fu))
                                                                       (now-seconds)
                                                                       (hasheq 'source "followup")))])
                                    (emit-session-event! bus
                                                         session-id
                                                         "followup.injected"
                                                         (hasheq 'count
                                                                 (length followups)
                                                                 'mode
                                                                 (symbol->string follow-up-mode)))
                                    (append-entries! log-path followup-msgs)
                                    (loop (append ctx-with-injected new-msgs followup-msgs)
                                          (add1 iteration)
                                          0
                                          (set)
                                          ws
                                          intent-retry-count
                                          0
                                          '()
                                          explore-count
                                          implement-count
                                          0))))))
                     (define base-result (if followup-continued? effective-result effective-result))
                     ;; v0.21.3: Intent/stall detection removed.
                     ;; The prompt is self-correcting by design - no runtime nudging.
                     base-result))]
                ;; ── Hard iteration limit reached: append and stop ──
                [(and (eq? termination 'tool-calls-pending) (>= (add1 iteration) max-iterations-hard))
                 (append-entries! log-path new-msgs)
                 (emit-session-event! bus
                                      session-id
                                      "runtime.error"
                                      (assert-payload "runtime.error"
                                                      (hasheq 'error
                                                              "max-iterations-exceeded"
                                                              'iteration
                                                              iteration
                                                              'maxIterations
                                                              max-iterations-hard)
                                                      error-detail-payload/c))
                 (make-loop-result new-msgs
                                   'max-iterations-exceeded
                                   (hash-set (loop-result-metadata result) 'maxIterationsReached #t))]

                ;; ── Soft iteration limit: warn but continue ──
                [(and (eq? termination 'tool-calls-pending)
                      (>= (add1 iteration) max-iterations)
                      (< (add1 iteration) max-iterations-hard))
                 (append-entries! log-path new-msgs)
                 (emit-session-event! bus
                                      session-id
                                      "iteration.soft-warning"
                                      (hasheq 'iteration
                                              (add1 iteration)
                                              'soft-limit
                                              max-iterations
                                              'hard-limit
                                              max-iterations-hard
                                              'remaining
                                              (- max-iterations-hard (add1 iteration))))
                 (define updated-ctx
                   (handle-tool-calls-pending new-msgs
                                              ctx-with-injected
                                              ext-reg
                                              reg
                                              bus
                                              session-id
                                              log-path
                                              token
                                              config))
                 ;; v0.14.1: mid-turn token budget check
                 ;; v0.28.22 W0: wire session for mid-turn compaction
                 (define ctx-after-budget
                   (check-mid-turn-budget! updated-ctx bus session-id config #:session sess))
                 (loop (if (list? ctx-after-budget) ctx-after-budget updated-ctx)
                       (add1 iteration)
                       (add1 consecutive-tool-count)
                       seen-paths
                       ws
                       intent-retry-count
                       consecutive-error-count
                       recent-tool-names
                       explore-count
                       implement-count
                       stall-retry-count)]

                ;; ── Tool calls pending: execute tools and re-run ──
                [(eq? termination 'tool-calls-pending)
                 (append-entries! log-path new-msgs)
                 (define updated-ctx
                   (handle-tool-calls-pending new-msgs
                                              ctx-with-injected
                                              ext-reg
                                              reg
                                              bus
                                              session-id
                                              log-path
                                              token
                                              config))
                 ;; v0.21.3: Exploration steering removed. Compute counters without steering.
                 (define current-tool-calls (extract-tool-calls-from-messages new-msgs))
                 ;; v0.26.0: Update working set after tool execution
                 (define tool-result-msgs
                   (filter (lambda (m) (eq? (message-role m) 'tool)) updated-ctx))
                 ;; Convert tool-call structs to hashes for working-set-update!
                 (define current-tool-calls-hashes
                   (for/list ([tc (in-list current-tool-calls)])
                     (hasheq 'name (tool-call-name tc) 'arguments (tool-call-arguments tc))))
                 ;; Use estimate-message-tokens from context-policy.rkt
                 ;; v0.26.0: Detect read spirals (re-reading a file already in working set)
                 (define read-spiral-paths
                   (for/list ([tc (in-list current-tool-calls)]
                              #:when (equal? (tool-call-name tc) "read"))
                     (define path (extract-tool-target-path tc))
                     (and path (member path (map ws-entry-path (working-set-entries ws))) path)))
                 (define valid-spiral-paths (filter string? read-spiral-paths))
                 (when (> (length valid-spiral-paths) 0)
                   (emit-session-event!
                    bus
                    session-id
                    "working-set.read-spiral-detected"
                    (hasheq 'paths valid-spiral-paths 'count (length valid-spiral-paths))))
                 (working-set-update! ws
                                      current-tool-calls-hashes
                                      tool-result-msgs
                                      message-id
                                      estimate-message-tokens)
                 ;; v0.26.0: Emit working-set.update after each change
                 (emit-session-event! bus
                                      session-id
                                      "working-set.update"
                                      (hasheq 'entry-count
                                              (working-set-entry-count ws)
                                              'token-count
                                              (working-set-token-count ws)
                                              'paths
                                              (map ws-entry-path (working-set-entries ws))))
                 (define-values (new-seen-paths should-increment?)
                   (update-seen-paths current-tool-calls seen-paths))
                 (define effective-tool-count
                   (if should-increment?
                       (add1 consecutive-tool-count)
                       consecutive-tool-count))
                 (define new-explore-count
                   (+ explore-count
                      (for/sum ([tc (extract-tool-calls-from-messages new-msgs)])
                               (if (member (tool-call-name tc) '("read" "grep" "find" "ls")) 1 0))))
                 (define new-implement-count
                   (+ implement-count
                      (for/sum ([tc (extract-tool-calls-from-messages new-msgs)])
                               (if (member (tool-call-name tc) '("edit" "write")) 1 0))))
                 (define new-error-count
                   (+ consecutive-error-count
                      (for/sum
                       ([tr (filter tool-result-part? (apply append (map message-content new-msgs)))])
                       (if (tool-result-part-is-error? tr) 1 0))))
                 (define new-recent-tools
                   (take-at-most (append recent-tool-names
                                         (map tool-call-name
                                              (extract-tool-calls-from-messages new-msgs)))
                                 20))
                 (loop updated-ctx
                       (add1 iteration)
                       effective-tool-count
                       new-seen-paths
                       ws
                       intent-retry-count
                       new-error-count
                       new-recent-tools
                       new-explore-count
                       new-implement-count
                       stall-retry-count)]

                ;; ── Unknown termination: append and return ──
                [else
                 (append-entries! log-path new-msgs)
                 result])])]))))
