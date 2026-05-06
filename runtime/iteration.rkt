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

(require (only-in racket/dict dict-ref dict-set)
         racket/contract
         racket/match
         racket/list
         racket/set
         racket/path
         json
         (only-in racket/string string-trim string-join string-contains?)
         (only-in "../util/json-helpers.rkt" ensure-hash-args)
         "iteration/loop-state.rkt"
         (only-in "../util/loop-result.rkt" loop-result?)
         "iteration/retry-policy.rkt"
         (only-in "iteration/tool-turn-bridge.rkt"
                  dequeue-all-steering!
                  drain-injected-messages!
                  make-injected-collector!
                  extract-tool-target-path
                  update-seen-paths
                  take-at-most)
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
                  queue-status
                  queue?)
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
         (only-in "../agent/event-emitter.rkt" emit-typed-event!)
         (only-in "../llm/provider.rkt" provider?)
         (only-in "../tools/registry.rkt" tool-registry?)
         (only-in "../extensions/api.rkt" extension-registry?)
         (only-in "../runtime/session-types.rkt" agent-session?)
         (only-in "../runtime/session-config.rkt" session-config?)
         (only-in "../agent/event-structs/hook-events.rkt" turn-cancelled-event)
         ;; v0.22.4 (MOD-01): provider turn + context assembly + extension registration
         (only-in "turn-orchestrator.rkt"
                  run-provider-turn
                  build-assembled-context
                  register-session-extensions!)
         "working-set.rkt"
         (only-in "context-policy.rkt" estimate-message-tokens))

(provide (contract-out [run-iteration-loop
                        (->* (list?
                                    (or/c provider? #f)
                                    event-bus?
                                    (or/c tool-registry? #f)
                                    (or/c extension-registry? #f)
                                    (or/c path-string? path?)
                                    string?
                                    exact-nonnegative-integer?)
                             (#:cancellation-token (or/c cancellation-token? #f)
                              #:config (or/c hash? session-config?)
                              #:queue (or/c queue? #f)
                              #:follow-up-delivery-mode (or/c 'all 'one-at-a-time)
                              #:injected-box (or/c box? #f)
                              #:shutdown-check (or/c procedure? #f)
                              #:force-shutdown-check (or/c procedure? #f)
                              #:compact-proc (or/c procedure? #f)
                              #:estimate-tokens (or/c procedure? #f)
                              #:inject-topic (or/c string? #f)
                              #:working-set (or/c working-set? #f)
                              #:session (or/c agent-session? #f))
                             list?)]
                       [decide-next-action (-> iteration-ctx? loop-result? symbol?)])
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
         estimate-mid-turn-tokens
         maybe-compact-mid-turn
         ;; v0.20.5 W3: pre-registration on session open (now from turn-orchestrator)
         register-session-extensions!
         ;; DI resolve functions (v0.29.5: simplified, no parameters)
         resolve-compact-proc
         resolve-estimate-tokens
         resolve-inject-topic
         ;; v0.29.1: Pure decision function exports
         iteration-ctx
         known-termination-reasons)

;; ============================================================
;; Pure decision function (v0.29.1: §10 Match Dispatch, §2 Pure/Effect)
;;
;; iteration-ctx captures the pure subset of loop state needed
;; to decide what to do next. No I/O, no mutation.
;; ============================================================

(struct iteration-ctx
        (iteration consecutive-tool-count explore-count max-iterations max-iterations-hard)
  #:transparent)

(define (known-termination-reasons)
  '(completed cancelled
              tool-calls-pending
              error
              force-shutdown
              shutdown
              max-iterations-exceeded
              hook-blocked))

(define (decide-next-action ctx result)
  (define term (loop-result-termination-reason result))
  (match term
    [(or 'completed 'cancelled 'force-shutdown 'shutdown) 'stop]
    ['hook-blocked 'stop]
    ['max-iterations-exceeded 'stop]
    ['error 'stop]
    ['tool-calls-pending
     (define next-iter (add1 (iteration-ctx-iteration ctx)))
     (cond
       [(>= next-iter (iteration-ctx-max-iterations-hard ctx)) 'stop-hard-limit]
       [(>= next-iter (iteration-ctx-max-iterations ctx)) 'stop-soft-limit]
       [else 'continue])]
    [_ 'stop]))

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

;; update-seen-paths moved to iteration/tool-turn-bridge.rkt

;; ============================================================
;; v0.29.13 (W1): Extracted sub-functions for run-iteration-loop
;; v0.29.16 (W1): Refactored to use loop-infra + loop-counters structs
;; ============================================================

;; check-cancellation: Evaluate cancellation/shutdown conditions.
;; Returns a loop-result if iteration should abort, or #f to continue.
(define (check-cancellation token force-shutdown-check shutdown-check bus session-id iteration ctx)
  (cond
    ;; Forced shutdown (double Ctrl+C) — immediate abort
    [(and force-shutdown-check (force-shutdown-check))
     (emit-typed-event! bus (turn-cancelled-event "turn.cancelled" (current-inexact-milliseconds) session-id #f "force-shutdown" iteration))
     (make-loop-result ctx 'cancelled (hasheq 'reason "force-shutdown" 'iteration iteration))]
    [(and token (cancellation-token-cancelled? token))
     (emit-typed-event! bus (turn-cancelled-event "turn.cancelled" (current-inexact-milliseconds) session-id #f "cancellation-token" iteration))
     (make-loop-result ctx 'cancelled (hasheq 'reason "cancellation-token" 'iteration iteration))]
    ;; Graceful shutdown — finish after current iteration
    [(and shutdown-check (shutdown-check))
     (emit-typed-event! bus (turn-cancelled-event "turn.cancelled" (current-inexact-milliseconds) session-id #f "graceful-shutdown" iteration))
     (make-loop-result ctx 'completed (hasheq 'reason "graceful-shutdown" 'iteration iteration))]
    [else #f]))

;; process-tool-results: Execute pending tool calls, update working set,
;; and compute new iteration counters. Shared by 'continue and 'stop-soft-limit.
;; Returns updated-ctx (with tool results appended).
(define (process-tool-results new-msgs infra config ws)
  (define updated-ctx
    (handle-tool-calls-pending new-msgs
                               (loop-infra-ctx infra)
                               (loop-infra-ext-reg infra)
                               (loop-infra-reg infra)
                               (loop-infra-bus infra)
                               (loop-infra-session-id infra)
                               (loop-infra-log-path infra)
                               (loop-infra-token infra)
                               config))
  (define current-tool-calls (extract-tool-calls-from-messages new-msgs))
  ;; Working set update
  (define tool-result-msgs (filter (lambda (m) (eq? (message-role m) 'tool)) updated-ctx))
  (define current-tool-calls-hashes
    (for/list ([tc (in-list current-tool-calls)])
      (hasheq 'name (tool-call-name tc) 'arguments (tool-call-arguments tc))))
  ;; Detect read spirals
  (define read-spiral-paths
    (for/list ([tc (in-list current-tool-calls)]
               #:when (equal? (tool-call-name tc) "read"))
      (define path (extract-tool-target-path tc))
      (and path (member path (map ws-entry-path (working-set-entries ws))) path)))
  (define valid-spiral-paths (filter string? read-spiral-paths))
  (when (> (length valid-spiral-paths) 0)
    (emit-session-event! (loop-infra-bus infra)
                         (loop-infra-session-id infra)
                         "working-set.read-spiral-detected"
                         (hasheq 'paths valid-spiral-paths 'count (length valid-spiral-paths))))
  (working-set-update! ws
                       current-tool-calls-hashes
                       tool-result-msgs
                       message-id
                       estimate-message-tokens)
  (emit-session-event! (loop-infra-bus infra)
                       (loop-infra-session-id infra)
                       "working-set.update"
                       (hasheq 'entry-count
                               (working-set-entry-count ws)
                               'token-count
                               (working-set-token-count ws)
                               'paths
                               (map ws-entry-path (working-set-entries ws))))
  updated-ctx)

;; compute-next-counters: Compute updated iteration counters after tool execution.
;; Returns a new loop-counters struct.
(define (compute-next-counters counters new-msgs)
  (define current-tool-calls (extract-tool-calls-from-messages new-msgs))
  (define-values (new-seen-paths should-increment?)
    (update-seen-paths current-tool-calls (loop-counters-seen-paths counters)))
  (define effective-tool-count
    (if should-increment?
        (add1 (loop-counters-consecutive-tool-count counters))
        (loop-counters-consecutive-tool-count counters)))
  (define new-explore-count
    (+ (loop-counters-explore-count counters)
       (for/sum ([tc (in-list current-tool-calls)])
                (if (member (tool-call-name tc) '("read" "grep" "find" "ls")) 1 0))))
  (define new-implement-count
    (+ (loop-counters-implement-count counters)
       (for/sum ([tc (in-list current-tool-calls)])
                (if (member (tool-call-name tc) '("edit" "write")) 1 0))))
  (define new-error-count
    (+ (loop-counters-consecutive-error-count counters)
       (for/sum ([tr (filter tool-result-part? (apply append (map message-content new-msgs)))])
                (if (tool-result-part-is-error? tr) 1 0))))
  (define new-recent-tools
    (take-at-most (append (loop-counters-recent-tool-names counters)
                          (map tool-call-name current-tool-calls))
                  20))
  (struct-copy loop-counters
               counters
               [seen-paths new-seen-paths]
               [consecutive-tool-count effective-tool-count]
               [explore-count new-explore-count]
               [implement-count new-implement-count]
               [consecutive-error-count new-error-count]
               [recent-tool-names new-recent-tools]))

;; handle-stop-action: Handle the 'stop action from decide-next-action.
;; Returns either a loop-result (if no follow-ups) or recurses via the
;; followup-continuation callback.
(define (handle-stop-action result
                            new-msgs
                            infra
                            counters
                            ws
                            config
                            steering-queue
                            follow-up-mode
                            followup-continuation)
  (define termination (loop-result-termination-reason result))
  (if (eq? termination 'completed)
      (begin
        (append-entries! (loop-infra-log-path infra) new-msgs)
        ;; Dispatch 'turn-end hook
        (let-values ([(amended-result after-hook-res)
                      (maybe-dispatch-hooks (loop-infra-ext-reg infra) 'turn-end result)])
          (let ([effective-result (if (and after-hook-res
                                           (eq? (hook-result-action after-hook-res) 'amend))
                                      amended-result
                                      result)])
            ;; Drain follow-up queue
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
                           (emit-session-event!
                            (loop-infra-bus infra)
                            (loop-infra-session-id infra)
                            "followup.injected"
                            (hasheq 'count (length followups) 'mode (symbol->string follow-up-mode)))
                           (append-entries! (loop-infra-log-path infra) followup-msgs)
                           (followup-continuation
                            (append (loop-infra-ctx infra) new-msgs followup-msgs)
                            (struct-copy loop-counters
                                         counters
                                         [iteration (add1 (loop-counters-iteration counters))]
                                         [consecutive-tool-count 0]
                                         [seen-paths (set)]
                                         [consecutive-error-count 0]
                                         [recent-tool-names '()]
                                         [stall-retry-count 0])
                            ws))))))
            (if followup-continued? effective-result effective-result))))
      ;; Other stop reasons: append and return
      (begin
        (append-entries! (loop-infra-log-path infra) new-msgs)
        result)))

;; ============================================================
;; dispatch-loop-action: extracted match block from run-iteration-loop
;; v0.29.16: Refactored from 26→13 params using loop-infra + loop-counters structs
;; ============================================================

(define (dispatch-loop-action action
                              result
                              new-msgs
                              infra
                              counters
                              ws
                              config
                              sess
                              max-iterations
                              max-iterations-hard
                              steering-queue
                              follow-up-mode
                              on-recurse)
  (define (call-process-tool-results)
    (process-tool-results new-msgs infra config ws))
  (match action
    ['stop
     (handle-stop-action result
                         new-msgs
                         infra
                         counters
                         ws
                         config
                         steering-queue
                         follow-up-mode
                         on-recurse)]
    ['stop-hard-limit
     (append-entries! (loop-infra-log-path infra) new-msgs)
     (emit-session-event! (loop-infra-bus infra)
                          (loop-infra-session-id infra)
                          "runtime.error"
                          (assert-payload "runtime.error"
                                          (hasheq 'error
                                                  "max-iterations-exceeded"
                                                  'iteration
                                                  (loop-counters-iteration counters)
                                                  'maxIterations
                                                  max-iterations-hard)
                                          error-detail-payload/c))
     (make-loop-result new-msgs
                       'max-iterations-exceeded
                       (hash-set (loop-result-metadata result) 'maxIterationsReached #t))]
    ['stop-soft-limit
     (append-entries! (loop-infra-log-path infra) new-msgs)
     (emit-session-event! (loop-infra-bus infra)
                          (loop-infra-session-id infra)
                          "iteration.soft-warning"
                          (hasheq 'iteration
                                  (add1 (loop-counters-iteration counters))
                                  'soft-limit
                                  max-iterations
                                  'hard-limit
                                  max-iterations-hard
                                  'remaining
                                  (- max-iterations-hard (add1 (loop-counters-iteration counters)))))
     (define updated-ctx (call-process-tool-results))
     (define ctx-after-budget
       (if sess
           (maybe-compact-mid-turn sess
                                   updated-ctx
                                   (loop-infra-bus infra)
                                   (loop-infra-session-id infra)
                                   config)
           (begin
             (estimate-mid-turn-tokens updated-ctx
                                       (loop-infra-bus infra)
                                       (loop-infra-session-id infra)
                                       config)
             updated-ctx)))
     (on-recurse ctx-after-budget
                 (struct-copy loop-counters
                              counters
                              [iteration (add1 (loop-counters-iteration counters))]
                              [consecutive-tool-count
                               (add1 (loop-counters-consecutive-tool-count counters))]
                              [stall-retry-count 0])
                 ws)]
    ['continue
     (append-entries! (loop-infra-log-path infra) new-msgs)
     (define updated-ctx (call-process-tool-results))
     (define new-counters (compute-next-counters counters new-msgs))
     ;; v0.28.22 W1: exploration loop detection
     (define loop-warning (detect-exploration-loop (loop-counters-recent-tool-names new-counters)))
     (when loop-warning
       (emit-session-event! (loop-infra-bus infra)
                            (loop-infra-session-id infra)
                            "iteration.exploration-loop"
                            (hasheq 'pattern
                                    loop-warning
                                    'recent-tools
                                    (loop-counters-recent-tool-names new-counters)
                                    'iteration
                                    (loop-counters-iteration counters))))
     (on-recurse updated-ctx
                 (struct-copy loop-counters
                              new-counters
                              [iteration (add1 (loop-counters-iteration counters))]
                              [stall-retry-count 0])
                 ws)]))

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
    (dict-ref config 'max-iterations-hard (max (inexact->exact (floor (* max-iterations 1.6))) 80)))
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
      (let ([infra (loop-infra context ext-reg reg bus session-id log-path token)])
        (let loop ([ctx context]
                   [counters (make-initial-counters)]
                   [ws ws])

          ;; ── Drain steering + injected messages ──
          (define ctx-with-steering
            (if steering-queue
                (let ([steering-msgs (dequeue-all-steering! steering-queue)])
                  (let ([qs (queue-status steering-queue)])
                    (when (or (> (hash-ref qs 'steering 0) 0) (> (hash-ref qs 'followup 0) 0))
                      (emit-session-event! bus session-id "queue.status-update" qs)))
                  (if (null? steering-msgs)
                      ctx
                      (append ctx steering-msgs)))
                ctx))
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

          ;; ── Cancellation/shutdown check ──
          (define cancel-result
            (check-cancellation token
                                force-shutdown-check
                                shutdown-check
                                bus
                                session-id
                                (loop-counters-iteration counters)
                                ctx-with-injected))
          (cond
            [cancel-result cancel-result]
            [else
             (define turn-id (generate-id))
             ;; Dispatch 'turn-start hook
             (define-values (ctx-to-use turn-blocked?)
               (let-values ([(amended-ctx hook-res)
                             (maybe-dispatch-hooks ext-reg 'turn-start ctx-with-injected)])
                 (if (and hook-res (eq? (hook-result-action hook-res) 'block))
                     (values ctx-with-injected #t)
                     (values amended-ctx #f))))
             (cond
               [turn-blocked?
                (emit-session-event!
                 bus
                 session-id
                 "turn.blocked"
                 (assert-payload "turn.blocked" (hasheq 'reason "extension-block") reason-payload/c))
                (make-loop-result '() 'completed (hasheq 'reason "extension-block"))]
               [else
                ;; Build assembled context
                (define config-with-ws
                  (dict-set config 'working-set ws))
                (define ctx-final
                  (build-assembled-context ctx-to-use
                                           config-with-ws
                                           ext-reg
                                           bus
                                           session-id
                                           (loop-counters-iteration counters)))
                ;; Run provider turn
                (define result
                  (run-provider-turn ctx-final prov bus reg ext-reg session-id turn-id token config))
                (define termination (loop-result-termination-reason result))
                (define new-msgs (loop-result-messages result))
                ;; Emit iteration decision
                (emit-session-event!
                 bus
                 session-id
                 "iteration.decision"
                 (assert-payload "iteration.decision"
                                 (hasheq 'iteration
                                         (add1 (loop-counters-iteration counters))
                                         'termination
                                         termination
                                         'consecutive_tools
                                         (loop-counters-consecutive-tool-count counters)
                                         'max_iterations
                                         max-iterations
                                         'max_iterations_hard
                                         max-iterations-hard)
                                 iteration-decision-payload/c))
                (define action
                  (decide-next-action (iteration-ctx (loop-counters-iteration counters)
                                                     (loop-counters-consecutive-tool-count counters)
                                                     (loop-counters-explore-count counters)
                                                     max-iterations
                                                     max-iterations-hard)
                                      result))
                (dispatch-loop-action action
                                      result
                                      new-msgs
                                      (struct-copy loop-infra infra [ctx ctx-with-injected])
                                      counters
                                      ws
                                      config
                                      sess
                                      max-iterations
                                      max-iterations-hard
                                      steering-queue
                                      follow-up-mode
                                      ;; on-recurse: callback for recursive loop cases
                                      (lambda (new-ctx new-counters ws2)
                                        (loop new-ctx new-counters ws2)))])])))))

;; ============================================================
;; for-testing submodule: expose internals for unit testing
;; v0.29.17 W0: exposed to support integration tests for struct refactor
;; ============================================================
(module+ for-testing
  (provide dispatch-loop-action
           handle-stop-action
           compute-next-counters
           process-tool-results ;; NOTE: requires tool-coordinator mock for isolated testing
           decide-next-action
           check-cancellation
           iteration-ctx))
