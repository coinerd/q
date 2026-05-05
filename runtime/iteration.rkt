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
         racket/match
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
         decide-next-action
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

;; update-seen-paths moved to iteration/tool-turn-bridge.rkt)

;; ============================================================
;; v0.29.13 (W1): Extracted sub-functions for run-iteration-loop
;; ============================================================

;; check-cancellation: Evaluate cancellation/shutdown conditions.
;; Returns a loop-result if iteration should abort, or #f to continue.
(define (check-cancellation token force-shutdown-check shutdown-check bus session-id iteration ctx)
  (cond
    ;; Forced shutdown (double Ctrl+C) — immediate abort
    [(and force-shutdown-check (force-shutdown-check))
     (emit-session-event! bus
                          session-id
                          "turn.cancelled"
                          (assert-payload "turn.cancelled"
                                          (hasheq 'reason "force-shutdown" 'iteration iteration)
                                          turn-cancelled-payload/c))
     (make-loop-result ctx 'cancelled (hasheq 'reason "force-shutdown" 'iteration iteration))]
    [(and token (cancellation-token-cancelled? token))
     (emit-session-event! bus
                          session-id
                          "turn.cancelled"
                          (assert-payload "turn.cancelled"
                                          (hasheq 'reason "cancellation-token" 'iteration iteration)
                                          turn-cancelled-payload/c))
     (make-loop-result ctx 'cancelled (hasheq 'reason "cancellation-token" 'iteration iteration))]
    ;; Graceful shutdown — finish after current iteration
    [(and shutdown-check (shutdown-check))
     (emit-session-event! bus
                          session-id
                          "turn.cancelled"
                          (assert-payload "turn.cancelled"
                                          (hasheq 'reason "graceful-shutdown" 'iteration iteration)
                                          turn-cancelled-payload/c))
     (make-loop-result ctx 'completed (hasheq 'reason "graceful-shutdown" 'iteration iteration))]
    [else #f]))

;; process-tool-results: Execute pending tool calls, update working set,
;; and compute new iteration counters. Shared by 'continue and 'stop-soft-limit.
;; Returns updated-ctx (with tool results appended).
(define (process-tool-results new-msgs ctx ext-reg reg bus session-id log-path token config ws)
  (define updated-ctx
    (handle-tool-calls-pending new-msgs ctx ext-reg reg bus session-id log-path token config))
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
    (emit-session-event! bus
                         session-id
                         "working-set.read-spiral-detected"
                         (hasheq 'paths valid-spiral-paths 'count (length valid-spiral-paths))))
  (working-set-update! ws
                       current-tool-calls-hashes
                       tool-result-msgs
                       message-id
                       estimate-message-tokens)
  (emit-session-event! bus
                       session-id
                       "working-set.update"
                       (hasheq 'entry-count
                               (working-set-entry-count ws)
                               'token-count
                               (working-set-token-count ws)
                               'paths
                               (map ws-entry-path (working-set-entries ws))))
  updated-ctx)

;; compute-next-counters: Compute updated iteration counters after tool execution.
;; Returns (values new-seen-paths effective-tool-count new-explore-count
;;                  new-implement-count new-error-count new-recent-tools)
(define (compute-next-counters new-msgs
                               seen-paths
                               consecutive-tool-count
                               explore-count
                               implement-count
                               consecutive-error-count
                               recent-tool-names)
  (define current-tool-calls (extract-tool-calls-from-messages new-msgs))
  (define-values (new-seen-paths should-increment?) (update-seen-paths current-tool-calls seen-paths))
  (define effective-tool-count
    (if should-increment?
        (add1 consecutive-tool-count)
        consecutive-tool-count))
  (define new-explore-count
    (+ explore-count
       (for/sum ([tc (in-list current-tool-calls)])
                (if (member (tool-call-name tc) '("read" "grep" "find" "ls")) 1 0))))
  (define new-implement-count
    (+ implement-count
       (for/sum ([tc (in-list current-tool-calls)])
                (if (member (tool-call-name tc) '("edit" "write")) 1 0))))
  (define new-error-count
    (+ consecutive-error-count
       (for/sum ([tr (filter tool-result-part? (apply append (map message-content new-msgs)))])
                (if (tool-result-part-is-error? tr) 1 0))))
  (define new-recent-tools
    (take-at-most (append recent-tool-names (map tool-call-name current-tool-calls)) 20))
  (values new-seen-paths
          effective-tool-count
          new-explore-count
          new-implement-count
          new-error-count
          new-recent-tools))

;; handle-stop-action: Handle the 'stop action from decide-next-action.
;; Returns either a loop-result (if no follow-ups) or recurses via the
;; followup-continuation callback.
(define (handle-stop-action result
                            new-msgs
                            log-path
                            ext-reg
                            bus
                            session-id
                            steering-queue
                            follow-up-mode
                            iteration
                            ctx-with-injected
                            ws
                            intent-retry-count
                            explore-count
                            implement-count
                            followup-continuation)
  (define termination (loop-result-termination-reason result))
  (if (eq? termination 'completed)
      (begin
        (append-entries! log-path new-msgs)
        ;; Dispatch 'turn-end hook
        (let-values ([(amended-result after-hook-res)
                      (maybe-dispatch-hooks ext-reg 'turn-end result)])
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
                            bus
                            session-id
                            "followup.injected"
                            (hasheq 'count (length followups) 'mode (symbol->string follow-up-mode)))
                           (append-entries! log-path followup-msgs)
                           (followup-continuation (append ctx-with-injected new-msgs followup-msgs)
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
            (if followup-continued? effective-result effective-result))))
      ;; Other stop reasons: append and return
      (begin
        (append-entries! log-path new-msgs)
        result)))

;; ============================================================
;; dispatch-loop-action: extracted match block from run-iteration-loop
;; ============================================================

(define (dispatch-loop-action action
                              result
                              new-msgs
                              ctx-with-injected
                              log-path
                              ext-reg
                              reg
                              bus
                              session-id
                              steering-queue
                              follow-up-mode
                              iteration
                              ws
                              intent-retry-count
                              explore-count
                              implement-count
                              consecutive-tool-count
                              seen-paths
                              consecutive-error-count
                              recent-tool-names
                              max-iterations
                              max-iterations-hard
                              sess
                              token
                              config
                              on-recurse)
  (define (call-process-tool-results)
    (process-tool-results new-msgs
                          ctx-with-injected
                          ext-reg
                          reg
                          bus
                          session-id
                          log-path
                          token
                          config
                          ws))
  (match action
    ['stop
     (handle-stop-action result
                         new-msgs
                         log-path
                         ext-reg
                         bus
                         session-id
                         steering-queue
                         follow-up-mode
                         iteration
                         ctx-with-injected
                         ws
                         intent-retry-count
                         explore-count
                         implement-count
                         on-recurse)]
    ['stop-hard-limit
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
    ['stop-soft-limit
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
     (define updated-ctx (call-process-tool-results))
     (define ctx-after-budget
       (if sess
           (maybe-compact-mid-turn sess updated-ctx bus session-id config)
           (begin
             (estimate-mid-turn-tokens updated-ctx bus session-id config)
             updated-ctx)))
     (on-recurse ctx-after-budget
                 (add1 iteration)
                 (add1 consecutive-tool-count)
                 seen-paths
                 ws
                 intent-retry-count
                 consecutive-error-count
                 recent-tool-names
                 explore-count
                 implement-count
                 0)]
    ['continue
     (append-entries! log-path new-msgs)
     (define updated-ctx (call-process-tool-results))
     (define-values (new-seen-paths
                     effective-tool-count
                     new-explore-count
                     new-implement-count
                     new-error-count
                     new-recent-tools)
       (compute-next-counters new-msgs
                              seen-paths
                              consecutive-tool-count
                              explore-count
                              implement-count
                              consecutive-error-count
                              recent-tool-names))
     ;; v0.28.22 W1: exploration loop detection
     (define loop-warning (detect-exploration-loop new-recent-tools))
     (when loop-warning
       (emit-session-event!
        bus
        session-id
        "iteration.exploration-loop"
        (hasheq 'pattern loop-warning 'recent-tools new-recent-tools 'iteration iteration)))
     (on-recurse updated-ctx
                 (add1 iteration)
                 effective-tool-count
                 new-seen-paths
                 ws
                 intent-retry-count
                 new-error-count
                 new-recent-tools
                 new-explore-count
                 new-implement-count
                 0)]))

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
                 [seen-paths (set)]
                 [ws ws]
                 [intent-retry-count 0]
                 [consecutive-error-count 0]
                 [recent-tool-names '()]
                 [explore-count 0]
                 [implement-count 0]
                 [stall-retry-count 0])

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
                              iteration
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
                (if (immutable? config)
                    (hash-set config 'working-set ws)
                    (begin
                      (hash-set! config 'working-set ws)
                      config)))
              (define ctx-final
                (build-assembled-context ctx-to-use config-with-ws ext-reg bus session-id iteration))
              ;; Run provider turn
              (define result
                (run-provider-turn ctx-final prov bus reg ext-reg session-id turn-id token config))
              (define termination (loop-result-termination-reason result))
              (define new-msgs (loop-result-messages result))
              ;; Emit iteration decision
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
              (define action
                (decide-next-action (iteration-ctx iteration
                                                   consecutive-tool-count
                                                   explore-count
                                                   max-iterations
                                                   max-iterations-hard)
                                    result))
              (dispatch-loop-action
               action
               result
               new-msgs
               ctx-with-injected
               log-path
               ext-reg
               reg
               bus
               session-id
               steering-queue
               follow-up-mode
               iteration
               ws
               intent-retry-count
               explore-count
               implement-count
               consecutive-tool-count
               seen-paths
               consecutive-error-count
               recent-tool-names
               max-iterations
               max-iterations-hard
               sess
               token
               config
               ;; on-recurse: callback for recursive loop cases
               (lambda (new-ctx iter tc sp ws2 irc ec rt ecnt implcnt src)
                 (loop new-ctx iter tc sp ws2 irc ec rt ecnt implcnt src)))])]))))
