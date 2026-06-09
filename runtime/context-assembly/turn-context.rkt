#lang racket/base

;; runtime/context-assembly/turn-context.rkt — Turn-level context assembly helpers
;; STABILITY: internal
;;
;; Extracted from turn-orchestrator.rkt (v0.96.0 W2) for single-responsibility.
;; Contains: symbol->task-state, assemble-context/pure,
;;           prepare-turn-context-state, emit-context-assembly-events!,
;;           current-last-task-fsm-state parameter.

(require racket/format
         racket/list
         racket/promise
         racket/set
         (only-in racket/string string-join)
         ;; Message/content types
         (only-in "../../util/message/message.rkt" message-id message-kind message-content)
         (only-in "../../util/content/content-parts.rkt"
                  text-part?
                  text-part-text
                  tool-result-part?
                  tool-result-part-content
                  tool-result-part-is-error?)
         ;; Event emission
         "../../agent/event-emitter.rkt"
         "../../agent/event-structs/iteration-events.rkt"
         "../../agent/event-structs/session-events.rkt"
         ;; Context assembly core
         (only-in "../context/context-assembly.rkt"
                  build-tiered-context-with-hooks
                  tiered-context->message-list
                  tiered-context?
                  tiered-context-tier-a
                  tiered-context-tier-b
                  tiered-context-tier-c)
         (only-in "../context-assembly/serialization.rkt"
                  gsd-progress-message?
                  build-tiered-context/state-aware
                  current-task-state-aware-assembly?)
         ;; Working set
         (only-in "../working-set.rkt"
                  working-set-resolve-messages
                  working-set-entry-count
                  working-set-token-count)
         ;; Session config accessors
         (only-in "../session/session-config.rkt"
                  config-tier-b-count
                  config-tier-c-count
                  config-max-tokens
                  config-working-set
                  config-task-state-aware?
                  config-context-assembly-profile
                  apply-context-assembly-profile!)
         ;; Session state accessors (read-only)
         (only-in "../session/session-types.rkt"
                  agent-session-task-fsm-state
                  agent-session-task-conclusions
                  agent-session-recent-tool-calls)
         ;; Session mutation (for auto-distill persistence)
         (only-in "../session/session-mutation.rkt"
                  guarded-set-working-set-evolved!
                  guarded-set-task-conclusions!)
         ;; Hook types
         (only-in "../../util/hook-types.rkt" hook-result? hook-result-action)
         ;; Token estimation
         (only-in "../../llm/token-budget.rkt" estimate-context-tokens)
         ;; WS evolution
         (only-in "../context-assembly/ws-evolution.rkt"
                  evolve-working-set-for-state/result
                  evolution-result?)
         (only-in "../context-assembly/state-aware-builder.rkt" current-ws-evolution-enabled?)
         (only-in "../context-assembly/rollback-actions.rkt" current-loop-warning-count)
         ;; Auto-distillation
         (only-in "../context-assembly/auto-distillation.rkt"
                  auto-distill
                  current-auto-distillation-enabled?)
         ;; Task-state singletons
         (only-in "../context-assembly/task-state.rkt"
                  task-idle
                  task-exploration
                  task-planning
                  task-implementation
                  task-verification
                  task-debugging))

(provide current-last-task-fsm-state
         symbol->task-state
         assemble-context/pure
         prepare-turn-context-state
         emit-context-assembly-events!)

;; ============================================================
;; Task state conversion
;; ============================================================

;; v0.79.2 GAP-2: Track last task FSM state for WS evolution old-state.
;; Set each turn from agent-session-task-fsm-state before it gets updated.
(define current-last-task-fsm-state (make-parameter #f))

;; Convert a raw state symbol to the canonical fsm-state? struct.
;; The runtime stores task-fsm-state as raw symbols, but downstream consumers
;; (ws-evolution, state-aware-builder) expect fsm-state? structs.
(define (symbol->task-state sym)
  (case sym
    [(idle) task-idle]
    [(exploration) task-exploration]
    [(planning) task-planning]
    [(implementation) task-implementation]
    [(verification) task-verification]
    [(debugging) task-debugging]
    [else #f]))

;; ============================================================
;; Pure context assembly
;; ============================================================

;; Pure context assembly: no side effects, no session mutation.
;; Returns (values assembled-messages hook-result tiered-context).
(define (assemble-context/pure ctx-to-use
                               config-raw
                               #:hook-dispatcher [hook-dispatcher #f]
                               #:task-state [task-state #f]
                               #:conclusions [conclusions '()]
                               #:state-aware? [state-aware? #f]
                               #:recent-tool-calls [recent-tool-calls '()])
  (define config config-raw)
  (define tier-b-count (config-tier-b-count config))
  (define tier-c-count (config-tier-c-count config))
  (define max-tokens (config-max-tokens config))
  (define ws (config-working-set config))
  (define ws-messages-promise
    (delay
      (if ws
          (working-set-resolve-messages ws ctx-to-use message-id)
          '())))
  (define ws-messages (force ws-messages-promise))
  (define-values (tc hook-result)
    (cond
      ;; v0.76.3: State-aware assembly when enabled (global flag or per-session rollout)
      [(and (or state-aware? (current-task-state-aware-assembly?)) task-state)
       (define sa-tc
         (build-tiered-context/state-aware ctx-to-use
                                           #:tier-b-count tier-b-count
                                           #:tier-c-count tier-c-count
                                           #:working-set-messages ws-messages
                                           #:task-state task-state
                                           #:conclusions conclusions
                                           #:recent-tool-calls recent-tool-calls
                                           #:session-config config))
       (values sa-tc #f)]
      ;; Standard assembly path
      [else
       (build-tiered-context-with-hooks ctx-to-use
                                        #:hook-dispatcher hook-dispatcher
                                        #:tier-b-count tier-b-count
                                        #:tier-c-count tier-c-count
                                        #:max-tokens max-tokens
                                        #:working-set-messages ws-messages)]))
  (values (tiered-context->message-list tc) hook-result tc))

;; ============================================================
;; Turn context state preparation
;; ============================================================

;; Prepare task state, conclusions (with auto-distill), and WS evolution
;; for context assembly. Mutates session state when auto-distill adds conclusions
;; or WS evolution produces a new working set.
(define (prepare-turn-context-state ctx-to-use config-raw session)
  (define ws-early (config-working-set config-raw))
  (define task-state-raw (and session (agent-session-task-fsm-state session)))
  (define task-state (or (symbol->task-state task-state-raw) task-state-raw))
  (define conclusions (and session (agent-session-task-conclusions session)))
  ;; v0.77.9 T2.1: Auto-distill uncovered WS entries when enabled
  (define augmented-conclusions
    (if (and (current-auto-distillation-enabled?) session conclusions task-state ws-early)
        (let ([ws-msgs (working-set-resolve-messages ws-early ctx-to-use message-id)])
          ;; v0.79.2 GAP-3: Build content summaries for richer auto-distill text
          ;; GAP-C: Include tool-result-parts in content summaries
          (define (content-part->text part)
            (cond
              [(text-part? part) (text-part-text part)]
              [(and (tool-result-part? part) (not (tool-result-part-is-error? part)))
               (define c (tool-result-part-content part))
               (define raw
                 (cond
                   [(string? c) c]
                   [(hash? c) (~a c)]
                   [(list? c) (string-join (map ~a c) " ")]
                   [else ""]))
               (if (> (string-length raw) 500)
                   (string-append (substring raw 0 497) "...")
                   raw)]
              [else ""]))
          (define summaries
            (for/hash ([m (in-list ws-msgs)])
              (define parts (map content-part->text (message-content m)))
              (define full-text (string-join (filter (lambda (s) (> (string-length s) 0)) parts) " "))
              (values (message-id m) full-text)))
          (append conclusions
                  (auto-distill (map message-id ws-msgs) conclusions task-state-raw summaries)))
        (or conclusions '())))
  ;; v0.78.2 G3: Persist auto-distilled conclusions back to session
  (when (and (current-auto-distillation-enabled?)
             session
             (pair? augmented-conclusions)
             conclusions
             (> (length augmented-conclusions) (length conclusions)))
    (guarded-set-task-conclusions! session augmented-conclusions))
  ;; v0.78.2 G2: WS evolution — evolve working set on state transition
  ;; MF1 (GAP-5): Guard at call site — skip when same state to avoid
  ;; unnecessary snapshot + evolve-working-set overhead.
  (define ws-old-state (current-last-task-fsm-state))
  (when (and (current-ws-evolution-enabled?)
             ws-early
             session
             task-state
             (not (eq? task-state-raw 'idle))
             ;; GAP-5: Skip same-state transitions (first transition: old=#f → proceed)
             (or (not ws-old-state) (not (eq? ws-old-state task-state))))
    (define result
      (evolve-working-set-for-state/result ws-early ws-old-state task-state augmented-conclusions))
    (current-last-task-fsm-state task-state)
    (when (and (evolution-result? result) session)
      (guarded-set-working-set-evolved! session result)))
  ;; v0.96.13 W4: Transition detection — trigger deterministic distillation on state change
  ;; Also resets the loop warning counter on state transition
  ;; MF1-1 fix: Use ws-old-state (captured before WS mutation) instead of
  ;; re-reading current-last-task-fsm-state, which was already mutated above.
  (when (and task-state
             (not (eq? task-state-raw 'idle))
             ws-old-state
             (not (eq? ws-old-state task-state)))
    ;; State transition detected — reset warning counter
    (current-loop-warning-count 0))
  (values task-state-raw task-state augmented-conclusions))

;; ============================================================
;; Context assembly telemetry
;; ============================================================

;; Emit telemetry events for context assembly results.
;; Fires: working-set.injected, context.assembled, context-assembly-detail.
(define (emit-context-assembly-events! bus
                                       session-id
                                       iteration
                                       ctx-to-use
                                       ctx-assembled
                                       tc-struct
                                       ws
                                       config-raw)
  ;; v0.26.0: Emit working-set.injected event
  (when ws
    (emit-typed-event! bus
                       (make-working-set-injected-event #:session-id session-id
                                                        #:turn-id ""
                                                        #:timestamp (current-inexact-milliseconds)
                                                        #:entries (working-set-entry-count ws)
                                                        #:tokens (working-set-token-count ws))))
  ;; Emit context.assembled event
  (define ctx-token-count-promise
    (delay
      (estimate-context-tokens ctx-assembled)))
  (emit-typed-event! bus
                     (make-context-assembled-event
                      #:session-id session-id
                      #:turn-id ""
                      #:timestamp (current-inexact-milliseconds)
                      #:iteration iteration
                      #:total-messages (length ctx-to-use)
                      #:assembled-messages (length ctx-assembled)
                      #:token-count (force ctx-token-count-promise)
                      #:working-set-entries (if ws
                                                (working-set-entry-count ws)
                                                0)
                      #:working-set-tokens (if ws
                                               (working-set-token-count ws)
                                               0)))
  ;; v0.45.5: Emit detailed assembly metrics
  (define tier-a-len (length (tiered-context-tier-a tc-struct)))
  (define tier-b-len (length (tiered-context-tier-b tc-struct)))
  (define tier-c-len (length (tiered-context-tier-c tc-struct)))
  (define assembled-total (+ tier-a-len tier-b-len tier-c-len))
  (define assembled-ids
    (for/set ([m (in-list ctx-assembled)])
      (message-id m)))
  (define excluded-id-list
    (for/list ([m (in-list ctx-to-use)]
               #:unless (set-member? assembled-ids (message-id m)))
      (message-id m)))
  (define excluded-ids-str (string-join excluded-id-list ","))
  (define summary-len
    (for/sum ([m (in-list ctx-assembled)] #:when (eq? (message-kind m) 'compaction-summary))
             (for/sum ([p (in-list (message-content m))] #:when (text-part? p))
                      (string-length (text-part-text p)))))
  (define gsd-pinned (for/sum ([m (in-list ctx-assembled)] #:when (gsd-progress-message? m)) 1))
  (emit-typed-event! bus
                     (make-context-assembly-detail-event
                      #:session-id session-id
                      #:turn-id ""
                      #:timestamp (current-inexact-milliseconds)
                      #:total-messages (length ctx-to-use)
                      #:tier-a-count tier-a-len
                      #:tier-b-count tier-b-len
                      #:tier-c-count tier-c-len
                      #:excluded-count (- (length ctx-to-use) assembled-total)
                      #:excluded-ids excluded-ids-str
                      #:summary-length summary-len
                      #:gsd-pinned-count gsd-pinned
                      #:ws-entry-count (if ws
                                           (working-set-entry-count ws)
                                           0)
                      #:ws-tokens (if ws
                                      (working-set-token-count ws)
                                      0)
                      #:cache-hit-p #f)))
