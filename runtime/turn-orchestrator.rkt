#lang racket/base

;; runtime/turn-orchestrator.rkt — single-turn provider dispatch and tool execution
;; STABILITY: internal
;;
;; Extracted from iteration.rkt for single-responsibility separation.
;; Handles: context assembly → provider turn → tool execution.
;;

(define-logger q-turn-orch)
;; This module is one of the boundary modules that imports upward into
;; the tools/ and extensions/ layers (agent-session.rkt is another).
;; Other runtime modules should not import tools/tool.rkt, tools/scheduler.rkt,
;; extensions/hooks.rkt, or extensions/context.rkt directly.
;;
;; ── LAYER EXCEPTION (ARCH-01 / #341) ──────────────────────────
;;   tools/tool.rkt       → list-tools-jsexpr, merge-tool-lists
;;   tools/scheduler.rkt  → dependency of tool-coordinator (transitive)
;;   layer-adapters.rkt   → dispatch-hooks (re-exported from extensions/hooks.rkt)
;;   extensions/context.rkt → make-extension-ctx (extension registration)
;; ───────────────────────────────────────────────────────────────

(require (only-in racket/dict in-dict)
         (only-in racket/list filter-map)
         racket/contract
         (only-in "../util/loop-result.rkt" loop-result? loop-result-messages)
         (only-in "../util/message/message.rkt"
                  message
                  message-id
                  message-parent-id
                  message-role
                  message-content
                  message-meta-safe)
         (only-in "../util/content/content-parts.rkt"
                  tool-call-part?
                  tool-call-part-id
                  tool-result-part?
                  tool-result-part-tool-call-id
                  text-part?
                  text-part-text)
         (only-in "../util/error/errors.rkt" raise-extension-error)
         "../util/event/event-bus.rkt"
         (only-in "../util/cancellation.rkt" cancellation-token?)
         (only-in "../llm/provider.rkt" provider?)
         (only-in "../llm/token-budget.rkt" estimate-context-tokens)
         (only-in "layer-adapters.rkt"
                  tool-registry?
                  extension-registry?
                  dispatch-hooks
                  list-tools-jsexpr
                  merge-tool-lists)
         "../agent/loop.rkt"
         (only-in "../agent/loop-fsm.rkt" current-turn-fsm-state turn-state-blocked)
         (only-in "../runtime/settings.rkt" setting-ref setting-ref*)
         "../util/ids.rkt"
         (only-in "../util/hook-types.rkt" hook-result-action hook-result?)
         (only-in "../runtime/auto-retry.rkt" default-cumulative-ceiling-secs)
         (only-in "provider/provider-factory.rkt" provider-is-mock?)
         (only-in "runtime-helpers.rkt" emit-session-event! maybe-dispatch-hooks)
         ;; v0.99.84: Post-turn memory extraction moved from agent/ to runtime/
         (only-in "memory/auto-extraction.rkt" maybe-auto-extract-after-response!)
         (only-in "memory/reflection.rkt" maybe-reflect-session-memories!)
         "../agent/event-emitter.rkt"
         "../agent/event-structs/iteration-events.rkt"
         "../agent/event-structs/session-events.rkt"
         (only-in "session/session-types.rkt" agent-session-recent-tool-calls)
         (only-in "session/session-config.rkt"
                  session-config?
                  config-working-set
                  config-settings
                  config-model-name
                  config-task-state-aware?
                  config-context-assembly-profile
                  config-max-context-tokens
                  profile->options
                  context-assembly-options?)
         ;; tiered-context? needed for provide contract
         (only-in "context/context-assembly.rkt" tiered-context?)
         (only-in "working-set.rkt"
                  working-set-enforce-context-share!
                  working-set-selective-remove!
                  ws-entry-path
                  ws-entry-message-id
                  ws-entry-token-estimate)
         (only-in "context-assembly/turn-context.rkt"
                  symbol->task-state
                  assemble-context/pure
                  prepare-turn-context-state
                  emit-context-assembly-events!)
         (only-in "extension-setup.rkt" register-session-extensions!)
         (only-in "provider-retry.rkt" call-with-provider-retry default-partial-recovery-min-chars)
         (only-in "provider-health.rkt"
                  make-provider-health
                  provider-health?
                  default-health-window-secs
                  default-health-failure-threshold))

;; v0.99.82 W2 NR-3: Per-session provider health trackers.
;; Keyed by session-id; persists across turns within the same session.
(define session-health-trackers (make-hash))

;; v1.00.08 W2 PN-7: Resolve the per-model cumulative retry ceiling.
;; providers.<name>.retry-ceiling-secs in session-config settings overrides the
;; module default (default-cumulative-ceiling-secs). Returns the effective ceiling
;; in seconds.
(define (resolve-retry-ceiling-secs config)
  (let* ([settings (config-settings config)]
         [model-name (config-model-name config)])
    (or (and settings
             model-name
             (setting-ref* settings `(providers ,(string->symbol model-name) retry-ceiling-secs) #f))
        default-cumulative-ceiling-secs)))

(provide (contract-out
          [run-provider-turn
           (->* (list? (or/c provider? #f)
                       event-bus?
                       (or/c tool-registry? #f)
                       (or/c extension-registry? #f)
                       string?
                       string?
                       (or/c cancellation-token? #f)
                       session-config?)
                (#:tool-list-proc (or/c procedure? #f))
                loop-result?)]
          [build-assembled-context
           (->* (list? session-config?
                       (or/c extension-registry? #f)
                       event-bus?
                       string?
                       exact-nonnegative-integer?)
                (#:session (or/c any/c #f))
                list?)]
          [register-session-extensions!
           (-> tool-registry? (or/c extension-registry? #f) event-bus? string? (listof hash?))]
          [assemble-context/pure
           (->* (list? session-config?)
                (#:hook-dispatcher (or/c procedure? #f)
                                   #:state-aware? (or/c boolean? #f)
                                   #:recent-tool-calls list?)
                (values list? (or/c hook-result? #f) tiered-context?))]
          ;; v1.00.08 W2 PN-7: per-model cumulative retry ceiling resolution.
          [resolve-retry-ceiling-secs (-> session-config? real?)]))

;; ============================================================
;; ============================================================
;; Context assembly — helpers imported from turn-context.rkt
;; ============================================================

(define (message-tool-result-call-ids m)
  (for/list ([part (in-list (message-content m))]
             #:when (tool-result-part? part))
    (tool-result-part-tool-call-id part)))

(define (filter-evicted-working-set-pairs ctx evicted)
  (define evicted-ids (map ws-entry-message-id evicted))
  (define removable-results
    (filter (lambda (m)
              (and (member (message-id m) evicted-ids)
                   (not (hash-ref (message-meta-safe m) 'gsd-pin #f))))
            ctx))
  (define removable-ids (map message-id removable-results))
  (define affected-parent-ids (filter values (map message-parent-id removable-results)))
  (define without-results (filter (lambda (m) (not (member (message-id m) removable-ids))) ctx))
  (define retained-call-ids
    (apply append
           (for/list ([m (in-list without-results)]
                      #:when (and (eq? (message-role m) 'tool)
                                  (member (message-parent-id m) affected-parent-ids)))
             (message-tool-result-call-ids m))))
  (filter-map (lambda (m)
                (if (member (message-id m) affected-parent-ids)
                    (let ([content (filter (lambda (part)
                                             (or (not (tool-call-part? part))
                                                 (member (tool-call-part-id part) retained-call-ids)))
                                           (message-content m))])
                      (and (pair? content) (struct-copy message m [content content])))
                    m))
              without-results))

(define (gsd-pinned-message? m)
  (hash-ref (message-meta-safe m) 'gsd-pin #f))

(define (enforce-provider-bound-working-set-share! ws ctx-pre-hook ctx-final bus session-id)
  ;; A final hook may remove an ordinary WS message. Reconcile it out of active
  ;; state. GSD-pinned entries remain as higher-authority state even when an
  ;; extension omits their rendered message.
  (when ws
    (define final-ids (map message-id ctx-final))
    (define gsd-authority-ids (map message-id (filter gsd-pinned-message? ctx-pre-hook)))
    (working-set-selective-remove! ws
                                   (lambda (entry)
                                     (or (member (ws-entry-message-id entry) final-ids)
                                         (member (ws-entry-message-id entry) gsd-authority-ids)))))
  (let loop ([ctx ctx-final]
             [all-actions '()])
    (define actions
      (if ws
          (working-set-enforce-context-share! ws (estimate-context-tokens ctx))
          '()))
    (define accumulated (append all-actions actions))
    (if (null? actions)
        (begin
          (when (pair? accumulated)
            (emit-session-event!
             bus
             session-id
             "working-set.context-share-enforced"
             (hasheq
              'evicted
              (for/list ([entry (in-list accumulated)])
                (hasheq 'path (ws-entry-path entry) 'tokens (ws-entry-token-estimate entry))))))
          ctx)
        (loop (filter-evicted-working-set-pairs ctx actions) accumulated))))

;; Build assembled context using tiered context assembly with hooks.
;; Returns the assembled message list.
(define (build-assembled-context ctx-to-use
                                 config-raw
                                 ext-reg
                                 bus
                                 session-id
                                 iteration
                                 #:session [session #f])
  (define config config-raw)
  (define ws (config-working-set config))
  (define ctx-assembly-hook-dispatcher
    (and ext-reg
         (lambda (hook-point payload)
           (define result (dispatch-hooks hook-point payload ext-reg))
           result)))
  ;; Phase 1: Prepare task state and conclusions
  (define-values (task-state-raw task-state augmented-conclusions reflection-evt)
    (prepare-turn-context-state ctx-to-use config-raw session))
  ;; Phase 2: Build options from profile and run pure assembly
  ;; v0.99.54 W2 R-8: Use profile->options (pure) instead of apply-context-assembly-profile! (parameter mutation)
  (define profile (config-context-assembly-profile config-raw))
  (define ca-options (profile->options profile (config-max-context-tokens config-raw)))
  (define-values (ctx-assembled assembly-hook-result tc-struct)
    (assemble-context/pure ctx-to-use
                           config-raw
                           #:hook-dispatcher ctx-assembly-hook-dispatcher
                           #:task-state task-state
                           #:conclusions augmented-conclusions
                           #:state-aware? (config-task-state-aware? config)
                           #:ca-options ca-options
                           #:recent-tool-calls (if session
                                                   (agent-session-recent-tool-calls session)
                                                   '())
                           #:reflection-event reflection-evt))
  ;; Handle block action from context-assembly hook
  (when (and assembly-hook-result (eq? (hook-result-action assembly-hook-result) 'block))
    (current-turn-fsm-state turn-state-blocked)
    (emit-typed-event! bus
                       (make-context-blocked-event #:session-id session-id
                                                   #:turn-id ""
                                                   #:timestamp (current-inexact-milliseconds)
                                                   #:reason "extension-block"))
    (raise-extension-error "Context assembly blocked by extension" "unknown" "turn.started"))
  ;; Phase 3: Final context hook dispatch. Telemetry must describe the exact
  ;; provider-bound payload, not the pre-hook candidate.
  (define-values (ctx-final _ctx-hook) (maybe-dispatch-hooks ext-reg 'context ctx-assembled))
  (define ctx-bounded
    (enforce-provider-bound-working-set-share! ws ctx-assembled ctx-final bus session-id))
  ;; Phase 4: Emit telemetry from final provider-bound context and bounded WS.
  (emit-context-assembly-events! bus
                                 session-id
                                 iteration
                                 ctx-to-use
                                 ctx-bounded
                                 tc-struct
                                 ws
                                 config-raw)
  ctx-bounded)

;; ============================================================
;; Extension Pre-Registration — delegated to extension-setup.rkt
;; ============================================================

;; Provider turn
;; ============================================================

;; Run the provider turn: dispatch before-provider-request hook, then run agent turn.
;; Returns the loop-result from run-agent-turn.
(define (run-provider-turn ctx-final
                           prov
                           bus
                           reg
                           ext-reg
                           session-id
                           turn-id
                           token
                           config-raw
                           #:tool-list-proc [tool-list-proc #f])
  (define config config-raw)
  ;; v0.28.20 T7: Emit system.warning if mock provider is being used
  (when (provider-is-mock? prov)
    (emit-session-event! bus
                         session-id
                         "system.warning"
                         (hasheq 'message
                                 "No API key found — using mock provider. Check .q/credentials.json"
                                 'provider
                                 "mock")))

  ;; Dispatch 'before-provider-request hook (informational)
  (define-values (_bpr-payload _bpr-res)
    (maybe-dispatch-hooks ext-reg
                          'before-provider-request
                          (hasheq 'session-id session-id 'turn-id turn-id)))

  ;; Get tools from registry for the LLM request
  (define base-tools (and reg (list-tools-jsexpr reg)))

  ;; #673: Merge extension-provided tools into the tool list
  ;; v0.20.5 W3: Uses shared register-session-extensions! function.
  ;; Idempotent — extensions track their own state.
  (define ext-tools (and ext-reg (register-session-extensions! reg ext-reg bus session-id)))
  (define tools
    (cond
      [tool-list-proc (tool-list-proc base-tools ext-tools)]
      [(and base-tools (pair? ext-tools)) (merge-tool-lists base-tools ext-tools)]
      [else base-tools]))

  ;; v0.14.4 Wave 2 FIX: Extract ONLY provider-specific settings from config.
  ;; The full config is a mutable hash with event-bus, extension-registry, etc.
  ;; Passing it to make-model-request causes hash-set contract violations
  ;; because provider.rkt's ensure-model-setting calls hash-set (immutable-only).
  (define provider-settings-raw
    (for/hash ([(k v) (in-dict config)]
               #:when (memq k '(max-tokens temperature top_p frequency_penalty presence_penalty)))
      (values k v)))
  (define active-model-name (config-model-name config))
  (define provider-settings-with-model
    (if active-model-name
        (hash-set provider-settings-raw 'model active-model-name)
        provider-settings-raw))
  ;; v0.15.1 Wave 1: Also resolve max-tokens from config if not in flat runtime hash.
  ;; Config may have max-tokens in: top-level, providers.<name>.max-tokens, or models.default.max-tokens.
  (define provider-settings
    (let* ([settings (config-settings config)]
           [model-name (config-model-name config)]
           [resolve-max-tokens
            (lambda ()
              (or
               (hash-has-key? provider-settings-with-model 'max-tokens)
               (and settings (setting-ref settings 'max-tokens #f))
               (and settings
                    model-name
                    (setting-ref* settings `(providers ,(string->symbol model-name) max-tokens) #f))
               (and settings (setting-ref* settings '(providers openai-compatible max-tokens) #f))
               (and settings (setting-ref* settings '(models default max-tokens) #f))))])
      (if (and settings (not (hash-has-key? provider-settings-with-model 'max-tokens)))
          (let ([mt (resolve-max-tokens)])
            (if mt
                (hash-set provider-settings-with-model 'max-tokens mt)
                provider-settings-with-model))
          provider-settings-with-model)))

  ;; v0.99.81 W2 PN-7: Resolve cumulative retry ceiling from settings.
  ;; providers.<name>.retry-ceiling-secs overrides the default.
  (define retry-ceiling-secs (resolve-retry-ceiling-secs config))

  ;; v0.99.82 W2 NR-3: Resolve provider health gate configuration and tracker.
  ;; providers.<name>.health-window-secs and health-failure-threshold override defaults.
  (define-values (health-tracker health-window health-threshold)
    (let* ([settings (config-settings config)]
           [model-name (config-model-name config)]
           [window (or (and settings
                            model-name
                            (setting-ref* settings
                                          `(providers ,(string->symbol model-name) health-window-secs)
                                          #f))
                       default-health-window-secs)]
           [threshold (or (and settings
                               model-name
                               (setting-ref* settings
                                             `(providers ,(string->symbol model-name)
                                                         health-failure-threshold)
                                             #f))
                          default-health-failure-threshold)])
      ;; Get or create per-session health tracker
      (define tracker (hash-ref! session-health-trackers session-id make-provider-health))
      (values tracker window threshold)))

  ;; v0.99.82 W3 NR-4: Resolve partial result recovery configuration.
  ;; providers.<name>.partial-recovery (default #f) enables continuation injection.
  ;; providers.<name>.partial-recovery-min-chars (default 200) sets the threshold.
  (define-values (partial-recovery? partial-recovery-min-chars)
    (let* ([settings (config-settings config)]
           [model-name (config-model-name config)]
           [enabled (and settings
                         model-name
                         (setting-ref* settings
                                       `(providers ,(string->symbol model-name) partial-recovery)
                                       #f))]
           [min-chars (or (and settings
                               model-name
                               (setting-ref* settings
                                             `(providers ,(string->symbol model-name)
                                                         partial-recovery-min-chars)
                                             #f))
                          default-partial-recovery-min-chars)])
      (values (and enabled #t) min-chars)))

  (define turn-result
    (call-with-provider-retry (lambda (retry-ctx retry-settings)
                                (run-agent-turn retry-ctx
                                                prov
                                                bus
                                                #:session-id session-id
                                                #:turn-id turn-id
                                                #:tools tools
                                                #:cancellation-token token
                                                #:provider-settings retry-settings))
                              ctx-final
                              provider-settings
                              bus
                              session-id
                              turn-id
                              retry-ceiling-secs
                              #:health-tracker health-tracker
                              #:health-window-secs health-window
                              #:health-failure-threshold health-threshold
                              #:partial-recovery partial-recovery?
                              #:partial-recovery-min-chars partial-recovery-min-chars
                              #:cancellation-token token))
  ;; v0.99.84: Post-turn memory extraction and reflection.
  ;; Moved from agent/loop-stream.rkt — Agent Core must not depend on Runtime Memory.
  ;; Non-fatal: extraction failures do not affect the turn result.
  (define assistant-text
    (for/or ([m (in-list (loop-result-messages turn-result))])
      (and (eq? (message-role m) 'assistant)
           (for/or ([p (in-list (message-content m))])
             (and (text-part? p) (text-part-text p))))))
  (when assistant-text
    (with-handlers ([exn:fail? (lambda (e)
                                 (log-q-turn-orch-warning "auto-extract-after-response failed: ~a"
                                                          (exn-message e)))])
      (maybe-auto-extract-after-response! assistant-text #:session-id session-id)))
  (with-handlers ([exn:fail? (lambda (e)
                               (log-q-turn-orch-warning "reflect-session-memories failed: ~a"
                                                        (exn-message e)))])
    (maybe-reflect-session-memories! #:session-id session-id))
  turn-result)
