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
         racket/contract
         (only-in "../util/loop-result.rkt" loop-result?)
         (only-in "../util/error/errors.rkt" raise-extension-error)
         "../agent/event-bus.rkt"
         (only-in "../util/cancellation.rkt" cancellation-token?)
         (only-in "../llm/provider.rkt" provider?)
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
         (only-in "../runtime/auto-retry.rkt" with-auto-retry)
         (only-in "provider/provider-factory.rkt" provider-is-mock?)
         (only-in "runtime-helpers.rkt" emit-session-event! maybe-dispatch-hooks)
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
                  apply-context-assembly-profile!)
         ;; tiered-context? needed for provide contract
         (only-in "context/context-assembly.rkt" tiered-context?)
         (only-in "context-assembly/turn-context.rkt"
                  current-last-task-fsm-state
                  symbol->task-state
                  assemble-context/pure
                  prepare-turn-context-state
                  emit-context-assembly-events!)
         (only-in "extension-setup.rkt" register-session-extensions!))

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
                (values list? (or/c hook-result? #f) tiered-context?))])
         current-last-task-fsm-state)

;; ============================================================
;; ============================================================
;; Context assembly — helpers imported from turn-context.rkt
;; ============================================================

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
  (define-values (task-state-raw task-state augmented-conclusions)
    (prepare-turn-context-state ctx-to-use config-raw session))
  ;; Phase 2: Apply profile and run pure assembly
  (define profile (config-context-assembly-profile config-raw))
  (unless (eq? profile 'off)
    ;; v0.97.4 GAP-E: Dynamic conclusion budget from actual max-context-tokens
    (apply-context-assembly-profile! profile (config-max-context-tokens config-raw)))
  (define-values (ctx-assembled assembly-hook-result tc-struct)
    (assemble-context/pure ctx-to-use
                           config-raw
                           #:hook-dispatcher ctx-assembly-hook-dispatcher
                           #:task-state task-state
                           #:conclusions augmented-conclusions
                           #:state-aware? (config-task-state-aware? config)
                           #:recent-tool-calls (if session
                                                   (agent-session-recent-tool-calls session)
                                                   '())))
  ;; Handle block action from context-assembly hook
  (when (and assembly-hook-result (eq? (hook-result-action assembly-hook-result) 'block))
    (current-turn-fsm-state turn-state-blocked)
    (emit-typed-event! bus
                       (make-context-blocked-event #:session-id session-id
                                                   #:turn-id ""
                                                   #:timestamp (current-inexact-milliseconds)
                                                   #:reason "extension-block"))
    (raise-extension-error "Context assembly blocked by extension" "unknown" "turn.started"))
  ;; Phase 3: Emit telemetry events
  (emit-context-assembly-events! bus
                                 session-id
                                 iteration
                                 ctx-to-use
                                 ctx-assembled
                                 tc-struct
                                 ws
                                 config-raw)
  ;; Phase 4: Final context hook dispatch
  (define-values (ctx-final _ctx-hook) (maybe-dispatch-hooks ext-reg 'context ctx-assembled))
  ctx-final)

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
  ;; v0.15.1 Wave 1: Also resolve max-tokens from config if not in flat runtime hash.
  ;; Config may have max-tokens in: top-level, providers.<name>.max-tokens, or models.default.max-tokens.
  (define provider-settings
    (let* ([settings (config-settings config)]
           [model-name (config-model-name config)]
           [resolve-max-tokens
            (lambda ()
              (or
               (hash-has-key? provider-settings-raw 'max-tokens)
               (and settings (setting-ref settings 'max-tokens #f))
               (and settings
                    model-name
                    (setting-ref* settings `(providers ,(string->symbol model-name) max-tokens) #f))
               (and settings (setting-ref* settings '(providers openai-compatible max-tokens) #f))
               (and settings (setting-ref* settings '(models default max-tokens) #f))))])
      (if (and settings (not (hash-has-key? provider-settings-raw 'max-tokens)))
          (let ([mt (resolve-max-tokens)])
            (if mt
                (hash-set provider-settings-raw 'max-tokens mt)
                provider-settings-raw))
          provider-settings-raw)))

  (define ctx-for-retry (box ctx-final))

  (with-auto-retry (lambda ()
                     (run-agent-turn (unbox ctx-for-retry)
                                     prov
                                     bus
                                     #:session-id session-id
                                     #:turn-id turn-id
                                     #:tools tools
                                     #:cancellation-token token
                                     #:provider-settings provider-settings))
                   #:max-retries 2
                   #:base-delay-ms 1000
                   #:on-retry (lambda (attempt max-retries delay-ms error-msg error-type)
                                (emit-typed-event! bus
                                                   (make-auto-retry-start-event
                                                    #:session-id session-id
                                                    #:turn-id turn-id
                                                    #:timestamp (current-inexact-milliseconds)
                                                    #:attempt attempt
                                                    #:max-retries max-retries
                                                    #:delay-ms delay-ms
                                                    #:error error-msg
                                                    #:error-type error-type)))))
