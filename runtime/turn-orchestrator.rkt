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
;;   extensions/hooks.rkt → dispatch-hooks (context assembly hook)
;;   extensions/context.rkt → make-extension-ctx (extension registration)
;; ───────────────────────────────────────────────────────────────

(require (only-in racket/dict in-dict)
         racket/contract
         racket/list
         racket/promise
         json
         (only-in racket/string string-contains? string-join)
         (only-in "../util/error/errors.rkt" raise-extension-error)
         (only-in "../util/json/json-helpers.rkt" ensure-hash-args)
         (only-in "../util/content/content-parts.rkt" make-text-part text-part? tool-result-part?)
         (only-in "../util/event/event.rkt" make-event event-ev)
         (only-in "../util/loop-result.rkt" make-loop-result loop-result-messages)
         (only-in "../util/message/message.rkt"
                  message?
                  message-id
                  message-role
                  message-content
                  make-message)
         (only-in "../util/tool/tool-types.rkt" tool-call-name tool-call-arguments)
         (only-in "../util/content/content-parts.rkt" text-part-text tool-result-part-is-error?)
         (only-in "../util/event/event.rkt" event-payload)
         (only-in "../util/loop-result.rkt" loop-result-termination-reason loop-result-metadata)
         "../agent/event-bus.rkt"
         (only-in "../util/loop-result.rkt" loop-result?)
         (only-in "../util/cancellation.rkt" cancellation-token?)
         (only-in "../llm/provider.rkt" provider?)
         ;; ARCH-01: tool registry + extension registry via adapter
         (only-in "layer-adapters.rkt" tool-registry? extension-registry?)
         "../agent/loop.rkt"
         (only-in "../agent/loop-fsm.rkt" current-turn-fsm-state turn-state-blocked)
         ;; ARCH-01: tool registry queries via adapter
         (only-in "layer-adapters.rkt" list-tools-jsexpr merge-tool-lists)
         ;; Settings struct for provider settings resolution
         (only-in "../runtime/settings.rkt" setting-ref setting-ref*)
         ;; Transitive dependency of tool-coordinator (via adapter)
         (only-in "layer-adapters.rkt" tool-result? tool-registry?)
         ;; Context assembly hooks via adapter
         (only-in "layer-adapters.rkt" dispatch-hooks)
         (only-in "layer-adapters.rkt" make-extension-ctx)
         ;; GSD session context via adapter
         (only-in "layer-adapters.rkt" current-gsd-ctx gsd-session-ctx?)
         "session/session-store.rkt"
         "../runtime/tool-coordinator.rkt"
         (only-in "context/context-assembly.rkt"
                  build-tiered-context-with-hooks
                  tiered-context->message-list
                  build-tiered-context
                  tiered-context?
                  tiered-context-tier-a
                  tiered-context-tier-b
                  tiered-context-tier-c)
         (only-in "../runtime/working-set.rkt"
                  working-set?
                  working-set-resolve-messages
                  working-set-entry-count
                  working-set-token-count)
         (only-in "context-assembly/serialization.rkt"
                  gsd-progress-message?
                  build-tiered-context/state-aware
                  current-task-state-aware-assembly?)
         (only-in "../runtime/tool-coordinator.rkt"
                  handle-tool-calls-pending
                  extract-tool-calls-from-messages)
         "compaction/cutpoint-rules.rkt"
         "../util/ids.rkt"
         (only-in "../util/cancellation.rkt" cancellation-token? cancellation-token-cancelled?)
         ;; R2-6: hook-result accessors
         (only-in "../util/hook-types.rkt" hook-result-action hook-result-payload hook-result?)
         (only-in "../runtime/auto-retry.rkt" with-auto-retry context-overflow-error?)
         ;; Token estimation for context assembly event
         (only-in "../llm/token-budget.rkt" estimate-context-tokens)
         ;; Mock provider detection
         (only-in "provider/provider-factory.rkt" provider-is-mock?)
         ;; Shared helpers
         (only-in "runtime-helpers.rkt" emit-session-event! maybe-dispatch-hooks)
         ;; G4: typed event emission
         "../agent/event-emitter.rkt"
         "../agent/event-structs/iteration-events.rkt"
         "../agent/event-structs/session-events.rkt"
         (only-in "session/session-types.rkt"
                  agent-session?
                  agent-session-task-fsm-state
                  agent-session-task-conclusions
                  agent-session-recent-tool-calls)
         (only-in "session/session-mutation.rkt"
                  guarded-set-working-set-evolved!
                  guarded-set-task-conclusions!)
         (only-in "../runtime/context-assembly/ws-evolution.rkt"
                  evolve-working-set-for-state/result
                  evolution-result?)
         (only-in "../runtime/context-assembly/state-aware-builder.rkt" current-ws-evolution-enabled?)
         (only-in "session/session-config.rkt"
                  session-config?
                  hash->session-config
                  config-tier-b-count
                  config-tier-c-count
                  config-max-tokens
                  config-working-set
                  config-settings
                  config-model-name
                  config-task-state-aware?)
         (only-in "../util/message/message.rkt" message-kind message-content)
         racket/set
         (only-in "../runtime/context-assembly/auto-distillation.rkt"
                  auto-distill
                  current-auto-distillation-enabled?)
         (only-in "session/session-config.rkt"
                  config-context-assembly-profile
                  apply-context-assembly-profile!)
         ;; Turn-context: extracted context assembly helpers
         (only-in "context-assembly/turn-context.rkt"
                  current-last-task-fsm-state
                  symbol->task-state
                  assemble-context/pure
                  prepare-turn-context-state
                  emit-context-assembly-events!))

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
    (apply-context-assembly-profile! profile))
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
;; Extension Pre-Registration
;; ============================================================

;;; register-session-extensions! : tool-registry? extension-registry? event-bus?
;;;                                  string? -> (listof hash?)
;;;
;;; Dispatches the 'register-tools hook through the extension registry
;;; so that extension tools are available and extension state (event bus,
;;; pinned dir) is initialized BEFORE the first run-prompt! call.
;;;
;;; Returns the list of extension-provided tools (as jsexpr hashes),
;;; or '() if no extensions or no tools registered.
;;;
;;; Idempotent: extensions track their own registration state internally.
;;; Safe to call multiple times.
(define (register-session-extensions! tool-reg ext-reg bus session-id)
  (cond
    [(not ext-reg) '()]
    [else
     (define the-ext-ctx
       (make-extension-ctx #:session-id session-id
                           #:session-dir #f
                           #:event-bus bus
                           #:extension-registry ext-reg
                           #:tool-registry tool-reg
                           #:gsd-ctx (current-gsd-ctx)))
     (define-values (_amended hook-res)
       (maybe-dispatch-hooks ext-reg 'register-tools (hasheq) #:ctx the-ext-ctx))
     (if (and hook-res (eq? (hook-result-action hook-res) 'amend))
         (hash-ref (hook-result-payload hook-res) 'tools '())
         '())]))

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
