#lang racket/base

;; runtime/turn-orchestrator.rkt — single-turn provider dispatch and tool execution
;; STABILITY: internal
;;
;; Extracted from iteration.rkt for single-responsibility separation.
;; Handles: context assembly → provider turn → tool execution.
;;
;; This module is the ONLY runtime module that imports upward into
;; the tools/ and extensions/ layers. Other runtime modules must
;; not import tools/tool.rkt, tools/scheduler.rkt, extensions/hooks.rkt,
;; or extensions/context.rkt directly.
;;
;; ── LAYER EXCEPTION (ARCH-01 / #341) ──────────────────────────
;;   tools/tool.rkt       → list-tools-jsexpr, merge-tool-lists
;;   tools/scheduler.rkt  → dependency of tool-coordinator (transitive)
;;   extensions/hooks.rkt → dispatch-hooks (context assembly hook)
;;   extensions/context.rkt → make-extension-ctx (extension registration)
;; ───────────────────────────────────────────────────────────────

(require (only-in racket/dict dict-ref in-dict)
         racket/contract
         racket/list
         racket/promise
         json
         (only-in racket/string string-contains?)
         (only-in "../util/errors.rkt" raise-extension-error)
         (only-in "../util/json-helpers.rkt" ensure-hash-args)
         (only-in "../util/protocol-types.rkt"
                  message?
                  message-id
                  message-role
                  message-content
                  make-message
                  make-text-part
                  text-part?
                  text-part-text
                  make-loop-result
                  loop-result-termination-reason
                  loop-result-messages
                  loop-result-metadata
                  make-event
                  event-ev
                  event-payload
                  tool-call-name
                  tool-call-arguments
                  tool-result-part?
                  tool-result-part-is-error?)
         "../agent/event-bus.rkt"
         (only-in "../util/loop-result.rkt" loop-result?)
         (only-in "../util/cancellation.rkt" cancellation-token?)
         (only-in "../llm/provider.rkt" provider?)
         (only-in "../tools/registry.rkt" tool-registry?)
         (only-in "../extensions/api.rkt" extension-registry?)
         "../agent/loop.rkt"
         ;; ARCH-01: tool registry queries for LLM tool definitions
         (only-in "../tools/tool.rkt" list-tools-jsexpr merge-tool-lists)
         ;; Settings struct for provider settings resolution
         (only-in "../runtime/settings.rkt" setting-ref setting-ref*)
         ;; Transitive dependency of tool-coordinator
         "../tools/scheduler.rkt"
         ;; Context assembly hook dispatch (dispatch-hooks for build-assembled-context)
         "../extensions/hooks.rkt"
         (only-in "../extensions/context.rkt" make-extension-ctx)
         "../runtime/session-store.rkt"
         "../runtime/tool-coordinator.rkt"
         (only-in "../runtime/context-assembly.rkt"
                  build-tiered-context-with-hooks
                  tiered-context->message-list
                  build-tiered-context)
         (only-in "../runtime/working-set.rkt"
                  working-set?
                  working-set-resolve-messages
                  working-set-entry-count
                  working-set-token-count)
         (only-in "../runtime/tool-coordinator.rkt"
                  handle-tool-calls-pending
                  extract-tool-calls-from-messages)
         "../runtime/cutpoint-rules.rkt"
         "../util/ids.rkt"
         (only-in "../util/cancellation.rkt" cancellation-token? cancellation-token-cancelled?)
         ;; R2-6: hook-result accessors
         (only-in "../util/hook-types.rkt" hook-result-action hook-result-payload hook-result?)
         (only-in "../runtime/auto-retry.rkt" with-auto-retry context-overflow-error?)
         ;; Token estimation for context assembly event
         (only-in "../llm/token-budget.rkt" estimate-context-tokens)
         ;; Mock provider detection
         (only-in "provider-factory.rkt" provider-is-mock?)
         ;; Shared helpers
         (only-in "runtime-helpers.rkt" emit-session-event! maybe-dispatch-hooks)
         ;; G4: typed event emission
         "../agent/event-emitter.rkt"
         "../agent/event-structs/iteration-events.rkt")

(provide (contract-out
          [run-provider-turn
           (->* (list?
                       (or/c provider? #f)
                       event-bus?
                       (or/c tool-registry? #f)
                       (or/c extension-registry? #f)
                       string?
                       string?
                       (or/c cancellation-token? #f)
                       hash?)
                (#:tool-list-proc (or/c procedure? #f))
                loop-result?)]
          [build-assembled-context
           (-> list? hash? (or/c extension-registry? #f) event-bus? string? exact-nonnegative-integer? list?)]
          [register-session-extensions! (-> tool-registry? (or/c extension-registry? #f) event-bus? string? (listof hash?))]))

;; ============================================================
;; Context assembly
;; ============================================================

;; Build assembled context using tiered context assembly with hooks.
;; Returns the assembled message list.
(define (build-assembled-context ctx-to-use config ext-reg bus session-id iteration)
  ;; WP-37 + R2-6: Context Assembly with Tier A/B/C separation and Hook support
  (define tier-b-count (dict-ref config 'tier-b-count 20))
  (define tier-c-count (dict-ref config 'tier-c-count 4))
  (define max-tokens (dict-ref config 'max-tokens 8192))
  ;; v0.26.0: Extract working set from config
  ;; v0.29.5 W3: Defer ws-message resolution
  (define ws (dict-ref config 'working-set #f))
  (define ws-messages-promise
    (delay
      (if ws
          (working-set-resolve-messages ws ctx-to-use message-id)
          '())))
  (define ws-messages (force ws-messages-promise))
  (define ws-message-ids (map message-id ws-messages))

  ;; R2-6: Create hook dispatcher function for context assembly
  (define ctx-assembly-hook-dispatcher
    (and ext-reg
         (lambda (hook-point payload)
           (define result (dispatch-hooks hook-point payload ext-reg))
           result)))

  ;; Build tiered context with hook support
  (define-values (tc assembly-hook-result)
    (build-tiered-context-with-hooks ctx-to-use
                                     #:hook-dispatcher ctx-assembly-hook-dispatcher
                                     #:tier-b-count tier-b-count
                                     #:tier-c-count tier-c-count
                                     #:max-tokens max-tokens
                                     #:working-set-messages ws-messages))

  ;; Handle block action from context-assembly hook
  (when (and assembly-hook-result (eq? (hook-result-action assembly-hook-result) 'block))
    (emit-session-event! bus session-id "context.assembly.blocked" (hasheq 'reason "extension-block"))
    (raise-extension-error "Context assembly blocked by extension" "unknown" "turn.started"))

  (define ctx-assembled (tiered-context->message-list tc))

  ;; v0.26.0: Emit working-set.injected event
  (when ws
    (emit-session-event!
     bus
     session-id
     "working-set.injected"
     (hasheq 'entries (working-set-entry-count ws) 'tokens (working-set-token-count ws))))

  ;; Emit context.assembled event (v0.19.12 W1: added tokenCount)
  ;; v0.29.5 W3: Defer token estimation — only computed when forced for event
  (define ctx-token-count-promise
    (delay
      (estimate-context-tokens ctx-assembled)))
  (emit-session-event! bus
                       session-id
                       "context.assembled"
                       (hasheq 'iteration
                               iteration
                               'total-messages
                               (length ctx-to-use)
                               'assembled-messages
                               (length ctx-assembled)
                               'tokenCount
                               (force ctx-token-count-promise)
                               'working-set-entries
                               (if ws
                                   (working-set-entry-count ws)
                                   0)
                               'working-set-tokens
                               (if ws
                                   (working-set-token-count ws)
                                   0)))

  ;; Dispatch 'context hook — extensions can amend final context
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
                           #:tool-registry tool-reg))
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
                           config
                           #:tool-list-proc [tool-list-proc #f])
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
    (let* ([settings (dict-ref config 'settings #f)]
           [model-name (dict-ref config 'model-name #f)]
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
                                (publish! bus
                                          (make-event "auto-retry.start"
                                                      (current-inexact-milliseconds)
                                                      session-id
                                                      turn-id
                                                      (hasheq 'attempt
                                                              attempt
                                                              'max-retries
                                                              max-retries
                                                              'delay-ms
                                                              delay-ms
                                                              'error
                                                              error-msg
                                                              'errorType
                                                              error-type)))
                                (emit-typed-event! bus
                                                   (make-auto-retry-event
                                                    #:session-id session-id
                                                    #:turn-id turn-id
                                                    #:timestamp (current-inexact-milliseconds)
                                                    #:attempt attempt
                                                    #:max-attempts max-retries
                                                    #:error-type error-type)))))
