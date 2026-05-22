#lang racket/base

;; runtime/iteration/loop-config.rkt — configuration bundle for iteration loop
;;
;; Bundles the many parameters of run-iteration-loop into a single struct,
;; reducing parameter coupling and improving call-site ergonomics.
;;
;; Provides:
;;   loop-config — configuration struct for the iteration loop
;;   loop-config? — predicate
;;   loop-config-* — field accessors

(require racket/contract
         (only-in "../../llm/provider.rkt" provider?)
         (only-in "../../agent/event-bus.rkt" event-bus?)
         (only-in "../../tools/registry.rkt" tool-registry?)
         (only-in "../../extensions/api.rkt" extension-registry?)
         (only-in "../../util/cancellation.rkt" cancellation-token?)
         (only-in "../../runtime/session-config.rkt" session-config? hash->session-config)
         (only-in "../../runtime/working-set.rkt" working-set?)
         (only-in "../../runtime/session-types.rkt" agent-session?)
         (only-in "../../util/loop-result.rkt" loop-result?))

;; ============================================================
;; loop-config struct
;; ============================================================

;; Bundles all iteration loop parameters into a single configuration record.
;; Required fields are positional; optional fields use keyword constructors
;; with sensible defaults.
(struct loop-config
        (context ; (listof message?) — initial context messages
         provider ; (or/c provider? #f) — LLM provider
         bus ; event-bus? — event bus for dispatch
         registry ; (or/c tool-registry? #f) — tool registry
         ext-registry ; (or/c extension-registry? #f) — extension registry
         log-path ; (or/c path-string? path?) — log file path
         session-id ; string? — session identifier
         max-iterations ; exact-nonnegative-integer? — iteration limit
         ;; Optional fields (keyword, default #f)
         cancellation-token ; (or/c cancellation-token? #f)
         config ; session-config?
         queue ; (or/c queue? #f) — steering message queue
         follow-up-delivery-mode ; (or/c 'all 'one-at-a-time)
         injected-box ; (or/c box? #f) — injected message box
         shutdown-check ; (or/c procedure? #f)
         force-shutdown-check ; (or/c procedure? #f)
         working-set ; (or/c working-set? #f)
         session ; (or/c agent-session? #f)
         )
  #:transparent)

;; Constructor wrapper with keyword args and defaults
(define (make-loop-config context
                          provider
                          bus
                          registry
                          ext-registry
                          log-path
                          session-id
                          max-iterations
                          #:cancellation-token [cancellation-token #f]
                          #:config [config (hash->session-config (hash))]
                          #:queue [queue #f]
                          #:follow-up-delivery-mode [follow-up-delivery-mode 'all]
                          #:injected-box [injected-box #f]
                          #:shutdown-check [shutdown-check #f]
                          #:force-shutdown-check [force-shutdown-check #f]
                          #:working-set [working-set #f]
                          #:session [session #f])
  (loop-config context
               provider
               bus
               registry
               ext-registry
               log-path
               session-id
               max-iterations
               cancellation-token
               config
               queue
               follow-up-delivery-mode
               injected-box
               shutdown-check
               force-shutdown-check
               working-set
               session))

(provide loop-config
         loop-config?
         loop-config-context
         loop-config-provider
         loop-config-bus
         loop-config-registry
         loop-config-ext-registry
         loop-config-log-path
         loop-config-session-id
         loop-config-max-iterations
         loop-config-cancellation-token
         loop-config-config
         loop-config-queue
         loop-config-follow-up-delivery-mode
         loop-config-injected-box
         loop-config-shutdown-check
         loop-config-force-shutdown-check
         loop-config-working-set
         loop-config-session
         (contract-out [make-loop-config
                        (->* ((listof any/c) (or/c provider? #f)
                                             event-bus?
                                             (or/c tool-registry? #f)
                                             (or/c extension-registry? #f)
                                             (or/c path-string? path?)
                                             string?
                                             exact-nonnegative-integer?)
                             (#:cancellation-token (or/c cancellation-token? #f)
                              #:config (or/c session-config? #f)
                              #:queue (or/c any/c #f)
                              #:follow-up-delivery-mode (or/c 'all 'one-at-a-time)
                              #:injected-box (or/c box? #f)
                              #:shutdown-check (or/c procedure? #f)
                              #:force-shutdown-check (or/c procedure? #f)
                              #:working-set (or/c working-set? #f)
                              #:session (or/c agent-session? #f))
                             loop-config?)]))
