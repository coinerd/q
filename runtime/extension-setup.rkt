#lang racket/base
;; runtime/extension-setup.rkt — Extension registration for sessions
;; STABILITY: internal
;;
;; Extracted from turn-orchestrator.rkt (v0.96.0 W3) for module sizing.

(require racket/contract
         (only-in "layer-adapters.rkt"
                  tool-registry?
                  extension-registry?
                  make-extension-ctx
                  dispatch-hooks)
         (only-in "layer-adapters.rkt" current-gsd-ctx)
         "../agent/event-bus.rkt"
         (only-in "../util/hook-types.rkt" hook-result-action hook-result-payload hook-result?)
         (only-in "runtime-helpers.rkt" maybe-dispatch-hooks))

(provide (contract-out
          [register-session-extensions!
           (-> tool-registry? (or/c extension-registry? #f) event-bus? string? (listof hash?))]))

;; register-session-extensions! : tool-registry? extension-registry? event-bus?
;;                                  string? -> (listof hash?)
;;
;; Dispatches the 'register-tools hook through the extension registry
;; so that extension tools are available and extension state (event bus,
;; pinned dir) is initialized BEFORE the first run-prompt! call.
;;
;; Returns the list of extension-provided tools (as jsexpr hashes),
;; or '() if no extensions or no tools registered.
;;
;; Idempotent: extensions track their own registration state internally.
;; Safe to call multiple times.
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
