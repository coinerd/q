#lang racket/base

;; llm/request-policy.rkt — the single owner of provider-request
;; lifecycle policy (v1.00.13, RL-1/RL-2/RL-9)
;;
;; Dependency direction (PLAN-v1.00.13 §10):
;;
;;     raw config → THIS MODULE → provider adapter / request lifecycle
;;                → stream/body mechanism → structured outcome → retry/TUI
;;
;; This module resolves raw, semantically untyped timeout configuration into
;; one `request-network-policy` value with named, invariant-checked fields.
;; Provider adapters and transport code consume the resolved policy; they must
;; never reinterpret raw timeout config (AC-1) or author generic phase
;; constants (AC-2).
;;
;; Field semantics (frozen by tests/test-request-network-policy.rkt):
;;
;;   request-budget-secs      per-model `request` meaning, unchanged
;;   connect-ttfb-secs        bounded connect+TLS+status+headers window,
;;                            (min request 120) — never the broad budget (RL-4)
;;   initial-idle-secs        dead-peer detection before any byte:
;;                            (min request 120), never widened by model config
;;   thinking-idle-secs       silent reasoning window:
;;                            (min request (min (or thinking-override 120) 300))
;;   content-idle-secs        per-chunk gap once content flows: 60, fixed
;;   stream-total-secs        total wall-clock budget (NOT a liveness
;;                            detector): (max 600 (* 2 request)) — frozen from
;;                            the intentional openai-compatible formula
;;                            (v0.45.12 L1); see .planning W0 ownership map
;;   body-read-budget-secs    eager/non-streaming full-body read:
;;                            explicit body-read > legacy sse-read > fallback
;;
;; Legacy `sse-read` compatibility (deprecated in v1.00.13): it feeds ONLY
;; thinking-idle and body-read — it can never influence connect/ttfb, initial,
;; or content. Explicit semantic keys always win over the legacy alias.
;;
;; This module is pure: no I/O, no network. Raw config arrives through the
;; parameters below (set by wiring/mode-helpers.rkt) or as explicit resolver
;; arguments.

(require racket/contract)

;; ============================================================
;; Policy constants (single owner — AC-2)
;; ============================================================

;; Default HTTP read timeout in seconds (also the body-read fallback).
(define http-read-timeout-default 120)

;; Default overall HTTP request timeout in seconds.
;; Covers connection + full response reading. Settable via settings.
(define http-request-timeout-default 600)

;; Per-chunk content-gap window; overrides must not widen it.
(define http-stream-timeout-default 60)

;; Hard ceiling on the mid-stream thinking gap regardless of configuration.
;; Preserves slow-reasoning models (kimi/glm 300 s) while capping runaway
;; overrides such as deepseek's legacy sse-read of 600 s (v1.00.12 SS-2/SS-3).
(define max-thinking-gap-secs 300)

;; v1.00.13 (RL-4): dedicated connect+TLS+status+headers bound. An
;; established-but-silent peer must not consume the broad request budget.
(define connect-ttfb-cap-secs 120)

;; Dead-peer detection bound before the first byte arrives.
(define initial-idle-cap-secs 120)

;; Default thinking window when no override is configured.
(define thinking-idle-default-secs 120)

;; Total wall-clock stream budget floor (v0.45.12 L1 semantics).
(define stream-total-floor-secs 600)

;; ============================================================
;; Raw config parameters (moved here from llm/stream.rkt in v1.00.13 W1;
;; stream.rkt re-provides them for compatibility)
;; ============================================================

;; Parameter: overall HTTP request timeout for the current session.
(define current-http-request-timeout (make-parameter http-request-timeout-default))

;; Parameter: per-model request timeouts (model-name → seconds).
(define current-model-timeouts (make-parameter (hash)))

;; Parameter: per-model legacy `sse-read` overrides (model-name → seconds).
;; DEPRECATED in v1.00.13: maps only to thinking-idle/body-read. Removal is
;; not part of this milestone.
(define current-model-sse-read-timeouts (make-parameter (hash)))

;; Parameter: per-model `thinking-idle` overrides (model-name → seconds),
;; from `timeouts.models.<model>.thinking-idle`. Explicit semantic key; wins
;; over legacy `sse-read`.
(define current-model-thinking-idle-timeouts (make-parameter (hash)))

;; Parameter: per-model `body-read` overrides (model-name → seconds), from
;; `timeouts.models.<model>.body-read`. Explicit semantic key; wins over
;; legacy `sse-read`.
(define current-model-body-read-timeouts (make-parameter (hash)))

;; ============================================================
;; Raw config accessors (the only code allowed to read these parameters
;; for policy purposes — AC-1; adapters receive resolved values only)
;; ============================================================

;; Get the effective request timeout for a specific model.
;; Checks per-model overrides first, then falls back to
;; current-http-request-timeout.
(define (effective-request-timeout-for model-name)
  (define overrides (current-model-timeouts))
  (define model-timeout (and (hash? overrides) model-name (hash-ref overrides model-name #f)))
  (or model-timeout (current-http-request-timeout)))

;; Get the effective legacy SSE-read timeout for a specific model, or #f when
;; the model has no per-model sse-read override. DEPRECATED in v1.00.13.
(define (effective-sse-read-timeout-for model-name)
  (define overrides (current-model-sse-read-timeouts))
  (and (hash? overrides) model-name (hash-ref overrides model-name #f)))

;; Get the explicit thinking-idle override for a model, or #f.
(define (effective-thinking-idle-timeout-for model-name)
  (define overrides (current-model-thinking-idle-timeouts))
  (and (hash? overrides) model-name (hash-ref overrides model-name #f)))

;; Get the explicit body-read override for a model, or #f.
(define (effective-body-read-timeout-for model-name)
  (define overrides (current-model-body-read-timeouts))
  (and (hash? overrides) model-name (hash-ref overrides model-name #f)))

;; ============================================================
;; Resolved policy value
;; ============================================================

;; One resolved request-network policy. Field semantics: see module header.
(struct request-network-policy
  (request-budget-secs
   connect-ttfb-secs
   initial-idle-secs
   thinking-idle-secs
   content-idle-secs
   stream-total-secs
   body-read-budget-secs)
  #:transparent)

(define (positive-duration? v) (and (real? v) (positive? v)))

;; Fail configuration early: zero/negative durations must never reach
;; transport code (PLAN-v1.00.13 §3.2).
(define (check-duration! label v)
  (unless (positive-duration? v)
    (raise (exn:fail:contract
            (format "request-policy: ~a must be a positive number of seconds, got ~s" label v)
            (current-continuation-marks)))))

;; Resolve one request-network policy from explicit raw inputs.
;;
;;   #:request-timeout        per-model request budget (required)
;;   #:sse-read-override      legacy `sse-read` value or #f (deprecated)
;;   #:thinking-idle-override explicit `thinking-idle` value or #f
;;   #:body-read-override     explicit `body-read` value or #f
;;   #:body-read-fallback     fallback body budget (default 120)
;;
;; Precedence (frozen in tests/test-request-network-policy.rkt):
;;   thinking-idle: explicit > legacy sse-read > 120, capped at 300 and by
;;                  the request budget
;;   body-read:     explicit > legacy sse-read > fallback
;; Legacy sse-read never influences connect/ttfb, initial, or content.
(define (resolve-request-network-policy
         #:request-timeout request-timeout
         #:sse-read-override [sse-read-override #f]
         #:thinking-idle-override [thinking-idle-override #f]
         #:body-read-override [body-read-override #f]
         #:body-read-fallback [body-read-fallback http-read-timeout-default])
  (check-duration! 'request-timeout request-timeout)
  (when sse-read-override (check-duration! 'sse-read-override sse-read-override))
  (when thinking-idle-override (check-duration! 'thinking-idle-override thinking-idle-override))
  (when body-read-override (check-duration! 'body-read-override body-read-override))
  (check-duration! 'body-read-fallback body-read-fallback)
  (define connect-ttfb (min request-timeout connect-ttfb-cap-secs))
  (define initial-idle (min request-timeout initial-idle-cap-secs))
  (define thinking-idle
    (min request-timeout
         (min (or thinking-idle-override sse-read-override thinking-idle-default-secs)
              max-thinking-gap-secs)))
  (define content-idle http-stream-timeout-default)
  (define stream-total (max stream-total-floor-secs (* 2 request-timeout)))
  (define body-read (or body-read-override sse-read-override body-read-fallback))
  (for ([(label v) (in-hash (hash 'connect-ttfb-secs connect-ttfb
                                  'initial-idle-secs initial-idle
                                  'thinking-idle-secs thinking-idle
                                  'content-idle-secs content-idle
                                  'stream-total-secs stream-total
                                  'body-read-budget-secs body-read))])
    (check-duration! label v))
  (request-network-policy request-timeout
                          connect-ttfb
                          initial-idle
                          thinking-idle
                          content-idle
                          stream-total
                          body-read))

;; Resolve the policy for a model from the wired configuration parameters.
;; This is the entry point provider request construction must use (W2):
;; adapters receive the resolved value, never raw config.
(define (resolve-request-network-policy-for-model model-name)
  (resolve-request-network-policy
   #:request-timeout (effective-request-timeout-for model-name)
   #:sse-read-override (effective-sse-read-timeout-for model-name)
   #:thinking-idle-override (effective-thinking-idle-timeout-for model-name)
   #:body-read-override (effective-body-read-timeout-for model-name)))

;; ============================================================
;; v1.00.12 compatibility surface
;; ============================================================

;; Resolve the three SSE stall windows for one streaming request.
;; Moved from llm/stream.rkt in v1.00.13 W1; the semantics are unchanged
;; (regression matrix: tests/test-sse-phase-timeout-bounds.rkt). New code
;; must consume `resolve-request-network-policy` instead.
;;
;; Returns (values initial thinking content).
(define (sse-phase-timeout-secs #:request-timeout request-timeout
                                #:sse-read-override [sse-read-override #f])
  (define policy (resolve-request-network-policy
                  #:request-timeout request-timeout
                  #:sse-read-override sse-read-override))
  (values (request-network-policy-initial-idle-secs policy)
          (request-network-policy-thinking-idle-secs policy)
          (request-network-policy-content-idle-secs policy)))

(provide (contract-out
          [resolve-request-network-policy
           (->* (#:request-timeout positive?)
                (#:sse-read-override (or/c positive? #f)
                 #:thinking-idle-override (or/c positive? #f)
                 #:body-read-override (or/c positive? #f)
                 #:body-read-fallback positive?)
                request-network-policy?)]
          [resolve-request-network-policy-for-model
           (-> (or/c string? #f) request-network-policy?)]
          [sse-phase-timeout-secs
           (->* (#:request-timeout positive?)
                (#:sse-read-override (or/c positive? #f))
                (values positive? positive? positive?))])
         request-network-policy
         request-network-policy?
         request-network-policy-request-budget-secs
         request-network-policy-connect-ttfb-secs
         request-network-policy-initial-idle-secs
         request-network-policy-thinking-idle-secs
         request-network-policy-content-idle-secs
         request-network-policy-stream-total-secs
         request-network-policy-body-read-budget-secs
         ;; Policy constants (single owner)
         http-read-timeout-default
         http-request-timeout-default
         http-stream-timeout-default
         max-thinking-gap-secs
         connect-ttfb-cap-secs
         initial-idle-cap-secs
         thinking-idle-default-secs
         stream-total-floor-secs
         ;; Raw config parameters (moved here from llm/stream.rkt)
         current-http-request-timeout
         current-model-timeouts
         current-model-sse-read-timeouts
         current-model-thinking-idle-timeouts
         current-model-body-read-timeouts
         ;; Raw config accessors (compatibility home)
         effective-request-timeout-for
         effective-sse-read-timeout-for
         effective-thinking-idle-timeout-for
         effective-body-read-timeout-for)
