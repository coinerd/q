#lang racket/base

;; extensions/hooks.rkt — hook dispatch and result normalization
;; STABILITY: stable
;;
;; Provides:
;;   - hook-result struct with action ('pass | 'amend | 'block) and payload
;;   - Convenience constructors: hook-pass, hook-amend, hook-block
;;   - dispatch-hooks: runs handlers for a hook point over a payload
;;   - Optional #:ctx parameter for ctx-aware dispatch
;;
;; Dispatch semantics:
;;   - Handlers run in registration order (via handlers-for-point)
;;   - 'pass: continue with unchanged payload
;;   - 'amend: replace payload for next handler
;;   - 'block: stop dispatch immediately, return block result
;;   - When #:ctx is provided, handlers receive (ctx payload) if they accept 2 args
;;   - Backward compat: 1-arg handlers still receive just (payload)
;;
;; ARCH-01: hook-result struct and constructors moved to util/hook-types.rkt.
;; Re-exported here for backward compatibility.

(require racket/contract
         racket/list
         racket/match
         "api.rkt"
         "context.rkt"
         "combinators.rkt"
         "../util/hook-types.rkt")

;; Re-export hook result types
(provide (all-from-out "../util/hook-types.rkt")
         ;; Re-export extension-ctx for convenience
         (all-from-out "context.rkt")
         ;; Hook dispatch
         (contract-out
          [dispatch-hooks
           (->* (symbol? any/c extension-registry?) (#:ctx (or/c extension-ctx? #f)) hook-result?)])
         current-hook-timeout-ms
         critical-hook?
         critical-hooks
         ;; Hook block guard helper (RA-31)
         with-hook-block-guard)

;; ============================================================
;; Hook criticality classification (#670, #671)
;; ============================================================

;; Critical hooks default to 'block on error (safety-first).
;; Advisory hooks default to 'pass on error (liveness-first).
(define critical-hooks
  '(tool-call session-before-switch session-before-fork session-before-compact input))

(define (critical-hook? hook-point)
  (and (member hook-point critical-hooks) #t))

;; ============================================================
;; Dispatch hooks
;; ============================================================

;; dispatch-hooks : symbol? any/c extension-registry? #:ctx (or/c extension-ctx? #f) -> hook-result?
;; Runs all registered handlers for `hook-point` over `payload`.
;; Returns a hook-result reflecting the combined outcome.
;; When #:ctx is provided, handlers receive (ctx payload) instead of just (payload).
;; Backward compat: handlers that only accept 1 arg (payload) still work.
;; Per-hook timeout: if a handler exceeds current-hook-timeout-ms,
;; it is skipped and a warning is logged.
(define (dispatch-hooks hook-point payload registry #:ctx [ctx #f])
  (define handlers (handlers-for-point registry hook-point))
  (let loop ([remaining handlers]
             [current-payload payload]
             [amended? #f])
    (match remaining
      ['() (hook-result (if amended? 'amend 'pass) current-payload)]
      [(cons (cons ext-name handler) rest)
       (define result (run-hook-with-timeout ext-name hook-point handler current-payload ctx))
       (match (hook-result-action result)
         ['block result]
         ['amend (loop rest (hook-result-payload result) #t)]
         ['pass (loop rest current-payload amended?)]
         [_ (loop rest current-payload amended?)])])))

;; Per-hook timeout in milliseconds (default: 500ms).
;; Set to #f to disable timeout.
(define current-hook-timeout-ms (make-parameter 500))

;; Internal: call handler with or without ctx based on arity.
;; If ctx is provided and handler accepts 2 args, call (handler ctx payload).
;; Otherwise call (handler payload) for backward compat.
(define (call-handler handler payload ctx)
  (if (and ctx (procedure-arity-includes? handler 2))
      (handler ctx payload)
      (handler payload)))

;; Internal: run a single hook handler with optional timeout.
(define (run-hook-with-timeout ext-name hook-point handler payload ctx)
  (define timeout-ms (current-hook-timeout-ms))
  ;; #671: Error default depends on hook criticality.
  (define error-default-thunk
    (lambda ()
      (if (critical-hook? hook-point)
          (hook-block (format "handler ~a failed for critical hook ~a" ext-name hook-point))
          (hook-pass payload))))
  ;; Compute raw result with timeout + call-handler
  (define raw-result
    (with-timeout
     (or timeout-ms 1000)
     (lambda () (call-handler handler payload ctx))
     #:on-timeout
     (lambda ()
       (log-debug "hook ~a/~a timed out after ~ams" ext-name hook-point (or timeout-ms 1000))
       (error-default-thunk))
     #:on-error (lambda (e)
                  (log-debug "hook ~a/~a error: ~a" ext-name hook-point (exn-message e))
                  (error-default-thunk))))
  ;; Validate hook result
  (with-hook-validation (if (symbol? ext-name) ext-name (string->symbol ext-name)) hook-point raw-result error-default-thunk))
;; ============================================================
;; with-hook-block-guard helper (RA-31)
;; ============================================================

;; with-hook-block-guard :
;;   extension-registry? symbol? any/c (-> any/c any) (-> any) -> any
;;
;; Dispatches a hook and branches on the result action.
;; If the hook returns 'block, calls on-block with the block payload.
;; Otherwise calls on-pass with no arguments.
;;
;; This extracts the repeated hook-dispatch/classify/match pattern
;; that appears in 3+ sites across the codebase.
(define (with-hook-block-guard registry hook-point payload on-block on-pass)
  (define result (dispatch-hooks hook-point payload registry))
  (match (hook-result-action result)
    ['block (on-block (hook-result-payload result))]
    [_ (on-pass)]))

;; v0.31.x milestone placeholder
