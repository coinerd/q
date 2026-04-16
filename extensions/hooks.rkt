#lang racket/base

;; extensions/hooks.rkt — hook dispatch and result normalization
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
         "api.rkt"
         "context.rkt"
         "../util/hook-types.rkt")

;; Re-export hook result types
(provide (all-from-out "../util/hook-types.rkt")
         ;; Re-export extension-ctx for convenience
         (all-from-out "context.rkt")
         ;; Hook dispatch
         dispatch-hooks
         current-hook-timeout-ms
         critical-hook?
         critical-hooks)

;; ============================================================
;; Hook criticality classification (#670, #671)
;; ============================================================

;; Critical hooks default to 'block on error (safety-first).
;; Advisory hooks default to 'pass on error (liveness-first).
(define critical-hooks '(tool-call session-before-fork session-before-compact input))

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
    (cond
      [(null? remaining) (hook-result (if amended? 'amend 'pass) current-payload)]
      [else
       (define ext-name (car (car remaining)))
       (define handler (cdr (car remaining)))
       (define result (run-hook-with-timeout ext-name hook-point handler current-payload ctx))
       (case (hook-result-action result)
         [(block) result]
         [(amend) (loop (cdr remaining) (hook-result-payload result) #t)]
         [(pass) (loop (cdr remaining) current-payload amended?)]
         [else (loop (cdr remaining) current-payload amended?)])])))

;; Per-hook timeout in milliseconds (default: 100ms for streaming hooks).
;; Set to #f to disable timeout.
(define current-hook-timeout-ms (make-parameter 100))

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
  (define error-default
    (if (critical-hook? hook-point)
        (hook-block (format "handler ~a failed for critical hook ~a" ext-name hook-point))
        (hook-pass payload)))
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning (format "Hook handler ~a for ~a threw: ~a [~a]"
                                                    ext-name
                                                    hook-point
                                                    (exn-message e)
                                                    (if (critical-hook? hook-point)
                                                        "CRITICAL->block"
                                                        "advisory->pass")))
                               error-default)])
    (define raw-result
      (if timeout-ms
          ;; Run with timeout
          (let ([chan (make-channel)])
            (define thd
              (thread (lambda ()
                        (channel-put chan
                                     (with-handlers ([exn:fail? (lambda (e) (cons 'error e))])
                                       (cons 'ok (call-handler handler payload ctx)))))))
            (define maybe (sync/timeout (/ timeout-ms 1000.0) chan))
            (unless maybe
              (kill-thread thd)) ; #447: prevent thread leak
            (cond
              [(not maybe)
               (log-warning
                (format "Hook handler ~a for ~a timed out after ~ams [~a]"
                        ext-name
                        hook-point
                        timeout-ms
                        (if (critical-hook? hook-point) "CRITICAL->block" "advisory->pass")))
               error-default]
              [(eq? (car maybe) 'error)
               (log-warning
                (format "Hook handler ~a for ~a threw: ~a [~a]"
                        ext-name
                        hook-point
                        (exn-message (cdr maybe))
                        (if (critical-hook? hook-point) "CRITICAL->block" "advisory->pass")))
               error-default]
              [else (cdr maybe)]))
          ;; No timeout — direct call
          (call-handler handler payload ctx)))
    (if (hook-result? raw-result)
        ;; FEAT-63: Validate action is valid for this hook-point
        (let ([valid? (validate-hook-result hook-point raw-result)])
          (if valid?
              raw-result
              ;; Invalid action: warn but still return the result (permissive)
              raw-result))
        (begin
          (log-warning (format "Hook handler ~a for ~a returned non-hook-result: ~v [~a]"
                               ext-name
                               hook-point
                               raw-result
                               (if (critical-hook? hook-point) "CRITICAL->block" "advisory->pass")))
          error-default))))
