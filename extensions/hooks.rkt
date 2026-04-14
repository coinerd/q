#lang racket/base

;; extensions/hooks.rkt — hook dispatch and result normalization
;;
;; Provides:
;;   - hook-result struct with action ('pass | 'amend | 'block) and payload
;;   - Convenience constructors: hook-pass, hook-amend, hook-block
;;   - dispatch-hooks: runs handlers for a hook point over a payload
;;
;; Dispatch semantics:
;;   - Handlers run in registration order (via handlers-for-point)
;;   - 'pass: continue with unchanged payload
;;   - 'amend: replace payload for next handler
;;   - 'block: stop dispatch immediately, return block result
;;
;; ARCH-01: hook-result struct and constructors moved to util/hook-types.rkt.
;; Re-exported here for backward compatibility.

(require racket/contract
         "api.rkt"
         "../util/hook-types.rkt")

(provide
 ;; Re-export hook result types
 (all-from-out "../util/hook-types.rkt")
 ;; Hook dispatch
 dispatch-hooks
 current-hook-timeout-ms)

;; ============================================================
;; Dispatch hooks
;; ============================================================

;; dispatch-hooks : symbol? any/c extension-registry? -> hook-result?
;; Runs all registered handlers for `hook-point` over `payload`.
;; Returns a hook-result reflecting the combined outcome.
;; Per-hook timeout: if a handler exceeds current-hook-timeout-ms,
;; it is skipped and a warning is logged.
(define (dispatch-hooks hook-point payload registry)
  (define handlers (handlers-for-point registry hook-point))
  (let loop ([remaining handlers]
             [current-payload payload]
             [amended? #f])
    (cond
      [(null? remaining)
       (hook-result (if amended? 'amend 'pass) current-payload)]
      [else
       (define ext-name (car (car remaining)))
       (define handler  (cdr (car remaining)))
       (define result
         (run-hook-with-timeout ext-name hook-point handler current-payload))
       (case (hook-result-action result)
         [(block) result]
         [(amend) (loop (cdr remaining) (hook-result-payload result) #t)]
         [(pass)  (loop (cdr remaining) current-payload amended?)]
         [else    (loop (cdr remaining) current-payload amended?)])])))

;; Per-hook timeout in milliseconds (default: 100ms for streaming hooks).
;; Set to #f to disable timeout.
(define current-hook-timeout-ms (make-parameter 100))

;; Internal: run a single hook handler with optional timeout.
(define (run-hook-with-timeout ext-name hook-point handler payload)
  (define timeout-ms (current-hook-timeout-ms))
  (with-handlers ([exn:fail?
                    (lambda (e)
                      (log-warning
                       (format "Hook handler ~a for ~a threw: ~a"
                               ext-name hook-point (exn-message e)))
                      (hook-pass payload))])
    (define raw-result
      (if timeout-ms
          ;; Run with timeout
          (let ([chan (make-channel)])
            (thread
             (lambda ()
               (channel-put chan
                 (with-handlers ([exn:fail? (lambda (e) (cons 'error e))])
                   (cons 'ok (handler payload))))))
            (define maybe (sync/timeout (/ timeout-ms 1000.0) chan))
            (cond
              [(not maybe)
               (log-warning
                (format "Hook handler ~a for ~a timed out after ~ams"
                        ext-name hook-point timeout-ms))
               (hook-pass payload)]
              [(eq? (car maybe) 'error)
               (log-warning
                (format "Hook handler ~a for ~a threw: ~a"
                        ext-name hook-point (exn-message (cdr maybe))))
               (hook-pass payload)]
              [else (cdr maybe)]))
          ;; No timeout — direct call
          (handler payload)))
    (if (hook-result? raw-result)
        raw-result
        (begin
          (log-warning
           (format "Hook handler ~a for ~a returned non-hook-result: ~v"
                   ext-name hook-point raw-result))
          (hook-pass payload)))))
