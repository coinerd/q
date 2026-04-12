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
 (all-from-out "../util/hook-types.rkt")
 dispatch-hooks)

;; ============================================================
;; Dispatch hooks
;; ============================================================

;; dispatch-hooks : symbol? any/c extension-registry? -> hook-result?
;; Runs all registered handlers for `hook-point` over `payload`.
;; Returns a hook-result reflecting the combined outcome.
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
         (with-handlers ([exn:fail?
                          (lambda (e)
                            (log-warning
                             (format "Hook handler ~a for ~a threw: ~a"
                                     ext-name hook-point (exn-message e)))
                            (hook-pass current-payload))])
           (define raw-result (handler current-payload))
           (if (hook-result? raw-result)
               raw-result
               (begin
                 (log-warning
                  (format "Hook handler ~a for ~a returned non-hook-result: ~v"
                          ext-name hook-point raw-result))
                 (hook-pass current-payload)))))
       (case (hook-result-action result)
         [(block) result]
         [(amend) (loop (cdr remaining) (hook-result-payload result) #t)]
         [(pass)  (loop (cdr remaining) current-payload amended?)]
         [else    (loop (cdr remaining) current-payload amended?)])])))
