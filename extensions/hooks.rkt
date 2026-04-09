#lang racket

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

(require racket/contract
         "api.rkt")

(provide
 (struct-out hook-result)
 hook-pass
 hook-amend
 hook-block
 dispatch-hooks)

;; ============================================================
;; Hook result struct
;; ============================================================

(struct hook-result (action payload) #:transparent)
;; action  : (or/c 'pass 'amend 'block)
;; payload : any/c

;; ============================================================
;; Convenience constructors
;; ============================================================

(define (hook-pass [payload #f])
  (hook-result 'pass payload))

(define (hook-amend payload)
  (hook-result 'amend payload))

(define (hook-block [reason #f])
  (hook-result 'block reason))

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
           (handler current-payload)))
       (case (hook-result-action result)
         [(block) result]
         [(amend) (loop (cdr remaining) (hook-result-payload result) #t)]
         [(pass)  (loop (cdr remaining) current-payload amended?)]
         [else    (loop (cdr remaining) current-payload amended?)])])))
