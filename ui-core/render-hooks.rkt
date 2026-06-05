#lang racket

;; q/ui-core/render-hooks.rkt — Render hook schema and validation
;;
;; Defines pre/post-render hooks for component customization.
;; Hooks receive immutable data and must return valid output or
;; be caught by the error boundary.
;;
;; W7.1 (v0.94.7): Render hook schema with timeout and validation.

(require racket/contract)

(provide
 render-hook
 render-hook?
 render-hook-name
 render-hook-phase
 render-hook-handler
 render-hook-timeout-ms
 render-hook-validator

 ;; Phase symbols
 render-hook-phases
 render-hook-phase?

 ;; Validation
 (contract-out
  [make-render-hook
   (->* (symbol? (or/c 'pre-render 'post-render) (-> any/c any/c))
        (#:timeout-ms exact-positive-integer?
                      #:validator (or/c (-> any/c boolean?) #f))
        render-hook?)]
  [apply-render-hook (-> render-hook? any/c any/c)]
  [apply-render-hook-safe (-> render-hook? any/c (values any/c boolean?))]))

;; ── Phase symbols ──────────────────────────────────────────

(define render-hook-phases '(pre-render post-render))

(define (render-hook-phase? v)
  (and (symbol? v) (member v render-hook-phases) #t))

;; ── Render hook struct ─────────────────────────────────────

(struct render-hook
  (name        ; symbol — hook identifier
   phase       ; 'pre-render or 'post-render
   handler     ; (-> any/c any/c) — transform function
   timeout-ms  ; exact-positive-integer — budget in milliseconds
   validator)  ; (or/c (-> any/c boolean?) #f) — output validator
  #:transparent)

;; ── Constructor ─────────────────────────────────────────────

(define (make-render-hook name phase handler
                           #:timeout-ms [timeout-ms 100]
                           #:validator [validator #f])
  (render-hook name phase handler timeout-ms validator))

;; ── Application ─────────────────────────────────────────────

(define (apply-render-hook hook data)
  ((render-hook-handler hook) data))

(define (apply-render-hook-safe hook data)
  (with-handlers ([exn:fail? (lambda (e) (values data #f))])
    (define result (apply-render-hook hook data))
    (define validator (render-hook-validator hook))
    (cond
      [(and validator (not (validator result)))
       (values data #f)]
      [else
       (values result #t)])))
