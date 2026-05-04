#lang racket/base

;; tools/middleware.rkt — Composable tool middleware pipeline (v0.29.6 W1)
;; STABILITY: evolving
;;
;; Provides a HOF-based middleware pattern for tool execution.
;; Each middleware wraps the next executor, enabling pre-check,
;; post-handle, and short-circuit semantics.
;;
;; Usage:
;;   (define pipeline
;;     (compose-middleware
;;       (make-logging-middleware)
;;       (make-validation-middleware validate-fn)
;;       (make-block-middleware block-fn)))
;;   (pipeline tool-call exec-ctx base-executor)

(require (only-in "../util/protocol-types.rkt" tool-call? tool-call-name tool-call-arguments))

;; ============================================================
;; Middleware type
;; ============================================================

;; A tool middleware is a procedure: (tool-call exec-ctx next) → result
;; where next is (tool-call exec-ctx) → result
;;
;; Pre-check: inspect tool-call before calling next
;; Post-handle: transform result after calling next
;; Short-circuit: return error result without calling next

;; ============================================================
;; compose-middleware
;; ============================================================

;; Compose multiple middleware into a single pipeline.
;; The rightmost middleware is closest to the base executor.
;; Execution order: first middleware's pre-check runs first,
;; its post-handle runs last (onion model).
(define (compose-middleware . middlewares)
  (lambda (tool-call exec-ctx base-executor)
    (define pipeline
      (foldr (lambda (mw next-executor) (lambda (tc ctx) (mw tc ctx next-executor)))
             base-executor
             middlewares))
    (pipeline tool-call exec-ctx)))

;; ============================================================
;; Built-in middleware factories
;; ============================================================

;; Middleware that dispatches a hook before execution.
;; If the hook returns 'block, short-circuits with an error result.
;; If the hook returns 'amend with payload, uses the amended tool-call.
(define (make-hook-middleware hook-dispatcher phase)
  (lambda (tc exec-ctx next)
    (define hook-result
      (and hook-dispatcher
           (with-handlers
               ([exn:fail?
                 (lambda (e)
                   (hasheq 'status 'error 'error-message (format "hook error: ~a" (exn-message e))))])
             (hook-dispatcher phase tc))))
    (cond
      [(and (hash? hook-result) (eq? (hash-ref hook-result 'status #f) 'error)) hook-result]
      [(and (hash? hook-result) (eq? (hash-ref hook-result 'action #f) 'block))
       (hasheq 'status
               'blocked
               'error-message
               (format "tool call '~a' blocked by ~a hook" (tool-call-name tc) phase))]
      [(and (hash? hook-result) (eq? (hash-ref hook-result 'action #f) 'amend))
       (next (hash-ref hook-result 'payload tc) exec-ctx)]
      [else (next tc exec-ctx)])))

;; Middleware that checks safe-mode tool restrictions.
(define (make-safe-mode-middleware safe-mode? allowed-tool?)
  (lambda (tc exec-ctx next)
    (define name (tool-call-name tc))
    (cond
      [(and (safe-mode?) (not (allowed-tool? name)))
       (hasheq 'status 'blocked 'error-message (format "tool '~a' blocked by safe-mode" name))]
      [else (next tc exec-ctx)])))

;; Middleware that validates tool arguments against a schema.
(define (make-validation-middleware validate-fn lookup-fn)
  (lambda (tc exec-ctx next)
    (define t (lookup-fn (tool-call-name tc)))
    (cond
      [(not t)
       (hasheq 'status 'error 'error-message (format "unknown tool: '~a'" (tool-call-name tc)))]
      [else
       (define validation-result
         (with-handlers ([exn:fail? (lambda (e) e)])
           (validate-fn t (tool-call-arguments tc))
           #f))
       (if (not validation-result)
           (next tc exec-ctx)
           (hasheq 'status 'error 'error-message (format "~a" (exn-message validation-result))))])))

;; Middleware that checks a permission gate.
(define (make-permission-middleware permission-check)
  (lambda (tc exec-ctx next)
    (define check-result (permission-check tc))
    (cond
      [(eq? check-result 'allowed) (next tc exec-ctx)]
      [(eq? check-result 'blocked)
       (hasheq 'status
               'blocked
               'error-message
               (format "tool '~a' blocked by permission gate" (tool-call-name tc)))]
      [else (next tc exec-ctx)])))

;; Middleware that queues mutations for sequential execution.
(define (make-mutation-queue-middleware is-mutation? enqueue!)
  (lambda (tc exec-ctx next)
    (cond
      [(is-mutation? (tool-call-name tc)) (enqueue! tc exec-ctx next)]
      [else (next tc exec-ctx)])))

;; ============================================================
;; Default pipeline constructor
;; ============================================================

;; Build a standard tool execution pipeline with all built-in middleware.
;; Returns a procedure: (tool-call exec-ctx base-executor) → result
(define (make-default-pipeline #:hook-dispatcher [hook-dispatcher #f]
                               #:safe-mode? [safe-mode? (lambda () #f)]
                               #:allowed-tool? [allowed-tool? (lambda (_) #t)]
                               #:validate-fn [validate-fn (lambda (t args) (void))]
                               #:lookup-fn [lookup-fn (lambda (_) #f)]
                               #:permission-check [permission-check (lambda (_) 'allowed)]
                               #:is-mutation? [is-mutation? (lambda (_) #f)]
                               #:enqueue! [enqueue! (lambda (tc ctx next) (next tc ctx))])
  (compose-middleware (make-hook-middleware hook-dispatcher 'preflight)
                      (make-safe-mode-middleware safe-mode? allowed-tool?)
                      (make-validation-middleware validate-fn lookup-fn)
                      (make-permission-middleware permission-check)
                      (make-mutation-queue-middleware is-mutation? enqueue!)))

;; ============================================================
;; Provide
;; ============================================================

(provide compose-middleware
         make-hook-middleware
         make-safe-mode-middleware
         make-validation-middleware
         make-permission-middleware
         make-mutation-queue-middleware
         make-default-pipeline)
