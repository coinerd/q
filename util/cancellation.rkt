#lang racket

;; util/cancellation.rkt — cooperative cancellation token
;;
;; Shared module for cancellation signalling between SDK and runtime.
;; Both interfaces/sdk.rkt and runtime/agent-session.rkt import from here,
;; avoiding upward dependencies.
;;
;; Provides:
;;   cancellation-token         — struct type
;;   cancellation-token?        — predicate
;;   cancellation-token-cancelled? — check if cancelled
;;   make-cancellation-token    — constructor
;;   cancel-token!              — signal cancellation

(provide cancellation-token
         cancellation-token?
         cancellation-token-cancelled?
         make-cancellation-token
         cancel-token!)

;; ============================================================
;; Struct & constructor
;; ============================================================

(struct cancellation-token (cancelled-box callback-box)
  #:transparent
  #:constructor-name make-cancellation-token-internal)

;; make-cancellation-token : (#:callback (or/c (-> cancellation-token? any) #f)) -> cancellation-token?
(define (make-cancellation-token #:callback [cb #f])
  (make-cancellation-token-internal (box #f) (box cb)))

;; ============================================================
;; Operations
;; ============================================================

;; cancellation-token-cancelled? : cancellation-token? -> boolean?
(define (cancellation-token-cancelled? tok)
  (unbox (cancellation-token-cancelled-box tok)))

;; cancel-token! : cancellation-token? -> cancellation-token?
;; Sets the cancelled flag and fires the callback (if any).
;; Note: calling cancel-token! multiple times fires the callback each time.
;; This is documented behavior — callers should guard if idempotency is needed.
(define (cancel-token! tok)
  (set-box! (cancellation-token-cancelled-box tok) #t)
  (define cb (unbox (cancellation-token-callback-box tok)))
  (when cb (cb tok))
  tok)
