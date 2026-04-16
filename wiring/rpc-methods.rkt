#lang racket/base

;; wiring/rpc-methods.rkt — FEAT-75: Core RPC method registry
;;
;; Registers the standard set of RPC methods:
;;   abort, subscribe, session_info, compact, fork
;;
;; Each handler receives params (hash) and returns a result (hash).
;; Dependencies are injected via a deps hash for testability.

(require racket/contract
         "../interfaces/rpc-mode.rkt")

(provide (contract-out [make-core-rpc-handlers (-> hash? hash?)]))

;; Sentinel for missing deps
(define sentinel (gensym 'missing))

;; Default implementations
(define (default-cancel-token)
  (void))
(define (default-session-info)
  #f)
(define (default-compact)
  (hasheq))
(define (default-fork entry-id)
  (hasheq 'status "error" 'message "fork not available"))
(define (default-subscribe filter)
  0)

;; ============================================================
;; make-core-rpc-handlers : hash? -> (hash/c symbol? procedure?)
;;
;; deps keys (all optional — defaults provided):
;;   'cancel-token      — thunk to cancel the active token
;;   'session-info-fn   — thunk returning session info hash or #f
;;   'compact-fn        — thunk performing compaction, returns result hash
;;   'fork-fn           — (or/c string? #f) -> hash with 'newSessionId
;;   'subscribe-fn      — (or/c list? #f) -> subscription-id
;; ============================================================

(define (make-core-rpc-handlers deps)
  (define cancel-token
    (let ([v (hash-ref deps 'cancel-token sentinel)]) (if (eq? v sentinel) default-cancel-token v)))
  (define session-info-fn
    (let ([v (hash-ref deps 'session-info-fn sentinel)])
      (if (eq? v sentinel) default-session-info v)))
  (define compact-fn
    (let ([v (hash-ref deps 'compact-fn sentinel)]) (if (eq? v sentinel) default-compact v)))
  (define fork-fn (let ([v (hash-ref deps 'fork-fn sentinel)]) (if (eq? v sentinel) default-fork v)))
  (define subscribe-fn
    (let ([v (hash-ref deps 'subscribe-fn sentinel)]) (if (eq? v sentinel) default-subscribe v)))

  (make-hash ;; ---- Abort ----
             (list (cons 'abort
                         (lambda (params)
                           (cancel-token)
                           (hasheq 'status "aborted")))
                   ;; ---- Subscribe ----
                   (cons 'subscribe
                         (lambda (params)
                           (define filter (hash-ref params 'filter #f))
                           (define sub-id (subscribe-fn filter))
                           (hasheq 'subscriptionId sub-id)))
                   ;; ---- Session Info ----
                   (cons 'session_info
                         (lambda (params)
                           (define info (session-info-fn))
                           (if info
                               info
                               (hasheq 'status "error" 'message "no active session"))))
                   ;; ---- Compact ----
                   (cons 'compact
                         (lambda (params)
                           (define result (compact-fn))
                           result))
                   ;; ---- Fork ----
                   (cons 'fork
                         (lambda (params)
                           (define entry-id (hash-ref params 'entryId #f))
                           (fork-fn entry-id))))))
