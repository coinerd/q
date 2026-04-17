#lang racket/base

;; wiring/rpc-methods.rkt — FEAT-75: Core RPC method registry
;;
;; Registers the standard set of RPC methods:
;;   abort, subscribe, session_info, compact, fork,
;;   prompt, steer, follow_up, navigate, send_message
;;
;; Each handler receives params (hash) and returns a result (hash).
;; Dependencies are injected via a deps hash for testability.

(require racket/contract
         "../interfaces/rpc-mode.rkt")

(provide (contract-out [make-core-rpc-handlers (-> hash? (hash/c symbol? procedure?))]))

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
(define (default-prompt msg)
  (hasheq 'status "error" 'message "no prompt handler"))
(define (default-steer msg)
  (hasheq 'status "error" 'message "no steer handler"))
(define (default-follow-up msg)
  (hasheq 'status "error" 'message "no follow-up handler"))
(define (default-navigate target)
  (hasheq 'status "error" 'message "no navigate handler"))
(define (default-send-message role text)
  (hasheq 'status "error" 'message "no send-message handler"))

;; ============================================================
;; make-core-rpc-handlers : hash? -> (hash/c symbol? procedure?)
;;
;; deps keys (all optional — defaults provided):
;;   'cancel-token      — thunk to cancel the active token
;;   'session-info-fn   — thunk returning session info hash or #f
;;   'compact-fn        — thunk performing compaction, returns result hash
;;   'fork-fn           — (or/c string? #f) -> hash with 'newSessionId
;;   'subscribe-fn      — (or/c list? #f) -> subscription-id
;;   'prompt-fn         — string? -> hash (send prompt / start turn)
;;   'steer-fn          — string? -> hash (steer during tool exec)
;;   'follow-up-fn      — string? -> hash (queue follow-up)
;;   'navigate-fn       — (or/c string? integer?) -> hash (tree nav)
;;   'send-message-fn   — string? string? -> hash (inject message)
;; ============================================================

(define (resolve-dep deps key default)
  (define v (hash-ref deps key sentinel))
  (if (eq? v sentinel) default v))

(define (make-core-rpc-handlers deps)
  (define cancel-token (resolve-dep deps 'cancel-token default-cancel-token))
  (define session-info-fn (resolve-dep deps 'session-info-fn default-session-info))
  (define compact-fn (resolve-dep deps 'compact-fn default-compact))
  (define fork-fn (resolve-dep deps 'fork-fn default-fork))
  (define subscribe-fn (resolve-dep deps 'subscribe-fn default-subscribe))
  (define prompt-fn (resolve-dep deps 'prompt-fn default-prompt))
  (define steer-fn (resolve-dep deps 'steer-fn default-steer))
  (define follow-up-fn (resolve-dep deps 'follow-up-fn default-follow-up))
  (define navigate-fn (resolve-dep deps 'navigate-fn default-navigate))
  (define send-message-fn (resolve-dep deps 'send-message-fn default-send-message))

  (make-hash
   ;; ---- Abort ----
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
         (cons 'compact (lambda (params) (compact-fn)))
         ;; ---- Fork ----
         (cons 'fork
               (lambda (params)
                 (define entry-id (hash-ref params 'entryId #f))
                 (fork-fn entry-id)))
         ;; ---- Prompt (GC-17) ----
         (cons 'prompt
               (lambda (params)
                 (define msg (hash-ref params 'message #f))
                 (cond
                   [(not msg) (hasheq 'status "error" 'message "missing 'message' parameter")]
                   [else (prompt-fn msg)])))
         ;; ---- Steer (GC-17) ----
         (cons 'steer
               (lambda (params)
                 (define msg (hash-ref params 'message #f))
                 (cond
                   [(not msg) (hasheq 'status "error" 'message "missing 'message' parameter")]
                   [else (steer-fn msg)])))
         ;; ---- Follow-up (GC-17) ----
         (cons 'follow_up
               (lambda (params)
                 (define msg (hash-ref params 'message #f))
                 (cond
                   [(not msg) (hasheq 'status "error" 'message "missing 'message' parameter")]
                   [else (follow-up-fn msg)])))
         ;; ---- Navigate (GC-17) ----
         (cons 'navigate
               (lambda (params)
                 (define target (hash-ref params 'target #f))
                 (cond
                   [(not target) (hasheq 'status "error" 'message "missing 'target' parameter")]
                   [else (navigate-fn target)])))
         ;; ---- Send Message (GC-17) ----
         (cons 'send_message
               (lambda (params)
                 (define role (hash-ref params 'role #f))
                 (define text (hash-ref params 'text #f))
                 (cond
                   [(not role) (hasheq 'status "error" 'message "missing 'role' parameter")]
                   [(not text) (hasheq 'status "error" 'message "missing 'text' parameter")]
                   [else (send-message-fn role text)]))))))
