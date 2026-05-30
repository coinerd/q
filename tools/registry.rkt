#lang racket/base
;; tools/registry.rkt — Thread-safe tool registry
;; Extracted from tools/tool.rkt (v0.30.8 W0)
;; STABILITY: stable


(define-logger q-tool-registry)
(require racket/contract
         racket/set
         (only-in racket/base make-semaphore call-with-semaphore)
         (only-in "tool-struct.rkt"
                  tool?
                  tool-name
                  tool-description
                  tool-schema
                  tool-prompt-snippet
                  tool-prompt-guidelines
                  tool-timeout-seconds))

(provide (contract-out [make-tool-registry (-> tool-registry?)]
                       [tool-registry-tools (-> tool-registry? (listof tool?))]
                       [register-tool! (-> tool-registry? tool? void?)]
                       [unregister-tool! (-> tool-registry? string? void?)]
                       [lookup-tool (-> tool-registry? (or/c string? #f) (or/c tool? #f))]
                       [list-tools (-> tool-registry? (listof tool?))]
                       [tool-names (-> tool-registry? (listof string?))]
                       [tool->jsexpr (-> tool? hash?)]
                       [list-active-tools (-> tool-registry? (listof tool?))]
                       [list-active-tools-jsexpr (-> tool-registry? (listof hash?))]
                       [list-tools-jsexpr (-> tool-registry? (listof hash?))]
                       [set-active-tools! (-> tool-registry? (or/c (listof string?) #f) void?)]
                       [tool-active? (-> tool-registry? string? boolean?)]
                       [with-registry-snapshot (-> tool-registry? procedure? any)])
         tool-registry?)

;; ============================================================
;; Tool registry
;; ============================================================

;; Import struct from util/ to break runtime→tools reverse dependency (H1).
(require "../util/tool-registry-struct.rkt"
         (submod "../util/tool-registry-struct.rkt" internal))
(provide tool-registry?)

;; Safe read-only accessor under lock
(define (tool-registry-tools reg)
  (with-registry-lock reg (lambda () (hash-values (tool-registry-tools-box reg)))))
(define (make-tool-registry)
  (make-tool-registry-internal (make-hash) (box #f) (make-semaphore 1)))

;; Thread-safe registry lock helper (Finding A2: 7 call sites)
;; L-10 WARNING: Racket semaphores are NOT reentrant. Calling
;; with-registry-lock from inside another with-registry-lock on the
;; same registry WILL deadlock. Do not nest lock acquisition.
(define (with-registry-lock reg thunk)
  (call-with-semaphore (tool-registry-sem reg) thunk))

;; F12: Snapshot-based read access — no lock held during thunk execution.
;; Takes a shallow copy of the tools hash while locked, then calls thunk
;; with the snapshot. Prevents callers from accidentally mutating the registry.
(define (with-registry-snapshot reg thunk)
  (define snapshot (with-registry-lock reg (lambda () (hash-copy (tool-registry-tools-box reg)))))
  (thunk snapshot))

(define (register-tool! reg t)
  (unless (tool? t)
    (raise-argument-error 'register-tool! "tool?" t))
  (with-registry-lock reg
                      (lambda ()
                        (define tbl (tool-registry-tools-box reg))
                        (hash-set! tbl (tool-name t) t)
                        (log-q-tool-registry-info "registered tool: ~a" (tool-name t)))))

(define (unregister-tool! reg name)
  (with-registry-lock reg
                      (lambda ()
                        (hash-remove! (tool-registry-tools-box reg) name)
                        (log-q-tool-registry-info "unregistered tool: ~a" name))))

(define (lookup-tool reg name)
  (if (not name)
      #f
      (with-registry-lock reg (lambda () (hash-ref (tool-registry-tools-box reg) name #f)))))

;; tool->jsexpr : tool? -> hash?
;; Returns a JSON-serializable hash representation of a tool.
;; Safe for external consumers (SDK, JSON mode, extensions).
;; Serialize a tool struct to the OpenAI normalized format.
(define (tool->jsexpr t)
  (define base-fn
    (hasheq 'name (tool-name t) 'description (tool-description t) 'parameters (tool-schema t)))
  (define fn-with-snippet
    (if (tool-prompt-snippet t)
        (hash-set base-fn 'promptSnippet (tool-prompt-snippet t))
        base-fn))
  (define fn-with-guidelines
    (if (tool-prompt-guidelines t)
        (hash-set fn-with-snippet 'promptGuidelines (tool-prompt-guidelines t))
        fn-with-snippet))
  (define fn-with-timeout
    (if (tool-timeout-seconds t)
        (hash-set fn-with-guidelines 'timeoutSeconds (tool-timeout-seconds t))
        fn-with-guidelines))
  (hasheq 'type "function" 'function fn-with-timeout))

;; ── Active tool management ──

(define (set-active-tools! reg active-names)
  (with-registry-lock reg
                      (lambda ()
                        (set-box! (tool-registry-active-set-box reg)
                                  (and active-names (list->set active-names))))))

(define (tool-active? reg name)
  (with-registry-lock reg
                      (lambda ()
                        (define active-set (unbox (tool-registry-active-set-box reg)))
                        (or (not active-set) (set-member? active-set name)))))

(define (list-active-tools reg)
  (with-registry-lock reg
                      (lambda ()
                        (define active-set (unbox (tool-registry-active-set-box reg)))
                        (filter (lambda (t)
                                  (or (not active-set) (set-member? active-set (tool-name t))))
                                (hash-values (tool-registry-tools-box reg))))))

;; R-12/R-24: list-active-tools-jsexpr — primary external API
;; Returns JSON-serializable list of active tools. Use this for
;; SDK consumers, JSON mode, and extension API surfaces.
;; tool-registry-tools returns internal tool? values for tool-layer use only.
(define (list-active-tools-jsexpr reg)
  (map tool->jsexpr (list-active-tools reg)))

(define (list-tools-jsexpr reg)
  (map tool->jsexpr (list-active-tools reg)))

(define (list-tools reg)
  (with-registry-lock reg (lambda () (hash-values (tool-registry-tools-box reg)))))

(define (tool-names reg)
  (with-registry-lock reg (lambda () (hash-keys (tool-registry-tools-box reg)))))
