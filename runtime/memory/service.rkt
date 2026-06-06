#lang racket/base
;; runtime/memory/service.rkt — Runtime-owned memory service boundary
;;
;; v0.95.13: Extracts current-memory-backend and current-memory-policy from
;; tools/builtins/memory-tools.rkt to fix the runtime→tools layering inversion.
;; v0.95.15: Added initialize-memory-backend! for session lifecycle integration.
;;
;; Rules:
;;   - tools/builtins/memory-tools.rkt imports from this module
;;   - runtime/context-assembly/memory-builder.rkt imports from this module
;;   - No runtime module imports q/tools/* for memory service
;;   - This module does NOT import concrete tool modules or UI modules

(require racket/dict
         racket/string
         "policy.rkt"
         "protocol.rkt"
         "backends/memory-hash.rkt"
         "backends/file-jsonl.rkt")

;; ---------------------------------------------------------------------------
;; Service parameters
;; ---------------------------------------------------------------------------

;; The active memory backend. #f when memory is disabled (default).
(define current-memory-backend (make-parameter #f))

;; The active memory policy. Defaults to the standard safe policy.
(define current-memory-policy (make-parameter default-memory-policy))

;; ---------------------------------------------------------------------------
;; Service helpers
;; ---------------------------------------------------------------------------

;; Check whether memory service is available (backend configured and not #f)
(define (memory-service-available?)
  (and (current-memory-backend) #t))

;; Resolve the effective backend, returning #f if unavailable
(define (resolve-memory-backend)
  (current-memory-backend))

;; ---------------------------------------------------------------------------
;; Session lifecycle integration
;; ---------------------------------------------------------------------------

;; Initialize memory backend from session config.
;; Called during session startup (make-agent-session, resume-agent-session).
;;
;; Config can specify:
;;   - #f / not set  → no backend (memory disabled)
;;   - 'hash         → in-memory hash backend (session-scoped)
;;   - 'file-jsonl   → file-based JSONL backend (persistent)
;;   - memory-backend? → pre-constructed backend (used as-is)
;;
;; When 'file-jsonl, uses the session directory for storage.
;; Returns the constructed backend (or #f).
(define (initialize-memory-backend! cfg)
  (define backend-spec
    (if (dict? cfg)
        (dict-ref cfg 'memory-backend #f)
        #f))
  (cond
    [(not backend-spec)
     (current-memory-backend #f)
     #f]
    [(memory-backend? backend-spec)
     (current-memory-backend backend-spec)
     backend-spec]
    [(eq? backend-spec 'hash)
     (define be (make-memory-hash-backend))
     (current-memory-backend be)
     be]
    [(eq? backend-spec 'file-jsonl)
     (define session-dir
       (if (dict? cfg)
           (dict-ref cfg 'session-dir #f)
           #f))
     (define memory-root
       (if session-dir
           (string->path session-dir)
           (find-system-path 'temp-dir)))
     (define be (make-file-jsonl-backend memory-root))
     (current-memory-backend be)
     be]
    [else
     ;; Unknown spec — leave disabled
     (current-memory-backend #f)
     #f]))

;; ---------------------------------------------------------------------------
;; Provide
;; ---------------------------------------------------------------------------

(provide current-memory-backend
         current-memory-policy
         memory-service-available?
         resolve-memory-backend
         initialize-memory-backend!)
