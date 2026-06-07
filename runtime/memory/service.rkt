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
         "backends/file-jsonl.rkt"
         "backends/chained.rkt"
         "backends/mem0-api.rkt")

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
;;   - hash spec     → complex backend (v0.95.16 W4):
;;       {type: 'chained, l1: <spec>, l2: <spec>, write-through?: <bool>}
;;       {type: 'external, ...} (handled by external factory, W5)
;;
;; When 'file-jsonl, uses the session directory for storage.
;; Returns the constructed backend (or #f).
(define (initialize-memory-backend! cfg)
  (define backend-spec
    (if (dict? cfg)
        (dict-ref cfg 'memory-backend #f)
        #f))
  (define result (build-backend-from-spec backend-spec cfg))
  (current-memory-backend result)
  result)

;; v0.95.16 W4: Recursive backend factory.
;; Converts a backend spec into a memory-backend? or #f.
;; Unknown/invalid specs fail closed (return #f).
(define (build-backend-from-spec spec cfg)
  (cond
    [(not spec) #f]
    [(memory-backend? spec) spec]
    [(eq? spec 'hash) (make-memory-hash-backend)]
    [(eq? spec 'file-jsonl)
     (define session-dir
       (if (dict? cfg)
           (dict-ref cfg 'session-dir #f)
           #f))
     (define memory-root
       (if session-dir
           (string->path session-dir)
           (find-system-path 'temp-dir)))
     (make-file-jsonl-backend memory-root)]
    [(hash? spec)
     (define spec-type (hash-ref spec 'type #f))
     (cond
       [(eq? spec-type 'chained) (build-chained-from-spec spec cfg)]
       [(eq? spec-type 'external) (build-external-from-spec spec cfg)]
       [else
        (log-warning (format "memory: unknown backend spec type: ~a" spec-type))
        #f])]
    [else
     (log-warning (format "memory: invalid backend spec: ~a" spec))
     #f]))

;; Build a chained backend from spec
(define (build-chained-from-spec spec cfg)
  (define l1-spec (hash-ref spec 'l1 #f))
  (define l2-spec (hash-ref spec 'l2 #f))
  (define write-through? (hash-ref spec 'write-through? #t))
  (cond
    [(not l1-spec)
     (log-warning "memory: chained backend missing l1 spec")
     #f]
    [(not l2-spec)
     (log-warning "memory: chained backend missing l2 spec")
     #f]
    [else
     (define l1 (build-backend-from-spec l1-spec cfg))
     (define l2 (build-backend-from-spec l2-spec cfg))
     (cond
       [(not l1)
        (log-warning "memory: chained backend l1 construction failed")
        #f]
       [(not l2)
        (log-warning "memory: chained backend l2 construction failed")
        #f]
       [else (make-chained-backend l1 l2 #:write-through? write-through?)])]))

;; Build an external backend from spec (v0.95.16 W5)
;; Spec shape: {type: 'external, provider: 'mem0, base-url: ..., api-key-env: ..., timeout-ms: ...}
(define (build-external-from-spec spec cfg)
  (define provider (hash-ref spec 'provider #f))
  (define base-url (hash-ref spec 'base-url "https://api.mem0.ai"))
  (define api-key-env (hash-ref spec 'api-key-env #f))
  (define timeout-ms (hash-ref spec 'timeout-ms 5000))
  (cond
    [(not provider)
     (log-warning "memory: external backend missing provider")
     #f]
    [(eq? provider 'mem0)
     (make-mem0-backend #:base-url base-url #:api-key-env api-key-env #:timeout-ms timeout-ms)]
    [else
     (log-warning (format "memory: unknown external provider: ~a" provider))
     #f]))

;; ---------------------------------------------------------------------------
;; Provide
;; ---------------------------------------------------------------------------

(provide current-memory-backend
         current-memory-policy
         memory-service-available?
         resolve-memory-backend
         initialize-memory-backend!
         build-backend-from-spec)
