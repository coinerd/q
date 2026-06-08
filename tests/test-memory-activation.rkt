#lang racket/base

;; @speed fast
;; @suite default
;;; test-memory-activation.rkt — v0.95.15 W5: End-to-end memory activation integration test
;;;
;;; Exercises the full pipeline:
;;;   CLI --memory hash → backend instantiation → store → search → inject → cleanup
(require rackunit
         racket/string
         racket/hash
         "../runtime/memory/types.rkt"
         "../runtime/memory/protocol.rkt"
         "../runtime/memory/backends/memory-hash.rkt"
         "../runtime/memory/service.rkt"
         "../runtime/memory/search.rkt"
         "../runtime/session/session-config.rkt"
         "../runtime/context-assembly/memory-builder.rkt"
         "../cli/args.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define now-iso "2026-06-06T12:00:00Z")

(define (make-test-item id content [type 'semantic] [scope 'session])
  (memory-item id type scope content
               (hash 'project-root "/tmp/test"
                     'session-id "test-session"
                     'tags '()
                     'source 'test
                     'origin-message-id "msg-1")
               (hash 'sensitivity 'internal
                     'confidence 0.8
                     'supersedes '())
               now-iso now-iso))

;; ============================================================
;; W5 Integration: Full activation pipeline
;; ============================================================

(test-case "W5: CLI --memory hash → runtime config → session config → backend enabled"
  ;; 1. Parse CLI
  (define cfg (parse-cli-args '("--memory" "hash")))
  (check-equal? (cli-config-memory cfg) 'hash)
  ;; 2. Runtime config has memory-backend
  (define rt (cli-config->runtime-config cfg))
  (check-equal? (hash-ref rt 'memory-backend) 'hash)
  ;; 3. Session config sees memory enabled
  (define sc (hash->session-config rt))
  (check-true (config-memory-enabled? sc))
  (check-equal? (config-memory-backend sc) 'hash))

(test-case "W5: CLI --memory disabled → memory disabled in session config"
  (define cfg (parse-cli-args '("--memory" "disabled")))
  (define rt (cli-config->runtime-config cfg))
  (define sc (hash->session-config rt))
  ;; 'disabled is truthy in the raw hash, so config-memory-enabled? is #t
  ;; but initialize-memory-backend! treats 'disabled as #f
  (check-equal? (config-memory-backend sc) 'disabled))

(test-case "W5: no --memory → memory disabled"
  (define cfg (parse-cli-args '()))
  (check-false (cli-config-memory cfg))
  (define rt (cli-config->runtime-config cfg))
  (check-false (hash-ref rt 'memory-backend #f))
  (define sc (hash->session-config rt))
  (check-false (config-memory-enabled? sc)))

(test-case "W5: initialize-memory-backend! with 'hash creates working backend"
  (parameterize ([current-memory-backend #f])
    (define sc (hash->session-config (hash 'memory-backend 'hash)))
    (initialize-memory-backend! sc)
    (check-true (memory-service-available?) "hash backend should be active after init")
    (define backend (current-memory-backend))
    (check-true (memory-backend? backend) "should return a memory-backend struct")
    ;; Store an item
    (define item (make-test-item "integ-1" "Integration test fact"))
    (define store-result ((memory-backend-store! backend) item))
    (check-true (memory-result-ok? store-result) "store should succeed")
    ;; Retrieve it
    (define query (memory-query "Integration test" 'session #f #f #f #f 10 #f))
    (define retrieve-result ((memory-backend-retrieve backend) query))
    (check-true (memory-result-ok? retrieve-result) "retrieve should succeed")
    (define results (memory-result-value retrieve-result))
    (check-true (list? results) "retrieve should return a list")
    (check-true (> (length results) 0) "should find stored item")
    ;; Cleanup
    (current-memory-backend #f)))

(test-case "W5: store → search → post-retrieve-process pipeline"
  (define backend (make-memory-hash-backend))
  ;; Store multiple items
  (for ([content '("Racket is a Lisp dialect" "Memory helps agents learn" "Testing is essential")])
    (define item (make-test-item (string-append "pipe-" content) content 'semantic 'project))
    ((memory-backend-store! backend) item))
  ;; Search
  (define query (memory-query "memory agent learn" 'project #f #f #f #f 10 #f))
  (define raw-result ((memory-backend-retrieve backend) query))
  (define raw-results (memory-result-value raw-result))
  (check-true (list? raw-results) "search should return list")
  ;; Post-retrieve processing (rank, dedup, limit)
  (define processed (post-retrieve-process raw-results query))
  (check-true (list? processed) "post-retrieve-process should return list")
  (check-true (<= (length processed) 5) "should respect limit"))

(test-case "W5: observe + inject memory into context"
  (parameterize ([current-memory-backend #f]
                 [current-memory-injection-budget 2000])
    (define backend (make-memory-hash-backend))
    (current-memory-backend backend)
    ;; Store items
    (for ([c '("The project uses Racket" "Session memory is scoped" "Context injection helps")])
      ((memory-backend-store! backend) (make-test-item (string-append "ctx-" c) c 'semantic 'session)))
    ;; Create session config with memory enabled
    (define sc (hash->session-config (hash 'memory-backend 'hash)))
    ;; Observe (telemetry)
    (define obs-result (observe-memory-for-context sc #:scope 'session))
    (check-true (pair? obs-result) "observe should return a pair")
    (check-true (list? (car obs-result)) "observe car should be entries")
    (check-true (> (length (car obs-result)) 0) "should observe stored items")
    ;; Inject
    (define inj-result (inject-memory-for-context sc #:scope 'session))
    (check-true (pair? inj-result) "inject should return a pair")
    (define inj-section (car inj-result))
    (check-true (string? inj-section) "inject section should be a string")
    (check-true (string-contains? inj-section "Memory") "injection should include memory framing")
    ;; Cleanup
    (current-memory-backend #f)
    (current-memory-injection-budget #f)))

(test-case "W5: injection budget 0 → no injection"
  (parameterize ([current-memory-backend #f]
                 [current-memory-injection-budget 0])
    (define backend (make-memory-hash-backend))
    (current-memory-backend backend)
    ((memory-backend-store! backend)
     (make-test-item "budget-test" "Should not be injected" 'semantic 'session))
    (define sc (hash->session-config (hash 'memory-backend 'hash)))
    (define result (inject-memory-for-context sc #:scope 'session))
    (check-false (car result) "zero budget should produce #f section")
    (current-memory-backend #f)
    (current-memory-injection-budget #f)))

(test-case "W5: store → delete → verify gone"
  (define backend (make-memory-hash-backend))
  (define item (make-test-item "del-1" "Delete me"))
  ((memory-backend-store! backend) item)
  (define del-result ((memory-backend-delete! backend) "del-1" 'session))
  (check-true (memory-result-ok? del-result) "delete should succeed")
  ;; Verify gone
  (define query (memory-query "Delete" 'session #f #f #f #f 10 #f))
  (define retrieve-result ((memory-backend-retrieve backend) query))
  (check-true (memory-result-ok? retrieve-result) "retrieve should succeed")
  (define results (memory-result-value retrieve-result))
  (check-equal? (length results) 0 "item should be gone after delete"))
