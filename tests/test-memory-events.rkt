#lang racket/base
;; tests/test-memory-events.rkt — SPEC memory event taxonomy and tool emission tests

(require rackunit
         racket/list
         "../tools/builtins/memory-tools.rkt"
         "../tools/tool.rkt"
         "../runtime/memory/types.rkt"
         "../runtime/memory/backends/memory-hash.rkt"
         "../runtime/memory/protocol.rkt"
         (only-in "../runtime/memory/policy.rkt" default-memory-policy)
         "../runtime/memory/service.rkt"
         "../agent/event-structs/memory-events.rkt"
         "../agent/event-structs/base.rkt"
         (only-in "../util/event/event-macro.rkt" lookup-event-schema-version))

(define (with-backend backend thunk)
  (parameterize ([current-memory-backend backend])
    (thunk)))

(define (collecting-context events)
  (make-exec-context #:working-directory "/tmp/q-memory-events"
                     #:session-metadata (hasheq 'session-id "sess-events")
                     #:event-publisher (lambda (evt) (set-box! events (cons evt (unbox events))))))

(define (event-types events)
  (map (lambda (evt)
         (cond
           [(typed-event? evt) (typed-event-type evt)]
           [(hash? evt) (hash-ref evt 'type #f)]
           [else #f]))
       (reverse (unbox events))))

(test-case "SPEC memory event type strings are canonical"
  (check-equal? mem-store-requested-event-type "memory.item.store.requested")
  (check-equal? mem-item-stored-event-type "memory.item.stored")
  (check-equal? mem-item-deleted-event-type "memory.item.deleted")
  (check-equal? mem-retrieval-performed-event-type "memory.retrieval.performed")
  (check-equal? mem-policy-blocked-event-type "memory.policy.blocked")
  (check-equal? mem-backend-unavailable-event-type "memory.backend.unavailable"))

(test-case "SPEC memory events construct and expose fields"
  (define requested
    (make-mem-store-requested-event #:session-id "s"
                                    #:turn-id "t"
                                    #:candidate-id "m"
                                    #:mem-type 'semantic
                                    #:scope 'project
                                    #:source 'tool))
  (check-equal? (typed-event-type requested) "memory.item.store.requested")
  (check-equal? (mem-store-requested-event-candidate-id requested) "m")
  (define stored
    (make-mem-item-stored-event #:session-id "s"
                                #:turn-id "t"
                                #:memory-id "m"
                                #:mem-type 'semantic
                                #:scope 'project
                                #:source 'tool
                                #:redacted-snippet "ok"))
  (check-equal? (typed-event-type stored) "memory.item.stored")
  (check-equal? (mem-item-stored-event-redacted-snippet stored) "ok")
  (define blocked
    (make-mem-policy-blocked-event #:session-id "s"
                                   #:turn-id "t"
                                   #:action 'store
                                   #:reason 'secret
                                   #:source 'tool
                                   #:redacted-snippet "[REDACTED]"))
  (check-equal? (typed-event-type blocked) "memory.policy.blocked")
  (check-equal? (mem-policy-blocked-event-redacted-snippet blocked) "[REDACTED]"))

(test-case "SPEC memory event fields are registered"
  (check-equal? mem-store-requested-event-fields '(candidate-id mem-type scope source))
  (check-equal? mem-item-stored-event-fields '(memory-id mem-type scope source redacted-snippet))
  (check-equal? mem-item-deleted-event-fields '(memory-id scope backend))
  (check-equal? mem-retrieval-performed-event-fields
                '(query-snippet result-count query-limit scope latency-ms))
  (check-equal? mem-policy-blocked-event-fields '(action reason source redacted-snippet))
  (check-equal? mem-backend-unavailable-event-fields '(backend action)))

(test-case "SPEC memory events have schema version 1"
  (for ([type (in-list '("memory.item.store.requested" "memory.item.stored"
                                                       "memory.item.deleted"
                                                       "memory.retrieval.performed"
                                                       "memory.policy.blocked"
                                                       "memory.backend.unavailable"))])
    (check-equal? (lookup-event-schema-version type) 1)))

(test-case "store_memory emits requested and stored audit events"
  (define b (make-memory-hash-backend))
  (define events (box '()))
  (define ctx (collecting-context events))
  (with-backend b
                (lambda ()
                  (define r (tool-store-memory (hash 'content "event fact") ctx))
                  (check-false (tool-result-is-error? r))
                  (check-equal? (event-types events)
                                '("memory.item.store.requested" "memory.item.stored")))))

(test-case "policy-blocked store emits redacted blocked event"
  (define b (make-memory-hash-backend))
  (define events (box '()))
  (define ctx (collecting-context events))
  (with-backend b
                (lambda ()
                  (define r (tool-store-memory (hash 'content "API_KEY=sk-super-secret") ctx))
                  (check-true (tool-result-is-error? r))
                  (define evts (reverse (unbox events)))
                  (check-not-false (member "memory.policy.blocked" (event-types events)))
                  (define blocked
                    (for/first ([evt (in-list evts)]
                                #:when (equal? (hash-ref evt 'type #f) "memory.policy.blocked"))
                      evt))
                  (check-false (regexp-match? #px"sk-super-secret"
                                              (hash-ref blocked 'redacted-snippet))))))

(test-case "backend unavailable emits unavailable audit event"
  (define events (box '()))
  (define ctx (collecting-context events))
  (parameterize ([current-memory-backend #f])
    (define r (tool-search-memory (hash 'query "x") ctx))
    (check-true (tool-result-is-error? r))
    (check-equal? (event-types events) '("memory.backend.unavailable"))))

(test-case "search_memory emits retrieval audit event"
  (define b (make-memory-hash-backend))
  (define events (box '()))
  (define ctx (collecting-context events))
  (with-backend b
                (lambda ()
                  (tool-store-memory (hash 'content "event retrieval") ctx)
                  (set-box! events '())
                  (define r (tool-search-memory (hash 'query "retrieval") ctx))
                  (check-false (tool-result-is-error? r))
                  (check-equal? (event-types events) '("memory.retrieval.performed")))))

(test-case "delete_memory emits deleted audit event"
  (define b (make-memory-hash-backend))
  (define events (box '()))
  (define ctx (collecting-context events))
  (with-backend b
                (lambda ()
                  (define sr (tool-store-memory (hash 'content "delete event") ctx))
                  (define id (hash-ref (tool-result-details sr) 'memory-id))
                  (set-box! events '())
                  (define r (tool-delete-memory (hash 'id id) ctx))
                  (check-false (tool-result-is-error? r))
                  (check-equal? (event-types events) '("memory.item.deleted")))))

;; ---------------------------------------------------------------------------
;; F22: Retrieval event payload verification
;; ---------------------------------------------------------------------------

(test-case "retrieval event contains all SPEC-required fields (F22)"
  (define b (make-memory-hash-backend))
  (define events (box '()))
  (define ctx (collecting-context events))
  (with-backend b
                (lambda ()
                  (tool-store-memory (hash 'content "retrieval payload test") ctx)
                  (set-box! events '())
                  (define r (tool-search-memory (hash 'query "payload") ctx))
                  (check-false (tool-result-is-error? r))
                  (define evts (reverse (unbox events)))
                  (check-equal? (length evts) 1)
                  (define evt (car evts))
                  (check-equal? (hash-ref evt 'type) "memory.retrieval.performed")
                  (check-true (hash-has-key? evt 'query-snippet))
                  (check-true (hash-has-key? evt 'result-count))
                  (check-true (integer? (hash-ref evt 'result-count)))
                  (check-true (hash-has-key? evt 'scope))
                  (check-true (hash-has-key? evt 'latency-ms))
                  (check-true (integer? (hash-ref evt 'latency-ms))))))

;; ---------------------------------------------------------------------------
;; F24: store.requested fires before policy.blocked
;; ---------------------------------------------------------------------------

(test-case "policy-blocked store emits store.requested before blocked (F24)"
  (define b (make-memory-hash-backend))
  (define events (box '()))
  (define ctx (collecting-context events))
  (with-backend b
                (lambda ()
                  ;; Secret content triggers policy block
                  (define r (tool-store-memory (hash 'content "API_KEY=sk-super-secret") ctx))
                  (check-true (tool-result-is-error? r))
                  (define evts (event-types events))
                  ;; Must have BOTH events in order
                  (check-equal? evts '("memory.item.store.requested" "memory.policy.blocked")))))

;; ---------------------------------------------------------------------------
;; F17: Backend store! called exactly once
;; ---------------------------------------------------------------------------

(test-case "store_memory calls backend store! exactly once (F17)"
  (define call-count (box 0))
  (define base-backend (make-memory-hash-backend))
  ;; Wrap the store! to count calls
  (define wrapped-backend
    (memory-backend "counting"
                    (lambda (item)
                      (set-box! call-count (+ 1 (unbox call-count)))
                      (gen:store-memory! base-backend item))
                    (lambda (query) (gen:retrieve-memory base-backend query))
                    (lambda (id patch) (gen:update-memory! base-backend id patch))
                    (lambda (id scope) (gen:delete-memory! base-backend id scope))
                    (lambda (query) (gen:list-memory base-backend query))
                    (lambda () (gen:memory-available? base-backend))
                    (lambda (policy) (gen:manage-memory! base-backend policy))))
  (define events (box '()))
  (define ctx (collecting-context events))
  (parameterize ([current-memory-backend wrapped-backend]
                 [current-memory-policy default-memory-policy])
    (define r (tool-store-memory (hash 'content "counting test") ctx))
    (check-false (tool-result-is-error? r))
    (check-equal? (unbox call-count) 1)))

;; ---------------------------------------------------------------------------
;; F18: Event observers do not cause double-write
;; ---------------------------------------------------------------------------

(test-case "event observers do not cause double-write (F18)"
  (define call-count (box 0))
  (define observer-called? (box #f))
  (define base-backend (make-memory-hash-backend))
  (define wrapped-backend
    (memory-backend "double-write-guard"
                    (lambda (item)
                      (set-box! call-count (+ 1 (unbox call-count)))
                      (gen:store-memory! base-backend item))
                    (lambda (query) (gen:retrieve-memory base-backend query))
                    (lambda (id patch) (gen:update-memory! base-backend id patch))
                    (lambda (id scope) (gen:delete-memory! base-backend id scope))
                    (lambda (query) (gen:list-memory base-backend query))
                    (lambda () (gen:memory-available? base-backend))
                    (lambda (policy) (gen:manage-memory! base-backend policy))))
  ;; Observer that tries to store again — should NOT cause another backend write
  (define (observing-publisher evt)
    (unless (unbox observer-called?)
      (set-box! observer-called? #t)
      ;; Observer tries to store via a DIFFERENT backend call — harmless
      (gen:store-memory!
       base-backend
       (memory-item "observer-item"
                    'semantic
                    'project
                    "observer artifact"
                    (hasheq 'project-root
                            "/tmp"
                            'session-id
                            "obs"
                            'tags
                            '()
                            'source
                            'test
                            'origin-message-id
                            "obs")
                    (hasheq 'sensitivity 'public 'confidence 1.0 'supersedes '() 'expires-at #f)
                    "2026-06-05T12:00:00Z"
                    "2026-06-05T12:00:00Z"))))
  (define ctx
    (make-exec-context #:working-directory "/tmp/q-memory-events"
                       #:session-metadata (hasheq 'session-id "sess-dw")
                       #:event-publisher observing-publisher))
  (parameterize ([current-memory-backend wrapped-backend]
                 [current-memory-policy default-memory-policy])
    (define r (tool-store-memory (hash 'content "double-write guard test") ctx))
    (check-false (tool-result-is-error? r))
    ;; The wrapped backend (tool's backend) should be called exactly once
    (check-equal? (unbox call-count) 1)))

;; ---------------------------------------------------------------------------
;; M13-F12: Event schema versioning
;; ---------------------------------------------------------------------------

(test-case "event: MEMORY-EVENT-SCHEMA-VERSION is defined"
  (check-true (exact-positive-integer? MEMORY-EVENT-SCHEMA-VERSION)))

(test-case "event: mem-item-updated-event is defined and constructable"
  (define e (make-mem-item-updated-event "test-id" 'session 'tool "snippet"))
  (check-equal? (mem-item-updated-event-memory-id e) "test-id")
  (check-equal? (mem-item-updated-event-scope e) 'session)
  (check-equal? (mem-item-updated-event-source e) 'tool)
  (check-equal? (mem-item-updated-event-redacted-snippet e) "snippet"))

(test-case "event: updated event has default values"
  (define e (make-mem-item-updated-event "id" 'project))
  (check-equal? (mem-item-updated-event-source e) 'tool)
  (check-equal? (mem-item-updated-event-redacted-snippet e) ""))
