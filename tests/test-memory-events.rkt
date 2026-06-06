#lang racket/base
;; tests/test-memory-events.rkt — SPEC memory event taxonomy and tool emission tests

(require rackunit
         racket/list
         "../tools/builtins/memory-tools.rkt"
         "../tools/tool.rkt"
         "../runtime/memory/types.rkt"
         "../runtime/memory/backends/memory-hash.rkt"
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
