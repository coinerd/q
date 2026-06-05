#lang racket/base
;; tests/test-memory-events.rkt — Memory event emission tests
;;
;; Tests that memory events have correct structure, field access,
;; type strings, field registry, and integration with memory tools.

(require rackunit
         racket/list
         "../tools/builtins/memory-tools.rkt"
         "../tools/tool.rkt"
         "../runtime/memory/types.rkt"
         "../runtime/memory/backends/memory-hash.rkt"
         "../runtime/memory/policy.rkt"
         "../agent/event-structs/memory-events.rkt"
         "../agent/event-structs/base.rkt"
         (only-in "../util/event/event-macro.rkt" lookup-event-schema-version))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (with-backend backend thunk)
  (parameterize ([current-memory-backend backend])
    (thunk)))

(define (call-tool tool args)
  (tool args #f))

;; ---------------------------------------------------------------------------
;; Event struct tests
;; ---------------------------------------------------------------------------

(test-case "mem-stored-event: construction and field access"
  (define evt
    (make-mem-stored-event #:session-id "sess-1"
                           #:turn-id "turn-1"
                           #:memory-id "mem-1"
                           #:scope 'session
                           #:mem-type 'semantic
                           #:sensitivity 'public
                           #:content-length 42))
  (check-equal? (typed-event-type evt) "memory.stored")
  (check-equal? (mem-stored-event-memory-id evt) "mem-1")
  (check-equal? (mem-stored-event-scope evt) 'session)
  (check-equal? (mem-stored-event-mem-type evt) 'semantic)
  (check-equal? (mem-stored-event-sensitivity evt) 'public)
  (check-equal? (mem-stored-event-content-length evt) 42))

(test-case "mem-retrieved-event: construction"
  (define evt
    (make-mem-retrieved-event #:session-id "sess-1"
                              #:turn-id "turn-1"
                              #:query-scope 'project
                              #:result-count 5
                              #:query-limit 10))
  (check-equal? (typed-event-type evt) "memory.retrieved")
  (check-equal? (mem-retrieved-event-result-count evt) 5)
  (check-equal? (mem-retrieved-event-query-limit evt) 10))

(test-case "mem-deleted-event: construction"
  (define evt
    (make-mem-deleted-event #:session-id "sess-1"
                            #:turn-id "turn-1"
                            #:memory-id "mem-1"
                            #:scope 'session))
  (check-equal? (typed-event-type evt) "memory.deleted")
  (check-equal? (mem-deleted-event-memory-id evt) "mem-1")
  (check-equal? (mem-deleted-event-scope evt) 'session))

;; ---------------------------------------------------------------------------
;; Event type string tests
;; ---------------------------------------------------------------------------

(test-case "memory event type strings are correct"
  (check-equal? mem-stored-event-type "memory.stored")
  (check-equal? mem-retrieved-event-type "memory.retrieved")
  (check-equal? mem-deleted-event-type "memory.deleted"))

;; ---------------------------------------------------------------------------
;; Event field registry tests
;; ---------------------------------------------------------------------------

(test-case "memory event fields registered"
  (check-equal? mem-stored-event-fields '(memory-id scope mem-type sensitivity content-length))
  (check-equal? mem-retrieved-event-fields '(query-scope result-count query-limit))
  (check-equal? mem-deleted-event-fields '(memory-id scope)))

;; ---------------------------------------------------------------------------
;; Event transparency (equality)
;; ---------------------------------------------------------------------------

(test-case "memory events are transparent"
  (define evt1
    (make-mem-stored-event #:session-id "s1"
                           #:turn-id "t1"
                           #:memory-id "m1"
                           #:scope 'session
                           #:mem-type 'semantic
                           #:sensitivity 'public
                           #:content-length 10))
  (define evt2
    (make-mem-stored-event #:session-id "s1"
                           #:turn-id "t1"
                           #:memory-id "m1"
                           #:scope 'session
                           #:mem-type 'semantic
                           #:sensitivity 'public
                           #:content-length 10))
  (check-equal? evt1 evt2))

;; ---------------------------------------------------------------------------
;; Integration: tool operations produce event-compatible data
;; ---------------------------------------------------------------------------

(test-case "store operation data compatible with event"
  (define b (make-memory-hash-backend))
  (with-backend
   b
   (lambda ()
     (define r
       (call-tool tool-store-memory (hash 'content "test fact" 'type 'semantic 'scope 'session)))
     (check-false (tool-result-is-error? r))
     (define items (memory-hash-backend-items b))
     (check-equal? (length items) 1)
     (define item (car items))
     ;; Build event from stored item
     (define evt
       (make-mem-stored-event #:session-id "test"
                              #:turn-id "test"
                              #:memory-id (memory-item-id item)
                              #:scope (memory-item-scope item)
                              #:mem-type (memory-item-type item)
                              #:sensitivity 'public
                              #:content-length (string-length (memory-item-content item))))
     (check-equal? (mem-stored-event-content-length evt) 9))))

(test-case "search operation data compatible with event"
  (define b (make-memory-hash-backend))
  (with-backend b
                (lambda ()
                  (call-tool tool-store-memory (hash 'content "item 1" 'scope 'project))
                  (call-tool tool-store-memory (hash 'content "item 2" 'scope 'project))
                  (define r (call-tool tool-search-memory (hash 'scope 'project 'limit 10)))
                  (check-false (tool-result-is-error? r))
                  ;; Build event from search results
                  (define evt
                    (make-mem-retrieved-event #:session-id "test"
                                              #:turn-id "test"
                                              #:query-scope 'project
                                              #:result-count 2
                                              #:query-limit 10))
                  (check-equal? (mem-retrieved-event-result-count evt) 2))))

(test-case "delete operation data compatible with event"
  (define b (make-memory-hash-backend))
  (with-backend
   b
   (lambda ()
     (call-tool tool-store-memory (hash 'content "temp" 'scope 'session))
     (define items (memory-hash-backend-items b))
     (define id (memory-item-id (car items)))
     (define r (call-tool tool-delete-memory (hash 'id id 'scope 'session)))
     (check-false (tool-result-is-error? r))
     ;; Build event from delete
     (define evt
       (make-mem-deleted-event #:session-id "test" #:turn-id "test" #:memory-id id #:scope 'session))
     (check-equal? (mem-deleted-event-memory-id evt) id))))

;; ---------------------------------------------------------------------------
;; Event schema version
;; ---------------------------------------------------------------------------

(test-case "memory events have schema version 1"
  (check-equal? (lookup-event-schema-version "memory.stored") 1)
  (check-equal? (lookup-event-schema-version "memory.retrieved") 1)
  (check-equal? (lookup-event-schema-version "memory.deleted") 1))
