#lang racket/base
;; runtime/memory/types.rkt — Memory domain model: structs, predicates, conversions
;;
;; Canonical types for the modular memory system.
;; No session/config/context imports. Pure data + conversion.

(require racket/contract
         racket/match)

(provide (struct-out memory-item)
         (struct-out memory-query)
         (struct-out memory-result)
         memory-type?
         memory-scope?
         sensitivity?
         memory-item->hash
         hash->memory-item
         memory-query->hash
         hash->memory-query
         memory-result->hash
         make-memory-error
         valid-memory-item?
         default-memory-limit)

;; ---------------------------------------------------------------------------
;; Predicates
;; ---------------------------------------------------------------------------

(define (memory-type? v)
  (and (memq v '(episodic semantic procedural)) #t))

(define (memory-scope? v)
  (and (memq v '(session project user)) #t))

(define (sensitivity? v)
  (and (memq v
             '(public internal
                      sensitive
                      secret))
       #t))

(define default-memory-limit 5)

;; ---------------------------------------------------------------------------
;; Structs
;; ---------------------------------------------------------------------------

(struct memory-item
        (id ; string — stable item id
         type ; 'episodic | 'semantic | 'procedural
         scope ; 'session | 'project | 'user
         content ; non-empty string
         metadata ; hash: project-root, session-id, tags, origin, source
         validity ; hash: sensitivity, confidence, expires-at, supersedes
         created-at ; ISO-8601 UTC string
         updated-at) ; ISO-8601 UTC string
  #:transparent)

(struct memory-query
        (text ; string — search text
         scope ; 'session | 'project | 'user | #f
         project-root ; string | #f
         session-id ; string | #f
         types ; (listof symbol) | #f — filter by type
         tags ; (listof string) | #f — filter by tags
         limit ; exact-positive-integer? — max results
         include-expired?) ; boolean
  #:transparent)

(struct memory-result
        (ok? ; boolean
         value ; item / (listof item) / id / #f
         error ; #f | hash with 'code, 'message, 'retryable?
         metadata) ; jsexpr-safe hash (count, latency-ms, backend, etc.)
  #:transparent)

;; ---------------------------------------------------------------------------
;; Validation
;; ---------------------------------------------------------------------------

(define (valid-memory-item? v)
  (and (memory-item? v)
       (string? (memory-item-id v))
       (> (string-length (memory-item-id v)) 0)
       (memory-type? (memory-item-type v))
       (memory-scope? (memory-item-scope v))
       (string? (memory-item-content v))
       (> (string-length (memory-item-content v)) 0)
       (hash? (memory-item-metadata v))
       (hash? (memory-item-validity v))
       (string? (memory-item-created-at v))
       (string? (memory-item-updated-at v))))

;; ---------------------------------------------------------------------------
;; Conversions
;; ---------------------------------------------------------------------------

(define (memory-item->hash item)
  (hash 'id
        (memory-item-id item)
        'type
        (memory-item-type item)
        'scope
        (memory-item-scope item)
        'content
        (memory-item-content item)
        'metadata
        (memory-item-metadata item)
        'validity
        (memory-item-validity item)
        'created-at
        (memory-item-created-at item)
        'updated-at
        (memory-item-updated-at item)))

(define (hash->memory-item h)
  (memory-item (hash-ref h 'id)
               (hash-ref h 'type 'semantic)
               (hash-ref h 'scope 'session)
               (hash-ref h 'content "")
               (hash-ref h 'metadata (hasheq))
               (hash-ref h 'validity (hasheq))
               (hash-ref h 'created-at "")
               (hash-ref h 'updated-at "")))

(define (memory-query->hash q)
  (hash 'text
        (memory-query-text q)
        'scope
        (memory-query-scope q)
        'project-root
        (memory-query-project-root q)
        'session-id
        (memory-query-session-id q)
        'types
        (memory-query-types q)
        'tags
        (memory-query-tags q)
        'limit
        (memory-query-limit q)
        'include-expired?
        (memory-query-include-expired? q)))

(define (hash->memory-query h)
  (memory-query (hash-ref h 'text "")
                (hash-ref h 'scope #f)
                (hash-ref h 'project-root #f)
                (hash-ref h 'session-id #f)
                (hash-ref h 'types #f)
                (hash-ref h 'tags #f)
                (hash-ref h 'limit default-memory-limit)
                (hash-ref h 'include-expired? #f)))

(define (memory-result->hash r)
  (define val (memory-result-value r))
  (hash 'ok?
        (memory-result-ok? r)
        'value
        (cond
          [(memory-item? val) (memory-item->hash val)]
          [(list? val)
           (map (lambda (v)
                  (if (memory-item? v)
                      (memory-item->hash v)
                      v))
                val)]
          [else val])
        'error
        (memory-result-error r)
        'metadata
        (memory-result-metadata r)))

;; ---------------------------------------------------------------------------
;; Error helper
;; ---------------------------------------------------------------------------

(define (make-memory-error code message [retryable? #f])
  (hash 'code code 'message message 'retryable? retryable?))
