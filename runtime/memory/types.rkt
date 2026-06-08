#lang racket/base
;; runtime/memory/types.rkt — Memory domain model: structs, predicates, conversions
;; STABILITY: internal
;;
;; Canonical types for the modular memory system.
;; No session/config/context imports. Pure data + conversion.

(require racket/contract
         racket/match)

;; Explicit provides (replaces struct-out for AF5 — hub module with 16+ consumers)
(provide memory-item
         memory-item?
         memory-item-id
         memory-item-type
         memory-item-scope
         memory-item-content
         memory-item-metadata
         memory-item-validity
         memory-item-created-at
         memory-item-updated-at
         memory-query
         memory-query?
         memory-query-text
         memory-query-scope
         memory-query-project-root
         memory-query-session-id
         memory-query-types
         memory-query-tags
         memory-query-limit
         memory-query-include-expired?
         memory-result
         memory-result?
         memory-result-ok?
         memory-result-value
         memory-result-error
         memory-result-metadata
         memory-type?
         memory-scope?
         sensitivity?
         iso-8601-timestamp?
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

(define (iso-8601-timestamp? v)
  (and (string? v)
       (>= (string-length v) 20)
       (regexp-match? #px"^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}" v)))

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
         limit ; exact-positive-integer? | #f — max results
         include-expired?) ; boolean
  #:guard (lambda (text scope project-root session-id types tags limit include-expired? _name)
            (when (and limit (or (not (integer? limit)) (<= limit 0)))
              (error 'memory-query "limit must be a positive integer or #f, got: ~a" limit))
            (values text scope project-root session-id types tags limit include-expired?))
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

;; SPEC §3.4: required metadata keys
(define required-metadata-keys '(project-root session-id tags source))

;; SPEC §3.5: required validity keys (F25: supersedes required)
(define required-validity-keys '(sensitivity confidence supersedes))

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
       ;; SPEC §3.4: required metadata keys (P2-1)
       (for/and ([k (in-list required-metadata-keys)])
         (hash-has-key? (memory-item-metadata v) k))
       ;; SPEC §3.5: required validity keys (P2-1, F25: supersedes)
       (for/and ([k (in-list required-validity-keys)])
         (hash-has-key? (memory-item-validity v) k))
       ;; SPEC §3.4: origin-message-id or origin-tool-call-id required (F2)
       (or (hash-has-key? (memory-item-metadata v) 'origin-message-id)
           (hash-has-key? (memory-item-metadata v) 'origin-tool-call-id))
       (iso-8601-timestamp? (memory-item-created-at v))
       (iso-8601-timestamp? (memory-item-updated-at v))))

;; ---------------------------------------------------------------------------
;; Conversions
;; ---------------------------------------------------------------------------

;; F34: memory-item->hash returns an equal?-based hash (acceptable for serialization)
;; while internal code uses hasheq. This is intentional — equal? works for both
;; symbol and string keys, making it safer for interop with JSON.
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

;; F35: WARNING — hash->memory-item is a partial/unsafe conversion for deserialization.
;; Resulting items may fail valid-memory-item? if the input hash is incomplete.
;; Callers MUST validate with valid-memory-item? before storing.
;; Consider adding #:validate? keyword in future if more callers need guard.
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

;; F36: memory-result->hash is for internal use. If used for JSON serialization,
;; the error hash keys (code, message, retryable?) may need stringification.
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
