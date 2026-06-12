#lang racket/base

;; util/message/mas-envelope.rkt — Inter-agent communication envelope
;; STABILITY: evolving
;;
;; Defines the structured message envelope for all MAS inter-agent
;; communication. Every message between agents flows through this
;; envelope, providing tracing, capability validation, and deadline
;; management.
;;
;; Schema version: 1

(require racket/contract
         racket/string
         (only-in "../../agent/capability.rkt" valid-capability? VALID-CAPABILITIES))

;; ============================================================
;; Struct Definition
;; ============================================================

(struct mas-envelope
        (message-id ; string? — unique message identifier (UUID)
         trace-id ; string? — distributed tracing ID
         source-agent ; symbol? — sending agent role
         target-agent ; symbol? — receiving agent role
         capability ; symbol? — required capability from VALID-CAPABILITIES
         payload ; any/c — message payload (hash, string, etc.)
         deadline ; (or/c exact-positive-integer? #f) — epoch-ms deadline, #f = no deadline
         risk-level ; (or/c 'low 'medium 'high 'critical) — risk assessment
         schema-version) ; exact-nonnegative-integer? — envelope schema version
  #:transparent)

;; ============================================================
;; Provides
;; ============================================================

(provide mas-envelope
         mas-envelope?
         mas-envelope-message-id
         mas-envelope-trace-id
         mas-envelope-source-agent
         mas-envelope-target-agent
         mas-envelope-capability
         mas-envelope-payload
         mas-envelope-deadline
         mas-envelope-risk-level
         mas-envelope-schema-version

         (contract-out [make-mas-envelope
                        (->* (symbol? symbol? symbol? any/c)
                             (#:message-id (or/c string? #f)
                                           #:trace-id (or/c string? #f)
                                           #:deadline (or/c exact-positive-integer? #f)
                                           #:risk-level (or/c 'low 'medium 'high 'critical)
                                           #:schema-version exact-nonnegative-integer?)
                             mas-envelope?)]
                       [envelope->hash (-> mas-envelope? hash?)]
                       [hash->envelope (-> any/c (or/c mas-envelope? #f))]))

;; ============================================================
;; Constructor
;; ============================================================

(define SCHEMA-VERSION 1)
(define DEFAULT-DEADLINE-MS 300000) ; 5 minutes

(define (make-mas-envelope source-agent
                           target-agent
                           capability
                           payload
                           #:message-id [message-id #f]
                           #:trace-id [trace-id #f]
                           #:deadline [deadline #f]
                           #:risk-level [risk-level 'low]
                           #:schema-version [schema-version SCHEMA-VERSION])
  ;; Validate capability
  (unless (valid-capability? capability)
    (raise-argument-error 'make-mas-envelope
                          (format "capability from ~a" VALID-CAPABILITIES)
                          capability))
  ;; Validate risk-level
  (unless (memq risk-level '(low medium high critical))
    (raise-argument-error 'make-mas-envelope "(or/c 'low 'medium 'high 'critical)" risk-level))
  (mas-envelope (or message-id (generate-id))
                (or trace-id (generate-id))
                source-agent
                target-agent
                capability
                payload
                (or deadline (+ (current-inexact-milliseconds) DEFAULT-DEADLINE-MS))
                risk-level
                schema-version))

;; ============================================================
;; Serialization
;; ============================================================

(define (envelope->hash env)
  (hasheq 'message-id
          (mas-envelope-message-id env)
          'trace-id
          (mas-envelope-trace-id env)
          'source-agent
          (mas-envelope-source-agent env)
          'target-agent
          (mas-envelope-target-agent env)
          'capability
          (mas-envelope-capability env)
          'payload
          (mas-envelope-payload env)
          'deadline
          (mas-envelope-deadline env)
          'risk-level
          (mas-envelope-risk-level env)
          'schema-version
          (mas-envelope-schema-version env)))

(define (hash->envelope h)
  (cond
    [(not (hash? h)) #f]
    [else
     (with-handlers ([exn:fail? (lambda (_) #f)])
       (mas-envelope (hash-ref h 'message-id "")
                     (hash-ref h 'trace-id "")
                     (hash-ref h 'source-agent 'unknown)
                     (hash-ref h 'target-agent 'unknown)
                     (hash-ref h 'capability 'any)
                     (hash-ref h 'payload #f)
                     (hash-ref h 'deadline #f)
                     (hash-ref h 'risk-level 'low)
                     (hash-ref h 'schema-version SCHEMA-VERSION)))]))

;; ============================================================
;; ID Generation
;; ============================================================

(define (generate-id)
  (define ms (current-inexact-milliseconds))
  (define rand (random 1000000000))
  (format "~a-~a" ms rand))
