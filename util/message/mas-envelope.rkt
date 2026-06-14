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
         (only-in "../../agent/capability.rkt"
                  valid-capability?
                  VALID-CAPABILITIES
                  ROLE-CAPABILITIES))

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
  ;; M4: Validate source and target agents are known roles
  (unless (hash-has-key? ROLE-CAPABILITIES source-agent)
    (raise-argument-error 'make-mas-envelope "valid role symbol" source-agent))
  (unless (hash-has-key? ROLE-CAPABILITIES target-agent)
    (raise-argument-error 'make-mas-envelope "valid role symbol" target-agent))
  ;; Validate risk-level
  (unless (memq risk-level '(low medium high critical))
    (raise-argument-error 'make-mas-envelope "(or/c 'low 'medium 'high 'critical)" risk-level))
  (mas-envelope (or message-id (generate-id))
                (or trace-id (generate-id))
                source-agent
                target-agent
                capability
                payload
                (or deadline
                    (inexact->exact (round (+ (current-inexact-milliseconds) DEFAULT-DEADLINE-MS))))
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
          (symbol->string (mas-envelope-source-agent env))
          'target-agent
          (symbol->string (mas-envelope-target-agent env))
          'capability
          (symbol->string (mas-envelope-capability env))
          'payload
          (mas-envelope-payload env)
          'deadline
          (mas-envelope-deadline env)
          'risk-level
          (symbol->string (mas-envelope-risk-level env))
          'schema-version
          (mas-envelope-schema-version env)))

;; Coerce a value to symbol, accepting both symbols and strings.
(define (coerce->symbol v [default 'unknown])
  (cond
    [(symbol? v) v]
    [(string? v) (string->symbol v)]
    [else default]))

;; Coerce risk-level string/symbol to symbol, defaulting to 'low for unknown values.
;; R5: Validate against known set to prevent silent envelope rejection.
(define (coerce->risk-level v)
  (define sym (coerce->symbol v 'low))
  (if (memq sym '(low medium high critical)) sym 'low))

(define (hash->envelope h)
  (cond
    [(not (hash? h)) #f]
    [else
     (with-handlers ([exn:fail? (lambda (_) #f)])
       ;; H4 fix: delegate to make-mas-envelope for validation
       (make-mas-envelope (coerce->symbol (hash-ref h 'source-agent 'supervisor))
                          (coerce->symbol (hash-ref h 'target-agent 'supervisor))
                          (coerce->symbol (hash-ref h 'capability 'any) 'any)
                          (hash-ref h 'payload #f)
                          #:message-id (hash-ref h 'message-id #f)
                          #:trace-id (hash-ref h 'trace-id #f)
                          #:deadline (let ([d (hash-ref h 'deadline #f)])
                                       (cond
                                         [(and (number? d) (real? d)) (inexact->exact (round d))]
                                         [else d]))
                          #:risk-level (coerce->risk-level (hash-ref h 'risk-level 'low))
                          #:schema-version (hash-ref h 'schema-version SCHEMA-VERSION)))]))

;; ============================================================
;; ID Generation
;; ============================================================

(define (generate-id)
  (define ms (current-inexact-milliseconds))
  (define rand (random 1000000000))
  (format "~a-~a" ms rand))
