#lang racket/base

;; tui/state-events/handler-helpers.rkt
;; W7 v0.99.35: Pure helper functions extracted from core-handlers.rkt
;;
;; These functions perform pure computation — no I/O, no state mutation,
;; no parameter side-effects. They handle event payload extraction, key
;; normalization, and display text formatting.
;;
;; STABILITY: evolving

(require racket/string
         racket/match
         (only-in "../../util/event/event.rkt" event-payload))

(provide kebab->camel
         hash-ref-multi
         verification-payload-ref
         verification-event-identity
         wave-outcome-envelope-ref
         retry-error-type-label
         tool-progress-status-text)

;; ============================================================
;; Key normalization (pure)
;; ============================================================

;; Converts kebab-case symbol to camelCase symbol.
;; e.g. 'artifact-count → 'artifactCount, 'risk-level → 'riskLevel
;; Single-word symbols are returned unchanged.
(define (kebab->camel sym)
  (define str (symbol->string sym))
  (define parts (string-split str #rx"-"))
  (if (null? (cdr parts))
      sym
      (string->symbol (string-append (car parts)
                                     (apply string-append
                                            (map (lambda (p) (string-titlecase p)) (cdr parts)))))))

;; Looks up a key in a hash, trying both kebab-case and camelCase forms.
(define (hash-ref-multi h key camel-key [default #f])
  (or (hash-ref h key #f) (hash-ref h camel-key #f) default))

;; Extracts a field from a verification event payload, handling both the
;; direct typed-event hash format and the GSD-wrapped format (with 'data sub-hash).
;; Also handles both kebab-case and camelCase key variants.
(define (verification-payload-ref evt key [default #f])
  (define payload (event-payload evt))
  (define camel-key (kebab->camel key))
  (cond
    [(not (hash? payload)) default]
    ;; GSD-wrapped format: data is under 'data key
    [(and (hash-has-key? payload 'data) (hash? (hash-ref payload 'data)))
     (hash-ref-multi (hash-ref payload 'data) key camel-key default)]
    ;; Direct typed-event hash format
    [else (hash-ref-multi payload key camel-key default)]))

;; R102 narrow accessor: the trusted identity of a verification event
;; (registry incarnation + registry job-id + verify identity key + the
;; session attachment generation bound at the source attach). Returns #f for
;; identity-less LEGACY events, which the reducer must handle
;; conservatively: they can never clear identified newer progress.
(define (verification-event-identity evt)
  (define job-id (verification-payload-ref evt 'job-id #f))
  (define incarnation (verification-payload-ref evt 'registry-incarnation #f))
  (and job-id
       incarnation
       (let ([vkey (verification-payload-ref evt 'verify-identity-key #f)]
             [generation (verification-payload-ref evt 'session-attachment-generation #f)])
         ;; the identity key is stable across started/completed and across
         ;; string/symbol spellings of the same fields (JSON round trips)
         (hasheq 'key
                 (format "~a|~a|~a" incarnation job-id (or vkey "-"))
                 'job-id
                 job-id
                 'registry-incarnation
                 incarnation
                 'verify-identity-key
                 vkey
                 'session-attachment-generation
                 generation))))

;; ============================================================
;; Wave outcome envelope (BUG-0093 / R93, pure)
;; ============================================================

;; Narrow accessor for the ONE nested outcome envelope. extensions/gsd/
;; events.rkt wraps every emission as (hasheq 'event … 'correlation-id …
;; 'timestamp … 'data <fields>), and extensions/gsd-planning.rkt forwards
;; that WRAPPER verbatim as the runtime event payload over the session
;; bus, so gsd.wave.outcome-error arrives with wave/kind/message nested
;; under the data key instead of at the payload's top level.
;;
;; Recognition rules — exactly this shape and nothing else:
;;   * payload is a hash carrying the wrapper key 'data (symbol) or
;;     "data" (string, post-JSON-transport),
;;   * the value under that key is a hash (the inner outcome fields),
;;   * the requested field is read from the INNER hash by symbol, then by
;;     the same field name spelled as a string (inner JSON keys are
;;     strings; the outer string→symbol normalization does not descend).
;; Everything else — payload not a hash, data missing, data not a hash,
;; field absent — yields `default`, so the reducer keeps its honest
;; unknown/empty fallback on malformed input without crashing. Outer
;; wrapper fields (event/correlation-id/timestamp/__typed) are never
;; read, so nothing unrelated can merge into the outcome details, and no
;; payload string is ever interned: the only string conversion is the
;; KNOWN field-name symbol→string used purely for lookup.
(define (wave-outcome-envelope-ref payload key [default #f])
  (define inner
    (and (hash? payload)
         (let ([v (hash-ref payload 'data (lambda () (hash-ref payload "data" #f)))])
           (and (hash? v) v))))
  (and inner
       (or (hash-ref inner key #f)
           (and (symbol? key) (hash-ref inner (symbol->string key) #f))
           default)))

;; ============================================================
;; Retry error type label (pure mapping)
;; ============================================================

;; Maps an error-type symbol to a human-readable label for retry messages.
;; Returns #f for unknown types (caller falls back to generic message).
(define (retry-error-type-label error-type)
  (case error-type
    [(timeout) "LLM timeout"]
    [(rate-limit) "rate limited"]
    [(context-overflow) "context too large"]
    [(provider-error) "server error"]
    [else #f]))

;; ============================================================
;; Tool progress status text (pure formatting)
;; ============================================================

;; Computes status text for tool execution progress display.
(define (tool-progress-status-text tool-name total running)
  (if (> total 1)
      (format "~a tools running (~a pending)" running total)
      (format "~a running" tool-name)))
