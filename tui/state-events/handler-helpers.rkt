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
