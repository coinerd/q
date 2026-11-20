#lang racket/base
;; STABILITY: internal
;;
;; extensions/gsd/delivery-outcome.rkt — the pure delivery-outcome VALUE
;; surface, extracted verbatim from delivery-coordinator.rkt (R3c #9948; W3
;; pure-production pilot #9951): the outcome constructor and accessors, the
;; operator-facing review-rejected-message rendering, and the ONE pure stop
;; classifier delivery-stop-outcome for typed controller effect results.
;;
;; Purity contract: no I/O, no parameters, no real clock, no journal access —
;; inputs in, fresh values out. The authenticated delivery EFFECTS (the 'ok
;; journal advance: durable fence recheck + eligibility blocker + journal
;; write) deliberately stay inside run-delivery-coordinator! in
;; delivery-coordinator.rkt; delivery-stop-outcome returns #f for 'ok by
;; contract, so a pure classification can never bypass that effect boundary.
;; delivery-coordinator.rkt requires this module and re-provides the same
;; identifiers, so every existing caller and the public API are unchanged.

(require racket/base
         (only-in json jsexpr->string))

(provide delivery-outcome
         delivery-outcome?
         delivery-outcome-kind
         delivery-outcome-message
         delivery-outcome-data
         review-rejected-message
         delivery-stop-outcome)

(define (delivery-outcome kind [message ""] [data #f])
  ;; Legacy shape (kind . message) when there is no typed payload; a
  ;; three-element list (kind message data) when the effect produced typed
  ;; facts. Every consumer uses the accessors, so both shapes coexist.
  (define text
    (if (string? message)
        message
        (format "~a" message)))
  (if data
      (list kind text data)
      (cons kind text)))

(define (delivery-outcome? o)
  (and (pair? o)
       (symbol? (car o))
       (let ([rest (cdr o)]) (or (string? rest) (and (pair? rest) (string? (car rest)))))))

(define (delivery-outcome-kind o)
  (car o))

;; The operator-facing message (unchanged for legacy and typed outcomes).
(define (delivery-outcome-message o)
  (let ([rest (cdr o)])
    (if (string? rest)
        rest
        (car rest))))

;; The typed payload carried by an outcome (a hash for review rejections), or
;; #f for a legacy value. Retry authorization reads THIS, never the message.
(define (delivery-outcome-data o)
  (let ([rest (cdr o)]) (and (pair? rest) (cadr rest))))

;; Operator-facing message for a typed review rejection: the exact receipt
;; identity plus every finding, so a blocked wave surfaces the genuine
;; review verdict (never an opaque strict-gate failure).
;;
;; GSD simplification W5: this is a DISPLAY rendering of the typed rejection
;; payload. Internal retry authorization reads the typed hash that travels
;; alongside it on the delivery outcome (delivery-outcome-data); the text is
;; never parsed back into authorization.
(define (review-rejected-message data)
  ;; Lossless checkpoint envelope: findings may themselves contain delimiters
  ;; or newlines.
  (string-append "review-rejected: " (jsexpr->string data)))

;; ONE pure stop classification for typed controller effect results (W3
;; pure-production pilot #9951). Before this function the review-outcome
;; mapping lived INLINE in run-delivery-coordinator! — twice verbatim (the
;; target effect after a cleared preflight, and every ordinary ladder
;; effect) and once more with preflight-specific reason fallbacks (a
;; refused preflight). This module is now the single classification
;; authority: it consumes the typed kind plus its immutable payload hash
;; and returns the delivery-outcome VALUE for every typed stop:
;;   awaiting-review — the effect stopped awaiting genuine independent review
;;   review-rejected — explicit head-bound REQUEST_CHANGES (typed payload kept)
;;   review-infra    — reviewer transport/timeout/stale/malformed failure
;;   retryable       — typed retryable delivery failure (e.g. CI pending)
;;   blocked         — the controller refused the effect
;; 'ok NEVER classifies here: the function returns #f for it because
;; advancing the journal one stage is the CALLER's authenticated effect
;; (durable fence recheck + eligibility blocker before the write). That
;; effect boundary is deliberately NOT pure and stays inside
;; run-delivery-coordinator!.
;; Purity: no I/O, no parameters, no real clock, no journal access — the
;; inputs are the kind symbol and payload hash, the output is a fresh
;; delivery-outcome value. #:reason-fallback (the preflight path) replaces
;; every per-kind default reason; #:blocked-message (the preflight path)
;; carries its pre-composed blocked text verbatim.
(define (delivery-stop-outcome kind
                               data
                               #:reason-fallback [reason-fallback #f]
                               #:blocked-message [blocked-message #f])
  (if (eq? kind 'ok)
      #f
      (case kind
        [(awaiting-review)
         (delivery-outcome
          'awaiting-review
          (hash-ref data 'reason (or reason-fallback "awaiting genuine independent review")))]
        [(review-rejected) (delivery-outcome 'review-rejected (review-rejected-message data) data)]
        [(review-infra)
         (delivery-outcome
          'review-infra
          (hash-ref data 'reason (or reason-fallback "review infrastructure failure")))]
        [(retryable)
         (delivery-outcome 'retryable
                           (hash-ref data 'reason (or reason-fallback "retryable delivery failure")))]
        [else
         (if blocked-message
             (delivery-outcome 'blocked blocked-message)
             (delivery-outcome
              'blocked
              (hash-ref data 'reason (or reason-fallback "controller refused the effect"))))])))
