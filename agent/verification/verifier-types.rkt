#lang racket/base

;; agent/verification/verifier-types.rkt — Verifier decision data structures
;; STABILITY: evolving
;;
;; W1 (v0.99.5): Structured verifier decision type + JSON round-trip.
;; The verifier decision is a structured result — never free-form text.
;; It encodes the verdict, reasoning, risk assessment, and whether human
;; approval is required.
;;
;; Part of MAS Schritt 3: Verifier-Agent (milestone #791).

(require racket/contract
         json)

;; ============================================================
;; Constants
;; ============================================================

;; Valid verdict values for a verifier decision.
(define VERIFIER-VERDICTS '(approve reject escalate))

;; Valid risk levels for a verifier decision.
(define VERIFIER-RISK-LEVELS '(low medium high))

;; Predicate: is this a valid verdict symbol?
(define (verifier-verdict? v)
  (and (symbol? v) (memq v VERIFIER-VERDICTS) #t))

;; Predicate: is this a valid risk-level symbol?
(define (verifier-risk-level? v)
  (and (symbol? v) (memq v VERIFIER-RISK-LEVELS) #t))

;; ============================================================
;; Struct Definition
;; ============================================================

(struct verifier-decision
        (verdict ; (or/c 'approve 'reject 'escalate)
         reason ; string? — one-sentence justification
         risk-level ; (or/c 'low 'medium 'high)
         requires-human? ; boolean? — triggers HITL gate
         artifact-refs ; (listof string?) — files/refs reviewed
         timestamp) ; (or/c real? #f) — epoch-ms or #f if unset
  #:transparent)

;; ============================================================
;; JSON Conversion
;; ============================================================

;; Convert a verifier-decision to a JSON-serializable hash.
;; Symbols are converted to strings (JSON has no symbols).
(define (verifier-decision->json vd)
  (hasheq 'verdict
          (symbol->string (verifier-decision-verdict vd))
          'reason
          (verifier-decision-reason vd)
          'risk_level
          (symbol->string (verifier-decision-risk-level vd))
          'requires_human
          (verifier-decision-requires-human? vd)
          'artifact_refs
          (verifier-decision-artifact-refs vd)
          'timestamp
          (verifier-decision-timestamp vd)))

;; Convert a JSON-parsed jsexpr back to a verifier-decision.
;; Returns #f on any parse failure (missing fields, wrong types, etc.)
;; — never throws.
(define (json->verifier-decision jsexpr)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (cond
      [(not (hash? jsexpr)) #f]
      [else
       (define verdict-str (hash-ref jsexpr 'verdict #f))
       (define verdict-sym (and (string? verdict-str) (string->symbol verdict-str)))
       (define risk-str (hash-ref jsexpr 'risk_level #f))
       (define risk-sym (and (string? risk-str) (string->symbol risk-str)))
       (define reason (hash-ref jsexpr 'reason #f))
       (define requires-human (hash-ref jsexpr 'requires_human #f))
       (define artifact-refs (hash-ref jsexpr 'artifact_refs #f))
       (define timestamp (hash-ref jsexpr 'timestamp #f))
       (cond
         ;; Validate verdict
         [(not (verifier-verdict? verdict-sym)) #f]
         ;; Validate risk-level
         [(not (verifier-risk-level? risk-sym)) #f]
         ;; Validate reason is a string
         [(not (string? reason)) #f]
         ;; Validate requires-human is a boolean
         [(not (boolean? requires-human)) #f]
         ;; Validate artifact-refs is a list of strings
         [(not (and (list? artifact-refs) (andmap string? artifact-refs))) #f]
         ;; Validate timestamp is real or #f
         [(not (or (not timestamp) (real? timestamp))) #f]
         [else
          (verifier-decision verdict-sym reason risk-sym requires-human artifact-refs timestamp)])])))

;; ============================================================
;; Convenience Constructors
;; ============================================================

;; Create an approve decision. requires-human? defaults to #f.
(define (make-approve-decision reason
                               #:risk-level [risk-level 'low]
                               #:artifact-refs [artifact-refs '()]
                               #:timestamp [timestamp #f])
  (verifier-decision 'approve reason risk-level #f artifact-refs timestamp))

;; Create a reject decision. requires-human? defaults to #f.
(define (make-reject-decision reason
                              #:risk-level [risk-level 'high]
                              #:artifact-refs [artifact-refs '()]
                              #:timestamp [timestamp #f])
  (verifier-decision 'reject reason risk-level #f artifact-refs timestamp))

;; Create an escalate decision. requires-human? defaults to #t.
(define (make-escalate-decision reason
                                #:risk-level [risk-level 'high]
                                #:artifact-refs [artifact-refs '()]
                                #:timestamp [timestamp #f])
  (verifier-decision 'escalate reason risk-level #t artifact-refs timestamp))

;; ============================================================
;; Provides
;; ============================================================

(provide VERIFIER-VERDICTS
         VERIFIER-RISK-LEVELS)

(provide (contract-out (struct verifier-decision
                               ([verdict verifier-verdict?] [reason string?]
                                                            [risk-level verifier-risk-level?]
                                                            [requires-human? boolean?]
                                                            [artifact-refs (listof string?)]
                                                            [timestamp (or/c real? #f)]))
                       [verifier-verdict? (-> any/c boolean?)]
                       [verifier-risk-level? (-> any/c boolean?)]
                       [verifier-decision->json (-> verifier-decision? hash?)]
                       [json->verifier-decision (-> any/c (or/c verifier-decision? #f))]
                       [make-approve-decision
                        (->* (string?)
                             (#:risk-level verifier-risk-level?
                                           #:artifact-refs (listof string?)
                                           #:timestamp (or/c real? #f))
                             verifier-decision?)]
                       [make-reject-decision
                        (->* (string?)
                             (#:risk-level verifier-risk-level?
                                           #:artifact-refs (listof string?)
                                           #:timestamp (or/c real? #f))
                             verifier-decision?)]
                       [make-escalate-decision
                        (->* (string?)
                             (#:risk-level verifier-risk-level?
                                           #:artifact-refs (listof string?)
                                           #:timestamp (or/c real? #f))
                             verifier-decision?)]))
