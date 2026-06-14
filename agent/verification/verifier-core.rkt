#lang racket/base

;; agent/verification/verifier-core.rkt — Verifier LLM invocation + response parsing
;; STABILITY: evolving
;;
;; W4 (v0.99.5): The "brain" of the verifier. Orchestrates the LLM call,
;; parses the JSON response, validates the schema, and constructs a
;; verifier-decision. All failure modes produce safe decisions:
;;   - Non-JSON response → auto-reject with requires-human? #t
;;   - Missing/invalid verdict → auto-reject
;;   - Provider error/timeout → escalate
;;
;; Feature-gated via current-verifier-enabled (defaults #f — inert).
;;
;; Part of MAS Schritt 3: Verifier-Agent (milestone #791).

(require racket/contract
         racket/match
         racket/string
         json
         "verifier-types.rkt"
         "verifier-prompt.rkt"
         (only-in "../../llm/provider.rkt" provider-send provider?)
         (only-in "../../llm/model.rkt" make-model-request model-response? model-response-content))

;; ============================================================
;; Config Parameters
;; ============================================================

;; Feature flag: verifier is disabled by default.
(define current-verifier-enabled (make-parameter #f))

;; Model override: #f means use the session's default model.
(define current-verifier-model (make-parameter #f))

;; Risk threshold: if risk-level >= threshold, force requires-human? #t.
;; Ordering: low < medium < high.
(define current-verifier-risk-threshold (make-parameter 'medium))

;; H2 fix (v0.99.6): Provider for verifier LLM calls.
;; Set from build-runtime-from-cli (wiring/run-modes.rkt).
(define current-verifier-provider (make-parameter #f))

;; ============================================================
;; Risk Level Utilities
;; ============================================================

;; Ordering for risk levels.
(define RISK-ORDER '(low medium high))

(define (risk-level->rank level)
  (or (for/or ([r (in-list RISK-ORDER)]
               [i (in-naturals)])
        (and (eq? r level) i))
      0))

(define (risk-at-or-above-threshold? risk-level threshold)
  (>= (risk-level->rank risk-level) (risk-level->rank threshold)))

;; ============================================================
;; Response Extraction
;; ============================================================

;; Extract text content from a model-response.
;; Content is a list of parts; text parts have a 'text key.
(define (extract-response-text resp)
  (define content (model-response-content resp))
  (string-join (for/list ([part (in-list content)])
                 (cond
                   [(hash? part) (hash-ref part 'text "")]
                   [(string? part) part]
                   [else (format "~a" part)]))
               ""))

;; ============================================================
;; JSON Response Parsing
;; ============================================================

;; Parse a JSON string from the LLM response into a verifier-decision.
;; Returns #f if parsing or validation fails.
(define (parse-verifier-response text)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define jsexpr (string->jsexpr (string-trim text)))
    (json->verifier-decision jsexpr)))

;; ============================================================
;; Risk Threshold Enforcement
;; ============================================================

;; If the decision's risk-level is at or above the configured threshold,
;; force requires-human? to #t. Otherwise, leave it unchanged.
(define (enforce-risk-threshold decision)
  (define risk (verifier-decision-risk-level decision))
  (define threshold (current-verifier-risk-threshold))
  (if (risk-at-or-above-threshold? risk threshold)
      (struct-copy verifier-decision decision [requires-human? #t])
      decision))

;; ============================================================
;; Main Entry Point
;; ============================================================

;; Run verification: build prompt, call LLM, parse response, return decision.
;; All failure modes produce safe decisions — never throws.
(define (run-verification provider
                          plan-summary
                          wave-name
                          files-changed
                          test-summary
                          #:diff-excerpt [diff-excerpt ""])
  ;; Provider errors → escalate (needs human review)
  (with-handlers ([exn:fail:network? (lambda (e)
                                       (make-escalate-decision (format "Provider network error: ~a"
                                                                       (exn-message e))
                                                               #:risk-level 'high
                                                               #:artifact-refs files-changed))]
                  [exn:fail? (lambda (e)
                               (make-escalate-decision (format "Provider error: ~a" (exn-message e))
                                                       #:risk-level 'high
                                                       #:artifact-refs files-changed))])
    ;; 1. Build prompts
    (define sys-prompt (build-verifier-system-prompt))
    (define user-msg
      (build-verifier-user-message #:plan-summary plan-summary
                                   #:wave-name wave-name
                                   #:files-changed files-changed
                                   #:test-summary test-summary
                                   #:diff-excerpt diff-excerpt))
    ;; 2. Build model request
    (define settings
      (let ([base (hasheq 'max_tokens 1000)])
        (if (current-verifier-model)
            (hash-set base 'model (current-verifier-model))
            base)))
    (define req
      (make-model-request (list (hasheq 'role "system" 'content sys-prompt)
                                (hasheq 'role "user" 'content user-msg))
                          #f
                          settings))
    ;; 3. Call LLM
    (define resp (provider-send provider req))
    ;; 4. Extract and parse response
    (define raw-text (extract-response-text resp))
    (define parsed (parse-verifier-response raw-text))
    ;; 5. Non-JSON or invalid → auto-reject with requires-human? #t
    (cond
      [(not parsed)
       (verifier-decision
        'reject
        (format "Verifier LLM returned unparseable response (not valid JSON decision): ~a"
                (if (> (string-length raw-text) 200)
                    (string-append (substring raw-text 0 200) "...")
                    raw-text))
        'high
        #t
        files-changed
        #f)]
      ;; 6. Enforce risk threshold
      [else (enforce-risk-threshold parsed)])))

;; ============================================================
;; Provides
;; ============================================================

(provide current-verifier-enabled
         current-verifier-model
         current-verifier-risk-threshold
         current-verifier-provider)

(provide (contract-out [run-verification
                        (->* (provider? string? string? (listof string?) string?)
                             (#:diff-excerpt string?)
                             verifier-decision?)]
                       [risk-at-or-above-threshold? (-> symbol? symbol? boolean?)]
                       [risk-level->rank (-> symbol? exact-nonnegative-integer?)]
                       [parse-verifier-response (-> string? (or/c verifier-decision? #f))]
                       [extract-response-text (-> model-response? string?)]
                       [enforce-risk-threshold (-> verifier-decision? verifier-decision?)]))
