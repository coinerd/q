#lang racket

;; @speed fast
;; @suite arch
;; @boundary architecture

;; tests/test-request-policy-architecture.rkt
;; v1.00.13 (RL-1..RL-3/RL-10, AC-1/AC-2): architecture gate for request
;; lifecycle policy ownership.
;;
;; End-state rules (see PLAN-v1.00.13 §7):
;;   R1 raw legacy timeout accessors (`effective-sse-read-timeout-for`,
;;      `effective-request-timeout-for`) are consumed only by the policy
;;      module (their definitions live in llm/request-policy.rkt since W1;
;;      llm/stream.rkt re-exports them for compatibility).
;;   R2 the generic total-budget formula (`(max 600 ...)`) is authored only
;;      by the policy module.
;;   R3 the phase resolver (`sse-phase-timeout-secs`) is consumed only by the
;;      policy module.
;;   R4 every provider `stream-sse-events` call passes the full resolved
;;      policy kwargs (#:initial-timeout, #:thinking-timeout,
;;      #:stream-timeout, #:max-total-timeout) — no default-reliance.
;;   R5 the legacy per-model timeout parameters (`current-model-sse-read-timeouts`)
;;      are referenced only by the policy module (owner) and the runtime
;;      wiring (wiring/mode-helpers.rkt, raw config plumbing — permanent
;;      legitimate consumer, not scanned).
;;
;; History: committed red in W0 (#9454) with an empty allowlist; promoted in
;; W1 (#9461) with the transitional adapter allowlist below; W2 (#9466) and
;; W4 (#9478) shrink it to empty.

(require rackunit
         racket/runtime-path
         racket/file
         racket/string)

(define-runtime-path repo-root "..")

(define (source relative)
  (file->string (build-path repo-root relative)))

(define (file-exists/relative? relative)
  (file-exists? (build-path repo-root relative)))

;; Provider adapters under the policy ownership rule.
(define adapter-files
  '("llm/openai-compatible.rkt" "llm/anthropic.rkt"
                                "llm/anthropic/sse.rkt"
                                "llm/gemini.rkt"
                                "llm/azure-openai.rkt"
                                "llm/openrouter.rkt"))

(define mechanism-files '("llm/stream.rkt" "llm/http-helpers.rkt"))

;; Temporary allowlist: (file rule-symbol) pairs.
;; W0 (#9454): EMPTY — the strict rule was red on the pre-W1 tree by design.
;; W1 (#9461): populated with the transitional adapter consumers.
;; W2 (#9466): all four adapters consume the resolved policy; raw accessors,
;;             local formulas, and default stream-sse-events calls are gone —
;;             the allowlist is EMPTY again (end state; W4 re-verifies).
(define temporary-allowlist '())

(define (allowed? file rule)
  (member (list file rule) temporary-allowlist))

(define (violating-files files pattern rule [skip-missing? #t])
  (for/list ([f (in-list files)]
             #:when (and (or (not skip-missing?) (file-exists/relative? f))
                         (string-contains? (source f) pattern)
                         (not (allowed? f rule))))
    f))

(test-case "R1: adapters do not consume raw legacy timeout accessors"
  (define offenders
    (append (violating-files adapter-files "effective-sse-read-timeout-for" 'raw-accessor)
            (violating-files adapter-files "effective-request-timeout-for" 'raw-accessor)))
  (check-equal? offenders
                '()
                (format "raw timeout config must be consumed only by the policy module: ~a"
                        offenders)))

(test-case "R2: the generic total-budget formula has one owner"
  (define offenders
    (violating-files (append adapter-files mechanism-files) "(max 600" 'total-formula))
  (check-equal?
   offenders
   '()
   (format "(max 600 ...) total-budget assembly belongs to llm/request-policy.rkt only: ~a"
           offenders)))

(test-case "R3: the phase resolver is consumed only by the policy module"
  (define offenders
    (violating-files (append adapter-files (list "llm/http-helpers.rkt"))
                     "sse-phase-timeout-secs"
                     'phase-resolver))
  (check-equal? offenders
                '()
                (format "sse-phase-timeout-secs semantics move behind the policy module in W1: ~a"
                        offenders)))

(test-case "R4: provider stream-sse-events calls pass the full resolved policy"
  (define offenders
    (for/list ([f (in-list adapter-files)]
               #:when (file-exists/relative? f)
               #:when (string-contains? (source f) "(stream-sse-events")
               #:when (not (allowed? f 'default-timeouts))
               #:when (not (and (string-contains? (source f) "(stream-sse-events")
                                (string-contains? (source f) "#:max-total-timeout")
                                (string-contains? (source f) "#:initial-timeout")
                                (string-contains? (source f) "#:thinking-timeout"))))
      f))
  (check-equal? offenders
                '()
                (format "adapters must pass the resolved policy kwargs explicitly: ~a" offenders)))

(test-case "R5: legacy per-model timeout parameters have a closed reference set"
  ;; wiring/mode-helpers.rkt is the permanent raw-config plumbing consumer and
  ;; is therefore not scanned; llm/stream.rkt re-exports for compatibility.
  (define offenders
    (violating-files (append adapter-files (list "llm/http-helpers.rkt"))
                     "current-model-sse-read-timeouts"
                     'legacy-parameters))
  (check-equal?
   offenders
   '()
   (format
    "only the policy module + runtime wiring + compat definition may touch the legacy parameters: ~a"
    offenders)))

(test-case "the policy module exists (W1) and owns the legacy translation"
  (check-true (file-exists/relative? "llm/request-policy.rkt")
              "llm/request-policy.rkt must exist after W1 (#9461)")
  (when (file-exists/relative? "llm/request-policy.rkt")
    (define policy (source "llm/request-policy.rkt"))
    (check-true (string-contains? policy "sse-read")
                "the policy module owns the legacy sse-read compatibility mapping")
    (check-true (string-contains? policy "request-network-policy")
                "the policy module defines request-network-policy")))
