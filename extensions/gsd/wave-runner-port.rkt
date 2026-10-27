#lang racket/base
;; STABILITY: internal

;; extensions/gsd/wave-runner-port.rkt — Wave executor boundary (v0.99.90 W3)
;;
;; ONE structured terminal outcome per wave invocation. The domain
;; (go-orchestrator) decides status from the outcome kind; the interface
;; supplies an executor port (tool/subagent execution) with explicit
;; cancellation. No raw symbols leak into the domain switch and no runtime
;; session structs (loop-result) cross this boundary — the interface adapter
;; translates them into a wave-execution-outcome before calling the domain.
;;
;; Terminal outcomes (exactly one per invocation):
;;   done         — wave work finished; verifier may still reject
;;   failed       — wave work errored
;;   cancelled    — user/durable cancellation observed
;;   timed-out    — deadline exceeded (run-wave-with-timeout)
;;   interrupted  — force interrupt (e.g. shutdown)

(require racket/contract)

(provide wave-execution-outcome
         wave-execution-outcome?
         wave-execution-outcome-kind
         wave-execution-outcome-message
         ;; GSD simplification W5: the typed failure CAUSE rides an additive
         ;; transparent subtype (historical 2-field struct line unchanged).
         typed-wave-execution-outcome
         typed-wave-execution-outcome?
         typed-wave-execution-outcome-cause
         wave-execution-outcome/typed
         wave-execution-outcome-cause
         OUTCOME-CAUSE-STALL
         OUTCOME-CAUSE-PROVIDER
         OUTCOME-CAUSE-EMPTY-RESPONSE
         make-wave-runner-port
         coerce-run-result
         (contract-out (struct gsd-wave-runner-port
                               ((run (-> exact-nonnegative-integer? wave-execution-outcome?))
                                (cancel! (-> void?))
                                (cancel-requested? (-> boolean?))))))

;; ============================================================
;; Terminal outcome
;; ============================================================

(define (valid-outcome-kind? k)
  ;; D8 (#9357): 'infra-failed — transient provider/network/SSE failure that
  ;; run-campaign-wave resolves to wave-cancelled without consuming the attempt.
  (memq k '(done failed cancelled timed-out interrupted infra-failed)))

(struct wave-execution-outcome (kind message)
  #:transparent
  #:guard
  (lambda (kind message name)
    (unless (valid-outcome-kind? kind)
      (raise-arguments-error
       name
       "invalid terminal outcome kind (expected done|failed|cancelled|timed-out|interrupted|infra-failed)"
       "kind"
       kind))
    (unless (string? message)
      (raise-arguments-error name "outcome message must be a string" "message" message))
    (values kind message)))

;; ============================================================
;; Typed failure cause (GSD simplification W5)
;; ============================================================

;; The ONE typed cause vocabulary for a terminal failure. It is ADDITIVE to the
;; historical kind switch above: a consumer that must distinguish "stalled"
;; from "provider blew up" reads this symbol, never the human message. Legacy
;; 2-argument constructions carry no cause (#f); the adapters that own the
;; typed knowledge (stall watchdog, prompt-result translation) set it.
(define OUTCOME-CAUSE-STALL 'stall)
(define OUTCOME-CAUSE-PROVIDER 'provider)
(define OUTCOME-CAUSE-EMPTY-RESPONSE 'empty-response)

;; The historical struct line above stays VERBATIM; the cause rides a
;; transparent subtype so every existing constructor call site, accessor and
;; match pattern is unchanged (the same pattern campaign-result.rkt uses).
(struct typed-wave-execution-outcome wave-execution-outcome (cause) #:transparent)

;; Typed producer constructor. `cause` must be a symbol from the vocabulary.
(define (wave-execution-outcome/typed kind message cause)
  (unless (memq cause (list OUTCOME-CAUSE-STALL OUTCOME-CAUSE-PROVIDER OUTCOME-CAUSE-EMPTY-RESPONSE))
    (raise-argument-error 'wave-execution-outcome/typed "a typed outcome cause symbol" cause))
  (typed-wave-execution-outcome kind message cause))

;; The typed cause of an outcome, or #f for a legacy value.
(define (wave-execution-outcome-cause outcome)
  (and (typed-wave-execution-outcome? outcome) (typed-wave-execution-outcome-cause outcome)))

;; ============================================================
;; Executor port
;; ============================================================

(struct gsd-wave-runner-port (run cancel! cancel-requested?) #:transparent)

;; Wrap a plain run function into a runner port. Cancellation defaults to
;; inert so plain function runners keep working unchanged.
(define (make-wave-runner-port run-fn
                               #:cancel! [cancel-fn void]
                               #:cancel-requested? [requested? (lambda () #f)])
  (gsd-wave-runner-port run-fn cancel-fn requested?))

;; ============================================================
;; Legacy symbol coercion (backward compatibility)
;; ============================================================

;; Map pre-W3 symbol runner results to structured outcomes. Unknown symbols
;; fail closed to 'failed so a typo can never be mistaken for success.
(define (coerce-run-result result)
  (cond
    [(wave-execution-outcome? result) result]
    [(eq? result 'ok) (wave-execution-outcome 'done "runner ok")]
    ;; runners commonly return 'done (the completion status symbol);
    ;; accept it as success rather than failing closed on a typo check
    [(eq? result 'done) (wave-execution-outcome 'done "runner ok")]
    [(eq? result 'error) (wave-execution-outcome 'failed "runner error")]
    [(eq? result 'failed) (wave-execution-outcome 'failed "runner failed")]
    [(eq? result 'cancelled) (wave-execution-outcome 'cancelled "runner cancelled")]
    [else (wave-execution-outcome 'failed (format "unknown runner result: ~s" result))]))
