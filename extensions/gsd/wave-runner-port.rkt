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
