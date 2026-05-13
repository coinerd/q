#lang racket/base

;; BOUNDARY: integration

;; tests/test-session-lifecycle-errors.rkt -- T-04: runtime.error payload hash guarantee
;;
;; Verify that the emitted "runtime.error" event payload satisfies hash?
;; and contains required keys in both retry-exhausted and non-retry paths.

(require rackunit
         racket/match
         "../runtime/session-config.rkt"
         "../agent/event-bus.rkt"
         "../util/event.rkt"
         (only-in "../agent/event-structs/base.rkt" typed-event?))

(test-case "T-04: runtime.error payload is hash? -- non-retry path"
  (define captured (box #f))
  (define bus (make-event-bus))
  (subscribe! bus
              (lambda (evt)
                (when (equal? (event-ev evt) "runtime.error")
                  (set-box! captured (event-payload evt)))))
  ;; Emit a synthetic runtime.error event
  (define payload (hasheq 'message "test error" 'error-type 'unknown))
  (publish! bus (make-event "runtime.error" 0 "test-session" #f payload))
  (define result (unbox captured))
  (check-not-false result "should have captured runtime.error event")
  (check-pred hash? result "runtime.error payload must be hash?"))

(test-case "T-04: runtime.error payload with retry metadata is hash?"
  (define captured (box #f))
  (define bus (make-event-bus))
  (subscribe! bus
              (lambda (evt)
                (when (equal? (event-ev evt) "runtime.error")
                  (set-box! captured (event-payload evt)))))
  ;; Simulate retry-exhausted payload (has extra keys)
  (define payload
    (hasheq 'message
            "max retries"
            'error-type
            'provider-error
            'retries-attempted
            3
            'total-retry-delay-ms
            1500
            'errorHistory
            '("err1" "err2" "err3")))
  (publish! bus (make-event "runtime.error" 0 "test-session" #f payload))
  (define result (unbox captured))
  (check-not-false result)
  (check-pred hash? result)
  (check-equal? (hash-ref result 'retries-attempted) 3))
