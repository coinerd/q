#lang racket

;; tests/test-hook-dispatch-transitions.rkt — Wave 12: E1-E8 extension hook dispatch
;;
;; Tests for hook dispatch ordering, timeout behavior, error handling,
;; critical vs advisory defaults, multi-handler semantics, and zero-handler case.

(require rackunit
         rackunit/text-ui
         "../util/protocol-types.rkt"
         "../util/hook-types.rkt"
         "../extensions/hooks.rkt"
         (only-in "../extensions/api.rkt"
                  make-extension-registry register-extension!
                  extension list-extensions))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-ext-reg-with hook-point handler #:name [name "test-ext"])
  (define reg (make-extension-registry))
  (define ext (extension name "0.1" "1.0" (hasheq hook-point handler)))
  (register-extension! reg ext)
  reg)

(define (make-ext-reg-with-2 hook-point handler1 handler2
                              #:name1 [name1 "ext-a"]
                              #:name2 [name2 "ext-b"])
  (define reg (make-extension-registry))
  (define ext1 (extension name1 "0.1" "1.0" (hasheq hook-point handler1)))
  (define ext2 (extension name2 "0.1" "1.0" (hasheq hook-point handler2)))
  (register-extension! reg ext1)
  (register-extension! reg ext2)
  reg)

;; ============================================================
;; Tests
;; ============================================================

(define hook-dispatch-tests
  (test-suite
   "Extension Hook Dispatch Transition Tests"

   ;; ============================================================
   ;; E1: Hook timeout on slow handler returns error-default
   ;; ============================================================
   (test-case
    "E1: hook timeout on slow handler returns error-default"
    ;; Use a non-critical hook point so timeout defaults to 'pass
    (parameterize ([current-hook-timeout-ms 10])
      (define (slow-handler payload)
        (sleep 0.1) ; way longer than 10ms
        (hook-amend "should not reach"))
      (define reg (make-ext-reg-with 'message-end slow-handler))
      (define result (dispatch-hooks 'message-end "original" reg))
      ;; Advisory hook times out → pass with original payload
      (check-equal? (hook-result-action result) 'pass
                    "slow advisory hook returns pass on timeout")
      (check-equal? (hook-result-payload result) "original"
                    "original payload preserved on timeout")))

   ;; ============================================================
   ;; E2: Critical hook timeout → block default
   ;; ============================================================
   (test-case
    "E2: critical hook timeout returns block default"
    (parameterize ([current-hook-timeout-ms 10])
      (define (slow-handler payload)
        (sleep 0.1)
        (hook-amend "should not reach"))
      ;; 'tool-call is a critical hook
      (define reg (make-ext-reg-with 'tool-call slow-handler))
      (define result (dispatch-hooks 'tool-call "payload" reg))
      (check-equal? (hook-result-action result) 'block
                    "slow critical hook returns block on timeout")))

   ;; ============================================================
   ;; E3: Advisory hook timeout → pass default
   ;; ============================================================
   (test-case
    "E3: advisory hook timeout returns pass default"
    (parameterize ([current-hook-timeout-ms 10])
      (define (slow-handler payload)
        (sleep 0.1)
        (hook-block "should not reach"))
      ;; 'message-end is advisory
      (define reg (make-ext-reg-with 'message-end slow-handler))
      (define result (dispatch-hooks 'message-end "data" reg))
      (check-equal? (hook-result-action result) 'pass
                    "slow advisory hook returns pass on timeout")
      (check-equal? (hook-result-payload result) "data"
                    "original payload preserved")))

   ;; ============================================================
   ;; E4: Hook exception on critical → block
   ;; ============================================================
   (test-case
    "E4: hook exception on critical hook returns block"
      (define (failing-handler payload)
        (error "handler boom!"))
      ;; 'tool-call is critical
      (define reg (make-ext-reg-with 'tool-call failing-handler))
      (define result (dispatch-hooks 'tool-call "payload" reg))
      (check-equal? (hook-result-action result) 'block
                    "critical hook exception returns block"))

   ;; ============================================================
   ;; E5: Hook exception on advisory → pass
   ;; ============================================================
   (test-case
    "E5: hook exception on advisory hook returns pass"
      (define (failing-handler payload)
        (error "handler boom!"))
      ;; 'message-end is advisory
      (define reg (make-ext-reg-with 'message-end failing-handler))
      (define result (dispatch-hooks 'message-end "payload" reg))
      (check-equal? (hook-result-action result) 'pass
                    "advisory hook exception returns pass")
      (check-equal? (hook-result-payload result) "payload"
                    "original payload preserved on advisory exception"))

   ;; ============================================================
   ;; E6: Multiple handlers: first amends, second blocks → block wins
   ;; ============================================================
   (test-case
    "E6: multiple handlers — first amends, second blocks → block wins"
    (define call-log (box '()))
    (define (amender payload)
      (set-box! call-log (cons 'amend (unbox call-log)))
      (hook-amend (string-append payload "-amended")))
    (define (blocker payload)
      (set-box! call-log (cons 'block (unbox call-log)))
      (hook-block "blocked by second"))
    (define reg (make-ext-reg-with-2 'message-end amender blocker))
    (define result (dispatch-hooks 'message-end "original" reg))
    (check-equal? (hook-result-action result) 'block
                  "block from second handler wins")
    ;; Both handlers should have been called
    (check-equal? (length (unbox call-log)) 2
                  "both handlers were called"))

   ;; ============================================================
   ;; E7: Multiple handlers: first blocks → second never called
   ;; ============================================================
   (test-case
    "E7: multiple handlers — first blocks → short-circuit"
    (define call-log (box '()))
    (define (blocker payload)
      (set-box! call-log (cons 'block (unbox call-log)))
      (hook-block "blocked by first"))
    (define (should-not-run payload)
      (set-box! call-log (cons 'should-not-run (unbox call-log)))
      (hook-amend "never"))
    (define reg (make-ext-reg-with-2 'tool-call blocker should-not-run))
    (define result (dispatch-hooks 'tool-call "payload" reg))
    (check-equal? (hook-result-action result) 'block
                  "first handler blocks")
    (check-equal? (unbox call-log) '(block)
                  "second handler never called"))

   ;; ============================================================
   ;; E8: dispatch-hooks with zero handlers returns hook-pass
   ;; ============================================================
   (test-case
    "E8: dispatch-hooks with zero handlers returns hook-pass"
    (define reg (make-extension-registry))
    ;; No extensions registered
    (define result (dispatch-hooks 'message-end "payload" reg))
    (check-equal? (hook-result-action result) 'pass
                  "zero handlers returns pass")
    (check-equal? (hook-result-payload result) "payload"
                  "original payload returned with pass"))))

(module+ main
  (run-tests hook-dispatch-tests))

(module+ test
  (run-tests hook-dispatch-tests))
