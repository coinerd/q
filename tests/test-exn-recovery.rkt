#lang racket/base

;; @speed fast  ;; @suite default
;; @boundary unit
;; Tests for util/exn.rkt — stream-error exception with recovery data.

(require rackunit
         racket/list
         rackunit/text-ui
         "../util/exn.rkt")

(define exn-suite
  (test-suite "exn:fail:stream-error recovery data"

    (test-case "constructor carries all fields"
      (define orig (exn:fail "timeout" (current-continuation-marks)))
      (define e
        (exn:fail:stream-error "stream error"
                               (current-continuation-marks)
                               "partial text"
                               (list 'msg1 'msg2)
                               orig))
      (check-pred exn:fail:stream-error? e)
      (check-equal? (exn:fail:stream-error-partial-text e) "partial text")
      (check-equal? (exn:fail:stream-error-partial-messages e) (list 'msg1 'msg2))
      (check-eq? (exn:fail:stream-error-original-exn e) orig))

    (test-case "is also exn:fail?"
      (define e (exn:fail:stream-error "test" (current-continuation-marks) #f '() #f))
      (check-pred exn:fail? e))

    (test-case "partial-text can be #f (no partial output)"
      (define e (exn:fail:stream-error "test" (current-continuation-marks) #f '() #f))
      (check-false (exn:fail:stream-error-partial-text e)))

    (test-case "partial-messages can be empty"
      (define e (exn:fail:stream-error "test" (current-continuation-marks) "text" '() #f))
      (check-equal? (exn:fail:stream-error-partial-messages e) '()))

    (test-case "can be raised and caught"
      (define orig (exn:fail "orig" (current-continuation-marks)))
      (define result
        (with-handlers ([exn:fail:stream-error? (lambda (e)
                                                  (list (exn:fail:stream-error-partial-text e)
                                                        (exn:fail:stream-error-original-exn e)))])
          (raise (exn:fail:stream-error "wrapped" (current-continuation-marks) "partial" '() orig))))
      (check-equal? (first result) "partial")
      (check-eq? (second result) orig))

    (test-case "exn-message matches wrapper message, not original"
      (define orig (exn:fail "original error" (current-continuation-marks)))
      (define e
        (exn:fail:stream-error "wrapped: original error"
                               (current-continuation-marks)
                               "text"
                               '()
                               orig))
      (check-equal? (exn-message e) "wrapped: original error"))))

(run-tests exn-suite 'verbose)
