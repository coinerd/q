#lang racket

;; tests/test-cancellation.rkt — unit tests for util/cancellation.rkt
;;
;; Covers:
;;   1. make-cancellation-token creates uncancelled token
;;   2. cancellation-token? predicate
;;   3. cancellation-token-cancelled? returns #f initially
;;   4. cancel-token! sets cancelled
;;   5. Callback fires on cancel
;;   6. Double cancel fires callback twice (documented behavior)

(require rackunit
         rackunit/text-ui
         "../util/cancellation.rkt")

;; ============================================================
;; Test suite
;; ============================================================

(define-test-suite test-cancellation-suite

  (test-case "make-cancellation-token creates uncancelled token"
    (define tok (make-cancellation-token))
    (check-pred cancellation-token? tok)
    (check-false (cancellation-token-cancelled? tok)))

  (test-case "cancellation-token? predicate works"
    (check-true (cancellation-token? (make-cancellation-token)))
    (check-false (cancellation-token? "not a token"))
    (check-false (cancellation-token? 42))
    (check-false (cancellation-token? #f)))

  (test-case "cancellation-token-cancelled? returns #f initially"
    (define tok (make-cancellation-token))
    (check-false (cancellation-token-cancelled? tok))
    (check-false (cancellation-token-cancelled? (make-cancellation-token #:callback (λ (_) (void))))))

  (test-case "cancel-token! sets cancelled"
    (define tok (make-cancellation-token))
    (cancel-token! tok)
    (check-true (cancellation-token-cancelled? tok)))

  (test-case "cancel-token! returns the token"
    (define tok (make-cancellation-token))
    (define result (cancel-token! tok))
    (check-eq? result tok "cancel-token! returns the token"))

  (test-case "callback fires on cancel"
    (define fired (box #f))
    (define tok (make-cancellation-token #:callback (lambda (t) (set-box! fired #t))))
    (cancel-token! tok)
    (check-true (unbox fired)))

  (test-case "callback receives the token as argument"
    (define received-tok (box #f))
    (define tok (make-cancellation-token #:callback (lambda (t) (set-box! received-tok t))))
    (cancel-token! tok)
    (check-eq? (unbox received-tok) tok))

  (test-case "double cancel fires callback twice (documented behavior)"
    (define call-count (box 0))
    (define tok (make-cancellation-token #:callback (lambda (_) (set-box! call-count (add1 (unbox call-count))))))
    (cancel-token! tok)
    (check-pred cancellation-token-cancelled? tok)
    (check-equal? (unbox call-count) 1 "callback fires once after first cancel")
    (cancel-token! tok)
    (check-pred cancellation-token-cancelled? tok)
    (check-equal? (unbox call-count) 2 "callback fires again on second cancel"))

  (test-case "token with no callback works fine"
    (define tok (make-cancellation-token))
    (cancel-token! tok)
    (check-true (cancellation-token-cancelled? tok)))

  (test-case "token with #f callback works fine"
    (define tok (make-cancellation-token #:callback #f))
    (cancel-token! tok)
    (check-true (cancellation-token-cancelled? tok)))

  (test-case "multiple tokens are independent"
    (define tok1 (make-cancellation-token))
    (define tok2 (make-cancellation-token))
    (cancel-token! tok1)
    (check-pred cancellation-token-cancelled? tok1)
    (check-false (cancellation-token-cancelled? tok2))))

;; ============================================================
;; Run
;; ============================================================

(run-tests test-cancellation-suite)
