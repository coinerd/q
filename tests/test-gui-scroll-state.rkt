#lang racket/base

;; @speed fast
;; @suite default

(require rackunit
         rackunit/text-ui
         "../gui/components/scroll-state.rkt")

(define-test-suite test-scroll-state
  (test-case "gui-scroll-state: make-scroll-state defaults to auto-scroll enabled"
    (define ss (make-scroll-state))
    (check-true (scroll-state-auto-scroll? ss))
    (check-false (scroll-state-user-scrolled-up? ss)))

  (test-case "gui-scroll-state: make-scroll-state can be created with auto disabled"
    (define ss (make-scroll-state #f))
    (check-false (scroll-state-auto-scroll? ss)))

  (test-case "gui-scroll-state: scroll-state-on-scroll near bottom enables auto-scroll"
    (define ss (make-scroll-state #f))
    (define updated (scroll-state-on-scroll ss 0.97))
    (check-true (scroll-state-auto-scroll? updated))
    (check-false (scroll-state-user-scrolled-up? updated)))

  (test-case "gui-scroll-state: scroll-state-on-scroll mid-way disables auto-scroll"
    (define ss (make-scroll-state))
    (define updated (scroll-state-on-scroll ss 0.5))
    (check-false (scroll-state-auto-scroll? updated))
    (check-true (scroll-state-user-scrolled-up? updated)))

  (test-case "gui-scroll-state: scroll-state-on-scroll at top disables auto-scroll"
    (define ss (make-scroll-state))
    (define updated (scroll-state-on-scroll ss 0.0))
    (check-false (scroll-state-auto-scroll? updated)))

  (test-case "gui-scroll-state: scroll-state-on-submit re-enables auto-scroll"
    (define ss (hash 'auto-scroll #f 'scroll-ratio 0.3 'user-scrolled-up #t))
    (define updated (scroll-state-on-submit ss))
    (check-true (scroll-state-auto-scroll? updated))
    (check-false (scroll-state-user-scrolled-up? updated)))

  (test-case "gui-scroll-state: scroll-state-on-scroll boundary at 0.95"
    (define ss (make-scroll-state #f))
    (define at-boundary (scroll-state-on-scroll ss 0.95))
    (check-true (scroll-state-auto-scroll? at-boundary))
    (define below-boundary (scroll-state-on-scroll ss 0.94))
    (check-false (scroll-state-auto-scroll? below-boundary))))

(run-tests test-scroll-state)
