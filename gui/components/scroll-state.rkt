#lang racket/base

;; q/gui/components/scroll-state.rkt — Pure scroll-state management
;;
;; Headless-testable auto-scroll logic for the GUI transcript.

(require racket/contract)

(provide (contract-out
          [make-scroll-state (->* () (boolean?) hash?)]
          [scroll-state-auto-scroll? (-> hash? boolean?)]
          [scroll-state-user-scrolled-up? (-> hash? boolean?)]
          [scroll-state-on-scroll (-> hash? (between/c 0.0 1.0) hash?)]
          [scroll-state-on-submit (-> hash? hash?)]))

(define (make-scroll-state [auto? #t])
  (hash 'auto-scroll auto?
        'scroll-ratio 1.0
        'user-scrolled-up #f))

(define (scroll-state-auto-scroll? ss)
  (hash-ref ss 'auto-scroll #t))

(define (scroll-state-user-scrolled-up? ss)
  (hash-ref ss 'user-scrolled-up #f))

(define (scroll-state-on-scroll ss ratio)
  (cond
    [(>= ratio 0.95)
     (hash-set (hash-set ss 'auto-scroll #t) 'user-scrolled-up #f)]
    [else
     (hash-set (hash-set ss 'auto-scroll #f) 'user-scrolled-up #t)]))

(define (scroll-state-on-submit ss)
  (hash-set (hash-set ss 'auto-scroll #t) 'user-scrolled-up #f))
