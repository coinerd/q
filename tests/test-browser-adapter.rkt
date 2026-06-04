#lang racket

;; tests/test-browser-adapter.rkt — Browser adapter interface tests
;;
;; Tests for browser/adapter.rkt: construction, delegation, edge cases.

(require rackunit
         "../browser/adapter.rkt"
         "../browser/types.rkt")

;; ---------------------------------------------------------------------------
;; Helpers — mock callback implementations
;; ---------------------------------------------------------------------------

(define call-log '())

(define (log-call! name . args)
  (set! call-log (append call-log (list (cons name args)))))

(define (mock-open sid target)
  (log-call! 'open sid target)
  'ok)

(define (mock-close sid)
  (log-call! 'close sid)
  'ok)

(define (mock-navigate sid url)
  (log-call! 'navigate sid url)
  (ok-navigate url))

(define (mock-observe sid selector)
  (log-call! 'observe sid selector)
  (make-observation "https://example.com"))

(define (mock-act sid action)
  (log-call! 'act sid action)
  'ok)

(define (mock-screenshot sid selector full-page?)
  (log-call! 'screenshot sid selector full-page?)
  #"PNG")

(define (make-observation url)
  (browser-observation url "Title" "text" "visible" #f #f #f #f '() '() #f (hasheq)))

(define (ok-navigate url)
  (browser-observation url "Title" "text" "visible" #f #f #f #f '() '() #f (hasheq)))

;; ---------------------------------------------------------------------------
;; Construction
;; ---------------------------------------------------------------------------

(test-case "make-browser-adapter creates a browser-adapter"
  (define a (make-browser-adapter
             #:open mock-open
             #:close mock-close
             #:navigate mock-navigate
             #:observe mock-observe
             #:act mock-act
             #:screenshot mock-screenshot))
  (check-true (browser-adapter? a)))

(test-case "browser-adapter is transparent"
  (define a (make-browser-adapter
             #:open mock-open #:close mock-close #:navigate mock-navigate
             #:observe mock-observe #:act mock-act #:screenshot mock-screenshot))
  (check-true (struct? a)))

;; ---------------------------------------------------------------------------
;; Delegation — each method calls the right callback
;; ---------------------------------------------------------------------------

(test-case "browser-adapter-open delegates to open-fn"
  (set! call-log '())
  (define a (make-browser-adapter
             #:open mock-open #:close mock-close #:navigate mock-navigate
             #:observe mock-observe #:act mock-act #:screenshot mock-screenshot))
  (define result (browser-adapter-open a "s1" "https://example.com"))
  (check-equal? result 'ok)
  (check-equal? call-log '((open . ("s1" . ("https://example.com"))))))

(test-case "browser-adapter-close delegates to close-fn"
  (set! call-log '())
  (define a (make-browser-adapter
             #:open mock-open #:close mock-close #:navigate mock-navigate
             #:observe mock-observe #:act mock-act #:screenshot mock-screenshot))
  (browser-adapter-close a "s1")
  (check-equal? call-log '((close . ("s1")))))

(test-case "browser-adapter-navigate delegates to navigate-fn"
  (set! call-log '())
  (define a (make-browser-adapter
             #:open mock-open #:close mock-close #:navigate mock-navigate
             #:observe mock-observe #:act mock-act #:screenshot mock-screenshot))
  (define result (browser-adapter-navigate a "s1" "https://example.com"))
  (check-true (browser-observation? result))
  (check-equal? (browser-observation-url result) "https://example.com"))

(test-case "browser-adapter-observe delegates to observe-fn"
  (set! call-log '())
  (define a (make-browser-adapter
             #:open mock-open #:close mock-close #:navigate mock-navigate
             #:observe mock-observe #:act mock-act #:screenshot mock-screenshot))
  (define result (browser-adapter-observe a "s1"))
  (check-true (browser-observation? result))
  (check-equal? call-log '((observe . ("s1" . (#f))))))

(test-case "browser-adapter-observe with selector"
  (set! call-log '())
  (define a (make-browser-adapter
             #:open mock-open #:close mock-close #:navigate mock-navigate
             #:observe mock-observe #:act mock-act #:screenshot mock-screenshot))
  (browser-adapter-observe a "s1" #:selector "#main")
  (check-equal? call-log '((observe . ("s1" . ("#main"))))))

(test-case "browser-adapter-act delegates to act-fn"
  (set! call-log '())
  (define a (make-browser-adapter
             #:open mock-open #:close mock-close #:navigate mock-navigate
             #:observe mock-observe #:act mock-act #:screenshot mock-screenshot))
  (define action (browser-action-click "#btn" "left"))
  (browser-adapter-act a "s1" action)
  (check-equal? (car (car call-log)) 'act))

(test-case "browser-adapter-screenshot delegates to screenshot-fn"
  (set! call-log '())
  (define a (make-browser-adapter
             #:open mock-open #:close mock-close #:navigate mock-navigate
             #:observe mock-observe #:act mock-act #:screenshot mock-screenshot))
  (define result (browser-adapter-screenshot a "s1"))
  (check-true (bytes? result))
  (check-equal? call-log '((screenshot . ("s1" . (#f . (#f)))))))

(test-case "browser-adapter-screenshot with options"
  (set! call-log '())
  (define a (make-browser-adapter
             #:open mock-open #:close mock-close #:navigate mock-navigate
             #:observe mock-observe #:act mock-act #:screenshot mock-screenshot))
  (browser-adapter-screenshot a "s1" #:selector "#main" #:full-page? #t)
  (check-equal? call-log '((screenshot . ("s1" . ("#main" . (#t)))))))
