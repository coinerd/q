#lang racket

;; @speed fast
;; @suite default

;; tests/test-browser-mock-adapter.rkt — Mock adapter tests
;;
;; Tests for browser/adapters/mock.rkt: deterministic responses,
;; call recording, error simulation, configurable responses.

(require rackunit
         "../browser/adapters/mock.rkt"
         "../browser/types.rkt"
         "../util/error/errors.rkt")

;; ---------------------------------------------------------------------------
;; Construction
;; ---------------------------------------------------------------------------

(test-case "make-mock-adapter constructs"
  (define a (make-mock-adapter))
  (check-true (mock-browser-adapter? a)))

(test-case "default delay is 0"
  (define a (make-mock-adapter))
  (check-equal? (mock-browser-adapter-delay-ms a) 0))

(test-case "default error-mode is #f"
  (define a (make-mock-adapter))
  (check-false (mock-browser-adapter-error-mode a)))

;; ---------------------------------------------------------------------------
;; Open / Close
;; ---------------------------------------------------------------------------

(test-case "mock-open returns observation"
  (define a (make-mock-adapter))
  (define obs (mock-open a "https://example.com" #f))
  (check-true (browser-observation? obs))
  (check-equal? (browser-observation-url obs) "https://example.com")
  (check-equal? (browser-observation-title obs) "Mock Page"))

(test-case "mock-open records call"
  (define a (make-mock-adapter))
  (mock-open a "https://example.com" #f)
  (define calls (mock-adapter-calls a))
  (check-equal? (length calls) 1)
  (check-equal? (first (first calls)) 'open))

(test-case "mock-close returns ok"
  (define a (make-mock-adapter))
  (check-equal? (mock-close a "s1") 'ok))

(test-case "mock-close records call"
  (define a (make-mock-adapter))
  (mock-close a "s1")
  (check-equal? (first (first (mock-adapter-calls a))) 'close))

;; ---------------------------------------------------------------------------
;; Navigate / Observe / Act
;; ---------------------------------------------------------------------------

(test-case "mock-navigate returns observation with url"
  (define a (make-mock-adapter))
  (define obs (mock-navigate a "s1" "https://example.com/page2"))
  (check-true (browser-observation? obs))
  (check-equal? (browser-observation-url obs) "https://example.com/page2"))

(test-case "mock-observe returns observation"
  (define a (make-mock-adapter))
  (define obs (mock-observe a "s1" "#main"))
  (check-true (browser-observation? obs))
  (check-not-false (browser-observation-text-content obs)))

(test-case "mock-act returns observation"
  (define a (make-mock-adapter))
  (define action (browser-action-click "#btn" "left"))
  (define obs (mock-act a "s1" action))
  (check-true (browser-observation? obs)))

(test-case "mock-screenshot returns observation with bytes"
  (define a (make-mock-adapter))
  (define obs (mock-screenshot a "s1" #f "png"))
  (check-true (browser-observation? obs))
  (check-true (bytes? (browser-observation-screenshot-bytes obs))))

;; ---------------------------------------------------------------------------
;; Call recording order
;; ---------------------------------------------------------------------------

(test-case "calls recorded in order"
  (define a (make-mock-adapter))
  (mock-open a "https://x.com" #f)
  (mock-navigate a "s1" "https://x.com/page")
  (mock-observe a "s1" "#main")
  (mock-close a "s1")
  (define calls (mock-adapter-calls a))
  (check-equal? (length calls) 4)
  (check-equal? (first (first calls)) 'open)
  (check-equal? (first (second calls)) 'navigate)
  (check-equal? (first (third calls)) 'observe)
  (check-equal? (first (fourth calls)) 'close))

;; ---------------------------------------------------------------------------
;; Error simulation
;; ---------------------------------------------------------------------------

(test-case "error-mode raises q-browser-error"
  (define a (make-mock-adapter #:error-mode 'timeout))
  (check-exn q-browser-error?
             (lambda ()
               (mock-open a "https://example.com" #f))))

(test-case "error-mode affects all calls"
  (define a (make-mock-adapter #:error-mode 'connection))
  (check-exn q-browser-error?
             (lambda () (mock-open a "https://x.com" #f)))
  (check-exn q-browser-error?
             (lambda () (mock-navigate a "s1" "https://x.com/p"))))

;; ---------------------------------------------------------------------------
;; Configurable responses
;; ---------------------------------------------------------------------------

(test-case "custom response for open"
  (define custom-obs (browser-observation "https://custom.com" "Custom" "text" "visible"
                                           #f #f #f #f '() '() (hash) (hash 'custom #t)))
  (define a (make-mock-adapter #:responses (hash 'open custom-obs)))
  (define obs (mock-open a "https://x.com" #f))
  (check-equal? (browser-observation-url obs) "https://custom.com"))

(test-case "custom response for navigate"
  (define custom-obs (browser-observation "https://new.com" "New Page" "text" "visible"
                                           #f #f #f #f '() '() (hash) (hash)))
  (define a (make-mock-adapter #:responses (hash 'navigate custom-obs)))
  (define obs (mock-navigate a "s1" "https://old.com"))
  (check-equal? (browser-observation-url obs) "https://new.com"))
