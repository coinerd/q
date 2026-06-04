#lang racket

;; browser/adapters/mock.rkt — Deterministic mock browser adapter
;;
;; Returns predictable observations based on input configuration.
;; Used for all browser testing without Node.js/Playwright.

(require racket/match
         "../types.rkt"
         "../../agent/event-structs/base.rkt"
         "../../util/error/errors.rkt")

(provide
 (struct-out mock-browser-adapter)
 make-mock-adapter
 mock-adapter-calls
 mock-open
 mock-close
 mock-navigate
 mock-observe
 mock-act
 mock-screenshot)

;; ---------------------------------------------------------------------------
;; Mock adapter struct
;; ---------------------------------------------------------------------------

(struct mock-browser-adapter
  (responses   ; (hash/c symbol? any) — action-type → response
   calls       ; box? (listof (list/c symbol? any)) — recorded calls
   delay-ms    ; exact-nonnegative-integer? — simulated latency
   error-mode) ; (or/c #f symbol?) — if set, raise this error on next call
  #:transparent)

;; ---------------------------------------------------------------------------
;; Constructor
;; ---------------------------------------------------------------------------

(define (make-mock-adapter #:responses [responses (make-hash)]
                           #:delay-ms [delay-ms 0]
                           #:error-mode [error-mode #f])
  (mock-browser-adapter responses (box '()) delay-ms error-mode))

;; ---------------------------------------------------------------------------
;; Default observation helper
;; ---------------------------------------------------------------------------

(define (make-default-observation #:url [url "about:blank"])
  (browser-observation url "Mock Page" "mock text content" "visible mock text"
                       #f #f #f #f '() '() (hash 'width 1280 'height 720)
                       (hash 'mock #t)))

;; ---------------------------------------------------------------------------
;; Record calls for verification
;; ---------------------------------------------------------------------------

(define (mock-adapter-calls adapter)
  (reverse (unbox (mock-browser-adapter-calls adapter))))

(define (record-call! adapter action-type args)
  (define calls-box (mock-browser-adapter-calls adapter))
  (set-box! calls-box (cons (list action-type args) (unbox calls-box))))

;; ---------------------------------------------------------------------------
;; Adapter methods (match browser-adapter interface)
;; ---------------------------------------------------------------------------

(define (mock-open adapter url options)
  (record-call! adapter 'open (list url options))
  (maybe-error adapter)
  (maybe-delay adapter)
  (define resp (hash-ref (mock-browser-adapter-responses adapter) 'open #f))
  (or resp (make-default-observation #:url url)))

(define (mock-close adapter session-id)
  (record-call! adapter 'close (list session-id))
  (maybe-error adapter)
  (maybe-delay adapter)
  (define resp (hash-ref (mock-browser-adapter-responses adapter) 'close #f))
  (or resp 'ok))

(define (mock-navigate adapter session-id url)
  (record-call! adapter 'navigate (list session-id url))
  (maybe-error adapter)
  (maybe-delay adapter)
  (define resp (hash-ref (mock-browser-adapter-responses adapter) 'navigate #f))
  (or resp (make-default-observation #:url url)))

(define (mock-observe adapter session-id selector)
  (record-call! adapter 'observe (list session-id selector))
  (maybe-error adapter)
  (maybe-delay adapter)
  (define resp (hash-ref (mock-browser-adapter-responses adapter) 'observe #f))
  (or resp (make-default-observation #:url (format "mock://~a" selector))))

(define (mock-act adapter session-id action)
  (record-call! adapter 'act (list session-id action))
  (maybe-error adapter)
  (maybe-delay adapter)
  (define resp (hash-ref (mock-browser-adapter-responses adapter) 'act #f))
  (or resp (make-default-observation #:url "mock://action")))

(define (mock-screenshot adapter session-id selector format)
  (record-call! adapter 'screenshot (list session-id selector format))
  (maybe-error adapter)
  (maybe-delay adapter)
  (define resp (hash-ref (mock-browser-adapter-responses adapter) 'screenshot #f))
  (or resp (browser-observation "mock://screenshot" "Screenshot" "" ""
                                 #f #f format (make-bytes 10 0) '() '()
                                 (hash 'width 1280 'height 720) (hash 'mock #t))))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (maybe-error adapter)
  (define mode (mock-browser-adapter-error-mode adapter))
  (when mode
    (raise-browser-error
     (format "mock error: ~a" mode)
     mode
     (hash 'source 'mock-adapter))))

(define (maybe-delay adapter)
  (define ms (mock-browser-adapter-delay-ms adapter))
  (when (> ms 0)
    (sleep (/ ms 1000.0))))
