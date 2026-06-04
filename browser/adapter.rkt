#lang racket

;; browser/adapter.rkt — Browser adapter contract
;;
;; Defines the adapter interface that browser backends must implement.
;; Real implementations (mock, playwright) live in browser/adapters/.
;; Contracts guard all public functions.

(require racket/contract
         "types.rkt")

(provide (struct-out browser-adapter)
         (contract-out [make-browser-adapter
                        (-> #:open (-> string? any/c any)
                            #:close (-> string? any)
                            #:navigate (-> string? string? any)
                            #:observe (-> string? (or/c string? #f) any)
                            #:act (-> string? browser-action? any)
                            #:screenshot (-> string? (or/c string? #f) boolean? any)
                            browser-adapter?)]
                       [browser-adapter-open (-> browser-adapter? string? any/c any)]
                       [browser-adapter-close (-> browser-adapter? string? any)]
                       [browser-adapter-navigate (-> browser-adapter? string? string? any)]
                       [browser-adapter-observe
                        (->* [browser-adapter? string?] [#:selector (or/c string? #f)] any)]
                       [browser-adapter-act (-> browser-adapter? string? browser-action? any)]
                       [browser-adapter-screenshot
                        (->* [browser-adapter? string?]
                             [#:selector (or/c string? #f) #:full-page? boolean?]
                             any)]))

;; ---------------------------------------------------------------------------
;; Adapter struct — wraps function callbacks
;; ---------------------------------------------------------------------------

(struct browser-adapter (open-fn close-fn navigate-fn observe-fn act-fn screenshot-fn) #:transparent)

(define (make-browser-adapter #:open open-fn
                              #:close close-fn
                              #:navigate navigate-fn
                              #:observe observe-fn
                              #:act act-fn
                              #:screenshot screenshot-fn)
  (browser-adapter open-fn close-fn navigate-fn observe-fn act-fn screenshot-fn))

(define (browser-adapter-open adapter session-id target)
  ((browser-adapter-open-fn adapter) session-id target))

(define (browser-adapter-close adapter session-id)
  ((browser-adapter-close-fn adapter) session-id))

(define (browser-adapter-navigate adapter session-id url)
  ((browser-adapter-navigate-fn adapter) session-id url))

(define (browser-adapter-observe adapter session-id #:selector [selector #f])
  ((browser-adapter-observe-fn adapter) session-id selector))

(define (browser-adapter-act adapter session-id action)
  ((browser-adapter-act-fn adapter) session-id action))

(define (browser-adapter-screenshot adapter
                                    session-id
                                    #:selector [selector #f]
                                    #:full-page? [full-page? #f])
  ((browser-adapter-screenshot-fn adapter) session-id selector full-page?))
