#lang racket/base

;; extensions/ui-channel.rkt — Channel-based UI interaction for extensions
;;
;; FEAT-59: Provides blocking UI primitives that extensions call to interact
;; with the user through the TUI via a channel-based request/response protocol.
;;
;; The ui-channel is a Racket channel? that carries UI request structs.
;; The TUI (or test harness) reads from this channel and sends responses
;; back via the response channel embedded in each request.

(require racket/contract)

;; UI request structs
(provide (struct-out ui-request)
         (struct-out ui-confirm-request)
         (struct-out ui-select-request)
         (struct-out ui-input-request)
         (struct-out ui-response)

         ;; High-level API for extensions
         ui-confirm!
         ui-select!
         ui-input!

         ;; Channel creation
         make-ui-channel
         ui-channel?

         ;; Response waiting
         wait-for-ui-response)

;; ============================================================
;; UI channel type
;; ============================================================

;; A ui-channel is just a Racket channel that carries ui-request? values.
(define (ui-channel? v)
  (channel? v))
(define (make-ui-channel)
  (make-channel))

;; ============================================================
;; UI request/response structs
;; ============================================================

;; Base request
(struct ui-request
        (type ; symbol — 'confirm, 'select, 'input
         response-ch ; channel? — where to send the response
         prompt ; string?
         )
  #:transparent)

;; Confirm request: yes/no
(struct ui-confirm-request
        ui-request
        (default ; boolean?
         )
  #:transparent)

;; Select request: pick from list
(struct ui-select-request
        ui-request
        (options ; (listof (cons/c string? string?)) — (id . label)
         multi? ; boolean? — allow multi-select
         )
  #:transparent)

;; Input request: free text
(struct ui-input-request
        ui-request
        (default ; (or/c string? #f)
         placeholder ; (or/c string? #f)
         )
  #:transparent)

;; Response from UI
(struct ui-response
        (value ; any/c — the user's response
         cancelled? ; boolean? — did the user cancel?
         )
  #:transparent)

;; ============================================================
;; High-level API
;; ============================================================

;; ui-confirm! : ui-channel? string? #:default boolean? #:timeout number? -> (or/c boolean? 'timed-out)
(define (ui-confirm! ch prompt #:default [default #f] #:timeout [timeout 30])
  (define resp-ch (make-channel))
  (define req (ui-confirm-request 'confirm resp-ch prompt default))
  (channel-put ch req)
  (wait-for-ui-response resp-ch
                        timeout
                        (lambda () default) ; timeout → default
                        (lambda (resp)
                          (if (ui-response-cancelled? resp)
                              #f
                              (ui-response-value resp)))))

;; ui-select! : ui-channel? (listof (cons/c string? string?))
;;              #:prompt string? #:multi? boolean? #:timeout number?
;;              -> (or/c (listof string?) 'timed-out #f)
(define (ui-select! ch
                    options
                    #:prompt [prompt "Select:"]
                    #:multi? [multi? #f]
                    #:timeout [timeout 60])
  (if (null? options)
      #f
      (let ([resp-ch (make-channel)])
        (define req (ui-select-request 'select resp-ch prompt options multi?))
        (channel-put ch req)
        (wait-for-ui-response resp-ch
                              timeout
                              (lambda () 'timed-out)
                              (lambda (resp)
                                (if (ui-response-cancelled? resp)
                                    #f
                                    (ui-response-value resp)))))))

;; ui-input! : ui-channel? string? #:default (or/c string? #f) #:timeout number?
;;             -> (or/c string? 'timed-out #f)
(define (ui-input! ch
                   prompt
                   #:default [default #f]
                   #:placeholder [placeholder ""]
                   #:timeout [timeout 120])
  (define resp-ch (make-channel))
  (define req (ui-input-request 'input resp-ch prompt default placeholder))
  (channel-put ch req)
  (wait-for-ui-response resp-ch
                        timeout
                        (lambda () 'timed-out)
                        (lambda (resp)
                          (if (ui-response-cancelled? resp)
                              #f
                              (ui-response-value resp)))))

;; ============================================================
;; Response waiting
;; ============================================================

;; wait-for-ui-response : channel? number? (-> any) (ui-response? -> any) -> any
(define (wait-for-ui-response resp-ch timeout on-timeout on-response)
  (define result (sync/timeout timeout resp-ch))
  (if (not result)
      (on-timeout)
      (on-response result)))
