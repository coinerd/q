#lang racket/base

;; q/gui/views/transcript.rkt — Transcript view for GUI
;;
;; Renders a scrollable transcript of message entries. Manages
;; scroll state, auto-scroll behavior, and message list rendering.

(require racket/contract
         racket/list
         "../../ui-core/theme-protocol.rkt")

(provide (contract-out [render-transcript
                        (->* (ui-theme? (listof hash?))
                             (#:width exact-nonnegative-integer?
                                      #:height exact-nonnegative-integer?
                                      #:scroll-offset exact-nonnegative-integer?
                                      #:auto-scroll boolean?
                                      #:focused boolean?)
                             hash?)]
                       [transcript-scroll-up (-> hash? hash?)]
                       [transcript-scroll-down (-> hash? hash?)]
                       [transcript-scroll-top (-> hash? hash?)]
                       [transcript-scroll-bottom (-> hash? hash?)]))

;; ──────────────────────────────
;; Render transcript
;; ──────────────────────────────
(define (render-transcript theme
                           messages
                           #:width [width 120]
                           #:height [height 30]
                           #:scroll-offset [scroll-offset 0]
                           #:auto-scroll [auto-scroll #t]
                           #:focused [focused #f])
  (define total-messages (length messages))
  (define max-offset (max 0 (sub1 total-messages)))
  (define clamped-offset (min scroll-offset max-offset))
  (hash 'view
        'transcript
        'messages
        messages
        'width
        width
        'height
        height
        'scroll-offset
        clamped-offset
        'auto-scroll
        auto-scroll
        'focused
        focused
        'total-messages
        total-messages
        'bg
        (theme-ref theme 'background)
        'fg
        (theme-ref theme 'foreground)))

;; ──────────────────────────────
;; Scroll operations
;; ──────────────────────────────
(define (transcript-scroll-up view-desc)
  (define offset (hash-ref view-desc 'scroll-offset 0))
  (hash-set view-desc 'scroll-offset (max 0 (sub1 offset))))

(define (transcript-scroll-down view-desc)
  (define offset (hash-ref view-desc 'scroll-offset 0))
  (define total (hash-ref view-desc 'total-messages 0))
  (define max-offset (max 0 (sub1 total)))
  (hash-set view-desc 'scroll-offset (min max-offset (add1 offset))))

(define (transcript-scroll-top view-desc)
  (hash-set view-desc 'scroll-offset 0))

(define (transcript-scroll-bottom view-desc)
  (define total (hash-ref view-desc 'total-messages 0))
  (hash-set view-desc 'scroll-offset (max 0 (sub1 total))))
