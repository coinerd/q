#lang racket

;; browser/types.rkt — Browser domain types
;;
;; Shared type vocabulary for the browser subsystem.
;; Pure data — no I/O, no browser engines.

(require racket/base
         net/base64)

;; browser-target
(provide (struct-out browser-target)
         browser-target->jsexpr
         jsexpr->browser-target
         ;; browser-action
         (struct-out browser-action-navigate)
         (struct-out browser-action-click)
         (struct-out browser-action-type)
         (struct-out browser-action-press)
         (struct-out browser-action-extract)
         (struct-out browser-action-screenshot)
         (struct-out browser-action-scroll)
         (struct-out browser-action-wait)
         browser-action?
         browser-action->jsexpr
         jsexpr->browser-action
         ;; browser-observation
         (struct-out browser-observation)
         browser-observation->jsexpr
         jsexpr->browser-observation
         ;; browser-session-info
         (struct-out browser-session-info)
         browser-session-info->jsexpr
         jsexpr->browser-session-info)

;; ---------------------------------------------------------------------------
;; browser-target — where to point the browser
;; ---------------------------------------------------------------------------

(struct browser-target (url tab-selector viewport-width viewport-height) #:transparent)

(define (browser-target->jsexpr t)
  (hasheq 'url
          (browser-target-url t)
          'tab-selector
          (browser-target-tab-selector t)
          'viewport-width
          (browser-target-viewport-width t)
          'viewport-height
          (browser-target-viewport-height t)))

(define (jsexpr->browser-target j)
  (browser-target (hash-ref j 'url)
                  (hash-ref j 'tab-selector #f)
                  (hash-ref j 'viewport-width 1280)
                  (hash-ref j 'viewport-height 720)))

;; ---------------------------------------------------------------------------
;; browser-action — what to do in the browser
;; ---------------------------------------------------------------------------

(struct browser-action-navigate (url wait-until?) #:transparent)
(struct browser-action-click (selector button) #:transparent)
(struct browser-action-type (selector text clear-first?) #:transparent)
(struct browser-action-press (key modifiers) #:transparent)
(struct browser-action-extract (selector extract-type) #:transparent)
(struct browser-action-screenshot (selector full-page?) #:transparent)
(struct browser-action-scroll (direction amount) #:transparent)
(struct browser-action-wait (selector timeout-ms) #:transparent)

(define (browser-action? v)
  (or (browser-action-navigate? v)
      (browser-action-click? v)
      (browser-action-type? v)
      (browser-action-press? v)
      (browser-action-extract? v)
      (browser-action-screenshot? v)
      (browser-action-scroll? v)
      (browser-action-wait? v)))

(define (browser-action->jsexpr a)
  (cond
    [(browser-action-navigate? a)
     (hasheq 'type
             "navigate"
             'url
             (browser-action-navigate-url a)
             'wait-until
             (browser-action-navigate-wait-until? a))]
    [(browser-action-click? a)
     (hasheq 'type
             "click"
             'selector
             (browser-action-click-selector a)
             'button
             (browser-action-click-button a))]
    [(browser-action-type? a)
     (hasheq 'type
             "type"
             'selector
             (browser-action-type-selector a)
             'text
             (browser-action-type-text a)
             'clear-first
             (browser-action-type-clear-first? a))]
    [(browser-action-press? a)
     (hasheq 'type
             "press"
             'key
             (browser-action-press-key a)
             'modifiers
             (browser-action-press-modifiers a))]
    [(browser-action-extract? a)
     (hasheq 'type
             "extract"
             'selector
             (browser-action-extract-selector a)
             'extract-type
             (browser-action-extract-extract-type a))]
    [(browser-action-screenshot? a)
     (hasheq 'type
             "screenshot"
             'selector
             (browser-action-screenshot-selector a)
             'full-page
             (browser-action-screenshot-full-page? a))]
    [(browser-action-scroll? a)
     (hasheq 'type
             "scroll"
             'direction
             (browser-action-scroll-direction a)
             'amount
             (browser-action-scroll-amount a))]
    [(browser-action-wait? a)
     (hasheq 'type
             "wait"
             'selector
             (browser-action-wait-selector a)
             'timeout-ms
             (browser-action-wait-timeout-ms a))]
    [else (error 'browser-action->jsexpr "unknown action: ~a" a)]))

(define (jsexpr->browser-action j)
  (define t (hash-ref j 'type))
  (case t
    [("navigate") (browser-action-navigate (hash-ref j 'url) (hash-ref j 'wait-until "load"))]
    [("click") (browser-action-click (hash-ref j 'selector) (hash-ref j 'button "left"))]
    [("type")
     (browser-action-type (hash-ref j 'selector) (hash-ref j 'text) (hash-ref j 'clear-first #f))]
    [("press") (browser-action-press (hash-ref j 'key) (hash-ref j 'modifiers '()))]
    [("extract") (browser-action-extract (hash-ref j 'selector) (hash-ref j 'extract-type "text"))]
    [("screenshot") (browser-action-screenshot (hash-ref j 'selector #f) (hash-ref j 'full-page #f))]
    [("scroll") (browser-action-scroll (hash-ref j 'direction "down") (hash-ref j 'amount 300))]
    [("wait") (browser-action-wait (hash-ref j 'selector #f) (hash-ref j 'timeout-ms 5000))]
    [else (error 'jsexpr->browser-action "unknown type: ~a" t)]))

;; ---------------------------------------------------------------------------
;; browser-observation — what the browser sees
;; ---------------------------------------------------------------------------

(struct browser-observation
        (url title
             text-content
             visible-text
             dom-summary
             accessibility-tree
             screenshot-mime
             screenshot-bytes
             console-errors
             network-requests
             viewport-size
             metadata)
  #:transparent)

(define (browser-observation->jsexpr o)
  (hasheq 'url
          (browser-observation-url o)
          'title
          (browser-observation-title o)
          'text-content
          (browser-observation-text-content o)
          'visible-text
          (browser-observation-visible-text o)
          'dom-summary
          (browser-observation-dom-summary o)
          'accessibility-tree
          (browser-observation-accessibility-tree o)
          'screenshot-mime
          (browser-observation-screenshot-mime o)
          'screenshot-bytes
          (and (browser-observation-screenshot-bytes o)
               (bytes->string/utf-8 (base64-encode (browser-observation-screenshot-bytes o) #"")))
          'console-errors
          (browser-observation-console-errors o)
          'network-requests
          (browser-observation-network-requests o)
          'viewport-size
          (browser-observation-viewport-size o)
          'metadata
          (browser-observation-metadata o)))

(define (jsexpr->browser-observation j)
  (browser-observation (hash-ref j 'url "")
                       (hash-ref j 'title "")
                       (hash-ref j 'text-content "")
                       (hash-ref j 'visible-text "")
                       (hash-ref j 'dom-summary "")
                       (hash-ref j 'accessibility-tree "")
                       (hash-ref j 'screenshot-mime #f)
                       (and (hash-ref j 'screenshot-bytes #f)
                            (base64-decode (string->bytes/utf-8 (hash-ref j 'screenshot-bytes))))
                       (hash-ref j 'console-errors '())
                       (hash-ref j 'network-requests '())
                       (hash-ref j 'viewport-size #f)
                       (hash-ref j 'metadata (hasheq))))

;; ---------------------------------------------------------------------------
;; browser-session-info — session metadata
;; ---------------------------------------------------------------------------

(struct browser-session-info (id status created-at last-activity profile-kind artifact-dir)
  #:transparent)

(define (browser-session-info->jsexpr s)
  (hasheq 'id
          (browser-session-info-id s)
          'status
          (browser-session-info-status s)
          'created-at
          (browser-session-info-created-at s)
          'last-activity
          (browser-session-info-last-activity s)
          'profile-kind
          (browser-session-info-profile-kind s)
          'artifact-dir
          (browser-session-info-artifact-dir s)))

(define (jsexpr->browser-session-info j)
  (browser-session-info (hash-ref j 'id)
                        (hash-ref j 'status "active")
                        (hash-ref j 'created-at 0)
                        (hash-ref j 'last-activity 0)
                        (hash-ref j 'profile-kind "ephemeral")
                        (hash-ref j 'artifact-dir #f)))
