#lang racket

;; tools/builtins/browser-tools.rkt — 9 browser tool handlers
;;
;; Each handler extracts arguments from a hash, delegates to
;; SecureBrowserService, and returns a JSON-compatible result.

(require racket/match
         json
         "../../browser/types.rkt"
         "../../browser/service.rkt"
         "../../browser/workflow.rkt"
         "../../util/error/errors.rkt")

(provide handle-browser-open
         handle-browser-observe
         handle-browser-click
         handle-browser-type
         handle-browser-press
         handle-browser-extract
         handle-browser-screenshot
         handle-browser-scroll
         handle-browser-close
         handle-browser-check-local-app)

;; ---------------------------------------------------------------------------
;; Helper: get service from exec context or parameter
;; ---------------------------------------------------------------------------

(define (get-svc)
  (or (current-browser-service)
      (raise-browser-error "no browser service configured"
                           'safe-mode
                           (hash))))

;; ---------------------------------------------------------------------------
;; JSON response helpers
;; ---------------------------------------------------------------------------

(define (ok-result . pairs)
  (apply hasheq 'status "ok" pairs))

(define (observation->hash obs)
  (hasheq 'url (browser-observation-url obs)
          'title (browser-observation-title obs)
          'text-content (browser-observation-text-content obs)
          'visible-text (browser-observation-visible-text obs)
          'dom-summary (browser-observation-dom-summary obs)
          'console-errors (browser-observation-console-errors obs)
          'viewport-size (browser-observation-viewport-size obs)))

;; ---------------------------------------------------------------------------
;; Tool handlers
;; ---------------------------------------------------------------------------

(define (handle-browser-open args)
  (define url (hash-ref args 'url))
  (define svc (get-svc))
  (define-values (sid obs)
    (browser-open! svc url
                   #:options (hash-ref args 'options #f)))
  (ok-result 'session-id sid
             'observation (observation->hash obs)))

(define (handle-browser-observe args)
  (define sid (hash-ref args 'session-id))
  (define svc (get-svc))
  (define obs
    (browser-observe! svc sid
                      #:selector (hash-ref args 'selector #f)))
  (ok-result 'observation (observation->hash obs)))

(define (handle-browser-click args)
  (define sid (hash-ref args 'session-id))
  (define selector (hash-ref args 'selector))
  (define button (hash-ref args 'button "left"))
  (define svc (get-svc))
  (define action (browser-action-click selector button))
  (define obs (browser-act! svc sid action))
  (ok-result 'observation (observation->hash obs)))

(define (handle-browser-type args)
  (define sid (hash-ref args 'session-id))
  (define selector (hash-ref args 'selector))
  (define text (hash-ref args 'text))
  (define clear-first? (hash-ref args 'clear-first? #f))
  (define svc (get-svc))
  (define action (browser-action-type selector text clear-first?))
  (define obs (browser-act! svc sid action))
  (ok-result 'observation (observation->hash obs)))

(define (handle-browser-press args)
  (define sid (hash-ref args 'session-id))
  (define key (hash-ref args 'key))
  (define modifiers (hash-ref args 'modifiers '()))
  (define svc (get-svc))
  (define action (browser-action-press key modifiers))
  (define obs (browser-act! svc sid action))
  (ok-result 'observation (observation->hash obs)))

(define (handle-browser-extract args)
  (define sid (hash-ref args 'session-id))
  (define selector (hash-ref args 'selector))
  (define extract-type (hash-ref args 'extract-type "text"))
  (define svc (get-svc))
  (define obs (browser-observe! svc sid #:selector selector))
  (hasheq 'status "ok"
          'extract-type extract-type
          'data (case (string->symbol extract-type)
                  [(text) (or (browser-observation-visible-text obs) "")]
                  [(html) (or (browser-observation-text-content obs) "")]
                  [(accessibility) (format "~a" (or (browser-observation-accessibility-tree obs) ""))]
                  [else (or (browser-observation-visible-text obs) "")])))

(define (handle-browser-screenshot args)
  (define sid (hash-ref args 'session-id))
  (define selector (hash-ref args 'selector #f))
  (define svc (get-svc))
  (define obs (browser-screenshot! svc sid #:selector selector))
  (ok-result 'mime-type (or (browser-observation-screenshot-mime obs) "image/png")
             'data (or (browser-observation-screenshot-bytes obs) "")))

(define (handle-browser-scroll args)
  (define sid (hash-ref args 'session-id))
  (define direction (hash-ref args 'direction "down"))
  (define amount (hash-ref args 'amount 3))
  (define svc (get-svc))
  (define action (browser-action-scroll direction amount))
  (define obs (browser-act! svc sid action))
  (ok-result 'observation (observation->hash obs)))

(define (handle-browser-close args)
  (define sid (hash-ref args 'session-id))
  (define svc (get-svc))
  (browser-close! svc sid)
  (ok-result 'session-id sid))

(define (handle-browser-check-local-app args)
  (browser-check-local-app args))
