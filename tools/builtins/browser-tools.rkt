#lang racket

;; tools/builtins/browser-tools.rkt — 10 browser tool handlers
;;
;; Each handler extracts arguments from a hash, delegates to
;; SecureBrowserService, and returns a tool-result? struct
;; (via make-success-result / make-error-result).
;;
;; All handlers accept (args [exec-ctx #f]) to match the scheduler's
;; 2-arg dispatch: ((tool-execute t) args exec-ctx).

(require racket/match
         json
         racket/string
         net/base64
         "../../browser/types.rkt"
         "../../browser/service.rkt"
         "../../browser/workflow.rkt"
         "../../util/error/errors.rkt"
         "../tool.rkt")

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

;; Graceful fallback: when browser is not configured, return a helpful
;; error tool-result instead of raising an opaque exception.
(define NOT-CONFIGURED-MSG
  (string-append "Browser tools are not enabled. "
                 "Add `{\"browser\": {\"enabled\": true}}` "
                 "to your ~/.q/config.json to enable browser tools."))

(define (browser-not-configured-result)
  (make-error-result NOT-CONFIGURED-MSG))

(define (get-svc)
  (current-browser-service))

;; ---------------------------------------------------------------------------
;; JSON response helpers
;; ---------------------------------------------------------------------------

;; Base64-encode binary data for JSON transport.
(define (bytes->base64-string bs)
  (if (bytes? bs)
      (bytes->string/utf-8 (base64-encode bs #""))
      bs))

;; Sanitize a hash so all byte-string values are base64-encoded.
(define (sanitize-binary-values h)
  (for/hasheq ([(k v) (in-hash h)])
    (values k (bytes->base64-string v))))

(define (ok-result . pairs)
  (make-success-result (apply hasheq 'status "ok" pairs)))

(define (browser-error-result tool-name e)
  (make-error-result (format "~a failed: ~a" tool-name (exn-message e))))

(define (observation->hash obs)
  (hasheq 'url
          (browser-observation-url obs)
          'title
          (browser-observation-title obs)
          'text-content
          (browser-observation-text-content obs)
          'visible-text
          (browser-observation-visible-text obs)
          'dom-summary
          (browser-observation-dom-summary obs)
          'console-errors
          (browser-observation-console-errors obs)
          'viewport-size
          (browser-observation-viewport-size obs)))

;; ---------------------------------------------------------------------------
;; Tool handlers
;; ---------------------------------------------------------------------------

(define (handle-browser-open args [exec-ctx #f])
  (define svc (get-svc))
  (cond
    [(not svc) (browser-not-configured-result)]
    [else
     (define url (hash-ref args 'url))
     (define-values (sid obs) (browser-open! svc url #:options (hash-ref args 'options #f)))
     (ok-result 'session-id sid 'observation (observation->hash obs))]))

(define (handle-browser-observe args [exec-ctx #f])
  (define svc (get-svc))
  (cond
    [(not svc) (browser-not-configured-result)]
    [else
     (define sid (hash-ref args 'session-id))
     (define obs (browser-observe! svc sid #:selector (hash-ref args 'selector #f)))
     (ok-result 'observation (observation->hash obs))]))

(define (handle-browser-click args [exec-ctx #f])
  (define svc (get-svc))
  (cond
    [(not svc) (browser-not-configured-result)]
    [else
     (define sid (hash-ref args 'session-id))
     (define selector (hash-ref args 'selector))
     (define button (hash-ref args 'button "left"))
     (define action (browser-action-click selector button))
     (with-handlers ([q-browser-error? (lambda (e) (browser-error-result "browser_click" e))])
       (define obs (browser-act! svc sid action))
       (ok-result 'observation (observation->hash obs)))]))

(define (handle-browser-type args [exec-ctx #f])
  (define svc (get-svc))
  (cond
    [(not svc) (browser-not-configured-result)]
    [else
     (define sid (hash-ref args 'session-id))
     (define selector (hash-ref args 'selector))
     (define text (hash-ref args 'text))
     (define clear-first? (hash-ref args 'clear-first? #f))
     (define action (browser-action-type selector text clear-first?))
     (with-handlers ([q-browser-error? (lambda (e) (browser-error-result "browser_type" e))])
       (define obs (browser-act! svc sid action))
       (ok-result 'observation (observation->hash obs)))]))

(define (handle-browser-press args [exec-ctx #f])
  (define svc (get-svc))
  (cond
    [(not svc) (browser-not-configured-result)]
    [else
     (define sid (hash-ref args 'session-id))
     (define key (hash-ref args 'key))
     (define modifiers (hash-ref args 'modifiers '()))
     (define action (browser-action-press key modifiers))
     (define obs (browser-act! svc sid action))
     (ok-result 'observation (observation->hash obs))]))

(define (handle-browser-extract args [exec-ctx #f])
  (define svc (get-svc))
  (cond
    [(not svc) (browser-not-configured-result)]
    [else
     (define sid (hash-ref args 'session-id))
     (define selector (hash-ref args 'selector))
     (define extract-type (hash-ref args 'extract-type "text"))
     (define obs (browser-observe! svc sid #:selector selector))
     (make-success-result
      (hasheq 'status
              "ok"
              'extract-type
              extract-type
              'data
              (case (string->symbol extract-type)
                [(text) (or (browser-observation-visible-text obs) "")]
                [(html) (or (browser-observation-text-content obs) "")]
                [(accessibility) (format "~a" (or (browser-observation-accessibility-tree obs) ""))]
                [else (or (browser-observation-visible-text obs) "")])))]))

(define (handle-browser-screenshot args [exec-ctx #f])
  (define svc (get-svc))
  (cond
    [(not svc) (browser-not-configured-result)]
    [else
     (define sid (hash-ref args 'session-id))
     (define selector (hash-ref args 'selector #f))
     (define obs (browser-screenshot! svc sid #:selector selector))
     (define raw-bytes (browser-observation-screenshot-bytes obs))
     ;; Base64-encode binary data for JSON-safe transport
     (make-success-result (hasheq 'status
                                  "ok"
                                  'mime-type
                                  (or (browser-observation-screenshot-mime obs) "image/png")
                                  'data
                                  (if (bytes? raw-bytes)
                                      (bytes->base64-string raw-bytes)
                                      (or raw-bytes ""))))]))

(define (handle-browser-scroll args [exec-ctx #f])
  (define svc (get-svc))
  (cond
    [(not svc) (browser-not-configured-result)]
    [else
     (define sid (hash-ref args 'session-id))
     (define direction (hash-ref args 'direction "down"))
     (define amount (hash-ref args 'amount 3))
     (define action (browser-action-scroll direction amount))
     (define obs (browser-act! svc sid action))
     (ok-result 'observation (observation->hash obs))]))

(define (handle-browser-close args [exec-ctx #f])
  (define svc (get-svc))
  (cond
    [(not svc) (browser-not-configured-result)]
    [else
     (define sid (hash-ref args 'session-id))
     (browser-close! svc sid)
     (ok-result 'session-id sid)]))

(define (handle-browser-check-local-app args [exec-ctx #f])
  (define svc (get-svc))
  (cond
    [(not svc) (browser-not-configured-result)]
    [else
     ;; workflow.rkt returns a raw hash — sanitize binary, wrap in tool-result
     (define raw-result (browser-check-local-app args))
     (make-success-result (sanitize-binary-values raw-result))]))
