#lang racket

;; browser/workflow.rkt — Composite workflow tools
;;
;; Higher-level browser operations that compose multiple primitives
;; into a single atomic workflow for common use cases.

(require racket/match
         json
         "types.rkt"
         "service.rkt"
         "../util/error/errors.rkt")

(provide browser-check-local-app)

;; ---------------------------------------------------------------------------
;; browser-check-local-app — Quick local app health check
;;
;; Opens a URL, waits for load, takes a screenshot, extracts text +
;; console errors, then closes. Returns a composite result suitable
;; for the LLM to reason about the app's state.
;;
;; Params:
;;   url            — (required) local app URL to check
;;   timeout-ms     — (optional, default 10000) navigation timeout
;;   selector       — (optional) CSS selector to wait for
;;   screenshot?    — (optional, default #t) whether to capture screenshot
;; ---------------------------------------------------------------------------

(define (browser-check-local-app args)
  (define url (hash-ref args 'url))
  (define timeout-ms (hash-ref args 'timeout_ms 10000))
  (define selector (hash-ref args 'selector #f))
  (define take-screenshot? (hash-ref args 'screenshot #t))
  (define svc (get-svc))

  (define start-ms (current-inexact-milliseconds))

  ;; 1. Open browser and navigate
  (define-values (session-id open-obs)
    (browser-open! svc url))

  ;; 2. Extract page state (with optional selector)
  (define extract-obs
    (browser-observe! svc session-id #:selector selector))

  ;; 3. Screenshot (optional)
  (define screenshot-obs
    (and take-screenshot?
         (browser-screenshot! svc session-id)))

  ;; 4. Close session
  (browser-close! svc session-id)

  (define end-ms (current-inexact-milliseconds))
  (define load-time-ms (inexact->exact (round (- end-ms start-ms))))

  ;; 5. Build composite result
  (define console-errors (browser-observation-console-errors extract-obs))
  (define has-errors? (and (list? console-errors) (not (null? console-errors))))
  (define page-text (browser-observation-text-content extract-obs))
  (define has-content? (and (string? page-text) (> (string-length page-text) 0)))

  (apply
   hasheq
   'status "ok"
   'url (browser-observation-url extract-obs)
   'title (browser-observation-title extract-obs)
   'text page-text
   'console_errors console-errors
   'loaded_successfully (and (not has-errors?) has-content?)
   'load_time_ms load-time-ms
   'page_errors (if has-errors? console-errors '())
   'session_id session-id
   (if (and screenshot-obs (browser-observation-screenshot-bytes screenshot-obs))
       (list 'screenshot_mime (browser-observation-screenshot-mime screenshot-obs)
             'screenshot_data (browser-observation-screenshot-bytes screenshot-obs))
       (list 'screenshot_mime #f
             'screenshot_data #f))))

;; ---------------------------------------------------------------------------
;; Internal helpers
;; ---------------------------------------------------------------------------

(define (get-svc)
  (or (current-browser-service)
      (raise-browser-error "no browser service configured"
                           'safe-mode
                           (hash))))
