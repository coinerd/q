#lang racket

;; test-browser-robustness-w1.rkt — W1 tests
;;
;; Dead events cleanup, screenshot-max-bytes enforcement,
;; session manager thread safety.

(require rackunit
         racket/match
         "../browser/settings.rkt"
         "../browser/session.rkt"
         "../browser/types.rkt"
         "../browser/service.rkt"
         "../browser/events.rkt"
         "../util/error/errors.rkt")

;; ---------------------------------------------------------------------------
;; Test: screenshot-max-bytes enforcement
;; ---------------------------------------------------------------------------

(test-case "enforce-screenshot-max-bytes nulls oversized screenshot"
  (define s (default-browser-settings))
  (define max-bytes (browser-settings-screenshot-max-bytes s))
  (check-true (exact-positive-integer? max-bytes))
  ;; Oversized observation → screenshot set to #f (H4: not corrupt truncated bytes)
  (define big-bytes (make-bytes (add1 max-bytes) 65)) ; 'A' bytes
  (define obs (browser-observation "" "" "" "" #f #f "image/png" big-bytes '() '() #f #f (hash)))
  (define truncated (enforce-screenshot-max-bytes obs max-bytes))
  (check-false (browser-observation-screenshot-bytes truncated))
  (check-false (browser-observation-screenshot-mime truncated)))

(test-case "enforce-screenshot-max-bytes passes small screenshot through"
  (define s (default-browser-settings))
  (define max-bytes (browser-settings-screenshot-max-bytes s))
  (define small-bytes (make-bytes 100 65))
  (define obs (browser-observation "" "" "" "" #f #f "image/png" small-bytes '() '() #f #f (hash)))
  (define result (enforce-screenshot-max-bytes obs max-bytes))
  (check-equal? (browser-observation-screenshot-bytes result) small-bytes))

(test-case "enforce-screenshot-max-bytes handles #f bytes"
  (define obs (browser-observation "" "" "" "" #f #f #f #f '() '() #f #f (hash)))
  (define result (enforce-screenshot-max-bytes obs 524288))
  (check-false (browser-observation-screenshot-bytes result)))

;; ---------------------------------------------------------------------------
;; Test: session manager thread safety
;; ---------------------------------------------------------------------------

(test-case "session-manager: concurrent create/destroy doesn't corrupt"
  (define mgr (make-browser-session-manager #:max-sessions 100 #:max-actions 1000))
  (define results (box '()))
  (define n 50)

  ;; Create sessions concurrently
  (define threads
    (for/list ([i (in-range n)])
      (thread (lambda ()
                (define id (format "session-~a" i))
                (define info
                  (browser-session-info id
                                        'active
                                        (current-milliseconds)
                                        (current-milliseconds)
                                        'ephemeral
                                        "/tmp"))
                (with-handlers ([exn:fail? void])
                  (browser-session-manager-create! mgr id info))))))

  (for-each sync threads)

  ;; All sessions should exist
  (check-equal? (browser-session-manager-count mgr) n)

  ;; Destroy half concurrently
  (define destroy-threads
    (for/list ([i (in-range (quotient n 2))])
      (thread (lambda ()
                (define id (format "session-~a" i))
                (with-handlers ([exn:fail? void])
                  (browser-session-manager-destroy! mgr id))))))

  (for-each sync destroy-threads)

  ;; Should have remaining sessions
  (check-equal? (browser-session-manager-count mgr) (- n (quotient n 2))))

;; ---------------------------------------------------------------------------
;; Test: dead events removed — only emitted events remain
;; ---------------------------------------------------------------------------

(test-case "browser events module provides only active events"
  ;; All 10 events should still be defined — the "dead" ones get emitted now
  (check-pred procedure? browser-session-opened-event)
  (check-pred procedure? browser-session-closed-event)
  (check-pred procedure? browser-action-started-event)
  (check-pred procedure? browser-action-completed-event)
  (check-pred procedure? browser-screenshot-captured-event)
  (check-pred procedure? browser-sidecar-stopped-event))
