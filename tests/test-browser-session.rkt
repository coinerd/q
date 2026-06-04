#lang racket

;; tests/test-browser-session.rkt — Browser session manager tests
;;
;; Tests for browser/session.rkt: lifecycle, max enforcement, action counting.

(require rackunit
         "../browser/session.rkt"
         "../browser/types.rkt"
         "../util/error/errors.rkt")

;; ---------------------------------------------------------------------------
;; Helper
;; ---------------------------------------------------------------------------

(define (make-test-info)
  (browser-session-info "test-session" 'active
                        (current-milliseconds) (current-milliseconds)
                        'ephemeral "/tmp/test"))

;; ---------------------------------------------------------------------------
;; Create / Get / List / Count
;; ---------------------------------------------------------------------------

(test-case "create session"
  (define mgr (make-browser-session-manager))
  (define s (browser-session-manager-create! mgr "s1" (make-test-info)))
  (check-true (browser-session? s))
  (check-equal? (browser-session-id s) "s1"))

(test-case "get session by id"
  (define mgr (make-browser-session-manager))
  (browser-session-manager-create! mgr "s1" (make-test-info))
  (define s (browser-session-manager-get mgr "s1"))
  (check-true (browser-session? s))
  (check-equal? (browser-session-id s) "s1"))

(test-case "get non-existent session returns #f"
  (define mgr (make-browser-session-manager))
  (check-false (browser-session-manager-get mgr "nope")))

(test-case "list sessions"
  (define mgr (make-browser-session-manager))
  (browser-session-manager-create! mgr "s1" (make-test-info))
  (browser-session-manager-create! mgr "s2" (make-test-info))
  (check-equal? (length (browser-session-manager-list mgr)) 2))

(test-case "count sessions"
  (define mgr (make-browser-session-manager))
  (check-equal? (browser-session-manager-count mgr) 0)
  (browser-session-manager-create! mgr "s1" (make-test-info))
  (check-equal? (browser-session-manager-count mgr) 1))

;; ---------------------------------------------------------------------------
;; Destroy
;; ---------------------------------------------------------------------------

(test-case "destroy session"
  (define mgr (make-browser-session-manager))
  (browser-session-manager-create! mgr "s1" (make-test-info))
  (browser-session-manager-destroy! mgr "s1")
  (check-false (browser-session-manager-get mgr "s1"))
  (check-equal? (browser-session-manager-count mgr) 0))

(test-case "destroy non-existent raises error"
  (define mgr (make-browser-session-manager))
  (check-exn q-browser-error?
             (lambda ()
               (browser-session-manager-destroy! mgr "nope"))))

;; ---------------------------------------------------------------------------
;; Max sessions enforcement
;; ---------------------------------------------------------------------------

(test-case "max-sessions enforced (default 3)"
  (define mgr (make-browser-session-manager))
  (browser-session-manager-create! mgr "s1" (make-test-info))
  (browser-session-manager-create! mgr "s2" (make-test-info))
  (browser-session-manager-create! mgr "s3" (make-test-info))
  (check-exn q-browser-error?
             (lambda ()
               (browser-session-manager-create! mgr "s4" (make-test-info)))))

(test-case "max-sessions configurable"
  (define mgr (make-browser-session-manager #:max-sessions 1))
  (browser-session-manager-create! mgr "s1" (make-test-info))
  (check-exn q-browser-error?
             (lambda ()
               (browser-session-manager-create! mgr "s2" (make-test-info)))))

(test-case "destroy frees session slot"
  (define mgr (make-browser-session-manager #:max-sessions 1))
  (browser-session-manager-create! mgr "s1" (make-test-info))
  (browser-session-manager-destroy! mgr "s1")
  (check-not-exn
   (lambda ()
     (browser-session-manager-create! mgr "s2" (make-test-info)))))

;; ---------------------------------------------------------------------------
;; Action counting + max-actions
;; ---------------------------------------------------------------------------

(test-case "record-action increments count"
  (define mgr (make-browser-session-manager))
  (browser-session-manager-create! mgr "s1" (make-test-info))
  (define c1 (browser-session-manager-record-action! mgr "s1"))
  (check-equal? c1 1)
  (define c2 (browser-session-manager-record-action! mgr "s1"))
  (check-equal? c2 2))

(test-case "max-actions enforced"
  (define mgr (make-browser-session-manager #:max-actions 2))
  (browser-session-manager-create! mgr "s1" (make-test-info))
  (browser-session-manager-record-action! mgr "s1")
  (browser-session-manager-record-action! mgr "s1")
  (check-exn q-browser-error?
             (lambda ()
               (browser-session-manager-record-action! mgr "s1"))))

(test-case "record-action on missing session raises error"
  (define mgr (make-browser-session-manager))
  (check-exn q-browser-error?
             (lambda ()
               (browser-session-manager-record-action! mgr "nope"))))

;; ---------------------------------------------------------------------------
;; Duplicate session id
;; ---------------------------------------------------------------------------

(test-case "duplicate session id raises error"
  (define mgr (make-browser-session-manager))
  (browser-session-manager-create! mgr "s1" (make-test-info))
  (check-exn q-browser-error?
             (lambda ()
               (browser-session-manager-create! mgr "s1" (make-test-info)))))
