#lang racket

;; tests/test-browser-events.rkt — Browser typed events + JSON codec roundtrip
;;
;; Tests for browser/events.rkt and event-json.rkt registration.

(require rackunit
         "../browser/events.rkt"
         "../agent/event-structs/base.rkt"
         "../agent/event-json.rkt")

;; ---------------------------------------------------------------------------
;; Event construction (10 events)
;; ---------------------------------------------------------------------------

(test-case "browser-session-opened-event"
  (define e (make-browser-session-opened-event #:session-id "s1" #:turn-id "t1"))
  (check-equal? (typed-event-type e) "browser.session.opened")
  (check-equal? (typed-event-session-id e) "s1"))

(test-case "browser-session-closed-event"
  (define e (make-browser-session-closed-event #:session-id "s1" #:turn-id "t1"))
  (check-equal? (typed-event-type e) "browser.session.closed"))

(test-case "browser-action-started-event"
  (define e (make-browser-action-started-event #:session-id "s1" #:turn-id "t1" #:action-type "navigate"))
  (check-equal? (browser-action-started-event-action-type e) "navigate"))

(test-case "browser-action-completed-event"
  (define e (make-browser-action-completed-event #:session-id "s1" #:turn-id "t1" #:action-type "click"))
  (check-equal? (browser-action-completed-event-action-type e) "click"))

(test-case "browser-action-failed-event"
  (define e (make-browser-action-failed-event #:session-id "s1" #:turn-id "t1"
                                               #:action-type "type" #:error-message "timeout"))
  (check-equal? (browser-action-failed-event-error-message e) "timeout"))

(test-case "browser-page-loaded-event"
  (define e (make-browser-page-loaded-event #:session-id "s1" #:turn-id "t1" #:url "https://example.com"))
  (check-equal? (browser-page-loaded-event-url e) "https://example.com"))

(test-case "browser-policy-blocked-event"
  (define e (make-browser-policy-blocked-event #:session-id "s1" #:turn-id "t1"
                                                #:url "https://blocked.com" #:reason "domain-policy"))
  (check-equal? (browser-policy-blocked-event-reason e) "domain-policy"))

(test-case "browser-sidecar-started-event"
  (define e (make-browser-sidecar-started-event #:session-id "s1" #:turn-id "t1" #:pid 12345))
  (check-equal? (browser-sidecar-started-event-pid e) 12345))

(test-case "browser-sidecar-stopped-event"
  (define e (make-browser-sidecar-stopped-event #:session-id "s1" #:turn-id "t1"
                                                 #:pid 12345 #:reason "shutdown"))
  (check-equal? (browser-sidecar-stopped-event-reason e) "shutdown"))

(test-case "browser-screenshot-captured-event"
  (define e (make-browser-screenshot-captured-event #:session-id "s1" #:turn-id "t1" #:size-bytes 4096))
  (check-equal? (browser-screenshot-captured-event-size-bytes e) 4096))

;; ---------------------------------------------------------------------------
;; Event registry (all-known-event-types)
;; ---------------------------------------------------------------------------

(test-case "browser events registered in all-known-event-types"
  (define known (all-known-event-types))
  (for ([evt '("browser.session.opened"
               "browser.session.closed"
               "browser.action.started"
               "browser.action.completed"
               "browser.action.failed"
               "browser.page.loaded"
               "browser.policy.blocked"
               "browser.sidecar.started"
               "browser.sidecar.stopped"
               "browser.screenshot.captured")])
    (check-not-false (member evt known) (format "~a not in registry" evt))))

;; ---------------------------------------------------------------------------
;; F10: browser events accessible via event-structs facade
;; ---------------------------------------------------------------------------

(require "../agent/event-structs.rkt")

(test-case "browser events accessible via event-structs facade"
  (define e (make-browser-session-opened-event #:session-id "s1" #:turn-id "t1"))
  (check-true (browser-session-opened-event? e))
  (check-equal? (typed-event-type e) "browser.session.opened"))

(test-case "browser action events via facade"
  (define e (make-browser-action-started-event #:session-id "s1" #:turn-id "t1" #:action-type "click"))
  (check-true (browser-action-started-event? e))
  (check-equal? (browser-action-started-event-action-type e) "click"))

(test-case "browser screenshot event via facade"
  (define e (make-browser-screenshot-captured-event #:session-id "s1" #:turn-id "t1" #:size-bytes 999))
  (check-true (browser-screenshot-captured-event? e))
  (check-equal? (browser-screenshot-captured-event-size-bytes e) 999))
