#lang racket

;; browser/events.rkt — Browser typed events
;;
;; Defines 10 browser event types using the existing typed-event infrastructure.
;; Note: define-typed-event automatically adds session-id and turn-id fields.

(require "../util/event/event-macro.rkt")

(provide (all-defined-out))

;; ---------------------------------------------------------------------------
;; Browser session events
;; ---------------------------------------------------------------------------

(define-typed-event browser-session-opened-event
  "browser.session.opened"
  ())

(define-typed-event browser-session-closed-event
  "browser.session.closed"
  ())

;; ---------------------------------------------------------------------------
;; Browser action events
;; ---------------------------------------------------------------------------

(define-typed-event browser-action-started-event
  "browser.action.started"
  (action-type))

(define-typed-event browser-action-completed-event
  "browser.action.completed"
  (action-type))

(define-typed-event browser-action-failed-event
  "browser.action.failed"
  (action-type error-message))

;; ---------------------------------------------------------------------------
;; Browser page events
;; ---------------------------------------------------------------------------

(define-typed-event browser-page-loaded-event
  "browser.page.loaded"
  (url))

;; ---------------------------------------------------------------------------
;; Browser policy events
;; ---------------------------------------------------------------------------

(define-typed-event browser-policy-blocked-event
  "browser.policy.blocked"
  (url reason))

;; ---------------------------------------------------------------------------
;; Browser sidecar events
;; ---------------------------------------------------------------------------

(define-typed-event browser-sidecar-started-event
  "browser.sidecar.started"
  (pid))

(define-typed-event browser-sidecar-stopped-event
  "browser.sidecar.stopped"
  (pid reason))

;; ---------------------------------------------------------------------------
;; Browser screenshot events
;; ---------------------------------------------------------------------------

(define-typed-event browser-screenshot-captured-event
  "browser.screenshot.captured"
  (size-bytes))
