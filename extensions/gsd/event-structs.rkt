#lang racket/base

;; extensions/gsd/event-structs.rkt -- Typed event definitions for GSD lifecycle
;;
;; R8: Replaces raw hasheq emissions with typed events for compile-time
;; field checking and auto-serialization via define-typed-event macro.

(require "../../util/event/event-macro.rkt")

(define-typed-event gsd-mode-changed-event "gsd.mode.changed" (mode)
  #:optional ([reason #f]
              [error #f]))

(define-typed-event gsd-transition-attempted-event "gsd.transition.attempted" (from to))

(define-typed-event gsd-transition-succeeded-event "gsd.transition.succeeded" (from to))

(define-typed-event gsd-wave-completed-event "gsd.wave.completed" (wave))

(define-typed-event gsd-wave-started-event "gsd.wave.started" (wave))

(define-typed-event gsd-plan-parsed-event "gsd.plan.parsed" (wave-count))

(define-typed-event gsd-command-received-event "gsd.command.received" (command args))

(define-typed-event gsd-archive-failed-event "gsd.archive.failed" (error))

(define-typed-event gsd-plan-validated-event "gsd.plan.validated" (wave-count)
  #:optional ([valid? #t]
              [error-count 0]
              [warning-count 0]))

(define-typed-event gsd-plan-normalized-event "gsd.plan.normalized" (wave-count))

(define-typed-event gsd-command-completed-event "gsd.command.completed" (command success))


;; CF1-1: gsd.plan.archived
(define-typed-event gsd-plan-archived-event "gsd.plan.archived" (path))

;; CF1-4: gsd.transition.failed
(define-typed-event gsd-transition-failed-event "gsd.transition.failed" (from to reason))

;; Future-ready: no emission sites yet
(define-typed-event gsd-wave-failed-event "gsd.wave.failed" (wave error))
(define-typed-event gsd-wave-skipped-event "gsd.wave.skipped" (wave reason))
(define-typed-event gsd-guard-blocked-event "gsd.guard.blocked" (tool reason))
(define-typed-event gsd-guard-allowed-event "gsd.guard.allowed" (tool))

;; Explicit provide list (A1-08: no all-defined-out)
;; Each define-typed-event generates: event-name, event-name?, make-event-name
(provide gsd-mode-changed-event
         gsd-mode-changed-event?
         make-gsd-mode-changed-event
         gsd-transition-attempted-event
         gsd-transition-attempted-event?
         make-gsd-transition-attempted-event
         gsd-transition-succeeded-event
         gsd-transition-succeeded-event?
         make-gsd-transition-succeeded-event
         gsd-wave-completed-event
         gsd-wave-completed-event?
         make-gsd-wave-completed-event
         gsd-wave-started-event
         gsd-wave-started-event?
         make-gsd-wave-started-event
         gsd-plan-parsed-event
         gsd-plan-parsed-event?
         make-gsd-plan-parsed-event
         gsd-command-received-event
         gsd-command-received-event?
         make-gsd-command-received-event
         gsd-archive-failed-event
         gsd-archive-failed-event?
         make-gsd-archive-failed-event
         gsd-plan-validated-event
         gsd-plan-validated-event?
         make-gsd-plan-validated-event
         gsd-plan-normalized-event
         gsd-plan-normalized-event?
         make-gsd-plan-normalized-event
         gsd-command-completed-event
         gsd-command-completed-event?
         make-gsd-command-completed-event
         gsd-plan-archived-event
         gsd-plan-archived-event?
         make-gsd-plan-archived-event
         gsd-transition-failed-event
         gsd-transition-failed-event?
         make-gsd-transition-failed-event
         gsd-wave-failed-event
         gsd-wave-failed-event?
         make-gsd-wave-failed-event
         gsd-wave-skipped-event
         gsd-wave-skipped-event?
         make-gsd-wave-skipped-event
         gsd-guard-blocked-event
         gsd-guard-blocked-event?
         make-gsd-guard-blocked-event
         gsd-guard-allowed-event
         gsd-guard-allowed-event?
         make-gsd-guard-allowed-event)
