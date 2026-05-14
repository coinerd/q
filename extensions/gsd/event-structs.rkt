#lang racket/base

;; extensions/gsd/event-structs.rkt -- Typed event definitions for GSD lifecycle
;;
;; R8: Replaces raw hasheq emissions with typed events for compile-time
;; field checking and auto-serialization via define-typed-event macro.

(require "../../util/event-macro.rkt")

(define-typed-event gsd-mode-changed-event "gsd.mode.changed" (mode))

(define-typed-event gsd-transition-attempted-event "gsd.transition.attempted" (from to))

(define-typed-event gsd-transition-succeeded-event "gsd.transition.succeeded" (from to))

(define-typed-event gsd-wave-completed-event "gsd.wave.completed" (wave))

(define-typed-event gsd-wave-started-event "gsd.wave.started" (wave))

(define-typed-event gsd-plan-parsed-event "gsd.plan.parsed" (wave-count))

(define-typed-event gsd-command-received-event "gsd.command.received" (command args))

(define-typed-event gsd-archive-failed-event "gsd.archive.failed" (error))

(define-typed-event gsd-plan-validated-event "gsd.plan.validated" (wave-count))

(define-typed-event gsd-plan-normalized-event "gsd.plan.normalized" (wave-count))

(define-typed-event gsd-command-completed-event "gsd.command.completed" (command success))

(provide (all-defined-out))
