#lang racket/base

;; @speed fast  ;; @suite tui

;; BOUNDARY: io

;; tests/test-tui-init-phases.rkt -- TUI init phase extraction tests (W-19)

(require rackunit
         "../tui/tui-init.rkt"
         "../tui/state-types.rkt"
         "../tui/tui-keybindings.rkt")

(test-case "subscribe-runtime-events! works with #f bus"
  (define ctx (make-tui-ctx #:event-bus #f))
  ;; Should not crash
  (subscribe-runtime-events! ctx)
  (check-true (tui-ctx? ctx)))

(test-case "tui-init-phases: make-tui-ctx creates valid context"
  (define ctx (make-tui-ctx))
  (check-true (tui-ctx? ctx))
  (check-false (tui-ctx-event-bus ctx))
  (check-true (unbox (tui-ctx-running-box ctx))))

(test-case "run-tui-with-runtime is exported"
  (check-true (procedure? run-tui-with-runtime)))

(test-case "run-tui is exported"
  (check-true (procedure? run-tui)))

(test-case "init-tui-terminal is exported"
  (check-true (procedure? init-tui-terminal)))
