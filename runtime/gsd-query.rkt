#lang racket/base
;; runtime/gsd-query.rkt — GSD mode query parameter
;; A1-03: Extracted from tui/state-events.rkt to break wiring→tui dependency.
;; TUI sets this parameter; agent loop reads it. Neither depends on the other.

(provide current-gsd-mode-query)

;; Injected callback to query GSD mode without direct import.
;; Default returns 'idle. TUI overrides with actual mode query.
(define current-gsd-mode-query (make-parameter (lambda () 'idle)))
