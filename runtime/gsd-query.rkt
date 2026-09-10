#lang racket/base
;; runtime/gsd-query.rkt — GSD mode query parameter
;; A1-03: Extracted from tui/state-events.rkt to break wiring→tui dependency.
;; TUI sets this parameter; agent loop reads it. Neither depends on the other.

(provide current-gsd-mode-query
         current-gsd-campaign-active-query)

;; Injected callback to query GSD mode without direct import.
;; Default returns 'idle. TUI overrides with actual mode query.
(define current-gsd-mode-query (make-parameter (lambda () 'idle)))

;; BUG-0069: Injected callback answering "does a GSD campaign currently own
;; this session?" (dynamic extent of call-with-gsd-campaign-ownership).
;; tui-init installs a lambda reading current-gsd-campaign-owner, so the
;; error-hint layer can tell the truth: during a campaign a failed turn is
;; re-attempted automatically and "/retry" advice would be wrong. Default
;; returns #f — non-campaign flows keep the existing hints verbatim.
(define current-gsd-campaign-active-query (make-parameter (lambda () #f)))
