#lang racket/base

;; tui/payload-types.rkt — Named structs for TUI event payloads (M-13)
;;
;; Replaces ad-hoc hasheq shapes accessed via hash-ref with named structs
;; for the most common payload types used in 2+ places.

(provide (struct-out runtime-error-payload)
         (struct-out tool-result-payload)
         (struct-out tool-call-meta))

;; Error payload used in runtime-error, model-error, and tool-error events.
(struct runtime-error-payload (text error-type reason) #:transparent)

;; Tool result metadata used in tool-call events.
(struct tool-result-payload (name result error) #:transparent)

;; Tool call metadata for entry construction.
(struct tool-call-meta (name arguments) #:transparent)
