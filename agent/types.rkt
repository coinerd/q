#lang racket/base

;; agent/types.rkt — canonical runtime structs and enums
;;
;; ARCH-01/04/05: all struct definitions moved to util/protocol-types.rkt
;; to eliminate layer violations. This module re-exports everything
;; for backward compatibility.
;;
;; All structs provide JSON serialization/deserialization via
;; message->jsexpr / jsexpr->message and event->jsexpr / jsexpr->event.

(require "../util/protocol-types.rkt")

(provide ;; Re-export canonical protocol types
 (all-from-out "../util/protocol-types.rkt"))
