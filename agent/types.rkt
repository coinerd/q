#lang racket/base

;; agent/types.rkt — BACKWARD-COMPAT RE-EXPORT SHELL
;;
;; DEPRECATED: re-export of util/protocol-types.rkt; new code should import
;; directly from util/protocol-types.rkt. This module exists only for
;; backward compatibility and will be removed in a future version.
;;
;; Re-exports: message, event, loop-result, text-part, tool-result-part,
;;   tool-call-part structs and their accessors/constructors.
;;   Also: message->jsexpr, jsexpr->message, event->jsexpr, jsexpr->event,
;;   make-event, make-message, make-text-part, make-tool-call-part,
;;   make-tool-result-part, make-loop-result, and all struct predicates.

(require "../util/protocol-types.rkt")

(provide ;; Re-export canonical protocol types
 (all-from-out "../util/protocol-types.rkt"))
