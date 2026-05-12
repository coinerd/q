#lang racket/base

;; tools/tool-internal.rkt — Internal tool execution access (R-15)
;; STABILITY: evolving
;;
;; Provides tool-execute for production internal callers (scheduler,
;; dynamic-tools, tests). NOT exported to extensions via tool-api.rkt.
;; Extensions must go through the scheduler for tool invocation.

(require "../tools/tool-struct.rkt")

(provide tool-execute
         tool-dangerous?)

;; Re-export from tool-struct for internal use
;; tool-execute and tool-dangerous? are already bound by the require
