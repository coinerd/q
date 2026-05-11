#lang racket/base

;; extensions/tool-api.rkt — Facade for tool result constructors
;;
;; ARCH-03 (v0.22.0): Extensions import from HERE, not from
;; tools/tool.rkt directly. Single indirection point so extensions
;; never depend on the tools layer's internal structure.
;;
;; Re-exports: tool result constructors, tool struct, tool registry,
;; and execution context helpers.

(require "../tools/tool.rkt"
         (only-in "../tools/tool-struct.rkt" tool-execute))

(provide
 ;; Tool result constructors (most common need)
 make-success-result
 make-error-result
 make-tool-result
 tool-result?
 tool-result-content
 tool-result-details
 tool-result-is-error?
 tool-result->jsexpr
 jsexpr->tool-result

 ;; Tool struct (dynamic-tools needs this)
 make-tool
 tool?
 tool-name
 tool-description
 tool-schema
 tool-execute
 tool-prompt-snippet
 tool-prompt-guidelines
 tool-dangerous?
 tool->jsexpr
 merge-tool-lists
 validate-tool-schema
 format-tool-schema-hint

 ;; Tool registry
 make-tool-registry
 tool-registry?
 register-tool!
 unregister-tool!
 set-active-tools!
 tool-active?
 list-active-tools
 list-active-tools-jsexpr
 lookup-tool
 list-tools
 list-tools-jsexpr
 tool-names

 ;; Execution context
 make-exec-context
 exec-context?
 exec-context-working-directory
 exec-context-cancellation-token
 exec-context-event-publisher
 exec-context-runtime-settings
 exec-context-call-id
 exec-context-session-metadata
 exec-context-progress-callback
 exec-context-permission-config
 emit-progress!

 ;; Tool call struct
 tool-call
 tool-call?
 tool-call-id)
