#lang racket/base

;;; tools/permission-gate.rkt — Permission gates for tool execution.
;;;
;;; Provides an interactive approval flow so dangerous tools (edit, write, bash,
;;; etc.) must be explicitly approved before execution.  Safe / read-only tools
;;; bypass the check entirely.
;;;
;;; Exports:
;;;   permission-config, permission-config?     — config struct
;;;   make-default-permission-config             — headless-friendly defaults
;;;   tool-needs-approval?                       — predicate
;;;   request-approval                           — invoke the callback
;;;   permission-config-auto-approved-tools      — accessor
;;;   permission-config-needs-approval-tools     — accessor
;;;   permission-config-approval-callback        — accessor

(require racket/set)

;; ============================================================
;; Struct
;; ============================================================

(struct permission-config
        (auto-approved-tools   ; (setof string?)
         needs-approval-tools  ; (setof string?)
         approval-callback)    ; (string? hash? -> boolean?)
  #:transparent)

;; ============================================================
;; Default config — headless mode (auto-approve everything)
;; ============================================================

(define (make-default-permission-config
         #:auto-approved [auto-approved #f]
         #:needs-approval [needs-approval #f]
         #:callback [callback #f])
  (permission-config
   (or auto-approved
       (set "read"
            "glob"
            "ls"
            "find"
            "grep"
            "context-files"))
   (or needs-approval
       (set "edit"
            "write"
            "bash"
            "delete"
            "move"
            "spawn_subagent"))
   (or callback (lambda (tool-name args) #t))))

;; ============================================================
;; Predicate — does this tool call require approval?
;; ============================================================

(define (tool-needs-approval? config tool-name)
  (cond
    ;; Explicitly auto-approved → no
    [(set-member? (permission-config-auto-approved-tools config) tool-name) #f]
    ;; Explicitly in the needs-approval set → yes
    [(set-member? (permission-config-needs-approval-tools config) tool-name) #t]
    ;; Unknown tool → require approval (safe default)
    [else #t]))

;; ============================================================
;; Request approval — invoke the callback
;; ============================================================

(define (request-approval config tool-name args)
  ((permission-config-approval-callback config) tool-name args))

;; ============================================================
;; Provides
;; ============================================================

(provide (struct-out permission-config)
         make-default-permission-config
         tool-needs-approval?
         request-approval)
