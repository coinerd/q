#lang racket/base

;;; tools/permission-gate.rkt — Permission gates for tool execution.
;;;
;;; Provides an interactive approval flow so dangerous tools (edit, write, bash,
;;; etc.) must be explicitly approved before execution.  Safe / read-only tools
;;; bypass the check entirely.
;;;
;;; Policy modes (v0.54.4):
;;;   'strict     — unknown tools require approval (deny-by-default). Default.
;;;   'permissive — unknown tools are auto-approved (allow-by-default).
;;;
;;; Exports:
;;;   permission-config, permission-config?     — config struct
;;;   make-default-permission-config             — headless-friendly defaults
;;;   tool-needs-approval?                       — predicate
;;;   request-approval                           — invoke the callback
;;;   permission-config-auto-approved-tools      — accessor
;;;   permission-config-needs-approval-tools     — accessor
;;;   permission-config-approval-callback        — accessor
;;;   permission-config-policy-mode              — accessor

(require racket/set
         racket/contract)

;; ============================================================
;; Struct
;; ============================================================

(struct permission-config
        (auto-approved-tools ; (set/c string?)
         needs-approval-tools ; (set/c string?)
         approval-callback ; (string? hash? -> boolean?)
         policy-mode) ; (or/c 'strict 'permissive)
  #:transparent)

;; ============================================================
;; Default config — headless mode (auto-approve everything)
;; ============================================================

(define (make-default-permission-config #:auto-approved [auto-approved #f]
                                        #:needs-approval [needs-approval #f]
                                        #:callback [callback #f]
                                        #:policy-mode [mode 'strict])
  (permission-config (or auto-approved
                         (set "read"
                              "glob"
                              "ls"
                              "find"
                              "grep"
                              "context-files"
                              "date"
                              "session_recall"
                              "skill-route"
                              "save-conclusion"
                              "record_conclusion"
                              "set-task-state"
                              "browser_observe"
                              "browser_extract"
                              "browser_screenshot"
                              "browser_scroll"
                              "browser_close"))
                     (or needs-approval
                         (set "edit"
                              "write"
                              "bash"
                              "delete"
                              "move"
                              "delete-lines"
                              "spawn-subagent"
                              "spawn-subagents"
                              "firecrawl"
                              "browser_open"
                              "browser_click"
                              "browser_type"
                              "browser_press"
                              "browser_check_local_app"))
                     (or callback (lambda (tool-name args) #t))
                     (if (memq mode '(strict permissive)) mode 'strict)))

;; ============================================================
;; Predicate — does this tool call require approval?
;; ============================================================

(define (tool-needs-approval? config tool-name)
  (cond
    ;; Explicitly auto-approved -> no
    [(set-member? (permission-config-auto-approved-tools config) tool-name) #f]
    ;; Explicitly in the needs-approval set -> yes
    [(set-member? (permission-config-needs-approval-tools config) tool-name) #t]
    ;; Unknown tool -- policy-dependent
    [(eq? (permission-config-policy-mode config) 'permissive) #f]
    ;; Strict mode (default) -- unknown tools require approval
    [else #t]))

;; ============================================================
;; Request approval — invoke the callback
;; ============================================================

(define (request-approval config tool-name args)
  ((permission-config-approval-callback config) tool-name args))

;; ============================================================
;; Provides
;; ============================================================

(provide permission-config
         permission-config?
         permission-config-auto-approved-tools
         permission-config-needs-approval-tools
         permission-config-approval-callback
         permission-config-policy-mode
         (contract-out [make-default-permission-config
                        (->* ()
                             (#:auto-approved (or/c (set/c string?) #f)
                                              #:needs-approval (or/c (set/c string?) #f)
                                              #:callback (or/c (-> string? hash? boolean?) #f)
                                              #:policy-mode (or/c 'strict 'permissive))
                             permission-config?)])
         tool-needs-approval?
         request-approval)
