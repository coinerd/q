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
;;;   make-strict-permission-config              — hard-deny (W1 v0.99.66)
;;;   make-permissive-permission-config          — --auto-approve opt-in (W2 v0.99.66)
;;;   tool-needs-approval?                       — predicate
;;;   request-approval                           — invoke the callback
;;;   permission-config-auto-approved-tools      — accessor
;;;   permission-config-needs-approval-tools     — accessor
;;;   permission-config-approval-callback        — accessor
;;;   permission-config-policy-mode              — accessor
;;;
;;; Source of truth: the auto-approved / needs-approval tool sets are
;;; defined once in tool-classification.rkt (the single classification
;;; source introduced in v0.99.66).  This module consumes them to
;;; populate default configs; it no longer maintains a parallel list.

(require racket/set
         racket/contract
         "tool-classification.rkt")

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
;; Default config
;; ============================================================

;; make-default-permission-config — general-purpose config builder.
;;
;; v0.99.66 (W2, finding #2 HIGH): the default approval callback now
;; DENIES by default.  A dangerous tool that reaches the approval
;; branch is blocked unless the caller explicitly supplies an
;; approving callback (e.g. interactive TUI broker or --auto-approve).
(define (make-default-permission-config #:auto-approved [auto-approved #f]
                                         #:needs-approval [needs-approval #f]
                                         #:callback [callback #f]
                                         #:policy-mode [mode 'strict])
  (permission-config (or auto-approved auto-approved-tool-names)
                     (or needs-approval needs-approval-tool-names)
                     (or callback (lambda (tool-name args) #f))   ; W2: deny-by-default
                     (if (memq mode '(strict permissive)) mode 'strict)))

;; make-strict-permission-config — the hard-deny config.
;;
;; v0.99.66 (W1, finding #1 CRITICAL): this is the default assigned to
;; every exec-context that does not explicitly configure permissions.
;; It never auto-approves a dangerous tool and never returns #f/'skip.
;; Safe (read-only) tools bypass the gate entirely via classification.
(define (make-strict-permission-config)
  (make-default-permission-config #:callback (lambda (tool-name args) #f)))

;; make-permissive-permission-config — explicit opt-in for dangerous tools.
;;
;; v0.99.66 (W2): used by --auto-approve and trusted-CLI paths.  All
;; dangerous calls are approved.  This is the ONLY way to get
;; auto-approval of dangerous tools post-W2.
(define (make-permissive-permission-config)
  (make-default-permission-config
   #:callback (lambda (tool-name args) #t)
   #:policy-mode 'permissive))


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
         make-strict-permission-config
         make-permissive-permission-config
         (contract-out [make-default-permission-config
                        (->* ()
                             (#:auto-approved (or/c (set/c string?) #f)
                                              #:needs-approval (or/c (set/c string?) #f)
                                              #:callback (or/c (-> string? hash? boolean?) #f)
                                              #:policy-mode (or/c 'strict 'permissive))
                             permission-config?)])
         tool-needs-approval?
         request-approval)
