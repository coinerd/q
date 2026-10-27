#lang racket/base

;;; tools/scheduler-preflight.rkt — Preflight/validation concerns extracted from scheduler.rkt
;;;
;;; Exports:
;;;   - preflight-entry struct (status, tool-call, tool, error-message)
;;;   - run-preflight — preflight stage (serial hook dispatch, validation, safe-mode checks)
;;;
;;; This module handles:
;;;   1. Hook dispatch for preflight ('tool-call hook point)
;;;   2. Argument revalidation after hook mutation
;;;   3. Tool capability checks (safe-mode restrictions, permissions)
;;;   4. Unknown tool detection

(require racket/match
         (only-in "tool.rkt"
                  tool-call?
                  tool-call-name
                  tool-call-arguments
                  lookup-tool
                  validate-tool-args
                  format-tool-schema-hint
                  tool-required-capability
                  make-exec-context
                  exec-context?
                  exec-context-capabilities)
         (only-in "../util/capability.rkt" capability-authorized?)
         (only-in "../util/hook-types.rkt" hook-result? hook-result-action hook-result-payload)
         (only-in "../util/safe-mode/safe-mode-predicates.rkt"
                  safe-mode?
                  allowed-tool?
                  allowed-path?
                  safe-mode-project-root))

(provide preflight-entry
         preflight-entry?
         preflight-entry-status
         preflight-entry-tool-call
         preflight-entry-tool
         preflight-entry-error-message
         run-preflight
         safe-mode-invocation-denial)

;; ============================================================
;; Preflight-entry struct
;; ============================================================

;; An entry in the preflight result list.
;; One of three states:
;;   - 'ready  + tool-call + tool struct   -> proceed to execution
;;   - 'blocked + tool-call + #f           -> hook blocked it
;;   - 'error   + tool-call + error-msg    -> post-hook validation failed / not found

;; v0.35.2 (W-10): Typed struct replaces ad-hoc hasheq
(struct preflight-entry (status tool-call tool error-message) #:transparent)

;; status: 'ready | 'blocked | 'error
;; tool: tool? (#f for blocked/error)
;; error-message: string? (#f for ready)

;; ============================================================
;; Internal: extract path argument from tool call args
;; ============================================================

(define (extract-path-arg args)
  (or (hash-ref args 'path #f) (hash-ref args 'root #f) (hash-ref args 'directory #f)))

;; ============================================================
;; Safe-mode admission for ONE concrete invocation
;; ============================================================

;; Returns #f when TOOL-NAME with ARGS may proceed under the current safe-mode
;; policy, or the denial message when it may not. The check covers both halves
;; of the policy -- the read-only tool allowlist and the project-root path
;; restriction -- and it is deliberately a function of the CONCRETE invocation,
;; not of a particular stage.
;;
;; Preflight is the first caller. The scheduler is the second: a tool-call-pre
;; hook may amend the arguments (an extension can do this) after preflight has
;; already admitted the call, so execution re-runs this exact check on the final
;; arguments before approval and before the tool runs. A path substituted into
;; the invocation by a hook can therefore never escape safe mode.
(define (safe-mode-invocation-denial tool-name args)
  (cond
    [(not (safe-mode?)) #f]
    [(not (allowed-tool? tool-name)) (format "tool '~a' blocked by safe-mode" tool-name)]
    [else
     (define path-arg (extract-path-arg args))
     (cond
       ;; A non-path-value 'path is not this check's business: a schema-defined
       ;; tool rejects it in argument validation.
       [(not (or (path? path-arg) (string? path-arg))) #f]
       [(allowed-path? path-arg) #f]
       [else
        (format
         "Access denied: ~a is outside project root (~a). Safe mode restricts file access to the project directory."
         path-arg
         (safe-mode-project-root))])]))

;; ============================================================
;; Preflight stage (serial)
;; ============================================================

(define (run-preflight tool-calls registry hook-dispatcher [exec-ctx (make-exec-context)])
  ;; Returns a list of preflight entries, one per tool-call, in order.
  ;; Capability authority is the immutable snapshot carried by exec-ctx.
  (define capabilities
    (if (exec-context? exec-ctx)
        (exec-context-capabilities exec-ctx)
        '()))
  (for/list ([tc (in-list tool-calls)])
    (define tc-after-hook
      (if hook-dispatcher
          (with-handlers ([exn:fail? (lambda (e)
                                       ;; Hook itself threw -> treat as block with error
                                       (define msg (format "hook error: ~a" (exn-message e)))
                                       (preflight-entry 'error tc #f msg))])
            (define result (hook-dispatcher 'tool-call tc))
            (match result
              [#f tc] ; no handler -> pass through
              [(? hook-result?)
               (case (hook-result-action result)
                 [(block) 'blocked]
                 [(amend) (hook-result-payload result)]
                 [else tc])]
              [_ result]))
          tc))
    (match tc-after-hook
      ;; Hook returned 'blocked
      [(== 'blocked)
       (preflight-entry 'blocked tc #f (format "tool call '~a' blocked by hook" (tool-call-name tc)))]
      ;; Hook threw an exception (returned as list)
      [(? preflight-entry? (app preflight-entry-status 'error)) tc-after-hook]
      ;; Hook returned a (possibly modified) tool-call
      [(? tool-call?)
       (define t (lookup-tool registry (tool-call-name tc-after-hook)))
       (cond
         [(not t)
          ;; Tool not found in registry
          (preflight-entry 'error
                           tc-after-hook
                           #f
                           (format "unknown tool: '~a'" (tool-call-name tc-after-hook)))]
         [else
          ;; Safe-mode tool allowlist + path restriction (SEC-01/ARCH-02).
          ;; Shared with execution, which re-runs it on post-hook arguments.
          (define safe-mode-denial
            (safe-mode-invocation-denial (tool-call-name tc-after-hook)
                                         (tool-call-arguments tc-after-hook)))
          (cond
            [safe-mode-denial (preflight-entry 'blocked tc-after-hook #f safe-mode-denial)]
            ;; Check capability enforcement against context-owned authority.
            [(not (capability-authorized? (tool-required-capability t) capabilities))
             (preflight-entry
              'blocked
              tc-after-hook
              #f
              (format "tool '~a' requires capability '~a' which is not granted to this session"
                      (tool-call-name tc-after-hook)
                      (tool-required-capability t)))]
            [else
             ;; Revalidate arguments after potential hook mutation (v0.19.3 W1)
             ;; Capture exception detail to produce actionable error feedback
             (define validation-result
               (with-handlers ([exn:fail? (lambda (e) e)])
                 (validate-tool-args t (tool-call-arguments tc-after-hook))
                 #f))
             (if (not validation-result)
                 (preflight-entry 'ready tc-after-hook t #f)
                 (preflight-entry 'error
                                  tc-after-hook
                                  #f
                                  (format "~a. Usage: ~a"
                                          (exn-message validation-result)
                                          (format-tool-schema-hint t))))])])]
      ;; Unexpected hook return value -- treat as error
      [else (preflight-entry 'error tc #f (format "unexpected hook return: ~v" tc-after-hook))])))
