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
                  format-tool-schema-hint)
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
         run-preflight)

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
;; Preflight stage (serial)
;; ============================================================

(define (run-preflight tool-calls registry hook-dispatcher)
  ;; Returns a list of preflight entries, one per tool-call, in order.
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
          ;; Check safe-mode tool restrictions (SEC-01)
          (cond
            [(and (safe-mode?) (not (allowed-tool? (tool-call-name tc-after-hook))))
             (preflight-entry 'blocked
                              tc-after-hook
                              #f
                              (format "tool '~a' blocked by safe-mode"
                                      (tool-call-name tc-after-hook)))]
            ;; Check safe-mode path restrictions (ARCH-02)
            [(and (safe-mode?)
                  (let ([path-arg (extract-path-arg (tool-call-arguments tc-after-hook))])
                    (and path-arg (not (allowed-path? path-arg)))))
             (define path-arg (extract-path-arg (tool-call-arguments tc-after-hook)))
             (preflight-entry
              'blocked
              tc-after-hook
              #f
              (format
               "Access denied: ~a is outside project root (~a). Safe mode restricts file access to the project directory."
               path-arg
               (safe-mode-project-root)))]
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
