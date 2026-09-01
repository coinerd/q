#lang racket/base

;; runtime/permission/permission-gate.rkt — facade for permission policy
;; resolution, introduced by BUG-0055 remediation (v1.00.24 W3).
;;
;; Re-exports the canonical permission configuration from
;; tools/permission-gate.rkt and adds the spawn-specific resolution rule
;; required for BUG-0055: dangerous subagent spawns are granted
;; automatically only when the resolved permission policy is 'permissive
;; (--auto-approve) AND the spawn tool is not explicitly pinned to
;; interactive HITL via the needs-approval set.
;;
;; Precedence (authoritative):
;;   1. no resolved config or strict policy-mode  -> no auto-grant (HITL/deny)
;;   2. explicit needs-approval pinning           -> no auto-grant (explicit HITL)
;;   3. permissive policy-mode                    -> auto-grant, audited
;;
;; Strict/interactive modes and explicit deny are never bypassed.

(require racket/set
         "../../tools/permission-gate.rkt")

(provide (all-from-out "../../tools/permission-gate.rkt")
         spawn-permission-auto-grant)

;; (or/c permission-config? #f) (listof string?)
;;   -> (values boolean? symbol?)
;; Resolves whether a dangerous spawn may be auto-granted under the
;; resolved permission policy.  `spawn-tool-names` are the concrete spawn
;; tool identifiers (e.g. "spawn-subagent", "spawn-subagents"); membership
;; in the needs-approval set pins the tool to interactive HITL.
(define (spawn-permission-auto-grant config spawn-tool-names)
  (cond
    [(not config)
     (values #f 'no-permission-config)]
    [(not (eq? (permission-config-policy-mode config) 'permissive))
     (values #f 'policy-mode-not-permissive)]
    [(for/or ([name (in-list spawn-tool-names)])
       (set-member? (permission-config-needs-approval-tools config) name))
     (values #f 'explicit-hitl-pinned)]
    [else
     (values #t 'policy-mode-permissive)]))
