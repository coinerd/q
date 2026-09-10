#lang racket/base

;; Compatibility facade. The pure shell-risk classifier is owned by util/ so
;; runtime code can consume it without depending upward on the tools layer.

(require "../util/shell-risk.rkt")

(provide shell-token
         shell-token?
         shell-token-type
         shell-token-value
         shell-token-start
         shell-token-end
         shell-risk-finding
         shell-risk-finding?
         shell-risk-finding-type
         shell-risk-finding-severity
         shell-risk-finding-message
         shell-risk-finding-position
         ;; v1.00.29 W1 (BUG-0066, #9635): process-kill classification —
         ;; the facade exposes the kill-target surface so tests and the
         ;; guard's diagnostics can consume it through the same module.
         kill-target
         kill-target?
         kill-target-verb
         kill-target-severity
         kill-target-provenance
         kill-target-pattern
         kill-target-position
         kill-targets
         interpreter-kill-targets
         (rename-out [tokenize-shell-command tokenize-shell-command]
                     [classify-shell-risks classify-shell-risks]
                     [shell-risk-summary shell-risk-summary]
                     [risk-severity? risk-severity?]
                     [token-type? token-type?]
                     [risk-type? risk-type?]))
