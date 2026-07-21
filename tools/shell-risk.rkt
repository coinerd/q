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
         (rename-out [tokenize-shell-command tokenize-shell-command]
                     [classify-shell-risks classify-shell-risks]
                     [shell-risk-summary shell-risk-summary]
                     [risk-severity? risk-severity?]
                     [token-type? token-type?]
                     [risk-type? risk-type?]))
