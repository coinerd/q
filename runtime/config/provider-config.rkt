#lang racket/base

;; runtime/config/provider-config.rkt — Provider/LLM config struct
;; STABILITY: internal
;;
;; v0.96.7 (F6): Grouping provider-related parameters into a config struct.

(provide (struct-out provider-config)
         default-provider-config)

;; Provider configuration
(struct provider-config
        (external-backend?   ; boolean? — enable external backend
         external-timeout-ms ; integer? — timeout for external calls
         oauth-enabled       ; boolean? — enable OAuth flow
         )
  #:transparent)

(define (default-provider-config)
  (provider-config #f     ; external-backend?
                   5000   ; external-timeout-ms
                   #f))   ; oauth-enabled
