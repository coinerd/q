#lang racket/base

;; runtime/config/tool-config.rkt — Tool subsystem config struct
;; STABILITY: internal
;;
;; v0.96.7 (F6): Grouping tool-related parameters into a config struct.

(provide (struct-out tool-config)
         default-tool-config)

;; Tool subsystem configuration
(struct tool-config
        (rollback-execution?   ; boolean? — enable rollback action execution
         safe-mode-locked?     ; boolean? — safe mode lock state
         )
  #:transparent)

(define (default-tool-config)
  (tool-config #f    ; rollback-execution?
               #f))  ; safe-mode-locked?
