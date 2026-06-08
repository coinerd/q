#lang racket/base

;; runtime/config/context-config.rkt — Context assembly config struct
;; STABILITY: internal
;;
;; v0.96.7 (F6): Grouping context-assembly parameters into a config struct.

(provide (struct-out context-config)
         default-context-config)

;; Context assembly configuration
(struct context-config
        (task-state-aware?        ; boolean? — enable task-state-aware assembly
         graph-conclusion?        ; boolean? — use graph-based conclusion selection
         conclusion-token-budget  ; integer? — max tokens for conclusions
         ws-evolution?            ; boolean? — enable working-set evolution
         state-inference-thresh   ; real? — threshold for state inference [0..1]
         auto-distillation?       ; boolean? — enable auto-distillation
         compact-rate-limit       ; integer? — max compactions per window
         compact-rate-window      ; integer? — window in seconds
         )
  #:transparent)

(define (default-context-config)
  (context-config #f     ; task-state-aware?
                  #f     ; graph-conclusion?
                  2000   ; conclusion-token-budget
                  #f     ; ws-evolution?
                  0.7    ; state-inference-thresh
                  #f     ; auto-distillation?
                  5      ; compact-rate-limit
                  60))   ; compact-rate-window
