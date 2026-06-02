#lang racket/base

;; runtime/context-assembly/config.rkt — Consolidated context-assembly configuration
;;
;; Replaces 12 individual parameters with a single struct.
;; v0.80.3: Introduced. Old parameters kept as deprecated wrappers.

;; Struct
(provide (struct-out context-assembly-config)
         ;; Constructor with defaults
         default-context-assembly-config
         ;; Current config parameter (eventually replaces all 12)
         current-context-assembly-config)

;; ============================================================
;; Config struct
;; ============================================================

;; Consolidates the 12 context-assembly parameters into a single record.
;; Fields are grouped by subsystem.

(struct context-assembly-config
        ;; State-aware assembly
        (state-aware? ; current-task-state-aware-assembly?
         graph-selection? ; current-graph-conclusion-selection?
         conclusion-token-budget ; current-conclusion-token-budget
         ws-evolution? ; current-ws-evolution-enabled?
         ;; Auto-distillation
         auto-distill? ; current-auto-distillation-enabled?
         auto-distill-callback ; current-llm-distill-fn
         ;; Rollback actions
         rollback? ; current-rollback-action-execution?
         force-distill-callback ; current-force-distill-fn
         expand-context-callback ; current-expand-context-fn
         revert-state-callback ; current-revert-state-fn
         rollback-log ; current-rollback-action-log
         ;; State inference
         state-inference-threshold) ; current-state-inference-threshold
  #:transparent)

;; Default config — all features off, standard budget, threshold 0.7
(define (default-context-assembly-config)
  (context-assembly-config #f ; state-aware?
                           #f ; graph-selection?
                           2000 ; conclusion-token-budget
                           #f ; ws-evolution?
                           #f ; auto-distill?
                           #f ; auto-distill-callback
                           #f ; rollback?
                           #f ; force-distill-callback
                           #f ; expand-context-callback
                           #f ; revert-state-callback
                           '() ; rollback-log
                           0.7)) ; state-inference-threshold

;; Global config parameter — for gradual migration
(define current-context-assembly-config (make-parameter (default-context-assembly-config)))
