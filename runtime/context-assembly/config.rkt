#lang racket/base
;; runtime/context-assembly/config.rkt — context-assembly configuration parameters
;; STABILITY: internal
;;
;; Thin config module to break the dependency cycle between
;; session-config.rkt and state-aware-builder.rkt.

(provide current-task-state-aware-assembly?
         current-graph-conclusion-selection?
         current-conclusion-token-budget
         current-ws-evolution-enabled?)

;; Whether to enable state-aware assembly for the current task.
(define current-task-state-aware-assembly? (make-parameter #f))

;; Whether to use graph-based conclusion selection.
(define current-graph-conclusion-selection? (make-parameter #f))

;; Token budget for conclusion generation.
(define current-conclusion-token-budget (make-parameter 2000))

;; Whether to enable working-set evolution.
(define current-ws-evolution-enabled? (make-parameter #f))
