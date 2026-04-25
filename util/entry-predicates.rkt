#lang racket/base

;; util/entry-predicates.rkt — Entry kind predicates
;;
;; Extracted from protocol-types.rkt (ARCH-05 split).
;; Predicates for different message entry kinds.

(require racket/contract
         "message.rkt")

(provide message-entry?
         model-change-entry?
         thinking-level-change-entry?
         branch-summary-entry?
         custom-message-entry?
         session-info-entry?
         compaction-summary-entry?
         tool-result-entry?
         bash-execution-entry?
         any-tool-result-entry?)

;; ============================================================
;; Entry kind predicates (#497)
;; ============================================================
;; Entry kinds extend the message struct's `kind` field to support
;; session metadata and special entries beyond user/assistant messages.

(define (message-entry? msg)
  (and (message? msg) (memq (message-kind msg) '(message)) #t))

(define (model-change-entry? msg)
  (and (message? msg) (eq? (message-kind msg) 'model-change)))

(define (thinking-level-change-entry? msg)
  (and (message? msg) (eq? (message-kind msg) 'thinking-level-change)))

(define (branch-summary-entry? msg)
  (and (message? msg) (eq? (message-kind msg) 'branch-summary)))

(define (custom-message-entry? msg)
  (and (message? msg) (eq? (message-kind msg) 'custom-message)))

(define (session-info-entry? msg)
  (and (message? msg) (eq? (message-kind msg) 'session-info)))

(define (compaction-summary-entry? msg)
  (and (message? msg) (eq? (message-kind msg) 'compaction-summary)))

(define (tool-result-entry? msg)
  (and (message? msg) (eq? (message-kind msg) 'tool-result)))

(define (bash-execution-entry? msg)
  (and (message? msg) (eq? (message-kind msg) 'bash-execution)))

(define (any-tool-result-entry? msg)
  (and (message? msg) (memq (message-kind msg) '(tool-result bash-execution))))
