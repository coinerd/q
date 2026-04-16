#lang racket/base

;; runtime/incremental-summarizer.rkt — FEAT-81: Incremental summarization
;;
;; Updates an existing summary with new messages instead of regenerating
;; from scratch. Uses the iterative-update-prompt from compactor.rkt
;; for efficient context updates.

(require racket/contract
         racket/string
         (only-in "../util/protocol-types.rkt"
                  message? message-role message-content)
         (only-in "../runtime/compactor.rkt"
                  llm-summarize
                  format-messages-for-summary
                  extract-file-tracker
                  find-previous-file-tracker))

(provide
 (contract-out
  [incremental-summarize (->* (list? string? procedure?)
                              ((or/c string? #f))
                              string?)]))

;; ============================================================
;; incremental-summarize : messages previous-summary provider [model-name] -> string
;;
;; Takes new messages, a previous summary string, and an LLM provider.
;; Returns an updated summary that incorporates the new messages.
;; ============================================================

(define (incremental-summarize new-messages previous-summary provider [model-name #f])
  (cond
    [(string=? previous-summary "")
     ;; No previous summary — generate fresh
     (define result (llm-summarize new-messages provider model-name))
     (if (string? result) result (format "~a" result))]
    [(null? new-messages)
     ;; No new messages — return previous unchanged
     previous-summary]
    [else
     ;; Incremental update
     (define result
       (llm-summarize new-messages provider model-name
                      #:previous-summary previous-summary))
     (if (string? result) result (format "~a" result))]))
