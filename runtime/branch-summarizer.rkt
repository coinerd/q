#lang racket/base

;; runtime/branch-summarizer.rkt — FEAT-79: Branch summarization
;;
;; Summarizes abandoned branches of conversation using LLM.
;; Produces a compact summary that captures key decisions and
;; outcomes from an abandoned exploration path.

(require racket/contract
         racket/string
         (only-in "../util/protocol-types.rkt"
                  message? message-role message-content)
         (only-in "../runtime/compactor.rkt"
                  llm-summarize))

(provide
 (contract-out
  [summarize-branch (->* (list? procedure?) ((or/c string? #f)) (or/c string? #f))]))

;; ============================================================
;; summarize-branch : messages provider [model-name] -> (or/c string? #f)
;;
;; Takes a list of messages from an abandoned branch and an LLM
;; provider. Returns a summary string or #f if no messages.
;; ============================================================

(define (summarize-branch messages provider [model-name #f])
  (cond
    [(null? messages) #f]
    [else
     (define summary (llm-summarize messages provider model-name))
     (cond
       [(string? summary) summary]
       [(message? summary)
        (define c (message-content summary))
        (if (string? c) c (format "~a" c))]
       [else (format "~a" summary)])]))
