#lang racket/base

;; runtime/compactor-llm-bridge.rkt — LLM-backed summarization for compactor
;; M-05 (LAYER-03): Decoupled from compactor.rkt to eliminate LLM layer dependency.
;;
;; This module bridges the compactor (runtime layer) with LLM providers (core layer).
;; Import this module only in code that needs LLM-powered summarization.

(require racket/string
         racket/contract
         (only-in "../llm/model.rkt" make-model-request model-response-content model-response?)
         (only-in "../llm/provider.rkt" provider-send provider?)
         (only-in "compaction-prompts.rkt"
                  summary-prompt
                  iterative-update-prompt
                  format-messages-for-summary)
         (only-in "context-assembly/summary-entities.rkt"
                  extract-key-entities
                  check-entity-preservation
                  entity-preservation-appendix))

(provide (contract-out [llm-summarize
                        (->* (list? any/c any/c)
                             (#:previous-summary (or/c string? #f) #:file-tracker any/c)
                             string?)]
                       [make-llm-summarize-fn (-> any/c any/c procedure?)]))

;; LLM-powered summarization of message history.
;; When #:previous-summary is provided, uses iterative update prompt.
;; Returns the summary text string.
(define (llm-summarize messages
                       provider
                       model-name
                       #:previous-summary [prev-summary #f]
                       #:file-tracker [file-tracker (hasheq)])
  (define formatted (format-messages-for-summary messages))
  (define prompt-text
    (if prev-summary
        (iterative-update-prompt prev-summary formatted file-tracker)
        (summary-prompt formatted file-tracker)))
  ;; Build a minimal model-request with just the prompt
  (define req
    (make-model-request (list (hasheq 'role "user" 'content prompt-text))
                        #f
                        (hasheq 'model model-name 'max_tokens 2000)))
  (define resp (provider-send provider req))
  ;; Extract text from response content
  (define content (model-response-content resp))
  (define raw-summary
    (string-join (for/list ([part (in-list content)])
                   (cond
                     [(hash? part) (hash-ref part 'text "")]
                     [(string? part) part]
                     [else (format "~a" part)]))
                 ""))
  ;; SAL-04 quality gate: check entity preservation
  (define entities (extract-key-entities messages))
  (define missing (check-entity-preservation entities raw-summary))
  (define final-summary
    (if (null? missing)
        raw-summary
        (string-append raw-summary (entity-preservation-appendix missing))))
  ;; Reject empty or too-short summaries
  (when (< (string-length final-summary) 50)
    (log-warning (format "context-summary: LLM summary too short (~a chars)"
                         (string-length final-summary))))
  final-summary)

;; Create a summarize function suitable for use with compact-history.
;; The returned function has signature: (listof message?) -> string
(define (make-llm-summarize-fn provider model-name)
  (lambda (messages) (llm-summarize messages provider model-name)))
