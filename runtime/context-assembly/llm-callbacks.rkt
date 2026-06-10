#lang racket/base

;; runtime/context-assembly/llm-callbacks.rkt
;; H3b (v0.97.13): Named callback factories for LLM-powered context assembly.
;; Extracted from wiring/run-modes.rkt inline lambdas.

(require racket/string
         (only-in "../../llm/model.rkt" make-model-request model-response-content)
         (only-in "../../llm/provider.rkt" provider-send)
         (only-in "task-conclusion.rkt" task-conclusion)
         (only-in "../../util/ids.rkt" generate-id))

(provide make-distill-callback
         make-reflection-callback)

;; make-distill-callback : provider? string? -> procedure?
;; Returns a (-> (listof string?) (or/c symbol? #f) [(hash/c string? string?)] (listof task-conclusion?))
;; suitable for current-llm-distill-fn. Re-raises exceptions so the outer
;; distill-with-llm handler can fall back to deterministic generation.
(define (make-distill-callback prov model-name)
  (lambda (uncovered-ids current-state [content-summaries (hash)])
    (with-handlers
        ([exn:fail?
          (lambda (e)
            (raise
             e))]) ; re-raise so distill-with-llm's outer handler generates deterministic fallback
      ;; GAP-6: Include content summaries in prompt for richer distillation
      (define content-lines
        (for/list ([id (in-list uncovered-ids)])
          (define summary (hash-ref content-summaries id ""))
          (if (string=? summary "")
              (format "  ~a: (no content)" id)
              (format "  ~a: ~a"
                      id
                      (if (> (string-length summary) 300)
                          (string-append (substring summary 0 300) "...")
                          summary)))))
      (define prompt-text
        (format
         "For each working set entry, generate a brief conclusion about what was learned.\nState: ~a\nEntries:\n~a\nOutput one conclusion per line."
         (or current-state 'unknown)
         (string-join content-lines "\n")))
      (define resp
        (provider-send prov
                       (make-model-request (list (hasheq 'role "user" 'content prompt-text))
                                           #f
                                           (hasheq 'model model-name 'max_tokens 1000))))
      (define resp-parts (model-response-content resp))
      (define text
        (string-trim (string-join (for/list ([p (in-list resp-parts)])
                                    (cond
                                      [(hash? p) (hash-ref p 'text "")]
                                      [(string? p) p]
                                      [else ""]))
                                  "")))
      (define llm-lines (string-split text "\n"))
      (define n-llm-lines (length llm-lines))
      ;; GAP-A: indexed iteration with fallback for truncated LLM output
      ;; GAP-B: unique conclusion IDs via generate-id
      (if (string=? text "")
          '()
          (for/list ([id (in-list uncovered-ids)]
                     [i (in-naturals)])
            (define line-text
              (if (< i n-llm-lines)
                  (string-trim (list-ref llm-lines i))
                  (format "[auto] uncovered entry ~a" id)))
            (task-conclusion (generate-id)
                             line-text
                             'fact
                             (or current-state 'unknown)
                             (list id)
                             (current-seconds)
                             '()
                             '()))))))

;; make-reflection-callback : provider? string? -> procedure?
;; Returns a (-> (listof string?) string?) suitable for current-reflection-llm-fn.
;; On failure, falls back to sorted join of input contents.
(define (make-reflection-callback prov model-name)
  (lambda (contents)
    (with-handlers ([exn:fail? (lambda (e) (string-join (sort contents string<?) "; "))])
      (define prompt-text
        (format
         "Synthesize these related memory items into one concise observation:\n~a\nOutput one synthesized observation."
         (string-join contents "\n- ")))
      (define resp
        (provider-send prov
                       (make-model-request (list (hasheq 'role "user" 'content prompt-text))
                                           #f
                                           (hasheq 'model model-name 'max_tokens 500))))
      (define resp-parts (model-response-content resp))
      (define text
        (string-trim (string-join (for/list ([p (in-list resp-parts)])
                                    (cond
                                      [(hash? p) (hash-ref p 'text "")]
                                      [(string? p) p]
                                      [else ""]))
                                  "")))
      (if (string=? text "")
          (string-join (sort contents string<?) "; ")
          text))))
