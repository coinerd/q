#lang racket/base
;; tests/test-gap6-llm-prompt-enrichment.rkt — GAP-6 TDD tests
;; Validates LLM distillation prompt enrichment with content summaries

(require rackunit
         rackunit/text-ui
         racket/string
         (only-in "../runtime/context-assembly/auto-distillation.rkt"
                  current-llm-distill-fn
                  current-auto-distillation-enabled?
                  auto-distill))

(define captured-prompt #f)
(define captured-args '())

;; Mock LLM distill function that captures its arguments
(define (mock-llm-distill uncovered-ids current-state [content-summaries (hash)])
  (set! captured-args (list uncovered-ids current-state content-summaries))
  ;; Return empty to verify no crash
  '())

(define-test-suite gap-6-tests
  (test-case "GAP-6: auto-distill passes content-summaries to LLM function"
    (set! captured-args '())
    (parameterize ([current-auto-distillation-enabled? #t]
                   [current-llm-distill-fn mock-llm-distill])
      (auto-distill '("id1" "id2") '() 'exploring
                    (hash "id1" "fixed auth bug in login.rkt"
                          "id2" "refactored utils module"))))
    (check = (length captured-args) 3 "Should receive 3 args")
    (check equal? (car captured-args) '("id1" "id2"))
    (check equal? (cadr captured-args) 'exploring)
    (check equal? (hash-ref (caddr captured-args) "id1" #f) "fixed auth bug in login.rkt"))

  (test-case "GAP-6: auto-distill works with empty content-summaries"
    (set! captured-args '())
    (parameterize ([current-auto-distillation-enabled? #t]
                   [current-llm-distill-fn mock-llm-distill])
      (auto-distill '("id3") '() 'exploring))
    (check = (length captured-args) 3)
    ;; Default content-summaries should be empty hash
    (check-equal? (hash-count (caddr captured-args)) 0))

  (test-case "GAP-6: auto-distill falls back when no LLM function"
    (parameterize ([current-auto-distillation-enabled? #t]
                   [current-llm-distill-fn #f])
      (define results (auto-distill '("id4") '() 'exploring
                                     (hash "id4" "some content")))
      ;; Should produce deterministic fallback conclusions
      (check-true (list? results))))

(run-tests gap-6-tests)
