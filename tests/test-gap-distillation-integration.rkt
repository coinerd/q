#lang racket/base

;; tests/test-gap-distillation-integration.rkt
;; v0.97.3 W2: Integration tests for distillation pipeline

(require rackunit
         racket/list
         racket/string
         rackunit/text-ui
         (only-in "../runtime/context-assembly/auto-distillation.rkt"
                  auto-distill
                  current-llm-distill-fn
                  current-auto-distillation-enabled?)
         (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion
                  task-conclusion?
                  task-conclusion-id
                  task-conclusion-text
                  task-conclusion-origin-message-ids))

(define (make-mock-conclusion id text)
  (task-conclusion (format "unique-~a" id) text 'fact 'exploration (list id) 0 '() '()))

(define suite
  (test-suite "distillation-pipeline-integration"

    ;; GAP-B: Conclusion IDs are unique
    (test-case "pipeline: conclusion IDs are unique per entry"
      (parameterize ([current-auto-distillation-enabled? #t]
                     [current-llm-distill-fn
                      (lambda (ids _state _summaries)
                        (for/list ([id (in-list ids)])
                          (make-mock-conclusion id (format "Summary for ~a" id))))])
        (define result (auto-distill '("msg-a" "msg-b" "msg-c") '() 'planning))
        (check-equal? (length result) 3)
        (define ids (map task-conclusion-id result))
        (check-equal? (length ids) (length (remove-duplicates ids)))
        (for ([c (in-list result)]
              [mid (in-list '("msg-a" "msg-b" "msg-c"))])
          (check-not-equal? (task-conclusion-id c) mid)
          (check-equal? (task-conclusion-origin-message-ids c) (list mid)))))

    ;; GAP-A resilience: LLM error → graceful fallback
    (test-case "pipeline: LLM error falls back to deterministic conclusions"
      (parameterize ([current-auto-distillation-enabled? #t]
                     [current-llm-distill-fn (lambda (_ids _state _summaries)
                                               (error "mock LLM failure"))])
        (define result (auto-distill '("m1" "m2" "m3") '() 'exploration))
        (check-equal? (length result) 3)
        (for ([c (in-list result)])
          (check-not-false (string-contains? (task-conclusion-text c) "Previously read")))))

    ;; GAP-C: Content summaries forwarded
    (test-case "pipeline: content summaries forwarded to LLM function"
      (parameterize ([current-auto-distillation-enabled? #t]
                     [current-llm-distill-fn (lambda (ids _state summaries)
                                               (check-true (hash? summaries))
                                               (check-equal? (hash-count summaries) 2)
                                               (for/list ([id (in-list ids)])
                                                 (make-mock-conclusion id (format "Entry ~a" id))))])
        (define summaries (hash "m1" "text content" "m2" "tool output result"))
        (define result (auto-distill '("m1" "m2") '() 'implementation summaries))
        (check-equal? (length result) 2)
        (check-true (task-conclusion? (car result)))))

    ;; GAP-C: No summaries → still works
    (test-case "pipeline: no content summaries still produces conclusions"
      (parameterize ([current-auto-distillation-enabled? #t]
                     [current-llm-distill-fn (lambda (ids _state summaries)
                                               (check-equal? (hash-count summaries) 0)
                                               (for/list ([id (in-list ids)])
                                                 (make-mock-conclusion id "no summary")))])
        (define result (auto-distill '("a" "b") '() 'idle))
        (check-equal? (length result) 2)
        (check-true (task-conclusion? (car result)))))))

(run-tests suite)
