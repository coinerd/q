#lang racket/base

(require rackunit
         rackunit/text-ui
         (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion
                  task-conclusion?
                  task-conclusion-origin-message-ids
                  task-conclusion-relevance-tags
                  task-conclusion-category
                  task-conclusion-id)
         (only-in "../runtime/context-assembly/auto-distillation.rkt"
                  find-uncovered-entries
                  generate-fallback-conclusions
                  auto-distill
                  current-auto-distillation-enabled?))

(define c-with-coverage
  (task-conclusion "c1" "Found pattern" 'fact 'idle '("msg-1" "msg-2") 100 '() '()))

(define suite
  (test-suite "auto-distillation"
    (test-case "find-uncovered-entries with full coverage"
      (define uncovered (find-uncovered-entries '("msg-1" "msg-2") (list c-with-coverage)))
      (check-equal? uncovered '()))

    (test-case "find-uncovered-entries with partial coverage"
      (define uncovered (find-uncovered-entries '("msg-1" "msg-2" "msg-3") (list c-with-coverage)))
      (check-equal? (length uncovered) 1)
      (check-equal? (car uncovered) "msg-3"))

    (test-case "find-uncovered-entries with no conclusions"
      (define uncovered (find-uncovered-entries '("msg-1" "msg-2") '()))
      (check-equal? (length uncovered) 2))

    (test-case "find-uncovered-entries with empty WS"
      (check-equal? (find-uncovered-entries '() (list c-with-coverage)) '()))

    (test-case "generate-fallback-conclusions creates valid conclusions"
      (define fallbacks (generate-fallback-conclusions '("m1" "m2") 'implementation))
      (check-equal? (length fallbacks) 2)
      (for ([c (in-list fallbacks)])
        (check-true (task-conclusion? c))
        (check-equal? (task-conclusion-category c) 'fact)
        (check-not-false (member 'auto-distilled (task-conclusion-relevance-tags c)))))

    (test-case "auto-distill disabled returns empty"
      (parameterize ([current-auto-distillation-enabled? #f])
        (define result (auto-distill '("m1") '() 'idle))
        (check-equal? result '())))

    (test-case "auto-distill enabled with uncovered entries"
      (parameterize ([current-auto-distillation-enabled? #t])
        (define result (auto-distill '("m1" "m2") '() 'exploration))
        (check-equal? (length result) 2)
        (for ([c (in-list result)])
          (check-true (task-conclusion? c)))))

    (test-case "auto-distill enabled with all covered returns empty"
      (parameterize ([current-auto-distillation-enabled? #t])
        (define result (auto-distill '("msg-1") (list c-with-coverage) 'idle))
        (check-equal? result '())))))

(run-tests suite)
