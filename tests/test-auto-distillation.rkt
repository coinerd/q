#lang racket/base

(require rackunit
         racket/string
         rackunit/text-ui
         (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion
                  task-conclusion?
                  task-conclusion-origin-message-ids
                  task-conclusion-relevance-tags
                  task-conclusion-category
                  task-conclusion-id
                  task-conclusion-text)
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

;; v0.79.2 GAP-3: Content summary produces richer fallback text
(test-case "auto-distill with content summary produces richer text"
  (parameterize ([current-auto-distillation-enabled? #t])
    (define summaries (hash "m1" "This file implements the authentication module using OAuth2"))
    (define result (auto-distill '("m1") '() 'exploration summaries))
    (check-equal? (length result) 1)
    (define c (car result))
    (check-not-false (string-contains? (task-conclusion-text c) "OAuth2"))
    (check-false (string-contains? (task-conclusion-text c) "Previously read file"))))

(test-case "auto-distill without content summary uses placeholder"
  (parameterize ([current-auto-distillation-enabled? #t])
    (define result (auto-distill '("m1") '() 'idle))
    (check-equal? (length result) 1)
    (check-not-false (string-contains? (task-conclusion-text (car result)) "Previously read file"))))

(test-case "auto-distill passes content summaries to fallback"
  (parameterize ([current-auto-distillation-enabled? #t])
    (define summaries (hash "m1" "Authentication uses JWT tokens with refresh"))
    (define result (auto-distill '("m1") '() 'exploration summaries))
    (check-equal? (length result) 1)
    (check-not-false (string-contains? (task-conclusion-text (car result)) "JWT"))))

(test-case "content summary truncates at 200 chars"
  (parameterize ([current-auto-distillation-enabled? #t])
    (define long-text (make-string 300 #\x))
    (define summaries (hash "m1" long-text))
    (define result (auto-distill '("m1") '() 'idle summaries))
    (check-true (<= (string-length (task-conclusion-text (car result))) 210))))

;; ---------------------------------------------------------------------------
;; v0.95.18 W0: Blank Auto summary regression (expected red before W2)
;; ---------------------------------------------------------------------------

(test-case "W0 F9: blank content summary falls back instead of bare [Auto]"
  (parameterize ([current-auto-distillation-enabled? #t])
    (define result (auto-distill '("m1") '() 'idle (hash "m1" "")))
    (check-equal? (length result) 1)
    (check-not-equal? (string-trim (task-conclusion-text (car result))) "[Auto]")
    (check-not-false (string-contains? (task-conclusion-text (car result)) "Previously read file"))))

(test-case "W0 F9: whitespace-only content summary falls back instead of bare [Auto]"
  (parameterize ([current-auto-distillation-enabled? #t])
    (define result (auto-distill '("m1") '() 'idle (hash "m1" "   \n\t  ")))
    (check-equal? (length result) 1)
    (check-not-equal? (string-trim (task-conclusion-text (car result))) "[Auto]")
    (check-not-false (string-contains? (task-conclusion-text (car result)) "Previously read file"))))
