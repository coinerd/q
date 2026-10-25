#lang racket/base

;; @speed fast
;; @suite default
;; @boundary unit

(require rackunit
         racket/string
         rackunit/text-ui
         json
         (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion
                  task-conclusion?
                  task-conclusion-origin-message-ids
                  task-conclusion-relevance-tags
                  task-conclusion-category
                  task-conclusion-id
                  task-conclusion-text
                  conclusion->hash
                  legacy-deterministic-fallback?
                  provenance-kind?
                  insight-kind?)
         (only-in "../runtime/session/session-store-goal-task.rkt"
                  conclusion-hash->json-safe)
         (only-in "../runtime/context-assembly/auto-distillation.rkt"
                  find-uncovered-entries
                  generate-fallback-conclusions
                  auto-distill
                  legacy-deterministic-fallback?
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

    (test-case "auto-distill called directly returns results (caller decides)"
      ;; v0.99.87: auto-distill no longer internally checks current-auto-distillation-enabled?
      ;; The caller (prepare-turn-context-state) uses effective-auto-distill? to decide.
      (parameterize ([current-auto-distillation-enabled? #f])
        (define result (auto-distill '("m1") '() 'idle))
        (check-equal? (length result) 1)))

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

;; ---------------------------------------------------------------------------
;; R99a (BUG-0099): frozen legacy deterministic-fallback recognizer + writer
;; byte-identity pins. The R99b writer change is HELD: fallback output must stay
;; byte-identical to the pre-R99a deterministic writer.
;; ---------------------------------------------------------------------------

(define (fallback-with-timestamp msg-id state [summary #f] [ts 0])
  ;; Struct-copy only the timestamp so the pinned bytes are deterministic.
  ;; Uses the exported generator — the internal writer is unchanged (R99b HELD).
  (define summaries (if summary (hash msg-id summary) (hash)))
  (struct-copy task-conclusion
               (car (generate-fallback-conclusions (list msg-id) state summaries))
               [timestamp ts]))

(define (fallback-json msg-id state [summary #f] [ts 0])
  (jsexpr->string (conclusion-hash->json-safe
                   (conclusion->hash (fallback-with-timestamp msg-id state summary ts)))))

(define-test-suite r99a-recognizer-tests
  (test-case "R99a: full fallback signature is recognized (tag + auto-id + [Auto] prefix)"
    (define c (car (generate-fallback-conclusions (list "m1") 'idle)))
    (check-true (legacy-deterministic-fallback? c))
    (check-true (provenance-kind? c))
    (check-false (insight-kind? c)))

  (test-case "R99a: tag alone is NOT recognized (ambiguous record keeps its type)"
    (define c (task-conclusion "c-llm-1" "A genuine LLM finding" 'fact 'idle '() 100 '(auto-distilled) '()))
    (check-false (legacy-deterministic-fallback? c))
    (check-true (insight-kind? c)))

  (test-case "R99a: known prefix alone is NOT recognized"
    (define c (task-conclusion "c-llm-2" "[Auto] looks like fallback text" 'fact 'idle '() 100 '() '()))
    (check-false (legacy-deterministic-fallback? c))
    (check-true (insight-kind? c)))

  (test-case "R99a: auto-id alone is NOT recognized"
    (define c (task-conclusion "auto-m1" "Curated note stored under auto id" 'decision 'idle '("m1") 100 '() '()))
    (check-false (legacy-deterministic-fallback? c))
    (check-true (insight-kind? c)))

  (test-case "R99a: tag+prefix without stable auto-id is NOT recognized"
    (define c (task-conclusion "c-manual" "[Auto] partial match" 'fact 'idle '() 100 '(auto-distilled) '()))
    (check-false (legacy-deterministic-fallback? c)))

  (test-case "R99a: tag+id without [Auto] prefix is NOT recognized"
    (define c (task-conclusion "auto-m1" "Curated analysis" 'fact 'idle '("m1") 100 '(auto-distilled) '()))
    (check-false (legacy-deterministic-fallback? c)))

  (test-case "R99a: bare-auto id prefix without origin is NOT recognized"
    (define c (task-conclusion "auto-" "[Auto] x" 'fact 'idle '() 100 '(auto-distilled) '()))
    (check-false (legacy-deterministic-fallback? c)))

  (test-case "R99a: genuine LLM conclusions are untouched by the recognizer"
    (define c (task-conclusion "c-9" "Root cause: missing null check" 'error-cause 'debugging '("m5") 100 '(bug) '()))
    (check-false (legacy-deterministic-fallback? c))
    (check-false (provenance-kind? c))
    (check-true (insight-kind? c))))

(define pin-fact-prefix "{\"category\":\"fact\",\"dependencies\":[],")
(define pin-no-summary
  (string-append pin-fact-prefix
                 "\"fsm-state-origin\":\"idle\",\"id\":\"auto-m1\","
                 "\"origin-message-ids\":[\"m1\"],"
                 "\"relevance-tags\":[\"auto-distilled\"],"
                 "\"text\":\"[Auto] Previously read file (origin: m1)\","
                 "\"timestamp\":0}"))

(define pin-with-summary
  (string-append pin-fact-prefix
                 "\"fsm-state-origin\":\"implementation\",\"id\":\"auto-msg-42\","
                 "\"origin-message-ids\":[\"msg-42\"],"
                 "\"relevance-tags\":[\"auto-distilled\"],"
                 "\"text\":\"[Auto] found the bug in parser\","
                 "\"timestamp\":0}"))

(define pin-blank-summary
  (string-append pin-fact-prefix
                 "\"fsm-state-origin\":\"idle\",\"id\":\"auto-m3\","
                 "\"origin-message-ids\":[\"m3\"],"
                 "\"relevance-tags\":[\"auto-distilled\"],"
                 "\"text\":\"[Auto] Previously read file (origin: m3)\","
                 "\"timestamp\":0}"))

(define-test-suite r99a-byte-identity-tests
  ;; R99b HOLD pin: the deterministic writer output must be byte-identical to
  ;; the pre-R99a writer. These are the exact serialized store bytes.
  (test-case "R99a: fallback store bytes are byte-identical (no summary)"
    (check-equal? (fallback-json "m1" #f) pin-no-summary))

  (test-case "R99a: fallback store bytes are byte-identical (with summary)"
    (check-equal? (fallback-json "msg-42" 'implementation "found the bug in parser")
                  pin-with-summary))

  (test-case "R99a: fallback truncates summary at exactly 200 chars (pinned shape)"
    (define long-text (make-string 250 #\y))
    (define c (fallback-with-timestamp "m2" 'idle long-text))
    (check-equal? (string-length (task-conclusion-text c)) (+ (string-length "[Auto] ") 200))
    (check-true (string-prefix? (task-conclusion-text c) "[Auto] "))
    (check-equal? (substring (task-conclusion-text c) 7) (make-string 200 #\y)))

  (test-case "R99a: blank summary uses provenance fallback phrasing (pinned bytes)"
    (check-equal? (fallback-json "m3" 'idle "   ") pin-blank-summary))

  (test-case "R99a: flag ON — auto-distill output byte-identical and recognized"
    (parameterize ([current-auto-distillation-enabled? #t])
      (define result (auto-distill '("m1") '() 'idle (hash "m1" "parser note")))
      (check-equal? (length result) 1)
      (define c (car result))
      (check-true (legacy-deterministic-fallback? c))
      (check-equal? (task-conclusion-id c) "auto-m1")
      (check-equal? (task-conclusion-category c) 'fact)
      (check-equal? (task-conclusion-text c) "[Auto] parser note")
      (check-equal? (task-conclusion-origin-message-ids c) '("m1"))
      (check-equal? (task-conclusion-relevance-tags c) '(auto-distilled)))))

(run-tests r99a-recognizer-tests)
(run-tests r99a-byte-identity-tests)
