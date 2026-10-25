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
                  task-conclusion-fsm-state-origin
                  task-conclusion-timestamp
                  task-conclusion-dependencies
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
        ;; R99b: internal provenance records — never fact-labeled excerpts.
        (check-equal? (task-conclusion-category c) 'provenance)
        (check-not-false (member 'auto-distilled (task-conclusion-relevance-tags c)))
        (check-not-false (member 'deterministic-fallback (task-conclusion-relevance-tags c)))))

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

;; ---------------------------------------------------------------------------
;; v0.79.2 GAP-3 / R99b (BUG-0099): content summaries are NEVER distilled into
;; the record — the deterministic fallback is an honest provenance marker.
;; ---------------------------------------------------------------------------

(test-case "R99b: fallback NEVER includes source excerpt text even when content-summary is provided"
  (parameterize ([current-auto-distillation-enabled? #t])
    (define summaries
      (hash "m1" "SECRET-EXCERPT This file implements the authentication module using OAuth2 and JWT tokens"))
    (define result (auto-distill '("m1") '() 'exploration summaries))
    (check-equal? (length result) 1)
    (define c (car result))
    ;; Exact-equality is the strongest no-leak form: nothing from the summary
    ;; (nor any other phrasing) can be part of the text.
    (check-equal? (task-conclusion-text c)
                  "Source message m1 retained as provenance; no distilled insight available.")
    (check-false (string-contains? (task-conclusion-text c) "SECRET-EXCERPT"))
    (check-false (string-contains? (task-conclusion-text c) "OAuth2"))
    (check-false (string-contains? (task-conclusion-text c) "JWT"))
    ;; No fabricated previously-read-file claim either.
    (check-false (string-contains? (task-conclusion-text c) "Previously read file"))
    (check-false (string-contains? (task-conclusion-text c) "[Auto]"))))

(test-case "R99b: without content summary the same honest provenance text is used"
  (parameterize ([current-auto-distillation-enabled? #t])
    (define result (auto-distill '("m1") '() 'idle))
    (check-equal? (length result) 1)
    (check-equal? (task-conclusion-text (car result))
                  "Source message m1 retained as provenance; no distilled insight available.")))

(test-case "R99b: long content summaries never leak (no truncation channel)"
  (parameterize ([current-auto-distillation-enabled? #t])
    (define long-text (make-string 300 #\x))
    (define summaries (hash "m1" long-text))
    (define result (auto-distill '("m1") '() 'idle summaries))
    (check-equal? (task-conclusion-text (car result))
                  "Source message m1 retained as provenance; no distilled insight available.")))

(test-case "W0 F9: blank content summary yields the honest provenance record"
  (parameterize ([current-auto-distillation-enabled? #t])
    (define result (auto-distill '("m1") '() 'idle (hash "m1" "")))
    (check-equal? (length result) 1)
    (check-equal? (task-conclusion-category (car result)) 'provenance)
    (check-not-equal? (string-trim (task-conclusion-text (car result))) "[Auto]")
    (check-equal? (task-conclusion-text (car result))
                  "Source message m1 retained as provenance; no distilled insight available.")))

(test-case "W0 F9: whitespace-only content summary yields the honest provenance record"
  (parameterize ([current-auto-distillation-enabled? #t])
    (define result (auto-distill '("m1") '() 'idle (hash "m1" "   \n\t  ")))
    (check-equal? (length result) 1)
    (check-equal? (task-conclusion-category (car result)) 'provenance)
    (check-not-equal? (string-trim (task-conclusion-text (car result))) "[Auto]")
    (check-equal? (task-conclusion-text (car result))
                  "Source message m1 retained as provenance; no distilled insight available.")))

;; ---------------------------------------------------------------------------
;; R99a (BUG-0099): frozen legacy deterministic-fallback recognizer.
;; R99b (owner-authorized): the WRITER is now activated and emits internal
;; 'provenance records. Old-signature records remain recognized as provenance
;; via the frozen recognizer; NEW records are provenance by CATEGORY and carry
;; the explicit 'deterministic-fallback tag. The writer pins below are updated
;; INTENTIONALLY from the R99a byte-identity HOLD pins to the activated shape.
;; ---------------------------------------------------------------------------

;; A legacy-signature fallback record exactly as the pre-R99b deterministic
;; writer stored it (auto-<origin> id + [Auto] text + auto-distilled tag).
(define (legacy-fallback-record msg-id state [text "[Auto] Previously read file (origin: ~a)"])
  (task-conclusion (format "auto-~a" msg-id)
                   (format text msg-id)
                   'fact
                   state
                   (list msg-id)
                   0
                   '(auto-distilled)
                   '()))

(define (fallback-with-timestamp msg-id state [summary #f] [ts 0])
  ;; Struct-copy only the timestamp so the pinned bytes are deterministic.
  (define summaries (if summary (hash msg-id summary) (hash)))
  (struct-copy task-conclusion
               (car (generate-fallback-conclusions (list msg-id) state summaries))
               [timestamp ts]))

(define (fallback-json msg-id state [summary #f] [ts 0])
  (jsexpr->string (conclusion-hash->json-safe
                   (conclusion->hash (fallback-with-timestamp msg-id state summary ts)))))

(define-test-suite r99a-recognizer-tests
  (test-case "R99a: frozen recognizer still classifies the OLD legacy fallback signature (stored pre-R99b records)"
    (define c (legacy-fallback-record "m1" 'idle))
    (check-true (legacy-deterministic-fallback? c))
    (check-true (provenance-kind? c))
    (check-false (insight-kind? c)))

  (test-case "R99b: new writer records are provenance by CATEGORY, not by the legacy recognizer"
    (define c (car (generate-fallback-conclusions (list "m1") 'idle)))
    ;; Distinguishable by category: no [Auto] prefix, so the frozen legacy
    ;; recognizer does NOT match — the internal category does.
    (check-false (legacy-deterministic-fallback? c))
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

(define pin-prov-prefix "{\"category\":\"provenance\",\"dependencies\":[],")
(define pin-no-summary
  (string-append pin-prov-prefix
                 "\"fsm-state-origin\":\"idle\",\"id\":\"auto-m1\","
                 "\"origin-message-ids\":[\"m1\"],"
                 "\"relevance-tags\":[\"auto-distilled\",\"deterministic-fallback\"],"
                 "\"text\":\"Source message m1 retained as provenance; no distilled insight available.\","
                 "\"timestamp\":0}"))

(define pin-with-summary
  (string-append pin-prov-prefix
                 "\"fsm-state-origin\":\"implementation\",\"id\":\"auto-msg-42\","
                 "\"origin-message-ids\":[\"msg-42\"],"
                 "\"relevance-tags\":[\"auto-distilled\",\"deterministic-fallback\"],"
                 "\"text\":\"Source message msg-42 retained as provenance; no distilled insight available.\","
                 "\"timestamp\":0}"))

(define pin-blank-summary
  (string-append pin-prov-prefix
                 "\"fsm-state-origin\":\"idle\",\"id\":\"auto-m3\","
                 "\"origin-message-ids\":[\"m3\"],"
                 "\"relevance-tags\":[\"auto-distilled\",\"deterministic-fallback\"],"
                 "\"text\":\"Source message m3 retained as provenance; no distilled insight available.\","
                 "\"timestamp\":0}"))

(define-test-suite r99b-writer-pins-tests
  ;; R99b INTENTIONAL pin change (owner-authorized writer activation): the
  ;; R99a HOLD byte-identity pins are replaced by the activated writer's
  ;; honest provenance bytes — no excerpt, no previously-read-file claim.
  (test-case "R99b: fallback store bytes are the honest provenance record (no summary)"
    (check-equal? (fallback-json "m1" #f) pin-no-summary))

  (test-case "R99b: fallback store bytes are the honest provenance record (with summary)"
    ;; Intentional: the summary text NO LONGER appears anywhere in the record.
    (check-equal? (fallback-json "msg-42" 'implementation "found the bug in parser")
                  pin-with-summary))

  (test-case "R99b: long summaries never appear (fixed honest text, no truncation)"
    (define long-text (make-string 250 #\y))
    (define c (fallback-with-timestamp "m2" 'idle long-text))
    (check-equal? (task-conclusion-text c)
                  "Source message m2 retained as provenance; no distilled insight available.")
    (check-false (string-contains? (task-conclusion-text c) (make-string 200 #\y))))

  (test-case "R99b: blank summary uses the same honest provenance phrasing (pinned bytes)"
    (check-equal? (fallback-json "m3" 'idle "   ") pin-blank-summary))

  (test-case "R99b: flag ON — auto-distill output is a pinned, category-provenance record"
    (parameterize ([current-auto-distillation-enabled? #t])
      (define result (auto-distill '("m1") '() 'idle (hash "m1" "parser note")))
      (check-equal? (length result) 1)
      (define c (car result))
      (check-true (provenance-kind? c))
      (check-false (legacy-deterministic-fallback? c))
      (check-equal? (task-conclusion-id c) "auto-m1")
      (check-equal? (task-conclusion-category c) 'provenance)
      (check-equal? (task-conclusion-text c)
                    "Source message m1 retained as provenance; no distilled insight available.")
      (check-equal? (task-conclusion-origin-message-ids c) '("m1"))
      (check-equal? (task-conclusion-relevance-tags c) '(auto-distilled deterministic-fallback))
      (check-false (string-contains? (task-conclusion-text c) "parser note"))))

  (test-case "R99b: record shape — state passthrough, idle default, arity compatibility"
    ;; state forwarded; #f defaults to 'idle (unchanged behavior)
    (check-equal? (task-conclusion-fsm-state-origin
                   (car (generate-fallback-conclusions '("m9") 'exploration)))
                  'exploration)
    (check-equal? (task-conclusion-fsm-state-origin
                   (car (generate-fallback-conclusions '("m9") #f)))
                  'idle)
    ;; make-deterministic-fallback keeps its optional content-summary arity
    ;; (exercised here through the exported generator, which forwards the
    ;; per-id summary); R99b: the argument is accepted but unused for the text.
    (define c (car (generate-fallback-conclusions '("m7") 'planning (hash "m7" "any summary"))))
    (check-equal? (task-conclusion-text c)
                  "Source message m7 retained as provenance; no distilled insight available.")
    (check-equal? (task-conclusion-category c) 'provenance)
    (check-equal? (task-conclusion-dependencies c) '())
    (check-true (exact-positive-integer? (task-conclusion-timestamp c)))))

(run-tests r99a-recognizer-tests)
(run-tests r99b-writer-pins-tests)
