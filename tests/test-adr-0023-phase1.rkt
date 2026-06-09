#lang racket/base

;; @speed fast
;; @suite default
;; ADR-0023 Phase 1 test scaffolding — GAP-1, GAP-3, GAP-6
;; TDD red phase: tests verify behaviors that don't exist yet.

(require rackunit
         racket/string
         rackunit/text-ui
         (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion
                  task-conclusion?
                  task-conclusion-id
                  task-conclusion-text
                  task-conclusion-category
                  task-conclusion-fsm-state-origin
                  task-conclusion-origin-message-ids)
         (only-in "../runtime/context-assembly/auto-distillation.rkt"
                  auto-distill
                  current-auto-distillation-enabled?
                  current-llm-distill-fn))

;; ═══════════════════════════════════════════════════════════════
;; GAP-1: LLM distillation wiring
;; ═══════════════════════════════════════════════════════════════

(define gap1-suite
  (test-suite "ADR-0023 GAP-1: LLM distillation wiring"

    (test-case "GAP-1: current-llm-distill-fn receives uncovered IDs when set"
      ;; When current-llm-distill-fn is a procedure, auto-distill should
      ;; call it with uncovered IDs and current-state instead of deterministic fallback.
      (define captured-ids #f)
      (define captured-state #f)
      (define mock-distill
        (lambda (ids state)
          (set! captured-ids ids)
          (set! captured-state state)
          (list (task-conclusion "distilled-1"
                                 "Semantic summary from LLM"
                                 'fact
                                 state
                                 ids
                                 (current-seconds)
                                 '(auto-distilled)
                                 '()))))
      (parameterize ([current-auto-distillation-enabled? #t]
                     [current-llm-distill-fn mock-distill])
        (define results (auto-distill '("msg-1" "msg-2") '() 'exploration))
        (check-true (and (list? results) (= (length results) 1))
                    "auto-distill should return LLM-distilled results")
        (check-equal? captured-ids '("msg-1" "msg-2") "LLM fn should receive uncovered message IDs")
        (check-equal? captured-state 'exploration "LLM fn should receive current state")
        (check-equal? (task-conclusion-text (car results))
                      "Semantic summary from LLM"
                      "Returned conclusion should contain LLM summary text")))

    (test-case "GAP-1: auto-distill falls back to deterministic when LLM fn returns #f"
      (define mock-distill-bad
        (lambda (ids state)
          ;; Return non-list to trigger fallback
          "not a list"))
      (parameterize ([current-auto-distillation-enabled? #t]
                     [current-llm-distill-fn mock-distill-bad])
        (define results (auto-distill '("msg-1") '() 'exploration))
        ;; Should fall back to deterministic fallback
        (check-true (and (list? results) (= (length results) 1))
                    "Should produce fallback conclusions when LLM returns invalid result")
        (check-true (string-contains? (task-conclusion-text (car results)) "[Auto]")
                    "Fallback conclusions should have [Auto] prefix")))

    (test-case "GAP-1: auto-distill falls back when LLM fn throws"
      (define mock-distill-error (lambda (ids state) (error "LLM provider unavailable")))
      (parameterize ([current-auto-distillation-enabled? #t]
                     [current-llm-distill-fn mock-distill-error])
        (define results (auto-distill '("msg-1") '() 'exploration))
        (check-true (and (list? results) (= (length results) 1)) "Should fall back on LLM error")
        (check-true (string-contains? (task-conclusion-text (car results)) "[Auto]")
                    "Fallback should be deterministic")))))

;; ═══════════════════════════════════════════════════════════════
;; GAP-3: Tool result content passed to auto-extraction
;; ═══════════════════════════════════════════════════════════════
;; Note: Full integration test requires step-interpreter wiring.
;; This test verifies the extraction function accepts tool-result-like content.

(require (only-in "../runtime/memory/auto-extraction.rkt"
                  try-auto-extract
                  current-auto-extraction-enabled
                  looks-like-file-dump?
                  extract-candidates))

(define gap3-suite
  (test-suite "ADR-0023 GAP-3: Tool result auto-extraction"

    (test-case "GAP-3: extract-candidates handles tool-result-like content"
      ;; Tool results are typically single paragraphs of useful info
      (define tool-result-text
        "The authentication module uses HMAC-SHA256 for token signing with a 24-hour expiry window.")
      (define candidates (extract-candidates tool-result-text))
      ;; Short text may not produce candidates (single paragraph, length OK)
      ;; but should not crash
      (check-true (list? candidates)
                  "extract-candidates should return a list for tool-result-like text"))

    (test-case "GAP-3: file-dump content is filtered out"
      (define file-dump
        (string-join (for/list ([i (in-range 10)])
                       (format "import module~a" i))
                     "\n"))
      (check-true (looks-like-file-dump? file-dump)
                  "Multi-line import blocks should be detected as file dumps"))

    (test-case "GAP-3: short factual tool result is not a file dump"
      (define factual-result "The project uses Racket 8.12 with rackunit for testing.")
      (check-false (looks-like-file-dump? factual-result)
                   "Short factual results should not be classified as file dumps"))))

;; ═══════════════════════════════════════════════════════════════
;; GAP-6: Default context assembly profile is 'observe
;; ═══════════════════════════════════════════════════════════════

(require (only-in "../runtime/settings.rkt"
                  load-settings
                  setting-context-assembly-profile
                  q-settings?))

(define gap6-suite
  (test-suite "ADR-0023 GAP-6: Default profile is 'observe"

    (test-case "GAP-6: setting-context-assembly-profile defaults to 'observe"
      ;; Load settings with no config files — should get default profile
      (define settings (load-settings #:home-dir "/nonexistent/home"))
      (check-true (q-settings? settings) "load-settings should return q-settings?")
      (check-eq? (setting-context-assembly-profile settings)
                 'observe
                 "Default context-assembly profile should be 'observe"))))

;; ═══════════════════════════════════════════════════════════════
;; Run all suites
;; ═══════════════════════════════════════════════════════════════

(define all-suites
  (test-suite "ADR-0023 Phase 1 Tests"
    gap1-suite
    gap3-suite
    gap6-suite))

(module+ main
  (void (run-tests all-suites)))

(module+ test
  (require (submod ".." main)))
