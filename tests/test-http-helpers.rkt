#lang racket/base

(require rackunit
         "../llm/http-helpers.rkt")

;; translate-stop-reason tests

(test-case "anthropic: end_turn -> stop"
  (check-equal? (translate-stop-reason 'anthropic "end_turn") 'stop))

(test-case "anthropic: max_tokens -> length"
  (check-equal? (translate-stop-reason 'anthropic "max_tokens") 'length))

(test-case "anthropic: stop_sequence -> stop"
  (check-equal? (translate-stop-reason 'anthropic "stop_sequence") 'stop))

(test-case "anthropic: tool_use -> tool-calls"
  (check-equal? (translate-stop-reason 'anthropic "tool_use") 'tool-calls))

(test-case "anthropic: unknown string -> string->symbol"
  (check-equal? (translate-stop-reason 'anthropic "something_new") 'something_new))

(test-case "anthropic: symbol passthrough"
  (check-equal? (translate-stop-reason 'anthropic 'already-a-symbol) 'already-a-symbol))

(test-case "gemini: STOP -> stop"
  (check-equal? (translate-stop-reason 'gemini "STOP") 'stop))

(test-case "gemini: MAX_TOKENS -> length"
  (check-equal? (translate-stop-reason 'gemini "MAX_TOKENS") 'length))

(test-case "gemini: SAFETY -> content-filtered"
  (check-equal? (translate-stop-reason 'gemini "SAFETY") 'content-filtered))

(test-case "gemini: RECITATION -> content-filtered"
  (check-equal? (translate-stop-reason 'gemini "RECITATION") 'content-filtered))

(test-case "openai-family: stop -> stop (default mapping)"
  (check-equal? (translate-stop-reason #f "stop") 'stop))

(test-case "openai-family: tool_calls -> tool-calls (underscore->hyphen)"
  (check-equal? (translate-stop-reason #f "tool_calls") 'tool-calls))

(test-case "openai-family: length -> length"
  (check-equal? (translate-stop-reason #f "length") 'length))

(test-case "fallback for non-string/symbol"
  (check-equal? (translate-stop-reason #f 42) 'stop))

(test-case "string-trim applied"
  (check-equal? (translate-stop-reason 'anthropic "  end_turn  ") 'stop))
