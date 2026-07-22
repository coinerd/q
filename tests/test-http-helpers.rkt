#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: integration

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

;; parse-provider-url tests (v0.99.58 W1-1)

(test-case "parse-provider-url: https with no explicit port"
  (define-values (host path-str port ssl?) (parse-provider-url "https://api.example.com/v1/chat"))
  (check-equal? host "api.example.com")
  (check-true (regexp-match? #rx"chat" path-str))
  (check-equal? port 443)
  (check-true ssl?))

(test-case "parse-provider-url: http with explicit port"
  (define-values (host path-str port ssl?)
    (parse-provider-url "http://localhost:8080/api/v1/messages"))
  (check-equal? host "localhost")
  (check-true (regexp-match? #rx"messages" path-str))
  (check-equal? port 8080)
  (check-false ssl?))

(test-case "parse-provider-url: query string preserved in path"
  (define-values (host path-str port ssl?)
    (parse-provider-url "https://api.x.com/v1beta/models/gpt:stream?alt=sse"))
  (check-equal? host "api.x.com")
  (check-equal? port 443)
  (check-true ssl?))
