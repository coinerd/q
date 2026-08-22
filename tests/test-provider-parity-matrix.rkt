#lang racket/base

;; @speed fast
;; @suite default
;; @boundary unit

;; tests/test-provider-parity-matrix.rkt — Provider parity matrix (v0.99.87 W3)
;;
;; Defines the cross-provider parity tests required by roadmap W3
;; (issue #9215). These tests assert EXISTING behavior only — no abstraction
;; is built. Characterization tests (P3, P8) pin the current duplication
;; inventory so that v0.99.91 must update them deliberately when extracting
;; or aligning anything.
;;
;; Matrix report: docs/reports/PROVIDER-DUPLICATION-PARITY-MATRIX-v0.99.87.md

(require rackunit
         racket/list
         racket/port
         racket/string
         json
         "../llm/anthropic.rkt"
         "../llm/gemini.rkt"
         "../llm/openai-compatible.rkt"
         "../llm/azure-openai.rkt"
         "../llm/http-helpers.rkt"
         "../llm/provider-errors.rkt"
         "../llm/model.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (file-text path)
  (call-with-input-file path port->string))

(define provider-module-paths
  '("../llm/anthropic/sse.rkt" "../llm/gemini.rkt"
                               "../llm/openai-compatible.rkt"
                               "../llm/azure-openai.rkt"))

;; ============================================================
;; P1 — structural: non-streaming HTTP delegates to shared helper
;; ============================================================

(test-case "P1: all provider modules delegate non-streaming HTTP to make-provider-http-request"
  (for ([path (in-list provider-module-paths)])
    (check-true (string-contains? (file-text path) "make-provider-http-request")
                (format "~a must delegate to make-provider-http-request (C1)" path))))

;; ============================================================
;; P2 — structural: all stream paths use shared SSE driver
;; ============================================================

(test-case "P2: all provider stream paths use stream-sse-events"
  (for ([path (in-list provider-module-paths)])
    (check-true (string-contains? (file-text path) "stream-sse-events")
                (format "~a must use shared stream-sse-events (C2)" path))))

;; ============================================================
;; P3 — characterization: inline status-regexp sites pinned (C10)
;;
;; extract-status-code exists in http-helpers.rkt, yet each provider stream
;; setup still extracts the status code with an inline regexp. Pin the exact
;; site count so removal in v0.99.91 requires updating this test.
;; ============================================================

(define (count-inline-status-regexp text)
  (define m (regexp-match-positions* #rx"regexp-match[^\n]*HTTP/\\[" text))
  (length m))

(test-case "P3: inline HTTP status regexp sites pinned at exactly 4 (C10 accidental duplication)"
  (define counts
    (for/list ([path (in-list provider-module-paths)])
      (cons path (count-inline-status-regexp (file-text path)))))
  (for ([pair (in-list counts)])
    (check-equal? (cdr pair) 1 (format "~a inline status-regexp count" (car pair))))
  (check-equal? (apply + (map cdr counts)) 4))

;; ============================================================
;; P4 — behavioral: canonical usage keys from each parse-response
;; ============================================================

(define anthropic-fixture
  (hasheq 'model
          "claude-test"
          'id
          "msg_1"
          'stop_reason
          "end_turn"
          'usage
          (hasheq 'input_tokens 10 'output_tokens 5)
          'content
          (list (hasheq 'type "text" 'text "hi"))))

(define gemini-fixture
  (hasheq 'modelVersion
          "gemini-test"
          'usageMetadata
          (hasheq 'promptTokenCount 7 'candidatesTokenCount 3 'totalTokenCount 10)
          'candidates
          (list (hasheq 'content (hasheq 'parts (list (hasheq 'text "hi")))) 'finishReason "STOP")))

(define openai-fixture
  (hasheq 'model
          "gpt-test"
          'id
          "cmpl_1"
          'usage
          (hasheq 'prompt_tokens 4 'completion_tokens 2 'total_tokens 6)
          'choices
          (list (hasheq 'message (hasheq 'content "hi" 'role "assistant") 'finish_reason "stop"))))

(define (check-canonical-usage! usage who)
  (check-true (hash? usage) (format "~a usage must be a hash" who))
  (for ([key (in-list '(prompt_tokens completion_tokens total_tokens))])
    (check-true (hash-has-key? usage key) (format "~a usage missing key ~a" who key))
    (check-true (exact-nonnegative-integer? (hash-ref usage key))
                (format "~a usage ~a must be a nonnegative integer" who key))))

(test-case "P4: parse-response yields canonical usage keys per provider"
  (check-canonical-usage! (model-response-usage (anthropic-parse-response anthropic-fixture))
                          "anthropic")
  (check-equal? (hash-ref (model-response-usage (anthropic-parse-response anthropic-fixture))
                          'prompt_tokens)
                10)
  (check-equal? (hash-ref (model-response-usage (anthropic-parse-response anthropic-fixture))
                          'completion_tokens)
                5)
  (check-canonical-usage! (model-response-usage (gemini-parse-response gemini-fixture)) "gemini")
  (check-equal? (hash-ref (model-response-usage (gemini-parse-response gemini-fixture))
                          'prompt_tokens)
                7)
  (check-canonical-usage! (model-response-usage (openai-parse-response openai-fixture)) "openai")
  (check-equal? (hash-ref (model-response-usage (openai-parse-response openai-fixture)) 'total_tokens)
                6))

;; ============================================================
;; P5 — behavioral: stop-reason canonical mapping
;; ============================================================

(test-case "P5: translate-stop-reason maps provider reasons to canonical symbols"
  (check-eq? (translate-stop-reason 'anthropic "end_turn") 'stop)
  (check-eq? (translate-stop-reason 'anthropic "max_tokens") 'length)
  (check-eq? (translate-stop-reason 'anthropic "tool_use") 'tool-calls)
  (check-eq? (translate-stop-reason 'gemini "STOP") 'stop)
  (check-eq? (translate-stop-reason 'gemini "SAFETY") 'content-filtered)
  (check-eq? (translate-stop-reason 'gemini "MAX_TOKENS") 'length)
  (check-eq? (translate-stop-reason #f "tool_calls") 'tool-calls)
  (check-eq? (translate-stop-reason #f "stop") 'stop))

;; ============================================================
;; P6 — behavioral: error classification parity across status checkers
;; ============================================================

(define (status-line-for code)
  (string->bytes/utf-8 (format "HTTP/1.1 ~a Reason" code)))

(define error-body (string->bytes/utf-8 "{\"error\":{\"message\":\"boom\"}}"))

(define (raised-category thunk)
  (with-handlers ([provider-error? (lambda (e) (provider-error-category e))])
    (thunk)
    'no-error-raised))

(test-case "P6: shared and azure status checkers raise provider-error with parity categories"
  (for ([pair (in-list '((400 . bad-request) (401 . auth) (429 . rate-limit) (500 . server)))])
    (define code (car pair))
    (define expected (cdr pair))
    (check-eq?
     (raised-category (lambda () (check-provider-status! "Parity" (status-line-for code) error-body)))
     expected
     (format "check-provider-status! ~a" code))
    (check-eq? (raised-category (lambda () (check-azure-status! (status-line-for code) error-body)))
               expected
               (format "check-azure-status! ~a" code)))
  ;; azure bespoke checker accepts 200 without raising
  (check-eq? (raised-category (lambda () (check-azure-status! (status-line-for 200) #"")))
             'no-error-raised))

;; ============================================================
;; P7 — behavioral: stream-chunk contract from each chunk parser
;; ============================================================

(test-case "P7: per-provider chunk parsers yield valid stream-chunks"
  ;; Anthropic text delta
  (define a-chunks
    (anthropic-parse-single-event
     (hasheq 'type "content_block_delta" 'delta (hasheq 'type "text_delta" 'text "hi"))
     (box #f)
     (box #f)
     (box 0)))
  (check-equal? (length a-chunks) 1)
  (check-true (stream-chunk? (car a-chunks)))
  (check-equal? (stream-chunk-delta-text (car a-chunks)) "hi")
  (check-false (stream-chunk-done? (car a-chunks)))
  ;; Anthropic done chunk with usage
  (define a-done
    (anthropic-parse-single-event (hasheq 'type
                                          "message_delta"
                                          'delta
                                          (hasheq 'stop_reason "end_turn")
                                          'usage
                                          (hasheq 'output_tokens 5))
                                  (box #f)
                                  (box #f)
                                  (box 0)))
  (check-true (stream-chunk-done? (car a-done)))
  (check-true (hash? (stream-chunk-usage (car a-done))))
  ;; Gemini text part
  (define g-chunks
    (gemini-parse-single-event
     (hasheq 'candidates (list (hasheq 'content (hasheq 'parts (list (hasheq 'text "yo"))))))))
  (check-equal? (length g-chunks) 1)
  (check-equal? (stream-chunk-delta-text (car g-chunks)) "yo")
  ;; OpenAI-compatible chunk; JSON null usage must coerce to #f
  (define o-chunk
    (normalize-openai-chunk
     (hasheq 'choices (list (hasheq 'delta (hasheq 'content "hey") 'finish_reason #f)) 'usage 'null)))
  (check-true (stream-chunk? o-chunk))
  (check-equal? (stream-chunk-delta-text o-chunk) "hey")
  (check-false (stream-chunk-usage o-chunk)))

;; ============================================================
;; P8 — characterization: parity gaps pinned (G1–G3)
;;
;; These asymmetries are documented in the matrix report §4. Any alignment
;; in v0.99.91 must update these tests deliberately.
;; ============================================================

(test-case "P8: documented parity gaps remain pinned (per-model timeout, stream error wrap, azure checker)"
  ;; G2 (closed by v1.00.13 W2 #9466): per-model timeout policy is consumed
  ;; via the resolved request-network policy in ALL provider modules; no
  ;; adapter reads raw timeout config anymore.
  (for ([path (in-list '("../llm/openai-compatible.rkt" "../llm/anthropic/sse.rkt"
                                                        "../llm/gemini.rkt"
                                                        "../llm/azure-openai.rkt"))])
    (check-true (string-contains? (file-text path) "resolve-request-network-policy-for-model")
                (format "~a must consume the resolved policy (G2 unification)" path))
    (check-false (string-contains? (file-text path) "effective-request-timeout-for")
                 (format "~a unexpectedly uses raw per-model timeout (G2 changed)" path)))
  ;; G3: stream-phase error wrapping only in openai-compatible.rkt
  (check-true (string-contains? (file-text "../llm/openai-compatible.rkt")
                                "openai-wrap-stream-error"))
  (for ([path (in-list '("../llm/anthropic/sse.rkt" "../llm/gemini.rkt" "../llm/azure-openai.rkt"))])
    (check-false (string-contains? (file-text path) "wrap-stream-error")
                 (format "~a unexpectedly wraps stream errors (G3 changed)" path)))
  ;; G1: azure keeps its bespoke status checker
  (check-true (string-contains? (file-text "../llm/azure-openai.rkt") "check-azure-status!")))
