#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; tests/test-ws-token-accounting.rkt — P6B: Integration test for working-set token coverage
;;
;; Verifies that working-set-token-count returns non-zero values after
;; tool-result processing (v0.99.68-hotfix-3 P6A fix).
;;
;; Depends on P6A: estimate-message-tokens now includes tool-result-part content.

(require rackunit
         rackunit/text-ui
         racket/list
         "../runtime/working-set.rkt"
         "../runtime/context/context-policy.rkt"
         "../util/message/message.rkt"
         "../util/content/content-parts.rkt")

;; ── Helpers ──

;; Mock message struct for tests that don't use real message structs
(struct mock-msg (id text) #:transparent)
(define (mock-id m)
  (mock-msg-id m))
(define (mock-tokens m)
  (string-length (mock-msg-text m)))

;; Make a tool-call hash (same shape as tool-turn-bridge produces)
(define (make-hash-tool name [path ""])
  (hasheq 'name name 'arguments (hasheq 'path path)))

;; Make a real message struct with tool-result content parts
(define (make-tool-result-msg id tool-call-id content)
  (message id
           #f
           'tool
           'message
           (list (make-tool-result-part tool-call-id content #f))
           (current-seconds)
           (hasheq)))

;; Make a real message struct with text content parts
(define (make-text-msg id text)
  (message id #f 'user 'message (list (make-text-part text)) (current-seconds) (hasheq)))

;; ── Test suites ──

(define ws-token-accounting-tests
  (test-suite "Working-Set Token Accounting (P6B)"

    ;; ── T01: token-fn for mock returns > 0 for non-empty content ──
    (test-case "T01: token-fn returns > 0 for mock message with content"
      (define ws (make-working-set))
      (define tc (list (make-hash-tool "read" "/tmp/test.rkt")))
      ;; Use mock-tokens = string-length (guaranteed > 0)
      (define result-msgs (list (mock-msg "m1" "file content here")))
      (working-set-update! ws tc result-msgs mock-id mock-tokens)
      (check-equal? (working-set-entry-count ws) 1)
      (check-true (positive? (working-set-token-count ws))
                  "working-set-token-count should be > 0 after read with content"))

    ;; ── T02: real message with text-part → working-set-token-count > 0 ──
    (test-case "T02: real text-part message yields non-zero tokens"
      (define ws (make-working-set))
      (define tc (list (make-hash-tool "read" "/tmp/foo.rkt")))
      (define rm (list (make-text-msg "m2" "This file contains important code.")))
      (working-set-update! ws tc rm message-id estimate-message-tokens)
      (check-equal? (working-set-entry-count ws) 1)
      (check-true (positive? (working-set-token-count ws))))

    ;; ── T03: real tool-result message → working-set-token-count > 0 ──
    ;; THIS IS THE CORE P6B TEST: before P6A, this returned 0.
    (test-case "T03: tool-result message yields non-zero tokens (P6A fix)"
      (define ws (make-working-set))
      (define tc (list (make-hash-tool "read" "/tmp/bar.rkt")))
      (define rm (list (make-tool-result-msg "m3" "tc1" "File content from read tool")))
      (working-set-update! ws tc rm message-id estimate-message-tokens)
      (check-equal? (working-set-entry-count ws) 1)
      (check-true (positive? (working-set-token-count ws))
                  "tool-result message should yield > 0 tokens after P6A fix"))

    ;; ── T04: empty tool-result content → working-set-token-count may be 0 ──
    (test-case "T04: empty tool-result content yields 0 tokens"
      (define ws (make-working-set))
      (define tc (list (make-hash-tool "read" "/tmp/empty.rkt")))
      (define rm (list (make-tool-result-msg "m4" "tc2" "")))
      (working-set-update! ws tc rm message-id estimate-message-tokens)
      (check-equal? (working-set-entry-count ws) 1)
      (check-equal? (working-set-token-count ws) 0 "empty tool-result content should yield 0 tokens"))

    ;; ── T05: multiple read tool results → sum > 0 ──
    (test-case "T05: multiple reads accumulate non-zero tokens"
      (define ws (make-working-set))
      (define tc (list (make-hash-tool "read" "/tmp/a.rkt") (make-hash-tool "read" "/tmp/b.rkt")))
      (define rm
        (list (make-tool-result-msg "m5" "tc3" "File A content")
              (make-tool-result-msg "m6" "tc4" "File B content")))
      (working-set-update! ws tc rm message-id estimate-message-tokens)
      (check-equal? (working-set-entry-count ws) 2)
      (check-true (positive? (working-set-token-count ws))))

    ;; ── T06: mix of reads and edits preserves non-zero tokens ──
    (test-case "T06: mixed read+edit sequence preserves non-zero tokens"
      (define ws (make-working-set))
      ;; Read foo
      (working-set-update! ws
                           (list (make-hash-tool "read" "/tmp/foo.rkt"))
                           (list (make-tool-result-msg "m7" "tc5" "foo content"))
                           message-id
                           estimate-message-tokens)
      (check-true (positive? (working-set-token-count ws)))
      ;; Edit foo → removes entry
      (working-set-update! ws
                           (list (make-hash-tool "edit" "/tmp/foo.rkt"))
                           (list (make-text-msg "m8" "edited"))
                           message-id
                           estimate-message-tokens)
      ;; After edit: foo removed, no new reads → 0 entries
      (check-equal? (working-set-entry-count ws) 0))

    ;; ── T07: estimate-message-tokens-cached also non-zero for tool-result ──
    (test-case "T07: cached estimation works for tool-result messages"
      (define msg (make-tool-result-msg "m9" "tc6" "Some tool result text"))
      (define tokens (estimate-message-tokens-cached msg))
      (check-true (positive? tokens) "cached estimate should be > 0 for tool-result with content"))

    ;; ── T08: tool result with nested complex content (hash/list) ──
    (test-case "T08: tool result with nested content (hash/list)"
      (define complex-content
        (hash 'stdout "Build succeeded" 'files '("/tmp/a.rkt" "/tmp/b.rkt") 'exit_code 0))
      (define rm (make-tool-result-msg "m10" "tc7" complex-content))
      (define ws (make-working-set))
      (define tc (list (make-hash-tool "read" "/tmp/complex.rkt")))
      (working-set-update! ws tc (list rm) message-id estimate-message-tokens)
      (check-equal? (working-set-entry-count ws) 1)
      ;; tool-result-content->string handles hashes, so this should be > 0
      (check-true (positive? (working-set-token-count ws))
                  "complex nested content should still yield > 0 tokens"))))

;; ── Runner ──

(define (run-all)
  (run-tests ws-token-accounting-tests 'verbose))

(run-all)
