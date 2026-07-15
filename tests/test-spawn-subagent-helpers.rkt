#lang racket

;; @speed fast
;; @suite fast

;; W2 v0.99.35: Tests for spawn-subagent-helpers.rkt
;; Verifies pure functions extracted from spawn-subagent.rkt:
;; - normalize-capabilities (deduplicated capability parsing)
;; - requires-hitl-approval? (pure capability check)
;; - extract-assistant-text (pure message-to-text extraction)
;; - extract-text-summary (pure text truncation)

(require rackunit
         rackunit/text-ui
         racket/string
         racket/list
         racket/runtime-path)

(require (only-in "../tools/builtins/spawn-subagent-helpers.rkt"
                  normalize-capabilities
                  normalize-capabilities/strict
                  canonical-datum-string
                  immutable-canonical-copy
                  sha256-digest
                  requires-hitl-approval?
                  bounded-delegated-capabilities
                  valid-plan-id?
                  extract-assistant-text
                  extract-text-summary
                  SUBAGENT-SUMMARY-MAX-CHARS))

(require (only-in "../util/message/message.rkt" make-message message-role message-content)
         (only-in "../util/content/content-parts.rkt"
                  make-tool-call-part
                  make-tool-result-part
                  text-part?))

(define-test-suite
 normalize-capabilities-tests
 (test-case "normalize-capabilities: #f input → #f"
   (check-false (normalize-capabilities #f)))
 (test-case "normalize-capabilities: empty list → #f"
   (check-false (normalize-capabilities '())))
 (test-case "normalize-capabilities: string → list of one symbol"
   (check-equal? (normalize-capabilities "read-only") '(read-only)))
 (test-case "normalize-capabilities: list of strings → list of symbols"
   (check-equal? (normalize-capabilities '("read-only" "file-write")) '(read-only file-write)))
 (test-case "normalize-capabilities: list of symbols → filtered list"
   (check-equal? (normalize-capabilities '(read-only file-write)) '(read-only file-write)))
 (test-case "normalize-capabilities: filters invalid capabilities"
   (check-equal? (normalize-capabilities '("read-only" "bogus")) '(read-only)))
 (test-case "normalize-capabilities: all-invalid → #f"
   (check-false (normalize-capabilities '("bogus" "also-bogus"))))
 (test-case "normalize-capabilities: mixed strings and symbols"
   (check-equal? (normalize-capabilities '("read-only" shell-exec)) '(read-only shell-exec)))
 (test-case "strict normalization rejects any explicitly invalid capability"
   (check-exn exn:fail:contract? (lambda () (normalize-capabilities/strict '("read-only" "bogus")))))
 (test-case "strict normalization accepts explicit valid values and removes duplicates"
   (check-equal? (normalize-capabilities/strict '("shell-exec" shell-exec read-only))
                 '(shell-exec read-only)))
 (test-case "strict normalization preserves explicit empty authority"
   (check-equal? (normalize-capabilities/strict '()) '())
   (check-equal? (normalize-capabilities/strict #f) '()))
 (test-case "strict delegated capabilities reject unrestricted any wildcard"
   (check-exn exn:fail:contract? (lambda () (normalize-capabilities/strict '(any))))
   (check-exn exn:fail:contract? (lambda () (normalize-capabilities/strict "any"))))
 (test-case "delegated capabilities are bounded by parent authority"
   (check-equal? (bounded-delegated-capabilities #f '(read-only)) '(read-only))
   (check-equal? (bounded-delegated-capabilities '(file-write) '(any)) '(file-write))
   (check-exn exn:fail?
              (lambda () (bounded-delegated-capabilities '(read-only shell-exec) '(read-only)))))
 (test-case "planned IDs are bounded and terminal-safe"
   (check-true (valid-plan-id? "batch:job-1.ok"))
   (check-false (valid-plan-id? ""))
   (check-false (valid-plan-id? "bad\nidentifier"))
   (check-false (valid-plan-id? (make-string 129 #\a)))))

(define-test-suite
 immutable-snapshot-tests
 (test-case "canonical form is independent of hash insertion order"
   (define left (hash 'task "abc" 'capabilities '(shell-exec read-only)))
   (define right (hash 'capabilities '(shell-exec read-only) 'task "abc"))
   (check-equal? (canonical-datum-string left) (canonical-datum-string right))
   (check-equal? (sha256-digest left) (sha256-digest right)))
 (test-case "canonical copy does not track later mutable request changes"
   (define raw (make-hasheq (list (cons 'task "before"))))
   (define snapshot (immutable-canonical-copy raw))
   (hash-set! raw 'task "after")
   (check-equal? (hash-ref snapshot 'task) "before"))
 (test-case "SHA-256 digest is exact"
   (check-equal? (sha256-digest "abc")
                 "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")))

(define-test-suite requires-hitl-approval-tests
                   (test-case "#t for shell-exec"
                     (check-true (and (requires-hitl-approval? '(shell-exec)) #t)))
                   (test-case "#t for git-write"
                     (check-true (and (requires-hitl-approval? '(git-write)) #t)))
                   (test-case "#t for mixed with shell-exec"
                     (check-true (and (requires-hitl-approval? '(read-only shell-exec)) #t)))
                   (test-case "#f for read-only only"
                     (check-false (requires-hitl-approval? '(read-only))))
                   (test-case "#f for file-write only"
                     (check-false (requires-hitl-approval? '(file-write))))
                   (test-case "#f for #f"
                     (check-false (requires-hitl-approval? #f)))
                   (test-case "#f for empty list"
                     (check-false (requires-hitl-approval? '()))))

(define-test-suite
 extract-assistant-text-tests
 (test-case "empty message list → empty string"
   (check-equal? (extract-assistant-text '()) ""))
 (test-case "extracts text from assistant message with string content"
   (define msg (make-message 'id1 #f 'assistant 'message '("Hello world") 0 (hasheq)))
   (check-equal? (extract-assistant-text (list msg)) "Hello world"))
 (test-case "ignores non-assistant messages"
   (define sys (make-message 'id0 #f 'system 'message '("system prompt") 0 (hasheq)))
   (define asst (make-message 'id1 #f 'assistant 'message '("result text") 0 (hasheq)))
   (check-equal? (extract-assistant-text (list sys asst)) "result text"))
 (test-case "joins multiple assistant messages with newline"
   (define m1 (make-message 'id1 #f 'assistant 'message '("first") 0 (hasheq)))
   (define m2 (make-message 'id2 #f 'assistant 'message '("second") 0 (hasheq)))
   (check-equal? (extract-assistant-text (list m1 m2)) "first\nsecond"))
 (test-case "summarizes tool-call-only messages"
   (define m1
     (make-message 'id1
                   #f
                   'assistant
                   'message
                   (list (make-tool-call-part "tc1" "read" "{}"))
                   0
                   (hasheq)))
   (check-true (string-contains? (extract-assistant-text (list m1)) "read"))))

(define-test-suite
 extract-text-summary-tests
 (test-case "constant is 4000"
   (check-equal? SUBAGENT-SUMMARY-MAX-CHARS 4000))
 (test-case "short content passes through"
   (check-equal? (extract-text-summary (list (hasheq 'type "text" 'text "hello"))) "hello"))
 (test-case "long content is truncated with ellipsis"
   (define long-text (make-string 5000 #\X))
   (define result (extract-text-summary (list (hasheq 'type "text" 'text long-text))))
   (check-true (>= (string-length result) 3997) "truncated content is substantial")
   (check-true (string-suffix? result "...") "ends with ellipsis"))
 (test-case "empty content → empty string"
   (check-equal? (extract-text-summary '()) "")))

;; ============================================================
;; Run all tests
;; ============================================================

(define-test-suite all-spawn-subagent-helpers-tests
                   normalize-capabilities-tests
                   immutable-snapshot-tests
                   requires-hitl-approval-tests
                   extract-assistant-text-tests
                   extract-text-summary-tests)

(module+ test
  (run-tests all-spawn-subagent-helpers-tests))

(module+ main
  (run-tests all-spawn-subagent-helpers-tests))
