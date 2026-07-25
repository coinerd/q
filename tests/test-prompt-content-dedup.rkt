#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: unit

;; tests/test-prompt-content-dedup.rkt
;; W2 (#8939): Project identical adjacent active user instructions once.
;;
;; Duplicate user records are persisted for audit, but when building
;; provider context, identical adjacent active instructions must be
;; projected once (keeping the newest identity and a duplicate marker).

(require rackunit
         rackunit/text-ui
         "../runtime/task-memory/working-set-continuity.rkt")

;; Mock message for testing
(struct msg (role content id) #:transparent)

;; Convenience wrappers using struct-generated accessors
(define (dedupe msgs)
  (dedupe-adjacent-user-prompts/messages msgs msg-role msg-content msg-id))
(define (dedupe/info msgs)
  (dedupe-adjacent-user-prompts/info msgs msg-role msg-content msg-id))

(define prompt-dedup-tests
  (test-suite "Prompt content deduplication"

    ;; ── T01: normalize prompt text ──
    (test-case "normalize strips whitespace and lowercases for comparison"
      (check-equal? (normalize-prompt-text "  Hello World  ") (normalize-prompt-text "hello world"))
      (check-equal? (normalize-prompt-text "Commit\nand\nPush")
                    (normalize-prompt-text "commit and push")))

    ;; ── T02: identical adjacent prompts are deduplicated ──
    (test-case "two identical adjacent user prompts project as one"
      (define prompts
        (list (msg "user" "implement feature X" "m1") (msg "user" "implement feature X" "m2")))
      (define result (dedupe prompts))
      (check-equal? (length result) 1)
      ;; Newest identity is kept
      (check-equal? (msg-id (car result)) "m2"))

    ;; ── T03: different adjacent prompts are kept ──
    (test-case "two different adjacent user prompts are both kept"
      (define prompts
        (list (msg "user" "implement feature X" "m1") (msg "user" "now commit and push" "m2")))
      (define result (dedupe prompts))
      (check-equal? (length result) 2))

    ;; ── T04: dedup only applies to adjacent ──
    (test-case "non-adjacent identical prompts are both kept"
      (define prompts
        (list (msg "user" "do task A" "m1")
              (msg "assistant" "done" "m2")
              (msg "user" "do task A" "m3")))
      (define result (dedupe prompts))
      (check-equal? (length result) 3))

    ;; ── T05: three identical adjacent prompts project as one ──
    (test-case "three identical adjacent prompts project as one"
      (define prompts
        (list (msg "user" "proceed" "m1") (msg "user" "proceed" "m2") (msg "user" "proceed" "m3")))
      (define result (dedupe prompts))
      (check-equal? (length result) 1)
      (check-equal? (msg-id (car result)) "m3"))

    ;; ── T06: duplicate marker is recorded ──
    (test-case "dedup result carries duplicate count metadata"
      (define prompts (list (msg "user" "proceed" "m1") (msg "user" "proceed" "m2")))
      (define-values (result dup-info) (dedupe/info prompts))
      (check-equal? (length result) 1)
      ;; m2 is the kept message, with 1 duplicate (m1) removed before it
      (check-true (hash-has-key? dup-info "m2")))

    ;; ── T07: whitespace-only difference is still duplicate ──
    (test-case "prompts differing only in whitespace are duplicates"
      (define prompts (list (msg "user" "do  this" "m1") (msg "user" "do this" "m2")))
      (define result (dedupe prompts))
      (check-equal? (length result) 1))

    ;; ── T08: empty prompt list ──
    (test-case "empty prompt list returns empty"
      (check-equal? (dedupe '()) '()))

    ;; ── T09: single prompt passes through ──
    (test-case "single prompt passes through unchanged"
      (define prompts (list (msg "user" "hello" "m1")))
      (define result (dedupe prompts))
      (check-equal? (length result) 1)
      (check-equal? (msg-id (car result)) "m1"))

    ;; ── T10: assistant messages break adjacency ──
    (test-case "assistant message between user prompts breaks dedup adjacency"
      (define prompts
        (list (msg "user" "do task" "m1")
              (msg "assistant" "working on it" "m2")
              (msg "user" "do task" "m3")))
      (define result (dedupe prompts))
      (check-equal? (length result) 3))

    ;; ── T11: newest user instruction remains final ──
    (test-case "after dedup, the newest user instruction is still last user msg"
      (define prompts
        (list (msg "user" "task A" "m1") (msg "user" "task A" "m2") (msg "assistant" "ok" "m3")))
      (define result (dedupe prompts))
      (define last-user (findf (lambda (m) (equal? (msg-role m) "user")) (reverse result)))
      (check-equal? (msg-id last-user) "m2"))))

(run-tests prompt-dedup-tests)
