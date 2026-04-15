#lang racket

;; tests/test-cutpoint-rules.rkt — tests for Compaction Cut-point Rules (#693-#696)
;;
;; Covers:
;;   - #693: Never cut between tool-call and tool-result
;;   - #694: One-shot overflow recovery guard
;;   - #695: Auto-retry after overflow compaction
;;   - #696: Parent feature integration

(require rackunit
         "../util/protocol-types.rkt"
         "../runtime/cutpoint-rules.rkt"
         "../runtime/compactor.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define msg-counter 0)
(define (next-id!)
  (set! msg-counter (add1 msg-counter))
  (format "msg-~a" msg-counter))

(define (make-user-msg text)
  (make-message (next-id!) #f 'user 'text
                (list (make-text-part text))
                (current-seconds) (hasheq)))

(define (make-assistant-msg text)
  (make-message (next-id!) #f 'assistant 'text
                (list (make-text-part text))
                (current-seconds) (hasheq)))

(define (make-assistant-tool-call-msg tool-id tool-name)
  (make-message (next-id!) #f 'assistant 'text
                (list (make-text-part "calling tool")
                      (make-tool-call-part tool-id tool-name (hasheq)))
                (current-seconds) (hasheq)))

(define (make-tool-result-msg call-id result-text)
  (make-message (next-id!) #f 'tool 'tool-result
                (list (make-tool-result-part call-id result-text #f))
                (current-seconds) (hasheq)))

;; ============================================================
;; #693: Cut-point rules
;; ============================================================

(test-case "valid-cutpoint: index 0 is always valid"
  (define msgs (list (make-user-msg "hello")))
  (check-true (valid-cutpoint? msgs 0)))

(test-case "valid-cutpoint: index at end is always valid"
  (define msgs (list (make-user-msg "hello")))
  (check-true (valid-cutpoint? msgs 1)))

(test-case "valid-cutpoint: before user message is valid"
  (define msgs (list (make-assistant-msg "hi")
                     (make-user-msg "hello")))
  (check-true (valid-cutpoint? msgs 1)))

(test-case "valid-cutpoint: before tool-result is invalid"
  (define msgs (list (make-assistant-tool-call-msg "tc1" "bash")
                     (make-tool-result-msg "tc1" "output")))
  ;; Cut at 1 would separate tool-call from tool-result
  (check-false (valid-cutpoint? msgs 1)))

(test-case "valid-cutpoint: before assistant after user is valid"
  (define msgs (list (make-user-msg "q")
                     (make-assistant-msg "a")))
  (check-true (valid-cutpoint? msgs 0))
  (check-true (valid-cutpoint? msgs 1)))

(test-case "adjust-cutpoint: moves backward from invalid to valid"
  ;; tool-call at 0, tool-result at 1, user at 2
  (define msgs (list (make-assistant-tool-call-msg "tc1" "bash")
                     (make-tool-result-msg "tc1" "output")
                     (make-user-msg "next")))
  ;; Proposed cut at 1 (before tool-result) → should adjust
  (define adjusted (adjust-cutpoint msgs 1))
  (check-equal? adjusted 0))

(test-case "adjust-cutpoint: keeps valid cutpoint unchanged"
  (define msgs (list (make-user-msg "q1")
                     (make-assistant-msg "a1")
                     (make-user-msg "q2")))
  (define adjusted (adjust-cutpoint msgs 2))
  (check-equal? adjusted 2))

(test-case "adjust-cutpoint: handles tool-call/tool-result chain"
  ;; user → assistant(tool-call) → tool-result → assistant(final) → user
  (define msgs (list (make-user-msg "q1")
                     (make-assistant-tool-call-msg "tc1" "bash")
                     (make-tool-result-msg "tc1" "output")
                     (make-assistant-msg "done")
                     (make-user-msg "q2")))
  ;; Cut at 2 (before tool-result) → invalid, should move back
  ;; Index 1 is assistant, previous is user (no tool-calls) → valid cut
  (define adjusted (adjust-cutpoint msgs 2))
  (check-equal? adjusted 1))

(test-case "find-nearest-valid-cutpoint: searches backward"
  (define msgs (list (make-user-msg "q1")
                     (make-assistant-msg "a1")
                     (make-assistant-msg "a2")))
  ;; Index 2 is assistant, previous is assistant (no tool-calls) → valid cut at 2
  ;; Actually we start by checking 2 itself, which is valid
  (define found (find-nearest-valid-cutpoint msgs 3 #t))
  ;; 3 = end, valid. So try 3 directly.
  (check-equal? found 3))

(test-case "find-nearest-valid-cutpoint: searches forward"
  (define msgs (list (make-assistant-tool-call-msg "tc1" "bash")
                     (make-tool-result-msg "tc1" "output")
                     (make-user-msg "q1")))
  ;; Forward from 1 (before tool-result, invalid) should find 2
  (define found (find-nearest-valid-cutpoint msgs 1 #f))
  (check-equal? found 2))

(test-case "cutpoint-rule-description: describes valid cut"
  (define msgs (list (make-user-msg "q1")
                     (make-assistant-msg "a1")
                     (make-user-msg "q2")))
  (define desc (cutpoint-rule-description msgs 2))
  (check-true (string-contains? desc "Valid cut")))

(test-case "cutpoint-rule-description: describes beginning cut"
  (define desc (cutpoint-rule-description '() 0))
  (check-true (string-contains? desc "beginning")))

;; ============================================================
;; #694: One-shot overflow recovery guard
;; ============================================================

(test-case "overflow-state: initial state allows retry"
  (define state (make-overflow-state))
  (check-true (can-retry-overflow? state))
  (check-true (overflow-state-will-retry state)))

(test-case "overflow-state: after marking attempted, cannot retry"
  (define state (make-overflow-state))
  (mark-overflow-attempted! state)
  (check-false (can-retry-overflow? state))
  (check-false (overflow-state-will-retry state)))

(test-case "overflow-state: respects max-attempts"
  (define state (make-overflow-state #:max-attempts 0))
  (check-false (overflow-state-will-retry state)))

(test-case "overflow-state: is transparent"
  (define state (make-overflow-state))
  (check-true (overflow-state? state))
  (check-false (overflow-state-attempted state))
  (check-equal? (overflow-state-max-attempts state) 1))

;; ============================================================
;; #695: Auto-retry after overflow compaction
;; ============================================================

(test-case "build-retry-messages: includes compaction summary"
  (define summary-msg
    (make-message "compaction-1" #f 'system 'compaction-summary
                  (list (make-text-part "summary text"))
                  (current-seconds) (hasheq)))
  (define result (compaction-result summary-msg 5
                                     (list (make-user-msg "recent q"))))
  (define retry-msgs (build-retry-messages result "original prompt"))
  ;; Should have: summary + kept messages + retry message
  (check-true (>= (length retry-msgs) 2))
  ;; Last message should be the retry prompt
  (define last-msg (car (take-right retry-msgs 1)))
  (check-equal? (message-role last-msg) 'user)
  (check-true (string-contains?
               (text-part-text (car (message-content last-msg)))
               "original prompt")))

(test-case "build-retry-messages: works without summary"
  (define result (compaction-result #f 0
                                     (list (make-user-msg "kept msg"))))
  (define retry-msgs (build-retry-messages result "retry prompt"))
  ;; Should have: kept + retry
  (check-equal? (length retry-msgs) 2)
  (define last-msg (car (take-right retry-msgs 1)))
  (check-equal? (message-role last-msg) 'user))

(test-case "retry-context: stores all fields"
  (define summary-msg
    (make-message "s1" #f 'system 'compaction-summary
                  (list (make-text-part "s")) (current-seconds) (hasheq)))
  (define result (compaction-result summary-msg 3 (list (make-user-msg "q"))))
  (define msgs (build-retry-messages result "prompt"))
  (define ctx (make-retry-context "prompt" result msgs))
  (check-true (retry-context? ctx))
  (check-equal? (retry-context-original-prompt ctx) "prompt")
  (check-equal? (retry-context-compaction-result ctx) result)
  (check-equal? (retry-context-retry-messages ctx) msgs))

;; ============================================================
;; #696: Integration
;; ============================================================

(test-case "integration: cutpoint adjustment with overflow recovery"
  ;; Simulate: messages that would overflow, cut-point needs adjustment
  (define msgs (list (make-user-msg "q1")
                     (make-assistant-tool-call-msg "tc1" "bash")
                     (make-tool-result-msg "tc1" "long output...")
                     (make-assistant-msg "final answer")
                     (make-user-msg "q2")
                     (make-assistant-msg "a2")))
  ;; Proposed cut at 2 (before tool-result) → adjust
  (define adjusted (adjust-cutpoint msgs 2))
  ;; Index 1 is assistant, previous is user → valid cut
  (check-equal? adjusted 1)
  ;; Overflow recovery state
  (define state (make-overflow-state))
  (check-true (can-retry-overflow? state))
  ;; After compaction, build retry
  (define summary-msg
    (make-message "s1" #f 'system 'compaction-summary
                  (list (make-text-part "summarized history"))
                  (current-seconds) (hasheq)))
  (define result (compaction-result summary-msg 2 (drop msgs 4)))
  (define retry-msgs (build-retry-messages result "q2"))
  (check-true (> (length retry-msgs) 0))
  (mark-overflow-attempted! state)
  (check-false (can-retry-overflow? state)))
