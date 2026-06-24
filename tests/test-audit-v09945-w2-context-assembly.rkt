#lang racket/base

;; @speed fast
;; @suite default
;; tests/test-audit-v09945-w2-context-assembly.rkt — Context assembly real-world audit
;;
;; Audit of the context assembly subsystem covering:
;;   1. Config creation with defaults and custom parameters
;;   2. Token estimation for various message content shapes
;;   3. Message selection (pure function) — budget fitting, pinning
;;   4. Tiered context building — system/user/GSD/assistant/tool partitioning
;;   5. Universal user message pinning — ALL user messages protected
;;   6. Working-set message pinning
;;   7. Dynamic tier sizing (tier-b and tier-c)
;;   8. Tier merging and windowed message selection
;;   9. Payload conversion round-trip
;;  10. GSD progress message detection
;;  11. Budget overflow handling
;;  12. Empty/edge-case message lists
;;  13. Large conversation tiering (windowed)
;;
;; FINDING: Tiered context is a windowed selection, not full preservation.
;; Only system/user/GSD/compaction messages are always kept (tier-a).
;; Regular assistant/tool messages are windowed: tier-b (recent) + tier-c (oldest).
;;
;; All tests use synthetic messages — no real API keys needed.

(require rackunit
         racket/list
         racket/string
         "../runtime/context-assembly/budgeting.rkt"
         "../runtime/context-assembly/selection.rkt"
         "../runtime/context/context-policy.rkt"
         "../runtime/context/context-pinning.rkt"
         "../runtime/context-assembly/context-floor.rkt"
         "../util/message/message.rkt"
         "../util/content/content-parts.rkt")

;; ---------------------------------------------------------------------------
;; Test helpers
;; ---------------------------------------------------------------------------

(define (make-text-msg id text #:role [role 'user] #:kind [kind 'user] #:meta [meta (hash)])
  (make-message id #f role kind (list (make-text-part text)) (current-seconds) meta))

(define (make-system-msg id text)
  (make-text-msg id text #:role 'system #:kind 'system-instruction))

(define (make-assistant-msg id text)
  (make-text-msg id text #:role 'assistant #:kind 'assistant))

(define (make-tool-msg id text)
  (make-text-msg id text #:role 'tool #:kind 'tool-result))

(define (make-gsd-progress-msg id text)
  (make-text-msg id text #:role 'assistant #:kind 'assistant #:meta (hash 'gsd-pin #t)))

(define (make-compaction-msg id text)
  (make-text-msg id text #:role 'user #:kind 'compaction-summary))

(define (const-estimate n)
  (lambda (msg) n))

;; ---------------------------------------------------------------------------
;; 1. Config Creation
;; ---------------------------------------------------------------------------

(test-case "audit-config-defaults"
  (define cfg (make-context-assembly-config))
  (check-equal? (context-assembly-config-recent-tokens cfg) 30000)
  (check-equal? (context-assembly-config-max-catalog-entries cfg) 40)
  (check-equal? (context-assembly-config-max-catalog-tokens cfg) 2000)
  (check-equal? (context-assembly-config-summary-window cfg) 4000))

(test-case "audit-config-custom"
  (define cfg
    (make-context-assembly-config #:recent-tokens 8000
                                  #:max-catalog-entries 10
                                  #:max-catalog-tokens 500
                                  #:summary-window 1000))
  (check-equal? (context-assembly-config-recent-tokens cfg) 8000)
  (check-equal? (context-assembly-config-max-catalog-entries cfg) 10)
  (check-equal? (context-assembly-config-max-catalog-tokens cfg) 500)
  (check-equal? (context-assembly-config-summary-window cfg) 1000))

;; ---------------------------------------------------------------------------
;; 2. Token Estimation
;; ---------------------------------------------------------------------------

(test-case "audit-token-estimate-short-text"
  (define msg (make-text-msg "t1" "Hello world"))
  (define est (estimate-message-tokens msg))
  (check-true (exact-nonnegative-integer? est))
  (check-true (> est 0) "Short text should estimate > 0 tokens"))

(test-case "audit-token-estimate-long-text"
  (define short (make-text-msg "t1" "Hello"))
  (define long (make-text-msg "t2" (make-string 5000 #\x)))
  (check-true (> (estimate-message-tokens long) (estimate-message-tokens short))
              "Long text should estimate more tokens than short"))

(test-case "audit-token-estimate-proportional"
  (define m1 (make-text-msg "t1" (make-string 100 #\x)))
  (define m2 (make-text-msg "t2" (make-string 200 #\x)))
  (define e1 (estimate-message-tokens m1))
  (define e2 (estimate-message-tokens m2))
  (check-true (< e1 e2) "200 chars should be more tokens than 100"))

;; ---------------------------------------------------------------------------
;; 3. Message Selection (Pure)
;; ---------------------------------------------------------------------------

(test-case "audit-select-fits-within-budget"
  (define msgs
    (for/list ([i (in-range 10)])
      (make-text-msg (format "m~a" i) (format "message ~a" i))))
  (define-values (selected excluded) (select-messages '() msgs 50 (const-estimate 10)))
  (check-true (<= (length selected) 5) "Budget 50 / 10 per msg = max 5")
  (check-true (> (length excluded) 0) "Some messages should be excluded"))

(test-case "audit-select-all-fit-when-budget-large"
  (define msgs (list (make-text-msg "a" "one") (make-text-msg "b" "two")))
  (define-values (selected excluded) (select-messages '() msgs 1000 (const-estimate 10)))
  (check-equal? (length selected) 2 "All should fit")
  (check-equal? (length excluded) 0))

(test-case "audit-select-nothing-fits-when-budget-too-small"
  (define msgs
    (for/list ([i (in-range 5)])
      (make-text-msg (format "b~a" i) (format "big ~a" i))))
  (define-values (selected excluded) (select-messages '() msgs 5 (const-estimate 10)))
  (check-equal? (length selected) 0 "Nothing should fit")
  (check-equal? (length excluded) 5))

(test-case "audit-select-pinned-always-included"
  (define pinned (list (make-text-msg "p1" "pinned" #:kind 'system-instruction)))
  (define removable
    (for/list ([i (in-range 5)])
      (make-text-msg (format "r~a" i) (format "rem ~a" i))))
  (define-values (selected excluded) (select-messages pinned removable 30 (const-estimate 10)))
  (check-not-false (member (car pinned) selected) "Pinned must be in selected"))

;; ---------------------------------------------------------------------------
;; 4. Tiered Context Building
;; ---------------------------------------------------------------------------

(test-case "audit-tiered-basic-partitioning"
  (define msgs
    (list (make-system-msg "sys" "You are a helpful assistant")
          (make-text-msg "u1" "First question")
          (make-assistant-msg "a1" "First answer")
          (make-text-msg "u2" "Second question")
          (make-assistant-msg "a2" "Second answer")))
  (define tc (build-tiered-context msgs))
  (check-true (tiered-context? tc))
  (check-true (>= (length (tiered-context-tier-a tc)) 1) "Tier-a should have system/user messages")
  (check-true (>= (length (tiered-context-tier-b tc)) 0))
  (check-true (>= (length (tiered-context-tier-c tc)) 0)))

(test-case "audit-tiered-all-messages-preserved-small"
  ;; Small conversation: all messages fit within tier-b/c window
  (define msgs
    (list (make-system-msg "sys" "System prompt")
          (make-text-msg "u1" "Question 1")
          (make-assistant-msg "a1" "Answer 1")
          (make-text-msg "u2" "Question 2")
          (make-assistant-msg "a2" "Answer 2")))
  (define tc (build-tiered-context msgs))
  (define all-msgs (tiered-context->message-list tc))
  (check-equal? (length all-msgs) (length msgs) "All messages should be preserved for small conv"))

(test-case "audit-tiered-user-messages-in-tier-a"
  (define msgs
    (append (list (make-system-msg "sys" "System"))
            (for/list ([i (in-range 6)])
              (make-assistant-msg (format "a~a" i) (format "Answer ~a" i)))
            (list (make-text-msg "u1" "Question 1") (make-text-msg "u2" "Question 2"))))
  (define tc (build-tiered-context msgs))
  (define tier-a (tiered-context-tier-a tc))
  (for ([m (in-list msgs)]
        #:when (eq? (message-role m) 'user))
    (check-not-false (member m tier-a) "User message must be in tier-a")))

(test-case "audit-tiered-gsd-progress-pinned"
  (define gsd (make-gsd-progress-msg "g1" "Wave 0 marked complete"))
  (define msgs (list (make-system-msg "sys" "System") gsd (make-assistant-msg "a1" "Answer")))
  (define tc (build-tiered-context msgs))
  (check-not-false (member gsd (tiered-context-tier-a tc))
                   "GSD progress message should be in tier-a"))

(test-case "audit-tiered-compaction-summary-pinned"
  (define comp (make-compaction-msg "comp" "Summary of earlier conversation"))
  (define msgs (list (make-system-msg "sys" "System") comp (make-assistant-msg "a1" "Answer")))
  (define tc (build-tiered-context msgs))
  (check-not-false (member comp (tiered-context-tier-a tc)) "Compaction summary should be in tier-a"))

;; ---------------------------------------------------------------------------
;; 5. Universal User Message Pinning
;; ---------------------------------------------------------------------------

(test-case "audit-universal-user-pinning"
  (define user-msgs
    (for/list ([i (in-range 5)])
      (make-text-msg (format "u~a" i) (format "User message ~a" i))))
  (define assistant-msgs
    (for/list ([i (in-range 10)])
      (make-assistant-msg (format "a~a" i) (format "Assistant message ~a" i))))
  (define msgs (append (list (make-system-msg "sys" "System")) user-msgs assistant-msgs))
  (define tc (build-tiered-context msgs))
  (define tier-a (tiered-context-tier-a tc))
  (for ([um (in-list user-msgs)])
    (check-not-false (member um tier-a) "Each user message must be pinned to tier-a")))

(test-case "audit-user-pinning-via-partition"
  (define msgs
    (list (make-system-msg "sys" "System")
          (make-text-msg "u1" "First")
          (make-assistant-msg "a1" "Reply")
          (make-text-msg "u2" "Second")))
  (define-values (pinned removable) (partition-messages/working-set msgs '()))
  (check-true (>= (length pinned) 3) "System + 2 user messages should be pinned"))

;; ---------------------------------------------------------------------------
;; 6. Working-Set Pinning
;; ---------------------------------------------------------------------------

(test-case "audit-working-set-pinning"
  (define ws-target (make-assistant-msg "ws-1" "This is in working set"))
  (define msgs
    (list (make-system-msg "sys" "System")
          (make-assistant-msg "a1" "Not in working set")
          ws-target
          (make-assistant-msg "a2" "Also not in working set")))
  (define-values (pinned removable) (partition-messages/working-set msgs (list "ws-1")))
  (check-not-false (member ws-target pinned) "Working-set message should be pinned"))

;; ---------------------------------------------------------------------------
;; 7. Dynamic Tier Sizing
;; ---------------------------------------------------------------------------

(test-case "audit-dynamic-tier-b-sizing"
  ;; min(50, max(20, N/10))
  ;; 10 messages → min(50, max(20, 1)) = 20
  (check-equal? (compute-dynamic-tier-b-count 10) 20)
  ;; 200 messages → min(50, max(20, 20)) = 20
  (check-equal? (compute-dynamic-tier-b-count 200) 20)
  ;; 300 messages → min(50, max(20, 30)) = 30
  (check-equal? (compute-dynamic-tier-b-count 300) 30)
  ;; 500 messages → min(50, max(20, 50)) = 50
  (check-equal? (compute-dynamic-tier-b-count 500) 50)
  ;; 1000 messages → min(50, max(20, 100)) = 50 (capped)
  (check-equal? (compute-dynamic-tier-b-count 1000) 50))

(test-case "audit-dynamic-tier-c-sizing"
  ;; min(12, max(4, N/50))
  ;; 0 messages → min(12, max(4, 0)) = 4
  (check-equal? (compute-tier-c-count 0) 4)
  ;; 100 messages → min(12, max(4, 2)) = 4
  (check-equal? (compute-tier-c-count 100) 4)
  ;; 600 messages → min(12, max(4, 12)) = 12
  (check-equal? (compute-tier-c-count 600) 12)
  ;; 1000 messages → min(12, max(4, 20)) = 12 (capped)
  (check-equal? (compute-tier-c-count 1000) 12))

;; ---------------------------------------------------------------------------
;; 8. Tier Merging — Windowed Selection
;; ---------------------------------------------------------------------------

(test-case "audit-tier-merge-windowed-drop"
  ;; FINDING: Tiered context is a windowed selection.
  ;; 30 unpinned assistant msgs: tier-c=4 (oldest), tier-b=20 (recent),
  ;; middle 6 dropped. tier-a has system only.
  (define msgs
    (append (list (make-system-msg "sys" "System"))
            (for/list ([i (in-range 30)])
              (make-assistant-msg (format "a~a" i) (format "Message ~a" i)))))
  (define tc (build-tiered-context msgs))
  (define merged (tiered-context->message-list tc))
  ;; tier-a=1 (system), tier-c=4 (oldest), tier-b=20 (recent) = 25 total
  (check-equal? (length merged) 25)
  ;; System always preserved
  (check-not-false (member (make-system-msg "sys" "System") (tiered-context-tier-a tc))))

(test-case "audit-tier-merge-pinned-preserved"
  ;; User messages are pinned and never dropped
  (define user-msgs
    (for/list ([i (in-range 10)])
      (make-text-msg (format "u~a" i) (format "User ~a" i))))
  (define asst-msgs
    (for/list ([i (in-range 100)])
      (make-assistant-msg (format "a~a" i) (format "Asst ~a" i))))
  (define msgs (append (list (make-system-msg "sys" "System")) user-msgs asst-msgs))
  (define tc (build-tiered-context msgs))
  (define tier-a (tiered-context-tier-a tc))
  (define tier-b (tiered-context-tier-b tc))
  (define tier-c (tiered-context-tier-c tc))
  ;; All user messages should be in tier-a
  (for ([um (in-list user-msgs)])
    (check-not-false (or (member um tier-a) (member um tier-b) (member um tier-c))
                     "User messages should not be dropped"))
  ;; System should be in tier-a
  (check-true (>= (length tier-a) 11) "System + 10 user messages should be in tier-a"))

;; ---------------------------------------------------------------------------
;; 9. Payload Conversion
;; ---------------------------------------------------------------------------

(test-case "audit-payload-round-trip"
  (define msgs (list (make-system-msg "sys" "System") (make-text-msg "u1" "Question")))
  (define tc (build-tiered-context msgs))
  (define payload (tiered-context->payload tc 8192 (hash 'test #t)))
  (check-true (context-assembly-payload? payload))
  (check-equal? (context-assembly-payload-max-tokens payload) 8192)
  (define tc2 (payload->tiered-context payload))
  (check-equal? (length (tiered-context->message-list tc))
                (length (tiered-context->message-list tc2))
                "Round-trip should preserve messages"))

;; ---------------------------------------------------------------------------
;; 10. GSD Progress Detection
;; ---------------------------------------------------------------------------

(test-case "audit-gsd-progress-detection-pinned-meta"
  (define m (make-gsd-progress-msg "g1" "Some text"))
  (check-true (gsd-progress-message? m) "Message with gsd-pin meta should be detected"))

(test-case "audit-gsd-progress-detection-wave-complete"
  ;; FINDING: regex is case-sensitive, requires lowercase "wave"
  (define m (make-text-msg "g2" "wave 0 marked complete" #:role 'assistant #:kind 'assistant))
  (check-true (gsd-progress-message? m) "Lowercase wave complete text should be detected"))

(test-case "audit-gsd-progress-detection-plan-md"
  (define m (make-text-msg "g3" "PLAN.md was created" #:role 'assistant #:kind 'assistant))
  (check-true (gsd-progress-message? m) "PLAN.md creation should be detected"))

(test-case "audit-gsd-progress-detection-state-md"
  (define m (make-text-msg "g4" "STATE.md was updated" #:role 'assistant #:kind 'assistant))
  (check-true (gsd-progress-message? m) "STATE.md update should be detected"))

(test-case "audit-gsd-progress-detection-negative"
  (define m (make-assistant-msg "n1" "Just a regular answer"))
  (check-false (gsd-progress-message? m) "Regular message should NOT be detected as GSD progress"))

;; ---------------------------------------------------------------------------
;; 11. Budget Overflow Handling
;; ---------------------------------------------------------------------------

(test-case "audit-budget-overflow-pinned-exceeds"
  (define pinned (list (make-text-msg "p1" "Pinned" #:kind 'system-instruction)))
  (define removable
    (for/list ([i (in-range 3)])
      (make-text-msg (format "r~a" i) (format "Rem ~a" i))))
  (define-values (selected excluded) (select-messages pinned removable 5 (const-estimate 10)))
  (check-equal? (length excluded) 3 "All removable should be excluded"))

;; ---------------------------------------------------------------------------
;; 12. Empty/Edge-Case Message Lists
;; ---------------------------------------------------------------------------

(test-case "audit-empty-message-list"
  (define tc (build-tiered-context '()))
  (check-true (tiered-context? tc))
  (check-equal? (length (tiered-context->message-list tc)) 0))

(test-case "audit-single-message"
  (define msgs (list (make-system-msg "sys" "Only system")))
  (define tc (build-tiered-context msgs))
  (check-equal? (length (tiered-context-tier-a tc)) 1)
  (check-equal? (length (tiered-context-tier-b tc)) 0)
  (check-equal? (length (tiered-context-tier-c tc)) 0))

(test-case "audit-select-empty-lists"
  (define-values (selected excluded) (select-messages '() '() 100 (const-estimate 10)))
  (check-equal? (length selected) 0)
  (check-equal? (length excluded) 0))

;; ---------------------------------------------------------------------------
;; 13. Large Conversation Tiering (Windowed)
;; ---------------------------------------------------------------------------

(test-case "audit-large-conversation-tiering"
  ;; FINDING: Tiered context is windowed — drops middle messages.
  ;; 302 total: 1 sys + 2 user + 300 assistant
  ;; tier-a: sys + 2 users = 3
  ;; tier-c: min(12, max(4, 300/50)) = min(12, 6) = 6
  ;; tier-b: min(30, 294) = 30
  ;; Total: 3 + 30 + 6 = 39
  (define msgs
    (append (list (make-system-msg "sys" "System"))
            (list (make-text-msg "u1" "First user question"))
            (for/list ([i (in-range 200)])
              (make-assistant-msg (format "a~a" i) (format "Answer ~a" i)))
            (list (make-text-msg "u2" "Second user question"))
            (for/list ([i (in-range 200 300)])
              (make-assistant-msg (format "a~a" i) (format "Answer ~a" i)))))
  (define tc (build-tiered-context msgs))
  (define merged (tiered-context->message-list tc))
  ;; Windowed: 3 (tier-a) + 30 (tier-b) + 6 (tier-c) = 39
  (check-true (< (length merged) (length msgs))
              "Large conversation should be windowed (fewer messages than input)")
  (check-true (>= (length merged) 30) "Should keep at least tier-a + tier-b worth of messages")
  ;; Tier-c should have messages (oldest assistants)
  (check-true (> (length (tiered-context-tier-c tc)) 0) "Large conversation should populate tier-c")
  ;; User messages should all be preserved (pinned)
  (define tier-a (tiered-context-tier-a tc))
  (check-true (>= (length tier-a) 3) "Tier-a should have system + 2 user messages"))
