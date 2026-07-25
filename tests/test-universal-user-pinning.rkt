#lang racket

;; @speed fast
;; @suite default
;; BOUNDARY: integration

;; tests/test-universal-user-pinning.rkt
;; Regression tests for universal user message pinning fix.
;; User messages are tiny, high-signal, and must not be dropped.

(require rackunit
         racket/list
         racket/string
         "../util/message/message.rkt"
         "../util/content/content-parts.rkt"
         "../runtime/context/context-assembly.rkt"
         "../runtime/context/context-pinning.rkt")

(displayln "test-universal-user-pinning.rkt loaded")

;; Helper: create a text message quickly
(define (make-text-msg id role kind text [parent #f])
  (make-message id parent role kind (list (make-text-part text)) (current-seconds) (hasheq)))

(define (id-in? id ids)
  (not (not (member id ids))))

(test-case "all user messages are protected by partition-messages"
  (define u1 (make-text-msg "u1" 'user 'message "first user prompt"))
  (define u2 (make-text-msg "u2" 'user 'message "follow-up prompt"))
  (define a1 (make-text-msg "a1" 'assistant 'message "assistant reply"))
  (define-values (protected removable) (partition-messages (list u1 a1 u2)))
  (check-equal? (length protected) 2)
  (check-not-false (member u1 protected))
  (check-not-false (member u2 protected))
  (check-false (member a1 protected)))

(test-case "build-tiered-context pins every user message to Tier A"
  ;; Create many user messages so only recent ones would survive recency filter
  (define base-msgs
    (for/list ([i (in-range 100)])
      (make-text-msg (format "m~a" i) 'user 'message (format "Msg ~a" i))))
  (define mid-user (make-text-msg "mid" 'user 'message "middle user prompt"))
  (define msgs (append (take base-msgs 50) (list mid-user) (drop base-msgs 50)))
  (define tiered (build-tiered-context msgs #:tier-c-count 4))
  (define tier-a (tiered-context-tier-a tiered))
  (define ids-in-a (map message-id tier-a))
  (check-true (id-in? "mid" ids-in-a) "middle user message must be pinned to Tier A")
  ;; Verify all 101 user messages survive in at least one tier
  (define all-tier-ids
    (append ids-in-a
            (map message-id (tiered-context-tier-b tiered))
            (map message-id (tiered-context-tier-c tiered))))
  (for ([m (in-list msgs)])
    (check-true (id-in? (message-id m) all-tier-ids)
                (format "user message ~a must appear somewhere in tiered context" (message-id m)))))

(test-case "pinning preserves chronological provider order"
  (define original (make-text-msg "u1" 'user 'message "original task"))
  (define assistant (make-text-msg "a1" 'assistant 'message "working" "u1"))
  (define tool-result (make-text-msg "t1" 'tool 'tool-result "result" "a1"))
  (define retry (make-text-msg "u2" 'user 'message "Retry" "t1"))
  (define msgs (list original assistant tool-result retry))
  (define tiered (build-tiered-context msgs #:tier-b-count 1 #:tier-c-count 1))
  (check-equal? (map message-id (tiered-context->message-list tiered))
                '("u1" "a1" "t1" "u2")
                "retained user prompts must not be moved ahead of later history"))

(test-case "working-set pinning deduplicates by canonical message id"
  (define original (make-text-msg "u1" 'user 'message "task"))
  (define tool-result (make-text-msg "t1" 'tool 'tool-result "result" "u1"))
  (define copied-result (make-text-msg "t1" 'tool 'tool-result "copied result" "u1"))
  (define retry (make-text-msg "u2" 'user 'message "Retry" "t1"))
  (define tiered
    (build-tiered-context (list original tool-result retry)
                          #:working-set-messages (list copied-result)))
  (check-equal? (map message-id (tiered-context->message-list tiered)) '("u1" "t1" "u2")))

(test-case "payload round trip preserves provider chronology"
  (define msgs
    (list (make-text-msg "u1" 'user 'message "task")
          (make-text-msg "a1" 'assistant 'message "working" "u1")
          (make-text-msg "u2" 'user 'message "Retry" "a1")))
  (define original (build-tiered-context msgs #:tier-b-count 1 #:tier-c-count 1))
  (define round-tripped (payload->tiered-context (tiered-context->payload original 1000)))
  (check-equal? (map message-id (tiered-context->message-list round-tripped)) '("u1" "a1" "u2")))

(test-case "tiered-context retains three-field match compatibility"
  (check-equal? (match (tiered-context '(a) '(b) '(c))
                  [(tiered-context a b c) (append a b c)])
                '(a b c)))

(test-case "new working-set injection stays before the latest user prompt"
  (define system (make-text-msg "s1" 'system 'system-instruction "system"))
  (define original (make-text-msg "u1" 'user 'message "task" "s1"))
  (define injected (make-text-msg "w1" 'system 'system-instruction "working context"))
  (define tiered (build-tiered-context (list system original) #:working-set-messages (list injected)))
  (check-equal? (map message-id (tiered-context->message-list tiered)) '("s1" "w1" "u1")))

(test-case "truncate-messages-to-budget preserves all user messages"
  (define users
    (for/list ([i (in-range 10)])
      (make-text-msg (format "u~a" i) 'user 'message (format "User ~a" i))))
  (define assistant-msgs
    (for/list ([i (in-range 50)])
      (make-text-msg (format "a~a" i) 'assistant 'message (make-string 500 #\x))))
  (define all-msgs (append users assistant-msgs))
  (define trimmed (truncate-messages-to-budget all-msgs 2000))
  (define trimmed-ids (map message-id trimmed))
  (for ([u (in-list users)])
    (check-true (id-in? (message-id u) trimmed-ids)
                (format "user message ~a must survive truncation" (message-id u)))))

(test-case "user messages outrank assistant messages under tight budget"
  (define u1 (make-text-msg "u1" 'user 'message "task definition"))
  (define u2 (make-text-msg "u2" 'user 'message "clarification"))
  (define a1 (make-text-msg "a1" 'assistant 'message (make-string 2000 #\y)))
  (define a2 (make-text-msg "a2" 'assistant 'message (make-string 2000 #\z)))
  (define trimmed (truncate-messages-to-budget (list u1 a1 u2 a2) 100))
  (define ids (map message-id trimmed))
  (check-true (and (id-in? "u1" ids) (id-in? "u2" ids)) "both user messages must survive")
  ;; Assistant messages are not protected and may be dropped
  (check-true (<= (length trimmed) 3) "assistant messages should be dropped before user messages"))

(test-case "multi-prompt session: all user prompts survive tool-failure truncation"
  ;; Simulate the regression scenario: 3 user prompts interleaved with
  ;; large assistant/tool outputs. Under tight budget, every prompt must
  ;; remain visible so the model does not regress to the first task.
  (define prompt1 (make-text-msg "p1" 'user 'message "Re-build and restart server"))
  (define assistant1 (make-text-msg "a1" 'assistant 'message (make-string 5000 #\b)))
  (define tool1 (make-text-msg "t1" 'tool 'message (make-string 3000 #\c)))
  (define prompt2 (make-text-msg "p2" 'user 'message "Create a list of pages"))
  (define assistant2 (make-text-msg "a2" 'assistant 'message (make-string 5000 #\d)))
  (define tool2 (make-text-msg "t2" 'tool 'message (make-string 3000 #\e)))
  (define prompt3 (make-text-msg "p3" 'user 'message "Copy contents from live URLs"))
  (define assistant3 (make-text-msg "a3" 'assistant 'message (make-string 5000 #\f)))
  (define tool-fail (make-text-msg "tf" 'tool 'message (make-string 2000 #\g)))
  (define all-msgs
    (list prompt1 assistant1 tool1 prompt2 assistant2 tool2 prompt3 assistant3 tool-fail))
  (define trimmed (truncate-messages-to-budget all-msgs 1000))
  (define ids (map message-id trimmed))
  (for ([pid (in-list '("p1" "p2" "p3"))])
    (check-true (id-in? pid ids) (format "prompt ~a must survive tight-budget truncation" pid))))
