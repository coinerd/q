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
