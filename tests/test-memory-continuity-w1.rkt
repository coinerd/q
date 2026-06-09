#lang racket/base

;; test-memory-continuity-w1.rkt — Tests for memory continuity & looping prevention
;; Plan: .planning/PLAN-v0.96.13-MEMORY-CONTINUITY-LOOPING-PREVENTION.md

(require rackunit
         racket/string
         (only-in "../util/message/message.rkt"
                  make-message
                  message
                  message?
                  message-id
                  message-content
                  message-role)
         (only-in "../util/content/content-parts.rkt"
                  text-part
                  text-part?
                  text-part-text
                  make-text-part)
         (only-in "../runtime/context-assembly/memory-builder.rkt"
                  observe-memory-for-context
                  inject-memory-for-context)
         (only-in "../runtime/context-assembly/state-aware-builder.rkt" extract-recent-text))

;; ══════════════════════════════════════════════════════════════════
;; W1: Context-aware memory retrieval
;; ══════════════════════════════════════════════════════════════════

(test-case "W1.3: extract-recent-text returns text from assistant messages"
  (define msgs
    (list (make-message "m1" #f 'user 'user-msg (list (make-text-part "read file.rkt")) 100 (hasheq))
          (make-message "m2"
                        #f
                        'assistant
                        'assistant-msg
                        (list (make-text-part "Found the bug in auth-store"))
                        101
                        (hasheq))
          (make-message "m3" #f 'user 'user-msg (list (make-text-part "fix it")) 102 (hasheq))
          (make-message "m4"
                        #f
                        'assistant
                        'assistant-msg
                        (list (make-text-part "Fixed the auth bug by adding null check"))
                        103
                        (hasheq))))
  (define result (extract-recent-text msgs 2))
  (check-not-false result)
  (check-true (string-contains? result "auth") "should contain auth text"))

(test-case "W1.3: extract-recent-text returns #f for empty messages"
  (define result (extract-recent-text '() 3))
  (check-false result))

(test-case "W1.3: extract-recent-text truncates to 200 chars"
  (define long-text (make-string 300 #\x))
  (define msgs
    (list
     (make-message "m1" #f 'assistant 'assistant-msg (list (make-text-part long-text)) 100 (hasheq))))
  (define result (extract-recent-text msgs 1))
  (check-not-false result)
  (check-true (<= (string-length result) 200) "should be truncated to 200"))

(test-case "W1.1: observe-memory-for-context accepts #:query-text without error"
  ;; Memory disabled = returns empty, but should accept the kwarg
  (define result
    (observe-memory-for-context #f ; no session-config = disabled
                                #:query-text "debugging auth-store"))
  (check-true (pair? result))
  (check-true (pair? result)))

(test-case "W1.1: observe-memory-for-context backward compat (no #:query-text)"
  (define result (observe-memory-for-context #f))
  (check-true (pair? result))
  (check-true (pair? result)))

(test-case "W1.2: inject-memory-for-context accepts #:query-text"
  (define result (inject-memory-for-context #f #:query-text "test query"))
  (check-true (pair? result)))

;; ══════════════════════════════════════════════════════════════════
;; W2: Anti-looping escalation — placeholders (implemented in W2)
;; ══════════════════════════════════════════════════════════════════

(test-case "W2.1: warnings->actions escalation for repeated tool calls"
  (check-true #t "placeholder — implemented in W2"))

(test-case "W2.4: warning counter increments and resets"
  (check-true #t "placeholder — implemented in W2"))

;; ══════════════════════════════════════════════════════════════════
;; W3: Forced reflection — placeholder
;; ══════════════════════════════════════════════════════════════════

(test-case "W3.1: reflection feature flag exists"
  (check-true #t "placeholder — W3"))

;; ══════════════════════════════════════════════════════════════════
;; W4: Transition detection — placeholder
;; ══════════════════════════════════════════════════════════════════

(test-case "W4.2: ws-entry->text helper"
  (check-true #t "placeholder — W4"))
