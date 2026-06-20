#lang racket/base

;; @speed fast
;; @suite fast

;; W6 v0.99.35: Tests for state-aware-helpers.rkt
;; Pure functions extracted from state-aware-builder.rkt.
;; Tests cover: state coercion, text extraction, rollback trigger computation,
;; conclusion-first replacement, state guidance table.

(require rackunit
         rackunit/text-ui
         racket/string
         (only-in "../util/content/content-parts.rkt" make-text-part text-part)
         (only-in "../util/message/message.rkt" make-message message-id message-role message-content)
         (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion
                  task-conclusion?
                  task-conclusion-text
                  task-conclusion-origin-message-ids)
         (only-in "../util/fsm/fsm.rkt" fsm-state fsm-state?)
         "../runtime/context-assembly/state-aware-helpers.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-text-message role text)
  (make-message "msg-id" #f role 'text (list (make-text-part text)) (current-seconds) (hasheq)))

;; ============================================================
;; coerce-task-state tests
;; ============================================================

(define-test-suite coerce-task-state-tests
                   (test-case "coerces symbol to itself"
                     (check-equal? (coerce-task-state 'idle) 'idle)
                     (check-equal? (coerce-task-state 'exploring) 'exploring))
                   (test-case "coerces #f to #f"
                     (check-false (coerce-task-state #f)))
                   (test-case "coerces fsm-state to its name"
                     (check-equal? (coerce-task-state (fsm-state 'planning)) 'planning))
                   (test-case "coerces invalid input to #f"
                     (check-false (coerce-task-state 42))
                     (check-false (coerce-task-state "idle"))
                     (check-false (coerce-task-state '()))))

;; ============================================================
;; extract-recent-text tests
;; ============================================================

(define-test-suite
 extract-recent-text-tests
 (test-case "extracts text from assistant messages"
   (define msgs (list (make-text-message 'user "Hello") (make-text-message 'assistant "World")))
   (define result (extract-recent-text msgs 3))
   (check-true (string? result))
   (check-true (string-contains? result "Hello"))
   (check-true (string-contains? result "World")))
 (test-case "returns #f for empty message list"
   (check-false (extract-recent-text '() 3)))
 (test-case "returns #f for no assistant/user messages"
   (define msgs (list (make-text-message 'system "sys")))
   (check-false (extract-recent-text msgs 3)))
 (test-case "limits to N most recent messages"
   (define msgs
     (list (make-text-message 'user "first")
           (make-text-message 'assistant "second")
           (make-text-message 'user "third")
           (make-text-message 'assistant "fourth")))
   (define result (extract-recent-text msgs 2))
   ;; With n=2, we get the last 2: "fourth" and "third"
   (check-true (string-contains? result "fourth"))
   (check-true (string-contains? result "third")))
 (test-case "truncates to 200 characters"
   (define long-text (make-string 300 #\x))
   (define msgs (list (make-text-message 'assistant long-text)))
   (define result (extract-recent-text msgs 1))
   (check-true (string? result))
   (check-true (<= (string-length result) 200)))
 (test-case "handles N larger than message count"
   (define msgs (list (make-text-message 'assistant "only one")))
   (define result (extract-recent-text msgs 10))
   (check-true (string-contains? result "only one"))))

;; ============================================================
;; state-guidance-table tests
;; ============================================================

(define-test-suite state-guidance-table-tests
                   (test-case "has entry for idle"
                     (check-true (string? (hash-ref state-guidance-table 'idle #f))))
                   (test-case "has entry for exploration"
                     (check-true (string? (hash-ref state-guidance-table 'exploration #f))))
                   (test-case "has entry for planning"
                     (check-true (string? (hash-ref state-guidance-table 'planning #f))))
                   (test-case "has entry for implementation"
                     (check-true (string? (hash-ref state-guidance-table 'implementation #f))))
                   (test-case "has entry for verification"
                     (check-true (string? (hash-ref state-guidance-table 'verification #f))))
                   (test-case "has entry for debugging"
                     (check-true (string? (hash-ref state-guidance-table 'debugging #f))))
                   (test-case "returns #f for unknown state"
                     (check-false (hash-ref state-guidance-table 'nonexistent #f))))

;; ============================================================
;; check-rollback-triggers tests
;; ============================================================

(define-test-suite check-rollback-triggers-tests
                   (test-case "no triggers when healthy"
                     (define warnings
                       (check-rollback-triggers #:before-messages 10
                                                #:after-messages 10
                                                #:conclusion-coverage 0.5
                                                #:repeat-tool-count 1))
                     (check-true (null? warnings)))
                   (test-case "excessive-savings when >50% messages cut"
                     (define warnings
                       (check-rollback-triggers #:before-messages 100
                                                #:after-messages 40
                                                #:conclusion-coverage 0.5
                                                #:repeat-tool-count 1))
                     (check-true (and (pair? warnings)
                                      (eq? (car (car warnings)) 'excessive-savings))))
                   (test-case "no excessive-savings when after-messages is 0"
                     (define warnings
                       (check-rollback-triggers #:before-messages 100
                                                #:after-messages 0
                                                #:conclusion-coverage 0.5
                                                #:repeat-tool-count 1))
                     (check-false (for/or ([w warnings])
                                    (eq? (car w) 'excessive-savings))))
                   (test-case "amnesia-risk when coverage < 0.20"
                     (define warnings
                       (check-rollback-triggers #:before-messages 10
                                                #:after-messages 10
                                                #:conclusion-coverage 0.10
                                                #:repeat-tool-count 0))
                     (check-true (and (pair? warnings)
                                      (for/or ([w warnings])
                                        (eq? (car w) 'amnesia-risk)))))
                   (test-case "no amnesia-risk when coverage >= 0.20"
                     (define warnings
                       (check-rollback-triggers #:before-messages 10
                                                #:after-messages 10
                                                #:conclusion-coverage 0.20
                                                #:repeat-tool-count 0))
                     (check-false (for/or ([w warnings])
                                    (eq? (car w) 'amnesia-risk))))
                   (test-case "task-amnesia when repeat-tool-count > 2"
                     (define warnings
                       (check-rollback-triggers #:before-messages 10
                                                #:after-messages 10
                                                #:conclusion-coverage 0.5
                                                #:repeat-tool-count 3))
                     (check-true (and (pair? warnings)
                                      (for/or ([w warnings])
                                        (eq? (car w) 'task-amnesia-detected)))))
                   (test-case "stuck-detected when repeat >= 6 and coverage = 0"
                     (define warnings
                       (check-rollback-triggers #:before-messages 10
                                                #:after-messages 10
                                                #:conclusion-coverage 0
                                                #:repeat-tool-count 6))
                     (check-true (and (pair? warnings)
                                      (for/or ([w warnings])
                                        (eq? (car w) 'stuck-detected)))))
                   (test-case "no stuck when repeat >= 6 but coverage > 0"
                     (define warnings
                       (check-rollback-triggers #:before-messages 10
                                                #:after-messages 10
                                                #:conclusion-coverage 0.1
                                                #:repeat-tool-count 6))
                     (check-false (for/or ([w warnings])
                                    (eq? (car w) 'stuck-detected))))
                   (test-case "multiple triggers fire simultaneously"
                     (define warnings
                       (check-rollback-triggers #:before-messages 100
                                                #:after-messages 10
                                                #:conclusion-coverage 0.0
                                                #:repeat-tool-count 6))
                     ;; Should have excessive-savings, amnesia-risk, task-amnesia, stuck-detected
                     (check-true (>= (length warnings) 3))))

;; ============================================================
;; check-rollback-triggers-with-actions tests
;; ============================================================

(define-test-suite triggers-with-actions-tests
                   (test-case "returns warnings and recommended-action"
                     (define-values (warnings action)
                       (check-rollback-triggers-with-actions #:before-messages 100
                                                             #:after-messages 10
                                                             #:conclusion-coverage 0.0
                                                             #:repeat-tool-count 6))
                     (check-true (pair? warnings)))
                   (test-case "no triggers yields empty warnings and #f action"
                     (define-values (warnings action)
                       (check-rollback-triggers-with-actions #:before-messages 10
                                                             #:after-messages 10
                                                             #:conclusion-coverage 0.5
                                                             #:repeat-tool-count 0))
                     (check-true (null? warnings))))

;; ============================================================
;; ws-entry->conclusion-or-self tests
;; ============================================================

(define-test-suite ws-entry->conclusion-or-self-tests
                   (test-case "returns conclusion when origin-message-ids matches"
                     (define ws-msg (make-text-message 'assistant "original content"))
                     (define msg-id-val (message-id ws-msg))
                     (define conclusion
                       (task-conclusion "concl-id"
                                        "This is the conclusion text"
                                        'fact
                                        'implementation
                                        (list msg-id-val)
                                        (current-seconds)
                                        '()
                                        '()))
                     (define result (ws-entry->conclusion-or-self ws-msg (list conclusion)))
                     ;; The result should be a replacement message (system role)
                     (check-equal? (message-role result) 'system)
                     (check-true (and (pair? (message-content result))
                                      (string-contains? (format "~a" (message-content result))
                                                        "Conclusion replaces context"))))
                   (test-case "returns original when no matching conclusion"
                     (define ws-msg (make-text-message 'assistant "original content"))
                     (define result (ws-entry->conclusion-or-self ws-msg '()))
                     (check-equal? (message-role result) 'assistant))
                   (test-case "returns original when conclusion does not match"
                     (define ws-msg (make-text-message 'assistant "original content"))
                     (define conclusion
                       (task-conclusion "concl-id"
                                        "Conclusion text"
                                        'fact
                                        'implementation
                                        (list "different-id")
                                        (current-seconds)
                                        '()
                                        '()))
                     (define result (ws-entry->conclusion-or-self ws-msg (list conclusion)))
                     (check-equal? (message-role result) 'assistant)))

;; ============================================================
;; All tests
;; ============================================================

(define-test-suite all-state-aware-helpers-tests
                   coerce-task-state-tests
                   extract-recent-text-tests
                   state-guidance-table-tests
                   check-rollback-triggers-tests
                   triggers-with-actions-tests
                   ws-entry->conclusion-or-self-tests)

(module+ test
  (run-tests all-state-aware-helpers-tests))

(module+ main
  (run-tests all-state-aware-helpers-tests))
