#lang racket

;; q/tests/test-gui-views.rkt — Tests for transcript + message-entry views

(require rackunit
         rackunit/text-ui
         "../ui-core/theme-protocol.rkt"
         "../gui/views/transcript.rkt"
         "../gui/views/message-entry.rkt")

(define-test-suite
 test-gui-views
 ;; ── Transcript ──
 (test-case "render-transcript with empty messages"
   (define result (render-transcript (default-theme) '()))
   (check-equal? (hash-ref result 'total-messages) 0)
   (check-equal? (hash-ref result 'scroll-offset) 0))
 (test-case "render-transcript with messages"
   (define msgs (build-list 10 (lambda (i) (hash 'role 'user 'text (format "msg ~a" i)))))
   (define result (render-transcript (default-theme) msgs))
   (check-equal? (hash-ref result 'total-messages) 10))
 (test-case "render-transcript clamps scroll offset"
   (define msgs (build-list 5 (lambda (i) (hash 'role 'user 'text (format "msg ~a" i)))))
   (define result (render-transcript (default-theme) msgs #:scroll-offset 100))
   (check-equal? (hash-ref result 'scroll-offset) 4))
 (test-case "transcript-scroll-up decrements offset"
   (define desc (hash 'scroll-offset 5 'total-messages 10))
   (define result (transcript-scroll-up desc))
   (check-equal? (hash-ref result 'scroll-offset) 4))
 (test-case "transcript-scroll-down increments offset"
   (define desc (hash 'scroll-offset 3 'total-messages 10))
   (define result (transcript-scroll-down desc))
   (check-equal? (hash-ref result 'scroll-offset) 4))
 (test-case "transcript-scroll-top sets offset to 0"
   (define desc (hash 'scroll-offset 5 'total-messages 10))
   (define result (transcript-scroll-top desc))
   (check-equal? (hash-ref result 'scroll-offset) 0))
 (test-case "transcript-scroll-bottom sets to last message"
   (define desc (hash 'scroll-offset 0 'total-messages 10))
   (define result (transcript-scroll-bottom desc))
   (check-equal? (hash-ref result 'scroll-offset) 9))
 ;; ── Message Entry ──
 (test-case "render-message-entry for user message"
   (define msg (hash 'role 'user 'text "Hello"))
   (define result (render-message-entry (default-theme) msg))
   (check-equal? (hash-ref result 'role) 'user)
   (check-equal? (hash-ref result 'text) "Hello")
   (check-equal? (hash-ref result 'label) "You"))
 (test-case "render-message-entry for assistant message"
   (define msg (hash 'role 'assistant 'text "Hi there"))
   (define result (render-message-entry (default-theme) msg))
   (check-equal? (hash-ref result 'role) 'assistant)
   (check-equal? (hash-ref result 'label) "Assistant"))
 (test-case "render-message-entry detects code blocks"
   (define msg (hash 'role 'assistant 'text "Here is code:\n```racket\n(+ 1 2)\n```\nDone."))
   (define result (render-message-entry (default-theme) msg))
   (check-true (hash-ref result 'has-code))
   (check-true (>= (hash-ref result 'code-blocks) 1)))
 (test-case "render-message-entry without code blocks"
   (define msg (hash 'role 'user 'text "Just plain text"))
   (define result (render-message-entry (default-theme) msg))
   (check-false (hash-ref result 'has-code)))
 (test-case "message-role-style maps all roles"
   (define theme (default-theme))
   (for ([role '(user assistant system tool error)])
     (define style (message-role-style role theme))
     (check-not-false (assoc 'fg style))
     (check-not-false (assoc 'label style)))))

(run-tests test-gui-views)
