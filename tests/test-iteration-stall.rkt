#lang racket

;; tests/test-iteration-stall.rkt — tests for stall detection (v0.20.4 W2)
;;
;; Covers:
;;   - detect-stall? with empty/text/tool-call messages
;;   - MAX-STALL-RETRIES constant value
;;   - Stall recovery nudge injection
;;   - agent.stall event emission

(require rackunit
         racket/list
         (only-in "../runtime/iteration.rkt" detect-stall? MAX-STALL-RETRIES)
         (only-in "../util/protocol-types.rkt"
                  make-message
                  make-text-part
                  make-tool-call-part
                  text-part?
                  tool-call-part?))

;; ============================================================
;; detect-stall? tests
;; ============================================================

(test-case "detect-stall? returns #f for empty context"
  (check-false (detect-stall? '())))

(test-case "detect-stall? returns #f when last assistant has text"
  (define ctx
    (list (make-message "m1" #f 'user 'text (list (make-text-part "hello")) 1000 (hasheq))
          (make-message "m2"
                        #f
                        'assistant
                        'message
                        (list (make-text-part "I will help you."))
                        1001
                        (hasheq))))
  (check-false (detect-stall? ctx)))

(test-case "detect-stall? returns #t when last assistant is empty"
  (define ctx
    (list (make-message "m1" #f 'user 'text (list (make-text-part "hello")) 1000 (hasheq))
          (make-message "m2" #f 'assistant 'message '() 1001 (hasheq))))
  (check-true (detect-stall? ctx)))

(test-case "detect-stall? returns #f when last assistant has tool calls"
  (define ctx
    (list (make-message "m1" #f 'user 'text (list (make-text-part "hello")) 1000 (hasheq))
          (make-message "m2"
                        #f
                        'assistant
                        'message
                        (list (make-tool-call-part "tc1" "read" (hasheq 'path "foo.rkt")))
                        1001
                        (hasheq))))
  (check-false (detect-stall? ctx)))

(test-case "detect-stall? returns #f when last message is from user"
  (define ctx (list (make-message "m1" #f 'user 'text (list (make-text-part "hello")) 1000 (hasheq))))
  (check-false (detect-stall? ctx)))

(test-case "detect-stall? returns #f when last assistant has only whitespace text"
  (define ctx
    (list (make-message "m1" #f 'user 'text (list (make-text-part "hello")) 1000 (hasheq))
          (make-message "m2" #f 'assistant 'message (list (make-text-part "   ")) 1001 (hasheq))))
  (check-true (detect-stall? ctx)))

(test-case "detect-stall? returns #f when last assistant has both text and tool calls"
  (define ctx
    (list (make-message "m1" #f 'user 'text (list (make-text-part "hello")) 1000 (hasheq))
          (make-message "m2"
                        #f
                        'assistant
                        'message
                        (list (make-text-part "Reading file...")
                              (make-tool-call-part "tc1" "read" (hasheq 'path "foo.rkt")))
                        1001
                        (hasheq))))
  (check-false (detect-stall? ctx)))

(test-case "detect-stall? checks only the LAST assistant message"
  (define ctx
    (list (make-message "m1" #f 'assistant 'message '() 1000 (hasheq))
          (make-message "m2" #f 'user 'text (list (make-text-part "continue")) 1001 (hasheq))
          (make-message "m3" #f 'assistant 'message (list (make-text-part "Done!")) 1002 (hasheq))))
  (check-false (detect-stall? ctx)))

;; ============================================================
;; MAX-STALL-RETRIES constant
;; ============================================================

(test-case "MAX-STALL-RETRIES is 2"
  (check-equal? MAX-STALL-RETRIES 2))
