#lang racket

;; BOUNDARY: pure

;; tests/test-agent-session-pure.rkt -- W1: Pure helper tests for agent-session.rkt
;;
;; Tests the extracted pure helpers:
;;   - slice-entries-up-to
;;   - make-session-struct

(require rackunit
         rackunit/text-ui
         "../runtime/agent-session.rkt"
         "../runtime/session-types.rkt"
         "../util/protocol-types.rkt")

;; Build a minimal message for testing
(define (test-msg id kind role content-text [parent-id #f])
  (make-message id parent-id role kind (list (make-text-part content-text)) 0 (hasheq)))

(define pure-suite
  (test-suite "agent-session pure helpers"

    ;; -- slice-entries-up-to: no parent-id returns all --
    (test-case "slice-entries-up-to with #f returns all entries"
      (define entries (list (test-msg "m1" 'message 'user "hi")
                            (test-msg "m2" 'message 'assistant "hello")))
      (check-equal? (slice-entries-up-to entries #f) entries))

    ;; -- slice-entries-up-to: parent-id found, truncates correctly --
    (test-case "slice-entries-up-to truncates up to parent-id"
      (define m1 (test-msg "m1" 'message 'user "a"))
      (define m2 (test-msg "m2" 'message 'user "b"))
      (define m3 (test-msg "m3" 'message 'assistant "c"))
      (define entries (list m1 m2 m3))
      (check-equal? (slice-entries-up-to entries "m2") (list m1 m2)))

    ;; -- slice-entries-up-to: parent-id not found returns all --
    (test-case "slice-entries-up-to with unknown id returns all"
      (define entries (list (test-msg "m1" 'message 'user "hi")))
      (check-equal? (slice-entries-up-to entries "unknown") entries))

    ;; -- slice-entries-up-to: empty list --
    (test-case "slice-entries-up-to with empty list returns empty"
      (check-equal? (slice-entries-up-to '() "m1") '()))

    ;; -- slice-entries-up-to: parent-id is first entry --
    (test-case "slice-entries-up-to parent-id first returns single entry"
      (define m1 (test-msg "m1" 'message 'user "a"))
      (define m2 (test-msg "m2" 'message 'user "b"))
      (check-equal? (slice-entries-up-to (list m1 m2) "m1") (list m1)))

    ;; -- make-session-struct: minimal construction --
    (test-case "make-session-struct creates agent-session?"
      (define sess (make-session-struct #:id "sid-1" #:dir "/tmp/test"))
      (check-true (agent-session? sess))
      (check-equal? (agent-session-session-id sess) "sid-1")
      (check-equal? (agent-session-session-dir sess) "/tmp/test")
      (check-true (agent-session-active? sess)))

    ;; -- make-session-struct: with optional fields --
    (test-case "make-session-struct respects optional fields"
      (define sess (make-session-struct
                    #:id "sid-2"
                    #:dir "/tmp/test2"
                    #:model-name "gpt-4"
                    #:system-instructions '("Be helpful")
                    #:active? #f
                    #:persisted? #t))
      (check-equal? (agent-session-model-name sess) "gpt-4")
      (check-equal? (agent-session-system-instructions sess) '("Be helpful"))
      (check-false (agent-session-active? sess))
      (check-true (agent-session-persisted? sess)))

    ;; -- make-session-struct: defaults --
    (test-case "make-session-struct defaults are sensible"
      (define sess (make-session-struct #:id "sid-3" #:dir "/tmp/test3"))
      (check-false (agent-session-compacting? sess))
      (check-false (agent-session-shutdown-requested? sess))
      (check-false (agent-session-prompt-running? sess))
      (check-equal? (agent-session-pending-entries sess) '()))))

(run-tests pure-suite 'verbose)
