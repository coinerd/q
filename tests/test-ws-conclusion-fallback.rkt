#lang racket/base

;; tests/test-ws-conclusion-fallback.rkt — M5 W0/W1: Conclusion-first working-set
;; v0.76.4: When WS is filtered/excluded, replace with matching conclusions.

(require rackunit
         racket/string
         "../runtime/context-assembly/state-aware-builder.rkt"
         "../runtime/context-assembly/context-floor.rkt"
         "../runtime/context-assembly/task-conclusion.rkt"
         (only-in "../util/protocol-types.rkt" make-message make-text-part message-content message-id)
         (only-in "../util/content-parts.rkt" text-part-text))

;; Helpers
(define (make-test-msg id text)
  (make-message id #f 'user 'text (list (make-text-part text)) (current-seconds) (hasheq)))

(define (make-conclusion text #:origin-ids [origin-ids '()])
  (task-conclusion "c1" text 'fact 'exploration origin-ids (current-seconds) '()))

;; ── W0: working-set-entry->conclusion-or-self ──

(test-case "ws-entry replaced when matching conclusion exists"
  (define ws-msg
    (make-test-msg "msg-1" "This is a very long file content that takes many tokens to represent."))
  (define conclusions (list (make-conclusion "Key insight about module X" #:origin-ids '("msg-1"))))
  (define result (ws-entry->conclusion-or-self ws-msg conclusions))
  ;; Result should be a message with conclusion text, not original text
  (define result-text (text-part-text (car (message-content result))))
  (check-true (string-contains? result-text "Key insight about module X"))
  (check-false (string-contains? result-text "very long file content")))

(test-case "ws-entry falls back when no matching conclusion"
  (define ws-msg (make-test-msg "msg-1" "This is a very long file content that takes many tokens."))
  (define conclusions (list (make-conclusion "Unrelated conclusion" #:origin-ids '("msg-2"))))
  (define result (ws-entry->conclusion-or-self ws-msg conclusions))
  ;; Should keep original message
  (check-equal? (message-id result) "msg-1")
  (define result-text (text-part-text (car (message-content result))))
  (check-true (string-contains? result-text "very long file content")))

(test-case "ws-entry falls back when no conclusions at all"
  (define ws-msg (make-test-msg "msg-1" "File content here."))
  (define result (ws-entry->conclusion-or-self ws-msg '()))
  (check-equal? (message-id result) "msg-1"))

;; ── W1: Integration with build-tiered-context/state-aware ──

(test-case "filtered WS replaces entries with conclusions"
  (define ws-msg (make-test-msg "ws-1" (make-string 500 #\x)))
  (define conclusions (list (make-conclusion "Short summary" #:origin-ids '("ws-1"))))
  (define tc
    (build-tiered-context/state-aware (list (make-test-msg "m1" "hello"))
                                      #:tier-b-count 5
                                      #:tier-c-count 2
                                      #:working-set-messages (list ws-msg)
                                      #:task-state 'implementation
                                      #:conclusions conclusions))
  ;; The WS message in tier-a should be replaced with conclusion text
  (define tier-a-texts
    (for/list ([m (tiered-context-tier-a tc)])
      (text-part-text (car (message-content m)))))
  ;; At least one tier-a message should contain "Short summary"
  (check-true (ormap (λ (t) (string-contains? t "Short summary")) tier-a-texts)))

(test-case "mixed WS: some replaced, some fall back"
  (define ws-1 (make-test-msg "ws-1" (make-string 500 #\x)))
  (define ws-2 (make-test-msg "ws-2" (make-string 500 #\y)))
  (define conclusions (list (make-conclusion "Summary of ws-1" #:origin-ids '("ws-1"))))
  ;; verification has ws-level='filtered, so WS entries are kept and replaced where possible
  (define tc
    (build-tiered-context/state-aware (list (make-test-msg "m1" "hello"))
                                      #:tier-b-count 5
                                      #:tier-c-count 2
                                      #:working-set-messages (list ws-1 ws-2)
                                      #:task-state 'verification
                                      #:conclusions conclusions))
  (define tier-a-texts
    (for/list ([m (tiered-context-tier-a tc)])
      (text-part-text (car (message-content m)))))
  ;; ws-1 should be replaced with conclusion
  (check-true (ormap (λ (t) (string-contains? t "Summary of ws-1")) tier-a-texts))
  ;; ws-2 should fall back (original long content) since no conclusion matches
  (check-true (ormap (λ (t) (string-contains? t (make-string 500 #\y))) tier-a-texts)))

(test-case "token count reduced when conclusions replace WS entries"
  (define ws-msg
    (make-test-msg "ws-1"
                   (apply string-append
                          (for/list ([i 100])
                            (format "Word~a " i)))))
  (define conclusions-no-match (list (make-conclusion "Unrelated" #:origin-ids '("other"))))
  (define conclusions-match (list (make-conclusion "Short" #:origin-ids '("ws-1"))))
  ;; verification has ws-level='filtered, so WS entries are kept
  (define tc-no-replace
    (build-tiered-context/state-aware (list (make-test-msg "m1" "hi"))
                                      #:tier-b-count 5
                                      #:tier-c-count 2
                                      #:working-set-messages (list ws-msg)
                                      #:task-state 'verification
                                      #:conclusions conclusions-no-match))
  (define tc-replace
    (build-tiered-context/state-aware (list (make-test-msg "m1" "hi"))
                                      #:tier-b-count 5
                                      #:tier-c-count 2
                                      #:working-set-messages (list ws-msg)
                                      #:task-state 'verification
                                      #:conclusions conclusions-match))

  ;; With replacement, total tokens should be fewer
  (define (tc-tokens tc)
    (+ (for/sum ([m (tiered-context-tier-a tc)])
                (string-length (text-part-text (car (message-content m)))))
       (for/sum ([m (tiered-context-tier-b tc)])
                (string-length (text-part-text (car (message-content m)))))
       (for/sum ([m (tiered-context-tier-c tc)])
                (string-length (text-part-text (car (message-content m)))))))
  (check-true (< (tc-tokens tc-replace) (tc-tokens tc-no-replace))))
