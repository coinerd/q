#lang racket/base

;; @speed fast
;; @suite default
;; @boundary unit

;; tests/test-ws-conclusion-fallback.rkt — M5 W0/W1: Conclusion-first working-set
;; v0.76.4: When WS is filtered/excluded, replace with matching conclusions.

(require rackunit
         racket/string
         "../runtime/context-assembly/state-aware-builder.rkt"
         "../runtime/context-assembly/context-floor.rkt"
         "../runtime/context-assembly/task-conclusion.rkt"
         (only-in "../util/message/protocol-types.rkt" make-message make-text-part message-content message-id)
         (only-in "../util/content/content-parts.rkt" text-part-text))

;; Helpers
(define (make-test-msg id text)
  (make-message id #f 'user 'text (list (make-text-part text)) (current-seconds) (hasheq)))

(define (make-conclusion text #:origin-ids [origin-ids '()])
  (task-conclusion "c1" text 'fact 'exploration origin-ids (current-seconds) '() '()))

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

;; ── W2-T2 (v0.76.7 W8): excluded WS path test ──

(test-case "excluded WS: matched entries replaced, unmatched dropped"
  ;; implementation state has ws-level='excluded
  ;; Matched WS entries become compact conclusions; unmatched entries are dropped entirely
  (define ws-1 (make-test-msg "ws-1" (make-string 500 #\x)))
  (define ws-2 (make-test-msg "ws-2" (make-string 500 #\y)))
  (define conclusions (list (make-conclusion "Summary of ws-1" #:origin-ids '("ws-1"))))
  (define tc
    (build-tiered-context/state-aware (list (make-test-msg "m1" "hello"))
                                      #:tier-b-count 5
                                      #:tier-c-count 2
                                      #:working-set-messages (list ws-1 ws-2)
                                      #:task-state 'implementation
                                      #:conclusions conclusions))
  (define tier-a-texts
    (for/list ([m (tiered-context-tier-a tc)])
      (text-part-text (car (message-content m)))))
  ;; ws-1 should be replaced with conclusion (present in tier-a)
  (check-true (ormap (λ (t) (string-contains? t "Summary of ws-1")) tier-a-texts)
              "matched WS entry should be replaced with conclusion")
  ;; ws-2 should be completely dropped (not in any tier)
  (define all-texts
    (append tier-a-texts
            (for/list ([m (tiered-context-tier-b tc)])
              (text-part-text (car (message-content m))))
            (for/list ([m (tiered-context-tier-c tc)])
              (text-part-text (car (message-content m))))))
  (check-false (ormap (λ (t) (string-contains? t (make-string 500 #\y))) all-texts)
               "unmatched WS entry should be dropped entirely in excluded state"))

;; ── R99b (BUG-0099): the ACTIVATED deterministic writer emits honest
;; internal 'provenance records; WS replacement stays explicitly labeled
;; provenance (never [Conclusion]), and a genuine insight sharing the origin
;; still wins. ──

(require (only-in "../runtime/context-assembly/auto-distillation.rkt"
                  generate-fallback-conclusions)
         (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  provenance-kind?))

(define (make-fallback-conclusion msg-id)
  ;; The real writer output (timestamp pinned for deterministic ids only).
  (car (generate-fallback-conclusions (list msg-id) 'implementation)))

(test-case "R99b: excluded WS — writer fallback replacement labeled [Source provenance] with the honest record"
  (define ws-1 (make-test-msg "ws-1" (make-string 500 #\x)))
  (define fallback (make-fallback-conclusion "ws-1"))
  (check-true (provenance-kind? fallback))
  (check-equal? (task-conclusion-text fallback)
                "Source message ws-1 retained as provenance; no distilled insight available.")
  (define tc
    (build-tiered-context/state-aware (list (make-test-msg "m1" "hello"))
                                      #:tier-b-count 5
                                      #:tier-c-count 2
                                      #:working-set-messages (list ws-1)
                                      #:task-state 'implementation
                                      #:conclusions (list fallback)))
  (define tier-a-texts
    (for/list ([m (tiered-context-tier-a tc)])
      (text-part-text (car (message-content m)))))
  (check-true
   (ormap (λ (t) (string-contains?
                  t
                  (string-append "[Source provenance replaces context] (unverified) "
                                 "Source message ws-1 retained as provenance; "
                                 "no distilled insight available.")))
          tier-a-texts)
   "fallback replacement must be labeled provenance with the honest record text")
  (check-false (ormap (λ (t) (string-contains? t "[Conclusion replaces context]"))
                      tier-a-texts)
               "fallback must not render as [Conclusion]")
  ;; The raw WS content must never leak into the replacement.
  (check-false (ormap (λ (t) (string-contains? t (make-string 100 #\x)))
                      tier-a-texts)
               "no source excerpt may leak into the provenance replacement"))

(test-case "R99b: genuine insight sharing the origin wins in excluded WS path"
  (define ws-1 (make-test-msg "ws-1" (make-string 500 #\x)))
  (define fallback (make-fallback-conclusion "ws-1"))
  (define insight (make-conclusion "Curated summary of ws-1" #:origin-ids '("ws-1")))
  (define tc
    (build-tiered-context/state-aware (list (make-test-msg "m1" "hello"))
                                      #:tier-b-count 5
                                      #:tier-c-count 2
                                      #:working-set-messages (list ws-1)
                                      #:task-state 'implementation
                                      #:conclusions (list fallback insight)))
  (define tier-a-texts
    (for/list ([m (tiered-context-tier-a tc)])
      (text-part-text (car (message-content m)))))
  (check-true (ormap (λ (t) (string-contains? t "[Conclusion replaces context] Curated summary of ws-1"))
                     tier-a-texts)
              "genuine insight wins the replacement")
  (check-false (ormap (λ (t) (string-contains? t "[Source provenance replaces context]"))
                      tier-a-texts)
               "no provenance replacement when a genuine insight covers the origin"))
