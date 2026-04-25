#lang racket/base

;; tests/test-protocol-types-split.rkt
;;
;; Verify that protocol-types.rkt facade re-exports all sub-module exports
;; and that sub-modules can be imported independently (ARCH-05).

(require rackunit
         "../util/protocol-types.rkt"
         ;; Also test direct sub-module imports
         "../util/content-parts.rkt"
         "../util/message.rkt"
         "../util/event.rkt"
         "../util/entry-predicates.rkt"
         "../util/tree-entries.rkt"
         "../util/loop-result.rkt")

;; ── Content parts (from facade and direct) ─────────────────
(check-equal? (text-part-text (make-text-part "hello")) "hello")
(check-equal? (tool-call-part-name (make-tool-call-part "1" "bash" '{})) "bash")
(check-equal? (tool-result-part-tool-call-id (make-tool-result-part "1" "ok" #f)) "1")

;; JSON round-trip
(define tp (make-text-part "test"))
(check-equal? (jsexpr->content-part (content-part->jsexpr tp)) tp)

;; ── Message (from facade and direct) ───────────────────────
(define msg (make-message "m1" #f 'user 'message (list tp) 1000 (hash)))
(check-equal? (message-id msg) "m1")
(check-equal? (message-role msg) 'user)
(check-equal? (jsexpr->message (message->jsexpr msg)) msg)

;; ── Event (from facade and direct) ─────────────────────────
(define evt (make-event "test-event" 1000 "sess1" "t1" (hash 'key "value")))
(check-equal? (event-ev evt) "test-event")
(check-equal? (event-event evt) "test-event")
(check-equal? (jsexpr->event (event->jsexpr evt)) evt)
(check-equal? CURRENT-EVENT-VERSION 1)

;; ── Entry predicates ───────────────────────────────────────
(check-true (message-entry? msg))
(check-false (tool-result-entry? msg))
(define tr-msg (make-message "m2" #f 'system 'tool-result '() 1000 (hash)))
(check-pred tool-result-entry? tr-msg)
(check-pred any-tool-result-entry? tr-msg)
(define bash-msg (make-message "m3" #f 'system 'bash-execution '() 1000 (hash)))
(check-pred bash-execution-entry? bash-msg)
(check-pred any-tool-result-entry? bash-msg)

;; ── Tree entries ───────────────────────────────────────────
(define be (make-branch-entry "b1" "parent1" "my-branch"))
(check-true (branch-entry? be))
(check-true (tree-entry? be))
(check-equal? (branch-entry-name be) "my-branch")
(check-equal? (branch-entry-parent-entry-id be) "parent1")

(define tne (make-tree-navigation-entry "n1" "from1" "to1"))
(check-true (tree-navigation-entry? tne))
(check-true (tree-entry? tne))
(check-equal? (tree-navigation-entry-target-entry-id tne) "to1")

(define bse (make-branch-summary-entry "s1" #f "summary text" '(1 . 10) 500))
(check-true (branch-summary-entry? bse))
(check-true (tree-entry? bse))
(check-equal? (branch-summary-entry-summary bse) "summary text")

;; ── Loop result ────────────────────────────────────────────
(define lr (make-loop-result '(msg1 msg2) 'max-turns (hash)))
(check-equal? (loop-result-messages lr) '(msg1 msg2))
(check-equal? (loop-result-termination-reason lr) 'max-turns)

;; ── Custom entry ───────────────────────────────────────────
(define ce (make-custom-entry "ext1" "key1" "data1"))
(check-true (custom-entry? ce))
(check-equal? (custom-entry-extension ce) "ext1")
(check-equal? (custom-entry-key ce) "key1")
(check-equal? (custom-entry-data ce) "data1")

;; ── Tool-call / tool-result (still in facade) ──────────────
(define tc (tool-call "tc1" "read" (hash 'path "/tmp")))
(check-equal? (tool-call-id tc) "tc1")
(check-equal? (tool-call-name tc) "read")

(define tr (tool-result "result content" (hash 'duration 1.5) #f))
(check-equal? (tool-result-content tr) "result content")
(check-false (tool-result-is-error? tr))
