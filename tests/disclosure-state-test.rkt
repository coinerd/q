#lang racket/base

;; @speed fast
;; @suite default

;; Tests for q/ui-core/disclosure-state.rkt

(require rackunit
         "../ui-core/disclosure-state.rkt"
         "../tui/state.rkt"
         "../tui/render/message-layout.rkt")

(define s0 (make-empty-disclosure-state))
(check-false (disclosure-expanded? s0 "a"))

(define s1 (disclosure-toggle s0 "a"))
(check-true (disclosure-expanded? s1 "a"))
(check-false (disclosure-expanded? s1 "b"))

(define s2 (disclosure-toggle s1 "a"))
(check-false (disclosure-expanded? s2 "a"))

(check-equal? (resolve-toggle-target s2 "explicit" "active" '("explicit" "c1" "c2")) "explicit")
(check-equal? (resolve-toggle-target s2 "input-component" "active" '("c1" "c2")) "active")
(check-equal? (resolve-toggle-target s2 "input-component" #f '("c1" "c2")) "c2")
(check-equal? (resolve-toggle-target s2 #f "active" '("c1" "c2")) "active")
(check-equal? (resolve-toggle-target s2 #f #f '("c1" "c2")) "c2")
(check-equal? (resolve-toggle-target s2 #f #f '()) #f)

(check-equal? (first-non-empty-line "\n\nhello\nworld") "hello")
(check-equal? (first-non-empty-line "   \n") #f)

(define preview (make-collapsed-preview "line1\nline2\nline3\nline4\nline5" 3 5))
(check-regexp-match #rx"line1" preview)
(check-regexp-match #rx"5 lines" preview)
(check-regexp-match #rx"Show 2 more" preview)
(check-regexp-match #rx"Ctrl\\+O to expand" preview)

(define small (make-collapsed-preview "only" 3 1))
(check-regexp-match #rx"only" small)
(check-regexp-match #rx"Show reasoning" small)
(check-regexp-match #rx"Ctrl\\+O to expand" small)

(define empty (make-collapsed-preview "" 3 0))
(check-regexp-match #rx"Thinking" empty)
(check-regexp-match #rx"Ctrl\\+O to expand" empty)

;; Rendering keys disclosure by canonical artifact ID, never the numeric row ID.
(define thinking-entry
  (transcript-entry 'thinking
                    "one\ntwo\nthree\nfour"
                    0
                    (hasheq 'artifact-id "session:turn:thinking")
                    7))
(define expanded (disclosure-toggle (make-empty-disclosure-state) "session:turn:thinking"))
(check-equal? (length (format-entry thinking-entry 80 expanded)) 4)
(define wrong-id-expanded (disclosure-toggle (make-empty-disclosure-state) 7))
(check-equal? (length (format-entry thinking-entry 80 wrong-id-expanded)) 1)
