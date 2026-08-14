#lang racket/base

;; Tests for q/ui-core/disclosure-state.rkt

(require rackunit
         "../ui-core/disclosure-state.rkt")

(define s0 (make-empty-disclosure-state))
(check-false (disclosure-expanded? s0 "a"))

(define s1 (disclosure-toggle s0 "a"))
(check-true (disclosure-expanded? s1 "a"))
(check-false (disclosure-expanded? s1 "b"))

(define s2 (disclosure-toggle s1 "a"))
(check-false (disclosure-expanded? s2 "a"))

(check-equal? (resolve-toggle-target s2 "explicit" "active" '("c1" "c2")) "explicit")
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
(check-false (regexp-match? #rx"Show" small))

(define empty (make-collapsed-preview "" 3 0))
(check-regexp-match #rx"Thinking" empty)
(check-regexp-match #rx"Ctrl\\+O to expand" empty)
