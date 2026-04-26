#lang racket

;; tests/test-gsd-prompts.rkt — Prompt template tests
;;
;; Wave 3a: Verify all prompt templates contain required content.

(require rackunit
         "../extensions/gsd/prompts.rkt"
         "../extensions/gsd/plan-types.rkt"
         "../extensions/gsd/wave-executor.rkt")

;; ============================================================
;; Exploring prompt
;; ============================================================

(test-case "exploring-prompt includes planning instructions"
  (define p (exploring-prompt "Fix the bug"))
  (check-true (or (string-contains? p "Explore") (string-contains? p "explore")) "mentions explore")
  (check-true (string-contains? p "plan") "mentions plan")
  (check-true (string-contains? p "planning-write") "mentions planning-write"))

(test-case "exploring-prompt includes user request"
  (define p (exploring-prompt "Fix the login bug"))
  (check-true (string-contains? p "Fix the login bug")))

(test-case "exploring-prompt works with empty request"
  (define p (exploring-prompt ""))
  (check-true (string? p))
  (check-false (string-contains? p "User request:")))

(test-case "exploring-prompt mentions no limits"
  (define p (exploring-prompt "test"))
  (check-true (string-contains? p "No time or call limits")))

;; ============================================================
;; Executing prompt
;; ============================================================

(test-case "executing-prompt includes wave count"
  (define plan (gsd-plan
                (list (gsd-wave 0 "Setup" 'pending "" '("a.rkt") '() "test" '())
                      (gsd-wave 1 "Fix" 'pending "" '("b.rkt") '() "test" '())
                      (gsd-wave 2 "Verify" 'pending "" '("c.rkt") '() "test" '()))
                "" '() '()))
  (define exec (make-wave-executor plan))
  (define p (executing-prompt plan exec))
  (check-true (string-contains? p "3 waves")))

(test-case "executing-prompt mentions wave-skip"
  (define plan (gsd-plan (list (gsd-wave 0 "Fix" 'pending "" '("a.rkt") '() "test" '())) "" '() '()))
  (define exec (make-wave-executor plan))
  (define p (executing-prompt plan exec))
  (check-true (string-contains? p "/skip") "mentions skip"))

(test-case "executing-prompt includes error recovery"
  (define plan (gsd-plan (list (gsd-wave 0 "Fix" 'pending "" '("a.rkt") '() "test" '())) "" '() '()))
  (define exec (make-wave-executor plan))
  (define p (executing-prompt plan exec))
  (check-true (string-contains? p "Error recovery") "mentions error recovery"))

(test-case "executing-prompt mentions /replan"
  (define plan (gsd-plan (list (gsd-wave 0 "Fix" 'pending "" '("a.rkt") '() "test" '())) "" '() '()))
  (define exec (make-wave-executor plan))
  (define p (executing-prompt plan exec))
  (check-true (string-contains? p "/replan") "mentions replan"))

;; ============================================================
;; Wave failure prompt
;; ============================================================

(test-case "wave-failure-prompt includes wave number"
  (define p (wave-failure-prompt 2 "compile error"))
  (check-true (string-contains? p "Wave 2")))

(test-case "wave-failure-prompt includes reason"
  (define p (wave-failure-prompt 3 "test failure"))
  (check-true (string-contains? p "test failure")))

(test-case "wave-failure-prompt mentions skip"
  (define p (wave-failure-prompt 1 "error"))
  (check-true (string-contains? p "/skip")))

(test-case "wave-failure-prompt handles #f reason"
  (define p (wave-failure-prompt 0 #f))
  (check-true (string-contains? p "Wave 0")))

;; ============================================================
;; Verifying prompt
;; ============================================================

(test-case "verifying-prompt includes PASS/FAIL"
  (define plan (gsd-plan (list (gsd-wave 0 "Fix" 'pending "" '("a.rkt") '() "raco test" '())) "" '() '()))
  (define exec (make-wave-executor plan))
  (define p (verifying-prompt plan exec))
  (check-true (string-contains? p "PASS") "mentions PASS")
  (check-true (string-contains? p "FAIL") "mentions FAIL"))

(test-case "verifying-prompt includes verify commands"
  (define plan (gsd-plan (list (gsd-wave 0 "Fix" 'pending "" '("a.rkt") '() "raco test a" '())) "" '() '()))
  (define exec (make-wave-executor plan))
  (define p (verifying-prompt plan exec))
  (check-true (string-contains? p "raco test a")))

(test-case "verifying-prompt handles no verify command"
  (define plan (gsd-plan (list (gsd-wave 0 "Fix" 'pending "" '("a.rkt") '() "" '())) "" '() '()))
  (define exec (make-wave-executor plan))
  (define p (verifying-prompt plan exec))
  (check-true (string-contains? p "no verify command")))

;; ============================================================
;; Status prompt
;; ============================================================

(test-case "status-prompt shows mode"
  (define plan (gsd-plan (list (gsd-wave 0 "Fix" 'pending "" '("a.rkt") '() "test" '())) "" '() '()))
  (define exec (make-wave-executor plan))
  (define p (status-prompt 'executing exec))
  (check-true (string-contains? p "executing")))

(test-case "status-prompt shows summary"
  (define plan (gsd-plan (list (gsd-wave 0 "Fix" 'pending "" '("a.rkt") '() "test" '())) "" '() '()))
  (define exec (make-wave-executor plan))
  (wave-complete! exec 0)
  (define p (status-prompt 'executing exec))
  (check-true (string-contains? p "Completed")))
