#lang racket

;; @speed fast  ;; @suite extensions

;; BOUNDARY: integration

;; tests/test-gsd-prompts.rkt — Prompt template tests
;;
;; Wave 3a: Verify all prompt templates contain required content.

(require rackunit
         "../extensions/gsd/prompts.rkt"
         "../extensions/gsd/plan-types.rkt"
         "../extensions/gsd/wave-executor.rkt")

;; ============================================================
;; Planning prompt (W0: renamed from exploring-prompt)
;; ============================================================

(test-case "planning-prompt includes planning instructions"
  (define p (planning-prompt "Fix the bug"))
  (check-true (or (string-contains? p "Plan") (string-contains? p "plan")) "mentions plan")
  (check-true (string-contains? p "planning-write") "mentions planning-write"))

(test-case "planning-prompt includes user request"
  (define p (planning-prompt "Fix the login bug"))
  (check-true (string-contains? p "Fix the login bug")))

(test-case "planning-prompt works with empty request"
  (define p (planning-prompt ""))
  (check-true (string? p))
  (check-false (string-contains? p "User request:")))

(test-case "planning-prompt enforces write-immediately workflow"
  (define p (planning-prompt "test"))
  (check-true (string-contains? p "IMMEDIATELY write") "mentions write-immediately")
  (check-true (string-contains? p "NEVER re-read") "mentions never re-read"))

(test-case "W0: planning-prompt contains - File: format instruction"
  (define p (planning-prompt "test"))
  (check-true (string-contains? p "- File: <path>") "shows exact File syntax"))

(test-case "W0: planning-prompt contains SEPARATE instruction"
  (define p (planning-prompt "test"))
  (check-true (string-contains? p "SEPARATE") "emphasizes separate files per wave"))

(test-case "W0: planning-prompt contains STEP 1-4 structure"
  (define p (planning-prompt "test"))
  (check-true (string-contains? p "STEP 1") "has step 1")
  (check-true (string-contains? p "STEP 2") "has step 2")
  (check-true (string-contains? p "STEP 3") "has step 3")
  (check-true (string-contains? p "STEP 4") "has step 4"))

(test-case "planning-prompt mentions planning-read tool"
  (define p (planning-prompt "test"))
  (check-true (string-contains? p "planning-read") "mentions planning-read"))

(test-case "planning-prompt includes filesystem path guidance"
  (define p (planning-prompt "test"))
  (check-true (string-contains? p "filesystem path") "mentions filesystem path"))

(test-case "planning-prompt wave template is language-agnostic"
  (define p (planning-prompt "test"))
  (check-false (string-contains? p "q/path/to") "no q/ hardcoded path")
  (check-false (string-contains? p "raco test") "no raco test hardcoded"))

;; ============================================================
;; Executing prompt
;; ============================================================

(test-case "executing-prompt includes wave count"
  (define plan
    (gsd-plan (list (gsd-wave 0 "Setup" 'pending "" '("a.rkt") '() "test" '())
                    (gsd-wave 1 "Fix" 'pending "" '("b.rkt") '() "test" '())
                    (gsd-wave 2 "Verify" 'pending "" '("c.rkt") '() "test" '()))
              ""
              '()
              '()))
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

(test-case "executing-prompt includes edit rules (#2165)"
  (define plan (gsd-plan (list (gsd-wave 0 "Fix" 'pending "" '("a.rkt") '() "test" '())) "" '() '()))
  (define exec (make-wave-executor plan))
  (define p (executing-prompt plan exec))
  (check-true (string-contains? p "Edit rules") "mentions edit rules")
  (check-true (string-contains? p "\u226420") "mentions 20-line limit")
  (check-true (string-contains? p "\u2264500") "mentions 500-char limit"))

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
  (define plan
    (gsd-plan (list (gsd-wave 0 "Fix" 'pending "" '("a.rkt") '() "raco test" '())) "" '() '()))
  (define exec (make-wave-executor plan))
  (define p (verifying-prompt plan exec))
  (check-true (string-contains? p "PASS") "mentions PASS")
  (check-true (string-contains? p "FAIL") "mentions FAIL"))

(test-case "verifying-prompt includes verify commands"
  (define plan
    (gsd-plan (list (gsd-wave 0 "Fix" 'pending "" '("a.rkt") '() "raco test a" '())) "" '() '()))
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
