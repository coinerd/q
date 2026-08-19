#lang racket/base

;; @covers runtime/memory/policy.rkt
;; @covers runtime/memory/types.rkt

;; @speed fast
;; @suite default
;; @boundary unit

;; v1.00.04 W5 — Mutation-survivor tests (assessment Action 6).
;;
;; Scope: kill the surviving boundary-value and boolean-policy mutation
;; classes found by the bounded mutation pilot for runtime/memory/policy.rkt
;; (docs/operations/test-consolidation-policy.md; scripts/run-tests/mutation-pilot.rkt).
;;
;; BEFORE (artifacts/mutation-pilot/run-6):
;;   planned=18 killed=4 survived=14 detection=0.222
;; AFTER (artifacts/mutation-pilot/run-8, scoped via
;;   `racket scripts/run-tests/mutation-pilot.rkt --modules runtime/memory/policy.rkt`):
;;   planned=18 killed=16 survived=2 detection=0.889
;;   The 2 remaining survivors are the EQUIVALENT MUTANTS below (lines 79
;;   and 115); every semantically observable boundary and boolean-policy
;;   mutant is now killed. (run-7 was discarded: it predated regenerating
;;   tests/.coverage-manifest.json, so the pilot did not see this file.)
;;
;; Survivor classification (line:operator -> class):
;;   79  numeric-boundary-1  -> EQUIVALENT MUTANT (see below)
;;   115 boolean-and->or     -> EQUIVALENT MUTANT (see below)
;;   146 boolean-and->or     -> boolean-policy      (killed here)
;;   163 comparison-le->lt   -> boundary            (killed here)
;;   165 numeric-boundary+1  -> boundary            (killed here)
;;   165 arithmetic-sub->add -> boundary            (killed here)
;;   180 comparison-le->lt   -> boundary            (killed here)
;;   186 boolean-and->or     -> boolean-policy      (killed here)
;;   193 boolean-and->or     -> boolean-policy      (killed here)
;;   195 comparison-le->lt   -> boundary            (killed here)
;;   195 numeric-boundary+1  -> boundary            (killed here)
;;   199 boolean-and->or     -> boolean-policy      (killed here)
;;   203 boolean-and->or     -> boolean-policy      (killed here)
;;   203 comparison-le->lt   -> boundary            (killed here)
;;
;; Equivalent-mutant proofs (the only allowed survivors; both are listed per
;; the wave's Done criteria and are outside any *semantic* boundary/boolean
;; class — they cannot change observable behaviour):
;;   * Line 79 (`;; Multi-line .env holistic pattern (P3-1)`): the mutated
;;     literal is the `1` inside the COMMENT text `(P3-1)`. The operator
;;     regex `(?<![\w#])1(?![\w.])` matches digits in comments; changing it
;;     to `P3-0` alters no code. No test can observe it.
;;   * Line 115 (`[(and requested-scope (memory-scope? requested-scope))
;;     requested-scope]` in effective-memory-scope): the very next cond
;;     clause is `[requested-scope requested-scope]`, so whenever
;;     `requested-scope` is truthy the mutant's first clause is truthy too
;;     (and both yield `requested-scope`); when it is `#f`,
;;     `(memory-scope? #f)` is `#f`, so the mutant's `(or ...)` is also
;;     falsy. Every input produces the identical result. Pinned below by
;;     explicit polarity tests anyway (the pass-through contract).
;;
;; Rule (Action 4): no existing test is deleted or weakened here; this file
;; only adds assertions. Consolidation of runtime/memory/policy.rkt tests
;; remains blocked per the test-consolidation policy.

(require rackunit
         racket/string
         "../../runtime/memory/policy.rkt"
         "../../runtime/memory/types.rkt")

;; Valid item factory: satisfies every valid-memory-item? requirement
;; (required metadata + validity keys, origin id, ISO-8601 timestamps).
(define (valid-item #:content [content "remember this"]
                    #:sensitivity [sensitivity 'public]
                    #:scope [scope 'project])
  (memory-item "mut-item"
               'semantic
               scope
               content
               (hasheq 'tags '()
                       'source 'tool
                       'project-root "/tmp/project"
                       'session-id "sess"
                       'origin-message-id "m1")
               (hasheq 'sensitivity sensitivity
                       'confidence 1.0
                       'supersedes '())
               "2026-01-01T00:00:00Z"
               "2026-01-01T00:00:00Z"))

;; ---------------------------------------------------------------------------
;; effective-memory-scope — polarity pin (survivor 115 is equivalent; the
;; pass-through behaviour itself is asserted on both sides of the condition)
;; ---------------------------------------------------------------------------

(test-case "W5 line 115 pin: scope resolution honours requested scope and fallbacks"
  (check-eq? (effective-memory-scope 'session #f) 'session)
  (check-eq? (effective-memory-scope 'project "/root") 'project)
  ;; Truthy but invalid scopes are passed through unchanged.
  (check-eq? (effective-memory-scope 'bogus "/root") 'bogus)
  (check-eq? (effective-memory-scope 'user #f) 'user)
  ;; Fallbacks when nothing is requested.
  (check-eq? (effective-memory-scope #f "/root") 'project)
  (check-eq? (effective-memory-scope #f #f) 'session))

;; ---------------------------------------------------------------------------
;; Boolean-policy survivor 115-adjacent — policy-allows-scope? (line 124/126)
;; ---------------------------------------------------------------------------

(test-case "W5: allows-scope requires a real memory scope"
  ;; 'bogus is not a memory-scope; it must stay rejected for every policy.
  (check-false (policy-allows-scope? default-memory-policy 'bogus)
               "non-scope symbols are never allowed, even with user scope enabled")
  (check-false (policy-allows-scope? (make-memory-policy #:user-scope-enabled? #t) 'bogus))
  ;; Both polarities for the real scopes (condition-negation detection).
  (check-true (policy-allows-scope? default-memory-policy 'session))
  (check-true (policy-allows-scope? default-memory-policy 'project))
  (check-false (policy-allows-scope? default-memory-policy 'user))
  (check-true (policy-allows-scope? (make-memory-policy #:user-scope-enabled? #t) 'user))
  ;; Non-policy input is rejected.
  (check-false (policy-allows-scope? "not-a-policy" 'project))
  (check-false (policy-allows-scope? #f 'project)))

;; ---------------------------------------------------------------------------
;; Boolean-policy survivor — safe-memory-content? (line 146)
;; ---------------------------------------------------------------------------

(test-case "W5 mutation 146: safe-memory-content? requires a string (and->or)"
  ;; Non-string content must be unsafe regardless of the pattern scan.
  (check-false (safe-memory-content? default-memory-policy 42))
  (check-false (safe-memory-content? default-memory-policy 'symbol))
  (check-false (safe-memory-content? default-memory-policy #f))
  (check-false (safe-memory-content? default-memory-policy (list "a" "b")))
  ;; Both polarities on the string side.
  (check-true (safe-memory-content? default-memory-policy "perfectly plain text"))
  (check-false (safe-memory-content? default-memory-policy "password=hunter2"))
  ;; Non-policy input is unsafe.
  (check-false (safe-memory-content? "not-a-policy" "text")))

;; ---------------------------------------------------------------------------
;; Boolean-policy survivor — policy-check-content-safety (line 186)
;; ---------------------------------------------------------------------------

(test-case "W5 mutation 186: content-safety requires policy and string (and->or)"
  (check-false (policy-check-content-safety "not-a-policy" "text"))
  (check-false (policy-check-content-safety #f "text"))
  (check-false (policy-check-content-safety default-memory-policy 42))
  (check-false (policy-check-content-safety default-memory-policy 'sym))
  (check-false (policy-check-content-safety default-memory-policy #f))
  ;; Both polarities of the pattern scan.
  (check-true (policy-check-content-safety default-memory-policy "safe content"))
  (check-false (policy-check-content-safety default-memory-policy "AKIAIOSFODNN7EXAMPLE")))

(test-case "W5 mutation 186: content-safety scans the whole blocked list"
  ;; A policy with several patterns: content matching only the LAST pattern
  ;; must be blocked, so the for/and cannot weaken into a disjunction.
  (define p
    (make-memory-policy #:blocked-content-patterns
                        (list #px"^forbidden-value$" #px"sk-live-[A-Za-z0-9_-]{8,}")))
  (check-true (policy-check-content-safety p "harmless words here"))
  (check-false (policy-check-content-safety p "key sk-live-abcdefgh1234 here")))

;; ---------------------------------------------------------------------------
;; Boundary survivor — store content length (line 180)
;; ---------------------------------------------------------------------------

(test-case "W5 mutation 180: store allows content exactly at max length (<= -> <)"
  (define p (make-memory-policy #:max-content-length 10))
  (check-true (policy-allows-store? p (valid-item #:content "0123456789"))
              "content of exactly max-content-length chars must be storable")
  (check-false (policy-allows-store? p (valid-item #:content "01234567890"))
               "content one char beyond the limit must be rejected"))

(test-case "W5 mutation 180: store boundary holds at the default 10000-char limit"
  (check-true (policy-allows-store? default-memory-policy
                                    (valid-item #:content (make-string 10000 #\a))))
  (check-false (policy-allows-store? default-memory-policy
                                     (valid-item #:content (make-string 10001 #\a)))))

;; ---------------------------------------------------------------------------
;; Boundary + boolean survivors — policy-allows-retrieve? (lines 193/195)
;; ---------------------------------------------------------------------------

(test-case "W5 mutations 195: retrieve boundary is inclusive at 0 and at max (<= -> <, 0 -> 1)"
  (define p (make-memory-policy #:max-retrieve-count 3))
  (check-true (policy-allows-retrieve? p 0) "requesting 0 items is within budget")
  (check-true (policy-allows-retrieve? p 3) "requesting exactly max is within budget")
  (check-false (policy-allows-retrieve? p 4) "requesting max+1 exceeds the budget")
  (check-true (policy-allows-retrieve? p 2))
  (check-false (policy-allows-retrieve? p -1) "negative counts are out of budget"))

(test-case "W5 mutation 193: retrieve requires an integer count (and->or)"
  (check-false (policy-allows-retrieve? default-memory-policy 'three))
  (check-false (policy-allows-retrieve? default-memory-policy "3"))
  (check-false (policy-allows-retrieve? default-memory-policy 3.5))
  (check-false (policy-allows-retrieve? default-memory-policy '(3)))
  ;; Non-policy input with an otherwise-valid count.
  (check-false (policy-allows-retrieve? "not-a-policy" 1))
  (check-true (policy-allows-retrieve? default-memory-policy 1)))

;; ---------------------------------------------------------------------------
;; Boolean survivor — policy-allows-delete? (line 199)
;; ---------------------------------------------------------------------------

(test-case "W5 mutation 199: delete requires a policy and its flag (and->or)"
  (check-false (policy-allows-delete? "not-a-policy"))
  (check-false (policy-allows-delete? #f))
  ;; Both polarities of the stored flag.
  (check-true (policy-allows-delete? (make-memory-policy #:allow-delete? #t)))
  (check-false (policy-allows-delete? (make-memory-policy #:allow-delete? #f))))

;; ---------------------------------------------------------------------------
;; Boundary + boolean survivors — policy-within-budget? (line 203)
;; ---------------------------------------------------------------------------

(test-case "W5 mutations 203: within-budget is inclusive at max (<= -> <, and->or)"
  (define p (make-memory-policy #:max-retrieve-count 3))
  (check-true (policy-within-budget? p '()) "empty result list is within budget")
  (check-true (policy-within-budget? p (list 1 2 3))
              "a result list of exactly max-retrieve-count items is within budget")
  (check-false (policy-within-budget? p (list 1 2 3 4))
               "max+1 results exceed the budget")
  ;; Non-policy input must be rejected even for an empty list (kills and->or).
  (check-false (policy-within-budget? "not-a-policy" '()))
  (check-false (policy-within-budget? #f '(1))))

;; ---------------------------------------------------------------------------
;; Boundary survivors — redacted-memory-snippet (lines 163/165)
;; Note: the implementation truncates via (substring trimmed 0 (- max-len 3)),
;; so callers must pass max-len >= 3; boundary assertions below stay in that
;; domain (the pre-existing crash below 3 is out of scope for a test-only wave).
;; ---------------------------------------------------------------------------

(test-case "W5 mutation 163: snippet boundary is inclusive at max-len (<= -> <)"
  (check-equal? (redacted-memory-snippet "abcdef" 6) "abcdef"
                "input of exactly max-len chars is returned untruncated")
  (check-equal? (redacted-memory-snippet "abcdefg" 6) "abc..."
                "input one char beyond max-len is truncated")
  (check-equal? (redacted-memory-snippet "abcdefgh" 8) "abcdefgh")
  ;; Default limit of 80.
  (check-equal? (string-length (redacted-memory-snippet (make-string 80 #\a))) 80)
  (check-equal? (redacted-memory-snippet (make-string 81 #\a))
                (string-append (make-string 77 #\a) "...")))

(test-case "W5 mutations 165: truncation offsets keep the ellipsis budget (0 -> 1, - -> +)"
  ;; (- max-len 3) chars of prefix + "..."; the prefix starts at offset 0.
  (check-equal? (redacted-memory-snippet "1234567890" 5) "12...")
  (check-equal? (redacted-memory-snippet "abcdefghij" 10) "abcdefghij")
  (define long (make-string 40 #\z))
  (check-equal? (redacted-memory-snippet long 20)
                (string-append (substring long 0 17) "...")))

(test-case "W5: snippet redacts before truncating"
  ;; The whole redacted value fits, so the redaction itself is observable.
  (check-equal? (redacted-memory-snippet "password=hunter2 and more text" 30)
                "[REDACTED] and more text")
  ;; Redacted material adjacent to the elision stays collapsed.
  (check-equal? (redacted-memory-snippet "password=hunter2" 30) "[REDACTED]"))
