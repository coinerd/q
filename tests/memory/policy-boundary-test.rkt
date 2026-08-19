#lang racket/base

;; @covers runtime/memory/policy.rkt

;; @speed fast
;; @suite default
;; @boundary unit

;; Mutation-survivor tests for runtime/memory/policy.rkt (W5, Assessment
;; Action 6). Added per docs/operations/test-consolidation-policy.md: the
;; bounded mutation pilot (run-5) found 4/18 mutants killed for this module;
;; the surviving boundary and boolean-policy mutants are killed here.
;;
;; Before/after (pilot scope runtime/memory/policy.rkt, budget 900s):
;;   before (artifacts/mutation-pilot/run-5): killed 4/18, survived 14
;;   after  (artifacts/mutation-pilot/run-6): killed 16/18, survived 2
;;     detection score 0.222 -> 0.889; run-6 invocation:
;;     racket scripts/run-tests/mutation-pilot.rkt \
;;       --modules runtime/memory/policy.rkt \
;;       --tests-for runtime/memory/policy.rkt=tests/memory/policy-boundary-test.rkt,tests/test-memory-policy.rkt \
;;       --budget 900 --out artifacts/mutation-pilot/run-6
;;   re-verify (artifacts/mutation-pilot/run-8, stale .zo caches purged):
;;     killed 16/18, survived 2 — same two equivalent mutants. (run-7 without
;;     the cache purge spuriously survived line 146 boolean-and->or; killed
;;     again in run-6 and run-8, i.e. a pilot bytecode-cache artifact, not a
;;     test gap.)
;;
;; The two remaining survivors are documented EQUIVALENT mutants (no test can
;; kill them; they are not evidence of a suite gap):
;;   1. line 79 numeric-boundary-1 — the matched `1` is inside the comment
;;      `;; Multi-line .env holistic pattern (P3-1)`; the mutation changes no
;;      code, so behaviour is identical by construction.
;;   2. line 115 boolean-and->or — `(and requested-scope (memory-scope?
;;      requested-scope))` vs `(or ...)`: cond branches 1 and 2 have the SAME
;;      body (`requested-scope`); for truthy requested-scope both forms select
;;      a branch returning requested-scope, for #f both fall through, and
;;      memory-scope? is a pure total predicate (memq), so evaluation order
;;      is unobservable.
;;
;; Every assertion below names the mutant it kills in a side comment
;; (site line + operator). No existing test was modified or deleted.

(require rackunit
         "../../runtime/memory/policy.rkt"
         "../../runtime/memory/types.rkt")

;; Fully valid memory item (passes valid-memory-item?): metadata carries all
;; required keys incl. an origin id, validity carries sensitivity/confidence/
;; supersedes. The existing suite's helper omits supersedes/origin, so its
;; items are invalid — which is why store-policy boundary mutants survived.
(define (valid-item content #:sensitivity [sensitivity 'public] #:scope [scope 'project])
  (memory-item "id"
               'semantic
               scope
               content
               (hasheq 'tags
                       '()
                       'source
                       'tool
                       'project-root
                       "/tmp/project"
                       'session-id
                       "sess"
                       'origin-message-id
                       "m1")
               (hasheq 'sensitivity sensitivity 'confidence 1.0 'supersedes #f)
               "2026-01-01T00:00:00Z"
               "2026-01-01T00:00:00Z"))

;; ---------------------------------------------------------------------------
;; safe-memory-content? — boolean policy (line 146: and -> or)
;; ---------------------------------------------------------------------------

(test-case "safe-memory-content?: both polarities (kills and->or line 146)"
  ;; and-form: unsafe string => #f. or-form returns the truthy (string?
  ;; content) instead => #t. Asserting #f detects the substitution.
  (check-false (safe-memory-content? default-memory-policy "token=abc123"))
  (check-false (safe-memory-content? default-memory-policy "password=hunter2"))
  ;; Polarity complement: safe string => #t under both forms (not vacuous).
  (check-true (safe-memory-content? default-memory-policy "a plain note")))

;; ---------------------------------------------------------------------------
;; policy-check-content-safety — boolean policy (line 186: and -> or)
;; ---------------------------------------------------------------------------

(test-case "policy-check-content-safety: non-policy argument (kills and->or line 186)"
  ;; and-form: non-policy => #f without touching content. or-form returns
  ;; truthy (string? content) => #t.
  (check-false (policy-check-content-safety 5 "plain"))
  (check-false (policy-check-content-safety "not-a-policy" "plain"))
  ;; Polarity complements.
  (check-true (policy-check-content-safety default-memory-policy "plain"))
  (check-false (policy-check-content-safety default-memory-policy "token=abc123")))

;; ---------------------------------------------------------------------------
;; policy-allows-retrieve? — boundary + boolean (lines 193/195)
;; ---------------------------------------------------------------------------

(test-case "policy-allows-retrieve?: zero-count boundary (kills <=->< and 0->1 line 195)"
  (define p (make-memory-policy #:max-retrieve-count 3))
  (check-true (policy-allows-retrieve? p 0)) ; strict < and (<= 1 ..) both flip this
  (check-true (policy-allows-retrieve? p 3)) ; at-threshold upper boundary
  (check-false (policy-allows-retrieve? p 4)) ; just above threshold (polarity)
  (check-false (policy-allows-retrieve? p -1)) ; below lower bound (polarity)
  (check-false (policy-allows-retrieve? p 1.5)) ; integer? guard branch
  (check-false (policy-allows-retrieve? p 'three)))

(test-case "policy-allows-retrieve?: non-policy argument (kills and->or line 193)"
  ;; and-form: #f. or-form: truthy (integer? 3) => #t.
  (check-false (policy-allows-retrieve? 5 3))
  (check-false (policy-allows-retrieve? 'p 0)))

;; ---------------------------------------------------------------------------
;; policy-allows-delete? — boolean policy (line 199: and -> or)
;; ---------------------------------------------------------------------------

(test-case "policy-allows-delete?: both polarities (kills and->or line 199)"
  ;; and-form: valid policy + allow-delete? #f => #f. or-form: first operand
  ;; truthy => #t.
  (check-false (policy-allows-delete? (make-memory-policy #:allow-delete? #f)))
  (check-true (policy-allows-delete? (make-memory-policy #:allow-delete? #t)))
  ;; default policy allows delete.
  (check-true (policy-allows-delete? default-memory-policy))
  ;; non-policy argument: and-form => #f.
  (check-false (policy-allows-delete? "not-a-policy")))

;; ---------------------------------------------------------------------------
;; policy-within-budget? — boundary + boolean (line 203)
;; ---------------------------------------------------------------------------

(test-case "policy-within-budget?: at-threshold boundary (kills <=->< line 203)"
  (define p (make-memory-policy #:max-retrieve-count 3))
  (check-true (policy-within-budget? p '())) ; empty result set
  (check-true (policy-within-budget? p (list 1 2 3))) ; exactly max: <= vs <
  (check-false (policy-within-budget? p (list 1 2 3 4))) ; one above (polarity)
  ;; default policy boundary: exactly 20 items.
  (check-true (policy-within-budget? default-memory-policy (build-list 20 values)))
  (check-false (policy-within-budget? default-memory-policy (build-list 21 values))))

(test-case "policy-within-budget?: non-policy argument (kills and->or line 203)"
  ;; and-form: #f (even for empty list). or-form: truthy (<= 0 max) => #t.
  (check-false (policy-within-budget? 5 '()))
  (check-false (policy-within-budget? 'p (list 1 2))))

;; ---------------------------------------------------------------------------
;; policy-allows-store? — content-length boundary (line 180: <= -> <)
;; ---------------------------------------------------------------------------

(test-case "policy-allows-store?: content length at threshold (kills <=->< line 180)"
  (define p (make-memory-policy #:max-content-length 5))
  (check-true (policy-allows-store? p (valid-item "abcde"))) ; exactly max
  (check-true (policy-allows-store? p (valid-item "abcd"))) ; one below
  (check-false (policy-allows-store? p (valid-item "abcdef"))) ; one above (polarity)
  ;; Sanity: the at-threshold item is valid, so a #f there can only come from
  ;; the length comparison — this is what the previous suite never exercised.
  (check-true (valid-memory-item? (valid-item "abcde")))
  ;; default policy boundary: exactly 10000 chars.
  (define content-10k (make-string 10000 #\z))
  (check-true (policy-allows-store? default-memory-policy (valid-item content-10k)))
  (check-false (policy-allows-store? default-memory-policy
                                     (valid-item (string-append content-10k "z")))))

;; ---------------------------------------------------------------------------
;; redacted-memory-snippet — length boundary + truncation arithmetic (163/165)
;; ---------------------------------------------------------------------------

(test-case "redacted-memory-snippet: exactly max-len is returned whole (kills <=->< line 163)"
  ;; 10 chars, max-len 10: original returns the string unchanged; strict <
  ;; truncates to 7 chars + "..." instead.
  (check-equal? (redacted-memory-snippet "abcdefghij" 10) "abcdefghij")
  (check-equal? (string-length (redacted-memory-snippet "abcdefghij" 10)) 10))

(test-case "redacted-memory-snippet: truncation keeps first max-len-3 chars (kills 0->1 line 165)"
  ;; start index 0 vs 1: original prefix is "abcdefg", mutant drops the 'a'.
  (check-equal? (redacted-memory-snippet "abcdefghijklmnopqrst" 10) "abcdefg...")
  (check-equal? (string-ref (redacted-memory-snippet "abcdefghijklmnopqrst" 10) 0) #\a))

(test-case "redacted-memory-snippet: truncation arithmetic is max-len minus 3 (kills - -> + line 165)"
  ;; (- max-len 3) vs (+ max-len 3): result length is exactly max-len.
  (define s (redacted-memory-snippet "abcdefghijklmnopqrst" 10))
  (check-equal? (string-length s) 10)
  (check-equal? (substring s (- (string-length s) 3)) "...")
  ;; Longer input, non-default max-len: substring end stays within bounds for
  ;; the original, so the mutant's longer output (or out-of-range error) is
  ;; detected by the exact-length assertion.
  (check-equal? (redacted-memory-snippet "abcdefghijklmnopqrstuvwxyz" 12) "abcdefghi...")
  (check-equal? (string-length (redacted-memory-snippet "abcdefghijklmnopqrstuvwxyz" 12)) 12))

;; ---------------------------------------------------------------------------
;; effective-memory-scope — both polarities of the scope guards
;; (documented-equivalent and->or survivor at line 115 is NOT killable; these
;; assertions pin the observable behaviour on every side of the condition.)
;; ---------------------------------------------------------------------------

(test-case "effective-memory-scope: polarity of requested-scope branches"
  (check-equal? (effective-memory-scope 'user "/tmp/project") 'user) ; valid scope
  (check-equal? (effective-memory-scope 'bogus "/tmp/project") 'bogus) ; invalid but present
  (check-equal? (effective-memory-scope #f "/tmp/project") 'project) ; absent + root
  (check-equal? (effective-memory-scope #f #f) 'session)) ; absent, no root
