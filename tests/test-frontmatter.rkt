#lang racket

;; tests/test-frontmatter.rkt — T01: Skills module tests
;;
;; Tests for skills/frontmatter.rkt — parse-skill-frontmatter,
;; valid-skill-name?, and validate-frontmatter.

(require rackunit
         "../skills/frontmatter.rkt")

;; ============================================================
;; valid-skill-name?
;; ============================================================

(test-case "valid-skill-name? accepts valid names"
  (check-true (valid-skill-name? "my-skill"))
  (check-true (valid-skill-name? "a"))
  (check-true (valid-skill-name? "skill123"))
  (check-true (valid-skill-name? "a-b-c")))

(test-case "valid-skill-name? rejects invalid names"
  (check-false (valid-skill-name? ""))
  (check-false (valid-skill-name? "-starts-dash"))
  (check-false (valid-skill-name? "ends-dash-"))
  (check-false (valid-skill-name? "UPPERCASE"))
  (check-false (valid-skill-name? "has space"))
  (check-false (valid-skill-name? (make-string 65 #\a))))  ; too long

(test-case "valid-skill-name? rejects non-strings"
  (check-false (valid-skill-name? 123))
  (check-false (valid-skill-name? 'symbol))
  (check-false (valid-skill-name? #f)))

;; ============================================================
;; parse-skill-frontmatter
;; ============================================================

(test-case "parse-skill-frontmatter parses valid frontmatter"
  (define content "---\nname: my-skill\ndescription: A test skill\n---\nBody here\n")
  (define fm (parse-skill-frontmatter content))
  (check-not-false fm)
  (check-equal? (hash-ref fm 'name) "my-skill")
  (check-equal? (hash-ref fm 'description) "A test skill"))

(test-case "parse-skill-frontmatter returns #f for no frontmatter"
  (check-false (parse-skill-frontmatter "Just content\nNo frontmatter\n"))
  (check-false (parse-skill-frontmatter ""))
  (check-false (parse-skill-frontmatter "---\nname: test\n")))  ; no closing ---

(test-case "parse-skill-frontmatter handles quoted values"
  (define content "---\nname: \"quoted name\"\n---\n")
  (define fm (parse-skill-frontmatter content))
  (check-not-false fm)
  (check-equal? (hash-ref fm 'name) "quoted name"))

;; ============================================================
;; validate-frontmatter
;; ============================================================

(test-case "validate-frontmatter returns ok for valid frontmatter"
  (define fm (make-hash '((name . "test-skill") (description . "A test"))))
  (define-values (status msg) (validate-frontmatter fm "test-skill"))
  (check-equal? status 'ok))

(test-case "validate-frontmatter returns error for missing name"
  (define fm (make-hash '((description . "A test"))))
  (define-values (status msg) (validate-frontmatter fm "test-skill"))
  (check-equal? status 'error)
  (check-true (string-contains? msg "name")))

(test-case "validate-frontmatter returns error for missing description"
  (define fm (make-hash '((name . "test-skill"))))
  (define-values (status msg) (validate-frontmatter fm "test-skill"))
  (check-equal? status 'error)
  (check-true (string-contains? msg "description")))

(test-case "validate-frontmatter returns warning for name/directory mismatch"
  (define fm (make-hash '((name . "other-name") (description . "A test"))))
  (define-values (status msg) (validate-frontmatter fm "test-skill"))
  (check-equal? status 'warning))
