#lang racket

;; tests/test-progressive-skills.rkt — Progressive Skill Disclosure tests
;;
;; Tests for two-tier skill loading: summaries in system prompt,
;; full content available on-demand.

(require rackunit
         "../skills/resource-loader.rkt")

;; ============================================================
;; parse-skill produces name + description + content
;; ============================================================

(test-case "parse-skill extracts name, description, content"
  (define raw "# My Skill\n\nThis is the description.\n\nFull content here.\nMore lines.")
  (define skill (parse-skill "my-skill" raw))
  (check-equal? (hash-ref skill 'name) "my-skill")
  (check-equal? (hash-ref skill 'description) "This is the description.")
  (check-equal? (hash-ref skill 'content) "Full content here.\nMore lines."))

(test-case "parse-skill handles header-only skill"
  (define raw "# Minimal")
  (define skill (parse-skill "minimal" raw))
  (check-equal? (hash-ref skill 'name) "minimal")
  (check-equal? (hash-ref skill 'description) "")
  (check-equal? (hash-ref skill 'content) ""))

(test-case "parse-skill handles description-only (no body)"
  (define raw "# Skill Name\n\nJust a description, no body content.")
  (define skill (parse-skill "desc-only" raw))
  (check-equal? (hash-ref skill 'name) "desc-only")
  (check-equal? (hash-ref skill 'description) "Just a description, no body content.")
  (check-equal? (hash-ref skill 'content) ""))

;; ============================================================
;; skill-summary-text — compact summary for system prompt
;; ============================================================

(test-case "skill-summary-text formats single skill"
  (define skills (list (hasheq 'name "reviewer"
                               'description "Read-only code review"
                               'content "Full content ignored")))
  (define summary (skill-summary-text skills))
  (check-true (string-contains? summary "reviewer"))
  (check-true (string-contains? summary "Read-only code review"))
  (check-false (string-contains? summary "Full content ignored")))

(test-case "skill-summary-text formats multiple skills"
  (define skills (list (hasheq 'name "builder" 'description "Build things" 'content "x")
                       (hasheq 'name "reviewer" 'description "Review things" 'content "y")))
  (define summary (skill-summary-text skills))
  (check-true (string-contains? summary "builder"))
  (check-true (string-contains? summary "reviewer"))
  (check-true (string-contains? summary "Build things"))
  (check-true (string-contains? summary "Review things")))

(test-case "skill-summary-text handles empty skill list"
  (define summary (skill-summary-text '()))
  (check-equal? summary ""))

(test-case "skill-summary-text handles skill with empty description"
  (define skills (list (hasheq 'name "bare" 'description "" 'content "full")))
  (define summary (skill-summary-text skills))
  (check-true (string-contains? summary "bare")))

;; ============================================================
;; skills-summary-section — full section for system prompt
;; ============================================================

(test-case "skills-summary-section produces header + summaries"
  (define skills (list (hasheq 'name "builder" 'description "Build" 'content "x")
                       (hasheq 'name "reviewer" 'description "Review" 'content "y")))
  (define section (skills-summary-section skills))
  (check-true (string-contains? section "Available Skills"))
  (check-true (string-contains? section "builder"))
  (check-true (string-contains? section "reviewer"))
  ;; Must NOT contain full content
  (check-false (regexp-match? #rx"On-demand" section)))

(test-case "skills-summary-section returns #f for empty skills"
  (check-false (skills-summary-section '())))
