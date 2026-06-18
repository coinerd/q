#lang racket

;; @speed fast
;; @suite skills

;; tests/test-skill-resource-loader-frontmatter.rkt
;; v0.99.28 W1: Frontmatter-aware skill descriptions (V27-B2)

(require rackunit
         rackunit/text-ui
         racket/string
         "../skills/resource-loader.rkt")

(define suite
  (test-suite "Skill Resource Loader — Frontmatter Stripping (v0.99.28 W1)"

    ;; ── strip-leading-frontmatter-lines ──

    (test-case "strips valid frontmatter block"
      (define lines '("---" "name: my-skill" "description: A skill" "---" "# My Skill" "" "Body"))
      (define result (strip-leading-frontmatter-lines lines))
      (check-equal? result '("# My Skill" "" "Body")))

    (test-case "no frontmatter — returns unchanged"
      (define lines '("# My Skill" "" "Body text"))
      (define result (strip-leading-frontmatter-lines lines))
      (check-equal? result lines))

    (test-case "opening --- but no closing --- — preserves original (conservative)"
      (define lines '("---" "name: my-skill" "description: broken" "# Title"))
      (define result (strip-leading-frontmatter-lines lines))
      (check-equal? result lines))

    (test-case "empty list — returns empty"
      (define result (strip-leading-frontmatter-lines '()))
      (check-true (null? result) "empty list returns empty"))

    (test-case "strips frontmatter with leading/trailing whitespace on markers"
      (define lines '("  ---  " "name: test" "  ---  " "# Title"))
      (define result (strip-leading-frontmatter-lines lines))
      (check-equal? result '("# Title")))

    ;; ── parse-skill with frontmatter ──

    (test-case "parse-skill strips frontmatter from description"
      (define raw-content
        (string-append "---\n"
                       "name: code-review\n"
                       "description: Reviews code for quality\n"
                       "---\n"
                       "# Code Review\n\n"
                       "Reviews code and provides feedback.\n\n"
                       "## Usage\n\n"
                       "Use this skill when reviewing pull requests."))
      (define result (parse-skill "code-review" raw-content))
      (check-equal? (hash-ref result 'name) "code-review")
      (define desc (hash-ref result 'description))
      (check-false (string-contains? desc "---") "description should not contain frontmatter markers")
      (check-false (string-contains? desc "name:") "description should not contain YAML keys")
      (check-false (string-contains? desc "description:") "description should not contain YAML keys")
      (check-true (string-contains? desc "Reviews code")
                  "description should contain actual description text"))

    (test-case "parse-skill without frontmatter — unchanged behavior"
      (define raw-content "# My Skill\n\nShort description.\n\nFull content here.")
      (define result (parse-skill "my-skill" raw-content))
      (check-equal? (hash-ref result 'name) "my-skill")
      (check-equal? (hash-ref result 'description) "Short description.")
      (check-equal? (hash-ref result 'content) "Full content here."))

    (test-case "parse-skill with complex frontmatter"
      (define raw-content
        (string-append "---\n"
                       "name: complex-skill\n"
                       "description: Complex skill\n"
                       "capabilities:\n"
                       "  - read-only\n"
                       "  - plan-write\n"
                       "---\n"
                       "# Complex Skill\n\n"
                       "Performs complex analysis."))
      (define result (parse-skill "complex-skill" raw-content))
      (define desc (hash-ref result 'description))
      (check-false (string-contains? desc "---") "no frontmatter in desc")
      (check-false (string-contains? desc "capabilities:") "no YAML in desc")
      (check-true (string-contains? desc "Performs complex analysis")
                  "description should have actual text"))

    (test-case "parse-skill with no closing --- — frontmatter treated as content"
      ;; Conservative: malformed frontmatter is preserved
      (define raw-content
        (string-append "---\n"
                       "name: broken-skill\n"
                       "description: No closing marker\n"
                       "# Title\n\n"
                       "Content"))
      (define result (parse-skill "broken-skill" raw-content))
      ;; Since frontmatter is malformed (no closing), it stays as content
      ;; The behavior should match pre-fix: first # line becomes header
      (check-equal? (hash-ref result 'name) "broken-skill"))

    (test-case "parse-skill: empty frontmatter block"
      (define raw-content "---\n---\n# Title\n\nDescription here.")
      (define result (parse-skill "test" raw-content))
      (check-equal? (hash-ref result 'description) "Description here."))

    (test-case "parse-skill description clean for workflow skills"
      ;; Verify description is clean for typical workflow skill
      (define raw-content "---\nname: wf\ndescription: A workflow\n---\n# WF\n\nDesc.\n\n## Content")
      (define result (parse-skill "wf" raw-content))
      (check-false (string-contains? (hash-ref result 'description) "---")))))

(run-tests suite)
