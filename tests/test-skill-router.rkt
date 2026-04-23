#lang racket

;; tests/test-skill-router.rkt — tests for skill-route tool
;;
;; Covers:
;;   - list: discover all skills
;;   - match: search by query text
;;   - load: load full skill content by name
;;   - Error cases: missing query, missing name, unknown action

(require rackunit
         racket/file
         racket/string
         json
         "../tools/builtins/skill-router.rkt"
         "../tools/tool.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-temp-skill-dir skills-alist)
  ;; skills-alist: ((name . content) ...)
  (define dir (make-temporary-file "skill-test-~a" 'directory))
  (define skills-dir (build-path dir ".q" "skills"))
  (make-directory* skills-dir)
  (for ([pair (in-list skills-alist)])
    (define skill-name (car pair))
    (define content (cdr pair))
    (define skill-dir (build-path skills-dir skill-name))
    (make-directory* skill-dir)
    (call-with-output-file (build-path skill-dir "SKILL.md")
                           (lambda (out)
                             (displayln (format "# ~a" skill-name) out)
                             (displayln "" out)
                             (displayln content out))))
  dir)

(define (cleanup-dir dir)
  (when (directory-exists? dir)
    (delete-directory/files dir)))

;; ============================================================
;; Test: list action returns all skills
;; ============================================================

(test-case "skill-route list returns all skills"
  (define dir
    (make-temp-skill-dir '(("test-skill" . "A test skill for routing.\n\nFull content here.")
                           ("another-skill" . "Another skill.\n\nMore content."))))
  (parameterize ([current-directory dir])
    (define result (tool-skill-route (hasheq 'action "list")))
    (check-pred tool-result? result)
    (check-false (tool-result-is-error? result))
    (define text (hash-ref (car (tool-result-content result)) 'text ""))
    (define parsed (string->jsexpr text))
    (check-equal? (length parsed) 2)
    (check-not-false (findf (lambda (s) (string=? (hash-ref s 'name "") "test-skill")) parsed))
    (check-not-false (findf (lambda (s) (string=? (hash-ref s 'name "") "another-skill")) parsed)))
  (cleanup-dir dir))

;; ============================================================
;; Test: match action finds skills by description
;; ============================================================

(test-case "skill-route match finds by description"
  (define dir
    (make-temp-skill-dir '(("bug-fixer" . "Fix bugs in the codebase.\n\nInstructions.")
                           ("docs-writer" . "Write documentation for the project.\n\nInstructions.")
                           ("code-reviewer" . "Review code changes.\n\nInstructions."))))
  (parameterize ([current-directory dir])
    (define result (tool-skill-route (hasheq 'action "match" 'query "bug")))
    (check-pred tool-result? result)
    (check-false (tool-result-is-error? result))
    (define text (hash-ref (car (tool-result-content result)) 'text ""))
    (check-true (string-contains? text "bug-fixer"))
    (check-false (string-contains? text "docs-writer")))
  (cleanup-dir dir))

;; ============================================================
;; Test: match action finds by name
;; ============================================================

(test-case "skill-route match finds by name"
  (define dir
    (make-temp-skill-dir '(("q-gsd-orchestrator" . "Top level router.\n\nContent.")
                           ("q-gsd-reviewer" . "Review code.\n\nContent."))))
  (parameterize ([current-directory dir])
    (define result (tool-skill-route (hasheq 'action "match" 'query "reviewer")))
    (check-pred tool-result? result)
    (check-false (tool-result-is-error? result))
    (define text (hash-ref (car (tool-result-content result)) 'text ""))
    (check-true (string-contains? text "reviewer")))
  (cleanup-dir dir))

;; ============================================================
;; Test: match without query returns error
;; ============================================================

(test-case "skill-route match without query returns error"
  (define result (tool-skill-route (hasheq 'action "match")))
  (check-pred tool-result? result)
  (check-true (tool-result-is-error? result)))

;; ============================================================
;; Test: load action returns full skill content
;; ============================================================

(test-case "skill-route load returns full content"
  (define dir
    (make-temp-skill-dir
     '(("my-skill" . "Short description.\n\nDetailed skill content here.\nMultiple lines."))))
  (parameterize ([current-directory dir])
    (define result (tool-skill-route (hasheq 'action "load" 'name "my-skill")))
    (check-pred tool-result? result)
    (check-false (tool-result-is-error? result))
    (define text (hash-ref (car (tool-result-content result)) 'text ""))
    (check-true (string-contains? text "Detailed skill content")))
  (cleanup-dir dir))

;; ============================================================
;; Test: load non-existent skill returns error
;; ============================================================

(test-case "skill-route load non-existent skill returns error"
  (define dir (make-temp-skill-dir '(("exists" . "A skill.\n\nContent."))))
  (parameterize ([current-directory dir])
    (define result (tool-skill-route (hasheq 'action "load" 'name "nonexistent")))
    (check-pred tool-result? result)
    (check-true (tool-result-is-error? result)))
  (cleanup-dir dir))

;; ============================================================
;; Test: load without name returns error
;; ============================================================

(test-case "skill-route load without name returns error"
  (define result (tool-skill-route (hasheq 'action "load")))
  (check-pred tool-result? result)
  (check-true (tool-result-is-error? result)))

;; ============================================================
;; Test: unknown action returns error
;; ============================================================

(test-case "skill-route unknown action returns error"
  (define result (tool-skill-route (hasheq 'action "nonexistent")))
  (check-pred tool-result? result)
  (check-true (tool-result-is-error? result)))

;; ============================================================
;; Test: list with no skills returns empty array
;; ============================================================

(test-case "skill-route list with no skills returns empty"
  (define dir (make-temporary-file "skill-test-~a" 'directory))
  (parameterize ([current-directory dir])
    (define result (tool-skill-route (hasheq 'action "list")))
    (check-pred tool-result? result)
    (check-false (tool-result-is-error? result))
    (define text (hash-ref (car (tool-result-content result)) 'text ""))
    (check-equal? (string->jsexpr text) '()))
  (cleanup-dir dir))

;; ============================================================
;; Test: match is case-insensitive
;; ============================================================

(test-case "skill-route match is case-insensitive"
  (define dir (make-temp-skill-dir '(("Bug-Fixer" . "Fix bugs.\n\nContent."))))
  (parameterize ([current-directory dir])
    (define result (tool-skill-route (hasheq 'action "match" 'query "BUG")))
    (check-pred tool-result? result)
    (check-false (tool-result-is-error? result))
    (define text (hash-ref (car (tool-result-content result)) 'text ""))
    (check-true (string-contains? text "Bug-Fixer")))
  (cleanup-dir dir))

;; ============================================================
;; Test: list default action
;; ============================================================

(test-case "skill-route defaults to list"
  (define dir (make-temp-skill-dir '(("default-test" . "A skill.\n\nContent."))))
  (parameterize ([current-directory dir])
    (define result (tool-skill-route (hasheq)))
    (check-pred tool-result? result)
    (check-false (tool-result-is-error? result))
    (define text (hash-ref (car (tool-result-content result)) 'text ""))
    (check-true (string-contains? text "default-test")))
  (cleanup-dir dir))

;; ============================================================
;; Test: recommend returns top-3 with confidence
;; ============================================================

(test-case "skill-route recommend returns top skills with confidence"
  (define dir
    (make-temp-skill-dir '(("bug-fixer" . "Fix bugs in the codebase.")
                           ("bug-analyzer" . "Analyze bugs and find root causes.")
                           ("docs-writer" . "Write documentation.")
                           ("code-reviewer" . "Review code changes."))))
  (parameterize ([current-directory dir])
    (define result (tool-skill-route (hasheq 'action "recommend" 'query "bug fix")))
    (check-pred tool-result? result)
    (check-false (tool-result-is-error? result))
    (define text (hash-ref (car (tool-result-content result)) 'text ""))
    (check-true (string-contains? text "bug-fixer"))
    (check-true (string-contains? text "bug-analyzer"))
    (check-true (string-contains? text "confidence")))
  (cleanup-dir dir))

(test-case "skill-route recommend without query returns error"
  (define result (tool-skill-route (hasheq 'action "recommend")))
  (check-pred tool-result? result)
  (check-true (tool-result-is-error? result)))

;; ============================================================
;; Test: context returns skill content with shared deps
;; ============================================================

(test-case "skill-route context returns skill content"
  (define dir (make-temp-skill-dir '(("my-skill" . "Skill description.\n\nDetailed instructions."))))
  (parameterize ([current-directory dir])
    (define result (tool-skill-route (hasheq 'action "context" 'name "my-skill")))
    (check-pred tool-result? result)
    (check-false (tool-result-is-error? result))
    (define text (hash-ref (car (tool-result-content result)) 'text ""))
    (check-true (string-contains? text "Detailed instructions")))
  (cleanup-dir dir))

(test-case "skill-route context with _shared dependencies"
  (define dir (make-temporary-file "skill-test-~a" 'directory))
  (define skills-dir (build-path dir ".q" "skills"))
  (define skill-dir (build-path skills-dir "shared-skill"))
  (define shared-dir (build-path skill-dir "_shared"))
  (make-directory* shared-dir)
  (call-with-output-file (build-path skill-dir "SKILL.md")
                         (lambda (out)
                           (displayln "# shared-skill" out)
                           (displayln "" out)
                           (displayln "A skill with shared deps." out)))
  (call-with-output-file (build-path shared-dir "PLAYBOOK.md")
                         (lambda (out)
                           (displayln "# Playbook" out)
                           (displayln "Common debugging steps." out)))
  (parameterize ([current-directory dir])
    (define result (tool-skill-route (hasheq 'action "context" 'name "shared-skill")))
    (check-pred tool-result? result)
    (check-false (tool-result-is-error? result))
    (define text (hash-ref (car (tool-result-content result)) 'text ""))
    (check-true (string-contains? text "A skill with shared deps"))
    (check-true (string-contains? text "Playbook"))
    (check-true (string-contains? text "Common debugging steps")))
  (cleanup-dir dir))

(test-case "skill-route context without name returns error"
  (define result (tool-skill-route (hasheq 'action "context")))
  (check-pred tool-result? result)
  (check-true (tool-result-is-error? result)))

(test-case "skill-route context non-existent skill returns error"
  (define dir (make-temp-skill-dir '(("exists" . "A skill."))))
  (parameterize ([current-directory dir])
    (define result (tool-skill-route (hasheq 'action "context" 'name "nonexistent")))
    (check-pred tool-result? result)
    (check-true (tool-result-is-error? result)))
  (cleanup-dir dir))
