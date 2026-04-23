#lang racket

;; tests/test-spawn-subagent-role-loading.rkt
;; v0.18.3 Wave 3: Spawn-subagent role loading tests
;;
;; Tests:
;;   - resolve-role-prompt with literal string
;;   - resolve-role-prompt with skill name
;;   - resolve-role-prompt with non-existent skill falls back
;;   - resolve-role-prompt with empty string uses default
;;   - Tool invocation with role parameter

(require rackunit
         racket/file
         racket/string
         json
         "../tools/builtins/spawn-subagent.rkt"
         "../tools/builtins/skill-router.rkt"
         "../tools/tool.rkt")

;; ── Helpers ──

(define (make-temp-skill-dir skills-alist)
  (define dir (make-temporary-file "role-test-~a" 'directory))
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

;; ── Tests ──

(test-case "resolve-role-prompt returns literal string when no skill matches"
  (define result (resolve-role-prompt "You are a helpful assistant."))
  (check-equal? result "You are a helpful assistant."))

(test-case "resolve-role-prompt loads skill content when name matches"
  (define dir
    (make-temp-skill-dir
     '(("test-role" . "Test role description.\n\nYou must review code carefully."))))
  (parameterize ([current-directory dir])
    (define result (resolve-role-prompt "test-role"))
    (check-true (string-contains? result "review code carefully")
                (format "Should contain skill content, got: ~a" result)))
  (cleanup-dir dir))

(test-case "resolve-role-prompt falls back for non-existent skill"
  (define dir (make-temporary-file "role-test-~a" 'directory))
  (parameterize ([current-directory dir])
    ;; A literal prompt that doesn't match any skill
    (define result (resolve-role-prompt "Just a plain prompt"))
    (check-equal? result "Just a plain prompt"))
  (cleanup-dir dir))

(test-case "resolve-role-prompt uses default for empty string"
  (define result (resolve-role-prompt ""))
  (check-true (non-empty-string? result) "Default role prompt should not be empty"))

(test-case "resolve-role-prompt resolves skill with _shared content"
  (define dir (make-temporary-file "role-test-~a" 'directory))
  (define skills-dir (build-path dir ".q" "skills"))
  (define skill-dir (build-path skills-dir "reviewer"))
  (define shared-dir (build-path skill-dir "_shared"))
  (make-directory* shared-dir)
  (call-with-output-file (build-path skill-dir "SKILL.md")
                         (lambda (out)
                           (displayln "# reviewer" out)
                           (displayln "" out)
                           (displayln "A code reviewer skill.\n\nReview code for quality." out)))
  (call-with-output-file (build-path shared-dir "CHECKLIST.md")
                         (lambda (out)
                           (displayln "# Review Checklist" out)
                           (displayln "- Check test coverage" out)))
  (parameterize ([current-directory dir])
    (define result (resolve-role-prompt "reviewer"))
    (check-true (string-contains? result "code reviewer"))
    (check-true (string-contains? result "Review Checklist") "Should include _shared content"))
  (cleanup-dir dir))

(test-case "tool-spawn-subagent requires task"
  (define result (tool-spawn-subagent (hasheq)))
  (check-true (tool-result? result))
  (check-true (tool-result-is-error? result)))
