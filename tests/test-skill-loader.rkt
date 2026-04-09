#lang racket

(require rackunit
         "../skills/types.rkt")

;; ============================================================
;; render-template
;; ============================================================

(test-case "render-template replaces {{var}} placeholders"
  (define result (render-template "Hello {{name}}!" (hash 'name "World")))
  (check-equal? result "Hello World!"))

(test-case "render-template handles multiple variables"
  (define result (render-template "{{greeting}} {{name}}"
                                  (hash 'greeting "Hi" 'name "Alice")))
  (check-equal? result "Hi Alice"))

(test-case "render-template leaves missing vars unchanged"
  (define result (render-template "Hello {{unknown}}!" (hash)))
  (check-equal? result "Hello {{unknown}}!"))

(test-case "render-template handles string keys"
  (define result (render-template "val={{x}}" (hash "x" "42")))
  (check-equal? result "val=42"))

(test-case "render-template with no placeholders returns original"
  (define result (render-template "no placeholders" (hash 'a 1)))
  (check-equal? result "no placeholders"))

;; ============================================================
;; Resource structs
;; ============================================================

(test-case "resource struct fields"
  (define r (resource 'skill "my-skill" 'global "content here"))
  (check-true (resource? r))
  (check-eq? (resource-kind r) 'skill)
  (check-equal? (resource-name r) "my-skill")
  (check-eq? (resource-source r) 'global))

(test-case "empty-resource-set has empty collections"
  (define rs (empty-resource-set))
  (check-true (resource-set? rs))
  (check-equal? (resource-set-instructions rs) '())
  (check-equal? (resource-set-skills rs) '())
  (check-equal? (resource-set-templates rs) (hash))
  (check-equal? (resource-set-config rs) (hash)))
