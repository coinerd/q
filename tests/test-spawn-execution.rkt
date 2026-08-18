#lang racket/base

;; @speed fast
;; @suite default
;; @boundary unit

;; Tests for spawn-execution — child-safe tool schemas, CWD propagation, agent loop.

(require rackunit
         racket/match
         "../tools/builtins/spawn-execution.rkt"
         "../tools/tool.rkt"
         "../util/capability.rkt")

(define child-tools (child-safe-tools))

(test-case "child-safe edit tool schema matches canonical schema"
  (define edit-tool (findf (lambda (t) (equal? (tool-name t) "edit")) child-tools))
  (check-not-false edit-tool "edit tool must be in child-safe-tools")
  (check-equal? (tool-required-capability edit-tool) 'file-write)
  ;; Schema must require path + old-text + new-text, NOT path + edits (array)
  ;; This matches core-tools.rkt's canonical edit schema
  (define schema (tool-schema edit-tool))
  (check-true (hash? schema) "schema should be a hash")
  (define required (hash-ref schema 'required #f))
  (check-not-false (member "path" required) "'path' must be required")
  (check-not-false (member "old-text" required) "'old-text' must be required")
  (check-not-false (member "new-text" required) "'new-text' must be required")
  ;; Ensure 'edits' is NOT in required
  (check-false (member "edits" required) "'edits' must NOT be in required")
  (define properties (hash-ref schema 'properties #f))
  (check-true (hash? properties) "schema must have properties")
  (check-true (hash-has-key? properties 'old-text) "must have old-text property")
  (check-true (hash-has-key? properties 'new-text) "must have new-text property")
  (check-true (hash-has-key? properties 'fuzzy?) "must have fuzzy? property")
  (check-false (hash-has-key? properties 'edits) "must NOT have edits property"))

(test-case "all child-safe tools have valid capabilities"
  (for ([t (in-list child-tools)])
    (check-true
     (valid-capability? (tool-required-capability t))
     (format "tool ~a has invalid capability: ~a" (tool-name t) (tool-required-capability t)))))

(test-case "child-safe tool list is non-empty"
  (check-true (pair? child-tools) "child-safe-tools should return a non-empty list"))
