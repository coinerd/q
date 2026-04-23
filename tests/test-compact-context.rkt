#lang racket/base

;; tests/test-compact-context.rkt — Tests for compact-context extension

(require rackunit
         racket/file
         racket/path
         racket/string
         "../extensions/compact-context.rkt"
         "../tools/tool.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-temp-planning-dir)
  (define dir (make-temporary-file "compact-test-~a" 'directory))
  (define planning-dir (build-path dir ".planning"))
  (make-directory planning-dir)
  dir)

(define (cleanup-dir dir)
  (when (directory-exists? dir)
    (delete-directory/files dir)))

;; ============================================================
;; gather-planning-summary tests
;; ============================================================

(test-case "gather-planning-summary returns message when no .planning dir"
  (define dir (make-temporary-file "compact-test-~a" 'directory))
  (check-equal? (gather-planning-summary dir) "No .planning/ directory found.")
  (cleanup-dir dir))

(test-case "gather-planning-summary reads PLAN.md"
  (define dir (make-temp-planning-dir))
  (define plan-path (build-path dir ".planning" "PLAN.md"))
  (call-with-output-file plan-path
    (lambda (out) (display "## Wave 1: Do stuff\n- [ ] Task A\n" out)))
  (define result (gather-planning-summary dir))
  (check-true (string-contains? result "PLAN.md"))
  (check-true (string-contains? result "Wave 1"))
  (cleanup-dir dir))

(test-case "gather-planning-summary reads multiple artifacts"
  (define dir (make-temp-planning-dir))
  (call-with-output-file (build-path dir ".planning" "PLAN.md")
    (lambda (out) (display "Plan content\n" out)))
  (call-with-output-file (build-path dir ".planning" "STATE.md")
    (lambda (out) (display "State content\n" out)))
  (call-with-output-file (build-path dir ".planning" "HANDOFF.json")
    (lambda (out) (display "{\"phase\":\"B\"}\n" out)))
  (define result (gather-planning-summary dir))
  (check-true (string-contains? result "PLAN.md"))
  (check-true (string-contains? result "STATE.md"))
  (check-true (string-contains? result "HANDOFF.json"))
  (cleanup-dir dir))

(test-case "gather-planning-summary returns no artifacts message for empty .planning"
  (define dir (make-temp-planning-dir))
  (define result (gather-planning-summary dir))
  (check-equal? result "No planning artifacts found.")
  (cleanup-dir dir))

;; ============================================================
;; handle-compact-context tests
;; ============================================================

(test-case "handle-compact-context returns success result"
  (define result (handle-compact-context (hasheq)))
  (check-pred tool-result? result)
  (check-false (tool-result-is-error? result)))

(test-case "handle-compact-context includes compaction-trigger type"
  (define result (handle-compact-context (hasheq 'reason "test")))
  (define content (tool-result-content result))
  (define types (map (lambda (c) (hash-ref c 'type #f)) content))
  (check-not-false (member "compaction-trigger" types))
  (check-not-false (member "text" types)))

(test-case "handle-compact-context includes reason in trigger"
  (define result (handle-compact-context (hasheq 'reason "context pressure")))
  (define content (tool-result-content result))
  (define trigger (findf (lambda (c) (equal? (hash-ref c 'type #f) "compaction-trigger")) content))
  (check-equal? (hash-ref trigger 'reason) "context pressure"))

(test-case "handle-compact-context reads planning state"
  (define dir (make-temp-planning-dir))
  (call-with-output-file (build-path dir ".planning" "PLAN.md")
    (lambda (out) (display "My plan\n" out)))
  (define result (handle-compact-context (hasheq 'project_dir (path->string dir))))
  (define content (tool-result-content result))
  (define text-item (findf (lambda (c) (equal? (hash-ref c 'type #f) "text")) content))
  (check-true (string-contains? (hash-ref text-item 'text) "My plan"))
  (cleanup-dir dir))

(test-case "handle-compact-context includes instructions"
  (define result (handle-compact-context
                  (hasheq 'instructions "Focus on Phase B next")))
  (define content (tool-result-content result))
  (define trigger (findf (lambda (c) (equal? (hash-ref c 'type #f) "compaction-trigger")) content))
  (check-equal? (hash-ref trigger 'instructions) "Focus on Phase B next"))

(test-case "handle-compact-context text result mentions compaction"
  (define result (handle-compact-context (hasheq)))
  (define content (tool-result-content result))
  (define text-item (findf (lambda (c) (equal? (hash-ref c 'type #f) "text")) content))
  (check-true (string-contains? (hash-ref text-item 'text) "Compaction triggered")))
