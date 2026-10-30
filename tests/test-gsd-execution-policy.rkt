#lang racket
;; @covers extensions/gsd-planning/execution-policy.rkt

;; @speed fast  ;; @suite extensions
;; @boundary unit

;; BOUNDARY: integration

;; tests/test-gsd-execution-policy.rkt — tests for tool blocking and mode-based guards

(require rackunit
         rackunit/text-ui
         racket/file
         "../extensions/gsd/plan-types.rkt"
         "../extensions/gsd/state-machine.rkt"
         (only-in "../extensions/gsd/session-state.rkt" current-gsd-ctx make-gsd-context)
         (only-in "../extensions/hooks.rkt" hook-result-action hook-result-payload)
         "../extensions/gsd-planning/execution-policy.rkt")

(define exec-policy-tests
  (test-suite "GSD Execution Policy"

    (test-case "gsd-tool-guard: planning-write blocked in executing mode"
      (gsm-transition! 'executing)
      (define result (gsd-tool-guard (hasheq 'tool-name "planning-write")))
      ;; hook-block returns a specific structure
      (check-not-false result))

    (test-case "gsd-tool-guard: tools allowed in idle mode"
      (gsm-transition! 'idle)
      (define result (gsd-tool-guard (hasheq 'tool-name "bash")))
      (check-not-false result))

    (test-case "gsd-tool-guard: tool blocked in plan-written mode"
      (gsm-transition! 'plan-written)
      (define result (gsd-tool-guard (hasheq 'tool-name "write")))
      (check-not-false result))

    ;; BUG-0072 seam regression: the real scheduler seam delivers these exact
    ;; keys; the planning-artifact guard must see them as a hash. Uses an
    ;; isolated ctx (current-gsd-ctx parameter) so the mode is authoritative.
    (test-case "gsd-tool-guard: hash seam delivers tool-arguments for planning-artifact guard"
      (define ctx (make-gsd-context))
      (gsm-ctx-transition-to! ctx 'executing)
      (define result
        (parameterize ([current-gsd-ctx ctx])
          (gsd-tool-guard
           (hasheq 'tool-name "write" 'tool-arguments (hasheq 'path "/repo/.planning/PLAN.md")))))
      (check-equal? (hook-result-action result)
                    'block
                    "executing-mode write to a protected planning artifact must be blocked")
      (gsm-ctx-reset! ctx))))

;; ── W1: delete-lines joins the protected planning mutation target guard ──
;; Inventory (tools/registry-table.rkt mutates-filesystem-tool-names):
;; write, edit and delete-lines are the ONLY installed generic file-mutation
;; tools with a 'path payload; bash is shell (never granted a planning write)
;; and planning-write has its own dedicated guard. delete-lines was the one
;; omission. Scope semantics are unchanged: the target guard fires only in
;; executing mode, the payload shape is the hook seam's tool-arguments hash,
;; and author-phase denial is decided before any target inspection.
(define (executing-delete-lines-guard path-arg)
  (define ctx (make-gsd-context))
  (gsm-ctx-transition-to! ctx 'executing)
  (define result
    (parameterize ([current-gsd-ctx ctx])
      (gsd-tool-guard
       (hasheq 'tool-name
               "delete-lines"
               'tool-arguments
               (hasheq 'path path-arg 'start-line 1 'end-line 3)))))
  (gsm-ctx-reset! ctx)
  result)

(test-case "delete-lines targeting a protected planning artifact is blocked in executing mode"
  (define (action-of result) (hook-result-action result))
  ;; Relative form.
  (check-eq? (action-of (executing-delete-lines-guard ".planning/PLAN.md")) 'block)
  ;; Absolute form.
  (check-eq? (action-of (executing-delete-lines-guard "/repo/.planning/PLAN.md")) 'block)
  ;; Hook-rewritten separator form (the seam may rewrite separators).
  (check-eq? (action-of (executing-delete-lines-guard ".planning\\PLAN.md")) 'block)
  ;; Path-object payload form.
  (check-eq? (action-of (executing-delete-lines-guard (string->path "/repo/.planning/PLAN.md")))
             'block)
  ;; Every authoritative artifact basename stays protected, dated or not.
  (check-eq? (action-of (executing-delete-lines-guard ".planning/STATE-2026-10-29.md")) 'block)
  (check-eq? (action-of (executing-delete-lines-guard ".planning/VALIDATION.md")) 'block)
  (check-eq? (action-of (executing-delete-lines-guard ".planning/HANDOFF.json")) 'block))

(test-case "delete-lines on ordinary paths stays allowed in executing mode"
  (check-eq? (hook-result-action (executing-delete-lines-guard "docs/gsd-guide.md")) 'pass)
  (check-eq? (hook-result-action (executing-delete-lines-guard "q/src/a.rkt")) 'pass)
  ;; Nested lookalikes are not the authoritative artifact.
  (check-eq? (hook-result-action (executing-delete-lines-guard ".planning/notes/PLAN.md"))
             'pass))

(test-case "the protected planning mutation target guard stays scoped to executing mode"
  ;; Pre-existing semantics: outside executing mode the mode policy rules and
  ;; the target guard adds nothing. delete-lines is not in any plan-written or
  ;; verifying blocklist today; widening that is a separate coverage decision,
  ;; not part of this guard consolidation.
  (define ctx (make-gsd-context))
  (gsm-ctx-transition-to! ctx 'idle)
  (define result
    (parameterize ([current-gsd-ctx ctx])
      (gsd-tool-guard
       (hasheq 'tool-name
               "delete-lines"
               'tool-arguments
               (hasheq 'path ".planning/PLAN.md" 'start-line 1 'end-line 3)))))
  (gsm-ctx-reset! ctx)
  (check-eq? (hook-result-action result) 'pass "idle mode keeps its existing tool policy"))

(test-case "author scope still denies delete-lines before any target inspection"
  ;; Safe mode/author scope preservation: the phase denial is decided first,
  ;; so the scope error names the tool list, never a planning-target reason.
  (define repo (make-temporary-file "exec-policy-scope-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define scope
       (hasheq 'phase
               'implementation
               'repo
               (path->complete-path repo)
               'files
               (list "q/a.rkt" "q/a-test.rkt")
               'tests
               (list "q/a-test.rkt")))
     (define ctx (make-gsd-context))
     (gsm-ctx-transition-to! ctx 'executing)
     (define result
       (parameterize ([current-gsd-ctx ctx] [current-gsd-author-phase scope])
         (gsd-tool-guard
          (hasheq 'tool-name
                  "delete-lines"
                  'tool-arguments
                  (hasheq 'path ".planning/PLAN.md" 'start-line 1 'end-line 3)))))
     (gsm-ctx-reset! ctx)
     (check-eq? (hook-result-action result) 'block)
     (check-true (string-contains? (format "~a" (hook-result-payload result))
                                   "AUTHOR_TOOL_NOT_ALLOWED")
                 "the denial is the author-scope tool denial, unchanged"))
   (lambda () (delete-directory/files repo #:must-exist? #f))))

;; Ensure clean state
(gsm-transition! 'idle)

(module+ main
  (run-tests exec-policy-tests))
