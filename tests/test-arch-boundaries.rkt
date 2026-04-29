#lang racket

;; tests/test-arch-boundaries.rkt — Architecture layer boundary tests
;;
;; Verifies that layering constraints are maintained:
;;   - Only known exceptions in runtime/ may import from tools/ or extensions/
;;   - TUI modules must not import from llm/, tools/
;;
;; Refs: #432, ARCH-01

(require rackunit
         rackunit/text-ui
         "helpers/arch-utils.rkt")

;; ============================================================
;; Boundary tests
;; ============================================================

(define boundary-tests
  (test-suite "architecture-boundaries"

    (test-case "Only known exceptions in runtime/ import from tools/ or extensions/"
      ;; Known exceptions (runtime/ importing from tools/ or extensions/):
      ;;   - agent-session.rkt — imports extensions/api.rkt for extension listing
      ;;   - iteration.rkt — imports extensions/message-inject.rkt for topic constant
      ;;   - runtime-helpers.rkt — imports extensions/hooks.rkt for hook dispatch
      ;;   - tool-coordinator.rkt — imports tools/ + extensions/ for tool execution
      ;;   - turn-orchestrator.rkt — imports tools/ for tool execution
      ;;   - package.rkt — imports extensions/manifest.rkt for package audit
      ;;   - extension-catalog.rkt — imports from extensions/
      (define runtime-files (rkt-files-in "runtime"))
      (define known-exceptions
        '("agent-session.rkt" "iteration.rkt"
                              "runtime-helpers.rkt"
                              "tool-coordinator.rkt"
                              "turn-orchestrator.rkt"
                              "package.rkt"
                              "extension-catalog.rkt"))
      (define violations
        (for/list ([f (in-list runtime-files)]
                   #:when (not (member (path->string (file-name-from-path f)) known-exceptions)))
          (define reqs (extract-requires f))
          (if (imports-from? reqs '("../tools/" "../../tools/" "../extensions/" "../../extensions/"))
              (format "~a: upward imports detected" (file-name-from-path f))
              #f)))
      (define actual-violations (filter identity violations))
      (check-equal? actual-violations
                    '()
                    (format "Unexpected upward imports in runtime/: ~a" actual-violations)))

    (test-case "TUI modules must not import from llm/, tools/"
      (define tui-files (rkt-files-in "tui"))
      (define violations
        (for/list ([f (in-list tui-files)])
          (define reqs (extract-requires f))
          (if (imports-from? reqs '("../llm/" "../../llm/" "../tools/" "../../tools/"))
              (format "~a: imports from forbidden layer" (file-name-from-path f))
              #f)))
      (define actual-violations (filter identity violations))
      (check-equal? actual-violations
                    '()
                    (format "TUI modules importing from forbidden layers: ~a" actual-violations)))))

(run-tests boundary-tests)
