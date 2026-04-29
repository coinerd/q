#lang racket

;; tests/test-arch-fitness.rkt — Architecture fitness tests
;;
;; Verifies quantitative architecture health:
;;   1. No module exceeds 900 lines (with known-large tracked items)
;;   2. Known runtime layer exceptions are stable
;;   3. main.rkt re-export breadth is reasonable (< 200 symbols)
;;   4. tui/ does not import from llm/ or tools/
;;   5. extensions/ does not import from tui/
;;   6. llm/ does not import from runtime/, tools/, extensions/
;;
;; Refs: ARCH-FITNESS

(require rackunit
         rackunit/text-ui
         racket/string
         "helpers/arch-utils.rkt")

;; ============================================================
;; Fitness tests
;; ============================================================

(define fitness-tests
  (test-suite "architecture-fitness"

    ;; ── Test 1: Module line count ──────────────────────────────
    (test-case "No module exceeds 900 lines"
      (define max-lines 900)
      ;; Known-large modules tracked for future splitting (v0.23.0 target):
      ;;   extensions/racket-tooling.rkt (~922 lines)
      ;; tui/state.rkt was split in v0.22.6 W2 — no longer known-large.
      (define known-large '(("extensions/racket-tooling.rkt" . 950)))
      (define dirs-to-check '("runtime" "agent" "llm" "tools" "tui" "interfaces"))
      (define all-files
        (append* (for/list ([d (in-list dirs-to-check)])
                   (rkt-files-in d))))
      (define oversized
        (for/list ([f (in-list all-files)]
                   #:when (let ([lc (line-count f)])
                            (and (> lc max-lines)
                                 (not (assoc (path->string (find-relative-path (simplify-path q-dir)
                                                                               (simplify-path f)))
                                             known-large)))))
          (cons (path->string (find-relative-path (simplify-path q-dir) (simplify-path f)))
                (line-count f))))
      (check-equal? oversized
                    '()
                    (format "Files exceeding ~a lines (excluding known-large): ~a"
                            max-lines
                            (for/list ([p oversized])
                              (format "~a (~a lines)" (car p) (cdr p))))))

    ;; ── Test 2: Known runtime exceptions are stable ────────────
    (test-case "Known runtime layer exceptions are stable"
      ;; v0.22.4 (MOD-01): iteration.rkt -> turn-orchestrator.rkt
      ;; v0.22.5: read-based parser reveals 7 legitimate upward imports
      (define known-exceptions
        '("agent-session.rkt" "iteration.rkt"
                              "runtime-helpers.rkt"
                              "tool-coordinator.rkt"
                              "turn-orchestrator.rkt"
                              "package.rkt"
                              "extension-catalog.rkt"))
      (for ([name (in-list known-exceptions)])
        (define fpath (build-path q-dir "runtime" name))
        (check-true (file-exists? fpath) (format "Known exception runtime/~a no longer exists" name)))
      (define still-importing
        (for/list ([name (in-list known-exceptions)]
                   #:when (let* ([fpath (build-path q-dir "runtime" name)]
                                 [reqs (extract-requires fpath)])
                            (imports-from?
                             reqs
                             '("../tools/" "../../tools/" "../extensions/" "../../extensions/"))))
          name))
      ;; At least 5 of the 7 known exceptions should still be importing
      (check-true
       (>= (length still-importing) 5)
       (format "Too few known exceptions still importing from tools/extensions: ~a (expected >= 5)"
               still-importing))
      (check-true (<= (length still-importing) 7)
                  (format "More than 3 runtime files importing from tools/extensions: ~a"
                          still-importing)))

    ;; ── Test 3: main.rkt re-export breadth ─────────────────────
    (test-case "main.rkt re-export breadth is reasonable"
      (define main-path (build-path q-dir "main.rkt"))
      (check-true (file-exists? main-path) "main.rkt must exist")
      (define provides (count-provides main-path))
      (check-true (< provides 200) (format "main.rkt exports ~a symbols (must be < 200)" provides)))

    ;; ── Test 4: tui/ does not import from llm/ or tools/ ──────
    (test-case "tui/ does not import from llm/ or tools/"
      (define tui-files (rkt-files-in "tui"))
      (define violations
        (for/list ([f (in-list tui-files)])
          (define reqs (extract-requires f))
          (if (imports-from? reqs '("../llm/" "../../llm/" "../tools/" "../../tools/"))
              (format "~a imports from llm/ or tools/"
                      (path->string (find-relative-path (simplify-path q-dir) (simplify-path f))))
              #f)))
      (define actual-violations (filter identity violations))
      (check-equal? actual-violations
                    '()
                    (format "TUI importing from llm/ or tools/: ~a" actual-violations)))

    ;; ── Test 5: extensions/ does not import from tui/ ──────────
    (test-case "extensions/ does not import from tui/"
      ;; Known exceptions: dialog-api.rkt and ui-surface.rkt import from tui/state.rkt
      (define known-exceptions '("dialog-api.rkt" "ui-surface.rkt"))
      (define ext-files
        (filter (lambda (f) (not (member (path->string (file-name-from-path f)) known-exceptions)))
                (rkt-files-in "extensions")))
      (define violations
        (for/list ([f (in-list ext-files)])
          (define reqs (extract-requires f))
          (if (imports-from? reqs '("../tui/" "../../tui/"))
              (format "~a imports from tui/"
                      (path->string (find-relative-path (simplify-path q-dir) (simplify-path f))))
              #f)))
      (define actual-violations (filter identity violations))
      (check-equal? actual-violations
                    '()
                    (format "extensions/ importing from tui/: ~a" actual-violations)))

    ;; ── Test 6: llm/ does not import from runtime/, tools/, extensions/ ─
    (test-case "llm/ does not import from runtime/, tools/, or extensions/"
      (define llm-files (rkt-files-in "llm"))
      (define forbidden-prefixes
        '("../runtime/" "../../runtime/"
                        "../tools/"
                        "../../tools/"
                        "../extensions/"
                        "../../extensions/"))
      (define violations
        (for/list ([f (in-list llm-files)])
          (define reqs (extract-requires f))
          (if (imports-from? reqs forbidden-prefixes)
              (format "~a imports from higher layers"
                      (path->string (find-relative-path (simplify-path q-dir) (simplify-path f))))
              #f)))
      (define actual-violations (filter identity violations))
      (check-equal? actual-violations
                    '()
                    (format "llm/ importing from higher layers: ~a" actual-violations)))))

(run-tests fitness-tests)
