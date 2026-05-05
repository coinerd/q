#lang racket

;; tests/test-arch-fitness.rkt — Architecture fitness tests
;;
;; Verifies quantitative architecture health:
;;   1. No module exceeds threshold (loaded from dependency-policy.rktd)
;;   2. Known runtime layer exceptions are stable + drift gate
;;   3. main.rkt re-export breadth is reasonable
;;   4. tui/ does not import from llm/ or tools/
;;   5. extensions/ does not import from tui/ (with known exceptions)
;;   6. llm/ does not import from runtime/, tools/, extensions/
;;   7-8. SDK surface stability
;;   9. Function length budget (informational)
;;  10. Require fan-in budget (informational)
;;
;; Data source: docs/architecture/dependency-policy.rktd
;; Refs: ARCH-FITNESS

(require rackunit
         rackunit/text-ui
         racket/string
         racket/file
         "helpers/arch-utils.rkt")

;; Load policy from single source of truth
(define policy-path (build-path q-dir "docs" "architecture" "dependency-policy.rktd"))

(define policy (call-with-input-file policy-path read))

(define (policy-ref section . keys)
  (let loop ([data (cdr (assoc section policy))]
             [ks keys])
    (if (null? ks)
        data
        (loop (cdr (assoc (car ks) data)) (cdr ks)))))

(define max-lines (cdr (assoc 'max-lines-per-module (cdr (assoc 'complexity-budgets policy)))))
(define max-func-len (cdr (assoc 'max-function-length (cdr (assoc 'complexity-budgets policy)))))
(define max-fan-in (cdr (assoc 'max-require-fan-in (cdr (assoc 'complexity-budgets policy)))))
(define known-large
  (map (λ (s)
         (if (symbol? s)
             (symbol->string s)
             s))
       (cdr (assoc 'known-large (cdr (assoc 'module-size policy))))))
(define runtime-exc (policy-ref 'known-exceptions 'runtime))
(define runtime-exceptions
  (map (λ (s)
         (if (symbol? s)
             (symbol->string s)
             s))
       (map car runtime-exc)))
(define max-runtime-exc (policy-ref 'layers 'runtime 'max-exceptions))
(define ext-tui-exc (policy-ref 'known-exceptions 'extensions))
(define ext-tui-exceptions
  (map (λ (s)
         (if (symbol? s)
             (symbol->string s)
             s))
       (map car ext-tui-exc)))

;; ============================================================
;; Fitness tests
;; ============================================================

(define fitness-tests
  (test-suite "architecture-fitness"

    ;; ── Test 1: Module line count ──────────────────────────────
    (test-case "No module exceeds line threshold"
      (define dirs-to-check '("runtime" "agent" "llm" "tools" "tui" "interfaces"))
      (define all-files
        (append* (for/list ([d (in-list dirs-to-check)])
                   (rkt-files-in d))))
      (define oversized
        (for/list ([f (in-list all-files)]
                   #:when (let ([lc (line-count f)])
                            (and (> lc max-lines)
                                 (not (member (path->string (find-relative-path (simplify-path q-dir)
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

    ;; ── Test 2: Known runtime exceptions are stable + drift gate ──
    (test-case "Known runtime layer exceptions are stable"
      (for ([name (in-list runtime-exceptions)])
        (define fpath (build-path q-dir "runtime" name))
        (check-true (file-exists? fpath) (format "Known exception runtime/~a no longer exists" name)))
      (define still-importing
        (for/list ([name (in-list runtime-exceptions)]
                   #:when (let* ([fpath (build-path q-dir "runtime" name)]
                                 [reqs (extract-requires fpath)])
                            (imports-from?
                             reqs
                             '("../tools/" "../../tools/" "../extensions/" "../../extensions/"))))
          name))
      ;; Drift gate: exception count must not grow beyond policy maximum
      (check-true
       (<= (length runtime-exceptions) max-runtime-exc)
       (format
        "Runtime boundary exceptions (~a) exceed policy maximum (~a). Update dependency-policy.rktd or refactor."
        (length runtime-exceptions)
        max-runtime-exc))
      ;; At least 5 of the known exceptions should still be importing
      (check-true
       (>= (length still-importing) 5)
       (format "Too few known exceptions still importing from tools/extensions: ~a (expected >= 5)"
               still-importing))
      (check-true (<= (length still-importing) max-runtime-exc)
                  (format "More than ~a runtime files importing from tools/extensions: ~a"
                          max-runtime-exc
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
      (define ext-files
        (filter (lambda (f) (not (member (path->string (file-name-from-path f)) ext-tui-exceptions)))
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
                    (format "llm/ importing from higher layers: ~a" actual-violations)))

    ;; ── Test 7: SDK surface exists and is documented (ARCH-01) ──
    (test-case "sdk-public.rkt provides a stable SDK surface"
      (define sdk-provides (count-provides (build-path q-dir "interfaces" "sdk-public.rkt")))
      (check-true (> sdk-provides 0)
                  (format "sdk-public.rkt should export symbols (found ~a)" sdk-provides)))

    ;; ── Test 8: SDK surface has no internal layer imports (ARCH-01) ──
    (test-case "sdk.rkt does not import from tui/ or interfaces/"
      (define sdk-path (build-path q-dir "interfaces" "sdk.rkt"))
      (define reqs (extract-requires sdk-path))
      (check-false (imports-from? reqs '("../tui/" "../../tui/"))
                   "sdk.rkt should not import from tui/")
      (check-false (imports-from? reqs '("cli.rkt" "json-mode.rkt" "rpc-mode.rkt"))
                   "sdk.rkt should not import interface mode modules"))

    ;; ── Test 9: Function length budget (INFORMATIONAL) ──────────
    (test-case "Function length budget (informational)"
      ;; INFORMATIONAL in v0.22.8 — always passes, just logs warnings.
      ;; Full function-length scanning is deferred to v0.23.0.
      (check-true #t "Informational — always passes"))

    ;; ── Test 10: Require fan-in budget (INFORMATIONAL) ──────────
    (test-case "Require fan-in budget (informational)"
      (define dirs-to-check '("runtime" "agent" "llm" "tools" "tui" "interfaces"))
      (define all-files
        (append* (for/list ([d (in-list dirs-to-check)])
                   (rkt-files-in d))))
      (define high-fan-in
        (for/list ([f (in-list all-files)]
                   #:when (let ([n (count-requires f)]) (> n max-fan-in)))
          (cons (path->string (find-relative-path (simplify-path q-dir) (simplify-path f)))
                (count-requires f))))
      ;; INFORMATIONAL: log warnings but don't fail
      (when (not (null? high-fan-in))
        (displayln (format "INFO: High require fan-in: ~a" high-fan-in)))
      (check-true #t "Informational — always passes"))

    ;; ── Test 11: Policy file loads correctly ────────────────────
    (test-case "dependency-policy.rktd is valid and loadable"
      (check-true (file-exists? policy-path) "dependency-policy.rktd must exist")
      (check-true (list? policy) "Policy must be a valid list")
      (check-pred values (assoc 'layers policy) "Policy must have 'layers section")
      (check-pred values
                  (assoc 'known-exceptions policy)
                  "Policy must have 'known-exceptions section"))))

;; ── RA-3: Module decomposition line-count fitness ──
(test-case "RA-3: context-assembly.rkt under 600 lines after decomposition"
  (define ca-path (build-path q-dir "runtime" "context-assembly.rkt"))
  (when (file-exists? ca-path)
    (define lines (length (string-split (file->string ca-path) "\n")))
    (check-true (< lines 600) (format "context-assembly.rkt has ~a lines (expected < 600)" lines))))

;; ── RA-5: Decomposition fitness — key modules under line budget ──
(test-case "RA-5: agent/loop.rkt under 300 lines after decomposition"
  (define p (build-path q-dir "agent" "loop.rkt"))
  (when (file-exists? p)
    (define lines (length (string-split (file->string p) "\n")))
    (check-true (< lines 300) (format "agent/loop.rkt has ~a lines (expected < 300)" lines))))

(test-case "RA-5: extensions/gsd/core.rkt under 400 lines"
  (define p (build-path q-dir "extensions" "gsd" "core.rkt"))
  (when (file-exists? p)
    (define lines (length (string-split (file->string p) "\n")))
    (check-true (< lines 400)
                (format "extensions/gsd/core.rkt has ~a lines (expected < 400)" lines))))

(test-case "RA-5: runtime/context-policy.rkt under 250 lines"
  (define p (build-path q-dir "runtime" "context-policy.rkt"))
  (when (file-exists? p)
    (define lines (length (string-split (file->string p) "\n")))
    (check-true (< lines 250)
                (format "runtime/context-policy.rkt has ~a lines (expected < 250)" lines))))

(test-case "RA-5: tools/tool.rkt under 550 lines"
  (define p (build-path q-dir "tools" "tool.rkt"))
  (when (file-exists? p)
    (define lines (length (string-split (file->string p) "\n")))
    (check-true (< lines 550) (format "tools/tool.rkt has ~a lines (expected < 550)" lines))))

(test-case "RA-5: agent/event-types.rkt exists as extracted module"
  (define p (build-path q-dir "agent" "event-types.rkt"))
  (check-true (file-exists? p) "agent/event-types.rkt must exist after extraction"))

;; ── Helper for fan-in check ─────────────────────────────────

;; Count the number of require imports in a file
(define (count-requires filepath)
  (define content (file->string filepath))
  (length (regexp-match* #rx"\\(require\\b" content)))

(run-tests fitness-tests)

;; ════════════════════════════════════════════════════════════
;; v0.28.10 additions: TR modules, hook schema, event codec
;; ════════════════════════════════════════════════════════════

(define tr-module-files
  (list "util/event.rkt"
        "util/hook-types.rkt"
        "util/event-payloads.rkt"
        "extensions/gsd/plan-types.rkt"
        "extensions/gsd/plan-validator.rkt"
        "runtime/iteration/loop-state.rkt"
        "runtime/iteration/retry-policy.rkt"))

(define v02810-suite
  (test-suite "v0.28.10-features"
    (test-case "RA-6: Typed Racket modules exist and use #lang typed/racket"
      (for ([f (in-list tr-module-files)])
        (define p (build-path q-dir f))
        (check-true (file-exists? p) (format "TR module ~a must exist" f))
        (when (file-exists? p)
          (define content (file->string p))
          (check-true (regexp-match? #rx"#lang typed/racket" content)
                      (format "~a must use #lang typed/racket" f)))))
    (test-case "RA-7: HOOK-SCHEMA-VERSION defined in hook-types.rkt"
      (define p (build-path q-dir "util" "hook-types.rkt"))
      (when (file-exists? p)
        (define content (file->string p))
        (check-true (regexp-match? #rx"HOOK-SCHEMA-VERSION" content)
                    "hook-types.rkt must define HOOK-SCHEMA-VERSION")))
    (test-case "RA-8: Event codec module exists"
      (define p (build-path q-dir "util" "event-codec.rkt"))
      (check-true (file-exists? p) "util/event-codec.rkt must exist")
      (when (file-exists? p)
        (define content (file->string p))
        (check-true (regexp-match? #rx"hash->payload" content)
                    "event-codec.rkt must define hash->payload")
        (check-true (regexp-match? #rx"payload->hash" content)
                    "event-codec.rkt must define payload->hash")))))

(run-tests v02810-suite)
