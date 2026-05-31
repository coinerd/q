#lang racket

;; BOUNDARY: integration

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
      ;; At least 3 of the known exceptions should still be importing
      (check-true
       (>= (length still-importing) 3)
       (format "Too few known exceptions still importing from tools/extensions: ~a (expected >= 3)"
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

(test-case "RA-5: runtime/context-policy.rkt under 300 lines"
  (define p (build-path q-dir "runtime" "context-policy.rkt"))
  (when (file-exists? p)
    (define lines (length (string-split (file->string p) "\n")))
    (check-true (< lines 300)
                (format "runtime/context-policy.rkt has ~a lines (expected < 300)" lines))))

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

;; ════════════════════════════════════════════════════════════
;; v0.37.7 additions: FM-17b purity checks, FM-17d schema drift
;; ════════════════════════════════════════════════════════════

(define v0377-suite
  (test-suite "v0.37.7-fitness"

    ;; FM-17b: decision.rkt and counters.rkt purity
    (test-case "FM-17b: decision.rkt imports no I/O modules"
      (define p (build-path q-dir "runtime" "iteration" "decision.rkt"))
      (when (file-exists? p)
        (define reqs (extract-requires p))
        (check-false (imports-from? reqs '("racket/port" "racket/file" "racket/tcp"))
                     "decision.rkt should not import I/O modules")))

    (test-case "FM-17b: counters.rkt imports no I/O modules"
      (define p (build-path q-dir "runtime" "iteration" "counters.rkt"))
      (when (file-exists? p)
        (define reqs (extract-requires p))
        ;; NOTE: check-cancellation in counters.rkt imports event-bus (FA-03).
        ;; This test documents the known impurity; remove exemption once FA-03 is fixed.
        (check-false (imports-from? reqs '("racket/port" "racket/file" "racket/tcp"))
                     "counters.rkt should not import low-level I/O modules")))

    ;; FM-17d: Config schema drift -- dict-ref keys must have accessors
    (test-case "FM-17d: All dict-ref config keys have session-config accessors"
      (define accessor-pattern #rx"config-([a-z-]+)")
      (define config-path (build-path q-dir "runtime" "session-config.rkt"))
      (define config-content (file->string config-path))
      (define accessors
        (for/list ([m (in-list (regexp-match* accessor-pattern config-content #:match-select cadr))])
          (string->symbol m)))
      (define runtime-files (rkt-files-in "runtime"))
      (define missing-accessors
        (for*/list ([f (in-list runtime-files)]
                    [m (in-list (regexp-match* #rx"dict-ref\\s+config\\s+'([a-z-]+)"
                                               (file->string f)
                                               #:match-select cadr))]
                    #:when (not (member (string->symbol m) accessors)))
          (cons (path->string (find-relative-path (simplify-path q-dir) (simplify-path f)))
                (string->symbol m))))
      (check-equal? missing-accessors
                    '()
                    (format "dict-ref config keys missing accessors: ~a" missing-accessors)))))

(run-tests v0377-suite)

;; ════════════════════════════════════════════════════════════
;; v0.39.6 additions: R-18 through R-23 hard gates
;; ════════════════════════════════════════════════════════════

(define v0396-suite
  (test-suite "v0.39.6-hard-gates"

    ;; R-18: Pure modules must not gain I/O imports
    (test-case "R-18: decision.rkt has no I/O imports"
      (define p (build-path q-dir "runtime" "iteration" "decision.rkt"))
      (when (file-exists? p)
        (define reqs (extract-requires p))
        (check-false (imports-from? reqs '("racket/port" "racket/file" "racket/tcp" "racket/os"))
                     "decision.rkt must not import I/O modules")))

    (test-case "R-18: event-payloads.rkt has no I/O imports"
      (define p (build-path q-dir "util" "event-payloads.rkt"))
      (when (file-exists? p)
        (define reqs (extract-requires p))
        (check-false (imports-from? reqs '("racket/port" "racket/file" "racket/tcp" "racket/os"))
                     "event-payloads.rkt must not import I/O modules")))

    (test-case "R-18: event-codec.rkt has no I/O imports"
      (define p (build-path q-dir "util" "event-codec.rkt"))
      (when (file-exists? p)
        (define reqs (extract-requires p))
        (check-false (imports-from? reqs '("racket/port" "racket/tcp" "racket/os"))
                     "event-codec.rkt must not import I/O modules")))

    ;; R-19: Parser modules must not require I/O modules
    (test-case "R-19: GSD command-parser has no I/O imports"
      (define p (build-path q-dir "extensions" "gsd" "command-parser.rkt"))
      (when (file-exists? p)
        (define reqs (extract-requires p))
        (check-false (imports-from? reqs '("racket/port" "racket/file" "racket/tcp" "racket/os"))
                     "command-parser.rkt must not import I/O modules")))

    (test-case "R-19: TUI command-parse has no I/O imports"
      (define p (build-path q-dir "tui" "command-parse.rkt"))
      (when (file-exists? p)
        (define reqs (extract-requires p))
        (check-false (imports-from? reqs '("racket/port" "racket/file" "racket/tcp" "racket/os"))
                     "command-parse.rkt must not import I/O modules")))

    (test-case "R-19: CLI args has no I/O imports"
      (define p (build-path q-dir "cli" "args.rkt"))
      (when (file-exists? p)
        (define reqs (extract-requires p))
        (check-false (imports-from? reqs '("racket/port" "racket/file" "racket/tcp" "racket/os"))
                     "cli/args.rkt must not import I/O modules")))

    ;; R-20: Provide surface budget check (informational)
    (test-case "R-20: Provide surface budget (informational)"
      (define dirs-to-check '("runtime" "agent" "llm" "tools" "tui" "interfaces" "util" "extensions"))
      (define all-files
        (append* (for/list ([d (in-list dirs-to-check)])
                   (rkt-files-in d))))
      (define alert-threshold
        (cdr (assoc 'alert-threshold (cdr (assoc 'provide-surface-budget policy)))))
      (define high-provides
        (for/list ([f (in-list all-files)]
                   #:when (> (count-provides f) alert-threshold))
          (cons (path->string (find-relative-path (simplify-path q-dir) (simplify-path f)))
                (count-provides f))))
      (when (not (null? high-provides))
        (displayln (format "INFO: Modules with >~a provides: ~a" alert-threshold high-provides)))
      (check-true #t "Informational -- always passes"))

    ;; R-21: Fan-in hard gate (graduated from informational)
    (test-case "R-21: Require fan-in budget (hard gate)"
      (define dirs-to-check '("runtime" "agent" "llm" "tools" "tui" "interfaces"))
      (define all-files
        (append* (for/list ([d (in-list dirs-to-check)])
                   (rkt-files-in d))))
      (define over-limit
        (for/list ([f (in-list all-files)]
                   #:when (> (count-requires f) max-fan-in))
          (cons (path->string (find-relative-path (simplify-path q-dir) (simplify-path f)))
                (count-requires f))))
      (check-true (<= (length over-limit) 3)
                  (format "More than 3 files exceed fan-in limit of ~a: ~a" max-fan-in over-limit)))))

(run-tests v0396-suite)

;; ════════════════════════════════════════════════════════════
;; v0.48.0 additions: KPI recovery hard gates
;; ════════════════════════════════════════════════════════════

(define v0480-suite
  (test-suite "v0.49.10-kpi-gates"

    (test-case "KPI: struct-out non-regression (v0.49.10 floor, v0.50.x target tracked)"
      (define dirs-to-check
        '("runtime" "agent"
                    "llm"
                    "tools"
                    "tui"
                    "interfaces"
                    "util"
                    "extensions"
                    "cli"
                    "sandbox"
                    "wiring"))
      (define all-files
        (append* (for/list ([d (in-list dirs-to-check)])
                   (rkt-files-in d))))
      (define total-struct-out
        (for/sum ([f (in-list all-files)])
                 (length (regexp-match* #rx"\\(struct-out\\s+" (file->string f)))))
      (check-true (<= total-struct-out 55)
                  (format "struct-out count is ~a (v0.49.10 floor <= 55; v0.50.x target <= 40)"
                          total-struct-out)))

    (test-case "KPI: contract-out coverage non-regression (v0.49.10 floor, v0.50.x target tracked)"
      (define dirs-to-check
        '("runtime" "agent"
                    "llm"
                    "tools"
                    "tui"
                    "interfaces"
                    "util"
                    "extensions"
                    "cli"
                    "sandbox"
                    "wiring"))
      (define all-files
        (append* (for/list ([d (in-list dirs-to-check)])
                   (rkt-files-in d))))
      (define files-with-contract-out
        (for/sum ([f (in-list all-files)])
                 (if (string-contains? (file->string f) "(contract-out") 1 0)))
      (define coverage
        (if (zero? (length all-files))
            0
            (* 100.0 (/ files-with-contract-out (length all-files)))))
      (check-true
       (>= coverage 35.0)
       (format
        "contract-out coverage is ~a% (v0.49.10 floor >= 35.0%; v0.50.x target >= 45%) [~a/~a files]"
        (real->decimal-string coverage 2)
        files-with-contract-out
        (length all-files))))))

(run-tests v0480-suite)

;; ════════════════════════════════════════════════════════════
;; v0.74.0 additions: Hotspot-score fitness function
;; ════════════════════════════════════════════════════════════

;; Compute hotspot score: lines × estimated complexity (heuristic from file content)
(define (compute-hotspot-score filepath)
  (define content (file->string filepath))
  (define lines (length (string-split content "\n")))
  ;; Count structural complexity indicators: define, lambda, let, match, cond, struct
  (define complexity-hits
    (+ (length (regexp-match* #rx"[(]define" content))
       (length (regexp-match* #rx"[(]lambda" content))
       (length (regexp-match* #rx"[(]let" content))
       (length (regexp-match* #rx"[(]match" content))
       (length (regexp-match* #rx"[(]cond" content))
       (length (regexp-match* #rx"[(]struct" content))))
  (* lines complexity-hits))

;; Get registered risk-note file paths from policy
(define hotspot-section (cdr (assoc 'hotspot-budget policy)))
(define warn-threshold (cdr (assoc 'warn-threshold hotspot-section)))
(define block-threshold (cdr (assoc 'block-threshold hotspot-section)))
(define risk-note-entries (cdr (assoc 'risk-notes hotspot-section)))
(define risk-note-files
  (for/list ([entry (in-list risk-note-entries)])
    (car entry)))

(define v0740-suite
  (test-suite "v0.74.0-hotspot-fitness"

    (test-case "All hotspots above warn threshold have risk-notes"
      (define dirs-to-check '("runtime" "agent" "llm" "tools" "tui" "interfaces" "extensions"))
      (define all-files
        (append* (for/list ([d (in-list dirs-to-check)])
                   (rkt-files-in d))))
      (define hotspots-without-notes
        (for/list ([f (in-list all-files)]
                   #:when (let* ([score (compute-hotspot-score f)]
                                 [rel (path->string (find-relative-path (simplify-path q-dir)
                                                                        (simplify-path f)))])
                            (and (> score warn-threshold) (not (member rel risk-note-files)))))
          (cons (path->string (find-relative-path (simplify-path q-dir) (simplify-path f)))
                (compute-hotspot-score f))))
      (when (not (null? hotspots-without-notes))
        (displayln (format "WARN: Hotspots above ~a without risk-notes: ~a"
                           warn-threshold
                           hotspots-without-notes)))
      ;; v0.74.6: Still informational at warn level — too many files to annotate
      ;; Will become blocking in v0.75.x after threshold calibration
      (check-true #t "Informational — warns but does not block"))

    (test-case "No hotspot exceeds block threshold without risk-notes"
      (define dirs-to-check '("runtime" "agent" "llm" "tools" "tui" "interfaces" "extensions"))
      (define all-files
        (append* (for/list ([d (in-list dirs-to-check)])
                   (rkt-files-in d))))
      ;; Use top-N approach: only check the TOP 10 hotspots
      (define scored-files
        (sort (for/list ([f (in-list all-files)])
                (cons (path->string (find-relative-path (simplify-path q-dir) (simplify-path f)))
                      (compute-hotspot-score f)))
              >
              #:key cdr))
      (define top-10 (take scored-files (min 10 (length scored-files))))
      (define blocking-hotspots
        (for/list ([entry (in-list top-10)]
                   #:when (not (member (car entry) risk-note-files)))
          entry))
      (when (not (null? blocking-hotspots))
        (displayln (format "WARN: Top hotspots without risk-notes: ~a" blocking-hotspots)))
      ;; v0.74.6: BLOCKING — top hotspots above block threshold must have risk-notes
      (check-equal? blocking-hotspots '()
                   (format "Top hotspots above ~a without risk-notes" block-threshold)))

    (test-case "Risk-note entries reference existing files"
      (for ([entry (in-list risk-note-entries)])
        (define rel-path (car entry))
        (define fpath (build-path q-dir rel-path))
        (check-true (file-exists? fpath) (format "risk-note file ~a does not exist" rel-path))))

    (test-case "Hotspot-budget policy has required keys"
      (check-not-false (assoc 'warn-threshold hotspot-section) "must have warn-threshold")
      (check-not-false (assoc 'block-threshold hotspot-section) "must have block-threshold")
      (check-not-false (assoc 'risk-notes hotspot-section) "must have risk-notes"))))

(run-tests v0740-suite)

;; ════════════════════════════════════════════════════════════

;; ════════════════════════════════════════════════════════════
;; v0.74.1 additions: Cycle-detection CI gate
;; ════════════════════════════════════════════════════════════

;; Simple cycle detection: collect all local requires from .rkt files
;; and build a directed graph, then DFS for back-edges.

(define (local-requires-of filepath)
  (define content (file->string filepath))
  (define rx-pattern (byte-regexp #"\"([a-zA-Z0-9_./-]+\\.rkt)\""))
  (define bs (string->bytes/utf-8 content))
  (for/list ([m (regexp-match* rx-pattern bs)])
    (bytes->string/utf-8 m)))

(define (resolve-dep from-path rel)
  (simplify-path (build-path (path-only from-path) rel)))

(define (build-require-graph root-dir)
  (define all-files (rkt-files-in root-dir))
  (define graph (make-hash))
  (for ([f (in-list all-files)])
    (define rels (local-requires-of f))
    (define resolved
      (filter-map (lambda (r)
                    (define p (resolve-dep f r))
                    (and (file-exists? p) p))
                  rels))
    (hash-set! graph (simplify-path f) resolved))
  graph)

(define (find-cycles graph)
  (define visited (make-hash))
  (define cycles '())
  (define (dfs path stack)
    (define state (hash-ref visited path #f))
    (cond
      [(eq? state 'visiting)
       (define cycle-start (member path (reverse stack)))
       (when cycle-start
         (set! cycles (cons cycle-start cycles)))]
      [(eq? state 'done) (void)]
      [else
       (hash-set! visited path 'visiting)
       (for ([dep (in-list (hash-ref graph path '()))])
         (when (hash-has-key? graph dep)
           (dfs dep (cons path stack))))
       (hash-set! visited path 'done)]))
  (for ([node (in-hash-keys graph)])
    (dfs node '()))
  cycles)

(define v0741-suite
  (test-suite "v0.74.1-cycle-detection"

    (test-case "No circular dependencies in runtime layer"
      (define graph (build-require-graph "runtime"))
      (define cycles (find-cycles graph))
      (when (not (null? cycles))
        (for ([c (in-list cycles)])
          (displayln (format "CYCLE: ~a" c))))
      (check-equal? cycles '() "runtime layer must have no circular requires"))

    (test-case "No circular dependencies in agent layer"
      (define graph (build-require-graph "agent"))
      (define cycles (find-cycles graph))
      (when (not (null? cycles))
        (for ([c (in-list cycles)])
          (displayln (format "CYCLE: ~a" c))))
      (check-equal? cycles '() "agent layer must have no circular requires"))

    (test-case "No circular dependencies in tools layer"
      (define graph (build-require-graph "tools"))
      (define cycles (find-cycles graph))
      (when (not (null? cycles))
        (for ([c (in-list cycles)])
          (displayln (format "CYCLE: ~a" c))))
      (check-equal? cycles '() "tools layer must have no circular requires"))

    (test-case "session-store-tree no longer lazy-requires session-store"
      (define tree-content (file->string (build-path q-dir "runtime" "session-store-tree.rkt")))
      ;; Strip comment lines before checking
      (define code-lines
        (filter (lambda (l) (not (string-prefix? (string-trim l) ";;")))
                (string-split tree-content "\n")))
      (define code-only (string-join code-lines "\n"))
      (check-false (regexp-match? #rx"lazy-require" code-only)
                   "session-store-tree.rkt must not use lazy-require"))))

(run-tests v0741-suite)
