#lang racket

;; @speed fast
;; @suite extensions
;; @boundary unit
;; @covers extensions/gsd/planning-artifact-validation.rkt
;; @covers extensions/gsd/wave-executor.rkt

;; tests/test-gsd-planning-preflight-kernel.rkt — historical alias (W1).
;;
;; The plan-validation/preflight pins for W1 (GSD-SIMPLIFICATION-PLAN) live in
;; tests/test-gsd-plan-validation.rkt (the canonical name the wave's gate list
;; uses). This module keeps the previous path working for references to it and
;; still EXECUTES the canonical pins: the `module+ test` body instantiates the
;; canonical module through a runtime-resolved path, so every canonical
;; test-case runs in this module's instantiation. Nothing here re-declares,
;; duplicates, or exempts those pins.
;;
;; Why `(module+ test ...)` + `dynamic-require` instead of a bare top-level
;; `(require ...)`: scripts/run-tests/runner.rkt only shells out to `raco test`
;; when a file textually carries `(module+ test` or rackunit forms. A bare alias
;; module was executed by plain `racket`, which ran the canonical checks
;; silently, printed nothing rackunit could parse, and was classified
;; ZERO_PARSED (strict mode exit 4) even though every assertion passed. The
;; alias now declares itself a real rackunit file and runs the canonical pins
;; from its own test body, so `raco test` on either path counts them.

(require rackunit
         racket/file
         racket/runtime-path)

;; Resolved relative to this source file at run time: correct in any checkout or
;; named worktree, under `raco test -t` (the runner's invocation) and under plain
;; `racket` (where `(find-system-path 'run-file)` is not this file).
(define-runtime-path canonical-plan-validation "test-gsd-plan-validation.rkt")
(define-runtime-path plan-validation-kernel "../extensions/gsd/planning-artifact-validation.rkt")

(module+ test
  ;; Instantiating the canonical module executes its test-cases: the actual
  ;; canonical pins, not a copy of them. The canonical file is a test module, so
  ;; it provides nothing itself; the kernel it pins is dynamically required from
  ;; its production home below.
  (dynamic-require canonical-plan-validation #f)

  (test-case "alias: the canonical plan-validation module is instantiated here"
    (check-true (file-exists? canonical-plan-validation))
    (check-true (module-declared? canonical-plan-validation #t))
    (check-true (procedure? (dynamic-require plan-validation-kernel 'validate-plan-artifacts))))

  (test-case "alias: the canonical preflight kernel answers through this path"
    (define validate-plan-artifacts (dynamic-require plan-validation-kernel 'validate-plan-artifacts))
    (define dir (make-temporary-file "gsd-preflight-alias-~a" 'directory))
    (dynamic-wind void
                  (lambda ()
                    ;; A base dir with no .planning/PLAN.md keeps the canonical
                    ;; no-plan refusal contract when reached through the alias.
                    (define result (validate-plan-artifacts dir))
                    (check-false (hash-ref result 'ok?))
                    (check-equal? (hash-ref result 'error-message)
                                  "No PLAN found in .planning/. Use /plan <task> to create one."))
                  (lambda () (delete-directory/files dir #:must-exist? #f)))))
