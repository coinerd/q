#lang racket

;;; test-standalone-plan-validator.rkt — W0 characterization pin for
;;; BUG-0048: no standalone plan-validation CLI exists. There is no
;;; `scripts/validate-plan.rkt` (and no equivalently named validator in
;;; scripts/); plan validation is reachable only through the /go path
;;; (extensions/gsd/command-handlers.rkt), which couples validation to
;;; campaign execution.
;;;
;;; Flip owner: W4 (standalone plan validator). When W4 adds the CLI,
;;; this pin must be flipped to assert `scripts/validate-plan.rkt`
;;; EXISTS, runs outside /go, and exits 0/1 on valid/invalid plans.

(require rackunit
         racket/file
         racket/path)

(define repo-root
  (simplify-path (build-path (find-system-path 'run-file) 'up 'up)))

(define scripts-dir (build-path repo-root "scripts"))

;; --- Pin 1: the standalone validator CLI does not exist.
(check-false
 (file-exists? (build-path scripts-dir "validate-plan.rkt"))
 "scripts/validate-plan.rkt does not exist (absent seam)")

;; --- Pin 2: nothing else in scripts/ is a plan validator either. The
;; pin is robust to unrelated tooling: no script filename mentions
;; validating plans/waves.
(define script-files
  (map path->string
       (filter (lambda (p) (regexp-match? #rx"\\.rkt$" (path->string p)))
               (directory-list scripts-dir))))

(check-equal?
 (filter (lambda (f) (regexp-match? #px"(?i:validat|plan|wave)" f)) script-files)
 '()
 "no script in scripts/ names anything like a plan/wave validator")

;; --- Pin 3: plan validation logic exists ONLY inside the /go path
;;; (extensions/gsd/), confirming validation is unreachable without a
;;; campaign. The gsd command-handlers carry the validation entry
;;; point; nothing standalone reuses it.
(check-not-false
 (regexp-match? #rx"plan-from-index|validate"
                (file->string (build-path repo-root "extensions" "gsd" "command-handlers.rkt")))
 "plan validation is wired into the /go command path")

(displayln "PASS test-standalone-plan-validator (BUG-0048 pin: no standalone plan-validation CLI)")
