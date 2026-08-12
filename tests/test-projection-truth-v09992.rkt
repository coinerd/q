#lang racket/base

;; @speed fast  ;; @suite arch

;; tests/test-projection-truth-v09992.rkt
;; v0.99.92 W5 — MA-07 projection hygiene.
;;
;; Enforces that the tracked planning projections are consistent with the
;; terminal maintainability ledger: every completed wave (W0..W4) is DONE in
;; STATE/VALIDATION, MA-10 is CLOSED, MA-11/12 are GUARDED, and no MA finding
;; is unassigned (W5 can close the series).

(require rackunit
         racket/file
         racket/runtime-path
         racket/string)

(define-runtime-path tests-dir ".")
(define root (simplify-path (build-path tests-dir "..")))
(define state-path
  (build-path root "docs" "planning" "STATE-v0.99.92-SESSION-LIFECYCLE-SERIES-CLOSURE.md"))
(define validation-path
  (build-path root "docs" "planning" "VALIDATION-v0.99.92-SESSION-LIFECYCLE-SERIES-CLOSURE.md"))
(define ledger-path (build-path root "docs" "architecture" "maintainability-terminal-v0.99.92.rktd"))

(test-case "W5-1: every completed wave W0..W4 is DONE in STATE and VALIDATION"
  (define state (file->string state-path))
  (define validation (file->string validation-path))
  (for ([wave (in-list '("W0" "W1" "W2" "W3" "W4"))])
    (check-true (regexp-match? (regexp (format "\\| ~a \\| ✅ DONE" wave)) state)
                (format "STATE missing DONE for ~a" wave))
    (check-true (regexp-match? (regexp (format "\\| ~a \\| .*✅ DONE" wave)) validation)
                (format "VALIDATION missing DONE for ~a" wave)))
  (check-true (regexp-match? #rx"W5" state) "STATE lists W5"))

(test-case "W5-2: terminal MA dispositions are reflected in the projections"
  (define state (file->string state-path))
  (check-true (regexp-match? #rx"MA-10 CLOSED" state) "STATE records MA-10 CLOSED")
  (check-true (regexp-match? #rx"MA-11 and MA-12 GUARDED" state) "STATE records MA-11/12 GUARDED"))

(test-case "W5-3: terminal ledger has no unassigned open MA finding"
  (define ledger (call-with-input-file ledger-path read))
  (define findings (hash-ref ledger 'findings))
  (for ([finding (in-list findings)])
    (define disposition (hash-ref finding 'disposition))
    (check-not-false (member disposition '(CLOSED PARTIAL OPEN REJECTED GUARDED))
                     (format "~a invalid disposition" (hash-ref finding 'id))))
  (check-false (ormap (lambda (f) (eq? (hash-ref f 'disposition) 'OPEN)) findings)
               "no MA finding may remain OPEN at series closure")
  (check-true (ormap (lambda (f) (eq? (hash-ref f 'id) 'MA-10)) findings)))
