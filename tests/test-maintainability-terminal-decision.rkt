#lang racket/base

;; @speed fast  ;; @suite arch

;; tests/test-maintainability-terminal-decision.rkt
;; v0.99.92 W4 — Repository-wide Maintainability Reassessment terminal ledger.
;;
;; Validates the machine ledger freezing the terminal disposition of every
;; MA-01..MA-12 finding plus the W0-F1..F5 / W3-F1/F2 wave findings.

(require rackunit
         racket/file
         racket/list
         racket/runtime-path
         racket/string)

(define-runtime-path tests-dir ".")
(define root (simplify-path (build-path tests-dir "..")))
(define ledger-path (build-path root "docs" "architecture" "maintainability-terminal-v0.99.92.rktd"))

(define expected-ma-ids '(MA-01 MA-02 MA-03 MA-04 MA-05 MA-06 MA-07 MA-08 MA-09 MA-10 MA-11 MA-12))
(define valid-dispositions '(CLOSED PARTIAL OPEN REJECTED GUARDED))
(define expected-wave-ids '(W0-F1 W0-F2 W0-F3 W0-F4 W0-F5 W3-F1 W3-F2))

(define (read-one path)
  (call-with-input-file path
                        (lambda (in)
                          (define datum (read in))
                          (check-true (eof-object? (read in)) "ledger must contain exactly one datum")
                          datum)))

(define (check-locator id locator)
  (define path (build-path root (car (string-split locator ":" #:trim? #f))))
  (check-true (file-exists? path) (format "~a evidence file absent: ~a" id locator)))

(define (exact-id-bijection label expected entries)
  (define ids (map (lambda (entry) (hash-ref entry 'id)) entries))
  (check-equal? (sort ids symbol<?) (sort expected symbol<?) label)
  (check-equal? (length ids) (length (remove-duplicates ids)) (format "~a IDs must be unique" label)))

(test-case "W4-1: terminal reassessment covers the exact MA-01..MA-12 bijection"
  (define ledger (read-one ledger-path))
  (check-equal? (hash-ref ledger 'schema-version) 1)
  (check-eq? (hash-ref ledger 'wave) 'W4)
  (check-false (hash-ref ledger 'production-change))
  (define findings (hash-ref ledger 'findings))
  (exact-id-bijection "MA findings" expected-ma-ids findings)
  (for ([finding (in-list findings)])
    (define id (hash-ref finding 'id))
    (check-not-false (member (hash-ref finding 'disposition) valid-dispositions)
                     (format "~a invalid disposition" id))
    (check-true (pair? (hash-ref finding 'evidence)) (format "~a needs evidence" id))
    (for ([locator (in-list (hash-ref finding 'evidence))])
      (check-locator id locator))))

(test-case "W4-2: MA-10 terminal decision and MA-11/MA-12 guards are recorded"
  (define findings (hash-ref (read-one ledger-path) 'findings))
  (define ma10 (findf (lambda (f) (eq? (hash-ref f 'id) 'MA-10)) findings))
  (check-eq? (hash-ref ma10 'disposition) 'CLOSED)
  (check-true (string? (hash-ref ma10 'closure-proof)))
  (define metrics (hash-ref ma10 'metrics))
  (check-equal? (hash-ref metrics 'loc) 566)
  (check-equal? (hash-ref metrics 'fan-out) 38)
  (check-equal? (hash-ref metrics 'changed-commits) 13)
  (check-equal? (hash-ref metrics 'hotspot) 7358)
  (check-true (hash-ref metrics 'pure-modules-testable-without-session))
  (define ma11 (findf (lambda (f) (eq? (hash-ref f 'id) 'MA-11)) findings))
  (define ma12 (findf (lambda (f) (eq? (hash-ref f 'id) 'MA-12)) findings))
  (check-eq? (hash-ref ma11 'disposition) 'GUARDED)
  (check-eq? (hash-ref ma12 'disposition) 'GUARDED))

(test-case "W4-3: every wave finding has an accountable disposition and follow-up"
  (define wave-findings (hash-ref (read-one ledger-path) 'wave-findings))
  (exact-id-bijection "wave findings" expected-wave-ids wave-findings)
  (for ([finding (in-list wave-findings)])
    (define id (hash-ref finding 'id))
    (check-not-false (member (hash-ref finding 'disposition) '(DEFERRED SEPARATE_MILESTONE CLOSED))
                     (format "~a invalid disposition" id))
    (check-true (symbol? (hash-ref finding 'owner)))
    (define follow-up (hash-ref finding 'follow-up))
    (unless (equal? follow-up "none")
      (check-true (regexp-match? #rx"^#[0-9]+$" follow-up)
                  (format "~a follow-up must be an issue or none" id)))
    (check-true (string? (hash-ref finding 'summary)))))
