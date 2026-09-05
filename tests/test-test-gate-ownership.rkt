#lang racket

;; @speed fast
;; @suite default
;; @boundary unit

;;; tests/test-test-gate-ownership.rkt — W0 cross-gate ownership map
;;;
;;; Unit/component tests for the deterministic `--gate-ownership-map` mode of
;;; scripts/run-tests/inventory.rkt. These tests are CWD-safe and repository-
;;; discovery-free: they exercise the pure validators/renderers with synthetic
;;; membership tables so the fast suite keeps paying no repository-wide walk
;;; (see W4 for why that matters). The real-repository `--check`
;;; invocation runs once in the wave verify command, not here.

(require rackunit
         rackunit/text-ui
         racket/string
         racket/file
         (only-in racket/list remove-duplicates)
         (prefix-in inv: (file "../scripts/run-tests/inventory.rkt")))

;; ---------------------------------------------------------------
;; Frozen candidate behavior table (W0 freeze + W4/W5/W6 rows)
;; ---------------------------------------------------------------

(test-case "behavior table freezes exactly the nineteen mandated candidate IDs"
  (define ids (map (lambda (r) (hash-ref r 'behavior-id)) inv:v124-behavior-table))
  (check-equal? (length ids) 19)
  (check-equal? (sort ids string<?)
                (sort (list "RETRY-LOGICAL-SEMANTICS-FAST"
                            "RETRY-REAL-TIMER-CANARY"
                            "CWD-INVOCATION-AUDIT-CANARY"
                            "PARTIAL-RESULT-AGENT-SESSION-RETRY-CHAIN"
                            "GSD-WAVE-TIMEOUT-CANCELLATION"
                            "GSD-TIMEOUT-DETERMINISTIC-SEAM-FAST"
                            "GSD-TIMEOUT-REAL-CLOCK-CANARY"
                            "RUNNER-REPOSITORY-DISCOVERY"
                            "RUNNER-DISCOVERY-UNIT-FIXTURE-ROOT"
                            "RUNNER-REPOSITORY-DISCOVERY-L4"
                            "GOLDEN-SESSION-LIFECYCLE"
                            "GSD-DELIVERY-VERIFIER-GIT-SANDBOXES"
                            "GSD-WAVE-WORKTREE-SANDBOXES"
                            "GROUPED-MODE-CHARACTERIZATION"
                            "PRIVATE-FIXTURE-TEMPLATE-CONTRACT"
                            "GOLDEN-SESSION-PRIVATE-TEMPLATE"
                            "GSD-DELIVERY-VERIFIER-PRIVATE-TEMPLATE"
                            "GSD-WAVE-WORKTREE-PRIVATE-TEMPLATE"
                            "GSD-BRANCH-DELIVERY-PRIVATE-TEMPLATE")
                      string<?)))

(test-case "every behavior row carries complete frozen ownership data"
  (for ([row (in-list inv:v124-behavior-table)])
    (define id (hash-ref row 'behavior-id))
    (check-true (string<? "" (hash-ref row 'source-gate "")) (format "~a: source-gate" id))
    (check-true (string<? "" (hash-ref row 'destination-gate "")) (format "~a: destination-gate" id))
    (check-true (pair? (hash-ref row 'members '())) (format "~a: members" id))
    (check-true (string<? "" (hash-ref row 'owner "")) (format "~a: owner" id))
    (check-not-false (member (hash-ref row 'status) '("retained-in-place" "re-tiered"))
                     (format "~a: status" id))
    (check-true (string<? "" (hash-ref row 'wave "")) (format "~a: wave" id))
    (check-true (string<? "" (hash-ref row 'rationale "")) (format "~a: rationale" id))))

(test-case "W0 rows retain destination in the source tier"
  (define w0-rows (filter (lambda (row) (equal? (hash-ref row 'wave) "W0")) inv:v124-behavior-table))
  (check-true (pair? w0-rows) "expected at least one W0 ownership row")
  (for ([row (in-list w0-rows)])
    (check-equal? (hash-ref row 'destination-gate)
                  (hash-ref row 'source-gate)
                  (format "~a: W0 destination must equal source tier" (hash-ref row 'behavior-id)))))

;; ---------------------------------------------------------------
;; Pure validators against synthetic membership
;; ---------------------------------------------------------------

(define (synthetic-memberships #:fast [fast '("tests/synthetic-one.rkt" "tests/synthetic-two.rkt")]
                               #:l4 [l4 '("tests/synthetic-l4.rkt")])
  (hasheq "fast" fast "platform" '() "security" '() "workflows" '() "unit-fast" '() "slow/L4" l4))

(define (synthetic-row #:id [id "SYNTH-BEHAVIOR"]
                       #:source [src "fast"]
                       #:destination [dest src]
                       #:members [members '("tests/synthetic-one.rkt" "tests/synthetic-two.rkt")])
  (hasheq 'behavior-id
          id
          'source-gate
          src
          'destination-gate
          dest
          'members
          members
          'owner
          "test-design"
          'status
          "retained-in-place"
          'wave
          "W1"
          'rationale
          "synthetic row for validator tests"))

(test-case "consistent table + membership yields no errors"
  (check-equal? (inv:gate-ownership-errors (list (synthetic-row)) (synthetic-memberships)) '()))

(test-case "duplicate behavior ID is an error"
  (define errors
    (inv:gate-ownership-errors (list (synthetic-row) (synthetic-row)) (synthetic-memberships)))
  (check-true (for/or ([e (in-list errors)])
                (string-contains? e "duplicate behavior ID"))
              (format "expected duplicate ID error, got: ~a" errors)))

(test-case "member missing from every gate is a missing-destination error"
  (define errors
    (inv:gate-ownership-errors (list (synthetic-row))
                               (synthetic-memberships #:fast '("tests/synthetic-two.rkt"))))
  (check-true (for/or ([e (in-list errors)])
                (string-contains? e "missing destination"))
              (format "expected missing destination error, got: ~a" errors)))

(test-case "undeclared membership drift is an error"
  ;; declared row says both members live in fast; synthetic membership lost one
  (define errors
    (inv:gate-ownership-errors (list (synthetic-row #:source "fast"))
                               (synthetic-memberships #:fast '("tests/synthetic-one.rkt"))))
  (check-true (for/or ([e (in-list errors)])
                (string-contains? e "membership drift"))
              (format "expected membership drift error, got: ~a" errors)))

(test-case "destination not selected by its declared tier is an error"
  (define errors
    (inv:gate-ownership-errors (list (synthetic-row #:destination "slow/L4"))
                               (synthetic-memberships)))
  (check-true (for/or ([e (in-list errors)])
                (string-contains? e "destination not selected"))
              (format "expected destination-not-selected error, got: ~a" errors)))

;; ---------------------------------------------------------------
;; Renderers are deterministic and complete
;; ---------------------------------------------------------------

(test-case "markdown renders every behavior ID and tier"
  (define md (inv:gate-ownership-markdown (list (synthetic-row))))
  (check-true (string-contains? md "SYNTH-BEHAVIOR"))
  (check-true (string-contains? md "fast"))
  (check-true (string-contains? md "retained-in-place"))
  (check-equal? md (inv:gate-ownership-markdown (list (synthetic-row)))))

(test-case "ledger text round-trips as a readable Racket datum"
  (define text (inv:gate-ownership-ledger-text (list (synthetic-row))))
  (define datum (read (open-input-string text)))
  (check-equal? (car datum) 'gate-ownership-ledger)
  (define rows (cadr datum))
  (check-equal? (hash-ref (first rows) 'behavior-id) "SYNTH-BEHAVIOR"))

(test-case "selected-path digest is a stable sha256 hex string"
  (define d (inv:selected-paths-digest '("b.rkt" "a.rkt" "a.rkt")))
  (check-equal? (string-length d) 64)
  (check-equal? d (inv:selected-paths-digest '("a.rkt" "b.rkt")))
  (check-false (equal? d (inv:selected-paths-digest '("a.rkt")))))
