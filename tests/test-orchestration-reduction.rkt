#lang racket/base

;; @speed fast  ;; @suite arch

;; tests/test-orchestration-reduction.rkt
;; v0.99.92 W3 — Orchestration Surface Reduction evidence ledger.
;;
;; Validates the machine ledger documenting that no coherent private helper in
;; runtime/session/session-lifecycle.rkt qualifies for extraction after W1/W2
;; thinning. The ledger records the rejection decision, locality metrics, and a
;; classification of every candidate block with source anchors.

(require rackunit
         racket/file
         racket/list
         racket/runtime-path
         racket/string)

(define-runtime-path tests-dir ".")
(define root (simplify-path (build-path tests-dir "..")))
(define ledger-path
  (build-path root "docs" "architecture" "orchestration-surface-reduction-v0.99.92.rktd"))

(define expected-candidate-ids
  '(busy-error-construction input-hook-handling
                            rollback-prompt-scope
                            acknowledgement-tracer
                            cleanup-turn-completed
                            emergency-persist))

(define (read-one path)
  (call-with-input-file path
                        (lambda (in)
                          (define datum (read in))
                          (check-true (eof-object? (read in)) "ledger must contain exactly one datum")
                          datum)))

(define (locator-parts locator)
  (string-split locator ":" #:trim? #f))

(define (check-locator id locator)
  (define path (build-path root (car (locator-parts locator))))
  (check-true (file-exists? path) (format "~a evidence file absent: ~a" id locator))
  (define anchor (string-join (cdr (locator-parts locator)) ":"))
  (check-true (string-contains? (file->string path) anchor)
              (format "~a evidence anchor absent: ~a" id locator)))

(define (exact-id-bijection label expected entries)
  (define ids (map (lambda (entry) (hash-ref entry 'id)) entries))
  (check-equal? (sort ids symbol<?) (sort expected symbol<?) label)
  (check-equal? (length ids) (length (remove-duplicates ids)) (format "~a IDs must be unique" label)))

(test-case "W3-1: rejection decision and baseline metrics are recorded"
  (define ledger (read-one ledger-path))
  (check-equal? (hash-ref ledger 'schema-version) 1)
  (check-eq? (hash-ref ledger 'wave) 'W3)
  (check-false (hash-ref ledger 'production-change))
  (check-eq? (hash-ref ledger 'decision) 'rejection)
  (define metrics (hash-ref ledger 'session-lifecycle-metrics))
  (check-equal? (hash-ref metrics 'loc) 566)
  (check-equal? (hash-ref metrics 'fan-out) 38)
  (check-equal? (hash-ref metrics 'hotspot-score) 7924)
  (check-true (pair? (hash-ref metrics 'co-change))))

(test-case "W3-2: every candidate block is classified with a valid verdict and rationale"
  (define candidates (hash-ref (read-one ledger-path) 'candidates))
  (exact-id-bijection "candidate blocks" expected-candidate-ids candidates)
  (for ([candidate (in-list candidates)])
    (define id (hash-ref candidate 'id))
    (check-not-false (member (hash-ref candidate 'verdict) '(reject defer-to-w4 accept))
                     (format "~a invalid verdict" id))
    (check-true (string? (hash-ref candidate 'rationale)))
    (check-true (exact-positive-integer? (hash-ref candidate 'lines)))
    (check-locator id (hash-ref candidate 'anchor)))
  (check-true (= (length (filter (lambda (c) (eq? (hash-ref c 'verdict) 'reject)) candidates)) 5)
              "five blocks rejected")
  (check-true (= (length (filter (lambda (c) (eq? (hash-ref c 'verdict) 'defer-to-w4)) candidates)) 1)
              "W3 historically deferred rollback scope to W4"))

(test-case "W3-F2 terminal follow-up records later extraction without rewriting W3 history"
  (define ledger (read-one ledger-path))
  (define dispositions (hash-ref ledger 'terminal-dispositions))
  (check-equal? (length dispositions) 1)
  (define disposition (car dispositions))
  (check-eq? (hash-ref disposition 'id) 'W3-F2)
  (check-eq? (hash-ref disposition 'version) 'v0.99.93)
  (check-equal? (hash-ref disposition 'issue) 9281)
  (check-eq? (hash-ref disposition 'disposition) 'extracted)
  (check-locator 'W3-F2-terminal (hash-ref disposition 'anchor))
  (check-locator 'W3-F2-evidence (hash-ref disposition 'evidence)))

(test-case "W3-3: existing primitives are already extracted and the rejection is non-vacuous"
  (define ledger (read-one ledger-path))
  (define primitives (hash-ref ledger 'existing-primitives))
  (check-true (>= (length primitives) 3))
  (for ([primitive (in-list primitives)])
    (check-true (symbol? (hash-ref primitive 'capability)))
    (define module-sym (hash-ref primitive 'module))
    (check-true (symbol? module-sym))
    (check-true (file-exists? (build-path root (symbol->string module-sym)))
                (format "primitive module absent: ~a" module-sym)))
  (define findings (hash-ref ledger 'findings))
  (check-equal? (map (lambda (finding) (hash-ref finding 'id)) findings) '(W3-F1 W3-F2))
  (for ([finding (in-list findings)])
    (check-true (string-contains? (hash-ref finding 'follow-up) "#")
                (format "~a must name an accountable issue" (hash-ref finding 'id)))))
