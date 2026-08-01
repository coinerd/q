#lang racket/base

;; q/tests/test-goal-evidence-provenance.rkt — W3 v0.99.78 (G-5)
;;
;; Contract: verification evidence is bound to the exact code state it was
;; produced on (base SHA + working-tree hash). If either changes, the
;; evidence is STALE and must be rejected until re-verified. Evidence
;; provenance persists via structured `goal.evidence` session-log entries.

(require rackunit
         racket/file
         racket/string
         (only-in "../runtime/session/session-store.rkt" write-session-version-header!)
         (only-in "../runtime/session/session-store-goal-task.rkt"
                  append-evidence-result!
                  load-goal-evidence)
         (only-in "../runtime/goal/goal-evidence.rkt"
                  make-evidence-provenance
                  evidence-provenance?
                  evidence-provenance-evidence-id
                  evidence-provenance-kind
                  evidence-provenance-base-sha
                  evidence-provenance-tree-hash
                  evidence-current?
                  evidence-stale?
                  reverify-instruction)
         (only-in "../runtime/goal/goal-state.rkt" make-check-result))

(define test-dir (make-temporary-file "goal-evidence-provenance-~a" 'directory))

;; ------------------------------------------------------------
;; Test 1: provenance is current when base SHA + tree hash both match
;; ------------------------------------------------------------

(test-case "evidence provenance: current when base and tree match"
  (define ev
    (make-evidence-provenance #:evidence-id "ev-1"
                              #:kind 'fast-gate
                              #:base-sha "AAAA"
                              #:tree-hash "TA"
                              #:captured-at 1000
                              #:result "PASS"))
  (check-true (evidence-provenance? ev))
  (check-equal? (evidence-provenance-base-sha ev) "AAAA")
  (check-equal? (evidence-provenance-tree-hash ev) "TA")
  (check-equal? (evidence-provenance-kind ev) 'fast-gate)
  (check-true (evidence-current? ev "AAAA" "TA"))
  (check-false (evidence-stale? ev "AAAA" "TA")))

;; ------------------------------------------------------------
;; Test 2: base change (SHA A -> B) marks evidence STALE
;; ------------------------------------------------------------

(test-case "evidence provenance: base change marks evidence STALE"
  (define ev
    (make-evidence-provenance #:evidence-id "ev-1"
                              #:kind 'fast-gate
                              #:base-sha "AAAA"
                              #:tree-hash "TA"
                              #:captured-at 1000
                              #:result "PASS"))
  (check-false (evidence-current? ev "BBBB" "TA")
               "evidence captured on base A is not current after base moved to B")
  (check-true (evidence-stale? ev "BBBB" "TA")))

;; ------------------------------------------------------------
;; Test 3: working-tree change (hash TA -> TB) marks evidence STALE
;; ------------------------------------------------------------

(test-case "evidence provenance: working-tree change marks evidence STALE"
  (define ev
    (make-evidence-provenance #:evidence-id "ev-1"
                              #:kind 'fast-gate
                              #:base-sha "AAAA"
                              #:tree-hash "TA"
                              #:captured-at 1000
                              #:result "PASS"))
  (check-false (evidence-current? ev "AAAA" "TB"))
  (check-true (evidence-stale? ev "AAAA" "TB")))

;; ------------------------------------------------------------
;; Test 4: provenance persists via append-evidence-result!/load-goal-evidence
;; ------------------------------------------------------------

(test-case "evidence provenance: persists via structured goal.evidence entries"
  (define log-path (build-path test-dir "evidence.jsonl"))
  (write-session-version-header! log-path)
  (define ev
    (make-evidence-provenance #:evidence-id "ev-9"
                              #:kind 'check
                              #:base-sha "AAAA"
                              #:tree-hash "TA"
                              #:captured-at 2000
                              #:result (list (make-check-result #:label "unit" #:exit-code 0))))
  (append-evidence-result! log-path ev)
  (define loaded (load-goal-evidence log-path))
  (check-equal? (length loaded) 1 "one evidence entry persists")
  (define restored (car loaded))
  (check-equal? (evidence-provenance-evidence-id restored) "ev-9")
  (check-equal? (evidence-provenance-base-sha restored) "AAAA")
  (check-equal? (evidence-provenance-tree-hash restored) "TA")
  (check-equal? (evidence-provenance-kind restored) 'check)
  ;; Stale detection works on the persisted entry
  (check-true (evidence-current? restored "AAAA" "TA"))
  (check-true (evidence-stale? restored "BBBB" "TA")))

;; ------------------------------------------------------------
;; Test 5: stale evidence is rejected — loop injects a re-verify
;; instruction instead of accepting the old result
;; ------------------------------------------------------------

(test-case "evidence provenance: stale evidence triggers re-verify instruction"
  (define ev
    (make-evidence-provenance #:evidence-id "ev-1"
                              #:kind 'fast-gate
                              #:base-sha "AAAA"
                              #:tree-hash "TA"
                              #:captured-at 1000
                              #:result "PASS"))
  (check-true (evidence-stale? ev "BBBB" "TB"))
  (define instruction (reverify-instruction ev "BBBB" "TB"))
  (check-true (string? instruction))
  (check-true (regexp-match? #rx"re-verify" (string-downcase instruction))
              "re-verify instruction names the required action")
  (check-true (regexp-match? #rx"base" (string-downcase instruction))
              "re-verify instruction names the base change")
  ;; Current evidence needs no re-verify instruction
  (check-false (evidence-stale? ev "AAAA" "TA")))

;; ------------------------------------------------------------
;; Test 6: /goal evidence CLI — render-goal-evidence lists persisted
;; evidence with CURRENT/STALE flags derived from provenance
;; ------------------------------------------------------------

(require (only-in "../tui/commands/goal-bridge.rkt" render-goal-evidence))

(test-case "evidence provenance: /goal evidence renders current/stale flags"
  (define log-path (build-path test-dir "session.log"))
  (write-session-version-header! log-path)
  (define ev-current
    (make-evidence-provenance #:evidence-id "ev-1"
                              #:kind 'fast-gate
                              #:base-sha "AAAA"
                              #:tree-hash "TA"
                              #:captured-at 1000
                              #:result "PASS"))
  (define ev-stale
    (make-evidence-provenance #:evidence-id "ev-2"
                              #:kind 'check
                              #:base-sha "AAAA"
                              #:tree-hash "TA"
                              #:captured-at 2000
                              #:result "PASS"))
  (append-evidence-result! log-path ev-current)
  (append-evidence-result! log-path ev-stale)
  (define rendered (render-goal-evidence log-path "AAAA" "TA"))
  (check-true (string? rendered))
  (check-true (regexp-match? #rx"CURRENT" rendered) "current evidence is flagged CURRENT")
  (check-false (regexp-match? #rx"STALE" rendered)
               "no STALE flag when provenance matches current code state")
  (check-true (regexp-match? #rx"ev-1" rendered))
  (check-true (regexp-match? #rx"ev-2" rendered))
  ;; After a base change (AAAA -> BBBB), both entries are STALE
  (define rendered-stale (render-goal-evidence log-path "BBBB" "TB"))
  (check-false (regexp-match? #rx"CURRENT" rendered-stale) "no CURRENT flag after base change")
  (check-true (regexp-match? #rx"STALE" rendered-stale)
              "evidence captured on the old base is flagged STALE")
  ;; Empty log renders a no-evidence line, not an error
  (define empty-log (build-path test-dir "empty.log"))
  (write-session-version-header! empty-log)
  (define rendered-empty (render-goal-evidence empty-log "AAAA" "TA"))
  (check-true (regexp-match? #rx"No verification evidence" rendered-empty))
  ;; No session log renders a clear message, not an error
  (define rendered-nolog (render-goal-evidence #f "AAAA" "TA"))
  (check-true (regexp-match? #rx"No session log" rendered-nolog)))

;; ------------------------------------------------------------
;; Cleanup
;; ------------------------------------------------------------

(delete-directory/files test-dir)
