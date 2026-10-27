#lang racket/base
;; @covers extensions/gsd/planning-paths.rkt

;; @speed fast  ;; @suite extensions
;; @boundary unit

;; BOUNDARY: unit

;; tests/test-gsd-planning-paths.rkt — one pure planning-path vocabulary.
;;
;; GSD-SIMPLIFICATION W1: the builders are PURE (no I/O, no existence probes,
;; no symlink policy). These tests pin the PERSISTED LAYOUT: every builder must
;; equal the literal path the owned producers built before the consolidation,
;; so no campaign, snapshot, review session or report moves on disk.

(require racket/file
         racket/path
         rackunit
         "../extensions/gsd/planning-paths.rkt")

(define (p->s p)
  (path->string p))

(define base "/tmp/q-planning-paths-fixture")

(test-case "planning tree builders match the historical literal layout"
  (check-equal? (p->s (planning-root base)) "/tmp/q-planning-paths-fixture/.planning")
  (check-equal? (p->s (planning-path base "waves/W1-x.md"))
                "/tmp/q-planning-paths-fixture/.planning/waves/W1-x.md")
  (check-equal? (p->s (plan-file base)) "/tmp/q-planning-paths-fixture/.planning/PLAN.md")
  (check-equal? (p->s (state-file base)) "/tmp/q-planning-paths-fixture/.planning/STATE.md")
  (check-equal? (p->s (plan-file base))
                (p->s (build-path base ".planning" "PLAN.md"))
                "PLAN.md builder equals the pre-W1 literal")
  (check-equal? (p->s (state-file base))
                (p->s (build-path base ".planning" "STATE.md"))
                "STATE.md builder equals the pre-W1 literal"))

(test-case "wave doc builders keep the classic W<idx>-<slug>.md synthesis"
  (check-equal? (wave-doc-rel-path 2 "harden-retry") "waves/W2-harden-retry.md")
  (check-equal? (p->s (waves-dir base)) "/tmp/q-planning-paths-fixture/.planning/waves")
  (check-equal? (p->s (wave-doc-file base 2 "harden-retry"))
                (p->s (build-path base ".planning" "waves" "W2-harden-retry.md"))
                "wave doc path equals the pre-W1 literal"))

(test-case "campaign builders keep .planning/campaigns/<id> layout"
  (define id (string-append (make-string 64 #\a)))
  (check-equal? (p->s (campaigns-root base)) "/tmp/q-planning-paths-fixture/.planning/campaigns")
  (check-equal? (p->s (campaign-dir base id))
                (format "/tmp/q-planning-paths-fixture/.planning/campaigns/~a" id))
  (check-equal? (p->s (campaign-path base id "binding-w1"))
                (format "/tmp/q-planning-paths-fixture/.planning/campaigns/~a/binding-w1" id))
  (check-equal? (campaign-record-filename id) (format "~a.rktd" id))
  (check-equal? (campaign-record-rel-path id) (format "campaigns/~a.rktd" id))
  (check-equal? (p->s (campaign-record-file base id))
                (format "/tmp/q-planning-paths-fixture/.planning/campaigns/~a.rktd" id)
                "record path equals the pre-W1 literal")
  (check-equal? (p->s (campaign-lint-verdict-file base id))
                (format "/tmp/q-planning-paths-fixture/.planning/campaigns/~a/lint-verdict.rktd" id)))

(test-case "snapshot builders keep the immutable snapshot boundary"
  (define id (string-append (make-string 64 #\b)))
  (check-equal? (p->s (campaign-snapshot-dir base id))
                (format "/tmp/q-planning-paths-fixture/.planning/campaigns/~a/plan-snapshot" id))
  (check-equal? (p->s (campaign-snapshot-manifest-file base id))
                (format (string-append "/tmp/q-planning-paths-fixture/.planning/campaigns/"
                                       "~a/plan-snapshot/snapshot-manifest.rktd")
                        id)
                "snapshot manifest equals the pre-W1 literal")
  (check-equal? snapshot-manifest-filename "snapshot-manifest.rktd"))

(test-case "review session builders keep the campaign-scoped private root"
  (define id (string-append (make-string 64 #\c)))
  (check-equal? (p->s (campaign-review-sessions-dir base id))
                (format "/tmp/q-planning-paths-fixture/.planning/campaigns/~a/review-sessions" id))
  (check-equal? (p->s (campaign-review-tool-home-dir base id))
                (format (string-append "/tmp/q-planning-paths-fixture/.planning/campaigns/"
                                       "~a/review-sessions/tool-home")
                        id)
                "review tool-home equals the pre-W1 literal"))

(test-case "report builders keep docs/reports persisted layout"
  (define plan "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
  (check-equal? (p->s (reports-root base)) "/tmp/q-planning-paths-fixture/docs/reports")
  (check-equal? (p->s (wave-evidence-report-dir base))
                "/tmp/q-planning-paths-fixture/docs/reports/gsd-wave-evidence")
  (check-equal? (p->s (wave-review-report-dir base))
                "/tmp/q-planning-paths-fixture/docs/reports/gsd-wave-reviews")
  (check-equal? (p->s (wave-validation-report-dir base))
                "/tmp/q-planning-paths-fixture/docs/reports/gsd-wave-validation")
  (check-equal? (wave-evidence-report-rel-prefix) "docs/reports/gsd-wave-evidence/")
  (check-equal? (wave-review-report-rel-prefix) "docs/reports/gsd-wave-reviews/")
  (check-equal? (wave-validation-report-rel-prefix) "docs/reports/gsd-wave-validation/")
  (check-equal? (wave-evidence-report-rel-path plan 1)
                (format "docs/reports/gsd-wave-evidence/~a-w1.rktd" plan)
                "generation 0 keeps the unsuffixed legacy layout")
  (check-equal? (wave-evidence-report-rel-path plan 1 3)
                (format "docs/reports/gsd-wave-evidence/~a-w1-r3.rktd" plan))
  (check-equal? (p->s (wave-evidence-report-file base plan 1))
                (format "/tmp/q-planning-paths-fixture/docs/reports/gsd-wave-evidence/~a-w1.rktd"
                        plan))
  (check-equal? (p->s (wave-evidence-report-file base plan 1 2))
                (format "/tmp/q-planning-paths-fixture/docs/reports/gsd-wave-evidence/~a-w1-r2.rktd"
                        plan)))

(test-case "canonical wave grammar permits safe dots but rejects parent segments"
  (check-true (wave-doc-rel-path? "waves/W0-racket..9.3.md"))
  (check-false (wave-doc-rel-path? "waves/../W0-racket-9.3.md")))

;; W5: two DISTINCT named grammars, never one narrowed grammar.
;;   * declared-wave-doc-reference is the historical LENIENT lexical grammar
;;     for author-declared arrow targets (nested/custom names accepted);
;;   * canonical-snapshot-wave-doc-* is the frozen-snapshot identity grammar
;;     (top-level W<n>-<slug>.md only).
(test-case "declared lexical grammar stays lenient while canonical snapshot names stay distinct"
  (check-equal? (declared-wave-doc-reference "→ waves/custom/nested-legal.md")
                "waves/custom/nested-legal.md")
  (check-equal? (declared-wave-doc-reference "see waves/W0-fine.md for details")
                "waves/W0-fine.md")
  (check-false (declared-wave-doc-reference "not-a-wave-path.md"))
  (check-false (declared-wave-doc-reference "waves/../escape.md")
               "traversal is not a lexical wave reference")
  ;; The SAFETY layer keeps accepting the historical nested declared path.
  (check-false (unsafe-declared-path-refusal "waves/custom/nested-legal.md"))
  ;; But that historical declared name is deliberately NOT a canonical
  ;; snapshot name: snapshot capture/binding continues to see the same
  ;; narrower vocabulary (the existing bindability advisory names the gap).
  (check-false (wave-doc-rel-path? "waves/custom/nested-legal.md"))
  (check-false (regexp-match? canonical-snapshot-wave-doc-reference-rx
                              "waves/custom/nested-legal.md"))
  ;; Canonical snapshot names are unchanged.
  (check-true (wave-doc-rel-path? "waves/W0-fine.md"))
  (check-equal? (regexp-match* canonical-snapshot-wave-doc-reference-rx
                               "→ waves/W0-fine.md")
                '("waves/W0-fine.md"))
  (check-equal? (regexp-match* canonical-snapshot-wave-doc-reference-rx
                               "→ waves/WAVE-2-fine_2.md")
                '("waves/WAVE-2-fine_2.md")))

(test-case "builders are pure: they create nothing on disk"
  (define root (make-temporary-file "planning-paths-purity-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (void (plan-file root))
     (void (state-file root))
     (void (wave-doc-file root 0 "x"))
     (void (campaign-dir root (make-string 64 #\d)))
     (void (campaign-snapshot-dir root (make-string 64 #\d)))
     (void (campaign-review-tool-home-dir root (make-string 64 #\d)))
     (void (wave-evidence-report-file root "p" 0))
     (check-equal? (directory-list root #:build? #t) '() "no builder creates directories or files"))
   (lambda () (delete-directory/files root #:must-exist? #f))))
