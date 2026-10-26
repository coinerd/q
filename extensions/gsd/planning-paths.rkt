#lang racket/base

;; extensions/gsd/planning-paths.rkt — THE pure planning-path vocabulary.
;;
;; GSD-SIMPLIFICATION W1 (canonical path ownership): every path this module
;; builds is a pure function of a repository base directory plus the artifact
;; identity. No I/O, no existence probes, no symlink policy, no normalization —
;; link safety remains owned by the readers that already enforce it
;; (wave-docs.rkt's declared-path resolver, plan-snapshot.rkt's snapshot
;; boundary checks). Persisted layouts are byte-for-byte unchanged:
;;
;;   <base>/.planning/PLAN.md
;;   <base>/.planning/STATE.md
;;   <base>/.planning/waves/W<n>-<slug>.md
;;   <base>/.planning/campaigns/<campaign-id>/...
;;   <base>/.planning/campaigns/<campaign-id>/plan-snapshot/snapshot-manifest.rktd
;;   <base>/.planning/campaigns/<campaign-id>/review-sessions/tool-home
;;   <base>/docs/reports/gsd-wave-evidence/<plan-id>-w<n>[-r<gen>].rktd
;;   <base>/docs/reports/gsd-wave-reviews/
;;   <base>/docs/reports/gsd-wave-validation/
;;
;; The wave-path GRAMMAR (which declared `waves/…` spellings are safe) is NOT
;; defined here: it stays in wave-docs.rkt so this module cannot silently
;; widen what an existing bound campaign may resolve.

(require racket/path)

(provide planning-dir-name
         waves-dir-name
         campaigns-dir-name
         plan-snapshot-dir-name
         snapshot-manifest-filename
         review-sessions-dir-name
         review-tool-home-name
         lint-verdict-filename
         reports-dir-name
         wave-evidence-report-dir-name
         wave-review-report-dir-name
         wave-validation-report-dir-name
         planning-root
         planning-path
         plan-file
         state-file
         waves-dir
         wave-doc-rel-path
         wave-doc-file
         campaigns-root
         campaign-dir
         campaign-path
         campaign-record-filename
         campaign-record-rel-path
         campaign-record-file
         campaign-snapshot-dir
         campaign-snapshot-manifest-file
         campaign-review-sessions-dir
         campaign-review-tool-home-dir
         campaign-lint-verdict-file
         reports-root
         wave-evidence-report-dir
         wave-review-report-dir
         wave-validation-report-dir
         wave-evidence-report-rel-path
         wave-evidence-report-file)

;; ============================================================
;; Layout vocabulary (single source for these directory/file names)
;; ============================================================

(define planning-dir-name ".planning")
(define waves-dir-name "waves")
(define campaigns-dir-name "campaigns")
(define plan-snapshot-dir-name "plan-snapshot")
(define snapshot-manifest-filename "snapshot-manifest.rktd")
(define review-sessions-dir-name "review-sessions")
(define review-tool-home-name "tool-home")
(define lint-verdict-filename "lint-verdict.rktd")
(define reports-dir-name "reports")
(define wave-evidence-report-dir-name "gsd-wave-evidence")
(define wave-review-report-dir-name "gsd-wave-reviews")
(define wave-validation-report-dir-name "gsd-wave-validation")

;; ============================================================
;; .planning tree
;; ============================================================

;; planning-root : path-string? -> path?
(define (planning-root base-dir)
  (build-path base-dir planning-dir-name))

;; planning-path : path-string? (or/c path? string?) ... -> path?
;; Resolve one .planning-relative artifact. Relative strings may contain
;; separators (e.g. "waves/W1-x.md"); they are still relative to .planning.
(define (planning-path base-dir rel-path)
  (build-path (planning-root base-dir) rel-path))

(define (plan-file base-dir)
  (planning-path base-dir "PLAN.md"))

(define (state-file base-dir)
  (planning-path base-dir "STATE.md"))

(define (waves-dir base-dir)
  (planning-path base-dir waves-dir-name))

;; wave-doc-rel-path : exact-nonnegative-integer? string? -> string?
;; The classic W<idx>-<slug>.md synthesis (arrowless legacy authoring). A
;; declared arrow path is used verbatim by its owner and never rebuilt here.
(define (wave-doc-rel-path idx slug)
  (format "~a/W~a-~a.md" waves-dir-name idx slug))

(define (wave-doc-file base-dir idx slug)
  (planning-path base-dir (wave-doc-rel-path idx slug)))

;; ============================================================
;; Campaigns
;; ============================================================

(define (campaigns-root base-dir)
  (planning-path base-dir campaigns-dir-name))

(define (campaign-dir base-dir campaign-id)
  (build-path (campaigns-root base-dir) campaign-id))

(define (campaign-path base-dir campaign-id name)
  (build-path (campaign-dir base-dir campaign-id) name))

(define (campaign-record-filename plan-id)
  (string-append plan-id ".rktd"))

(define (campaign-record-rel-path plan-id)
  (format "~a/~a" campaigns-dir-name (campaign-record-filename plan-id)))

(define (campaign-record-file base-dir plan-id)
  (planning-path base-dir (campaign-record-rel-path plan-id)))

;; Immutable plan snapshot boundary. Snapshot link/marker policy stays in
;; plan-snapshot.rkt; this module only names the location.
(define (campaign-snapshot-dir base-dir campaign-id)
  (campaign-path base-dir campaign-id plan-snapshot-dir-name))

(define (campaign-snapshot-manifest-file base-dir campaign-id)
  (build-path (campaign-snapshot-dir base-dir campaign-id) snapshot-manifest-filename))

;; Reviewer session scratch, scoped to one campaign.
(define (campaign-review-sessions-dir base-dir campaign-id)
  (campaign-path base-dir campaign-id review-sessions-dir-name))

(define (campaign-review-tool-home-dir base-dir campaign-id)
  (build-path (campaign-review-sessions-dir base-dir campaign-id) review-tool-home-name))

;; Recorded wave-doc lint verdict (write-once at campaign creation).
(define (campaign-lint-verdict-file base-dir plan-id)
  (campaign-path base-dir plan-id lint-verdict-filename))

;; ============================================================
;; Committed report paths (docs/reports)
;; ============================================================
;; Report paths are derived from campaign+wave identity, never from a
;; checkout. Generation 0 is the unsuffixed legacy layout.

(define (reports-root base-dir)
  (build-path base-dir "docs" reports-dir-name))

(define (wave-evidence-report-dir base-dir)
  (build-path (reports-root base-dir) wave-evidence-report-dir-name))

(define (wave-review-report-dir base-dir)
  (build-path (reports-root base-dir) wave-review-report-dir-name))

(define (wave-validation-report-dir base-dir)
  (build-path (reports-root base-dir) wave-validation-report-dir-name))

;; wave-evidence-report-rel-path : string? exact-nonnegative-integer?
;;                                 [exact-nonnegative-integer?] -> string?
(define (wave-evidence-report-rel-path plan-id wave [generation 0])
  (format "docs/~a/~a/~a-w~a~a.rktd"
          reports-dir-name
          wave-evidence-report-dir-name
          plan-id
          wave
          (if (zero? generation)
              ""
              (format "-r~a" generation))))

(define (wave-evidence-report-file base-dir plan-id wave [generation 0])
  (build-path (reports-root base-dir)
              wave-evidence-report-dir-name
              (format "~a-w~a~a.rktd"
                      plan-id
                      wave
                      (if (zero? generation)
                          ""
                          (format "-r~a" generation)))))
