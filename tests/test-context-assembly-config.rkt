#lang racket/base

;; @skip BROKEN: Tests obsolete 12-field context-assembly-config struct.
;; The config was refactored to a 4-field struct (recent-tokens,
;; max-catalog-entries, max-catalog-tokens, summary-window) in budgeting.rkt.
;; These fields (state-aware?, graph-selection?, conclusion-token-budget, etc.)
;; no longer exist as struct fields — they are now runtime parameters in config.rkt.
;; Would need complete rewrite. Deferred to future test debt cleanup.

;; @speed fast
;; @suite default

(module+ test
  (void))
