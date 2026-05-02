#lang typed/racket
;; STABILITY: stable

;; util/version.rkt — single source of truth for q version
;;
;; All modules that need the version string should import from here.
;; Created for Issue #203.
;; Migrated to Typed Racket in v0.22.6 W5 (RKT-01 pilot).

(provide q-version)

(: q-version String)
(define q-version "0.28.3")
