#lang racket/base

;; util/package-audit.rkt -- Re-export facade (ARCH-03)
;;
;; *** LAYER EXCEPTION (ARCH-03) — RELOCATED MODULE ***
;; The implementation was moved to extensions/package-audit.rkt because
;; it imports extensions/manifest.rkt, violating util's leaf-only contract.
;; This file re-exports the full public API so existing consumers
;; (e.g. tests/test-package-audit.rkt) continue to work unchanged.

(require "../extensions/package-audit.rkt")

(provide (all-from-out "../extensions/package-audit.rkt"))
