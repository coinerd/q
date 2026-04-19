#lang racket/base

;; util/version.rkt — single source of truth for q version
;;
;; All modules that need the version string should import from here.
;; Created for Issue #203.

(provide q-version)

(define q-version "0.11.2")
