#lang racket/base
;; STABILITY: stable

;; util/version.rkt — single source of truth for q version
;;
;; All modules that need the version string should import from here.
;; Created for Issue #203.
;; Reverted to racket/base for contract-out support.

(require racket/contract)

(provide (contract-out [q-version string?]))

(define q-version "0.76.6")
