#lang racket/base

;; util/extensions.rkt — Extension struct and accessors (foundation layer)
;;
;; Extracted from extensions/api.rkt (v0.33.0 W1: fix upward imports).
;; The extension struct is defined here (foundation) so that both
;; runtime/ and extensions/ can import it without upward dependencies.
;;
;; This module provides ONLY the data type and pure accessors.
;; The extension registry (extension-registry, register-extension!, etc.)
;; remains in extensions/api.rkt.

(provide (struct-out extension))

;; ============================================================
;; Extension struct
;; ============================================================

(struct extension (name version api-version hooks)
  #:transparent
  #:guard (lambda (name version api-version hooks type)
            (unless (string? name)
              (raise-argument-error 'extension "string?" name))
            (unless (string? version)
              (raise-argument-error 'extension "string?" version))
            (unless (string? api-version)
              (raise-argument-error 'extension "string?" api-version))
            (unless (hash? hooks)
              (raise-argument-error 'extension "hash?" hooks))
            (values name version api-version hooks)))
;; name       : string?
;; version    : string?
;; api-version: string?
;; hooks      : (hash/c symbol? procedure?)
