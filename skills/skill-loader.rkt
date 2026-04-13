#lang racket/base

;; skills/skill-loader.rkt — backward-compatible re-export
;;
;; Skill loading functions are defined in skills/resource-loader.rkt.
;; This module re-exports all resource-loader bindings so that any existing
;; code requiring "skills/skill-loader.rkt" continues to work.

(require "resource-loader.rkt")

(provide ;; Re-export resource loader types
 (all-from-out "resource-loader.rkt"))
