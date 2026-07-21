#lang racket/base

;; skills/skill-loader.rkt — backward-compatible re-export
;;
;; Skill loading functions are defined in skills/resource-loader.rkt.
;; This module re-exports all resource-loader bindings so that any existing
;; code requiring "skills/skill-loader.rkt" continues to work.

(require "resource-loader.rkt")

(provide ;; Re-export resource loader types
 (struct-out resource)
         (struct-out resource-set)
         (rename-out [empty-resource-set empty-resource-set]
                     [load-global-resources load-global-resources]
                     [load-project-resources load-project-resources]
                     [merge-resources merge-resources]
                     [skill-summary-text skill-summary-text]
                     [skills-summary-section skills-summary-section]
                     [parse-skill parse-skill]
                     [strip-leading-frontmatter-lines strip-leading-frontmatter-lines]
                     [try-read-file try-read-file]))
