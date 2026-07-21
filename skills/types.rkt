#lang racket/base

;; skills/types.rkt — facade re-exporting from sub-modules
;;
;; Split from the original monolithic types.rkt (Issue #205, QUAL-09).
;; Now re-exports everything from:
;;   - resource-loader.rkt (resource types, loading, merging)
;;   - template.rkt (template rendering)
;;
;; All existing consumers importing skills/types.rkt continue to work unchanged.

(require "resource-loader.rkt"
         "template.rkt")

(provide ;; Re-export resource loader and template types
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
                     [try-read-file try-read-file])
         render-template
         render-template-with-positional-args)
