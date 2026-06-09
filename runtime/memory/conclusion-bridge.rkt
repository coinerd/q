#lang racket/base
;; runtime/memory/conclusion-bridge.rkt — Persist high-value conclusions to memory
;; STABILITY: evolving
;;
;; GAP-10: On session termination, evaluate task conclusions and persist
;; high-value ones (decisions, patterns) as project-scoped memory items.

(require (only-in "types.rkt" memory-item)
         (only-in "backends/helpers.rkt" current-iso-8601)
         (only-in "protocol.rkt" gen:store-memory!)
         (only-in "../context-assembly/task-conclusion.rkt"
                  task-conclusion
                  task-conclusion-id
                  task-conclusion-text
                  task-conclusion-category
                  task-conclusion-relevance-tags))

(provide current-conclusion-to-memory-bridge-enabled
         persist-high-value-conclusions!)

(define current-conclusion-to-memory-bridge-enabled (make-parameter #f))

(define high-value-categories '(decision pattern))

(define (persist-high-value-conclusions! conclusions
                                         #:backend backend
                                         #:session-id session-id
                                         #:project-root [project-root "."])
  (when (and (current-conclusion-to-memory-bridge-enabled) backend)
    (for ([c (in-list conclusions)]
          #:when (memq (task-conclusion-category c) high-value-categories))
      (define item
        (memory-item (format "conc_~a" (task-conclusion-id c))
                     'semantic
                     'project
                     (task-conclusion-text c)
                     (hasheq 'source
                             'conclusion-bridge
                             'session-id
                             session-id
                             'project-root
                             project-root
                             'original-conclusion-id
                             (task-conclusion-id c)
                             'category
                             (task-conclusion-category c)
                             'tags
                             (task-conclusion-relevance-tags c))
                     (hasheq 'sensitivity 'public 'confidence 0.8)
                     (current-iso-8601)
                     (current-iso-8601)))
      (gen:store-memory! backend item))))
