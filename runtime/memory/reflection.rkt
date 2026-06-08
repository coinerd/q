#lang racket/base
;; runtime/memory/reflection.rkt — Deterministic memory reflection
;;
;; v0.95.17 W5: Group session memories by shared tags or token overlap,
;; produce deterministic semantic reflection items at project scope.
;;
;; Design:
;;   - Reads session-scoped memories
;;   - Groups by shared tags or content-token overlap (Jaccard >= 0.3)
;;   - Merges each group into a single semantic item at project scope
;;   - Each merged item supersedes its source session items
;;   - No-op when < 2 items or no groupable items found
;;   - Deterministic: sorted input, stable grouping, deterministic IDs

(require racket/list
         racket/set
         racket/string
         "types.rkt"
         "protocol.rkt"
         (only-in "search.rkt" tokenize)
         (only-in "backends/helpers.rkt" current-iso-8601)
         (only-in "service.rkt"
                  current-memory-backend
                  current-auto-reflection-enabled
                  current-auto-reflection-min-items))

;; ---------------------------------------------------------------------------
;; Parameters
;; ---------------------------------------------------------------------------

(define current-reflection-min-group-size (make-parameter 3))
(define current-reflection-overlap-threshold (make-parameter 0.4))

;; ---------------------------------------------------------------------------
;; Grouping logic
;; ---------------------------------------------------------------------------

;; Compute Jaccard similarity between two token sets (as sorted lists)
(define (jaccard-similarity tokens-a tokens-b)
  (define set-a (list->set tokens-a))
  (define set-b (list->set tokens-b))
  (define intersection-size (set-count (set-intersect set-a set-b)))
  (define union-size (set-count (set-union set-a set-b)))
  (if (zero? union-size)
      0.0
      (/ intersection-size union-size 1.0)))

;; Check if two items share at least 2 tags (conservative grouping)
(define (shared-tags? a b)
  (define tags-a (hash-ref (memory-item-metadata a) 'tags '()))
  (define tags-b (hash-ref (memory-item-metadata b) 'tags '()))
  (define shared-count (for/sum ([ta (in-list tags-a)]) (if (member ta tags-b) 1 0)))
  (>= shared-count 2))

;; Check if two items are "related" (shared tags OR token overlap >= threshold)
(define (items-related? a b threshold)
  (or (shared-tags? a b)
      (>= (jaccard-similarity (tokenize (memory-item-content a)) (tokenize (memory-item-content b)))
          threshold)))

;; Build groups using single-linkage clustering
;; Deterministic: items sorted by id before grouping
(define (group-items items threshold)
  (define sorted (sort items string<? #:key memory-item-id))
  ;; Each item starts in its own group
  (define groups
    (for/list ([item (in-list sorted)])
      (list item)))
  ;; Merge groups that have any related pair
  (let loop ([groups groups])
    (define-values (merged-groups did-merge)
      (for/fold ([acc '()]
                 [merged? #f])
                ([group (in-list groups)])
        (define existing
          (for/first ([g (in-list acc)]
                      #:when (pair-related? g group threshold))
            g))
        (if existing
            (values (cons (append existing group) (remove existing acc)) #t)
            (values (cons group acc) merged?))))
    (if did-merge
        (loop merged-groups)
        merged-groups)))

;; Check if any item in group-a is related to any item in group-b
(define (pair-related? group-a group-b threshold)
  (for*/or ([a (in-list group-a)]
            [b (in-list group-b)])
    (items-related? a b threshold)))

;; ---------------------------------------------------------------------------
;; Merging
;; ---------------------------------------------------------------------------

;; Merge a group of items into a single reflection summary
(define (merge-group-items items)
  (define contents (map memory-item-content items))
  (define ids (map memory-item-id items))
  ;; Collect all unique tags from sources
  (define all-tags
    (remove-duplicates (apply append
                              (for/list ([item (in-list items)])
                                (hash-ref (memory-item-metadata item) 'tags '())))))
  ;; Build deterministic merged content
  (define merged-text (string-append "[reflection] " (string-join (sort contents string<?) "; ")))
  (values merged-text ids all-tags))

;; Build a memory-item for a reflection
(define (make-reflection-item merged-content source-ids tags project-root session-id)
  ;; Deterministic ID: hash of sorted source IDs, no wall-clock
  (define id (format "refl_~a" (abs (equal-hash-code (sort source-ids string<?)))))
  (define now (current-iso-8601))
  (memory-item id
               'semantic
               'project
               merged-content
               (hash 'source
                     'reflection
                     'session-id
                     (or session-id "unknown")
                     'project-root
                     (or project-root ".")
                     'tags
                     tags
                     'origin-tool-call-id
                     "reflect-session-memories")
               (hash 'sensitivity 'public 'confidence 0.7 'supersedes source-ids)
               now
               now))

;; ---------------------------------------------------------------------------
;; Main entrypoint
;; ---------------------------------------------------------------------------

;; Reflect session memories into project-scope semantic items.
;; Returns a list of newly created reflection items (empty if no-op).
(define (reflect-session-memories! backend
                                   #:session-id session-id
                                   #:project-root [project-root #f]
                                   #:min-group-size [min-size (current-reflection-min-group-size)]
                                   #:overlap-threshold
                                   [threshold (current-reflection-overlap-threshold)])
  (cond
    [(not backend) '()]
    [else
     ;; Retrieve session-scoped items
     (define q (memory-query "" 'session project-root session-id #f #f 100 #f))
     (define result (gen:retrieve-memory backend q))
     (cond
       [(not (memory-result-ok? result)) '()]
       [else
        (define items (memory-result-value result))
        (cond
          [(< (length items) min-size) '()]
          [else
           (define groups (group-items items threshold))
           ;; Only keep groups with >= min-size items
           (define valid-groups (filter (lambda (g) (>= (length g) min-size)) groups))
           (cond
             [(null? valid-groups) '()]
             [else
              (filter memory-item?
                      (for/list ([group (in-list valid-groups)])
                        (define-values (merged-text source-ids tags) (merge-group-items group))
                        (define refl-item
                          (make-reflection-item merged-text source-ids tags project-root session-id))
                        (define store-result (gen:store-memory! backend refl-item))
                        (if (memory-result-ok? store-result) refl-item #f)))])])])]))

;; v0.95.21 W3: Non-fatal auto-reflection trigger.
;; Gated by current-auto-reflection-enabled and current-auto-reflection-min-items.
;; Catches all exceptions to prevent reflection errors from disrupting the agent loop.
(define (maybe-reflect-session-memories! #:session-id session-id
                                         #:project-root [project-root #f])
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning (format "memory: auto-reflection failed: ~a"
                                                    (exn-message e))))])
    (cond
      [(not (current-auto-reflection-enabled)) (void)]
      [(not (current-memory-backend)) (void)]
      [else
       (define backend (current-memory-backend))
       (reflect-session-memories! backend
                                  #:session-id session-id
                                  #:project-root project-root
                                  #:min-group-size (current-auto-reflection-min-items))])))

;; ---------------------------------------------------------------------------
;; Provide
;; ---------------------------------------------------------------------------

(provide reflect-session-memories!
         maybe-reflect-session-memories!
         current-reflection-min-group-size
         current-reflection-overlap-threshold
         ;; Exports for testing
         jaccard-similarity
         shared-tags?
         items-related?
         group-items
         merge-group-items)
