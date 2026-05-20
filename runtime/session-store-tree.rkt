#lang racket/base
;; STABILITY: evolving

;; runtime/session-store-tree.rkt — tree store operations
;;
;; Extracted from session-store.rkt (v0.22.9 W2).
;; Tree operations work on top of the JSONL session log.
;; Tree structure is derived from parentId fields.
;;
;; Uses lazy-require for session-store load/append to avoid circular deps.

(require racket/contract
         racket/lazy-require
         "../util/protocol-types.rkt")

(provide (contract-out [append-tree-entry!
                        (->* (path-string? message?)
                             (#:before-hook (or/c #f procedure?) #:after-hook (or/c #f procedure?))
                             void?)]
                       [load-tree (-> path-string? hash?)]
                       [get-tree-branch (-> hash? string? list?)]
                       [get-children (-> hash? string? list?)]
                       [resolve-active-branch (-> hash? list?)]
                       [tree-info (-> hash? hash?)]))

;; Lazy-require to break circular dependency with session-store.rkt
(lazy-require ["session-store.rkt" (load-session-log append-entry!)])

;; ── Tree store operations ──

(define (append-tree-entry! path entry #:before-hook [before-hook #f] #:after-hook [after-hook #f])
  (when before-hook
    (before-hook 'session-before-tree
                 (hasheq 'entry-type
                         (message-kind entry)
                         'entry-id
                         (message-id entry)
                         'parent-id
                         (message-parent-id entry))))
  (append-entry! path entry)
  (when after-hook
    (after-hook 'session-tree
                (hasheq 'entry-type
                        (message-kind entry)
                        'entry-id
                        (message-id entry)
                        'parent-id
                        (message-parent-id entry)))))

(define (load-tree path)
  (define entries (load-session-log path))
  (define children-map (make-hash))
  (for ([e (in-list entries)])
    (hash-set! children-map (message-id e) '()))
  (for ([e (in-list entries)])
    (define pid (message-parent-id e))
    (when pid
      (define existing (hash-ref children-map pid '()))
      (hash-set! children-map pid (append existing (list e)))))
  (hash-set! children-map '%%entries%% entries)
  children-map)

(define (get-tree-branch tree target-id)
  (define entries (hash-ref tree '%%entries%% '()))
  (define by-id (make-hash))
  (for ([e (in-list entries)])
    (hash-set! by-id (message-id e) e))
  (define target (hash-ref by-id target-id #f))
  (cond
    [(not target) '()]
    [else
     (define (walk-up entry acc)
       (define new-acc (cons entry acc))
       (define pid (message-parent-id entry))
       (cond
         [(not pid) new-acc]
         [else
          (define parent (hash-ref by-id pid #f))
          (if parent
              (walk-up parent new-acc)
              new-acc)]))
     (walk-up target '())]))

(define (get-children tree entry-id)
  (hash-ref tree entry-id '()))

(define (resolve-active-branch tree)
  (define entries (hash-ref tree '%%entries%% '()))
  (cond
    [(null? entries) '()]
    [else
     (define by-id (make-hash))
     (for ([e (in-list entries)])
       (hash-set! by-id (message-id e) e))
     (define last-entry (list-ref entries (sub1 (length entries))))
     (define (walk-up entry acc)
       (define new-acc (cons entry acc))
       (define pid (message-parent-id entry))
       (cond
         [(not pid) new-acc]
         [else
          (define parent (hash-ref by-id pid #f))
          (if parent
              (walk-up parent new-acc)
              new-acc)]))
     (walk-up last-entry '())]))

(define (tree-info tree)
  (define entries (hash-ref tree '%%entries%% '()))
  (define branch-count 0)
  (define navigation-count 0)
  (define summary-count 0)
  (for ([e (in-list entries)])
    (case (message-kind e)
      [(branch) (set! branch-count (add1 branch-count))]
      [(tree-navigation) (set! navigation-count (add1 navigation-count))]
      [(branch-summary) (set! summary-count (add1 summary-count))]
      [else (void)]))
  (define leaf-ids
    (for/list ([e (in-list entries)]
               #:when (null? (hash-ref tree (message-id e) '())))
      (message-id e)))
  (hasheq 'total-entries
          (length entries)
          'branch-count
          branch-count
          'navigation-count
          navigation-count
          'summary-count
          summary-count
          'leaf-ids
          leaf-ids))
