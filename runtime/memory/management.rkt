#lang racket/base
;; runtime/memory/management.rkt — Deterministic memory consolidation/management
;;
;; v0.95.16 W6: Turns manage! from no-op into bounded, policy-aware maintenance.
;; Provides deterministic duplicate detection and cleanup WITHOUT LLM summarization.
;;
;; Operations:
;;   1. Expire: Remove items past their expires-at timestamp
;;   2. Dedup: Detect and mark superseded for exact normalized content duplicates
;;      within the same scope+type
;;   3. Compact: Rewrite storage to exclude expired/superseded items
;;
;; All operations are deterministic and scope-bounded.

(require "types.rkt"
         "protocol.rkt"
         "policy.rkt"
         racket/string
         racket/format
         racket/list)

;; ---------------------------------------------------------------------------
;; Management policy shape
;; ---------------------------------------------------------------------------

;; A management policy controls what manage! does.
;; Passed as a hash to gen:manage-memory!.
;;   dry-run?      — #t = report only, no mutations
;;   dedupe?       — #t = detect and supersede content duplicates
;;   expire?       — #t = remove expired items
;;   scope         — 'session, 'project, 'user, or #f (all)
;;   max-items     — maximum items to keep per scope (oldest removed first)

(define (make-management-policy #:dry-run? [dry-run? #f]
                                #:dedupe? [dedupe? #t]
                                #:expire? [expire? #t]
                                #:scope [scope #f]
                                #:max-items [max-items #f])
  (hash 'dry-run? dry-run? 'dedupe? dedupe? 'expire? expire? 'scope scope 'max-items max-items))

;; Default management policy — expire + dedupe, no item limit
(define default-management-policy (make-management-policy #:dry-run? #f #:dedupe? #t #:expire? #t))

;; ---------------------------------------------------------------------------
;; Content fingerprinting for deterministic dedup
;; ---------------------------------------------------------------------------

;; Normalize content: lowercase, collapse whitespace, trim
(define (normalize-content content)
  (define trimmed (string-trim content))
  (define collapsed (regexp-replace* #rx" +" trimmed " "))
  (string-downcase collapsed))

;; Build a fingerprint: scope|type|normalized-content
(define (content-fingerprint item)
  (format "~a|~a|~a"
          (memory-item-scope item)
          (memory-item-type item)
          (normalize-content (memory-item-content item))))

;; ---------------------------------------------------------------------------
;; Expired item detection
;; ---------------------------------------------------------------------------

(define (item-expired? item)
  (define validity (memory-item-validity item))
  (define expires-at (hash-ref validity 'expires-at #f))
  (and expires-at (string? expires-at) (string<? expires-at (format-iso-now))))

;; Get current ISO timestamp (simplified — uses seconds since epoch)
(define (format-iso-now)
  (define secs (current-seconds))
  (define d (seconds->date secs #f))
  (format "~a-~a-~aT~a:~a:~aZ"
          (date-year d)
          (~a (date-month d) #:width 2 #:align 'right #:pad-string "0")
          (~a (date-day d) #:width 2 #:align 'right #:pad-string "0")
          (~a (date-hour d) #:width 2 #:align 'right #:pad-string "0")
          (~a (date-minute d) #:width 2 #:align 'right #:pad-string "0")
          (~a (date-second d) #:width 2 #:align 'right #:pad-string "0")))

;; ---------------------------------------------------------------------------
;; Deterministic management engine
;; ---------------------------------------------------------------------------

;; Analyze items against management policy.
;; Returns a management-report hash:
;;   total-items, expired-count, duplicate-count, supersedes-actions, kept-ids
(define (analyze-items items mgmt-policy)
  (define dry-run? (hash-ref mgmt-policy 'dry-run? #f))
  (define do-expire? (hash-ref mgmt-policy 'expire? #t))
  (define do-dedupe? (hash-ref mgmt-policy 'dedupe? #t))
  (define scope-filter (hash-ref mgmt-policy 'scope #f))
  (define max-items (hash-ref mgmt-policy 'max-items #f))

  ;; Filter by scope if specified
  (define scoped-items
    (if scope-filter
        (filter (lambda (i) (eq? (memory-item-scope i) scope-filter)) items)
        items))

  ;; Phase 1: Identify expired items
  (define expired-ids
    (if do-expire?
        (for/list ([item (in-list scoped-items)]
                   #:when (item-expired? item))
          (memory-item-id item))
        '()))

  ;; Phase 2: Identify duplicates (same fingerprint, keep newest)
  (define non-expired
    (filter (lambda (i) (not (member (memory-item-id i) expired-ids))) scoped-items))

  ;; Build fingerprint → items map
  (define fp->items
    (for/fold ([acc (hash)]) ([item (in-list non-expired)])
      (define fp (content-fingerprint item))
      (hash-set acc fp (cons item (hash-ref acc fp '())))))

  ;; For each fingerprint with >1 item, keep newest (by updated-at), mark rest superseded
  (define duplicate-actions
    (if do-dedupe?
        (for*/list ([(fp items) (in-hash fp->items)]
                    #:when (> (length items) 1))
          (define sorted (sort items string>? #:key (lambda (i) (memory-item-updated-at i))))
          (define keeper (car sorted))
          (define superseded (cdr sorted))
          (hash 'fingerprint
                fp
                'kept-id
                (memory-item-id keeper)
                'superseded-ids
                (map memory-item-id superseded)))
        '()))

  (define superseded-ids
    (apply append (map (lambda (a) (hash-ref a 'superseded-ids '())) duplicate-actions)))

  ;; Phase 3: Apply max-items limit (keep newest)
  (define removable-ids
    (if max-items
        (let* ([surviving (filter (lambda (i)
                                    (and (not (member (memory-item-id i) expired-ids))
                                         (not (member (memory-item-id i) superseded-ids))))
                                  scoped-items)]
               [sorted (sort surviving string<? #:key (lambda (i) (memory-item-updated-at i)))]
               [to-remove (if (> (length sorted) max-items)
                              (take sorted (- (length sorted) max-items))
                              '())])
          (map memory-item-id to-remove))
        '()))

  (define all-removed (remove-duplicates (append expired-ids superseded-ids removable-ids)))
  (define kept-ids
    (for/list ([item (in-list scoped-items)]
               #:when (not (member (memory-item-id item) all-removed)))
      (memory-item-id item)))

  (hash 'total-items
        (length scoped-items)
        'expired-count
        (length expired-ids)
        'duplicate-count
        (length superseded-ids)
        'max-items-removed
        (length removable-ids)
        'superseded-actions
        duplicate-actions
        'kept-ids
        kept-ids
        'removed-ids
        all-removed
        'dry-run?
        dry-run?))

;; Execute management: analyze + mutate backend
;; Returns memory-result with report as metadata
(define (execute-management! backend mgmt-policy)
  (define dry-run? (hash-ref mgmt-policy 'dry-run? #f))
  (define scope-filter (hash-ref mgmt-policy 'scope #f))

  ;; Get all items (including expired)
  (define list-query (memory-query #f scope-filter #f #f #f #f 10000 #t))
  (define list-result (gen:list-memory backend list-query))
  (define all-items
    (if (memory-result-ok? list-result)
        (memory-result-value list-result)
        '()))

  ;; Analyze
  (define report (analyze-items all-items mgmt-policy))

  (cond
    ;; Report only — no mutations
    [dry-run? (memory-result #t #f #f (hash-set report 'dry-run #t))]
    [else
     ;; Execute: delete expired + superseded + overflow items
     (define removed-ids (hash-ref report 'removed-ids '()))
     (define deleted-count
       (for/sum ([id (in-list removed-ids)])
                (define item (findf (lambda (i) (equal? (memory-item-id i) id)) all-items))
                (define scope (and item (memory-item-scope item)))
                (define r (gen:delete-memory! backend id scope))
                (if (memory-result-ok? r) 1 0)))
     (memory-result #t #f #f (hash-set* report 'deleted-count deleted-count 'dry-run #f))]))

;; ---------------------------------------------------------------------------
;; Provide
;; ---------------------------------------------------------------------------

(provide make-management-policy
         default-management-policy
         analyze-items
         execute-management!
         content-fingerprint
         normalize-content
         item-expired?)
