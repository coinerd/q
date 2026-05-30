#lang racket/base

;; util/tree-entries.rkt — Tree entry types for session branching
;;
;; Extracted from protocol-types.rkt (ARCH-05 split).
;; Branch-entry, tree-navigation-entry, branch-summary-entry constructors,
;; predicates, and accessors.

(require racket/contract
         "message.rkt")

(provide (contract-out
          [make-branch-entry (-> string? string? string? message?)]
          [branch-entry? (-> any/c boolean?)]
          [branch-entry-parent-entry-id (-> message? (or/c string? #f))]
          [branch-entry-name (-> message? (or/c string? #f))]
          [make-tree-navigation-entry (-> string? string? string? message?)]
          [tree-navigation-entry? (-> any/c boolean?)]
          [tree-navigation-entry-target-entry-id (-> message? (or/c string? #f))]
          [tree-navigation-entry-from-entry-id (-> message? (or/c string? #f))]
          [make-branch-summary-entry
           (-> string? (or/c string? #f) string? (or/c string? #f) exact-nonnegative-integer? message?)]
          [branch-summary-entry-summary (-> message? (or/c string? #f))]
          [branch-summary-entry-entry-range (-> message? (or/c string? #f))]
          [branch-summary-entry-token-count (-> message? (or/c exact-nonnegative-integer? #f))]
          [tree-entry? (-> any/c boolean?)]))

;; ============================================================
;; Tree entry types (#1314)
;; ============================================================
;; Tree entries are message structs with specific kinds and metadata.
;; They support session branching and navigation within the JSONL log.

;; branch-entry: marks where a branch was created
(define (make-branch-entry id parent-entry-id branch-name)
  (make-message id
                parent-entry-id
                'system
                'branch
                '()
                (current-seconds)
                (hasheq 'branchName branch-name 'parentEntryId parent-entry-id)))

(define (branch-entry? msg)
  (and (message? msg) (eq? (message-kind msg) 'branch)))

(define (branch-entry-parent-entry-id msg)
  (hash-ref (message-meta-safe msg) 'parentEntryId #f))

(define (branch-entry-name msg)
  (hash-ref (message-meta-safe msg) 'branchName #f))

;; tree-navigation-entry: marks navigation to a different branch
(define (make-tree-navigation-entry id from-entry-id target-entry-id)
  (make-message id
                from-entry-id
                'system
                'tree-navigation
                '()
                (current-seconds)
                (hasheq 'targetEntryId target-entry-id 'fromEntryId from-entry-id)))

(define (tree-navigation-entry? msg)
  (and (message? msg) (eq? (message-kind msg) 'tree-navigation)))

(define (tree-navigation-entry-target-entry-id msg)
  (hash-ref (message-meta-safe msg) 'targetEntryId #f))

(define (tree-navigation-entry-from-entry-id msg)
  (hash-ref (message-meta-safe msg) 'fromEntryId #f))

;; branch-summary-entry: LLM-generated summary of a branch
(define (make-branch-summary-entry id parent-id summary entry-range token-count)
  (make-message id
                parent-id
                'system
                'branch-summary
                '()
                (current-seconds)
                (hasheq 'summary summary 'entryRange entry-range 'tokenCount token-count)))

(define (branch-summary-entry-summary msg)
  (hash-ref (message-meta-safe msg) 'summary #f))

(define (branch-summary-entry-entry-range msg)
  (hash-ref (message-meta-safe msg) 'entryRange #f))

(define (branch-summary-entry-token-count msg)
  (hash-ref (message-meta-safe msg) 'tokenCount #f))

;; tree-entry? — matches any tree entry type
(define (tree-entry? msg)
  (and (message? msg) (memq (message-kind msg) '(branch tree-navigation branch-summary)) #t))
