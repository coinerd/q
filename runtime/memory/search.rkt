#lang racket/base
;; runtime/memory/search.rkt — Deterministic lexical ranking and dedup helpers
;;
;; v0.95.13 M13-F3: Replaces simple substring-only search with deterministic
;; lexical ranking (token overlap + exact phrase boost + recency/id tiebreaker).
;; M13-F4: Content fingerprint dedup and supersedes-aware filtering.
;;
;; Design constraints:
;;   - No external dependencies beyond racket/base and racket/string
;;   - Fully deterministic: same input → same output, always
;;   - Backend-neutral: backends call these helpers after their own filters
;;   - O(n·m) where n=items, m=tokens — acceptable for in-memory/disk backends

(require racket/list
         racket/string
         "types.rkt")

;; ---------------------------------------------------------------------------
;; Tokenization
;; ---------------------------------------------------------------------------

;; Normalize and tokenize text for matching.
;; Splits on whitespace/punctuation, lowercases, strips empties.
(define (tokenize text)
  (define lowered (string-downcase text))
  (define parts (regexp-split #rx"[^a-z0-9]+" lowered))
  (filter (lambda (s) (> (string-length s) 0)) parts))

;; ---------------------------------------------------------------------------
;; Relevance scoring (M13-F3)
;; ---------------------------------------------------------------------------

;; Score an item against a query text. Returns an exact integer ≥ 0.
;; Higher = more relevant.
;; Factors:
;;   1. Token overlap: count of query tokens found in item content
;;   2. Exact phrase boost: +10 if the full query text appears as substring
;;   3. Recency: tiebreaker via updated-at (more recent = higher)
;;   4. ID: final tiebreaker via string comparison (deterministic)

(define (relevance-score item query-text)
  (define content-tokens (tokenize (memory-item-content item)))
  (define query-tokens (tokenize query-text))
  (cond
    [(null? query-tokens) 0] ; empty query → score 0
    [else
     ;; Token overlap: how many query tokens appear in content
     (define overlap (for/sum ([qt (in-list query-tokens)]) (if (member qt content-tokens) 1 0)))
     ;; Exact phrase boost
     (define phrase-boost
       (if (string-contains? (string-downcase (memory-item-content item))
                             (string-downcase query-text))
           10
           0))
     (+ (* overlap 3) phrase-boost)]))

;; Sort items by relevance score descending, then by recency/id for determinism.
;; Returns a new sorted list.
(define (rank-by-relevance items query-text)
  (sort items
        (lambda (a b)
          (define sa (relevance-score a query-text))
          (define sb (relevance-score b query-text))
          (cond
            [(> sa sb) #t]
            [(< sa sb) #f]
            ;; Tiebreaker 1: updated-at descending
            [else
             (define ta (memory-item-updated-at a))
             (define tb (memory-item-updated-at b))
             (cond
               [(string>? ta tb) #t]
               [(string<? ta tb) #f]
               ;; Tiebreaker 2: id descending
               [else (string>? (memory-item-id a) (memory-item-id b))])]))))

;; ---------------------------------------------------------------------------
;; Content fingerprint (M13-F4)
;; ---------------------------------------------------------------------------

;; Produce a canonical fingerprint for an item's content within a visibility domain.
;; Two items with the same scope + project/session visibility + type +
;; content-normalized fingerprint are considered duplicates.
;; Fingerprint = (scope . project-root . session-id . type . sorted-tokens-hash)
;; We use a simple token-frequency string as the fingerprint key.

(define (content-fingerprint item)
  (define tokens (tokenize (memory-item-content item)))
  (define sorted (sort tokens string<?))
  (define scope (memory-item-scope item))
  (define type (memory-item-type item))
  (define meta (memory-item-metadata item))
  (define project-root (hash-ref meta 'project-root #f))
  (define session-id (hash-ref meta 'session-id #f))
  (format "~a|~a|~a|~a|~a" scope project-root session-id type (string-join sorted " ")))

;; Check if two items are content-duplicates (same scope, type, normalized content)
(define (content-duplicate? a b)
  (and (eq? (memory-item-scope a) (memory-item-scope b))
       (eq? (memory-item-type a) (memory-item-type b))
       (string=? (content-fingerprint a) (content-fingerprint b))))

;; Remove content duplicates from a list, keeping the most recent.
;; Returns deduplicated list preserving input order of kept items.
(define (dedup-by-content items)
  (define seen (make-hash))
  ;; First pass: for each fingerprint, track the best (most recent) item
  (for ([item (in-list items)])
    (define fp (content-fingerprint item))
    (cond
      [(hash-has-key? seen fp)
       (define existing (hash-ref seen fp))
       (when (string>? (memory-item-updated-at item) (memory-item-updated-at existing))
         (hash-set! seen fp item))]
      [else (hash-set! seen fp item)]))
  ;; Build result in input order, keeping only items that are the chosen representative
  (define kept-items (make-hash))
  (for ([(fp item) (in-hash seen)])
    (hash-set! kept-items (memory-item-id item) item))
  (for/list ([item (in-list items)]
             #:when (hash-has-key? kept-items (memory-item-id item)))
    item))

;; ---------------------------------------------------------------------------
;; Supersedes filtering (M13-F4)
;; ---------------------------------------------------------------------------

;; Build a set of IDs that are superseded by other items in the list.
;; An item's `supersedes` field (in validity hash) lists IDs it replaces.
;; Those IDs should be suppressed from results.

(define (superseded-ids items)
  (for*/fold ([acc (hash)])
             ([item (in-list items)]
              [sid (in-list (hash-ref (memory-item-validity item) 'supersedes '()))])
    (hash-set acc sid #t)))

;; Filter out items whose ID appears in the superseded set.
(define (remove-superseded items)
  (define superseded (superseded-ids items))
  (filter (lambda (item) (not (hash-has-key? superseded (memory-item-id item)))) items))

;; Combined post-retrieval processing:
;; 1. Remove superseded items
;; 2. Dedup by content fingerprint
;; 3. Rank by relevance (if query text provided) or sort by recency
;; 4. Apply limit
(define (post-retrieve-process items query #:all-items [all-items items])
  (define query-text (memory-query-text query))
  (define limit (memory-query-limit query))
  ;; Step 1: remove superseded using the full scoped candidate set, not just
  ;; the text-matching subset. Otherwise a query that matches only an old item
  ;; can resurrect it even when a newer item supersedes it.
  (define superseded (superseded-ids all-items))
  (define un-superseded
    (filter (lambda (item) (not (hash-has-key? superseded (memory-item-id item)))) items))
  ;; Step 2: dedup by content
  (define deduped (dedup-by-content un-superseded))
  ;; Step 3: rank or sort
  (define ranked
    (if (and query-text (not (string=? query-text "")))
        (rank-by-relevance deduped query-text)
        ;; No query text → sort by updated-at/id descending
        (sort deduped
              (lambda (a b)
                (define ta (memory-item-updated-at a))
                (define tb (memory-item-updated-at b))
                (cond
                  [(string>? ta tb) #t]
                  [(string<? ta tb) #f]
                  [else (string>? (memory-item-id a) (memory-item-id b))])))))
  ;; Step 4: apply limit
  (if (and limit (< limit (length ranked)))
      (take ranked limit)
      ranked))

;; ---------------------------------------------------------------------------
;; Provide
;; ---------------------------------------------------------------------------

(provide tokenize
         relevance-score
         rank-by-relevance
         content-fingerprint
         content-duplicate?
         dedup-by-content
         superseded-ids
         remove-superseded
         post-retrieve-process)
