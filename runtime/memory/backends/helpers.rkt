#lang racket/base
;; runtime/memory/backends/helpers.rkt — Shared filter/sort/timestamp helpers
;;
;; F28 (P3): Extracted from duplicated code across memory-hash.rkt and
;; file-jsonl.rkt to eliminate redundancy and ensure consistent behavior.

(require racket/list
         racket/string
         "../types.rkt")

(provide scope-match?
         type-match?
         tag-match?
         text-match?
         expired?
         sort-items
         take-at-most
         current-iso-8601
         pad2)

;; ---------------------------------------------------------------------------
;; Scope filter: match item scope, project-root, and session-id against query
;; ---------------------------------------------------------------------------

(define (scope-match? item query)
  (define q-scope (memory-query-scope query))
  (define q-project (memory-query-project-root query))
  (define q-session (memory-query-session-id query))
  (and (or (not q-scope) (eq? q-scope (memory-item-scope item)))
       (or (not q-project) (equal? q-project (hash-ref (memory-item-metadata item) 'project-root #f)))
       (or (not q-session) (equal? q-session (hash-ref (memory-item-metadata item) 'session-id #f)))))

;; ---------------------------------------------------------------------------
;; Type filter: match item type against query types list
;; ---------------------------------------------------------------------------

(define (type-match? item query)
  (define q-types (memory-query-types query))
  (or (not q-types) (memq (memory-item-type item) q-types)))

;; ---------------------------------------------------------------------------
;; Tag filter: match all query tags against item tags
;; ---------------------------------------------------------------------------

(define (tag-match? item query)
  (define q-tags (memory-query-tags query))
  (or (not q-tags)
      (let ([item-tags (hash-ref (memory-item-metadata item) 'tags '())])
        (for/and ([t (in-list q-tags)])
          (member t item-tags)))))

;; ---------------------------------------------------------------------------
;; Text filter (F7): case-insensitive substring match on content
;; ---------------------------------------------------------------------------

(define (text-match? item query)
  (define q-text (memory-query-text query))
  (or (not q-text)
      (string=? q-text "")
      (string-contains? (string-downcase (memory-item-content item))
                        (string-downcase q-text))))

;; ---------------------------------------------------------------------------
;; Expiry check: item has expires-at timestamp in the past
;; ---------------------------------------------------------------------------

(define (expired? item)
  (define expires (hash-ref (memory-item-validity item) 'expires-at #f))
  (and expires (string<? expires (current-iso-8601))))

;; ---------------------------------------------------------------------------
;; Sort: by (updated-at desc, id desc) for deterministic ordering
;; ---------------------------------------------------------------------------

(define (sort-items items)
  (sort items
        (lambda (a b)
          (define ta (memory-item-updated-at a))
          (define tb (memory-item-updated-at b))
          (cond
            [(string>? ta tb) #t]
            [(string<? ta tb) #f]
            [else (string>? (memory-item-id a) (memory-item-id b))]))))

;; ---------------------------------------------------------------------------
;; Take at most n items
;; ---------------------------------------------------------------------------

(define (take-at-most lst n)
  (if (and n (< n (length lst)))
      (take lst n)
      lst))

;; ---------------------------------------------------------------------------
;; ISO-8601 timestamp helpers
;; ---------------------------------------------------------------------------

(define (current-iso-8601)
  (define ts (current-seconds))
  (define d (seconds->date ts #f))
  (format "~a-~a-~aT~a:~a:~aZ"
          (date-year d)
          (pad2 (date-month d))
          (pad2 (date-day d))
          (pad2 (date-hour d))
          (pad2 (date-minute d))
          (pad2 (date-second d))))

(define (pad2 v)
  (define s (format "~a" v))
  (if (< (string-length s) 2)
      (string-append "0" s)
      s))
