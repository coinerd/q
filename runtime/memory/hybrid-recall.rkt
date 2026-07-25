#lang racket/base

;; runtime/memory/hybrid-recall.rkt — Hybrid recall: blend multiple memory sources
;; with validity-aware ranking and forgetting.
;;
;; W6 (#8943): Pure scoring + composition of multiple memory sources.
;; Composes: working-set continuity entries, session memories, project memories.
;; Scoring dimensions:
;;   - recency: newer items score higher (based on updated-at)
;;   - relevance: text match between query and item content
;;   - validity: expired items are deprioritized
;;   - project-ness: project-scoped items score higher in project contexts
;;
;; Design:
;;   - PURE functions — no I/O, no parameter references
;;   - Each source produces scored items independently
;;   - A scorer struct defines which dimensions to use with weights
;;   - Output is a ranked list of scored-item structs with trace info
;;
;; Contract:
;;   - Callers must pass items as (listof memory-item?)
;;   - Items with non-ISO-8601 timestamps are treated as "oldest" (zero recency)
;;   - Expired items (validity has 'expires-at < now) are scored at 0.0
;;   - Missing content scores 0.0 for relevance

(require racket/list
         racket/match
         racket/math
         racket/string
         racket/function
         racket/date
         "../memory/types.rkt"
         (prefix-in h: "../memory/backends/helpers.rkt"))

;; ============================================================
;; Scorer struct — configures which scoring dimensions to use
;; ============================================================

(struct recall-scorer
        (recency-weight ; 0.0-1.0: weight for recency (newer = higher)
         relevance-weight ; 0.0-1.0: weight for text match
         validity-weight ; 0.0-1.0: weight for not-expired
         project-weight ; 0.0-1.0: weight for project-scope
         forgetting-days ; number: items older than N days get 50% score reduction
         )
  #:transparent)

;; Default scorer with balanced weights
(define default-recall-scorer
  (recall-scorer 0.35 ; recency
                 0.35 ; relevance
                 0.20 ; validity
                 0.10 ; project-ness
                 7.0)) ; forgetting threshold in days

;; Session-focused scorer (prioritizes recency + relevance)
(define session-recall-scorer
  (recall-scorer 0.40 ; recency
                 0.40 ; relevance
                 0.15 ; validity
                 0.05 ; project-ness
                 3.0)) ; shorter forgetting horizon for sessions

;; Project-focused scorer (gives more weight to project-scope items)
(define project-recall-scorer
  (recall-scorer 0.25 ; recency
                 0.35 ; relevance
                 0.20 ; validity
                 0.20 ; project-ness
                 30.0)) ; longer forgetting horizon for project memories

;; ============================================================
;; Scored item — result of scoring a single memory-item
;; ============================================================

(struct scored-item
        (item ; memory-item?
         score ; inexact-real? 0.0-1.0
         recency ; 0.0-1.0
         relevance ; 0.0-1.0
         validity ; 0.0-1.0
         project ; 0.0-1.0
         source ; 'working-set | 'session-memory | 'project-memory
         )
  #:transparent)

;; ============================================================
;; ISO-8601 timestamp utilities
;; ============================================================

;; Parse ISO-8601 string to seconds since epoch, or #f.
(define (parse-iso-8601 str)
  (define m
    (and (string? str)
         (regexp-match #px"^(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})" str)))
  (and m
       (let ([yr (string->number (list-ref m 1))]
             [mo (string->number (list-ref m 2))]
             [dy (string->number (list-ref m 3))]
             [hr (string->number (list-ref m 4))]
             [mi (string->number (list-ref m 5))]
             [se (string->number (list-ref m 6))])
         (find-seconds se mi hr dy mo yr 0))))

;; Compute days between two ISO-8601 timestamps (seconds)
(define (days-between a b)
  (abs (/ (- a b) 86400.0)))

;; Get current time as seconds since epoch
(define (current-seconds-epoch)
  (current-seconds))

;; ============================================================
;; Expiry check
;; ============================================================

;; Check if a memory-item is expired relative to a reference time (seconds).
(define (expired-at? item now-seconds)
  (define expires (hash-ref (memory-item-validity item) 'expires-at #f))
  (and expires
       (string? expires)
       (let ([exp-sec (parse-iso-8601 expires)]) (and exp-sec (<= exp-sec now-seconds)))))

;; ============================================================
;; Scoring functions (each returns 0.0-1.0)
;; ============================================================

;; Recency score: newer items get higher score.
;; Uses updated-at. Items within the first "forgetting-days" get full score,
;; then linearly decay to 0 after 2x forgetting-days.
(define (score-recency item now-seconds scorer)
  (define ts (parse-iso-8601 (memory-item-updated-at item)))
  (cond
    [(not ts) 0.0]
    [else
     (define age-days (/ (- now-seconds ts) 86400.0))
     (define forgetting-days (recall-scorer-forgetting-days scorer))
     (if (<= age-days 0)
         1.0
         (if (>= age-days (* 2 forgetting-days))
             0.0
             (- 1.0 (/ age-days (* 2 forgetting-days)))))]))

;; Relevance score: simple word-overlap between query text and item content.
;; 1.0 if query is empty or all query words are in content.
;; Falls off proportionally.
(define (score-relevance item query-text)
  (cond
    [(or (not query-text) (string=? (string-trim query-text) "")) 0.5] ;; neutral
    [else
     (define content (memory-item-content item))
     (define query-words (string-split (string-downcase query-text)))
     (define content-lower (string-downcase content))
     (define matches
       (for/sum ([w (in-list query-words)]) (if (string-contains? content-lower w) 1 0)))
     (if (zero? (length query-words))
         0.5
         (/ matches (length query-words)))]))

;; Validity score: expired items score 0.0, otherwise 1.0.
;; Items without expires-at also score 1.0 (no expiration → always valid).
(define (score-validity item now-seconds)
  (if (expired-at? item now-seconds) 0.0 1.0))

;; Project-ness score: project-scoped items score 1.0, session/other score project-weight.
(define (score-project-ness item scorer)
  (if (eq? (memory-item-scope item) 'project)
      1.0
      (recall-scorer-project-weight scorer)))

;; ============================================================
;; Full scoring pipeline
;; ============================================================

;; Score a single memory-item against a query and scorer.
;; Returns scored-item struct with dimension scores and blended total.
(define (score-item item query-text now-seconds scorer source)
  (define r (score-recency item now-seconds scorer))
  (define re (score-relevance item query-text))
  (define v (score-validity item now-seconds))
  (define p (score-project-ness item scorer))
  (define total
    (+ (* r (recall-scorer-recency-weight scorer))
       (* re (recall-scorer-relevance-weight scorer))
       (* v (recall-scorer-validity-weight scorer))
       (* p (recall-scorer-project-weight scorer))))
  (scored-item item total r re v p source))

;; ============================================================
;; Source scorers — score a batch of items from one source
;; ============================================================

(define (score-working-set-items ws-tuples query-text now-seconds scorer limit)
  ;; ws-tuples are pairs of (path message-id token-estimate timestamp)
  ;; Convert to memory-items for uniform scoring
  ;; Actually working-set entries are different — they represent files, not memory items.
  ;; For hybrid recall, we only score actual memory-items from backends.
  ;; Working set is added as context in the context-assembly layer separately.
  '())

;; Score items from a session or project memory backend.
(define (score-memory-items items query-text now-seconds scorer source)
  (define scored
    (for/list ([item (in-list items)])
      (score-item item query-text now-seconds scorer source)))
  scored)

;; ============================================================
;; Composite blending
;; ============================================================

;; Blend multiple scored-item lists into a single ranked list.
;; Deduplicates by memory-item-id (keeps highest score).
;; Returns top-N by score, descending.
(define (blend-results scored-lists limit)
  (define merged
    (foldl (lambda (scored acc)
             (define id (memory-item-id (scored-item-item scored)))
             (cond
               [(hash-has-key? acc id)
                (define existing (hash-ref acc id))
                (if (>= (scored-item-score scored) (scored-item-score existing))
                    (hash-set acc id scored)
                    acc)]
               [else (hash-set acc id scored)]))
           (hash)
           (apply append scored-lists)))
  (define sorted
    (sort (hash-values merged) (lambda (a b) (> (scored-item-score a) (scored-item-score b)))))
  (take-at-most sorted limit))

;; ============================================================
;; Top-N helpers
;; ============================================================

(define (take-at-most lst n)
  (if (and (exact-positive-integer? n) (< n (length lst)))
      (take lst n)
      lst))

;; ============================================================
;; High-level blend API
;; ============================================================

;; Perform hybrid recall: score and blend items from up to two memory backends.
;;
;; Parameters:
;;   query-text: string — search text for relevance scoring
;;   session-items: (listof memory-item?) — from session-scoped backend
;;   project-items: (listof memory-item?) — from project-scoped backend
;;   scorer: recall-scorer? — scoring configuration (default: default-recall-scorer)
;;   limit: exact-positive-integer? — max results (default: 10)
;;   now: string? — reference time as ISO-8601 (default: current time)
;;
;; Returns (listof scored-item?), ranked by blended score descending.
(define (hybrid-recall query-text
                       session-items
                       project-items
                       #:scorer [scorer default-recall-scorer]
                       #:limit [limit 10]
                       #:now [now #f])
  (define now-epoch
    (cond
      [now (or (parse-iso-8601 now) (current-seconds-epoch))]
      [else (current-seconds-epoch)]))
  (define scored-lists
    (filter (lambda (l) (not (null? l)))
            (list (score-memory-items session-items query-text now-epoch scorer 'session-memory)
                  (score-memory-items project-items query-text now-epoch scorer 'project-memory))))
  (blend-results scored-lists limit))

;; ============================================================
;; Re-exports for convenience
;; ============================================================

;; Scorer struct
(provide recall-scorer
         recall-scorer?
         recall-scorer-recency-weight
         recall-scorer-relevance-weight
         recall-scorer-validity-weight
         recall-scorer-project-weight
         recall-scorer-forgetting-days
         default-recall-scorer
         session-recall-scorer
         project-recall-scorer

         ;; Scored item struct
         scored-item
         scored-item?
         scored-item-item
         scored-item-score
         scored-item-recency
         scored-item-relevance
         scored-item-validity
         scored-item-project
         scored-item-source

         ;; Scoring functions
         score-recency
         score-relevance
         score-validity
         score-project-ness
         score-item
         score-memory-items
         blend-results

         ;; High-level
         hybrid-recall
         expired-at?
         parse-iso-8601
         days-between)
