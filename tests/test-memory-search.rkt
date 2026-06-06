#lang racket/base
;; tests/test-memory-search.rkt — Search ranking + dedup + supersedes tests
;;
;; v0.95.13 W2: Tests for runtime/memory/search.rkt
;; M13-F3: Deterministic lexical ranking
;; M13-F4: Content fingerprint dedup + supersedes filtering

(require rackunit
         racket/string
         "../runtime/memory/search.rkt"
         "../runtime/memory/types.rkt")

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (make-item #:id [id "m1"]
                   #:content [content "test content"]
                   #:scope [scope 'project]
                   #:type [type 'semantic]
                   #:supersedes [supersedes '()]
                   #:updated [updated "2026-06-05T12:00:00Z"])
  (memory-item id type scope content
               (hasheq 'project-root "/tmp" 'session-id "s1"
                       'tags '() 'source "test" 'origin-message-id "test")
               (hasheq 'sensitivity 'public 'confidence 0.9
                       'supersedes supersedes 'expires-at #f)
               "2026-06-05T00:00:00Z" updated))

;; ---------------------------------------------------------------------------
;; M13-F3: Tokenization
;; ---------------------------------------------------------------------------

(test-case "tokenize splits on whitespace and punctuation"
  (check-equal? (tokenize "Hello, World! Foo-bar") '("hello" "world" "foo" "bar")))

(test-case "tokenize handles empty string"
  (check-equal? (tokenize "") '()))

(test-case "tokenize lowercases"
  (check-equal? (tokenize "ABC def GHI") '("abc" "def" "ghi")))

;; ---------------------------------------------------------------------------
;; M13-F3: Relevance scoring
;; ---------------------------------------------------------------------------

(test-case "relevance-score: 0 for no query text"
  (check-equal? (relevance-score (make-item) "") 0))

(test-case "relevance-score: 0 for no token overlap"
  (check-equal? (relevance-score (make-item #:content "alpha beta") "gamma delta") 0))

(test-case "relevance-score: positive for token overlap"
  (define score (relevance-score (make-item #:content "hello world foo bar") "hello world"))
  (check-true (> score 0)))

(test-case "relevance-score: exact phrase gets boost"
  (define item (make-item #:content "the quick brown fox jumps"))
  (define score-phrase (relevance-score item "quick brown fox"))
  (define score-tokens (relevance-score item "fox quick"))
  (check-true (> score-phrase score-tokens)))

(test-case "relevance-score: more overlap = higher score"
  (define item (make-item #:content "alpha beta gamma delta epsilon"))
  (define s2 (relevance-score item "alpha beta"))
  (define s4 (relevance-score item "alpha beta gamma delta"))
  (check-true (> s4 s2)))

;; ---------------------------------------------------------------------------
;; M13-F3: Ranking
;; ---------------------------------------------------------------------------

(test-case "rank-by-relevance: higher score first"
  (define items
    (list (make-item #:id "low" #:content "unrelated content")
          (make-item #:id "high" #:content "hello world search test")
          (make-item #:id "mid" #:content "search something")))
  (define ranked (rank-by-relevance items "hello world search"))
  (check-equal? (memory-item-id (car ranked)) "high"))

(test-case "rank-by-relevance: deterministic tiebreaker by updated-at"
  (define items
    (list (make-item #:id "a" #:content "same content" #:updated "2026-01-01T00:00:00Z")
          (make-item #:id "b" #:content "same content" #:updated "2026-06-01T00:00:00Z")))
  (define ranked (rank-by-relevance items "same content"))
  ;; Same score, but b is newer → b first
  (check-equal? (memory-item-id (car ranked)) "b"))

(test-case "rank-by-relevance: deterministic tiebreaker by id"
  (define items
    (list (make-item #:id "z" #:content "same content" #:updated "2026-01-01T00:00:00Z")
          (make-item #:id "a" #:content "same content" #:updated "2026-01-01T00:00:00Z")))
  (define ranked (rank-by-relevance items "same content"))
  ;; Same score and timestamp → z > a lexicographically → z first
  (check-equal? (memory-item-id (car ranked)) "z"))

;; ---------------------------------------------------------------------------
;; M13-F4: Content fingerprint
;; ---------------------------------------------------------------------------

(test-case "content-fingerprint: same content/scope/type = same fingerprint"
  (define a (make-item #:content "hello world"))
  (define b (make-item #:id "m2" #:content "world hello"))  ; tokens reorder
  (check-equal? (content-fingerprint a) (content-fingerprint b)))

(test-case "content-fingerprint: different content = different fingerprint"
  (define a (make-item #:content "hello world"))
  (define b (make-item #:id "m2" #:content "goodbye world"))
  (check-not-equal? (content-fingerprint a) (content-fingerprint b)))

(test-case "content-fingerprint: different scope = different fingerprint"
  (define a (make-item #:content "hello" #:scope 'project))
  (define b (make-item #:id "m2" #:content "hello" #:scope 'session))
  (check-not-equal? (content-fingerprint a) (content-fingerprint b)))

(test-case "content-duplicate?: detects duplicates"
  (define a (make-item #:content "test"))
  (define b (make-item #:id "m2" #:content "test"))
  (check-true (content-duplicate? a b)))

(test-case "content-duplicate?: different content is not duplicate"
  (define a (make-item #:content "alpha"))
  (define b (make-item #:id "m2" #:content "beta"))
  (check-false (content-duplicate? a b)))

;; ---------------------------------------------------------------------------
;; M13-F4: Dedup by content
;; ---------------------------------------------------------------------------

(test-case "dedup-by-content: keeps newest, removes older duplicate"
  (define items
    (list (make-item #:id "old" #:content "test" #:updated "2026-01-01T00:00:00Z")
          (make-item #:id "new" #:content "test" #:updated "2026-06-01T00:00:00Z")))
  (define deduped (dedup-by-content items))
  (check-equal? (length deduped) 1)
  (check-equal? (memory-item-id (car deduped)) "new"))

(test-case "dedup-by-content: different content items preserved"
  (define items
    (list (make-item #:id "a" #:content "alpha")
          (make-item #:id "b" #:content "beta")))
  (define deduped (dedup-by-content items))
  (check-equal? (length deduped) 2))

;; ---------------------------------------------------------------------------
;; M13-F4: Supersedes filtering
;; ---------------------------------------------------------------------------

(test-case "superseded-ids: collects all superseded IDs"
  (define items
    (list (make-item #:id "v3" #:supersedes '("v2"))
          (make-item #:id "v2" #:supersedes '("v1"))
          (make-item #:id "v1" #:supersedes '())))
  (define superseded (superseded-ids items))
  (check-true (hash-has-key? superseded "v1"))
  (check-true (hash-has-key? superseded "v2"))
  (check-false (hash-has-key? superseded "v3")))

(test-case "remove-superseded: filters out superseded items"
  (define items
    (list (make-item #:id "v3" #:content "version 3" #:supersedes '("v2"))
          (make-item #:id "v2" #:content "version 2" #:supersedes '("v1"))
          (make-item #:id "v1" #:content "version 1" #:supersedes '())))
  (define filtered (remove-superseded items))
  (define ids (map memory-item-id filtered))
  (check-false (member "v1" ids))
  (check-false (member "v2" ids))
  (check-true (and (member "v3" ids) #t)))

;; ---------------------------------------------------------------------------
;; M13-F3+F4: Post-retrieve processing integration
;; ---------------------------------------------------------------------------

(test-case "post-retrieve-process: dedup + rank + limit"
  (define items
    (list (make-item #:id "old" #:content "test query alpha" #:updated "2026-01-01T00:00:00Z")
          (make-item #:id "new" #:content "test query alpha" #:updated "2026-06-01T00:00:00Z")
          (make-item #:id "other" #:content "unrelated thing" #:updated "2026-06-01T00:00:00Z")))
  (define query (memory-query "query" #f #f #f #f #f 10 #f))
  (define result (post-retrieve-process items query))
  ;; Deduped: old+new same content → keeps new. other has score 0 → ranked last
  ;; Items: new (score>0) and other (score=0). Both present but ranked.
  (check-true (>= (length result) 1))
  (check-equal? (memory-item-id (car result)) "new")
  ;; 'other' is present but ranked last (score 0)
  (when (= (length result) 2)
    (check-equal? (memory-item-id (cadr result)) "other")))

(test-case "post-retrieve-process: no query text → sort by recency"
  (define items
    (list (make-item #:id "a" #:content "alpha" #:updated "2026-01-01T00:00:00Z")
          (make-item #:id "b" #:content "beta" #:updated "2026-06-01T00:00:00Z")))
  (define query (memory-query "" #f #f #f #f #f 10 #f))
  (define result (post-retrieve-process items query))
  (check-equal? (memory-item-id (car result)) "b"))

(test-case "post-retrieve-process: limit is respected"
  (define items
    (for/list ([i (in-range 10)])
      (make-item #:id (format "m~a" i) #:content (format "unique content ~a" i))))
  (define query (memory-query "" #f #f #f #f #f 3 #f))
  (define result (post-retrieve-process items query))
  (check-equal? (length result) 3))

(test-case "post-retrieve-process: supersedes removes chain"
  (define items
    (list (make-item #:id "v3" #:content "latest version" #:supersedes '("v2"))
          (make-item #:id "v2" #:content "middle version" #:supersedes '("v1"))
          (make-item #:id "v1" #:content "first version" #:supersedes '())))
  (define query (memory-query "" #f #f #f #f #f 10 #f))
  (define result (post-retrieve-process items query))
  (define ids (map memory-item-id result))
  (check-equal? ids '("v3")))
