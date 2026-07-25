#lang racket/base

;; tests/test-hybrid-recall.rkt — W6 (#8943): Hybrid recall tests
;;
;; Tests pure scoring, blending, forgetting, and edge cases.
;; No I/O, no backends — uses manually constructed memory-items.

(require rackunit
         rackunit/text-ui
         racket/match
         racket/date
         "../runtime/memory/types.rkt"
         "../runtime/memory/hybrid-recall.rkt")

;; ============================================================
;; Helpers
;; ============================================================

;; Make a simple memory-item for testing.
(define (make-test-item id
                        content
                        #:scope [scope 'session]
                        #:type [type 'episodic]
                        #:created-at [created "2025-01-01T00:00:00Z"]
                        #:updated-at [updated "2025-01-01T00:00:00Z"]
                        #:expires-at [expires #f])
  (define validity
    (if expires
        (hasheq 'sensitivity 'low 'confidence 0.5 'supersedes #f 'expires-at expires)
        (hasheq 'sensitivity 'low 'confidence 0.5 'supersedes #f)))
  (memory-item
   id
   type
   scope
   content
   (hasheq 'project-root "/p" 'session-id "s1" 'tags '() 'source "test" 'origin-message-id "m1")
   validity
   created
   updated))

;; Parse ISO-8601 to seconds for reference time
(define (parse-ref str)
  (or (parse-iso-8601 str) 0))

(define test-scorer default-recall-scorer)

;; ============================================================
;; Test suite
;; ============================================================

(define-test-suite
 hybrid-recall-suite
 ;; ── Parse ISO-8601 ──
 (test-case "parse-iso-8601: valid timestamp"
   (define ts (parse-iso-8601 "2025-06-15T10:30:00Z"))
   (check-not-false ts)
   (check-true (exact-positive-integer? ts)))
 (test-case "parse-iso-8601: invalid returns #f"
   (check-false (parse-iso-8601 "not-a-timestamp"))
   (check-false (parse-iso-8601 "2025/01/01"))
   (check-false (parse-iso-8601 ""))
   (check-false (parse-iso-8601 #f)))
 ;; ── Expiry check ──
 (test-case "expired-at?: item with no expires-at is not expired"
   (define item (make-test-item "id-1" "hello"))
   (check-false (expired-at? item 1000000000)))
 (test-case "expired-at?: item with future expires-at is not expired"
   (define item (make-test-item "id-1" "hello" #:expires-at "2026-01-01T00:00:00Z"))
   (check-false (expired-at? item (parse-ref "2025-06-15T00:00:00Z"))))
 (test-case "expired-at?: item with past expires-at is expired"
   (define item (make-test-item "id-1" "hello" #:expires-at "2025-01-01T00:00:00Z"))
   (check-true (expired-at? item (parse-ref "2025-06-15T00:00:00Z"))))
 ;; ── Recency scoring ──
 (test-case "score-recency: very recent item scores 1.0"
   (define now (parse-ref "2025-06-15T12:00:00Z"))
   (define item (make-test-item "id-1" "hello" #:updated-at "2025-06-15T11:59:00Z"))
   (check-true (>= (score-recency item now test-scorer) 0.99)))
 (test-case "score-recency: very old item scores near 0"
   (define now (parse-ref "2025-06-15T12:00:00Z"))
   (define item (make-test-item "id-1" "hello" #:updated-at "2020-01-01T00:00:00Z"))
   (check-true (< (score-recency item now test-scorer) 0.05)))
 (test-case "score-recency: item with unparseable timestamp scores 0"
   (define item (make-test-item "id-1" "hello" #:updated-at "bogus"))
   (check-true (zero? (score-recency item 1000000000 test-scorer))))
 ;; ── Relevance scoring ──
 (test-case "score-relevance: empty query returns 0.5 (neutral)"
   (define item (make-test-item "id-1" "hello world"))
   (check-= (score-relevance item "") 0.5 0.001)
   (check-= (score-relevance item #f) 0.5 0.001))
 (test-case "score-relevance: all query words match → 1.0"
   (define item (make-test-item "id-1" "hello world foo bar"))
   (check-= (score-relevance item "hello world") 1.0 0.001))
 (test-case "score-relevance: partial match"
   (define item (make-test-item "id-1" "hello world"))
   (check-= (score-relevance item "hello racket") 0.5 0.001))
 (test-case "score-relevance: no match → 0.0"
   (define item (make-test-item "id-1" "hello world"))
   (check-= (score-relevance item "xyzzy") 0.0 0.001))
 ;; ── Validity scoring ──
 (test-case "score-validity: valid item scores 1.0"
   (define item (make-test-item "id-1" "hello"))
   (check-= (score-validity item (parse-ref "2025-06-15T00:00:00Z")) 1.0 0.001))
 (test-case "score-validity: expired item scores 0.0"
   (define item (make-test-item "id-1" "hello" #:expires-at "2025-01-01T00:00:00Z"))
   (check-= (score-validity item (parse-ref "2025-06-15T00:00:00Z")) 0.0 0.001))
 ;; ── Project-ness scoring ──
 (test-case "score-project-ness: project scope scores 1.0"
   (define item (make-test-item "id-1" "hello" #:scope 'project))
   (check-= (score-project-ness item test-scorer) 1.0 0.001))
 (test-case "score-project-ness: session scope scores project-weight"
   (define item (make-test-item "id-1" "hello" #:scope 'session))
   (check-= (score-project-ness item test-scorer) (recall-scorer-project-weight test-scorer) 0.001))
 ;; ── Full scoring ──
 (test-case "score-item: combines all dimensions"
   (define now (parse-ref "2025-06-15T12:00:00Z"))
   (define item
     (make-test-item "id-1"
                     "hello project memory"
                     #:scope 'project
                     #:updated-at "2025-06-15T11:00:00Z"))
   (define si (score-item item "hello memory" now test-scorer 'project-memory))
   (check-true (scored-item? si))
   (check-true (> (scored-item-score si) 0.5))
   (check-equal? (scored-item-source si) 'project-memory))
 (test-case "score-item: expired item scores lower than valid item"
   (define now (parse-ref "2025-06-15T12:00:00Z"))
   (define valid-item (make-test-item "id-1" "hello" #:updated-at "2025-06-15T11:00:00Z"))
   (define expired-item
     (make-test-item "id-2"
                     "hello"
                     #:updated-at "2025-06-15T11:00:00Z"
                     #:expires-at "2025-01-01T00:00:00Z"))
   (define si-valid (score-item valid-item "hello" now test-scorer 'session-memory))
   (define si-expired (score-item expired-item "hello" now test-scorer 'session-memory))
   (check-true (> (scored-item-score si-valid) (scored-item-score si-expired))))
 ;; ── Blend results ──
 (test-case "blend-results: merges two lists, deduplicates by id"
   (define now (parse-ref "2025-06-15T12:00:00Z"))
   (define item-a (make-test-item "id-1" "alpha" #:updated-at "2025-06-15T11:00:00Z"))
   (define item-b (make-test-item "id-2" "beta" #:updated-at "2025-06-15T10:00:00Z"))
   (define si-a (score-item item-a "alpha" now test-scorer 'session-memory))
   (define si-b (score-item item-b "beta" now test-scorer 'project-memory))
   (define result (blend-results (list (list si-a) (list si-b)) 10))
   (check-equal? (length result) 2)
   (check-true (scored-item? (car result))))
 (test-case "blend-results: deduplication keeps higher score"
   (define now (parse-ref "2025-06-15T12:00:00Z"))
   ;; Same id, different recency → higher recency should win
   (define item-old (make-test-item "id-1" "hello" #:updated-at "2025-01-01T00:00:00Z"))
   (define item-new (make-test-item "id-1" "hello" #:updated-at "2025-06-15T11:00:00Z"))
   (define si-old (score-item item-old "hello" now test-scorer 'session-memory))
   (define si-new (score-item item-new "hello" now test-scorer 'project-memory))
   (define result (blend-results (list (list si-old) (list si-new)) 10))
   (check-equal? (length result) 1)
   (check-equal? (scored-item-source (car result)) 'project-memory))
 (test-case "blend-results: respects limit"
   (define now (parse-ref "2025-06-15T12:00:00Z"))
   (define items
     (for/list ([i (in-range 5)])
       (make-test-item (format "id-~a" i)
                       (format "item ~a" i)
                       #:updated-at (format "2025-06-1~aT00:00:00Z" i))))
   (define scored
     (for/list ([item items])
       (score-item item "item" now test-scorer 'session-memory)))
   (define result (blend-results (list scored) 3))
   (check-equal? (length result) 3))
 ;; ── High-level hybrid-recall ──
 (test-case "hybrid-recall: blends session and project items"
   (define session-items
     (list (make-test-item "s-1"
                           "session memory alpha"
                           #:scope 'session
                           #:updated-at "2025-06-15T11:00:00Z")))
   (define project-items
     (list (make-test-item "p-1"
                           "project memory beta"
                           #:scope 'project
                           #:updated-at "2025-06-14T10:00:00Z")))
   (define result
     (hybrid-recall "memory alpha"
                    session-items
                    project-items
                    #:limit 5
                    #:now "2025-06-15T12:00:00Z"))
   (check-equal? (length result) 2)
   ;; session memory should rank higher (matches query better + more recent)
   (check-equal? (scored-item-source (car result)) 'session-memory))
 (test-case "hybrid-recall: empty sources yield empty result"
   (check-equal? (hybrid-recall "hello" '() '() #:limit 5) '()))
 (test-case "hybrid-recall: expired items deprioritized"
   (define now "2025-06-15T12:00:00Z")
   ;; Both items same recency and content, but one is expired
   (define items
     (list (make-test-item "a-1" "hello" #:updated-at "2025-06-15T11:00:00Z")
           (make-test-item "a-2"
                           "hello"
                           #:updated-at "2025-06-15T11:00:00Z"
                           #:expires-at "2025-01-01T00:00:00Z")))
   (define result (hybrid-recall "hello" '() items #:limit 5 #:now now))
   (check-equal? (length result) 2)
   ;; Non-expired (a-1) should be first
   (check-equal? (memory-item-id (scored-item-item (car result))) "a-1"))
 ;; ── Session vs project scorer ──
 (test-case "session-recall-scorer has shorter forgetting-days"
   (check-true (< (recall-scorer-forgetting-days session-recall-scorer)
                  (recall-scorer-forgetting-days default-recall-scorer))))
 (test-case "project-recall-scorer has longer forgetting-days"
   (check-true (> (recall-scorer-forgetting-days project-recall-scorer)
                  (recall-scorer-forgetting-days default-recall-scorer))))
 ;; ── Forgetting: old items deprioritized ──
 (test-case "score-recency: item beyond 2x forgetting-days scores 0"
   (define scorer (recall-scorer 1.0 0.0 0.0 0.0 1.0)) ;; 1-day forgetting
   (define now (parse-ref "2025-06-15T12:00:00Z"))
   ;; Item from 3 days ago → beyond 2-day threshold
   (define item (make-test-item "old-1" "hello" #:updated-at "2025-06-12T00:00:00Z"))
   (check-= (score-recency item now scorer) 0.0 0.001))
 (test-case "score-recency: item within forgetting-days gets good score"
   (define scorer (recall-scorer 1.0 0.0 0.0 0.0 7.0)) ;; 7-day forgetting
   (define now (parse-ref "2025-06-15T12:00:00Z"))
   ;; Item from same day, just 1 hour ago
   (define item (make-test-item "recent-1" "hello" #:updated-at "2025-06-15T11:00:00Z"))
   (check-true (> (score-recency item now scorer) 0.9)))
 ;; ── days-between helper ──
 (test-case "days-between: computes correct day difference"
   (define d (days-between (parse-ref "2025-06-15T00:00:00Z") (parse-ref "2025-06-10T00:00:00Z")))
   (check-= d 5.0 0.1))
 (test-case "days-between: same day is ~0"
   (define d (days-between (parse-ref "2025-06-15T00:00:00Z") (parse-ref "2025-06-15T12:00:00Z")))
   (check-true (< d 1.0))))

;; ============================================================
;; Run
;; ============================================================

(run-tests hybrid-recall-suite)
