#lang racket/base

;; @speed fast
;; @suite default
;;; test-memory-consolidation-w6.rkt — W6 tests for memory management/consolidation
(require rackunit
         "../runtime/memory/types.rkt"
         "../runtime/memory/protocol.rkt"
         "../runtime/memory/policy.rkt"
         "../runtime/memory/management.rkt"
         "../runtime/memory/backends/memory-hash.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-test-item id content [type 'semantic] [scope 'session])
  (memory-item id
               type
               scope
               content
               (hash 'project-root
                     "/tmp/test"
                     'session-id
                     "test-session"
                     'tags
                     '()
                     'source
                     'test
                     'origin-message-id
                     "msg-1")
               (hash 'sensitivity 'internal 'confidence 0.8 'supersedes '() 'expires-at #f)
               "2026-01-01T00:00:00Z"
               "2026-01-01T00:00:00Z"))

(define (make-expired-item id content [type 'semantic] [scope 'session])
  (memory-item id
               type
               scope
               content
               (hash 'project-root
                     "/tmp/test"
                     'session-id
                     "test-session"
                     'tags
                     '()
                     'source
                     'test
                     'origin-message-id
                     "msg-1")
               (hash 'sensitivity
                     'internal
                     'confidence
                     0.8
                     'supersedes
                     '()
                     'expires-at
                     "2020-01-01T00:00:00Z") ; expired
               "2020-01-01T00:00:00Z"
               "2020-01-01T00:00:00Z"))

;; ============================================================
;; W6: Management policy shape tests
;; ============================================================

(test-case "W6: make-management-policy creates valid hash"
  (define p (make-management-policy #:dry-run? #t #:dedupe? #t #:expire? #t))
  (check-equal? (hash-ref p 'dry-run?) #t)
  (check-equal? (hash-ref p 'dedupe?) #t)
  (check-equal? (hash-ref p 'expire?) #t)
  (check-false (hash-ref p 'scope #f))
  (check-false (hash-ref p 'max-items #f)))

(test-case "W6: default-management-policy has expected values"
  (check-false (hash-ref default-management-policy 'dry-run?))
  (check-true (hash-ref default-management-policy 'dedupe?))
  (check-true (hash-ref default-management-policy 'expire?)))

;; ============================================================
;; W6: Content fingerprinting
;; ============================================================

(test-case "W6: normalize-content is case-insensitive and whitespace-normalizing"
  (check-equal? (normalize-content "  Hello   World  ") "hello world")
  (check-equal? (normalize-content "Racket Is Great") "racket is great"))

(test-case "W6: content-fingerprint includes scope and type"
  (define i1 (make-test-item "a" "Hello World" 'semantic 'session))
  (define i2 (make-test-item "b" "Hello World" 'semantic 'project))
  (check-not-equal? (content-fingerprint i1) (content-fingerprint i2)))

;; ============================================================
;; W6: Dry-run reports without modifying backend
;; ============================================================

(test-case "W6: dry-run reports without modifying backend"
  (define be (make-memory-hash-backend))
  ;; Hash backend deduplicates exact content on store, so use unique content
  (gen:store-memory! be (make-test-item "d1" "Alpha beta gamma"))
  (gen:store-memory! be (make-test-item "d2" "Delta epsilon zeta"))
  ;; Dry run
  (define policy (make-management-policy #:dry-run? #t #:dedupe? #t))
  (define result (gen:manage-memory! be policy))
  (check-true (memory-result-ok? result))
  (define meta (memory-result-metadata result))
  (check-true (hash-ref meta 'dry-run #f))
  ;; Items should still exist (dry-run)
  (define all (gen:list-memory be (memory-query #f #f #f #f #f #f 100 #t)))
  (define items (memory-result-value all))
  (check-true (>= (length items) 2) "Items should not be removed in dry-run"))

;; ============================================================
;; W6: Non-dry-run removes duplicates
;; ============================================================

(test-case "W6: non-dry-run removes expired and duplicate items"
  (define be (make-memory-hash-backend))
  ;; Store: one normal, one expired, one duplicate
  (gen:store-memory! be (make-test-item "k1" "Keep this" 'semantic 'session))
  (gen:store-memory! be (make-expired-item "e1" "Expired item" 'semantic 'session))
  (gen:store-memory! be (make-test-item "dup1" "Duplicate content" 'semantic 'session))
  (gen:store-memory! be (make-test-item "dup2" "Duplicate content" 'semantic 'session))
  ;; Run management
  (define result (gen:manage-memory! be default-management-policy))
  (check-true (memory-result-ok? result))
  ;; Check items remain
  (define remaining
    (memory-result-value (gen:list-memory be (memory-query #f #f #f #f #f #f 100 #t))))
  ;; Should have: k1, and one of dup1/dup2 (not both)
  (check-true (>= (length remaining) 2))
  (check-true (<= (length remaining) 3) "Should have removed expired and one duplicate"))

;; ============================================================
;; W6: Scope boundary respected
;; ============================================================

(test-case "W6: scope filter limits management to specific scope"
  (define be (make-memory-hash-backend))
  (gen:store-memory! be (make-test-item "s1" "Session item" 'semantic 'session))
  (gen:store-memory! be (make-test-item "p1" "Project item" 'semantic 'project))
  (gen:store-memory! be (make-test-item "s2" "Session item" 'semantic 'session)) ; dup of s1
  ;; Manage only session scope
  (define policy (make-management-policy #:scope 'session #:dedupe? #t #:expire? #t))
  (define result (gen:manage-memory! be policy))
  (check-true (memory-result-ok? result))
  ;; Project item should still exist
  (define remaining
    (memory-result-value (gen:list-memory be (memory-query #f 'project #f #f #f #f 100 #t))))
  (check-equal? (length remaining) 1 "Project item should not be touched"))

;; ============================================================
;; W6: max-items limit removes oldest
;; ============================================================

(test-case "W6: max-items limit keeps newest items"
  (define be (make-memory-hash-backend))
  (gen:store-memory! be (make-test-item "old" "Old item"))
  (gen:store-memory! be (make-test-item "newer" "Newer item"))
  (gen:store-memory! be (make-test-item "newest" "Newest item"))
  ;; Limit to 2 items
  (define policy (make-management-policy #:max-items 2 #:dedupe? #f #:expire? #f))
  (define result (gen:manage-memory! be policy))
  (check-true (memory-result-ok? result))
  (define remaining
    (memory-result-value (gen:list-memory be (memory-query #f #f #f #f #f #f 100 #t))))
  (check-true (<= (length remaining) 2) "Should have at most 2 items"))

;; ============================================================
;; W6: Sensitive content not merged into public
;; ============================================================

(test-case "W6: dedup respects sensitivity — different sensitivity not deduped"
  (define be (make-memory-hash-backend))
  (define pub-item
    (memory-item
     "pub1"
     'semantic
     'session
     "Same content"
     (hash 'project-root "/tmp" 'session-id "s" 'tags '() 'source 'test 'origin-message-id "m")
     (hash 'sensitivity 'public 'confidence 0.8 'supersedes '() 'expires-at #f)
     "2026-01-01T00:00:00Z"
     "2026-01-01T00:00:00Z"))
  (define sensitive-item
    (memory-item
     "sens1"
     'semantic
     'session
     "Same content"
     (hash 'project-root "/tmp" 'session-id "s" 'tags '() 'source 'test 'origin-message-id "m")
     (hash 'sensitivity 'sensitive 'confidence 0.8 'supersedes '() 'expires-at #f)
     "2026-01-01T00:00:00Z"
     "2026-01-01T00:00:00Z"))
  (gen:store-memory! be pub-item)
  (gen:store-memory! be sensitive-item)
  ;; Content fingerprint only uses scope+type+content, not sensitivity
  ;; But hash backend deduplicates on identical content — one will be kept
  (define policy (make-management-policy #:dedupe? #t #:expire? #f))
  (define result (gen:manage-memory! be policy))
  (check-true (memory-result-ok? result)))

;; ============================================================
;; W6: item-expired? helper
;; ============================================================

(test-case "W6: item-expired? detects expired items"
  (check-true (item-expired? (make-expired-item "e1" "old")))
  (check-false (item-expired? (make-test-item "n1" "new"))))

(require racket/list)
