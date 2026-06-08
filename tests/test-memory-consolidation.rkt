#lang racket/base

;; @speed fast
;; @suite default
;;; test-memory-consolidation.rkt — W0/W6 characterization tests
;;;
;;; W0: Characterize current management/consolidation gaps.
;;; W6: Verify deterministic consolidation (after implementation).
(require rackunit
         "../runtime/memory/types.rkt"
         "../runtime/memory/protocol.rkt"
         "../runtime/memory/policy.rkt"
         "../runtime/memory/backends/memory-hash.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define now-iso "2026-06-07T12:00:00Z")

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
               (hash 'sensitivity 'internal 'confidence 0.8 'supersedes '())
               now-iso
               now-iso))

;; ============================================================
;; W0: Current management/consolidation characterization
;; ============================================================

(test-case "W0: manage! exists but may be no-op"
  (define be (make-memory-hash-backend))
  (check-true (memory-backend? be))
  ;; manage! should exist as a procedure
  (check-true (procedure? (memory-backend-manage! be)) "manage! should be a procedure"))

(test-case "W0: manage! returns a result for hash backend"
  (define be (make-memory-hash-backend))
  ;; Store some items first
  ((memory-backend-store! be) (make-test-item "c1" "Racket is a Lisp dialect"))
  ((memory-backend-store! be) (make-test-item "c2" "The project uses Racket"))
  ;; Call manage with a simple policy
  (define result ((memory-backend-manage! be) default-memory-policy))
  ;; Result should be a memory-result (ok or not)
  (check-true (or (memory-result? result) (eq? result #f))
              "manage should return memory-result or #f"))

(test-case "W0: duplicate content is idempotent (dedup by content)"
  (define be (make-memory-hash-backend))
  ((memory-backend-store! be) (make-test-item "d1" "Same content here" 'semantic 'session))
  ((memory-backend-store! be) (make-test-item "d2" "Same content here" 'semantic 'session))
  ;; Hash backend deduplicates identical content (idempotent)
  (define all ((memory-backend-list be)
               (memory-query #f 'session #f #f #f #f 100 #f)))
  (define items (if (memory-result? all) (memory-result-value all) all))
  (check-equal? (length items) 1 "identical content should be deduped"))

(test-case "W0: consolidation placeholder — no auto-merge"
  ;; After W6, calling manage! should detect duplicates
  ;; and mark older ones as superseded
  (define be (make-memory-hash-backend))
  ((memory-backend-store! be) (make-test-item "m1" "Memory fact A" 'semantic 'project))
  ((memory-backend-store! be) (make-test-item "m2" "Memory fact A" 'semantic 'project))
  (define result ((memory-backend-manage! be) default-memory-policy))
  ;; Currently this may be a no-op — characterize
  (check-true (or (memory-result? result) (eq? result #f))
              "manage result should be memory-result or #f"))
