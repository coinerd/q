#lang racket

;; tests/test-gsd-planning-state.rkt — Semaphore-protected state module tests
;;
;; Wave 0 of v0.20.3: Tests for gsd-planning-state.rkt.
;; Covers: semaphore-protected concurrent access, atomic reset,
;; pinned-dir cross-thread visibility, edit-limit, budget, read-counts.

(require rackunit
         "../extensions/gsd-planning-state.rkt")

;; ============================================================
;; gsd-mode tests
;; ============================================================

(test-case "gsd-mode defaults to #f"
  (reset-all-gsd-state!)
  (check-equal? (gsd-mode) #f))

(test-case "set-gsd-mode! sets and reads back"
  (reset-all-gsd-state!)
  (set-gsd-mode! 'planning)
  (check-eq? (gsd-mode) 'planning)
  (set-gsd-mode! 'executing)
  (check-eq? (gsd-mode) 'executing))

(test-case "gsd-mode? checks current mode"
  (reset-all-gsd-state!)
  (set-gsd-mode! 'planning)
  (check-true (gsd-mode? 'planning))
  (check-false (gsd-mode? 'executing)))

;; ============================================================
;; pinned-planning-dir tests
;; ============================================================

(test-case "pinned-planning-dir defaults to #f"
  (reset-all-gsd-state!)
  (check-false (pinned-planning-dir)))

(test-case "set-pinned-planning-dir! sets and reads back"
  (reset-all-gsd-state!)
  (set-pinned-planning-dir! "/tmp/test")
  (check-equal? (pinned-planning-dir) "/tmp/test"))

(test-case "pinned-planning-dir visible across threads"
  (reset-all-gsd-state!)
  (set-pinned-planning-dir! "/tmp/cross-thread")
  (define result-box (box #f))
  (thread (lambda () (set-box! result-box (pinned-planning-dir))))
  (sync (system-idle-evt))
  (check-equal? (unbox result-box) "/tmp/cross-thread"))

;; ============================================================
;; go-read-budget tests
;; ============================================================

(test-case "go-read-budget defaults to #f"
  (reset-all-gsd-state!)
  (check-false (go-read-budget)))

(test-case "reset-go-budget! sets budget to GO-READ-BUDGET"
  (reset-all-gsd-state!)
  (reset-go-budget!)
  (check-equal? (go-read-budget) GO-READ-BUDGET)
  (check-equal? (go-read-budget) 30))

(test-case "set-go-read-budget! sets arbitrary value"
  (reset-all-gsd-state!)
  (set-go-read-budget! 10)
  (check-equal? (go-read-budget) 10))

(test-case "decrement-budget! decrements and returns new value"
  (reset-all-gsd-state!)
  (reset-go-budget!)
  (define v1 (decrement-budget!))
  (check-equal? v1 29)
  (check-equal? (go-read-budget) 29)
  (define v2 (decrement-budget!))
  (check-equal? v2 28))

;; ============================================================
;; current-max-old-text-len tests
;; ============================================================

(test-case "current-max-old-text-len defaults to 500"
  (reset-all-gsd-state!)
  (check-equal? (current-max-old-text-len) 500))

(test-case "set-current-max-old-text-len! sets and reads back"
  (reset-all-gsd-state!)
  (set-current-max-old-text-len! 1200)
  (check-equal? (current-max-old-text-len) 1200))

(test-case "current-max-old-text-len visible across threads"
  (reset-all-gsd-state!)
  (set-current-max-old-text-len! 999)
  (define result-box (box #f))
  (thread (lambda () (set-box! result-box (current-max-old-text-len))))
  (sync (system-idle-evt))
  (check-equal? (unbox result-box) 999))

;; ============================================================
;; read-counts tests
;; ============================================================

(test-case "read-counts starts empty"
  (reset-all-gsd-state!)
  (define counts (read-counts))
  (check-equal? (hash-count counts) 0))

(test-case "increment-read-count! adds and returns count"
  (reset-all-gsd-state!)
  (define c1 (increment-read-count! "/tmp/a.txt"))
  (check-equal? c1 1)
  (define c2 (increment-read-count! "/tmp/a.txt"))
  (check-equal? c2 2)
  (check-equal? (get-read-count "/tmp/a.txt") 2))

(test-case "get-read-count returns 0 for unknown keys"
  (reset-all-gsd-state!)
  (check-equal? (get-read-count "/tmp/unknown.txt") 0))

(test-case "clear-read-counts! removes all entries"
  (reset-all-gsd-state!)
  (increment-read-count! "/tmp/a.txt")
  (increment-read-count! "/tmp/b.txt")
  (clear-read-counts!)
  (check-equal? (get-read-count "/tmp/a.txt") 0)
  (check-equal? (get-read-count "/tmp/b.txt") 0)
  (check-equal? (hash-count (read-counts)) 0))

(test-case "read-counts tracks different files independently"
  (reset-all-gsd-state!)
  (increment-read-count! "/tmp/a.txt")
  (increment-read-count! "/tmp/a.txt")
  (increment-read-count! "/tmp/b.txt")
  (check-equal? (get-read-count "/tmp/a.txt") 2)
  (check-equal? (get-read-count "/tmp/b.txt") 1))

;; ============================================================
;; reset-all-gsd-state! tests
;; ============================================================

(test-case "reset-all-gsd-state! resets everything atomically"
  (reset-all-gsd-state!)
  ;; Set all state to non-default values
  (set-gsd-mode! 'executing)
  (set-pinned-planning-dir! "/tmp/gsd-test-dir")
  (reset-go-budget!)
  (set-current-max-old-text-len! 1200)
  (increment-read-count! "/tmp/x.txt")
  ;; Reset
  (reset-all-gsd-state!)
  ;; Verify all cleared
  (check-equal? (gsd-mode) #f)
  (check-false (pinned-planning-dir))
  (check-false (go-read-budget))
  (check-equal? (current-max-old-text-len) 500)
  (check-equal? (hash-count (read-counts)) 0))

;; ============================================================
;; Concurrent access tests
;; ============================================================

(test-case "concurrent budget decrement has no lost updates"
  (reset-all-gsd-state!)
  (set-go-read-budget! 1000)
  (define n-threads 10)
  (define decrements-per-thread 100)
  (define threads
    (for/list ([_ (in-range n-threads)])
      (thread (lambda ()
                (for ([_ (in-range decrements-per-thread)])
                  (decrement-budget!))))))
  (for-each sync threads)
  ;; Expected: 1000 - (10 * 100) = 0
  (check-equal? (go-read-budget) 0))

(test-case "concurrent read-count increment has no lost updates"
  (reset-all-gsd-state!)
  (define n-threads 10)
  (define increments-per-thread 100)
  (define threads
    (for/list ([_ (in-range n-threads)])
      (thread (lambda ()
                (for ([_ (in-range increments-per-thread)])
                  (increment-read-count! "/tmp/concurrent.txt"))))))
  (for-each sync threads)
  ;; Expected: 10 * 100 = 1000
  (check-equal? (get-read-count "/tmp/concurrent.txt") 1000))

(test-case "concurrent gsd-mode writes — last writer wins"
  (reset-all-gsd-state!)
  (define threads
    (for/list ([i (in-range 100)])
      (thread (lambda () (set-gsd-mode! (if (even? i) 'planning 'executing))))))
  (for-each sync threads)
  ;; Value must be one of the two valid modes
  (check-true (or (eq? (gsd-mode) 'planning) (eq? (gsd-mode) 'executing))
              "mode must be a valid symbol after concurrent writes"))

(test-case "constants have expected values"
  (check-equal? GO-READ-BUDGET 30)
  (check-equal? GO-READ-WARN-THRESHOLD 5)
  (check-equal? GO-READ-BLOCK-THRESHOLD -3)
  (check-equal? READ-ONLY-TOOLS '("read" "grep" "find" "ls" "glob")))
