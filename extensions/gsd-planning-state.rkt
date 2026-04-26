#lang racket/base

;; extensions/gsd-planning-state.rkt — Semaphore-protected shared state for GSD planning
;;
;; Extracted from gsd-planning.rkt and tools/builtins/edit.rkt.
;; All cross-thread shared mutable state lives here with semaphore protection.
;;
;; Wave 0 of v0.20.3: Addresses C1 (race condition), C2 (pinned-planning-dir parameter),
;; W2 (non-atomic resets).

(require racket/contract)

(provide gsd-mode
         gsd-mode?
         set-gsd-mode!
         pinned-planning-dir
         set-pinned-planning-dir!
         go-read-budget
         set-go-read-budget!
         decrement-budget!
         reset-go-budget!
         GO-READ-BUDGET
         GO-READ-WARN-THRESHOLD
         GO-READ-BLOCK-THRESHOLD
         read-counts
         get-read-count
         increment-read-count!
         clear-read-counts!
         current-max-old-text-len
         set-current-max-old-text-len!
         reset-all-gsd-state!
         READ-ONLY-TOOLS)

;; ============================================================
;; Internal state storage
;; ============================================================

;; Single semaphore protects all state below.
;; This is simpler and more correct than per-value semaphores because
;; cross-thread operations (e.g. budget check + decrement) need to be atomic.
(define state-sem (make-semaphore 1))

;; GSD workflow mode: #f | 'planning | 'plan-written | 'executing
(define gsd-mode-box (box #f))

;; Pinned planning directory — set once during tool registration.
;; Was a parameter (C2: thread-isolated); now a box for cross-thread visibility.
(define pinned-dir-box (box #f))

;; Read budget for /go execution sessions.
(define budget-box (box #f))

;; Dynamic edit limit (moved from tools/builtins/edit.rkt).
(define edit-limit-box (box 500))

;; Per-file read count for redundant-read detection.
(define read-counts-box (box (make-hash)))

;; ============================================================
;; Constants
;; ============================================================

(define GO-READ-BUDGET 30)
(define GO-READ-WARN-THRESHOLD 5)
(define GO-READ-BLOCK-THRESHOLD -3)

(define READ-ONLY-TOOLS '("read" "grep" "find" "ls" "glob"))

;; ============================================================
;; Semaphore-protected accessors
;; ============================================================

;; --- gsd-mode ---

(define (gsd-mode)
  (call-with-semaphore state-sem (lambda () (unbox gsd-mode-box))))

(define (gsd-mode? v)
  (eq? (gsd-mode) v))

(define (set-gsd-mode! v)
  (call-with-semaphore state-sem
                       (lambda ()
                         (log-debug (format "gsd-planning: mode ~a → ~a" (unbox gsd-mode-box) v))
                         (set-box! gsd-mode-box v))))

;; --- pinned-planning-dir ---

(define (pinned-planning-dir)
  (call-with-semaphore state-sem (lambda () (unbox pinned-dir-box))))

(define (set-pinned-planning-dir! v)
  (call-with-semaphore state-sem (lambda () (set-box! pinned-dir-box v))))

;; --- go-read-budget ---

(define (go-read-budget)
  (call-with-semaphore state-sem (lambda () (unbox budget-box))))

(define (set-go-read-budget! v)
  (call-with-semaphore state-sem (lambda () (set-box! budget-box v))))

;; Atomic decrement-and-read: returns the NEW value after decrement.
(define (decrement-budget!)
  (call-with-semaphore state-sem
                       (lambda ()
                         (define new-val (sub1 (unbox budget-box)))
                         (set-box! budget-box new-val)
                         new-val)))

(define (reset-go-budget!)
  (call-with-semaphore state-sem (lambda () (set-box! budget-box GO-READ-BUDGET))))

;; --- read-counts ---

;; Returns a snapshot of the current read-counts hash.
;; NOTE: This returns a COPY, not the live hash. For mutation, use
;; get-read-count / increment-read-count! / clear-read-counts!.
(define (read-counts)
  (call-with-semaphore state-sem (lambda () (hash-copy (unbox read-counts-box)))))

(define (get-read-count key)
  (call-with-semaphore state-sem (lambda () (hash-ref (unbox read-counts-box) key 0))))

(define (increment-read-count! key)
  (call-with-semaphore state-sem
                       (lambda ()
                         (define h (unbox read-counts-box))
                         (define new-count (add1 (hash-ref h key 0)))
                         (hash-set! h key new-count)
                         new-count)))

(define (clear-read-counts!)
  (call-with-semaphore state-sem (lambda () (set-box! read-counts-box (make-hash)))))

;; --- current-max-old-text-len ---

(define (current-max-old-text-len)
  (call-with-semaphore state-sem (lambda () (unbox edit-limit-box))))

(define (set-current-max-old-text-len! v)
  (call-with-semaphore state-sem (lambda () (set-box! edit-limit-box v))))

;; ============================================================
;; Atomic reset
;; ============================================================

;; Resets ALL GSD state atomically under the semaphore.
;; Called on session-shutdown and between test runs.
(define (reset-all-gsd-state!)
  (call-with-semaphore state-sem
                       (lambda ()
                         (set-box! gsd-mode-box #f)
                         (set-box! pinned-dir-box #f)
                         (set-box! budget-box #f)
                         (set-box! edit-limit-box 500)
                         (set-box! read-counts-box (make-hash)))))
