#lang racket/base

;; runtime/memory/backends/file-jsonl.rkt — Project-local JSONL memory backend
;;
;; Durable append-only JSONL storage for memory items.
;; v0.96.6 (F8): Extracted operations to file-jsonl-ops.rkt.
;; This module is now a thin constructor.

(require racket/file
         racket/path
         "../types.rkt"
         "../protocol.rkt"
         "file-jsonl-ops.rkt")

(provide make-file-jsonl-backend
         file-jsonl-backend-path
         safe-memory-path)

;; Backend path registry — weak hash from backend -> actual jsonl-path (P3-4)
(define backend-paths (make-weak-hasheq))

(define (file-jsonl-backend-path backend)
  (hash-ref backend-paths backend (memory-backend-name backend)))

;; ---------------------------------------------------------------------------
;; Constructor: creates closures over shared mutable cache state
;; ---------------------------------------------------------------------------

(define (make-file-jsonl-backend memory-root)
  ;; Lazy directory creation
  (define dir-initialized? #f)
  (define (ensure-dir!)
    (unless dir-initialized?
      (unless (directory-exists? memory-root)
        (make-directory* memory-root))
      (restrict-path-permissions! memory-root #o700)
      (set! dir-initialized? #t)))
  (define jsonl-path (build-path memory-root "memory.jsonl"))

  ;; In-memory cache with incremental processing (M13-F6)
  (define cache #f)
  (define cache-dirty? #f)
  (define cached-file-size 0)

  (define (get-view)
    (define current-size
      (if (file-exists? jsonl-path)
          (file-size jsonl-path)
          0))
    (cond
      [(and cache (not cache-dirty?) (= current-size cached-file-size)) cache]
      [else
       (set! cache
             (if (file-exists? jsonl-path)
                 (incremental-load-view jsonl-path cache cached-file-size)
                 (hash)))
       (set! cached-file-size current-size)
       (set! cache-dirty? #f)
       cache]))

  (define (invalidate-cache!)
    (set! cache-dirty? #t))
  (define (set-cache! v)
    (set! cache v)
    (set! cache-dirty? #f))
  (define (set-cache-size! v)
    (set! cached-file-size v))

  (define the-backend
    (memory-backend
     "file-jsonl"
     (lambda (item) (fjl-store! get-view ensure-dir! invalidate-cache! memory-root item))
     (lambda (query) (fjl-retrieve get-view query))
     (lambda (id patch) (fjl-update! get-view ensure-dir! invalidate-cache! memory-root id patch))
     (lambda (id scope) (fjl-delete! get-view ensure-dir! invalidate-cache! memory-root id scope))
     (lambda (query) (fjl-list-items get-view query))
     (lambda () #t) ; available?
     (lambda (policy)
       (fjl-manage! get-view ensure-dir! set-cache! set-cache-size! memory-root policy))))
  (hash-set! backend-paths the-backend (path->string jsonl-path))
  the-backend)
