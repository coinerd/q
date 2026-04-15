#lang racket/base

;;; tools/file-mutation-queue.rkt — per-file semaphore for serializing mutations.
;;;
;;; Prevents concurrent read-modify-write races when parallel tool calls
;;; target the same file path.
;;;
;;; Exports:
;;;   - with-file-mutation-queue — wrap a thunk with per-path serialization
;;;   - mutation-queue-stats     — (for testing) return active count

(require racket/path)

(provide with-file-mutation-queue
         mutation-queue-stats)

;; ============================================================
;; Internal state
;; ============================================================

;; Global hash: resolved-path-string → (box count)
;; The box tracks pending operations; when it drops to 0, entry is removed.
(define path-locks (make-hash))
(define path-locks-mutex (make-semaphore 1))

;; Per-path semaphores: resolved-path-string → semaphore
(define path-semaphores (make-hash))
(define path-sema-mutex (make-semaphore 1))

;; ============================================================
;; Resolve a path to its canonical form for deduplication
;; ============================================================

(define (canonicalize-path path-str)
  (with-handlers ([exn:fail? (lambda (_) path-str)])
    ;; resolve-path handles symlinks; needs an actual path
    (if (file-exists? path-str)
        (path->string (resolve-path (string->path path-str)))
        ;; For non-existent files, normalize the string
        (path->string (simplify-path (string->path path-str))))))

;; ============================================================
;; Get or create a semaphore for a path
;; ============================================================

(define (get-path-semaphore canonical-path)
  (call-with-semaphore path-sema-mutex
    (lambda ()
      (hash-ref! path-semaphores canonical-path (lambda () (make-semaphore 1))))))

;; ============================================================
;; Acquire/release tracking
;; ============================================================

(define (track-acquire canonical-path)
  (call-with-semaphore path-locks-mutex
    (lambda ()
      (define existing (hash-ref path-locks canonical-path #f))
      (if existing
          (set-box! existing (add1 (unbox existing)))
          (hash-set! path-locks canonical-path (box 1))))))

(define (track-release canonical-path)
  (call-with-semaphore path-locks-mutex
    (lambda ()
      (define existing (hash-ref path-locks canonical-path #f))
      (when existing
        (set-box! existing (sub1 (unbox existing)))
        (when (zero? (unbox existing))
          (hash-remove! path-locks canonical-path)
          (hash-remove! path-semaphores canonical-path))))))

;; ============================================================
;; Public API
;; ============================================================

;; Wrap a thunk so that concurrent calls for the same file path are serialized.
;; path-str is the raw file path (may contain ~, symlinks, etc.)
;; If path-str is #f, runs the thunk without serialization.
(define (with-file-mutation-queue path-str thunk)
  (if (not path-str)
      (thunk)
      (let* ([canonical (canonicalize-path path-str)]
             [sema (get-path-semaphore canonical)])
        (track-acquire canonical)
        (dynamic-wind
          (lambda () (semaphore-wait sema))
          thunk
          (lambda ()
            (semaphore-post sema)
            (track-release canonical))))))

;; Return the number of active (pending) path locks (for testing)
(define (mutation-queue-stats)
  (call-with-semaphore path-locks-mutex
    (lambda ()
      (hash-count path-locks))))
