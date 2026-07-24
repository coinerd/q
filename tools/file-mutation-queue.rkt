#lang racket/base

;;; tools/file-mutation-queue.rkt — per-file semaphore for serializing mutations.
;;;
;;; Prevents concurrent read-modify-write races when parallel tool calls
;;; target the same file path.

(require racket/path)

(provide with-file-mutation-queue
         mutation-queue-stats
         current-file-mutation-queue-hook)

;; One registry entry owns both the semaphore and its pending-operation count.
;; Looking up the semaphore and incrementing pending must happen under the same
;; mutex; otherwise an entry can be removed while an operation still holds a
;; reference to its semaphore.
(struct queue-entry (semaphore pending) #:mutable)

(define path-locks (make-hash))
(define path-locks-mutex (make-semaphore 1))

;; Test-only synchronization point. The hook runs after atomic registration and
;; before waiting on the per-path semaphore.
(define current-file-mutation-queue-hook (make-parameter (lambda (_event _canonical-path) (void))))

;; Resolve every spelling to an absolute, simplified queue key. `simplify-path`
;; may consult existing path prefixes, but it does not require the final target
;; to exist. A final `resolve-path` retains direct-symlink deduplication and also
;; returns the input for ordinary nonexistent targets. If filesystem-assisted
;; simplification fails, retain absolute lexical canonicalization instead of
;; falling back to the caller's raw, possibly relative spelling.
(define (canonicalize-path path-str)
  (define complete (path->complete-path (expand-user-path path-str)))
  (define simplified
    (with-handlers ([exn:fail:filesystem? (lambda (_) (simplify-path complete #f))])
      (simplify-path complete)))
  (path->string (with-handlers ([exn:fail:filesystem? (lambda (_) simplified)])
                  (define resolved (resolve-path simplified))
                  ;; A relative symlink target is relative to the link's parent, not to the
                  ;; process current directory. Keep the resulting queue key absolute.
                  (simplify-path (if (complete-path? resolved)
                                     resolved
                                     (path->complete-path resolved
                                                          (or (path-only simplified)
                                                              (current-directory))))))))

;; Atomically obtain the path semaphore and register this pending operation.
(define (register-operation canonical-path)
  (call-with-semaphore
   path-locks-mutex
   (lambda ()
     (define entry
       (hash-ref! path-locks canonical-path (lambda () (queue-entry (make-semaphore 1) 0))))
     (set-queue-entry-pending! entry (add1 (queue-entry-pending entry)))
     entry)))

(define (unregister-operation canonical-path entry)
  (call-with-semaphore path-locks-mutex
                       (lambda ()
                         (define pending (sub1 (queue-entry-pending entry)))
                         (set-queue-entry-pending! entry pending)
                         (when (zero? pending)
                           ;; Identity protects against deleting a newer entry if this code is
                           ;; changed later to permit replacement for the same canonical path.
                           (when (eq? (hash-ref path-locks canonical-path #f) entry)
                             (hash-remove! path-locks canonical-path))))))

;; Wrap a thunk so that concurrent calls for the same file path are serialized.
;; path-str is the raw file path (may contain ~, symlinks, etc.).
;; If path-str is #f, run the thunk without serialization.
(define (with-file-mutation-queue path-str thunk)
  (if (not path-str)
      (thunk)
      (let* ([canonical (canonicalize-path path-str)]
             [entry (register-operation canonical)])
        (dynamic-wind void
                      (lambda ()
                        ((current-file-mutation-queue-hook) 'registered canonical)
                        (call-with-semaphore (queue-entry-semaphore entry) thunk))
                      (lambda () (unregister-operation canonical entry))))))

;; Return the number of paths with active or pending operations (for testing).
(define (mutation-queue-stats)
  (call-with-semaphore path-locks-mutex (lambda () (hash-count path-locks))))
