#lang racket/base

;;; tools/file-mutation-queue.rkt — per-file semaphore for serializing mutations.
;;;
;;; Prevents concurrent read-modify-write races when parallel tool calls
;;; target the same file path.
;;;
;;; WP3.5 (BUG-0056): truthful contention diagnostics. While a path lock is
;;; held the module records the holder label and every registered waiter's
;;; owner label and wait-start timestamp. `file-mutation-queue-diagnostics`
;;; snapshots one `file-lock-diagnostic` per contended path. Labels carry
;;; owner identity and timing only — never file content or command bodies.

(require racket/path
         racket/list)

(provide with-file-mutation-queue
         mutation-queue-stats
         current-file-mutation-queue-hook
         ;; WP3.5 (BUG-0056): truthful file-lock contention diagnostics.
         current-file-mutation-queue-owner
         file-lock-diagnostic
         file-lock-diagnostic?
         file-lock-diagnostic-path
         file-lock-diagnostic-holder
         file-lock-diagnostic-waiter-owners
         file-lock-diagnostic-oldest-wait-ms
         file-mutation-queue-diagnostics)

;; One registry entry owns both the semaphore and its pending-operation count.
;; Looking up the semaphore and incrementing pending must happen under the same
;; mutex; otherwise an entry can be removed while an operation still holds a
;; reference to its semaphore.
;;
;; WP3.5 (BUG-0056): `holder-owner` names the operation that currently holds
;; the path semaphore; `waiters` maps waiter tokens to waiter-record values.
;; All state changes happen under `path-locks-mutex`. These labels carry owner
;; identity and timing only — never file content or command bodies.
(struct queue-entry (semaphore pending holder-owner waiters) #:mutable)

;; WP3.5 (BUG-0056): owner label for the current operation (e.g.
;; "session-id/tool"). Owner metadata is surfaced in diagnostics; no file
;; content or command body is ever recorded here.
(define current-file-mutation-queue-owner (make-parameter "unowned"))

;; WP3.5 (BUG-0056): registered waiter: owner label + wait start timestamp.
(struct waiter-record (owner started-ms) #:transparent)

;; Immutable snapshot of one contended path, for reporting.
(struct file-lock-diagnostic (path holder waiter-owners oldest-wait-ms) #:transparent)

(define path-locks (make-hash))
(define path-locks-mutex (make-semaphore 1))

;; Test-only synchronization point. The hook runs after atomic registration and
;; before waiting on the per-path semaphore. Events: 'registered (always, after
;; atomic registration) and 'lock-wait (only when the caller actually blocks).
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

;; Atomically obtain the path entry, register this pending operation, and
;; record the waiter under its owner label. Returns (cons entry token); the
;; token removes this waiter from the map on acquire/unregister.
(define (register-operation canonical-path owner)
  (call-with-semaphore path-locks-mutex
                       (lambda ()
                         (define entry
                           (hash-ref! path-locks
                                      canonical-path
                                      (lambda () (queue-entry (make-semaphore 1) 0 #f (make-hash)))))
                         (set-queue-entry-pending! entry (add1 (queue-entry-pending entry)))
                         (define token (gensym 'fmq-waiter))
                         (hash-set! (queue-entry-waiters entry)
                                    token
                                    (waiter-record owner (current-inexact-milliseconds)))
                         (cons entry token))))

;; Remove a waiter token and release one pending count. Identity protects
;; against deleting a newer entry if this code is ever changed to permit
;; replacement for the same canonical path.
(define (unregister-operation canonical-path entry token)
  (call-with-semaphore path-locks-mutex
                       (lambda ()
                         (hash-remove! (queue-entry-waiters entry) token)
                         (define pending (sub1 (queue-entry-pending entry)))
                         (set-queue-entry-pending! entry pending)
                         (when (zero? pending)
                           (when (eq? (hash-ref path-locks canonical-path #f) entry)
                             (hash-remove! path-locks canonical-path))))))

;; WP3.5 (BUG-0056): promote a registered waiter to holder. The internal
;; holder pair retains the operation token so cancellation cleanup can clear
;; only its own label and can never erase a successor's label.
(define (mark-holder! entry token owner)
  (call-with-semaphore path-locks-mutex
                       (lambda ()
                         (hash-remove! (queue-entry-waiters entry) token)
                         (set-queue-entry-holder-owner! entry (cons token owner)))))

;; Clear this operation's holder label. The token check makes this safe both
;; on the normal path and as outer cancellation cleanup after the semaphore
;; has already handed off to another operation.
(define (unmark-holder! entry token)
  (call-with-semaphore path-locks-mutex
                       (lambda ()
                         (define holder (queue-entry-holder-owner entry))
                         (when (and (pair? holder) (eq? (car holder) token))
                           (set-queue-entry-holder-owner! entry #f)))))

;; Wrap a thunk so that concurrent calls for the same file path are serialized.
;; path-str is the raw file path (may contain ~, symlinks, etc.).
;; If path-str is #f, run the thunk without serialization.
(define (with-file-mutation-queue path-str thunk)
  (if (not path-str)
      (thunk)
      (let* ([canonical (canonicalize-path path-str)]
             [owner (current-file-mutation-queue-owner)]
             [entry+token (register-operation canonical owner)]
             [entry (car entry+token)]
             [token (cdr entry+token)]
             [sem (queue-entry-semaphore entry)])
        ;; Registration precedes this wind, so every escape from the hook,
        ;; semaphore wait, holder transition, or thunk reaches unregister.
        (dynamic-wind
         void
         (lambda ()
           ((current-file-mutation-queue-hook) 'registered canonical)
           ;; Peek without consuming. `call-with-semaphore` below owns the
           ;; break-safe acquire/release lifecycle; the peek is diagnostics
           ;; only and avoids a lock leak between a try-wait and its cleanup.
           (unless (sync/timeout 0 (semaphore-peek-evt sem))
             ((current-file-mutation-queue-hook) 'lock-wait canonical))
           (call-with-semaphore sem
                                (lambda ()
                                  (dynamic-wind (lambda () (mark-holder! entry token owner))
                                                thunk
                                                (lambda () (unmark-holder! entry token))))))
         (lambda ()
           ;; Also clear here for cancellation during the inner pre-thunk.
           ;; Token ownership prevents clearing a successor after handoff.
           (unmark-holder! entry token)
           (unregister-operation canonical entry token))))))

;; Return the number of paths with active or pending operations (for testing).
(define (mutation-queue-stats)
  (call-with-semaphore path-locks-mutex (lambda () (hash-count path-locks))))

;; WP3.5 (BUG-0056): one immutable diagnostic snapshot per registered path:
;; current holder label, ordered waiter owner labels, and the oldest waiter's
;; wait duration in ms. Metadata only — never file content or command bodies.
(define (file-mutation-queue-diagnostics)
  (call-with-semaphore
   path-locks-mutex
   (lambda ()
     (define now (current-inexact-milliseconds))
     (hash-map path-locks
               (lambda (path entry)
                 (define waiters
                   (sort (hash-map (queue-entry-waiters entry) (lambda (_token rec) rec))
                         <
                         #:key waiter-record-started-ms))
                 (define oldest-start
                   (and (not (null? waiters)) (waiter-record-started-ms (car waiters))))
                 (define holder (queue-entry-holder-owner entry))
                 (file-lock-diagnostic path
                                       (if (pair? holder)
                                           (cdr holder)
                                           holder)
                                       (map waiter-record-owner waiters)
                                       (if oldest-start
                                           (max 0 (inexact->exact (floor (- now oldest-start))))
                                           0)))))))
