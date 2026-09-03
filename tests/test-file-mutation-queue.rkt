#lang racket

;; @speed fast
;; @suite default
;; @boundary integration

;; BOUNDARY: integration

;;; tests/test-file-mutation-queue.rkt — tests for per-file serialization.
;;;
;;; Verifies:
;;;   - Two concurrent edits on same file are serialized (no race)
;;;   - Different files run in parallel
;;;   - Self-cleaning: semaphore removed after completion
;;;   - #f path runs without serialization

(require rackunit
         racket/file
         "../tools/file-mutation-queue.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-temp-file)
  (define tmp (make-temporary-file "/tmp/mq-test-~a"))
  (with-output-to-file tmp (lambda () (display "0")) #:exists 'replace)
  tmp)

(define (increment-file path-str)
  (with-file-mutation-queue path-str
                            (lambda ()
                              ;; read-modify-write
                              (define val
                                (with-input-from-file (string->path path-str)
                                                      (lambda () (string->number (port->string)))))
                              (sleep 0.01) ;; simulate latency to make races more likely
                              (with-output-to-file (string->path path-str)
                                                   (lambda () (display (add1 val)))
                                                   #:exists 'replace))))

(define (check-path-spellings-serialize first-path second-path)
  (define holder-entered (make-semaphore 0))
  (define release-holder (make-semaphore 0))
  (define contender-entered (make-semaphore 0))
  (define holder
    (thread (lambda ()
              (with-file-mutation-queue first-path
                                        (lambda ()
                                          (semaphore-post holder-entered)
                                          (semaphore-wait release-holder))))))
  (semaphore-wait holder-entered)
  (define contender
    (thread (lambda ()
              (with-file-mutation-queue second-path (lambda () (semaphore-post contender-entered))))))
  (check-false (sync/timeout 0.05 contender-entered)
               "equivalent path spellings must share one queue lock")
  (semaphore-post release-holder)
  (thread-wait holder)
  (thread-wait contender)
  (check-not-false (sync/timeout 0 contender-entered)
                   "contender should run after the shared lock is released"))

;; ============================================================
;; Tests
;; ============================================================

(test-case "two concurrent edits on same file: no data loss"
  (define tmp (make-temp-file))
  (define path-str (path->string tmp))
  ;; Run 10 parallel increments — without the queue, some would be lost
  (define threads
    (for/list ([_ (in-range 10)])
      (thread (lambda () (increment-file path-str)))))
  (for-each thread-wait threads)
  (define final (with-input-from-file tmp (lambda () (string->number (port->string)))))
  (check-equal? final 10 "all 10 increments should be preserved")
  (delete-file tmp))

(test-case "different files run in parallel"
  (define tmp1 (make-temp-file))
  (define tmp2 (make-temp-file))
  (define path1 (path->string tmp1))
  (define path2 (path->string tmp2))
  (define started (current-inexact-milliseconds))
  (define t1 (thread (lambda () (increment-file path1))))
  (define t2 (thread (lambda () (increment-file path2))))
  (thread-wait t1)
  (thread-wait t2)
  (define elapsed (- (current-inexact-milliseconds) started))
  ;; If truly parallel, should be ~10ms, not ~20ms
  ;; Use generous threshold for slow CI runners
  (check-true (< elapsed 200)
              (format "two different files should run in parallel, took ~ams" elapsed))
  (delete-file tmp1)
  (delete-file tmp2))

(test-case "self-cleaning: stats drop to zero after completion"
  (define tmp (make-temp-file))
  (define path-str (path->string tmp))
  (with-file-mutation-queue path-str (lambda () 'done))
  (check-equal? (mutation-queue-stats) 0 "no active locks after completion")
  (delete-file tmp))

(test-case "#f path runs without serialization"
  (define result (with-file-mutation-queue #f (lambda () 42)))
  (check-equal? result 42 "#f path runs thunk directly"))

(test-case "queue stats reflect active operations"
  (define tmp (make-temp-file))
  (define path-str (path->string tmp))
  (define ch (make-channel))
  (thread (lambda ()
            (with-file-mutation-queue path-str
                                      (lambda ()
                                        (channel-put ch 'started)
                                        (sleep 0.05)
                                        'done))))
  (channel-get ch) ;; wait for start
  (check-true (positive? (mutation-queue-stats)) "should have 1 active lock during operation")
  ;; Poll until stats drop to zero (replaces fragile sleep)
  (let poll ([attempts 0])
    (when (and (< attempts 50) (positive? (mutation-queue-stats)))
      (sync/timeout 0.02 never-evt)
      (poll (add1 attempts))))
  (check-equal? (mutation-queue-stats) 0 "active lock cleaned up after completion")
  (delete-file tmp))

(test-case "registration and semaphore lookup are one atomic lifecycle step"
  (define tmp (make-temp-file))
  (define path-str (path->string tmp))
  (define first-registration-ready (make-semaphore 0))
  (define release-first-registration (make-semaphore 0))
  (define a-entered (make-semaphore 0))
  (define c-entered (make-semaphore 0))
  (define release-c (make-semaphore 0))
  (define first-registration? #t)
  (define (interleave-hook event _path)
    (when (and (eq? event 'registered) first-registration?)
      (set! first-registration? #f)
      (semaphore-post first-registration-ready)
      (semaphore-wait release-first-registration)))
  (parameterize ([current-file-mutation-queue-hook interleave-hook])
    (define a #f)
    (define b #f)
    (define c #f)
    ;; A is registered but deliberately held before waiting on the path semaphore.
    (set! a
          (thread (lambda ()
                    (with-file-mutation-queue path-str (lambda () (semaphore-post a-entered))))))
    (semaphore-wait first-registration-ready)
    ;; B may finish while A is paused. The registry must retain A's semaphore.
    (set! b (thread (lambda () (with-file-mutation-queue path-str void))))
    (thread-wait b)
    ;; C must still receive that same semaphore.
    (set! c
          (thread (lambda ()
                    (with-file-mutation-queue path-str
                                              (lambda ()
                                                (semaphore-post c-entered)
                                                (semaphore-wait release-c))))))
    (semaphore-wait c-entered)
    (semaphore-post release-first-registration)
    (check-false (sync/timeout 0.05 a-entered)
                 "A must not enter on a detached semaphore while C holds the path lock")
    (semaphore-post release-c)
    (thread-wait c)
    (thread-wait a)
    (check-equal? (mutation-queue-stats) 0))
  (delete-file tmp))

(test-case "relative and absolute paths share the same lock"
  (define dir (make-temporary-file "mq-relative-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (define absolute (build-path dir "target.txt"))
                  (display-to-file "unchanged" absolute)
                  (parameterize ([current-directory dir])
                    (check-path-spellings-serialize "target.txt" absolute)))
                (lambda () (delete-directory/files dir))))

(test-case "nonexistent target path spellings share the same lock"
  (define dir (make-temporary-file "mq-nonexistent-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (define absolute (build-path dir "future.txt"))
                  (check-false (file-exists? absolute))
                  (parameterize ([current-directory dir])
                    (check-path-spellings-serialize "./future.txt" absolute))
                  (check-false (file-exists? absolute)
                               "queue canonicalization must not create the target"))
                (lambda () (delete-directory/files dir))))

(test-case "symlink resolves to same lock"
  (define tmp (make-temp-file))
  (define path-str (path->string tmp))
  (define link-path "/tmp/mq-test-link")
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (when (file-exists? link-path)
      (delete-file link-path))
    (make-file-or-directory-link tmp link-path)
    (define t1 (thread (lambda () (increment-file path-str))))
    (define t2 (thread (lambda () (increment-file link-path))))
    (thread-wait t1)
    (thread-wait t2)
    (define final (with-input-from-file tmp (lambda () (string->number (port->string)))))
    (check-equal? final 2 "symlink and real path should share the same lock")
    (when (file-exists? link-path)
      (delete-file link-path)))
  (when (file-exists? tmp)
    (delete-file tmp)))

;; ============================================================
;; WP3.5 (BUG-0056): holder/waiter diagnostics — truthful file-lock
;; contention without content leakage.
;; ============================================================

(define (find-diag diags path)
  (for/first ([d (in-list diags)]
              #:when (string=? (file-lock-diagnostic-path d) path))
    d))

(test-case "holder and waiter owners are identifiable with wait duration"
  (define tmp (make-temp-file))
  (define path-str (path->string tmp))
  (define holder-entered (make-semaphore 0))
  (define release-holder (make-semaphore 0))
  (define waiter-waiting (make-semaphore 0))
  (define waiter-entered (make-semaphore 0))
  (define holder
    (thread (lambda ()
              (parameterize ([current-file-mutation-queue-owner "session-A/edit"])
                (with-file-mutation-queue path-str
                                          (lambda ()
                                            (semaphore-post holder-entered)
                                            (semaphore-wait release-holder)))))))
  (semaphore-wait holder-entered)
  (define waiter-releasing (make-semaphore 0))
  (define waiter
    (thread (lambda ()
              (parameterize ([current-file-mutation-queue-owner "session-B/edit"]
                             [current-file-mutation-queue-hook (lambda (event _path)
                                                                 (when (eq? event 'lock-wait)
                                                                   (semaphore-post waiter-waiting)))])
                (with-file-mutation-queue path-str
                                          (lambda ()
                                            (semaphore-post waiter-entered)
                                            ;; Hold the lock so the handoff snapshot below
                                            ;; is deterministic.
                                            (semaphore-wait waiter-releasing)))))))
  (semaphore-wait waiter-waiting)
  ;; While blocked, diagnostics must identify holder and waiter.
  (define diags (file-mutation-queue-diagnostics))
  (define d (find-diag diags path-str))
  (check-not-false d "the contended path must appear in diagnostics")
  (when d
    (check-equal? (file-lock-diagnostic-holder d) "session-A/edit" "holder owner must be reported")
    (check-equal? (file-lock-diagnostic-waiter-owners d)
                  (list "session-B/edit")
                  "waiter owners must be reported")
    (check-true (real? (file-lock-diagnostic-oldest-wait-ms d)) "wait duration must be tracked"))
  ;; Wait duration advances while blocked (truthful timing, no fixed zero).
  (sync/timeout 0.08 never-evt)
  (define d2 (find-diag (file-mutation-queue-diagnostics) path-str))
  (when (and d d2)
    (check-true (>= (file-lock-diagnostic-oldest-wait-ms d2) (file-lock-diagnostic-oldest-wait-ms d))
                "wait duration must be monotonic while blocked"))
  ;; No content leakage: the file contains "0"; diagnostics carry only
  ;; owner labels, path, and timing — never body/content payloads.
  (check-false (member "0"
                       (list (and d (file-lock-diagnostic-holder d))
                             (and d2 (file-lock-diagnostic-holder d2))))
               "diagnostics must not leak file content")
  ;; Release: waiter acquires, becomes the holder, waiters clear. The waiter
  ;; holds the lock until explicitly released, so d3 is deterministic.
  (semaphore-post release-holder)
  (sync/timeout 1 waiter-entered)
  (thread-wait holder)
  (define d3 (find-diag (file-mutation-queue-diagnostics) path-str))
  (check-not-false d3 "waiter must still hold the lock at the handoff snapshot")
  (when d3
    (check-equal? (file-lock-diagnostic-holder d3)
                  "session-B/edit"
                  "waiter becomes holder after release")
    (check-equal? (file-lock-diagnostic-waiter-owners d3) (list) "no waiters remain after handoff"))
  (semaphore-post waiter-releasing)
  (thread-wait waiter)
  (check-equal? (mutation-queue-stats) 0 "registry cleans up after both finish")
  (delete-file tmp))

(test-case "immediate acquisition raises no lock-wait event"
  (define tmp (make-temp-file))
  (define path-str (path->string tmp))
  (define events '())
  (parameterize ([current-file-mutation-queue-hook (lambda (event _path)
                                                     (set! events (cons event events)))])
    (with-file-mutation-queue path-str (lambda () 'ok)))
  (check-false (member 'lock-wait events) "uncontended acquisition must not report lock-wait")
  (delete-file tmp))

(test-case "holder exception releases the lock; waiter proceeds"
  (define tmp (make-temp-file))
  (define path-str (path->string tmp))
  (define holder-entered (make-semaphore 0))
  (define waiter-entered (make-semaphore 0))
  (define captured 'nothing-raised)
  (define holder
    (thread (lambda ()
              (with-handlers ([(lambda (v) (eq? v 'holder-boom)) (lambda (v) (set! captured v))])
                (with-file-mutation-queue path-str
                                          (lambda ()
                                            (semaphore-post holder-entered)
                                            (raise 'holder-boom)))))))
  (semaphore-wait holder-entered)
  (thread-wait holder)
  (check-eq? captured 'holder-boom "exception must propagate out of the critical section")
  (define waiter
    (thread (lambda ()
              (with-file-mutation-queue path-str (lambda () (semaphore-post waiter-entered))))))
  (check-not-false (sync/timeout 1 waiter-entered) "exception in holder must release the path lock")
  (thread-wait waiter)
  (check-equal? (mutation-queue-stats) 0 "registry must clean up after failure path")
  (delete-file tmp))

(test-case "cancelled waiter unregisters without disturbing the holder"
  (define tmp (make-temp-file))
  (define path-str (path->string tmp))
  (define holder-entered (make-semaphore 0))
  (define release-holder (make-semaphore 0))
  (define waiter-waiting (make-semaphore 0))
  (define holder
    (thread (lambda ()
              (with-file-mutation-queue path-str
                                        (lambda ()
                                          (semaphore-post holder-entered)
                                          (semaphore-wait release-holder))))))
  (semaphore-wait holder-entered)
  (define waiter
    (thread
     (lambda ()
       (with-handlers ([exn:break? void])
         (parameterize ([current-file-mutation-queue-owner "cancelled-waiter"]
                        [current-file-mutation-queue-hook (lambda (event _path)
                                                            (when (eq? event 'lock-wait)
                                                              (semaphore-post waiter-waiting)))])
           (with-file-mutation-queue path-str void))))))
  (semaphore-wait waiter-waiting)
  (break-thread waiter)
  (thread-wait waiter)
  (define d (find-diag (file-mutation-queue-diagnostics) path-str))
  (check-not-false d)
  (when d
    (check-equal? (file-lock-diagnostic-waiter-owners d)
                  '()
                  "cancelled waiter must disappear from diagnostics"))
  (check-equal? (mutation-queue-stats) 1 "the live holder remains registered")
  (semaphore-post release-holder)
  (thread-wait holder)
  (check-equal? (mutation-queue-stats) 0 "cancelled waiter must not leak the registry entry")
  (delete-file tmp))

(test-case "registration-hook failure unregisters the operation"
  (define tmp (make-temp-file))
  (define path-str (path->string tmp))
  (check-exn exn:fail?
             (lambda ()
               (parameterize ([current-file-mutation-queue-hook (lambda (_event _path)
                                                                  (error 'hook "boom"))])
                 (with-file-mutation-queue path-str void))))
  (check-equal? (mutation-queue-stats) 0 "hook failure must not leak the registry entry")
  (delete-file tmp))
