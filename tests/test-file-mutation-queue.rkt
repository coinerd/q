#lang racket

;; @speed fast
;; @suite default

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
