#lang racket/base

;; extensions/gsd/campaign-lease.rkt — the ONE process-safe campaign lease.
;;
;; W3 extraction: the OS advisory lock, its stable lock file, the recorded
;; owner/pid diagnostics, and the busy-stop diagnostics moved OUT of
;; go-orchestrator.rkt so the campaign interpreter stays under its size pin.
;; Ownership is unchanged:
;;
;;   * The lock lives at `.planning/campaigns/<plan-id>.lock` and is taken with
;;     port-try-file-lock? ('exclusive). A crash releases the OS lock even
;;     though the stable file survives, so a stale file alone never blocks and
;;     no force-release command exists.
;;   * run-campaign! still owns the dynamic-wind that releases the lease on
;;     EVERY exit path (normal return, cancellation, exception). This module
;;     only acquires, releases, and describes.
;;   * Persistent file content is diagnostic only: it names the holder and the
;;     acquisition time, and it NEVER authorizes release.
;;
;; Pure routing never lives here: acquire-lease is an effect performed once by
;; the interpreter before the campaign loop.

(require racket/file
         racket/format
         racket/os
         racket/string
         "campaign-result.rkt"
         (only-in "planning-paths.rkt" campaign-lock-file))

(provide campaign-lease
         campaign-lease?
         lease-path
         acquire-lease
         release-lease!
         lease-busy-diagnostics)

;; ============================================================
;; Lease (D5: process-safe OS advisory lock)
;; ============================================================

(struct campaign-lease (path port owner-pid owner-session) #:mutable)

(define (lease-path base-dir plan-id)
  ;; W4: the lock path lives in planning-paths.rkt, shared with the read-only
  ;; plan inspection, so the doctor and the lease can never name two files.
  (campaign-lock-file base-dir plan-id))

(define (acquire-lease base-dir plan-id #:session-id [session-id "unknown"])
  (define p (lease-path base-dir plan-id))
  (define-values (dir _ __) (split-path p))
  (make-directory* dir)
  (with-handlers ([exn:fail:filesystem? (lambda (_) #f)])
    ;; The lock file may survive a crash, but the OS advisory lock cannot.
    ;; Reopening the stable path therefore recovers safely after process exit.
    (define port (open-output-file p #:exists 'can-update))
    (if (port-try-file-lock? port 'exclusive)
        (begin
          ;; S2a (#9358): the file may hold a LONGER lease from a previous
          ;; owner (e.g. "01M0645J64E772Q0ZFNVGGEKK0"). open-output-file
          ;; 'can-update does NOT truncate, so file-position 0 + write left
          ;; a stale tail after a shorter write — corrupting the lease
          ;; (observed: `…VGGEKK0") (pid …)`). Truncate to zero before write.
          ;; D4 (#9351): record the owning session id AND pid so a stale
          ;; lock file names its holder (incident 81f9be4b: "unknown").
          ;; S2a (#9358): never write an empty owner — a re-dispatch path
          ;; passed "" (observed in attempt-5), defeating D4 diagnostics.
          (let ([owner
                 (if (and (string? session-id) (not (string=? session-id ""))) session-id "unknown")])
            (file-truncate port 0)
            (file-position port 0)
            (write (hasheq 'owner owner 'pid (getpid) 'acquired (current-seconds)) port)
            (flush-output port)
            (campaign-lease p port (current-seconds) owner)))
        (begin
          (close-output-port port)
          #f))))

(define (release-lease! lease)
  (when (and lease (campaign-lease? lease))
    (with-handlers ([exn:fail? void])
      (port-file-unlock (campaign-lease-port lease))
      (close-output-port (campaign-lease-port lease)))))

;; ============================================================
;; Busy-stop diagnostics
;; ============================================================

;; W3: busy diagnostics name the lock path, the recorded holder, and the
;; live/stale assessment. A failed acquisition is proof that the OS advisory
;; lock is HELD by a live process — a stale lock FILE alone cannot block, so
;; the recorded owner is diagnostic only and never authorizes a force release.
(define (lease-busy-diagnostics base-dir plan-id)
  (define lock-path (lease-path base-dir plan-id))
  (define recorded
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (and (file-exists? lock-path)
           (call-with-input-file
            lock-path
            (lambda (in)
              (define datum (read in))
              (and (hash? datum) datum))))))
  (define owner (and recorded (hash-ref recorded 'owner #f)))
  (define pid (and recorded (hash-ref recorded 'pid #f)))
  (define acquired (and recorded (hash-ref recorded 'acquired #f)))
  (define lock-text (path->string lock-path))
  (campaign-diagnostics
   'lease-busy
   #:summary "campaign lease held by another live process"
   #:detail
   (format (string-append
            "lock: ~a; recorded holder: ~a (pid ~a, acquired ~a); "
            "the OS advisory lock is held by a live process, so this is not a stale lock file")
           lock-text
           (or owner "unrecorded")
           (or pid "unrecorded")
           (or acquired "unrecorded"))
   #:next-action
   "inspect the lock holder; wait for release or stop that process, then re-run /go"
   #:artifacts (list lock-text)
   #:extra (hasheq 'lock-path lock-text
                   'recorded-owner owner
                   'recorded-pid pid
                   'recorded-acquired acquired
                   'lease-state 'held-live)))
