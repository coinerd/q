#lang racket/base

;; @speed slow
;; @suite security
;;
;; tests/test-session-atomic-containment.rkt
;; W2 acceptance for the atomic no-follow session filesystem boundary.
;;
;; Closes finding F-02/A-03 (session check-to-use race). Every test holds a
;; directory capability and then performs an external "attacker" symlink swap;
;; held-capability operations must hit the original inode and never follow the
;; swap, or reject (no-follow) rather than read/write outside the boundary.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/system
         racket/port
         (only-in "helpers/session-fs-harness.rkt"
                  make-fs-root
                  fs-root-path
                  with-fs-root
                  make-outside-sentinel
                  outside-sentinel-value
                  swap-session-dir-to-symlink!
                  swap-artifact-to-symlink!
                  make-named-pipe!
                  supported-on-this-platform?)
         "../runtime/session/session-filesystem.rkt"
         "../runtime/session/session-repository.rkt"
         (prefix-in posix: "../runtime/session/session-filesystem-posix.rkt")
         (prefix-in win32: "../runtime/session/session-filesystem-win32.rkt"))

;; A held-root helper: create a root, open its capability, run a thunk with the
;; root-cap, and always close it. The held dir-cap passed to the thunk survives
;; external session-entry swaps because it points at the original inode.
(define (with-held-root thunk)
  (define root (fs-root-path (make-fs-root)))
  (define root-cap (open-session-root root))
  (dynamic-wind void (lambda () (thunk root root-cap)) (lambda () (close-session-dir-cap root-cap))))

(define suite
  (test-suite "session-atomic-containment"
    #:before void
    #:after void

    ;; -----------------------------------------------------------------------
    ;; Test 1 — descriptor-held root/session cannot be redirected by a later
    ;; pathname replacement. The held dir-cap keeps pointing at the original dir.
    ;; -----------------------------------------------------------------------
    (test-case "1-held-cap-still-reads-original-after-session-entry-swap"
      (with-fs-root (lambda ()
                      (with-held-root
                       (lambda (root root-cap)
                         (define dir-cap (create-session-dir root-cap "s1"))
                         (append-artifact dir-cap "session.jsonl" #"ORIGINAL")
                         ;; External attacker swaps the "s1" entry to a symlink pointing outside.
                         (define-values (sent-dir _sent-file) (make-outside-sentinel "SWAPPED"))
                         (swap-session-dir-to-symlink! root "s1" sent-dir)
                         ;; Reading via the HELD capability returns ORIGINAL (original inode).
                         (check-equal? (read-artifact dir-cap "session.jsonl") #"ORIGINAL")
                         (close-session-dir-cap dir-cap))))))

    ;; -----------------------------------------------------------------------
    ;; Test 2 — artifact symlink swap during read cannot read outside the
    ;; sentinel. openat with O_NOFOLLOW rejects the symlink, never follows it.
    ;; -----------------------------------------------------------------------
    (test-case "2-artifact-symlink-swap-rejected-not-followed"
      (with-fs-root (lambda ()
                      (with-held-root
                       (lambda (root root-cap)
                         (define dir-cap (create-session-dir root-cap "s1"))
                         (append-artifact dir-cap "session.jsonl" #"GOOD")
                         (define-values (sent-dir sent-file) (make-outside-sentinel "OUTSIDE-SECRET"))
                         (swap-artifact-to-symlink! root "s1" "session.jsonl" sent-file)
                         (check-exn exn:fail? (lambda () (read-artifact dir-cap "session.jsonl")))
                         ;; The sentinel was never read.
                         (check-equal? (outside-sentinel-value sent-dir) "OUTSIDE-SECRET")
                         (close-session-dir-cap dir-cap))))))

    ;; -----------------------------------------------------------------------
    ;; Test 3 — pending marker and session JSONL remain in the same held dir.
    ;; -----------------------------------------------------------------------
    (test-case "3-both-artifacts-readable-via-held-cap-after-swap"
      (with-fs-root (lambda ()
                      (with-held-root (lambda (root root-cap)
                                        (define dir-cap (create-session-dir root-cap "s1"))
                                        (append-artifact dir-cap "session.jsonl" #"LOG")
                                        (append-artifact dir-cap "session.jsonl.pending" #"PENDING")
                                        (define-values (sent-dir _) (make-outside-sentinel))
                                        (swap-session-dir-to-symlink! root "s1" sent-dir)
                                        (check-equal? (read-artifact dir-cap "session.jsonl") #"LOG")
                                        (check-equal? (read-artifact dir-cap "session.jsonl.pending")
                                                      #"PENDING")
                                        (close-session-dir-cap dir-cap))))))

    ;; -----------------------------------------------------------------------
    ;; Test 4 — temp create plus rename is descriptor-relative and no-follow.
    ;; atomic-replace onto a swapped symlink replaces the symlink entry inside the
    ;; held dir; the outside target is never written.
    ;; -----------------------------------------------------------------------
    (test-case "4-atomic-replace-replaces-symlink-entry-not-target"
      (with-fs-root (lambda ()
                      (with-held-root
                       (lambda (root root-cap)
                         (define dir-cap (create-session-dir root-cap "s1"))
                         (define-values (sent-dir sent-file) (make-outside-sentinel "SENTINEL-KEEP"))
                         (swap-artifact-to-symlink! root "s1" "session.jsonl" sent-file)
                         (atomic-replace-artifact dir-cap "session.jsonl" #"NEWDATA")
                         (check-equal? (read-artifact dir-cap "session.jsonl") #"NEWDATA")
                         ;; The outside sentinel file is untouched.
                         (check-equal? (outside-sentinel-value sent-dir) "SENTINEL-KEEP")
                         (close-session-dir-cap dir-cap))))))

    ;; -----------------------------------------------------------------------
    ;; Test 5 — delete unlinks a contained symlink, never its target.
    ;; -----------------------------------------------------------------------
    (test-case "5-unlink-removes-symlink-target-survives"
      (with-fs-root (lambda ()
                      (with-held-root
                       (lambda (root root-cap)
                         (define dir-cap (create-session-dir root-cap "s1"))
                         (define-values (sent-dir sent-file) (make-outside-sentinel "TGT"))
                         (swap-artifact-to-symlink! root "s1" "session.jsonl" sent-file)
                         (unlink-artifact dir-cap "session.jsonl")
                         (check-false (member "session.jsonl" (list-artifacts dir-cap)))
                         (check-equal? (outside-sentinel-value sent-dir) "TGT")
                         (close-session-dir-cap dir-cap))))))

    ;; -----------------------------------------------------------------------
    ;; Test 6 — fork source/destination independently held and no-follow.
    ;; -----------------------------------------------------------------------
    (test-case "6-fork-copies-original-then-source-swap-inert"
      (with-fs-root (lambda ()
                      (with-held-root
                       (lambda (root root-cap)
                         (define src-cap (create-session-dir root-cap "s1"))
                         (append-artifact src-cap "session.jsonl" #"FORK-ME")
                         (close-session-dir-cap src-cap)
                         (define dst-cap (fork-session-dirs root-cap "s1" root-cap "s2"))
                         (define-values (sent-dir _) (make-outside-sentinel))
                         (swap-session-dir-to-symlink! root "s1" sent-dir)
                         (check-equal? (read-artifact dst-cap "session.jsonl") #"FORK-ME")
                         (close-session-dir-cap dst-cap))))))

    ;; -----------------------------------------------------------------------
    ;; Test 7 — non-regular artifact or unexpected hard-link count rejects.
    ;; -----------------------------------------------------------------------
    (test-case "7a-named-pipe-not-regular"
      (with-fs-root (lambda ()
                      (with-held-root (lambda (root root-cap)
                                        (define dir-cap (create-session-dir root-cap "s1"))
                                        (make-named-pipe! root "s1" "fifo")
                                        (check-false (artifact-regular? dir-cap "fifo"))
                                        (close-session-dir-cap dir-cap))))))

    (test-case "7b-extra-hardlink-detected"
      (with-fs-root (lambda ()
                      (with-held-root
                       (lambda (root root-cap)
                         (define dir-cap (create-session-dir root-cap "s1"))
                         (append-artifact dir-cap "session.jsonl" #"X")
                         (define ln-cmd (find-executable-path "ln"))
                         (unless (and ln-cmd
                                      (system* ln-cmd
                                               (path->string (build-path root "s1" "session.jsonl"))
                                               (path->string (build-path root "s1" "alias"))))
                           (error 'test "ln failed"))
                         (define stat (artifact-stat dir-cap "session.jsonl"))
                         (check-pred stat-info? stat)
                         (check-equal? (stat-info-nlink stat) 2)
                         ;; Policy: a session artifact must have exactly one link.
                         (check-false (= (stat-info-nlink stat) 1))
                         (close-session-dir-cap dir-cap))))))

    ;; -----------------------------------------------------------------------
    ;; Test 8 — backends fail closed on unsupported primitives; no pathname fallback.
    ;; -----------------------------------------------------------------------
    (test-case "8-win32-backend-denies-off-windows-no-fallback"
      (unless (eq? (system-type 'os) 'windows)
        (parameterize ([current-filesystem-backend 'win32])
          (check-exn exn:fail? (lambda () (win32:win32-open-root "/tmp")))
          (check-exn exn:fail? (lambda () (win32:win32-open-session-dir #f "x")))
          (check-exn exn:fail? (lambda () (win32:win32-read-artifact #f "x")))
          (check-exn exn:fail? (lambda () (win32:win32-append-artifact #f "x" #"y")))
          (check-exn exn:fail? (lambda () (win32:win32-fork-session-dirs #f "a" #f "b")))
          (check-false (win32:win32-backend-supported?)))))

    (test-case "8-posix-backend-denies-off-posix"
      (when (eq? (system-type 'os) 'windows)
        (check-exn exn:fail? (lambda () (posix:posix-open-root "C:/tmp")))
        (check-false (posix:posix-backend-supported?))))

    ;; -----------------------------------------------------------------------
    ;; Test 9 — descriptor exceptions close exactly once and leak no resources.
    ;; -----------------------------------------------------------------------
    (test-case "9a-double-close-is-safe"
      (with-fs-root (lambda ()
                      (with-held-root (lambda (root root-cap)
                                        (close-session-dir-cap root-cap)
                                        ;; Second close must not raise (fd already -1).
                                        (close-session-dir-cap root-cap))))))

    (test-case "9b-failed-open-does-not-leak-fd"
      (with-fs-root (lambda ()
                      (with-held-root (lambda (root root-cap)
                                        (define dir-cap (create-session-dir root-cap "s1"))
                                        (check-exn exn:fail?
                                                   (lambda () (read-artifact dir-cap "nope")))
                                        (append-artifact dir-cap "session.jsonl" #"OK")
                                        (check-equal? (read-artifact dir-cap "session.jsonl") #"OK")
                                        (close-session-dir-cap dir-cap))))))

    ;; -----------------------------------------------------------------------
    ;; Test 10 — platform constants and stat classification pass CI probes.
    ;; -----------------------------------------------------------------------
    (test-case "10-stat-classification-on-known-fixtures"
      (with-fs-root (lambda ()
                      (with-held-root (lambda (root root-cap)
                                        (define dir-cap (create-session-dir root-cap "s1"))
                                        (append-artifact dir-cap "session.jsonl" #"Z")
                                        (check-true (artifact-regular? dir-cap "session.jsonl"))
                                        (check-true (session-dir-exists? root-cap "s1"))
                                        (close-session-dir-cap dir-cap))))))

    (test-case "10-backend-supported-on-this-ci-platform"
      (check-true (filesystem-backend-supported?)))))

(when (supported-on-this-platform?)
  (exit (run-tests suite)))
