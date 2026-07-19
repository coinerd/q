#lang racket/base

;; tests/helpers/session-fs-harness.rkt
;; Test helpers for the atomic no-follow session filesystem (W2).
;; Provides isolated temp roots and external "attacker" symlink swaps that
;; simulate the check-to-use race (finding F-02/A-03).

(require racket/file
         racket/system
         racket/port)

(provide make-fs-root
         fs-root-path
         with-fs-root
         make-outside-sentinel
         outside-sentinel-value
         swap-to-symlink!
         swap-session-dir-to-symlink!
         swap-artifact-to-symlink!
         make-named-pipe!
         supported-on-this-platform?
         current-fs-root
         (struct-out fs-root))

(struct fs-root (dir) #:transparent)

(define current-fs-root (make-parameter #f))

(define (make-fs-root)
  (define base (find-system-path 'temp-dir))
  (define p (make-temporary-file "q-fs-root-~a" 'directory))
  (fs-root (path->string (path->complete-path p))))

(define (fs-root-path r)
  (fs-root-dir r))

(define (with-fs-root thunk)
  (define r (make-fs-root))
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-fs-root r])
                    (thunk)))
                (lambda ()
                  (with-handlers ([exn:fail? (lambda (_) (void))])
                    (delete-directory/files (fs-root-path r))))))

;; A sentinel directory OUTSIDE the held root. If a no-follow violation occurs,
;; an operation would read/write into here.
(define (make-outside-sentinel [value "SENTINEL-DATA-OUTSIDE-ROOT"])
  (define base (find-system-path 'temp-dir))
  (define dir
    (path->string (path->complete-path (make-temporary-file "q-fs-sentinel-dir-~a" 'directory))))
  (define file (build-path dir "secret"))
  (call-with-output-file file (lambda (out) (write-string value out)))
  (values dir file))

(define (outside-sentinel-value sentinel-dir)
  (call-with-input-file (build-path sentinel-dir "secret") port->string))

;; External "attacker" operation: replace an entry inside the root with a symlink
;; pointing OUTSIDE the root. This is the TOCTOU swap.
;;
;; For a DIRECTORY entry, the original directory is RENED ASIDE (not deleted) so
;; that a held descriptor still points at the original inode with its files
;; intact. The attacker then installs a symlink in the vacated name. The
;; containment property under test is that operations via the HELD capability hit
;; the original (renamed) directory, never the symlink target.
(define (swap-to-symlink! root-path entry-name target-path)
  (define entry (build-path root-path entry-name))
  (cond
    [(directory-exists? entry)
     (define moved (build-path root-path (string-append entry-name ".swapped")))
     (rename-file-or-directory entry moved #t)
     (make-file-or-directory-link target-path entry)]
    [else
     (with-handlers ([exn:fail? (lambda (_) (void))])
       (when (or (file-exists? entry) (link-exists? entry))
         (delete-file entry)))
     (make-file-or-directory-link target-path entry)]))

;; Swap a session directory entry to a symlink pointing outside.
(define (swap-session-dir-to-symlink! root-path session-id target-dir)
  (swap-to-symlink! root-path session-id target-dir))

;; Swap an artifact (inside a session dir) to a symlink pointing outside.
(define (swap-artifact-to-symlink! root-path session-id artifact-name target-file)
  (define entry (build-path root-path session-id artifact-name))
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (when (or (file-exists? entry) (link-exists? entry))
      (delete-file entry)))
  (make-file-or-directory-link target-file entry))

;; Create a named pipe (FIFO) inside a session dir to test non-regular rejection.
(define (make-named-pipe! root-path session-id name)
  (define p (build-path root-path session-id name))
  (define cmd (find-executable-path "mkfifo"))
  (unless (and cmd (system* cmd p))
    (error 'make-named-pipe! "mkfifo failed for ~a" p)))

;; The atomic containment tests require POSIX mkfifo/symlink support.
(define (supported-on-this-platform?)
  (and (memq (system-type 'os) '(unix macos)) #t))
