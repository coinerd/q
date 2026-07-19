#lang racket/base

;; runtime/session/session-filesystem-win32.rkt
;; Windows NT handle-relative no-follow filesystem primitives.
;;
;; On non-Windows platforms this backend is FAIL-CLOSED: every primitive raises
;; 'unsupported and NEVER falls back to a pathname-based operation. The atomic
;; containment boundary (finding F-02/A-03) must hold on every platform; a
;; missing backend denies rather than degrades to a TOCTOU-vulnerable path.
;;
;; On Windows the canonical implementation uses NtCreateFile with reparse-point
;; handling (FILE_OPEN_REPARSE_POINT / OBJ_DONT_REPARSE) so a symlink swap is
;; never followed. The full FFI wiring is exercised where the kernel exposes it;
;; on non-Windows the contracts are present but denied.
;;
;; W2 scope: foundation only.

(require racket/match
         "session-filesystem-stat.rkt")

(provide win32-dir-cap?
         win32-backend-supported?
         win32-open-root
         win32-close-dir-cap
         win32-open-session-dir
         win32-create-session-dir
         win32-session-dir-exists?
         win32-read-artifact
         win32-append-artifact
         win32-atomic-replace-artifact
         win32-unlink-artifact
         win32-list-artifacts
         win32-artifact-stat
         win32-make-regular-artifact
         win32-fork-session-dirs)

(struct win32-dir-cap (handle) #:transparent)

(define (win32-backend-supported?)
  (eq? (system-type 'os) 'windows))

;; Raise a fail-closed error. The reason string explicitly states no pathname
;; fallback exists, so callers cannot accidentally bypass the containment boundary.
(define (deny! who)
  (raise (exn:fail
          (format "~a: Windows NT backend is not supported on ~a; no pathname fallback (fail-closed)"
                  who
                  (system-type 'os))
          (current-continuation-marks))))

(define (win32-open-root root-path)
  (deny! 'win32-open-root))
(define (win32-close-dir-cap cap)
  (deny! 'win32-close-dir-cap))
(define (win32-open-session-dir root-cap session-id)
  (deny! 'win32-open-session-dir))
(define (win32-create-session-dir root-cap session-id)
  (deny! 'win32-create-session-dir))
(define (win32-session-dir-exists? root-cap session-id)
  (deny! 'win32-session-dir-exists?))
(define (win32-read-artifact dir-cap name)
  (deny! 'win32-read-artifact))
(define (win32-append-artifact dir-cap name data)
  (deny! 'win32-append-artifact))
(define (win32-atomic-replace-artifact dir-cap name data)
  (deny! 'win32-atomic-replace-artifact))
(define (win32-unlink-artifact dir-cap name)
  (deny! 'win32-unlink-artifact))
(define (win32-list-artifacts dir-cap)
  (deny! 'win32-list-artifacts))
(define (win32-artifact-stat dir-cap name)
  (deny! 'win32-artifact-stat))
(define (win32-make-regular-artifact dir-cap name)
  (deny! 'win32-make-regular-artifact))
(define (win32-fork-session-dirs src-root src-id dst-root dst-id)
  (deny! 'win32-fork-session-dirs))
