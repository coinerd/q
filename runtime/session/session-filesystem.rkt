#lang racket/base

;; runtime/session/session-filesystem.rkt
;; Generic atomic no-follow session filesystem boundary.
;;
;; This is the frontend-neutral interface to the atomic containment boundary
;; (finding F-02/A-03). It selects a backend by platform:
;;   - POSIX (Linux/macOS): descriptor-relative *at syscalls with O_NOFOLLOW.
;;   - Win32/NT:             handle-relative NtCreateFile, fail-closed off-Windows.
;;
;; Every operation is relative to an opaque, session-owned directory capability.
;; There is no pathname fallback anywhere in the stack: a symlink swapped between
;; capability acquisition and use is never followed. The lexical session-path
;; module (kept for validation/diagnostics only) is deliberately NOT used here.
;;
;; W2 scope: foundation only. No production session-store cutover (that is W3).

(require racket/match
         (only-in "session-filesystem-posix.rkt")
         (only-in "session-filesystem-win32.rkt")
         "session-filesystem-stat.rkt")

(require (prefix-in posix: "session-filesystem-posix.rkt"))
(require (prefix-in win32: "session-filesystem-win32.rkt"))

;; Backend descriptor
(provide current-filesystem-backend
         filesystem-backend-supported?
         ;; Capability predicates (opaque)
         session-dir-cap?
         ;; Root / session directory operations
         open-session-root
         close-session-dir-cap
         open-session-dir
         create-session-dir
         session-dir-exists?
         ;; Artifact operations — all relative to a held session-dir-cap, all no-follow
         read-artifact
         append-artifact
         atomic-replace-artifact
         unlink-artifact
         list-artifacts
         artifact-stat
         artifact-regular?
         make-regular-artifact
         ;; Fork
         fork-session-dirs
         ;; Stat info
         (struct-out stat-info)
         ;; Re-export stat predicates
         stat-regular?
         stat-dir?
         stat-link?)

;; ---------------------------------------------------------------------------
;; Stat info is imported from session-filesystem-stat.rkt and shared with both
;; backends, so artifact-stat returns a value usable by these predicates.
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; Backend selection
;; ---------------------------------------------------------------------------

;; The active backend parameter. Defaults to the platform-native backend.
;; Both backends are required at compile time so that a wrong-backend call fails
;; closed at the boundary (denies) rather than silently degrading.
(define current-filesystem-backend
  (make-parameter (if (eq? (system-type 'os) 'windows) 'win32 'posix)))

(define (active-posix?)
  (eq? (current-filesystem-backend) 'posix))
(define (active-win32?)
  (eq? (current-filesystem-backend) 'win32))

(define (filesystem-backend-supported?)
  (cond
    [(active-posix?) (posix:posix-backend-supported?)]
    [(active-win32?) (win32:win32-backend-supported?)]
    [else #f]))

;; A session-dir-cap is whatever the active backend returns. We accept both
;; backend capability types transparently so consumers stay backend-neutral.
(define (session-dir-cap? v)
  (or (posix:posix-dir-cap? v) (win32:win32-dir-cap? v)))

;; ---------------------------------------------------------------------------
;; Capability operations — dispatch to the active backend
;; ---------------------------------------------------------------------------

(define (open-session-root root-path)
  (cond
    [(active-posix?) (posix:posix-open-root root-path)]
    [(active-win32?) (win32:win32-open-root root-path)]
    [else (error 'open-session-root "unsupported backend")]))

(define (close-session-dir-cap cap)
  (cond
    [(active-posix?) (posix:posix-close-dir-cap cap)]
    [(active-win32?) (win32:win32-close-dir-cap cap)]
    [else (void)]))

(define (open-session-dir root-cap session-id)
  (cond
    [(active-posix?) (posix:posix-open-session-dir root-cap session-id)]
    [(active-win32?) (win32:win32-open-session-dir root-cap session-id)]
    [else (error 'open-session-dir "unsupported backend")]))

(define (create-session-dir root-cap session-id)
  (cond
    [(active-posix?) (posix:posix-create-session-dir root-cap session-id)]
    [(active-win32?) (win32:win32-create-session-dir root-cap session-id)]
    [else (error 'create-session-dir "unsupported backend")]))

(define (session-dir-exists? root-cap session-id)
  (cond
    [(active-posix?) (posix:posix-session-dir-exists? root-cap session-id)]
    [(active-win32?) (win32:win32-session-dir-exists? root-cap session-id)]
    [else #f]))

(define (read-artifact dir-cap name)
  (cond
    [(active-posix?) (posix:posix-read-artifact dir-cap name)]
    [(active-win32?) (win32:win32-read-artifact dir-cap name)]
    [else (error 'read-artifact "unsupported backend")]))

(define (append-artifact dir-cap name data)
  (cond
    [(active-posix?) (posix:posix-append-artifact dir-cap name data)]
    [(active-win32?) (win32:win32-append-artifact dir-cap name data)]
    [else (error 'append-artifact "unsupported backend")]))

(define (atomic-replace-artifact dir-cap name data)
  (cond
    [(active-posix?) (posix:posix-atomic-replace-artifact dir-cap name data)]
    [(active-win32?) (win32:win32-atomic-replace-artifact dir-cap name data)]
    [else (error 'atomic-replace-artifact "unsupported backend")]))

(define (unlink-artifact dir-cap name)
  (cond
    [(active-posix?) (posix:posix-unlink-artifact dir-cap name)]
    [(active-win32?) (win32:win32-unlink-artifact dir-cap name)]
    [else (error 'unlink-artifact "unsupported backend")]))

(define (list-artifacts dir-cap)
  (cond
    [(active-posix?) (posix:posix-list-artifacts dir-cap)]
    [(active-win32?) (win32:win32-list-artifacts dir-cap)]
    [else (error 'list-artifacts "unsupported backend")]))

(define (artifact-stat dir-cap name)
  (cond
    [(active-posix?) (posix:posix-artifact-stat dir-cap name)]
    [(active-win32?) (win32:win32-artifact-stat dir-cap name)]
    [else #f]))

(define (artifact-regular? dir-cap name)
  (stat-regular? (artifact-stat dir-cap name)))

(define (make-regular-artifact dir-cap name)
  (cond
    [(active-posix?) (posix:posix-make-regular-artifact dir-cap name)]
    [(active-win32?) (win32:win32-make-regular-artifact dir-cap name)]
    [else (error 'make-regular-artifact "unsupported backend")]))

(define (fork-session-dirs src-root-cap src-id dst-root-cap dst-id)
  (cond
    [(active-posix?) (posix:posix-fork-session-dirs src-root-cap src-id dst-root-cap dst-id)]
    [(active-win32?) (win32:win32-fork-session-dirs src-root-cap src-id dst-root-cap dst-id)]
    [else (error 'fork-session-dirs "unsupported backend")]))
