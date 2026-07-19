#lang racket/base

;; runtime/session/session-repository.rkt
;; Session-owned opaque capability repository.
;;
;; Owns the atomic no-follow filesystem boundary (session-filesystem.rkt) on
;; behalf of a session root. The repository is the only owner of directory
;; capabilities; consumers receive capabilities by reference but never re-resolve
;; pathnames. This closes the check-to-use race (finding F-02/A-03): a capability
;; is acquired once via a held descriptor and every subsequent operation is
;; relative to that descriptor.
;;
;; W2 scope: foundation only. No production session-store cutover (that is W3).

(require racket/match
         racket/set
         "session-filesystem.rkt")

;; Repository lifecycle
(provide make-session-repository ; root-path-string -> session-repository?
         session-repository?
         session-repository-root ; repository -> path-string
         close-session-repository! ; repository -> void (closes the held root cap)
         ;; Session directory access (capabilities owned by the repository)
         repository-open-session-dir ; repository session-id -> session-dir-cap (NOFOLLOW)
         repository-create-session-dir ; repository session-id -> session-dir-cap
         repository-session-dir-exists? ; repository session-id -> boolean
         ;; Convenience: read/append/atomic-replace/unlink/list/fork via the repository
         repository-read-artifact
         repository-append-artifact
         repository-atomic-replace-artifact
         repository-unlink-artifact
         repository-list-artifacts
         repository-fork-session
         ;; Backend descriptor (passthrough)
         repository-backend-supported?)

;; ---------------------------------------------------------------------------
;; Repository
;; ---------------------------------------------------------------------------

;; A session-repository owns the root directory capability and tracks which
;; session directory capabilities it has handed out (for fail-closed teardown).
(struct session-repository
        (root-path ; the canonical root path (diagnostics only — never re-opened)
         [root-cap #:mutable] ; held opaque capability for the root directory
         open-caps ; (set) tracked session dir capabilities (diagnostics)
         )
  #:transparent)

(define (make-session-repository root-path)
  (define cap (open-session-root root-path))
  (session-repository root-path cap (mutable-set)))

(define (session-repository-root repo)
  (session-repository-root-path repo))

(define (repository-backend-supported?)
  (filesystem-backend-supported?))

(define (close-session-repository! repo)
  (define cap (session-repository-root-cap repo))
  (when cap
    (close-session-dir-cap cap)
    (set-session-repository-root-cap! repo #f)))

;; Acquire a session directory capability. The capability is tracked by the
;; repository. The returned capability is opaque; the caller must not re-resolve
;; it to a pathname.
(define (repository-open-session-dir repo session-id)
  (define dir-cap (open-session-dir (session-repository-root-cap repo) session-id))
  (set-add! (session-repository-open-caps repo) dir-cap)
  dir-cap)

(define (repository-create-session-dir repo session-id)
  (define dir-cap (create-session-dir (session-repository-root-cap repo) session-id))
  (set-add! (session-repository-open-caps repo) dir-cap)
  dir-cap)

(define (repository-session-dir-exists? repo session-id)
  (session-dir-exists? (session-repository-root-cap repo) session-id))

(define (repository-read-artifact repo session-id name)
  (define dir-cap (repository-open-session-dir repo session-id))
  (dynamic-wind void
                (lambda () (read-artifact dir-cap name))
                (lambda () (close-session-dir-cap dir-cap))))

(define (repository-append-artifact repo session-id name data)
  (define dir-cap (repository-open-session-dir repo session-id))
  (dynamic-wind void
                (lambda () (append-artifact dir-cap name data))
                (lambda () (close-session-dir-cap dir-cap))))

(define (repository-atomic-replace-artifact repo session-id name data)
  (define dir-cap (repository-open-session-dir repo session-id))
  (dynamic-wind void
                (lambda () (atomic-replace-artifact dir-cap name data))
                (lambda () (close-session-dir-cap dir-cap))))

(define (repository-unlink-artifact repo session-id name)
  (define dir-cap (repository-open-session-dir repo session-id))
  (dynamic-wind void
                (lambda () (unlink-artifact dir-cap name))
                (lambda () (close-session-dir-cap dir-cap))))

(define (repository-list-artifacts repo session-id)
  (define dir-cap (repository-open-session-dir repo session-id))
  (dynamic-wind void
                (lambda () (list-artifacts dir-cap))
                (lambda () (close-session-dir-cap dir-cap))))

;; Fork a session into a destination session under the same repository root.
;; Source and destination are held independently; both no-follow.
(define (repository-fork-session repo src-session-id dst-session-id)
  (fork-session-dirs (session-repository-root-cap repo)
                     src-session-id
                     (session-repository-root-cap repo)
                     dst-session-id))
