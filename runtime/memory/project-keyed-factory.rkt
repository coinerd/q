#lang racket/base

;; runtime/memory/project-keyed-factory.rkt
;; STABILITY: evolving
;;
;; W5B (#8942): Project-keyed isolated memory backend factory.
;;
;; Bridges the canonical project identity (W5A project-identity.rkt) to
;; physical memory storage. Replaces the UNSAFE pattern in service.rkt
;; where memory-root fell back to session-dir or (find-system-path
;; 'temp-dir) — which caused cross-project leakage and meant memory did
;; not survive across sessions.
;;
;; CONTRACT (per W5 issue):
;;   - Physical project memory lives in a project-keyed USER DATA location,
;;     not the repository or session directory.
;;   - Multi-session isolation belongs to a session/project service, not
;;     process-global parameters.
;;   - Fresh same-project session recalls promoted facts (same path).
;;   - Different project cannot retrieve them (different path).
;;
;; Layering: RUNTIME module. Imports project-identity (pure) and the
;; existing file-jsonl backend (effectful storage).

(require racket/contract
         racket/file
         racket/path
         "project-identity.rkt"
         "backends/file-jsonl.rkt")

(provide project-keyed-memory-root
         project-keyed-memory-root/aliased
         make-project-keyed-backend
         project-keyed-backend-path
         default-base-data-dir)

;; ============================================================
;; Default base data directory
;; ============================================================

;; The user-data location under which all project-keyed memory lives.
;; ~/.q/projects/<shard>/project-<key>/
;; This is OUTSIDE the repository and OUTSIDE any session directory.
(define (default-base-data-dir)
  (path->string (build-path (find-system-path 'home-dir) ".q")))

;; Coerce a path-or-string to a string. Accepts #f (returns "").
(define (->string-path p)
  (cond
    [(not p) ""]
    [(path? p) (path->string p)]
    [(string? p) p]
    [else (error '->string-path "expected path or string, given: ~e" p)]))

;; ============================================================
;; Path derivation
;; ============================================================

;; Derive the physical memory-root directory for a project identity,
;; under the given base data dir. Sharded by key prefix.
;;
;;   <base>/projects/<key[0:2]>/project-<key>/
(define (project-keyed-memory-root id #:base-data-dir [base (default-base-data-dir)])
  (unless (project-identity? id)
    (error 'project-keyed-memory-root "expected project-identity, given: ~e" id))
  (project-memory-subdir id (->string-path base)))

;; Like project-keyed-memory-root, but resolves worktree aliases: if `id`
;; aliases to `canonical` (per same-project?), use the canonical's path so
;; that a worktree and its main repo share the same physical memory.
;;
;; This is what makes "fresh same-project session recalls promoted facts"
;; hold across worktrees and relocations (when origin is stable).
(define (project-keyed-memory-root/aliased id
                                           canonical
                                           #:base-data-dir [base (default-base-data-dir)])
  (unless (project-identity? id)
    (error 'project-keyed-memory-root/aliased "expected project-identity for id"))
  (unless (project-identity? canonical)
    (error 'project-keyed-memory-root/aliased "expected project-identity for canonical"))
  (define resolved (if (same-project? id canonical) canonical id))
  (project-keyed-memory-root resolved #:base-data-dir (->string-path base)))

;; ============================================================
;; Backend factory
;; ============================================================

;; Build a file-jsonl memory backend whose physical storage is keyed by
;; the project identity. Returns #f if id is #f (fail-closed: no identity
;; means no project-keyed storage — caller must not fall back to cwd/temp).
(define (make-project-keyed-backend id #:base-data-dir [base (default-base-data-dir)])
  (cond
    [(not id) #f]
    [(not (project-identity? id)) #f]
    [else
     (define root (project-keyed-memory-root id #:base-data-dir base))
     (define root-path (string->path root))
     ;; Ensure parent directories exist (the backend itself lazily creates
     ;; the leaf, but the sharded parent may not exist yet).
     (make-directory* (path-only root-path))
     (make-file-jsonl-backend root-path)]))

;; Return the physical storage path a backend writes to, as a string.
;; Used for diagnostics and isolation tests.
(define (project-keyed-backend-path backend)
  (if backend
      (file-jsonl-backend-path backend)
      #f))
