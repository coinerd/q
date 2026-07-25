#lang racket/base

;; runtime/task-memory/workspace-revision.rkt
;; STABILITY: internal
;;
;; W3B (#8940): Workspace revision — file mutation generation tracking
;; for evidence invalidation.
;;
;; When a file is mutated (edit/write), prior evidence (test outputs,
;; git status) that referenced that file at an earlier generation becomes
;; stale. This module maintains per-path generation counters so evidence
;; can be marked invalid after mutations.
;;
;; Design:
;;   - Immutable: bump operations return a NEW workspace-revision
;;   - Pure data: no I/O, no side effects
;;   - Integrates with typed-tool-outcome via bump-from-outcome
;;
;; Staleness rule:
;;   evidence(path, observed-gen) is STALE iff
;;     current-gen(path) > observed-gen
;;   Future gens (observed-gen > current) are NOT stale (optimistic).

(require racket/contract
         racket/list
         racket/set
         "../../util/outcome/outcome-types.rkt")

(provide workspace-revision
         workspace-revision?
         make-workspace-revision
         workspace-revision-ref
         workspace-revision-bump
         workspace-revision-bump-all
         workspace-revision-count
         workspace-revision-paths
         workspace-revision-stale?
         workspace-revision-snapshot
         workspace-revision-from-snapshot
         workspace-revision-bump-from-outcome)

;; ============================================================
;; Struct
;; ============================================================

;; Internal representation: an immutable hash of path(string) → gen(exact-positive-integer).
;; The struct wraps the hash to provide a distinct type and validated API.
(struct workspace-revision (table) #:transparent)

;; ============================================================
;; Construction
;; ============================================================

;; Create an empty workspace-revision (no tracked paths).
(define (make-workspace-revision)
  (workspace-revision (hash)))

;; ============================================================
;; Lookup
;; ============================================================

;; Get the current generation for a path. Returns 0 if unknown.
(define (workspace-revision-ref ws path)
  (hash-ref (workspace-revision-table ws) path 0))

;; Count distinct tracked paths.
(define (workspace-revision-count ws)
  (hash-count (workspace-revision-table ws)))

;; List all tracked paths (unsorted).
(define (workspace-revision-paths ws)
  (hash-keys (workspace-revision-table ws)))

;; ============================================================
;; Mutation (immutable — returns new revision)
;; ============================================================

;; Bump the generation for a single path. Returns a new workspace-revision.
(define (workspace-revision-bump ws path)
  (define tbl (workspace-revision-table ws))
  (define next (add1 (hash-ref tbl path 0)))
  (workspace-revision (hash-set tbl path next)))

;; Bump generations for multiple paths (dedupes via set).
(define (workspace-revision-bump-all ws paths)
  (for/fold ([acc ws]) ([p (in-list (remove-dups paths))])
    (workspace-revision-bump acc p)))

;; Remove duplicate strings from a list (preserving order is not required).
(define (remove-dups lst)
  (reverse (for/fold ([seen '()]) ([x (in-list lst)])
             (if (member x seen)
                 seen
                 (cons x seen)))))

;; ============================================================
;; Staleness check
;; ============================================================

;; Check whether evidence observed at observed-gen is now stale.
;; Stale iff current-gen > observed-gen.
(define (workspace-revision-stale? ws path observed-gen)
  (define current (workspace-revision-ref ws path))
  ;; If path is unknown (gen 0), nothing has mutated it → not stale.
  (and (> current 0) (> current observed-gen)))

;; ============================================================
;; Snapshot / restore (for checkpoint integration)
;; ============================================================

;; Produce a plain hash snapshot of the revision (path → gen).
(define (workspace-revision-snapshot ws)
  (workspace-revision-table ws))

;; Reconstruct a workspace-revision from a snapshot hash.
(define (workspace-revision-from-snapshot snap)
  (if (hash? snap)
      (workspace-revision snap)
      (make-workspace-revision)))

;; ============================================================
;; Outcome integration
;; ============================================================

;; Bump the revision based on an artifact outcome's path payload.
;; Non-artifact outcomes are a no-op (return the same revision).
(define (workspace-revision-bump-from-outcome ws outcome)
  (cond
    [(not (eq? (typed-tool-outcome-kind outcome) 'artifact)) ws]
    [else
     (define payload (typed-tool-outcome-payload outcome))
     (define path (hash-ref payload 'path #f))
     (if (and path (string? path))
         (workspace-revision-bump ws path)
         ws)]))
