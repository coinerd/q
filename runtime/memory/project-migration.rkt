#lang racket/base

;; runtime/memory/project-migration.rkt
;; STABILITY: evolving
;;
;; W5C (#8942): Migration from old memory namespaces to project-keyed storage.
;;
;; Before W5, memory was stored at a path derived from session-dir or
;; (find-system-path 'temp-dir) — shared across projects and sessions.
;; This migration reads items from an OLD backend and writes them into a
;; NEW project-keyed backend.
;;
;; CONTRACT:
;;   - Idempotent: re-running the same migration is safe (deduplicates by ID).
;;   - Resumable: partial migration followed by full migration works.
;;   - Quarantine: non-project-scope items are quarantined, not silently promoted.
;;   - Fail-closed: invalid backends return #f, never crash the caller.
;;
;; Layering: RUNTIME module. Uses existing migration.rkt for the export/import
;; plumbing and project-keyed-factory.rkt for the destination backend.

(require racket/list
         racket/match
         "types.rkt"
         "protocol.rkt"
         "migration.rkt"
         "project-keyed-factory.rkt")

(provide migrate-from-old-backend!
         migrate-project-memory!
         migration-report?
         migration-report->string)

;; ============================================================
;; Migration report
;; ============================================================

;; A migration-report is a hash with:
;;   exported:     number of items exported from source
;;   imported:     number of items newly stored in destination
;;   duplicates:   number of items skipped (already in dest)
;;   quarantined:  number of items excluded on scope/policy grounds
;;   errors:       list of error messages (may be empty)
(define (migration-report? v)
  (and (hash? v)
       (number? (hash-ref v 'exported #f))
       (number? (hash-ref v 'imported #f))
       (number? (hash-ref v 'duplicates #f))))

(define (migration-report->string report)
  (format "Migration: ~a exported, ~a imported, ~a duplicates, ~a quarantined"
          (hash-ref report 'exported 0)
          (hash-ref report 'imported 0)
          (hash-ref report 'duplicates 0)
          (hash-ref report 'quarantined 0)))

;; ============================================================
;; Migration: old backend → new project-keyed backend
;; ============================================================

;; Migrate all ITEMS from old-backend into new-backend.
;;
;; Rules:
;;   1. Only items with scope 'project are migrated (session-scoped items
;;      are quarantined — they belong to a specific session, not cross-session).
;;   2. Items already present in new-backend (by ID) are skipped (idempotent).
;;   3. Returns a migration-report hash.
;;   4. Returns #f if either backend is invalid.
;;
;; This function is SAFE to call multiple times: it skips duplicates.
(define (migrate-from-old-backend! old-backend new-backend)
  (cond
    [(not (and old-backend new-backend)) #f]
    [(not (memory-backend? old-backend))
     (log-warning "migration: old-backend is not a memory-backend")
     #f]
    [(not (memory-backend? new-backend))
     (log-warning "migration: new-backend is not a memory-backend")
     #f]
    [else
     ;; Step 1: Export all items from old backend
     (define export-result (export-memory-items old-backend #:include-expired? #t))
     (cond
       [(not (memory-result-ok? export-result))
        (log-warning "migration: export failed")
        (hasheq 'exported
                0
                'imported
                0
                'duplicates
                0
                'quarantined
                0
                'errors
                (format "export failed: ~a" (memory-result-error export-result)))]
       [else
        (define old-items (memory-result-value export-result))
        (define exported-count (length old-items))
        ;; Step 2: Classify items
        (define-values (to-import quarantined)
          (partition (lambda (item) (and (memory-item? item) (eq? (memory-item-scope item) 'project)))
                     old-items))
        (define quarantined-count (length quarantined))
        ;; Step 3: Pre-fetch destination IDs for dedup check
        (define dest-ids (list-items-ids new-backend))
        ;; Step 4: Import new items
        (define-values (imported duplicates)
          (partition (lambda (item) (not (member (memory-item-id item) dest-ids))) to-import))
        (define imported-count
          (cond
            [(null? imported) 0]
            [else
             (define import-result (import-memory-items! new-backend imported))
             (if (memory-result-ok? import-result)
                 (memory-result-value import-result) ; imported count
                 0)]))
        (hasheq 'exported
                exported-count
                'imported
                imported-count
                'duplicates
                (length duplicates)
                'quarantined
                quarantined-count
                'errors
                '())])]))

;; ============================================================
;; Full project memory migration (high-level entrypoint)
;; ============================================================

;; Convenience: given a project identity and an old backend, build the
;; project-keyed backend and migrate. Returns the migration report.
;;
;; The base-data-dir defaults to ~/.q (matching project-keyed-factory).
(define (migrate-project-memory! id old-backend #:base-data-dir [base (default-base-data-dir)])
  (define new-backend (make-project-keyed-backend id #:base-data-dir base))
  (cond
    [(not new-backend)
     (log-warning "migration: could not create project-keyed backend")
     (hasheq 'exported
             0
             'imported
             0
             'duplicates
             0
             'quarantined
             0
             'errors
             '("failed to create project-keyed backend"))]
    [else (migrate-from-old-backend! old-backend new-backend)]))

;; ============================================================
;; Internal helper: list item IDs from a backend
;; ============================================================

(define (list-items-ids backend)
  (define query
    (memory-query #f ; text
                  #f ; scope
                  #f ; project-root
                  #f ; session-id
                  #f ; types
                  #f ; tags
                  10000 ; limit
                  #t)) ; include-expired?
  (define result (gen:list-memory backend query))
  (if (memory-result-ok? result)
      (map memory-item-id (memory-result-value result))
      '()))
