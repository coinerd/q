#lang racket/base

;; extensions/gsd/projection-effects.rkt — ATOMIC projection effect shell
;;
;; v0.99.89 W2 "Plan/State Projection Kernel": the filesystem boundary for
;; GSD projections. All projection content is computed PURELY by
;; projection-kernel.rkt; this shell is the only module that turns a neutral
;; projection set into on-disk changes, and it does so atomically:
;;
;;   1. compute every new file content first (pure, in memory);
;;   2. write every file to a same-directory temp file;
;;   3. rename all temps into place (per-file atomic replace).
;;
;; A crash at any point leaves each file either fully-old or fully-new —
;; never torn — and the durable campaign record stays the source of truth so
;; reconcile-projections-from-waves! can re-derive the projection set after a
;; crash between the durable commit and the projection apply (golden-trace
;; oracle finding #2). The reconcile entry point is what makes a resumed
;; campaign repair stale projections instead of silently carrying them.

(require racket/file
         racket/path
         racket/format
         racket/string
         racket/port
         "projection-kernel.rkt")

;; ============================================================
;; Atomic single-file write (temp + rename)
;; ============================================================

;; Write content to path via a same-directory temp file + rename, so readers
;; never observe a partially written file. Mirrors the durable outbox write
;; pattern from wave-completion.rkt.
(define (atomic-write-file! path content)
  (define dir (path-only path))
  (unless (directory-exists? dir)
    (make-directory* dir))
  (define tmp (string->path (string-append (path->string path) ".tmp~a")))
  (call-with-output-file tmp (lambda (out) (display content out)) #:exists 'truncate)
  (rename-file-or-directory tmp path #t)
  path)

;; ============================================================
;; Multi-file atomic apply (write all temps, then rename all)
;; ============================================================

;; files: list of (cons path content). Phase 1 writes every temp; phase 2
;; renames every temp into place. A crash during phase 1 leaves no
;; half-applied files; a crash during phase 2 leaves some files new and some
;; old — never torn — which reconcile-projections-from-waves! repairs.
(define (apply-atomic-files! files)
  (define temps
    (for/list ([f files])
      (define p (car f))
      (define content (cdr f))
      (define dir (path-only p))
      (unless (directory-exists? dir)
        (make-directory* dir))
      (define tmp (string->path (string-append (path->string p) ".tmp~a")))
      (call-with-output-file tmp (lambda (out) (display content out)) #:exists 'truncate)
      (cons tmp p)))
  (for ([tp temps])
    (rename-file-or-directory (car tp) (cdr tp) #t))
  (map cdr temps))

;; ============================================================
;; Path resolution
;; ============================================================

;; base-dir: campaign root (contains .planning/).
;; slug-of: (idx → slug-string) resolver for wave-doc entries.
;; Returns the concrete path for a projection entry, or #f when the entry
;; cannot be resolved (e.g. wave-doc without a slug).
(define (resolve-projection-path base-dir kind entry slug-of)
  (define planning (build-path base-dir ".planning"))
  (case kind
    [(plan-index) (build-path planning "PLAN.md")]
    [(state-table) (build-path planning "STATE.md")]
    [(wave-doc)
     (define idx (projection-entry-wave-idx entry))
     (define slug (and idx (slug-of idx)))
     (and slug (build-path planning "waves" (format "W~a-~a.md" idx slug)))]
    [else #f]))

;; ============================================================
;; Projection set application
;; ============================================================

;; Apply a neutral projection set atomically. Returns the list of paths that
;; were actually written (changed content only — idempotent projections are
;; skipped). slug-of resolves wave-doc indices to slugs.
(define (apply-projection-set! base-dir set slug-of)
  (define files
    (for/list ([entry set]
               #:when (projection-set? (list entry)))
      (define kind (projection-entry-kind entry))
      (define path (resolve-projection-path base-dir kind entry slug-of))
      (define new-content (projection-entry-content entry))
      (cond
        [(not path) #f]
        [(and (file-exists? path) (string=? (call-with-input-file path port->string) new-content))
         #f] ; idempotent — already matches
        [else (cons path new-content)])))
  (define to-write
    (for/list ([f files]
               #:when f)
      f))
  (if (null? to-write)
      '()
      (apply-atomic-files! to-write)))

;; ============================================================
;; Wave status projection (completion / failure transitions)
;; ============================================================

;; Read the current PLAN.md / wave-doc / STATE.md texts, compute the complete
;; projection set for one wave status change purely, and apply it atomically.
;; Returns the list of written paths. status is a display string ("DONE",
;; "FAILED", "DEFERRED", ...).
(define (apply-wave-status-projections! base-dir wave-idx status slug-of)
  (define planning (build-path base-dir ".planning"))
  (define plan-path (build-path planning "PLAN.md"))
  (define state-path (build-path planning "STATE.md"))
  (define doc-path
    (and (slug-of wave-idx)
         (build-path planning "waves" (format "W~a-~a.md" wave-idx (slug-of wave-idx)))))
  (cond
    [(not (file-exists? plan-path)) '()]
    [else
     (define plan-text (call-with-input-file plan-path port->string))
     (define doc-text
       (and doc-path (file-exists? doc-path) (call-with-input-file doc-path port->string)))
     (define state-text
       (and (file-exists? state-path) (call-with-input-file state-path port->string)))
     (define set (project-wave-status-set plan-text doc-text state-text wave-idx status))
     (apply-projection-set! base-dir set slug-of)]))

;; ============================================================
;; Crash-repair reconciliation
;; ============================================================

;; Recompute the projection set from the DURABLE wave statuses and apply it.
;; waves: list of (idx . status-symbol) from the durable campaign record.
;; slug-map: hash idx → slug (built by the caller from the plan index).
;; Returns the list of repaired paths (empty when the projections already
;; match the durable record — the normal case).
(define (reconcile-projections-from-waves! base-dir waves slug-map)
  (define planning (build-path base-dir ".planning"))
  (define plan-path (build-path planning "PLAN.md"))
  (cond
    [(not (file-exists? plan-path)) '()]
    [else
     (define plan-text (call-with-input-file plan-path port->string))
     (define state-path (build-path planning "STATE.md"))
     (define state-text
       (and (file-exists? state-path) (call-with-input-file state-path port->string)))
     (define doc-map
       (for/hash ([w waves])
         (define idx (car w))
         (define slug (hash-ref slug-map idx #f))
         (define p (and slug (build-path planning "waves" (format "W~a-~a.md" idx slug))))
         (values idx (and p (file-exists? p) (call-with-input-file p port->string)))))
     (define set (project-reconciliation-set waves plan-text doc-map state-text))
     (apply-projection-set! base-dir set (lambda (idx) (hash-ref slug-map idx #f)))]))

;; ============================================================
;; Provide
;; ============================================================

(provide atomic-write-file!
         apply-atomic-files!
         resolve-projection-path
         apply-projection-set!
         apply-wave-status-projections!
         reconcile-projections-from-waves!)
