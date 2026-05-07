#lang racket/base

;; extensions/gsd/archive.rkt — GSD Plan Archival
;;
;; v0.21.9 #2155: Archive completed plans.
;; Validates all waves complete, creates archive directory,
;; moves plan files, resets GSD state.

(require racket/file
         racket/path
         racket/port
         racket/set
         racket/string
         racket/format
         "plan-types.rkt"
         "wave-docs.rkt"
         "state-machine.rkt"
         "command-types.rkt"
         (only-in "shared.rkt" extract-plan-title))

(provide archive-completed-plan!
         all-waves-complete?
         archive-path-for-plan
         move-to-archive!
         reset-gsd-after-archive!
         cleanup-empty-subdirs!
         ensure-state-md!
         sync-executor-to-plan!)

;; ============================================================
;; Validation
;; ============================================================

;; Check if all waves in PLAN.md are complete (DONE or DEFERRED).
;; Case-insensitive to handle LLM-written "Done" vs canonical "DONE".
(define (all-waves-complete? base-dir)
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (cond
    [(not (file-exists? plan-path)) #f]
    [else
     (define text (call-with-input-file plan-path port->string))
     (define entries (parse-plan-index text))
     (cond
       [(null? entries) #f]
       [else
        (for/and ([e entries])
          (define status (string-upcase (wave-index-entry-status e)))
          (or (string=? status "DONE") (string=? status "DEFERRED")))])]))

;; ============================================================
;; Archive path
;; ============================================================

;; Generate archive directory path from plan title.
;; Format: .planning/archive/<slug>/
;; If collision, append epoch timestamp.
(define (archive-path-for-plan base-dir plan-title)
  (define slug (slugify plan-title))
  (define archive-root (build-path base-dir ".planning" "archive"))
  (define candidate (build-path archive-root slug))
  (cond
    [(not (directory-exists? candidate)) candidate]
    ;; Collision: append epoch timestamp
    [else (build-path archive-root (format "~a-~a" slug (current-seconds)))]))

;; ============================================================
;; File movement
;; ============================================================

;; Move planning files to archive directory.
;; Moves: PLAN.md, STATE.md, HANDOFF.json, wave docs in waves/
;; Returns the archive directory path.
(define (move-to-archive! base-dir archive-dir)
  (define planning-dir (build-path base-dir ".planning"))
  (unless (directory-exists? archive-dir)
    (make-directory* archive-dir))

  ;; Files to move from .planning/
  (define files-to-move
    (for/list ([f '("PLAN.md" "STATE.md" "HANDOFF.json" "VALIDATION.md" "SUMMARY.md")]
               #:when (file-exists? (build-path planning-dir f)))
      f))

  ;; Move top-level files
  (for ([f files-to-move])
    (define src (build-path planning-dir f))
    (define dst (build-path archive-dir f))
    (rename-file-or-directory src dst))

  ;; Move waves/ directory if it exists
  (define waves-dir (build-path planning-dir "waves"))
  (when (directory-exists? waves-dir)
    (define dst-waves (build-path archive-dir "waves"))
    (rename-file-or-directory waves-dir dst-waves))

  ;; Clean up empty subdirs after archive (#2160)
  (cleanup-empty-subdirs! planning-dir)

  archive-dir)

;; ============================================================
;; Reset
;; ============================================================

;; Reset GSD state machine after archival.
(define (reset-gsd-after-archive!)
  (reset-gsm!))

;; ============================================================
;; Main entry point
;; ============================================================

;; Archive a completed plan.
;; Steps: validate → create archive dir → move files → reset state.
;; Returns gsd-command-result (gsd-ok on success, gsd-err on failure).
;; When force? is #f and waves are incomplete, returns gsd-err with incomplete count.
;; When force? is #t, archives regardless of wave completion status.
(define (archive-completed-plan! base-dir [force? #f])
  (define planning-dir (build-path base-dir ".planning"))
  (define plan-path (build-path planning-dir "PLAN.md"))
  (define current-mode (gsm-current))
  (cond
    [(not (file-exists? plan-path)) (gsd-err #:mode current-mode #:message "No PLAN.md found")]
    [else
     ;; v0.32.0: Auto-sync wave executor state to PLAN.md before checking completion.
     ;; If the LLM completed all waves but never called /wave-done, the executor
     ;; knows they're done but PLAN.md still shows [Inbox]. Sync now.
     (sync-executor-to-plan! base-dir)
     (cond
       [(and (not force?) (not (all-waves-complete? base-dir)))
        (define text (call-with-input-file plan-path port->string))
        (define entries (parse-plan-index text))
        (define incomplete
          (for/list ([e entries]
                     #:when (not (member (wave-index-entry-status e) '("DONE" "DEFERRED"))))
            (wave-index-entry-status e)))
        (gsd-err
         #:mode current-mode
         #:message
         (format
          "Not all waves are complete (~a/~a have status: ~a). Use /done --force to archive anyway."
          (length incomplete)
          (length entries)
          (string-join incomplete ", ")))]
       [else
        (define plan-text (call-with-input-file plan-path port->string))
        ;; Update any remaining [Inbox] markers to [DONE] before archiving
        (update-all-wave-statuses! base-dir)
        (define title (extract-plan-title plan-text))
        (define archive-dir (archive-path-for-plan base-dir title))
        (with-handlers ([exn:fail:filesystem? (lambda (e)
                                                (gsd-err #:mode current-mode
                                                         #:message (format "Filesystem error: ~a"
                                                                           (exn-message e))))])
          (move-to-archive! base-dir archive-dir)
          (reset-gsd-after-archive!)
          ;; Count archived files
          (define archived-count
            (if (directory-exists? archive-dir)
                (length (directory-list archive-dir))
                0))
          (gsd-ok
           #:mode 'idle
           #:message (format "Plan archived to ~a" (path->string archive-dir))
           #:data
           (hasheq 'archive-path (path->string archive-dir) 'files-archived archived-count)))])]))

;; ============================================================
;; Helpers
;; ============================================================

;; v0.32.0: Sync state machine completed-waves to PLAN.md status markers.
;; When /go executes all waves but the LLM never calls /wave-done for all
;; of them, the state machine's completed-waves set knows about waves that
;; were marked complete (via /wave-done) but PLAN.md might have mixed-case
;; "Done" or still show [Inbox]. This normalizes everything to [DONE].
(define (sync-executor-to-plan! base-dir)
  ;; Source 1: gsm-completed-waves (updated by /wave-done calls)
  (define completed (gsm-completed-waves))
  (for ([idx (set->list completed)])
    (mark-wave-status! base-dir idx "DONE"))
  ;; Source 2: Normalize any mixed-case status markers in PLAN.md
  ;; (e.g., LLM wrote [Done] instead of [DONE])
  (normalize-plan-status-markers! base-dir)
  ;; Source 3: If in executing mode with completed waves, auto-complete
  ;; remaining [Inbox] waves. The LLM often completes all waves but
  ;; forgets to call /wave-done for the last one or doesn't update PLAN.md.
  ;; If at least one wave was completed via /wave-done and we're still in
  ;; executing mode, the execution ran and remaining Inbox waves are done.
  (auto-complete-inbox-waves! base-dir))

;; Normalize mixed-case status markers in PLAN.md to canonical forms.
;; The LLM often writes [Done] instead of [DONE]. This rewrites
;; all recognized variants to their canonical form.
(define (normalize-plan-status-markers! base-dir)
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (when (file-exists? plan-path)
    (define text (call-with-input-file plan-path port->string))
    (define entries (parse-plan-index text))
    (for ([e entries])
      (define status (wave-index-entry-status e))
      (define idx (wave-index-entry-idx e))
      (define upcase (string-upcase status))
      (cond
        ;; Already canonical — skip
        [(member status '("DONE" "DEFERRED" "FAILED" "Inbox" "In-Progress")) (void)]
        ;; Case variant of DONE (Done, done, Done, etc.)
        [(string=? upcase "DONE") (mark-wave-status! base-dir idx "DONE")]
        ;; Case variant of DEFERRED
        [(string=? upcase "DEFERRED") (mark-wave-status! base-dir idx "DEFERRED")]
        ;; Case variant of FAILED
        [(string=? upcase "FAILED") (mark-wave-status! base-dir idx "FAILED")]
        ;; Other non-canonical status — leave as-is
        [else (void)]))))

;; Auto-complete remaining [Inbox] waves when in executing mode.
;; Heuristic: if we're in executing mode and at least one wave was
;; completed via /wave-done, the LLM ran the execution. Remaining
;; [Inbox] waves were likely completed but the LLM forgot to mark them.
(define (auto-complete-inbox-waves! base-dir)
  (define mode (gsm-current))
  (define completed (gsm-completed-waves))
  (cond
    [(and (eq? mode 'executing) (not (set-empty? completed)))
     ;; In executing mode with some completions → auto-complete remaining
     (define plan-path (build-path base-dir ".planning" "PLAN.md"))
     (when (file-exists? plan-path)
       (define text (call-with-input-file plan-path port->string))
       (define entries (parse-plan-index text))
       (for ([e entries])
         (define status (string-upcase (wave-index-entry-status e)))
         (when (or (string=? status "INBOX") (string=? status "IN-PROGRESS"))
           (mark-wave-status! base-dir (wave-index-entry-idx e) "DONE"))))]
    [else (void)]))

;; Update all wave status markers in PLAN.md to [DONE] before archiving.
;; base-dir is the directory CONTAINING .planning/ (not .planning/ itself).
(define (update-all-wave-statuses! base-dir)
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (when (file-exists? plan-path)
    (define plan-text (call-with-input-file plan-path port->string))
    (define entries (parse-plan-index plan-text))
    (for ([e entries]
          #:when (not (string=? (wave-index-entry-status e) "DONE")))
      (update-wave-in-index! base-dir (wave-index-entry-idx e) "DONE"))))

;; extract-plan-title: imported from shared.rkt (v0.32.1 Wave 1 DRY))

;; Remove empty subdirectories under .planning/ after archive (#2160).
;; Does NOT delete the archive/ subdir itself.
(define (cleanup-empty-subdirs! planning-dir)
  (when (directory-exists? planning-dir)
    (for ([sub (directory-list planning-dir)])
      (define sub-path (build-path planning-dir sub))
      (when (and (directory-exists? sub-path) (not (string=? (path->string sub) "archive")))
        (with-handlers ([exn:fail? (lambda (e) (void))])
          (when (null? (directory-list sub-path))
            (delete-directory sub-path)))))))

;; Auto-create minimal STATE.md if it doesn't exist (#2164).
;; Called from /plan initialization.
(define (ensure-state-md! base-dir)
  (define state-path (build-path base-dir ".planning" "STATE.md"))
  (unless (file-exists? state-path)
    (define planning-dir (build-path base-dir ".planning"))
    (unless (directory-exists? planning-dir)
      (make-directory* planning-dir))
    (call-with-output-file
     state-path
     (lambda (out)
       (display "# Project State\n\nStatus: Active\n\n## Progress\n\n- [ ] Initial state\n" out))
     #:exists 'error)))
