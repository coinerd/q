#lang racket/base

;; extensions/gsd/archive.rkt — GSD Plan Archival
;;
;; v0.21.9 #2155: Archive completed plans.
;; Validates all waves complete, creates archive directory,
;; moves plan files, resets GSD state.

(require racket/file
         racket/path
         racket/port
         racket/string
         racket/format
         "plan-types.rkt"
         "wave-docs.rkt"
         "state-machine.rkt")

(provide archive-completed-plan!
         all-waves-complete?
         archive-path-for-plan
         move-to-archive!
         reset-gsd-after-archive!
         cleanup-empty-subdirs!
         ensure-state-md!)

;; ============================================================
;; Validation
;; ============================================================

;; Check if all waves in PLAN.md are complete (DONE or DEFERRED).
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
          (define status (wave-index-entry-status e))
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
;; When force? is #f and waves are incomplete, returns a warning with incomplete count.
;; When force? is #t, archives regardless of wave completion status.
;; Returns (hasheq 'success #t 'archive-path ...) on success.
;; Returns (hasheq 'success #f 'error ...) on failure.
(define (archive-completed-plan! base-dir [force? #f])
  (define planning-dir (build-path base-dir ".planning"))
  (define plan-path (build-path planning-dir "PLAN.md"))
  (cond
    [(not (file-exists? plan-path)) (hasheq 'success #f 'error "No PLAN.md found")]
    [(and (not force?) (not (all-waves-complete? base-dir)))
     (define text (call-with-input-file plan-path port->string))
     (define entries (parse-plan-index text))
     (define incomplete
       (for/list ([e entries]
                  #:when (not (member (wave-index-entry-status e) '("DONE" "DEFERRED"))))
         (wave-index-entry-status e)))
     (hasheq
      'success
      #f
      'error
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
     (define moved-files '())
     (with-handlers ([exn:fail:filesystem?
                      (lambda (e)
                        (hasheq 'success #f 'error (format "Filesystem error: ~a" (exn-message e))))])
       (move-to-archive! base-dir archive-dir)
       (reset-gsd-after-archive!)
       ;; Count archived files
       (define archived-count
         (if (directory-exists? archive-dir)
             (length (directory-list archive-dir))
             0))
       (hasheq 'success
               #t
               'archive-path
               (path->string archive-dir)
               'files-archived
               archived-count))]))

;; ============================================================
;; Helpers
;; ============================================================

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

(define (extract-plan-title plan-text)
  (define lines (string-split plan-text "\n"))
  (define title-line
    (for/first ([line lines]
                #:when (regexp-match? #rx"^# +Plan:" line))
      line))
  (cond
    [(not title-line) "archived-plan"]
    [else
     (define m (regexp-match #rx"^# +Plan: +(.+)$" title-line))
     (if m
         (string-trim (cadr m))
         "archived-plan")]))

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
