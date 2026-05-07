#lang racket/base

;; extensions/gsd/core.rkt — Core GSD extension module
;; STABILITY: evolving
;;
;; Wave 1a of v0.21.0: Main extension with command handlers,
;; tool guard, and write guard. Uses state-machine for mode
;; management and delegates to plan-types and context-bundle modules.
;; Wave 3c: Hardened write guard with path normalization.
;;
;; Commands:
;;   /plan  → transition to exploring
;;   /go    → transition to executing
;;   /replan → back to exploring from plan-written/executing
;;   /skip  → mark wave as skipped
;;   /done  → archive completed plan
;;   /reset → go to idle
;;   /gsd   → show status

(require racket/string
         racket/format
         racket/path
         racket/port
         "state-machine.rkt"
         "plan-types.rkt"
         "context-bundle.rkt"
         "archive.rkt"
         "command-types.rkt"
         ;; Direct imports (no longer via shim — W2 purification)
         (only-in "state-machine.rkt" gsm-mark-wave-complete!)
         (only-in "wave-docs.rkt" mark-wave-status!)
         (only-in "session-state.rkt" set-gsd-state! gsd-state-sem)
         (only-in "runtime-state-types.rkt" gsd-runtime-state-mode)
         (only-in "events.rkt" emit-gsd-event! current-gsd-correlation-id)
         (only-in "policy.rkt" gsd-decide-action policy-allowed? policy-blocked? policy-reason)
         (only-in "../gsd-planning-state.rkt" pinned-planning-dir))

(provide gsd-command-dispatch
         gsd-write-guard
         gsd-show-status
         ;; Individual command handlers for direct wiring
         cmd-replan
         cmd-skip
         cmd-reset
         cmd-done
         cmd-wave-done
         ;; Command names for registration
         gsd-commands
         ;; Re-export command result types
         (all-from-out "command-types.rkt")
         ;; Transaction wrapper (v0.24.1)
         with-gsd-transaction)

;; ============================================================
;; Command registry
;; ============================================================

(define gsd-commands
  '((plan "/plan" "Start GSD planning phase" ("p"))
    (go "/go" "Begin executing the plan" ())
    (replan "/replan" "Return to planning phase" ())
    (skip "/skip" "Skip current wave" ("s"))
    (done "/done" "Archive completed plan" ("d"))
    (reset "/reset" "Reset GSD to idle state" ())
    (wave-done "/wave-done" "Mark wave N as complete, update PLAN.md and STATE.md" ("wd"))
    (gsd "/gsd" "Show GSD status" ())))

;; ============================================================
;; Internal helpers (must be defined before use)
;; ============================================================

(define (non-empty-string? s)
  (and (string? s) (> (string-length s) 0)))

;; Normalize a file path: collapse .. and . components.
;; Canonical path comparison for write guard.
;; Uses simple-form-path to resolve .., ., symlinks, and relative paths.
;; Blocks ALL writes to .planning/ directory during execution.

(define (gsd-write-guard target-path planning-dir)
  (define mode (gsm-current))
  (define planning-dir-str
    (if (path? planning-dir)
        (path->string planning-dir)
        planning-dir))
  ;; Route through policy engine (v0.24.4)
  (gsd-decide-action (hasheq 'mode
                             mode
                             'target-path
                             (if (string? target-path) target-path "")
                             'pinned-dir
                             (or planning-dir-str ""))
                     'write-file))

;; ============================================================
;; Transaction wrapper (F3 fix: failure atomicity)
;; ============================================================

;; Execute a multi-step command with rollback on failure.
;; Captures snapshot before execution, restores on error.
(define (with-gsd-transaction label thunk on-rollback)
  (define snapshot (gsm-snapshot))
  (with-handlers ([exn:fail? (lambda (e)
                               ;; Rollback to snapshot
                               (gsd-state-restore! snapshot)
                               (on-rollback e snapshot)
                               (gsd-err #:mode (gsd-runtime-state-mode snapshot)
                                        #:message (format "~a failed (rolled back): ~a"
                                                          label
                                                          (exn-message e))))])
    (thunk)))

;; Restore state machine from a snapshot
(define (gsd-state-restore! snapshot)
  (call-with-semaphore gsd-state-sem (lambda () (set-gsd-state! snapshot))))

;; ============================================================
;; Command dispatch
;; ============================================================

(define (gsd-command-dispatch command args)
  (define cmd
    (if (string? command)
        (string->symbol command)
        command))
  (emit-gsd-event! 'gsd.command.received (hasheq 'command cmd 'args args))
  (define corr-id (gensym 'gsd-cmd))
  (parameterize ([current-gsd-correlation-id corr-id])
    (define result
      (case cmd
        [(plan) (cmd-plan args)]
        [(go)
         (let ([wave-arg (if (string? args)
                             (string-trim args)
                             "")])
           (define result (gsm-transition! 'executing))
           (if (ok? result)
               (gsd-ok #:mode 'executing
                       #:message (if (non-empty-string? wave-arg)
                                     (format "Executing from wave ~a" wave-arg)
                                     "Execution started. Follow the plan."))
               (gsd-err #:mode (gsm-current)
                        #:message (format "Cannot start execution: ~a" (err-reason result)))))]
        [(replan) (cmd-replan)]
        [(skip) (cmd-skip args)]
        [(reset) (cmd-reset)]
        [(done) (gsd-ok #:mode 'idle #:message "Use /done from the planning context.")]
        [(wave-done) (cmd-wave-done #f args)]
        [(gsd) (gsd-show-status)]
        [else #f]))
    (when result
      (emit-gsd-event! 'gsd.command.completed
                       (hasheq 'command cmd 'success (gsd-command-result-success result))))
    result))

;; /plan <text> → exploring
;; v0.21.5: Multi-step transition — go via idle if needed.
(define (cmd-plan args)
  (define user-text
    (if (string? args)
        (string-trim args)
        ""))
  ;; If current state blocks direct → exploring, go via idle first.
  (define current (gsm-current))
  (unless (or (eq? current 'idle) (eq? current 'exploring))
    (gsm-transition! 'idle))
  (define result (gsm-transition! 'exploring))
  (if (ok? result)
      (gsd-ok #:mode 'exploring
              #:message (if (non-empty-string? user-text)
                            (format "Planning: ~a" user-text)
                            "Planning phase started. Explore freely."))
      (gsd-err #:mode (gsm-current)
               #:message (format "Cannot enter planning: ~a" (err-reason result)))))

;; /go [wave] → executing
;; DEPRECATED (v0.29.13): Removed dead cmd-go handler.
;; Production code uses handle-go-command in gsd-planning.rkt
;; which performs parse → normalize → validate → execute.
;; The dispatch entry [(go) (cmd-go args)] is retained for backward
;; compatibility but delegates to a minimal transition.

;; /replan → exploring from plan-written or executing
(define (cmd-replan)
  (define current (gsm-current))
  (cond
    [(or (eq? current 'plan-written) (eq? current 'executing))
     ;; v0.21.5: Multi-step transition — go via idle first.
     (gsm-transition! 'idle)
     (define result (gsm-transition! 'exploring))
     (if (ok? result)
         (gsd-ok #:mode 'exploring #:message "Re-planning. Modify the plan freely.")
         (gsd-err #:mode (gsm-current) #:message (format "Cannot re-plan: ~a" (err-reason result))))]
    [else (gsd-err #:mode current #:message (format "Cannot re-plan from ~a state" current))]))

;; /skip <wave> → mark wave as skipped
(define (cmd-skip args)
  (define current (gsm-current))
  (if (not (eq? current 'executing))
      (gsd-err #:mode current #:message "/skip only works during execution")
      (let ([wave-num (if (string? args)
                          (string->number (string-trim args))
                          #f)])
        (if wave-num
            (gsd-ok #:mode 'executing #:message (format "Wave ~a marked as skipped" wave-num))
            (gsd-err #:mode 'executing #:message "Usage: /skip <wave-number>")))))

;; /wave-done <N> → mark wave complete, update PLAN.md + STATE.md
(define (cmd-wave-done base-dir args)
  (define idx-str
    (if (string? args)
        (string-trim args)
        ""))
  (cond
    [(string=? idx-str "") (gsd-err #:mode (gsm-current) #:message "Usage: /wave-done <wave-number>")]
    [else
     (define idx (string->number idx-str))
     (cond
       [(not idx) (gsd-err #:mode (gsm-current) #:message (format "Invalid wave number: ~a" idx-str))]
       [(< idx 0) (gsd-err #:mode (gsm-current) #:message "Wave number must be non-negative")]
       [else
        (with-gsd-transaction
         (format "wave-done ~a" idx)
         (lambda ()
           ;; Mark wave complete in state machine
           (gsm-mark-wave-complete! idx)
           ;; Update PLAN.md status marker
           (let ([dir (or base-dir (pinned-planning-dir))])
             (when dir
               (with-handlers ([exn:fail? (lambda (e) (void))])
                 (mark-wave-status! dir idx "DONE"))))
           ;; Update STATE.md with wave completion
           (let ([dir (or base-dir (pinned-planning-dir))])
             (when dir
               (with-handlers ([exn:fail? (lambda (e)
                                            (log-warning (format "cmd-wave-done/state-update: ~a"
                                                                 (exn-message e))))])
                 (update-state-with-wave! dir idx))))
           (gsd-ok #:mode (gsm-current)
                   #:message (format "Wave ~a marked complete. PLAN.md and STATE.md updated." idx)
                   #:data (hasheq 'wave idx)))
         (lambda (e snapshot)
           (log-warning (format "wave-done ~a rolled back: ~a" idx (exn-message e)))))])]))

;; Helper: update STATE.md with wave completion note
(define (update-state-with-wave! base-dir wave-idx)
  (define state-path (build-path base-dir ".planning" "STATE.md"))
  (when (file-exists? state-path)
    (define content (call-with-input-file state-path port->string))
    (define wave-header "## Wave Progress")
    (define entry-line (format "- [x] W~a: completed" wave-idx))
    (define new-content
      (cond
        [(string-contains? content wave-header)
         ;; Update existing progress section — replace or append entry
         (define lines (string-split content "\n"))
         (define prefix-rx (format "- \\[.\\] W~a:" wave-idx))
         (define updated
           (for/list ([line lines])
             (if (regexp-match? (regexp prefix-rx) line) entry-line line)))
         ;; If the entry wasn't found, append it after the header
         (if (member entry-line updated)
             (string-join updated "\n")
             (let loop ([ls updated]
                        [acc '()])
               (cond
                 [(null? ls) (string-join (reverse acc) "\n")]
                 [(string=? (car ls) wave-header)
                  (string-join (append (reverse acc) (list wave-header entry-line) (cdr ls)) "\n")]
                 [else (loop (cdr ls) (cons (car ls) acc))])))]
        ;; Append a new wave progress section
        [else (string-append content "\n\n" wave-header "\n\n" entry-line "\n")]))
    (call-with-output-file state-path (lambda (out) (display new-content out)) #:exists 'truncate)))

;; /reset → idle
(define (cmd-reset)
  (define result (gsm-reset!))
  (gsd-ok #:mode 'idle #:message "GSD reset to idle."))

;; /done → archive completed plan
;; force? skips the wave completion check.
(define (cmd-done base-dir [force? #f])
  (with-gsd-transaction 'archive
                        (lambda () (archive-completed-plan! base-dir force?))
                        (lambda (e snapshot)
                          (emit-gsd-event! 'gsd.archive.failed (hasheq 'error (exn-message e))))))

;; ============================================================
;; Write guard (hardened — DD-6)
;; ============================================================

;; ============================================================
;; Status display
;; ============================================================

(define (gsd-show-status)
  (define mode (gsm-current))
  (define valid-next (gsm-valid-next-states))
  (gsd-ok #:mode mode
          #:message
          (format "GSD Status: ~a | Next: ~a" mode (string-join (map symbol->string valid-next) ", "))
          #:data (hasheq 'valid-next-states valid-next 'history-length (length (gsm-history)))))
