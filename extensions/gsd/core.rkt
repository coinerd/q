#lang racket/base

;; extensions/gsd/core.rkt — Core GSD extension module
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
         ;; v0.21.10: wave-done needs mark-wave-complete! which also updates PLAN.md
         (only-in "../gsd-planning-state.rkt" mark-wave-complete! pinned-planning-dir))

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
         gsd-commands)

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

(define (non-empty? s)
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
  (cond
    [(not (eq? mode 'executing)) #t]
    [(not (string? target-path)) #t]
    [(not planning-dir-str) #t]
    [else
     ;; Resolve both paths to canonical form
     (define canonical-target
       (with-handlers ([exn:fail? (λ (_) target-path)])
         (path->string (simple-form-path (string->path target-path)))))
     (define canonical-planning
       (with-handlers ([exn:fail? (λ (_) planning-dir-str)])
         (path->string (simple-form-path (string->path planning-dir-str)))))
     ;; Check if target is inside .planning/ directory
     (define in-planning-dir?
       (or (string=? canonical-target canonical-planning)
           (string-prefix? canonical-target (string-append canonical-planning "/"))))
     (if in-planning-dir?
         (hasheq 'blocked
                 #t
                 'tool
                 "write"
                 'mode
                 'executing
                 'reason
                 (format "Cannot write to ~a during execution (use /replan to modify)" target-path))
         #t)]))

;; ============================================================
;; Command dispatch
;; ============================================================

(define (gsd-command-dispatch command args)
  (define cmd
    (if (string? command)
        (string->symbol command)
        command))
  (case cmd
    [(plan) (cmd-plan args)]
    [(go) (cmd-go args)]
    [(replan) (cmd-replan)]
    [(skip) (cmd-skip args)]
    [(reset) (cmd-reset)]
    [(done) (hasheq 'success #t 'mode 'idle 'message "Use /done from the planning context.")]
    [(wave-done) (cmd-wave-done #f args)]
    [(gsd) (gsd-show-status)]
    [else #f]))

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
      (hasheq 'success
              #t
              'mode
              'exploring
              'message
              (if (non-empty? user-text)
                  (format "Planning: ~a" user-text)
                  "Planning phase started. Explore freely."))
      (hasheq 'success
              #f
              'mode
              (gsm-current)
              'message
              (format "Cannot enter planning: ~a" (err-reason result)))))

;; /go [wave] → executing
(define (cmd-go args)
  (define wave-arg
    (if (string? args)
        (string-trim args)
        ""))
  (define result (gsm-transition! 'executing))
  (if (ok? result)
      (hasheq 'success
              #t
              'mode
              'executing
              'message
              (if (non-empty? wave-arg)
                  (format "Executing from wave ~a" wave-arg)
                  "Execution started. Follow the plan."))
      (hasheq 'success
              #f
              'mode
              (gsm-current)
              'message
              (format "Cannot start execution: ~a" (err-reason result)))))

;; /replan → exploring from plan-written or executing
(define (cmd-replan)
  (define current (gsm-current))
  (cond
    [(or (eq? current 'plan-written) (eq? current 'executing))
     ;; v0.21.5: Multi-step transition — go via idle first.
     (gsm-transition! 'idle)
     (define result (gsm-transition! 'exploring))
     (if (ok? result)
         (hasheq 'success #t 'mode 'exploring 'message "Re-planning. Modify the plan freely.")
         (hasheq 'success
                 #f
                 'mode
                 (gsm-current)
                 'message
                 (format "Cannot re-plan: ~a" (err-reason result))))]
    [else
     (hasheq 'success #f 'mode current 'message (format "Cannot re-plan from ~a state" current))]))

;; /skip <wave> → mark wave as skipped
(define (cmd-skip args)
  (define current (gsm-current))
  (if (not (eq? current 'executing))
      (hasheq 'success #f 'mode current 'message "/skip only works during execution")
      (let ([wave-num (if (string? args)
                          (string->number (string-trim args))
                          #f)])
        (if wave-num
            (hasheq 'success
                    #t
                    'mode
                    'executing
                    'message
                    (format "Wave ~a marked as skipped" wave-num))
            (hasheq 'success #f 'mode 'executing 'message "Usage: /skip <wave-number>")))))

;; /wave-done <N> → mark wave complete, update PLAN.md + STATE.md
(define (cmd-wave-done base-dir args)
  (define idx-str
    (if (string? args)
        (string-trim args)
        ""))
  (cond
    [(string=? idx-str "") (hasheq 'success #f 'message "Usage: /wave-done <wave-number>")]
    [else
     (define idx (string->number idx-str))
     (cond
       [(not idx) (hasheq 'success #f 'message (format "Invalid wave number: ~a" idx-str))]
       [(< idx 0) (hasheq 'success #f 'message "Wave number must be non-negative")]
       [else
        ;; Mark wave complete in state machine + PLAN.md
        (mark-wave-complete! idx)
        ;; Update STATE.md with wave completion
        (when base-dir
          (with-handlers ([exn:fail? (lambda (e)
                                       (log-warning (format "cmd-wave-done/state-update: ~a"
                                                            (exn-message e))))])
            (update-state-with-wave! base-dir idx)))
        (hasheq 'success
                #t
                'wave
                idx
                'message
                (format "Wave ~a marked complete. PLAN.md and STATE.md updated." idx))])]))

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
  (hasheq 'success #t 'mode 'idle 'message "GSD reset to idle."))

;; /done → archive completed plan
;; force? skips the wave completion check.
(define (cmd-done base-dir [force? #f])
  (define result (archive-completed-plan! base-dir force?))
  (if (hash-ref result 'success #f)
      (hasheq 'success
              #t
              'mode
              'idle
              'message
              (format "\u2705 Plan archived to ~a (~a files)"
                      (hash-ref result 'archive-path "?")
                      (hash-ref result 'files-archived 0)))
      (hasheq 'success #f 'mode (gsm-current) 'message (hash-ref result 'error "Archive failed"))))

;; ============================================================
;; Write guard (hardened — DD-6)
;; ============================================================

;; ============================================================
;; Status display
;; ============================================================

(define (gsd-show-status)
  (define mode (gsm-current))
  (define valid-next (gsm-valid-next-states))
  (hasheq
   'mode
   mode
   'valid-next-states
   valid-next
   'history-length
   (length (gsm-history))
   'message
   (format "GSD Status: ~a | Next: ~a" mode (string-join (map symbol->string valid-next) ", "))))
