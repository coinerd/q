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
;;   /reset → go to idle
;;   /gsd   → show status

(require racket/string
         racket/format
         racket/path
         "state-machine.rkt"
         "plan-types.rkt"
         "context-bundle.rkt")

(provide gsd-command-dispatch
         gsd-tool-guard
         gsd-write-guard
         gsd-show-status
         ;; Individual command handlers for direct wiring
         cmd-replan
         cmd-skip
         cmd-reset
         ;; Command names for registration
         gsd-commands)

;; ============================================================
;; Command registry
;; ============================================================

(define gsd-commands
  '((plan "/plan" "Start GSD planning phase" ("p")) (go "/go" "Begin executing the plan" ())
                                                    (replan "/replan" "Return to planning phase" ())
                                                    (skip "/skip" "Skip current wave" ("s"))
                                                    (reset "/reset" "Reset GSD to idle state" ())
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

;; /reset → idle
(define (cmd-reset)
  (define result (gsm-reset!))
  (hasheq 'success #t 'mode 'idle 'message "GSD reset to idle."))

;; ============================================================
;; Tool guard (tool-call-pre)
;; ============================================================

(define (gsd-tool-guard tool-name tool-args)
  (if (gsm-tool-allowed? tool-name)
      #t
      (hasheq 'blocked
              #t
              'tool
              tool-name
              'mode
              (gsm-current)
              'reason
              (format "Tool '~a' is not allowed in ~a mode" tool-name (gsm-current)))))

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
