#lang racket/base

;; extensions/gsd/core.rkt — Core GSD extension module
;;
;; Wave 1a of v0.21.0: Main extension with command handlers,
;; tool guard, and write guard. Uses state-machine for mode
;; management and delegates steering to steering module.
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
         "state-machine.rkt"
         "plan-types.rkt"
         "context-bundle.rkt"
         "steering.rkt")

(provide gsd-command-dispatch
         gsd-tool-guard
         gsd-write-guard
         gsd-show-status
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
(define (normalize-path p)
  (cond
    [(not (string? p)) p]
    [else
     (define parts (string-split (string-trim p) "/"))
     (define resolved (resolve-dotdots parts))
     (string-join resolved "/")]))

;; Resolve .. by folding: each .. pops the last component.
(define (resolve-dotdots parts)
  (define rev
    (foldl (lambda (part acc)
             (cond
               [(string=? part "..")
                (if (null? acc)
                    acc
                    (cdr acc))]
               [(string=? part ".") acc]
               [(string=? part "") acc]
               [else (cons part acc)]))
           '()
           parts))
  (reverse rev))

;; Check if a normalized path targets PLAN.md in the planning dir.
;; Handles suffix matching for relative paths and .. traversal.
(define (path-targets-plan? normalized plan-path)
  (cond
    [(not (non-empty? plan-path)) #f]
    [(string=? normalized plan-path) #t]
    ;; Suffix match: does the path end with .planning/PLAN.md?
    [else (string-suffix? normalized ".planning/PLAN.md")]))

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
(define (cmd-plan args)
  (define user-text
    (if (string? args)
        (string-trim args)
        ""))
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
  (reset-steering-state!)
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

;; During executing mode, block writes to .planning/PLAN.md.
;; Uses path normalization to catch .. traversal and other tricks.
(define (gsd-write-guard target-path planning-dir)
  (define mode (gsm-current))
  (cond
    [(not (eq? mode 'executing)) #t]
    [(not (string? target-path)) #t]
    [else
     (define normalized (normalize-path target-path))
     (define plan-path
       (if (string? planning-dir)
           (normalize-path (string-append planning-dir "/PLAN.md"))
           ""))
     (if (path-targets-plan? normalized plan-path)
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
;; Status display
;; ============================================================

(define (gsd-show-status)
  (define mode (gsm-current))
  (define valid-next (gsm-valid-next-states))
  (define snapshot (gsm-snapshot))
  (hasheq
   'mode
   mode
   'valid-next-states
   valid-next
   'history-length
   (length (hash-ref snapshot 'history))
   'message
   (format "GSD Status: ~a | Next: ~a" mode (string-join (map symbol->string valid-next) ", "))))
