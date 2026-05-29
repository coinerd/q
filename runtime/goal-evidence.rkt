#lang racket/base

;; q/runtime/goal-evidence.rkt — Evidence discipline + no-progress detection
;;
;; Two responsibilities:
;; 1. System prompt injection: forces worker agent to produce verifiable evidence
;; 2. No-progress detector: detects stall conditions from consecutive evaluations

(require racket/contract
         racket/match
         racket/format
         racket/string
         racket/list
         "goal-state.rkt")

;; ============================================================
;; Provides
;; ============================================================

(provide GOAL-EVIDENCE-SYSTEM-PROMPT
         goal-system-instructions
         detect-no-progress
         consecutive-same-reason?
         evidence-prompt-for-goal)

;; ============================================================
;; Evidence system prompt
;; ============================================================

(define GOAL-EVIDENCE-SYSTEM-PROMPT
  (string-append "You are working under an active q goal. Before stopping:\n"
                 "1. Report which goal criteria are satisfied with specific evidence\n"
                 "2. List exact checks you ran and their outputs (commands, exit codes, file diffs)\n"
                 "3. Describe remaining blockers precisely\n"
                 "4. If you believe the goal is achieved, state 'GOAL ACHIEVED' with proof\n"
                 "5. Never claim success without running at least one verification command"))

;; Build the full system instructions to inject when a goal is active.
;; Returns a list of strings to append to system-instructions.
(define/contract (goal-system-instructions goal-st)
  (-> goal-state? (listof string?))
  (list GOAL-EVIDENCE-SYSTEM-PROMPT
        (format "Active goal: ~a (turn ~a/~a)"
                (goal-state-goal-text goal-st)
                (goal-state-turns-used goal-st)
                (goal-state-max-turns goal-st))))

;; Build a goal-specific evidence prompt for the continuation message.
(define/contract (evidence-prompt-for-goal goal-text evaluation)
  (-> string? (or/c evaluation-result? #f) string?)
  (define base (format "Continue working toward: ~a" goal-text))
  (if evaluation
      (format
       "~a\n\nPrevious evaluation: ~a\n\nProvide specific evidence: run commands, check files, show outputs."
       base
       (evaluation-result-reason evaluation))
      (format "~a\n\nProvide specific evidence: run commands, check files, show outputs." base)))

;; ============================================================
;; No-progress detection
;; ============================================================

;; Returns #t if the last NO-PROGRESS-THRESHOLD evaluations have the same reason
;; AND none achieved the goal.
(define/contract (consecutive-same-reason? evaluations)
  (-> (listof evaluation-result?) boolean?)
  (cond
    [(< (length evaluations) NO-PROGRESS-THRESHOLD) #f]
    [else
     (define last-n (take-right evaluations NO-PROGRESS-THRESHOLD))
     (define reasons (map evaluation-result-reason last-n))
     (define all-failed? (andmap (lambda (e) (not (evaluation-result-achieved? e))) last-n))
     (define all-same? (andmap (lambda (r) (equal? r (car reasons))) (cdr reasons)))
     (and all-failed? all-same?)]))

;; Detect no-progress from a list of evaluation results.
;; Returns #t if stall is detected, #f otherwise.
(define/contract (detect-no-progress evaluations)
  (-> (listof evaluation-result?) boolean?)
  (cond
    [(< (length evaluations) NO-PROGRESS-THRESHOLD) #f]
    [else (consecutive-same-reason? evaluations)]))
