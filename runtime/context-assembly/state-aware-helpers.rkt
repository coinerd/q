#lang racket/base

;; runtime/context-assembly/state-aware-helpers.rkt
;; W6 v0.99.35: Pure helpers extracted from state-aware-builder.rkt
;;
;; These functions perform pure computation — no I/O, no parameter mutation,
;; no event emission, no logging. They can be tested without runtime setup.
;;
;; STABILITY: evolving

(require racket/list
         racket/string
         (only-in "../../util/content/content-parts.rkt" make-text-part text-part? text-part-text)
         (only-in "../../util/message/message.rkt"
                  message?
                  message-id
                  message-content
                  message-role
                  make-message)
         (only-in "task-conclusion.rkt"
                  task-conclusion?
                  task-conclusion-text
                  task-conclusion-origin-message-ids)
         (only-in "../../util/fsm/fsm.rkt" fsm-state? fsm-state-name)
         (only-in "rollback-actions.rkt" warnings->actions select-highest-priority-action))

(provide coerce-task-state
         extract-recent-text
         check-rollback-triggers
         check-rollback-triggers-with-actions
         ws-entry->conclusion-or-self
         state-guidance-table)

;; ============================================================
;; State coercion (pure)
;; ============================================================

;; Converts task-state (symbol, fsm-state struct, or #f) to a bare symbol or #f.
(define (coerce-task-state ts)
  (cond
    [(not ts) #f]
    [(symbol? ts) ts]
    [(fsm-state? ts) (fsm-state-name ts)]
    [else #f]))

;; ============================================================
;; Recent text extraction (pure)
;; ============================================================

;; Takes a list of messages and count N, returns concatenated text of the last N
;; assistant/user messages truncated to 200 chars. Returns #f if no text found.
(define (extract-recent-text messages n)
  (define assistant-msgs
    (for/list ([m (in-list messages)]
               #:when (and (message? m) (memq (message-role m) '(assistant user))))
      (define content-parts (message-content m))
      (define text
        (for/fold ([acc ""])
                  ([part (in-list (if (list? content-parts)
                                      content-parts
                                      '()))])
          (if (text-part? part)
              (string-append acc (if (string=? acc "") "" " ") (text-part-text part))
              acc)))
      text))
  (define recent (take (reverse assistant-msgs) (min n (length assistant-msgs))))
  (define joined (string-join (filter (lambda (s) (> (string-length s) 0)) recent) ". "))
  (if (> (string-length joined) 0)
      (substring joined 0 (min (string-length joined) 200))
      #f))

;; ============================================================
;; State guidance table (pure constant)
;; ============================================================

;; State-specific guidance strings (action-oriented instructions)
(define state-guidance-table
  (hasheq 'idle
          "Awaiting instructions."
          'exploration
          (string-append "Focus on reading and understanding the codebase. "
                         "Record key findings with record_conclusion before moving on.")
          'planning
          (string-append "Break down the task into steps. "
                         "Save your plan as conclusions with record_conclusion.")
          'implementation
          (string-append "Focus on editing files. "
                         "Record key decisions with record_conclusion. "
                         "Prefer existing conclusions over re-reading files.")
          'verification
          (string-append "Run tests and verify correctness. "
                         "Record test results as conclusions with record_conclusion.")
          'debugging
          (string-append "Focus on error-related files. "
                         "Save error analysis as conclusions with record_conclusion.")))

;; ============================================================
;; Rollback trigger checks (pure)
;; ============================================================

;; Returns a list of warning tuples: (trigger-type message).
;; Triggers are observational — callers decide what to do.
(define (check-rollback-triggers #:before-messages before-messages
                                 #:after-messages after-messages
                                 #:conclusion-coverage conclusion-coverage
                                 #:repeat-tool-count repeat-tool-count)
  (define warnings
    (reverse
     (for/fold ([acc '()])
               ([trigger
                 (in-list
                  ;; Trigger 1: Excessive savings (>50% messages cut)
                  (list (and (and (> before-messages 0)
                                  (> after-messages 0)
                                  (< after-messages (* before-messages 0.50)))
                             (list 'excessive-savings
                                   (format "Context reduced by >50%: ~a → ~a messages"
                                           before-messages
                                           after-messages)))
                        ;; Trigger 2: Low conclusion coverage (amnesia risk)
                        (and (< conclusion-coverage 0.20)
                             (list 'amnesia-risk
                                   (format "Conclusion coverage too low: ~a" conclusion-coverage)))
                        ;; Trigger 3: Repeated tool calls (same file re-read > 2x)
                        (and (> repeat-tool-count 2)
                             (list 'task-amnesia-detected
                                   (format "Repeated tool calls detected: ~a re-reads"
                                           repeat-tool-count)))
                        ;; Trigger 4 — Stuck detection (>=6 tool calls, 0 conclusions)
                        (and (>= repeat-tool-count 6)
                             (= conclusion-coverage 0)
                             (list 'stuck-detected
                                   (format "stuck: ~a tool calls without recording conclusions"
                                           repeat-tool-count)))))])
       (if trigger
           (cons trigger acc)
           acc))))
  warnings)

;; Returns (values warnings recommended-action) where recommended-action
;; is the highest-priority rollback-action or #f.
(define (check-rollback-triggers-with-actions #:before-messages before-messages
                                              #:after-messages after-messages
                                              #:conclusion-coverage conclusion-coverage
                                              #:repeat-tool-count repeat-tool-count)
  (define warnings
    (check-rollback-triggers #:before-messages before-messages
                             #:after-messages after-messages
                             #:conclusion-coverage conclusion-coverage
                             #:repeat-tool-count repeat-tool-count))
  ;; Pass full (symbol . string) pairs to warnings->actions for symbol-based matching.
  (define actions (warnings->actions warnings))
  (define recommended (select-highest-priority-action actions))
  (values warnings recommended))

;; ============================================================
;; Conclusion-first working-set replacement (pure)
;; ============================================================

;; If a conclusion references this message's ID via origin-message-ids,
;; returns a compact replacement message with conclusion text.
;; Otherwise returns the original message unchanged.
(define (ws-entry->conclusion-or-self ws-msg conclusions)
  (define msg-id-val (message-id ws-msg))
  (define (origin-matches? c)
    (define oids (task-conclusion-origin-message-ids c))
    (and (pair? oids) (ormap (lambda (oid) (equal? msg-id-val oid)) oids)))
  (define matching-conclusion
    (for/first ([c (in-list conclusions)]
                #:when (and (task-conclusion? c) (origin-matches? c)))
      c))
  (cond
    [matching-conclusion
     (make-message (message-id ws-msg)
                   #f
                   'system
                   'system-instruction
                   (list (make-text-part (format "[Conclusion replaces context] ~a"
                                                 (task-conclusion-text matching-conclusion))))
                   (current-seconds)
                   (hasheq))]
    [else ws-msg]))
