#lang racket/base

;; extensions/gsd/command-handlers.rkt — GSD slash-command handlers
;; STABILITY: stable
;;
;; Extracted from extensions/gsd-planning.rkt (v0.34.6 W0b — A-02 decomposition).

(require racket/match
         racket/string
         racket/set
         json
         "../define-extension.rkt"
         "../ext-commands.rkt"
         "../hooks.rkt"
         "../tool-api.rkt"
         "../gsd-planning-state.rkt"
         "../gsd-planning/command-normalization.rkt"
         "../gsd-planning/plan-diff.rkt"
         "../gsd/state-machine.rkt"
         (only-in "../gsd/core.rkt"
                  cmd-replan
                  cmd-skip
                  cmd-reset
                  cmd-done
                  cmd-wave-done
                  gsd-command-result-success
                  gsd-command-result-message
                  gsd-command-result-data
                  gsd-command-result?
                  gsd-result?
                  gsd-success?
                  gsd-failed?
                  gsd-command-result-mode
                  with-gsd-transaction)
         "../gsd/plan-types.rkt"
         "../gsd/plan-validator.rkt"
         (except-in "../gsd/wave-executor.rkt" next-pending-wave)
         "../gsd/prompts.rkt"
         "../gsd/context-bundle.rkt"
         "../gsd/wave-docs.rkt"
         "tool-handlers.rkt"
         (only-in "../gsd/archive.rkt" ensure-state-md!)
         (only-in "../gsd/events.rkt"
                  [emit-gsd-event! events:emit-gsd-event!]
                  [set-gsd-event-bus! events:set-gsd-event-bus!])
         (only-in "../gsd/session-state.rkt" set-gsd-state! gsd-state-sem))

(provide register-gsd-commands
         handle-execute-command
         handle-go-command
         handle-gsd-status
         handle-artifact-command)

;; ============================================================
;; Command registration
;; ============================================================

(define (register-gsd-commands ctx)
  (ext-register-command! ctx "/plan" "Display current GSD plan" 'general '() '("p"))
  (ext-register-command! ctx "/state" "Display current project state" 'general '() '("s"))
  (ext-register-command! ctx "/handoff" "Display handoff status" 'general '() '("ho"))
  (ext-register-command! ctx "/go" "Start implementing the current plan" 'general '() '("implement" "i"))
  (ext-register-command! ctx "/replan" "Return to planning phase" 'general '() '())
  (ext-register-command! ctx "/skip" "Skip a wave (usage: /skip N)" 'general '() '())
  (ext-register-command! ctx "/reset" "Reset GSD to idle state" 'general '() '())
  (ext-register-command! ctx "/wave-done"
                         "Mark wave N complete, update PLAN.md and STATE.md"
                         'general '() '("wd"))
  (ext-register-command! ctx "/done" "Archive completed plan" 'general '() '("d"))
  (ext-register-command! ctx "/gsd" "Show GSD workflow status" 'general '() '())
  (hook-pass #f))

;; ============================================================
;; Command dispatch
;; ============================================================

(define (handle-execute-command payload)
  (define cmd (hash-ref payload 'command #f))
  (define input-text (hash-ref payload 'input ""))
  (define base-dir (or (pinned-planning-dir) (current-directory)))
  (match cmd
    [(? (lambda (c) (member c '("/go" "/implement" "/i"))))
     (handle-go-command base-dir input-text)]
    ["/gsd" (handle-gsd-status)]
    ["/replan"
     (define result (cmd-replan))
     (events:emit-gsd-event! 'gsd.mode.changed (hasheq 'mode 'exploring))
     (hook-amend (hasheq 'text (or (gsd-command-result-message result) "")))]
    ["/skip"
     (define args-text (extract-cmd-args input-text))
     (define result (cmd-skip args-text))
     (when (and (gsd-success? result) base-dir)
       (define idx (and (string->number (string-trim args-text))))
       (when idx
         (mark-wave-status! base-dir idx "DEFERRED")
         (define exec (gsm-wave-executor))
         (when exec
           (wave-skip! exec idx))))
     (hook-amend (hasheq 'text (or (gsd-command-result-message result) "")))]
    ["/reset"
     (define result (cmd-reset))
     (events:emit-gsd-event! 'gsd.mode.changed (hasheq 'mode 'idle))
     (hook-amend (hasheq 'text (or (gsd-command-result-message result) "")))]
    [(? (lambda (c) (member c '("/wave-done" "/wd"))))
     (define wd-args (extract-cmd-args input-text))
     (define result (cmd-wave-done base-dir wd-args))
     (when (gsd-success? result)
       (define data (gsd-command-result-data result))
       (define wave-idx (and (hash? data) (hash-ref data 'wave #f)))
       (when wave-idx
         (events:emit-gsd-event! 'gsd.wave.completed (hasheq 'wave wave-idx))))
     (hook-amend (hasheq 'text (or (gsd-command-result-message result) "")))]
    ["/done"
     (define done-args (extract-cmd-args input-text))
     (define force? (and done-args (string-contains? done-args "--force")))
     (define result (cmd-done base-dir force?))
     (when (gsd-success? result)
       (define data (gsd-command-result-data result))
       (events:emit-gsd-event! 'gsd.plan.archived
                               (hasheq 'path
                                       (if (hash? data)
                                           (hash-ref data 'archive-path "")
                                           ""))))
     (hook-amend (hasheq 'text (or (gsd-command-result-message result) "")))]
    [_ (handle-artifact-command cmd input-text base-dir payload)]))

;; ============================================================
;; /go command handler
;; ============================================================

(define (handle-go-command base-dir input-text)
  (define plan-content (read-planning-artifact base-dir "PLAN"))
  (match plan-content
    [#f (hook-amend (hasheq 'text "No PLAN found in .planning/. Use /plan <task> to create one."))]
    [_
     ;; v0.24.4: Use normalized pipeline (parse → normalize → validate → executor)
     ;; v0.21.1: Try loading from wave doc index first, fall back to inline parse
     (define plan-from-index (load-plan-from-index base-dir))
     (define plan
       (or plan-from-index
           (let ([waves (parse-waves-from-markdown plan-content)])
             (gsd-plan waves "" '() '()))))
     ;; Emit gsd.plan.parsed event
     (events:emit-gsd-event! 'gsd.plan.parsed
                             (hasheq 'wave-count (length (gsd-plan-waves plan))))
     ;; Step 1: Normalize plan to canonical IR
     (define norm-result (normalize-plan plan))
     (match norm-result
       [(? string?)
        ;; Normalization failed (structural error)
        (hook-amend (hasheq 'text
                            (string-append "Plan normalization failed:
"
                                           norm-result
                                           "

Fix the plan before using /go.")))]
       [_
        ;; Emit gsd.plan.normalized event
        (events:emit-gsd-event! 'gsd.plan.normalized
                                (hasheq 'wave-count (length (gsd-normalized-plan-waves norm-result))))
        ;; Step 2: Validate normalized plan
        (define validation (validate-normalized-plan norm-result))
        (define validated-plan? (gsd-validated-plan? validation))
        ;; Emit gsd.plan.validated event
        (events:emit-gsd-event! 'gsd.plan.validated
                                (hasheq 'valid? validated-plan?
                                        'error-count
                                        (if validated-plan?
                                            0
                                            (length (validation-errors validation)))
                                        'warning-count
                                        (if validated-plan?
                                            0
                                            (length (validation-warnings validation)))))
        (match validated-plan?
          [#f
           (define report (format-validation-report validation))
           (hook-amend (hasheq 'text
                               (string-append "Plan validation failed:
"
                                              report
                                              "

Fix the plan before using /go.")))]
          [_
           ;; v0.24.6: Use with-gsd-transaction for snapshot/restore
           (define-values (executor wave-indices)
             (with-gsd-transaction
              "go"
              (lambda ()
                (set-gsd-mode! 'executing)
                (events:emit-gsd-event! 'gsd.mode.changed (hasheq 'mode 'executing))
                (set-current-max-old-text-len! 1200)
                (define wis
                  (for/list ([w (gsd-plan-waves plan)])
                    (gsd-wave-index w)))
                (when (not (null? wis))
                  (set-total-waves! (add1 (apply max wis))))
                (set-current-wave-index! 0)
                (define exec (make-wave-executor-from-validated validation))
                (gsm-set-wave-executor! exec)
                (values exec wis))
              (lambda (e snap)
                (events:emit-gsd-event!
                 'gsd.mode.changed
                 (hasheq 'reason "transaction-rollback" 'error (exn-message e))))))
           ;; Transaction returns gsd-err on failure, or the thunk values on success
           (match executor
             [(? gsd-command-result?)
              ;; Transaction failed — executor is actually gsd-err result
              (hook-amend (hasheq 'text (gsd-command-result-message executor)))]
             [_
              (define state-content (read-planning-artifact base-dir "STATE"))
              (define state-note
                (if state-content
                    (format "
Current state:
~a
" state-content)
                    ""))
              (define wave-arg
                (let* ([trimmed (string-trim input-text)]
                       [parts (string-split trimmed)])
                  (if (>= (length parts) 2)
                      (let ([maybe-num (string-trim (string-join (cdr parts) " "))])
                        (if (and (> (string-length maybe-num) 0)
                                 (regexp-match? #rx"^[0-9]+$" maybe-num))
                            (format "
Start with wave ~a." maybe-num)
                            ""))
                      "")))
              (define plan-text-for-prompt
                (if plan-from-index
                    (string-append plan-content "

" (wave-docs-summary plan-from-index))
                    plan-content))
              (define exec-prompt (executing-prompt plan executor))
              (define augmented-text
                (string-append planning-implement-prompt
                               exec-prompt
                               "
Plan:
"
                               plan-text-for-prompt
                               "
"
                               state-note
                               wave-arg))
              (hook-amend (hasheq 'new-session
                                  augmented-text
                                  'text
                                  (format "Implementing plan~a..." wave-arg)))])])])]))

;; ============================================================
;; /gsd status handler
;; ============================================================

(define (handle-gsd-status)
  (define mode (gsd-mode))
  (define tw (total-waves))
  (define cw (completed-waves))
  (define parts
    (list (format "Mode: ~a" (or mode "inactive"))
          (if (> tw 0)
              (format "Waves: ~a/~a complete" (set-count cw) tw)
              "Waves: not set")))
  (hook-amend (hasheq 'text (string-join parts "
"))))

;; ============================================================
;; Artifact display and /plan <text> handler
;; ============================================================

(define (handle-artifact-command cmd input-text base-dir payload)
  (define artifact
    (match cmd
      [(? (lambda (c) (member c '("/plan" "/p")))) "PLAN"]
      [(? (lambda (c) (member c '("/state" "/s")))) "STATE"]
      [(? (lambda (c) (member c '("/handoff" "/ho")))) "HANDOFF"]
      [_ #f]))
  (match artifact
    [#f (hook-pass payload)]
    [_
     (define args-text
       (let* ([trimmed (string-trim input-text)]
              [parts (and (> (string-length trimmed) 0)
                          (char=? (string-ref trimmed 0) #\/)
                          (string-split trimmed))])
         (and (pair? parts)
              (let ([rest (string-trim (substring input-text (string-length (car parts))))])
                (and (> (string-length rest) 0) rest)))))
     (match (and (member cmd '("/plan" "/p")) args-text)
       ;; /plan <text> → submit as planning prompt
       [(? values)
        (define saved-bus (gsd-event-bus)) ;; Preserve event bus across reset
        (define saved-dir (pinned-planning-dir)) ;; Preserve pinned dir
        (reset-all-gsd-state!) ;; Clean state for fresh plan (F1 fix)
        (when saved-bus
          (set-gsd-event-bus! saved-bus))
        (when saved-dir
          (set-pinned-planning-dir! saved-dir))
        (set-gsd-mode! 'planning)
        (events:emit-gsd-event! 'gsd.mode.changed (hasheq 'mode 'planning))
        (set-current-max-old-text-len! 500)
        ;; Auto-create STATE.md if missing (#2164)
        (ensure-state-md! base-dir)
        (define existing-plan (read-planning-artifact base-dir "PLAN"))
        (define stale-warning
          (if existing-plan
              "
NOTE: An existing PLAN.md was found. OVERWRITE it completely with the new plan. Do NOT keep or merge old content.
"
              ""))
        (define augmented-text (string-append (planning-prompt args-text) stale-warning))
        (hook-amend (hasheq 'submit augmented-text 'text (format "Planning: ~a" args-text)))]
       [else
        ;; Display artifact content
        (define content (read-planning-artifact base-dir artifact))
        (define text
          (match content
            [#f (format "No ~a found in .planning/" artifact)]
            [(? hash?) (jsexpr->string content)]
            [_ content]))
        (hook-amend (hasheq 'text text))])]))
