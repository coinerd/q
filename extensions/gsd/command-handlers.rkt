#lang racket/base

;; extensions/gsd/command-handlers.rkt -- GSD slash-command handlers
;; STABILITY: stable
;;
;; Extracted from extensions/gsd-planning.rkt (v0.34.6 W0b -- A-02 decomposition).

(require racket/contract
         racket/match
         racket/string
         racket/set
         json
         "../define-extension.rkt"
         "../ext-commands.rkt"
         "../hooks.rkt"
         "../tool-api.rkt"
         "../gsd-planning/command-normalization.rkt"
         "../gsd/command-parser.rkt"
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
                  with-gsd-transaction
                  reset-all-gsd-state!)
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
         (only-in "event-structs.rkt"
                  make-gsd-mode-changed-event
                  make-gsd-wave-completed-event
                  make-gsd-plan-parsed-event
                  make-gsd-plan-validated-event
                  make-gsd-plan-normalized-event
                  make-gsd-plan-archived-event)
         (only-in "../gsd/session-state.rkt"
                  set-gsd-state!
                  with-gsd-lock
                  current-pinned-dir
                  set-pinned-dir!
                  set-edit-limit!
                  current-gsd-event-bus
                  set-gsd-event-bus!))

(provide (contract-out [register-gsd-commands (-> any/c any/c)]
                       [handle-execute-command (-> any/c any/c)]
                       [handle-go-command (-> any/c any/c any/c)]
                       [handle-gsd-status (-> any/c)]
                       [handle-artifact-command (-> any/c any/c any/c any/c any/c)]
                       [dispatch-gsd-command (-> any/c any/c any/c (values symbol? any/c))]))

;; ============================================================
;; Command registration
;; ============================================================

;; Legacy mode wrappers (DEBT-01: migrated from gsd-planning-state.rkt)
(define (gsd-mode)
  (let ([s (gsm-current)])
    (cond
      [(eq? s 'idle) #f]
      [(eq? s 'exploring) 'planning]
      [else s])))

(define (gsd-mode? v)
  (eq? (gsd-mode) v))

(define (set-gsd-mode! v)
  (cond
    [(not v) (gsm-reset!)]
    [(eq? v 'planning) (gsm-transition-to! 'exploring)]
    [(eq? v 'plan-written) (gsm-transition-to! 'plan-written)]
    [(eq? v 'executing) (gsm-transition-to! 'executing)]
    [else (gsm-transition! v)]))

;; R6: Iterate gsd-command-specs for registration (single source of truth)
(define (register-gsd-commands ctx)
  (for ([spec (in-list gsd-command-specs)])
    (ext-register-command! ctx
                           (gsd-command-spec-canonical spec)
                           (gsd-command-spec-description spec)
                           'general
                           '()
                           (map (lambda (a) (substring a 1)) (gsd-command-spec-aliases spec))))
  (hook-pass #f))

;; ============================================================
;; Command dispatch
;; ============================================================

;; dispatch-gsd-command : parsed-command? string? path? -> (values symbol? any/c)
;; Pure routing: maps parsed command to action tag + payload data.
;; ZERO cmd-* calls -- side effects handled by caller (handle-execute-command).
(define (dispatch-gsd-command parsed input-text base-dir)
  (cond
    [(gsd-cmd-go? parsed) (values 'go (list base-dir input-text))]
    [(gsd-cmd-status? parsed) (values 'status #f)]
    [(gsd-cmd-replan? parsed) (values 'replan parsed)]
    [(gsd-cmd-skip? parsed) (values 'skip parsed)]
    [(gsd-cmd-reset? parsed) (values 'reset parsed)]
    [(gsd-cmd-wave-done? parsed) (values 'wave-done parsed)]
    [(gsd-cmd-done? parsed) (values 'done parsed)]
    [(gsd-cmd-plan? parsed)
     (define plan-text (gsd-cmd-plan-plan-text parsed))
     (if plan-text
         (values 'plan-submit plan-text)
         (values 'artifact parsed))]
    [(gsd-cmd-artifact? parsed) (values 'artifact parsed)]
    [else (values 'artifact parsed)]))

;; R-04/R-16: Refactored to parse->dispatch.
;; Pure parsing is in command-parser.rkt; dispatch-gsd-command routes;
;; this function handles side effects.
(define (handle-execute-command payload)
  (define cmd (hash-ref payload 'command #f))
  (define input-text (hash-ref payload 'input ""))
  (define base-dir (or (current-pinned-dir) (current-directory)))
  (define parsed (parse-gsd-command cmd input-text))
  (define-values (action result) (dispatch-gsd-command parsed input-text base-dir))
  (case action
    [(go) (apply handle-go-command result)]
    [(status) (handle-gsd-status)]
    [(replan)
     (define cmd-result (cmd-replan))
     (events:emit-gsd-event!
      'gsd.mode.changed
      (make-gsd-mode-changed-event #:session-id "" #:turn-id 0 #:mode 'exploring))
     (hook-amend (hasheq 'text (or (gsd-command-result-message cmd-result) "")))]
    [(skip)
     (define args-text (gsd-cmd-skip-skip-arg parsed))
     (define skip-result (cmd-skip args-text))
     (when (and (gsd-success? skip-result) base-dir)
       (define idx (and (string->number (string-trim args-text))))
       (when idx
         (mark-wave-status! base-dir idx "DEFERRED")
         (define exec (gsm-wave-executor))
         (when exec
           (wave-skip! exec idx))))
     (hook-amend (hasheq 'text (or (gsd-command-result-message skip-result) "")))]
    [(reset)
     (define cmd-result (cmd-reset))
     (events:emit-gsd-event! 'gsd.mode.changed
                             (make-gsd-mode-changed-event #:session-id "" #:turn-id 0 #:mode 'idle))
     (hook-amend (hasheq 'text (or (gsd-command-result-message cmd-result) "")))]
    [(wave-done)
     (define wd-args (gsd-cmd-wave-done-wave-arg parsed))
     (define cmd-result (cmd-wave-done base-dir wd-args))
     (when (gsd-success? cmd-result)
       (define data (gsd-command-result-data cmd-result))
       (define wave-idx (and (hash? data) (hash-ref data 'wave #f)))
       (when wave-idx
         (events:emit-gsd-event!
          'gsd.wave.completed
          (make-gsd-wave-completed-event #:session-id "" #:turn-id 0 #:wave wave-idx))))
     ;; SAL-06: Set gsd-pin so progress messages survive context assembly
     (hook-amend (hasheq 'text (or (gsd-command-result-message cmd-result) "") 'gsd-pin #t))]
    [(done)
     (define force? (gsd-cmd-done-force? parsed))
     (define cmd-result (cmd-done base-dir force?))
     (when (gsd-success? cmd-result)
       (define data (gsd-command-result-data cmd-result))
       (events:emit-gsd-event! 'gsd.plan.archived
                               (make-gsd-plan-archived-event #:session-id ""
                                                             #:turn-id 0
                                                             #:path
                                                             (if (hash? data)
                                                                 (hash-ref data 'archive-path "")
                                                                 ""))))
     ;; SAL-06: Set gsd-pin on plan-archive (done) messages
     (hook-amend (hasheq 'text (or (gsd-command-result-message cmd-result) "") 'gsd-pin #t))]
    [(plan-submit) (handle-plan-submit result base-dir input-text parsed)]
    [(artifact) (handle-artifact-command cmd input-text base-dir payload)]
    [else (handle-artifact-command cmd input-text base-dir payload)]))

;; ============================================================
;; /go decomposition helpers (S5-F1)
;; ============================================================

;; validate-plan-for-go : path? -> (or/c (list/c 'ok gsd-plan? gsd-normalized-plan? gsd-validated-plan?) (list/c 'error string?))
;; Load, normalize, and validate the plan. Returns 'ok with validated data or 'error with message.
(define (validate-plan-for-go base-dir)
  (define plan-content (read-planning-artifact base-dir "PLAN"))
  (match plan-content
    [#f (list 'error "No PLAN found in .planning/. Use /plan <task> to create one.")]
    [_
     (define plan-from-index (load-plan-from-index base-dir))
     (define plan
       (or plan-from-index
           (let ([waves (parse-waves-from-markdown plan-content)]) (gsd-plan waves "" '() '()))))
     (events:emit-gsd-event! 'gsd.plan.parsed
                             (make-gsd-plan-parsed-event #:session-id ""
                                                         #:turn-id 0
                                                         #:wave-count (length (gsd-plan-waves plan))))
     (define norm-result (normalize-plan plan))
     (match norm-result
       [(? string?)
        (list 'error
              (string-append "Plan normalization failed:\n"
                             norm-result
                             "\n\nFix the plan before using /go."))]
       [_
        (events:emit-gsd-event! 'gsd.plan.normalized
                                (make-gsd-plan-normalized-event
                                 #:session-id ""
                                 #:turn-id 0
                                 #:wave-count (length (gsd-normalized-plan-waves norm-result))))
        (define validation (validate-normalized-plan norm-result))
        (define validated-plan? (gsd-validated-plan? validation))
        (events:emit-gsd-event! 'gsd.plan.validated
                                (make-gsd-plan-validated-event
                                 #:session-id ""
                                 #:turn-id 0
                                 #:wave-count 0
                                 #:valid? validated-plan?
                                 #:error-count (if validated-plan?
                                                   0
                                                   (length (validation-errors validation)))
                                 #:warning-count (if validated-plan?
                                                     0
                                                     (length (validation-warnings validation)))))
        (match validated-plan?
          [#f
           (list 'error
                 (string-append "Plan validation failed:\n"
                                (format-validation-report validation)
                                "\n\nFix the plan before using /go."))]
          [_ (list 'ok plan norm-result validation)])])]))

;; launch-wave-executor : gsd-validated-plan? gsd-plan? path? -> (or/c (list/c 'ok any/c (listof exact-nonnegative-integer?)) (list/c 'error string?))
;; Configure state machine and create wave executor inside a transaction.
(define (launch-wave-executor validation plan base-dir)
  (define result
    (with-gsd-transaction
     "go"
     (lambda ()
       (set-gsd-mode! 'executing)
       (events:emit-gsd-event!
        'gsd.mode.changed
        (make-gsd-mode-changed-event #:session-id "" #:turn-id 0 #:mode 'executing))
       (set-edit-limit! 1200)
       (define wis
         (for/list ([w (gsd-plan-waves plan)])
           (gsd-wave-index w)))
       (when (not (null? wis))
         (gsm-set-total-waves! (add1 (apply max wis))))
       (gsm-set-current-wave! 0)
       (define exec (make-wave-executor-from-validated validation))
       (gsm-set-wave-executor! exec)
       (list exec wis))
     (lambda (e snap)
       (events:emit-gsd-event! 'gsd.mode.changed
                               (make-gsd-mode-changed-event #:session-id ""
                                                            #:turn-id 0
                                                            #:mode (gsm-current)
                                                            #:reason "transaction-rollback"
                                                            #:error (exn-message e))))))
  (cond
    [(gsd-failed? result) (list 'error (gsd-command-result-message result))]
    [else
     (match-define (list executor wave-indices) result)
     (list 'ok executor wave-indices)]))

;; build-go-prompt : path? string? (or/c gsd-plan? #f) any/c string? gsd-plan? -> (values string? string?)
;; Assemble augmented prompt text and display text for /go.
(define (build-go-prompt base-dir plan-content plan-from-index executor wave-arg plan)
  (define state-content (read-planning-artifact base-dir "STATE"))
  (define state-note
    (if state-content
        (format "\nCurrent state:\n~a\n" state-content)
        ""))
  (define plan-text-for-prompt
    (if plan-from-index
        (string-append plan-content "\n\n" (wave-docs-summary plan-from-index))
        plan-content))
  (define exec-prompt (executing-prompt plan executor))
  (define augmented-text
    (string-append planning-implement-prompt
                   exec-prompt
                   "\nPlan:\n"
                   plan-text-for-prompt
                   "\n"
                   state-note
                   wave-arg))
  (define display-text (format "Implementing plan~a..." wave-arg))
  (values augmented-text display-text))

;; ============================================================
;; /go command handler
;; ============================================================

(define (handle-go-command base-dir input-text)
  (define validation-result (validate-plan-for-go base-dir))
  (match validation-result
    [(list 'error msg) (hook-amend (hasheq 'text msg))]
    [(list 'ok plan norm-result validation)
     (define launch-result (launch-wave-executor validation plan base-dir))
     (match launch-result
       [(list 'error msg) (hook-amend (hasheq 'text msg))]
       [(list 'ok executor _)
        (define wave-arg
          (let* ([trimmed (string-trim input-text)]
                 [parts (string-split trimmed)])
            (if (>= (length parts) 2)
                (let ([maybe-num (string-trim (string-join (cdr parts) " "))])
                  (if (and (> (string-length maybe-num) 0) (regexp-match? #rx"^[0-9]+$" maybe-num))
                      (format "\nStart with wave ~a." maybe-num)
                      ""))
                "")))
        (define plan-content (read-planning-artifact base-dir "PLAN"))
        (define plan-from-index (load-plan-from-index base-dir))
        (define-values (augmented-text display-text)
          (build-go-prompt base-dir plan-content plan-from-index executor wave-arg plan))
        (hook-amend (hasheq 'new-session augmented-text 'text display-text))])]))

;; ============================================================
;; /gsd status handler
;; ============================================================

(define (handle-gsd-status)
  (define mode (gsd-mode))
  (define tw (gsm-total-waves))
  (define cw (gsm-completed-waves))
  (define parts
    (list (format "Mode: ~a" (or mode "inactive"))
          (if (> tw 0)
              (format "Waves: ~a/~a complete" (set-count cw) tw)
              "Waves: not set")))
  (hook-amend (hasheq 'text (string-join parts "\n"))))

;; ============================================================
;; Artifact display and /plan <text> handler
;; ============================================================

;; R-04/R-16: Focused handler for /plan <text> submit
(define (handle-plan-submit plan-text base-dir input-text parsed)
  (define saved-bus (current-gsd-event-bus)) ;; Preserve event bus across reset
  (define saved-dir (current-pinned-dir)) ;; Preserve pinned dir
  (reset-all-gsd-state!) ;; Clean state for fresh plan (F1 fix)
  (when saved-bus
    (set-gsd-event-bus! saved-bus))
  (when saved-dir
    (set-pinned-dir! saved-dir))
  (set-gsd-mode! 'planning)
  (events:emit-gsd-event! 'gsd.mode.changed
                          (make-gsd-mode-changed-event #:session-id "" #:turn-id 0 #:mode 'planning))
  (set-edit-limit! 500)
  ;; Auto-create STATE.md if missing (#2164)
  (ensure-state-md! base-dir)
  (define existing-plan (read-planning-artifact base-dir "PLAN"))
  (define stale-warning
    (if existing-plan
        "\nNOTE: An existing PLAN.md was found. OVERWRITE it completely with the new plan. Do NOT keep or merge old content.\n"
        ""))
  (define augmented-text (string-append (planning-prompt plan-text) stale-warning))
  (hook-amend (hasheq 'submit augmented-text 'text (format "Planning: ~a" plan-text))))

(define (handle-artifact-command cmd input-text base-dir payload)
  (define artifact
    (match cmd
      [(? (lambda (c) (member c (aliases-for "/plan")))) "PLAN"]
      [(? (lambda (c) (member c (aliases-for "/state")))) "STATE"]
      [(? (lambda (c) (member c (aliases-for "/handoff")))) "HANDOFF"]
      [_ #f]))
  (match artifact
    [#f (hook-pass payload)]
    [_
     ;; Display artifact content
     (define content (read-planning-artifact base-dir artifact))
     (define text
       (match content
         [#f (format "No ~a found in .planning/" artifact)]
         [(? hash?) (jsexpr->string content)]
         [_ content]))
     (hook-amend (hasheq 'text text))]))
