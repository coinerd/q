#lang racket/base

;; extensions/gsd/command-handlers.rkt -- GSD slash-command handlers
;; STABILITY: stable
;;
;; Extracted from extensions/gsd-planning.rkt (v0.34.6 W0b -- A-02 decomposition).

(require racket/contract
         racket/match
         racket/string
         racket/format
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
                  [ctx-emit-gsd-event! events:ctx-emit-gsd-event!]
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
                  set-gsd-event-bus!
                  current-gsd-ctx
                  current-gsd-session-id)
         (only-in "plan-context-builder.rkt" build-enriched-plan-ctx find-git-root-dir)
         (only-in racket/path find-relative-path)
         (only-in "../../agent/verification/verifier-gate.rkt" execute-verification-gate)
         (only-in "../../agent/verification/verifier-core.rkt" current-verifier-enabled)
         racket/file
         "campaign-state.rkt"
         "campaign-repository.rkt"
         "go-orchestrator.rkt"
         "delivery-verifier.rkt"
         (only-in "../../runtime/settings-core.rkt" load-global-settings)
         (only-in "policy.rkt"
                  current-gsd-wave-timeout-seconds
                  current-gsd-wave-max-iterations
                  current-gsd-max-consecutive-tool-calls
                  current-gsd-wave-failure-context
                  current-gsd-wave-no-change-retries)
         (only-in "../../agent/state.rkt" current-empty-response-nudge))

(provide (contract-out
          [register-gsd-commands (-> extension-ctx? hook-result?)]
          [handle-execute-command (-> hash? hook-result?)]
          [handle-go-command (-> (or/c path-string? #f) string? hook-result?)]
          [build-single-wave-prompt (-> path-string? gsd-plan? exact-nonnegative-integer? string?)]
          [handle-gsd-status (-> hook-result?)]
          [handle-artifact-command (-> string? string? (or/c path-string? #f) hash? hook-result?)]
          [dispatch-gsd-command
           (-> (or/c parsed-gsd-command? #f) string? (or/c path-string? #f) (values symbol? any/c))])
         extract-task-summary
         extract-last-failure)

;; ============================================================
;; Command registration
;; ============================================================

;; Legacy mode wrappers (DEBT-01: migrated from gsd-planning-state.rkt)
(define (gsd-mode)
  (match (gsm-ctx-current (current-gsd-ctx))
    ['idle #f]
    ['exploring 'planning]
    [s s]))

(define (gsd-mode? v)
  (eq? (gsd-mode) v))

(define (set-gsd-mode! v)
  (match v
    [#f (gsm-reset!)]
    ['planning (gsm-transition-to! 'exploring)]
    ['plan-written (gsm-transition-to! 'plan-written)]
    ['executing (gsm-transition-to! 'executing)]
    [_ (gsm-transition! v)]))

;; Helper: emit a mode-changed event with standard boilerplate
(define (emit-mode-change! mode #:reason [reason #f] #:error [err #f])
  (events:ctx-emit-gsd-event!
   (current-gsd-ctx)
   'gsd.mode.changed
   (if reason
       (make-gsd-mode-changed-event #:session-id (current-gsd-session-id)
                                    #:turn-id 0
                                    #:mode mode
                                    #:reason reason
                                    #:error (or err ""))
       (make-gsd-mode-changed-event #:session-id (current-gsd-session-id) #:turn-id 0 #:mode mode))))

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
     (emit-mode-change! 'exploring)
     (hook-amend (hasheq 'text (or (gsd-command-result-message cmd-result) "")))]
    [(skip)
     (define args-text (gsd-cmd-skip-skip-arg parsed))
     (define skip-result (cmd-skip args-text))
     (when (and (gsd-success? skip-result) base-dir)
       (define idx (and (string->number (string-trim args-text))))
       (when idx
         (mark-wave-status! base-dir idx "DEFERRED")
         (define exec (gsm-ctx-wave-executor (current-gsd-ctx)))
         (when exec
           (wave-skip! exec idx))))
     (hook-amend (hasheq 'text (or (gsd-command-result-message skip-result) "")))]
    [(reset)
     (define cmd-result (cmd-reset))
     (emit-mode-change! 'idle)
     (hook-amend (hasheq 'text (or (gsd-command-result-message cmd-result) "")))]
    [(wave-done)
     (define wd-args (gsd-cmd-wave-done-wave-arg parsed))
     (define cmd-result (cmd-wave-done base-dir wd-args))
     ;; v0.99.6 C1: If verifier enabled, run verification gate after wave-done
     (define gate-result
       (if (and (gsd-success? cmd-result) (current-verifier-enabled))
           (let ([ctx (current-gsd-ctx)])
             ;; v0.99.23 B-1/B-2: Enrich plan-ctx with real wave data
             ;; Previously all fields were empty strings/lists, making
             ;; §6.1 skip heuristic and §6.2 dynamic threshold inert.
             (define wd-args-num
               (let ([n (and wd-args (string->number wd-args))])
                 (if (and n (exact-nonnegative-integer? n)) n 0)))
             (define plan (load-plan-from-index base-dir))
             (define plan-ctx (build-enriched-plan-ctx base-dir plan wd-args-num))
             (execute-verification-gate ctx plan-ctx))
           'approved))
     ;; Only emit wave.completed if verification approved (or was not run)
     (when (and (gsd-success? cmd-result) (eq? gate-result 'approved))
       (define data (gsd-command-result-data cmd-result))
       (define wave-idx (and (hash? data) (hash-ref data 'wave #f)))
       (when wave-idx
         (events:ctx-emit-gsd-event! (current-gsd-ctx)
                                     'gsd.wave.completed
                                     (make-gsd-wave-completed-event #:session-id
                                                                    (current-gsd-session-id)
                                                                    #:turn-id 0
                                                                    #:wave wave-idx))))
     ;; Build message: include verification result if relevant
     (define final-msg
       (cond
         [(eq? gate-result 'rejected) "Verification REJECTED -- wave requires rework."]
         [(eq? gate-result 'escalated) "Verification ESCALATED -- human review required."]
         [else (or (gsd-command-result-message cmd-result) "")]))
     ;; SAL-06: Set gsd-pin so progress messages survive context assembly
     (hook-amend (hasheq 'text final-msg 'gsd-pin #t))]
    [(done)
     (define force? (gsd-cmd-done-force? parsed))
     (define cmd-result (cmd-done base-dir force?))
     (when (gsd-success? cmd-result)
       (define data (gsd-command-result-data cmd-result))
       (events:ctx-emit-gsd-event! (current-gsd-ctx)
                                   'gsd.plan.archived
                                   (make-gsd-plan-archived-event #:session-id (current-gsd-session-id)
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
     (events:ctx-emit-gsd-event! (current-gsd-ctx)
                                 'gsd.plan.parsed
                                 (make-gsd-plan-parsed-event #:session-id (current-gsd-session-id)
                                                             #:turn-id 0
                                                             #:wave-count
                                                             (length (gsd-plan-waves plan))))
     (define norm-result (normalize-plan plan))
     (match norm-result
       [(? string?)
        (list 'error
              (string-append "Plan normalization failed:\n"
                             norm-result
                             "\n\nFix the plan before using /go."))]
       [_
        (events:ctx-emit-gsd-event! (current-gsd-ctx)
                                    'gsd.plan.normalized
                                    (make-gsd-plan-normalized-event
                                     #:session-id (current-gsd-session-id)
                                     #:turn-id 0
                                     #:wave-count (length (gsd-normalized-plan-waves norm-result))))
        (define validation (validate-normalized-plan norm-result))
        (define validated-plan? (gsd-validated-plan? validation))
        (events:ctx-emit-gsd-event! (current-gsd-ctx)
                                    'gsd.plan.validated
                                    (make-gsd-plan-validated-event
                                     #:session-id (current-gsd-session-id)
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
    (with-gsd-transaction "go"
                          (lambda ()
                            (set-gsd-mode! 'executing)
                            (emit-mode-change! 'executing)
                            (set-edit-limit! 2000)
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
                            (emit-mode-change! (gsm-ctx-current (current-gsd-ctx))
                                               #:reason "transaction-rollback"
                                               #:error (exn-message e)))))
  (cond
    [(gsd-failed? result) (list 'error (gsd-command-result-message result))]
    [else
     (match-define (list executor wave-indices) result)
     (list 'ok executor wave-indices)]))

;; Extract a concise task summary from the plan for persistent reminder
(define (extract-task-summary plan)
  (define waves (gsd-plan-waves plan))
  (define wave-titles
    (for/list ([w waves])
      (format "W~a: ~a" (gsd-wave-index w) (gsd-wave-title w))))
  (if (null? wave-titles)
      ""
      (format
       "\n## Task Summary (DO NOT FORGET)\nYou are implementing: ~a\nStay focused on this task.\n"
       (string-join wave-titles ", "))))

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
  (define task-summary (extract-task-summary plan))
  (define augmented-text
    (string-append planning-implement-prompt
                   exec-prompt
                   task-summary
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

;; Trailing numeric token of the /go input (pure; see command-parser.rkt
;; command-wave-intent). Kept as a thin delegate so the executor consumes the
;; pure intent boundary instead of re-parsing.
(define (requested-wave-index input-text)
  (command-wave-intent input-text))

(define (state-for-wave state-text wave-idx)
  ;; Only a canonical table row whose first cell is exactly WN may cross the
  ;; isolation boundary. Free-form dependency lines are excluded.
  (define current-row-rx (regexp (format "^\\| *W~a *\\|" wave-idx)))
  (string-join (for/list ([line (in-list (string-split state-text "\n"))]
                          #:when (regexp-match? current-row-rx line))
                 line)
               "\n"))

;; BUG-0027 (W4): executors ran git at the project base and burned 3-6 calls
;; on "fatal: Kein Git-Repository" before rediscovering q/ (observed in three
;; sessions 2026-08-24/25; contributed to the v1.00.16 W3 budget timeout).
;; The Working Directory Contract must therefore pin the git root, resolved
;; at runtime with the delivery verifier's git-root-for logic (git-root-for
;; delegates to find-git-root-dir, imported here so there is exactly one
;; resolution path). The "run all git commands" correction line is emitted
;; only when base-dir is NOT the git root — nothing to correct otherwise.
(define (git-root-contract-lines base-dir)
  (define git-root (find-git-root-dir base-dir))
  (cond
    [(not git-root) (list (format "- Git root: none found (no .git at or above ~a)\n" base-dir))]
    [else
     (define root-s (simplify-path (path->complete-path git-root)))
     (define base-s (simplify-path (path->complete-path base-dir)))
     (if (equal? root-s base-s)
         (list (format "- Git root: ~a\n" git-root))
         (let ([rel (path->string (find-relative-path base-s root-s))])
           (list (format "- Git root: ~a\n" git-root)
                 (format "- run all git commands against the git root (`cd ~a` or `git -C ~a`)\n"
                         rel
                         rel))))]))

(define (build-single-wave-prompt base-dir plan wave-idx)
  (define wave (plan-wave-ref plan wave-idx))
  (unless wave
    (error 'build-single-wave-prompt "wave ~a is not present in the validated plan" wave-idx))
  (define plan-content (read-planning-artifact base-dir "PLAN"))
  (define entry
    (for/first ([candidate (in-list (parse-plan-index plan-content))]
                #:when (= (wave-index-entry-idx candidate) wave-idx))
      candidate))
  (define wave-doc (and entry (read-wave-doc base-dir wave-idx (wave-index-entry-slug entry))))
  (define wave-details
    (if wave-doc
        (hash-ref wave-doc 'content "")
        (string-append (format "Title: ~a\n" (gsd-wave-title wave))
                       (if (string=? (gsd-wave-root-cause wave) "")
                           ""
                           (format "Root cause: ~a\n" (gsd-wave-root-cause wave)))
                       (if (null? (gsd-wave-files wave))
                           ""
                           (format "Files: ~a\n" (string-join (gsd-wave-files wave) ", ")))
                       (if (string=? (gsd-wave-verify wave) "")
                           ""
                           (format "Required verification: ~a\n" (gsd-wave-verify wave))))))
  (define state-content (state-for-wave (or (read-planning-artifact base-dir "STATE") "") wave-idx))
  ;; S2b/D5 (#9359): pin the executor's working-directory contract so the
  ;; agent never confuses the repo root with the q/ source subdir (attempt-3
  ;; and attempt-5 burned tool budget on "Wrong working dir" / dropped
  ;; path segments like q/tui/key-dispatch.rkt vs
  ;; q/tui/keybindings/key-dispatch.rkt). Also validate each File: target
  ;; against base-dir up front and report existence so the executor can
  ;; distinguish a genuinely missing file from a path-resolution mistake.
  (define repo-root (or (current-pinned-dir) (current-directory)))
  (define (target-exists? t)
    (define p
      (if (absolute-path? t)
          t
          (build-path base-dir t)))
    (file-exists? p))
  (define file-contract
    (string-append
     (format "## Working Directory Contract\n")
     (format "- Project root (base-dir): ~a\n" base-dir)
     (format "- Process working directory: ~a\n" repo-root)
     ;; BUG-0027 (W4): pin the git root next to the contract lines above so
     ;; executors run git against the right checkout from the first call.
     (apply string-append (git-root-contract-lines base-dir))
     (format
      "- Source subdir is 'q' under the project root. Resolve 'File:' paths relative to the project root unless they are absolute.\n")
     (if (null? (gsd-wave-files wave))
         "- No file targets declared.\n"
         (string-append
          "- Declared file targets (existence checked against project root):\n"
          (string-join (for/list ([t (gsd-wave-files wave)])
                         (format "  * ~a [~a]\n" t (if (target-exists? t) "exists" "MISSING")))
                       "")
          "\n"))))
  (define last-failure (extract-last-failure wave-details))
  ;; BUG-0041 (W4): bake the wave-doc lint verdict into the executor prompt
  ;; so degradation is explicit up front (missing Files/Verify/Done sections,
  ;; non-canonical status header) instead of being discovered mid-wave as
  ;; degraded steering and guessed verify commands.
  (define lint-verdict
    (if (not wave-doc)
        ""
        (let ([violations (lint-wave-doc wave-doc)])
          (if (null? violations)
              ""
              (string-append
               "\n## Wave-doc lint verdict (BUG-0041)\n"
               (format-wave-doc-lint-warning wave-idx (hash-ref wave-doc 'path "") violations)
               "\n")))))
  (string-append
   planning-implement-prompt
   file-contract
   ;; BUG-0030 (action 1): checkpoint cadence contract so mid-wave infra
   ;; stops leave committed, discoverable progress on the delivery branch.
   (apply string-append (checkpoint-contract-lines))
   "# Runtime-Enforced Single-Wave Execution\n\n"
   (format "Execute ONLY wave W~a in this session. Do not start or inspect later waves.\n" wave-idx)
   "Return normally only after implementation and required verification complete.\n"
   "Do not call /wave-done: the coordinator is the only component allowed to commit wave status.\n"
   "After you return, the coordinator verifies real delivery evidence: the expected branch is\n"
   "checked out, the wave's target files were changed, and the wave's verify command exits 0.\n"
   "A wave is marked DONE only when that evidence is present and verification passes; otherwise\n"
   "it is marked FAILED and the campaign stops for review.\n"
   (format (string-append "This wave has a bounded runtime budget: ~a seconds, ~a iterations, "
                          "and ~a consecutive tool-only turns. Budget exhaustion fails closed.\n\n")
           (current-gsd-wave-timeout-seconds)
           (current-gsd-wave-max-iterations)
           (current-gsd-max-consecutive-tool-calls))
   (if last-failure
       (string-append
        "## Previous Attempt Failed — Adapt\n\n"
        "A previous run of this wave FAILED delivery verification. The recorded reason is:\n\n"
        last-failure
        "\nYou MUST address this specific failure before completing the wave. Do not repeat the\n"
        "same approach that produced it; verify your delivery addresses the failure reason\n"
        "before signalling completion.\n\n")
       "")
   (format "## Wave W~a\n~a\n" wave-idx wave-details)
   lint-verdict
   (if (string=? state-content "")
       ""
       (format "\n## Current State\n~a\n" state-content))
   ;; v1.00.17 W3 (#9515): the go-orchestrator's bounded no-change retry sets
   ;; current-gsd-wave-failure-context around the retrying run; the builder
   ;; executes synchronously inside that parameterize extent, so the block is
   ;; baked into THIS prompt (verbatim verifier message + target file list +
   ;; the imperative "apply the first edit now").
   (let ([failure-context (current-gsd-wave-failure-context)])
     (if (and (string? failure-context) (non-empty-string? failure-context))
         (string-append failure-context "\n")
         ""))
   ;; v1.00.21 W5 (BUG-0029 action 2): the go-orchestrator parameterizes
   ;; current-gsd-wave-inherited-artifacts around the runner; the builder
   ;; runs inside that extent, so the PRIOR ARTIFACTS block (prior attempts'
   ;; branches/worktrees with terminal/merge status, bounded ~1 KB) is baked
   ;; into THIS prompt — successor executors stop guessing about leftover
   ;; branches like fix/delivery-verifier-annotations-retry-adapt.
   (let ([inherited (current-gsd-wave-inherited-artifacts)])
     (if (and (string? inherited) (non-empty-string? inherited))
         (string-append inherited "\n")
         ""))))

;; Extract the "## Last Failure" section (recorded by record-wave-failure! when
;; a previous attempt failed delivery verification) so a retry can adapt. The
;; wave doc content is embedded in the prompt, so the agent sees the reason.
;; Returns the section body (without the heading) or "".
(define (extract-last-failure wave-details)
  ;; Line-based extraction (robust across Racket regexp quirks): find the
  ;; "## Last Failure" heading and collect the following lines up to the
  ;; next top-level heading (##) or end of the document.
  (define lines (string-split wave-details "\n"))
  (define heading-idx
    (for/first ([i (in-naturals)]
                [l (in-list lines)]
                #:when (string=? (string-trim l) "## Last Failure"))
      i))
  (if (not heading-idx)
      ""
      (string-join (for/list ([l (in-list (list-tail lines (add1 heading-idx)))]
                              #:break (and (>= (string-length (string-trim l)) 2)
                                           (string-prefix? (string-trim l) "##")))
                     l)
                   "\n")))

;; v1.00.03: resolve the per-campaign wave budget at /go time.
;; Precedence: /go --wave-timeout=SECONDS flag > ~/.q/config.json
;; wave-timeout-seconds > current-gsd-wave-timeout-seconds (default 3600).
;; The resolved value travels on the campaign request because the campaign
;; runs in a separate thread where a parameterize here would not apply.
(define (resolve-wave-timeout-secs input-text)
  (or (command-wave-timeout-arg input-text)
      (let* ([cfg (load-global-settings)]
             [raw (hash-ref cfg 'wave-timeout-seconds #f)])
        (and (number? raw) (positive? raw) raw))
      (current-gsd-wave-timeout-seconds)))

;; BUG-0034 (W2): dual-source status consistency warnings.
;; Wave status lives twice (PLAN.md index row + wave-doc `Status:`
;; header). This turns any divergence into named, user-visible warnings
;; (one per wave, naming both files). ADVISORY ONLY: they never block
;; /go by themselves — selection already resolves through the documented
;; precedence (see resolve-status-precedence in wave-docs.rkt).
(define (status-divergence-warning-lines base-dir)
  (if (not base-dir)
      '()
      (for/list ([d (in-list (check-status-consistency base-dir))])
        (format-status-divergence-warning d))))

;; Append the warning block to a user-facing text (identity when silent).
(define (append-divergence-warnings text warnings)
  (if (null? warnings)
      text
      (string-append text "\n\n" (string-join warnings "\n"))))

(define (prepare-go-campaign base-dir input-text plan validation warnings)
  (with-handlers ([exn:fail:campaign-migration?
                   (lambda (e)
                     (hook-amend (hasheq 'text
                                         (format "Campaign migration failed closed: ~a"
                                                 (exn-message e)))))])
    (define rec (load-or-migrate-campaign! base-dir))
    ;; BUG-0041 (W4): record the lint verdict as durable campaign evidence at
    ;; creation (write-once .planning/campaigns/<plan-id>/lint-verdict.rktd;
    ;; best-effort — evidence storage must never block /go).
    (store-wave-doc-lint-verdict! base-dir (campaign-plan-id rec))
    (define next-wave (select-next-actionable-wave rec))
    (define requested (requested-wave-index input-text))
    (cond
      [(not next-wave) (hook-amend (hasheq 'text "Campaign has no actionable waves."))]
      [(and requested (not (assert-go-n rec requested)))
       (hook-amend
        (hasheq 'text
                (format "/go ~a rejected: earliest actionable wave is W~a." requested next-wave)))]
      [else
       (match (launch-wave-executor validation plan base-dir)
         [(list 'error msg) (hook-amend (hasheq 'text msg))]
         [(list 'ok _ _)
          (define gsd-ctx (current-gsd-ctx))
          (define effective-timeout (resolve-wave-timeout-secs input-text))
          ;; v1.00.17 W3 (#9514): role-anchor the wave-executor session. If a
          ;; turn ends reasoning-only, the runtime's empty-response retry
          ;; re-sends THIS re-anchor prompt (verbatim executor role + order to
          ;; continue) instead of the generic output nudge, so the model can
          ;; never reinterpret itself as an interactive assistant (the
          ;; v1.00.16 W3 attempt-2 failure mode).
          (define reanchor-wave (plan-wave-ref plan next-wave))
          (define reanchor
            (executor-reanchor-prompt
             (format "W~a" next-wave)
             (campaign-plan-id rec)
             (if reanchor-wave
                 (format "W~a: ~a" (gsd-wave-index reanchor-wave) (gsd-wave-title reanchor-wave))
                 "implement the wave")
             "(session start — no tool has run yet in this session)"))
          (parameterize ([current-empty-response-nudge reanchor])
            (define request
              (make-campaign-request
               base-dir
               rec
               (lambda (wave-idx)
                 (gsm-ctx-transition-to! gsd-ctx 'executing)
                 (build-single-wave-prompt base-dir plan wave-idx))
               (make-delivery-verifier base-dir plan (campaign-record-created-at rec))
               #:timeout-sec effective-timeout
               ;; v1.00.19 W3 (BUG-0031): the trailing `allow-stale` token
               ;; parsed out of /go <args> by command-parser.rkt flows onto
               ;; the request; execute-campaign-request! then bypasses the
               ;; version-freshness refusal and records stale-override: true.
               #:allow-stale? (command-allow-stale? input-text)))
            (hook-amend (hasheq 'campaign-token
                                (register-campaign-request! request)
                                'new-session
                                (build-single-wave-prompt base-dir plan next-wave)
                                'text
                                (append-divergence-warnings (format "Executing campaign from W~a..."
                                                                    next-wave)
                                                            warnings))))])])))

(define (handle-go-command base-dir input-text)
  ;; Report plan validation failures first. Repository identity becomes a hard
  ;; precondition only once there is a runnable campaign to isolate.
  (match (validate-plan-for-go base-dir)
    [(list 'error msg) (hook-amend (hasheq 'text msg))]
    [(list 'ok plan _ validation)
     (if (not (git-available? base-dir))
         (hook-amend (hasheq 'text
                             (format "/go blocked: no Git repository reachable from ~a." base-dir)))
         ;; BUG-0034 (W2): computed at /go validation time; advisory only —
         ;; divergences never block /go, they surface as named warnings.
         ;; BUG-0035 (W6): plan-format deprecation warnings (inline sections /
         ;; relaxed status-less index rows) join the same advisory block.
         ;; BUG-0041 (W4): wave-doc lint verdict joins the same advisory
         ;; block — WARN, never block; the durable copy is recorded on the
         ;; campaign record path by prepare-go-campaign below. Arrow-target
         ;; ↔ doc-slug mismatches ride the v1.00.20 W2 consistency checker
         ;; (slug-mismatch-warning-lines, same module as the status check):
         ;; one divergence surface, no parallel reporting mechanism.
         (prepare-go-campaign base-dir
                              input-text
                              plan
                              validation
                              (append (status-divergence-warning-lines base-dir)
                                      (plan-format-deprecation-warning-lines base-dir)
                                      (slug-mismatch-warning-lines base-dir)
                                      (wave-doc-lint-warning-lines base-dir))))]))
;; ============================================================
;; /gsd status handler
;; ============================================================

(define (handle-gsd-status)
  (define mode (gsd-mode))
  (define tw (gsm-ctx-total-waves (current-gsd-ctx)))
  (define cw (gsm-ctx-completed-waves (current-gsd-ctx)))
  (define base-dir (or (current-pinned-dir) (current-directory)))
  ;; BUG-0039 (W5): /gsd shows spent-so-far per wave and campaign total,
  ;; read from the durable record (honest accounting: waves without usage
  ;; metadata are named usage-missing, never faked as zero).
  (define spend-lines
    (with-handlers ([exn:fail? (lambda (e) '())])
      (define rec (load-or-migrate-campaign! base-dir))
      (define wave-line
        (lambda (w)
          (define s (wave-usage-summary w))
          (cond
            [(positive? (usage-summary-missing-attempts s))
             (format "W~a: usage-missing (~a attempt(s) no usage metadata)"
                     (campaign-wave-index w)
                     (usage-summary-missing-attempts s))]
            [(and (usage-summary-cost-usd s) (usage-summary-total-tokens s))
             (format "W~a: $~a (~a tokens)"
                     (campaign-wave-index w)
                     (~r (usage-summary-cost-usd s) #:precision '(= 2))
                     (usage-summary-total-tokens s))]
            [else (format "W~a: no usage yet" (campaign-wave-index w))])))
      (define total (campaign-usage-summary rec))
      (append (list "Spend:")
              (map wave-line (campaign-record-waves rec))
              (list (format "Total: $~a (~a tokens)"
                            (~r (or (usage-summary-cost-usd total) 0) #:precision '(= 2))
                            (or (usage-summary-total-tokens total) 0))))))
  (define parts
    (append (list (format "Mode: ~a" (or mode "inactive"))
                  (if (> tw 0)
                      (format "Waves: ~a/~a complete" (set-count cw) tw)
                      "Waves: not set"))
            spend-lines))
  ;; BUG-0034 (W2): /gsd surfaces wave-status dual-source divergences  ;; (PLAN.md index row vs wave-doc `Status:` header) alongside the normal
  ;; status block. Advisory only, never blocks anything.
  ;; BUG-0035 (W6): plan-format deprecation warnings (inline sections /
  ;; relaxed status-less index rows) join the same advisory block, matching
  ;; the /go surface (docs/gsd-guide.md: both warn since v1.00.21).
  (define advisory-lines
    (if (not base-dir)
        '()
        (append (status-divergence-warning-lines base-dir)
                (plan-format-deprecation-warning-lines base-dir))))
  (hook-amend (hasheq 'text (append-divergence-warnings (string-join parts "\n") advisory-lines))))

;; ============================================================
;; Artifact display and /plan <text> handler
;; ============================================================

;; R-04/R-16: Focused handler for /plan <text> submit
(define (handle-plan-submit plan-text base-dir input-text parsed)
  (define saved-bus (current-gsd-event-bus)) ;; Preserve event bus across reset
  (define saved-dir (current-pinned-dir)) ;; Preserve pinned dir
  (reset-all-gsd-state!) ;; Clean state for fresh plan (F1 fix)
  ;; Clean old wave files to prevent stale state (fast: delete+recreate dir).
  ;; BUG-0032 fix: a bare delete here wiped ACTIVE campaign wave docs whenever
  ;; /plan <text> ran mid-campaign (observed twice live, 2026-08-25). Back up
  ;; the previous waves dir instead of destroying it; the backup is rotated.
  (define waves-dir (build-path base-dir ".planning" "waves"))
  (define waves-backup-dir (build-path base-dir ".planning" "waves-pre-plan-backup"))
  (with-handlers ([exn:fail? (lambda (e)
                               (log-debug "gsd: wave dir cleanup failed: ~a" (exn-message e)))])
    (when (directory-exists? waves-dir)
      (when (directory-exists? waves-backup-dir)
        (delete-directory/files waves-backup-dir))
      (rename-file-or-directory waves-dir waves-backup-dir)))
  (make-directory* waves-dir)
  (when saved-bus
    (set-gsd-event-bus! saved-bus))
  (when saved-dir
    (set-pinned-dir! saved-dir))
  (set-gsd-mode! 'planning)
  (emit-mode-change! 'planning)
  (set-edit-limit! 2000)
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
