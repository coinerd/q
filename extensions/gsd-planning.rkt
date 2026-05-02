#lang racket/base

;; extensions/gsd-planning.rkt — GSD Planning Extension
;; STABILITY: evolving
;;
;; Integrated v0.21.0: Wires all extensions/gsd/ modules into the
;; extension hook system. Each hook handler delegates to the new
;; modules for logic, wrapping results in hook-amend/hook-block/hook-pass.
;;
;; New modules used:
;;   gsd/state-machine.rkt — central state machine (gsm-transition!, gsm-current)
;;   gsd/core.rkt          — command dispatch, tool guard, write guard
;;   gsd/plan-types.rkt    — structured plan/wave/task types
;;   gsd/plan-validator.rkt — validates plan before /go
;;   gsd/wave-executor.rkt — wave lifecycle with error recovery
;;   gsd/prompts.rkt       — phase-specific prompt templates
;;   gsd/context-bundle.rkt — role-specific context assembly
;;
;; gsd-planning-state.rkt remains the backward-compat shim layer
;; for budget/counts/waves/event-bus state.

(require racket/contract
         racket/match
         racket/port
         racket/string
         racket/file
         racket/path
         racket/set
         json
         "define-extension.rkt"
         "dynamic-tools.rkt"
         "ext-commands.rkt"
         "context.rkt"
         "hooks.rkt"
         "tool-api.rkt"
         "../util/event.rkt"
         "api.rkt"
         "gsd-planning-state.rkt"
         "gsd-planning/command-normalization.rkt"
         "gsd-planning/execution-policy.rkt"
         "gsd-planning/plan-diff.rkt"
         ;; v0.21.0 new modules
         "gsd/state-machine.rkt"
         (only-in "gsd/core.rkt"
                  gsd-write-guard
                  gsd-show-status
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
         (only-in "gsd/policy.rkt" policy-decision policy-blocked? policy-reason)

         "gsd/plan-types.rkt"
         "gsd/plan-validator.rkt"
         (except-in "gsd/wave-executor.rkt" next-pending-wave)
         "gsd/prompts.rkt"
         "gsd/context-bundle.rkt"
         "gsd/wave-docs.rkt"
         (only-in "gsd/archive.rkt" ensure-state-md!)
         (only-in "gsd/events.rkt"
                  [gsd-event-bus-box events:event-bus-box]
                  [set-gsd-event-bus! events:set-event-bus!]
                  [emit-gsd-event! events:emit-gsd-event!])
         (only-in "gsd/session-state.rkt" set-gsd-state! gsd-state-sem))

(provide the-extension
         gsd-planning-extension
         planning-system-prompt
         planning-artifact-path
         valid-artifact-name?
         read-planning-artifact
         write-planning-artifact!
         handle-planning-read
         handle-planning-write
         planning-implement-prompt
         gsd-mode
         set-gsd-mode!
         gsd-tool-guard
         ;; Re-export state module accessors for backward compat
         pinned-planning-dir
         set-pinned-planning-dir!
         current-max-old-text-len
         set-current-max-old-text-len!
         reset-all-gsd-state!
         gsd-mode?
         parse-wave-headers
         completed-waves
         total-waves
         set-total-waves!
         mark-wave-complete!
         wave-complete?
         next-pending-wave
         current-wave-index
         set-current-wave-index!
         gsd-session-cleanup
         gsd-event-bus
         set-gsd-event-bus!
         emit-gsd-event!)

;; extract-cmd-args, parse-wave-headers, valid-artifact-name? delegated to command-normalization.rkt

;; ============================================================
;; Event emission helper
;; ============================================================

;; Event emission: delegates to events.rkt via session bus bridge
;; See register-gsd-tools for bus wiring.
(define (emit-gsd-event! event-sym payload)
  (events:emit-gsd-event! event-sym payload))

;; ============================================================
;; Constants
;; ============================================================

;; planning-dir-name, artifact-extensions, json-artifact? delegated to command-normalization.rkt

;; ============================================================
;; Path helpers
;; ============================================================

(define (resolve-project-root start-dir)
  (let loop ([dir (if (string? start-dir)
                      (string->path start-dir)
                      start-dir)])
    (define planning (build-path dir planning-dir-name))
    (match #t
      [_
       #:when (directory-exists? planning)
       dir]
      [_
       #:when (or (directory-exists? (build-path dir "q"))
                  (directory-exists? (build-path dir "BLUEPRINT"))
                  (directory-exists? (build-path dir ".git")))
       dir]
      [_
       #:when (let ([parent (simple-form-path (build-path dir 'up))])
                (equal? parent (simple-form-path dir)))
       start-dir]
      [_ (loop (simple-form-path (build-path dir 'up)))])))

(define (planning-artifact-path base-dir name)
  (define ext (assoc name artifact-extensions))
  (match ext
    [(cons _ suffix) (build-path base-dir planning-dir-name (string-append name suffix))]
    [_
     #:when (or (string-suffix? name ".md") (string-suffix? name ".json"))
     (build-path base-dir planning-dir-name name)]
    [_ #f]))

;; valid-artifact-name?, json-artifact? delegated to command-normalization.rkt

;; ============================================================
;; File I/O
;; ============================================================

(define (get-base-dir args [exec-ctx #f])
  (or (hash-ref args 'base_dir #f)
      (pinned-planning-dir)
      (and exec-ctx (ctx-cwd exec-ctx))
      (current-directory)))

(define (read-planning-artifact base-dir name)
  (define path (planning-artifact-path base-dir name))
  (match path
    [#f #f]
    [(? (lambda (p) (not (file-exists? p)))) #f]
    [_
     #:when (json-artifact? name)
     (with-handlers ([exn:fail? (lambda (e)
                                  (log-warning (format "gsd-planning/read: ~a" (exn-message e)))
                                  #f)])
       (call-with-input-file path (lambda (in) (read-json in))))]
    [_ (call-with-input-file path (lambda (in) (port->string in)))]))

(define (write-planning-artifact! base-dir name content)
  (define path (planning-artifact-path base-dir name))
  (if (not path)
      #f
      (let ([parent (path-only path)])
        (when parent
          (unless (directory-exists? parent)
            (make-directory* parent)))
        (if (json-artifact? name)
            (call-with-output-file path
                                   (lambda (out)
                                     (write-json content out)
                                     (newline out))
                                   #:exists 'truncate)
            (call-with-output-file path (lambda (out) (display content out)) #:exists 'truncate))
        path)))

;; ============================================================
;; Wave header parsing
;; ============================================================

;; parse-wave-headers delegated to command-normalization.rkt

;; ============================================================
;; Prompts
;; ============================================================

(define planning-system-prompt planning-prompt)

(define planning-implement-prompt
  (string-append "[gsd-planning] EXECUTE the plan below. IMPLEMENT NOW — do NOT explore.\n"
                 "\n"
                 "CRITICAL RULES:\n"
                 "1. Do NOT re-read the plan. It is provided below in full.\n"
                 "2. Do NOT write a new plan. Execute the existing one.\n"
                 "3. Do NOT use planning-write during implementation.\n"
                 "   planning-read is allowed to check STATE or VALIDATION.\n"
                 "4. Read each target file BEFORE editing it. You need the current content\n"
                 "   to apply edits correctly. Read is necessary and expected.\n"
                 "5. After reading, apply the edits specified in the wave doc actions.\n"
                 "6. After completing each wave, run its verify command.\n"
                 "\n"
                 "The plan follows. Start implementing immediately.\n\n"))

;; ============================================================
;; Tool schemas
;; ============================================================

(define planning-read-schema
  (hasheq
   'type
   "object"
   'properties
   (hasheq
    'artifact
    (hasheq 'type
            "string"
            'description
            (string-append "Artifact name. Canonical: "
                           (string-join (map car artifact-extensions) ", ")
                           ". Or any .md/.json filename."))
    'base_dir
    (hasheq 'type
            "string"
            'description
            "Project root directory. Auto-resolved at session start. Override only if needed."))
   'required
   '("artifact")))

(define planning-write-schema
  (hasheq
   'type
   "object"
   'properties
   (hasheq
    'artifact
    (hasheq 'type
            "string"
            'description
            (string-append "Artifact name. Use: PLAN, STATE, HANDOFF, etc. "
                           "For wave documents use: waves/W0-slug.md, waves/W1-slug.md. "
                           "Or any .md/.json filename in .planning/."))
    'content
    (hasheq 'type "string" 'description "Content to write (string for .md, JSON string for .json)")
    'base_dir
    (hasheq 'type
            "string"
            'description
            "Project root directory. Auto-resolved at session start. Override only if needed."))
   'required
   '("artifact" "content")))

;; ============================================================
;; Tool handlers
;; ============================================================

(define (handle-planning-read args [exec-ctx #f])
  (define name (hash-ref args 'artifact ""))
  (define base-dir (get-base-dir args exec-ctx))
  (match name
    ["" (make-error-result "Missing required argument: artifact")]
    [(? (lambda (n) (not (valid-artifact-name? n))))
     (make-error-result (format "Invalid artifact name '~a'. Must be one of: ~a or end in .md/.json"
                                name
                                (string-join (map car artifact-extensions) ", ")))]
    [_
     (define content (read-planning-artifact base-dir name))
     (match content
       [#f (make-error-result (format "Artifact '~a' not found in ~a/" name planning-dir-name))]
       [(? hash?) (make-success-result (list (hasheq 'type "text" 'text (jsexpr->string content))))]
       [_ (make-success-result (list (hasheq 'type "text" 'text content)))])]))

(define (handle-planning-write args [exec-ctx #f])
  (define name (hash-ref args 'artifact ""))
  (define content-str (hash-ref args 'content ""))
  (define base-dir (get-base-dir args exec-ctx))
  (match (list name content-str)
    [(list "" _) (make-error-result "Missing required argument: artifact")]
    [(list _ "") (make-error-result "Missing required argument: content")]
    [(list (? (lambda (n) (not (valid-artifact-name? n)))) _)
     (make-error-result (format "Invalid artifact name '~a'. Must be one of: ~a or end in .md/.json"
                                name
                                (string-join (map car artifact-extensions) ", ")))]
    [_
     ;; v0.21.0: Use write guard from gsd/core.rkt
     (define art-path (planning-artifact-path base-dir name))
     (define guard-arg (and art-path (path->string art-path)))
     (define guard-result
       (if guard-arg
           (gsd-write-guard guard-arg (pinned-planning-dir))
           (policy-decision #t #f '())))
     (match (policy-blocked? guard-result)
       [#t (make-error-result (format "Blocked: ~a" (policy-reason guard-result)))]
       [_
        (define parsed-content
          (if (json-artifact? name)
              (with-handlers ([exn:fail? (lambda (e) e)])
                (string->jsexpr content-str))
              content-str))
        (match parsed-content
          [(? exn:fail?)
           (make-error-result (format "Invalid JSON content: ~a" (exn-message parsed-content)))]
          [_
           (define result-path (write-planning-artifact! base-dir name parsed-content))
           (if (not result-path)
               (make-error-result (format "Failed to write artifact '~a'" name))
               (begin
                 (when (and (eq? (gsd-mode) 'planning) (string=? name "PLAN"))
                   (set-gsd-mode! 'plan-written)
                   (emit-gsd-event! (quote gsd.mode.changed) (hasheq 'mode 'plan-written)))
                 (make-success-result (list (hasheq 'type
                                                    "text"
                                                    'text
                                                    (format "Written: ~a"
                                                            (path->string result-path)))))))])])]))

;; ============================================================
;; Extension hooks
;; ============================================================

;; --- register-tools ---
(define (register-gsd-tools ctx _payload)
  (unless (pinned-planning-dir)
    (define cwd (ctx-cwd ctx))
    (when cwd
      (set-pinned-planning-dir! (resolve-project-root cwd))))
  ;; Wire events.rkt bus into session event bus
  (define session-bus (ctx-event-bus ctx))
  (set-gsd-event-bus! session-bus) ; backward compat shim
  (events:set-event-bus!
   (lambda (event-name wrapped-event)
     ;; Bridge: forward events.rkt events to the session event bus
     (when session-bus
       (publish! session-bus (make-event event-name (current-seconds) #f #f wrapped-event)))))
  (ext-register-tool!
   ctx
   "planning-read"
   (string-append "Read GSD planning artifacts from the .planning/ directory. "
                  "Canonical artifacts: "
                  (string-join (map car artifact-extensions) ", ")
                  ". Returns file content as text (for .md) or JSON (for .json).")
   planning-read-schema
   handle-planning-read
   #:prompt-guidelines
   "Use planning-read with artifact='PLAN' or artifact='STATE' to check current plan/state before taking action.")
  (ext-register-tool!
   ctx
   "planning-write"
   (string-append "Write or update GSD planning artifacts in the .planning/ directory. "
                  "Validates artifact names. Creates .planning/ if needed. "
                  "For .json artifacts, pass content as JSON string.")
   planning-write-schema
   handle-planning-write
   #:prompt-guidelines
   (string-append
    "Use planning-write with artifact='PLAN' to update PLAN.md after completing a wave. "
    "Use artifact='STATE' to update STATE.md with current status. "
    "Use artifact='HANDOFF' to write HANDOFF.json before machine switches."))
  (hook-pass #f))

;; --- register-shortcuts ---
(define (register-gsd-commands ctx)
  (ext-register-command! ctx "/plan" "Display current GSD plan" 'general '() '("p"))
  (ext-register-command! ctx "/state" "Display current project state" 'general '() '("s"))
  (ext-register-command! ctx "/handoff" "Display handoff status" 'general '() '("ho"))
  (ext-register-command! ctx
                         "/go"
                         "Start implementing the current plan"
                         'general
                         '()
                         '("implement" "i"))
  (ext-register-command! ctx "/replan" "Return to planning phase" 'general '() '())
  (ext-register-command! ctx "/skip" "Skip a wave (usage: /skip N)" 'general '() '())
  (ext-register-command! ctx "/reset" "Reset GSD to idle state" 'general '() '())
  (ext-register-command! ctx
                         "/wave-done"
                         "Mark wave N complete, update PLAN.md and STATE.md"
                         'general
                         '()
                         '("wd"))
  (ext-register-command! ctx "/done" "Archive completed plan" 'general '() '("d"))
  (ext-register-command! ctx "/gsd" "Show GSD workflow status" 'general '() '())
  (hook-pass #f))

;; --- execute-command: dispatch to helper functions ---
(define (handle-execute-command payload)
  (define cmd (hash-ref payload 'command #f))
  (define input-text (hash-ref payload 'input ""))
  (define base-dir (or (pinned-planning-dir) (current-directory)))
  (match cmd
    [(? (lambda (c) (member c '("/go" "/implement" "/i")))) (handle-go-command base-dir input-text)]
    ["/gsd" (handle-gsd-status)]
    ["/replan"
     (define result (cmd-replan))
     (emit-gsd-event! 'gsd.mode.changed (hasheq 'mode 'exploring))
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
     (emit-gsd-event! 'gsd.mode.changed (hasheq 'mode 'idle))
     (hook-amend (hasheq 'text (or (gsd-command-result-message result) "")))]
    [(? (lambda (c) (member c '("/wave-done" "/wd"))))
     (define wd-args (extract-cmd-args input-text))
     (define result (cmd-wave-done base-dir wd-args))
     (when (gsd-success? result)
       (define data (gsd-command-result-data result))
       (define wave-idx (and (hash? data) (hash-ref data 'wave #f)))
       (when wave-idx
         (emit-gsd-event! 'gsd.wave.completed (hasheq 'wave wave-idx))))
     (hook-amend (hasheq 'text (or (gsd-command-result-message result) "")))]
    ["/done"
     (define done-args (extract-cmd-args input-text))
     (define force? (and done-args (string-contains? done-args "--force")))
     (define result (cmd-done base-dir force?))
     (when (gsd-success? result)
       (define data (gsd-command-result-data result))
       (emit-gsd-event! 'gsd.plan.archived
                        (hasheq 'path
                                (if (hash? data)
                                    (hash-ref data 'archive-path "")
                                    ""))))
     (hook-amend (hasheq 'text (or (gsd-command-result-message result) "")))]
    [_ (handle-artifact-command cmd input-text base-dir payload)]))

;; wave-docs-summary delegated to plan-diff.rkt

;; Handler for /go command
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
           (let ([waves (parse-waves-from-markdown plan-content)]) (gsd-plan waves "" '() '()))))
     ;; Emit gsd.plan.parsed event
     (emit-gsd-event! 'gsd.plan.parsed (hasheq 'wave-count (length (gsd-plan-waves plan))))
     ;; Step 1: Normalize plan to canonical IR
     (define norm-result (normalize-plan plan))
     (match norm-result
       [(? string?)
        ;; Normalization failed (structural error)
        (hook-amend (hasheq 'text
                            (string-append "Plan normalization failed:\n"
                                           norm-result
                                           "\n\nFix the plan before using /go.")))]
       [_
        ;; Emit gsd.plan.normalized event
        (emit-gsd-event! 'gsd.plan.normalized
                         (hasheq 'wave-count (length (gsd-normalized-plan-waves norm-result))))
        ;; Step 2: Validate normalized plan
        (define validation (validate-normalized-plan norm-result))
        (define validated-plan? (gsd-validated-plan? validation))
        ;; Emit gsd.plan.validated event
        (emit-gsd-event! 'gsd.plan.validated
                         (hasheq 'valid?
                                 validated-plan?
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
                               (string-append "Plan validation failed:\n"
                                              report
                                              "\n\nFix the plan before using /go.")))]
          [_
           ;; v0.24.6: Use with-gsd-transaction for snapshot/restore
           (define-values (executor wave-indices)
             (with-gsd-transaction
              "go"
              (lambda ()
                (set-gsd-mode! 'executing)
                (emit-gsd-event! 'gsd.mode.changed (hasheq 'mode 'executing))
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
                (emit-gsd-event! 'gsd.mode.changed
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
                    (format "\nCurrent state:\n~a\n" state-content)
                    ""))
              (define wave-arg
                (let* ([trimmed (string-trim input-text)]
                       [parts (string-split trimmed)])
                  (if (>= (length parts) 2)
                      (let ([maybe-num (string-trim (string-join (cdr parts) " "))])
                        (if (and (> (string-length maybe-num) 0)
                                 (regexp-match? #rx"^[0-9]+$" maybe-num))
                            (format "\nStart with wave ~a." maybe-num)
                            ""))
                      "")))
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
              (hook-amend (hasheq 'new-session
                                  augmented-text
                                  'text
                                  (format "Implementing plan~a..." wave-arg)))])])])]))

;; Handler for /gsd status
(define (handle-gsd-status)
  (define mode (gsd-mode))
  (define tw (total-waves))
  (define cw (completed-waves))
  (define parts
    (list (format "Mode: ~a" (or mode "inactive"))
          (if (> tw 0)
              (format "Waves: ~a/~a complete" (set-count cw) tw)
              "Waves: not set")))
  (hook-amend (hasheq 'text (string-join parts "\n"))))

;; Handler for artifact display and /plan <text> commands
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
        (emit-gsd-event! (quote gsd.mode.changed) (hasheq 'mode 'planning))
        (set-current-max-old-text-len! 500)
        ;; Auto-create STATE.md if missing (#2164)
        (ensure-state-md! base-dir)
        (define existing-plan (read-planning-artifact base-dir "PLAN"))
        (define stale-warning
          (if existing-plan
              "\nNOTE: An existing PLAN.md was found. OVERWRITE it completely with the new plan. Do NOT keep or merge old content.\n"
              ""))
        (define augmented-text (string-append (planning-system-prompt args-text) stale-warning))
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

;; ============================================================
;; ============================================================
;; GSD mode tool guard (tool-call-pre hook)
;; ============================================================

;; v0.21.3: Only mode-based blocking. No budgets.
;; Note (v0.21.8, #2148): Mode is set BEFORE the LLM prompt is submitted
;; (see /plan handler: set-gsd-mode! precedes hook-amend with submit key).
;; Therefore by the time the first tool call arrives, the mode is already
;; 'planning' (mapped from 'exploring'). No race condition exists.
;; planning-write is allowed in 'exploring' and 'plan-written' states;
;; it is only blocked during 'executing' and 'verifying'.
;; gsd-tool-guard: delegated to gsd-planning/execution-policy.rkt

(define (gsd-session-cleanup payload)
  (log-debug "gsd-planning: session shutdown, resetting state")
  (reset-all-gsd-state!)
  (hook-pass payload))

;; ============================================================
;; Extension definition
;; ============================================================

(define-q-extension gsd-planning-extension
                    #:version "1.1.0"
                    #:api-version "1"
                    #:on register-tools
                    register-gsd-tools
                    #:on register-shortcuts
                    register-gsd-commands
                    #:on execute-command
                    handle-execute-command
                    #:on tool-call-pre
                    gsd-tool-guard
                    #:on session-shutdown
                    gsd-session-cleanup)

(define the-extension gsd-planning-extension)
