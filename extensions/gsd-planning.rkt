#lang racket/base

;; extensions/gsd-planning.rkt — GSD Planning Extension
;;
;; Integrated v0.21.0: Wires all extensions/gsd/ modules into the
;; extension hook system. Each hook handler delegates to the new
;; modules for logic, wrapping results in hook-amend/hook-block/hook-pass.
;;
;; New modules used:
;;   gsd/state-machine.rkt — central state machine (gsm-transition!, gsm-current)
;;   gsd/core.rkt          — command dispatch, tool guard, write guard
;;   gsd/steering.rkt      — mode-aware stall detection during executing
;;   gsd/bash-detect.rkt   — detects bash used for file reads
;;   gsd/plan-types.rkt    — structured plan/wave/task types
;;   gsd/plan-validator.rkt — validates plan before /go
;;   gsd/wave-executor.rkt — wave lifecycle with error recovery
;;   gsd/prompts.rkt       — phase-specific prompt templates
;;   gsd/context-bundle.rkt — role-specific context assembly
;;
;; gsd-planning-state.rkt remains the backward-compat shim layer
;; for budget/counts/waves/event-bus state.

(require racket/contract
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
         "../tools/tool.rkt"
         "../util/event.rkt"
         "../agent/event-bus.rkt"
         "gsd-planning-state.rkt"
         ;; v0.21.0 new modules
         "gsd/state-machine.rkt"
         "gsd/core.rkt"
         "gsd/steering.rkt"
         "gsd/bash-detect.rkt"
         "gsd/plan-types.rkt"
         "gsd/plan-validator.rkt"
         (except-in "gsd/wave-executor.rkt" next-pending-wave)
         "gsd/prompts.rkt"
         "gsd/context-bundle.rkt")

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
         reset-read-counts!
         gsd-read-tracker
         go-read-budget
         set-go-read-budget!
         reset-go-budget!
         GO-READ-BUDGET
         GO-READ-WARN-THRESHOLD
         GO-READ-BLOCK-THRESHOLD
         READ-ONLY-TOOLS
         EXPLORATION-BUDGET
         IMPL-READ-PER-WAVE
         IMPL-READ-TOTAL-WARN
         ;; Re-export state module accessors for backward compat
         pinned-planning-dir
         set-pinned-planning-dir!
         current-max-old-text-len
         set-current-max-old-text-len!
         reset-all-gsd-state!
         gsd-mode?
         decrement-budget!
         read-counts
         get-read-count
         increment-read-count!
         clear-read-counts!
         parse-wave-headers
         completed-waves
         total-waves
         set-total-waves!
         mark-wave-complete!
         wave-complete?
         next-pending-wave
         plan-tool-budget
         decrement-plan-budget!
         reset-plan-budget!
         gsd-session-cleanup
         gsd-event-bus
         set-gsd-event-bus!
         emit-gsd-event!)

;; ============================================================
;; Event emission helper
;; ============================================================

(define (emit-gsd-event! type payload)
  (define bus (gsd-event-bus))
  (when bus
    (publish! bus (make-event type (current-seconds) #f #f payload))))

;; ============================================================
;; Constants
;; ============================================================

(define planning-dir-name ".planning")

(define EXPLORATION-BUDGET 30)
(define IMPL-READ-PER-WAVE 2)
(define IMPL-READ-TOTAL-WARN 5)

(define artifact-extensions
  '(("PLAN" . ".md") ("STATE" . ".md")
                     ("HANDOFF" . ".json")
                     ("VALIDATION" . ".md")
                     ("BUG_REPORT" . ".md")
                     ("BUG_PLAN" . ".md")
                     ("BUG_STATE" . ".md")
                     ("BUG_VALIDATION" . ".md")
                     ("SUMMARY" . ".md")
                     ("REVIEW" . ".md")
                     ("ANALYSIS" . ".md")))

;; ============================================================
;; Path helpers
;; ============================================================

(define (resolve-project-root start-dir)
  (let loop ([dir (if (string? start-dir)
                      (string->path start-dir)
                      start-dir)])
    (define planning (build-path dir planning-dir-name))
    (cond
      [(directory-exists? planning) dir]
      [(or (directory-exists? (build-path dir "q"))
           (directory-exists? (build-path dir "BLUEPRINT"))
           (directory-exists? (build-path dir ".git")))
       dir]
      [(let ([parent (simple-form-path (build-path dir 'up))]) (equal? parent (simple-form-path dir)))
       start-dir]
      [else (loop (simple-form-path (build-path dir 'up)))])))

(define (planning-artifact-path base-dir name)
  (define ext (assoc name artifact-extensions))
  (cond
    [ext (build-path base-dir planning-dir-name (string-append name (cdr ext)))]
    [(or (string-suffix? name ".md") (string-suffix? name ".json"))
     (build-path base-dir planning-dir-name name)]
    [else #f]))

(define (valid-artifact-name? name)
  (and (string? name)
       (not (string-contains? name "/"))
       (not (string-contains? name ".."))
       (not (string-contains? name "\x00"))
       (or (assoc name artifact-extensions) (string-suffix? name ".md") (string-suffix? name ".json"))
       #t))

(define (json-artifact? name)
  (or (string-suffix? name ".json")
      (let ([ext (assoc name artifact-extensions)]) (and ext (string=? (cdr ext) ".json")))))

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
  (cond
    [(not path) #f]
    [(not (file-exists? path)) #f]
    [(json-artifact? name)
     (with-handlers ([exn:fail? (lambda (e)
                                  (log-warning (format "gsd-planning/read: ~a" (exn-message e)))
                                  #f)])
       (call-with-input-file path (lambda (in) (read-json in))))]
    [else (call-with-input-file path (lambda (in) (port->string in)))]))

(define (write-planning-artifact! base-dir name content)
  (define path (planning-artifact-path base-dir name))
  (if (not path)
      #f
      (let ([dir (build-path base-dir planning-dir-name)])
        (unless (directory-exists? dir)
          (make-directory* dir))
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

(define (parse-wave-headers plan-text)
  (define matches (regexp-match* #rx"## [Ww]ave +([0-9]+)" plan-text))
  (for/list ([m (in-list matches)])
    (define num-match (regexp-match #rx"([0-9]+)$" m))
    (if num-match
        (string->number (cadr num-match))
        0)))

;; ============================================================
;; Prompts
;; ============================================================

(define planning-system-prompt
  (string-append
   "[gsd-planning] Create a structured implementation plan for the following request.\n"
   "Write your plan to .planning/PLAN.md using the planning-write tool.\n"
   "\n"
   "STEP 1 — EXPLORE: Read the codebase thoroughly. For each wave you MUST identify:\n"
   "  - Root cause: what causes the bug or what needs to change, and where exactly\n"
   "  - Exact file path and line numbers\n"
   "  - The old-text that the edit tool will match\n"
   "  - The new-text replacement code\n"
   (format "EXPLORATION BUDGET: Maximum ~a tool calls (read, grep, find, planning-read).\n"
           EXPLORATION-BUDGET)
   "After 30 calls, write the plan with what you know. Partial plans are better than no plans.\n"
   "Prioritize files most likely to contain the root cause. Skip tangential exploration.\n"
   "\n"
   "STEP 2 — PLAN: Write PLAN.md with this exact format per wave:\n"
   "\n"
   "## Wave N: <title>\n"
   "- Root cause: <what causes the bug, where>\n"
   "- File: <path>, lines <start>-<end>\n"
   "- Old text: <exact code to find, must be unique in file>\n"
   "- New text: <exact replacement code>\n"
   "- Verify: <command to run>\n"
   "\n"
   "Include actual code snippets in old-text and new-text fields.\n"
   "Each wave must be directly executable without further exploration.\n"
   "\n"
   "STEP 3 — FINISH: Tell the user: 'Use /go to start implementing.'\n"
   "Do NOT implement — only plan.\n"
   "OVERWRITE: Replace the entire existing PLAN.md. Do NOT append or merge with old content.\n"
   "Write the new plan from scratch.\n\n"
   "IMPORTANT: [SYSTEM NOTICE: ...] messages in tool results are steering signals from the runtime.\n"
   "Always read and follow them. They provide budget warnings, read-duplication hints,\n"
   "and stall-recovery instructions. Ignoring them degrades performance.\n\n"
   "User request: "))

(define planning-implement-prompt
  (string-append "[gsd-planning] EXECUTE the plan below. IMPLEMENT NOW — do NOT explore.\n"
                 "\n"
                 "CRITICAL RULES:\n"
                 "1. Do NOT re-read the plan. It is provided below in full.\n"
                 "2. Do NOT write a new plan. Execute the existing one.\n"
                 "3. Do NOT use planning-write during implementation.\n"
                 "   planning-read is allowed to check STATE or VALIDATION.\n"
                 "4. Do NOT run read-only tools (read, find, grep, ls) unless a wave's\n"
                 "   old-text match fails and you need to re-read the target file ONCE.\n"
                 "5. Use the edit or write tool for EVERY wave. Your budget is:\n"
                 (format "   - Max ~a read-only tool calls per wave before you MUST write/edit.\n"
                         IMPL-READ-PER-WAVE)
                 (format "   - After ~a total read-only calls across all waves, STOP and summarize.\n"
                         IMPL-READ-TOTAL-WARN)
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
    (hasheq 'type "string" 'description "Artifact name (e.g. PLAN, STATE, HANDOFF)")
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
  (cond
    [(string=? name "") (make-error-result "Missing required argument: artifact")]
    [(not (valid-artifact-name? name))
     (make-error-result (format "Invalid artifact name '~a'. Must be one of: ~a or end in .md/.json"
                                name
                                (string-join (map car artifact-extensions) ", ")))]
    [else
     (define content (read-planning-artifact base-dir name))
     (cond
       [(not content)
        (make-error-result (format "Artifact '~a' not found in ~a/" name planning-dir-name))]
       [(hash? content)
        (make-success-result (list (hasheq 'type "text" 'text (jsexpr->string content))))]
       [else (make-success-result (list (hasheq 'type "text" 'text content)))])]))

(define (handle-planning-write args [exec-ctx #f])
  (define name (hash-ref args 'artifact ""))
  (define content-str (hash-ref args 'content ""))
  (define base-dir (get-base-dir args exec-ctx))
  (cond
    [(string=? name "") (make-error-result "Missing required argument: artifact")]
    [(string=? content-str "") (make-error-result "Missing required argument: content")]
    [(not (valid-artifact-name? name))
     (make-error-result (format "Invalid artifact name '~a'. Must be one of: ~a or end in .md/.json"
                                name
                                (string-join (map car artifact-extensions) ", ")))]
    [else
     ;; v0.21.0: Use write guard from gsd/core.rkt
     (define art-path (planning-artifact-path base-dir name))
     (define guard-arg (and art-path (path->string art-path)))
     (define guard-result
       (if guard-arg
           (gsd-write-guard guard-arg (pinned-planning-dir))
           #t))
     (cond
       [(hash? guard-result)
        (make-error-result (format "Blocked: ~a" (hash-ref guard-result 'reason "write blocked")))]
       [else
        (define parsed-content
          (if (json-artifact? name)
              (with-handlers ([exn:fail? (lambda (e) e)])
                (string->jsexpr content-str))
              content-str))
        (cond
          [(exn:fail? parsed-content)
           (make-error-result (format "Invalid JSON content: ~a" (exn-message parsed-content)))]
          [else
           (define result-path (write-planning-artifact! base-dir name parsed-content))
           (if (not result-path)
               (make-error-result (format "Failed to write artifact '~a'" name))
               (begin
                 (when (and (eq? (gsd-mode) 'planning) (string=? name "PLAN"))
                   (set-gsd-mode! 'plan-written)
                   (emit-gsd-event! "gsd.mode.changed" (hasheq 'mode 'plan-written)))
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
  (set-gsd-event-bus! (ctx-event-bus ctx))
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
  (ext-register-command! ctx "/gsd" "Show GSD workflow status" 'general '() '())
  (hook-pass #f))

;; --- execute-command: dispatch to helper functions ---
(define (handle-execute-command payload)
  (define cmd (hash-ref payload 'command #f))
  (define input-text (hash-ref payload 'input ""))
  (define base-dir (or (pinned-planning-dir) (current-directory)))
  (cond
    [(member cmd '("/go" "/implement" "/i")) (handle-go-command base-dir input-text)]
    [(equal? cmd "/gsd") (handle-gsd-status)]
    [else (handle-artifact-command cmd input-text base-dir payload)]))

;; Handler for /go command
(define (handle-go-command base-dir input-text)
  (define plan-content (read-planning-artifact base-dir "PLAN"))
  (cond
    [(not plan-content)
     (hook-amend (hasheq 'text "No PLAN found in .planning/. Use /plan <task> to create one."))]
    [else
     ;; v0.21.0: Validate plan before allowing /go
     (define waves (parse-waves-from-markdown plan-content))
     (define plan (gsd-plan waves "" '() '()))
     (define validation (validate-plan-strict plan))
     (cond
       [(not (validation-valid? validation))
        (define report (format-validation-report validation))
        (hook-amend (hasheq 'text
                            (string-append "Plan validation failed:\n"
                                           report
                                           "\n\nFix the plan before using /go.")))]
       [else
        (set-gsd-mode! 'executing)
        (emit-gsd-event! "gsd.mode.changed" (hasheq 'mode 'executing))
        (set-current-max-old-text-len! 1200)
        (reset-read-counts!)
        (reset-go-budget!)
        (define wave-indices (parse-wave-headers plan-content))
        (when (not (null? wave-indices))
          (set-total-waves! (add1 (apply max wave-indices))))
        (reset-plan-budget!)
        (define executor (make-wave-executor plan))
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
                  (if (and (> (string-length maybe-num) 0) (regexp-match? #rx"^[0-9]+$" maybe-num))
                      (format "\nStart with wave ~a." maybe-num)
                      ""))
                "")))
        (define exec-prompt (executing-prompt plan executor))
        (define augmented-text
          (string-append planning-implement-prompt
                         exec-prompt
                         "\nPlan:\n"
                         plan-content
                         "\n"
                         state-note
                         wave-arg))
        (hook-amend
         (hasheq 'submit augmented-text 'text (format "Implementing plan~a..." wave-arg)))])]))

;; Handler for /gsd status
(define (handle-gsd-status)
  (define mode (gsd-mode))
  (define tw (total-waves))
  (define cw (completed-waves))
  (define pb (plan-tool-budget))
  (define budget (go-read-budget))
  (define rc (hash-count (read-counts)))
  (define parts
    (list (format "Mode: ~a" (or mode "inactive"))
          (if (> tw 0)
              (format "Waves: ~a/~a complete" (set-count cw) tw)
              "Waves: not set")
          (if pb
              (format "Plan budget: ~a/~a remaining" pb EXPLORATION-BUDGET)
              "Plan budget: inactive")
          (if budget
              (format "Read budget: ~a/~a" budget GO-READ-BUDGET)
              "Read budget: inactive")
          (format "Files read: ~a" rc)))
  (hook-amend (hasheq 'text (string-join parts "\n"))))

;; Handler for artifact display and /plan <text> commands
(define (handle-artifact-command cmd input-text base-dir payload)
  (define artifact
    (cond
      [(member cmd '("/plan" "/p")) "PLAN"]
      [(member cmd '("/state" "/s")) "STATE"]
      [(member cmd '("/handoff" "/ho")) "HANDOFF"]
      [else #f]))
  (cond
    [(not artifact) (hook-pass payload)]
    [else
     (define args-text
       (let* ([trimmed (string-trim input-text)]
              [parts (and (> (string-length trimmed) 0)
                          (char=? (string-ref trimmed 0) #\/)
                          (string-split trimmed))])
         (and (pair? parts)
              (let ([rest (string-trim (substring input-text (string-length (car parts))))])
                (and (> (string-length rest) 0) rest)))))
     (cond
       ;; /plan <text> → submit as planning prompt
       [(and (member cmd '("/plan" "/p")) args-text)
        (set-gsd-mode! 'planning)
        (emit-gsd-event! "gsd.mode.changed" (hasheq 'mode 'planning))
        (reset-plan-budget!)
        (set-current-max-old-text-len! 500)
        (reset-read-counts!)
        (set-go-read-budget! #f)
        (define existing-plan (read-planning-artifact base-dir "PLAN"))
        (define stale-warning
          (if existing-plan
              "\nNOTE: An existing PLAN.md was found. OVERWRITE it completely with the new plan. Do NOT keep or merge old content.\n"
              ""))
        (define augmented-text (string-append planning-system-prompt stale-warning args-text))
        (hook-amend (hasheq 'submit augmented-text 'text (format "Planning: ~a" args-text)))]
       [else
        ;; Display artifact content
        (define content (read-planning-artifact base-dir artifact))
        (define text
          (cond
            [(not content) (format "No ~a found in .planning/" artifact)]
            [(hash? content) (jsexpr->string content)]
            [else content]))
        (hook-amend (hasheq 'text text))])]))

;; ============================================================
;; Budget warning injection + redundant read detection (tool-result-post)
;; ============================================================

(define READ_HINT_THRESHOLD 3)

(define (budget-warning-text)
  (define remaining (go-read-budget))
  (cond
    [(and remaining (<= remaining GO-READ-WARN-THRESHOLD))
     (format
      "\n\n[SYSTEM NOTICE: BUDGET WARNING: ~a/~a read calls remaining. Focus on implementation."
      remaining
      GO-READ-BUDGET)]
    [(and remaining (<= remaining (quotient GO-READ-BUDGET 3)))
     (format "\n\n[SYSTEM NOTICE: ~a/~a read calls remaining. Consider narrowing focus.]"
             remaining
             GO-READ-BUDGET)]
    [else #f]))

(define (maybe-append-budget-warning content-list)
  (define warn (budget-warning-text))
  (if warn
      (append content-list (list (hasheq 'type "text" 'text warn)))
      content-list))

(define (maybe-inject-budget-warning payload)
  (define warn (budget-warning-text))
  (if warn
      (let* ([result (hash-ref payload 'result #f)]
             [content (and result (tool-result? result) (tool-result-content result))]
             [base-content (if (list? content)
                               content
                               (list content))]
             [final-content (append base-content (list (hasheq 'type "text" 'text warn)))])
        (hook-amend (hasheq 'result
                            (make-success-result final-content (tool-result-details result)))))
      (hook-pass payload)))

;; v0.21.0: gsd-read-tracker now also:
;;   - Runs stall detection via gsd/steering.rkt during executing mode
;;   - Detects bash file-read bypass via gsd/bash-detect.rkt
;;   - Tracks wave progress during executing mode
(define (gsd-read-tracker payload)
  (define tool-name (hash-ref payload 'tool-name #f))
  (define result (hash-ref payload 'result #f))
  (cond
    ;; Track successful reads with path info
    [(and (equal? tool-name "read") result (tool-result? result) (not (tool-result-is-error? result)))
     (handle-read-result payload tool-name result)]
    ;; Detect bash file-read bypass
    [(and (equal? tool-name "bash") result (tool-result? result) (not (tool-result-is-error? result)))
     (handle-bash-result payload result)]
    ;; Other read-only tools: inject budget warning
    [(and (member tool-name READ-ONLY-TOOLS)
          result
          (tool-result? result)
          (not (tool-result-is-error? result)))
     (maybe-inject-budget-warning payload)]
    ;; Wave progress after successful edit during executing mode
    [(and (eq? (gsd-mode) 'executing)
          (member tool-name '("edit" "write"))
          result
          (tool-result? result)
          (not (tool-result-is-error? result)))
     (handle-wave-progress payload result)]
    [else (hook-pass payload)]))

;; Handle read tool results: read-count tracking + stall detection + budget warnings
(define (handle-read-result payload tool-name result)
  (define details (tool-result-details result))
  (define file-path (and (hash? details) (hash-ref details 'path #f)))
  (cond
    [(not file-path) (maybe-inject-budget-warning payload)]
    [else
     (define new-count (increment-read-count! file-path))
     ;; v0.21.0: Run steering stall detection during executing mode
     (define stall-prompt (gsd-steering-check tool-name (hasheq 'path file-path) result))
     (cond
       ;; At hint threshold: inject read hint + stall + budget
       [(and (>= new-count READ_HINT_THRESHOLD) (= 0 (modulo new-count READ_HINT_THRESHOLD)))
        (define hint-text
          (format
           "\n\n[SYSTEM NOTICE: You have read '~a' ~a times. Consider using your memory of previous reads instead of re-reading.]"
           file-path
           new-count))
        (define content (tool-result-content result))
        (define base-content
          (append (if (list? content)
                      content
                      (list content))
                  (list (hasheq 'type "text" 'text hint-text))))
        (define with-stall
          (if stall-prompt
              (append base-content (list (hasheq 'type "text" 'text stall-prompt)))
              base-content))
        (define final-content (maybe-append-budget-warning with-stall))
        (hook-amend (hasheq 'result (make-success-result final-content (tool-result-details result))))]
       ;; Stall detected but not at hint threshold
       [stall-prompt
        (define content (tool-result-content result))
        (define base-content
          (if (list? content)
              content
              (list content)))
        (define with-stall (append base-content (list (hasheq 'type "text" 'text stall-prompt))))
        (define final-content (maybe-append-budget-warning with-stall))
        (hook-amend (hasheq 'result
                            (make-success-result final-content (tool-result-details result))))]
       [else (maybe-inject-budget-warning payload)])]))

;; Handle bash tool results: detect file-read bypass
(define (handle-bash-result payload result)
  (define args (or (hash-ref payload 'arguments #f) (hash-ref payload 'tool-args #f)))
  (define cmd-str
    (if (hash? args)
        (hash-ref args 'command "")
        ""))
  (define-values (is-read? detail) (detect-file-read-bash cmd-str))
  (cond
    [is-read?
     (emit-gsd-event! "gsd.bash.read-bypass" (hasheq 'command cmd-str 'detail detail))
     (define bypass-text
       (format
        "\n\n[SYSTEM NOTICE: Detected file read via bash (~a). Consider using the read tool for better tracking.]"
        (or detail cmd-str)))
     (define content (tool-result-content result))
     (define base-content
       (if (list? content)
           content
           (list content)))
     (define final-content (append base-content (list (hasheq 'type "text" 'text bypass-text))))
     (hook-amend (hasheq 'result (make-success-result final-content (tool-result-details result))))]
    [else (hook-pass payload)]))

;; Handle wave progress after successful edit/write during executing
(define (handle-wave-progress payload result)
  (define npw (next-pending-wave))
  (when npw
    (mark-wave-complete! npw)
    (emit-gsd-event! "gsd.wave.complete" (hasheq 'wave npw)))
  (define tw (total-waves))
  (define cw (completed-waves))
  (define progress-text
    (if (> tw 0)
        (format "\n\n[PROGRESS: Wave ~a/~a complete. ~a remaining.]"
                (set-count cw)
                tw
                (- tw (set-count cw)))
        ""))
  (if (string=? progress-text "")
      (hook-pass payload)
      (let* ([content (tool-result-content result)]
             [base-content (if (list? content)
                               content
                               (list content))]
             [final-content (append base-content (list (hasheq 'type "text" 'text progress-text)))])
        (hook-amend (hasheq 'result
                            (make-success-result final-content (tool-result-details result)))))))

(define (reset-read-counts!)
  (clear-read-counts!))

;; ============================================================
;; GSD mode tool guard (tool-call-pre hook)
;; ============================================================

;; v0.21.0: Uses gsm-tool-allowed? from state-machine.rkt for mode-aware blocking.
(define (gsd-tool-guard payload)
  (define mode (gsd-mode))
  (define tool-name (hash-ref payload 'tool-name #f))
  (define allowed (gsm-tool-allowed? tool-name))
  (cond
    ;; Block planning-write during executing (want specific message)
    [(and (eq? mode 'executing) (equal? tool-name "planning-write"))
     (hook-block "Cannot update plan during /go. Focus on executing the existing plan.")]
    ;; State machine says tool is blocked in plan-written mode
    [(and (not allowed) (eq? mode 'plan-written))
     (hook-block "Plan written to PLAN.md. Use /go to start implementing.")]
    ;; State machine says tool is blocked
    [(not allowed) (hook-block (format "Tool '~a' blocked in ~a mode." tool-name mode))]
    ;; During planning mode, enforce exploration budget
    [(and (eq? mode 'planning)
          (member tool-name '("read" "grep" "find" "ls" "glob" "planning-read"))
          (plan-tool-budget))
     (define remaining (decrement-plan-budget!))
     (cond
       [(and remaining (<= remaining 0))
        (emit-gsd-event! "gsd.budget.warning" (hasheq 'budget-type 'exploration 'remaining 0))
        (hook-block (format "Exploration budget exhausted (~a calls used). Write your plan now."
                            EXPLORATION-BUDGET))]
       [(and remaining (<= remaining 10)) (hook-pass payload)]
       [else (hook-pass payload)])]
    ;; During /go, enforce read budget
    [(and (eq? mode 'executing) (member tool-name READ-ONLY-TOOLS) (go-read-budget))
     (define remaining (decrement-budget!))
     (cond
       [(< remaining GO-READ-BLOCK-THRESHOLD)
        (hook-block
         (format
          "Read budget exhausted (~a read calls used, budget is ~a). Stop exploring and implement."
          (- GO-READ-BUDGET remaining)
          GO-READ-BUDGET))]
       [else (hook-pass payload)])]
    [else (hook-pass payload)]))

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
                    #:on tool-result-post
                    gsd-read-tracker
                    #:on session-shutdown
                    gsd-session-cleanup)

(define the-extension gsd-planning-extension)
