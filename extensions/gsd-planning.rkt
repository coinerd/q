#lang racket/base

;; extensions/gsd-planning.rkt — GSD Planning Extension
;;
;; Wave A2: Registers planning-read and planning-write tools, plus
;; /plan, /state, /handoff slash commands for GSD workflow support.

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
         "gsd-planning-state.rkt")

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
         gsd-session-cleanup)

;; ============================================================
;; Event emission helper (v0.20.4 W1)
;; ============================================================

;; Emit a GSD event to the current session's event bus.
;; No-op if no event bus is available (e.g. during tests).
(define (emit-gsd-event! type payload)
  (define bus (current-gsd-event-bus))
  (when bus
    (publish! bus (make-event type (current-seconds) #f #f payload))))

;; Lazy reference to the current event bus — resolved at call time.
(define current-gsd-event-bus (make-parameter #f))

;; ============================================================
;; Constants
;; ============================================================

(define planning-dir-name ".planning")

;; pinned-planning-dir is now in gsd-planning-state.rkt (box with semaphore).
;; Tests and call sites use (pinned-planning-dir) / (set-pinned-planning-dir! v).

;; GSD workflow mode: #f | 'planning | 'plan-written | 'executing
;; NOW in gsd-planning-state.rkt — semaphore-protected.

;; Resolve the project root by walking up from 'start-dir' looking for
;; a directory containing .planning/, q/, BLUEPRINT/, or .git/.
(define (resolve-project-root start-dir)
  (let loop ([dir (if (string? start-dir)
                      (string->path start-dir)
                      start-dir)])
    (define planning (build-path dir planning-dir-name))
    (cond
      ;; Found .planning/ — use this directory
      [(directory-exists? planning) dir]
      ;; Found project markers
      [(or (directory-exists? (build-path dir "q"))
           (directory-exists? (build-path dir "BLUEPRINT"))
           (directory-exists? (build-path dir ".git")))
       dir]
      ;; Reached filesystem root — fall back to start-dir
      [(let ([parent (simple-form-path (build-path dir 'up))]) (equal? parent (simple-form-path dir)))
       start-dir]
      [else (loop (simple-form-path (build-path dir 'up)))])))

;; ============================================================
;; Wave header parsing (v0.20.4 W1.1)
;; ============================================================

;; Parse wave headers from PLAN.md content.
;; Returns a sorted list of wave indices found in the plan.
;; Matches lines like: ## Wave 0:, ## Wave 1:, ## Wave N:
(define (parse-wave-headers plan-text)
  (define matches (regexp-match* #rx"## [Ww]ave +([0-9]+)" plan-text))
  (for/list ([m (in-list matches)])
    (define num-match (regexp-match #rx"([0-9]+)$" m))
    (if num-match
        (string->number (cadr num-match))
        0)))

;; Planning preamble prepended to user text when /plan <text> submits.
;; Gives the agent explicit GSD planning instructions.

;; Prompt constants (W4 fix: avoid hardcoded magic numbers)
(define EXPLORATION-BUDGET 30)
(define IMPL-READ-PER-WAVE 2)
(define IMPL-READ-TOTAL-WARN 5)

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

;; Get the base-dir for planning operations.
;; Priority: explicit base_dir arg > pinned dir from session start > exec-ctx cwd > current-directory.
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
     (let ([content (read-planning-artifact base-dir name)])
       (cond
         [(not content)
          (make-error-result (format "Artifact '~a' not found in ~a/" name planning-dir-name))]
         [(hash? content)
          (make-success-result (list (hasheq 'type "text" 'text (jsexpr->string content))))]
         [else (make-success-result (list (hasheq 'type "text" 'text content)))]))]))

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
     (let ([parsed-content (if (json-artifact? name)
                               (with-handlers ([exn:fail? (lambda (e) e)])
                                 (string->jsexpr content-str))
                               content-str)])
       (cond
         [(exn:fail? parsed-content)
          (make-error-result (format "Invalid JSON content: ~a" (exn-message parsed-content)))]
         [else
          (let ([result-path (write-planning-artifact! base-dir name parsed-content)])
            (if (not result-path)
                (make-error-result (format "Failed to write artifact '~a'" name))
                (begin
                  ;; Transition from planning to plan-written mode.
                  ;; This blocks further write/edit/bash calls until /go is invoked.
                  (when (and (eq? (gsd-mode) 'planning) (string=? name "PLAN"))
                    (set-gsd-mode! 'plan-written)
                    (emit-gsd-event! "gsd.mode.changed" (hasheq 'mode 'plan-written)))
                  (make-success-result (list (hasheq 'type
                                                     "text"
                                                     'text
                                                     (format "Written: ~a"
                                                             (path->string result-path))))))))]))]))

;; ============================================================
;; Extension definition
;; ============================================================

(define (register-gsd-tools ctx _payload)
  ;; Pin the planning directory from the extension context.
  ;; Walk up from cwd to find project root with .planning/, q/, or .git/.
  ;; Only pin when we have a real working-directory (not in tests).
  (unless (pinned-planning-dir)
    (define cwd (ctx-cwd ctx))
    (when cwd
      (set-pinned-planning-dir! (resolve-project-root cwd))))
  ;; v0.20.4 W1: capture event bus for later event emission
  (current-gsd-event-bus (ctx-event-bus ctx))
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

(define planning-implement-prompt
  (string-append "[gsd-planning] EXECUTE the plan below. IMPLEMENT NOW — do NOT explore.\n"
                 "\n"
                 "CRITICAL RULES:\n"
                 "1. Do NOT re-read the plan. It is provided below in full.\n"
                 "2. Do NOT write a new plan. Execute the existing one.\n"
                 "3. Do NOT use planning-write during implementation.\n"
                 ;; I1 fix: planning-read IS allowed during /go (harmless, may need to re-check STATE)
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

;; Execute command handler: responds to /plan, /state, /handoff, /go dispatch
(define (handle-execute-command payload)
  (define cmd (hash-ref payload 'command #f))
  (define input-text (hash-ref payload 'input ""))
  (define base-dir (or (pinned-planning-dir) (current-directory)))
  (cond
    ;; /go, /implement, /i → start implementing the plan
    [(member cmd '("/go" "/implement" "/i"))
     (define plan-content (read-planning-artifact base-dir "PLAN"))
     (cond
       [(not plan-content)
        (hook-amend (hasheq 'text "No PLAN found in .planning/. Use /plan <task> to create one."))]
       [else
        (set-gsd-mode! 'executing)
        ;; v0.20.4 W1: emit mode-changed event
        (emit-gsd-event! "gsd.mode.changed" (hasheq 'mode 'executing))
        ;; Raise edit limit during execution mode (500 → 1200)
        (set-current-max-old-text-len! 1200)
        ;; Reset read counter for fresh execution
        (reset-read-counts!)
        ;; Reset read budget for fresh execution
        (reset-go-budget!)
        ;; v0.20.4 W1.1: Parse wave headers and set total waves
        (define wave-indices (parse-wave-headers plan-content))
        (when (not (null? wave-indices))
          (set-total-waves! (add1 (apply max wave-indices))))
        ;; v0.20.4 W1: reset plan budget for /go phase
        (reset-plan-budget!)
        (define state-content (read-planning-artifact base-dir "STATE"))
        (define state-note
          (if state-content
              (format "\nCurrent state:\n~a\n" state-content)
              ""))
        (define wave-arg
          (let* ([trimmed (string-trim input-text)]
                 ;; Input might be "/go wave 2" or "/go 2"
                 [parts (string-split trimmed)])
            (if (>= (length parts) 2)
                (let ([maybe-num (string-trim (string-join (cdr parts) " "))])
                  (if (and (> (string-length maybe-num) 0) (regexp-match? #rx"^[0-9]+$" maybe-num))
                      (format "\nStart with wave ~a." maybe-num)
                      ""))
                "")))
        (define augmented-text
          (string-append planning-implement-prompt "Plan:\n" plan-content "\n" state-note wave-arg))
        (hook-amend
         (hasheq 'submit augmented-text 'text (format "Implementing plan~a..." wave-arg)))])]
    ;; /gsd status command (v0.20.4 W1.4)
    [(equal? cmd "/gsd")
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
     (hook-amend (hasheq 'text (string-join parts "\n")))]
    ;; Artifact display commands
    [else
     (define artifact
       (cond
         [(member cmd '("/plan" "/p")) "PLAN"]
         [(member cmd '("/state" "/s")) "STATE"]
         [(member cmd '("/handoff" "/ho")) "HANDOFF"]
         [else #f]))
     (cond
       [(not artifact) (hook-pass payload)]
       [else
        (cond
          ;; /plan <text> and /p <text> → submit as prompt to the agent
          [(and (member cmd '("/plan" "/p"))
                (let* ([trimmed (string-trim input-text)]
                       [parts (and (> (string-length trimmed) 0)
                                   (char=? (string-ref trimmed 0) #\/)
                                   (string-split trimmed))])
                  (and (pair? parts)
                       (let ([rest (string-trim (substring input-text (string-length (car parts))))])
                         (and (> (string-length rest) 0) rest)))))
           =>
           (lambda (args)
             ;; Reset mode for new planning session
             (set-gsd-mode! 'planning)
             ;; v0.20.4 W1: emit mode-changed event + init plan budget
             (emit-gsd-event! "gsd.mode.changed" (hasheq 'mode 'planning))
             (reset-plan-budget!)
             ;; Reset edit limit to default during planning
             (set-current-max-old-text-len! 500)
             ;; Reset read counter for fresh planning
             (reset-read-counts!)
             ;; Reset budget
             (set-go-read-budget! #f)
             ;; v0.19.12 W2: Detect stale existing PLAN.md and inject warning
             (define existing-plan (read-planning-artifact base-dir "PLAN"))
             (define stale-warning
               (if existing-plan
                   (format
                    "\nNOTE: An existing PLAN.md was found. OVERWRITE it completely with the new plan. Do NOT keep or merge old content.\n")
                   ""))
             (define augmented-text (string-append planning-system-prompt stale-warning args))
             (hook-amend (hasheq 'submit augmented-text 'text (format "Planning: ~a" args))))]
          [else
           ;; Display artifact content (always for /state, /handoff; no-args /plan)
           (define content (read-planning-artifact base-dir artifact))
           (define text
             (cond
               [(not content) (format "No ~a found in .planning/" artifact)]
               [(hash? content) (jsexpr->string content)]
               [else content]))
           (hook-amend (hasheq 'text text))])])]))

;; ============================================================
;; Budget warning injection + Redundant read detection (tool-result-post hook)
;; ============================================================

;; Tracks per-file read count to detect redundant reads.
;; State lives in gsd-planning-state.rkt.
;; Also injects budget warnings into tool results so the LLM can see them.

(define READ_HINT_THRESHOLD 3)

;; Helper: inject budget warning into tool result via tool-result-post.
;; This makes the warning visible to the LLM in the result content.
(define (budget-warning-text)
  (define remaining (go-read-budget))
  (cond
    [(and remaining (<= remaining GO-READ-WARN-THRESHOLD))
     (format
      "\n\n[SYSTEM NOTICE: BUDGET WARNING: ~a/~a read calls remaining. Focus on implementation."
      remaining
      GO-READ-BUDGET)]
    ;; v0.20.4 W5: soft warning at 2/3 budget used
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

(define (gsd-read-tracker payload)
  (define tool-name (hash-ref payload 'tool-name #f))
  (define result (hash-ref payload 'result #f))
  (cond
    ;; Only track successful reads with path info
    [(and (equal? tool-name "read") result (tool-result? result) (not (tool-result-is-error? result)))
     (define details (tool-result-details result))
     (define file-path (and (hash? details) (hash-ref details 'path #f)))
     (cond
       [(not file-path) (maybe-inject-budget-warning payload)]
       [else
        (define new-count (increment-read-count! file-path))
        (cond
          [(and (>= new-count READ_HINT_THRESHOLD) (= 0 (modulo new-count READ_HINT_THRESHOLD)))
           ;; Inject both read hint and budget warning if applicable
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
           (define final-content (maybe-append-budget-warning base-content))
           (hook-amend (hasheq 'result
                               (make-success-result final-content (tool-result-details result))))]
          [else (maybe-inject-budget-warning payload)])])]
    ;; For other read-only tools (grep, find, ls, glob), still inject budget warning
    [(and (member tool-name READ-ONLY-TOOLS)
          result
          (tool-result? result)
          (not (tool-result-is-error? result)))
     (maybe-inject-budget-warning payload)]
    ;; v0.20.4 W1.2: Wave progress injection after successful edit during executing mode
    [(and (eq? (gsd-mode) 'executing)
          (member tool-name '("edit" "write"))
          result
          (tool-result? result)
          (not (tool-result-is-error? result)))
     ;; Check if assistant text contains a wave header completion pattern
     ;; We detect which wave was being executed based on completed-waves state
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
                [final-content (append base-content
                                       (list (hasheq 'type "text" 'text progress-text)))])
           (hook-amend (hasheq 'result
                               (make-success-result final-content (tool-result-details result))))))]
    [else (hook-pass payload)]))

;; Reset read counts when entering a new mode
(define (reset-read-counts!)
  (clear-read-counts!))

;; ============================================================
;; /go Budget Counter
;; ============================================================

;; Budget constants now in gsd-planning-state.rkt.
;; READ-ONLY-TOOLS, GO-READ-BUDGET, GO-READ-WARN-THRESHOLD, GO-READ-BLOCK-THRESHOLD
;; are imported from there.

;; ============================================================
;; GSD mode tool guard (tool-call-pre hook handler)
;; ============================================================

;; Block edit/write/bash after PLAN written (awaiting /go).
;; Block planning-write during /go (plan is frozen during execution).
;; Budget enforcement for read-only tools during /go.
;; All other tool calls pass through.
(define (gsd-tool-guard payload)
  (define mode (gsd-mode))
  (define tool-name (hash-ref payload 'tool-name #f))
  (cond
    ;; After plan written, block write tools until /go
    [(and (eq? mode 'plan-written) (member tool-name '("edit" "write" "bash")))
     (hook-block "Plan written to PLAN.md. Use /go to start implementing.")]
    ;; During /go, block plan rewrites
    [(and (eq? mode 'executing) (equal? tool-name "planning-write"))
     (hook-block "Cannot update plan during /go. Focus on executing the existing plan.")]
    ;; v0.20.4 W1.3: During planning mode, enforce exploration budget
    [(and (eq? mode 'planning)
          (member tool-name '("read" "grep" "find" "ls" "glob" "planning-read"))
          (plan-tool-budget))
     (define remaining (decrement-plan-budget!))
     (cond
       [(and remaining (<= remaining 0))
        (emit-gsd-event! "gsd.budget.warning" (hasheq 'budget-type 'exploration 'remaining 0))
        (hook-block (format "Exploration budget exhausted (~a calls used). Write your plan now."
                            EXPLORATION-BUDGET))]
       ;; Soft warning at 2/3 budget — pass through but will be warned via tracker
       [(and remaining (<= remaining 10)) (hook-pass payload)]
       [else (hook-pass payload)])]
    ;; During /go, enforce read budget
    [(and (eq? mode 'executing) (member tool-name READ-ONLY-TOOLS) (go-read-budget))
     (define remaining (decrement-budget!))
     (cond
       [(< remaining GO-READ-BLOCK-THRESHOLD)
        ;; Hard block: way over budget
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

(define-q-extension gsd-planning-extension
                    #:version "1.0.0"
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
