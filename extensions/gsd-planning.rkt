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
         json
         "define-extension.rkt"
         "dynamic-tools.rkt"
         "ext-commands.rkt"
         "context.rkt"
         "hooks.rkt"
         "../tools/tool.rkt")

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
         pinned-planning-dir)

;; ============================================================
;; Constants
;; ============================================================

(define planning-dir-name ".planning")

;; Pinned planning directory — set once during tool registration from ctx-cwd.
;; Ensures all planning-read/planning-write calls within a session use the
;; same .planning/ folder, even if the LLM guesses a different base_dir.
;; Uses a parameter so tests can override it per-scope.
(define pinned-planning-dir (make-parameter #f))

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

;; Planning preamble prepended to user text when /plan <text> submits.
;; Gives the agent explicit GSD planning instructions.
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
   "EXPLORATION BUDGET: Maximum 30 tool calls (read, grep, find, planning-read).\n"
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
   "User request: "))

(define artifact-extensions
  '(("PLAN" . ".md") ("STATE" . ".md")
                     ("HANDOFF" . ".json")
                     ("VALIDATION" . ".md")
                     ("BUG_REPORT" . ".md")
                     ("BUG_PLAN" . ".md")
                     ("BUG_STATE" . ".md")
                     ("BUG_VALIDATION" . ".md")
                     ("SUMMARY" . ".md")))

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
     (with-handlers ([exn:fail? (lambda (e) #f)])
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
                (make-success-result (list (hasheq 'type
                                                   "text"
                                                   'text
                                                   (format "Written: ~a"
                                                           (path->string result-path)))))))]))]))

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
      (pinned-planning-dir (resolve-project-root cwd))))
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
                 "3. Do NOT use planning-read or planning-write during implementation.\n"
                 "4. Do NOT run read-only tools (read, find, grep, ls) unless a wave's\n"
                 "   old-text match fails and you need to re-read the target file ONCE.\n"
                 "5. Use the edit or write tool for EVERY wave. Your budget is:\n"
                 "   - Max 2 read-only tool calls per wave before you MUST write/edit.\n"
                 "   - After 5 total read-only calls across all waves, STOP and summarize.\n"
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

(define-q-extension gsd-planning-extension
                    #:version "1.0.0"
                    #:api-version "1"
                    #:on register-tools
                    register-gsd-tools
                    #:on register-shortcuts
                    register-gsd-commands
                    #:on execute-command
                    handle-execute-command)

(define the-extension gsd-planning-extension)
