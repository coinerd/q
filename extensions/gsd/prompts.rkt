#lang racket/base

;; extensions/gsd/prompts.rkt — Prompt templates for all GSD phases
;;
;; Wave 3a of v0.21.0: Prompt templates that guide the LLM through
;; the GSD workflow phases.

(require "supplemental-author-contract.rkt"
         racket/format
         racket/path
         racket/string
         "plan-types.rkt"
         "wave-executor.rkt"
         ;; W5: the ONE required wave-doc header/section contract. The authoring
         ;; template below is GENERATED from these constants, and
         ;; `lint-wave-doc` validates against the same values, so the template
         ;; and the validator cannot drift apart (R-003/R-015).
         (only-in "wave-docs.rkt"
                  WAVE-DOC-HEADING-PREFIX
                  WAVE-DOC-STATUS-PREFIX
                  WAVE-DOC-FILE-LINE-PREFIX
                  WAVE-DOC-FILES-SECTION
                  WAVE-DOC-VERIFY-SECTION
                  WAVE-DOC-DONE-SECTION
                  WAVE-DOC-INITIAL-STATUS)
         ;; W2: the ONE trusted per-session scratch allocation.
         "../../tools/scratch.rkt"
         ;; W2 review: safe mode is read-only, so scratch guidance must say so.
         (only-in "../../util/safe-mode/safe-mode-predicates.rkt" safe-mode?))

(provide planning-prompt
         planning-implement-prompt
         executing-prompt
         wave-failure-prompt
         verifying-prompt
         status-prompt
         executor-reanchor-role-line
         executor-reanchor-prompt
         wave-failure-context-block
         verification-repair-context-block
         wave-attempt-context-block
         wave-retry-context
         scratch-guidance-section)

;; ============================================================
;; Executor scratch guidance (W2 single trusted allocation)
;; ============================================================

;; Pure render of the executor-owned scratch-directory contract. The runtime
;; allocates ONE opaque root per agent session (tools/scratch.rkt) below the
;; platform temporary directory; the same root is used by direct and sandboxed
;; write/edit and by the bash redirection policy for EXACTLY that session.
;;
;; Safe mode renders the path with an explicit read-only disclaimer and no
;; write/redirection instructions, because safe mode blocks bash/edit/write and
;; the structured tools refuse the scratch root there.
;;
;; `absolute-root` is the already-allocated root the caller is about to bind to
;; the executor session (the only value advertised when it is a trusted scratch
;; path). `scratch-token` is the opaque per-/go token and is used only to
;; resolve (or, on first use, allocate) the root keyed by that token, so an
;; already allocated root is preserved instead of moved. Nothing here falls
;; back to the scratch base directory: without a trusted root or token there is
;; no scratch section at all.
;;
;; A legacy `.planning/scratch/...` value is never advertised.
(define (scratch-guidance-section scratch-token absolute-root)
  (define root
    (cond
      [(trusted-session-scratch-path? absolute-root)
       (ensure-scratch-root! (simplify-path (path->complete-path absolute-root) #f))]
      [(and (string? scratch-token) (positive? (string-length scratch-token)))
       (or (allocated-session-scratch-root scratch-token)
           (allocate-session-scratch-root! scratch-token))]
      [else #f]))
  (if (not root)
      ""
      (let ([root-str (path->string root)])
        ;; W2 review: safe mode is READ-ONLY (bash/edit/write are blocked and the
        ;; structured tools refuse this root). Render the path plus an explicit
        ;; read-only disclaimer and NO write/redirection instructions, instead of
        ;; telling a safe-mode executor to write where it cannot. Normal sessions
        ;; render the section exactly as before.
        (if (safe-mode?)
            (string-append
             "\nSession scratch directory (single trusted allocation, disposable):\n"
             (format "- Path: `~a`\n" root-str)
             "- Safe mode is read-only: this path is NOT writable here (write/edit are\n"
             "  blocked and read/write/edit refuse it); do not plan scratch work in this run\n"
             "- Scratch is disposable: never reference it as a wave deliverable, approval, release or\n"
             "  verification receipt, and never commit it\n")
            (string-append
             "\nSession scratch directory (single trusted allocation, disposable):\n"
             (format "- Path: `~a`\n" root-str)
             "- Create diagnostic scripts and probes ONLY here, with the structured write or\n"
             "  edit tool; missing parent directories are created automatically\n"
             "- Run probes with read-only bash (`racket file`); the SAME root is the only\n"
             (format "  sanctioned output-redirection target (e.g. `> ~a/probe.log`)\n" root-str)
             "- Do not create scratch under `.planning/`, inside the repository, or under any\n"
             "  other /tmp path: those writes stay denied or pollute delivery evidence\n"
             "- Scratch is disposable: NEVER reference it as a wave deliverable, approval, release or\n"
             "  verification receipt, and never commit it\n"
             "- No cleanup obligation; do not delete other sessions' scratch directories\n")))))

;; ============================================================
;; Executor re-anchor prompt (v1.00.17 W3 — #9514)
;; ============================================================

;; The executor role, stated once. `executor-reanchor-prompt` restates this
;; VERBATIM so an empty-response retry cannot drift into interactive-assistant
;; behavior (v1.00.16 W3 attempt-2 asked "What would you like me to do next?").
(define executor-reanchor-role-line
  (string-append "You are the WAVE EXECUTOR of a GSD /go campaign. Your ONLY job is to "
                 "implement the assigned wave: edit the target files, then run the verify "
                 "command. You are NOT an interactive assistant. Do not ask the user "
                 "questions. Do not propose options. Do not read STATE/HANDOFF/other "
                 "planning artifacts. Continue the implementation now."))

;; Pure constructor: (wave-id campaign-id task-line last-tool-result-excerpt)
;; → prompt that restates the executor role verbatim and ORDERS continuation
;; of implementation. No I/O.
(define (executor-reanchor-prompt wave-id campaign-id task-line last-tool-result-excerpt)
  (if (current-supplemental-author-contract)
      (render-supplemental-author-prompt)
      (ordinary-executor-reanchor-prompt wave-id campaign-id task-line last-tool-result-excerpt)))
(define (ordinary-executor-reanchor-prompt wave-id campaign-id task-line last-tool-result-excerpt)
  (string-append "[SYSTEM — EXECUTOR RE-ANCHOR — NOT A USER MESSAGE]\n\n"
                 (format "Campaign: ~a\n" campaign-id)
                 (format "Wave: ~a\n" wave-id)
                 (format "Task (one line): ~a\n\n" task-line)
                 "Your previous turn ended with reasoning but produced NO visible output, "
                 "so the runtime is re-anchoring you.\n\n"
                 "ROLE (binding, verbatim):\n"
                 executor-reanchor-role-line
                 "\n\n"
                 (format "Last tool result (excerpt):\n~a\n\n"
                         (if (and (string? last-tool-result-excerpt)
                                  (non-empty-string? (string-trim last-tool-result-excerpt)))
                             (string-trim last-tool-result-excerpt)
                             "(none — no tool ran)"))
                 "IMMEDIATELY continue where you left off: perform the next concrete "
                 "implementation action (an edit or the verify command). Do not summarize, "
                 "do not ask what to do next, do not re-read the plan. Act now."))

;; Pure constructor: (verifier-message target-files) → failure-context block
;; appended to the wave executor prompt for the bounded no-change retry
;; (v1.00.17 W3 — #9515).
(define (wave-failure-context-block verifier-message target-files)
  (string-append "\n\n=== PREVIOUS ATTEMPT FAILED VERIFICATION — RETRY WITH CONTEXT ===\n"
                 "Verifier message (verbatim):\n"
                 (format "~a\n\n" verifier-message)
                 "Declared wave target files:\n"
                 (string-join (for/list ([f (in-list (if (list? target-files)
                                                         target-files
                                                         '()))])
                                (format "- ~a" f))
                              "\n")
                 "\n\n"
                 "Your previous attempt made ZERO edits to the declared wave target files. "
                 "That is why verification failed. On this retry you MUST produce at least "
                 "one real edit to a declared target file: read the file you are editing "
                 "(only the one you are about to edit), then apply the first edit now."))

;; BUG-0060: bounded coordinator-verification diagnostics for a same-wave
;; repair attempt. The verifier message includes the exact command, exit code,
;; bounded failed-output summary, and durable log path when the owned delivery
;; verifier produced it. The successor must repair and then return control so
;; the coordinator reruns the COMPLETE declared Verify chain.
(define (verification-repair-context-block verifier-message)
  (define bounded
    (let ([s (if (string? verifier-message)
                 verifier-message
                 (format "~a" verifier-message))])
      (if (> (string-length s) 6144)
          (string-append (substring s 0 6144) "\n...[verification diagnostics truncated]")
          s)))
  (string-append
   "\n\n=== COORDINATOR VERIFICATION FAILED — REPAIR THIS SAME WAVE ===\n"
   "Bounded verifier diagnostics (verbatim):\n"
   bounded
   "\n\nPreserve the existing implementation and checkpoints. Fix the reported failure now. "
   "The first compiler error may hide other missing deliverables. Check the frozen wave's "
   "declared Files and Done criteria and implement the remaining modules/APIs before returning. "
   "Do not change the tests or Verify command merely to hide missing implementation. "
   "Focused checks may guide the repair, but completion requires returning to the coordinator, "
   "which will rerun the COMPLETE declared Verify chain. Do not claim success from a focused check alone."))

;; Pure combination: (latest-durable-reason transient-infra-context) → the
;; failure context for the NEXT same-wave attempt. Transient provider/infra
;; context SUPPLEMENTS the durable verifier diagnosis instead of masking it
;; (`or` previously erased the repair obligation). The caller rebuilds this
;; from the single latest durable reason each run, so no retry history is
;; recursively accumulated. #f when both are absent/empty.
(define (wave-retry-context durable transient)
  (cond
    [(and (non-empty-string? durable) (non-empty-string? transient))
     (string-append durable "\n" transient)]
    [(non-empty-string? durable) durable]
    [else (and (non-empty-string? transient) transient)]))

;; Pure constructor: (prior-attempt-context) → context block PREFIXED to the
;; wave executor prompt for any non-first attempt of the same wave after an
;; automatic campaign-level infra retry (v1.00.18 BUG-0024 W3). Carried through
;; current-gsd-wave-failure-context so it reuses the existing prompt plumbing —
;; no parallel state. Empty context → #f (no block).
(define (wave-attempt-context-block prior-context)
  (if (or (not prior-context) (zero? (string-length prior-context)))
      #f
      (string-append
       "=== PRIOR ATTEMPT CONTEXT (infra failure — resume, do NOT restart from zero) ===\n"
       prior-context
       "\n=== END PRIOR ATTEMPT CONTEXT ===\n\n")))

;; ============================================================
;; Planning implement prompt
;; ============================================================

(define planning-implement-prompt
  (string-append
   "[gsd-planning] EXECUTE the plan below. IMPLEMENT NOW — do NOT explore.
"
   "
"
   "CRITICAL RULES:
"
   "1. Do NOT re-read the plan. It is provided below in full.
"
   "2. Do NOT write a new plan. Execute the existing one.
"
   "3. Do NOT use planning-write during implementation.
"
   "   planning-read is allowed to check STATE or VALIDATION.
"
   "4. Read each target file BEFORE editing it. You need the current content
"
   "   to apply edits correctly. Read is necessary and expected.
"
   "5. After reading, apply the edits specified in the wave doc actions.
"
   "6. Run focused tests while implementing, but do NOT launch the wave's long Verify command through bash.
"
   "   After you return, the coordinator runs that exact declaration through the owned verification lane.
"
   "7. Do NOT call /wave-done; the runtime coordinator owns status transitions only.
"
   "   It verifies the expected branch, changed wave files, and the owned gate's terminal result.
"
   "   Only that evidence marks the wave DONE.
"
   "8. Scratch files (BUG-0026): for throwaway experiments use the edit tool to create files under `tmp/`, run them, then delete them.
"
   "
"
   "The plan follows. Start implementing immediately.\n"))
;; ============================================================
;; Exploring prompt
;; ============================================================

;; ============================================================
;; Wave-doc authoring template (generated from the ONE contract)
;; ============================================================

;; The fenced wave-doc template emitted by planning-prompt. Every part the
;; wave-doc lint enforces (machine header, `- File:` bullet shape, required
;; section names) is taken from `wave-docs.rkt`, never re-typed here. The
;; narrative bodies stay literal because they are instructions, not contract.
(define (wave-doc-authoring-template)
  (string-append "```\n"
                 WAVE-DOC-HEADING-PREFIX
                 "0\n"
                 WAVE-DOC-STATUS-PREFIX
                 WAVE-DOC-INITIAL-STATUS
                 "\n"
                 "\n"
                 "## Root Cause\n"
                 "<what causes the bug or what needs to change>\n\n"
                 "## "
                 WAVE-DOC-FILES-SECTION
                 "\n"
                 WAVE-DOC-FILE-LINE-PREFIX
                 "path/to/file (relative to project root)\n"
                 WAVE-DOC-FILE-LINE-PREFIX
                 "path/to/other\n\n"
                 "## Action\n"
                 "<what to do, include old-text/new-text for edits>\n\n"
                 "## "
                 WAVE-DOC-VERIFY-SECTION
                 "\n"
                 "<appropriate test command>\n\n"
                 "## "
                 WAVE-DOC-DONE-SECTION
                 "\n"
                 "- <completion criteria>\n"
                 "```\n\n"))

(define (planning-prompt user-request)
  (string-append
   "# GSD Planning Phase — Write-Immediately Workflow\n\n"
   "GOAL: Write wave documents efficiently.\n\n"
   "## RULES (non-negotiable)\n"
   "1. Read the PRIMARY target file with bounded read calls (each call returns at most one page, default 2000 lines / 50000 bytes). "
   "If a read result ends with a truncation notice, follow its suggested offset to continue the file.\n"
   "2. You may do 1–2 follow-up calls (grep a pattern, check a dependency).\n"
   "3. Then IMMEDIATELY write wave documents using planning-write.\n"
   "4. Do not re-read a file without a reason; rereading for continuation is allowed and expected "
   "when a read ends with a truncation notice (use the suggested offset).\n"
   "5. NEVER investigate tangential concerns (package versions, font systems, etc.).\n"
   "6. NEVER run the target script — focus on source code analysis only.\n"
   "7. CRITICAL ORDERING: You MUST write ALL wave documents (STEP 2) BEFORE writing PLAN.md (STEP 3).\n"
   "   If PLAN.md is written without corresponding wave files, /go will fail.\n"
   "8. After writing each wave doc, verify with: planning-read artifact=\"waves/W0-slug.md\"\n"
   "9. Read the files you need, then IMMEDIATELY write wave documents.\n"
   "    Do NOT read more than 8 files total — read less, write more.\n\n"
   "10. CRITICAL: For ANY file in the .planning/ directory, you MUST use\n"
   "    planning-read artifact=\"NAME\" — NEVER use the read tool for .planning/ files.\n"
   "    Examples: planning-read artifact=\"PLAN\", planning-read artifact=\"STATE\",\n"
   "    planning-read artifact=\"waves/W0-slug.md\", planning-read artifact=\"IMPROVE_VISUALS.md\"\n\n"
   "## STEP 1: Read (1–2 calls)\n"
   "Read ONLY the primary source file(s) mentioned in the request.\n"
   "Use filesystem paths relative to CWD, NOT import/module paths.\n"
   "Example: if CWD is /project/ and you see 'from package.module import X',\n"
   "the file is at module.py (NOT package/package/module.py).\n"
   "Use ls or find to locate files if unsure of the exact path.\n"
   "Identify: root cause, exact file paths, line numbers.\n\n"
   "## STEP 2: Write Wave Documents (the MAIN work — MANDATORY)\n"
   "MANDATORY: You MUST write wave documents. Without wave files, /go will fail.\n"
   "Write ALL wave documents first, then write PLAN.md last.\n"
   "For EACH wave, write a SEPARATE file using planning-write:\n"
   "  planning-write artifact=\"waves/W0-short-title.md\" content=\"...\"\n"
   "  planning-write artifact=\"waves/W1-short-title.md\" content=\"...\"\n"
   "If you have 3 waves, you MUST make 3 SEPARATE planning-write calls.\n\n"
   "### Wave Doc Format (use EXACTLY this syntax):\n"
   (wave-doc-authoring-template)
   "MANDATORY HEADER: every wave doc MUST begin with the two header lines shown above:\n"
   (format "exactly ONE `~a<N>` H1 followed by exactly ONE `~a~a` line. `Wave 0` is\n"
           WAVE-DOC-HEADING-PREFIX
           WAVE-DOC-STATUS-PREFIX
           WAVE-DOC-INITIAL-STATUS)
   "shown for W0 — for every other wave SUBSTITUTE the wave index in the H1 (`# Wave 2`\n"
   "for W2, `# Wave 3` for W3, ...). planning-write stores the content VERBATIM: it does\n"
   "NOT add this header for you, and NO second `Status:` line may appear anywhere in the\n"
   "document.\n\n"
   "REQUIRED SECTIONS: every wave doc needs ALL of "
   (string-join (for/list ([name (in-list (list WAVE-DOC-FILES-SECTION
                                                WAVE-DOC-VERIFY-SECTION
                                                WAVE-DOC-DONE-SECTION))])
                  (format "`## ~a`" name))
                ", ")
   " (the same list /go lints); a missing section refuses the wave.\n\n"
   "IMPORTANT: Use `- File: <path>` (singular, one per line) for file references.\n"
   "NOT 'Files:', not prose, not a heading without the dash prefix.\n\n"
   "FINALIZATION: Do NOT write `gh-wave-start` / `gh-wave-finish` / `gh-board` Action\n"
   "steps into wave docs. `gh-wave-finish` is QUARANTINED by security policy and always\n"
   "fails closed (the external authenticated PR workflow is the sole finalization\n"
   "authority). Waves deliver file-level changes + a scoped Verify command; the\n"
   "coordinator handles branch/PR/merge/board bookkeeping externally.\n\n"
   "## STEP 3: Write PLAN.md Index\n"
   "Write PLAN.md with:\n"
   "```\n"
   "# Plan: <title>\n"
   "## Overview\n"
   "<2-3 sentence description>\n"
   "## Waves\n"
   "- [Inbox] W0: <title> → waves/W0-slug.md\n"
   "- [Inbox] W1: <title> → waves/W1-slug.md\n"
   "## Constraints\n"
   "- <constraints>\n"
   "```\n\n"
   "## STEP 4: Finish\n"
   "Tell the user: 'Use /go to start implementing.'\n"
   "Do NOT implement — only plan.\n\n"
   "IMPORTANT: [SYSTEM NOTICE: ...] messages are from the runtime, not the user.\n\n"
   (if (and (string? user-request) (non-empty-string? user-request))
       (format "User request: ~a\n" user-request)
       "")))

;; ============================================================
;; Executing prompt
;; ============================================================

(define (executing-prompt plan executor)
  (define waves (gsd-plan-waves plan))
  (define next-idx (next-pending-wave executor))
  (define wave-count (length waves))
  (string-append
   "# GSD Execution Phase\n\n"
   (format "Executing plan with ~a waves. Starting from wave ~a.\n\n" wave-count (or next-idx 0))
   "CRITICAL — Runtime-owned wave checkpointing:\n"
   "- Complete ONE assigned wave at a time: read → edit → verify → format → syntax-check\n"
   "- Do NOT call /wave-done; the runtime coordinator alone verifies and commits status\n"
   "- Return after the assigned wave; a normal response is not itself a DONE commit\n"
   "- The coordinator starts later waves only after the current commit succeeds\n\n"
   "Other instructions:\n"
   "- Follow the plan strictly — do not expand scope\n"
   "- If a wave fails, use `/skip <N>` to skip it and proceed\n"
   "- Use `/replan` if the plan needs fundamental changes\n"
   "- Run verify commands after each wave\n\n"
   "Error recovery:\n"
   "- Failed waves do NOT block subsequent waves\n"
   "- Skip failed waves and document the reason\n"
   "- After all waves, report which ones failed\n\n"
   "Edit rules (non-negotiable):\n"
   "- For removing 3+ consecutive lines, prefer delete-lines (specify start/end line numbers)\n"
   "- Keep each edit ≤20 lines — split large changes into sequential edits\n"
   "- Keep oldText ≤2000 characters — include just enough surrounding context for uniqueness\n"
   "- Verify oldText is unique in the file before editing\n"
   "- For Racket files, prefer racket_edit over raw edit for structural changes\n"
   "- After each edit, run format + syntax check before proceeding\n\n"
   "Wave overview:\n"
   (format-wave-list waves)))

;; ============================================================
;; Wave failure prompt
;; ============================================================

(define (wave-failure-prompt wave-idx reason)
  (string-append (format "# Wave ~a Failed\n\n" wave-idx)
                 (format "~a\n\n" (or reason "Unknown error"))
                 "Skip this wave and continue to the next wave. "
                 "Do NOT retry the same approach. Document what went wrong.\n"
                 (format "Use `/skip ~a` if not already done.\n" wave-idx)
                 "Proceed to the next pending wave.\n"))

;; ============================================================
;; Verifying prompt
;; ============================================================

(define (verifying-prompt plan executor)
  (define waves (gsd-plan-waves plan))
  (string-append "# GSD Verification Phase\n\n"
                 "Run all verify commands from completed waves. Report PASS/FAIL per wave.\n\n"
                 "For each wave:\n"
                 "1. Run the verify command specified in the plan\n"
                 "2. Report the result as PASS or FAIL\n"
                 "3. If FAIL, note what went wrong\n\n"
                 "Wave verify commands:\n"
                 (format-verify-list waves)))

;; ============================================================
;; Status prompt
;; ============================================================

(define (status-prompt mode executor)
  (define statuses (wave-executor-statuses executor))
  (define summary (wave-summary executor))
  (format "# GSD Status\n\nMode: ~a\n\n~a\n" mode summary))

;; ============================================================
;; Internal helpers
;; ============================================================

(define (format-wave-list waves)
  (string-join (for/list ([w waves])
                 (format "- Wave ~a: ~a (~a files)"
                         (gsd-wave-index w)
                         (gsd-wave-title w)
                         (length (gsd-wave-files w))))
               "\n"))

(define (format-verify-list waves)
  (string-join (for/list ([w waves])
                 (format "- Wave ~a: ~a"
                         (gsd-wave-index w)
                         (if (non-empty-string? (gsd-wave-verify w))
                             (gsd-wave-verify w)
                             "(no verify command)")))
               "\n"))
