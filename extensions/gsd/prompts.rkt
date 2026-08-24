#lang racket/base

;; extensions/gsd/prompts.rkt — Prompt templates for all GSD phases
;;
;; Wave 3a of v0.21.0: Prompt templates that guide the LLM through
;; the GSD workflow phases.

(require racket/format
         racket/string
         "plan-types.rkt"
         "wave-executor.rkt")

(provide planning-prompt
         planning-implement-prompt
         executing-prompt
         wave-failure-prompt
         verifying-prompt
         status-prompt
         executor-reanchor-role-line
         executor-reanchor-prompt
         wave-failure-context-block)

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
   "6. After completing the assigned wave, run its verify command.
"
   "7. Do NOT call /wave-done; the runtime coordinator owns status transitions only.
"
   "   After you return, the coordinator verifies real delivery evidence (expected branch,
"
   "   changed wave files, passing verify command). Only that evidence marks the wave DONE.
"
   "
"
   "The plan follows. Start implementing immediately.\n"))
;; ============================================================
;; Exploring prompt
;; ============================================================

(define (planning-prompt user-request)
  (string-append
   "# GSD Planning Phase — Write-Immediately Workflow\n\n"
   "GOAL: Write wave documents efficiently.\n\n"
   "## RULES (non-negotiable)\n"
   "1. Read the PRIMARY target file ONCE — no offset needed, you get the full file.\n"
   "2. You may do 1–2 follow-up calls (grep a pattern, check a dependency).\n"
   "3. Then IMMEDIATELY write wave documents using planning-write.\n"
   "4. NEVER re-read a file you have already read.\n"
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
   "```\n"
   "## Root Cause\n"
   "<what causes the bug or what needs to change>\n\n"
   "## Files\n"
   "- File: path/to/file (relative to project root)\n"
   "- File: path/to/other\n\n"
   "## Action\n"
   "<what to do, include old-text/new-text for edits>\n\n"
   "## Verify\n"
   "<appropriate test command>\n\n"
   "## Done\n"
   "- <completion criteria>\n"
   "```\n\n"
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
