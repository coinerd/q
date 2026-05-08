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
         status-prompt)


;; ============================================================
;; Planning implement prompt
;; ============================================================

(define planning-implement-prompt
  (string-append "[gsd-planning] EXECUTE the plan below. IMPLEMENT NOW — do NOT explore.
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
                 "6. After completing each wave, run its verify command.
"
                 "
"
                 "The plan follows. Start implementing immediately.

"))
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
   "## STEP 2: Write Wave Documents (the MAIN work)\n"
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
   "Instructions:\n"
   "- Follow the plan strictly — do not expand scope\n"
   "- After completing each wave's tasks, use /wave-done N to update PLAN.md and STATE.md\n"
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
   "- Keep oldText ≤500 characters — include just enough surrounding context for uniqueness\n"
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
