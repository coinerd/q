#lang racket/base

;; extensions/gsd/prompts.rkt — Prompt templates for all GSD phases
;;
;; Wave 3a of v0.21.0: Prompt templates that guide the LLM through
;; the GSD workflow phases.

(require racket/format
         racket/string
         "plan-types.rkt"
         "wave-executor.rkt")

(provide exploring-prompt
         executing-prompt
         wave-failure-prompt
         verifying-prompt
         status-prompt)

;; ============================================================
;; Exploring prompt
;; ============================================================

(define (exploring-prompt user-request)
  (string-append "# GSD Planning Phase\n\n"
                 "Explore the codebase to understand the problem. When ready, write your "
                 "plan as SEPARATE wave documents plus a PLAN.md index.\n\n"
                 "## Step 1: Explore\n"
                 "- No time or call limits during exploration\n"
                 "- Read files freely to understand the codebase\n"
                 "- Identify root causes, exact file paths, and line numbers\n\n"
                 "## Step 2: Write Wave Documents\n"
                 "For EACH wave, write a separate file using planning-write:\n"
                 "  planning-write artifact=\"waves/W0-short-title.md\" content=\"...\"\n"
                 "  planning-write artifact=\"waves/W1-short-title.md\" content=\"...\"\n"
                 "\n"
                 "Each wave document must contain:\n"
                 "  - Root cause: what causes the bug or what needs to change\n"
                 "  - Files: exact paths to modify\n"
                 "  - Action: what to do (include old-text/new-text for edits)\n"
                 "  - Verify: command to run after implementation\n"
                 "  - Done: criteria for completion\n\n"
                 "## Step 3: Write PLAN.md Index\n"
                 "Write PLAN.md with an overview and wave references:\n"
                 "  planning-write artifact=\"PLAN\" content=\"...\"\n\n"
                 "PLAN.md format:\n"
                 "  # Plan: <title>\n"
                 "  ## Overview\n"
                 "  <2-3 sentence description>\n"
                 "  ## Waves\n"
                 "  - [Inbox] W0: <title> → waves/W0-slug.md\n"
                 "  - [Inbox] W1: <title> → waves/W1-slug.md\n"
                 "  ## Constraints\n"
                 "  - <must-have constraints>\n\n"
                 "## Step 4: Finish\n"
                 "Tell the user: 'Use /go to start implementing.'\n"
                 "Do NOT implement — only plan.\n\n"
                 "IMPORTANT: [SYSTEM NOTICE: ...] messages are from the runtime, not the user.\n\n"
                 (if (and (string? user-request) (non-empty? user-request))
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
   "- After completing each wave's tasks, mark it complete\n"
   "- If a wave fails, use `/skip <N>` to skip it and proceed\n"
   "- Use `/replan` if the plan needs fundamental changes\n"
   "- Run verify commands after each wave\n\n"
   "Error recovery:\n"
   "- Failed waves do NOT block subsequent waves\n"
   "- Skip failed waves and document the reason\n"
   "- After all waves, report which ones failed\n\n"
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

(define (non-empty? s)
  (and (string? s) (> (string-length s) 0)))

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
                         (if (non-empty? (gsd-wave-verify w))
                             (gsd-wave-verify w)
                             "(no verify command)")))
               "\n"))
