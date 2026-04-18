#lang racket/base

;; q/runtime/compaction-prompts.rkt — Prompt templates for LLM-powered compaction
;;
;; Provides structured summary prompt and iterative update prompt
;; for the compaction system.

(require racket/string)

(provide summary-prompt
         iterative-update-prompt
         MAX-TOOL-RESULT-CHARS
         MAX-SUMMARY-WORDS)

(define MAX-TOOL-RESULT-CHARS 2000)
(define MAX-SUMMARY-WORDS 500)

;; Helper: format file tracker section for inclusion in prompts
;; Uses explicit XML tags for machine-readable file tracking.
(define (file-tracker-section file-tracker)
  (if (and file-tracker (> (hash-count file-tracker) 0))
      (let ([reads (hash-ref file-tracker 'readFiles '())]
            [writes (hash-ref file-tracker 'modifiedFiles '())])
        (string-append
         "\n<file-tracker>\n"
         (if (pair? reads)
             (format "<read-files>\n~a\n</read-files>\n"
                     (string-join (for/list ([p (in-list reads)]) (format "  ~a" p)) "\n"))
             "")
         (if (pair? writes)
             (format "<modified-files>\n~a\n</modified-files>\n"
                     (string-join (for/list ([p (in-list writes)]) (format "  ~a" p)) "\n"))
             "")
         "</file-tracker>"))
      ""))

;; Prompt for initial summarization of messages
(define (summary-prompt formatted-messages file-tracker)
  (string-append
   "You are summarizing a coding assistant session. Produce a structured summary with these EXACT sections:\n\n"
   "## Goal\n<one-line description of what the user is trying to accomplish>\n\n"
   "## Constraints & Preferences\n- <key constraints, style requirements, or user preferences discovered>\n\n"
   "## Progress\n### Done\n- [x] <completed items>\n\n"
   "### In Progress\n- <current work if any>\n\n"
   "### Blocked\n- <blockers if any>\n\n"
   "## Key Decisions\n- <important decisions made during the session>\n\n"
   "## Next Steps\n1. <next actions the user or assistant should take>\n\n"
   "## Critical Context\n- <must-preserve information: file paths, variable names, API details, error states>\n\n"
   "RULES:\n"
   "- Maximum ~500 words\n"
   "- Preserve specific file paths, function names, variable names exactly\n"
   "- Include any error messages or states that are being debugged\n"
   "- Note any user preferences or constraints mentioned\n"
   "- Keep the Goal to ONE line\n"
   (file-tracker-section file-tracker)
   "\n\nSESSION MESSAGES:\n"
   formatted-messages))

;; Prompt for iterative update when a previous summary exists
(define (iterative-update-prompt previous-summary new-messages file-tracker)
  (string-append
   "You are updating an existing session summary with new information. Merge the new messages into the existing summary.\n\n"
   "EXISTING SUMMARY:\n"
   previous-summary
   "\n\nNEW MESSAGES SINCE LAST SUMMARY:\n"
   new-messages
   "\n\nProduce an UPDATED summary with these EXACT sections:\n"
   "## Goal\n"
   "## Constraints & Preferences\n"
   "## Progress\n### Done\n### In Progress\n### Blocked\n"
   "## Key Decisions\n"
   "## Next Steps\n"
   "## Critical Context\n\n"
   "RULES:\n"
   "- Maximum ~500 words\n"
   "- Preserve all information from existing summary that is still relevant\n"
   "- Add new information from new messages\n"
   "- Update Progress sections (move items between Done/In Progress/Blocked)\n"
   "- Remove resolved blockers\n"
   "- Update Next Steps to reflect current state\n"
   "- Keep the Goal to ONE line\n"
   (file-tracker-section file-tracker)))
