#lang racket

;;; tests/test-compaction-prompts.rkt — TDD tests for runtime/compaction-prompts.rkt
;;;
;;; Covers:
;;;   - Constants: MAX-TOOL-RESULT-CHARS, MAX-SUMMARY-WORDS
;;;   - summary-prompt structure and content
;;;   - iterative-update-prompt structure and content
;;;   - file-tracker inclusion when present
;;;   - file-tracker omission when empty or #f

(require rackunit
         (only-in "../runtime/compaction-prompts.rkt"
                  summary-prompt
                  iterative-update-prompt
                  MAX-TOOL-RESULT-CHARS
                  MAX-SUMMARY-WORDS))

;; ============================================================
;; Constants
;; ============================================================

(test-case "MAX-TOOL-RESULT-CHARS is 2000"
  (check-equal? MAX-TOOL-RESULT-CHARS 2000))

(test-case "MAX-SUMMARY-WORDS is 500"
  (check-equal? MAX-SUMMARY-WORDS 500))

;; ============================================================
;; summary-prompt
;; ============================================================

(test-case "summary-prompt includes session messages"
  (define result (summary-prompt "test messages here" #f))
  (check-true (string-contains? result "test messages here")))

(test-case "summary-prompt includes required sections"
  (define result (summary-prompt "msgs" #f))
  (check-true (string-contains? result "## Goal"))
  (check-true (string-contains? result "## Constraints & Preferences"))
  (check-true (string-contains? result "## Progress"))
  (check-true (string-contains? result "### Done"))
  (check-true (string-contains? result "### In Progress"))
  (check-true (string-contains? result "### Blocked"))
  (check-true (string-contains? result "## Key Decisions"))
  (check-true (string-contains? result "## Next Steps"))
  (check-true (string-contains? result "## Critical Context")))

(test-case "summary-prompt includes word limit rule"
  (define result (summary-prompt "msgs" #f))
  (check-true (string-contains? result "500 words")))

(test-case "summary-prompt omits file tracker when #f"
  (define result (summary-prompt "msgs" #f))
  (check-false (string-contains? result "<file-tracker>")))

(test-case "summary-prompt omits file tracker when empty hash"
  (define result (summary-prompt "msgs" (hasheq)))
  (check-false (string-contains? result "<file-tracker>")))

(test-case "summary-prompt includes file tracker when present"
  (define tracker (hasheq 'readFiles '("/tmp/foo.rkt") 'modifiedFiles '()))
  (define result (summary-prompt "msgs" tracker))
  (check-true (string-contains? result "<file-tracker>"))
  (check-true (string-contains? result "/tmp/foo.rkt")))

;; ============================================================
;; iterative-update-prompt
;; ============================================================

(test-case "iterative-update-prompt includes previous summary"
  (define result (iterative-update-prompt "old summary text" "new msgs" #f))
  (check-true (string-contains? result "old summary text")))

(test-case "iterative-update-prompt includes new messages"
  (define result (iterative-update-prompt "old" "new message content" #f))
  (check-true (string-contains? result "new message content")))

(test-case "iterative-update-prompt includes required sections"
  (define result (iterative-update-prompt "old" "new" #f))
  (check-true (string-contains? result "## Goal"))
  (check-true (string-contains? result "## Progress"))
  (check-true (string-contains? result "## Critical Context")))

(test-case "iterative-update-prompt includes merge instruction"
  (define result (iterative-update-prompt "old" "new" #f))
  (check-true (string-contains? result "Merge the new messages")))

(test-case "iterative-update-prompt includes file tracker when present"
  (define tracker (hasheq 'readFiles '() 'modifiedFiles '("/src/bar.rkt")))
  (define result (iterative-update-prompt "old" "new" tracker))
  (check-true (string-contains? result "<file-tracker>"))
  (check-true (string-contains? result "/src/bar.rkt")))

(test-case "iterative-update-prompt omits file tracker when #f"
  (define result (iterative-update-prompt "old" "new" #f))
  (check-false (string-contains? result "<file-tracker>")))
