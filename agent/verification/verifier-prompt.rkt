#lang racket/base

;; agent/verification/verifier-prompt.rkt — Verifier prompt builder
;; STABILITY: evolving
;;
;; W3 (v0.99.5): Pure functions that build structured system and user
;; prompts for the verifier LLM call.
;;
;; Design:
;;   - build-verifier-system-prompt: constant instructions that tell the
;;     LLM to respond ONLY with JSON matching the verifier-decision schema.
;;   - build-verifier-user-message: assembles plan context, wave name,
;;     files changed, test summary, and diff excerpt into a reviewable string.
;;
;; Part of MAS Schritt 3: Verifier-Agent (milestone #791).

(require racket/contract
         racket/format
         racket/string
         (only-in racket/list take))

;; ============================================================
;; Configurable Parameters (L1 fix v0.99.7)
;; ============================================================

;; Maximum number of files shown in the user message before truncation.
(define current-verifier-max-files-shown (make-parameter 30))

;; Maximum length of the diff excerpt in characters.
(define current-verifier-max-diff-chars (make-parameter 8000))

;; Maximum number of lines shown for each file entry.
(define current-verifier-max-file-lines (make-parameter 3))

;; ============================================================
;; System Prompt
;; ============================================================

;; The constant system prompt instructs the verifier LLM to produce
;; ONLY a JSON object matching the verifier-decision schema.
(define (build-verifier-system-prompt)
  (string-append "You are the Verifier agent in a multi-agent system.\n"
                 "Your job is to review code changes and test results from a wave\n"
                 "and decide whether they are safe to merge.\n\n"
                 "You MUST respond with a single JSON object and NOTHING else.\n"
                 "Do NOT include markdown, prose, or explanation outside the JSON.\n\n"
                 "The JSON object MUST have this exact schema:\n\n"
                 "{\n"
                 "  \"verdict\": \"approve\" | \"reject\" | \"escalate\",\n"
                 "  \"reason\": \"<one-sentence justification>\",\n"
                 "  \"risk_level\": \"low\" | \"medium\" | \"high\",\n"
                 "  \"requires_human\": true | false,\n"
                 "  \"artifact_refs\": [\"<file1>\", ...],\n"
                 "  \"timestamp\": null\n"
                 "}\n\n"
                 "Rules:\n"
                 "- \"approve\": changes are safe, tests pass, no regressions.\n"
                 "- \"reject\": tests fail, contracts broken, or security issues.\n"
                 "- \"escalate\": uncertain — needs human review.\n"
                 "- requires_human MUST be true when verdict is \"escalate\".\n"
                 "- risk_level MUST reflect the severity of potential issues.\n"
                 "- artifact_refs MUST list the files you reviewed.\n"))

;; ============================================================
;; User Message
;; ============================================================

;; Truncate a string to at most n characters, appending an ellipsis
;; if truncation occurred.
(define (truncate-string s max-chars)
  (if (<= (string-length s) max-chars)
      s
      (string-append (substring s 0 max-chars) "\n... [truncated]")))

;; Format the files-changed list, truncating if too many files.
(define (format-files-changed files)
  (define max-files (current-verifier-max-files-shown))
  (define count (length files))
  (if (> count max-files)
      (string-append (string-join (take files max-files) "\n")
                     (format "\n... and ~a more file(s) not shown" (- count max-files)))
      (string-join files "\n")))

;; Build the user message for the verifier LLM.
;; All keyword arguments are required except diff-excerpt which
;; defaults to an empty string.
(define (build-verifier-user-message #:plan-summary plan-summary
                                     #:wave-name wave-name
                                     #:files-changed files-changed
                                     #:test-summary test-summary
                                     #:diff-excerpt [diff-excerpt ""])
  (define file-list-str
    (if (null? files-changed)
        "(none)"
        (format-files-changed files-changed)))
  (define diff-str
    (if (or (not diff-excerpt) (string=? diff-excerpt ""))
        "(no diff provided)"
        (truncate-string diff-excerpt (current-verifier-max-diff-chars))))
  (string-append (format "Wave: ~a\n" wave-name)
                 (format "Plan Summary: ~a\n" plan-summary)
                 (format "\nFiles Changed (~a):\n~a\n" (length files-changed) file-list-str)
                 (format "\nTest Results:\n~a\n" test-summary)
                 (format "\nDiff Excerpt:\n~a\n" diff-str)))

;; ============================================================
;; Provides
;; ============================================================

;; L1 fix: Export configurable parameters
(provide current-verifier-max-files-shown
         current-verifier-max-diff-chars
         current-verifier-max-file-lines)

(provide (contract-out [build-verifier-system-prompt (-> string?)]
                       [build-verifier-user-message
                        (->* (#:plan-summary string?
                                             #:wave-name string?
                                             #:files-changed (listof string?)
                                             #:test-summary string?)
                             (#:diff-excerpt string?)
                             string?)]))
