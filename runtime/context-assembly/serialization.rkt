#lang racket/base
(require racket/contract)

;; runtime/context-assembly/serialization.rkt — tiered context, entry conversion, agents discovery
;;
;; Wire format construction, tiered context, and file discovery.
;; v0.76.0 W2: Core tiered context building extracted to context-floor.rkt.
;;              State-aware assembly extracted to state-aware-builder.rkt.

(require racket/list
         racket/string
         racket/file
         racket/match
         (only-in "../../util/protocol-types.rkt"
                  message
                  message-id
                  message-kind
                  message-role
                  message-content
                  message-meta-safe
                  make-message
                  make-text-part)
         (only-in "../../util/content-parts.rkt" text-part text-part? text-part-text)
         (only-in "../context-policy.rkt" estimate-message-tokens)
         (only-in "../context-fit.rkt" truncate-messages-to-budget)
         (only-in "../../llm/provider.rkt" provider?)
         "../../skills/context-files.rkt"
         "session-walk.rkt")

;; Re-export from context-floor.rkt
(require "context-floor.rkt")
(provide (all-from-out "context-floor.rkt"))

;; Re-export from state-aware-builder.rkt
(require "state-aware-builder.rkt")
(provide (all-from-out "state-aware-builder.rkt"))

;; Remaining exports (not moved)
(provide build-session-context/tokens
         entry->context-message
         load-agents-context
         build-system-preamble
         truncate-messages-to-budget
         summarize-tool-result)

;; Token-aware context assembly
;; CA-05: Uses build-session-context from session-walk.rkt
(define (build-session-context/tokens idx #:max-tokens max-tokens)
  (define messages (build-session-context idx))
  (match messages
    ['() (values '() 0)]
    [_
     (define total (for/sum ([m (in-list messages)]) (estimate-message-tokens m)))
     (cond
       [(<= total max-tokens) (values messages total)]
       [else
        (define truncated (truncate-messages-to-budget messages max-tokens))
        (define new-total (for/sum ([m (in-list truncated)]) (estimate-message-tokens m)))
        (values truncated new-total)])]))

;; AGENTS.md context discovery
(define (load-agents-context working-directory)
  (define paths (discover-agents-files working-directory))
  (match paths
    ['() ""]
    [_
     (define contexts
       (for/list ([p (in-list paths)]
                  #:when (file-exists? p))
         (define content (file->string p))
         (parse-agent-file content)))
     (match contexts
       ['() ""]
       [_ (agent-context-instructions (merge-agent-contexts contexts))])]))

(define (build-system-preamble working-directory)
  (define paths (discover-agents-files working-directory))
  (match paths
    ['() ""]
    [_
     (define contexts
       (for/list ([p (in-list paths)]
                  #:when (file-exists? p))
         (define content (file->string p))
         (parse-agent-file content)))
     (match contexts
       ['() ""]
       [_
        (define merged (merge-agent-contexts contexts))
        (define name (agent-context-name merged))
        (define desc (agent-context-description merged))
        (define inst (agent-context-instructions merged))
        (define parts
          (filter (lambda (s) (not (string=? s "")))
                  (list (if (string=? name "Unnamed Agent")
                            ""
                            (format "# ~a" name))
                        desc
                        inst)))
        (string-join parts "\n\n")])]))

;; ============================================================
;; TEST-01: Isolated unit tests for gsd-progress-message?
;; ============================================================
(module+ test
  (require rackunit)

  (define (make-test-msg role text [meta (hasheq)])
    (make-message "test-id" #f role 'text (list (make-text-part text)) (current-seconds) meta))

  ;; Flag-based pinning
  (test-case "gsd-pin-flag-pins-message"
    (define m (make-test-msg 'assistant "wave done" (hasheq 'gsd-pin #t)))
    (check-true (gsd-progress-message? m)))

  (test-case "no-gsd-pin-flag-does-not-pin"
    (define m (make-test-msg 'assistant "regular message" (hasheq)))
    (check-false (gsd-progress-message? m)))

  ;; Regex-based pinning (fallback)
  (test-case "regex-matches-wave-complete"
    (define m (make-test-msg 'assistant "wave 3 marked complete"))
    (check-true (gsd-progress-message? m)))

  (test-case "regex-matches-wave-is-complete"
    (define m (make-test-msg 'tool "wave 5 is complete"))
    (check-true (gsd-progress-message? m)))

  (test-case "regex-matches-wave-done"
    (define m (make-test-msg 'assistant "wave 2 done"))
    (check-true (gsd-progress-message? m)))

  (test-case "regex-matches-plan-md-updated"
    (define m (make-test-msg 'tool "PLAN.md has been updated with wave 2 status"))
    (check-true (gsd-progress-message? m)))

  (test-case "regex-matches-state-md-created"
    (define m (make-test-msg 'assistant "STATE.md created"))
    (check-true (gsd-progress-message? m)))

  (test-case "regex-matches-handoff-json-written"
    (define m (make-test-msg 'assistant "HANDOFF.json written successfully"))
    (check-true (gsd-progress-message? m)))

  (test-case "regex-matches-milestone-complete"
    (define m (make-test-msg 'assistant "milestone v0.45.0 is now complete"))
    (check-true (gsd-progress-message? m)))

  (test-case "regex-matches-review-approved"
    (define m (make-test-msg 'assistant "review: APPROVED"))
    (check-true (gsd-progress-message? m)))

  ;; Negative cases
  (test-case "regex-no-match-user-role"
    (define m (make-test-msg 'user "wave 5 marked complete"))
    (check-false (gsd-progress-message? m)))

  (test-case "regex-no-match-non-gsd-text"
    (define m (make-test-msg 'assistant "I have completed the refactoring"))
    (check-false (gsd-progress-message? m)))

  ;; v0.75.7 W3: gsd-execution-instruction meta flag
  (test-case "gsd-execution-instruction-flag-pins-message"
    (define m (make-test-msg 'user "Implement the plan" (hasheq 'gsd-execution-instruction #t)))
    (check-true (gsd-progress-message? m)))

  (test-case "gsd-execution-instruction-flag-pins-any-role"
    (define m (make-test-msg 'system "Task context" (hasheq 'gsd-execution-instruction #t)))
    (check-true (gsd-progress-message? m)))

  (test-case "no-execution-instruction-flag-does-not-pin"
    (define m (make-test-msg 'user "Just a regular message"))
    (check-false (gsd-progress-message? m))))
