#lang racket/base

;; tools/builtins/spawn-subagent-helpers.rkt — Pure functions for spawn-subagent
;;
;; W2 v0.99.35: Extracted from spawn-subagent.rkt to create a clean boundary
;; between pure request/result normalization and effectful process orchestration.
;;
;; All functions in this module are pure (no I/O, no mutation, no parameters).
;; They can be tested in isolation without mocking providers or event buses.
;;
;; Boundary contract:
;;   INPUTS:  Plain data (hashes, lists, strings, symbols, message structs)
;;   OUTPUTS: Plain data (strings, lists of symbols, normalized hashes)
;;   EFFECTS: None — safe to call from any context

(require racket/string
         racket/list
         (only-in "../../util/capability.rkt" valid-capability?)
         (only-in "../../util/message/message.rkt" make-message message-role message-content)
         (only-in "../../util/content/content-parts.rkt"
                  text-part?
                  text-part-text
                  tool-call-part?
                  tool-call-part-name))

(provide normalize-capabilities
         requires-hitl-approval?
         extract-assistant-text
         extract-text-summary
         SUBAGENT-SUMMARY-MAX-CHARS)

;; ============================================================
;; Capability normalization
;; ============================================================

;; Normalize raw capability input to a validated list of symbols, or #f.
;;
;; Accepts: #f, '(), a single string/symbol, or a list of strings/symbols.
;; Returns: #f when no valid capabilities remain, or a list of symbols.
;;
;; This deduplicates the logic previously inlined in both:
;;   - parse-subagent-config (spawn-subagent.rkt)
;;   - parse-job-capabilities (spawn-subagent.rkt)
(define (normalize-capabilities caps-raw)
  (cond
    [(not caps-raw) #f]
    [(null? caps-raw) #f]
    [(string? caps-raw)
     (define sym (string->symbol caps-raw))
     (if (valid-capability? sym)
         (list sym)
         #f)]
    [(list? caps-raw)
     (define filtered
       (filter valid-capability?
               (map (lambda (c)
                      (if (string? c)
                          (string->symbol c)
                          c))
                    caps-raw)))
     (if (null? filtered) #f filtered)]
    [else #f]))

;; ============================================================
;; HITL approval check
;; ============================================================

;; Check if subagent capabilities require HITL approval.
;; Returns #t when capabilities include shell-exec or git-write.
;; Returns #f for #f, '(), or read-only/file-write-only capabilities.
(define (requires-hitl-approval? capabilities)
  (and (list? capabilities)
       (pair? capabilities)
       (or (memq 'shell-exec capabilities) (memq 'git-write capabilities))))

;; ============================================================
;; Result text extraction
;; ============================================================

;; Extract text content from assistant messages in a message list.
;;
;; For each assistant message:
;;   - String content → the string
;;   - List content → extract strings and text-parts, join with \n
;;   - Tool-call-only messages → summarize tool names as [called: tool1, tool2]
;;
;; Non-assistant messages (system, user, tool) are skipped.
;; Multiple assistant messages are joined with \n.
(define (extract-assistant-text messages)
  (string-join (for/list ([m (in-list messages)]
                          #:when (eq? (message-role m) 'assistant))
                 (define content (message-content m))
                 (cond
                   [(string? content) content]
                   [(list? content)
                    (define strings-only
                      (for/list ([c (in-list content)]
                                 #:when (string? c))
                        c))
                    (define text-parts
                      (for/list ([c (in-list content)]
                                 #:when (text-part? c))
                        (text-part-text c)))
                    (define tool-parts
                      (for/list ([c (in-list content)]
                                 #:when (tool-call-part? c))
                        (tool-call-part-name c)))
                    (define all-text (append strings-only text-parts))
                    (cond
                      [(pair? all-text) (string-join all-text "\n")]
                      [(pair? tool-parts) (format "[called: ~a]" (string-join tool-parts ", "))]
                      [else ""])]
                   [else (format "~a" content)]))
               "\n"))

;; ============================================================
;; Text summary truncation
;; ============================================================

;; Maximum characters in subagent result summaries.
;; Bug fix: Raised from 200→4000 chars — the old 200-char limit destroyed
;; nearly all useful content from subagent tasks.
(define SUBAGENT-SUMMARY-MAX-CHARS 4000)

;; Extract a text summary from tool result content.
;; Truncates to max-chars characters with ellipsis.
(define (extract-text-summary content [max-chars SUBAGENT-SUMMARY-MAX-CHARS])
  (define full-text
    (string-join (for/list ([c (in-list (if (list? content)
                                            content
                                            '()))])
                   (cond
                     [(and (hash? c) (hash-ref c 'text #f)) (hash-ref c 'text "")]
                     [(string? c) c]
                     [else ""]))
                 "\n"))
  (if (> (string-length full-text) max-chars)
      (string-append (substring full-text 0 (- max-chars 3)) "...")
      full-text))
