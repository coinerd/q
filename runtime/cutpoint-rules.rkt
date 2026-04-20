#lang racket/base

;; runtime/cutpoint-rules.rkt — Compaction cut-point rules and overflow recovery (#693-#696)
;;
;; #693: Never cut between tool-call and tool-result. Valid cut points:
;;       before user messages, before assistant messages with no pending
;;       tool calls, or before custom messages. Invalid: splitting a
;;       tool-call → tool-result pair.
;;
;; #694: One-shot overflow recovery guard. When an LLM returns a context
;;       overflow error, compact once with willRetry=true, then retry.
;;       The _overflow-recovery-attempted flag prevents infinite loops.
;;
;; #695: Auto-retry after overflow compaction. Store the original user
;;       prompt, compact, then re-inject the prompt for retry.
;;
;; #696: Parent feature combining all of the above.

(require racket/contract
         racket/list
         racket/match
         "../util/protocol-types.rkt"
         "../util/message-helpers.rkt"
         "compactor.rkt")

;; #693: Cut-point rules
(provide adjust-cutpoint
         valid-cutpoint?
         find-nearest-valid-cutpoint
         cutpoint-rule-description
         ;; #694: Overflow recovery
         make-overflow-state
         overflow-state?
         overflow-state-attempted
         overflow-state-max-attempts
         overflow-state-will-retry
         mark-overflow-attempted!
         can-retry-overflow?
         ;; #695: Auto-retry
         make-retry-context
         retry-context?
         retry-context-original-prompt
         retry-context-compaction-result
         retry-context-retry-messages
         build-retry-messages)

;; ============================================================
;; #693: Cut-point rules
;; ============================================================

;; A message with role='assistant and kind='text may contain tool-call-parts.
;; A message with role='tool and kind='tool-result is a tool result.
;; We must NEVER cut between them.

;; has-tool-calls? and tool-result-message? imported from util/message-helpers.rkt

;; Check if a message is a user message (valid cut point).
(define (user-message? msg)
  (eq? (message-role msg) 'user))

;; Check if an assistant message is a valid cut point.
;; An assistant message is valid IF it has no pending tool calls
;; (i.e., the next message is NOT a tool-result).
(define (assistant-valid-cut? msg next-msg)
  (and (eq? (message-role msg) 'assistant) (or (not next-msg) (not (tool-result-message? next-msg)))))

;; Check if placing a cut BEFORE index `idx` is valid.
;; A cut at index N means messages[0..N) are old, messages[N..] are recent.
;; Valid cuts:
;;   - before a user message (turn boundary)
;;   - before an assistant message IF the previous message is not a tool-call
;;     (i.e., we're not splitting a tool-call → tool-result pair)
;; Invalid: before a tool-result message (would orphan its tool-call)
(define (valid-cutpoint? messages idx)
  (cond
    [(or (< idx 0) (> idx (length messages))) #f]
    [(= idx 0) #t] ;; Cutting at beginning is always valid
    [(= idx (length messages)) #t] ;; Cutting at end is always valid
    [else
     (define msg-at (list-ref messages idx))
     (define role-at (message-role msg-at))
     (cond
       ;; Before tool-result = always invalid (would split tool-call pair)
       [(tool-result-message? msg-at) #f]
       ;; Before user message = always valid (turn boundary)
       [(eq? role-at 'user) #t]
       ;; Before assistant = check previous doesn't have orphaned tool-calls
       [(eq? role-at 'assistant)
        (define msg-before (list-ref messages (sub1 idx)))
        (not (has-tool-calls? msg-before))]
       ;; Default: allow
       [else #t])]))

;; Find the nearest valid cut point, searching backward from `idx`.
;; If `backward?` is #t, search backward; otherwise forward.
(define (find-nearest-valid-cutpoint messages idx [backward? #t])
  (cond
    [(valid-cutpoint? messages idx) idx]
    [backward?
     (let loop ([i (sub1 idx)])
       (cond
         [(<= i 0) 0]
         [(valid-cutpoint? messages i) i]
         [else (loop (sub1 i))]))]
    [else
     (let loop ([i (add1 idx)])
       (cond
         [(> i (length messages)) (length messages)]
         [(valid-cutpoint? messages i) i]
         [else (loop (add1 i))]))]))

;; Adjust a proposed cut point to a valid one.
;; Moves backward to the nearest valid cut (user message boundary).
;; If the cut falls in the middle of a tool-call → tool-result sequence,
;; moves forward to include the complete sequence, then backward to the
;; next user boundary.
(define (adjust-cutpoint messages proposed-idx)
  (cond
    [(valid-cutpoint? messages proposed-idx) proposed-idx]
    ;; Walk backward to find nearest user message boundary
    [else (find-nearest-valid-cutpoint messages proposed-idx #t)]))

;; Human-readable description of why a cutpoint is valid or invalid.
(define (cutpoint-rule-description messages idx)
  (cond
    [(= idx 0) "Cut at beginning: always valid"]
    [(= idx (length messages)) "Cut at end: always valid"]
    [(valid-cutpoint? messages idx)
     (define msg-at (list-ref messages idx))
     (format "Valid cut before ~a message at index ~a" (message-role msg-at) idx)]
    [else
     (define msg-at (list-ref messages idx))
     (format "Invalid cut at index ~a (before ~a message) — adjusted to nearest user boundary"
             idx
             (message-role msg-at))]))

;; ============================================================
;; #694: One-shot overflow recovery guard
;; ============================================================

;; State tracking for overflow recovery. Prevents infinite compaction loops.
(struct overflow-state (attempted max-attempts) #:mutable #:transparent)

(define (make-overflow-state #:max-attempts [max 1])
  (overflow-state #f max))

(define (overflow-state-will-retry? state)
  (and (not (overflow-state-attempted state)) (< 0 (overflow-state-max-attempts state))))

;; Alias for external use
(define (overflow-state-will-retry state)
  (overflow-state-will-retry? state))

(define (can-retry-overflow? state)
  (not (overflow-state-attempted state)))

(define (mark-overflow-attempted! state)
  (set-overflow-state-attempted! state #t))

;; ============================================================
;; #695: Auto-retry after overflow compaction
;; ============================================================

;; Context for retrying after overflow compaction.
(struct retry-context (original-prompt compaction-result retry-messages) #:transparent)

(define (make-retry-context prompt compaction-result messages)
  (retry-context prompt compaction-result messages))

;; Build the message list for retrying after overflow compaction.
;; Takes the compaction result (summary + kept messages) and prepends
;; the original user prompt as a fresh message.
(define (build-retry-messages compaction-result original-prompt)
  (define summary (compaction-result-summary-message compaction-result))
  (define kept (compaction-result-kept-messages compaction-result))
  ;; Build retry message list: summary + kept + re-injected prompt
  (define base
    (if summary
        (cons summary kept)
        kept))
  ;; Create a fresh user message with the original prompt
  (define retry-msg
    (make-message (format "retry-~a" (current-inexact-milliseconds))
                  #f
                  'user
                  'text
                  (list (make-text-part original-prompt))
                  (current-seconds)
                  (hasheq)))
  (append base (list retry-msg)))
