#lang racket/base

;; runtime/context-assembly/session-walk.rkt — Shared session context tree walk
;;
;; Extracted from selection.rkt and serialization.rkt to eliminate duplication.
;; CA-05 fix: single source of truth for session context assembly.
;; Uses the serialization.rkt version of entry->context-message which calls
;; summarize-tool-result to truncate large tool outputs (safer behavior).

(require racket/list
         racket/match
         racket/string
         (only-in "../../util/content-parts.rkt" make-text-part)
         (only-in "../../util/message.rkt"
                  message
                  message-id
                  message-kind
                  message-role
                  message-content
                  message-meta
                  make-message)
         (only-in "../../util/protocol-types.rkt" compaction-summary-entry?)
         (only-in "../../util/content-parts.rkt" text-part text-part? text-part-text)
         (only-in "../session-index.rkt" active-leaf get-branch))

(provide build-session-context
         assemble-context
         split-at-compaction
         entry->context-message
         summarize-tool-result)

;; ---------------------------------------------------------------------------
;; Session tree walk
;; ---------------------------------------------------------------------------

(define (build-session-context idx)
  (define leaf (active-leaf idx))
  (match leaf
    [#f '()]
    [_
     (define path (get-branch idx (message-id leaf)))
     (match path
       [#f '()]
       [_ (assemble-context path)])]))

;; Token-aware variant lives in serialization.rkt

(define (assemble-context path)
  (define-values (pre post) (split-at-compaction path))
  (define relevant (if post post path))
  (filter-map entry->context-message relevant))

(define (split-at-compaction path)
  (define idx
    (for/last ([entry (in-list path)]
               [i (in-naturals)]
               #:when (compaction-summary-entry? entry))
      i))
  (match idx
    [#f (values path #f)]
    [_
     (define-values (pre post) (split-at path idx))
     (values pre post)]))

;; ---------------------------------------------------------------------------
;; Tool result summarization (truncates large tool/bash results)
;; ---------------------------------------------------------------------------

;; v0.28.21 W5: Tool result summarization
;; Truncates tool/bash results exceeding max-chars to a summary.
;; Preserves first and last lines with a [... N lines truncated ...] indicator.
(define MAX-TOOL-RESULT-CHARS 8000)

(define (summarize-tool-result entry)
  (define content (message-content entry))
  (define text
    (string-join (for/list ([part (in-list content)]
                            #:when (text-part? part))
                   (text-part-text part))
                 "\n"))
  (cond
    [(<= (string-length text) MAX-TOOL-RESULT-CHARS) entry]
    [else
     (define lines (string-split text "\n"))
     (cond
       [(<= (length lines) 40) entry]
       [else
        (define head (take lines 10))
        (define tail (take-right lines 10))
        (define dropped (- (length lines) 20))
        (define summary-text
          (string-join (append head (list (format "... ~a lines truncated ..." dropped)) tail) "\n"))
        (struct-copy message entry [content (list (make-text-part summary-text))])])]))

;; ---------------------------------------------------------------------------
;; Entry → context message conversion
;; ---------------------------------------------------------------------------

(define (entry->context-message entry)
  (define kind (message-kind entry))
  (match kind
    [(or 'message) entry]
    ['compaction-summary (struct-copy message entry [role 'user])]
    ['branch-summary (struct-copy message entry [role 'user])]
    [(or 'session-info 'model-change 'thinking-level-change) #f]
    ;; Use summarize-tool-result to truncate large tool/bash outputs
    [(or 'tool-result 'bash-execution) (summarize-tool-result entry)]
    ['system-instruction entry]
    ['custom-message entry]
    [(or 'goal-state) #f]
    [_ entry]))
