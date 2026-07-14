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
         (only-in "../../util/content/content-parts.rkt"
                  make-text-part
                  image-part?
                  image-part-mime-type
                  image-part-data)
         (only-in "../../util/message/message.rkt"
                  message
                  message-id
                  message-kind
                  message-role
                  message-content
                  message-meta
                  make-message)
         (only-in "../../util/entry-predicates.rkt" compaction-summary-entry?)
         (only-in "../../util/content/content-parts.rkt" text-part text-part? text-part-text)
         (only-in "../../util/content/content-parts.rkt" tool-result-part? tool-result-part-content)
         (only-in "../../util/content/content-helpers.rkt" result-content->string)
         (only-in "../session-index.rkt" active-leaf get-branch))

(provide build-session-context
         assemble-context
         split-at-compaction
         entry->context-message
         summarize-tool-result
         strip-image-parts)

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
  (define relevant
    (cond
      [(not post) path]
      [else
       (define summary (car post))
       (define first-kept-id (hash-ref (message-meta summary) 'firstKeptEntryId #f))
       (define kept
         (if first-kept-id
             (dropf pre (lambda (entry) (not (equal? (message-id entry) first-kept-id))))
             '()))
       ;; Durable logs are append-only, so kept entries precede the summary on
       ;; disk. Provider context is summary, then kept entries, then new turns.
       (append (list summary) kept (cdr post))]))
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

;; ---------------------------------------------------------------------------
;; Ephemeral vision context management (W3: strip old image-parts)
;; ---------------------------------------------------------------------------

;; L8: Maximum age (turns) before stripping images from context
(define default-vision-ephemeral-turns 5)

(define (strip-image-parts msg [max-age default-vision-ephemeral-turns])
  ;; Replace image-part content with text summary when age exceeds threshold.
  ;; Non-image content is preserved unchanged.
  (define content (message-content msg))
  (define has-images?
    (for/or ([p (in-list content)])
      (image-part? p)))
  (if (not has-images?)
      msg
      (struct-copy message
                   msg
                   [content
                    (for/list ([p (in-list content)])
                      (if (image-part? p)
                          (make-text-part (format "[screenshot: ~a, ~a bytes expired]"
                                                  (image-part-mime-type p)
                                                  (string-length (image-part-data p))))
                          p))])))

(define (summarize-tool-result entry)
  (define content (message-content entry))
  ;; Bug fix: also extract text from tool-result-part (browser screenshot, etc.)
  ;; Without this, large binary/hash tool results bypass truncation entirely.
  (define text
    (string-join (for/list ([part (in-list content)]
                            #:when (or (text-part? part) (tool-result-part? part) (image-part? part)))
                   (cond
                     [(text-part? part) (text-part-text part)]
                     [(image-part? part)
                      (format "[image: ~a, ~a chars]"
                              (image-part-mime-type part)
                              (string-length (image-part-data part)))]
                     [(tool-result-part? part)
                      (result-content->string (tool-result-part-content part) #:handle-hash? #t)]
                     [else ""]))
                 "\n"))
  (cond
    [(<= (string-length text) MAX-TOOL-RESULT-CHARS) entry]
    [else
     (define lines (string-split text "\n"))
     (cond
       ;; Multi-line tool result: preserve first/last 10 lines.
       [(> (length lines) 40)
        (define head (take lines 10))
        (define tail (take-right lines 10))
        (define dropped (- (length lines) 20))
        (define summary-text
          (string-join (append head (list (format "... ~a lines truncated ..." dropped)) tail) "\n"))
        (struct-copy message entry [content (list (make-text-part summary-text))])]
       ;; Single (or few) very long lines: hard truncate at char limit.
       [else
        (define summary-text
          (string-append (substring text 0 MAX-TOOL-RESULT-CHARS)
                         (format "\n... ~a chars truncated ..."
                                 (- (string-length text) MAX-TOOL-RESULT-CHARS))))
        (struct-copy message entry [content (list (make-text-part summary-text))])])]))

;; ---------------------------------------------------------------------------
;; Entry → context message conversion
;; ---------------------------------------------------------------------------

(define (entry->context-message entry)
  (define kind (message-kind entry))
  (match kind
    [(or 'message) (strip-image-parts entry)]
    ['compaction-summary (struct-copy message entry [role 'user])]
    ['branch-summary (struct-copy message entry [role 'user])]
    [(or 'session-info 'model-change 'thinking-level-change) #f]
    ;; Use summarize-tool-result to truncate large tool/bash outputs
    [(or 'tool-result 'bash-execution) (summarize-tool-result entry)]
    ['system-instruction entry]
    ['custom-message entry]
    [(or 'goal-state) #f]
    [_ entry]))
