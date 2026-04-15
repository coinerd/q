#lang racket/base

;; runtime/compactor.rkt — history summarization and compaction
;;
;; Provides:
;;   compaction-strategy          — struct: summary-window-size, keep-recent-count
;;   compaction-result            — struct: summary-message, removed-count, kept-messages
;;   compact-history              — summarize history and produce compaction entries (ADVISORY-ONLY)
;;   compact-history-advisory     — explicit alias for compact-history (advisory, no filesystem)
;;   compact-and-persist!         — compact history AND write summary entry to session log
;;   build-summary-window         — determine what to summarize vs what to keep recent
;;   write-compaction-entry!      — write a compaction entry to session log
;;   compaction-result->message-list — flatten result into a single message list
;;   default-strategy             — default compaction strategy
;;   default-summarize            — simple concatenation summarizer
;;
;; Key invariant: compaction NEVER silently destroys prior history.
;; The original log remains reconstructible. compact-history produces
;; a NEW message list without modifying or deleting the originals.

(require racket/contract
         racket/string
         racket/list
         racket/set
         "../util/protocol-types.rkt"
         "../runtime/session-store.rkt"
         "../runtime/compaction-prompts.rkt"
         (only-in "../llm/model.rkt" make-model-request model-response-content model-response?)
         (only-in "../llm/provider.rkt" provider-send provider?)
         (only-in "../util/hook-types.rkt"
                  hook-result
                  hook-result?
                  hook-result-action
                  hook-result-payload)
         (only-in "token-compaction.rkt"
                  build-token-summary-window
                  default-token-compaction-config
                  token-compaction-config))

(provide (struct-out compaction-strategy)
         (struct-out compaction-result)
         ;; Compaction functions (WP-37: now also exports compact-history for tests)
         compact-history
         compact-history-advisory
         compact-and-persist!
         build-summary-window
         write-compaction-entry!
         compaction-result->message-list
         default-strategy
         default-summarize
         ;; LLM-powered summarization (v0.8.9)
         llm-summarize
         make-llm-summarize-fn
         extract-file-tracker
         format-messages-for-summary
         ;; Tiered Context Assembly (WP-37) — DEPRECATED (#648)
         ;; Use build-session-context/tokens from context-builder.rkt instead.
         (struct-out tiered-context)
         build-tiered-context
         tiered-context->message-list
         ;; R2-6: Context Assembly Hooks — DEPRECATED (#648)
         build-tiered-context-with-hooks
         (struct-out context-assembly-payload)
         payload->tiered-context
         tiered-context->payload)

;; ============================================================
;; Structs
;; ============================================================

;; Compaction strategy parameters
(struct compaction-strategy (summary-window-size keep-recent-count) #:transparent)
;; summary-window-size : integer — max number of past messages to summarize
;; keep-recent-count   : integer — how many recent messages to keep as-is

;; Compaction result
(struct compaction-result (summary-message removed-count kept-messages) #:transparent)
;; summary-message : (or/c message? #f) — a single 'system message with the summary text, or #f if nothing was summarized
;; removed-count   : integer — how many messages were replaced by the summary
;; kept-messages   : (listof message?) — the recent messages that were kept verbatim

;; ============================================================
;; Default strategy and summarize function
;; ============================================================

(define (default-strategy)
  (compaction-strategy 50 20))

(define (default-summarize messages)
  ;; Simple summarization: produce a structured summary string.
  ;; Concatenates the text content of each message.
  ;; In production, this would call the LLM to produce a real summary.
  (define n (length messages))
  (define parts
    (for/list ([m (in-list messages)])
      (define content (message-content m))
      (define text
        (string-join (for/list ([part (in-list content)]
                                #:when (text-part? part))
                       (text-part-text part))
                     " "))
      (format "[~a] ~a" (message-role m) text)))
  (format "[Compaction summary of ~a messages]\n~a" n (string-join parts "\n")))

;; ============================================================
;; LLM-powered summarization (v0.8.9)
;; ============================================================

;; Format a list of messages into a text representation for the LLM.
;; Truncates long tool results to keep the prompt within bounds.
(define (format-messages-for-summary messages)
  (string-join (for/list ([m (in-list messages)])
                 (define role (message-role m))
                 (define content (message-content m))
                 (define text
                   (string-join (for/list ([part (in-list content)]
                                           #:when (text-part? part))
                                  (define t (text-part-text part))
                                  (if (> (string-length t) MAX-TOOL-RESULT-CHARS)
                                      (string-append (substring t 0 MAX-TOOL-RESULT-CHARS)
                                                     "\n... [truncated]")
                                      t))
                                " "))
                 (format "[~a] ~a" role text))
               "\n"))

;; Walk messages to find file paths from tool calls.
;; Returns a hash: 'readFiles -> list of paths,
;;                  'modifiedFiles -> list of paths.
(define (extract-file-tracker messages)
  (define reads (mutable-set))
  (define writes (mutable-set))
  (for ([m (in-list messages)])
    (define content (message-content m))
    (for ([part (in-list content)])
      (cond
        [(tool-call-part? part)
         (define tool-name (tool-call-part-name part))
         (define args (tool-call-part-arguments part))
         (define path
           (cond
             [(hash? args) (hash-ref args 'path #f)]
             [(string? args)
              ;; Try to extract path from JSON string
              (and (string-contains? args "path")
                   (let ([m (regexp-match #rx"\"path\"[[:space:]]*:[[:space:]]*\"([^\"]+)\"" args)])
                     (and m (cadr m))))]
             [else #f]))
         (when path
           (cond
             [(member tool-name '("read" "find" "grep" "ls")) (set-add! reads path)]
             [(member tool-name '("edit" "write")) (set-add! writes path)]))]
        ;; Tool results may contain file paths in content
        [(tool-result-part? part) (void)])))
  (hasheq 'readFiles (set->list reads) 'modifiedFiles (set->list writes)))

;; Find the most recent compaction-summary message in the list.
;; Returns the text of the summary or #f.
(define (find-previous-summary messages)
  (for/first ([m (in-list (reverse messages))]
              #:when (eq? (message-kind m) 'compaction-summary))
    (string-join (for/list ([part (in-list (message-content m))]
                            #:when (text-part? part))
                   (text-part-text part))
                 "")))

;; Call the LLM to summarize messages.
;; When #:previous-summary is provided, uses iterative update prompt.
;; Returns the summary text string.
(define (llm-summarize messages
                       provider
                       model-name
                       #:previous-summary [prev-summary #f]
                       #:file-tracker [file-tracker (hasheq)])
  (define formatted (format-messages-for-summary messages))
  (define prompt-text
    (if prev-summary
        (iterative-update-prompt prev-summary formatted file-tracker)
        (summary-prompt formatted file-tracker)))
  ;; Build a minimal model-request with just the prompt
  (define req
    (make-model-request (list (hasheq 'role "user" 'content prompt-text))
                        #f
                        (hasheq 'model model-name 'max_tokens 2000)))
  (define resp (provider-send provider req))
  ;; Extract text from response content
  (define content (model-response-content resp))
  (string-join (for/list ([part (in-list content)])
                 (cond
                   [(hash? part) (hash-ref part 'text "")]
                   [(string? part) part]
                   [else (format "~a" part)]))
               ""))

;; Create a summarize function suitable for use with compact-history.
;; The returned function has signature: (listof message?) -> string
(define (make-llm-summarize-fn provider model-name)
  (lambda (messages) (llm-summarize messages provider model-name)))

;; ============================================================
;; build-summary-window
;; ============================================================

;; Split messages into old (to summarize) and recent (to keep).
;; The "old" portion is further capped at summary-window-size.
;; Returns: (values old-messages recent-messages)
(define (build-summary-window messages strategy)
  (define total (length messages))
  (define keep-recent (compaction-strategy-keep-recent-count strategy))
  (define window-size (compaction-strategy-summary-window-size strategy))
  (cond
    ;; All messages fit in the "recent" window — nothing to summarize
    [(<= total keep-recent) (values '() messages)]
    [else
     (define split-point (max 0 (- total keep-recent)))
     (define all-old (take messages split-point))
     (define recent (drop messages split-point))
     ;; Cap the old messages at summary-window-size
     ;; (take the LAST window-size of the old messages, to summarize the most recent of the old)
     (define old-to-summarize
       (if (> (length all-old) window-size)
           (take-right all-old window-size)
           all-old))
     (values old-to-summarize recent)]))

;; ============================================================
;; Inline cutpoint adjustment (avoids circular dep with cutpoint-rules.rkt)
;; ============================================================

;; Check if a message contains tool-call parts.
(define (has-tool-calls? msg)
  (define content (message-content msg))
  (and (list? content)
       (ormap tool-call-part? content)))

;; Check if a message is a tool-result.
(define (tool-result-message? msg)
  (eq? (message-kind msg) 'tool-result))

;; Check if placing a cut BEFORE index `idx` is valid.
;; A cut at N means messages[0..N) are old, messages[N..] are recent.
(define (valid-cutpoint? messages idx)
  (cond
    [(or (< idx 0) (> idx (length messages))) #f]
    [(or (= idx 0) (= idx (length messages))) #t]
    [else
     (define msg-at (list-ref messages idx))
     (cond
       [(tool-result-message? msg-at) #f]
       [(eq? (message-role msg-at) 'user) #t]
       [(eq? (message-role msg-at) 'assistant)
        (define msg-before (list-ref messages (sub1 idx)))
        (not (has-tool-calls? msg-before))]
       [else #t])]))

;; Adjust a proposed cut point backward to nearest valid position.
(define (adjust-cutpoint messages proposed-idx)
  (cond
    [(valid-cutpoint? messages proposed-idx) proposed-idx]
    [else
     (let loop ([i (sub1 proposed-idx)])
       (cond
         [(<= i 0) 0]
         [(valid-cutpoint? messages i) i]
         [else (loop (sub1 i))]))]))

;; ============================================================
;; compact-history
;; ============================================================

;; Main compaction function.
;; Returns a compaction-result. Does NOT modify the original log.
;;
;; CONTRACT: This function is ADVISORY-ONLY. It does NOT modify the session log
;; or touch the filesystem in any way. To persist the compaction, use
;; `compact-and-persist!` or call `write-compaction-entry!` explicitly.
;;
;; v0.8.9: Added #:provider, #:model-name for LLM-powered summarization,
;;         #:hook-dispatcher for extension hooks, #:previous-summary for
;;         iterative compaction.
;; v0.9.1 (#636): Hook blocking now actually prevents compaction.
(define (compact-history messages
                         #:strategy [strategy (default-strategy)]
                         #:summarize-fn [summarize-fn default-summarize]
                         #:provider [provider #f]
                         #:model-name [model-name #f]
                         #:previous-summary [prev-summary #f]
                         #:hook-dispatcher [hook-dispatcher #f]
                         #:token-config [token-config (default-token-compaction-config)])
  ;; Dispatch 'session-before-compact hook if dispatcher provided
  ;; Returns identity result if hook blocks, otherwise proceeds with compaction.
  (cond
    [(and hook-dispatcher
          (let ([hook-res (hook-dispatcher 'session-before-compact
                                          (hasheq 'message-count (length messages) 'strategy strategy))])
            (and (hook-result? hook-res) (eq? (hook-result-action hook-res) 'block))))
     ;; Hook blocked compaction — return identity result immediately
     (compaction-result #f 0 messages)]
    [else
     ;; Use token-based window split instead of fixed-count
     (define-values (old recent) (build-token-summary-window messages token-config))
     ;; Adjust cutpoint to a valid boundary (e.g., don't split mid-tool-call)
     (define-values (adjusted-old adjusted-recent)
       (if (and (pair? old) (pair? recent))
           (let ([cut-idx (adjust-cutpoint messages (length old))])
             (if (and cut-idx (> cut-idx 0) (< cut-idx (length messages)))
                 (values (take messages cut-idx) (drop messages cut-idx))
                 (values old recent)))
           (values old recent)))
     (cond
       ;; Nothing to summarize — return identity result
       [(null? adjusted-old) (compaction-result #f 0 adjusted-recent)]
       [else
        ;; Choose summarization: LLM if provider given, else use summarize-fn
        (define summary-text
          (if (and provider model-name)
              (let ([file-tracker (extract-file-tracker adjusted-old)])
                (llm-summarize adjusted-old
                               provider
                               model-name
                               #:previous-summary (or prev-summary (find-previous-summary adjusted-recent))
                               #:file-tracker file-tracker))
              (summarize-fn adjusted-old)))
        ;; Build file tracker metadata for the summary message
        (define file-tracker-meta
          (if (and provider model-name)
              (let ([ft (extract-file-tracker adjusted-old)])
                (if (> (hash-count ft) 0)
                    (hasheq 'fileTracker ft)
                    (hasheq)))
              (hasheq)))
        (define summary-msg
          (make-message
           (format "compaction-~a" (current-inexact-milliseconds))
           #f ; no parent — compaction summaries are root-level context
           'system
           'compaction-summary
           (list (make-text-part summary-text))
           (current-seconds)
           (hash-set (hash-set file-tracker-meta 'type "compaction") 'removedCount (length adjusted-old))))
        (compaction-result summary-msg (length adjusted-old) adjusted-recent)])]))

;; ============================================================
;; compaction-result->message-list
;; ============================================================

;; Flatten a compaction-result into a single message list.
;; If a summary exists, prepends it before the kept messages.
(define (compaction-result->message-list result)
  (define summary (compaction-result-summary-message result))
  (define kept (compaction-result-kept-messages result))
  (if summary
      (cons summary kept)
      kept))

;; ============================================================
;; compact-history-advisory
;; ============================================================

;; Explicit advisory variant — returns the result but does NOT write
;; to the session log. This is a pure function that does not touch
;; the filesystem. Equivalent to `compact-history` but named to make
;; the advisory contract crystal clear.
(define (compact-history-advisory messages
                                  #:strategy [strategy (default-strategy)]
                                  #:summarize-fn [summarize-fn default-summarize]
                                  #:provider [provider #f]
                                  #:model-name [model-name #f]
                                  #:previous-summary [prev-summary #f]
                                  #:hook-dispatcher [hook-dispatcher #f]
                                  #:token-config [token-config (default-token-compaction-config)])
  (compact-history messages
                   #:strategy strategy
                   #:summarize-fn summarize-fn
                   #:provider provider
                   #:model-name model-name
                   #:previous-summary prev-summary
                   #:hook-dispatcher hook-dispatcher
                   #:token-config token-config))

;; ============================================================
;; compact-and-persist!
;; ============================================================

;; Compacts history AND persists the summary entry to the session log.
;; Returns the compaction-result for inspection.
;; This is the "persisting" variant — the summary entry becomes part of
;; the durable session history.
;; The original messages are NOT deleted — they remain in the log.
;; Future context assembly can use the summary + recent messages
;; instead of replaying the full history.
(define (compact-and-persist! messages
                              session-log-path
                              #:strategy [strategy (default-strategy)]
                              #:summarize-fn [summarize-fn default-summarize]
                              #:provider [provider #f]
                              #:model-name [model-name #f]
                              #:previous-summary [prev-summary #f]
                              #:hook-dispatcher [hook-dispatcher #f]
                              #:token-config [token-config (default-token-compaction-config)])
  (define result
    (compact-history messages
                     #:strategy strategy
                     #:summarize-fn summarize-fn
                     #:provider provider
                     #:model-name model-name
                     #:previous-summary prev-summary
                     #:hook-dispatcher hook-dispatcher
                     #:token-config token-config))
  (write-compaction-entry! session-log-path result)
  result)

;; ============================================================
;; write-compaction-entry!
;; ============================================================

;; Write a compaction-summary entry to the session log.
;; The entry records what was summarized. Original entries remain intact.
;; If no compaction happened (summary is #f), this is a no-op.
(define (write-compaction-entry! session-path compaction-res)
  (define summary (compaction-result-summary-message compaction-res))
  (when summary
    (append-entry! session-path summary)))

;; ============================================================
;; Tiered Context Assembly (WP-37)
;; DEPRECATED (#648): Use build-session-context/tokens from context-builder.rkt
;; instead. build-tiered-context remains for backward compatibility but will
;; be removed in a future version.

;; Tiered context holds messages split into three tiers:
;; - Tier A: Compacted summaries of older messages
;; - Tier B: Recent messages kept verbatim (not compacted)
;; - Tier C: Current turn messages (most recent, always included in full)
(struct tiered-context (tier-a tier-b tier-c) #:transparent)

;; Default tier boundaries
(define DEFAULT-TIER-B-COUNT 20)
(define DEFAULT-TIER-C-COUNT 4)

;; R2-6: Context Assembly Hook Payload
;; Serializable payload for context-assembly hook point
(struct context-assembly-payload (tier-a-messages tier-b-messages tier-c-messages max-tokens metadata)
  #:transparent)

;; Helper: Convert payload back to tiered-context
(define (payload->tiered-context payload)
  (tiered-context (context-assembly-payload-tier-a-messages payload)
                  (context-assembly-payload-tier-b-messages payload)
                  (context-assembly-payload-tier-c-messages payload)))

;; Helper: Convert tiered-context to payload
(define (tiered-context->payload tc max-tokens [metadata (hasheq)])
  (context-assembly-payload (tiered-context-tier-a tc)
                            (tiered-context-tier-b tc)
                            (tiered-context-tier-c tc)
                            max-tokens
                            metadata))

;; Split messages into three tiers based on compaction status and recency.
;; Parameters:
;;   messages - list of messages (may include compaction-summary messages)
;;   tier-b-count - how many recent non-summary messages to keep in Tier B
;;   tier-c-count - how many most recent messages to treat as "current turn" (Tier C)
;; Returns: tiered-context struct
(define (build-tiered-context messages
                              #:tier-b-count [tier-b-count DEFAULT-TIER-B-COUNT]
                              #:tier-c-count [tier-c-count DEFAULT-TIER-C-COUNT])
  ;; Separate compaction summaries (Tier A candidates) from regular messages
  (define-values (compaction-summaries regular-msgs)
    (partition (lambda (m) (eq? (message-kind m) 'compaction-summary)) messages))

  ;; Calculate how many messages go to Tier C (most recent)
  (define total (length regular-msgs))
  (define tier-c-size (min tier-c-count total))

  ;; Tier C: most recent messages
  (define tier-c
    (if (> tier-c-size 0)
        (take-right regular-msgs tier-c-size)
        '()))

  ;; Remaining messages after Tier C
  (define remaining-after-c
    (if (> tier-c-size 0)
        (drop-right regular-msgs tier-c-size)
        regular-msgs))

  ;; Tier B: recent messages (up to tier-b-count)
  (define remaining-count (length remaining-after-c))
  (define tier-b-size (min tier-b-count remaining-count))

  (define tier-b
    (if (> tier-b-size 0)
        (take-right remaining-after-c tier-b-size)
        '()))

  ;; Any remaining regular messages would conceptually go to Tier A
  ;; but we only include explicit compaction summaries in Tier A
  ;; (those have already been processed into summaries)

  (tiered-context compaction-summaries tier-b tier-c))

;; R2-6: Build tiered context with hook support
;; Parameters:
;;   messages - list of messages
;;   hook-dispatcher - optional function (hook-point payload) -> hook-result
;;   tier-b-count, tier-c-count - tier boundaries
;;   max-tokens - max tokens for context (for payload)
;; Returns: (values tiered-context hook-result-or-#f)
;; Raises: exn:fail if hook returns 'block
(define (build-tiered-context-with-hooks messages
                                         #:hook-dispatcher [hook-dispatcher #f]
                                         #:tier-b-count [tier-b-count DEFAULT-TIER-B-COUNT]
                                         #:tier-c-count [tier-c-count DEFAULT-TIER-C-COUNT]
                                         #:max-tokens [max-tokens 8192])
  ;; First build the base tiered context
  (define base-context
    (build-tiered-context messages #:tier-b-count tier-b-count #:tier-c-count tier-c-count))

  ;; Create the hook payload (serializable for logging)
  (define payload
    (tiered-context->payload base-context
                             max-tokens
                             (hasheq 'tier-b-count
                                     tier-b-count
                                     'tier-c-count
                                     tier-c-count
                                     'total-messages
                                     (length messages))))

  ;; Dispatch hook if dispatcher provided
  (if hook-dispatcher
      (let ([result (hook-dispatcher 'context-assembly payload)])
        (case (hook-result-action result)
          [(block)
           ;; Hook blocked - raise error with reason
           (define reason (hook-result-payload result))
           (raise (exn:fail (format "Context assembly blocked by hook: ~a"
                                    (if reason reason "no reason given"))
                            (current-continuation-marks)))]
          [(amend)
           ;; Hook amended - use new payload to build tiered context
           (define amended-payload (hook-result-payload result))
           (values (payload->tiered-context amended-payload) result)]
          ;; Hook passed - use original context
          [(pass) (values base-context result)]
          ;; Unknown action - use original context
          [else (values base-context result)]))
      ;; No hook dispatcher - return base context
      (values base-context #f)))

;; Flatten tiered context back into a single message list.
;; Order: Tier A (summaries) → Tier B (recent) → Tier C (current)
(define (tiered-context->message-list tc)
  (append (tiered-context-tier-a tc) (tiered-context-tier-b tc) (tiered-context-tier-c tc)))
