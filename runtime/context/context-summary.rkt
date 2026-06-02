#lang racket/base
;; STABILITY: evolving

;; q/runtime/context-summary.rkt — Summary and catalog generation for context assembly
;;
;; Provides summary cache (LRU), catalog entry generation, context summary
;; prompting/generation, and related helpers. Extracted from context-assembly.rkt.

(require racket/contract
         racket/match
         racket/string
         racket/list
         (only-in "../../util/content-parts.rkt" text-part text-part-text text-part?)
         (only-in "../../util/message.rkt" message message-content message-id message-role message?)
         "../../util/string-helpers.rkt"
         "../../llm/token-budget.rkt"
         (only-in "../compaction-prompts.rkt" format-messages-for-summary)
         (only-in "../compactor-llm-bridge.rkt" llm-summarize)
         (only-in "../../llm/provider.rkt" provider?))

;; Summary cache
(provide catalog-entry
         catalog-entry?
         catalog-entry-id
         catalog-entry-role
         catalog-entry-summary
         context-summary
         context-summary?
         context-summary-from-id
         context-summary-to-id
         context-summary-text
         context-summary-entry-count
         summary-cache?
         summary-cache-table
         summary-cache-order
         summary-cache-max-entries
         DEFAULT-CACHE-MAX-ENTRIES
         (contract-out [make-summary-cache
                        (->* () (#:max-entries exact-nonnegative-integer?) summary-cache?)]
                       [summary-cache-lookup (-> summary-cache? any/c any/c (or/c string? #f))]
                       [summary-cache-store! (-> summary-cache? any/c any/c string? void?)]
                       [summary-cache-count (-> summary-cache? exact-nonnegative-integer?)]
                       [generate-catalog
                        (->* ((listof message?))
                             (#:max-entries exact-nonnegative-integer?
                                            #:max-tokens exact-nonnegative-integer?
                                            #:estimate-text (-> string? exact-nonnegative-integer?))
                             (listof catalog-entry?))]
                       [collapse-consecutive-tools (-> (listof any/c) (listof any/c))]
                       [message->catalog-entry (-> any/c (or/c catalog-entry? #f))]
                       [tool-group->catalog-entry (-> (listof any/c) (or/c catalog-entry? #f))]
                       [context-summary-prompt
                        (->* ((listof message?)) (#:previous-summary (or/c string? #f)) string?)]
                       [generate-context-summary
                        (->* ((listof message?) (or/c provider? #f) (or/c string? #f))
                             (#:cache (or/c summary-cache? #f))
                             (or/c context-summary? #f))]
                       [simple-summary-text (-> (listof message?) string?)]
                       [simple-summary-count (-> (listof message?) exact-nonnegative-integer?)]
                       [extract-message-text (-> message? string?)]
                       [truncate-string (-> string? exact-nonnegative-integer? string?)]))

;; ============================================================
;; Catalog entry struct
;; ============================================================

(struct catalog-entry (id role summary) #:transparent)

;; ============================================================
;; Summary struct
;; ============================================================

(struct context-summary (from-id to-id text entry-count) #:transparent)

;; ============================================================
;; Summary cache — LRU eviction when capacity exceeded
;; ============================================================

;; Default maximum cache entries before LRU eviction
(define DEFAULT-CACHE-MAX-ENTRIES 50)

(struct summary-cache
        ([table #:mutable] ; hash: key=(from-id . to-id) → value=text
         [order #:mutable] ; list of keys in LRU order (newest first)
         [max-entries #:mutable]) ; capacity limit
  )

(define (make-summary-cache #:max-entries [max DEFAULT-CACHE-MAX-ENTRIES])
  (summary-cache (hash) '() max))

(define (summary-cache-lookup cache from-id to-id)
  (define key (cons from-id to-id))
  (define result (hash-ref (summary-cache-table cache) key #f))
  ;; Promote to front of LRU on hit
  (when result
    (set-summary-cache-order! cache (cons key (remove key (summary-cache-order cache) equal?))))
  result)

(define (summary-cache-store! cache from-id to-id text)
  (define key (cons from-id to-id))
  (define current-table (summary-cache-table cache))
  (define current-order (summary-cache-order cache))
  ;; If key already exists, just update and promote
  (cond
    [(hash-has-key? current-table key)
     (set-summary-cache-table! cache (hash-set current-table key text))
     (set-summary-cache-order! cache (cons key (remove key current-order equal?)))]
    [else
     ;; Evict oldest if at capacity
     (when (>= (hash-count current-table) (summary-cache-max-entries cache))
       (define oldest-key (last current-order))
       (set-summary-cache-table! cache (hash-remove current-table oldest-key))
       (set-summary-cache-order! cache (remove oldest-key current-order equal?)))
     ;; Insert new entry at front
     (set-summary-cache-table! cache (hash-set (summary-cache-table cache) key text))
     (set-summary-cache-order! cache (cons key (summary-cache-order cache)))]))

;; Query cache size (for testing)
(define (summary-cache-count cache)
  (hash-count (summary-cache-table cache)))

;; ============================================================
;; Catalog generation
;; ============================================================

;; generate-catalog: Build a summary catalog from older entries.
;; Returns a list of catalog-entry fitting within token budget.
(define (generate-catalog entries
                          #:max-entries [max-entries 40]
                          #:max-tokens [max-tokens 2000]
                          #:estimate-text [estimate-text-fn estimate-text-tokens])
  (define collapsed (collapse-consecutive-tools entries))
  (let loop ([remaining collapsed]
             [acc '()]
             [used-tokens 0]
             [count 0])
    (cond
      [(null? remaining) (reverse acc)]
      [(>= count max-entries) (reverse acc)]
      [else
       (define entry (car remaining))
       (define tokens (estimate-text-fn (catalog-entry-summary entry)))
       (if (> (+ used-tokens tokens) max-tokens)
           (reverse acc)
           (loop (cdr remaining) (cons entry acc) (+ used-tokens tokens) (add1 count)))])))

;; collapse-consecutive-tools: Merge adjacent tool-call/tool-result sequences
;; into a single summary entry to reduce context size.
(define (collapse-consecutive-tools entries)
  (define-values (result current-group)
    (for/fold ([acc '()]
               [group '()])
              ([m (in-list entries)])
      (cond
        [(eq? (message-role m) 'tool) (values acc (cons m group))]
        [(null? group) (values (cons (message->catalog-entry m) acc) '())]
        [else
         (values (cons (message->catalog-entry m)
                       (cons (tool-group->catalog-entry (reverse group)) acc))
                 '())])))
  (reverse (if (null? current-group)
               result
               (cons (tool-group->catalog-entry (reverse current-group)) result))))

(define (message->catalog-entry m)
  (catalog-entry (message-id m)
                 (symbol->string (message-role m))
                 (truncate-string (extract-message-text m) 80)))

(define (tool-group->catalog-entry tool-msgs)
  (define ids (string-join (map message-id tool-msgs) ","))
  (define texts (map extract-message-text tool-msgs))
  (catalog-entry
   ids
   "tool"
   (format "[~a tool calls: ~a]" (length tool-msgs) (truncate-string (string-join texts "; ") 50))))

;; ============================================================
;; Summary prompt template
;; ============================================================

(define (context-summary-prompt messages #:previous-summary [prev-summary #f])
  (define formatted (format-messages-for-summary messages))
  (cond
    [prev-summary
     (string-append "You are updating an existing session summary with new information. "
                    "Merge the new messages into the existing summary.\n\n"
                    "EXISTING SUMMARY:\n"
                    prev-summary
                    "\n\nNEW MESSAGES SINCE LAST SUMMARY:\n"
                    formatted
                    "\n\nProduce an UPDATED summary with these EXACT sections:\n"
                    "## Goal\n"
                    "## Progress\n### Done\n### In Progress\n### Blocked\n"
                    "## Key Decisions\n"
                    "## Next Steps\n"
                    "## Critical Context\n\n"
                    "RULES:\n"
                    "- Maximum ~500 words\n"
                    "- Preserve all information from existing summary that is still relevant\n"
                    "- Add new information from new messages\n"
                    "- Update Progress sections (move items between Done/In Progress/Blocked)\n"
                    "- Keep the Goal to ONE line\n")]
    [else
     (string-append
      "You are summarizing a coding assistant session. "
      "Produce a structured summary with these EXACT sections:\n\n"
      "## Goal\n<one-line description of what the user is trying to accomplish>\n\n"
      "## Progress\n### Done\n- [x] <completed items>\n\n"
      "### In Progress\n- <current work if any>\n\n"
      "### Blocked\n- <blockers if any>\n\n"
      "## Key Decisions\n- <important decisions made during the session>\n\n"
      "## Next Steps\n1. <next actions>\n\n"
      "## Critical Context\n- <must-preserve information: file paths, variable names, API details, error states>\n\n"
      "RULES:\n"
      "- Maximum ~500 words\n"
      "- Preserve specific file paths, function names, variable names exactly\n"
      "- Include any error messages or states being debugged\n"
      "- Keep the Goal to ONE line\n\n"
      "SESSION MESSAGES:\n"
      formatted)]))

;; ============================================================
;; Generate context summary
;; ============================================================

;; generate-context-summary: Produce a text summary of entries using LLM or fallback.
;; Returns a context-summary struct or #f if no entries.
(define (generate-context-summary entries provider model-name #:cache [cache #f])
  (match entries
    ['() #f]
    [_
     (define from-id (message-id (first entries)))
     (define to-id (message-id (last entries)))
     (define cached (and cache (summary-cache-lookup cache from-id to-id)))
     (match cached
       [#f
        (define summary-text
          (cond
            [(and provider model-name (provider? provider))
             (llm-summarize entries provider model-name)]
            [else (simple-summary-text entries)]))
        (when cache
          (summary-cache-store! cache from-id to-id summary-text))
        (define actual-count
          (if (and provider model-name (provider? provider))
              (length entries)
              (simple-summary-count entries)))
        (context-summary from-id to-id summary-text actual-count)]
       [_ (context-summary from-id to-id cached (length entries))])]))

;; CA-04 fix: expanded from 20×100 to 30×200 with file path extraction
(define FILE-IN-TEXT-RE
  (regexp (string-append "[a-zA-Z0-9_./-]+" "\\.[a-zA-Z][a-zA-Z][a-zA-Z]?[a-zA-Z]?")))

(define (simple-summary-text entries)
  (string-append "## Progress Summary (fallback — LLM summarization unavailable)\n\n"
                 "### Done\n"
                 (string-join (for/list ([m (in-list entries)]
                                         [i (in-naturals)]
                                         #:break (>= i 30))
                                (define text (extract-message-text m))
                                (define files (regexp-match* FILE-IN-TEXT-RE text))
                                (define truncated (truncate-string text 200))
                                (format "- [~a] ~a: ~a~a"
                                        (message-id m)
                                        (symbol->string (message-role m))
                                        truncated
                                        (if (pair? files)
                                            (format " [files: ~a]"
                                                    (string-join (take files (min 5 (length files)))
                                                                 ", "))
                                            "")))
                              "\n")))

;; Count of entries actually represented in simple-summary-text
(define (simple-summary-count entries)
  (min (length entries) 30))

;; ============================================================
;; Helpers
;; ============================================================

(define (extract-message-text msg)
  (define parts (message-content msg))
  (define texts
    (for/list ([part (in-list parts)]
               #:when (text-part? part))
      (text-part-text part)))
  (string-join texts " "))

;; truncate-string — now canonical in util/string-helpers.rkt
