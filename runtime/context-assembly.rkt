#lang racket/base
;; STABILITY: evolving

;; q/runtime/context-assembly.rkt — Unified context assembly pipeline
;;
;; Single module for all context assembly logic. Replaces context-manager.rkt
;; and consolidates tiered-context from compactor.rkt.
;;
;; Pipeline:
;;   1. Tree walk from session index
;;   2. Pin system prompt + first user + compaction summaries
;;   3. Fit recent messages within budget (pair-preserving)
;;   4. Summarize excluded entries (LLM or fallback)
;;   5. Generate catalog of excluded entries
;;   6. Reassemble + inject summary
;;   7. Return context-result with diagnostics
;;
;; v0.23.1 W0: Created from context-manager.rkt + compactor tiered-context
;; v0.23.3 W0: Merged tree-walk from context-builder.rkt, deleted context-builder

(require racket/contract
         racket/file
         racket/list
         racket/string
         racket/set
         "../util/protocol-types.rkt"
         "../runtime/session-index.rkt"
         "../llm/token-budget.rkt"
         (only-in "../runtime/context-policy.rkt"
                  estimate-message-tokens
                  ensure-first-user-pinned
                  fit-messages-pair-preserving
                  user-message?
                  system-message?)
         (only-in "../runtime/compactor.rkt" llm-summarize)
         (only-in "../runtime/compaction-prompts.rkt" format-messages-for-summary)
         (only-in "../llm/provider.rkt" provider?)
         (only-in "../util/hook-types.rkt" hook-result-action hook-result-payload hook-result?)
         "../skills/context-files.rkt")

;; Config
(provide context-assembly-config
         context-assembly-config?
         context-assembly-config-recent-tokens
         context-assembly-config-max-catalog-entries
         context-assembly-config-max-catalog-tokens
         context-assembly-config-summary-window
         make-context-assembly-config
         ;; Backward compat aliases — DEPRECATED, use context-assembly-config-* instead
         ;; TODO: Remove in v0.25.0
         (rename-out
          [context-assembly-config context-manager-config]
          [context-assembly-config? context-manager-config?]
          [context-assembly-config-recent-tokens context-manager-config-recent-tokens]
          [context-assembly-config-max-catalog-entries context-manager-config-max-catalog-entries]
          [context-assembly-config-max-catalog-tokens context-manager-config-max-catalog-tokens]
          [context-assembly-config-summary-window context-manager-config-summary-window]
          [make-context-assembly-config make-context-manager-config])
         ;; Core API with contracts
         (contract-out [build-assembled-context
                        (->* [session-index? context-assembly-config?]
                             [#:provider (or/c #f provider?)
                              #:model-name (or/c #f string?)
                              #:cache (or/c #f summary-cache?)
                              #:trace-callback (or/c #f procedure?)]
                             context-result?)]
                       [build-session-context (-> session-index? (listof message?))]
                       [build-session-context/tokens
                        (->* [session-index? #:max-tokens exact-nonnegative-integer?]
                             (values (listof message?) exact-nonnegative-integer?))]
                       [truncate-messages-to-budget
                        (-> (listof message?) exact-nonnegative-integer? (listof message?))]
                       [entry->context-message (-> message? (or/c message? #f))]
                       [load-agents-context (-> path-string? string?)]
                       [build-system-preamble (-> path-string? string?)])
         ;; Result struct
         context-result
         context-result?
         context-result-messages
         context-result-total-tokens
         context-result-pinned-count
         context-result-recent-count
         context-result-excluded-count
         context-result-over-budget?
         context-result-catalog
         context-result-summary
         ;; Tiered context (from compactor)
         tiered-context
         tiered-context?
         tiered-context-tier-a
         tiered-context-tier-b
         tiered-context-tier-c
         build-tiered-context
         tiered-context->message-list
         build-tiered-context-with-hooks
         (struct-out context-assembly-payload)
         payload->tiered-context
         tiered-context->payload
         ;; Catalog
         catalog-entry
         catalog-entry?
         catalog-entry-id
         catalog-entry-role
         catalog-entry-summary
         generate-catalog
         ;; Summary
         context-summary
         context-summary?
         context-summary-from-id
         context-summary-to-id
         context-summary-text
         context-summary-entry-count
         context-summary-prompt
         generate-context-summary
         ;; Summary cache
         summary-cache
         make-summary-cache
         summary-cache-lookup
         summary-cache-store!
         summary-cache-count
         summary-cache-max-entries
         DEFAULT-CACHE-MAX-ENTRIES)

;; ============================================================
;; Configuration
;; ============================================================

;; Note: 0 is valid for max-catalog-* (disables catalog)
(struct context-assembly-config
        (recent-tokens ; tokens for the recent window (default 30000)
         max-catalog-entries ; max catalog entries (default 40)
         max-catalog-tokens ; max catalog token budget (default 2000)
         summary-window) ; tokens for summary window (default 4000)
  #:guard
  (lambda (recent max-entries max-tokens summary _name)
    (unless (and (exact-nonnegative-integer? recent) (> recent 0))
      (error 'context-assembly-config "recent-tokens must be a positive integer, got: ~a" recent))
    (unless (exact-nonnegative-integer? max-entries)
      (error 'context-assembly-config
             "max-catalog-entries must be a non-negative integer, got: ~a"
             max-entries))
    (unless (exact-nonnegative-integer? max-tokens)
      (error 'context-assembly-config
             "max-catalog-tokens must be a non-negative integer, got: ~a"
             max-tokens))
    (unless (and (exact-nonnegative-integer? summary) (> summary 0))
      (error 'context-assembly-config "summary-window must be a positive integer, got: ~a" summary))
    (values recent max-entries max-tokens summary))
  #:transparent)

(define (make-context-assembly-config #:recent-tokens [recent 30000]
                                      #:max-catalog-entries [max-entries 40]
                                      #:max-catalog-tokens [max-tokens 2000]
                                      #:summary-window [summary 4000])
  (context-assembly-config recent max-entries max-tokens summary))

;; ============================================================
;; Result struct — full diagnostics for observability
;; ============================================================

(struct context-result
        (messages ; (listof message?) — assembled context for LLM
         total-tokens ; integer — estimated total tokens in result
         pinned-count ; integer — number of pinned messages
         recent-count ; integer — number of recent (non-pinned) messages
         excluded-count ; integer — number of messages excluded from context
         over-budget? ; boolean — did messages exceed the token budget?
         catalog ; (listof catalog-entry?) — summaries of excluded
         summary) ; (or/c context-summary? #f) — generated summary
  #:transparent)

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
  #:transparent)

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
;; Core: build-assembled-context
;; ============================================================

;; Build the assembled context from a session index.
;; Returns a context-result struct with full diagnostics.
(define (build-assembled-context idx
                                 config
                                 #:cache [cache #f]
                                 #:provider [provider #f]
                                 #:model-name [model-name #f]
                                 #:trace-callback [trace #f])
  (define (emit-trace phase data)
    (when trace
      (trace phase data)))
  (define raw-messages (build-session-context idx))
  (emit-trace 'start (hash 'raw-count (length raw-messages)))
  (if (null? raw-messages)
      (begin
        (emit-trace 'empty (hash))
        (context-result '() 0 0 0 0 #f '() #f))
      (let ()
        ;; Per-assembly token memoization: estimate each message at most once
        (define token-memo (make-hash))
        (define (memoized-estimate msg)
          (hash-ref! token-memo (message-id msg) (lambda () (estimate-message-tokens msg))))

        (define max-tokens (context-assembly-config-recent-tokens config))
        ;; Phase 1: Identify pinned items (system + first user + compaction summaries)
        (define-values (pinned removable) (partition-messages raw-messages))
        (define pinned-tokens (for/sum ([m (in-list pinned)]) (memoized-estimate m)))
        (define pinned-count (length pinned))
        (define removable-count (length removable))
        (emit-trace 'phase1-pinned
                    (hash 'pinned-count
                          pinned-count
                          'removable-count
                          removable-count
                          'pinned-tokens
                          pinned-tokens))

        ;; Phase 3: Fit recent messages within remaining budget (pair-preserving)
        (define remaining-budget (- max-tokens pinned-tokens))
        (define-values (recent excluded)
          (if (<= remaining-budget 0)
              (values '() removable)
              (let ()
                (define kept
                  (fit-messages-pair-preserving removable remaining-budget memoized-estimate))
                (define kept-ids
                  (for/hash ([m (in-list kept)])
                    (values (message-id m) #t)))
                (define exc
                  (filter (lambda (m) (not (hash-has-key? kept-ids (message-id m)))) removable))
                (values kept exc))))
        (define recent-count (length recent))
        (define excluded-count (length excluded))
        (emit-trace 'phase3-fitted
                    (hash 'recent-count
                          recent-count
                          'excluded-count
                          excluded-count
                          'remaining-budget
                          remaining-budget))

        ;; Phase 2: Generate summary for excluded entries
        (define summary-obj (generate-context-summary excluded provider model-name #:cache cache))
        (emit-trace 'phase2-summary
                    (hash 'has-summary?
                          (and summary-obj #t)
                          'entry-count
                          (and summary-obj (context-summary-entry-count summary-obj))))

        (define summary-msg
          (and summary-obj
               (make-message (format "summary-~a-~a"
                                     (context-summary-from-id summary-obj)
                                     (context-summary-to-id summary-obj))
                             #f
                             'user
                             'context-assembly-summary
                             (list (make-text-part (context-summary-text summary-obj)))
                             (current-seconds)
                             (hasheq))))

        ;; Phase 4: Generate catalog for excluded entries
        (define max-entries (context-assembly-config-max-catalog-entries config))
        (define catalog
          (generate-catalog excluded
                            #:max-entries max-entries
                            #:max-tokens (context-assembly-config-max-catalog-tokens config)
                            #:estimate-text estimate-text-tokens))
        (define catalog-count (length catalog))
        (emit-trace 'phase4-catalog (hash 'catalog-count catalog-count))

        ;; Phase 5: Reassemble in original order
        (define pinned-ids
          (for/hash ([m (in-list pinned)])
            (values (message-id m) #t)))
        (define recent-ids
          (for/hash ([m (in-list recent)])
            (values (message-id m) #t)))
        (define result-messages
          (for/list ([m (in-list raw-messages)]
                     #:when (or (hash-has-key? pinned-ids (message-id m))
                                (hash-has-key? recent-ids (message-id m))))
            m))

        ;; Inject summary message after pinned, before recent
        (define result-with-summary
          (if (not summary-msg)
              result-messages
              (let ()
                (define pinned-last-pos
                  (for/last ([m (in-list result-messages)]
                             [i (in-naturals)]
                             #:when (hash-has-key? pinned-ids (message-id m)))
                    i))
                (define insert-pos
                  (if pinned-last-pos
                      (add1 pinned-last-pos)
                      0))
                (define-values (before after)
                  (split-at result-messages (min insert-pos (length result-messages))))
                (append before (list summary-msg) after))))

        ;; Ensure first user message is in result (pin guarantee)
        (define result-with-pin (ensure-first-user-pinned result-with-summary raw-messages))
        (define total-tokens (for/sum ([m (in-list result-with-pin)]) (memoized-estimate m)))
        (define over-budget? (> total-tokens max-tokens))

        (define result-count (length result-with-pin))
        (emit-trace 'done
                    (hash 'total-tokens
                          total-tokens
                          'message-count
                          result-count
                          'over-budget?
                          over-budget?
                          'memo-hits
                          (hash-count token-memo)))

        (context-result result-with-pin
                        total-tokens
                        pinned-count
                        recent-count
                        excluded-count
                        over-budget?
                        catalog
                        summary-obj))))

;; Backward-compatible entry point: returns just messages list
;; v0.23.3: Merged tree-walk from context-builder.rkt
(define (build-session-context idx)
  ;; Build a provider-ready message list from the session index.
  ;; Algorithm:
  ;;   1. Get active leaf from index
  ;;   2. Walk leaf→root collecting path entries
  ;;   3. Find latest compaction-summary on path
  ;;   4. If compaction found → include summary + messages after compaction point
  ;;   5. If no compaction → include all path messages
  ;;   6. Filter out settings/label/bookmark entries
  ;;   7. Transform summaries to user-role messages
  ;;   8. Return provider-ready message list (reversed to root→leaf order)
  (define leaf (active-leaf idx))
  (cond
    [(not leaf) '()]
    [else
     (define path (get-branch idx (message-id leaf)))
     (cond
       [(not path) '()]
       ;; path is root→leaf order from get-branch
       [else (assemble-context path)])]))

(define (assemble-context path)
  ;; Process a root→leaf path into context messages.
  (define-values (pre-compaction post-compaction) (split-at-compaction path))
  (define relevant-entries (if post-compaction post-compaction path))
  (filter-map entry->context-message relevant-entries))

(define (split-at-compaction path)
  ;; Split path at the last compaction summary.
  (define compaction-idx
    (for/last ([entry (in-list path)]
               [i (in-naturals)]
               #:when (compaction-summary-entry? entry))
      i))
  (cond
    [(not compaction-idx) (values path #f)]
    [else
     (define-values (pre post) (split-at path compaction-idx))
     (values pre post)]))

(define (entry->context-message entry)
  ;; Convert a session entry to a context message for LLM consumption.
  (define kind (message-kind entry))
  (cond
    [(memq kind '(message)) entry]
    [(eq? kind 'compaction-summary) (transform-summary-to-user entry)]
    [(eq? kind 'branch-summary) (transform-summary-to-user entry)]
    [(memq kind '(session-info model-change thinking-level-change)) #f]
    [(memq kind '(tool-result bash-execution)) entry]
    [(eq? kind 'system-instruction) entry]
    [(eq? kind 'custom-message) entry]
    [else entry]))

(define (transform-summary-to-user entry)
  (struct-copy message entry [role 'user]))

;; ============================================================
;; Phase 1: Pin system prompt + first user + compaction summaries
;; ============================================================

(define (partition-messages messages)
  (define first-user
    (for/first ([m (in-list messages)]
                #:when (eq? (message-role m) 'user))
      m))
  (define-values (protected removable)
    (partition (lambda (m)
                 (or (eq? (message-kind m) 'system-instruction)
                     (eq? (message-kind m) 'compaction-summary)
                     (eq? (message-kind m) 'context-assembly-summary)
                     (and first-user (eq? m first-user))))
               messages))
  (values protected removable))

;; ============================================================
;; Phase 3: Fit recent messages
;; ============================================================

;; ============================================================
;; Phase 4: Catalog generation
;; ============================================================
;; generate-catalog: Build a summary catalog from older entries.
;; Returns a list of (text . entry) pairs fitting within token budget.
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
  (cond
    [(null? entries) #f]
    [else
     (define from-id (message-id (first entries)))
     (define to-id (message-id (last entries)))
     (define cached (and cache (summary-cache-lookup cache from-id to-id)))
     (cond
       [cached (context-summary from-id to-id cached (length entries))]
       [else
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
        (context-summary from-id to-id summary-text actual-count)])]))

(define (simple-summary-text entries)
  (string-append "## Progress\n### Done\n"
                 (string-join (for/list ([m (in-list entries)]
                                         [i (in-naturals)]
                                         #:break (>= i 20))
                                (format "- [~a] ~a: ~a"
                                        (message-id m)
                                        (symbol->string (message-role m))
                                        (truncate-string (extract-message-text m) 100)))
                              "\n")))

;; Count of entries actually represented in simple-summary-text
(define (simple-summary-count entries)
  (min (length entries) 20))

;; ============================================================
;; Tiered Context Assembly (from compactor.rkt)
;; ============================================================

;; Default tier boundaries
(define DEFAULT-TIER-B-COUNT 20)
(define DEFAULT-TIER-C-COUNT 4)

;; R2-6: Context Assembly Hook Payload
(struct context-assembly-payload (tier-a-messages tier-b-messages tier-c-messages max-tokens metadata)
  #:transparent)

(struct tiered-context (tier-a tier-b tier-c) #:transparent)

(define (payload->tiered-context payload)
  (tiered-context (context-assembly-payload-tier-a-messages payload)
                  (context-assembly-payload-tier-b-messages payload)
                  (context-assembly-payload-tier-c-messages payload)))

(define (tiered-context->payload tc max-tokens [metadata (hasheq)])
  (context-assembly-payload (tiered-context-tier-a tc)
                            (tiered-context-tier-b tc)
                            (tiered-context-tier-c tc)
                            max-tokens
                            metadata))

(define (build-tiered-context messages
                              #:tier-b-count [tier-b-count DEFAULT-TIER-B-COUNT]
                              #:tier-c-count [tier-c-count DEFAULT-TIER-C-COUNT])
  (define-values (compaction-summaries regular-msgs)
    (partition (lambda (m) (eq? (message-kind m) 'compaction-summary)) messages))

  (define-values (sys-protected regular)
    (partition (lambda (m) (eq? (message-kind m) 'system-instruction)) regular-msgs))

  (define first-user-idx
    (for/first ([m (in-list regular)]
                [i (in-naturals)]
                #:when (eq? (message-role m) 'user))
      i))
  (define-values (pinned-user unpinned)
    (if first-user-idx
        (values (list (list-ref regular first-user-idx))
                (append (take regular first-user-idx) (drop regular (add1 first-user-idx))))
        (values '() regular)))

  (define total (length unpinned))
  (define tier-c-size (min tier-c-count total))
  (define tier-c
    (if (> tier-c-size 0)
        (take-right unpinned tier-c-size)
        '()))

  (define remaining-after-c
    (if (> tier-c-size 0)
        (drop-right unpinned tier-c-size)
        unpinned))

  (define remaining-count (length remaining-after-c))
  (define tier-b-size (min tier-b-count remaining-count))
  (define tier-b
    (if (> tier-b-size 0)
        (take-right remaining-after-c tier-b-size)
        '()))

  (tiered-context (append sys-protected pinned-user compaction-summaries) tier-b tier-c))

;; build-tiered-context-with-hooks: Assemble a tiered context (catalog + summary + recent)
;; and dispatch extension hooks at each stage. Returns a tiered-context struct.
(define (build-tiered-context-with-hooks messages
                                         #:hook-dispatcher [hook-dispatcher #f]
                                         #:tier-b-count [tier-b-count DEFAULT-TIER-B-COUNT]
                                         #:tier-c-count [tier-c-count DEFAULT-TIER-C-COUNT]
                                         #:max-tokens [max-tokens 8192])
  (define base-context
    (build-tiered-context messages #:tier-b-count tier-b-count #:tier-c-count tier-c-count))

  (define payload
    (tiered-context->payload base-context
                             max-tokens
                             (hasheq 'tier-b-count
                                     tier-b-count
                                     'tier-c-count
                                     tier-c-count
                                     'total-messages
                                     (length messages))))

  (if hook-dispatcher
      (let ([result (hook-dispatcher 'context-assembly payload)])
        (case (hook-result-action result)
          [(block)
           (define reason (hook-result-payload result))
           (raise (exn:fail (format "Context assembly blocked by hook: ~a"
                                    (if reason reason "no reason given"))
                            (current-continuation-marks)))]
          [(amend)
           (define amended-payload (hook-result-payload result))
           (values (payload->tiered-context amended-payload) result)]
          [(pass) (values base-context result)]
          [else (values base-context result)]))
      (values base-context #f)))

(define (tiered-context->message-list tc)
  (append (tiered-context-tier-a tc) (tiered-context-tier-b tc) (tiered-context-tier-c tc)))

;; ============================================================
;; Token-aware context assembly (from context-builder.rkt)
;; ============================================================

(define (build-session-context/tokens idx #:max-tokens max-tokens)
  (define messages (build-session-context idx))
  (cond
    [(null? messages) (values '() 0)]
    [else
     (define total (for/sum ([m (in-list messages)]) (estimate-message-tokens m)))
     (cond
       [(<= total max-tokens) (values messages total)]
       [else
        (define truncated (truncate-messages-to-budget messages max-tokens))
        (define new-total (for/sum ([m (in-list truncated)]) (estimate-message-tokens m)))
        (values truncated new-total)])]))

;; truncate-messages-to-budget: Trim messages from the front to fit within token budget.
;; Returns truncated message list with first-user pinning preserved.
(define (truncate-messages-to-budget messages max-tokens)
  (cond
    [(null? messages) '()]
    [(<= (for/sum ([m (in-list messages)]) (estimate-message-tokens m)) max-tokens) messages]
    [else
     (define-values (protected removable)
       (partition (lambda (m) (memq (message-kind m) '(system-instruction compaction-summary)))
                  messages))
     (define first-user-msg
       (for/first ([m (in-list messages)]
                   #:when (eq? (message-role m) 'user))
         m))
     (define protected-tokens (for/sum ([m (in-list protected)]) (estimate-message-tokens m)))
     (define pinned-tokens
       (if (and first-user-msg (not (member first-user-msg protected)))
           (estimate-message-tokens first-user-msg)
           0))
     (define remaining-budget (- max-tokens protected-tokens pinned-tokens))
     (cond
       [(<= remaining-budget 0) (ensure-first-user-pinned protected messages)]
       [else
        (define kept-removable (fit-messages-from-recent removable remaining-budget))
        (define kept-ids
          (for/set ([m (in-list kept-removable)])
            (message-id m)))
        (ensure-first-user-pinned
         (for/list ([m (in-list messages)]
                    #:when (or (memq (message-kind m) '(system-instruction compaction-summary))
                               (set-member? kept-ids (message-id m))))
           m)
         messages)])]))

;; Fit messages from recent end within budget (pair-preserving).
(define (fit-messages-from-recent messages budget)
  (fit-messages-pair-preserving messages budget))

;; ============================================================
;; AGENTS.md context discovery (from context-builder.rkt)
;; ============================================================

(define (load-agents-context working-directory)
  (define paths (discover-agents-files working-directory))
  (cond
    [(null? paths) ""]
    [else
     (define contexts
       (for/list ([p (in-list paths)]
                  #:when (file-exists? p))
         (define content (file->string p))
         (parse-agent-file content)))
     (cond
       [(null? contexts) ""]
       [else (agent-context-instructions (merge-agent-contexts contexts))])]))

(define (build-system-preamble working-directory)
  (define paths (discover-agents-files working-directory))
  (cond
    [(null? paths) ""]
    [else
     (define contexts
       (for/list ([p (in-list paths)]
                  #:when (file-exists? p))
         (define content (file->string p))
         (parse-agent-file content)))
     (cond
       [(null? contexts) ""]
       [else
        (define merged (merge-agent-contexts contexts))
        (define name (agent-context-name merged))
        (define desc (agent-context-description merged))
        (define inst (agent-context-instructions merged))
        (define parts
          (filter (λ (s) (not (string=? s "")))
                  (list (if (string=? name "Unnamed Agent")
                            ""
                            (format "# ~a" name))
                        desc
                        inst)))
        (string-join parts "\n\n")])]))

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

(define (truncate-string s max-len)
  (if (<= (string-length s) max-len)
      s
      (string-append (substring s 0 (- max-len 3)) "...")))
