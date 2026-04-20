#lang racket/base

;; q/runtime/context-manager.rkt — Context assembly from session log
;;
;; Replaces context-builder.rkt truncation with strategy-driven assembly.
;; The session log is immutable — this module decides what goes into the
;; LLM context window using pluggable strategies:
;;   1. Pin: system prompt + first user message (always)
;;   2. Summary: placeholder for LLM-generated summaries (Wave 2A)
;;   3. Recent: last N tokens kept verbatim
;;   4. Catalog: one-line-per-entry summary of excluded entries
;;   5. Budget enforcement: total ≤ token budget
;;
;; Issue #1390: Wave 0 — Context Manager Core

(require racket/list
         racket/string
         "../util/protocol-types.rkt"
         "../runtime/session-index.rkt"
         "../runtime/context-builder.rkt"
         "../llm/token-budget.rkt")

(provide context-manager-config
         context-manager-config?
         context-manager-config-recent-tokens
         context-manager-config-max-catalog-entries
         context-manager-config-max-catalog-tokens
         context-manager-config-summary-window
         make-context-manager-config
         ;; Core API
         assemble-context
         generate-catalog
         ;; Catalog entry struct
         catalog-entry
         catalog-entry?
         catalog-entry-id
         catalog-entry-role
         catalog-entry-summary
         ;; Token estimation (re-exported for tests)
         estimate-cm-message-tokens
         ;; Summary (Wave 2A #1395)
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
         summary-cache-store!)

;; ============================================================
;; Configuration
;; ============================================================

(struct context-manager-config
        (recent-tokens ; tokens for the recent window (default 30000)
         max-catalog-entries ; max catalog entries (default 40)
         max-catalog-tokens ; max catalog token budget (default 2000)
         summary-window) ; tokens for summary window (default 4000, Wave 2A placeholder)
  #:transparent)

(define (make-context-manager-config #:recent-tokens [recent 30000]
                                     #:max-catalog-entries [max-entries 40]
                                     #:max-catalog-tokens [max-tokens 2000]
                                     #:summary-window [summary 4000])
  (context-manager-config recent max-entries max-tokens summary))

;; ============================================================
;; Catalog entry struct
;; ============================================================

(struct catalog-entry (id role summary) #:transparent)

;; ============================================================
;; Summary struct (Wave 2A #1395)
;; ============================================================

(struct context-summary (from-id to-id text entry-count) #:transparent)

;; ============================================================
;; Summary cache (Wave 2A #1395)
;; ============================================================

(struct summary-cache ([table #:mutable]) #:transparent)

(define (make-summary-cache)
  (summary-cache (hash)))

(define (summary-cache-lookup cache from-id to-id)
  (hash-ref (summary-cache-table cache) (cons from-id to-id) #f))

(define (summary-cache-store! cache from-id to-id text)
  (set-summary-cache-table! cache (hash-set (summary-cache-table cache) (cons from-id to-id) text)))

;; ============================================================
;; Token estimation
;; ============================================================

(define (estimate-cm-message-tokens msg)
  (define text-parts
    (for/list ([part (in-list (message-content msg))]
               #:when (text-part? part))
      (text-part-text part)))
  (estimate-text-tokens (string-join text-parts " ")))

;; ============================================================
;; Core: assemble-context
;; ============================================================

;; Assemble a provider-ready message list from the session index.
;; Returns (values messages catalog-entries).
;;   messages: (listof message?) — the context window for the LLM
;;   catalog-entries: (listof catalog-entry?) — summaries of excluded entries
(define (assemble-context idx config)
  (define raw-messages (build-session-context idx))
  (cond
    [(null? raw-messages) (values '() '())]
    [else
     (define max-tokens (context-manager-config-recent-tokens config))
     ;; Phase 1: Identify pinned items (system prompt + first user message)
     (define-values (pinned removable) (partition-messages raw-messages))
     (define pinned-tokens (for/sum ([m (in-list pinned)]) (estimate-cm-message-tokens m)))

     ;; Phase 3: Fit recent messages within remaining budget
     (define remaining-budget (- max-tokens pinned-tokens))
     (define-values (recent excluded)
       (if (<= remaining-budget 0)
           (values '() removable)
           (fit-recent removable remaining-budget)))

     ;; Phase 4: Generate catalog for excluded entries
     (define max-entries (context-manager-config-max-catalog-entries config))
     (define catalog (generate-catalog excluded #:max-entries max-entries))

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

     ;; Ensure first user message is in result (pin guarantee)
     (define result-with-pin (ensure-first-user-pinned result-messages raw-messages))

     (values result-with-pin catalog)]))

;; ============================================================
;; Phase 1: Pin system prompt + first user message
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
                     (and first-user (eq? m first-user))))
               messages))
  (values protected removable))

;; ============================================================
;; Phase 3: Fit recent messages
;; ============================================================

(define (fit-recent messages budget)
  (let loop ([remaining (reverse messages)]
             [kept '()]
             [excluded '()]
             [used 0])
    (cond
      [(null? remaining) (values (reverse kept) (reverse excluded))]
      [else
       (define m (car remaining))
       (define tokens (estimate-cm-message-tokens m))
       (cond
         [(> (+ used tokens) budget) (loop (cdr remaining) kept (cons m excluded) used)]
         [else (loop (cdr remaining) (cons m kept) excluded (+ used tokens))])])))

;; ============================================================
;; Phase 4: Catalog generation
;; ============================================================

(define (generate-catalog entries #:max-entries [max-entries 40])
  (for/list ([m (in-list entries)]
             [i (in-naturals)]
             #:break (>= i max-entries))
    (define text (extract-message-text m))
    (define summary (truncate-string text 80))
    (define role-str (symbol->string (message-role m)))
    (catalog-entry (message-id m) role-str summary)))

;; ============================================================
;; Summary prompt template (Wave 2A #1395)
;; ============================================================

(define SUMMARY-MAX-WORDS 500)

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

(define (format-messages-for-summary messages)
  (string-join (for/list ([m (in-list messages)])
                 (define role (symbol->string (message-role m)))
                 (define text (extract-message-text m))
                 (format "[~a] (~a):\n  ~a" (message-id m) role (truncate-string text 500)))
               "\n\n"))

;; ============================================================
;; Generate context summary (Wave 2A #1395)
;; ============================================================

;; Summarize excluded entries. When provider+model are given, uses LLM.
;; Otherwise returns a simple concatenation summary.
;; Checks cache first; only generates if cache miss.
;; Returns context-summary? or #f.
(define (generate-context-summary entries provider model-name #:cache [cache #f])
  (cond
    [(null? entries) #f]
    [else
     (define from-id (message-id (first entries)))
     (define to-id (message-id (last entries)))
     ;; Check cache
     (define cached (and cache (summary-cache-lookup cache from-id to-id)))
     (cond
       [cached (context-summary from-id to-id cached (length entries))]
       [else
        ;; Generate summary
        (define summary-text
          (cond
            ;; LLM summarization would go here — requires provider-send
            ;; For now, use concatenation fallback
            [(and provider model-name) (simple-summary-text entries)]
            [else (simple-summary-text entries)]))
        ;; Store in cache
        (when cache
          (summary-cache-store! cache from-id to-id summary-text))
        (context-summary from-id to-id summary-text (length entries))])]))

;; Simple fallback: concatenate first lines of each entry
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

(define (ensure-first-user-pinned result original)
  (define first-user
    (for/first ([m (in-list original)]
                #:when (eq? (message-role m) 'user))
      m))
  (cond
    [(not first-user) result]
    [(member first-user result) result]
    [else
     (define target-id (message-id first-user))
     (define after-id
       (for/first ([m (in-list (dropf original (lambda (m) (not (equal? (message-id m) target-id)))))]
                   #:when (member m result))
         (message-id m)))
     (cond
       [after-id
        (let loop ([acc '()]
                   [rem result])
          (cond
            [(null? rem) (reverse (cons first-user acc))]
            [(equal? (message-id (car rem)) after-id)
             (loop (cons (car rem) (cons first-user acc)) (cdr rem))]
            [else (loop (cons (car rem) acc) (cdr rem))]))]
       [else (cons first-user result)])]))
