#lang racket/base

;; q/runtime/context-builder.rkt — Context assembly from session tree
;;
;; Walks the session tree from active leaf to root, handling
;; compaction summaries, branch summaries, and settings entries.
;; Returns a provider-ready message list for LLM consumption.
;;
;; Issue #498: Context assembly pipeline (tree walk).

(require racket/file
         racket/list
         racket/string
         racket/set
         "../util/protocol-types.rkt"
         "../runtime/session-index.rkt"
         "../llm/token-budget.rkt"
         "../skills/context-files.rkt")

(provide build-session-context
         build-session-context/tokens
         truncate-messages-to-budget
         estimate-message-tokens
         entry->context-message
         load-agents-context
         build-system-preamble)

;; ============================================================
;; Context assembly pipeline (#498)
;; ============================================================

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
  ;;
  ;; Returns (listof message?) suitable for LLM provider consumption.
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
  ;; Handles compaction summaries and entry type filtering.
  (define-values (pre-compaction post-compaction) (split-at-compaction path))
  (define relevant-entries
    (if post-compaction
        ;; Include compaction summary + everything after it
        post-compaction
        ;; No compaction — use full path
        path))
  ;; Filter and transform entries
  (filter-map entry->context-message relevant-entries))

(define (split-at-compaction path)
  ;; Split path at the last compaction summary.
  ;; Returns (values pre-compaction post-compaction).
  ;; post-compaction includes the compaction summary itself.
  ;; If no compaction found, returns (values path #f).
  ;;
  ;; If the compaction summary has firstKeptEntryId in metadata,
  ;; use it to find the exact start of kept entries.
  (define compaction-idx
    (for/last ([entry (in-list path)]
               [i (in-naturals)]
               #:when (compaction-summary-entry? entry))
      i))
  (cond
    [(not compaction-idx) (values path #f)]
    [else
     (define compaction-entry (list-ref path compaction-idx))
     (define meta (message-meta compaction-entry))
     (define first-kept-id (and (hash? meta) (hash-ref meta 'firstKeptEntryId #f)))
     (cond
       ;; If firstKeptEntryId is set, find it in path and use it as the boundary
       [(and first-kept-id
             (for/or ([entry (in-list path)]
                      [i (in-naturals)])
               (and (equal? (message-id entry) first-kept-id) i)))
        =>
        (lambda (kept-idx)
          (define-values (pre post) (split-at path compaction-idx))
          ;; post = [compaction-summary, ..., first-kept, ...]
          ;; We want [compaction-summary] + messages from first-kept onward
          (values pre post))]
       [else
        (define-values (pre post) (split-at path compaction-idx))
        (values pre post)])]))

(define (entry->context-message entry)
  ;; Convert a session entry to a context message for LLM consumption.
  ;; Returns a message or #f if the entry should be excluded.
  (define kind (message-kind entry))
  (cond
    ;; Standard messages pass through
    [(memq kind '(message)) entry]
    ;; Compaction summaries become user-role messages
    [(eq? kind 'compaction-summary) (transform-summary-to-user entry)]
    ;; Branch summaries become user-role messages
    [(eq? kind 'branch-summary) (transform-summary-to-user entry)]
    ;; Filter out metadata entries
    [(memq kind '(session-info model-change thinking-level-change)) #f]
    ;; Tool results and bash executions pass through
    [(memq kind '(tool-result bash-execution)) entry]
    ;; System instructions pass through
    [(eq? kind 'system-instruction) entry]
    ;; Custom messages pass through
    [(eq? kind 'custom-message) entry]
    ;; Unknown kinds pass through (backward compatibility)
    [else entry]))

(define (transform-summary-to-user entry)
  ;; Transform a summary entry into a user-role message for context.
  (struct-copy message entry [role 'user]))

;; ============================================================
;; Token-aware context assembly (#646, #647)
;; ============================================================

;; Estimate token count for a single message struct.
;; Extracts text from all text-parts in the message content.
(define (estimate-message-tokens msg)
  (define text-parts
    (for/list ([part (in-list (message-content msg))]
               #:when (text-part? part))
      (text-part-text part)))
  (estimate-text-tokens (string-join text-parts " ")))

;; Build session context with token budget enforcement.
;; If the assembled messages exceed max-tokens, truncates older messages
;; while preserving system instructions and compaction summaries.
;; Returns (values messages total-tokens)
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

;; Truncate a message list to fit within a token budget.
;; Strategy:
;;   1. Always keep system-instruction and compaction-summary messages
;;   2. Remove oldest non-system messages until within budget
;;   3. Return truncated list
(define (truncate-messages-to-budget messages max-tokens)
  (cond
    [(null? messages) '()]
    [(<= (for/sum ([m (in-list messages)]) (estimate-message-tokens m)) max-tokens) messages]
    [else
     ;; Separate protected (system/compaction) from removable messages
     (define-values (protected removable)
       (partition (lambda (m) (memq (message-kind m) '(system-instruction compaction-summary)))
                  messages))
     ;; #1380: Find first user message to pin (never drop it)
     (define first-user-msg
       (for/first ([m (in-list messages)]
                   #:when (eq? (message-role m) 'user))
         m))
     ;; Protected messages always included; drop oldest removable until budget met
     (define protected-tokens (for/sum ([m (in-list protected)]) (estimate-message-tokens m)))
     (define pinned-tokens
       (if (and first-user-msg (not (member first-user-msg protected)))
           (estimate-message-tokens first-user-msg)
           0))
     (define remaining-budget (- max-tokens protected-tokens pinned-tokens))
     (cond
       [(<= remaining-budget 0) (pin-first-user protected first-user-msg messages)]
       [else
        ;; Keep most recent removable messages that fit
        (define kept-removable (fit-messages-from-recent removable remaining-budget))
        ;; Reassemble in original order: protected + kept-removable + pinned first-user
        (define kept-ids
          (for/set ([m (in-list kept-removable)])
            (message-id m)))
        (pin-first-user (for/list ([m (in-list messages)]
                                   #:when (or (memq (message-kind m)
                                                    '(system-instruction compaction-summary))
                                              (set-member? kept-ids (message-id m))))
                          m)
                        first-user-msg
                        messages)])]))

;; #1380: Ensure first user message is in the result list, preserving original order.
;; If it's already present, returns lst unchanged. Otherwise, inserts it at its original position.
(define (pin-first-user lst first-user-msg original-messages)
  (cond
    [(not first-user-msg) lst]
    [(member first-user-msg lst) lst]
    [else
     ;; Insert at correct position (where it was in original)
     (define target-id (message-id first-user-msg))
     ;; Find the message in original-messages that comes right after first-user-msg
     (define after-id
       (for/first ([m (in-list (dropf original-messages
                                      (lambda (m) (not (equal? (message-id m) target-id)))))]
                   #:when (member m lst))
         (message-id m)))
     (if after-id
         ;; Insert before the message that followed first-user in original
         (let loop ([result '()]
                    [remaining lst])
           (cond
             [(null? remaining) (reverse (cons first-user-msg result))]
             [(equal? (message-id (car remaining)) after-id)
              (loop (cons (car remaining) (cons first-user-msg result)) (cdr remaining))]
             [else (loop (cons (car remaining) result) (cdr remaining))]))
         ;; Can't find position — prepend
         (cons first-user-msg lst))]))

;; Fit as many messages as possible from the recent end within a token budget.
;; Returns messages in their original order.
(define (fit-messages-from-recent messages budget)
  ;; Walk backwards from newest, keeping messages that fit.
  ;; v0.15.2: Preserve tool_call/tool_result pairing — if we include a tool
  ;; result, also include its parent assistant (tool_call) message, and
  ;; if we include an assistant with tool_calls, include the tool results.
  ;; This prevents "messages parameter is illegal" 400 errors from the API.
  (define msg-ids
    (for/set ([m (in-list messages)])
      (message-id m)))
  ;; Build a map: tool-result-id → assistant-msg-id (via parentId)
  ;; and assistant-msg-id → list of tool-result-msg-ids
  (define tr->assistant (make-hash))
  (define assistant->trs (make-hash))
  (for ([m (in-list messages)])
    (when (eq? (message-role m) 'tool)
      (define pid (message-parent-id m))
      (when (and pid (set-member? msg-ids pid))
        (hash-set! tr->assistant (message-id m) pid)
        (hash-update! assistant->trs pid (lambda (lst) (cons (message-id m) lst)) '()))))
  ;; Also check assistant messages for tool_call parts
  (for ([m (in-list messages)])
    (when (eq? (message-role m) 'assistant)
      (define parts (message-content m))
      (when (and (pair? parts) (ormap tool-call-part? parts))
        ;; This assistant has tool_calls; find its tool results (next messages)
        (when (not (hash-has-key? assistant->trs (message-id m)))
          (hash-set! assistant->trs (message-id m) '())))))
  ;; Now walk backwards
  (let loop ([remaining (reverse messages)]
             [acc '()]
             [acc-ids (set)]
             [used 0])
    (cond
      [(null? remaining) (reverse acc)]
      [else
       (define m (car remaining))
       (define mid (message-id m))
       (cond
         ;; Already included (e.g., as a pair dependency)
         [(set-member? acc-ids mid) (loop (cdr remaining) acc acc-ids used)]
         [else
          (define tokens (estimate-message-tokens m))
          ;; Calculate pair tokens that must come with this message
          (define pair-tokens 0)
          ;; If this is a tool result, include its parent assistant
          (define required-assistant (hash-ref tr->assistant mid #f))
          (when required-assistant
            (unless (set-member? acc-ids required-assistant)
              (define ass-msg
                (for/first ([mm (in-list messages)]
                            #:when (equal? (message-id mm) required-assistant))
                  mm))
              (when ass-msg
                (set! pair-tokens (+ pair-tokens (estimate-message-tokens ass-msg))))))
          ;; If this is an assistant with tool_calls, include its tool results
          (define child-trs (hash-ref assistant->trs mid #f))
          (when child-trs
            (for ([tr-id (in-list child-trs)])
              (unless (set-member? acc-ids tr-id)
                (define tr-msg
                  (for/first ([mm (in-list messages)]
                              #:when (equal? (message-id mm) tr-id))
                    mm))
                (when tr-msg
                  (set! pair-tokens (+ pair-tokens (estimate-message-tokens tr-msg)))))))
          (define total-needed (+ used tokens pair-tokens))
          (cond
            [(> total-needed budget) (reverse acc)]
            [else
             ;; Include this message and its required pairs
             (define new-acc (cons m acc))
             (define new-ids (set-add acc-ids mid))
             (define new-used used)
             (set! new-used (+ new-used tokens))
             ;; Include parent assistant if needed
             (define-values (final-acc final-ids final-used)
               (if (and required-assistant (not (set-member? new-ids required-assistant)))
                   (let ([ass-msg (for/first ([mm (in-list messages)]
                                              #:when (equal? (message-id mm) required-assistant))
                                    mm)])
                     (if ass-msg
                         (values (cons ass-msg new-acc)
                                 (set-add new-ids required-assistant)
                                 (+ new-used (estimate-message-tokens ass-msg)))
                         (values new-acc new-ids new-used)))
                   (values new-acc new-ids new-used)))
             ;; Include child tool results if needed
             (define-values (final-acc2 final-ids2 final-used2)
               (if child-trs
                   (for/fold ([fa final-acc]
                              [fi final-ids]
                              [fu final-used])
                             ([tr-id (in-list child-trs)])
                     (if (set-member? fi tr-id)
                         (values fa fi fu)
                         (let ([tr-msg (for/first ([mm (in-list messages)]
                                                   #:when (equal? (message-id mm) tr-id))
                                         mm)])
                           (if tr-msg
                               (values (cons tr-msg fa)
                                       (set-add fi tr-id)
                                       (+ fu (estimate-message-tokens tr-msg)))
                               (values fa fi fu)))))
                   (values final-acc final-ids final-used)))
             (loop (cdr remaining) final-acc2 final-ids2 final-used2)])])])))

;; ============================================================
;; AGENTS.md context discovery (G2.3)
;; ============================================================

;; Discover and load AGENTS.md files from working-directory up to git root.
;; Returns the merged instructions string, or "" if none found.
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

;; Build a system preamble string from AGENTS.md discovery.
;; Returns a formatted string suitable for prepending to system messages,
;; or "" if no AGENTS.md files were found.
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
;; Utility
;; ============================================================

(define (filter-map f lst)
  (let loop ([lst lst]
             [acc '()])
    (cond
      [(null? lst) (reverse acc)]
      [else
       (let ([r (f (car lst))])
         (if r
             (loop (cdr lst) (cons r acc))
             (loop (cdr lst) acc)))])))
