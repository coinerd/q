#lang racket/base

;; runtime/context-assembly/serialization.rkt — tiered context, entry conversion, agents discovery
;;
;; Wire format construction, tiered context, and file discovery.

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
                  make-message
                  make-text-part)
         (only-in "../../util/content-parts.rkt" text-part text-part? text-part-text)
         (only-in "../context-policy.rkt" estimate-message-tokens)
         (only-in "../context-fit.rkt" truncate-messages-to-budget)
         (only-in "../../llm/provider.rkt" provider?)
         (only-in "../../util/hook-types.rkt" hook-result-action hook-result-payload)
         "../../skills/context-files.rkt"
         "budgeting.rkt")

(provide tiered-context
         tiered-context?
         tiered-context-tier-a
         tiered-context-tier-b
         tiered-context-tier-c
         build-tiered-context
         tiered-context->message-list
         build-tiered-context-with-hooks
         compute-dynamic-tier-b-count
         summarize-tool-result
         context-assembly-payload
         context-assembly-payload?
         context-assembly-payload-tier-a-messages
         context-assembly-payload-tier-b-messages
         context-assembly-payload-tier-c-messages
         context-assembly-payload-max-tokens
         context-assembly-payload-metadata
         payload->tiered-context
         tiered-context->payload
         build-session-context/tokens
         entry->context-message
         load-agents-context
         build-system-preamble
         truncate-messages-to-budget)

;; Default tier boundaries
(define DEFAULT-TIER-B-COUNT 20)
(define DEFAULT-TIER-C-COUNT 4)

;; v0.28.21 W4: Dynamic Tier-B sizing
;; Scales Tier-B with total message count: min(50, max(20, total/10))
;; More messages → larger Tier-B window for better mid-context retention.
(define (compute-dynamic-tier-b-count total-messages)
  (min 50 (max 20 (quotient total-messages 10))))

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
                              #:tier-b-count [tier-b-count #f]
                              #:tier-c-count [tier-c-count DEFAULT-TIER-C-COUNT]
                              #:working-set-messages [ws-messages '()])
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
  ;; v0.28.21 W4: Use dynamic Tier-B sizing when not explicitly specified
  (define effective-tier-b (or tier-b-count (compute-dynamic-tier-b-count total)))
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
  (define tier-b-size (min effective-tier-b remaining-count))
  (define tier-b
    (if (> tier-b-size 0)
        (take-right remaining-after-c tier-b-size)
        '()))
  (tiered-context (append sys-protected pinned-user compaction-summaries ws-messages) tier-b tier-c))

(define (build-tiered-context-with-hooks messages
                                         #:hook-dispatcher [hook-dispatcher #f]
                                         #:tier-b-count [tier-b-count #f]
                                         #:tier-c-count [tier-c-count DEFAULT-TIER-C-COUNT]
                                         #:max-tokens [max-tokens 8192]
                                         #:working-set-messages [ws-messages '()])
  (define base-context
    (build-tiered-context messages
                          #:tier-b-count tier-b-count
                          #:tier-c-count tier-c-count
                          #:working-set-messages ws-messages))
  (define effective-tier-b (or tier-b-count (compute-dynamic-tier-b-count (length messages))))
  (define payload
    (tiered-context->payload base-context
                             max-tokens
                             (hasheq 'tier-b-count
                                     effective-tier-b
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

;; Token-aware context assembly
(define (build-session-context/tokens idx #:max-tokens max-tokens)
  (define messages (build-session-context-from-idx idx))
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

;; Import session index for tree walk
(require (only-in "../session-index.rkt" active-leaf get-branch))
(require (only-in "../../util/protocol-types.rkt" compaction-summary-entry? message-parent-id))

(define (build-session-context-from-idx idx)
  (define leaf (active-leaf idx))
  (match leaf
    [#f '()]
    [_
     (define path (get-branch idx (message-id leaf)))
     (match path
       [#f '()]
       [_ (assemble-context path)])]))

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
        (struct-copy message entry [content (list (text-part "text" summary-text))])])]))

(define (entry->context-message entry)
  (define kind (message-kind entry))
  (match kind
    [(or 'message) entry]
    ['compaction-summary (struct-copy message entry [role 'user])]
    ['branch-summary (struct-copy message entry [role 'user])]
    [(or 'session-info 'model-change 'thinking-level-change) #f]
    [(or 'tool-result 'bash-execution) (summarize-tool-result entry)]
    ['system-instruction entry]
    ['custom-message entry]
    [_ entry]))

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
