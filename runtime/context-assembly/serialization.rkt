#lang racket/base

;; runtime/context-assembly/serialization.rkt — tiered context, entry conversion, agents discovery
;;
;; Wire format construction, tiered context, and file discovery.

(require racket/list
         racket/string
         racket/file
         (only-in "../../util/protocol-types.rkt"
                  message
                  message-id
                  message-kind
                  message-role
                  make-message
                  make-text-part)
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
         context-assembly-payload
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
  (define tier-c-size (min tier-c-count total))
  (define tier-c (if (> tier-c-size 0) (take-right unpinned tier-c-size) '()))
  (define remaining-after-c (if (> tier-c-size 0) (drop-right unpinned tier-c-size) unpinned))
  (define remaining-count (length remaining-after-c))
  (define tier-b-size (min tier-b-count remaining-count))
  (define tier-b (if (> tier-b-size 0) (take-right remaining-after-c tier-b-size) '()))
  (tiered-context (append sys-protected pinned-user compaction-summaries ws-messages) tier-b tier-c))

(define (build-tiered-context-with-hooks messages
                                         #:hook-dispatcher [hook-dispatcher #f]
                                         #:tier-b-count [tier-b-count DEFAULT-TIER-B-COUNT]
                                         #:tier-c-count [tier-c-count DEFAULT-TIER-C-COUNT]
                                         #:max-tokens [max-tokens 8192]
                                         #:working-set-messages [ws-messages '()])
  (define base-context
    (build-tiered-context messages
                          #:tier-b-count tier-b-count
                          #:tier-c-count tier-c-count
                          #:working-set-messages ws-messages))
  (define payload
    (tiered-context->payload base-context max-tokens
                             (hasheq 'tier-b-count tier-b-count
                                     'tier-c-count tier-c-count
                                     'total-messages (length messages))))
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

;; Import session index for tree walk
(require (only-in "../session-index.rkt" active-leaf get-branch))
(require (only-in "../../util/protocol-types.rkt"
                  compaction-summary-entry?
                  message-parent-id))

(define (build-session-context-from-idx idx)
  (define leaf (active-leaf idx))
  (cond
    [(not leaf) '()]
    [else
     (define path (get-branch idx (message-id leaf)))
     (cond
       [(not path) '()]
       [else (assemble-context path)])]))

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
  (cond
    [(not idx) (values path #f)]
    [else (define-values (pre post) (split-at path idx)) (values pre post)]))

(define (entry->context-message entry)
  (define kind (message-kind entry))
  (cond
    [(memq kind '(message)) entry]
    [(eq? kind 'compaction-summary)
     (struct-copy message entry [role 'user])]
    [(eq? kind 'branch-summary)
     (struct-copy message entry [role 'user])]
    [(memq kind '(session-info model-change thinking-level-change)) #f]
    [(memq kind '(tool-result bash-execution)) entry]
    [(eq? kind 'system-instruction) entry]
    [(eq? kind 'custom-message) entry]
    [else entry]))

;; AGENTS.md context discovery
(define (load-agents-context working-directory)
  (define paths (discover-agents-files working-directory))
  (cond
    [(null? paths) ""]
    [else
     (define contexts
       (for/list ([p (in-list paths)] #:when (file-exists? p))
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
       (for/list ([p (in-list paths)] #:when (file-exists? p))
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
          (filter (lambda (s) (not (string=? s "")))
                  (list (if (string=? name "Unnamed Agent") "" (format "# ~a" name))
                        desc inst)))
        (string-join parts "\n\n")])]))
