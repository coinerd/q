#lang racket/base

;; runtime/context-assembly/context-floor.rkt — Core tiered context building
;; v0.76.0 W2: Extracted from serialization.rkt

(require racket/contract
         racket/list
         racket/match
         racket/string
         (only-in "../../util/content/content-parts.rkt" make-text-part)
         (only-in "../../util/message/message.rkt"
                  message
                  message?
                  message-id
                  message-kind
                  message-role
                  message-content
                  message-meta-safe
                  make-message)
         (only-in "../../util/content/content-parts.rkt" text-part text-part? text-part-text)
         (only-in "../../util/hook-types.rkt" hook-result-action hook-result-payload))

(provide tiered-context
         tiered-context?
         tiered-context-tier-a
         tiered-context-tier-b
         tiered-context-tier-c
         build-tiered-context
         tiered-context->message-list
         build-tiered-context-with-hooks
         compute-dynamic-tier-b-count
         context-assembly-payload
         context-assembly-payload?
         context-assembly-payload-tier-a-messages
         context-assembly-payload-tier-b-messages
         context-assembly-payload-tier-c-messages
         context-assembly-payload-max-tokens
         context-assembly-payload-metadata
         payload->tiered-context
         tiered-context->payload
         gsd-progress-message?
         tiered-context-with-tier-a)

;; Default tier boundaries
(define DEFAULT-TIER-B-COUNT 20)
(define DEFAULT-TIER-C-COUNT 4)

;; v0.45.6 (SAL-03): Dynamic Tier C sizing
(define (compute-tier-c-count total-messages)
  (min 12 (max 4 (quotient total-messages 50))))

(provide (contract-out [compute-tier-c-count
                        (-> exact-nonnegative-integer? exact-nonnegative-integer?)]))

;; v0.28.21 W4: Dynamic Tier-B sizing
(define (compute-dynamic-tier-b-count total-messages)
  (min 50 (max 20 (quotient total-messages 10))))

;; Context assembly hook payload
(struct context-assembly-payload (tier-a-messages tier-b-messages tier-c-messages max-tokens metadata)
  #:transparent)

;; Retention tier and provider protocol order are independent concerns. Keep
;; ordering metadata out of the public three-field struct so constructor and
;; match compatibility remain unchanged.
(struct tiered-context (tier-a tier-b tier-c) #:transparent)

(define provider-order-by-context (make-weak-hasheq))
(define PROVIDER-ORDER-METADATA-KEY 'q:provider-message-order)

(define (tiered-context-ordered tc)
  (hash-ref provider-order-by-context tc #f))

(define (make-tiered-context/ordered tier-a tier-b tier-c ordered)
  (define tc (tiered-context tier-a tier-b tier-c))
  (hash-set! provider-order-by-context tc ordered)
  tc)

(define (message-key m)
  (if (message? m)
      (message-id m)
      m))

(define (deduplicate/messages messages)
  (define seen (make-hash))
  (for/list ([m (in-list messages)]
             #:unless (hash-ref seen (message-key m) #f)
             #:do [(hash-set! seen (message-key m) #t)])
    m))

(define deduplicate/identity deduplicate/messages)

;; Retention tiers may classify messages out of chronology. Reconstruct selected
;; persisted messages in source order, then add genuinely injected records once.
(define (provider-ordered-messages source tier-a tier-b tier-c)
  (define selected (deduplicate/messages (append tier-a tier-b tier-c)))
  (define selected-keys
    (for/hash ([m (in-list selected)])
      (values (message-key m) #t)))
  (define source-keys
    (for/hash ([m (in-list source)])
      (values (message-key m) #t)))
  (define from-source
    (deduplicate/messages (for/list ([m (in-list source)]
                                     #:when (hash-ref selected-keys (message-key m) #f))
                            m)))
  (define injected
    (for/list ([m (in-list selected)]
               #:unless (hash-ref source-keys (message-key m) #f)
               ;; A standalone tool result has no assistant tool-call pair and
               ;; is invalid provider protocol. Working-set tool records that
               ;; belong to history are already retained through from-source.
               #:unless (and (message? m) (eq? (message-role m) 'tool)))
      m))
  (define-values (leading-system conversation)
    (splitf-at from-source (lambda (m) (eq? (message-kind m) 'system-instruction))))
  (append leading-system injected conversation))

(define (payload->tiered-context payload)
  (define tc
    (tiered-context (context-assembly-payload-tier-a-messages payload)
                    (context-assembly-payload-tier-b-messages payload)
                    (context-assembly-payload-tier-c-messages payload)))
  (define original-ordered
    (hash-ref (context-assembly-payload-metadata payload) PROVIDER-ORDER-METADATA-KEY #f))
  (when original-ordered
    ;; Hooks may add, remove, or reclassify tier records. Reconcile amended
    ;; membership against the original protocol order.
    (hash-set! provider-order-by-context
               tc
               (provider-ordered-messages original-ordered
                                          (tiered-context-tier-a tc)
                                          (tiered-context-tier-b tc)
                                          (tiered-context-tier-c tc))))
  tc)

(define (tiered-context->payload tc max-tokens [metadata (hasheq)])
  (define ordered (tiered-context-ordered tc))
  (context-assembly-payload (tiered-context-tier-a tc)
                            (tiered-context-tier-b tc)
                            (tiered-context-tier-c tc)
                            max-tokens
                            (if ordered
                                (hash-set metadata PROVIDER-ORDER-METADATA-KEY ordered)
                                metadata)))

;; gsd-progress-message? : message? -> boolean?
;; Detects messages that should be pinned to tier-a (GSD progress indicators).
(define (gsd-progress-message? m)
  (or (hash-ref (message-meta-safe m) 'gsd-pin #f)
      (hash-ref (message-meta-safe m) 'gsd-execution-instruction #f)
      (and (memq (message-role m) '(tool assistant))
           (let ([txt (string-join (for/list ([part (message-content m)]
                                              #:when (text-part? part))
                                     (text-part-text part)))])
             (and (non-empty-string? txt)
                  (regexp-match?
                   (regexp (string-append "wave [0-9]+ (marked complete|is complete|done)"
                                          "|PLAN\\.md.*(updated|created|written)"
                                          "|STATE\\.md.*(updated|created|written)"
                                          "|HANDOFF\\.json.*(written|updated)"
                                          "|milestone.*complete"
                                          "|review.*(APPROVED|REQUEST_CHANGES)"))
                   txt))))))

;; build-tiered-context : (listof message?) -> tiered-context?
;; Partitions messages into three tiers:
;;   tier-a: system instructions, pinned user messages, GSD progress, compaction summaries, working-set
;;   tier-b: recent regular messages (sliding window)
;;   tier-c: oldest regular messages (sliding window)
(define (build-tiered-context messages
                              #:tier-b-count [tier-b-count #f]
                              #:tier-c-count [tier-c-count DEFAULT-TIER-C-COUNT]
                              #:working-set-messages [ws-messages '()]
                              #:trace [trace-cb #f])
  (when trace-cb
    (trace-cb 'start (hasheq 'total (length messages))))
  (define-values (compaction-summaries regular-msgs)
    (partition (lambda (m) (eq? (message-kind m) 'compaction-summary)) messages))
  (cond
    [(pair? compaction-summaries)
     ;; Compaction already bounded this context. Preserve its protocol order
     ;; exactly: summary, verbatim kept window, then the new user prompt.
     (define ordered (provider-ordered-messages messages messages '() ws-messages))
     (when trace-cb
       (trace-cb
        'partition
        (hasheq 'tier-a (length ordered) 'tier-b 0 'tier-c 0 'gsd-pinned 0 'compaction-ordered? #t)))
     (define unique-ordered (deduplicate/identity ordered))
     (make-tiered-context/ordered unique-ordered '() '() unique-ordered)]
    [else
     (define-values (gsd-pinned regular) (partition gsd-progress-message? regular-msgs))
     (define-values (sys-protected unpinned-raw)
       (partition (lambda (m) (eq? (message-kind m) 'system-instruction)) regular))
     ;; UNIVERSAL PINNING: Pin ALL user messages to Tier A, not just the first.
     (define-values (pinned-user unpinned)
       (partition (lambda (m) (eq? (message-role m) 'user)) unpinned-raw))
     (define total (length unpinned))
     (define effective-tier-b (or tier-b-count (compute-dynamic-tier-b-count total)))
     (define effective-tier-c
       (if (= tier-c-count DEFAULT-TIER-C-COUNT)
           (compute-tier-c-count total)
           tier-c-count))
     (define tier-c-size (min effective-tier-c total))
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
     (define tier-a (append sys-protected pinned-user gsd-pinned ws-messages))
     (when trace-cb
       (trace-cb 'partition
                 (hasheq 'tier-a
                         (length tier-a)
                         'tier-b
                         (length tier-b)
                         'tier-c
                         (length tier-c)
                         'gsd-pinned
                         (length gsd-pinned))))
     (make-tiered-context/ordered tier-a
                                  tier-b
                                  tier-c
                                  (provider-ordered-messages messages tier-a tier-b tier-c))]))

;; build-tiered-context-with-hooks : variant with hook dispatch
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
        (match (hook-result-action result)
          ['block
           (define reason (hook-result-payload result))
           (raise (exn:fail (format "Context assembly blocked by hook: ~a"
                                    (if reason reason "no reason given"))
                            (current-continuation-marks)))]
          ['amend
           (define amended-payload (hook-result-payload result))
           (values (payload->tiered-context amended-payload) result)]
          ['pass (values base-context result)]
          [_ (values base-context result)]))
      (values base-context #f)))

(define (tiered-context->message-list tc)
  (or (tiered-context-ordered tc)
      (append (tiered-context-tier-a tc) (tiered-context-tier-b tc) (tiered-context-tier-c tc))))

;; Replace Tier A while preserving the base context's provider chronology.
;; Newly injected state/system records are prepended once; retained records keep
;; their original position from the base context.
(define (tiered-context-with-tier-a tc new-tier-a)
  (define base-ordered (tiered-context->message-list tc))
  (define base-keys
    (for/hash ([m (in-list base-ordered)])
      (values (message-key m) #t)))
  (define injected
    (for/list ([m (in-list (deduplicate/messages new-tier-a))]
               #:unless (hash-ref base-keys (message-key m) #f))
      m))
  (make-tiered-context/ordered new-tier-a
                               (tiered-context-tier-b tc)
                               (tiered-context-tier-c tc)
                               (append injected base-ordered)))
