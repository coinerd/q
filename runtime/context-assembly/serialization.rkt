#lang racket/base
(require racket/contract)

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
                  message-meta-safe
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
         summarize-tool-result ;; re-exported from session-walk.rkt via context-assembly.rkt
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
         truncate-messages-to-budget
         gsd-progress-message?)

;; Default tier boundaries
(define DEFAULT-TIER-B-COUNT 20)
(define DEFAULT-TIER-C-COUNT 4)

;; v0.45.6 (SAL-03): Dynamic Tier C sizing — scales with total message count
;; At 200 messages: 4. At 400: 8. At 500+: 10-12.
(define (compute-tier-c-count total-messages)
  (min 12 (max 4 (quotient total-messages 50))))

(provide (contract-out [compute-tier-c-count
                        (-> exact-nonnegative-integer? exact-nonnegative-integer?)]))

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

(define (gsd-progress-message? m)
  (or (hash-ref (message-meta-safe m) 'gsd-pin #f)
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

(define (build-tiered-context messages
                              #:tier-b-count [tier-b-count #f]
                              #:tier-c-count [tier-c-count DEFAULT-TIER-C-COUNT]
                              #:working-set-messages [ws-messages '()]
                              #:trace [trace-cb #f])
  (when trace-cb
    (trace-cb 'start (hasheq 'total (length messages))))
  (define-values (compaction-summaries regular-msgs)
    (partition (lambda (m) (eq? (message-kind m) 'compaction-summary)) messages))
  (define-values (gsd-pinned regular) (partition gsd-progress-message? regular-msgs))
  (define-values (sys-protected unpinned-raw)
    (partition (lambda (m) (eq? (message-kind m) 'system-instruction)) regular))
  (define first-user-idx
    (for/first ([m (in-list unpinned-raw)]
                [i (in-naturals)]
                #:when (eq? (message-role m) 'user))
      i))
  (define-values (pinned-user unpinned)
    (if first-user-idx
        (values (list (list-ref unpinned-raw first-user-idx))
                (append (take unpinned-raw first-user-idx) (drop unpinned-raw (add1 first-user-idx))))
        (values '() unpinned-raw)))
  (define total (length unpinned))
  ;; v0.28.21 W4: Use dynamic Tier-B sizing when not explicitly specified
  (define effective-tier-b (or tier-b-count (compute-dynamic-tier-b-count total)))
  ;; v0.45.6 (SAL-03): Use dynamic Tier-C sizing when using default
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
  (define tier-a (append sys-protected pinned-user gsd-pinned compaction-summaries ws-messages))
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
  (tiered-context tier-a tier-b tier-c))

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
  (append (tiered-context-tier-a tc) (tiered-context-tier-b tc) (tiered-context-tier-c tc)))

;; Token-aware context assembly
;; CA-05: Uses build-session-context from session-walk.rkt
(define (build-session-context/tokens idx #:max-tokens max-tokens)
  (define messages (build-session-context idx))
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

;; CA-05: Import shared session walk logic from session-walk.rkt
(require "session-walk.rkt") ;; provides build-session-context, assemble-context, etc.

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

;; ============================================================
;; TEST-01: Isolated unit tests for gsd-progress-message?
;; ============================================================
(module+ test
  (require rackunit)

  (define (make-test-msg role text [meta (hasheq)])
    (make-message "test-id" #f role 'text (list (make-text-part text)) (current-seconds) meta))

  ;; Flag-based pinning
  (test-case "gsd-pin-flag-pins-message"
    (define m (make-test-msg 'assistant "wave done" (hasheq 'gsd-pin #t)))
    (check-true (gsd-progress-message? m)))

  (test-case "no-gsd-pin-flag-does-not-pin"
    (define m (make-test-msg 'assistant "regular message" (hasheq)))
    (check-false (gsd-progress-message? m)))

  ;; Regex-based pinning (fallback)
  (test-case "regex-matches-wave-complete"
    (define m (make-test-msg 'assistant "wave 3 marked complete"))
    (check-true (gsd-progress-message? m)))

  (test-case "regex-matches-wave-is-complete"
    (define m (make-test-msg 'tool "wave 5 is complete"))
    (check-true (gsd-progress-message? m)))

  (test-case "regex-matches-wave-done"
    (define m (make-test-msg 'assistant "wave 2 done"))
    (check-true (gsd-progress-message? m)))

  (test-case "regex-matches-plan-md-updated"
    (define m (make-test-msg 'tool "PLAN.md has been updated with wave 2 status"))
    (check-true (gsd-progress-message? m)))

  (test-case "regex-matches-state-md-created"
    (define m (make-test-msg 'assistant "STATE.md created"))
    (check-true (gsd-progress-message? m)))

  (test-case "regex-matches-handoff-json-written"
    (define m (make-test-msg 'assistant "HANDOFF.json written successfully"))
    (check-true (gsd-progress-message? m)))

  (test-case "regex-matches-milestone-complete"
    (define m (make-test-msg 'assistant "milestone v0.45.0 is now complete"))
    (check-true (gsd-progress-message? m)))

  (test-case "regex-matches-review-approved"
    (define m (make-test-msg 'assistant "review: APPROVED"))
    (check-true (gsd-progress-message? m)))

  ;; Negative cases
  (test-case "regex-no-match-user-role"
    (define m (make-test-msg 'user "wave 5 marked complete"))
    (check-false (gsd-progress-message? m)))

  (test-case "regex-no-match-non-gsd-text"
    (define m (make-test-msg 'assistant "I have completed the refactoring"))
    (check-false (gsd-progress-message? m))))
