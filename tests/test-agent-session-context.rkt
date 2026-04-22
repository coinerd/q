#lang racket

;; tests/test-agent-session-context.rkt — tiered context and event wiring
;;
;; Split from test-agent-session.rkt (T3-01, v0.16.1).

;; tests/test-agent-session.rkt — tests for runtime/agent-session.rkt
;;
;; Covers:
;;   1. Session creation
;;   2. Text-only prompt
;;   3. History accumulation across multiple prompts
;;   4. Session resume
;;   5. Session fork (full + partial)
;;   6. Tool-call execution loop
;;   7. Max-iteration guard
;;   8. Event emission order
;;   9. Session active/close
;;  10. Message struct input to run-prompt!
;;  11. Provider exception handling (BUG-34)

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         (only-in "../util/protocol-types.rkt"
                  message
                  message?
                  message-id
                  message-role
                  message-content
                  message-parent-id
                  message-kind
                  make-message
                  make-text-part
                  make-tool-result-part
                  text-part?
                  text-part-text
                  tool-call-part?
                  tool-call-part-id
                  tool-call-part-name
                  tool-result-part?
                  tool-result-part-content
                  tool-result-part-is-error?
                  make-loop-result
                  loop-result?
                  loop-result-termination-reason
                  loop-result-messages
                  loop-result-metadata
                  event
                  event-event
                  event-payload
                  event-ev
                  make-event
                  content-part->jsexpr)
         "../agent/event-bus.rkt"
         "../llm/model.rkt"
         "../llm/provider.rkt"
         (only-in "../tools/tool.rkt" make-tool make-tool-registry register-tool! make-success-result)
         (only-in "../extensions/api.rkt"
                  extension-registry?
                  make-extension-registry
                  register-extension!
                  extension)
         (only-in "../extensions/hooks.rkt" hook-pass hook-amend hook-block)
         "../runtime/agent-session.rkt"
         (only-in "../runtime/session-store.rkt" append-entry! load-session-log)
         (only-in "../runtime/compactor.rkt"
                  compaction-strategy
                  compaction-result->message-list
                  compact-history
                  build-tiered-context
                  tiered-context
                  tiered-context?
                  tiered-context-tier-a
                  tiered-context-tier-b
                  tiered-context-tier-c
                  tiered-context->message-list)
         (only-in "../runtime/token-compaction.rkt" token-compaction-config)
         (only-in "helpers/mock-provider.rkt"
                  make-multi-mock-provider
                  make-test-config
                  make-simple-mock-provider
                  make-tool-call-mock-provider)
         (only-in "../util/cancellation.rkt"
                  make-cancellation-token
                  cancellation-token?
                  cancellation-token-cancelled?
                  cancel-token!))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-temp-dir)
  (make-temporary-file "q-agent-session-test-~a" 'directory))

(define (make-event-collector bus)
  (define collected (box '()))
  (subscribe! bus (λ (evt) (set-box! collected (append (unbox collected) (list evt)))))
  collected)

(define (event-names collected-box)
  (map event-event (unbox collected-box)))

(define (text-of msg)
  (text-part-text (first (message-content msg))))

;; Helper: parse tool-call arguments (streaming produces JSON strings,
;; but test tools expect Racket hashes)
(define (parse-tool-args args)
  (cond
    [(hash? args) args]
    [(string? args)
     (with-handlers ([exn:fail? (lambda (_) (hash))])
       (define cleaned (string-trim args))
       (if (or (string=? cleaned "") (string=? cleaned "{}"))
           (hash)
           (for/hash ([m (in-list (regexp-match* #rx"\"([^\"]+)\"\\s*:\\s*\"([^\"]*)\""
                                                 cleaned
                                                 #:match-select values))])
             (values (string->symbol (cadr m)) (caddr m)))))]
    [else (hash)]))

;; ============================================================
;; Test suite
;; ============================================================

(define-test-suite
 test-tiered-context-suite
 ;; ── Helper to create messages with specific properties ──
 (test-case "build-tiered-context splits messages into three tiers"
   (define msgs
     (list
      ;; Older messages (will be Tier A - compacted)
      (make-message "id-1" #f 'user 'message (list (make-text-part "Old user 1")) 1000 (hasheq))
      (make-message "id-2"
                    "id-1"
                    'assistant
                    'message
                    (list (make-text-part "Old assistant 1"))
                    1001
                    (hasheq))
      ;; Recent messages (Tier B - full)
      (make-message "id-3" #f 'user 'message (list (make-text-part "Recent user")) 2000 (hasheq))
      (make-message "id-4"
                    "id-3"
                    'assistant
                    'message
                    (list (make-text-part "Recent assistant"))
                    2001
                    (hasheq))
      ;; Current turn (Tier C)
      (make-message "id-5" #f 'user 'message (list (make-text-part "Current user")) 3000 (hasheq))))

   ;; Compact the older portion to simulate Tier A
   (define-values (old-msgs recent-msgs) (values (take msgs 2) (drop msgs 2)))
   (define compact-result (compact-history msgs #:token-config (token-compaction-config 10 0 10)))
   (define compacted-context (compaction-result->message-list compact-result))

   ;; Now build tiered context
   (define tiered (build-tiered-context compacted-context #:tier-b-count 2 #:tier-c-count 1))

   ;; Tier A should have the summary message
   (check-equal? (length (tiered-context-tier-a tiered)) 1 "Tier A should have 1 summary message")
   (check-equal? (message-kind (first (tiered-context-tier-a tiered)))
                 'compaction-summary
                 "Tier A should contain compaction-summary message")

   ;; Tier B should have recent messages
   ;; With token compaction: summary + 3 kept = 4 msgs. Tier C=1, Tier B=2, Tier A=1
   (check-equal? (length (tiered-context-tier-b tiered))
                 2
                 "Tier B should have 2 messages (remaining after Tier C)")

   ;; Tier C should have the most recent message
   (check-equal? (length (tiered-context-tier-c tiered)) 1 "Tier C should have 1 message")
   (check-equal? (message-content (first (tiered-context-tier-c tiered)))
                 (message-content (last msgs))
                 "Tier C should contain the most recent message"))
 (test-case "build-tiered-context with no compaction returns empty Tier A"
   (define msgs
     (list (make-message "id-1" #f 'user 'message (list (make-text-part "User 1")) 1000 (hasheq))
           (make-message "id-2"
                         "id-1"
                         'assistant
                         'message
                         (list (make-text-part "Assistant 1"))
                         1001
                         (hasheq))
           (make-message "id-3" #f 'user 'message (list (make-text-part "User 2")) 1002 (hasheq))))

   ;; Build tiered context without prior compaction
   (define tiered (build-tiered-context msgs #:tier-b-count 2 #:tier-c-count 1))

   ;; Tier A should be empty (no compaction summaries)
   (check-equal? (length (tiered-context-tier-a tiered))
                 0
                 "Tier A should be empty when no compaction summaries exist")

   ;; Tier B should have the older messages
   (check-equal? (length (tiered-context-tier-b tiered)) 2 "Tier B should have 2 messages")

   ;; Tier C should have the most recent
   (check-equal? (length (tiered-context-tier-c tiered)) 1 "Tier C should have 1 message"))
 (test-case "tiered-context->message-list flattens in correct order"
   (define tier-a
     (list (make-message "sum-1"
                         #f
                         'system
                         'compaction-summary
                         (list (make-text-part "Summary"))
                         1000
                         (hasheq))))
   (define tier-b
     (list (make-message "id-1" #f 'user 'message (list (make-text-part "Tier B")) 2000 (hasheq))))
   (define tier-c
     (list (make-message "id-2" #f 'user 'message (list (make-text-part "Tier C")) 3000 (hasheq))))

   (define tiered (tiered-context tier-a tier-b tier-c))
   (define flat (tiered-context->message-list tiered))

   ;; Order should be: Tier A (summary), Tier B (recent), Tier C (current)
   (check-equal? (length flat) 3)
   (check-equal? (message-kind (first flat)) 'compaction-summary)
   (check-equal? (text-part-text (first (message-content (second flat)))) "Tier B")
   (check-equal? (text-part-text (first (message-content (third flat)))) "Tier C"))
 (test-case "build-tiered-context respects custom tier boundaries"
   (define msgs
     (for/list ([i (in-range 10)])
       (make-message (format "id-~a" i)
                     #f
                     'user
                     'message
                     (list (make-text-part (format "Message ~a" i)))
                     i
                     (hasheq))))

   ;; tier-b-count=3, tier-c-count=2
   (define tiered (build-tiered-context msgs #:tier-b-count 3 #:tier-c-count 2))

   ;; Total: 10 messages, Tier C takes last 2, Tier B takes 3 before that
   ;; Tier A is empty (no compaction), Tier B has 5, Tier C has 2? No...
   ;; Actually: Tier B = recent but not current = 3, Tier C = current = 2
   ;; Remaining = 10 - 3 - 2 = 5 would be Tier A if compacted, but we have no summaries
   (check-equal? (length (tiered-context-tier-a tiered)) 0)
   (check-equal? (length (tiered-context-tier-b tiered)) 3 "Tier B should have exactly 3 messages")
   (check-equal? (length (tiered-context-tier-c tiered)) 2 "Tier C should have exactly 2 messages"))
 (test-case "build-tiered-context with empty input"
   (define tiered (build-tiered-context '() #:tier-b-count 5 #:tier-c-count 1))
   (check-equal? (length (tiered-context-tier-a tiered)) 0)
   (check-equal? (length (tiered-context-tier-b tiered)) 0)
   (check-equal? (length (tiered-context-tier-c tiered)) 0))
 (test-case "build-tiered-context handles messages smaller than tier-c-count"
   (define msgs
     (list (make-message "id-1" #f 'user 'message (list (make-text-part "Only")) 1000 (hasheq))))

   (define tiered (build-tiered-context msgs #:tier-b-count 5 #:tier-c-count 3))

   ;; With only 1 message and tier-c-count=3, all go to Tier C
   (check-equal? (length (tiered-context-tier-a tiered)) 0)
   (check-equal? (length (tiered-context-tier-b tiered)) 0)
   (check-equal? (length (tiered-context-tier-c tiered)) 1)))

;; ============================================================

;; Fork/compact event wiring tests
;; ============================================================

(define test-event-wiring-suite
  (test-suite "event wiring for fork/compact"

    (test-case "fork.requested event triggers fork-session"
      (define bus (make-event-bus))
      (define tmpdir (make-temp-dir))
      (define prov
        (make-mock-provider
         (make-model-response (list (hash 'type "text" 'text "hi")) (hash) "mock" 'stop)))
      (define cfg
        (hasheq 'provider
                prov
                'tool-registry
                (make-tool-registry)
                'event-bus
                bus
                'session-dir
                tmpdir))
      (define sess (make-agent-session cfg))
      ;; Add a user message to the session via run-prompt!
      (run-prompt! sess "test message")
      ;; Collect events from fork
      (define fork-events (box '()))
      (subscribe! bus
                  (lambda (evt) (set-box! fork-events (cons evt (unbox fork-events))))
                  #:filter (lambda (e)
                             (member (event-ev e) '("session.fork.completed" "session.fork.failed"))))
      ;; Publish fork.requested with the first user message ID
      (define history (session-history sess))
      (define first-msg-id (and (pair? history) (message-id (car history))))
      (publish! bus
                (make-event "fork.requested"
                            1001
                            (session-id sess)
                            #f
                            (hasheq 'entry-id (or first-msg-id "unknown"))))
      ;; Allow thread to process (publish! is synchronous but yield for safety)
      (sync/timeout 0.5 never-evt)
      ;; Verify fork completed event was emitted
      (check-not-equal? (length (unbox fork-events))
                        0
                        "fork.requested should trigger session.fork.completed or session.forked")
      (close-session! sess)
      (delete-directory/files tmpdir))

    (test-case "compact.requested event triggers compaction"
      (define bus (make-event-bus))
      (define tmpdir (make-temp-dir))
      (define prov
        (make-mock-provider
         (make-model-response (list (hash 'type "text" 'text "hi")) (hash) "mock" 'stop)))
      (define cfg
        (hasheq 'provider
                prov
                'tool-registry
                (make-tool-registry)
                'event-bus
                bus
                'session-dir
                tmpdir))
      (define sess (make-agent-session cfg))
      ;; Add messages via run-prompt!
      (for ([i (in-range 3)])
        (run-prompt! sess (format "message ~a" i)))
      ;; Collect events from compact
      (define compact-events (box '()))
      (subscribe! bus
                  (lambda (evt) (set-box! compact-events (cons evt (unbox compact-events))))
                  #:filter (lambda (e)
                             (member (event-ev e)
                                     '("session.compact.completed" "session.compact.failed"))))
      ;; Publish compact.requested
      (publish! bus (make-event "compact.requested" 1001 (session-id sess) #f (hasheq)))
      ;; Allow thread to process (publish! is synchronous but yield for safety)
      (sync/timeout 0.5 never-evt)
      ;; Verify compact completed event was emitted
      (check-not-equal? (length (unbox compact-events))
                        0
                        "compact.requested should trigger session.compact.completed")
      (close-session! sess)
      (delete-directory/files tmpdir))))

(run-tests test-tiered-context-suite)
(run-tests test-event-wiring-suite)
