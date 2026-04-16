#lang racket

;; tests/workflows/session/test-session-compact.rkt — Session compaction workflow test (#175)
;;
;; Verifies that long session compaction preserves usable continuity.
;; Tests both persisting and advisory compaction modes.

(require rackunit
         rackunit/text-ui
         (prefix-in sdk: "../../../interfaces/sdk.rkt")
         "../../../agent/event-bus.rkt"
         "../../../util/protocol-types.rkt"
         (only-in "../../../tools/tool.rkt" make-tool-registry)
         (only-in "../../../runtime/compactor.rkt"
                  compaction-result?
                  compaction-result-removed-count
                  compaction-result-summary-message
                  compaction-result-kept-messages
                  compact-and-persist!
                  compact-history)
         (only-in "../../../runtime/token-compaction.rkt" token-compaction-config)
         "../../helpers/compaction-helpers.rkt"
         (only-in "../../../runtime/agent-session.rkt" session-history)
         "../fixtures/mock-provider.rkt"
         "../fixtures/temp-project.rkt"
         "../fixtures/session-assert.rkt"
         "../fixtures/event-recorder.rkt"
         "../fixtures/workflow-runner.rkt")

;; ============================================================
;; Helpers
;; ============================================================

;; Build a list of N text responses for the scripted provider
(define (make-n-text-responses n)
  (for/list ([i (in-range n)])
    (text-response (format "Response ~a" (add1 i)))))

;; ============================================================
;; Test suite
;; ============================================================

(define session-compact-tests
  (test-suite "Session Compaction Workflow"

    ;; ── Outcome: persisting compaction reduces history ──
    (test-case "persisting compaction removes old messages and appends summary"

      ;; Use 30 prompts: default strategy keeps 20 recent,
      ;; so 60 messages (30 user + 30 assistant) means 40 should be compactable.
      (define num-prompts 30)
      (define provider (make-scripted-provider (make-n-text-responses num-prompts)))

      (define-values (project-dir session-dir) (make-temp-project '()))

      (with-handlers ([exn:fail? (lambda (e)
                                   (cleanup-temp-project! project-dir session-dir)
                                   (raise e))])
        (define reg (make-tool-registry))
        (define bus (make-event-bus))
        (define recorder (make-event-recorder bus))

        (define rt
          (sdk:make-runtime #:provider provider
                            #:session-dir session-dir
                            #:tool-registry reg
                            #:event-bus bus
                            #:max-iterations 10))
        (define rt-open (sdk:open-session rt))
        (define sid (hash-ref (sdk:session-info rt-open) 'session-id))

        ;; Run prompts through the same session
        (define final-rt
          (for/fold ([current-rt rt-open]) ([i (in-range num-prompts)])
            (define-values (next-rt _) (sdk:run-prompt! current-rt (format "Prompt ~a" (add1 i))))
            next-rt))

        (define log-path (build-path session-dir sid "session.jsonl"))
        (define entries-before (session-log-entries log-path))
        (define count-before (length entries-before))

        ;; Verify we have expected user + assistant entries
        (check-equal? (length (entries-with-role entries-before 'user))
                      num-prompts
                      (format "Should have ~a user entries before compaction" num-prompts))
        (check-equal? (length (entries-with-role entries-before 'assistant))
                      num-prompts
                      (format "Should have ~a assistant entries before compaction" num-prompts))

        ;; Compact with persist — use low token config so compaction actually triggers
        (define history (session-history (sdk:runtime-rt-session final-rt)))
        (define low-tc LOW-TOKEN-CONFIG)
        (define compaction-res (compact-and-persist! history log-path #:token-config low-tc))

        ;; ── Outcome: compaction result has removed-count > 0 ──
        (check-pred compaction-result?
                    compaction-res
                    "compact-and-persist! should return a compaction-result")
        (check >
               (compaction-result-removed-count compaction-res)
               0
               "Compaction should remove at least one message")

        ;; ── Durable state: session log still valid after compaction ──
        (check-eq? (check-session-jsonl-valid log-path)
                   #t
                   "Session log should remain valid after compaction")

        ;; ── Durable state: log has compaction summary entry appended ──
        (define entries-after (session-log-entries log-path))
        (check >
               (length entries-after)
               count-before
               "Session log should have more entries after persisting compaction (summary appended)")

        ;; Find compaction summary entries
        (define compaction-summaries
          (filter (lambda (m) (eq? (message-kind m) 'compaction-summary)) entries-after))
        (check-equal? (length compaction-summaries)
                      1
                      "Should have exactly one compaction-summary entry")

        ;; Verify summary message content
        (define summary-msg (car compaction-summaries))
        (check-equal? (message-role summary-msg) 'system "Compaction summary should have system role")

        ;; ── Side-effects: compaction events emitted ──
        (clear-events! recorder) ;; clear events from the session run
        ;; Re-run compaction to capture events cleanly
        (define-values (_rt2 res2) (sdk:compact-session! final-rt #:persist? #t))
        (check-not-false (member "compaction.started" (event-names recorder))
                         "Events should include compaction.started")
        (check-not-false (member "compaction.completed" (event-names recorder))
                         "Events should include compaction.completed")

        ;; ── Tree structure still valid ──
        (check-eq? (check-session-tree-structure log-path)
                   #t
                   "Session tree structure should remain valid after compaction")

        (cleanup-temp-project! project-dir session-dir)))

    ;; ── Outcome: advisory compaction does not modify log ──
    (test-case "advisory compaction returns result without modifying log"

      ;; Use 30 prompts: default strategy keeps 20 recent,
      ;; so 60 messages means 40 should be compactable.
      (define num-prompts 30)
      (define provider (make-scripted-provider (make-n-text-responses num-prompts)))

      (define-values (project-dir session-dir) (make-temp-project '()))

      (with-handlers ([exn:fail? (lambda (e)
                                   (cleanup-temp-project! project-dir session-dir)
                                   (raise e))])
        (define reg (make-tool-registry))
        (define bus (make-event-bus))

        (define rt
          (sdk:make-runtime #:provider provider
                            #:session-dir session-dir
                            #:tool-registry reg
                            #:event-bus bus
                            #:max-iterations 10))
        (define rt-open (sdk:open-session rt))
        (define sid (hash-ref (sdk:session-info rt-open) 'session-id))

        ;; Run prompts
        (define final-rt
          (for/fold ([current-rt rt-open]) ([i (in-range num-prompts)])
            (define-values (next-rt _) (sdk:run-prompt! current-rt (format "Prompt ~a" (add1 i))))
            next-rt))

        (define log-path (build-path session-dir sid "session.jsonl"))
        (define entries-before (session-log-entries log-path))
        (define count-before (length entries-before))

        ;; Advisory compaction — use low token config so compaction triggers
        (define history (session-history (sdk:runtime-rt-session final-rt)))
        (define low-tc LOW-TOKEN-CONFIG)
        (define compaction-res (compact-history history #:token-config low-tc))

        ;; Result should still report removed-count > 0
        (check-pred compaction-result? compaction-res)
        (check >
               (compaction-result-removed-count compaction-res)
               0
               "Advisory compaction should still report removed messages")

        ;; But log should be UNCHANGED
        (define entries-after (session-log-entries log-path))
        (check-equal? (length entries-after)
                      count-before
                      "Advisory compaction should NOT modify the log")

        ;; No compaction-summary entries should exist
        (define compaction-summaries
          (filter (lambda (m) (eq? (message-kind m) 'compaction-summary)) entries-after))
        (check-equal? (length compaction-summaries)
                      0
                      "Advisory compaction should NOT append summary entries")

        ;; Log should still be valid
        (check-eq? (check-session-jsonl-valid log-path)
                   #t
                   "Session log should remain valid after advisory compaction")

        (cleanup-temp-project! project-dir session-dir)))

    ;; ── Boundary: compaction on short session removes nothing ──
    (test-case "compaction on short session removes nothing"

      (define provider (make-scripted-provider (list (text-response "Hello"))))

      (define-values (project-dir session-dir) (make-temp-project '()))

      (with-handlers ([exn:fail? (lambda (e)
                                   (cleanup-temp-project! project-dir session-dir)
                                   (raise e))])
        (define reg (make-tool-registry))
        (define bus (make-event-bus))

        (define rt
          (sdk:make-runtime #:provider provider
                            #:session-dir session-dir
                            #:tool-registry reg
                            #:event-bus bus
                            #:max-iterations 10))
        (define rt-open (sdk:open-session rt))
        (define sid (hash-ref (sdk:session-info rt-open) 'session-id))

        ;; Run just 1 prompt — well below compaction threshold
        (define-values (rt-1 _) (sdk:run-prompt! rt-open "Hi"))

        (define-values (_rt-after compaction-res) (sdk:compact-session! rt-1 #:persist? #t))

        ;; Compaction should report 0 removed for a short session
        (check-pred compaction-result? compaction-res)
        (check-equal? (compaction-result-removed-count compaction-res)
                      0
                      "Short session compaction should remove 0 messages")

        (cleanup-temp-project! project-dir session-dir)))))

;; ============================================================
;; Run
;; ============================================================

(module+ main
  (run-tests session-compact-tests))

(module+ test
  (run-tests session-compact-tests))
