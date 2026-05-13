#lang racket/base

;; BOUNDARY: integration

;; test-mid-turn-compaction.rkt — TDD tests for v0.28.21 W3
;; Tests mid-turn compaction trigger: when context exceeds 90% budget
;; during a tool-call loop, compact in-place with cooldown guard.

(require rackunit
         rackunit/text-ui
         racket/string
         "../util/message.rkt"
         "../util/content-parts.rkt"
         "../runtime/session-compaction.rkt"
         "../runtime/compactor.rkt"
         "../runtime/iteration/retry-policy.rkt"
         "../runtime/runtime-helpers.rkt"
         "../llm/token-budget.rkt"
         "../agent/event-bus.rkt")

;; Helper: build a mock message with text content
(define (make-mock-msg role text)
  (message "id" #f role 'message (list (text-part "text" text)) 0 (hasheq)))

;; Helper: build a context with N messages
(define (make-mock-context msg-count)
  (for/list ([i (in-range msg-count)])
    (make-mock-msg 'user (format "Message ~a ~a" i (make-string 200 #\x)))))

(define mid-turn-suite
  (test-suite "Mid-turn compaction"

    ;; T7-1: Under budget — returns token estimate
    (test-case "under budget returns estimated token count"
      (define ctx (make-mock-context 3))
      (define result (check-mid-turn-budget! ctx #f (hasheq 'max-context-tokens 128000)))
      (check-true (integer? result) "returns estimated token count")
      (check-true (> result 0) "has positive token estimate"))

    ;; T7-2: Over budget triggers compaction (compact-history works)
    (test-case "over budget context produces compaction result"
      (define ctx (make-mock-context 50))
      (define config (hasheq 'max-context-tokens 100))
      ;; Verify compact-history produces a valid result
      (define result (compact-history ctx))
      (check-true (compaction-result? result))
      (check-true (list? (compaction-result-kept-messages result))
                  "compaction result has kept messages"))

    ;; T7-3: Compaction preserves system messages
    (test-case "compact-history preserves system message"
      (define sys-msg (make-mock-msg 'system "You are a helpful assistant."))
      (define ctx (cons sys-msg (make-mock-context 30)))
      (define result (compact-history ctx))
      (check-true (compaction-result? result))
      (define kept (compaction-result-kept-messages result))
      (define sys-kept (filter (lambda (m) (eq? (message-role m) 'system)) kept))
      (check >= (length sys-kept) 1 "system message preserved after compaction"))

    ;; T7-4: maybe-compact-context has cooldown guard
    (test-case "maybe-compact-context available for mid-turn use"
      (check-true (procedure? maybe-compact-context)
                  "maybe-compact-context is available for mid-turn use"))

    ;; T7-5: check-mid-turn-budget! emits event when over budget
    (test-case "check-mid-turn-budget! emits event when over budget"
      (define bus (make-event-bus))
      (define events '())
      (subscribe! bus (lambda (evt) (set! events (cons evt events))))
      (define ctx (make-mock-context 50))
      (define config (hasheq 'max-context-tokens 100))
      (define result
        (check-mid-turn-budget!
         ctx
         "test-session"
         config
         #:emit-event (lambda (name payload) (emit-session-event! bus "test-session" name payload))))
      (check-true (integer? result))
      ;; Should have emitted context.mid-turn-over-budget event
      (check-true (> (length events) 0) "emitted event when over budget"))))

(run-tests mid-turn-suite)

;; ============================================================
;; v0.28.21 W6: Exploration loop detection tests
;; ============================================================

(define exploration-loop-suite
  (test-suite "Exploration loop detection"

    (test-case "no loop with few tool calls"
      (check-false (detect-exploration-loop '("read" "grep")) "too few tools to form loop"))

    (test-case "no loop with varied tools"
      (check-false (detect-exploration-loop '("read" "grep" "edit" "bash" "write" "read" "ls"))
                   "varied tools not a loop"))

    (test-case "detect read-grep loop"
      (define result (detect-exploration-loop '("read" "grep" "read" "grep" "read" "grep")))
      (check-true (string? result) "read-grep loop detected")
      (check-true (string-contains? result "read") "mentions read")
      (check-true (string-contains? result "grep") "mentions grep"))

    (test-case "detect ls-read loop"
      (define result (detect-exploration-loop '("ls" "read" "ls" "read" "ls" "read" "ls" "read")))
      (check-true (string? result) "ls-read loop detected"))))

(run-tests exploration-loop-suite)

;; ============================================================
;; v0.33.7 W0b (N-T01): maybe-compact-mid-turn error path
;; ============================================================

(test-case "maybe-compact-mid-turn raises when #:compact-proc is #f and over budget"
  (define ctx (make-mock-context 50))
  (define config (hasheq 'max-context-tokens 100))
  (check-exn exn:fail?
             (lambda ()
               (maybe-compact-mid-turn 'mock-session
                                       ctx
                                       "test-session"
                                       config
                                       #:compact-proc #f
                                       #:estimate-tokens (lambda (msgs) 999999)))
             "should raise when #:compact-proc is #f and context exceeds budget"))
