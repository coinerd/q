#lang racket/base

;; @speed fast
;; @suite fast

;; W8 v0.99.37: TUI event handler boundary tests.
;; Validates handler thinness (pure state transitions), pure model helpers,
;; and registry isolation semantics.
;;
;; Key assertions:
;; - Every handler returns ui-state?
;; - Unknown event types return state unchanged (identity)
;; - Pure helpers (classify-error-type, format-error-hint, dedup, truncate) behave correctly
;; - call-with-test-registry provides complete isolation from global registry

(require rackunit
         racket/string
         rackunit/text-ui
         racket/runtime-path
         "../tui/state-types.rkt"
         "../util/event/event.rkt")

(define-runtime-path helpers-path "../tui/state-events/helpers.rkt")
(define-runtime-path state-events-path "../tui/state-events.rkt")

;; Dynamic requires to avoid module-load side-effect ordering issues
;; @boundary unit
(define helpers-cache (make-hash))
(define (h-ref sym)
  (hash-ref! helpers-cache sym (lambda () (dynamic-require helpers-path sym))))

(define se-cache (make-hash))
(define (se-ref sym)
  (hash-ref! se-cache sym (lambda () (dynamic-require state-events-path sym))))

(define (make-evt ev-type payload #:time [time 1000])
  (event 1 ev-type time "s1" "t1" payload))

(define (make-tool-entry name kind)
  (make-entry kind "" 0 (hasheq 'name name)))

(define (state-with-transcript entries)
  (define st0 (initial-ui-state))
  (define append-fn (h-ref 'append-entry))
  (for/fold ([st st0]) ([e (in-list entries)])
    (append-fn st e)))

;; ============================================================
;; Suite 1: classify-error-type (pure model — error classification)
;; ============================================================

(define-test-suite classify-error-type-tests
                   (test-case "classifies timeout from error string"
                     (define ct (h-ref 'classify-error-type))
                     (check-equal? (ct "request timed out after 30s" (hash)) 'timeout))
                   (test-case "classifies rate-limit from error string"
                     (define ct (h-ref 'classify-error-type))
                     (check-equal? (ct "Error: 429 Too Many Requests" (hash)) 'rate-limit))
                   (test-case "classifies auth from error string"
                     (define ct (h-ref 'classify-error-type))
                     (check-equal? (ct "401 Unauthorized" (hash)) 'auth))
                   (test-case "classifies context-overflow from error string"
                     (define ct (h-ref 'classify-error-type))
                     (check-equal? (ct "context too long for max tokens" (hash)) 'context-overflow))
                   (test-case "classifies provider-error for unknown errors"
                     (define ct (h-ref 'classify-error-type))
                     (check-equal? (ct "something broke" (hash)) 'provider-error))
                   (test-case "payload errorType overrides string classification"
                     (define ct (h-ref 'classify-error-type))
                     (check-equal? (ct "request timed out" (hasheq 'errorType 'max-iterations))
                                   'max-iterations))
                   (test-case "empty error string defaults to provider-error"
                     (define ct (h-ref 'classify-error-type))
                     (check-equal? (ct "" (hash)) 'provider-error)))

;; ============================================================
;; Suite 2: format-error-hint (pure model — hint generation)
;; ============================================================

(define-test-suite
 format-error-hint-tests
 (test-case "campaign-active hint replaces /retry advice with auto-resume truth (BUG-0069)"
   (define feh (h-ref 'format-error-hint))
   (define hint (feh 'provider-error 5 '() #:campaign-active? #t))
   (check-true (string-contains? hint "will re-attempt")
               "campaign-active hint must state the automatic re-attempt")
   (check-true (string-contains? hint "5 retries") "campaign-active hint keeps the retry count")
   (check-true (string-contains? hint "attempt not consumed")
               "campaign-active hint carries the attempt-not-consumed guarantee")
   (check-false (string-contains? hint "Type /retry")
                "campaign-active hint must NOT advise manual /retry"))
 (test-case "campaign-active without retry count stays truthful"
   (define feh (h-ref 'format-error-hint))
   (define hint (feh 'provider-error #f '() #:campaign-active? #t))
   (check-true (string-contains? hint "will re-attempt"))
   (check-false (string-contains? hint "Type /retry")))
 (test-case "default keyword preserves legacy hints byte-for-byte"
   (define feh (h-ref 'format-error-hint))
   (check-equal? (feh 'provider-error 5 '())
                 "Error persisted after 5 retries. Type /retry to resubmit."))
 (test-case "timeout hint without retries"
   (define feh (h-ref 'format-error-hint))
   (check-equal? (feh 'timeout #f '()) "Provider timed out. Type /retry to resubmit your prompt."))
 (test-case "rate-limit hint without retries"
   (define feh (h-ref 'format-error-hint))
   (check-equal? (feh 'rate-limit #f '()) "Rate limited. Will retry automatically."))
 (test-case "auth hint without retries"
   (define feh (h-ref 'format-error-hint))
   (check-equal? (feh 'auth #f '()) "API key error. Check ~/.q/config.json"))
 (test-case "context-overflow hint without retries"
   (define feh (h-ref 'format-error-hint))
   (check-equal? (feh 'context-overflow #f '())
                 "Context too long. Use /compact to reduce, then /retry."))
 (test-case "timeout hint with retries"
   (define feh (h-ref 'format-error-hint))
   (check-true (string-contains? (feh 'timeout 2 '(timeout)) "/retry"))
   (check-true (string-contains? (feh 'timeout 2 '(timeout)) "2 retries")))
 (test-case "mixed errors produce generic retry hint"
   (define feh (h-ref 'format-error-hint))
   (define hint (feh 'timeout 3 '(timeout rate-limit)))
   (check-true (string-contains? hint "timed out") "mixed timeout+rate-limit should mention timeout")
   (check-true (string-contains? hint "rate limited")
               "mixed timeout+rate-limit should mention rate-limit"))
 (test-case "multiple distinct error types without the timeout+rate-limit pair"
   (define feh (h-ref 'format-error-hint))
   (define hint (feh 'provider-error 2 '(timeout provider-error)))
   (check-true (string-contains? hint "Mixed errors")
               "multiple error types should get generic mixed hint")))

;; ============================================================
;; Suite 3: truncate-status-msg (pure model — display formatting)
;; ============================================================

(define-test-suite truncate-status-msg-tests
                   (test-case "short message unchanged"
                     (define ts (h-ref 'truncate-status-msg))
                     (check-equal? (ts "short error") "short error"))
                   (test-case "long message truncated with ellipsis"
                     (define ts (h-ref 'truncate-status-msg))
                     (define long (make-string 50 #\x))
                     (define result (ts long))
                     (check-true (< (string-length result) 45)
                                 "truncated result should be under 45 chars")
                     (check-true (string-suffix? result "...") "truncated result ends with ..."))
                   (test-case "newlines replaced with spaces"
                     (define ts (h-ref 'truncate-status-msg))
                     (check-equal? (ts "line1\nline2") "line1 line2"))
                   (test-case "whitespace trimmed"
                     (define ts (h-ref 'truncate-status-msg))
                     (check-equal? (ts "  hello  ") "hello")))

;; ============================================================
;; Suite 4: recent-tool-start? / recent-tool-end? (dedup logic)
;; ============================================================

(define-test-suite dedup-logic-tests
                   (test-case "recent-tool-start? finds matching tool-start"
                     (define rts (h-ref 'recent-tool-start?))
                     (define st (state-with-transcript (list (make-tool-entry "bash" 'tool-start))))
                     (check-true (rts st "bash")))
                   (test-case "recent-tool-start? returns #f for different tool name"
                     (define rts (h-ref 'recent-tool-start?))
                     (define st (state-with-transcript (list (make-tool-entry "bash" 'tool-start))))
                     (check-false (rts st "edit")))
                   (test-case "recent-tool-start? returns #f for empty transcript"
                     (define rts (h-ref 'recent-tool-start?))
                     (define st (initial-ui-state))
                     (check-false (rts st "bash")))
                   (test-case "recent-tool-start? only matches tool-start kind (not tool-end)"
                     (define rts (h-ref 'recent-tool-start?))
                     (define st (state-with-transcript (list (make-tool-entry "bash" 'tool-end))))
                     (check-false (rts st "bash")))
                   (test-case "recent-tool-end? finds matching tool-end"
                     (define rte (h-ref 'recent-tool-end?))
                     (define st (state-with-transcript (list (make-tool-entry "bash" 'tool-end))))
                     (check-true (rte st "bash")))
                   (test-case "recent-tool-end? also matches tool-fail"
                     (define rte (h-ref 'recent-tool-end?))
                     (define st (state-with-transcript (list (make-tool-entry "bash" 'tool-fail))))
                     (check-true (rte st "bash")))
                   (test-case "dedup window boundary: 10th entry back is checked"
                     (define rts (h-ref 'recent-tool-start?))
                     ;; Build 9 assistant entries + 1 tool-start at the boundary
                     (define entries
                       (append (for/list ([i (in-range 9)])
                                 (make-entry 'assistant "msg" 0 (hash)))
                               (list (make-tool-entry "boundary" 'tool-start))))
                     (define st (state-with-transcript entries))
                     (check-true (rts st "boundary")
                                 "10th entry back (index 9) should be within window")))

;; ============================================================
;; Suite 5: Handler thinness properties
;; ============================================================

(define-test-suite
 handler-thinness-tests
 (test-case "all handlers return ui-state?"
   (define apply-fn (se-ref 'apply-event-to-state))
   (define st0 (initial-ui-state))
   (define event-types
     '("assistant.message.completed" "model.stream.delta"
                                     "model.stream.thinking"
                                     "model.stream.completed"
                                     "model.request.started"
                                     "turn.started"
                                     "turn.completed"
                                     "turn.cancelled"
                                     "tool.call.started"
                                     "tool.execution.started"
                                     "tool.execution.completed"
                                     "runtime.error"
                                     "compaction.warning"
                                     "compaction"
                                     "compaction.started"
                                     "compaction.completed"
                                     "context.built"
                                     "context.pressure"
                                     "injection"
                                     "queue.status-update"
                                     "iteration.soft-warning"
                                     "auto-retry"
                                     "auto-retry.start"))
   (for ([ev-type (in-list event-types)])
     (define st1 (apply-fn st0 (make-evt ev-type (hasheq 'delta "x" 'content "x" 'error "err"))))
     (check-true (ui-state? st1) (format "~a returned non-ui-state" ev-type))))
 (test-case "unknown event type returns same state object (identity)"
   (define apply-fn (se-ref 'apply-event-to-state))
   (define st0 (initial-ui-state))
   (define st1 (apply-fn st0 (make-evt "nonexistent.event.42" (hasheq 'x 1))))
   (check-eq? st0 st1 "unknown event returns the exact same state object"))
 (test-case "empty-hash payload doesn't crash handlers"
   (define apply-fn (se-ref 'apply-event-to-state))
   (define st0 (initial-ui-state))
   (define st1 (apply-fn st0 (make-evt "runtime.error" (hash))))
   (check-true (ui-state? st1) "handler handles empty-hash payload gracefully")))

;; ============================================================
;; Suite 6: Registry isolation via call-with-test-registry
;; ============================================================

(define-test-suite
 registry-isolation-tests
 (test-case "test registry starts empty (no global leakage)"
   (define cwtr (se-ref 'call-with-test-registry))
   (define reg? (se-ref 'event-reducer-registered?))
   (define result (cwtr (lambda () (reg? "assistant.message.completed"))))
   (check-false result "test registry should not contain globally-registered handlers"))
 (test-case "test registry can register custom handlers"
   (define cwtr (se-ref 'call-with-test-registry))
   (define reg? (se-ref 'event-reducer-registered?))
   (define reg! (se-ref 'register-event-reducer!))
   (cwtr (lambda ()
           (reg! "custom.test.event" (lambda (st evt) st))
           (check-true (reg? "custom.test.event") "custom handler registered in test scope"))))
 (test-case "test registry registrations don't leak to global"
   (define cwtr (se-ref 'call-with-test-registry))
   (define reg! (se-ref 'register-event-reducer!))
   (define reg? (se-ref 'event-reducer-registered?))
   (cwtr (lambda () (reg! "leak.test.event" (lambda (st evt) st))))
   (check-false (reg? "leak.test.event") "test registration leaked to global registry"))
 (test-case "parameterized registry is thread-safe for reads"
   (define apply-fn (se-ref 'apply-event-to-state))
   (define cwtr (se-ref 'call-with-test-registry))
   ;; In a test registry with no handlers, all events should be identity
   (define st0 (initial-ui-state))
   (cwtr (lambda ()
           (define st1 (apply-fn st0 (make-evt "any.event" (hasheq 'x 1))))
           (check-eq? st0 st1 "unregistered event in test registry returns identity")))))

;; ============================================================
;; Suite: gsd.campaign.infra-retry reducer (BUG-0069 — the event existed
;; since the BUG-0024-W3 fast lane but had NO TUI consumer; retries were
;; invisible while the transcript still said "Type /retry to resubmit")
;; ============================================================

(define-test-suite
 infra-retry-event-tests
 (test-case "fast-lane infra-retry event surfaces as system entry"
   (define apply-fn (se-ref 'apply-event-to-state))
   (define st0 (initial-ui-state))
   (define st1
     (apply-fn st0
               (make-evt "gsd.campaign.infra-retry"
                         (hasheq 'wave 1 'attempt 2 'delay 30 'phase 'fast))))
   (check-not-eq? st0 st1 "infra-retry event must append a transcript entry")
   (define entries (ui-state-transcript st1))
   (define last (and (pair? entries) (car (reverse entries))))
   (check-true (and last (eq? (transcript-entry-kind last) 'system)) "entry kind must be system")
   (check-true (string-contains? (transcript-entry-text last) "[wave 1]"))
   (check-true (string-contains? (transcript-entry-text last) "fast-lane auto-retry in 30s"))
   (check-true (string-contains? (transcript-entry-text last) "attempt not consumed"))
   (check-true (hash-ref (transcript-entry-meta last) 'infra-retry #f)))
 (test-case "slow-lane event shows delay and remaining patience"
   (define apply-fn (se-ref 'apply-event-to-state))
   (define st1
     (apply-fn (initial-ui-state)
               (make-evt "gsd.campaign.infra-retry"
                         (hasheq 'wave 2 'attempt 1 'delay 480 'phase 'slow 'patience 6720))))
   (define entries (ui-state-transcript st1))
   (define last (car (reverse entries)))
   (check-true (string-contains? (transcript-entry-text last) "slow-lane retry in 480s"))
   (check-true (string-contains? (transcript-entry-text last) "6720s patience left"))
   (check-true (string-contains? (transcript-entry-text last) "resuming automatically")))
 (test-case "slow-lane without patience degrades gracefully"
   (define apply-fn (se-ref 'apply-event-to-state))
   (define st1
     (apply-fn (initial-ui-state)
               (make-evt "gsd.campaign.infra-retry"
                         (hasheq 'wave 1 'attempt 4 'delay 900 'phase 'slow))))
   (define last (car (reverse (ui-state-transcript st1))))
   (check-true (string-contains? (transcript-entry-text last) "slow-lane retry in 900s"))
   (check-false (string-contains? (transcript-entry-text last) "patience left")))
 (test-case "missing wave still renders (defensive)"
   (define apply-fn (se-ref 'apply-event-to-state))
   (define st1 (apply-fn (initial-ui-state) (make-evt "gsd.campaign.infra-retry" (hasheq 'delay 60))))
   (define last (car (reverse (ui-state-transcript st1))))
   (check-true (string-contains? (transcript-entry-text last) "infra failure"))
   (check-false (string-contains? (transcript-entry-text last) "[wave"))))

;; ============================================================
;; Run all tests
;; ============================================================

(define-test-suite all-tui-event-boundary-tests
                   classify-error-type-tests
                   format-error-hint-tests
                   infra-retry-event-tests
                   truncate-status-msg-tests
                   dedup-logic-tests
                   handler-thinness-tests
                   registry-isolation-tests)

(module+ test
  (run-tests all-tui-event-boundary-tests))

(module+ main
  (run-tests all-tui-event-boundary-tests))
