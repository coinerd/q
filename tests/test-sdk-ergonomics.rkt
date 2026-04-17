#lang racket

;; q/tests/test-sdk-ergonomics.rkt — tests for Wave 10 SDK ergonomics
;;
;; Tests for:
;;   #1152: Unified create-agent-session entry point
;;   #1153: Thinking level control for reasoning models
;;   #1154: Context usage query API

(require rackunit
         racket/file
         "../llm/model.rkt"
         "../llm/provider.rkt"
         (only-in "../tools/tool.rkt" make-tool-registry)
         "../agent/event-bus.rkt"
         (only-in "../runtime/agent-session.rkt"
                  make-agent-session
                  agent-session?
                  agent-session-thinking-level
                  set-thinking-level!
                  thinking-level?
                  thinking-level->budget
                  thinking-levels
                  session-id)
         (only-in "../llm/token-budget.rkt"
                  context-usage
                  context-usage?
                  context-usage-total-tokens
                  context-usage-max-tokens
                  context-usage-usage-percent
                  context-usage-compaction-threshold
                  get-context-usage
                  context-usage-near-threshold?)
         "../interfaces/sdk.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-temp-session-dir)
  (make-temporary-file "q-sdk-ergo-test-~a" 'directory))

(define (make-test-provider)
  (make-mock-provider (make-model-response (list (hasheq 'type "text" 'text "Hello"))
                                           (hasheq 'inputTokens 5 'outputTokens 5)
                                           "mock"
                                           'stop)))

(define (cleanup-dir dir)
  (with-handlers ([exn:fail? (λ (_) (void))])
    (delete-directory/files dir #:must-exist? #f)))

;; ============================================================
;; Thinking level tests (#1153)
;; ============================================================

(test-case "thinking-level? validates all known levels"
  (for ([level '(off minimal low medium high xhigh)])
    (check-true (thinking-level? level) (format "expected ~a to be a valid thinking level" level)))
  (check-false (thinking-level? 'invalid))
  (check-false (thinking-level? "medium"))
  (check-false (thinking-level? 42))
  (check-false (thinking-level? #f)))

(test-case "thinking-level->budget maps correctly"
  (check-equal? (thinking-level->budget 'off) 0)
  (check-equal? (thinking-level->budget 'minimal) 1024)
  (check-equal? (thinking-level->budget 'low) 4096)
  (check-equal? (thinking-level->budget 'medium) 8192)
  (check-equal? (thinking-level->budget 'high) 16384)
  (check-equal? (thinking-level->budget 'xhigh) 32768)
  ;; Unknown level returns 0
  (check-equal? (thinking-level->budget 'invalid) 0)
  ;; Ordering: higher levels have larger budgets
  (check-true (> (thinking-level->budget 'high) (thinking-level->budget 'medium)))
  (check-true (> (thinking-level->budget 'medium) (thinking-level->budget 'low))))

(test-case "thinking-levels list contains all expected symbols"
  (check-equal? thinking-levels '(off minimal low medium high xhigh)))

(test-case "set-thinking-level! sets level on agent-session"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define bus (make-event-bus))
    (define cfg
      (hasheq 'provider prov 'tool-registry (make-tool-registry) 'event-bus bus 'session-dir tmp))
    (define sess (make-agent-session cfg))
    ;; Default is 'medium
    (check-equal? (agent-session-thinking-level sess) 'medium)
    ;; Set to high
    (set-thinking-level! sess 'high)
    (check-equal? (agent-session-thinking-level sess) 'high)
    ;; Set to off
    (set-thinking-level! sess 'off)
    (check-equal? (agent-session-thinking-level sess) 'off)
    (cleanup-dir tmp)))

(test-case "set-thinking-level! rejects invalid levels"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define bus (make-event-bus))
    (define cfg
      (hasheq 'provider prov 'tool-registry (make-tool-registry) 'event-bus bus 'session-dir tmp))
    (define sess (make-agent-session cfg))
    (check-exn exn:fail:contract? (λ () (set-thinking-level! sess 'bogus)))
    (cleanup-dir tmp)))

;; ============================================================
;; Context usage tests (#1154)
;; ============================================================

(test-case "context-usage struct accessors"
  (define cu (context-usage 5000 100000 5.0 72.0))
  (check-equal? (context-usage-total-tokens cu) 5000)
  (check-equal? (context-usage-max-tokens cu) 100000)
  (check-equal? (context-usage-usage-percent cu) 5.0)
  (check-equal? (context-usage-compaction-threshold cu) 72.0))

(test-case "context-usage? predicate"
  (define cu (context-usage 0 100000 0.0 72.0))
  (check-true (context-usage? cu))
  (check-false (context-usage? "not a struct"))
  (check-false (context-usage? #f)))

(test-case "get-context-usage computes percentage"
  ;; 50k / 100k = 50%
  (define cu (get-context-usage 50000 100000))
  (check-equal? (context-usage-total-tokens cu) 50000)
  (check-equal? (context-usage-max-tokens cu) 100000)
  (check-within (context-usage-usage-percent cu) 50.0 0.01)
  ;; threshold should be positive
  (check-true (> (context-usage-compaction-threshold cu) 0.0)))

(test-case "get-context-usage with zero max returns 0 percent"
  (define cu (get-context-usage 5000 0))
  (check-equal? (context-usage-total-tokens cu) 5000)
  (check-equal? (context-usage-max-tokens cu) 0)
  (check-equal? (context-usage-usage-percent cu) 0.0))

(test-case "context-usage-near-threshold? returns true when close"
  ;; 75% usage, 80% threshold, 10% margin → 75 > (80-10)=70 → true
  (define cu1 (context-usage 75000 100000 75.0 80.0))
  (check-true (context-usage-near-threshold? cu1 10.0)))

(test-case "context-usage-near-threshold? returns false when far"
  ;; 5% usage, 80% threshold, 10% margin → 5 > (80-10)=70 → false
  (define cu2 (context-usage 5000 100000 5.0 80.0))
  (check-false (context-usage-near-threshold? cu2 10.0)))

(test-case "context-usage-near-threshold? at exact threshold"
  ;; 80% usage, 80% threshold, 0% margin → 80 > (80-0)=80 → false (not strictly >)
  (define cu (context-usage 80000 100000 80.0 80.0))
  (check-false (context-usage-near-threshold? cu 0.0))
  ;; But with margin > 0: 80 > (80-0.1) → true
  (check-true (context-usage-near-threshold? cu 0.1)))

(test-case "get-context-usage returns transparent struct"
  (define cu (get-context-usage 1000 10000))
  (check-true (context-usage? cu))
  ;; Transparent — can inspect fields
  (check-pred context-usage? cu))

;; ============================================================
;; Unified create-agent-session tests (#1152)
;; ============================================================

(test-case "create-agent-session creates a runtime with session"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (create-agent-session #:provider prov #:session-dir tmp))
    (check-pred runtime? rt)
    ;; Session should be attached
    (check-not-false (runtime-rt-session rt))
    ;; Session ID should be non-empty
    (check-true (string? (session-id (runtime-rt-session rt))))
    (cleanup-dir tmp)))

(test-case "create-agent-session passes model-name through"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (create-agent-session #:provider prov #:session-dir tmp #:model-name "test-model"))
    (check-pred runtime? rt)
    (check-equal? (runtime-config-model-name (runtime-rt-config rt)) "test-model")
    (cleanup-dir tmp)))

(test-case "create-agent-session sets thinking-level when provided"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (create-agent-session #:provider prov #:session-dir tmp #:thinking-level 'high))
    (check-pred runtime? rt)
    (define sess (runtime-rt-session rt))
    (check-not-false sess)
    (check-equal? (agent-session-thinking-level sess) 'high)
    (cleanup-dir tmp)))

(test-case "create-agent-session defaults thinking-level to medium when not provided"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (create-agent-session #:provider prov #:session-dir tmp))
    (define sess (runtime-rt-session rt))
    (check-not-false sess)
    ;; Default from agent-session constructor is 'medium
    (check-equal? (agent-session-thinking-level sess) 'medium)
    (cleanup-dir tmp)))

(test-case "create-agent-session passes max-iterations through"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (create-agent-session #:provider prov #:session-dir tmp #:max-iterations 20))
    (check-equal? (runtime-config-max-iterations (runtime-rt-config rt)) 20)
    (cleanup-dir tmp)))

(test-case "create-agent-session passes system-instructions through"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt
      (create-agent-session #:provider prov
                            #:session-dir tmp
                            #:system-instructions '("You are a test agent.")))
    (check-equal? (runtime-config-system-instructions (runtime-rt-config rt))
                  '("You are a test agent."))
    (cleanup-dir tmp)))
