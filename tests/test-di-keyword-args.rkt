#lang racket

;; tests/test-di-keyword-args.rkt — MOD-02: DI keyword args for iteration loop
;; v0.22.6 W4
;;
;; Tests that the DI keyword args on run-iteration-loop and run-provider-turn
;; accept mock functions and produce correct behavior.

(require rackunit
         rackunit/text-ui
         "../runtime/iteration.rkt"
         "../runtime/turn-orchestrator.rkt"
         "../runtime/compactor.rkt"
         "../runtime/auto-retry.rkt"
         "../agent/event-bus.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt"
         (only-in "../tools/tool.rkt" make-tool-registry)
         "../tools/registry-defaults.rkt"
         "../util/protocol-types.rkt"
         "../util/ids.rkt")

;; Helper: create a simple message for testing
(define (make-test-msg [role 'user] [content "test"])
  (define id (symbol->string (gensym 'msg)))
  (message id #f role 'text (list (hasheq 'type 'text 'text content)) (current-seconds) (hasheq)))

(define di-keyword-args-tests
  (test-suite "DI Keyword Args (MOD-02 v0.22.6)"

    ;; -------------------------------------------------------
    ;; call-with-overflow-recovery: #:compact-proc DI
    ;; -------------------------------------------------------
    (test-case "call-with-overflow-recovery uses injected compact-proc"
      (define bus (make-event-bus))
      (define ctx (list (make-test-msg)))
      (define compact-called? (box #f))
      ;; The overflow recovery calls compact-proc when a context-overflow-error? is raised
      (define (mock-compact ctx)
        (set-box! compact-called? #t)
        ;; Return a valid compaction-result
        (compaction-result (make-test-msg 'system "summary") 1 '()))
      (define retry-count (box 0))
      ;; First call raises overflow, retry (after compact) succeeds
      (with-check-info
       (['msg "compact-proc should be called"])
       (call-with-overflow-recovery (lambda ()
                                      (set-box! retry-count (add1 (unbox retry-count)))
                                      (when (= (unbox retry-count) 1)
                                        (raise (exn:fail "context_length_exceeded"
                                                         (current-continuation-marks)))))
                                    ctx
                                    bus
                                    "test-session"
                                    #:compact-proc mock-compact)
       (check-true (unbox compact-called?) "mock compact-proc was called")))

    ;; -------------------------------------------------------
    ;; call-with-overflow-recovery: default still works
    ;; -------------------------------------------------------
    (test-case "call-with-overflow-recovery default works (no overflow)"
      (define bus (make-event-bus))
      (define ctx (list (make-test-msg)))
      (define result (call-with-overflow-recovery (lambda () 'success) ctx bus "test-session"))
      (check-equal? result 'success "no-overflow case returns thunk result"))

    ;; -------------------------------------------------------
    ;; check-mid-turn-budget!: #:estimate-tokens DI
    ;; -------------------------------------------------------
    (test-case "check-mid-turn-budget! uses injected estimate-tokens"
      (define bus (make-event-bus))
      (define ctx (list (make-test-msg 'user "hello world")))
      ;; Mock that always returns 5000 tokens
      (define (mock-estimate messages)
        5000)
      ;; Use the default (real) estimate for comparison
      (define result
        (check-mid-turn-budget! ctx bus "test-session" (hash) #:estimate-tokens (lambda (msgs) 5000)))
      (check-equal? result 5000 "mock estimate-tokens was used"))

    ;; -------------------------------------------------------
    ;; check-mid-turn-budget!: DI estimate exceeds budget triggers event
    ;; -------------------------------------------------------
    (test-case "check-mid-turn-budget! emits event when DI estimate exceeds budget"
      (define bus (make-event-bus))
      (define events-received (box '()))
      (subscribe! bus
                  (lambda (evt)
                    (set-box! events-received (cons (event-ev evt) (unbox events-received)))))
      (define ctx (list (make-test-msg 'user "test")))
      ;; Set budget very low, mock estimate very high
      (define result
        (check-mid-turn-budget! ctx
                                bus
                                "test-session"
                                (hash 'max-context-tokens 1000)
                                #:estimate-tokens (lambda (msgs) 50000)))
      (check-equal? result 50000 "returns mock estimate")
      (check-not-false (member "context.mid-turn-over-budget" (unbox events-received))
                       "budget warning event was emitted"))

    ;; -------------------------------------------------------
    ;; run-provider-turn: #:tool-list-proc DI
    ;; -------------------------------------------------------
    (test-case "run-provider-turn uses injected tool-list-proc"
      (define bus (make-event-bus))
      (define reg (make-tool-registry))
      (define tool-called-with (box #f))
      (define (mock-tool-list base ext)
        (set-box! tool-called-with (cons base ext))
        ;; Return empty tool list — provider should still work
        '())
      (define prov
        (make-mock-provider
         (model-response (list (hasheq 'type 'text 'text "di-test"))
                         (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
                         "di-test-model"
                         'stop)
         #:name "di-tool-list-test"))
      (define ctx (list (make-test-msg 'user "test")))
      (define result
        (run-provider-turn ctx
                           prov
                           bus
                           reg
                           #f
                           "test-session"
                           "test-turn"
                           #f
                           (hash)
                           #:tool-list-proc mock-tool-list))
      (check-pred values result "run-provider-turn returns with mock tool-list-proc")
      (check-not-false (unbox tool-called-with) "mock tool-list-proc was called"))))

(run-tests di-keyword-args-tests)
