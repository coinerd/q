#lang racket

;; benchmarks/run.rkt — benchmark harness entry point
;;
;; Runs benchmark tasks through q's agent loop (mock provider by default)
;; and collects timing, turn-count, and validation metrics.
;;
;; Usage:
;;   racket q/benchmarks/run.rkt                # mock mode (default)
;;   racket q/benchmarks/run.rkt --real          # use real provider
;;   racket q/benchmarks/run.rkt --verbose       # show per-task progress

(require racket/format
         racket/list
         racket/match
         racket/file
         racket/string
         "tasks.rkt"
         "metrics.rkt"
         "../llm/model.rkt"
         "../llm/provider.rkt"
         (only-in "../tools/tool.rkt"
                  make-tool-registry)
         "../agent/event-bus.rkt"
         "../agent/types.rkt"
         "../interfaces/sdk.rkt")

(provide run-benchmarks)

;; ============================================================
;; Mock provider for benchmarks
;; ============================================================

;; Deterministic mock responses keyed by task name.
;; Each produces a plausible response for its task category.
(define (make-benchmark-mock-provider task-name)
  (define response-text
    (case task-name
      [(explain-code)
       "The event-bus module provides a publish/subscribe pattern for event-driven communication. Subscribers register handlers via subscribe!, and publishers broadcast events via publish!. Each subscriber can optionally filter events by predicate."]
      [(write-test)
       "#lang racket\n(require rackunit\n  \"../agent/event-bus.rkt\")\n\n(test-case \"subscribe! adds handler\"\n  (define bus (make-event-bus))\n  (define results '())\n  (subscribe! bus (lambda (e) (set! results (cons e results))))\n  (publish! bus (make-event \"test\" 0 \"s1\" #f (hasheq)))\n  (check-equal? (length results) 1))"]
      [(edit-function)
       "(define (clear-all-subscribers! bus)\n  (define subs (event-bus-subscribers bus))\n  (set-box! subs '())\n  (void))\n\n;; Clears all subscribers from the event bus."]
      [(search-codebase)
       "Files importing from agent/types.rkt:\n1. q/agent/loop.rkt\n2. q/agent/event-bus.rkt\n3. q/runtime/agent-session.rkt\n4. q/interfaces/sdk.rkt\n5. q/tools/tool.rkt\n6. q/tests/test-types.rkt"]
      [(session-resume)
       "Session resumed. Previous context: we discussed the event-bus module, its publish/subscribe pattern, and how subscribers can filter events by predicate."]
      [else "Acknowledged."]))
  (make-mock-provider
   (make-model-response
    (list (hasheq 'type "text" 'text response-text))
    (hasheq 'inputTokens 10 'outputTokens 20)
    "benchmark-mock"
    'stop)))

;; ============================================================
;; Run a single task
;; ============================================================

(define (run-task task #:provider [provider #f])
  (define tmp-dir (make-temporary-file "q-bench-~a" 'directory))
  (define task-name (benchmark-task-name task))
  (define prov (or provider (make-benchmark-mock-provider task-name)))
  (define bus (make-event-bus))
  (define turn-count (box 0))
  (define tool-call-count (box 0))
  (define token-total (box 0))

  ;; Track events for metrics
  (subscribe! bus
              (lambda (evt)
                (case (event-event evt)
                  [("turn.completed")
                   (set-box! turn-count (add1 (unbox turn-count)))]
                  [("tool.call.started")
                   (set-box! tool-call-count (add1 (unbox tool-call-count)))]
                  [("model.stream.completed")
                   (define usage (hash-ref (event-payload evt) 'usage (hasheq)))
                   (define out-tok (hash-ref usage 'outputTokens 0))
                   (define in-tok (hash-ref usage 'inputTokens 0))
                   (set-box! token-total (+ (unbox token-total) out-tok in-tok))]
                  [else (void)])))

  (define start-ms (current-inexact-milliseconds))
  (define output
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (format "ERROR: ~a" (exn-message e)))])
      (define rt (make-runtime #:provider prov
                               #:session-dir tmp-dir
                               #:event-bus bus))
      (define rt-opened (open-session rt))
      (define-values (_rt-final result) (run-prompt! rt-opened (benchmark-task-prompt task)))
      ;; Extract text from result
      (cond
        [(loop-result? result)
         (define msgs (loop-result-messages result))
         ;; Get last assistant message text
         (define assistant-msgs
           (filter (lambda (m) (eq? (message-role m) 'assistant)) msgs))
         (if (null? assistant-msgs)
             "(no assistant response)"
             (let ([last-msg (last assistant-msgs)])
               (string-join
                (for/list ([p (in-list (message-content last-msg))]
                           #:when (text-part? p))
                  (text-part-text p))
                "")))]
        [(string? result) result]
        [else (format "~a" result)])))

  (define elapsed-ms (inexact->exact (truncate (- (current-inexact-milliseconds) start-ms))))

  ;; Cleanup temp directory
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (delete-directory/files tmp-dir #:must-exist? #f))

  (define status (if (string-prefix? output "ERROR:") 'error 'completed))
  (define m (make-metrics #:status status
                          #:elapsed-ms elapsed-ms
                          #:turns (unbox turn-count)
                          #:tool-calls (unbox tool-call-count)
                          #:tokens (unbox token-total)))
  (task-result task m output))

;; ============================================================
;; Report
;; ============================================================

(define (print-report results)
  (displayln "")
  (displayln "=== Benchmark Results ===")
  (displayln (format "~a~a~a~a~a"
                      (pad-right "Task" 22)
                      (pad-right "Status" 10)
                      (pad-right "Time" 10)
                      (pad-right "Turns" 7)
                      (pad-right "Valid" 8)))
  (displayln (make-string 57 #\-))
  (for ([r results])
    (define m (task-result-metrics r))
    (define t (task-result-task r))
    (define validation (validate-task-result r))
    (printf "~a~a~a~a~a\n"
            (pad-right (symbol->string (benchmark-task-name t)) 22)
            (pad-right (symbol->string (metrics-status m)) 10)
            (pad-right (format "~ams" (metrics-elapsed-ms m)) 10)
            (pad-right (~a (metrics-turns m)) 7)
            (pad-right (symbol->string validation) 8)))
  ;; Summary
  (define total-ms (apply + (map metrics-elapsed-ms (map task-result-metrics results))))
  (define pass-count (count (lambda (r) (eq? (validate-task-result r) 'pass)) results))
  (define total (length results))
  (displayln "")
  (printf "Total: ~a tasks, ~a passed, ~a failed, ~a errors\n"
          total pass-count
          (count (lambda (r) (eq? (validate-task-result r) 'fail)) results)
          (count (lambda (r) (eq? (validate-task-result r) 'error)) results))
  (printf "Total time: ~ams\n" total-ms)
  (displayln ""))

;; ============================================================
;; Public entry
;; ============================================================

(define (run-benchmarks #:provider [provider #f] #:verbose? [verbose? #f])
  (displayln "q benchmark harness")
  (displayln "")
  (define results
    (for/list ([task (all-tasks)])
      (when verbose?
        (printf "Running: ~a...\n" (benchmark-task-name task)))
      (define result (run-task task #:provider provider))
      (when verbose?
        (printf "  Status: ~a (~ams)\n"
                (metrics-status (task-result-metrics result))
                (metrics-elapsed-ms (task-result-metrics result))))
      result))
  (print-report results)
  results)

;; ============================================================
;; CLI entry point
;; ============================================================

(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (define verbose? (member "--verbose" args))
  (define real-mode? (member "--real" args))
  (define provider #f)  ;; #f = mock mode (default)

  (when real-mode?
    (displayln "WARNING: --real mode requires a configured provider."))
  (unless real-mode?
    (displayln "Mode: mock (deterministic, no API calls)"))

  (run-benchmarks #:provider provider #:verbose? verbose?))

;; ============================================================
;; Helpers
;; ============================================================

(define (pad-right s len)
  (define pad (- len (string-length s)))
  (if (> pad 0) (string-append s (make-string pad #\space)) s))
