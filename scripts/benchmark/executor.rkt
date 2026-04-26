#lang racket/base

;; scripts/benchmark/executor.rkt — Live and mock benchmark execution engine
;;
;; Runs a benchmark task through the SDK runtime with either a real LLM
;; provider (from config) or a mock provider. Captures trace and returns
;; structured execution results.

(require racket/file
         racket/format
         racket/list
         racket/match
         racket/port
         racket/string
         json
         (prefix-in sdk: "../../interfaces/sdk.rkt")
         (only-in "../../agent/event-bus.rkt" make-event-bus)
         (only-in "../../tools/tool.rkt" make-tool-registry)
         (only-in "../../tools/registry-defaults.rkt" register-default-tools!)
         (only-in "../../extensions/api.rkt" make-extension-registry)
         (only-in "../../wiring/run-modes.rkt" load-extensions-from-dir!)
         (only-in "../../runtime/trace-logger.rkt"
                  make-trace-logger
                  start-trace-logger!
                  stop-trace-logger!)
         (only-in "../../runtime/provider-factory.rkt" build-provider)
         (only-in "../../runtime/settings.rkt" load-settings)
         (only-in "../../llm/provider.rkt" make-mock-provider)
         (only-in "../../llm/model.rkt" make-model-response)
         (only-in "../../util/protocol-types.rkt" message-role message-content text-part-text)
         (only-in "../../util/jsonl.rkt" jsonl-read-all-valid)
         (only-in "../../extensions/gsd-planning-state.rkt" gsd-snapshot)
         "task.rkt")

(provide (struct-out execution-result)
         execute-task/mock
         execute-task/live
         execute-task/gsd-workflow
         build-live-provider
         trace-tool-calls
         trace-gsd-events)

;; ============================================================
;; Result struct
;; ============================================================

(struct execution-result
        (task-name ; string?
         trace-path ; (or/c path? #f)
         session-dir ; path?
         duration-ms ; exact-nonnegative-integer?
         iterations-used ; exact-nonnegative-integer?
         outcome ; symbol: 'completed | 'timeout | 'error
         error-msg ; (or/c string? #f)
         project-dir ; (or/c path? #f)
         raw-result) ; any — SDK run-prompt! result
  #:transparent)

;; ============================================================
;; Provider construction
;; ============================================================

;; build-live-provider : (or/c string? #f) -> provider?
;; Builds a real LLM provider from ~/.q/config.json or env vars.
;; If provider-override is given (format "provider/model"), uses that.
(define (build-live-provider [provider-override #f])
  (define config
    (if provider-override
        (hasheq 'model provider-override 'project-dir (current-directory))
        (hasheq 'project-dir (current-directory))))
  (define settings (load-settings (current-directory)))
  (build-provider config settings))

;; build-mock-provider-for-task : benchmark-task? -> provider?
(define (build-mock-provider-for-task task)
  (define mock-response
    (make-model-response (list (hasheq 'type
                                       "text"
                                       'text
                                       (format "Mock response for benchmark task: ~a"
                                               (benchmark-task-name task))))
                         (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
                         "mock-benchmark"
                         'stop))
  (make-mock-provider mock-response #:name "benchmark-mock"))

;; ============================================================
;; Trace analysis helpers
;; ============================================================

;; trace-tool-calls : path? -> (listof string?)
;; Extracts tool call names from a trace JSONL file.
;; q's trace format: {"phase": "tool.call.started", "data": {"name": "read", ...}}
(define (trace-tool-calls trace-path)
  (if (not (and trace-path (file-exists? trace-path)))
      '()
      (for*/list ([entry (in-list (jsonl-read-all-valid trace-path))]
                  #:when (hash? entry)
                  #:when (equal? (hash-ref entry 'phase #f) "tool.call.started"))
        (define data (hash-ref entry 'data (hasheq)))
        (hash-ref data 'name "unknown"))))

;; ============================================================
;; Core execution
;; ============================================================

;; execute-task/mock : benchmark-task? [path?] -> execution-result?
;; Execute with mock provider. For testing the benchmark infrastructure.
(define (execute-task/mock task [output-dir #f])
  (define provider (build-mock-provider-for-task task))
  (execute-task-internal task provider #f output-dir))

;; execute-task/live : benchmark-task? [string?] [path?] -> execution-result?
;; Execute with real LLM provider from config.
(define (execute-task/live task [provider-override #f] [output-dir #f])
  (define provider (build-live-provider provider-override))
  (execute-task-internal task provider provider-override output-dir))

;; execute-task-internal : benchmark-task? provider? (or/c string? #f) (or/c path? #f)
;;                       -> execution-result?
(define (execute-task-internal task provider provider-label output-dir)
  (define tmp-dir (task-setup task))
  (define trace-dir (or output-dir (build-path tmp-dir "traces")))
  (make-directory* trace-dir)

  (define start-ms (current-inexact-milliseconds))
  (define bus (make-event-bus))

  ;; Setup trace logger
  (define tlogger (make-trace-logger bus trace-dir #:enabled? #t))
  (start-trace-logger! tlogger)

  ;; Build extension registry
  (define ext-reg (make-extension-registry))

  ;; Try loading project extensions
  (define project-ext-dir (build-path tmp-dir ".q" "extensions"))
  (when (directory-exists? project-ext-dir)
    (load-extensions-from-dir! ext-reg project-ext-dir #:event-bus bus))

  ;; Build runtime with selective tools (exclude session-recall, skill-router in benchmark mode)
  (define benchmark-tools '("read" "write" "edit" "bash" "grep" "find" "ls" "spawn-subagent"))
  (define reg (make-tool-registry))
  (register-default-tools! reg #:only benchmark-tools)
  (define rt
    (sdk:make-runtime #:provider provider
                      #:session-dir (build-path tmp-dir "sessions")
                      #:tool-registry reg
                      #:event-bus bus
                      #:max-iterations (benchmark-task-max-iterations task)
                      #:extension-registry ext-reg))

  ;; Run the task
  (define-values (outcome iterations error-msg raw-result)
    (with-handlers ([exn:fail? (lambda (e) (values 'error 0 (exn-message e) #f))])
      (define rt2 (sdk:open-session rt))

      ;; Execute with timeout via channel
      (define time-limit (benchmark-task-time-limit-seconds task))
      (define ch (make-channel))
      (define exec-thread
        (thread (lambda ()
                  (channel-put ch
                               (with-handlers ([exn:fail? (lambda (e) (list 'error e))])
                                 (define-values (rt3 result)
                                   (parameterize ([current-directory tmp-dir])
                                     (sdk:run-prompt! rt2 (benchmark-task-prompt task))))
                                 (list 'ok rt3 result))))))
      (define exec-result (sync/timeout time-limit ch))

      (cond
        ;; Timeout — sync/timeout returned #f
        [(not exec-result)
         (kill-thread exec-thread)
         (values 'timeout 0 "Time limit exceeded" #f)]
        ;; Error from inside the thread
        [(and (list? exec-result) (eq? (car exec-result) 'error))
         (values 'error 0 (exn-message (cadr exec-result)) #f)]
        [else
         (match exec-result
           [(list 'ok rt3 result)
            (define sess-info (sdk:session-info rt3))
            (define iter-count
              (if sess-info
                  (hash-ref sess-info 'history-length 1)
                  1))
            (values 'completed iter-count #f result)]
           [_ (values 'error 0 "Unexpected channel result" #f)])])))

  ;; Stop trace logger
  (stop-trace-logger! tlogger)

  (define end-ms (current-inexact-milliseconds))
  (define duration-ms (inexact->exact (round (- end-ms start-ms))))

  ;; Find trace file
  (define trace-path
    (let ([trace-files (for/list ([f (in-directory trace-dir)]
                                  #:when (regexp-match? #rx"\\.jsonl$" (path->string f)))
                         f)])
      (if (null? trace-files)
          #f
          (car trace-files))))

  (execution-result (benchmark-task-name task)
                    trace-path
                    (build-path tmp-dir "sessions")
                    duration-ms
                    (if (and (eq? outcome 'timeout) trace-path (file-exists? trace-path))
                        (max iterations (trace-tool-call-count trace-path))
                        iterations)
                    outcome
                    error-msg
                    tmp-dir
                    raw-result))

;; ============================================================
;; v0.20.4 W4: GSD workflow execution + event filtering
;; ============================================================

;; trace-gsd-events : path? -> (listof hash?)
;; Extracts GSD-related events from trace JSONL.
(define (trace-gsd-events trace-path)
  (if (not (and trace-path (file-exists? trace-path)))
      '()
      (for/list ([entry (in-list (jsonl-read-all-valid trace-path))]
                 #:when (and (hash? entry)
                             (let ([ph (hash-ref entry 'phase #f)])
                               (and (string? ph)
                                    (or (string-prefix? ph "gsd.")
                                        (string-prefix? ph "agent.stall"))))))
        entry)))

;; execute-task/gsd-workflow : benchmark-task? [(or/c string? #f)] [(or/c path? #f)] -> execution-result?
;; Specialized execution for planning-workflow tasks.
;; Captures GSD events and includes snapshot in the result.
(define (execute-task/gsd-workflow task [provider-override #f] [output-dir #f])
  (define provider
    (if provider-override
        (build-live-provider provider-override)
        (build-mock-provider-for-task task)))
  (define result (execute-task-internal task provider provider-override output-dir))
  (define gsd-events (trace-gsd-events (execution-result-trace-path result)))
  (if (null? gsd-events)
      result
      (struct-copy execution-result
                   result
                   [raw-result
                    (hasheq 'gsd-events
                            gsd-events
                            'gsd-snapshot
                            (gsd-snapshot)
                            'original-result
                            (execution-result-raw-result result))])))

;; trace-tool-call-count : path? -> exact-nonnegative-integer?
;; Count tool.call.started events in trace JSONL as iteration proxy.
(define (trace-tool-call-count trace-path)
  (define entries
    (with-handlers ([exn:fail? (lambda (e) '())])
      (jsonl-read-all-valid trace-path)))
  (for/sum ([e (in-list entries)] #:when (and (hash? e)
                                              (equal? (hash-ref e 'phase #f) "tool.call.started")))
           1))
