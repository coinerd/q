#lang racket/base

;;; tools/scheduler-execution.rkt — Execution strategy/ordering extracted from scheduler.rkt
;;;
;;; Exports:
;;;   - tool-pre-hook-payload struct
;;;   - tool-post-hook-payload struct
;;;   - ipc-response->tool-result — IPC response translation
;;;   - max-parallel-tools — bounded thread pool parameter
;;;
;;; This module handles:
;;;   1. Single tool execution with pre/post hooks
;;;   2. Batch execution (serial or parallel with bounded thread pool)
;;;   3. IPC response translation for worker-based execution

(require racket/match
         racket/contract
         (only-in "tool.rkt"
                  tool-call?
                  tool-call-id
                  tool-call-name
                  tool-call-arguments
                  make-tool-call
                  make-error-result
                  make-success-result
                  validate-tool-result
                  exec-context?
                  exec-context-cancellation-token
                  exec-context-event-publisher
                  exec-context-permission-config
                  tool-execute)
         (only-in "tool-struct.rkt"
                  tool-execute
                  tool-dangerous?
                  tool-timeout-seconds
                  tool-externalizable?
                  tool-required-capability)
         (only-in "../sandbox/gateway-bridge.rkt"
                  current-execution-plane-enabled
                  ensure-worker!
                  shutdown-worker!
                  execute-tool-via-worker
                  ipc-response-status
                  ipc-response-content
                  ipc-response-details
                  ipc-response-error-message)
         (only-in "file-mutation-queue.rkt" with-file-mutation-queue)
         (only-in "permission-gate.rkt" permission-config? tool-needs-approval? request-approval)
         (only-in "../util/hook-types.rkt" hook-result? hook-result-action hook-result-payload)
         (only-in "../util/cancellation.rkt" cancellation-token-cancelled?)
         (only-in "scheduler-preflight.rkt"
                  preflight-entry
                  preflight-entry?
                  preflight-entry-status
                  preflight-entry-tool-call
                  preflight-entry-tool
                  preflight-entry-error-message))

(provide tool-pre-hook-payload
         tool-pre-hook-payload?
         tool-pre-hook-payload-tool-name
         tool-pre-hook-payload-args
         tool-pre-hook-payload-entry-id
         tool-post-hook-payload
         tool-post-hook-payload?
         tool-post-hook-payload-tool-name
         tool-post-hook-payload-result
         tool-post-hook-payload-entry-id
         tool-post-hook-payload-arguments
         ipc-response->tool-result
         run-execution
         (contract-out [max-parallel-tools (parameter/c exact-positive-integer?)]))

;; ============================================================
;; Typed hook payloads
;; ============================================================

;; v0.44.2 (R5): Typed hook payloads for scheduler observability
(struct tool-pre-hook-payload (tool-name args entry-id) #:transparent)
(struct tool-post-hook-payload (tool-name result entry-id arguments) #:transparent)

;; v0.21.5 (F6): Maximum parallel tool execution threads.
;; Default 8 — prevents unbounded thread spawning.
(define max-parallel-tools (make-parameter 8))

;; ============================================================
;; Execution-plane bridge: route dangerous tools through worker
;; ============================================================

;; Translate ipc-response to tool-result for the scheduler.
;; H4: The actual IPC request building is now in gateway-bridge.rkt
;; via execute-tool-via-worker, so this function only handles translation.
(define (ipc-response->tool-result resp)
  (define status (ipc-response-status resp))
  (define content (ipc-response-content resp))
  (define details (ipc-response-details resp))
  (define err-msg (ipc-response-error-message resp))
  (case status
    [(ok) (make-success-result (or content "ok") details)]
    [(timeout) (make-error-result (format "tool execution timed out: ~a" (or err-msg "")))]
    [(crashed) (make-error-result (format "worker crashed: ~a" (or err-msg "")))]
    ;; F-2 (v0.99.26): Tool ran but returned non-zero exit (e.g., bash syntax error).
    ;; Show stderr and exit code so the agent can diagnose the failure.
    [(error)
     (define stderr (and details (hash? details) (hash-ref details 'stderr #f)))
     (define exit-code (and details (hash? details) (hash-ref details 'exit-code #f)))
     (make-error-result
      (format "command failed (exit ~a): ~a" (or exit-code "?") (or stderr err-msg "unknown")))]
    [else (make-error-result (format "execution plane error: ~a" (or err-msg "unknown")))]))

;; ============================================================
;; Execute a single tool call (with exception handling)
;; Includes tool-call-pre and tool-result-post hooks (R2-7)
;; ============================================================

(define (execute-single tc t exec-ctx hook-dispatcher)
  ;; Dispatch 'tool-call-pre hook
  (define tc-id (tool-call-id tc))
  (define tc-name (tool-call-name tc))
  (define tc-args (tool-call-arguments tc))

  ;; FEAT-73: emit tool.execution.started lifecycle event
  ;; W-05: include per-tool start-ms for accurate duration tracking
  (define tool-start-ms (current-inexact-milliseconds))
  (define ev-pub (and exec-ctx (exec-context-event-publisher exec-ctx)))
  (when ev-pub
    (ev-pub "tool.execution.started"
            (hasheq 'tool-name tc-name 'tool-call-id tc-id 'start-ms tool-start-ms)))

  (define pre-payload (tool-pre-hook-payload tc-name tc-args tc-id))

  ;; Check if tool-call-pre hook blocks or amends
  (define pre-hook-result
    (if hook-dispatcher
        (with-handlers ([exn:fail? (lambda (e)
                                     (log-warning "tool-call-pre hook threw: ~a" (exn-message e))
                                     #f)])
          (hook-dispatcher 'tool-call-pre pre-payload))
        #f))

  (cond
    [(and (hook-result? pre-hook-result) (eq? (hook-result-action pre-hook-result) 'block))
     ;; Return early with blocked result
     (make-error-result (format "tool '~a' blocked by tool-call-pre hook" tc-name))]
    [else
     ;; Determine which tool call to execute (possibly amended args)
     ;; Validate that hook-amended args is a hash before use
     (define tc-to-execute
       (if (and (hook-result? pre-hook-result)
                (eq? (hook-result-action pre-hook-result) 'amend)
                (hash? (hook-result-payload pre-hook-result))
                (hash-has-key? (hook-result-payload pre-hook-result) 'args))
           (let ([amended-args (hash-ref (hook-result-payload pre-hook-result) 'args)])
             (if (hash? amended-args)
                 (make-tool-call tc-id tc-name amended-args)
                 tc))
           tc))

     ;; G3.4: Permission gate -- check if tool needs approval
     (define perm-cfg (exec-context-permission-config exec-ctx))
     (cond
       [(and (permission-config? perm-cfg)
             (tool-needs-approval? perm-cfg tc-name)
             (not (request-approval perm-cfg tc-name (tool-call-arguments tc-to-execute))))
        (make-error-result (format "tool '~a' blocked -- approval denied" tc-name))]
       [else
        ;; R-03/R-22: Use tool-dangerous? metadata instead of hardcoded list
        (define raw-args (tool-call-arguments tc-to-execute))
        ;; v0.70.7: Inject per-tool timeout into args if the tool defines one
        (define args
          (if (and (tool-timeout-seconds t) (not (hash-has-key? raw-args 'timeout)))
              (hash-set raw-args 'timeout (tool-timeout-seconds t))
              raw-args))
        (define exec-result
          (cond
            [(and (current-execution-plane-enabled) (tool-dangerous? t) (tool-externalizable? t))
             ;; H4: Route through gateway-bridge facade (consolidated IPC logic)
             (define resp (execute-tool-via-worker tc-name args (tool-required-capability t)))
             (ipc-response->tool-result resp)]
            [else
             ;; Existing in-process execution (unchanged)
             (with-handlers ([exn:fail? (lambda (e)
                                          (make-error-result
                                           (format "tool '~a' raised: ~a" tc-name (exn-message e))))])
               (define path-arg (and (tool-dangerous? t) (hash? args) (hash-ref args 'path #f)))
               (with-file-mutation-queue path-arg (lambda () ((tool-execute t) args exec-ctx))))]))

        ;; Dispatch 'tool-result-post hook
        (define post-payload (tool-post-hook-payload tc-name exec-result tc-id args))

        (define post-hook-result
          (if hook-dispatcher
              (with-handlers ([exn:fail? (lambda (e)
                                           (log-warning "tool-result-post hook threw: ~a"
                                                        (exn-message e))
                                           #f)])
                (hook-dispatcher 'tool-result-post post-payload))
              #f))

        (match post-hook-result
          [(? hook-result? (app hook-result-action 'block))
           ;; Treat block as error
           (make-error-result (format "tool '~a' result blocked by tool-result-post hook" tc-name))]
          [(? hook-result? (app hook-result-action 'amend) (app hook-result-payload (? hash?)))
           (define payload (hook-result-payload post-hook-result))
           (if (hash-has-key? payload 'result)
               (let ([amended-result (hash-ref payload 'result)])
                 (if (validate-tool-result amended-result) amended-result exec-result))
               exec-result)]
          ;; Return original result
          [_ exec-result])])]))

;; ============================================================
;; Execution stage
;; ============================================================

(define (run-execution preflight-entries exec-ctx parallel? hook-dispatcher)
  ;; Returns a list of tool-result in the same order as preflight-entries.
  ;; For 'blocked and 'error entries, produces error results directly.
  ;; For 'ready entries, executes the tool.

  (define cancellation-token (and exec-ctx (exec-context-cancellation-token exec-ctx)))
  (define (cancelled?)
    (and cancellation-token (cancellation-token-cancelled? cancellation-token)))
  (define (cancelled-result)
    (make-error-result "tool execution cancelled before start"))

  ;; Collect indices and ready entries for execution
  (define indexed-ready
    (for/list ([entry (in-list preflight-entries)]
               [idx (in-naturals)]
               #:when (eq? (preflight-entry-status entry) 'ready))
      (cons idx entry)))

  ;; Execute ready calls
  (define execution-results
    (if parallel?
        ;; Parallel execution using threads with bounded pool (F6)
        (let* ([sem (make-semaphore (max-parallel-tools))]
               [channels
                (for/list ([ie (in-list indexed-ready)])
                  (define ch (make-channel))
                  (define idx (car ie))
                  (define entry (cdr ie))
                  (define tc (preflight-entry-tool-call entry))
                  (define t (preflight-entry-tool entry))
                  (thread (lambda ()
                            (semaphore-wait sem)
                            (define result
                              (if (cancelled?)
                                  (cancelled-result)
                                  (with-handlers ([exn:fail? (lambda (e)
                                                               (make-error-result
                                                                (format "tool '~a' raised: ~a"
                                                                        (tool-call-name tc)
                                                                        (exn-message e))))])
                                    (execute-single tc t exec-ctx hook-dispatcher))))
                            (semaphore-post sem)
                            (channel-put ch (cons idx result))))
                  ch)])
          (for/list ([ch (in-list channels)])
            (channel-get ch)))
        ;; Serial execution
        (for/list ([ie (in-list indexed-ready)])
          (define idx (car ie))
          (define entry (cdr ie))
          (define tc (preflight-entry-tool-call entry))
          (define t (preflight-entry-tool entry))
          (cons idx
                (if (cancelled?)
                    (cancelled-result)
                    (execute-single tc t exec-ctx hook-dispatcher))))))

  ;; Build a map from index -> result
  (define results-by-idx
    (for/hasheq ([pair (in-list execution-results)])
      (values (car pair) (cdr pair))))

  ;; Build final ordered list
  (for/list ([entry (in-list preflight-entries)]
             [idx (in-naturals)])
    (define status (preflight-entry-status entry))
    (case status
      [(ready)
       (hash-ref results-by-idx
                 idx
                 (lambda () (make-error-result "internal: missing execution result")))]
      [(blocked) (make-error-result (preflight-entry-error-message entry))]
      [(error) (make-error-result (preflight-entry-error-message entry))])))
