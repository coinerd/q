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
                  make-error-result
                  make-success-result
                  validate-tool-args
                  validate-tool-result
                  exec-context?
                  exec-context-working-directory
                  exec-context-cancellation-token
                  exec-context-event-publisher
                  exec-context-permission-config
                  exec-context-capabilities
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
         (only-in "permission-gate.rkt"
                  permission-config?
                  tool-needs-approval?
                  request-approval
                  tool-approval-commitment-digest)
         (only-in "tool-classification.rkt" tool-name-tool-owned-approval?)
         (only-in "../runtime/approval/broker.rkt" approval-grant? call-with-approval-grant)
         (only-in "../util/hook-types.rkt" hook-result? hook-result-action hook-result-payload)
         (only-in "../util/cancellation.rkt" cancellation-token-cancelled?)
         (only-in "../util/capability.rkt" capability-authorized?)
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

;; Deep-copy invocation data into immutable containers. Approval callbacks and
;; execution handlers share this exact snapshot, preventing post-approval
;; mutation through hook-owned nested values.
(define (immutable-invocation-copy value)
  (cond
    [(hash? value)
     (define copied-pairs
       (for/list ([(key item) (in-hash value)])
         (cons (immutable-invocation-copy key) (immutable-invocation-copy item))))
     (cond
       [(hash-eq? value)
        (for/hasheq ([item (in-list copied-pairs)])
          (values (car item) (cdr item)))]
       [(hash-eqv? value)
        (for/hasheqv ([item (in-list copied-pairs)])
          (values (car item) (cdr item)))]
       [else
        (for/hash ([item (in-list copied-pairs)])
          (values (car item) (cdr item)))])]
    [(list? value) (map immutable-invocation-copy value)]
    [(pair? value)
     (cons (immutable-invocation-copy (car value)) (immutable-invocation-copy (cdr value)))]
    [(vector? value)
     (vector->immutable-vector (for/vector ([item (in-vector value)])
                                 (immutable-invocation-copy item)))]
    [(string? value) (string->immutable-string (string-copy value))]
    [(bytes? value) (bytes->immutable-bytes (bytes-copy value))]
    [(box? value) (box-immutable (immutable-invocation-copy (unbox value)))]
    [else value]))

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
    ;; Defense in depth: plans may be retained or constructed independently,
    ;; so execution rechecks the authority snapshot supplied for this run.
    [(not (capability-authorized? (tool-required-capability t) (exec-context-capabilities exec-ctx)))
     (make-error-result (format "tool '~a' blocked — required capability '~a' is not authorized"
                                tc-name
                                (tool-required-capability t)))]
    [(and (hook-result? pre-hook-result) (eq? (hook-result-action pre-hook-result) 'block))
     ;; Return early with blocked result
     (make-error-result (format "tool '~a' blocked by tool-call-pre hook" tc-name))]
    [else
     ;; A declared amendment must contain hash arguments. Malformed amendments
     ;; fail closed rather than silently executing the original invocation.
     (define amendment?
       (and (hook-result? pre-hook-result) (eq? (hook-result-action pre-hook-result) 'amend)))
     (define amendment-payload (and amendment? (hook-result-payload pre-hook-result)))
     (define amended-args
       (and (hash? amendment-payload)
            (hash-has-key? amendment-payload 'args)
            (hash-ref amendment-payload 'args)))
     (define raw-args
       (cond
         [(not amendment?) tc-args]
         [(hash? amended-args) amended-args]
         [else #f]))

     (cond
       [(not raw-args)
        (make-error-result (format "tool '~a' blocked — invalid tool-call-pre amendment" tc-name))]
       [else
        ;; Construct the complete invocation before approval: hook amendments,
        ;; scheduler timeout, and scheduler CWD are committed together.
        (define final-args
          (immutable-invocation-copy
           (let* ([args-copy (immutable-invocation-copy raw-args)]
                  [with-timeout (if (and (tool-timeout-seconds t)
                                         (not (hash-has-key? args-copy 'timeout)))
                                    (hash-set args-copy 'timeout (tool-timeout-seconds t))
                                    args-copy)]
                  ;; Inject working-directory for both worker and in-process paths
                  ;; so approval describes the exact execution invocation.
                  [wd (and exec-ctx (exec-context-working-directory exec-ctx))]
                  [wd-string (and wd
                                  (if (path? wd)
                                      (path->string wd)
                                      wd))])
             (if wd-string
                 (hash-set with-timeout 'working-directory wd-string)
                 with-timeout))))

        ;; Validate the final post-hook, post-injection invocation. Invalid
        ;; amendments never reach either approval or execution.
        (define validation-error
          (with-handlers ([exn:fail? (lambda (e) e)])
            (validate-tool-args t final-args)
            #f))
        (cond
          [validation-error (make-error-result (exn-message validation-error))]
          [else
           ;; G3.4: Permission gate — check if tool needs approval.
           ;; v0.99.66 (W1, finding #1 CRITICAL): fail-closed. If perm-cfg
           ;; is missing or not a permission-config, execution is refused.
           (define perm-cfg (exec-context-permission-config exec-ctx))
           (define (execute-committed-invocation)
             ;; R-03/R-22: Use tool-dangerous? metadata instead of hardcoded list.
             ;; final-args is the exact immutable object approved above.
             (define exec-result
               (cond
                 [(and (current-execution-plane-enabled) (tool-dangerous? t) (tool-externalizable? t))
                  ;; H4: Route through gateway-bridge facade (consolidated IPC logic)
                  (define resp
                    (execute-tool-via-worker tc-name final-args (tool-required-capability t)))
                  (ipc-response->tool-result resp)]
                 [else
                  ;; Existing in-process execution (unchanged)
                  (with-handlers ([exn:fail? (lambda (e)
                                               (make-error-result (format "tool '~a' raised: ~a"
                                                                          tc-name
                                                                          (exn-message e))))])
                    (define path-arg (and (tool-dangerous? t) (hash-ref final-args 'path #f)))
                    (with-file-mutation-queue path-arg
                                              (lambda () ((tool-execute t) final-args exec-ctx))))]))

             ;; Dispatch 'tool-result-post hook with the committed invocation.
             (define post-payload (tool-post-hook-payload tc-name exec-result tc-id final-args))
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
                (make-error-result (format "tool '~a' result blocked by tool-result-post hook"
                                           tc-name))]
               [(? hook-result? (app hook-result-action 'amend) (app hook-result-payload (? hash?)))
                (define payload (hook-result-payload post-hook-result))
                (if (hash-has-key? payload 'result)
                    (let ([amended-result (hash-ref payload 'result)])
                      (if (validate-tool-result amended-result) amended-result exec-result))
                    exec-result)]
               [_ exec-result]))

           (cond
             [(not (permission-config? perm-cfg))
              (make-error-result
               (format "tool '~a' blocked — permission gate misconfigured (no config)" tc-name))]
             ;; Tool-owned approval (spawn) executes without the generic
             ;; callback; the handler's internal broker gate remains authoritative.
             [(tool-name-tool-owned-approval? tc-name) (execute-committed-invocation)]
             [(not (tool-needs-approval? perm-cfg tc-name)) (execute-committed-invocation)]
             [else
              (define commitment-digest (tool-approval-commitment-digest tc-name final-args))
              (define approval
                (with-handlers ([exn:fail? (lambda (_) #f)])
                  (request-approval perm-cfg tc-name final-args ev-pub)))
              (cond
                [(eq? approval #t) (execute-committed-invocation)]
                [(approval-grant? approval)
                 ;; Consume the one-use grant immediately around execution of
                 ;; the same final-args object committed by the callback.
                 (or
                  (call-with-approval-grant approval commitment-digest execute-committed-invocation)
                  (make-error-result (format "tool '~a' blocked — approval grant invalid" tc-name)))]
                [else
                 (make-error-result (format "tool '~a' blocked — approval denied" tc-name))])])])])]))

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
