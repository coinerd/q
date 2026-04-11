#lang racket/base

;;; tools/scheduler.rkt — preflight, validation, scheduling, ordered result commit.
;;;
;;; Exports:
;;;   - run-tool-batch       — main entry point
;;;   - scheduler-result     — result struct
;;;
;;; Behavior (from ARCHITECTURE.md section 6.4):
;;;   1. preserve source order
;;;   2. run preflight hooks serially
;;;   3. revalidate arguments after hook mutation
;;;   4. execute approved calls, optionally in parallel
;;;   5. emit final results in source order

(require (only-in "tool.rkt"
                  tool? tool-name tool-schema tool-execute
                  make-tool make-tool-registry make-exec-context
                  register-tool! lookup-tool validate-tool-args
                  tool-result? tool-result-content
                  tool-result-details tool-result-is-error?
                  make-error-result make-success-result
                  exec-context? exec-context-event-publisher
                  tool-call tool-call? make-tool-call
                  tool-call-id tool-call-name tool-call-arguments)
         (only-in "../extensions/hooks.rkt"
                  hook-result? hook-result-action hook-result-payload)
         (only-in "../runtime/safe-mode.rkt"
                  safe-mode? allowed-tool?))

(provide
 ;; ── Result struct ──
 (struct-out scheduler-result)

 ;; ── Main entry point ──
 run-tool-batch)

;; ============================================================
;; Scheduler result struct
;; ============================================================

(struct scheduler-result (results metadata) #:transparent)

;; ============================================================
;; Internal: preflight outcome for a single tool call
;; ============================================================

;; An entry in the preflight result list.
;; One of three states:
;;   - 'ready  + tool-call + tool struct   → proceed to execution
;;   - 'blocked + tool-call + #f           → hook blocked it
;;   - 'error   + tool-call + error-msg    → post-hook validation failed / not found

;; Represented as a hash for simplicity
;;   hasheq 'status 'ready|'blocked|'error
;;          'tool-call <tool-call>
;;          'tool <tool|#f>        (only for 'ready)
;;          'error-message <str>   (only for 'blocked/'error)

;; ============================================================
;; Preflight stage (serial)
;; ============================================================

(define (run-preflight tool-calls registry hook-dispatcher)
  ;; Returns a list of preflight entries, one per tool-call, in order.
  (for/list ([tc (in-list tool-calls)])
    (define tc-after-hook
      (if hook-dispatcher
          (with-handlers ([exn:fail?
                           (lambda (e)
                             ;; Hook itself threw → treat as block with error
                             (define msg (format "hook error: ~a" (exn-message e)))
                             (hasheq 'status 'error
                                     'tool-call tc
                                     'error-message msg))])
            (define result (hook-dispatcher 'tool-call tc))
            (cond
              [(hook-result? result)
               (case (hook-result-action result)
                 [(block) 'blocked]
                 [(amend) (hook-result-payload result)]
                 [else tc])]
              [else result]))
          tc))
    (cond
      ;; Hook returned 'blocked
      [(eq? tc-after-hook 'blocked)
       (hasheq 'status 'blocked
               'tool-call tc
               'error-message (format "tool call '~a' blocked by hook"
                                      (tool-call-name tc)))]
      ;; Hook threw an exception (returned as list)
      [(and (hash? tc-after-hook)
            (eq? (hash-ref tc-after-hook 'status #f) 'error))
       tc-after-hook]
      ;; Hook returned a (possibly modified) tool-call
      [(tool-call? tc-after-hook)
       (define t (lookup-tool registry (tool-call-name tc-after-hook)))
       (cond
         [(not t)
          ;; Tool not found in registry
          (hasheq 'status 'error
                  'tool-call tc-after-hook
                  'error-message (format "unknown tool: '~a'"
                                         (tool-call-name tc-after-hook)))]
         [else
          ;; Check safe-mode tool restrictions (SEC-01)
          (cond
            [(and (safe-mode?) (not (allowed-tool? (tool-call-name tc-after-hook))))
             (hasheq 'status 'blocked
                     'tool-call tc-after-hook
                     'error-message (format "tool '~a' blocked by safe-mode"
                                            (tool-call-name tc-after-hook)))]
            [else
             ;; Revalidate arguments after potential hook mutation
             (define validated?
               (with-handlers ([exn:fail? (lambda (e) #f)])
                 (validate-tool-args t (tool-call-arguments tc-after-hook))
                 #t))
             (if validated?
                 (hasheq 'status 'ready
                         'tool-call tc-after-hook
                         'tool t)
                 (hasheq 'status 'error
                         'tool-call tc-after-hook
                         'error-message
                         (format "post-hook validation failed for tool '~a'"
                                 (tool-call-name tc-after-hook))))])])]
      [else
       ;; Unexpected hook return value — treat as error
       (hasheq 'status 'error
               'tool-call tc
               'error-message (format "unexpected hook return: ~v" tc-after-hook))])))

;; ============================================================
;; Execute a single tool call (with exception handling)
;; Includes tool-call-pre and tool-result-post hooks (R2-7)
;; ============================================================

(define (execute-single tc t exec-ctx hook-dispatcher)
  ;; Dispatch 'tool-call-pre hook
  (define tc-id (tool-call-id tc))
  (define tc-name (tool-call-name tc))
  (define tc-args (tool-call-arguments tc))

  (define pre-payload
    (hasheq 'tool-name tc-name
            'args tc-args
            'entry-id tc-id))

  ;; Check if tool-call-pre hook blocks or amends
  (define pre-hook-result
    (if hook-dispatcher
        (hook-dispatcher 'tool-call-pre pre-payload)
        #f))

  (cond
    [(and (hook-result? pre-hook-result)
          (eq? (hook-result-action pre-hook-result) 'block))
     ;; Return early with blocked result
     (make-error-result (format "tool '~a' blocked by tool-call-pre hook" tc-name))]
    [else
     ;; Determine which tool call to execute (possibly amended args)
     (define tc-to-execute
       (if (and (hook-result? pre-hook-result)
                (eq? (hook-result-action pre-hook-result) 'amend)
                (hash? (hook-result-payload pre-hook-result))
                (hash-has-key? (hook-result-payload pre-hook-result) 'args))
           (make-tool-call tc-id tc-name (hash-ref (hook-result-payload pre-hook-result) 'args))
           tc))

     ;; Execute the tool
     (define exec-result
       (with-handlers ([exn:fail?
                        (lambda (e)
                          (make-error-result
                           (format "tool '~a' raised: ~a"
                                   tc-name
                                   (exn-message e))))])
         ((tool-execute t) (tool-call-arguments tc-to-execute) exec-ctx)))

     ;; Dispatch 'tool-result-post hook
     (define post-payload
       (hasheq 'tool-name tc-name
               'result exec-result
               'entry-id tc-id))

     (define post-hook-result
       (if hook-dispatcher
           (hook-dispatcher 'tool-result-post post-payload)
           #f))

     (cond
       [(and (hook-result? post-hook-result)
             (eq? (hook-result-action post-hook-result) 'block))
        ;; Treat block as error
        (make-error-result (format "tool '~a' result blocked by tool-result-post hook" tc-name))]
       [(and (hook-result? post-hook-result)
             (eq? (hook-result-action post-hook-result) 'amend)
             (hash? (hook-result-payload post-hook-result))
             (hash-has-key? (hook-result-payload post-hook-result) 'result))
        ;; Use amended result
        (hash-ref (hook-result-payload post-hook-result) 'result)]
       [else
        ;; Return original result
        exec-result])])

)

;; ============================================================
;; Execution stage
;; ============================================================

(define (run-execution preflight-entries exec-ctx parallel? hook-dispatcher)
  ;; Returns a list of tool-result in the same order as preflight-entries.
  ;; For 'blocked and 'error entries, produces error results directly.
  ;; For 'ready entries, executes the tool.

  ;; Collect indices and ready entries for execution
  (define indexed-ready
    (for/list ([entry (in-list preflight-entries)]
               [idx (in-naturals)]
               #:when (eq? (hash-ref entry 'status) 'ready))
      (cons idx entry)))

  ;; Execute ready calls
  (define execution-results
    (if parallel?
        ;; Parallel execution using threads
        (let ([channels (for/list ([ie (in-list indexed-ready)])
                          (define ch (make-channel))
                          (define idx (car ie))
                          (define entry (cdr ie))
                          (define tc (hash-ref entry 'tool-call))
                          (define t (hash-ref entry 'tool))
                          (thread
                           (lambda ()
                             (channel-put ch (cons idx (execute-single tc t exec-ctx hook-dispatcher)))))
                          ch)])
          (for/list ([ch (in-list channels)])
            (channel-get ch)))
        ;; Serial execution
        (for/list ([ie (in-list indexed-ready)])
          (define idx (car ie))
          (define entry (cdr ie))
          (define tc (hash-ref entry 'tool-call))
          (define t (hash-ref entry 'tool))
          (cons idx (execute-single tc t exec-ctx hook-dispatcher)))))

  ;; Build a map from index → result
  (define results-by-idx
    (for/hasheq ([pair (in-list execution-results)])
      (values (car pair) (cdr pair))))

  ;; Build final ordered list
  (for/list ([entry (in-list preflight-entries)]
             [idx (in-naturals)])
    (define status (hash-ref entry 'status))
    (case status
      [(ready)
       (hash-ref results-by-idx idx
                  (lambda ()
                    (make-error-result "internal: missing execution result")))]
      [(blocked)
       (make-error-result (hash-ref entry 'error-message "blocked"))]
      [(error)
       (make-error-result (hash-ref entry 'error-message "error"))])))

;; ============================================================
;; Compute metadata
;; ============================================================

(define (compute-metadata results preflight-entries)
  (define total (length results))
  (define blocked
    (for/sum ([entry (in-list preflight-entries)])
      (if (eq? (hash-ref entry 'status) 'blocked) 1 0)))
  ;; Count errors that are NOT from blocked entries
  (define blocked-indices
    (for/list ([entry (in-list preflight-entries)]
               [idx (in-naturals)]
               #:when (eq? (hash-ref entry 'status) 'blocked))
      idx))
  (define errors
    (for/sum ([r (in-list results)]
              [idx (in-naturals)]
              #:when (and (tool-result-is-error? r)
                          (not (member idx blocked-indices))))
      1))
  (define executed (- total blocked errors))
  (hasheq 'total total
          'executed executed
          'blocked blocked
          'errors errors))

;; ============================================================
;; Main entry point
;; ============================================================

(define (run-tool-batch tool-calls registry
                        #:hook-dispatcher [hook-dispatcher #f]
                        #:exec-context [exec-ctx (make-exec-context)]
                        #:parallel? [parallel? #f])
  ;; Resolve event publisher from exec-context (may be #f)
  (define ev-pub (and exec-ctx (exec-context-event-publisher exec-ctx)))

  ;; Emit batch preflight started event
  (when ev-pub
    (ev-pub "tool.batch.preflight.started"
            (hasheq 'toolCount (length tool-calls)
                    'toolNames (map tool-call-name tool-calls))))

  ;; Step 1: Preflight (serial)
  (define preflight-entries (run-preflight tool-calls registry hook-dispatcher))

  ;; Step 2: Execution (serial or parallel)
  (define results (run-execution preflight-entries exec-ctx parallel? hook-dispatcher))

  ;; Step 3: Metadata
  (define metadata (compute-metadata results preflight-entries))

  ;; Emit batch completed event
  (when ev-pub
    (ev-pub "tool.batch.completed" metadata))

  ;; Step 4: Return ordered result
  (scheduler-result results metadata))
