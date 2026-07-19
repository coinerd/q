#lang racket/base

;; @speed fast
;; @suite security

(require rackunit
         racket/list
         racket/string
         "../tools/builtins/spawn-subagent.rkt"
         (only-in "../tools/builtins/spawn-subagent-helpers.rkt" sha256-digest)
         (only-in "../runtime/approval/broker.rkt"
                  make-approval-channel
                  set-approval-channel!
                  clear-approval-channel!
                  pending-approval-count
                  approval-request-view
                  approval-decide!)
         "../tools/tool.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt"
         "../util/ids.rkt"
         (only-in "../util/capability.rkt" current-session-capabilities)
         (only-in "../util/safe-mode/safe-mode-state.rkt" current-safe-mode))

(define (make-test-context publisher)
  ;; Deliberately omit a provider: these tests must never use a real provider.
  (make-exec-context #:working-directory (current-directory)
                     #:event-publisher publisher
                     #:runtime-settings (hasheq 'model "mock-model")
                     #:call-id (generate-id)
                     #:session-metadata (hasheq 'session-id "approval-test" 'role "parent")))

(define (make-counting-context publisher sends)
  (define provider
    (make-provider
     (lambda () "counting-mock")
     (lambda () (hasheq 'streaming #f))
     (lambda (_request)
       (set-box! sends (add1 (unbox sends)))
       (make-model-response (list (hasheq 'type "text" 'text "done"))
                            (hasheq 'prompt-tokens 1 'completion-tokens 1 'total-tokens 2)
                            "counting-mock"
                            'stop))
     (lambda (_request) (error 'counting-mock "streaming not expected"))))
  (make-exec-context #:working-directory (current-directory)
                     #:event-publisher publisher
                     #:runtime-settings (hasheq 'provider provider 'model "counting-mock")
                     #:call-id (generate-id)
                     #:session-metadata (hasheq 'session-id "approval-test" 'role "parent")))

(define (result-text result)
  (string-join (for/list ([part (in-list (tool-result-content result))]
                          #:when (hash? part))
                 (hash-ref part 'text ""))
               ""))

(define (approval-events events)
  (filter (lambda (event) (string-prefix? (car event) "mas.spawn-approval")) events))

(define (terminal-events events)
  (filter (lambda (event) (string=? (car event) "subagent.terminal")) events))

(define (run/capture args decision)
  (define events '())
  (define (publisher type payload)
    (define captured-payload
      (if (string=? type "mas.spawn-approval-requested")
          (let ([view (approval-request-view (hash-ref payload 'request-id)
                                             (hash-ref payload 'commitment-digest))])
            (if view
                (for/fold ([merged payload]) ([(key value) (in-hash view)])
                  (hash-set merged key value))
                payload))
          payload))
    (set! events (append events (list (cons type captured-payload))))
    (when (string=? type "mas.spawn-approval-requested")
      (cond
        [(boolean? decision)
         (check-true (approval-decide! (hash-ref payload 'request-id)
                                       (hash-ref payload 'commitment-digest)
                                       decision))]
        [(eq? decision 'cancelled) (clear-approval-channel!)])))
  (define (run)
    (parameterize ([current-spawn-timestamps (box '())])
      (tool-spawn-subagents args (make-test-context publisher))))
  (cond
    [(eq? decision 'no-channel)
     (clear-approval-channel!)
     (values (run) events)]
    [else
     (dynamic-wind (lambda ()
                     (set-approval-channel!
                      (make-approval-channel #:timeout-ms (if (eq? decision 'timeout) 20 1000))))
                   (lambda () (values (run) events))
                   clear-approval-channel!)]))

(test-case "explicit invalid capability rejects the whole batch before approval"
  (define-values (result events)
    (run/capture (hasheq 'jobs
                         (list (hasheq 'task "valid" 'capabilities '(read-only))
                               (hasheq 'task "invalid" 'capabilities '(shell-exec not-a-capability))))
                 #t))
  (check-true (tool-result-is-error? result))
  (check-true (string-contains? (result-text result) "invalid capability"))
  (check-equal? events '()))

(test-case "omitted capabilities deny batch headlessly with bounded defaults"
  (define-values (result events)
    (run/capture (hasheq 'jobs (list (hasheq 'task "ordinary"))) 'no-channel))
  (check-true (tool-result-is-error? result))
  (check-true (string-contains? (result-text result) "approval denied"))
  (check-equal? (length events) 1)
  (define planned-job (car (hash-ref (cdar events) 'jobs)))
  (check-equal? (hash-ref planned-job 'effective-capabilities) '(read-only file-write shell-exec))
  (check-equal? (hash-ref planned-job 'model-preview) "mock-model"))

(test-case "explicit empty authority stays empty and needs no approval"
  (define-values (result events)
    (run/capture (hasheq 'jobs (list (hasheq 'task "no tools" 'capabilities '()))) #f))
  (check-false (tool-result-is-error? result) (result-text result))
  (check-equal? (approval-events events) '())
  (check-equal? (length (terminal-events events)) 1))

(test-case "delegated any wildcard is rejected before approval"
  (define-values (result events)
    (run/capture (hasheq 'jobs (list (hasheq 'task "wildcard" 'capabilities '(any)))) #t))
  (check-true (tool-result-is-error? result))
  (check-true (string-contains? (result-text result) "invalid capability"))
  (check-equal? events '()))

(test-case "explicit delegation cannot exceed parent session capabilities"
  (define-values (result events)
    (parameterize ([current-session-capabilities '(read-only)])
      (run/capture (hasheq 'jobs
                           (list (hasheq 'task "escalate" 'capabilities '(read-only shell-exec))))
                   #t)))
  (check-true (tool-result-is-error? result))
  (check-true (string-contains? (result-text result) "exceed parent"))
  (check-equal? events '()))

(test-case "safe mode removes blocked tools from the approved execution snapshot"
  (define-values (result events)
    (parameterize ([current-safe-mode #t])
      (run/capture
       (hasheq 'jobs (list (hasheq 'task "bounded" 'capabilities '(read-only file-write shell-exec))))
       #f)))
  (check-true (tool-result-is-error? result))
  (define planned-job (car (hash-ref (cdar events) 'jobs)))
  (define tools (hash-ref planned-job 'effective-tools))
  (check-false (member "bash" tools))
  (check-false (member "write" tools))
  (check-false (member "edit" tools)))

(test-case "explicit safe capabilities do not request approval"
  (define-values (result events)
    (run/capture (hasheq 'jobs (list (hasheq 'task "inspect" 'capabilities '(read-only)))) #f))
  (check-false (tool-result-is-error? result))
  (check-equal? (approval-events events) '())
  (check-equal? (length (terminal-events events)) 1))

(test-case "mixed dangerous batch requests exactly one all-or-nothing approval"
  (define args
    (hasheq 'batchId
            "batch-explicit"
            'jobs
            (list (hasheq 'jobId "safe" 'task "inspect" 'capabilities '(read-only))
                  (hasheq 'jobId "danger-1" 'task "danger" 'capabilities '(shell-exec))
                  (hasheq 'jobId "danger-2" 'task "commit" 'capabilities '(read-only git-write)))))
  (define-values (result events) (run/capture args #f))
  (check-true (tool-result-is-error? result))
  (check-true (string-contains? (result-text result) "approval denied"))
  (check-equal? (length events) 2)
  (define payload (cdar events))
  (check-equal? (caar events) "mas.spawn-approval-requested")
  (check-equal? (hash-ref payload 'batch-id) "batch-explicit")
  (check-equal? (length (hash-ref payload 'dangerous-jobs)) 2)
  (check-true (non-empty-string? (hash-ref payload 'commitment-digest)))
  (define dangerous (car (hash-ref payload 'dangerous-jobs)))
  (check-equal? (hash-ref dangerous 'task-digest) (sha256-digest "danger"))
  (check-false (hash-has-key? dangerous 'task))
  (check-equal? (hash-ref dangerous 'task-preview) "danger")
  (check-true (string-contains? (hash-ref payload 'task-preview) "danger"))
  (define planned-jobs (hash-ref payload 'jobs))
  (for ([job (in-list planned-jobs)])
    (check-true (non-empty-string? (hash-ref job 'job-id)))
    (check-true (non-empty-string? (hash-ref job 'tool-call-id)))
    (check-true (non-empty-string? (hash-ref job 'child-id)))
    (check-true (non-empty-string? (hash-ref job 'session-id)))))

(test-case "approval and terminal events retain no raw secret task text"
  (dynamic-wind
   (lambda () (set-approval-channel! (make-approval-channel #:timeout-ms 1000)))
   (lambda ()
     (define payloads '())
     (define publisher
       (lambda (type payload)
         (cond
           [(string=? type "mas.spawn-approval-requested")
            (define request-id (hash-ref payload 'request-id))
            (define digest (hash-ref payload 'commitment-digest))
            (define view (approval-request-view request-id digest))
            (define captured
              (for/fold ([merged payload]) ([(key value) (in-hash view)])
                (hash-set merged key value)))
            (set! payloads (append payloads (list (cons type captured))))
            (check-true (approval-decide! request-id digest #t))]
           [else (set! payloads (append payloads (list (cons type payload))))])))
     (define secret "api_key=super-secret-value-123456")
     (define result
       (tool-spawn-subagents (hasheq 'jobs
                                     (list (hasheq 'jobId
                                                   "secret-job"
                                                   'task
                                                   secret
                                                   'role
                                                   "token=role-secret-value-123456"
                                                   'model
                                                   "credential=model-secret-value-123456"
                                                   'capabilities
                                                   '(shell-exec))))
                             (make-test-context publisher)))
     (check-false (tool-result-is-error? result))
     (check-equal? (map car payloads)
                   '("mas.spawn-approval-requested" "mas.spawn-approval-terminal"
                                                    "subagent.terminal"))
     (for* ([entry (in-list payloads)]
            [secret-fragment (in-list '("super-secret-value-123456" "role-secret-value-123456"
                                                                    "model-secret-value-123456"))])
       (check-false (string-contains? (format "~s" (cdr entry)) secret-fragment)))
     (define request (cdar payloads))
     (for* ([job (in-list (append (hash-ref request 'jobs) (hash-ref request 'dangerous-jobs)))]
            [raw-key (in-list '(task role model))])
       (check-false (hash-has-key? job raw-key))))
   clear-approval-channel!))

(test-case "approval previews strip terminal control characters"
  (define hostile-task (string-append "first\n" (string (integer->char 27)) "[2Jsecond\rthird"))
  (define-values (result events)
    (run/capture (hasheq 'jobs
                         (list (hasheq 'jobId
                                       "terminal-safe-id"
                                       'task
                                       hostile-task
                                       'role
                                       (string-append "role" (string (integer->char 7)) "bell")
                                       'capabilities
                                       '(shell-exec))))
                 #f))
  (check-true (tool-result-is-error? result))
  (define payload (cdar events))
  (define planned-job (car (hash-ref payload 'jobs)))
  (for ([payload-text (in-list (list (hash-ref payload 'task-preview)
                                     (hash-ref planned-job 'task-preview)
                                     (hash-ref planned-job 'role-preview)))])
    (check-false (for/or ([char (in-string payload-text)])
                   (define code (char->integer char))
                   (or (< code 32) (and (>= code 127) (<= code 159)))))))

(test-case "terminal teardown revokes batch grant before rate or child effects"
  (define sends (box 0))
  (define timestamps (box '()))
  (define events '())
  (dynamic-wind (lambda () (set-approval-channel! (make-approval-channel #:timeout-ms 1000)))
                (lambda ()
                  (define ctx
                    (make-counting-context
                     (lambda (type payload)
                       (set! events (cons type events))
                       (cond
                         [(string=? type "mas.spawn-approval-requested")
                          (check-true (approval-decide! (hash-ref payload 'request-id)
                                                        (hash-ref payload 'commitment-digest)
                                                        #t))]
                         [(string=? type "mas.spawn-approval-terminal") (clear-approval-channel!)]))
                     sends))
                  (parameterize ([current-spawn-timestamps timestamps])
                    (define result
                      (tool-spawn-subagents
                       (hasheq 'jobs
                               (list (hasheq 'task "revoked batch" 'capabilities '(shell-exec))))
                       ctx))
                    (check-true (tool-result-is-error? result))
                    (check-equal? (hash-ref (tool-result-details result) 'terminal-status) "denied")
                    (check-equal? (unbox sends) 0)
                    (check-equal? (unbox timestamps) '())
                    (check-false (member "subagent.terminal" events))))
                clear-approval-channel!))

(test-case "dangerous denial reaches zero provider sends"
  (define sends (box 0))
  (define events '())
  (define ctx
    (make-counting-context (lambda (type payload)
                             (set! events (append events (list (cons type payload)))))
                           sends))
  (define timestamps (box '()))
  (clear-approval-channel!)
  (define result
    (parameterize ([current-spawn-timestamps timestamps])
      (tool-spawn-subagents (hasheq 'jobs
                                    (list (hasheq 'task "must not run" 'capabilities '(shell-exec))))
                            ctx)))
  (check-true (tool-result-is-error? result))
  (check-equal? (unbox sends) 0)
  (check-equal? (unbox timestamps) '())
  (check-equal? (map car events) '("mas.spawn-approval-terminal")))

(test-case "denial timeout and cancellation all start zero jobs"
  (for ([decision (in-list (list #f 'timeout 'cancelled))])
    (define-values (result events)
      (run/capture
       (hasheq 'jobs (list (hasheq 'jobId "danger" 'task "do not run" 'capabilities '(shell-exec))))
       decision))
    (check-true (tool-result-is-error? result))
    (check-equal? (length events) 2)))

(test-case "correlated timeout denies the whole batch and cleans the registry"
  (dynamic-wind
   (lambda () (set-approval-channel! (make-approval-channel #:timeout-ms 20)))
   (lambda ()
     (define events '())
     (define sends (box 0))
     (define ctx
       (make-counting-context (lambda (type payload) (set! events (append events (list type))))
                              sends))
     (define result
       (tool-spawn-subagents (hasheq 'jobs
                                     (list (hasheq 'task "must not run" 'capabilities '(shell-exec))))
                             ctx))
     (check-true (tool-result-is-error? result))
     (check-equal? events '("mas.spawn-approval-requested" "mas.spawn-approval-terminal"))
     (check-equal? (unbox sends) 0)
     (check-equal? (pending-approval-count) 0))
   clear-approval-channel!))

(test-case "interactive teardown cancels a blocked batch before child work"
  (dynamic-wind
   (lambda () (set-approval-channel! (make-approval-channel #:timeout-ms 5000)))
   (lambda ()
     (define requested (make-channel))
     (define result-ch (make-channel))
     (define sends (box 0))
     (define ctx
       (make-counting-context (lambda (type payload)
                                (when (string=? type "mas.spawn-approval-requested")
                                  (channel-put requested payload)))
                              sends))
     (thread (lambda ()
               (channel-put result-ch
                            (tool-spawn-subagents
                             (hasheq 'jobs
                                     (list (hasheq 'task "must not run" 'capabilities '(shell-exec))))
                             ctx))))
     (check-true (hash? (sync/timeout 1 requested)))
     (clear-approval-channel!)
     (define result (sync/timeout 1 result-ch))
     (check-true (tool-result? result))
     (check-true (tool-result-is-error? result))
     (check-equal? (unbox sends) 0)
     (check-equal? (pending-approval-count) 0))
   clear-approval-channel!))

(test-case "dangerous work after interactive teardown remains denied headlessly"
  (set-approval-channel! (make-approval-channel #:timeout-ms 5000))
  (clear-approval-channel!)
  (define sends (box 0))
  (define result
    (tool-spawn-subagents
     (hasheq 'jobs (list (hasheq 'task "must remain closed" 'capabilities '(shell-exec))))
     (make-counting-context void sends)))
  (check-true (tool-result-is-error? result))
  (check-equal? (unbox sends) 0))

(test-case "approved batch reserves one rate slot per child"
  (define timestamps (box '()))
  (parameterize ([current-spawn-timestamps timestamps])
    (define result
      (tool-spawn-subagents (hasheq 'jobs
                                    (list (hasheq 'task "one" 'capabilities '(read-only))
                                          (hasheq 'task "two" 'capabilities '(read-only))
                                          (hasheq 'task "three" 'capabilities '(read-only))))))
    (check-false (tool-result-is-error? result))
    (check-equal? (length (unbox timestamps)) 3)))

(test-case "approved dangerous jobs run once and preserve input order"
  (define-values (result events)
    (run/capture (hasheq 'jobs
                         (list (hasheq 'jobId "first" 'task "one" 'capabilities '(shell-exec))
                               (hasheq 'jobId "second" 'task "two" 'capabilities '(read-only))))
                 #t))
  (check-false (tool-result-is-error? result) (result-text result))
  (check-equal? (length (approval-events events)) 2)
  (check-equal? (length (terminal-events events)) 2)
  (define jobs (hash-ref (tool-result-details result) 'jobs))
  (check-equal? (map (lambda (job) (hash-ref job 'jobId)) jobs) '("first" "second"))
  (define approved-plan (hash-ref (cdar events) 'jobs))
  (for ([actual (in-list jobs)]
        [planned (in-list approved-plan)])
    (check-equal? (hash-ref actual 'toolCallId) (hash-ref planned 'tool-call-id))
    (check-equal? (hash-ref actual 'childId) (hash-ref planned 'child-id))
    (check-equal? (hash-ref actual 'sessionId) (hash-ref planned 'session-id))))

(test-case "concurrent batches sharing caller batchId cannot cross-deliver decisions"
  (dynamic-wind
   (lambda () (set-approval-channel! (make-approval-channel #:timeout-ms 2000)))
   (lambda ()
     (define sends (box 0))
     (define publisher
       (lambda (type payload)
         (when (string=? type "mas.spawn-approval-requested")
           (define request-id (hash-ref payload 'request-id))
           (define digest (hash-ref payload 'commitment-digest))
           (define view (approval-request-view request-id digest))
           (approval-decide! request-id
                             digest
                             (string-contains? (hash-ref view 'task-preview) "allow")))))
     (define ctx (make-counting-context publisher sends))
     (define results (make-channel))
     (define (start task)
       (thread
        (lambda ()
          (channel-put
           results
           (tool-spawn-subagents
            (hasheq 'batchId "shared-id" 'jobs (list (hasheq 'task task 'capabilities '(shell-exec))))
            ctx)))))
     (define allow-thread (start "allow this one"))
     (define deny-thread (start "deny this one"))
     (define first (channel-get results))
     (define second (channel-get results))
     (thread-wait allow-thread)
     (thread-wait deny-thread)
     (check-equal? (length (filter tool-result-is-error? (list first second))) 1)
     (check-equal? (unbox sends) 1)
     (check-equal? (pending-approval-count) 0))
   clear-approval-channel!))

(test-case "approved batch starts each planned provider job exactly once"
  (define sends (box 0))
  (dynamic-wind
   (lambda () (set-approval-channel! (make-approval-channel #:timeout-ms 1000)))
   (lambda ()
     (define publisher
       (lambda (type payload)
         (when (string=? type "mas.spawn-approval-requested")
           (check-true (approval-decide! (hash-ref payload 'request-id)
                                         (hash-ref payload 'commitment-digest)
                                         #t)))))
     (define result
       (parameterize ([current-spawn-timestamps (box '())])
         (tool-spawn-subagents
          (hasheq 'jobs
                  (list (hasheq 'jobId "one" 'task "one" 'capabilities '(shell-exec))
                        (hasheq 'jobId "two" 'task "two" 'capabilities '(read-only))
                        (hasheq 'jobId "three" 'task "three" 'capabilities '(file-write))))
          (make-counting-context publisher sends))))
     (check-false (tool-result-is-error? result) (result-text result))
     (check-equal? (unbox sends) 3))
   clear-approval-channel!))

(test-case "mutation reorder insertion removal widening and substitution invalidate approval"
  (define mutations
    (list (lambda (args jobs) (hash-set! (car jobs) 'task "substituted task"))
          (lambda (args jobs) (hash-set! (cadr jobs) 'capabilities '(read-only shell-exec)))
          (lambda (args jobs)
            (hash-set! args 'jobs (append jobs (list (hasheq 'jobId "inserted" 'task "new")))))
          (lambda (args jobs) (hash-set! args 'jobs (list (car jobs))))
          (lambda (args jobs) (hash-set! args 'jobs (reverse jobs)))
          (lambda (args jobs)
            (hash-set! args
                       'jobs
                       (cons (hasheq 'jobId "danger" 'task "replacement" 'capabilities '(shell-exec))
                             (cdr jobs))))))
  (for ([mutate! (in-list mutations)])
    (define danger
      (make-hasheq
       (list (cons 'jobId "danger") (cons 'task "before") (cons 'capabilities '(shell-exec)))))
    (define safe
      (make-hasheq
       (list (cons 'jobId "safe") (cons 'task "inspect") (cons 'capabilities '(read-only)))))
    (define jobs (list danger safe))
    (define mutable-args (make-hasheq (list (cons 'jobs jobs))))
    (define events '())
    (dynamic-wind
     (lambda () (set-approval-channel! (make-approval-channel #:timeout-ms 1000)))
     (lambda ()
       (define ctx
         (make-test-context (lambda (type payload)
                              (set! events (cons (cons type payload) events))
                              (when (string=? type "mas.spawn-approval-requested")
                                (mutate! mutable-args jobs)
                                (check-true (approval-decide! (hash-ref payload 'request-id)
                                                              (hash-ref payload 'commitment-digest)
                                                              #t))))))
       (define result (tool-spawn-subagents mutable-args ctx))
       (check-true (tool-result-is-error? result))
       (check-true (string-contains? (result-text result) "changed after approval"))
       (check-equal? (length events) 2))
     clear-approval-channel!)))

(test-case "approved batch uses provider and model captured by the committed plan"
  (define original-sends (box 0))
  (define replacement-sends (box 0))
  (define observed-models (box '()))
  (define (counting-provider name counter)
    (make-provider
     (lambda () name)
     (lambda () (hasheq 'streaming #f))
     (lambda (request)
       (set-box! counter (add1 (unbox counter)))
       (when (string=? name "original")
         (set-box! observed-models
                   (cons (hash-ref (model-request-settings request) 'model #f)
                         (unbox observed-models))))
       (make-model-response (list (hasheq 'type "text" 'text "done"))
                            (hasheq 'prompt-tokens 1 'completion-tokens 1 'total-tokens 2)
                            name
                            'stop))
     (lambda (_request) (error name "streaming not expected"))))
  (define original (counting-provider "original" original-sends))
  (define replacement (counting-provider "replacement" replacement-sends))
  (define settings (make-hasheq (list (cons 'provider original) (cons 'model "original-model"))))
  (dynamic-wind
   (lambda () (set-approval-channel! (make-approval-channel #:timeout-ms 1000)))
   (lambda ()
     (define ctx
       (make-exec-context #:event-publisher
                          (lambda (type payload)
                            (when (string=? type "mas.spawn-approval-requested")
                              (hash-set! settings 'provider replacement)
                              (hash-set! settings 'model "replacement-model")
                              (check-true (approval-decide! (hash-ref payload 'request-id)
                                                            (hash-ref payload 'commitment-digest)
                                                            #t))))
                          #:runtime-settings settings
                          #:call-id "provider-binding-test"))
     (define result
       (tool-spawn-subagents
        (hasheq 'jobs
                (list (hasheq 'task "bound" 'model "approved-model" 'capabilities '(shell-exec))))
        ctx))
     (check-false (tool-result-is-error? result) (result-text result))
     (check-equal? (unbox original-sends) 1)
     (check-equal? (unbox replacement-sends) 0)
     (check-equal? (unbox observed-models) '("approved-model")))
   clear-approval-channel!))
