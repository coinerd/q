#lang racket/base

;; @speed fast
;; @suite security

(require rackunit
         racket/list
         racket/string
         "../tools/builtins/spawn-subagent.rkt"
         (only-in "../tools/builtins/spawn-subagent-helpers.rkt" sha256-digest)
         (only-in "../tui/approval-channel.rkt"
                  make-approval-channel
                  set-approval-channel!
                  clear-approval-channel!
                  set-headless-approval-mode!
                  pending-approval-count
                  approval-put-for-id!)
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

(define (run/capture args decision)
  (set-headless-approval-mode!)
  (define events '())
  (define ctx
    (make-test-context (lambda (type payload) (set! events (cons (cons type payload) events)))))
  (define result
    (parameterize ([current-spawn-approval-result decision]
                   [current-spawn-timestamps (box '())])
      (tool-spawn-subagents args ctx)))
  (values result (reverse events)))

(test-case "explicit invalid capability rejects the whole batch before approval"
  (define-values (result events)
    (run/capture (hasheq 'jobs
                         (list (hasheq 'task "valid" 'capabilities '(read-only))
                               (hasheq 'task "invalid" 'capabilities '(shell-exec not-a-capability))))
                 #t))
  (check-true (tool-result-is-error? result))
  (check-true (string-contains? (result-text result) "invalid capability"))
  (check-equal? events '()))

(test-case "omitted capabilities retain bounded defaults and bind the effective model"
  (define-values (result events) (run/capture (hasheq 'jobs (list (hasheq 'task "ordinary"))) #f))
  (check-true (tool-result-is-error? result))
  (check-equal? (length events) 1)
  (define planned-job (car (hash-ref (hash-ref (cdar events) 'snapshot) 'jobs)))
  (check-equal? (hash-ref planned-job 'effective-capabilities) '(read-only file-write shell-exec))
  (check-equal? (hash-ref planned-job 'model-preview) "mock-model"))

(test-case "explicit empty authority stays empty and needs no approval"
  (define-values (result events)
    (run/capture (hasheq 'jobs (list (hasheq 'task "no tools" 'capabilities '()))) #f))
  (check-false (tool-result-is-error? result) (result-text result))
  (check-equal? events '()))

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
  (define planned-job (car (hash-ref (hash-ref (cdar events) 'snapshot) 'jobs)))
  (define tools (hash-ref planned-job 'effective-tools))
  (check-false (member "bash" tools))
  (check-false (member "write" tools))
  (check-false (member "edit" tools)))

(test-case "explicit safe capabilities do not request approval"
  (define-values (result events)
    (run/capture (hasheq 'jobs (list (hasheq 'task "inspect" 'capabilities '(read-only)))) #f))
  (check-false (tool-result-is-error? result))
  (check-equal? events '()))

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
  (check-equal? (length events) 1)
  (define payload (cdar events))
  (check-equal? (caar events) "mas.spawn-approval-requested")
  (check-equal? (hash-ref payload 'batch-id) "batch-explicit")
  (check-equal? (length (hash-ref payload 'dangerous-jobs)) 2)
  (check-true (non-empty-string? (hash-ref payload 'snapshot-digest)))
  (define dangerous (car (hash-ref payload 'dangerous-jobs)))
  (check-equal? (hash-ref dangerous 'task-digest) (sha256-digest "danger"))
  (check-false (hash-has-key? dangerous 'task))
  (check-equal? (hash-ref dangerous 'task-preview) "danger")
  (check-true (string-contains? (hash-ref payload 'task-preview) "danger"))
  (define planned-jobs (hash-ref (hash-ref payload 'snapshot) 'jobs))
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
         (set! payloads (append payloads (list (cons type payload))))
         (when (string=? type "mas.spawn-approval-requested")
           (check-true (approval-put-for-id! (hash-ref payload 'request-id) #t)))))
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
     (check-equal? (map car payloads) '("mas.spawn-approval-requested" "mas.spawn-approval-terminal"))
     (for* ([entry (in-list payloads)]
            [secret-fragment (in-list '("super-secret-value-123456" "role-secret-value-123456"
                                                                    "model-secret-value-123456"))])
       (check-false (string-contains? (format "~s" (cdr entry)) secret-fragment)))
     (define request (cdar payloads))
     (for* ([job (in-list (append (hash-ref (hash-ref request 'snapshot) 'jobs)
                                  (hash-ref request 'dangerous-jobs)))]
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
  (define planned-job (car (hash-ref (hash-ref payload 'snapshot) 'jobs)))
  (for ([payload-text (in-list (list (hash-ref payload 'task-preview)
                                     (hash-ref planned-job 'task-preview)
                                     (hash-ref planned-job 'role-preview)))])
    (check-false (for/or ([char (in-string payload-text)])
                   (define code (char->integer char))
                   (or (< code 32) (and (>= code 127) (<= code 159)))))))

(test-case "dangerous denial reaches zero provider sends"
  (define sends (box 0))
  (define events '())
  (define ctx
    (make-counting-context (lambda (type payload)
                             (set! events (append events (list (cons type payload)))))
                           sends))
  (define result
    (parameterize ([current-spawn-approval-result #f])
      (tool-spawn-subagents (hasheq 'jobs
                                    (list (hasheq 'task "must not run" 'capabilities '(shell-exec))))
                            ctx)))
  (check-true (tool-result-is-error? result))
  (check-equal? (unbox sends) 0)
  (check-equal? (map car events) '("mas.spawn-approval-requested")))

(test-case "denial timeout and cancellation all start zero jobs"
  (for ([decision (in-list (list #f 'timeout 'cancelled))])
    (define-values (result events)
      (run/capture
       (hasheq 'jobs (list (hasheq 'jobId "danger" 'task "do not run" 'capabilities '(shell-exec))))
       decision))
    (check-true (tool-result-is-error? result))
    (check-equal? (length events) 1)))

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

(test-case "dangerous work after interactive teardown cannot use permissive headless hook"
  (set-approval-channel! (make-approval-channel #:timeout-ms 5000))
  (clear-approval-channel!)
  (define sends (box 0))
  (define result
    (parameterize ([current-spawn-approval-result #t])
      (tool-spawn-subagents
       (hasheq 'jobs (list (hasheq 'task "must remain closed" 'capabilities '(shell-exec))))
       (make-counting-context void sends))))
  (check-true (tool-result-is-error? result))
  (check-equal? (unbox sends) 0))

(test-case "approved batch reserves one rate slot per child"
  (define timestamps (box '()))
  (parameterize ([current-spawn-approval-result #t]
                 [current-spawn-timestamps timestamps])
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
  (check-equal? (length events) 1)
  (define jobs (hash-ref (tool-result-details result) 'jobs))
  (check-equal? (map (lambda (job) (hash-ref job 'jobId)) jobs) '("first" "second"))
  (define approved-plan (hash-ref (hash-ref (cdar events) 'snapshot) 'jobs))
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
           (approval-put-for-id! (hash-ref payload 'request-id)
                                 (string-contains? (hash-ref payload 'task-preview) "allow")))))
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
  (set-headless-approval-mode!)
  (define sends (box 0))
  (define result
    (parameterize ([current-spawn-approval-result #t]
                   [current-spawn-timestamps (box '())])
      (tool-spawn-subagents
       (hasheq 'jobs
               (list (hasheq 'jobId "one" 'task "one" 'capabilities '(shell-exec))
                     (hasheq 'jobId "two" 'task "two" 'capabilities '(read-only))
                     (hasheq 'jobId "three" 'task "three" 'capabilities '(file-write))))
       (make-counting-context void sends))))
  (check-false (tool-result-is-error? result) (result-text result))
  (check-equal? (unbox sends) 3))

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
    (define ctx
      (make-test-context (lambda (type payload)
                           (set! events (cons (cons type payload) events))
                           (mutate! mutable-args jobs))))
    (define result
      (parameterize ([current-spawn-approval-result #t])
        (tool-spawn-subagents mutable-args ctx)))
    (check-true (tool-result-is-error? result))
    (check-true (string-contains? (result-text result) "changed after approval"))
    (check-equal? (length events) 1)))
