#lang racket/base

;; Correlated singleton and immutable-batch HITL approval transport.
;; This boundary owns redacted terminal-safe previews and explicit headless policy.

(require racket/list
         racket/string
         "../../tools/tool.rkt"
         (only-in "../../util/credential-redaction.rkt" redact-secrets)
         (only-in "../../tui/approval-channel.rkt"
                  current-approval-channel
                  headless-approval-mode?
                  register-approval-request-for-channel!
                  approval-await-for-id
                  cancel-approval-request!))

(provide current-spawn-approval-result
         request-spawn-approval
         request-batch-spawn-approval)

;; Tests and explicitly headless process startup may override this decision.
;; Interactive teardown never consults it because the lifecycle mode is closed.
(define current-spawn-approval-result (make-parameter #t))

(define (redacted-task-preview task [limit 200])
  (define redacted (redact-secrets (if (string? task) task "")))
  (define control-safe
    (list->string (for/list ([char (in-string redacted)])
                    (define code (char->integer char))
                    (if (or (< code 32) (and (>= code 127) (<= code 159))) #\space char))))
  (define safe (string-join (string-split control-safe) " "))
  (if (> (string-length safe) limit)
      (substring safe 0 limit)
      safe))

(define (await-interactive-approval ch publisher request-payload terminal-payload)
  (define req-id (register-approval-request-for-channel! ch))
  (cond
    [(or (not req-id) (not publisher))
     (when req-id
       (cancel-approval-request! req-id))
     #f]
    [else
     (define status "cancelled")
     (define owner (current-thread))
     (define owner-finished (make-semaphore 0))
     ;; kill-thread does not unwind dynamic-wind, so a monitor removes the
     ;; exact request if the publisher/owner dies before normal cleanup.
     (thread (lambda ()
               (define abandoned?
                 (sync (handle-evt (thread-dead-evt owner) (lambda (_) #t))
                       (handle-evt owner-finished (lambda (_) #f))))
               (when abandoned?
                 (cancel-approval-request! req-id))))
     (dynamic-wind
      void
      (lambda ()
        (publisher "mas.spawn-approval-requested" (hash-set request-payload 'request-id req-id))
        (define-values (approved? delivered?) (approval-await-for-id req-id))
        (set! status
              (cond
                [(not delivered?) "timed-out-or-cancelled"]
                [approved? "approved"]
                [else "denied"]))
        (and delivered? approved?))
      (lambda ()
        (semaphore-post owner-finished)
        (cancel-approval-request! req-id)
        (with-handlers ([exn:fail? (lambda (_) (void))])
          (publisher
           "mas.spawn-approval-terminal"
           (hash-set (hash-set terminal-payload 'request-id req-id) 'terminal-status status)))))]))

(define (headless-spawn-approved? payload)
  (and (headless-approval-mode?)
       (let ([decision (current-spawn-approval-result)])
         (eq? (if (procedure? decision)
                  (decision payload)
                  decision)
              #t))))

(define (request-spawn-approval capabilities task-desc exec-ctx)
  (define publisher (and exec-ctx (exec-context-event-publisher exec-ctx)))
  (define ch (current-approval-channel))
  (define payload (hasheq 'capabilities capabilities 'task-preview (redacted-task-preview task-desc)))
  (cond
    [ch (await-interactive-approval ch publisher payload (hasheq))]
    [else
     (when publisher
       (publisher "mas.spawn-approval-requested" payload))
     (headless-spawn-approved? payload)]))

(define (request-batch-spawn-approval batch-id snapshot snapshot-digest dangerous-jobs exec-ctx)
  (define publisher (and exec-ctx (exec-context-event-publisher exec-ctx)))
  (define ch (current-approval-channel))
  (define capabilities
    (remove-duplicates (append-map (lambda (job) (or (hash-ref job 'effective-capabilities #f) '()))
                                   dangerous-jobs)
                       eq?))
  (define (safe-job-manifest job)
    (define without-caller-text (hash-remove (hash-remove (hash-remove job 'task) 'role) 'model))
    (hash-set (hash-set (hash-set without-caller-text
                                  'task-preview
                                  (redacted-task-preview (hash-ref job 'task "") 120))
                        'role-preview
                        (redacted-task-preview (hash-ref job 'role "") 120))
              'model-preview
              (redacted-task-preview (hash-ref job 'model "") 80)))
  (define safe-snapshot (hash-set snapshot 'jobs (map safe-job-manifest (hash-ref snapshot 'jobs))))
  (define safe-dangerous-jobs (map safe-job-manifest dangerous-jobs))
  (define base-payload
    (hasheq
     'batch-id
     batch-id
     'snapshot
     safe-snapshot
     'snapshot-digest
     snapshot-digest
     'dangerous-jobs
     safe-dangerous-jobs
     'capabilities
     capabilities
     'task-preview
     (string-join
      (cons (format "Dangerous subagent batch (~a job~a)"
                    (length dangerous-jobs)
                    (if (= (length dangerous-jobs) 1) "" "s"))
            (for/list ([job (in-list dangerous-jobs)])
              (format
               "[~a] task=~s role=~s model=~s task-sha256=~a caps=~a tool-call=~a child=~a session=~a"
               (hash-ref job 'job-id)
               (redacted-task-preview (hash-ref job 'task "") 120)
               (redacted-task-preview (hash-ref job 'role "") 120)
               (redacted-task-preview (hash-ref job 'model "") 80)
               (hash-ref job 'task-digest)
               (string-join (map symbol->string (hash-ref job 'effective-capabilities)) ",")
               (hash-ref job 'tool-call-id)
               (hash-ref job 'child-id)
               (hash-ref job 'session-id))))
      " | ")))
  (cond
    [ch
     (await-interactive-approval ch
                                 publisher
                                 base-payload
                                 (hasheq 'batch-id batch-id 'snapshot-digest snapshot-digest))]
    [else
     (when publisher
       (publisher "mas.spawn-approval-requested" base-payload))
     (headless-spawn-approved? base-payload)]))
