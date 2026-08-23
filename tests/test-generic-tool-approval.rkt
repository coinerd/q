#lang racket/base

;; @speed fast
;; @suite security
;; @boundary unit

(require rackunit
         rackunit/text-ui
         racket/string
         "../runtime/approval/broker.rkt"
         "../tools/permission-gate.rkt"
         "../tools/tool.rkt"
         "../tools/scheduler.rkt"
         (only-in "../util/tool/tool-types.rkt" make-tool-call))

(define (result-error? batch)
  (tool-result-is-error? (car (scheduler-result-results batch))))

(define (run-probe cfg publisher executed)
  (define registry (make-tool-registry))
  (register-tool! registry
                  (make-tool "danger-probe"
                             "generic approval probe"
                             (hasheq 'type "object" 'properties (hasheq))
                             (lambda (args _ctx)
                               (set-box! executed args)
                               (make-success-result "ok"))
                             #:dangerous? #t
                             #:timeout-seconds 7))
  (run-tool-batch (list (make-tool-call "generic-1"
                                        "danger-probe"
                                        (hasheq 'command "deploy" 'password "super-secret-value")))
                  registry
                  #:parallel? #f
                  #:exec-context (make-exec-context #:working-directory "/tmp/approval-project"
                                                    #:event-publisher publisher
                                                    #:permission-config cfg)))

(define suite
  (test-suite "generic digest-bound tool approval"

    (test-case "interactive grant commits and executes exact scheduler final args"
      (define executed (box #f))
      (define events '())
      (define seen-view (box #f))
      (define seen-commitment (box #f))
      (dynamic-wind
       (lambda () (set-approval-channel! (make-approval-channel #:timeout-ms 500)))
       (lambda ()
         (define (publisher type payload)
           (set! events (append events (list (cons type payload))))
           (when (string=? type "tool.approval-requested")
             (define id (hash-ref payload 'request-id))
             (define digest (hash-ref payload 'commitment-digest))
             (set-box! seen-commitment digest)
             (set-box! seen-view (approval-request-view id digest))
             (check-true (approval-decide! id digest #t))))
         (define result (run-probe (make-interactive-permission-config) publisher executed))
         (check-false (result-error? result))
         (define final-args (unbox executed))
         (check-true (immutable? final-args))
         (check-equal? (hash-ref final-args 'command) "deploy")
         (check-equal? (hash-ref final-args 'timeout) 7)
         (check-equal? (hash-ref final-args 'working-directory) "/tmp/approval-project")
         (check-equal? (unbox seen-commitment)
                       (tool-approval-commitment-digest "danger-probe" final-args))
         (define view (unbox seen-view))
         (check-equal? (hash-ref view 'approval-kind) "tool")
         (check-equal? (hash-ref view 'tool-name) "danger-probe")
         (check-true (<= (string-length (hash-ref view 'arguments-preview)) 500))
         (check-false (string-contains? (format "~s" view) "super-secret-value"))
         ;; Telemetry carries correlation/digests only, never final arguments or previews.
         (for ([entry (in-list events)])
           (check-false (string-contains? (format "~s" (cdr entry)) "super-secret-value")))
         (check-equal?
          (map car (filter (lambda (entry) (string-prefix? (car entry) "tool.approval")) events))
          '("tool.approval-requested" "tool.approval-terminal")))
       clear-approval-channel!))

    (test-case "missing channel publisher and timeout all deny"
      (define cfg (make-interactive-permission-config))
      (clear-approval-channel!)
      (check-false (request-approval cfg "bash" (hasheq 'command "echo no channel") void))
      (dynamic-wind
       (lambda () (set-approval-channel! (make-approval-channel #:timeout-ms 15)))
       (lambda ()
         (check-false (request-approval cfg "bash" (hasheq 'command "echo no publisher")))
         (check-equal? (pending-approval-count) 0)
         (check-false (request-approval cfg "bash" (hasheq 'command "echo timeout") void))
         (check-equal? (pending-approval-count) 0))
       clear-approval-channel!))

    (test-case "wrong digest decision cannot authorize generic execution"
      (define executed (box #f))
      (dynamic-wind (lambda () (set-approval-channel! (make-approval-channel #:timeout-ms 15)))
                    (lambda ()
                      (define result
                        (run-probe (make-interactive-permission-config)
                                   (lambda (type payload)
                                     (when (string=? type "tool.approval-requested")
                                       (check-false (approval-decide! (hash-ref payload 'request-id)
                                                                      (make-string 64 #\f)
                                                                      #t))))
                                   executed))
                      (check-true (result-error? result))
                      (check-false (unbox executed)))
                    clear-approval-channel!))))

(module+ test
  (run-tests suite))
