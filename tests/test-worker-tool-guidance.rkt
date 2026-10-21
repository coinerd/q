#lang racket/base

;; @speed fast
;; @suite default
;; @boundary unit

(require rackunit
         racket/string
         racket/file
         "../util/worker-tool-policy.rkt"
         (only-in "../runtime/iteration/step-executor.rkt" interpret-step)
         (only-in "../agent/iteration/loop-state.rkt"
                  loop-infra
                  iteration-snapshot
                  make-initial-counters)
         (only-in "../agent/state.rkt" current-empty-response-retried? current-empty-response-nudge)
         (only-in "../util/iteration/decision.rkt" step-result)
         (only-in "../util/iteration/directive.rkt" directive-recurse-new-ctx)
         (only-in "../util/loop-result.rkt" make-loop-result)
         (only-in "../util/event/event-bus.rkt" make-event-bus)
         (only-in "../runtime/session/session-config.rkt" hash->session-config)
         "../runtime/context-assembly/state-aware-builder.rkt"
         "../runtime/context-assembly/context-floor.rkt"
         "../runtime/context-assembly/task-conclusion.rkt"
         "../util/message/message.rkt"
         "../util/content/content-parts.rkt")

(define policy
  (worker-tool-policy
   (lambda (schemas) (filter (lambda (s) (equal? (hash-ref s 'name #f) "read")) schemas))
   (lambda (name args) (and (not (equal? name "read")) "task scope forbids this tool"))))

(define (text m)
  (string-join (for/list ([p (in-list (message-content m))]
                          #:when (text-part? p))
                 (text-part-text p))
               "\n"))

(module+ test
  (test-case "protected worker context preserves task and reference text, not general workflow"
    (define reference "Read-only source example: Use record_conclusion; set_task_state.")
    (define source
      (make-message "source"
                    #f
                    'system
                    'system-instruction
                    (list (make-text-part reference))
                    0
                    (hasheq)))
    (define memory (task-conclusion "c" "Existing code context" 'fact 'exploration '() 0 '() '()))
    (parameterize ([current-worker-tool-policy policy])
      (for ([state '(idle exploration planning implementation verification debugging)])
        (define context
          (build-tiered-context/state-aware (list source)
                                            #:task-state state
                                            #:conclusions (list memory)
                                            #:reflection-event (hasheq 'tools '("read"))))
        (define entries (tiered-context-tier-a context))
        (define preamble (text (car entries)))
        (check-false (regexp-match?
                      #rx"record_conclusion|set_task_state|Workflow:|Focus on editing|Run tests"
                      preamble))
        (check-true (string-contains? preamble "task contract"))
        (check-true (string-contains? preamble "Existing code context"))
        (check-not-false (member reference (map text entries))))))

  (test-case "empty memory and reflection never request unavailable tools"
    (parameterize ([current-worker-tool-policy policy])
      (for ([state '(idle exploration planning implementation verification debugging)])
        (define preamble (text (build-state-awareness-preamble state '() #:reflection-event #t)))
        (check-true (string-contains? preamble "No conclusions in memory yet."))
        (check-false (regexp-match? #rx"record_conclusion|set_task_state|Workflow:|Reminder:"
                                    preamble)))))

  (test-case "empty-response retry respects worker role and owner reanchor"
    (define counters (make-initial-counters))
    (define log-path (make-temporary-file "worker-guidance-~a.jsonl"))
    (define infra (loop-infra '() #f #f (make-event-bus) "worker-guidance" log-path #f))
    (define snapshot (iteration-snapshot counters #f (hash->session-config (hasheq)) #f 5 10))
    (define (retry-text)
      (define next
        (interpret-step (step-result 'continue #f counters (hasheq 'emptyResponse #t))
                        (make-loop-result '() 'completed (hasheq))
                        '()
                        infra
                        snapshot))
      (text (car (directive-recurse-new-ctx next))))
    (parameterize ([current-worker-tool-policy policy]
                   [current-empty-response-retried? #f]
                   [current-empty-response-nudge #f])
      (check-true (string-contains? (retry-text) "task contract")))
    (parameterize ([current-worker-tool-policy policy]
                   [current-empty-response-retried? #f]
                   [current-empty-response-nudge "Owner: author only the two approved tests."])
      (check-equal? (retry-text) "Owner: author only the two approved tests."))
    (delete-file log-path))

  (test-case "ordinary session guidance remains unchanged"
    (parameterize ([current-worker-tool-policy #f])
      (define preamble
        (text (build-state-awareness-preamble 'implementation '() #:reflection-event #t)))
      (check-true (string-contains? preamble "You are currently in the IMPLEMENTATION phase."))
      (check-true (string-contains? preamble "Focus on editing files."))
      (check-true (string-contains? preamble "Use record_conclusion"))
      (check-true (string-contains? preamble "Use set_task_state to transition:"))
      (check-true (string-contains? preamble "Reminder:")))))
