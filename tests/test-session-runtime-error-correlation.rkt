#lang racket

;; @speed fast
;; @suite runtime
;; @boundary integration

;; v0.99.95 W2: prompt lifecycle error and terminal identity correlation.

(require rackunit
         racket/file
         "../runtime/agent-session.rkt"
         "../tools/tool.rkt"
         "../util/event/event-bus.rkt"
         "../util/event/event.rkt"
         (only-in "helpers/mock-failure-provider.rkt" make-failure-provider)
         (only-in "../runtime/auto-retry.rkt" current-random-source))

(define (prompt-scope? evt)
  (and (member (event-ev evt) '("turn.started" "turn.completed"))
       (equal? (hash-ref (event-payload evt) 'scope #f) "prompt")))

(test-case "retry exhaustion preserves one prompt identity across error and terminal"
  (define tmpdir (make-temporary-file "q-runtime-error-correlation-~a" 'directory))
  (define bus (make-event-bus))
  (define events (box '()))
  (subscribe! bus (lambda (evt) (set-box! events (append (unbox events) (list evt)))))
  (define-values (provider _stats) (make-failure-provider #:failure-mode 'rate-limit #:fail-times 10))
  (dynamic-wind void
                (lambda ()
                  (define sess
                    (make-agent-session (hasheq 'provider
                                                provider
                                                'tool-registry
                                                (make-tool-registry)
                                                'event-bus
                                                bus
                                                'session-dir
                                                (path->string tmpdir)
                                                'model-name
                                                "retry-exhaustion-test")))
                  (parameterize ([current-random-source (lambda () 0.0)])
                    (run-prompt! sess "trigger retry exhaustion"))
                  (define prompt-events (filter prompt-scope? (unbox events)))
                  (define errors
                    (filter (lambda (evt) (equal? (event-ev evt) "runtime.error")) (unbox events)))
                  (check-equal? (map event-ev prompt-events) '("turn.started" "turn.completed"))
                  (check-equal? (length errors) 1)
                  (define prompt-turn-id (event-turn-id (first prompt-events)))
                  (check-pred string? prompt-turn-id)
                  (check-equal? (event-turn-id (first errors)) prompt-turn-id)
                  (check-equal? (event-turn-id (second prompt-events)) prompt-turn-id)
                  ;; BUG-0022 W2B: retries-attempted was 2 (old buggy truncation);
                  ;; the fixed health gate allows the full 5-retry budget.
                  (check-equal? (hash-ref (event-payload (first errors)) 'retries-attempted) 5)
                  (check-equal? (cdr (assq 'fail-count (_stats))) 6)
                  (check-equal? (hash-ref (event-payload (second prompt-events)) 'reason) "error"))
                (lambda () (delete-directory/files tmpdir #:must-exist? #f))))
