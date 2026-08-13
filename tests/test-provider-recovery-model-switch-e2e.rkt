#lang racket

;; @speed fast
;; @suite runtime
;; BOUNDARY: integration

;; v0.99.95 W3: failed provider turn recovers the TUI, then the production
;; parsed /model path changes the live session and a later prompt succeeds.

(require rackunit
         racket/file
         racket/list
         "../runtime/agent-session.rkt"
         "../runtime/provider/model-registry.rkt"
         "../runtime/auto-retry.rkt"
         "../llm/model.rkt"
         "../llm/provider.rkt"
         "../tools/tool.rkt"
         "../tui/command-parse.rkt"
         "../tui/commands.rkt"
         "../tui/state.rkt"
         "../util/event/event-bus.rkt"
         "../util/event/event.rkt"
         (only-in "helpers/mock-failure-provider.rkt" make-failure-provider)
         (only-in "tui/event-simulator.rkt" simulate-events)
         (only-in "tui/state-assertions.rkt"
                  assert-error-displayed?
                  assert-idle?
                  assert-transcript-contains?))

(define (make-model-config)
  (hasheq 'providers
          (hasheq 'mock
                  (hasheq 'base-url
                          "https://mock.invalid/v1"
                          'default-model
                          "failed-model"
                          'models
                          '("failed-model" "deepseek-v4-flash")))
          'default-provider
          "mock"
          'default-model
          "failed-model"))

(define (make-command-context state bus registry sess)
  (cmd-ctx (box state)
           (box #t)
           bus
           #f
           (box #f)
           (box registry)
           (box #f)
           #f
           (box "")
           (box #f)
           #f
           (box sess)
           (box #f)))

(define (prompt-event? evt)
  (and (member (event-ev evt) '("turn.started" "turn.completed"))
       (equal? (hash-ref (event-payload evt) 'scope #f) "prompt")))

(test-case "rate-limit exhaustion recovers TUI before model switch and next prompt"
  (define tmpdir (make-temporary-file "q-provider-recovery-e2e-~a" 'directory))
  (define bus (make-event-bus))
  (define events (box '()))
  (subscribe! bus (lambda (evt) (set-box! events (append (unbox events) (list evt)))))
  (define-values (failing-provider stats)
    (make-failure-provider #:failure-mode 'rate-limit #:fail-times 3))
  (define requested-models (box '()))
  (define provider
    (make-provider (lambda () "model-capturing")
                   (lambda () (hasheq 'streaming #t))
                   (lambda (_req) (error 'unexpected-non-stream-request))
                   (lambda (req)
                     (if (< (cdr (assq 'fail-count (stats))) 3)
                         (provider-stream failing-provider req)
                         (begin
                           (set-box! requested-models
                                     (cons (hash-ref (model-request-settings req) 'model #f)
                                           (unbox requested-models)))
                           (list (make-stream-chunk "Recovered response" #f #f #f)
                                 (make-stream-chunk #f #f (hasheq 'total-tokens 1) #t)))))))
  (dynamic-wind
   void
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
                                   "failed-model")))
     ;; The real TUI subscribes after session construction, so its prompt slice
     ;; does not replay session.started into an already initialized state.
     (set-box! events '())
     (parameterize ([current-random-source (lambda () 0.0)])
       (run-prompt! sess "trigger rate limit"))

     (define first-events (unbox events))
     (define first-prompts (filter prompt-event? first-events))
     (define first-errors
       (filter (lambda (evt) (equal? (event-ev evt) "runtime.error")) first-events))
     (check-equal? (map event-ev first-prompts) '("turn.started" "turn.completed"))
     (check-equal? (length first-errors) 1)
     (define first-turn-id (event-turn-id (first first-prompts)))
     (define correlated-events
       (filter (lambda (evt) (or (equal? (event-ev evt) "runtime.error") (prompt-event? evt)))
               first-events))
     (check-equal? (map event-ev correlated-events)
                   '("turn.started" "runtime.error" "turn.completed"))
     (check-equal? (map event-turn-id correlated-events) (make-list 3 first-turn-id))
     (check-equal? (cdr (assq 'fail-count (stats))) 3)

     (define recovered-state
       (simulate-events (initial-ui-state #:session-id (session-id sess) #:model-name "failed-model")
                        first-events))
     (assert-idle? recovered-state)
     (assert-error-displayed? recovered-state)
     (assert-transcript-contains? recovered-state "429")
     (check-false (ui-state-active-turn-id recovered-state))

     (define registry (make-model-registry-from-config (make-model-config)))
     (define cctx (make-command-context recovered-state bus registry sess))
     (define parsed (parse-command-name "/model deepseek-v4-flash"))
     (check-true (parsed-command? parsed))
     (check-equal? (process-slash-command cctx parsed) 'continue)
     (check-equal? (agent-session-model-name sess) "deepseek-v4-flash")
     (check-equal? (ui-state-model-name (unbox (cmd-ctx-state-box cctx))) "deepseek-v4-flash")
     (assert-transcript-contains? (unbox (cmd-ctx-state-box cctx)) "switched to model")

     (set-box! events '())
     (run-prompt! sess "continue after switch")
     (define second-events (unbox events))
     (check-equal? (unbox requested-models) '("deepseek-v4-flash"))
     (check-false (findf (lambda (evt) (equal? (event-ev evt) "runtime.error")) second-events))
     (define final-state (simulate-events (unbox (cmd-ctx-state-box cctx)) second-events))
     (assert-idle? final-state)
     (check-equal? (hash-ref (event-payload (last (filter prompt-event? second-events))) 'reason)
                   "completed"))
   (lambda () (delete-directory/files tmpdir #:must-exist? #f))))
