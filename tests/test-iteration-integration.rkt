#lang racket/base

;; BOUNDARY: integration

;; tests/test-iteration-integration.rkt — integration tests for struct refactor
;; v0.29.17 W0: Tests for interpret-step internals via (module+ for-testing)

(require rackunit
         (only-in "../runtime/session-config.rkt" hash->session-config)
         racket/set
         racket/file
         (only-in "../agent/iteration/step-interpreter.rkt"
                  handle-stop-action
                  interpret-step
                  execute-pending-tool-calls)
         (only-in "../agent/iteration/counters.rkt" compute-next-counters check-cancellation)
         (only-in "../runtime/iteration/decision.rkt"
                  decide-next-action
                  compute-step-result
                  iteration-ctx
                  step-result)
         (only-in "../agent/iteration/loop-state.rkt"
                  make-initial-counters
                  loop-infra
                  iteration-snapshot
                  loop-counters
                  loop-counters-iteration
                  loop-counters-consecutive-tool-count
                  loop-counters-seen-paths
                  loop-counters-explore-count
                  loop-counters-implement-count
                  loop-counters-consecutive-error-count
                  loop-counters-recent-tool-names)
         (only-in "../util/protocol-types.rkt"
                  make-message
                  make-text-part
                  make-tool-result-part
                  make-tool-call-part
                  message-role
                  message-content
                  tool-result-part-is-error?)
         (only-in "../util/loop-result.rkt"
                  make-loop-result
                  loop-result?
                  loop-result-termination-reason
                  loop-result-metadata)
         (only-in "../util/ids.rkt" generate-id)
         (only-in "../util/cancellation.rkt"
                  make-cancellation-token
                  cancel-token!
                  cancellation-token-cancelled?)
         (only-in "../util/event.rkt" event-ev)
         (only-in "../runtime/iteration/decision.rkt" step-result)
         (only-in "../agent/iteration/step-interpreter.rkt"
                  handle-stop-action
                  interpret-step
                  execute-pending-tool-calls)
         (only-in "../agent/iteration/counters.rkt" compute-next-counters check-cancellation)
         (only-in "../runtime/iteration/decision.rkt"
                  decide-next-action
                  compute-step-result
                  iteration-ctx
                  step-result)
         (only-in "../runtime/iteration/directive.rkt"
                  directive-recurse?
                  directive-stop?
                  directive-recurse-new-ctx
                  directive-recurse-new-counters
                  directive-stop-result)
         "../agent/event-bus.rkt"
         "../runtime/working-set.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-mock-infra)
  (loop-infra '() #f #f (make-event-bus) "test-session" "/tmp/test.log" #f))

(define (make-msg-with-tool-call name args)
  (make-message (generate-id)
                #f
                'assistant
                'text
                (list (make-tool-call-part (generate-id) name args))
                1000
                (hasheq)))

(define (make-msg-with-tool-result text is-error?)
  (make-message (generate-id)
                #f
                'tool
                'tool-result
                (list (make-tool-result-part (generate-id) text is-error?))
                1000
                (hasheq)))

;; ============================================================
;; compute-next-counters tests
;; ============================================================

(test-case "compute-next-counters: read-only tools increment explore-count"
  (define counters (make-initial-counters))
  (define msgs (list (make-msg-with-tool-call "read" (hasheq 'path "/tmp/x.rkt"))))
  (define new-counters (compute-next-counters counters msgs))
  (check-equal? (loop-counters-explore-count new-counters) 1)
  (check-equal? (loop-counters-implement-count new-counters) 0)
  (check-true (set-member? (loop-counters-seen-paths new-counters) "/tmp/x.rkt")))

(test-case "compute-next-counters: edit/write tools increment implement-count"
  (define counters (make-initial-counters))
  (define msgs (list (make-msg-with-tool-call "edit" (hasheq 'path "/tmp/x.rkt"))))
  (define new-counters (compute-next-counters counters msgs))
  (check-equal? (loop-counters-implement-count new-counters) 1)
  (check-equal? (loop-counters-explore-count new-counters) 0))

(test-case "compute-next-counters: tool errors increment consecutive-error-count"
  (define counters (make-initial-counters))
  (define msgs (list (make-msg-with-tool-result "error" #t)))
  (define new-counters (compute-next-counters counters msgs))
  (check-equal? (loop-counters-consecutive-error-count new-counters) 1))

(test-case "compute-next-counters: consecutive-tool-count increments on new path"
  (define counters (make-initial-counters))
  (define msgs (list (make-msg-with-tool-call "read" (hasheq 'path "/tmp/a.rkt"))))
  (define new-counters (compute-next-counters counters msgs))
  (check-equal? (loop-counters-consecutive-tool-count new-counters) 1))

;; ============================================================
;; decide-next-action tests
;; ============================================================

(test-case "decide-next-action: soft-limit when iteration >= max"
  (define ctx (iteration-ctx 5 0 0 5 10))
  (define result (make-loop-result '() 'tool-calls-pending (hasheq)))
  (check-equal? (decide-next-action ctx result) 'stop-soft-limit))

(test-case "decide-next-action: hard-limit when iteration >= hard-max"
  (define ctx (iteration-ctx 10 0 0 5 10))
  (define result (make-loop-result '() 'tool-calls-pending (hasheq)))
  (check-equal? (decide-next-action ctx result) 'stop-hard-limit))

(test-case "decide-next-action: continue when under limits"
  (define ctx (iteration-ctx 3 0 0 5 10))
  (define result (make-loop-result '() 'tool-calls-pending (hasheq)))
  (check-equal? (decide-next-action ctx result) 'continue))

(test-case "decide-next-action: stop on completed termination"
  (define ctx (iteration-ctx 3 0 0 5 10))
  (define result (make-loop-result '() 'completed (hasheq)))
  (check-equal? (decide-next-action ctx result) 'stop))

;; ============================================================
;; check-cancellation tests
;; ============================================================

(test-case "check-cancellation: returns #f when nothing cancelled"
  (define bus (make-event-bus))
  (define ctx '())
  (define tok (make-cancellation-token))
  (define result (check-cancellation tok #f #f bus "test-session" 3 ctx))
  (check-false result))

(test-case "check-cancellation: returns loop-result when token cancelled"
  (define bus (make-event-bus))
  (define ctx '())
  (define tok (make-cancellation-token))
  (cancel-token! tok)
  (define result (check-cancellation tok #f #f bus "test-session" 3 ctx))
  (check-true (loop-result? result))
  (check-equal? (loop-result-termination-reason result) 'cancelled))

(test-case "check-cancellation: returns loop-result on force-shutdown"
  (define bus (make-event-bus))
  (define ctx '())
  (define force-check (lambda () #t))
  (define result (check-cancellation #f force-check #f bus "test-session" 3 ctx))
  (check-true (loop-result? result))
  (check-equal? (loop-result-termination-reason result) 'cancelled))

(test-case "check-cancellation: returns loop-result on graceful shutdown"
  (define bus (make-event-bus))
  (define ctx '())
  (define shutdown-check (lambda () #t))
  (define result (check-cancellation #f #f shutdown-check bus "test-session" 3 ctx))
  (check-true (loop-result? result))
  (check-equal? (loop-result-termination-reason result) 'completed))

;; ============================================================
;; interpret-step tests
;; ============================================================

;; Helper: create a temp log file for append-entries!
(define (make-temp-log-path)
  (make-temporary-file "iteration-test-~a.log"))

;; Helper: subscribe to bus and collect event payloads matching name
(define (collect-events bus event-name)
  (define events-box (box '()))
  (subscribe! bus
              (lambda (evt) (set-box! events-box (cons evt (unbox events-box))))
              #:filter (lambda (e) (equal? (event-ev e) event-name)))
  events-box)

(test-case "interpret-step: 'stop-hard-limit emits runtime.error event"
  (define bus (make-event-bus))
  (define log-path (make-temp-log-path))
  (define infra (loop-infra '() #f #f bus "test-session" log-path #f))
  (define counters (make-initial-counters))
  (define ws (make-working-set))
  (define result (make-loop-result '() 'tool-calls-pending (hasheq)))
  (define events-box (collect-events bus "runtime.error"))
  (define max-iter 5)
  (define max-hard 10)
  (define step-res
    (step-result 'stop-hard-limit 'max-iterations-exceeded (make-initial-counters) (hasheq)))
  (define out
    (interpret-step
     step-res
     result
     '()
     infra
     (iteration-snapshot counters ws (hash->session-config (hash)) #f max-iter max-hard)))
  (check-true (directive-stop? out))
  (define stop-result (directive-stop-result out))
  (check-equal? (loop-result-termination-reason stop-result) 'max-iterations-exceeded)
  (check-true (hash-has-key? (loop-result-metadata stop-result) 'maxIterationsReached))
  (check-true (pair? (unbox events-box)) "runtime.error event was emitted")
  (delete-file log-path))

(test-case "interpret-step: 'continue returns directive-recurse with updated counters"
  (define bus (make-event-bus))
  (define log-path (make-temp-log-path))
  (define infra (loop-infra '() #f #f bus "test-session" log-path #f))
  (define counters (make-initial-counters))
  (define ws (make-working-set))
  (define result-no-msgs (make-loop-result '() 'tool-calls-pending (hasheq)))
  (define step-res
    (step-result 'continue 'tool-calls-pending (compute-next-counters counters '()) (hasheq)))
  (define out
    (interpret-step step-res
                    result-no-msgs
                    '()
                    infra
                    (iteration-snapshot counters ws (hash->session-config (hash)) #f 5 10)))
  (check-true (directive-recurse? out) "continue returns directive-recurse")
  (define new-counters (directive-recurse-new-counters out))
  (check-equal? (loop-counters-iteration new-counters) 1 "iteration incremented on continue")
  (delete-file log-path))

(test-case "interpret-step: 'stop delegates to handle-stop-action"
  (define bus (make-event-bus))
  (define log-path (make-temp-log-path))
  (define infra (loop-infra '() #f #f bus "test-session" log-path #f))
  (define counters (make-initial-counters))
  (define ws (make-working-set))
  (define result (make-loop-result '() 'completed (hasheq)))
  (define step-res (step-result 'stop 'completed (make-initial-counters) (hasheq)))
  (define out
    (interpret-step step-res
                    result
                    '()
                    infra
                    (iteration-snapshot counters ws (hash->session-config (hash)) #f 5 10)))
  (check-true (directive-stop? out))
  (delete-file log-path))

(test-case "interpret-step: 'stop-soft-limit emits warning and recurses"
  (define bus (make-event-bus))
  (define log-path (make-temp-log-path))
  (define infra (loop-infra '() #f #f bus "test-session" log-path #f))
  (define counters (make-initial-counters))
  (define ws (make-working-set))
  (define result (make-loop-result '() 'tool-calls-pending (hasheq)))
  (define events-box (collect-events bus "iteration.soft-warning"))
  (define step-res
    (step-result 'stop-soft-limit 'tool-calls-pending (make-initial-counters) (hasheq)))
  (define out
    (interpret-step step-res
                    result
                    '()
                    infra
                    (iteration-snapshot counters ws (hash->session-config (hash)) #f 5 10)))
  (check-true (directive-recurse? out) "soft-limit returns directive-recurse")
  (check-true (pair? (unbox events-box)) "iteration.soft-warning event was emitted")
  (define new-counters (directive-recurse-new-counters out))
  (check-equal? (loop-counters-iteration new-counters)
                1
                "iteration incremented on soft-limit recurse")
  (delete-file log-path))
