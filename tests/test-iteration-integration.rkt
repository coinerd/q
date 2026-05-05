#lang racket/base

;; tests/test-iteration-integration.rkt — integration tests for struct refactor
;; v0.29.17 W0: Tests for dispatch-loop-action internals via (module+ for-testing)

(require rackunit
         racket/set
         racket/file
         (submod "../runtime/iteration.rkt" for-testing)
         (only-in "../runtime/iteration/loop-state.rkt"
                  make-initial-counters
                  loop-infra
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
  (define ctx (iteration-ctx 3 0 0 5 10))
  (define tok (make-cancellation-token))
  (define result (check-cancellation tok #f #f bus "test-session" 3 ctx))
  (check-false result))

(test-case "check-cancellation: returns loop-result when token cancelled"
  (define bus (make-event-bus))
  (define ctx (iteration-ctx 3 0 0 5 10))
  (define tok (make-cancellation-token))
  (cancel-token! tok)
  (define result (check-cancellation tok #f #f bus "test-session" 3 ctx))
  (check-true (loop-result? result))
  (check-equal? (loop-result-termination-reason result) 'cancelled))

(test-case "check-cancellation: returns loop-result on force-shutdown"
  (define bus (make-event-bus))
  (define ctx (iteration-ctx 3 0 0 5 10))
  (define force-check (lambda () #t))
  (define result (check-cancellation #f force-check #f bus "test-session" 3 ctx))
  (check-true (loop-result? result))
  (check-equal? (loop-result-termination-reason result) 'cancelled))

(test-case "check-cancellation: returns loop-result on graceful shutdown"
  (define bus (make-event-bus))
  (define ctx (iteration-ctx 3 0 0 5 10))
  (define shutdown-check (lambda () #t))
  (define result (check-cancellation #f #f shutdown-check bus "test-session" 3 ctx))
  (check-true (loop-result? result))
  (check-equal? (loop-result-termination-reason result) 'completed))

;; ============================================================
;; dispatch-loop-action tests
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

(test-case "dispatch-loop-action: 'stop-hard-limit emits runtime.error event"
  (define bus (make-event-bus))
  (define log-path (make-temp-log-path))
  (define infra (loop-infra '() #f #f bus "test-session" log-path #f))
  (define counters (make-initial-counters))
  (define ws (make-working-set))
  (define result (make-loop-result '() 'tool-calls-pending (hasheq)))
  (define events-box (collect-events bus "runtime.error"))
  (define max-iter 5)
  (define max-hard 10)
  ;; on-recurse should NOT be called for hard-limit
  (define on-recurse (lambda args (fail "on-recurse should not be called for hard-limit")))
  (define out
    (dispatch-loop-action 'stop-hard-limit
                          result
                          '()
                          infra
                          counters
                          ws
                          (hash)
                          #f
                          max-iter
                          max-hard
                          #f
                          'all
                          on-recurse))
  (check-true (loop-result? out))
  (check-equal? (loop-result-termination-reason out) 'max-iterations-exceeded)
  (check-true (hash-has-key? (loop-result-metadata out) 'maxIterationsReached))
  (check-true (pair? (unbox events-box)) "runtime.error event was emitted")
  (delete-file log-path))

(test-case "dispatch-loop-action: 'continue calls on-recurse with updated counters"
  (define bus (make-event-bus))
  (define log-path (make-temp-log-path))
  (define infra (loop-infra '() #f #f bus "test-session" log-path #f))
  (define counters (make-initial-counters))
  (define ws (make-working-set))
  (define msgs (list (make-msg-with-tool-call "read" (hasheq 'path "/tmp/test.rkt"))))
  (define result (make-loop-result msgs 'tool-calls-pending (hasheq)))
  ;; process-tool-results needs tool-coordinator; use empty msg list
  ;; to avoid calling handle-tool-calls-pending with real tools
  (define result-no-msgs (make-loop-result '() 'tool-calls-pending (hasheq)))
  (define recurse-args-box (box #f))
  (define on-recurse
    (lambda (ctx new-counters ws2)
      (set-box! recurse-args-box (list ctx new-counters ws2))
      (make-loop-result ctx 'completed (hasheq))))
  (define out
    (dispatch-loop-action 'continue
                          result-no-msgs
                          '()
                          infra
                          counters
                          ws
                          (hash)
                          #f
                          5
                          10
                          #f
                          'all
                          on-recurse))
  (check-true (loop-result? out))
  (check-not-false (unbox recurse-args-box) "on-recurse was called")
  (define new-counters (cadr (unbox recurse-args-box)))
  (check-equal? (loop-counters-iteration new-counters) 1 "iteration incremented on continue")
  (delete-file log-path))

(test-case "dispatch-loop-action: 'stop delegates to handle-stop-action"
  (define bus (make-event-bus))
  (define log-path (make-temp-log-path))
  (define infra (loop-infra '() #f #f bus "test-session" log-path #f))
  (define counters (make-initial-counters))
  (define ws (make-working-set))
  (define result (make-loop-result '() 'completed (hasheq)))
  ;; on-recurse used as followup-continuation — should NOT be called for simple completed stop
  (define on-recurse (lambda args (fail "followup should not trigger without followup queue")))
  (define out
    (dispatch-loop-action 'stop result '() infra counters ws (hash) #f 5 10 #f 'all on-recurse))
  (check-true (loop-result? out))
  (delete-file log-path))

(test-case "dispatch-loop-action: 'stop-soft-limit emits warning and recurses"
  (define bus (make-event-bus))
  (define log-path (make-temp-log-path))
  (define infra (loop-infra '() #f #f bus "test-session" log-path #f))
  (define counters (make-initial-counters))
  (define ws (make-working-set))
  (define result (make-loop-result '() 'tool-calls-pending (hasheq)))
  (define events-box (collect-events bus "iteration.soft-warning"))
  (define recurse-args-box (box #f))
  (define on-recurse
    (lambda (ctx new-counters ws2)
      (set-box! recurse-args-box (list ctx new-counters ws2))
      (make-loop-result ctx 'completed (hasheq))))
  (define out
    (dispatch-loop-action 'stop-soft-limit
                          result
                          '()
                          infra
                          counters
                          ws
                          (hash)
                          #f
                          5
                          10
                          #f
                          'all
                          on-recurse))
  (check-true (loop-result? out))
  (check-true (pair? (unbox events-box)) "iteration.soft-warning event was emitted")
  (check-not-false (unbox recurse-args-box) "on-recurse was called for soft-limit")
  (define new-counters (cadr (unbox recurse-args-box)))
  (check-equal? (loop-counters-iteration new-counters)
                1
                "iteration incremented on soft-limit recurse")
  (delete-file log-path))
