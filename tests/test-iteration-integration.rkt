#lang racket/base

;; tests/test-iteration-integration.rkt — integration tests for struct refactor
;; v0.29.17 W0: Tests for dispatch-loop-action internals via (module+ for-testing)

(require rackunit
         racket/set
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
                  make-loop-result)
         (only-in "../util/ids.rkt" generate-id)
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
