#lang racket

;; @speed fast  ;; @suite extensions

;; tests/test-blackboard.rkt — W1 (v0.99.7) Blackboard State Tests
;;
;; Tests for the thread-safe blackboard container:
;; - Empty blackboard defaults
;; - read/update/reset lifecycle
;; - Thread safety (concurrent updates serialized)
;; - Struct field accessors and predicates

(require rackunit
         rackunit/text-ui
         racket/list
         "../agent/blackboard.rkt")

(define suite
  (test-suite "Blackboard State (W1 v0.99.7)"

    ;; ── Empty Blackboard Defaults ──

    (test-case "empty-blackboard: active-plan is #f"
      (check-false (blackboard-state-active-plan empty-blackboard)))

    (test-case "empty-blackboard: all list fields are empty"
      (for ([lst (list (blackboard-state-open-hypotheses empty-blackboard)
                       (blackboard-state-test-results empty-blackboard)
                       (blackboard-state-artifact-refs empty-blackboard)
                       (blackboard-state-verifier-decisions empty-blackboard)
                       (blackboard-state-agent-activities empty-blackboard))])
        (check-equal? lst '())))

    (test-case "empty-blackboard: wave-status is empty hash"
      (check-equal? (hash-count (blackboard-state-wave-status empty-blackboard)) 0))

    (test-case "empty-blackboard: last-updated is 0"
      (check-equal? (blackboard-state-last-updated empty-blackboard) 0))

    (test-case "empty-blackboard: blackboard-state? predicate"
      (check-true (blackboard-state? empty-blackboard)))

    ;; ── Container Creation ──

    (test-case "make-blackboard: returns blackboard-container?"
      (define bb (make-blackboard))
      (check-true (blackboard-container? bb)))

    (test-case "read-blackboard: returns empty-blackboard initially"
      (define bb (make-blackboard))
      (define state (read-blackboard bb))
      (check-true (blackboard-state? state))
      (check-false (blackboard-state-active-plan state)))

    ;; ── Update Lifecycle ──

    (test-case "update-blackboard!: reducer applied atomically"
      (define bb (make-blackboard))
      (define new-state
        (update-blackboard!
         (lambda (s) (struct-copy blackboard-state s [active-plan (hasheq 'summary "test plan")]))
         bb))
      (check-equal? (hash-ref (blackboard-state-active-plan new-state) 'summary) "test plan")
      ;; Verify persisted
      (define read-state (read-blackboard bb))
      (check-equal? (hash-ref (blackboard-state-active-plan read-state) 'summary) "test plan"))

    (test-case "update-blackboard!: multiple sequential updates accumulate"
      (define bb (make-blackboard))
      (update-blackboard!
       (lambda (s)
         (struct-copy blackboard-state s [test-results (list (hasheq 'file "a.rkt" 'result 'pass))]))
       bb)
      (update-blackboard! (lambda (s)
                            (struct-copy blackboard-state
                                         s
                                         [test-results
                                          (append (blackboard-state-test-results s)
                                                  (list (hasheq 'file "b.rkt" 'result 'fail)))]))
                          bb)
      (define state (read-blackboard bb))
      (check-equal? (length (blackboard-state-test-results state)) 2))

    ;; ── Reset ──

    (test-case "reset-blackboard!: restores to empty state"
      (define bb (make-blackboard))
      (update-blackboard! (lambda (s)
                            (struct-copy blackboard-state
                                         s
                                         [active-plan (hasheq 'summary "temp")]
                                         [artifact-refs (list "foo")]))
                          bb)
      (reset-blackboard! bb)
      (define state (read-blackboard bb))
      (check-false (blackboard-state-active-plan state))
      (check-equal? (blackboard-state-artifact-refs state) '()))

    ;; ── No Blackboard Set ──

    (test-case "read-blackboard: returns #f when no blackboard in parameter"
      (parameterize ([current-blackboard #f])
        (check-false (read-blackboard))))

    (test-case "update-blackboard!: returns empty-blackboard when no container"
      (parameterize ([current-blackboard #f])
        (define result
          (update-blackboard! (lambda (s) (struct-copy blackboard-state s [active-plan "x"]))))
        (check-true (blackboard-state? result))
        (check-false (blackboard-state-active-plan result))))

    ;; ── Thread Safety ──

    (test-case "thread-safety: concurrent updates are serialized"
      (define bb (make-blackboard))
      (define num-threads 20)
      (define num-iters 50)
      (define barrier (make-semaphore 0))
      ;; Launch threads
      (for ([_ (in-range num-threads)])
        (thread (lambda ()
                  (semaphore-wait barrier) ; wait for all threads to be ready
                  (semaphore-post barrier)
                  (for ([_ (in-range num-iters)])
                    (update-blackboard! (lambda (s)
                                          (struct-copy blackboard-state
                                                       s
                                                       [test-results
                                                        (append (blackboard-state-test-results s)
                                                                (list (hasheq 'file "concurrent")))]))
                                        bb)))))
      ;; Release barrier
      (semaphore-post barrier)
      ;; Wait briefly for threads to finish
      (sleep 2)
      (define state (read-blackboard bb))
      ;; Each thread does num-iters appends. Total = num-threads * num-iters.
      ;; With proper serialization, no lost updates.
      (check-equal? (length (blackboard-state-test-results state)) (* num-threads num-iters)))))

(run-tests suite)
