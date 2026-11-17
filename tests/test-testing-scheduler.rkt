#lang racket

;; tests/test-testing-scheduler.rkt
;; The ONE work-conserving dynamic scheduler (scripts/testing/scheduler.rkt,
;; final plan §2): one result per selected task in selection order,
;; work-conserving dispatch, the repository-exclusive resource with the
;; mutation-owner-first dispatch order, fail-closed worker death and
;; cancellation, and the single-queue dispatch-latency telemetry.
;;
;; In-process only: exec adapters here are thunks, never subprocesses — the
;; subprocess-facing behavior (worker death through run-suite-once, owned
;; process groups) keeps its own tests (test-run-tests-batch-worker-death.rkt,
;; test-qt-process-ownership.rkt).

;; @speed fast
;; @suite testing
;; @boundary unit

(require rackunit
         racket/list
         "../scripts/testing/scheduler.rkt")

;; ── Helpers ─────────────────────────────────────────────────────────────

(struct task-run (start end) #:transparent)

;; Records start/end timestamps per task id (semaphore-protected: workers run
;; concurrently).
(define (make-recorder)
  (define lock (make-semaphore 1))
  (define table (make-hash))
  (define (record id kind)
    (call-with-semaphore lock
                         (lambda ()
                           (define entry (hash-ref table id (cons #f #f)))
                           (hash-set! table
                                      id
                                      (if (eq? kind 'start)
                                          (cons (current-inexact-milliseconds) (cdr entry))
                                          (cons (car entry) (current-inexact-milliseconds)))))))
  (define (start! id)
    (record id 'start))
  (define (end! id)
    (record id 'end))
  (lambda (op [id #f])
    (case op
      [(start) (start! id)]
      [(end) (end! id)]
      [(table) (hash-copy table)]
      [(intervals)
       (for/list ([id (in-list (sort (hash-keys (hash-copy table)) string<?))])
         (define pair (hash-ref table id))
         (task-run (car pair) (cdr pair)))])))

;; The exec adapter RETURNS the task id, so the one-result-per-task check can
;; compare returned identities against the selection.
(define (task id sleep-s rec)
  (sched-task id
              (lambda (_id _timeout)
                (rec 'start id)
                (sleep sleep-s)
                (rec 'end id)
                id)
              30
              #f
              #f
              0
              0))

;; Overlap check: two open intervals (start < other end) and (other start < end).
(define (overlaps? a b)
  (and (< (task-run-start a) (task-run-end b)) (< (task-run-start b) (task-run-end a))))

;; ── Tests ───────────────────────────────────────────────────────────────

(module+ test
  (test-case "exactly one result per task, in deterministic selection order"
    (define rec (make-recorder))
    ;; The FIRST task finishes last: completion order is the reverse of
    ;; selection order, and the result order must not follow it.
    (define tasks
      (for/list ([id (in-list '("a" "b" "c" "d"))]
                 [s (in-list '(0.2 0.15 0.1 0.05))]
                 [i (in-naturals)])
        (struct-copy sched-task (task id s rec) [order i])))
    (define results (run-scheduled tasks 4))
    (check-equal? (length results) 4 "one result per selected task")
    (check-equal? results '("a" "b" "c" "d") "results follow the order field, not completion order"))

  (test-case "work-conserving: no worker idles while an eligible task is available"
    (define rec (make-recorder))
    (define slow (struct-copy sched-task (task "slow" 0.5 rec) [order 0]))
    (define fast-tasks
      (for/list ([i (in-range 6)])
        (struct-copy sched-task (task (format "fast-~a" i) 0.05 rec) [order (add1 i)])))
    (define results (run-scheduled (cons slow fast-tasks) 2))
    (check-equal? (length results) 7)
    ;; While `slow` occupies its worker (0.5s), the second worker must churn
    ;; through every 0.05s task: all six complete before `slow` ends. A
    ;; fixed-batch barrier would leave the second worker idle waiting for the
    ;; batch boundary instead.
    (define intervals (rec 'intervals))
    (define slow-interval
      (for/first ([r (in-list intervals)]
                  #:when (> (- (task-run-end r) (task-run-start r)) 400))
        r))
    (check-true (task-run? slow-interval) "the slow task is identifiable by its duration")
    (define fast-before-slow-end
      (count (lambda (r)
               (and (not (eq? r slow-interval)) (< (task-run-end r) (task-run-end slow-interval))))
             intervals))
    (check-equal? fast-before-slow-end
                  6
                  "all eligible fast tasks ran while the slow one held a worker"))

  (test-case "repository-exclusive: exclusive tasks never overlap"
    (define rec (make-recorder))
    (define tasks
      (for/list ([id (in-list '("ex-1" "ex-2" "ex-3"))]
                 [i (in-naturals)])
        (struct-copy sched-task (task id 0.08 rec) [exclusive? #t] [priority 0] [order i])))
    (run-scheduled tasks 3)
    (define intervals (rec 'intervals))
    (check-equal? (length intervals) 3)
    (check-false (overlaps? (first intervals) (second intervals)) "ex-1 and ex-2 must not interleave")
    (check-false (overlaps? (second intervals) (third intervals))
                 "ex-2 and ex-3 must not interleave"))

  (test-case "dispatch order: owners first, readers wait, co-run isolation joins early"
    (define rec (make-recorder))
    ;; owner sleeps long enough that a co-run task can complete inside it;
    ;; the plain reader must NOT start until the owner has finished.
    (define owner
      (struct-copy sched-task (task "owner" 0.35 rec) [exclusive? #t] [priority 0] [order 0]))
    (define co-run
      (struct-copy sched-task (task "co-run" 0.05 rec) [co-runs? #t] [priority 1] [order 1]))
    (define reader (struct-copy sched-task (task "reader" 0.05 rec) [priority 1] [order 2]))
    (run-scheduled (list owner co-run reader) 3)
    (define table (rec 'table))
    (define owner-start (car (hash-ref table "owner")))
    (define owner-end (cdr (hash-ref table "owner")))
    (define corun-start (car (hash-ref table "co-run")))
    (define reader-start (car (hash-ref table "reader")))
    (check-true (< corun-start owner-end)
                "co-run isolation may execute while the owner holds the lock")
    (check-true (> reader-start owner-end)
                "a reader must not start while the mutation set is outstanding"))

  (test-case "co-run task cannot start before an exclusive owner when jobs are scarce"
    (define rec (make-recorder))
    ;; jobs=1: the owner IS the only worker's first pick (priority 0); the
    ;; co-run task follows, never preempting the lock owner.
    (define owner
      (struct-copy sched-task (task "owner" 0.15 rec) [exclusive? #t] [priority 0] [order 0]))
    (define co-run
      (struct-copy sched-task (task "co-run" 0.05 rec) [co-runs? #t] [priority 1] [order 1]))
    (run-scheduled (list co-run owner) 1)
    (define table (rec 'table))
    (check-true (> (car (hash-ref table "co-run")) (cdr (hash-ref table "owner")))
                "with one worker the owner dispatches before the co-run task"))

  (test-case "the co-run cap restores barrier semantics at jobs=2"
    (define rec (make-recorder))
    ;; Measured on CI: an unbounded co-run oversubscribes 2-worker lanes
    ;; (security lane 272s -> 400s). With jobs=2 the cap is 0: while an owner
    ;; holds the lock, NO co-run task starts — barrier semantics, never
    ;; slower than the barrier (§2 acceptance).
    (define owner (struct-copy sched-task (task "owner" 0.25 rec) [exclusive? #t] [priority 0] [order 0]))
    (define co-run (struct-copy sched-task (task "co-run" 0.05 rec) [co-runs? #t] [priority 1] [order 1]))
    (run-scheduled (list co-run owner) 2)
    (define table (rec 'table))
    (check-true (> (car (hash-ref table "co-run")) (cdr (hash-ref table "owner")))
                "at jobs=2 no co-run may start while the owner holds the lock"))

  (test-case "worker death without a result fails the whole run (fail closed)"
    (define dying
      (sched-task "dying" (lambda (_id _timeout) (error 'exec "simulated crash")) 30 #f #f 0 0))
    (define healthy (sched-task "healthy" (lambda (_id _timeout) 'ok) 30 #f #f 1 1))
    (check-exn #rx"worker terminated without a result"
               (lambda () (run-scheduled (list dying healthy) 2))))

  (test-case "duplicate identities and out-of-contract arguments fail closed"
    (define ok (sched-task "x" (lambda (_id _timeout) 1) 30 #f #f 0 0))
    (define ok-again (sched-task "x" (lambda (_id _timeout) 2) 30 #f #f 1 1))
    (check-exn #rx"duplicate task identity" (lambda () (run-scheduled (list ok ok-again) 1)))
    (check-exn #rx"jobs must be a positive integer" (lambda () (run-scheduled (list ok) 0)))
    (check-exn #rx"tasks must be a list of sched-task values"
               (lambda () (run-scheduled (list "not-a-task") 1))))

  (test-case "empty input returns an empty result list"
    (check-equal? (run-scheduled '() 2) '()))

  (test-case "cancellation ends the run without a partial result set"
    ;; The coordinator runs INSIDE the custodian; shutdown kills it and its
    ;; workers. The run must END (never hang) and publish nothing — a killed
    ;; run can never be read as success.
    (define done-chan (make-channel))
    (define cust (make-custodian))
    (define coord
      (parameterize ([current-custodian cust])
        (thread (lambda ()
                  (define r
                    (run-scheduled (list (sched-task "long"
                                                     (lambda (_id _timeout)
                                                       (sleep 5)
                                                       'ok)
                                                     60
                                                     #f
                                                     #f
                                                     0
                                                     0))
                                   1))
                  (channel-put done-chan r)))))
    (sleep 0.1)
    (custodian-shutdown-all cust)
    (check-true (and (sync/timeout 5 (thread-dead-evt coord)) #t)
                "the cancelled run must end, never hang")
    (check-false (sync/timeout 0.1 done-chan) "a cancelled run must not publish results"))

  (test-case "late-ms-box receives the single-queue dispatch latency"
    (define late (box #f))
    (run-scheduled (list (sched-task "t"
                                     (lambda (_id _timeout)
                                       (sleep 0.05)
                                       'ok)
                                     30
                                     #f
                                     #f
                                     0
                                     0))
                   1
                   #:late-ms-box late)
    (check-true (exact-positive-integer? (unbox late)) "dispatch latency is recorded in ms")
    (check-true (< (unbox late) 3000) "dispatch latency stays far below the task's own runtime")))
