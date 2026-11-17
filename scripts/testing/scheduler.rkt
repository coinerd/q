#lang racket/base

;; scripts/testing/scheduler.rkt — THE ONE work-conserving dynamic scheduler
;; (final plan §2, rev 3).
;;
;; Every execution path (`./qt quick`, `./qt full/default/release` through
;; full-plan.rkt's run-files, and scripts/run-tests/runner.rkt's run-all-files)
;; schedules through this module; only the per-test execution adapters differ,
;; because their evidence formats differ (audited `quick-result` re-checks on
;; one side, `run-suite-once` surface guard/gate evidence on the other).
;;
;; The module requires ONLY racket/base and sits below both callers
;; (`run-tests/runner.rkt` requires `../testing/runner-cli.rkt`; full-plan.rkt
;; dynamically requires the runner), so a require cycle is impossible.
;;
;; Contract (final plan §2 "The scheduler must"):
;;   1. start up to `jobs` eligible tasks concurrently;
;;   2. dispatch another eligible task as soon as a worker finishes
;;      (work-conserving: no worker idles while an eligible,
;;      resource-unblocked task is available);
;;   3. never block the whole queue behind a resource-constrained task
;;      (a resource-blocked head-of-line task is skipped in favor of eligible
;;      later tasks);
;;   4. enforce the `repository-exclusive` resource: exclusive tasks run one at
;;      a time, and a non-co-running task is eligible only while NO exclusive
;;      task is incomplete (running or still queued ahead of it). Exclusive
;;      tasks sort first, so the lock is acquired once at the start of the run
;;      and released when the mutation set drains (rev 3 dispatch order);
;;   5. keep every test in a separate OS process — the scheduler only schedules;
;;      each `exec` adapter owns its subprocess;
;;   6. preserve process-group cleanup, cancellation and timeout behavior —
;;      workers inherit the caller's custodian and subprocess parameters, and
;;      each task's `timeout` is passed through to its adapter, which enforces
;;      it;
;;   7. detect unexpected worker termination (a worker that dies without
;;      publishing its result fails the whole run — never a silent shortfall);
;;   8. return exactly one result per selected task;
;;   9. present results in deterministic selection order (the `order` field),
;;      independent of completion order.
;;
;; Resource model (rev 3 §2): no capability registry. `exclusive?` is derived
;; by the CALLER from metadata that already exists (`mutating-file?` / a
;; declared `@mutates` other than `none`; unknown or ambiguous mutation
;; requirements must acquire the lock — conservative default). `co-runs?` marks
;; the only tasks that may execute while the lock is held: tests whose declared
;; `@isolation` is `temp-dir` or `sandboxed-home` run outside the checkout and
;; cannot observe a managed-surface mutation. There is deliberately no
;; reader-side lock: the managed-surface guard is one before/after snapshot and
;; cannot attribute a mutation, so the sound cheap rule is *mutation excludes
;; observation*.
;;
;; Failure semantics are fail-closed: a worker that raises, dies, or is
;; cancelled before publishing its result aborts the whole scheduling run with
;; an error. Partial result sets are never returned.

(require racket/list
         racket/math)

(provide (struct-out sched-task)
         run-scheduled)

;; One planned test:
;;   id         — stable test identity (unique within one run; opaque string)
;;   exec       — (-> id timeout result): runs in a worker thread and must
;;                RETURN its result. Raising, dying, or being cancelled without
;;                returning is unexpected worker termination and fails the run.
;;   timeout    — wall-clock bound in seconds, owned by the adapter
;;   exclusive? — #t: owns the `repository-exclusive` resource
;;   co-runs?   — #t: eligible while `repository-exclusive` is held
;;   priority   — lower dispatches first (mutation owners use 0, the rest 1)
;;   order      — original selection order; the result order
(struct sched-task (id exec timeout exclusive? co-runs? priority order) #:transparent)

(define (task<? a b)
  (or (< (sched-task-priority a) (sched-task-priority b))
      (and (= (sched-task-priority a) (sched-task-priority b))
           (< (sched-task-order a) (sched-task-order b)))))

(define (check-tasks! tasks jobs)
  (unless (and (exact-integer? jobs) (positive? jobs))
    (error 'run-scheduled "jobs must be a positive integer, got ~s" jobs))
  (unless (and (list? tasks) (andmap sched-task? tasks))
    (error 'run-scheduled "tasks must be a list of sched-task values, got ~s" tasks))
  ;; Adjacent-equal scan over the sorted ids: duplicate identities are a
  ;; caller bug, never a run (no racket/list dependency needed for this).
  (define ids (sort (map sched-task-id tasks) string<?))
  (define dup
    (and (> (length ids) 1)
         (for/first ([a (in-list ids)]
                     [b (in-list (cdr ids))]
                     #:when (equal? a b))
           a)))
  (when dup
    (error 'run-scheduled "duplicate task identity is not supported: ~s" dup))
  (for ([t (in-list tasks)])
    (unless (string? (sched-task-id t))
      (error 'run-scheduled "task id must be a string, got ~s" (sched-task-id t)))
    (unless (procedure? (sched-task-exec t))
      (error 'run-scheduled "task exec must be a procedure, task ~s" (sched-task-id t)))
    (unless (exact-positive-integer? (sched-task-timeout t))
      (error 'run-scheduled "task timeout must be a positive integer, task ~s" (sched-task-id t)))
    (unless (exact-integer? (sched-task-priority t))
      (error 'run-scheduled "task priority must be an integer, task ~s" (sched-task-id t)))
    (unless (exact-integer? (sched-task-order t))
      (error 'run-scheduled "task order must be an integer, task ~s" (sched-task-id t)))))

;; Run every task once, with at most `jobs` workers, honoring the
;; repository-exclusive resource, and return exactly one result per task in
;; selection order. Raises on: an out-of-contract argument, duplicate ids, a
;; worker that dies/raises without publishing, or cancellation.
;; `#:late-ms-box` receives the dispatch latency (run start → first completion,
;; in ms) when given — the single-queue redefinition of the legacy
;; `first_batch_ms` telemetry (rev 3 §2 "Keep (do not silently drop)").
(define (run-scheduled tasks jobs #:late-ms-box [late-ms-box #f])
  (check-tasks! tasks jobs)
  (cond
    [(null? tasks) '()]
    [else
     (define sorted (sort tasks task<?))
     (define n (length sorted))
     (define max-workers (min jobs n))
     (define start-ms (current-inexact-milliseconds))
     ;; Synchronous completion channel: a worker's `channel-put` returns only
     ;; when the value is retrievable, so a worker that dies has ALWAYS either
     ;; published (visible to the drain below) or genuinely lost its result —
     ;; an async-channel put would race the worker's own death detection.
     (define done (make-channel))
     (define pending (box sorted)) ; tasks not yet dispatched, in dispatch order
     (define results (make-hash)) ; id -> result
     (define running (make-hash)) ; id -> worker thread
     (define task-by-id
       (for/hash ([t (in-list sorted)])
         (values (sched-task-id t) t)))
     (define exclusive-outstanding (count sched-task-exclusive? sorted))
     (define exclusive-running? (box #f))
     (define late-recorded? (box (not late-ms-box)))

     (define (record-late!)
       (unless (unbox late-recorded?)
         (set-box! late-recorded? #t)
         (when late-ms-box
           (set-box! late-ms-box (exact-round (- (current-inexact-milliseconds) start-ms))))))

     ;; Eligibility: exclusive owners need a free lock; every other task waits
     ;; until the exclusive set has drained — EXCEPT co-run tasks (declared
     ;; temp-dir/sandboxed-home isolation), which may co-run while the lock is
     ;; held, bounded to HALF of the non-owner headroom. Measured on CI (final
     ;; plan §2 acceptance "no slower than the barrier"): on 2-worker lanes an
     ;; unbounded co-run oversubscribes the CPU and stretched owner+reader
     ;; durations by 25-45% (security lane 272s -> 400s); the cap restores
     ;; barrier semantics at jobs=2 and keeps partial work recovery at jobs>=4.
     (define co-run-cap (quotient (sub1 max-workers) 2))
     (define co-runs-running 0)
     (define (eligible? t)
       (cond
         [(sched-task-exclusive? t) (not (unbox exclusive-running?))]
         [(and (sched-task-co-runs? t) (positive? exclusive-outstanding))
          (< co-runs-running co-run-cap)]
         [(sched-task-co-runs? t) #t]
         [else (zero? exclusive-outstanding)]))

     ;; First eligible pending task, preserving dispatch order. A blocked head
     ;; never idles an eligible later task (§2 acceptance).
     (define (take-eligible!)
       (let scan ([rest (unbox pending)]
                  [seen '()])
         (cond
           [(null? rest) #f]
           [(eligible? (car rest))
            (set-box! pending (append (reverse seen) (cdr rest)))
            (car rest)]
           [else (scan (cdr rest) (cons (car rest) seen))])))

     (define (spawn! t)
       (define worker
         (thread (lambda ()
                   ;; exec MUST return. A raise escapes and the worker dies without
                   ;; publishing, which the coordinator reports as unexpected worker
                   ;; termination (fail closed — never a synthesized pass).
                   (channel-put done
                                (cons (sched-task-id t)
                                      ((sched-task-exec t) (sched-task-id t)
                                                           (sched-task-timeout t)))))))
       (hash-set! running (sched-task-id t) worker)
       (when (sched-task-exclusive? t)
         (set-box! exclusive-running? #t))
       (when (sched-task-co-runs? t)
         (set! co-runs-running (add1 co-runs-running))))

     (define (task-done! id)
       (hash-remove! running id)
       (define t (hash-ref task-by-id id))
       (when (sched-task-exclusive? t)
         (set! exclusive-outstanding (sub1 exclusive-outstanding))
         (set-box! exclusive-running? #f))
       (when (sched-task-co-runs? t)
         (set! co-runs-running (sub1 co-runs-running))))

     ;; Wait for the next event: a completion or an unexpected worker death.
     ;; The dead-evt may win a race against the completion put, so completions
     ;; are drained (below) before any death is reported.
     (define (wait-event!)
       (define live (hash-values running))
       (if (null? live)
           (sync (handle-evt done (lambda (_) 'completion)))
           (apply choice-evt
                  (handle-evt done (lambda (_) 'completion))
                  (for/list ([w (in-list live)])
                    (handle-evt (thread-dead-evt w) (lambda (_) 'death))))))

     (let loop ()
       (cond
         ;; Every selected task has exactly one published result: join the
         ;; workers (their results are already in) and fall through to the
         ;; selection-ordered return.
         [(= (hash-count results) n)
          (for ([w (in-list (hash-values running))])
            (thread-wait w))
          ;; Exactly one result per selected identity, in selection order.
          (for/list ([t (in-list (sort sorted < #:key sched-task-order))])
            (hash-ref results
                      (sched-task-id t)
                      (lambda () (error 'run-scheduled "missing result for ~s" (sched-task-id t)))))]
         [else
          ;; Dispatch: work-conserving, in dispatch order (mutation owners
          ;; first; then co-runs while the lock is held; then the rest).
          (let dispatch ()
            (define t (and (< (hash-count running) max-workers) (take-eligible!)))
            (when t
              (spawn! t)
              (dispatch)))
          (cond
            [(= (hash-count results) n) (loop)]
            [(and (zero? (hash-count running)) (pair? (unbox pending)))
             ;; Nothing running, nothing dispatchable, work outstanding: the
             ;; only way here is an internal eligibility bug. Fail closed.
             (error 'run-scheduled
                    "scheduling deadlock: ~a result(s) missing and no eligible task"
                    (- n (hash-count results)))]
            [else
             (wait-event!)
             ;; Drain every completion that has already arrived.
             (let drain ()
               (define v (sync/timeout 0 done))
               (when v
                 (hash-set! results (car v) (cdr v))
                 (task-done! (car v))
                 (record-late!)
                 (drain)))
             ;; A worker that died without publishing fails the run.
             (for ([id (in-list (hash-keys running))])
               (define w (hash-ref running id))
               (when (thread-dead? w)
                 (error
                  'run-scheduled
                  (string-append "worker terminated without a result for ~s "
                                 "(~a result(s) missing, ~a of ~a inputs reported; fail closed)")
                  id
                  (- n (hash-count results))
                  (hash-count results)
                  n)))
             (loop)])]))]))
