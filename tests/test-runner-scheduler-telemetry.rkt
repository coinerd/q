#lang racket/base

;; @speed fast
;; @suite testing
;; @isolation process
;; @boundary integration  ;; @mutates fs

;; W1: unit + JSON-schema coverage for the additive scheduler telemetry
;; (partition formulas, per-file execution-mode counts, versioned scheduler
;; object, distinct serial/parallel first-batch fields).

(require rackunit
         rackunit/text-ui
         json
         racket/file
         racket/port
         racket/runtime-path
         racket/string
         "../scripts/run-tests/runner.rkt"
         "../scripts/run-tests/reporting.rkt"
         "../scripts/run-tests/parse.rkt")

(define-runtime-path here ".")
(define project-root (simplify-path (build-path here "..")))

(define (mk-result path #:exit [exit 0] #:elapsed [elapsed 5] #:total [total 1])
  (make-test-file-result path exit #"" #"" elapsed 0 0 total))

;; ---------------------------------------------------------------------------
;; partition-scheduler-fields formulas (units: ms; see docs/TEST_CONVENTIONS.md
;; "Scheduler telemetry" for the aggregation formulas)
;; ---------------------------------------------------------------------------

(define (tel-batches batches #:gc-count [gc 0] #:gc-ms [ms 0])
  (hasheq 'batches batches 'gc_count gc 'gc_pause_ms ms))

(define suite
  (test-suite "scheduler telemetry"

    (test-case "serial partition: single batch, worker=1"
      (define f (partition-scheduler-fields (list (cons 1 30)) 1))
      (check-equal? (hash-ref f 'queue_wait_ms) 0)
      (check-equal? (hash-ref f 'worker_busy_ms) 30)
      (check-equal? (hash-ref f 'worker_idle_ms) 0)
      (check-equal? (hash-ref f 'partition_ms) 30))

    (test-case "parallel partition: queue-wait/busy/idle aggregation (2 workers)"
      ;; batches ((2 files . 100ms) (1 file . 50ms)); worker_count = 2.
      ;; offsets: 0, 100 -> queue_wait = 0*2 + 100*1 = 100
      ;; busy      = round(100*min(1,2/2)) + round(50*min(1,1/2)) = 100 + 25 = 125
      ;; partition = 150; idle = 150*2 - 125 = 175
      (define f (partition-scheduler-fields (list (cons 2 100) (cons 1 50)) 2))
      (check-equal? (hash-ref f 'queue_wait_ms) 100)
      (check-equal? (hash-ref f 'worker_busy_ms) 125)
      (check-equal? (hash-ref f 'worker_idle_ms) 175)
      (check-equal? (hash-ref f 'partition_ms) 150))

    (test-case "serial partition fields use worker_count=1 regardless of jobs"
      (define f (partition-scheduler-fields (list (cons 4 80)) 1))
      (check-equal? (hash-ref f 'queue_wait_ms) 0)
      (check-equal? (hash-ref f 'worker_busy_ms) 80)
      (check-equal? (hash-ref f 'worker_idle_ms) 0)
      (check-equal? (hash-ref f 'partition_ms) 80))

    (test-case "empty batches -> no fields (callers default to 0)"
      (check-false (partition-scheduler-fields '() 2)))

    (test-case "telemetry merge: serial+parallel partitions stay distinct"
      (define tel
        (compute-scheduler-telemetry
         (tel-batches (list (cons 2 100) (cons 1 50)) #:gc-count 3 #:gc-ms 40)
         (tel-batches (list (cons 2 60)) #:gc-count 2 #:gc-ms 60)
         4
         '()))
      (check-equal? (hash-ref tel 'scheduler_mode) "batch")
      (check-equal? (hash-ref tel 'worker_count) 4)
      ;; serial: batches (2 . 100) (1 . 50), workers 1 ->
      ;;         queue 100, busy 150, idle 0, partition 150
      ;; parallel: batch (2 . 60), workers 4 ->
      ;;         queue 0, busy round(60*min(1,2/4))=30, idle 240-30=210, partition 60
      (check-equal? (hash-ref tel 'queue_wait_ms) 100)
      (check-equal? (hash-ref tel 'worker_busy_ms) 180)
      (check-equal? (hash-ref tel 'worker_idle_ms) 210)
      (check-equal? (hash-ref tel 'serial_partition_ms) 150)
      (check-equal? (hash-ref tel 'parallel_partition_ms) 60)
      (check-equal? (hash-ref tel 'gc_count) 5)
      (check-equal? (hash-ref tel 'gc_pause_ms) 100))

    (test-case "telemetry: missing optional telemetry defaults to zeros"
      (define tel (compute-scheduler-telemetry (hasheq) (hasheq) 2 '()))
      (check-equal? (hash-ref tel 'scheduler_mode) "batch")
      (check-equal? (hash-ref tel 'worker_count) 2)
      (check-equal? (hash-ref tel 'queue_wait_ms) 0)
      (check-equal? (hash-ref tel 'worker_busy_ms) 0)
      (check-equal? (hash-ref tel 'worker_idle_ms) 0)
      (check-equal? (hash-ref tel 'serial_partition_ms) 0)
      (check-equal? (hash-ref tel 'parallel_partition_ms) 0)
      (check-equal? (hash-ref tel 'gc_count) 0)
      (check-equal? (hash-ref tel 'gc_pause_ms) 0)
      (check-equal? (hash-ref tel 'process_start_count) 0))

    (test-case "per-file execution-mode counts reflect ACTUAL modes"
      (define p1 (build-path project-root "tmp/w1-sched-a.rkt"))
      (define p2 (build-path project-root "tmp/w1-sched-b.rkt"))
      (mark-execution-mode! p1 'grouped-in-process)
      (mark-execution-mode! p2 'subprocess)
      (define tel
        (compute-scheduler-telemetry (hasheq) (hasheq) 2 (list (mk-result p1) (mk-result p2))))
      (check-equal? (hash-ref tel 'grouped_in_process_count) 1)
      (check-equal? (hash-ref tel 'subprocess_count) 1)
      (check-equal? (hash-ref tel 'process_start_count) 1))

    (test-case "run-summary JSON carries versioned scheduler object (new schema)"
      (define out (make-temporary-file "w1-sched-json-~a.json" #f))
      (define p1 (build-path project-root "tmp/w1-json-a.rkt"))
      (define p2 (build-path project-root "tmp/w1-json-b.rkt"))
      (mark-execution-mode! p1 'subprocess)
      (mark-execution-mode! p2 'grouped-in-process)
      (define tel
        (compute-scheduler-telemetry (tel-batches (list (cons 2 100)) #:gc-count 3 #:gc-ms 40)
                                     (tel-batches (list (cons 1 60)) #:gc-count 2 #:gc-ms 60)
                                     2
                                     (list (mk-result p1) (mk-result p2))))
      (write-json-results!
       out
       (list (mk-result p1) (mk-result p2))
       #:suite 'testing
       #:mode 'subprocess
       #:elapsed-ms 500
       #:ledger #f
       #:profile 'local
       #:shard #f
       #:runner-version "1.0.23-test"
       #:extra (let ([m (make-hasheq)])
                 (hash-set! m 'serial_first_batch_ms 100)
                 (hash-set! m 'parallel_first_batch_ms 60)
                 (hash-set! m 'first_batch_ms 100)
                 (hash-set! m 'scheduler tel)
                 (hash-set! m
                            'prepared_environment
                            (hasheq 'result "unavailable" 'restore_ms 'null 'fallback_ms 'null))
                 m))
      (define js (call-with-input-file out read-json))
      (check-true (hash? js) "decoded run-summary is a hash")
      (check-equal? (hash-ref js 'suite) "testing")
      (define extra (hash-ref js 'extra))
      (define scheduler (hash-ref extra 'scheduler))
      (check-equal? (hash-ref scheduler 'schema_version) 1)
      (check-equal? (hash-ref scheduler 'scheduler_mode) "batch")
      (check-equal? (hash-ref scheduler 'worker_count) 2)
      (check-equal? (hash-ref scheduler 'queue_wait_ms) 0)
      ;; serial (2 files . 100ms, w=1): busy 100 idle 0; parallel (1 . 60ms, w=2): busy 30 idle 90
      (check-equal? (hash-ref scheduler 'worker_busy_ms) 130)
      (check-equal? (hash-ref scheduler 'worker_idle_ms) 90)
      (check-equal? (hash-ref scheduler 'serial_partition_ms) 100)
      (check-equal? (hash-ref scheduler 'parallel_partition_ms) 60)
      (check-equal? (hash-ref scheduler 'process_start_count) 1)
      (check-equal? (hash-ref scheduler 'subprocess_count) 1)
      (check-equal? (hash-ref scheduler 'grouped_in_process_count) 1)
      (check-equal? (hash-ref scheduler 'gc_count) 5)
      (check-equal? (hash-ref scheduler 'gc_pause_ms) 100)
      ;; distinct first-batch fields, legacy field preserved
      (check-equal? (hash-ref extra 'serial_first_batch_ms) 100)
      (check-equal? (hash-ref extra 'parallel_first_batch_ms) 60)
      (check-equal? (hash-ref extra 'first_batch_ms) 100)
      (define pe (hash-ref extra 'prepared_environment))
      (check-equal? (hash-ref pe 'result) "unavailable")
      (delete-file out))))

(run-tests suite)
