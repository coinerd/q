#lang racket/base

;; @speed fast
;; @boundary unit
;; @area tools
;; @isolation process

(require rackunit
         json
         racket/list
         racket/path
         racket/file
         racket/runtime-path
         (only-in "../scripts/run-tests/runner.rkt"
                  run-all-files
                  make-run-all-telemetry
                  compute-scheduler-telemetry)
         (only-in "helpers/temp-fs.rkt" with-temp-dir))

(define-runtime-path this-file "test-run-tests-scheduler-telemetry-truth.rkt")

(module+ test
  (test-case "queue telemetry does not invent concurrent timings"
    (define serial (make-run-all-telemetry))
    (define parallel (make-run-all-telemetry))
    (hash-set! parallel 'batches '((1 . 100) (1 . 100)))
    (define telemetry (compute-scheduler-telemetry serial parallel 2 '() #:scheduler 'queue))
    (for ([key
           '(queue_wait_ms worker_busy_ms worker_idle_ms serial_partition_ms parallel_partition_ms)])
      (check-equal? (hash-ref telemetry key) 'null))
    (check-not-exn (lambda () (jsexpr->string telemetry))))
  (test-case "GC counts actual scheduled collections at five and six completions"
    (with-temp-dir
     (dir)
     (define fixture (build-path dir "fixture.rkt"))
     (call-with-output-file fixture
                            #:exists 'truncate
                            (lambda (out) (display "#lang racket/base\n" out)))
     (for* ([scheduler '(batch queue)]
            [n '(0 5 6)])
       (define telemetry (make-run-all-telemetry))
       (parameterize ([current-output-port (open-output-string)])
         (run-all-files (make-list n fixture) 1 #f #:scheduler scheduler #:telemetry telemetry))
       (check-equal? (hash-ref telemetry 'gc_count)
                     (case n
                       [(0) 0]
                       [(5) 1]
                       [(6) 2])
                     (format "~a ~a completions" scheduler n))))))

(module+ main
  (dynamic-require (make-resolved-module-path (cons (simple-form-path this-file) '(test))) #f))
