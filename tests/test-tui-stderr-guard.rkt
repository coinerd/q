#lang racket

;; @speed fast  ;; @suite default
;; @boundary unit

;; tests/test-tui-stderr-guard.rkt — BUG-0072: TUI owns its error-level log
;; stream. The guard must (1) swap current-logger to an unparented logger so
;; the default stderr pump never sees runtime error records, (2) surface
;; records to an optional callback + spill file, (3) restore cleanly.

(require rackunit
         rackunit/text-ui
         (only-in "../tui/stderr-guard.rkt"
                  install-stderr-guard!
                  restore-stderr-guard!
                  stderr-guard-active?
                  stderr-guard-spill-path
                  stderr-guard-spill-port))

(define captured (box '()))
(define orig-logger (current-logger))

(define guard-tests
  (test-suite "TUI stderr guard (BUG-0072)"

    (test-case "install swaps current-logger to an unparented guard logger"
      ;; BUG-0098/R98: the callback receives (message captured-spill-path);
      ;; records land in the shared top-level captured box.
      (define spill
        (install-stderr-guard!
         #:on-error-record (lambda (msg captured-path)
                             (set-box! captured
                                       (append (unbox captured)
                                               (list (list msg captured-path)))))))
      (check-true (string? (path->string spill)) "install returns a spill path")
      (check-true (stderr-guard-active?))
      (check-equal? (path->string (stderr-guard-spill-path)) (path->string spill))
      (define new-logger (current-logger))
      (check-false (equal? new-logger orig-logger) "logger must be swapped"))

    (test-case "bare log-error reaches the on-error-record callback, not the original pump"
      ;; A receiver on the ORIGINAL logger plays the role of the default
      ;; stderr pump in this test: it must see NOTHING after the swap.
      (define pump-saw (box #f))
      (define pump-recv (make-log-receiver orig-logger 'error))
      (define pump-thread
        (thread (lambda ()
                  (define v (sync/timeout 0.2 pump-recv))
                  (when v
                    (set-box! pump-saw #t)))))
      (set-box! captured '())
      (log-error "bug0072-test-record")
      (sleep 0.15) ; let the guard thread drain
      (define joined (string-join (map ~a (unbox captured)) "\n"))
      (check-true (string-contains? joined "bug0072-test-record")
                  "on-error-record callback must receive the record")
      ;; BUG-0098/R98: the second argument is the guard's REAL spill path —
      ;; proof that this record was captured before surfacing.
      (for ([rec (in-list (unbox captured))])
        (check-equal? (second rec) (stderr-guard-spill-path)
                      "callback must receive the real spill path for a captured record"))
      (sync pump-thread)
      (check-false (unbox pump-saw)
                   "original-logger receiver (pump stand-in) must not receive the record")
      (check-true (file-exists? (stderr-guard-spill-path)) "spill file must exist")
      (check-true (string-contains? (file->string (stderr-guard-spill-path)) "bug0072-test-record")
                  "spill file must contain the record"))

    (test-case "restore reinstates the original logger and tears down cleanly"
      (restore-stderr-guard!)
      (check-false (stderr-guard-active?))
      (check-equal? (current-logger) orig-logger)
      ;; After restore, bare log-error goes back to the original chain.
      (define saw (box #f))
      (define recv (make-log-receiver orig-logger 'error))
      (define t
        (thread (lambda ()
                  (define v (sync/timeout 0.2 recv))
                  (when v
                    (set-box! saw #t)))))
      (log-error "post-restore-record")
      (sleep 0.15)
      (sync t)
      (check-true (unbox saw) "original logger receives records again after restore"))

    (test-case "BUG-0098/R98: spill-write failure passes #f capture status"
      (define captured (box '()))
      (install-stderr-guard!
       #:on-error-record (lambda (msg captured-path)
                           (set-box! captured
                                     (append (unbox captured)
                                             (list (list msg captured-path))))))
      ;; Inject: close the guard's private spill port; the next write fails.
      (close-output-port (stderr-guard-spill-port))
      (log-error "bug0072-spill-failed-record")
      (sleep 0.15)
      (define recs (unbox captured))
      (check-true (pair? recs) "callback still invoked when capture fails")
      (when (pair? recs)
        (check-true (string-contains? (~a (first (last recs))) "bug0072-spill-failed-record"))
        (check-false (second (last recs))
                     "capture failure must be disclosed as #f, not a path"))
      (restore-stderr-guard!))

    (test-case "BUG-0098/R98: callback failure disclosed in spill, guard survives"
      (define spill
        (install-stderr-guard!
         #:on-error-record (lambda (msg captured-path) (error "injected callback failure"))))
      (log-error "bug0072-callback-failure-record")
      (check-true
       (let loop ([n 0])
         (or (string-contains? (file->string spill) "diagnostic surface FAILED")
             (and (< n 100) (sleep 0.02) (loop (add1 n)))))
       "failed surface attempt must be disclosed in the spill")
      (log-error "bug0072-after-failure-record")
      (check-true
       (let loop ([n 0])
         (or (string-contains? (file->string spill) "bug0072-after-failure-record")
             (and (< n 100) (sleep 0.02) (loop (add1 n)))))
       "guard thread must keep draining after a callback failure")
      (restore-stderr-guard!))

    (test-case "BUG-0098/R98: teardown drains queued records without loss"
      (define spill (install-stderr-guard!))
      (define probe (make-log-receiver (current-logger) 'error))
      (log-error "bug0072-teardown-queued-record")
      (check-true (and (sync/timeout 5 probe) #t) "record delivered to receivers")
      (restore-stderr-guard!)
      (check-false (stderr-guard-active?))
      (check-true (string-contains? (file->string spill) "bug0072-teardown-queued-record")
                  "queued record must survive teardown, not vanish")
      (check-equal? (file-or-directory-permissions spill) '(write read)
                    "spill stays owner-only (0600)"))

    (test-case "restore is idempotent and re-install works after restore"
      (restore-stderr-guard!)
      (check-false (stderr-guard-active?))
      (define spill2 (install-stderr-guard!))
      (check-true (stderr-guard-active?))
      (restore-stderr-guard!)
      (check-false (stderr-guard-active?)))))

(module+ main
  (run-tests guard-tests))
