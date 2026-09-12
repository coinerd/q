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
                  stderr-guard-spill-path))

(define captured (box '()))
(define orig-logger (current-logger))

(define guard-tests
  (test-suite "TUI stderr guard (BUG-0072)"

    (test-case "install swaps current-logger to an unparented guard logger"
      (define spill
        (install-stderr-guard!
         #:on-error-record (lambda (msg) (set-box! captured (append (unbox captured) (list msg))))))
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

    (test-case "restore is idempotent and re-install works after restore"
      (restore-stderr-guard!)
      (check-false (stderr-guard-active?))
      (define spill2 (install-stderr-guard!))
      (check-true (stderr-guard-active?))
      (restore-stderr-guard!)
      (check-false (stderr-guard-active?)))))

(module+ main
  (run-tests guard-tests))
