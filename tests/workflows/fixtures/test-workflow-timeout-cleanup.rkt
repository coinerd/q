#lang racket

;; tests/workflows/fixtures/test-workflow-timeout-cleanup.rkt — Timeout/cleanup tests (#5470)
;;
;; Tests that workflow timeout and cleanup wrappers work correctly:
;; - Timeouts are deterministic with diagnostics
;; - Temp dirs are cleaned up even on exception
;; - Debug mode preserves temp dirs

(require rackunit
         rackunit/text-ui
         racket/file
         "workflow-runner.rkt"
         "temp-project.rkt")

(define timeout-cleanup-tests
  (test-suite "Workflow timeout and cleanup (#5470)"

    (test-case "with-workflow-timeout returns result within deadline"
      (define result (with-workflow-timeout (lambda () (+ 1 2)) #:timeout-ms 5000))
      (check-equal? result 3))

    (test-case "with-workflow-timeout fires on timeout"
      (check-exn exn:fail?
                 (lambda ()
                   (with-workflow-timeout (lambda ()
                                            (sleep 10)
                                            'done)
                                          #:timeout-ms 100))))

    (test-case "with-workflow-timeout propagates exceptions"
      (check-exn exn:fail?
                 (lambda ()
                   (with-workflow-timeout (lambda () (error "test-error")) #:timeout-ms 5000))))

    (test-case "with-workflow-cleanup cleans up on success"
      (define-values (proj-dir sess-dir) (make-temp-project '()))
      (check-true (directory-exists? proj-dir))
      (with-workflow-cleanup proj-dir sess-dir (lambda () 'ok))
      (check-false (directory-exists? proj-dir))
      (check-false (directory-exists? sess-dir)))

    (test-case "with-workflow-cleanup cleans up on exception"
      (define-values (proj-dir sess-dir) (make-temp-project '()))
      (check-true (directory-exists? proj-dir))
      (with-handlers ([exn:fail? (lambda (e) (void))])
        (with-workflow-cleanup proj-dir sess-dir (lambda () (error "boom"))))
      (check-false (directory-exists? proj-dir))
      (check-false (directory-exists? sess-dir)))

    (test-case "debug mode preserves temp dirs"
      (define-values (proj-dir sess-dir) (make-temp-project '()))
      (check-true (directory-exists? proj-dir))
      (parameterize ([workflow-debug? #t])
        (with-workflow-cleanup proj-dir sess-dir (lambda () 'ok)))
      (check-true (directory-exists? proj-dir) "debug mode preserves project dir")
      ;; Clean up manually
      (cleanup-temp-project! proj-dir sess-dir))

    (test-case "with-workflow-timeout context parameter works"
      (check-exn
       exn:fail?
       (lambda ()
         (with-workflow-timeout (lambda () (sleep 10)) #:timeout-ms 50 #:context "test-context"))))

    (test-case "custom on-timeout handler is called"
      (define called? (box #f))
      (with-workflow-timeout (lambda () (sleep 10))
                             #:timeout-ms 50
                             #:on-timeout (lambda ()
                                            (set-box! called? #t)
                                            'timed-out))
      (check-true (unbox called?)))))

(module+ main
  (run-tests timeout-cleanup-tests))
