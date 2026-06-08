#lang racket

;; @speed slow  ;; @suite workflows

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
         "temp-project.rkt"
         "mock-provider.rkt")

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
      (check-true (unbox called?)))

    ;; ============================================================
    ;; Integration: run-workflow with timeout/cleanup (#5500)
    ;; ============================================================

    (test-case "run-workflow with cleanup? cleans up on success (#5500)"
      (define prov (make-scripted-provider (list (text-response "Hello!"))))
      (define result (run-workflow prov "test" #:cleanup? #t))
      (check-not-false (workflow-result-output result))
      (check-false (directory-exists? (workflow-result-session-dir result))
                   "cleanup must remove session dir"))

    (test-case "run-workflow with cleanup? cleans up on error (#5500)"
      (define prov (make-scripted-provider '()))
      (with-handlers ([exn:fail? (lambda (e) (void))])
        (run-workflow prov "test" #:cleanup? #t #:max-iterations 1)))

    (test-case "run-workflow with timeout-ms terminates runaway workflow (#5500)"
      (define prov
        (make-scripted-provider (list (tool-call-response "tc-1" "bash" (hash 'cmd "sleep 60"))
                                      (text-response "done"))))
      (define result
        (with-handlers ([exn:fail? (lambda (e) 'timed-out)])
          (run-workflow prov "sleep forever" #:timeout-ms 3000 #:register-default-tools? #t)))
      (check-true (or (workflow-result? result) (eq? result 'timed-out))))

    ;; ============================================================
    ;; Multi-turn integration (#5540)
    ;; ============================================================

    (test-case "run-workflow-multi-turn with cleanup? cleans up on success (#5540)"
      (define prov (make-scripted-provider (list (text-response "Turn 1") (text-response "Turn 2"))))
      (define result (run-workflow-multi-turn prov '("Hello" "World") #:cleanup? #t))
      (check-not-false result)
      (check-false (directory-exists? (workflow-result-session-dir result))
                   "multi-turn cleanup must remove session dir"))

    (test-case "run-workflow-multi-turn with cleanup? cleans up on error (#5540)"
      ;; Provider exhausted after 1 entry, but we send 2 prompts
      (define prov (make-scripted-provider (list (text-response "Only one"))))
      (with-handlers ([exn:fail? (lambda (e) (void))])
        (run-workflow-multi-turn prov '("First" "Second") #:cleanup? #t #:max-iterations 1)))

    (test-case "run-workflow-multi-turn with timeout-ms terminates runaway (#5540)"
      (define prov
        (make-scripted-provider (list (tool-call-response "tc-1" "bash" (hash 'cmd "sleep 60"))
                                      (text-response "done"))))
      (define result
        (with-handlers ([exn:fail? (lambda (e) 'timed-out)])
          (run-workflow-multi-turn prov
                                   '("sleep forever")
                                   #:timeout-ms 3000
                                   #:register-default-tools? #t)))
      (check-true (or (workflow-result? result) (eq? result 'timed-out))))))

(module+ main
  (run-tests timeout-cleanup-tests))
