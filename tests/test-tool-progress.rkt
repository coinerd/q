#lang racket

;; tests/test-tool-progress.rkt — FEAT-74: Tool progress callbacks

(require rackunit
         rackunit/text-ui
         "../tools/tool.rkt"
         "../tools/scheduler.rkt")

(define progress-tests
  (test-suite "tool progress callbacks"

    ;; ============================================================
    ;; exec-context has progress-callback field
    ;; ============================================================
    (test-case "make-exec-context accepts #:progress-callback"
      (define calls '())
      (define ctx
        (make-exec-context #:progress-callback
                           (lambda (pct msg) (set! calls (cons (list pct msg) calls)))))
      (check-not-false (exec-context-progress-callback ctx))
      ;; Default is #f
      (check-false (exec-context-progress-callback (make-exec-context))))

    ;; ============================================================
    ;; emit-progress! calls the callback
    ;; ============================================================
    (test-case "emit-progress! invokes callback with percentage and message"
      (define calls '())
      (define ctx
        (make-exec-context #:progress-callback
                           (lambda (pct msg) (set! calls (cons (list pct msg) calls)))))
      (emit-progress! ctx 50 "halfway done")
      (emit-progress! ctx 100 "complete")
      (check-equal? (reverse calls) '((50 "halfway done") (100 "complete"))))

    (test-case "emit-progress! is no-op when no callback"
      (define ctx (make-exec-context))
      ;; Should not raise
      (emit-progress! ctx 50 "nothing happens")
      (check-true #t))

    ;; ============================================================
    ;; Tool can emit progress during execution
    ;; ============================================================
    (test-case "tool emits progress during execution"
      (define progress-calls '())
      (define ctx
        (make-exec-context #:progress-callback
                           (lambda (pct msg)
                             (set! progress-calls (cons (list pct msg) progress-calls)))))
      (define reg (make-tool-registry))
      (register-tool! reg
                      (make-tool "progress-tool"
                                 "reports progress"
                                 (hasheq)
                                 (lambda (args exec-ctx)
                                   (emit-progress! exec-ctx 25 "starting")
                                   (emit-progress! exec-ctx 75 "almost done")
                                   (make-tool-result "done" (hasheq) #f))))
      (define tool-calls (list (make-tool-call "tc-1" "progress-tool" (hasheq))))
      (define result (run-tool-batch tool-calls reg #:exec-context ctx))
      (check-equal? (length (scheduler-result-results result)) 1)
      (check-equal? (reverse progress-calls) '((25 "starting") (75 "almost done"))))

    ;; ============================================================
    ;; Multiple tools emit progress independently
    ;; ============================================================
    (test-case "multiple tools emit progress independently"
      (define progress-calls '())
      (define ctx
        (make-exec-context #:progress-callback
                           (lambda (pct msg)
                             (set! progress-calls (cons (list pct msg) progress-calls)))))
      (define reg (make-tool-registry))
      (register-tool! reg
                      (make-tool "tool-a"
                                 "tool a"
                                 (hasheq)
                                 (lambda (args exec-ctx)
                                   (emit-progress! exec-ctx 50 "tool-a half")
                                   (make-tool-result "a-done" (hasheq) #f))))
      (register-tool! reg
                      (make-tool "tool-b"
                                 "tool b"
                                 (hasheq)
                                 (lambda (args exec-ctx)
                                   (emit-progress! exec-ctx 100 "tool-b done")
                                   (make-tool-result "b-done" (hasheq) #f))))
      (define tool-calls
        (list (make-tool-call "tc-1" "tool-a" (hasheq)) (make-tool-call "tc-2" "tool-b" (hasheq))))
      (define result (run-tool-batch tool-calls reg #:exec-context ctx #:parallel? #f))
      ;; Both tools should have emitted progress
      (check >= (length progress-calls) 2))))

(run-tests progress-tests)
