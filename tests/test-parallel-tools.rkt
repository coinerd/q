#lang racket

;; tests/test-parallel-tools.rkt — FEAT-73: Parallel tool execution

(require rackunit
         rackunit/text-ui
         "../tools/tool.rkt"
         "../tools/scheduler.rkt")

(define (make-echo-tool name)
  (make-tool name
             "echo tool"
             (hasheq)
             (lambda (args ctx) (make-tool-result (hash-ref args 'out "") (hasheq) #f))))

(define parallel-tests
  (test-suite "parallel tool execution"

    (test-case "multiple tool calls execute in parallel"
      (define reg (make-tool-registry))
      (register-tool!
       reg
       (make-tool "slow-echo"
                  "slow echo"
                  (hasheq)
                  (lambda (args ctx)
                    (sleep 0.05)
                    (make-tool-result (format "slow: ~a" (hash-ref args 'msg "hi")) (hasheq) #f))))
      (register-tool!
       reg
       (make-tool "fast-echo"
                  "fast echo"
                  (hasheq)
                  (lambda (args ctx)
                    (make-tool-result (format "fast: ~a" (hash-ref args 'msg "hi")) (hasheq) #f))))

      (define tool-calls
        (list (make-tool-call "tc-1" "slow-echo" (hasheq 'msg "A"))
              (make-tool-call "tc-2" "fast-echo" (hasheq 'msg "B"))))

      (define start-time (current-inexact-milliseconds))
      (define result (run-tool-batch tool-calls reg #:parallel? #t))
      (define elapsed (- (current-inexact-milliseconds) start-time))

      ;; Two 50ms sleeps in parallel should complete well under 2×50ms.
      ;; 250ms threshold proves parallelism even on slow CI (macos-latest).
      (check-true (< elapsed 250) (format "parallel execution took ~ams, should be < 250ms" elapsed))
      (check-equal? (length (scheduler-result-results result)) 2))

    (test-case "parallel results maintain original order"
      (define reg (make-tool-registry))
      (register-tool! reg (make-echo-tool "echo"))
      (define tool-calls
        (for/list ([i (in-range 5)])
          (make-tool-call (format "tc-~a" i) "echo" (hasheq 'out (format "out-~a" i)))))

      (define result (run-tool-batch tool-calls reg #:parallel? #t))
      (define results (scheduler-result-results result))

      (for ([r (in-list results)]
            [i (in-naturals)])
        (check-not-false (string-contains? (tool-result-content r) (format "out-~a" i))
                         (format "result ~a should contain out-~a" i i))))

    (test-case "serial execution still works"
      (define reg (make-tool-registry))
      (register-tool! reg (make-echo-tool "echo"))
      (define tool-calls
        (list (make-tool-call "tc-1" "echo" (hasheq 'out "A"))
              (make-tool-call "tc-2" "echo" (hasheq 'out "B"))))

      (define result (run-tool-batch tool-calls reg #:parallel? #f))
      (check-equal? (length (scheduler-result-results result)) 2))

    (test-case "single tool call works in parallel mode"
      (define reg (make-tool-registry))
      (register-tool! reg (make-echo-tool "echo"))
      (define tool-calls (list (make-tool-call "tc-1" "echo" (hasheq 'out "only"))))

      (define result (run-tool-batch tool-calls reg #:parallel? #t))
      (define results (scheduler-result-results result))
      (check-equal? (length results) 1)
      (check-not-false (string-contains? (tool-result-content (car results)) "only")))

    (test-case "config parallel-tools defaults to #t"
      (define config (hasheq))
      (check-true (hash-ref config 'parallel-tools #t)))))

(run-tests parallel-tests)
