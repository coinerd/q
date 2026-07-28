#lang racket/base

;; tests/test-scheduler-failure-symmetry.rkt
;; W0-execution: Serial/Parallel Failure Symmetry tests
;;
;; Tests:
;; - Same exception mapping in serial and parallel modes
;; - One tool failure cannot crash the batch
;; - Interrupt (exn:break) cleans resources and produces one terminal result
;; - Mixed-success batch preserves ordering
;; - Gateway failure handling

(require rackunit
         racket/string
         (only-in "../tools/tool.rkt"
                  make-tool
                  make-tool-registry
                  register-tool!
                  tool-result?
                  tool-result-content
                  tool-result-is-error?
                  make-error-result
                  make-success-result
                  make-tool-call)
         (only-in "../tools/scheduler.rkt" run-tool-batch scheduler-result-results)
         (only-in "../tools/scheduler-execution.rkt" max-parallel-tools)
         racket/list
         "../tools/permission-gate.rkt"
         (only-in "../tools/permission-gate.rkt" make-permissive-permission-config)
         (only-in "../tools/tool.rkt" make-exec-context))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-test-registry)
  (define reg (make-tool-registry))

  ;; echo tool — returns the 'msg argument
  (register-tool!
   reg
   (make-tool
    "echo"
    "Echo tool"
    (hasheq 'type "object" 'properties (hasheq 'msg (hasheq 'type "string")) 'required '("msg"))
    (lambda (args ctx)
      (make-success-result (list (hasheq 'type "text" 'text (hash-ref args 'msg)))))))

  ;; fail tool — always throws an exception
  (register-tool! reg
                  (make-tool "fail"
                             "Always fails"
                             (hasheq 'type "object" 'properties (hasheq) 'required (list))
                             (lambda (args ctx) (error 'fail "deliberate failure"))))

  ;; break-tool — throws exn:break when 'should-break is true
  (register-tool!
   reg
   (make-tool "break-trigger"
              "Throws break exception"
              (hasheq 'type
                      "object"
                      'properties
                      (hasheq 'should-break (hasheq 'type "boolean"))
                      'required
                      '("should-break"))
              (lambda (args ctx)
                (if (hash-ref args 'should-break #f)
                    (let/ec k
                      (raise (exn:break "simulated break" (current-continuation-marks) k)))
                    (make-success-result (list (hasheq 'type "text" 'text "no-break")))))))

  ;; slow-echo — sleeps then echoes
  (register-tool!
   reg
   (make-tool
    "slow-echo"
    "Slow echo"
    (hasheq 'type "object" 'properties (hasheq 'msg (hasheq 'type "string")) 'required '("msg"))
    (lambda (args ctx)
      (sleep 0.03)
      (make-success-result (list (hasheq 'type "text" 'text (hash-ref args 'msg)))))))

  reg)

(define (result-text tr)
  (define content (tool-result-content tr))
  (if (and (list? content) (pair? content))
      (hash-ref (car content) 'text "")
      ""))

(define (run-test-batch tcs reg #:parallel? [parallel? #f])
  (run-tool-batch tcs
                  reg
                  #:parallel? parallel?
                  #:exec-context (make-exec-context #:permission-config
                                                    (make-permissive-permission-config))))

;; ============================================================
;; 1. Same exception mapping in serial and parallel modes
;; ============================================================

(test-case "Serial: tool exception returns error result, not crash"
  (define reg (make-test-registry))
  (define tcs (list (make-tool-call "tc-1" "fail" (hasheq))))
  (define sr (run-test-batch tcs reg #:parallel? #f))
  (define results (scheduler-result-results sr))
  (check-equal? (length results) 1)
  (check-true (tool-result? (car results)))
  (check-true (tool-result-is-error? (car results)))
  (check-true (string-contains? (result-text (car results)) "failure")))

(test-case "Parallel: tool exception returns error result, not crash"
  (define reg (make-test-registry))
  (define tcs (list (make-tool-call "tc-1" "fail" (hasheq))))
  (define sr (run-test-batch tcs reg #:parallel? #t))
  (define results (scheduler-result-results sr))
  (check-equal? (length results) 1)
  (check-true (tool-result? (car results)))
  (check-true (tool-result-is-error? (car results)))
  (check-true (string-contains? (result-text (car results)) "failure")))

(test-case "Serial and parallel produce same error message for tool failure"
  (define reg (make-test-registry))
  (define tcs (list (make-tool-call "tc-1" "fail" (hasheq))))
  (define serial-result (run-test-batch tcs reg #:parallel? #f))
  (define parallel-result (run-test-batch tcs reg #:parallel? #t))
  (define serial-text (result-text (car (scheduler-result-results serial-result))))
  (define parallel-text (result-text (car (scheduler-result-results parallel-result))))
  (check-equal? serial-text parallel-text "serial and parallel should produce identical error text"))

;; ============================================================
;; 2. One tool failure cannot crash the batch
;; ============================================================

(test-case "Serial: one failure does not abort the batch"
  (define reg (make-test-registry))
  (define tcs
    (list (make-tool-call "tc-1" "echo" (hasheq 'msg "first"))
          (make-tool-call "tc-2" "fail" (hasheq))
          (make-tool-call "tc-3" "echo" (hasheq 'msg "third"))))
  (define sr (run-test-batch tcs reg #:parallel? #f))
  (define results (scheduler-result-results sr))
  (check-equal? (length results) 3)
  (check-false (tool-result-is-error? (first results)))
  (check-true (tool-result-is-error? (second results)))
  (check-false (tool-result-is-error? (third results)))
  (check-equal? (result-text (first results)) "first")
  (check-equal? (result-text (third results)) "third"))

(test-case "Parallel: one failure does not abort the batch"
  (define reg (make-test-registry))
  (define tcs
    (list (make-tool-call "tc-1" "echo" (hasheq 'msg "first"))
          (make-tool-call "tc-2" "fail" (hasheq))
          (make-tool-call "tc-3" "echo" (hasheq 'msg "third"))))
  (define sr (run-test-batch tcs reg #:parallel? #t))
  (define results (scheduler-result-results sr))
  (check-equal? (length results) 3)
  (check-false (tool-result-is-error? (first results)))
  (check-true (tool-result-is-error? (second results)))
  (check-false (tool-result-is-error? (third results)))
  (check-equal? (result-text (first results)) "first")
  (check-equal? (result-text (third results)) "third"))

;; ============================================================
;; 3. Interrupt (exn:break) cleans resources and produces terminal result
;; ============================================================

(test-case "Serial: exn:break produces cancellation error result"
  (define reg (make-test-registry))
  (define tcs (list (make-tool-call "tc-1" "break-trigger" (hasheq 'should-break #t))))
  (define sr (run-test-batch tcs reg #:parallel? #f))
  (define results (scheduler-result-results sr))
  (check-equal? (length results) 1)
  (check-true (tool-result-is-error? (car results)))
  (check-true (string-contains? (result-text (car results)) "cancelled")))

(test-case "Parallel: exn:break produces cancellation error result"
  (define reg (make-test-registry))
  (define tcs (list (make-tool-call "tc-1" "break-trigger" (hasheq 'should-break #t))))
  (define sr (run-test-batch tcs reg #:parallel? #t))
  (define results (scheduler-result-results sr))
  (check-equal? (length results) 1)
  (check-true (tool-result-is-error? (car results)))
  (check-true (string-contains? (result-text (car results)) "cancelled")))

(test-case "Serial: break in mixed batch cleans up and preserves ordering"
  (define reg (make-test-registry))
  (define tcs
    (list (make-tool-call "tc-1" "echo" (hasheq 'msg "first"))
          (make-tool-call "tc-2" "break-trigger" (hasheq 'should-break #t))
          (make-tool-call "tc-3" "echo" (hasheq 'msg "third"))))
  (define sr (run-test-batch tcs reg #:parallel? #f))
  (define results (scheduler-result-results sr))
  (check-equal? (length results) 3)
  (check-false (tool-result-is-error? (first results)))
  (check-true (tool-result-is-error? (second results)))
  (check-false (tool-result-is-error? (third results)))
  (check-equal? (result-text (first results)) "first")
  (check-equal? (result-text (third results)) "third"))

(test-case "Parallel: break in mixed batch cleans up and preserves ordering"
  (define reg (make-test-registry))
  (define tcs
    (list (make-tool-call "tc-1" "echo" (hasheq 'msg "first"))
          (make-tool-call "tc-2" "break-trigger" (hasheq 'should-break #t))
          (make-tool-call "tc-3" "echo" (hasheq 'msg "third"))))
  (define sr (run-test-batch tcs reg #:parallel? #t))
  (define results (scheduler-result-results sr))
  (check-equal? (length results) 3)
  (check-false (tool-result-is-error? (first results)))
  (check-true (tool-result-is-error? (second results)))
  (check-false (tool-result-is-error? (third results)))
  (check-equal? (result-text (first results)) "first")
  (check-equal? (result-text (third results)) "third"))

;; ============================================================
;; 4. Mixed-success batch preserves ordering
;; ============================================================

(test-case "Serial: complex mixed batch preserves order"
  (define reg (make-test-registry))
  (define tcs
    (list (make-tool-call "tc-1" "echo" (hasheq 'msg "A"))
          (make-tool-call "tc-2" "fail" (hasheq))
          (make-tool-call "tc-3" "echo" (hasheq 'msg "C"))
          (make-tool-call "tc-4" "fail" (hasheq))
          (make-tool-call "tc-5" "echo" (hasheq 'msg "E"))))
  (define sr (run-test-batch tcs reg #:parallel? #f))
  (define results (scheduler-result-results sr))
  (check-equal? (length results) 5)
  (check-equal? (result-text (first results)) "A")
  (check-true (tool-result-is-error? (second results)))
  (check-equal? (result-text (third results)) "C")
  (check-true (tool-result-is-error? (fourth results)))
  (check-equal? (result-text (fifth results)) "E"))

(test-case "Parallel: complex mixed batch preserves order"
  (define reg (make-test-registry))
  (define tcs
    (list (make-tool-call "tc-1" "slow-echo" (hasheq 'msg "A"))
          (make-tool-call "tc-2" "fail" (hasheq))
          (make-tool-call "tc-3" "slow-echo" (hasheq 'msg "C"))
          (make-tool-call "tc-4" "fail" (hasheq))
          (make-tool-call "tc-5" "slow-echo" (hasheq 'msg "E"))))
  (define sr (run-test-batch tcs reg #:parallel? #t))
  (define results (scheduler-result-results sr))
  (check-equal? (length results) 5)
  (check-equal? (result-text (first results)) "A")
  (check-true (tool-result-is-error? (second results)))
  (check-equal? (result-text (third results)) "C")
  (check-true (tool-result-is-error? (fourth results)))
  (check-equal? (result-text (fifth results)) "E"))

;; ============================================================
;; 5. Terminal event cardinality: one result per input
;; ============================================================

(test-case "Serial: result count equals input count"
  (define reg (make-test-registry))
  (define tcs
    (list (make-tool-call "tc-1" "echo" (hasheq 'msg "a"))
          (make-tool-call "tc-2" "fail" (hasheq))
          (make-tool-call "tc-3" "break-trigger" (hasheq 'should-break #t))
          (make-tool-call "tc-4" "echo" (hasheq 'msg "d"))))
  (define sr (run-test-batch tcs reg #:parallel? #f))
  (check-equal? (length (scheduler-result-results sr)) 4))

(test-case "Parallel: result count equals input count"
  (define reg (make-test-registry))
  (define tcs
    (list (make-tool-call "tc-1" "echo" (hasheq 'msg "a"))
          (make-tool-call "tc-2" "fail" (hasheq))
          (make-tool-call "tc-3" "break-trigger" (hasheq 'should-break #t))
          (make-tool-call "tc-4" "echo" (hasheq 'msg "d"))))
  (define sr (run-test-batch tcs reg #:parallel? #t))
  (check-equal? (length (scheduler-result-results sr)) 4))
