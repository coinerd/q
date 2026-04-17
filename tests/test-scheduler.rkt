#lang racket

;;; tests/test-scheduler.rkt — TDD tests for tools/scheduler.rkt
;;;
;;; Covers (from TEST_STRATEGY.md scenarios C, D, E):
;;;   - Single tool call executes correctly
;;;   - Multiple tool calls execute in order
;;;   - Scheduler preserves commit order under parallel execution
;;;   - Hook that blocks a tool call produces blocked error result
;;;   - Hook that mutates args + revalidation passes → executes with new args
;;;   - Hook that mutates args + revalidation fails → error result
;;;   - Tool exception caught → error result (not crash)
;;;   - Unknown tool name → error result
;;;   - Empty tool-calls list → empty results
;;;   - Metadata counters

(require rackunit
         (only-in "../tools/tool.rkt"
                  tool?
                  tool-name
                  tool-schema
                  tool-execute
                  make-tool
                  make-tool-registry
                  make-exec-context
                  register-tool!
                  lookup-tool
                  validate-tool-args
                  tool-result?
                  tool-result-content
                  tool-result-details
                  tool-result-is-error?
                  make-error-result
                  make-success-result)
         (only-in "../tools/scheduler.rkt"
                  run-tool-batch
                  scheduler-result
                  scheduler-result-results
                  scheduler-result-metadata)
         (only-in "../util/protocol-types.rkt"
                  tool-call
                  tool-call?
                  tool-call-id
                  tool-call-name
                  tool-call-arguments))

;; ============================================================
;; Helper: build a simple registry with fake tools
;; ============================================================

(define (make-test-registry)
  (define reg (make-tool-registry))

  ;; echo tool — returns the 'msg argument as text content
  (register-tool!
   reg
   (make-tool
    "echo"
    "Echo tool"
    (hasheq 'type "object" 'properties (hasheq 'msg (hasheq 'type "string")) 'required '("msg"))
    (lambda (args ctx)
      (make-success-result (list (hasheq 'type "text" 'text (hash-ref args 'msg)))))))

  ;; add tool — returns sum of 'a and 'b as text
  (register-tool! reg
                  (make-tool "add"
                             "Add two numbers"
                             (hasheq 'type
                                     "object"
                                     'properties
                                     (hasheq 'a (hasheq 'type "integer") 'b (hasheq 'type "integer"))
                                     'required
                                     '("a" "b"))
                             (lambda (args ctx)
                               (define sum (+ (hash-ref args 'a) (hash-ref args 'b)))
                               (make-success-result
                                (list (hasheq 'type "text" 'text (number->string sum)))))))

  ;; fail tool — always throws an exception
  (register-tool!
   reg
   (make-tool "fail" "Always fails" (hasheq) (lambda (args ctx) (error 'fail "deliberate failure"))))

  ;; slow-echo tool — sleeps then echoes (for parallel order testing)
  (register-tool!
   reg
   (make-tool
    "slow-echo"
    "Slow echo"
    (hasheq 'type "object" 'properties (hasheq 'msg (hasheq 'type "string")) 'required '("msg"))
    (lambda (args ctx)
      (sleep 0.05) ; 50ms delay
      (make-success-result (list (hasheq 'type "text" 'text (hash-ref args 'msg)))))))
  reg)

;; Helper: extract text from a tool-result's content
(define (result-text tr)
  (define content (tool-result-content tr))
  (if (and (list? content) (pair? content))
      (hash-ref (car content) 'text "")
      ""))

;; ============================================================
;; 1. Single tool call executes correctly
;; ============================================================

(test-case "Single tool call executes correctly"
  (define reg (make-test-registry))
  (define tc (tool-call "tc-1" "echo" (hasheq 'msg "hello")))
  (define sr (run-tool-batch (list tc) reg))
  (define results (scheduler-result-results sr))
  (check-equal? (length results) 1)
  (check-equal? (result-text (car results)) "hello")
  (check-false (tool-result-is-error? (car results))))

;; ============================================================
;; 2. Multiple tool calls execute in order (serial)
;; ============================================================

(test-case "Multiple tool calls execute in order (serial)"
  (define reg (make-test-registry))
  (define tcs
    (list (tool-call "tc-1" "echo" (hasheq 'msg "first"))
          (tool-call "tc-2" "echo" (hasheq 'msg "second"))
          (tool-call "tc-3" "echo" (hasheq 'msg "third"))))
  (define sr (run-tool-batch tcs reg #:parallel? #f))
  (check-equal? (map result-text (scheduler-result-results sr)) '("first" "second" "third")))

;; ============================================================
;; 3. Scheduler preserves commit order under parallel execution
;; ============================================================

(test-case "Parallel execution preserves commit order"
  (define reg (make-test-registry))
  ;; slow-echo first, fast echo second — if parallel,
  ;; slow finishes last, but results should still be in input order
  (define tcs
    (list (tool-call "tc-1" "slow-echo" (hasheq 'msg "slow"))
          (tool-call "tc-2" "echo" (hasheq 'msg "fast"))))
  (define sr (run-tool-batch tcs reg #:parallel? #t))
  (check-equal? (map result-text (scheduler-result-results sr)) '("slow" "fast")))

;; ============================================================
;; 3a. Larger parallel batch also preserves order
;; ============================================================

(test-case "Larger parallel batch preserves order"
  (define reg (make-test-registry))
  (define tcs
    (list (tool-call "tc-1" "slow-echo" (hasheq 'msg "A"))
          (tool-call "tc-2" "echo" (hasheq 'msg "B"))
          (tool-call "tc-3" "slow-echo" (hasheq 'msg "C"))
          (tool-call "tc-4" "echo" (hasheq 'msg "D"))))
  (define sr (run-tool-batch tcs reg #:parallel? #t))
  (check-equal? (map result-text (scheduler-result-results sr)) '("A" "B" "C" "D")))

;; ============================================================
;; 4. Hook that blocks a tool call produces blocked error result
;; ============================================================

(test-case "Hook blocks tool call → error result"
  (define reg (make-test-registry))
  (define hook
    (lambda (hook-point data)
      (if (and (eq? hook-point 'tool-call) (tool-call? data) (equal? (tool-call-name data) "echo"))
          'blocked
          data)))
  (define tcs
    (list (tool-call "tc-1" "echo" (hasheq 'msg "blocked"))
          (tool-call "tc-2" "add" (hasheq 'a 1 'b 2))))
  (define sr (run-tool-batch tcs reg #:hook-dispatcher hook))
  (define results (scheduler-result-results sr))
  ;; First is blocked → error
  (check-true (tool-result-is-error? (first results)))
  (check-true (string-contains? (result-text (first results)) "blocked"))
  ;; Second is not blocked → success
  (check-false (tool-result-is-error? (second results)))
  (check-equal? (result-text (second results)) "3"))

;; ============================================================
;; 5. Hook that mutates args + revalidation passes → executes with new args
;; ============================================================

(test-case "Hook mutates args → revalidation passes → executes with new args"
  (define reg (make-test-registry))
  ;; Hook changes 'msg from "original" to "mutated"
  (define hook
    (lambda (hook-point data)
      (if (and (eq? hook-point 'tool-call) (tool-call? data) (equal? (tool-call-name data) "echo"))
          (struct-copy tool-call data [arguments (hasheq 'msg "mutated")])
          data)))
  (define tcs (list (tool-call "tc-1" "echo" (hasheq 'msg "original"))))
  (define sr (run-tool-batch tcs reg #:hook-dispatcher hook))
  (check-equal? (result-text (car (scheduler-result-results sr))) "mutated")
  (check-false (tool-result-is-error? (car (scheduler-result-results sr)))))

;; ============================================================
;; 6. Hook that mutates args + revalidation fails → error result
;; ============================================================

(test-case "Hook mutates args → revalidation fails → error result"
  (define reg (make-test-registry))
  ;; Hook changes 'a to a string, but schema requires integer
  (define hook
    (lambda (hook-point data)
      (if (and (eq? hook-point 'tool-call) (tool-call? data) (equal? (tool-call-name data) "add"))
          (struct-copy tool-call data [arguments (hasheq 'a "not-a-number" 'b 2)])
          data)))
  (define tcs (list (tool-call "tc-1" "add" (hasheq 'a 1 'b 2))))
  (define sr (run-tool-batch tcs reg #:hook-dispatcher hook))
  (check-true (tool-result-is-error? (car (scheduler-result-results sr))))
  (check-true (string-contains? (result-text (car (scheduler-result-results sr))) "validation")))

;; ============================================================
;; 7. Tool exception caught → error result (not crash)
;; ============================================================

(test-case "Tool exception caught → error result"
  (define reg (make-test-registry))
  (define tcs (list (tool-call "tc-1" "fail" (hasheq))))
  (define sr (run-tool-batch tcs reg))
  (check-equal? (length (scheduler-result-results sr)) 1)
  (check-true (tool-result-is-error? (car (scheduler-result-results sr)))))

;; ============================================================
;; 8. Unknown tool name → error result
;; ============================================================

(test-case "Unknown tool name → error result"
  (define reg (make-test-registry))
  (define tcs (list (tool-call "tc-1" "nonexistent_tool" (hasheq))))
  (define sr (run-tool-batch tcs reg))
  (check-true (tool-result-is-error? (car (scheduler-result-results sr)))))

;; ============================================================
;; 9. Empty tool-calls list → empty results
;; ============================================================

(test-case "Empty tool-calls list → empty results with zero metadata"
  (define reg (make-test-registry))
  (define sr (run-tool-batch '() reg))
  (check-equal? (length (scheduler-result-results sr)) 0)
  (check-equal? (hash-ref (scheduler-result-metadata sr) 'total) 0)
  (check-equal? (hash-ref (scheduler-result-metadata sr) 'executed) 0)
  (check-equal? (hash-ref (scheduler-result-metadata sr) 'blocked) 0)
  (check-equal? (hash-ref (scheduler-result-metadata sr) 'errors) 0))

;; ============================================================
;; 10. Metadata counters are correct
;; ============================================================

(test-case "Metadata counters track total/executed/blocked/errors"
  (define reg (make-test-registry))
  (define hook
    (lambda (hook-point data)
      (if (and (eq? hook-point 'tool-call) (equal? (tool-call-name data) "echo")) 'blocked data)))
  (define tcs
    (list (tool-call "tc-1" "echo" (hasheq 'msg "blocked")) ; blocked
          (tool-call "tc-2" "add" (hasheq 'a 3 'b 4)) ; success
          (tool-call "tc-3" "fail" (hasheq)) ; exception
          (tool-call "tc-4" "nonexistent" (hasheq)))) ; unknown
  (define sr (run-tool-batch tcs reg #:hook-dispatcher hook))
  (define meta (scheduler-result-metadata sr))
  (check-equal? (hash-ref meta 'total) 4)
  (check-equal? (hash-ref meta 'executed) 1) ; add succeeded
  (check-equal? (hash-ref meta 'blocked) 1) ; echo blocked
  (check-equal? (hash-ref meta 'errors) 2)) ; fail exception + nonexistent unknown

;; ============================================================
;; 11. Mixed batch: block, mutate-pass, mutate-fail, execute, exception
;; ============================================================

(test-case "Mixed batch: block, mutate-pass, mutate-fail, exception"
  (define reg (make-test-registry))
  (define hook
    (lambda (hook-point data)
      (cond
        ;; Only handle 'tool-call (preflight), not tool-call-pre/post
        [(not (eq? hook-point 'tool-call)) data]
        [(equal? (tool-call-name data) "echo") 'blocked]
        [(equal? (tool-call-name data) "add")
         ;; mutate: change a to string → validation will fail
         (struct-copy tool-call data [arguments (hasheq 'a "bad" 'b 2)])]
        [(equal? (tool-call-name data) "slow-echo")
         ;; mutate: valid change
         (struct-copy tool-call data [arguments (hasheq 'msg "hooked")])]
        [else data])))
  (define tcs
    (list (tool-call "tc-1" "echo" (hasheq 'msg "x")) ; blocked
          (tool-call "tc-2" "add" (hasheq 'a 1 'b 2)) ; mutated-invalid
          (tool-call "tc-3" "slow-echo" (hasheq 'msg "x")) ; mutated-valid
          (tool-call "tc-4" "fail" (hasheq)))) ; exception
  (define sr (run-tool-batch tcs reg #:hook-dispatcher hook))
  (define results (scheduler-result-results sr))
  ;; All but slow-echo are errors
  (check-true (tool-result-is-error? (first results))) ; blocked
  (check-true (tool-result-is-error? (second results))) ; validation fail
  (check-false (tool-result-is-error? (third results))) ; mutated-valid success
  (check-equal? (result-text (third results)) "hooked") ; got mutated text
  (check-true (tool-result-is-error? (fourth results)))) ; exception

;; ============================================================
;; 12. No hook dispatcher — tools execute normally
;; ============================================================

(test-case "No hook dispatcher → tools execute normally"
  (define reg (make-test-registry))
  (define tcs
    (list (tool-call "tc-1" "echo" (hasheq 'msg "no-hook"))
          (tool-call "tc-2" "add" (hasheq 'a 10 'b 20))))
  (define sr (run-tool-batch tcs reg))
  (check-equal? (result-text (first (scheduler-result-results sr))) "no-hook")
  (check-equal? (result-text (second (scheduler-result-results sr))) "30"))

;; ============================================================
;; 13. FUNC-07 (#105): Parallel tool execution doesn't deadlock on hook exception
;; ============================================================

(test-case "Parallel execution survives pre-hook exception"
  (define reg (make-test-registry))
  ;; Hook that throws for 'tool-call-pre, passes for everything else
  (define hook
    (lambda (hook-point data)
      (cond
        [(eq? hook-point 'tool-call-pre) (error 'test-hook "deliberate pre-hook crash")]
        [else data])))
  (define tcs (list (tool-call "tc-1" "echo" (hasheq 'msg "parallel-resilient"))))
  (define sr (run-tool-batch tcs reg #:hook-dispatcher hook #:parallel? #t))
  (define results (scheduler-result-results sr))
  (check-equal? (length results) 1 "parallel with throwing pre-hook should return results")
  ;; Tool should still execute (hook error is caught, treated as #f)
  (check-false (tool-result-is-error? (car results))
               "parallel execution should succeed despite pre-hook exception")
  (check-equal? (result-text (car results))
                "parallel-resilient"
                "result content should be from tool execution"))

;; ============================================================
;; 13a. FUNC-07 (#105): Serial mode also survives hook exception
;; ============================================================

(test-case "Serial execution survives pre-hook exception"
  (define reg (make-test-registry))
  (define hook
    (lambda (hook-point data)
      (cond
        [(eq? hook-point 'tool-call-pre) (error 'test-hook "deliberate pre-hook crash")]
        [else data])))
  (define tcs (list (tool-call "tc-1" "echo" (hasheq 'msg "serial-resilient"))))
  (define sr (run-tool-batch tcs reg #:hook-dispatcher hook #:parallel? #f))
  (define results (scheduler-result-results sr))
  (check-equal? (length results) 1 "serial with throwing pre-hook should return results")
  (check-false (tool-result-is-error? (car results))
               "serial execution should succeed despite pre-hook exception")
  (check-equal? (result-text (car results))
                "serial-resilient"
                "result content should be from tool execution"))

;; ============================================================
;; 13b. FUNC-07 (#105): Parallel with throwing post-hook also survives
;; ============================================================

(test-case "Parallel execution survives post-hook exception"
  (define reg (make-test-registry))
  (define hook
    (lambda (hook-point data)
      (cond
        [(eq? hook-point 'tool-result-post) (error 'test-hook "deliberate post-hook crash")]
        [else data])))
  (define tcs (list (tool-call "tc-1" "echo" (hasheq 'msg "post-resilient"))))
  (define sr (run-tool-batch tcs reg #:hook-dispatcher hook #:parallel? #t))
  (define results (scheduler-result-results sr))
  (check-equal? (length results) 1 "parallel with throwing post-hook should return results")
  (check-false (tool-result-is-error? (car results))
               "parallel execution should succeed despite post-hook exception")
  (check-equal? (result-text (car results))
                "post-resilient"
                "result content should be from tool execution"))
