#lang racket/base

;; tests/test-tool-dsl-w0.rkt — Tool DSL improvement tests (W-16, W-20, I-09)

(require rackunit
         "../tools/tool.rkt"
         "../llm/stream.rkt")

;; ============================================================
;; W-16: Arity wrapper in make-tool
;; ============================================================

(test-case "make-tool rejects 0-arity handler (no args)"
  (check-exn exn:fail? (lambda () (make-tool "bad-tool" "desc" (hasheq) (lambda () "no args")))))

(test-case "make-tool accepts 1-arity handler (legacy compat)"
  (define t (make-tool "legacy-tool" "desc" (hasheq) (lambda (args) "one arg")))
  (check-true (tool? t)))

(test-case "make-tool accepts 2-arity handler"
  (define t (make-tool "good-tool" "desc" (hasheq) (lambda (args ctx) "two args")))
  (check-true (tool? t)))

(test-case "make-tool accepts 3+ arity handler (variadic compat)"
  (define t (make-tool "flex-tool" "desc" (hasheq) (lambda (args ctx . rest) "rest args")))
  (check-true (tool? t)))

;; ============================================================
;; I-09: tool-call-accum struct
;; ============================================================

(test-case "tool-call-accum struct has named fields"
  (define accum (tool-call-accum "call-1" "bash" "ls "))
  (check-equal? (tool-call-accum-id accum) "call-1")
  (check-equal? (tool-call-accum-name accum) "bash")
  (check-equal? (tool-call-accum-arguments accum) "ls "))

(test-case "tool-call-accum is transparent"
  (define accum (tool-call-accum #f "read" ""))
  (check-true (tool-call-accum? accum))
  (check-false (tool-call-accum-id accum))
  (check-equal? (tool-call-accum-arguments accum) ""))

(test-case "tool-call-accum replaces 3-element list pattern"
  ;; Verify the struct works the same as the old (list id name args) pattern
  (define old-style (list "id-1" "bash" "ls -la"))
  (define new-style (tool-call-accum "id-1" "bash" "ls -la"))
  (check-equal? (car old-style) (tool-call-accum-id new-style))
  (check-equal? (cadr old-style) (tool-call-accum-name new-style))
  (check-equal? (caddr old-style) (tool-call-accum-arguments new-style)))
