#lang racket

;; tests/test-tool-coordinator-phases.rkt -- N-02: tool-coordinator phase tests

(require rackunit
         rackunit/text-ui
         "../runtime/tool-coordinator.rkt"
         "../util/protocol-types.rkt"
         (except-in "../tools/tool.rkt" make-tool-result))

(define phase-suite
  (test-suite "tool-coordinator phase helpers"

    (test-case "classify-tool-results: empty lists"
      (check-equal? (classify-tool-results '() '()) '()))

    (test-case "classify-tool-results: mixed success and error"
      (define tcs
        (list (make-tool-call "tc1" "bash" (hasheq))
              (make-tool-call "tc2" "read" (hasheq))))
      (define results
        (list (make-success-result "ok")
              (make-error-result "failed")))
      (define classified (classify-tool-results tcs results))
      (check-equal? (length classified) 2)
      (check-equal? (hash-ref (first classified) 'name) "bash")
      (check-equal? (hash-ref (first classified) 'status) 'completed)
      (check-equal? (hash-ref (second classified) 'name) "read")
      (check-equal? (hash-ref (second classified) 'status) 'error))

    (test-case "build-blocked-tool-results: empty list"
      (check-equal? (build-blocked-tool-results '()) '()))

    (test-case "build-blocked-tool-results: blocked tool calls"
      (define tcs
        (list (make-tool-call "tc1" "bash" (hasheq))
              (make-tool-call "tc2" "read" (hasheq))))
      (define blocked (build-blocked-tool-results tcs))
      (check-equal? (length blocked) 2)
      (check-true (tool-result? (first blocked)))
      (check-true (tool-result-is-error? (first blocked)))
      (check-true (tool-result-is-error? (second blocked))))

    (test-case "classify-tool-results: unequal lengths truncates"
      (define tcs
        (list (make-tool-call "tc1" "bash" (hasheq))
              (make-tool-call "tc2" "read" (hasheq))
              (make-tool-call "tc3" "edit" (hasheq))))
      (define results
        (list (make-success-result "ok")
              (make-error-result "failed")))
      (define classified (classify-tool-results tcs results))
      (check-equal? (length classified) 2)
      (check-equal? (hash-ref (first classified) 'name) "bash")
      (check-equal? (hash-ref (second classified) 'name) "read"))

    (test-case "classify-tool-results: more results than calls truncates"
      (define tcs
        (list (make-tool-call "tc1" "bash" (hasheq))))
      (define results
        (list (make-success-result "ok")
              (make-error-result "extra")))
      (define classified (classify-tool-results tcs results))
      (check-equal? (length classified) 1)
      (check-equal? (hash-ref (first classified) 'status) 'completed))))

(run-tests phase-suite 'verbose)
