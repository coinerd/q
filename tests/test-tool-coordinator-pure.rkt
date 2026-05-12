#lang racket

;; tests/test-tool-coordinator-pure.rkt -- W2: Pure helper tests for tool-coordinator.rkt
;;
;; Tests the extracted pure helpers:
;;   - classify-tool-results
;;   - build-blocked-tool-results

(require rackunit
         rackunit/text-ui
         "../runtime/tool-coordinator.rkt"
         "../util/protocol-types.rkt")

;; Helper to extract text string from a tool-result
(define (result-text tr)
  (hash-ref (first (tool-result-content tr)) 'text))

(define pure-suite
  (test-suite "tool-coordinator pure helpers"

    ;; -- classify-tool-results: all completed --
    (test-case "classify-tool-results marks all as completed when no errors"
      (define tc1 (make-tool-call "id-1" "grep" (hasheq 'pattern "foo")))
      (define tc2 (make-tool-call "id-2" "ls" (hasheq)))
      (define tr1 (make-tool-result "found" (hasheq) #f))
      (define tr2 (make-tool-result "dir" (hasheq) #f))
      (define result (classify-tool-results (list tc1 tc2) (list tr1 tr2)))
      (check-equal? (length result) 2)
      (check-equal? (hash-ref (first result) 'status) 'completed)
      (check-equal? (hash-ref (second result) 'status) 'completed)
      (check-equal? (hash-ref (first result) 'name) "grep")
      (check-equal? (hash-ref (second result) 'name) "ls"))

    ;; -- classify-tool-results: mixed errors --
    (test-case "classify-tool-results marks errors correctly"
      (define tc1 (make-tool-call "id-1" "grep" (hasheq)))
      (define tc2 (make-tool-call "id-2" "read" (hasheq)))
      (define tr1 (make-tool-result "error" (hasheq) #t))
      (define tr2 (make-tool-result "ok" (hasheq) #f))
      (define result (classify-tool-results (list tc1 tc2) (list tr1 tr2)))
      (check-equal? (hash-ref (first result) 'status) 'error)
      (check-equal? (hash-ref (second result) 'status) 'completed))

    ;; -- classify-tool-results: empty lists --
    (test-case "classify-tool-results with empty lists returns empty"
      (check-equal? (classify-tool-results '() '()) '()))

    ;; -- build-blocked-tool-results: creates errors for all --
    (test-case "build-blocked-tool-results creates error results"
      (define tc1 (make-tool-call "id-1" "grep" (hasheq)))
      (define tc2 (make-tool-call "id-2" "ls" (hasheq)))
      (define result (build-blocked-tool-results (list tc1 tc2)))
      (check-equal? (length result) 2)
      (check-true (tool-result-is-error? (first result)))
      (check-true (tool-result-is-error? (second result)))
      (check-true (string-contains? (result-text (first result)) "blocked")))

    ;; -- build-blocked-tool-results: empty list --
    (test-case "build-blocked-tool-results with empty list returns empty"
      (check-equal? (build-blocked-tool-results '()) '()))

    ;; -- build-blocked-tool-results: single entry --
    (test-case "build-blocked-tool-results single entry"
      (define tc (make-tool-call "id-1" "read" (hasheq)))
      (define result (build-blocked-tool-results (list tc)))
      (check-equal? (length result) 1)
      (check-true (tool-result-is-error? (first result)))
      (check-true (string-contains? (result-text (first result)) "read")))))

(run-tests pure-suite 'verbose)
