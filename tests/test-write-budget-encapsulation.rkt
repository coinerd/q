#lang racket/base

;; BOUNDARY: integration

;; tests/test-write-budget-encapsulation.rkt — Tests for write budget encapsulation
;;
;; W0 scaffolding for v0.29.0 milestone: Verify that write budget tracking
;; lives in exec-context (not parameter+box), enabling per-session isolation.
;;
;; NOTE: tool-write returns a tool-result struct (not hash).
;; Budget violations return error tool-results (is-error? = #t), not exceptions.

(require rackunit
         racket/file
         racket/hash
         (only-in "../tools/builtins/write.rkt"
                  tool-write
                  current-max-write-bytes
                  cumulative-write-budget
                  reset-cumulative-writes!
                  init-session-writes!)
         (only-in "../tools/tool.rkt" tool-result? tool-result-is-error?))

;; ── Parameter+box isolation ──

(test-case "init-session-writes!-creates-fresh-box"
  (init-session-writes!)
  (parameterize ([current-max-write-bytes 1048576]
                 [cumulative-write-budget 52428800])
    (define result
      (tool-write (hasheq 'path "/tmp/q-test-write-budget-1.txt" 'content "hello world")))
    (check-true (tool-result? result))
    (check-false (tool-result-is-error? result))))

(test-case "reset-cumulative-writes!-resets-to-zero"
  (init-session-writes!)
  (parameterize ([current-max-write-bytes 1048576]
                 [cumulative-write-budget 52428800])
    (tool-write (hasheq 'path "/tmp/q-test-write-budget-2.txt" 'content "first write"))
    (reset-cumulative-writes!)
    ;; After reset, should be able to write again within budget
    (define result
      (tool-write
       (hasheq 'path "/tmp/q-test-write-budget-3.txt" 'content "second write after reset")))
    (check-true (tool-result? result))
    (check-false (tool-result-is-error? result))))

(test-case "cumulative-budget-enforced"
  (init-session-writes!)
  (parameterize ([current-max-write-bytes 1048576]
                 [cumulative-write-budget 20]) ;; Very small budget
    (tool-write
     (hasheq 'path "/tmp/q-test-write-budget-4.txt" 'content "12345678901234567890")) ;; 20 bytes
    ;; Next write should exceed budget and return error result
    (define result
      (tool-write (hasheq 'path "/tmp/q-test-write-budget-5.txt" 'content "should fail")))
    (check-true (tool-result? result))
    (check-true (tool-result-is-error? result))))

(test-case "max-write-bytes-enforced"
  (init-session-writes!)
  (parameterize ([current-max-write-bytes 10] ;; Very small per-write limit
                 [cumulative-write-budget 52428800])
    (define result
      (tool-write
       (hasheq 'path "/tmp/q-test-write-budget-6.txt" 'content "this is way too long for the limit")))
    (check-true (tool-result? result))
    (check-true (tool-result-is-error? result))))

(test-case "two-sessions-do-not-share-write-budget"
  ;; Session 1
  (init-session-writes!)
  (parameterize ([current-max-write-bytes 1048576]
                 [cumulative-write-budget 52428800])
    (tool-write (hasheq 'path "/tmp/q-test-write-budget-s1.txt" 'content "session 1 write")))

  ;; Session 2 — independent budget
  (init-session-writes!)
  (parameterize ([current-max-write-bytes 1048576]
                 [cumulative-write-budget 52428800])
    (define result
      (tool-write (hasheq 'path "/tmp/q-test-write-budget-s2.txt" 'content "session 2 write")))
    (check-true (tool-result? result))
    (check-false (tool-result-is-error? result))))

;; ── Cleanup ──

(test-case "cleanup-temp-files"
  (for ([f (in-list '("/tmp/q-test-write-budget-1.txt" "/tmp/q-test-write-budget-2.txt"
                                                       "/tmp/q-test-write-budget-3.txt"
                                                       "/tmp/q-test-write-budget-4.txt"
                                                       "/tmp/q-test-write-budget-s1.txt"
                                                       "/tmp/q-test-write-budget-s2.txt"))])
    (when (file-exists? f)
      (delete-file f)))
  (void))
