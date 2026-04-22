#lang racket

;; test-lockfile.rkt — tests for util/lockfile.rkt
;; Covers: FFI getpid, pid-alive?, lock acquisition, stale detection

(require rackunit
         rackunit/text-ui
         "../util/lockfile.rkt")

(define-test-suite
 lockfile-tests
 ;; --- getpid (FFI-based, no shell) ---
 (test-case "getpid returns positive integer"
   (define pid (getpid))
   (check-true (exact-positive-integer? pid) (format "expected positive integer, got ~a" pid)))
 (test-case "getpid is reasonable range"
   (define pid (getpid))
   (check-true (> pid 1) "PID should be > 1 (not init)")
   (check-true (< pid 4194304) "PID should be < PID_MAX"))
 ;; --- pid-alive? ---
 (test-case "pid-alive? returns #t for current process"
   (check-true (pid-alive? (getpid))))
 (test-case "pid-alive? returns #f for nonexistent PID"
   ;; Use a very high PID that almost certainly doesn't exist
   (check-false (pid-alive? 4194303)))
 (test-case "pid-alive? rejects non-integer input"
   (check-exn exn:fail? (lambda () (pid-alive? "not-a-pid"))))
 (test-case "pid-alive? rejects negative input"
   (check-exn exn:fail? (lambda () (pid-alive? -1))))
 ;; --- Lock acquisition ---
 (test-case "call-with-lock: basic acquire and release"
   (define tmp (make-temporary-file "locktest-~a"))
   (delete-file tmp)
   (define result (call-with-lock tmp (lambda () 'ok)))
   (check-equal? result 'ok))
 (test-case "with-lock-result: returns (ok result) on success"
   (define tmp (make-temporary-file "locktest-~a"))
   (delete-file tmp)
   (define result (with-lock-result tmp (lambda () 42)))
   (check-equal? result '(ok 42)))
 (test-case "with-lock-result: sequential locking works"
   (define tmp (make-temporary-file "locktest-~a"))
   (delete-file tmp)
   (define r1 (with-lock-result tmp (lambda () 'first)))
   (define r2 (with-lock-result tmp (lambda () 'second)))
   (check-equal? r1 '(ok first))
   (check-equal? r2 '(ok second)))
 (test-case "call-with-lock: cleanup on error"
   (define tmp (make-temporary-file "locktest-~a"))
   (delete-file tmp)
   (check-exn exn:fail? (lambda () (call-with-lock tmp (lambda () (error 'test "boom")))))
   ;; Lock should be released even after error
   (define result (with-lock-result tmp (lambda () 'recovered)))
   (check-equal? result '(ok recovered)))
 ;; --- Stale lock detection ---
 (test-case "stale lock from dead PID is cleaned up"
   (define tmp-name (format "locktest-stale-~a" (current-milliseconds)))
   ;; Manually create a lock file with a dead PID
   (define lf (build-path (find-system-path 'home-dir) ".q" "locks" (string-append "lock-" tmp-name)))
   (make-directory* (build-path (find-system-path 'home-dir) ".q" "locks"))
   (call-with-output-file lf
                          (lambda (out)
                            (display 4194303 out)
                            (newline out))
                          #:exists 'truncate)
   ;; Now try to acquire — should detect stale lock and succeed
   (define result (with-lock-result tmp-name (lambda () 'stale-cleaned)))
   (check-equal? result '(ok stale-cleaned))))

(run-tests lockfile-tests)
