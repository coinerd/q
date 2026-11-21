#lang racket

;; tests/test-cl3-eval-probe.rkt — CL3 Wave 1 evaluation-prototype contract.
;; These tests exercise a SEPARATE child process. They explicitly do not touch
;; q's tool registry, test runner, admission records or CI policy.
;;
;; @speed slow
;; @suite testing
;; @boundary integration

(require rackunit
         racket/file
         racket/list
         racket/runtime-path
         racket/string
         "../scripts/cl3-eval-probe.rkt")

(define-runtime-path repo-root "..")

(define (write-module path body)
  (make-directory* (path-only path))
  (call-with-output-file path (lambda (out) (display body out)) #:exists 'truncate/replace))

(define (with-session thunk)
  (define session (start-worker! #:directory repo-root))
  (dynamic-wind (lambda () (void)) (lambda () (thunk session)) (lambda () (kill-worker! session))))

(define (eval! session code #:timeout [timeout 8])
  (request! session (hash 'op "eval" 'code code) #:timeout timeout))

(define (value-text response)
  (define values (hash-ref response 'values '()))
  (and (pair? values) (hash-ref (car values) 'text #f)))

(test-case "evaluations share state in the persistent child"
  (with-session (lambda (session)
                  (check-equal? (hash-ref (eval! session "(define cl3-x 41)") 'status) "ok")
                  (check-equal? (value-text (eval! session "(+ cl3-x 1)")) "42"))))

(test-case "reload reports stale-instance risk while reset is a clean environment"
  (define module-path (build-path (find-system-path 'temp-dir) "cl3-probe-reload.rkt"))
  (write-module module-path "#lang racket/base\n(provide f)\n(define (f) 1)\n")
  (with-session
   (lambda (session)
     (check-equal? (hash-ref (request! session (hash 'op "load" 'path (path->string module-path)))
                             'status)
                   "ok")
     (check-equal? (value-text (eval! session "(f)")) "1")
     (check-equal? (hash-ref (eval! session "(define old-f f)") 'status) "ok")
     (write-module module-path "#lang racket/base\n(provide f)\n(define (f) 2)\n")
     (define reload (request! session (hash 'op "reload" 'path (path->string module-path))))
     (check-equal? (hash-ref reload 'status) "ok")
     (check-true (string-contains? (hash-ref reload 'note) "may be stale"))
     ;; The evaluation finding: a captured closure can survive reload unchanged.
     (check-equal? (value-text (eval! session "(old-f)")) "1")
     (check-equal? (hash-ref (request! session (hash 'op "reset")) 'status) "ok")
     (check-equal? (hash-ref (eval! session "(old-f)") 'status) "error")))
  (delete-file module-path))

(test-case "malformed expressions produce structured errors without killing the worker"
  (with-session (lambda (session)
                  (define bad (eval! session "(define (broken"))
                  (check-equal? (hash-ref bad 'status) "error")
                  (check-true (hash-has-key? (hash-ref bad 'error) 'message))
                  (check-equal? (value-text (eval! session "(+ 1 1)")) "2"))))

(test-case "excessive output is bounded and flagged"
  (with-session (lambda (session)
                  (define result (eval! session "(displayln (make-string 20000 #\\x))"))
                  (check-equal? (hash-ref result 'status) "ok")
                  (check-true (hash-ref result 'truncated?))
                  (check-true (<= (string-length (hash-ref result 'stdout)) 4096)))))

(test-case "an infinite loop is timed out and the child is reaped"
  (define session (start-worker! #:directory repo-root))
  (define result (eval! session "(let loop () (loop))" #:timeout 2))
  (check-equal? (hash-ref result 'status) "timeout")
  (check-true (hash-ref result 'terminated?))
  (check-true (number? (hash-ref (kill-worker! session) 'exit-status))))

(test-case "worker crash is detected and cleaned up"
  (define session (start-worker! #:directory repo-root))
  (define result (eval! session "(exit 1)" #:timeout 5))
  (check-true (and (member (hash-ref result 'status) '("crash" "timeout")) #t))
  (check-true (number? (hash-ref (kill-worker! session) 'exit-status))))

(test-case "evaluation prototype is not wired into production verification"
  (define qt (file->string (build-path repo-root "qt")))
  (define registry (file->string (build-path repo-root "tools" "registry-table" "core-tools.rkt")))
  (for ([text (in-list (list qt registry))])
    (check-false (string-contains? text "cl3-worker-probe"))
    (check-false (string-contains? text "cl3-eval-probe"))))
