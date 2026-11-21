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
         "../scripts/cl3-benchmark-probe.rkt"
         "../scripts/cl3-eval-probe.rkt"
         "../scripts/cl3-worker-probe.rkt")

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

(test-case "reload is stale, reset is a clean namespace, and worker replacement is the process boundary"
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
     (check-equal? (value-text (eval! session "(old-f)")) "1")
     (check-equal? (value-text (eval! session "(f)")) "1")
     (define reset (request! session (hash 'op "reset")))
     (check-equal? (hash-ref reset 'status) "ok")
     (check-true (string-contains? (hash-ref reset 'note) "process-global state"))
     (check-true (string-contains? (hash-ref reset 'note) "worker replacement"))
     (check-equal? (hash-ref (eval! session "(old-f)") 'status) "error")
     (check-equal? (hash-ref (request! session (hash 'op "load" 'path (path->string module-path)))
                             'status)
                   "ok")
     (check-equal? (value-text (eval! session "(f)")) "2")))
  (delete-file module-path))

(test-case "malformed expressions produce structured errors without killing the worker"
  (with-session (lambda (session)
                  (define bad (eval! session "(define (broken"))
                  (check-equal? (hash-ref bad 'status) "error")
                  (check-true (hash-has-key? (hash-ref bad 'error) 'message))
                  (check-equal? (value-text (eval! session "(+ 1 1)")) "2"))))

(test-case "bounded collectors cap stored bytes and rendered values"
  (define-values (port collect) (make-bounded-collector 128))
  (display (make-string 100000 #\x) port)
  (flush-output port)
  (define result (collect))
  (check-equal? (hash-ref result 'stored-bytes) 128)
  (check-true (> (hash-ref result 'discarded-bytes) 0))
  (check-true (hash-ref result 'truncated?))
  (check-equal? (string-length (hash-ref result 'text)) 128)
  (define rendered (render-value (make-list 10000 'x)))
  (check-true (<= (string-length (car rendered)) max-value-chars))
  (check-true (cdr rendered)))

(test-case "excessive output is bounded and flagged end to end"
  (with-session (lambda (session)
                  (define result (eval! session "(displayln (make-string 20000 #\\x))"))
                  (check-equal? (hash-ref result 'status) "ok")
                  (check-true (hash-ref result 'bounded-output?))
                  (check-true (hash-ref result 'truncated?))
                  (check-true (<= (string-length (hash-ref result 'stdout)) max-diagnostic-chars)))))

(test-case "an infinite loop is timed out and the direct child is reaped"
  (define session (start-worker! #:directory repo-root))
  (define result (eval! session "(let loop () (loop))" #:timeout 2))
  (check-equal? (hash-ref result 'status) "timeout")
  (check-true (hash-ref result 'terminated?))
  (define cleanup (kill-worker! session))
  (check-true (number? (hash-ref cleanup 'exit-status)))
  (check-false (hash-ref cleanup 'descendant-cleanup-verified?)))

(test-case "worker crash is detected and cleaned up"
  (define session (start-worker! #:directory repo-root))
  (define result (eval! session "(exit 1)" #:timeout 5))
  (check-true (and (member (hash-ref result 'status) '("crash" "timeout")) #t))
  (check-true (number? (hash-ref (kill-worker! session) 'exit-status))))

(test-case "percentiles use the CL0-compatible linear-interpolation definition"
  (check-equal? (percentile '(1 2 3 4) 0.5) 2.5)
  (check-equal? (percentile '(1 2 3 4) 0.9) 3.7))

(test-case "evaluation prototype is not wired into production verification"
  (define qt (file->string (build-path repo-root "qt")))
  (define registry (file->string (build-path repo-root "tools" "registry-table" "core-tools.rkt")))
  (for ([text (in-list (list qt registry))])
    (check-false (string-contains? text "cl3-worker-probe"))
    (check-false (string-contains? text "cl3-eval-probe"))))
