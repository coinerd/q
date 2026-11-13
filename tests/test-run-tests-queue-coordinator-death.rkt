#lang racket/base

;; @suite testing
;; @speed fast
;; @timeout 120
;; @boundary unit
;; @isolation process

(require rackunit
         rackunit/text-ui
         racket/file
         racket/list
         "../scripts/run-tests.rkt")

(define (wait-until pred seconds)
  (define deadline (+ (current-inexact-milliseconds) (* seconds 1000)))
  (let loop ()
    (cond
      [(pred) #t]
      [(>= (current-inexact-milliseconds) deadline) #f]
      [else
       (sleep 0.01)
       (loop)])))

(define (write-text path text)
  (call-with-output-file path #:exists 'truncate (lambda (out) (display text out))))

(define (descendants cust parent)
  (append-map (lambda (v)
                (cons v
                      (if (custodian? v)
                          (descendants v cust)
                          '())))
              (custodian-managed-list cust parent)))

(define suite
  (test-suite "queue coordinator death"
    (test-case "killing coordinator terminates worker and active child"
      (define dir (make-temporary-file "queue-coordinator-death-~a" 'directory))
      (define ready (build-path dir "ready"))
      (define late (build-path dir "late"))
      (define fixture (build-path dir "fixture.rkt"))
      ;; The child exits shortly after the delayed side effect, even if orphaned.
      (write-text fixture
                  (format (string-append "#lang racket/base\n"
                                         "(call-with-output-file ~s #:exists 'truncate void)\n"
                                         "(sleep 4)\n"
                                         "(call-with-output-file ~s #:exists 'truncate void)\n")
                          (path->string ready)
                          (path->string late)))
      (define parent (make-custodian))
      (define coordinator
        (parameterize ([current-custodian parent]
                       [current-subprocess-custodian-mode 'kill])
          (thread (lambda ()
                    (run-all-files (list (path->string fixture)) 1 20000 #:scheduler 'queue)))))
      (dynamic-wind
       void
       (lambda ()
         (unless (wait-until (lambda () (file-exists? ready)) 15)
           (fail "fixture did not become ready"))
         (define managed (descendants parent (current-custodian)))
         (define workers
           (filter (lambda (v)
                     (and (thread? v)
                          (assq 'run-single-file/subprocess
                                (continuation-mark-set->context (continuation-marks v)))))
                   managed))
         (unless (= (length workers) 1)
           (fail (format "expected one active worker, found ~a" (length workers))))
         (define children (filter subprocess? managed))
         (unless (= (length children) 1)
           (fail (format "expected one active child handle, found ~a" (length children))))
         (define worker (car workers))
         (define child (car children))
         (kill-thread coordinator)
         (define coordinator-dead? (and (sync/timeout 1 coordinator) #t))
         ;; Keep parent alive throughout the assertions: its shutdown would mask the bug.
         (define worker-dead? (and (sync/timeout 1 worker) #t))
         (define child-dead? (and (sync/timeout 1 child) #t))
         (sleep 5)
         (check-equal? (list coordinator-dead? worker-dead? child-dead? (file-exists? late))
                       (list #t #t #t #f)
                       "coordinator, worker, child terminated; no delayed marker"))
       (lambda ()
         (custodian-shutdown-all parent)
         (sync/timeout 5 coordinator)
         ;; No saved PID: let any escaped fixture finish before deleting its directory.
         (sleep 5)
         (delete-directory/files dir))))))

(module+ main
  (exit (run-tests suite)))
(module+ test
  (unless (zero? (run-tests suite))
    (error 'queue-coordinator-death "tests failed")))
