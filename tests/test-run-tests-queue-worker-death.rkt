#lang racket/base

;; @suite testing
;; @speed fast
;; @timeout 120
;; @boundary unit
;; @isolation process

(require rackunit
         rackunit/text-ui
         racket/async-channel
         racket/file
         racket/list
         "../scripts/run-tests.rkt")

(define (wait-until pred [seconds 15])
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

(define (descendant-threads cust parent)
  (append-map (lambda (v)
                (cond
                  [(thread? v) (list v)]
                  [(custodian? v) (descendant-threads v cust)]
                  [else '()]))
              (custodian-managed-list cust parent)))

(define suite
  (test-suite "queue worker death"
    (test-case "worker death reports failure and kills its active child"
      (define dir (make-temporary-file "queue-worker-death-~a" 'directory))
      (define ready (build-path dir "ready"))
      (define release (build-path dir "release"))
      (define late (build-path dir "late"))
      (define fixture (build-path dir "fixture.rkt"))
      ;; Bounded even if subprocess ownership is broken; release proves whether
      ;; the child survives after the coordinator should have handled death.
      (write-text fixture
                  (format (string-append
                           "#lang racket/base\n(call-with-output-file ~s #:exists 'truncate void)\n"
                           "(define deadline (+ (current-inexact-milliseconds) 15000))\n"
                           "(let loop () (cond [(file-exists? ~s) "
                           "(call-with-output-file ~s #:exists 'truncate void)] "
                           "[(< (current-inexact-milliseconds) deadline) (sleep 0.01) (loop)]))\n")
                          (path->string ready)
                          (path->string release)
                          (path->string late)))
      (define cust (make-custodian))
      (define results (make-async-channel))
      (define coordinator
        (parameterize ([current-custodian cust]
                       [current-subprocess-custodian-mode 'kill])
          (thread (lambda ()
                    (async-channel-put
                     results
                     (with-handlers ([exn? values])
                       (run-all-files (list (path->string fixture)) 1 30000 #:scheduler 'queue)))))))
      (dynamic-wind void
                    (lambda ()
                      (unless (wait-until (lambda () (file-exists? ready)))
                        (fail "fixture did not become ready"))
                      (define workers
                        (filter (lambda (t)
                                  (assq 'run-single-file/subprocess
                                        (continuation-mark-set->context (continuation-marks t))))
                                (descendant-threads cust (current-custodian))))
                      (unless (= (length workers) 1)
                        (fail (format "expected one active queue worker, found ~a" (length workers))))
                      (kill-thread (car workers))
                      (define outcome (sync/timeout 5 results))
                      (check-true (or (exn:fail? outcome)
                                      (and (list? outcome)
                                           (= (length outcome) 1)
                                           (not (zero? (test-file-result-exit-code (car outcome))))))
                                  "coordinator must terminate with an explicit failure, not deadlock")
                      (write-text release "")
                      (check-false (wait-until (lambda () (file-exists? late)) 2)
                                   "active child must not survive to write the late marker"))
                    (lambda ()
                      (custodian-shutdown-all cust)
                      (write-text release "")
                      (sync/timeout 5 coordinator)
                      ;; An escaped child has a finite lifetime; allow it to exit before
                      ;; removing its directory, without signalling a saved PID.
                      (sleep 16)
                      (delete-directory/files dir))))))

(module+ main
  (exit (run-tests suite)))
(module+ test
  (unless (zero? (run-tests suite))
    (error 'queue-worker-death "tests failed")))
