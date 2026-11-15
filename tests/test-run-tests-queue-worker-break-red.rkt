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
  (test-suite "queue worker break subprocess ownership"
    (test-case "worker break kills the active descendant before launching the next file"
      (define dir (make-temporary-file "queue-worker-break-~a" 'directory))
      (define ready (build-path dir "ready"))
      (define release (build-path dir "release"))
      (define late (build-path dir "late"))
      (define launched (build-path dir "launched"))
      (define first (build-path dir "first.rkt"))
      (define second (build-path dir "second.rkt"))
      ;; The first fixture has a finite lifetime even if ownership is broken.
      ;; Only the second fixture releases it during the actual test.
      (write-text first
                  (format (string-append
                           "#lang racket/base\n(call-with-output-file ~s #:exists 'truncate void)\n"
                           "(define deadline (+ (current-inexact-milliseconds) 15000))\n"
                           "(let loop () (cond [(file-exists? ~s) "
                           "(call-with-output-file ~s #:exists 'truncate void)] "
                           "[(< (current-inexact-milliseconds) deadline) (sleep 0.01) (loop)]))\n")
                          (path->string ready)
                          (path->string release)
                          (path->string late)))
      (write-text
       second
       (format
        "#lang racket/base\n(call-with-output-file ~s #:exists 'truncate void)\n(call-with-output-file ~s #:exists 'truncate void)\n(sleep 4)\n"
        (path->string launched)
        (path->string release)))
      (define files (map path->string (list first second)))
      (define cust (make-custodian))
      (define results (make-async-channel))
      (define coordinator
        (parameterize ([current-custodian cust]
                       [current-subprocess-custodian-mode 'kill])
          (thread (lambda ()
                    (async-channel-put results
                                       (with-handlers ([exn? values])
                                         (run-all-files files 1 20000 #:scheduler 'queue)))))))
      (dynamic-wind
       void
       (lambda ()
         (unless (wait-until (lambda () (file-exists? ready)))
           (fail "first fixture did not become ready"))
         (define workers
           (filter (lambda (t)
                     (assq 'run-single-file/subprocess
                           (continuation-mark-set->context (continuation-marks t))))
                   (descendant-threads cust (current-custodian))))
         (unless (= (length workers) 1)
           (fail (format "expected one queue worker, found ~a" (length workers))))
         (break-thread (car workers))
         (check-true (wait-until (lambda () (file-exists? launched))) "second fixture launched")
         ;; The second fixture releases the first and remains alive, so a marker
         ;; here proves the first survived into the next work item.
         (define survived? (wait-until (lambda () (file-exists? late)) 2))
         (define outcome (sync/timeout 15 results))
         (check-pred list? outcome "queue returns results")
         (when (list? outcome)
           (check-equal? (map test-file-result-path outcome) files "exactly one result per input")
           (check-equal? (map test-file-result-exit-code outcome)
                         '(1 0)
                         "interrupted file fails and subsequent file succeeds"))
         (check-false (or survived? (file-exists? late))
                      "first descendant must be killed before the second starts; no late marker"))
       (lambda ()
         ;; No PID-based signals: release any escaped fixture, then allow its
         ;; finite body to finish before removing the temporary directory.
         (custodian-shutdown-all cust)
         (write-text release "")
         (sync/timeout 5 coordinator)
         (sleep 5)
         (delete-directory/files dir))))))

(module+ main
  (exit (run-tests suite)))
(module+ test
  (unless (zero? (run-tests suite))
    (error 'queue-worker-break-red "tests failed")))
