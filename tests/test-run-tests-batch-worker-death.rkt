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
  (test-suite "scheduler worker death"
    (test-case "a lost selected file cannot produce a successful suite"
      (define dir (make-temporary-file "batch-worker-death-~a" 'directory))
      (define ready (build-path dir "ready"))
      (define passed (build-path dir "passed"))
      (define release (build-path dir "release"))
      (define late (build-path dir "late"))
      (define blocked (build-path dir "blocked.rkt"))
      (define passing (build-path dir "passing.rkt"))
      (define later (build-path dir "later.rkt"))
      (define later-started (build-path dir "later-started"))
      (write-text
       blocked
       (format (string-append "#lang racket/base\n"
                              "(call-with-output-file ~s #:exists 'truncate void)\n"
                              "(define deadline (+ (current-inexact-milliseconds) 15000))\n"
                              "(let loop () (when (and (not (file-exists? ~s)) "
                              "(< (current-inexact-milliseconds) deadline)) (sleep 0.01) (loop)))\n"
                              "(call-with-output-file ~s #:exists 'truncate void)\n")
               (path->string ready)
               (path->string release)
               (path->string late)))
      (write-text passing
                  (format (string-append "#lang racket/base\n"
                                         "(require rackunit)\n"
                                         "(module+ test (check-equal? (+ 1 1) 2) "
                                         "(call-with-output-file ~s #:exists 'truncate void))\n")
                          (path->string passed)))
      (write-text later
                  (format "#lang racket/base\n(call-with-output-file ~s #:exists 'truncate void)\n"
                          (path->string later-started)))
      (define selected (map path->string (list blocked passing later)))
      (define cust (make-custodian))
      (define results (make-async-channel))
      (define coordinator
        (parameterize ([current-custodian cust]
                       [current-subprocess-custodian-mode 'kill])
          (thread (lambda ()
                    (async-channel-put
                     results
                     (with-handlers ([exn? values])
                       (call-with-values
                        (lambda ()
                          ;; The deterministic batch barrier is the only scheduler.
                          (run-suite-once selected 2 30000 #f "explicit" 1 1 'subprocess #f #f 'vps))
                        list)))))))
      ;; A worker is "active" while it is inside the runner's per-file
      ;; execution path (`run-single-file`, its subprocess executor, or the
      ;; result builder that waits on the child). The marker set is named rather
      ;; than pinned to one internal frame: §11.3 folded the execution modes into
      ;; a single subprocess path, which changes which frames are reified while a
      ;; child is awaited.
      (define (active-workers)
        (define entry-frames '(run-single-file run-single-file/subprocess build-result-from-process))
        (filter (lambda (t)
                  (for/or ([frame (in-list (continuation-mark-set->context (continuation-marks t)))])
                    (and (pair? frame) (memq (car frame) entry-frames))))
                (descendant-threads cust (current-custodian))))
      (dynamic-wind
       void
       (lambda ()
         (unless (wait-until
                  (lambda ()
                    (and (file-exists? ready) (file-exists? passed) (= (length (active-workers)) 1))))
           (fail (format
                  "fixtures did not reach one blocked worker after the passing fixture ran (~a)"
                  (hasheq 'ready
                          (file-exists? ready)
                          'passed
                          (file-exists? passed)
                          'workers
                          (length (active-workers))
                          'contexts
                          (for/list ([t (in-list (descendant-threads cust (current-custodian)))])
                            (continuation-mark-set->context (continuation-marks t)))))))
         (kill-thread (car (active-workers)))
         ;; The shared work-conserving queue (final plan §2) dispatches any
         ;; eligible later file once a worker frees up — so `later` MAY have
         ;; started by now. What MUST hold is fail-closed termination: the
         ;; lost worker terminates the whole run with an error (never a
         ;; partial green), and the killed child never survives.
         (define outcome (sync/timeout 5 results))
         (unless outcome
           (fail "coordinator did not terminate after a worker was lost"))
         (check-true (exn? outcome)
                     "losing a selected file must fail the suite, never a partial result")
         (when (exn? outcome)
           (check-regexp-match #rx"(worker terminated|missing worker results|missing result)"
                               (exn-message outcome)))
         (write-text release "")
         (check-false (wait-until (lambda () (file-exists? late)) 2)
                      "worker death must not leave the child running"))
       (lambda ()
         (custodian-shutdown-all cust)
         (write-text release "")
         (sync/timeout 5 coordinator)
         ;; An escaped subprocess is bounded by its own deadline; no saved PID is signalled.
         (sleep 16)
         (delete-directory/files dir))))))

(module+ main
  (exit (run-tests suite)))
(module+ test
  (unless (zero? (run-tests suite))
    (error 'batch-worker-death "tests failed")))
