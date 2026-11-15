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
         racket/string
         racket/system
         "../scripts/run-tests.rkt")

(define (wait-until pred [seconds 15])
  (define deadline (+ (current-inexact-milliseconds) (* seconds 1000)))
  (let loop ()
    (cond
      [(pred) #t]
      [(>= (current-inexact-milliseconds) deadline) #f]
      [else
       (sync/timeout 0.01 never-evt)
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
  (test-suite "queue cancellation truth"
    (test-case "interrupting a worker after recording cannot duplicate its result"
      (define dir (make-temporary-file "queue-interrupt-~a" 'directory))
      (define started (build-path dir "started"))
      (define release (build-path dir "release"))
      (define fixture (build-path dir "fixture.rkt"))
      (write-text
       fixture
       (format
        "#lang racket/base\n(call-with-output-file ~s #:exists 'truncate void)\n(let loop () (unless (file-exists? ~s) (sleep 0.01) (loop)))\n"
        (path->string started)
        (path->string release)))
      (define cust (make-custodian))
      (define output (open-output-string))
      (define results (make-async-channel))
      (define coordinator
        (parameterize ([current-custodian cust]
                       [current-subprocess-custodian-mode 'kill]
                       [current-output-port output])
          (thread (lambda ()
                    (async-channel-put
                     results
                     (with-handlers ([exn? values])
                       (run-all-files (list (path->string fixture)) 1 10000 #:scheduler 'queue)))))))
      (dynamic-wind
       void
       (lambda ()
         (check-true (wait-until (lambda () (file-exists? started))) "fixture started")
         (thread-suspend coordinator)
         (write-text release "")
         (check-true (wait-until (lambda () (string-contains? (get-output-string output) ".")))
                     "worker recorded completion while coordinator is suspended")
         ;; The coordinator cannot receive done-ch, so its worker is blocked
         ;; after adding its result. Interrupt exactly that worker.
         (define (blocked-workers)
           (filter (lambda (t)
                     (and (not (eq? t coordinator))
                          (assq 'channel-put
                                (continuation-mark-set->context (continuation-marks t)))))
                   (descendant-threads cust (current-custodian))))
         (unless (wait-until (lambda () (= (length (blocked-workers)) 1)))
           (fail "worker did not reach completion-channel handoff"))
         (break-thread (car (blocked-workers)))
         (thread-resume coordinator)
         (define outcome (sync/timeout 15 results))
         (check-pred list? outcome "interrupted queue returns results, not an exception")
         (when (list? outcome)
           (check-equal? (length outcome) 1 "one result per input, even across interruption")))
       (lambda ()
         (thread-resume coordinator)
         (custodian-shutdown-all cust)
         (sync/timeout 5 coordinator)
         (delete-directory/files dir))))
    (test-case "coordinator cancellation propagates and kills same-group descendants"
      (define dir (make-temporary-file "queue-descendant-~a" 'directory))
      (define started (build-path dir "started"))
      (define late (build-path dir "late"))
      (define child-ready (build-path dir "child-ready"))
      (define fixture (build-path dir "fixture.rkt"))
      (define child (build-path dir "child.rkt"))
      (write-text
       child
       (format (string-append
                "#lang racket/base\n(call-with-output-file ~s #:exists 'truncate void)\n(sleep 4)\n"
                "(call-with-output-file ~s #:exists 'truncate void)\n(sleep 1)\n")
               (path->string child-ready)
               (path->string late)))
      (write-text
       fixture
       (format
        (string-append
         "#lang racket/base\n(require racket/system)\n"
         "(define-values (p o i e) (subprocess #f #f #f (find-executable-path \"racket\") ~s))\n"
         "(call-with-output-file ~s #:exists 'truncate "
         "(lambda (out) (display (subprocess-pid p) out)))\n"
         "(sleep 30)\n")
        (path->string child)
        (path->string started)))
      (define cust (make-custodian))
      (define results (make-async-channel))
      (define coordinator
        (parameterize ([current-custodian cust]
                       [current-subprocess-custodian-mode 'kill])
          (thread (lambda ()
                    (async-channel-put
                     results
                     (with-handlers ([exn? values])
                       (run-all-files (list (path->string fixture)) 1 20000 #:scheduler 'queue)))))))
      (dynamic-wind void
                    (lambda ()
                      (check-true (wait-until (lambda () (file-exists? started)))
                                  "descendant spawned")
                      (check-true (wait-until (lambda () (file-exists? child-ready)))
                                  "descendant running before cancellation")
                      (break-thread coordinator)
                      (define outcome (sync/timeout 10 results))
                      (check-pred exn:break? outcome "cancellation propagates to caller")
                      (check-false (wait-until (lambda () (file-exists? late)) 6)
                                   "same-group descendant must not survive cancellation"))
                    (lambda ()
                      ;; Do not signal a saved numeric PID: it may have been reused.
                      ;; The descendant's finite lifetime is at most five seconds.
                      (custodian-shutdown-all cust)
                      (sync/timeout 5 coordinator)
                      (sleep 6)
                      (delete-directory/files dir))))))

(module+ main
  (exit (run-tests suite)))
(module+ test
  (unless (zero? (run-tests suite))
    (error 'queue-cancellation-truth "tests failed")))
