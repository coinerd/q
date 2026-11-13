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
  (test-suite "queue deferred idle break"
    (test-case "a break after publication does not consume the next item"
      (define dir (make-temporary-file "queue-idle-break-~a" 'directory))
      (define ready (build-path dir "ready"))
      (define release (build-path dir "release"))
      (define starts (build-path dir "starts"))
      (define first (build-path dir "first.rkt"))
      (define second (build-path dir "second.rkt"))
      (write-text
       first
       (format (string-append "#lang racket/base\n"
                              "(call-with-output-file ~s #:exists 'truncate void)\n"
                              "(define deadline (+ (current-inexact-milliseconds) 15000))\n"
                              "(let loop () (unless (or (file-exists? ~s) "
                              "(>= (current-inexact-milliseconds) deadline)) "
                              "(sleep 0.01) (loop)))\n")
               (path->string ready)
               (path->string release)))
      (write-text second
                  (format (string-append "#lang racket/base\n"
                                         "(call-with-output-file ~s #:exists 'append "
                                         "(lambda (out) (displayln \"start\" out)))\n")
                          (path->string starts)))
      (define files (map path->string (list first second)))
      (define cust (make-custodian))
      (define results (make-async-channel))
      (define output (open-output-bytes))
      (define coordinator
        (parameterize ([current-custodian cust]
                       [current-output-port output]
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
           (fail (format "expected one worker, found ~a" (length workers))))
         (define worker (car workers))
         (thread-suspend coordinator)
         (write-text release "")
         (unless (wait-until (lambda () (positive? (bytes-length (get-output-bytes output)))))
           (fail "first result was not reported"))
         (unless (wait-until (lambda ()
                               (assq 'channel-put
                                     (continuation-mark-set->context (continuation-marks worker)))))
           (fail "worker did not publish and block on completion"))
         (check-false (file-exists? starts) "second item not started")
         (break-thread worker)
         (thread-resume coordinator)
         (define outcome (sync/timeout 25 results))
         (check-pred list? outcome "queue returns results")
         (when (list? outcome)
           (check-equal? (map test-file-result-path outcome) files "one result per item")
           (check-equal? (map test-file-result-exit-code outcome) '(0 0) "both items pass"))
         (check-true (file-exists? starts) "second item ran")
         (when (file-exists? starts)
           (check-equal? (file->lines starts) '("start") "second item ran once")))
       (lambda ()
         (custodian-shutdown-all cust)
         (sync/timeout 5 coordinator)
         (delete-directory/files dir))))))

(module+ main
  (exit (run-tests suite)))
(module+ test
  (unless (zero? (run-tests suite))
    (error 'queue-idle-break-truth "tests failed")))
