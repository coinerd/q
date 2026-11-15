#lang racket
;; @speed slow
;; @suite testing
;; @boundary integration
(require rackunit
         racket/runtime-path
         "../scripts/testing/quick-runner.rkt")

(define (write-text path text)
  (call-with-output-file path #:exists 'truncate (lambda (out) (display text out))))

;; Record the incarnation as well as the PID; zombies are no longer live.
(define (identity pid)
  (with-handlers ([exn:fail:filesystem? (lambda (_) #f)])
    (define line (file->string (format "/proc/~a/stat" pid)))
    (define m (regexp-match #rx"^[0-9]+ \\(.*\\) (.*)$" line))
    (define fields (and m (string-split (cadr m))))
    (and fields (>= (length fields) 20) (cons (car fields) (list-ref fields 19)))))

(define (live-incarnation? pid start)
  (define state (identity pid))
  (and state (equal? (cdr state) start) (not (member (car state) '("Z" "X")))))

(define (with-fixture proc)
  (define dir (make-temporary-file "qt-ownership-~a" 'directory))
  (dynamic-wind void (lambda () (proc dir)) (lambda () (delete-directory/files dir))))

(define (ownership-case timeout?)
  (with-fixture
   (lambda (dir)
     (define ready (build-path dir "ready"))
     (define ack (build-path dir "ack"))
     (define fixture (build-path dir "fixture.rkt"))
     ;; The descendant publishes its own PID before the fixture continues.
     (write-text fixture
                 (format #<<SOURCE
#lang racket
(require rackunit)
(module+ test
  (define-values (p out in err)
    (subprocess #f #f #f "/bin/sh" "-c"
                "echo $$ > \"$1.tmp\"; /bin/mv \"$1.tmp\" \"$1\"; exec /bin/sleep 300" "child" ~s))
  (close-output-port in)
  (let loop () (unless (file-exists? ~s) (sleep 0.01) (loop)))
  (let loop () (unless (file-exists? ~s) (sleep 0.01) (loop)))
  (check-true #t)
  ~a)
SOURCE
                         (path->string ready)
                         (path->string ready)
                         (path->string ack)
                         (if timeout? "(sync never-evt)" "(void)")))
     (define pid #f)
     (define start #f)
     (define worker #f)
     (dynamic-wind
      void
      (lambda ()
        (define result-channel (make-channel))
        (set! worker
              (thread (lambda ()
                        (channel-put
                         result-channel
                         (car (run-quick (list (path->string fixture)) 1 #:timeout 15))))))
        (define deadline (+ (current-inexact-monotonic-milliseconds) 60000))
        (let loop ()
          (unless (file-exists? ready)
            (when (> (current-inexact-monotonic-milliseconds) deadline)
              (error 'ownership-case "descendant did not become ready"))
            (sleep 0.01)
            (loop)))
        (set! pid (string->number (string-trim (file->string ready))))
        (unless (exact-positive-integer? pid)
          (error 'ownership-case "invalid descendant PID"))
        (define state (identity pid))
        (unless state
          (error 'ownership-case "descendant vanished before identity capture"))
        (set! start (cdr state))
        (write-text ack "ready")
        (define result (sync/timeout 30 result-channel))
        (check-not-false result "quick runner failed to return")
        (when result
          (if timeout?
              (begin
                (check-not-equal? (quick-result-exit result) 0)
                (check-regexp-match #rx"Command timed out" (quick-result-stderr result)))
              (check-true (quick-result-valid? result) (quick-result-stderr result)))
          (check-false (live-incarnation? pid start) "descendant survived quick runner return")))
      (lambda ()
        (when (and pid start (live-incarnation? pid start))
          (system* (find-executable-path "kill") "-KILL" (number->string pid)))
        (when worker
          (kill-thread worker)))))))

(module+ test
  (if (and (eq? (system-type 'os*) 'linux)
           (directory-exists? "/proc/self")
           (find-executable-path "setsid"))
      (begin
        (test-case "normal immediate exit cleans descendants"
          (ownership-case #f))
        (test-case "timeout cleans descendants"
          (ownership-case #t))
        (test-case "owners with main run the body and one entry point"
          (with-fixture (lambda (dir)
                          (define marker (build-path dir "marker"))
                          (define fixture (build-path dir "fixture.rkt"))
                          (write-text fixture
                                      (format #<<SOURCE
#lang racket
(require rackunit)
(define (mark s) (call-with-output-file ~s #:exists 'append (lambda (out) (displayln s out))))
(mark "body")
(module+ test (mark "test") (check-true #t))
(module+ main (mark "main"))
SOURCE
                                              (path->string marker)))
                          (define result (car (run-quick (list (path->string fixture)) 1)))
                          ;; Entry-point convention: module body, then exactly
                          ;; one submodule -- test when declared, else main.
                          (check-true (quick-result-valid? result))
                          (check-equal? (quick-result-total result) 1)
                          (check-equal? (file->lines marker) '("body" "test"))))))
      (displayln "SKIP qt process ownership: Linux /proc or setsid unavailable")))
