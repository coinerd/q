#lang racket/base
;; @suite testing
;; @speed fast
;; @boundary unit
(require rackunit
         racket/file
         racket/port
         "../scripts/run-tests/runner.rkt")
(module+ test
  (test-case "grouped silent execution is not replayed"
    (define dir (make-temporary-file "grouped-once-~a" 'directory))
    (define marker (build-path dir "marker"))
    (define fixture (build-path dir "fixture.rkt"))
    (dynamic-wind
     (lambda ()
       (display-to-file
        (format
         "#lang racket/base\n(module+ test (call-with-output-file ~s #:exists 'append (lambda (o) (display \"x\" o))))\n"
         (path->string marker))
        fixture))
     (lambda ()
       (run-single-file fixture #:mode 'grouped-in-process #:timeout 5000)
       (check-equal? (file->string marker) "x"))
     (lambda () (delete-directory/files dir))))
  (test-case "queue cancellation stops an already started child"
    (define dir (make-temporary-file "queue-cancel-~a" 'directory))
    (define started (build-path dir "started"))
    (define marker (build-path dir "marker"))
    (define fixture (build-path dir "fixture.rkt"))
    (dynamic-wind
     (lambda ()
       (display-to-file
        (format
         "#lang racket/base\n(call-with-output-file ~s void)\n(sleep 2)\n(call-with-output-file ~s void)\n"
         (path->string started)
         (path->string marker))
        fixture))
     (lambda ()
       (define coordinator
         (thread (lambda ()
                   (with-handlers ([exn:break? void])
                     (run-all-files (list fixture) 1 10000 #:scheduler 'queue)))))
       (let loop ([n 100])
         (unless (or (file-exists? started) (zero? n))
           (sleep 0.05)
           (loop (sub1 n))))
       (check-true (file-exists? started))
       (break-thread coordinator)
       (check-not-false (sync/timeout 5 coordinator))
       (sleep 3)
       (check-false (file-exists? marker)))
     (lambda () (delete-directory/files dir)))))
