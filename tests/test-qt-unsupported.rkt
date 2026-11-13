#lang racket
;; @speed fast
;; @suite testing
;; @boundary integration
(require rackunit
         "../scripts/testing/quick-runner.rkt")

(module+ test
  (test-case "missing setsid fails closed before executing the fixture"
    (define dir (make-temporary-file "qt-unsupported-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (define marker (build-path dir "executed"))
       (define fixture (build-path dir "fixture.rkt"))
       (call-with-output-file
        fixture
        (lambda (out)
          (fprintf out "#lang racket\n(call-with-output-file ~s void)\n" (path->string marker))))
       (define env (environment-variables-copy (current-environment-variables)))
       (environment-variables-set! env #"PATH" (path->bytes dir))
       (parameterize ([current-environment-variables env])
         (check-false (find-executable-path "setsid"))
         (check-exn #rx"owned process group cleanup unavailable"
                    (lambda () (run-quick (list (path->string fixture)) 1)))
         (check-false (file-exists? marker))))
     (lambda () (delete-directory/files dir)))))
