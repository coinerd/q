#lang racket
;; @speed fast
;; @suite testing
;; @boundary unit
;; @covers scripts/testing/quick-child.rkt
(require rackunit
         racket/runtime-path)
(define-runtime-path child "../scripts/testing/quick-child.rkt")
(module+ test
  (test-case "printed summary does not produce positive RackUnit evidence"
    (define dir (make-temporary-file "qt-forged-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (define fixture (build-path dir "forged.rkt"))
       (define evidence (build-path dir "evidence"))
       (call-with-output-file fixture
                              #:exists 'truncate
                              (lambda (out)
                                (display "#lang racket/base\n(displayln \"1 test passed\")\n" out)))
       (define-values (proc out in err)
         (subprocess #f #f #f (find-executable-path "racket") child fixture evidence))
       (close-output-port in)
       (port->string out)
       (port->string err)
       (subprocess-wait proc)
       (check-true (file-exists? evidence))
       (define record (call-with-input-file evidence read))
       (check-equal? (last record) 0))
     (lambda () (delete-directory/files dir)))))
