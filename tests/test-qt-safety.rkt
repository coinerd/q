#lang racket
;; @speed fast
;; @suite testing
;; @boundary integration
(require rackunit
         racket/runtime-path
         "../scripts/testing/quick-audit.rkt"
         "../scripts/testing/quick-runner.rkt")
(define-runtime-path root "..")
(module+ test
  (test-case "audit rejects changed content and symlinks"
    (define dir (make-temporary-file "qt-audit-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (make-directory (build-path dir "tests"))
       (make-directory (build-path dir "util"))
       (copy-file (build-path root "util/exn.rkt") (build-path dir "util/exn.rkt"))
       (define entry (car audited))
       (define target (build-path dir (car entry)))
       (copy-file (build-path root (car entry)) target)
       (check-true (audit-ok? dir entry))
       (call-with-output-file target #:exists 'append (lambda (out) (display "; drift" out)))
       (check-false (audit-ok? dir entry))
       (delete-file target)
       (define linked?
         (with-handlers ([exn:fail:filesystem? (lambda (_) #f)])
           (make-file-or-directory-link (build-path root (car entry)) target)
           #t))
       (if linked?
           (check-false (audit-ok? dir entry))
           (displayln "SKIP audit symlink: link creation unavailable")))
     (lambda () (delete-directory/files dir))))
  (test-case "early exit and forged summary cannot pass"
    (if (and (eq? (system-type 'os*) 'linux)
             (directory-exists? "/proc/self")
             (find-executable-path "setsid"))
        (let ([dir (make-temporary-file "qt-child-~a" 'directory)])
          (dynamic-wind
           void
           (lambda ()
             (define early (build-path dir "early.rkt"))
             (define forged (build-path dir "forged.rkt"))
             (call-with-output-file early
                                    (lambda (out) (display "#lang racket/base\n(exit 0)\n" out)))
             (call-with-output-file
              forged
              (lambda (out) (display "#lang racket/base\n(displayln \"1 test passed\")\n" out)))
             (define results (run-quick (map path->string (list early forged)) 2))
             (check-equal? (length results) 2)
             (for ([r results])
               (check-false (quick-result-valid? r))))
           (lambda () (delete-directory/files dir))))
        (displayln "SKIP early exit and forged summary: Linux /proc or setsid unavailable"))))
