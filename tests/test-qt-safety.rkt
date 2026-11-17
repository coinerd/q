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
  (test-case "the committed record gates every owner through audit-ok?"
    ;; The executable contract under the generated admission record: every
    ;; table owner is admitted against the shipped tree. The old
    ;; changed-content/symlink mechanics live in tests/test-testing-audit.rkt
    ;; (stale record, structure drift, symlink refusal, denylist hits).
    (check-true (andmap (lambda (owner) (audit-ok? root owner))
                        (admitted-owners))))
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
