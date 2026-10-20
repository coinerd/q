#lang racket/base
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 600
;; @covers scripts/gsd-binding-data.rkt scripts/gsd-gates/owned-audit.rkt
;; @covers extensions/gsd/review-audit-promotion.rkt
(require rackunit
         racket/file
         "helpers/supplemental-promotion-fixture.rkt"
         "../extensions/gsd/supplemental-evidence-promotion.rkt"
         "../extensions/gsd/review-audit-promotion.rkt"
         "../scripts/gsd-binding-data.rkt"
         "../scripts/gsd-gates/owned-audit.rkt")
(module+ test
  (test-case "real owner-promoted P is exported and offline-validated against the trusted request"
    (with-certified-promotion-fixture
     (lambda (env)
       (define root (hash-ref env 'root))
       (promote-supplemental-evidence! root fixture-plan 1 (hash-ref env 'sapref) (lambda () #t))
       (define staging (make-temporary-file "audit-p-~a" 'directory))
       (dynamic-wind
        void
        (lambda ()
          (define authority (owned-review-authority root fixture-plan 1 staging))
          (define sha (audit-promotion-sha256 authority))
          (check-true (string? sha))
          (check-exn exn:fail?
                     (lambda ()
                       (audit-promotion-sha256
                        (hash-set authority 'supplemental-promotion-sha256 (make-string 64 #\b)))))
          (check-exn exn:fail?
                     (lambda () (audit-promotion-sha256 (hash-set authority 'generation 999))))
          (check-exn exn:fail?
                     (lambda ()
                       (audit-promotion-sha256 (hash-set authority
                                                         'evidence-registration
                                                         (hash-set (hash-ref authority
                                                                             'evidence-registration)
                                                                   'operation-id
                                                                   "wrong-operation")))))
          (check-true (regexp-match? #rx"tests/test-witness[.]rkt$"
                                     (hash-ref (hash-ref (hash-ref authority 'green-gates)
                                                         'focused-tests)
                                               'command)))
          (validate-owned-authority staging
                                    authority
                                    #:source-repo (hash-ref (hash-ref authority 'evidence-context)
                                                            'repo))
          (for ([key '(supplemental-promotion supplemental-promotion-sha256)])
            (check-exn exn:fail?
                       (lambda () (validate-owned-authority staging (hash-remove authority key))))))
        (lambda () (delete-directory/files staging)))))))
