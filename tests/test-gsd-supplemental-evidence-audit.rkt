#lang racket/base
;; @speed fast
;; @suite extensions
;; @boundary unit
;; @covers extensions/gsd/review-audit-format.rkt
(require rackunit
         "../extensions/gsd/review-audit-format.rkt")
(define sha (make-string 64 #\a))
(define other (make-string 64 #\b))
(define authority
  (hasheq 'reference (hasheq 'supplemental-contract (hasheq)) 'supplemental-promotion-sha256 sha))
(define request
  (hasheq 'owned-authority
          authority
          'evidence-revision
          1
          'evidence-registration-sha256
          other
          'supplemental-promotion-sha256
          sha))
(module+ test
  (test-case "supplemental request and ACK explicitly bind P"
    (define binding (audit-binding request))
    (check-equal? (hash-ref binding 'supplemental-promotion-sha256 #f) sha)
    (define ack
      (hasheq 'mode
              owned-audit-mode
              'verification-origin
              "audited-owned"
              'source-read
              #t
              'raw-output-read
              #t
              'binding
              binding))
    (check-true (valid-audit-ack? ack binding))
    (check-false (valid-audit-ack?
                  (hash-set ack 'binding (hash-remove binding 'supplemental-promotion-sha256))
                  binding)))
  (test-case "supplemental request cannot omit or substitute P"
    (check-exn exn:fail?
               (lambda () (audit-binding (hash-remove request 'supplemental-promotion-sha256))))
    (check-exn exn:fail?
               (lambda () (audit-binding (hash-set request 'supplemental-promotion-sha256 other))))
    (check-exn exn:fail?
               (lambda () (audit-binding (hash-set request 'supplemental-promotion-sha256 "bad")))))
  (test-case "ordinary old bindings remain unchanged"
    (define old (audit-binding (hasheq 'head "old")))
    (check-false (hash-has-key? old 'supplemental-promotion-sha256))
    (check-equal? (hash-count old) 10)))
