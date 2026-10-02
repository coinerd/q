#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers extensions/gsd/delivery-finalize.rkt
;; D1 regression (v1.00.33 audit, §16): record-attempt-delivery-provenance!
;; is documented to return "the recorded (branch . head) or #f when no real
;; provenance exists". The verification-context side effect must not change
;; that contract — these tests pin the success and refusal return values.
;; record-wave-delivery! is best-effort persistence (warns without a campaign
;; record); the contract under test is the return value, not the side effect.
(require rackunit
         racket/file
         "../extensions/gsd/delivery-finalize.rkt"
         "../extensions/gsd/delivery-journal.rkt")
(define plan (make-string 64 #\a))
(define head (make-string 40 #\b))
(define receipt
  (hasheq 'repo
          "/repo/q"
          'branch
          "campaign/w2"
          'head
          head
          'tree
          (make-string 40 #\c)
          'origin
          "https://github.com/example/q.git"
          'verified-at
          1
          'evidence
          "full Verify passed"
          'attempt-id
          "attempt-9"
          'attempt-fence
          3))
(define (with-root f)
  (define root (make-temporary-file "delivery-finalize-~a" 'directory))
  (dynamic-wind void (lambda () (f root)) (lambda () (delete-directory/files root))))
(module+ test
  (test-case "receipt-path success returns (branch . head) — not void (D1 regression)"
    (with-root (lambda (root)
                 (record-delivery-receipt! root plan 2 receipt)
                 (define result (record-attempt-delivery-provenance! root plan 2 "attempt-9" 3))
                 (check-true (pair? result) "documented contract: (branch . head) pair")
                 (check-equal? result (cons "campaign/w2" head)))))
  (test-case "refusal: receipt identity mismatch (attempt-id, fence) returns #f"
    (with-root (lambda (root)
                 (record-delivery-receipt! root plan 2 receipt)
                 (check-false (record-attempt-delivery-provenance! root plan 2 "attempt-8" 3))
                 (check-false (record-attempt-delivery-provenance! root plan 2 "attempt-9" 4)))))
  (test-case "refusal: no journal / no provenance at all returns #f"
    (with-root (lambda (root)
                 (check-false (record-attempt-delivery-provenance! root plan 2 "attempt-9" 3)))))
  (define vc
    (hasheq 'base
            (make-string 40 #\d)
            'merge-sha
            (make-string 40 #\e)
            'pr-head
            (make-string 40 #\f)
            'branch
            "binding/plan-w2"
            'repo-root
            "/repo/q"
            'verified-at
            1
            'snapshot-refs
            (list "origin/main" "origin/pr/9768")))
  (test-case "success with verification context: exact (branch . head) return, context persisted"
    (with-root
     (lambda (root)
       (record-delivery-receipt! root plan 2 receipt)
       (define result
         (record-attempt-delivery-provenance! root plan 2 "attempt-9" 3 #:verification-context vc))
       (check-equal? result (cons "campaign/w2" head))
       (check-equal? (hash-ref (load-delivery-journal root plan 2) 'verification-context #f) vc))))
  (test-case "refusal with verification context: #f return, context still persisted"
    (with-root
     (lambda (root)
       (record-delivery-receipt! root plan 2 receipt)
       (check-false
        (record-attempt-delivery-provenance! root plan 2 "attempt-8" 3 #:verification-context vc))
       (check-equal? (hash-ref (load-delivery-journal root plan 2) 'verification-context #f) vc))))
  (test-case "context persistence without provenance raises separately (no return value)"
    (with-root (lambda (root)
                 (check-exn (lambda (e)
                              (and (exn:fail? e)
                                   (regexp-match? #rx"missing verified provenance" (exn-message e))))
                            (lambda ()
                              (record-attempt-delivery-provenance! root
                                                                   plan
                                                                   2
                                                                   "attempt-9"
                                                                   3
                                                                   #:verification-context vc)))))))
