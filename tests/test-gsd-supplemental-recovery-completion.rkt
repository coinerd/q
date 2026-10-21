#lang racket
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 1200
;; @covers extensions/gsd/supplemental-evidence-promotion.rkt
;; @covers extensions/gsd/supplemental-evidence-recovery.rkt
;; Isolated engineering evidence, never live readiness or review admission.
(require rackunit
         json
         racket/file
         racket/path
         "helpers/supplemental-promotion-fixture.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/supplemental-evidence-journal.rkt"
         "../extensions/gsd/supplemental-evidence-format.rkt"
         "../extensions/gsd/supplemental-evidence-promotion.rkt"
         "../extensions/gsd/supplemental-evidence-publication.rkt"
         (only-in "../extensions/gsd/evidence-readiness.rkt" current-evidence-context)
         (only-in "../extensions/gsd/evidence-repair-ledger.rkt" load-evidence-repair)
         (only-in "../extensions/gsd/evidence-repair-format.rkt" canonical-evidence-digest)
         (only-in "../extensions/gsd/review-proof.rkt" load-receipt-owned-proof)
         (only-in "../extensions/gsd/evidence-probe.rkt"
                  validate-baseline-probe!
                  prepare-baseline-probe!))
(module+ test
  (test-case "claimed v2 child completes the genuine GREEN/r*/E/R1/CAS/P registration ladder"
    (with-certified-promotion-fixture
     (lambda (env)
       (define root (hash-ref env 'root))
       (define approval (hash-ref env 'aref))
       (define staged (hash-ref env 'staged))
       (define context (hash-ref env 'context))
       (define receipt (hash-ref env 'r2))
       (define red (hash-ref env 'red-ref))
       (define sref (hash-ref env 'sref))
       (define sapref (hash-ref env 'sapref))
       (define (check-parent!)
         (check-equal? (hash-ref (load-delivery-journal root fixture-plan 1) 'evidence-author-repair)
                       (hash-ref env 'parent-marker))
         (for ([entry (hash-ref env 'parent-snapshot)])
           (check-equal? (file->bytes (car entry)) (cdr entry))))
       (check-equal? (hash-ref (hash-ref env 'task) 'kind) "supplemental-evidence-task/v2")
       (check-parent!)
       (check-equal? (hash-ref (load-supplemental-operation approval) 'state) "receipt-certified")
       (define journal (load-delivery-journal root fixture-plan 1))
       (check-equal? (hash-ref journal 'receipt) receipt)
       (check-equal? (hash-ref journal 'receipt-history) (list (hash-ref env 'r0) (hash-ref env 'r1)))
       (check-equal? (hash-ref context 'receipt-sha256) (canonical-evidence-digest receipt))
       (check-equal? (current-evidence-context root fixture-plan 1) context)
       (define physical (path->string (hash-ref env 'repo)))
       (define logical (hash-ref receipt 'repo))
       (check-not-equal? logical physical)
       (check-equal? (hash-ref context 'repo) physical)
       (printf "linked identities: receipt.repo=~s E.repo=~s directory-port=~s\n"
               logical
               physical
               (path->string (path->directory-path physical)))
       ;; Both historical ordinary proof and current supplemental G1 retain
       ;; logical receipt identity and authenticate the physical execution port.
       (for ([r (list (hash-ref env 'r0) (hash-ref env 'r1) receipt)])
         (define owned (load-receipt-owned-proof r root fixture-plan 1))
         (check-equal? (hash-ref r 'repo) logical)
         (check-equal? (hash-ref (hash-ref owned 'binding) 'repo) logical)
         (check-equal? (hash-ref owned 'execution-cwd) physical))
       (define old-journal
         (hash-set* journal 'receipt (hash-ref env 'r1) 'receipt-history (list (hash-ref env 'r0))))
       (check-equal? (predict-supplemental-context old-journal
                                                   receipt
                                                   (hash-ref env 'frozen)
                                                   fixture-plan
                                                   1
                                                   #:base root)
                     context)
       ;; Point the supplemental reference at a real, retained but different
       ;; manifest, rather than merely checking a missing-file refusal.
       (define bad-proof-receipt
         (hash-set receipt
                   'owned-proof
                   (hash-set (hash-ref receipt 'owned-proof)
                             'sha256
                             (hash-ref (hash-ref (hash-ref env 'r1) 'owned-proof) 'sha256))))
       (check-exn exn:fail?
                  (lambda ()
                    (predict-supplemental-context old-journal
                                                  bad-proof-receipt
                                                  (hash-ref env 'frozen)
                                                  fixture-plan
                                                  1
                                                  #:base root)))
       (validate-baseline-probe! physical red context)
       (check-exn #rx"context mismatch"
                  (lambda ()
                    (validate-baseline-probe! physical red (hash-set context 'repo logical))))
       (define probe (call-with-input-file (hash-ref red 'path) read-json))
       (define spec (call-with-input-file (hash-ref (hash-ref probe 'spec) 'path) read-json))
       (define id (hash-ref probe 'operation-id))
       ;; Refuse the logical checkout even though it shares Git objects with
       ;; the physical checkout. Refuse a wrong context at the correct port too.
       (check-exn #rx"repository mismatch" (lambda () (prepare-baseline-probe! logical spec id)))
       (check-exn #rx"repository mismatch"
                  (lambda ()
                    (prepare-baseline-probe! physical
                                             (hash-set spec 'context (hash-set context 'repo logical))
                                             id)))
       ;; A directory separator changes no path identity: all spec checks pass
       ;; before the existing operation is refused, without launching a probe.
       (check-exn #rx"operation already prepared"
                  (lambda () (prepare-baseline-probe! (path->directory-path physical) spec id)))
       (check-equal? (hash-ref (load-evidence-repair root fixture-plan 1 2) 'registrations) '())
       (define promotion (supplemental-promotion-record staged sref sapref))
       (check-true (valid-supplemental-record? promotion))
       (check-equal? (hash-ref promotion 'context) context)
       (check-equal? (hash-ref promotion 'red-proof-ref) red)
       (define readiness (promote-supplemental-evidence! root fixture-plan 1 sapref (lambda () #t)))
       (check-equal? (hash-ref readiness 'status) "ready")
       (check-equal? (hash-ref readiness 'code) "registered-probe-ready")
       (define ledger (load-evidence-repair root fixture-plan 1 2))
       (define registrations (hash-ref ledger 'registrations))
       (check-equal? (length registrations) 1)
       (define registration (car registrations))
       (check-equal? (hash-ref registration 'context) context)
       (check-equal? (hash-ref registration 'probe) red)
       (check-equal? (hash-ref readiness 'registration) registration)
       (check-equal? (hash-ref (hash-ref ledger 'operation) 'promoted)
                     (hasheq 'promotion (hash-ref promotion 'sha256) 'generation 2))
       (check-equal? (hash-ref (load-supplemental-operation approval) 'state) "completed")
       (check-parent!)
       (define replay (recover-supplemental-promotion! root fixture-plan 1 sapref (lambda () #t)))
       (check-equal? (hash-ref replay 'registration) registration)
       (check-equal? (hash-ref (load-evidence-repair root fixture-plan 1 2) 'registrations)
                     registrations)
       (check-parent!))
     #:recovery? #t
     #:physical-worktree? #t)))
