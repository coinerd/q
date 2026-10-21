#lang racket
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 300
(require rackunit
         "helpers/supplemental-go-fixture.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/delivery-receipt.rkt"
         "../extensions/gsd/review-proof.rkt"
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         "../extensions/gsd/supplemental-evidence-controller.rkt"
         "../extensions/gsd/supplemental-evidence-journal.rkt"
         "../extensions/gsd/wave-runner-port.rkt")

(define (fixture-grant fx)
  (hash-ref
   (load-supplemental-pointer (supplemental-fixture-root fx) (supplemental-fixture-plan fx) 1)
   'task-approval))

(module+ test
  (test-case "status authenticates real physical checkout through owned proof"
    (call-with-supplemental-fixture
     (lambda (fx)
       (define base (supplemental-fixture-root fx))
       (define plan (supplemental-fixture-plan fx))
       (define repo (supplemental-fixture-repo fx))
       (define receipt (hash-ref (load-delivery-journal base plan 1) 'receipt))
       (define manifest (load-receipt-owned-proof receipt base plan 1))
       (check-not-equal? (simplify-path repo) (simplify-path (hash-ref receipt 'repo)))
       (check-equal? (simplify-path (hash-ref manifest 'execution-cwd)) (simplify-path repo))
       (define grant (fixture-grant fx))
       (define initial
         (start-supplemental-operation!
          grant
          (hasheq 'context (supplemental-fixture-task-context fx) 'coordinator-fence 8)))
       (define reserved
         (transition-supplemental-operation!
          grant
          (hash-ref initial 'revision)
          "proposal-approved"
          "reserved"
          (hasheq 'allocation (hasheq 'attempt-id "physical" 'attempt-fence 8 'coordinator-fence 8))))
       (define launched
         (transition-supplemental-operation! grant
                                             (hash-ref reserved 'revision)
                                             "reserved"
                                             "launched"
                                             (hasheq 'dispatch-id "physical")))
       (define summary (supplemental-operation-summary base plan 1))
       (check-not-false summary)
       (check-equal? (hash-ref summary 'state) "launched")
       (check-equal? (hash-ref summary 'revision) (hash-ref launched 'revision))
       (check-equal? (hash-ref summary 'grant-sha256) (hash-ref grant 'sha256))
       (check-true (supplemental-resumable? base plan 1))
       ;; Physical-path authentication must not weaken the addressed proof pin.
       (define manifest-path
         (build-path (current-owned-proof-root)
                     plan
                     (hash-ref (hash-ref receipt 'owned-proof) 'path)))
       (define manifest-bytes (file->bytes manifest-path))
       (define manifest-mode (file-or-directory-permissions manifest-path 'bits))
       (dynamic-wind (lambda ()
                       (file-or-directory-permissions manifest-path #o600)
                       (call-with-output-file manifest-path
                                              (lambda (out)
                                                (write-bytes (bytes-append manifest-bytes #"\n") out))
                                              #:exists 'truncate)
                       (file-or-directory-permissions manifest-path manifest-mode))
                     (lambda ()
                       (check-false (supplemental-operation-summary base plan 1))
                       (check-false (supplemental-resumable? base plan 1)))
                     (lambda ()
                       (file-or-directory-permissions manifest-path #o600)
                       (call-with-output-file manifest-path
                                              (lambda (out) (write-bytes manifest-bytes out))
                                              #:exists 'truncate)
                       (file-or-directory-permissions manifest-path manifest-mode)))
       (check-true (supplemental-resumable? base plan 1)))
     #:physical-worktree? #t))

  (for ([kind '(failed cancelled)])
    (test-case (format "actual ~a runner result blocks once without redispatch" kind)
      (call-with-supplemental-fixture
       (lambda (fx)
         (define base (supplemental-fixture-root fx))
         (define plan (supplemental-fixture-plan fx))
         (define repo (supplemental-fixture-repo fx))
         (define grant (fixture-grant fx))
         (define approval (load-supplemental-record grant))
         (define pointer (load-supplemental-pointer base plan 1))
         (define proposal (load-supplemental-record (hash-ref pointer 'task-proposal)))
         (define source (committed-delivery-snapshot repo))
         (define journal (load-delivery-journal base plan 1))
         (define calls 0)
         (define (run)
           (set! calls (add1 calls))
           (wave-execution-outcome kind "provider stopped"))
         (define (invoke)
           (parameterize ([current-directory repo])
             (run-supplemental-author-phases! base plan 1 "physical" 8 run #:guard (lambda () #t))))
         (check-equal? (wave-execution-outcome-kind (invoke)) kind)
         (define blocked (load-supplemental-operation grant))
         (check-equal? (hash-ref blocked 'state) "blocked")
         (check-equal? (hash-ref blocked 'revision) 3)
         (check-equal? (hash-ref (supplemental-operation-summary base plan 1) 'state) "blocked")
         (check-false (supplemental-resumable? base plan 1))
         (check-true (hash-ref blocked 'launch-consumed))
         (check-equal? (hash-ref (hash-ref blocked 'facts) 'block-reason) "provider stopped")
         (invoke)
         (check-equal? calls 1)
         (check-equal? (load-supplemental-operation grant) blocked)
         (check-equal? (load-supplemental-record grant) approval)
         (check-equal? (load-supplemental-record (hash-ref pointer 'task-proposal)) proposal)
         (check-equal? (committed-delivery-snapshot repo) source)
         (check-equal? (load-delivery-journal base plan 1) journal))
       #:physical-worktree? #t))))
