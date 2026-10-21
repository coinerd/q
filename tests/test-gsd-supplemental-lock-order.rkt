#lang racket
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 600
;; @covers extensions/gsd/supplemental-evidence-proof.rkt
(require rackunit
         racket/async-channel
         "helpers/supplemental-promotion-fixture.rkt"
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         "../extensions/gsd/supplemental-evidence-journal.rkt"
         "../extensions/gsd/supplemental-evidence-publication.rkt")

(define (await evt label)
  (or (sync/timeout 30 evt) (error 'lock-order "timed out: ~a" label)))

(define (exercise-lock-order facts)
  (define receipt (hash-ref facts 'receipt))
  (define contract (hash-ref facts 'contract))
  (define grant (hash-ref contract 'task-approval))
  ;; The real kernel lock used by under-lock, not a fabricated semaphore.
  (define lock-path
    (build-path (current-supplemental-authority-root) "supplemental-operations"
                (string-append (hash-ref grant 'sha256) ".lock")))
  (define op-held (make-semaphore 0))
  (define authority-held (make-semaphore 0))
  (define reader-attempting (make-semaphore 0))
  (define reader-result (make-async-channel))
  (define stage-result (make-async-channel))
  (define workers (make-custodian))
  (define (worker result proc)
    (parameterize ([current-custodian workers])
      (thread (lambda ()
                (with-handlers ([exn? (lambda (e) (async-channel-put result e))])
                  (async-channel-put result (proc)))))))
  (define (checked-result channel label)
    (define result (await channel label))
    (when (exn? result) (raise result))
    result)
  (dynamic-wind
   void
   (lambda ()
     (worker reader-result
             (lambda ()
               (call-with-file-lock/timeout
                #f 'exclusive
                (lambda ()
                  (semaphore-post op-held)
                  (await authority-held 'authority-held)
                  ;; B already owns authority; A retains op.lock until it gets
                  ;; the authority section, exactly the journal writer order.
                  (semaphore-post reader-attempting)
                  (call-with-supplemental-authority-lock
                   (lambda () (inspect-supplemental-entry-operation grant))))
                (lambda () (error 'lock-order "could not acquire fixture operation lock"))
                #:lock-file lock-path)))
     (await op-held 'operation-held)
     (worker stage-result
             (lambda ()
               (call-with-supplemental-authority-lock
                (lambda ()
                  (semaphore-post authority-held)
                  (await reader-attempting 'reader-attempting)
                  (stage-supplemental-evidence!
                   (hash-ref facts 'root) fixture-plan 1
                   (hash-ref receipt 'attempt-id) (hash-ref receipt 'attempt-fence)
                   contract receipt (hash-ref receipt 'owned-proof)
                   (hash-ref facts 'task) (hash-ref facts 'candidate))))))
     (check-true (supplemental-staged-evidence? (checked-result stage-result 'stage-completed)))
     (check-equal? (hash-ref (checked-result reader-result 'reader-authority-section) 'state) "green-captured"))
   (lambda () (custodian-shutdown-all workers))))

(module+ test
  (test-case "staging authenticates proof without acquiring operation lock under authority"
    ;; Continue through real R1/publication/certification after the concurrent
    ;; stage: success is not a shortcut around the genuine fixture ladder.
    (with-certified-promotion-fixture
     (lambda (_facts) (void))
     #:recovery? #t
     #:before-stage exercise-lock-order)))
