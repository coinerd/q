#lang racket/base
;; @covers extensions/gsd/tracker-production-wiring.rkt
;; @speed fast
;; @suite gsd
(require rackunit
         json
         racket/file
         "../runtime/settings.rkt"
         "../extensions/gsd/tracker-production-wiring.rkt"
         "../extensions/gsd/reconciliation-checkpoint.rkt")
(define PLAN (make-string 64 #\a))
(define (settings tracker)
  (make-minimal-settings #:overrides (hasheq 'gsd (hasheq 'tracker tracker))))
(define full-binding
  (hasheq 'live
          #t
          'plan-id
          PLAN
          'wave
          1
          'repository
          "coinerd/q"
          'issue-number
          9763
          'board-field
          "Status"
          'board-value
          "Done"
          'project-item-id
          "PVTI_item"
          'project-id
          "PVT_project"
          'field-id
          "PVTSSF_status"
          'option-id
          "done123"))
(define (attempt tracker)
  (define made (box 0))
  (define reconciler
    (resolve-live-tracker-reconciler "/tmp/no-real-campaign"
                                     PLAN
                                     1
                                     #:settings (settings tracker)
                                     #:adapter-maker
                                     (lambda (#:live? _ #:repository _repo)
                                       (set-box! made (add1 (unbox made)))
                                       (error 'test "adapter should not be constructed"))))
  (values reconciler made))

(test-case "project .q/config.json opt-in resolves only the matching campaign wave"
  (define dir (make-temporary-file "tracker-settings~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (make-directory* (build-path dir ".q"))
                  (call-with-output-file
                   (build-path dir ".q" "config.json")
                   (lambda (out) (write-json (hasheq 'gsd (hasheq 'tracker full-binding)) out)))
                  (check-true (procedure? (resolve-live-tracker-reconciler dir PLAN 1)))
                  (check-false (resolve-live-tracker-reconciler
                                dir
                                PLAN
                                1
                                #:settings (settings (hash-set full-binding 'live #f)))))
                (lambda () (delete-directory/files dir))))

(test-case "no binding or no explicit live authorization means no reconciler"
  (for ([tracker (in-list (list #f (hash) (hash-set full-binding 'live #f)))])
    (define-values (reconciler made) (attempt tracker))
    (check-false reconciler)
    (check-equal? (unbox made) 0)))

(test-case "a wrong wave or plan is refused before any GitHub command"
  (for ([tracker (in-list (list (hash-set full-binding 'wave 2)
                                (hash-set full-binding 'plan-id (make-string 64 #\b))
                                (hash-remove full-binding 'project-id)))])
    (define-values (reconciler made) (attempt tracker))
    (check-true (procedure? reconciler))
    (define outcome
      (run-tracker-reconciliation! "/tmp/no-real-campaign"
                                   PLAN
                                   1
                                   reconciler
                                   #:delivery-reader (lambda _ (error 'test "not consulted"))))
    (check-equal? (reconciliation-checkpoint-result-status outcome) 'blocked)
    (check-equal? (unbox made) 0)))

(test-case "a valid explicit binding constructs the live Racket adapter, never a dry-run default"
  (define calls (box '()))
  (define reconciler
    (resolve-live-tracker-reconciler "/tmp/no-real-campaign"
                                     PLAN
                                     1
                                     #:settings (settings full-binding)
                                     #:adapter-maker
                                     (lambda (#:live? live? #:repository repo)
                                       (set-box! calls (list live? repo))
                                       (error 'test "fake stops before any network"))))
  (define result
    (run-tracker-reconciliation! "/tmp/no-real-campaign"
                                 PLAN
                                 1
                                 reconciler
                                 #:delivery-reader (lambda _ (error 'test "not consulted"))))
  (check-equal? (unbox calls) (list #t "coinerd/q"))
  (check-equal? (reconciliation-checkpoint-result-status result) 'blocked))
