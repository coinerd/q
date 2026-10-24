#lang racket/base
;; Genuine spent v1/v2 history; fixture data never supplies broker authority.
(require racket/runtime-path
         "supplemental-go-fixture.rkt"
         "honest-delivery-fixture.rkt"
         "../../extensions/gsd/go-orchestrator.rkt"
         "../../extensions/gsd/campaign-repository.rkt"
         "../../extensions/gsd/campaign-state.rkt"
         "../../extensions/gsd/delivery-journal.rkt"
         "../../extensions/gsd/delivery-publication.rkt"
         "../../extensions/gsd/supplemental-evidence-controller.rkt"
         "../../extensions/gsd/supplemental-evidence-format.rkt"
         "../../extensions/gsd/supplemental-evidence-journal.rkt"
         "../../extensions/gsd/supplemental-disposition-policy.rkt"
         (only-in "../../extensions/gsd/evidence-repair-format.rkt" canonical-evidence-json)
         (only-in "../../util/json/checksum.rkt" sha256-string))
(provide (struct-out disposition-fixture)
         call-with-disposition-fixture)
(struct disposition-fixture (fx root-pointer parent-pointer root-operation parent-operation amendment)
  #:transparent)
(define-runtime-path policy "../../extensions/gsd/supplemental-disposition-policy.rkt")

(define (consume! fx)
  (define base (supplemental-fixture-root fx))
  (define plan (supplemental-fixture-plan fx))
  (void (run-campaign! base
                       (load-campaign-record base plan)
                       #:isolate? #f
                       #:timeout-sec 7200
                       #:delivery-reader (test-delivered-reader)
                       #:delivery-coordinator
                       (lambda _ (error 'disposition-fixture "ordinary coordinator"))
                       #:runner (lambda (_) 'ok)
                       #:verifier (lambda _ (error 'disposition-fixture "ordinary verifier")))))
(define (terminal! approval)
  (define op (load-supplemental-operation approval))
  (unless (and (equal? (hash-ref op 'state) "blocked")
               (= (hash-ref op 'revision) 3)
               (hash-ref op 'launch-consumed))
    (error 'disposition-fixture "real no-edit launch did not produce blocked revision 3"))
  op)
(define (call-with-disposition-fixture proc #:second? [second? #f])
  (call-with-supplemental-fixture
   (lambda (fx)
     (define base (supplemental-fixture-root fx))
     (define plan (supplemental-fixture-plan fx))
     (fixture-finalize-predecessor! fx)
     ;; Retain genuine prior publication through the ordinary production API.
     ;; Only the remote effect is isolated, as in the promotion fixture.
     (define receipt (hash-ref (load-delivery-journal base plan 1) 'receipt))
     (parameterize ([current-gsd-approved-head-publisher
                     (lambda (_base _plan _wave source _attempt _fence _coordinator _evidence)
                       (hasheq 'status
                               "published"
                               'branch
                               (hash-ref source 'branch)
                               'head
                               (hash-ref source 'head)))])
       (publish-approved-head! base
                               plan
                               1
                               (for/hasheq ([key '(repo origin branch head tree)])
                                 (values key (hash-ref receipt key)))
                               (hash-ref receipt 'attempt-id)
                               (hash-ref receipt 'attempt-fence)
                               #f
                               (hash-ref receipt 'evidence)
                               (lambda () (load-campaign-record base plan))
                               #:owned-proof (hash-ref receipt 'owned-proof)))
     (define root (load-supplemental-pointer base plan 1))
     (consume! fx)
     (define root-op (terminal! (hash-ref root 'task-approval)))
     (define-values (ref _task)
       (propose-supplemental-recovery! base
                                       plan
                                       1
                                       (hash-ref (hash-ref root 'task-proposal) 'sha256)
                                       (+ (current-seconds) 14400)))
     (install-supplemental-recovery! base plan 1 ref (fixture-approve! ref))
     (define parent (load-supplemental-pointer base plan 1))
     (consume! fx)
     (define parent-op (terminal! (hash-ref parent 'task-approval)))
     (define campaign (load-campaign-record base plan))
     (define amendment
       (seal-supplemental-record (hash-set* disposition-amendment-1
                                            'plan-id
                                            plan
                                            'wave
                                            1
                                            'root-proposal
                                            (hash-ref root 'task-proposal)
                                            'root-approval
                                            (hash-ref root 'task-approval)
                                            'parent-proposal
                                            (hash-ref parent 'task-proposal)
                                            'parent-approval
                                            (hash-ref parent 'task-approval)
                                            'terminal-body-sha256
                                            (hash-ref parent-op 'sha256)
                                            'terminal-record-sha256
                                            (sha256-string (canonical-evidence-json parent-op))
                                            'allocation
                                            (hash-ref (hash-ref parent-op 'facts) 'allocation)
                                            'snapshot-sha256
                                            (campaign-record-plan-snapshot-digest campaign))))
     ((dynamic-require `(submod ,policy test-support) 'call-with-test-disposition-amendment)
      amendment
      (lambda ()
        (cond
          [second?
           (define-values (p t)
             (propose-supplemental-disposition! base
                                                plan
                                                1
                                                (hash-ref (hash-ref parent 'task-proposal) 'sha256)
                                                (+ (current-seconds) 14400)))
           (install-supplemental-disposition! base plan 1 p (fixture-approve! p))
           (define v3 (load-supplemental-pointer base plan 1))
           (consume! fx)
           (define v3-op (terminal! (hash-ref v3 'task-approval)))
           (define second
             (seal-supplemental-record (hash-set* amendment
                                                  'v2-proposal
                                                  (hash-ref parent 'task-proposal)
                                                  'v2-approval
                                                  (hash-ref parent 'task-approval)
                                                  'parent-proposal
                                                  (hash-ref v3 'task-proposal)
                                                  'parent-approval
                                                  (hash-ref v3 'task-approval)
                                                  'terminal-body-sha256
                                                  (hash-ref v3-op 'sha256)
                                                  'terminal-record-sha256
                                                  (sha256-string (canonical-evidence-json v3-op))
                                                  'allocation
                                                  (hash-ref (hash-ref v3-op 'facts) 'allocation))))
           ((dynamic-require `(submod ,policy test-support) 'call-with-test-disposition-amendment)
            second
            (lambda () (proc (disposition-fixture fx root v3 root-op v3-op second)))
            #:predecessor amendment)]
          [else (proc (disposition-fixture fx root parent root-op parent-op amendment))]))))
   #:completed-author? #t
   #:physical-worktree? #t
   #:wave-index 1
   #:expires-in-sec 14400))
