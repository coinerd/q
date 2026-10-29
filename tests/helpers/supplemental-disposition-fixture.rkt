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
         (only-in "../../extensions/gsd/supplemental-evidence-authority.rkt"
                  load-supplemental-record
                  propose-retain!)
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
;; Genuine late-stage terminal for the fixed third generation. The operation
;; is driven through the production journal transitions (never a rewritten or
;; hand-built record): reserved -> launched -> candidate-sealed ->
;; candidate-approved -> verifying -> blocked/revision 6, with the real
;; candidate proposal/approval records and the campaign's own current
;; attempt/fences as the bound allocation. The expired attempt-6 operation in
;; production reached the same shape before the supported terminal writer
;; sealed it.
(define (spend-late! base plan grant proposal-ref attempt-id attempt-fence coordinator-fence)
  (define task (hash-ref (hash-ref (load-supplemental-record proposal-ref) 'payload) 'task))
  (define allocation
    (hasheq 'attempt-id attempt-id 'attempt-fence attempt-fence 'coordinator-fence coordinator-fence))
  (void (start-supplemental-operation!
         grant
         (hasheq 'context (hash-ref task 'context) 'coordinator-fence coordinator-fence)))
  (define (step! from to facts)
    (define op (load-supplemental-operation grant))
    (transition-supplemental-operation! grant (hash-ref op 'revision) from to facts))
  (step! "proposal-approved" "reserved" (hasheq 'allocation allocation))
  (step! "reserved" "launched" (hasheq 'dispatch-id "supplemental-author-late"))
  (define retained (load-supplemental-operation grant))
  (define candidate-ref
    (propose-retain! "candidate-proposal"
                     (hasheq 'task (hash-ref retained 'proposal-ref))
                     (list (hash-ref retained 'proposal-ref))))
  (step! "launched" "candidate-sealed" (hasheq 'candidate-ref candidate-ref))
  (step! "candidate-sealed"
         "candidate-approved"
         (hasheq 'candidate-approval-ref (fixture-approve! candidate-ref)))
  (step! "candidate-approved"
         "verifying"
         (hasheq 'verify-job (hasheq 'id "fixture-verify-job" 'owner "fixture-controller")))
  (step! "verifying" "blocked" (hasheq 'block-reason "operator deadline expired"))
  (load-supplemental-operation grant))
(define (call-with-disposition-fixture proc
                                       #:second? [second? #f]
                                       #:third? [third? #f]
                                       #:late-terminal? [late-terminal? #t])
  (when third?
    (set! second? #t))
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
     (define (build-second)
       ;; Genuine amendment-2 fixture generation: install the disposition,
       ;; consume the v3 child into a real spent blocked revision-3 terminal,
       ;; and derive every digest from that sealed record.
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
       (values v3
               v3-op
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
                                                    (hash-ref (hash-ref v3-op 'facts) 'allocation)))))
     ((dynamic-require `(submod ,policy test-support) 'call-with-test-disposition-amendment)
      amendment
      (lambda ()
        (cond
          [(and second? third?)
           ;; Third bounded generation: the amendment-2 disposition child (the
           ;; production 15e68c8b/ccf20bc9 analogue) is installed, genuinely
           ;; spent into the late blocked/revision-6 verifying-prefix terminal
           ;; (the live attempt-6 shape; `#:late-terminal? #f` keeps the
           ;; exhausted revision-3 shape for negative tests), and the
           ;; immutable amendment-3 fixture binds that exact pair, both real
           ;; terminal digest namespaces and its failed allocation — never
           ;; fabricated digests and never a fourth successor grant. Each
           ;; generation's disposition is proposed inside the PREVIOUS
           ;; generation's binding: it disposes that binding's parent pair.
           (define-values (v3 _v3-op second) (build-second))
           ((dynamic-require `(submod ,policy test-support) 'call-with-test-disposition-amendment)
            second
            (lambda ()
              (define-values (sref _stask)
                (propose-supplemental-disposition! base
                                                   plan
                                                   1
                                                   (hash-ref (hash-ref v3 'task-proposal) 'sha256)
                                                   (+ (current-seconds) 14400)))
              (install-supplemental-disposition! base plan 1 sref (fixture-approve! sref))
              (define v4 (load-supplemental-pointer base plan 1))
              (define v4-op
                (if late-terminal?
                    (let* ([rec (load-campaign-record base plan)]
                           [w (for/first ([candidate (in-list (campaign-record-waves rec))]
                                          #:when (= 1 (campaign-wave-index candidate)))
                                candidate)]
                           [attempt (and w (campaign-wave-current-attempt w))])
                      (unless (and rec w attempt)
                        (error 'disposition-fixture
                               "campaign attempt for the late terminal is absent"))
                      (spend-late! base
                                   plan
                                   (hash-ref v4 'task-approval)
                                   (hash-ref v4 'task-proposal)
                                   (campaign-attempt-id attempt)
                                   (campaign-attempt-fence-token attempt)
                                   (campaign-fence-token rec)))
                    (begin
                      (consume! fx)
                      (terminal! (hash-ref v4 'task-approval)))))
              (define third
                (seal-supplemental-record (hash-set* second
                                                     'prior-parent-proposal
                                                     (hash-ref second 'parent-proposal)
                                                     'prior-parent-approval
                                                     (hash-ref second 'parent-approval)
                                                     'parent-proposal
                                                     (hash-ref v4 'task-proposal)
                                                     'parent-approval
                                                     (hash-ref v4 'task-approval)
                                                     'terminal-body-sha256
                                                     (hash-ref v4-op 'sha256)
                                                     'terminal-record-sha256
                                                     (sha256-string (canonical-evidence-json v4-op))
                                                     'allocation
                                                     (hash-ref (hash-ref v4-op 'facts) 'allocation))))
              ((dynamic-require `(submod ,policy test-support) 'call-with-test-disposition-amendment)
               third
               (lambda () (proc (disposition-fixture fx root v4 root-op v4-op third)))
               #:predecessor second))
            #:predecessor amendment)]
          [second?
           (define-values (v3 v3-op second) (build-second))
           ((dynamic-require `(submod ,policy test-support) 'call-with-test-disposition-amendment)
            second
            (lambda () (proc (disposition-fixture fx root v3 root-op v3-op second)))
            #:predecessor amendment)]
          [else (proc (disposition-fixture fx root parent root-op parent-op amendment))]))))
   #:completed-author? #t
   #:physical-worktree? #t
   #:wave-index 1
   #:expires-in-sec 14400))
