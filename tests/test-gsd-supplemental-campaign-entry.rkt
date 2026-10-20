#lang racket
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 600
;; Measured full-file RED run: 315.242s; bounded slow integration gate.
(require rackunit
         "helpers/supplemental-go-fixture.rkt"
         "helpers/honest-delivery-fixture.rkt"
         "../extensions/gsd/go-orchestrator.rkt"
         "../extensions/gsd/owned-lane.rkt"
         "../extensions/gsd/wave-runner-port.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/review-proof.rkt"
         "../extensions/gsd/delivery-verifier.rkt"
         "../extensions/gsd/composition-root.rkt"
         "../extensions/gsd/verification-job.rkt"
         "../extensions/gsd/supplemental-evidence-controller.rkt"
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         (submod "../extensions/gsd/supplemental-evidence-authority.rkt" owner-frontend)
         "../extensions/gsd/supplemental-evidence-journal.rkt"
         "../extensions/gsd-planning/execution-policy.rkt")

;; The first case exercises real W1 with a receipt-bound delivered W0.
;; Remaining W0 cases isolate checkpoint/reentry fences. Every campaign entry
;; calls run-campaign!, with no advance override. The retained branch stays w1.
(define (campaign-path root plan)
  (build-path root ".planning" "campaigns" (string-append plan ".rktd")))

(define (with-entry proc #:expires-in-sec [expires-in-sec 3600])
  (call-with-supplemental-fixture
   (lambda (fx)
     (define root (supplemental-fixture-root fx))
     (define repo (supplemental-fixture-repo fx))
     (parameterize ([current-owned-proof-root (build-path root "owned-proofs")]
                    [current-gsd-verification-registry (make-verification-registry)]
                    [current-gsd-delivery-branch-context
                     (make-branch-delivery-context
                      #:repo-root repo
                      #:branch "campaign/w1"
                      #:base-commit (hash-ref (supplemental-fixture-task-context fx) 'head)
                      #:worktree-path repo)])
       (proc fx)))
   #:completed-author? #t
   #:wave-index 0
   #:expires-in-sec expires-in-sec))

(define (go fx
            runner
            [coordinator (lambda _ (cons 'blocked "evidence-author-allowance-exhausted"))]
            #:reader [reader (lambda _ #f)])
  (run-campaign! (supplemental-fixture-root fx)
                 (load-campaign-record (supplemental-fixture-root fx) (supplemental-fixture-plan fx))
                 #:isolate? #f
                 #:timeout-sec 30
                 #:delivery-reader reader
                 #:delivery-coordinator coordinator
                 #:runner runner
                 #:verifier (lambda (_) (error 'entry "ordinary verifier must not run"))))

(module+ test
  (test-case "W1 full entry preserves prior-install approved T and requires delivered W0"
    (call-with-supplemental-fixture
     (lambda (fx)
       (define root (supplemental-fixture-root fx))
       (define plan (supplemental-fixture-plan fx))
       (parameterize ([current-owned-proof-root (build-path root "owned-proofs")]
                      [current-gsd-verification-registry (make-verification-registry)]
                      [current-gsd-delivery-branch-context
                       (make-branch-delivery-context
                        #:repo-root (supplemental-fixture-repo fx)
                        #:branch "campaign/w1"
                        #:base-commit (hash-ref (supplemental-fixture-task-context fx) 'head)
                        #:worktree-path (supplemental-fixture-repo fx))])
         (define rec (load-campaign-record root plan))
         (define w1 (car (campaign-record-waves rec)))
         (check-equal? (campaign-wave-index w1) 1)
         (check-equal? (campaign-wave-status w1) 'awaiting-delivery)
         ;; Finalize W0 through the existing honest delivery operator fixture,
         ;; including receipt, handoff reconciliation and controller readback.
         (define (with-waves waves)
           (define copy
             (make-campaign-record plan
                                   (make-campaign-manifest 1 "fixture" '() '() "constraints")
                                   waves
                                   #f
                                   7
                                   'test
                                   0
                                   0))
           (set-campaign-record-plan-snapshot-path! copy (campaign-record-plan-snapshot-path rec))
           (set-campaign-record-plan-snapshot-digest! copy (campaign-record-plan-snapshot-digest rec))
           copy)
         (set! rec
               (with-waves (list (make-campaign-wave 0
                                                     "Predecessor"
                                                     'awaiting-delivery
                                                     1
                                                     (campaign-attempt "predecessor" 7 0)))))
         (persist-campaign! root rec)
         (define completed (finalize-awaiting! root rec))
         (check-equal? (campaign-result-status completed) 'campaign-complete)
         (define ready (load-campaign-record root plan))
         (check-equal? (campaign-wave-status (car (campaign-record-waves ready))) 'done)
         (set! ready (with-waves (append (campaign-record-waves ready) (list w1))))
         (persist-campaign! root ready)
         (define before (file->bytes (delivery-journal-path root plan 1)))
         (define pointer (load-supplemental-pointer root plan 1))
         (define tref (hash-ref pointer 'task-proposal))
         (define aref (hash-ref pointer 'task-approval))
         (define proposal (load-supplemental-record tref))
         (define approval (load-supplemental-record aref))
         (define campaign-before (file->bytes (campaign-path root plan)))
         (define calls 0)
         (define legacy 0)
         (define (runner _)
           (set! calls (add1 calls))
           (check-equal? (hash-ref (current-gsd-author-phase) 'phase) 'test-author)
           (check-equal? (hash-ref (current-gsd-author-phase) 'files) (fixture-task-paths fx))
           (fixture-author-commit! (supplemental-fixture-repo fx))
           'ok)
         (define (coordinator . _)
           (set! legacy (add1 legacy))
           (cons 'blocked "evidence-author-allowance-exhausted"))
         (define honest-reader (test-delivered-reader))
         ;; Missing or invalid authenticated predecessor readback cannot launch.
         (for ([reader (list (lambda _ #f)
                             (lambda (b p w)
                               (hash-set (honest-reader b p w) 'attempt-id "foreign")))])
           (void (go fx runner coordinator #:reader reader))
           (check-equal? calls 0)
           (check-false (load-supplemental-operation aref))
           (display-to-file campaign-before (campaign-path root plan) #:exists 'truncate))
         ;; Actual live source-context drift remains forbidden, independent of
         ;; the proposal-time installed-engine provenance.
         (display-to-file "drift" (build-path (supplemental-fixture-repo fx) "drift"))
         (void (go fx runner coordinator #:reader honest-reader))
         (check-equal? calls 0)
         (check-false (load-supplemental-operation aref))
         (delete-file (build-path (supplemental-fixture-repo fx) "drift"))
         (display-to-file campaign-before (campaign-path root plan) #:exists 'truncate)
         (set! legacy 0)
         (void (go fx runner coordinator #:reader honest-reader))
         (check-equal? calls 1 "unchanged prior-install approved T must launch")
         (check-equal? legacy 0)
         (void (go fx runner coordinator #:reader honest-reader))
         (check-equal? calls 1 "reentry cannot redispatch")
         (check-equal? (file->bytes (delivery-journal-path root plan 1)) before)
         (check-equal? (load-supplemental-record tref) proposal)
         (check-equal? (load-supplemental-record aref) approval)))
     #:completed-author? #t
     #:wave-index 1
     #:proposal-control-plane-head (make-string 40 #\a)))

  (test-case "spent no-edit launch never falls through on full campaign reentry or retry"
    (with-entry
     (lambda (fx)
       (define root (supplemental-fixture-root fx))
       (define plan (supplemental-fixture-plan fx))
       (define before (file->bytes (delivery-journal-path root plan 0)))
       (define pointer (load-supplemental-pointer root plan 0))
       (define proposal (load-supplemental-record (hash-ref pointer 'task-proposal)))
       (define approval (load-supplemental-record (hash-ref pointer 'task-approval)))
       (define calls 0)
       (define phases '())
       (define (runner _)
         (set! calls (add1 calls))
         (set! phases (cons (current-gsd-author-phase) phases))
         'ok)
       (define first (go fx runner))
       (printf "no-edit first full entry: ~s; calls=~a\n" first calls)
       (check-equal? calls 1)
       (check-equal? (hash-ref (car phases) 'phase) 'test-author)
       (check-equal? (hash-ref (load-supplemental-operation (hash-ref pointer 'task-approval)) 'state)
                     "blocked")
       (void (go fx runner))
       (void (go fx runner))
       (check-equal? calls 1 "unmodified repeated /go cannot dispatch again")
       ;; Include the ordinary retry shape: a failed/pending retained wave
       ;; must not turn the same spent task into an ordinary model dispatch.
       (for ([status '(awaiting-delivery pending failed pending)])
         (define rec (load-campaign-record root plan))
         (set-campaign-wave-status! (car (campaign-record-waves rec)) status)
         (persist-campaign! root rec)
         (void (go fx runner (lambda _ (error 'entry "ordinary coordinator fallback"))))
         (check-equal? calls 1 (format "~a reentry cannot call any model" status)))
       ;; Exercise the dispatch seam used by internal retries independently
       ;; of the pre-attempt entry stop. A spent pointer must not select the
       ;; proactive/legacy runner even if the port was already constructed.
       (define rec (load-campaign-record root plan))
       (define attempt (campaign-wave-current-attempt (car (campaign-record-waves rec))))
       (define port
         (make-owned-runner-port
          root
          plan
          0
          (campaign-attempt-id attempt)
          (campaign-attempt-fence-token attempt)
          (lambda () (load-campaign-record root plan))
          (make-wave-runner-port (lambda (idx)
                                   (runner idx)
                                   (wave-execution-outcome 'done "unexpected fallback")))))
       (check-equal? (wave-execution-outcome-kind ((gsd-wave-runner-port-run port) 0)) 'failed)
       (check-equal? calls 1 "internal retry dispatch seam refuses a spent task")
       (define rev
         (propose-retain! "revocation-proposal"
                          (hasheq 'reason "spent launch regression")
                          (list (hash-ref pointer 'task-approval))))
       (revoke! rev (fixture-interactive-grant rev))
       (void (go fx runner))
       (check-equal? calls 1 "revoking a spent launch cannot release ordinary fallback")
       (check-equal? (file->bytes (delivery-journal-path root plan 0)) before)
       (check-equal? (load-supplemental-record (hash-ref pointer 'task-proposal)) proposal)
       (check-equal? (load-supplemental-record (hash-ref pointer 'task-approval)) approval))))

  (test-case "revoked unlaunched task cannot fall through from a retained pending wave"
    (with-entry
     (lambda (fx)
       (define root (supplemental-fixture-root fx))
       (define plan (supplemental-fixture-plan fx))
       (define pointer (load-supplemental-pointer root plan 0))
       (define before (file->bytes (delivery-journal-path root plan 0)))
       (define proposal (load-supplemental-record (hash-ref pointer 'task-proposal)))
       (define approval (load-supplemental-record (hash-ref pointer 'task-approval)))
       (define rev
         (propose-retain! "revocation-proposal"
                          (hasheq 'reason "entry regression")
                          (list (hash-ref pointer 'task-approval))))
       (revoke! rev (fixture-interactive-grant rev))
       (check-false (supplemental-author-pending? root plan 0))
       (define rec (load-campaign-record root plan))
       (set-campaign-wave-status! (car (campaign-record-waves rec)) 'pending)
       (persist-campaign! root rec)
       (define calls 0)
       (for ([_ (in-range 2)])
         (void (go fx
                   (lambda (_)
                     (set! calls (add1 calls))
                     'ok)))
         (check-equal? calls 0))
       (check-exn #rx"live authenticated task approval required"
                  (lambda () (load-supplemental-operation (hash-ref pointer 'task-approval))))
       (check-equal? (file->bytes (delivery-journal-path root plan 0)) before)
       (check-equal? (load-supplemental-record (hash-ref pointer 'task-proposal)) proposal)
       (check-equal? (load-supplemental-record (hash-ref pointer 'task-approval)) approval))))

  (test-case "full /go admits the same approved T once; cancellation, budget and stale entry fail closed"
    (with-entry
     (lambda (fx)
       (define root (supplemental-fixture-root fx))
       (define plan (supplemental-fixture-plan fx))
       (define calls 0)
       (define coordinator-calls 0)
       (define phases '())
       (define (runner _)
         (set! calls (add1 calls))
         (set! phases (cons (current-gsd-author-phase) phases))
         ;; Only the scoped test files change; the owner commits them. Stop
         ;; at unavailable candidate approval, before any expensive C1 run.
         (fixture-author-commit! (supplemental-fixture-repo fx))
         'ok)
       (define (journal-bytes)
         (file->bytes (delivery-journal-path root plan 0)))
       (define before (journal-bytes))
       (define pointer (load-supplemental-pointer root plan 0))
       (define proposal (load-supplemental-record (hash-ref pointer 'task-proposal)))
       (define approval (load-supplemental-record (hash-ref pointer 'task-approval)))
       (check-equal? (hash-ref (journal-evidence-author-repair (load-delivery-journal root plan 0))
                               'state)
                     "completed")
       (check-true (supplemental-author-pending? root plan 0))
       (check-false (supplemental-resumable? root plan 0))
       (check-false (load-supplemental-operation (hash-ref pointer 'task-approval)))
       ;; Each refusal must leave the parked record and retained receipt
       ;; untouched and must not initialize any protected operation.
       (define pristine (file->bytes (campaign-path root plan)))
       (for ([kind '(cancel budget attempt fence dirty)])
         (define rec (load-campaign-record root plan))
         (define w (car (campaign-record-waves rec)))
         (case kind
           [(cancel) (set-campaign-cancellation! rec (make-campaign-cancellation "stop" 0))]
           [(budget)
            (set-campaign-record-budget-pause! rec (campaign-budget-pause 'max-cost 1 2 "paused" 0))]
           [(attempt) (set-campaign-wave-current-attempt! w (campaign-attempt "foreign" 7 0))]
           [(fence) (set-campaign-fence-token! rec 6)]
           [(dirty) (display-to-file "drift" (build-path (supplemental-fixture-repo fx) "drift"))])
         (persist-campaign! root rec)
         (define blocked-record (file->bytes (campaign-path root plan)))
         (void (go fx runner))
         (check-equal? calls 0 (format "~a cannot dispatch" kind))
         (check-equal? (file->bytes (campaign-path root plan)) blocked-record)
         (check-equal? (journal-bytes) before)
         (check-false (load-supplemental-operation (hash-ref pointer 'task-approval)))
         (when (eq? kind 'dirty)
           (delete-file (build-path (supplemental-fixture-repo fx) "drift")))
         (display-to-file pristine (campaign-path root plan) #:exists 'truncate))
       (define result
         (go fx
             runner
             (lambda _
               (set! coordinator-calls (add1 coordinator-calls))
               (cons 'blocked "evidence-author-allowance-exhausted"))))
       (printf "full entry: ~s; supplemental dispatches=~a; ordinary coordinator calls=~a\n"
               result
               calls
               coordinator-calls)
       (check-equal? calls 1 "full entry must reach the scoped supplemental author")
       (check-equal? coordinator-calls 0 "admission precedes the ordinary exhausted coordinator")
       (when (pair? phases)
         (check-equal? (hash-ref (car phases) 'phase) 'test-author)
         (check-equal? (hash-ref (car phases) 'files) (fixture-task-paths fx)))
       (check-equal? (journal-bytes) before "historical marker and receipt remain byte-identical")
       (check-equal? (load-supplemental-record (hash-ref pointer 'task-proposal)) proposal)
       (check-equal? (load-supplemental-record (hash-ref pointer 'task-approval)) approval)
       (void (go fx runner))
       (check-equal? calls 1 "second /go cannot dispatch the spent task again")
       (check-equal? (journal-bytes) before))))

  (test-case "full /go refuses an expired authentic approval without changing the parked entry"
    (with-entry (lambda (fx)
                  (define root (supplemental-fixture-root fx))
                  (define plan (supplemental-fixture-plan fx))
                  (define before (file->bytes (delivery-journal-path root plan 0)))
                  (define record-before (file->bytes (campaign-path root plan)))
                  (sleep 6)
                  (define calls 0)
                  (void (go fx
                            (lambda (_)
                              (set! calls (add1 calls))
                              'ok)))
                  (check-equal? calls 0)
                  (check-equal? (file->bytes (campaign-path root plan)) record-before)
                  (check-equal? (file->bytes (delivery-journal-path root plan 0)) before))
                #:expires-in-sec 5))

  (test-case "full /go refuses a lost reserved allocation and a revoked authentic approval"
    (with-entry
     (lambda (fx)
       (define root (supplemental-fixture-root fx))
       (define plan (supplemental-fixture-plan fx))
       (define pointer (load-supplemental-pointer root plan 0))
       ;; Crash cut after reservation: a new full entry may not allocate
       ;; again, even though no model dispatch has yet been consumed.
       (define grant (hash-ref pointer 'task-approval))
       (define op
         (start-supplemental-operation!
          grant
          (hasheq 'context (supplemental-fixture-task-context fx) 'coordinator-fence 7)))
       (transition-supplemental-operation!
        grant
        (hash-ref op 'revision)
        "proposal-approved"
        "reserved"
        (hasheq 'allocation
                (hasheq 'attempt-id "lost-allocation" 'attempt-fence 8 'coordinator-fence 8)))
       (define reserved-record (file->bytes (campaign-path root plan)))
       (define reserved-operation (load-supplemental-operation grant))
       (define reserved-calls 0)
       (void (go fx
                 (lambda (_)
                   (set! reserved-calls (add1 reserved-calls))
                   'ok)))
       (check-equal? reserved-calls 0)
       (check-equal? (file->bytes (campaign-path root plan)) reserved-record)
       (check-equal? (load-supplemental-operation grant) reserved-operation)
       (define rev
         (propose-retain! "revocation-proposal"
                          (hasheq 'reason "entry regression")
                          (list (hash-ref pointer 'task-approval))))
       (revoke! rev (fixture-interactive-grant rev))
       (define before (file->bytes (delivery-journal-path root plan 0)))
       (define record-before (file->bytes (campaign-path root plan)))
       (define calls 0)
       (void (go fx
                 (lambda (_)
                   (set! calls (add1 calls))
                   'ok)))
       (check-equal? calls 0)
       (check-equal? (file->bytes (campaign-path root plan)) record-before)
       (check-equal? (file->bytes (delivery-journal-path root plan 0)) before)))))
