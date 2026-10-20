#lang racket
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 900
;; @covers extensions/gsd/supplemental-evidence-controller.rkt
;; @covers extensions/gsd/supplemental-evidence-publication.rkt
;; @covers extensions/gsd/supplemental-evidence-promotion.rkt
;; @covers extensions/gsd/supplemental-evidence-journal.rkt
;; BUG-0088 review B1: the typed recover-pending late WAL states
;; (recover-pending-verified/-published/-receipt-certified/-registration)
;; must recover through the PRODUCTION resume/replay routes — the owned /go
;; entry seam, the delivery-finalize checkpoint seam and the advance engine —
;; using the journal's authenticated same-operation reconcile. Every recovery
;; here starts from a REAL late cut (an injected storage fault during a real
;; production transition) and is driven exclusively through the production
;; entry points on complete temp-home campaign fixtures. No relaunch, no
;; second model dispatch, no second registration and no fake authority: the
;; journal's live-approval revalidation is asserted to refuse a revoked task.
(require rackunit
         racket/file
         json
         racket/string
         "helpers/supplemental-go-fixture.rkt"
         (only-in "helpers/supplemental-promotion-fixture.rkt"
                  with-certified-promotion-fixture
                  fixture-plan
                  fixture-revoke-approval!)
         "../runtime/approval/broker.rkt"
         "../extensions/gsd/campaign-result.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/go-orchestrator.rkt"
         "../extensions/gsd/delivery-finalize.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/delivery-publication.rkt"
         "../extensions/gsd/supplemental-evidence-journal.rkt"
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         "../extensions/gsd/supplemental-evidence-controller.rkt"
         "../extensions/gsd/supplemental-evidence-promotion.rkt"
         (only-in "../extensions/gsd/supplemental-evidence-publication.rkt"
                  load-supplemental-staged-evidence
                  supplemental-staged-evidence-record
                  supplemental-staged-probe-transition!)
         (only-in "../extensions/gsd/evidence-readiness.rkt" evidence-readiness)
         (only-in "../extensions/gsd/evidence-repair-ledger.rkt" load-evidence-repair)
         (only-in "../extensions/gsd/evidence-repair-format.rkt" canonical-evidence-digest)
         (only-in "../extensions/gsd/policy.rkt" current-gsd-wave-verification-repair-retries)
         (only-in "../extensions/gsd/supplemental-evidence-operator.rkt" current-supplemental-operator-publisher))

;; ------------------------------------------------------------------
;; Shared production-route drivers (same shapes the lifecycle suite uses).
;; ------------------------------------------------------------------
(define (author-run root rec runner)
  (parameterize ([current-gsd-wave-verification-repair-retries 0]
                 [current-gsd-approved-head-publisher
                  (lambda args (error 'fixture "publication awaits explicit fixture readback"))])
    (run-campaign-wave root
                       rec
                       1
                       #:fence-token 8
                       #:timeout-sec 600
                       #:timeout-retries 0
                       #:no-change-retries 0
                       #:stall-soft-limit #f
                       #:stall-hard-limit #f
                       #:isolate? #f
                       #:advance-override? #t
                       #:runner runner
                       #:verifier (lambda (_) (error 'verifier "ordinary verify must never run")))))

(define (auto-operator-approval! event payload)
  (when (equal? event "supplemental-evidence.approval-requested")
    (thread (lambda () (approval-decide! (hash-ref payload 'request-id) (hash-ref payload 'proposal-sha256) #t)))))

(define (with-auto-operator thunk)
  (parameterize ([current-supplemental-operator-publisher auto-operator-approval!])
    (thunk)))

;; The injected remote push/readback seam (the same one the publication and
;; promotion suites inject): only the network effect is faked, never any
;; authority decision.
(define (stub-publisher)
  (lambda (_base _plan _wave snapshot _attempt-id _fence _coordinator-fence _evidence)
    (hasheq 'status "published" 'branch (hash-ref snapshot 'branch) 'head (hash-ref snapshot 'head))))

;; A storage fault that interrupts ONLY the write-ahead intent whose sealed
;; target state is `wanted`: a genuine late cut at exactly one production
;; transition, leaving the typed recover-pending view on disk. The fired box
;; proves the cut really happened.
(define (intent-state-fault intent-path wanted fired)
  (lambda (at)
    (when (eq? at 'intent-written)
      (define next-state
        (with-handlers ([exn:fail? (lambda (_) #f)])
          (hash-ref (hash-ref (call-with-input-file intent-path read-json) 'next #f) 'state #f)))
      (when (equal? next-state wanted)
        (set-box! fired #t)
        (error 'injected-crash "interrupted the ~a transition" wanted)))))

(define (plain-intent-fault wanted)
  (lambda (at)
    (when (and (eq? at 'intent-written) wanted)
      (error 'injected-crash "interrupted the ~a transition" wanted))))

(define (operation-intent-path private grant)
  (build-path private "supplemental-operations" (string-append (hash-ref grant 'sha256) ".intent.json")))

;; The go fixture has an authenticated receipt but no remote-publication
;; history. Install its predecessor through the real publication path before
;; the supplemental author changes source, with only remote readback stubbed.
(define (publish-predecessor! root plan)
  (define receipt (hash-ref (load-delivery-journal root plan 1) 'receipt))
  (parameterize ([current-gsd-approved-head-publisher (stub-publisher)])
    (publish-approved-head! root
                            plan
                            1
                            (for/hasheq ([k '(repo origin branch head tree)])
                              (values k (hash-ref receipt k)))
                            (hash-ref receipt 'attempt-id)
                            (hash-ref receipt 'attempt-fence)
                            #f
                            (hash-ref receipt 'evidence)
                            (lambda () (load-campaign-record root plan)))))

(define (wave-status root plan)
  (campaign-wave-status (car (campaign-record-waves (load-campaign-record root plan)))))

(define (attempt-count root plan)
  (campaign-wave-attempt-count (car (campaign-record-waves (load-campaign-record root plan)))))

;; ------------------------------------------------------------------
;; Test 1: a registration cut (receipt-certified -> registered) recovers
;; through the owned /go ENTRY seam without relaunch and without a second
;; registration.
;; ------------------------------------------------------------------
(module+ registration-test
  (test-case "a late registration cut recovers through the /go entry seam exactly once"
    (with-certified-promotion-fixture
     (lambda (env)
       (define root (hash-ref env 'root))
       (define plan fixture-plan)
       (define aref (hash-ref env 'aref))
       (define sapref (hash-ref env 'sapref))
       (define r2 (hash-ref env 'r2))
       (define E (hash-ref env 'context))
       (define red-ref (hash-ref env 'red-ref))
       ;; The certified fixture carries no public pointer (the promotion
       ;; surface is pointerless); the campaign entry seam FINDS the private
       ;; operation only through the inert pointer, so write it exactly as
       ;; the owner would.
       (define tref (hash-ref (validate-supplemental-approval aref) 'proposal))
       (check-true (and (write-supplemental-pointer! root plan 1 tref aref) #t))
       (check-equal? (hash-ref (load-supplemental-operation aref) 'state) "receipt-certified")

       ;; Real production registration transition under a storage fault: the
       ;; registration is durably admitted, then the protected operation's
       ;; own write-ahead intent is interrupted before its snapshot lands.
       (parameterize ([current-supplemental-journal-crash-cut (plain-intent-fault #t)])
         (check-exn #rx"injected-crash" (lambda () (promote-supplemental-evidence! root plan 1 sapref (lambda () #t)))))
       (define pending (load-supplemental-operation aref))
       (check-equal? (hash-ref pending 'state) "recover-pending-registration")
       (check-equal? (hash-ref (hash-ref pending 'facts) 'pending-recovery) "registered")
       (check-true (hash-has-key? (hash-ref pending 'facts) 'interrupted-intent))
       ;; A pending view exposes the pre-cut facts, not the uninstalled
       ;; target. The actual registration is already durable in the ledger.
       (check-false (hash-has-key? (hash-ref pending 'facts) 'registration))
       (check-true (hash-ref pending 'launch-consumed))

       ;; RED/GREEN through the production /go entry seam: the recovered
       ;; entry must reconcile the SAME operation (never re-reserve, never
       ;; dispatch) and carry it to the registered, ready, completed state.
       (define outcome (supplemental-entry-outcome root plan 1))
       (check-not-false outcome "the entry seam must recover the pending cut")
       (check-equal? (campaign-result-status outcome) 'wave-blocked)
       (check-true (string-contains? (campaign-result-message outcome) "completed"))

       ;; The recovered operation is terminal-complete with every captured
       ;; fact preserved and the readiness admitted by the promotion gate.
       (define done (load-supplemental-operation aref))
       (check-equal? (hash-ref done 'state) "completed")
       (check-false (hash-has-key? (hash-ref done 'facts) 'pending-recovery))
       (check-true (hash-has-key? (hash-ref done 'facts) 'registration))
       (check-true (hash-has-key? (hash-ref done 'facts) 'receipt))
       (check-equal? (hash-ref (hash-ref done 'facts) 'receipt) (canonical-evidence-digest r2))
       (define readiness (evidence-readiness root plan 1))
       (check-equal? (hash-ref readiness 'status) "ready")
       (check-equal? (hash-ref readiness 'code) "registered-probe-ready")

       ;; Exactly one registration exists, bound to the staged E and the
       ;; owned R1: the interrupted effect was recovered, never repeated.
       (define ledger (load-evidence-repair root plan 1 (hash-ref E 'receipt-generation)))
       (check-equal? (length (hash-ref ledger 'registrations)) 1)
       (check-equal? (hash-ref (car (hash-ref ledger 'registrations)) 'probe) red-ref)
       (check-true (and (load-supplemental-promotion-intent root plan 1) #t))
       (define journal (load-delivery-journal root plan 1))
       (check-equal? (hash-ref journal 'receipt) r2)
       (check-equal? (length (hash-ref journal 'receipt-history '())) 2)

       ;; The delivery-finalize checkpoint seam stays honest afterwards: one
       ;; typed advance for the completed operation, no further supplemental
       ;; resumability, and no way to re-arm the spent task.
       (define-values (verdict _detail) (supplemental-checkpoint-step! root plan 1))
       (check-equal? verdict 'advance)
       (check-false (supplemental-resumable? root plan 1))
       (check-false (supplemental-author-pending? root plan 1))
       (check-exn #rx"terminal operation cannot transition"
                  (lambda ()
                    (transition-supplemental-operation! aref
                                                        (hash-ref done 'revision)
                                                        (hash-ref done 'state)
                                                        'launched
                                                        (hasheq 'dispatch-id "forbidden"))))))))

;; ------------------------------------------------------------------
;; Test 2: a green-capture cut (verifying -> green-captured) recovers
;; through the delivery-finalize checkpoint seam and completes the whole
;; governed ladder WITHOUT a second model dispatch.
;; ------------------------------------------------------------------
(module+ green-test
  (test-case "a late green-capture cut resumes past G1 without any model redispatch"
    (call-with-supplemental-fixture
     (lambda (fx)
       (define root (supplemental-fixture-root fx))
       (define repo (supplemental-fixture-repo fx))
       (define private (supplemental-fixture-private fx))
       (define plan (supplemental-fixture-plan fx))
       (define aref (hash-ref (load-supplemental-pointer root plan 1) 'task-approval))
       (define intent-path (operation-intent-path private aref))
       (define dispatches (box 0))
       (define cut-fired (box #f))
       (publish-predecessor! root plan)

       ;; One real author attempt; the storage fault interrupts exactly the
       ;; green-captured write-ahead intent after G1 was captured and staged.
       (define result
         (with-auto-operator (lambda ()
                               (parameterize ([current-supplemental-journal-crash-cut
                                               (intent-state-fault intent-path "green-captured" cut-fired)])
                                 (author-run root
                                             (load-campaign-record root plan)
                                             (lambda (_)
                                               (set-box! dispatches (add1 (unbox dispatches)))
                                               (fixture-author-commit! (current-directory))
                                               'ok))))))
       (check-true (unbox cut-fired) "the injected green-capture cut must have happened")
       (check-equal? (unbox dispatches) 1 "the cut must not spend a second dispatch")
       ;; The typed pending view left by the cut must NEVER park the run: the
       ;; advance engine reconciles the SAME operation (never re-reserves,
       ;; never redispatches) and continues to the honest publication wait.
       (check-equal? (campaign-result-status result) 'wave-blocked)
       (check-false (string-contains? (campaign-result-message result) "recover-pending")
                    (campaign-result-message result))
       (define after-run (load-supplemental-operation aref))
       (check-false (recovery-pending-state-name (hash-ref after-run 'state)))
       (check-true (and (member (hash-ref after-run 'state) '("probe-captured" "supersession-approved")) #t)
                   (format "unexpected post-cut state ~a: ~a" (hash-ref after-run 'state) (hash-ref after-run 'facts)))

       ;; The delivery-finalize checkpoint seam then carries the task through
       ;; S-authorized publication, certification, promotion and readiness.
       (define advanced (box #f))
       (define blocked-reason (box #f))
       (define published-fired (box #f))
       (define certified-fired (box #f))
       (define pushes (box 0))
       (with-auto-operator
        (lambda ()
          (parameterize ([current-gsd-approved-head-publisher (lambda args
                                                                (set-box! pushes (add1 (unbox pushes)))
                                                                (apply (stub-publisher) args))]
                         [current-supplemental-journal-crash-cut
                          (intent-state-fault intent-path "published" published-fired)])
            (run-coordinator-checkpoint root
                                        plan
                                        (car (campaign-record-waves (load-campaign-record root plan)))
                                        "supplemental recovery"
                                        (lambda (_dir _id _wave) (cons 'blocked "delivery coordinator must not run"))
                                        #:author-session-live? (lambda () #f)
                                        #:on-advance (lambda () (set-box! advanced #t))
                                        #:on-blocked (lambda (why) (set-box! blocked-reason (format "~a" why)))))))
       (check-true (unbox advanced) (format "checkpoint must carry the recovered task: ~a" (unbox blocked-reason)))

       (check-true (unbox published-fired))
       (check-equal? (hash-ref (load-supplemental-operation aref) 'state) "recover-pending-published")
       (define certified-journal (load-delivery-journal root plan 1))
       ;; Resume the same confirmed publication through /go entry; cut its
       ;; certification tail next. Neither recovery may repeat the push.
       (parameterize ([current-gsd-approved-head-publisher
                       (lambda args (error 'fixture "confirmed publication must never push twice"))]
                      [current-supplemental-journal-crash-cut
                       (intent-state-fault intent-path "receipt-certified" certified-fired)])
         (define outcome (supplemental-entry-outcome root plan 1))
         (check-not-false outcome)
         (check-equal? (campaign-result-status outcome) 'wave-blocked))
       (check-true (unbox certified-fired))
       (check-equal? (hash-ref (load-supplemental-operation aref) 'state) "recover-pending-receipt-certified")
       (check-equal? (unbox pushes) 1)

       ;; Completed WITHOUT a second dispatch or a second attempt.
       (check-equal? (unbox dispatches) 1 "a recovered green capture must never redispatch")
       (check-equal? (attempt-count root plan) 2)
       (define-values (final-kind final-detail) (advance-supplemental-evidence! root plan 1))
       (check-equal? final-kind 'completed final-detail)
       (define done (load-supplemental-operation aref))
       (check-equal? (hash-ref done 'state) "completed")
       (define readiness (evidence-readiness root plan 1))
       (check-equal? (hash-ref readiness 'status) "ready")
       (check-equal? (hash-ref readiness 'code) "registered-probe-ready")
       (define journal (load-delivery-journal root plan 1))
       (check-equal? (hash-ref journal 'receipt) (hash-ref certified-journal 'receipt))
       (check-equal? (hash-ref journal 'receipt-history) (hash-ref certified-journal 'receipt-history))
       (check-equal? (length (hash-ref journal 'receipt-history '())) 1)
       (define ledger (load-evidence-repair root plan 1 1))
       (check-equal? (length (hash-ref ledger 'registrations)) 1)
       (check-false (supplemental-resumable? root plan 1))))))

;; ------------------------------------------------------------------
;; Test 3: a publication cut (supersession-approved -> published) whose
;; supersession approval is revoked before the resume NEVER reconciles: the
;; production route fails closed with the intent and journal untouched.
;; ------------------------------------------------------------------
(module+ revocation-test
  (test-case "a revoked publication cut refuses recovery and preserves the sealed intent"
    (call-with-supplemental-fixture
     (lambda (fx)
       (define root (supplemental-fixture-root fx))
       (define repo (supplemental-fixture-repo fx))
       (define private (supplemental-fixture-private fx))
       (define plan (supplemental-fixture-plan fx))
       (define aref (hash-ref (load-supplemental-pointer root plan 1) 'task-approval))
       (define intent-path (operation-intent-path private aref))
       (publish-predecessor! root plan)

       ;; Drive the governed ladder to the S-approved publication boundary
       ;; through the production author route (the real remote publisher
       ;; refuses the fixture origin, so the task parks at publication).
       (define result
         (with-auto-operator (lambda ()
                               (author-run root
                                           (load-campaign-record root plan)
                                           (lambda (_)
                                             (fixture-author-commit! (current-directory))
                                             'ok)))))
       (check-equal? (campaign-result-status result) 'wave-blocked)
       (check-true (string-contains? (campaign-result-message result) "publication") (campaign-result-message result))
       (define operation (load-supplemental-operation aref))
       (check-equal? (hash-ref operation 'state) "supersession-approved")
       (define sapref (hash-ref (hash-ref operation 'facts) 'supersession-approval-ref))
       (define r2
         (hash-ref (supplemental-staged-evidence-record (load-supplemental-staged-evidence root plan 1)) 'receipt))

       ;; Late cut at the publication tail transition: the exact interrupted
       ;; write-ahead intent stays sealed on disk as a pending view.
       (define fired (box #f))
       (parameterize ([current-gsd-approved-head-publisher (stub-publisher)]
                      [current-supplemental-journal-crash-cut (intent-state-fault intent-path "published" fired)])
         (define-values (kind detail) (advance-supplemental-evidence! root plan 1))
         (check-equal? kind 'paused detail))
       (check-true (unbox fired) "the production publication tail must have been interrupted")
       (check-true (file-exists? intent-path))
       (check-equal? (hash-ref (load-supplemental-operation aref) 'state) "recover-pending-published")
       (define journal-bytes (file->bytes (delivery-journal-path root plan 1)))

       ;; Revoke the S approval: the journal's live-approval revalidation
       ;; must refuse the recovery through the production route — no
       ;; reconcile, no install, no relaunch — and leave the sealed intent
       ;; and the delivery journal byte-identical. The pending view itself
       ;; stays readable (the task approval is still live); it is the
       ;; reconciliation that must refuse.
       (fixture-revoke-approval! sapref)
       (check-false (validate-supplemental-approval sapref))
       (check-equal? (hash-ref (load-supplemental-operation aref) 'state) "recover-pending-published")
       (define-values (outcome-kind _outcome-detail) (advance-supplemental-evidence! root plan 1))
       (check-equal? outcome-kind 'failed)
       (check-true (file-exists? intent-path) "the sealed intent must survive a refused recovery")
       (check-equal? (hash-ref (load-supplemental-operation aref) 'state) "recover-pending-published")
       (check-equal? (file->bytes (delivery-journal-path root plan 1)) journal-bytes)
       ;; The current receipt is now the product of S. Revoking S also
       ;; removes controller admission to that superseded context; the
       ;; private pending view remains readable but cannot confer authority.
       (check-false (supplemental-resumable? root plan 1))
       (check-false (supplemental-operation-live? root plan 1))))))

(module+ test
  (require (submod ".." registration-test)
           (submod ".." green-test)
           (submod ".." revocation-test)))
