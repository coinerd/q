#lang racket
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 900
;; @covers extensions/gsd/supplemental-evidence-controller.rkt
;;         extensions/gsd/supplemental-evidence-operator.rkt
;;         extensions/gsd/author-execution.rkt
;; End-to-end supplemental task lifecycle on a complete temp-home campaign.
;; One real model-author dispatch, operator approvals through the REAL broker,
;; a real bounded C1 Verify (G1), the staged baseline-overlay probe (R1), the
;; sealed supersession proposal S with operator authorization, and an honest
;; typed pause at the (possibly absent) protected publication route. The
;; historical receipt and delivery journal stay byte-identical throughout.
(require rackunit
         rackunit/text-ui
         racket/string
         racket/system
         "helpers/supplemental-go-fixture.rkt"
         "../runtime/approval/broker.rkt"
         "../extensions/gsd/campaign-result.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/go-orchestrator.rkt"
         "../extensions/gsd/delivery-finalize.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/supplemental-evidence-journal.rkt"
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         (only-in "../extensions/gsd/supplemental-evidence-publication.rkt"
                  load-supplemental-staged-evidence
                  supplemental-staged-evidence
                  supplemental-staged-evidence-record)
         "../extensions/gsd/supplemental-evidence-controller.rkt"
         (only-in "../extensions/gsd/supplemental-evidence-operator.rkt"
                  current-supplemental-operator-publisher)
         (only-in "../extensions/gsd-planning/execution-policy.rkt" current-gsd-author-phase)
         (only-in "../extensions/gsd/policy.rkt" current-gsd-wave-verification-repair-retries))
;; Verification repair must never buy a second supplemental model dispatch.
(define (author-run root rec runner)
  (parameterize ([current-gsd-wave-verification-repair-retries 0])
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
;; Simulated trusted operator frontend: every presented supplemental
;; approval is approved exactly as a real owner decision would be. The
;; decision runs on its own thread, like a real UI answering the broker.
(define (auto-operator-approval! event payload)
  (when (equal? event "supplemental-evidence.approval-requested")
    (thread
     (lambda ()
       (approval-decide! (hash-ref payload 'request-id) (hash-ref payload 'proposal-sha256) #t)))))
(define (with-auto-operator thunk)
  (parameterize ([current-supplemental-operator-publisher auto-operator-approval!])
    (thunk)))
(define (wave-status root plan)
  (campaign-wave-status (car (campaign-record-waves (load-campaign-record root plan)))))
(module+ test
  (test-case "one dispatch carries the task from author phase to an honest publication pause"
    (call-with-supplemental-fixture
     (lambda (fx)
       (define root (supplemental-fixture-root fx))
       (define repo (supplemental-fixture-repo fx))
       (define plan (supplemental-fixture-plan fx))
       (define dispatches (box 0))
       (define result
         (with-auto-operator
          (lambda ()
            (author-run
             root
             (load-campaign-record root plan)
             (lambda (_)
               (set-box! dispatches (add1 (unbox dispatches)))
               ;; The retained checkout IS the task context's physical repo.
               ;; current-directory always normalizes to directory form, so
               ;; compare the physical checkout identity, not raw spellings.
               (let ([cwd (path->string (simplify-path (path->directory-path (current-directory))
                                                       #f))]
                     [want (path->string (simplify-path
                                          (path->directory-path
                                           (hash-ref (supplemental-fixture-task-context fx) 'repo))
                                          #f))])
                 (unless (equal? cwd want)
                   (eprintf "CASE1 cwd-check: cwd=~s want=~s\n" cwd want))
                 (check-equal? cwd want))
               (let ([phase (hash-ref (current-gsd-author-phase) 'phase #f)])
                 (unless (eq? phase 'test-author)
                   (eprintf "CASE1 phase-check: phase=~s\n" phase))
                 (check-eq? phase 'test-author))
               (fixture-author-commit! (current-directory))
               'ok)))))
       ;; Exactly ONE model dispatch; the task pauses honestly at publication.
       (unless (equal? (unbox dispatches) 1)
         (eprintf "CASE2 dispatches=~s status=~s message=~s\n"
                  (unbox dispatches)
                  (campaign-result-status result)
                  (campaign-result-message result)))
       (check-equal? (unbox dispatches) 1)
       (unless (eq? (campaign-result-status result) 'wave-blocked)
         (eprintf "CASE1 status=~s message=~s\n"
                  (campaign-result-status result)
                  (campaign-result-message result)))
       (check-equal? (campaign-result-status result) 'wave-blocked)
       (check-true (string-contains? (campaign-result-message result) "publication")
                   (campaign-result-message result))
       ;; The protected operation reached S approval and waits on publication.
       (define summary (supplemental-operation-summary root plan 1))
       (check-equal? (hash-ref summary 'state) "supersession-approved")
       (check-true (hash-ref summary 'launch-consumed))
       ;; The staged wrapper carries the exact r*/E and the captured R1.
       (define staged (load-supplemental-staged-evidence root plan 1))
       (check-true (and staged #t))
       (check-true (and (hash-ref (supplemental-staged-evidence-record staged) 'red-proof-ref #f) #t))
       (check-not-false (hash-ref (supplemental-staged-evidence-record staged) 'receipt #f))
       ;; The attempt is parked, resumable, and NOT re-reserved.
       (check-eq? (wave-status root plan) 'awaiting-delivery)
       (define rec (load-campaign-record root plan))
       (check-equal? (campaign-wave-attempt-count (car (campaign-record-waves rec))) 2)
       (check-equal?
        (campaign-attempt-id (campaign-wave-current-attempt (car (campaign-record-waves rec))))
        "attempt-2")
       ;; Exactly the two approved files were committed beyond the old head.
       (define new-files
         (string-split (string-trim (with-output-to-string
                                     (lambda ()
                                       (system* (find-executable-path "git")
                                                "-C"
                                                repo
                                                "diff"
                                                "--name-only"
                                                "--no-renames"
                                                (hash-ref (supplemental-fixture-task-context fx)
                                                          'head)
                                                "HEAD"))))
                       "\n"))
       (check-true (pair? new-files))
       (for ([p (in-list new-files)]
             #:when (not (equal? p "")))
         (check-true (and (member p (fixture-task-paths fx)) #t)
                     (format "unexpected path ~s" p)))
       ;; The old receipt and its history are untouched; no fake readiness.
       (define journal (load-delivery-journal root plan 1))
       (check-equal? (hash-ref (hash-ref journal 'receipt) 'attempt-id) "old-attempt")
       (check-equal? (length (hash-ref journal 'receipt-history '())) 0)
       (check-equal? (hash-ref (supplemental-operation-summary root plan 1) 'state)
                     "supersession-approved"))))
  (test-case "no interactive channel pauses the sealed candidate honestly; resume completes"
    (call-with-supplemental-fixture
     (lambda (fx)
       (define root (supplemental-fixture-root fx))
       (define plan (supplemental-fixture-plan fx))
       ;; The operator is away: no active channel during the attempt.
       (define channel (current-approval-channel))
       (clear-approval-channel!)
       (define dispatches (box 0))
       (define result
         (run-campaign-wave root
                            (load-campaign-record root plan)
                            1
                            #:fence-token 8
                            #:timeout-sec 600
                            #:timeout-retries 0
                            #:no-change-retries 0
                            #:stall-soft-limit #f
                            #:stall-hard-limit #f
                            #:isolate? #f
                            #:advance-override? #t
                            #:runner (lambda (_)
                                       (set-box! dispatches (add1 (unbox dispatches)))
                                       (fixture-author-commit! (current-directory))
                                       'ok)
                            #:verifier
                            (lambda (_) (error 'verifier "ordinary verify must never run"))))
       (unless (equal? (unbox dispatches) 1)
         (eprintf "CASE2 dispatches=~s status=~s message=~s\n"
                  (unbox dispatches)
                  (campaign-result-status result)
                  (campaign-result-message result)))
       (check-equal? (unbox dispatches) 1)
       (unless (eq? (campaign-result-status result) 'wave-blocked)
         (eprintf "CASE1 status=~s message=~s\n"
                  (campaign-result-status result)
                  (campaign-result-message result)))
       (check-equal? (campaign-result-status result) 'wave-blocked)
       (check-true (string-contains? (campaign-result-message result) "operator candidate approval")
                   (campaign-result-message result))
       (check-equal? (hash-ref (supplemental-operation-summary root plan 1) 'state)
                     "candidate-sealed")
       (check-eq? (wave-status root plan) 'awaiting-delivery)
       ;; The operator returns: the checkpoint resumes WITHOUT a new attempt,
       ;; runs G1 and R1, seals S, and pauses at the publication route. The
       ;; first visit reports durable private-journal progress; the second
       ;; visit reports the typed publication pause.
       (set-approval-channel! channel)
       (define rec* (load-campaign-record root plan))
       (define advanced (box #f))
       (with-auto-operator (lambda ()
                             (run-coordinator-checkpoint
                              root
                              plan
                              (car (campaign-record-waves rec*))
                              "supplemental resume"
                              (lambda (_dir _id _wave)
                                (cons 'blocked "delivery coordinator must not run"))
                              #:author-session-live? (lambda () #f)
                              #:on-advance (lambda () (set-box! advanced #t))
                              #:on-blocked (lambda (_) (fail "first visit must advance")))))
       (check-true (unbox advanced) "durable private-journal progress must advance the checkpoint")
       (check-equal? (hash-ref (supplemental-operation-summary root plan 1) 'state)
                     "supersession-approved")
       (define blocked-reason (box #f))
       (with-auto-operator
        (lambda ()
          (run-coordinator-checkpoint
           root
           plan
           (car (campaign-record-waves (load-campaign-record root plan)))
           "supplemental resume"
           (lambda (_dir _id _wave) (cons 'blocked "delivery coordinator must not run"))
           #:author-session-live? (lambda () #f)
           #:on-advance (lambda () (fail "no further progress without the publication route"))
           #:on-blocked (lambda (why) (set-box! blocked-reason (format "~a" why))))))
       (check-true (string-contains? (or (unbox blocked-reason) "") "publication")
                   (unbox blocked-reason))
       (define rec (load-campaign-record root plan))
       (check-equal? (campaign-wave-attempt-count (car (campaign-record-waves rec))) 2)
       (check-equal?
        (campaign-attempt-id (campaign-wave-current-attempt (car (campaign-record-waves rec))))
        "attempt-2"))))
  (test-case "a launch consumed without captured author output is spent, never redispatched"
    (call-with-supplemental-fixture
     (lambda (fx)
       (define root (supplemental-fixture-root fx))
       (define repo (supplemental-fixture-repo fx))
       (define plan (supplemental-fixture-plan fx))
       ;; Simulate a crash after the durable launch intent: reserved + launched.
       (define pointer (load-supplemental-pointer root plan 1))
       (define aref (hash-ref pointer 'task-approval))
       (start-supplemental-operation!
        aref
        (hasheq 'context (supplemental-fixture-task-context fx) 'coordinator-fence 8))
       (define op (load-supplemental-operation aref))
       (transition-supplemental-operation!
        aref
        (hash-ref op 'revision)
        (hash-ref op 'state)
        "reserved"
        (hasheq 'allocation (hasheq 'attempt-id "attempt-2" 'attempt-fence 8 'coordinator-fence 8)))
       (define op* (load-supplemental-operation aref))
       (transition-supplemental-operation! aref
                                           (hash-ref op* 'revision)
                                           (hash-ref op* 'state)
                                           "launched"
                                           (hasheq 'dispatch-id "supplemental-author-attempt-2"))
       ;; /go entry must refuse to begin another attempt and spend the task.
       (define dispatches (box 0))
       (define result
         (author-run root
                     (load-campaign-record root plan)
                     (lambda (_)
                       (set-box! dispatches (add1 (unbox dispatches)))
                       'ok)))
       (check-equal? (unbox dispatches) 0 "no model dispatch may follow an ambiguous launch")
       (check-equal? (campaign-result-status result) 'wave-failed)
       (check-true (string-contains? (campaign-result-message result) "spent")
                   (campaign-result-message result))
       (check-equal? (hash-ref (supplemental-operation-summary root plan 1) 'state) "blocked")
       (define rec (load-campaign-record root plan))
       (check-equal? (campaign-wave-attempt-count (car (campaign-record-waves rec)))
                     1
                     "the ambiguous launch must not consume another attempt"))))
  (test-case "the author hook scope refuses writes outside the two approved paths"
    (call-with-supplemental-fixture
     (lambda (fx)
       (define root (supplemental-fixture-root fx))
       (define plan (supplemental-fixture-plan fx))
       (define result
         (author-run root
                     (load-campaign-record root plan)
                     (lambda (_)
                       (fixture-author-commit! (current-directory))
                       ;; An out-of-scope model edit: the owner commit phase must refuse.
                       (display-to-file "tampered\n"
                                        (build-path (current-directory) "README.md")
                                        #:exists 'truncate)
                       'ok)))
       (check-equal? (campaign-result-status result) 'wave-failed)
       (check-true (string-contains? (campaign-result-message result) "forbidden")
                   (campaign-result-message result))
       ;; The out-of-scope edit was never committed, and the durably consumed
       ;; launch is SPENT: a forbidden edit can never silently re-arm the task.
       (define summary (supplemental-operation-summary root plan 1))
       (check-equal? (hash-ref summary 'state) "blocked")
       (check-true (and (member "block-reason" (hash-ref summary 'facts-keys '())) #t))
       (check-equal? (hash-ref summary 'launch-consumed) #t)))))
