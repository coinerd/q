#lang racket/base

;; @speed fast  ;; @suite extensions

;; tests/test-gsd-end-to-end-recovery.rkt — v0.99.90 W5 (#9236)
;;
;; Crash-Recovery an jedem persistierten Übergang: a full campaign driven
;; through success / failure / interruption / restart must converge — the
;; durable campaign record (Campaign Truth), the completion outbox and the
;; PLAN/STATE/wave-doc projections must AGREE after EVERY injected
;; interruption at a persisted transition, and recovery must be idempotent.
;;
;; External effects: publishing a completion outbox event through the
;; v0.99.90 W4 github-port must produce EXACTLY ONE external effect even
;; across a crash + restart (journal replay within a process, external
;; dedup across processes).

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/port
         racket/string
         racket/match
         "helpers/gsd-golden-trace.rkt"
         (only-in "../extensions/gsd/campaign-state.rkt"
                  migrate-campaign!
                  campaign-plan-id
                  campaign-fence-token
                  campaign-record-waves
                  campaign-wave-index
                  campaign-wave-status
                  campaign-wave-current-attempt
                  campaign-attempt-id
                  set-campaign-wave-status!
                  set-campaign-fence-token!
                  begin-attempt!)
         (only-in "../extensions/gsd/campaign-repository.rkt"
                  persist-campaign!
                  load-campaign-record
                  load-or-migrate-campaign!)
         (only-in "../extensions/gsd/go-orchestrator.rkt"
                  run-campaign!
                  campaign-result-status
                  campaign-result-completed-waves)
         (only-in "../extensions/gsd/wave-completion.rkt"
                  make-event-id
                  load-outbox
                  count-completion-events
                  reconcile-completion-outbox!)
         (only-in "../extensions/gsd/projection-effects.rkt" reconcile-projections-from-waves!)
         (only-in "../extensions/gsd/wave-docs.rkt" plan-slug-map)
         (only-in "../extensions/gsd/effect-ports.rkt"
                  gsd-github-command
                  gsd-github-port-execute
                  gsd-github-command-result-external-id
                  gsd-github-command-result-already-done?)
         (only-in "../extensions/gsd/github-port.rkt" make-github-port)
         (only-in "helpers/gsd-port-fakes.rkt" make-fake-github-adapter fake-github-call-count))

;; ============================================================
;; Fixture: deterministic 4-wave campaign project
;; ============================================================

(define e2e-wave-specs
  '((0 "E2E Wave Alpha" "alpha") (1 "E2E Wave Beta" "beta")
                                 (2 "E2E Wave Gamma" "gamma")
                                 (3 "E2E Wave Delta" "delta")))

(define (make-e2e-project)
  (define dir (make-temporary-file "gsd-e2e-~a" 'directory))
  (seed-golden-project! dir e2e-wave-specs)
  dir)

(define (cleanup! dir)
  (delete-directory/files dir #:must-exist? #f))

(define (wave* rec idx)
  (for/first ([w (campaign-record-waves rec)]
              #:when (= (campaign-wave-index w) idx))
    w))

(define (wave-status* rec idx)
  (campaign-wave-status (wave* rec idx)))

(define (read-text p)
  (call-with-input-file p port->string))

(define approve-wave (lambda (_) #t))

;; ============================================================
;; Recovery helper (mirrors run-campaign! startup + W2/W5 reconcile)
;; ============================================================

;; Fresh-process recovery: reload the durable record from disk, then run both
;; reconciles. Returns (values outbox-added projection-paths).
(define (recover-fresh! dir rec)
  (define durable (load-or-migrate-campaign! dir))
  (define outbox-added (reconcile-completion-outbox! dir durable))
  (define projection-paths
    (reconcile-projections-from-waves! dir
                                       (for/list ([w (campaign-record-waves durable)])
                                         (cons (campaign-wave-index w) (campaign-wave-status w)))
                                       (plan-slug-map dir)))
  (values outbox-added projection-paths))

;; The projection set must agree with the durable statuses after recovery.
(define (check-projections-agree! dir rec)
  (define plan-text (read-text (build-path dir ".planning" "PLAN.md")))
  (for ([w (campaign-record-waves rec)])
    (define idx (campaign-wave-index w))
    (define st (campaign-wave-status w))
    (define expected-marker
      (case st
        [(done) "[DONE]"]
        [(failed) "[FAILED]"]
        [(deferred) "[DEFERRED]"]
        [else "[Inbox]"]))
    (check-true (regexp-match? (regexp (format "- ~a W~a:" (regexp-quote expected-marker) idx))
                               plan-text)
                (format "PLAN.md W~a marker matches durable ~a" idx st)))
  (void))

;; Expected outbox: one stable completion event per durable 'done wave.
(define (check-outbox-derived! dir rec)
  (define expected
    (for/list ([w (campaign-record-waves rec)]
               #:when (eq? (campaign-wave-status w) 'done))
      (define a (campaign-wave-current-attempt w))
      (make-event-id (campaign-plan-id rec) (campaign-wave-index w) (campaign-attempt-id a))))
  (check-equal? (load-outbox dir (campaign-plan-id rec))
                expected
                "outbox is exactly the derived done-wave event set"))

;; Begin a wave the way run-campaign-wave does (fence + attempt + persist).
(define (begin-wave-persisted! dir rec idx)
  (set-campaign-fence-token! rec (add1 (campaign-fence-token rec)))
  (begin-attempt! rec idx (campaign-fence-token rec))
  (persist-campaign! dir rec))

;; ============================================================
;; Test suite
;; ============================================================

(define all-tests
  (test-suite "end-to-end recovery (W5 #9236)"

    (test-case "e2e campaign: success/failure/interruption/restart converges to all-done"
      (define dir (make-e2e-project))
      (dynamic-wind
       void
       (lambda ()
         ;; --- Process 1: W0 succeeds, W1 fails (campaign stops) ---
         (define rec1 (migrate-campaign! dir))
         (define r1
           (run-campaign! dir
                          rec1
                          #:runner (lambda (idx) (if (= idx 0) 'ok 'error))
                          #:verifier approve-wave))
         (check-eq? (campaign-result-status r1) 'wave-failed)
         (check-eq? (wave-status* rec1 0) 'done)
         (check-eq? (wave-status* rec1 1) 'failed)
         (check-eq? (wave-status* rec1 2) 'pending)
         (check-eq? (wave-status* rec1 3) 'pending)
         ;; Recovery after this persisted transition: outbox + projections agree.
         ;; (Production run already appended the outbox event + projections, so
         ;; recovery is a no-op here — the durable record is the commit point.)
         (define-values (oa1 pp1) (recover-fresh! dir rec1))
         (check-equal? oa1 0 "production already appended the event")
         (define durable1 (load-or-migrate-campaign! dir))
         (check-outbox-derived! dir durable1)
         (check-projections-agree! dir durable1)
         ;; Idempotent: second recovery writes nothing.
         (define-values (oa1b pp1b) (recover-fresh! dir rec1))
         (check-equal? oa1b 0 "second recovery appends nothing")
         (check-equal? pp1b '() "second recovery repairs no projections")

         ;; --- Process 2 (restart): failed W1 is retried and succeeds; W2
         ;; is then interrupted (campaign stops). Restart semantics: failed /
         ;; interrupted waves are actionable (retryable-status?), so a restart
         ;; re-runs them rather than skipping forward. ---
         (define rec2 (load-or-migrate-campaign! dir))
         (define r2
           (run-campaign! dir
                          rec2
                          #:runner (lambda (idx) (if (= idx 2) 'cancelled 'ok))
                          #:verifier approve-wave))
         (check-eq? (campaign-result-status r2) 'wave-cancelled)
         (check-eq? (wave-status* rec2 0) 'done)
         (check-eq? (wave-status* rec2 1)
                    'done
                    "W1 failed in process 1, retried + succeeds in process 2")
         (check-eq? (wave-status* rec2 2) 'interrupted)
         (check-eq? (wave-status* rec2 3) 'pending)
         (define-values (oa2 pp2) (recover-fresh! dir rec2))
         (check-equal? oa2 0 "interrupted wave emits no completion event")
         (define durable2 (load-or-migrate-campaign! dir))
         (check-outbox-derived! dir durable2)
         (check-projections-agree! dir durable2)

         ;; --- Process 3 (restart): interrupted W2 is retried and succeeds;
         ;; W3 succeeds -> campaign complete ---
         (define rec3 (load-or-migrate-campaign! dir))
         (define r3 (run-campaign! dir rec3 #:runner (lambda (_) 'ok) #:verifier approve-wave))
         (check-eq? (campaign-result-status r3) 'campaign-complete)
         (check-equal? (campaign-result-completed-waves r3) '(2 3))
         (check-eq? (wave-status* rec3 0) 'done)
         (check-eq? (wave-status* rec3 1) 'done)
         (check-eq? (wave-status* rec3 2) 'done)
         (check-eq? (wave-status* rec3 3) 'done)
         (define-values (oa3 pp3) (recover-fresh! dir rec3))
         (check-equal? oa3 0 "outbox already derived")
         (check-equal? pp3 '() "projections already converged")
         (define durable3 (load-or-migrate-campaign! dir))
         (check-outbox-derived! dir durable3)
         (check-projections-agree! dir durable3)
         (check-equal? (count-completion-events dir durable3)
                       4
                       "four done waves (W0, W1-retry, W2-retry, W3) -> exactly four events"))
       (lambda () (cleanup! dir))))

    ;; ------------------------------------------------------------
    ;; Crash-window matrix: interruption at EVERY persisted transition
    ;; ------------------------------------------------------------

    (test-case "crash after begin-attempt persist: recovery leaves in-progress, no event"
      (define dir (make-e2e-project))
      (dynamic-wind
       void
       (lambda ()
         (define rec (migrate-campaign! dir))
         (begin-wave-persisted! dir rec 0)
         ;; Durable state: W0 in-progress with an attempt.
         (define durable (load-campaign-record dir (campaign-plan-id rec)))
         (check-eq? (wave-status* durable 0) 'in-progress)
         (check-not-false (campaign-wave-current-attempt (wave* durable 0))
                          "attempt attached to in-progress wave")
         ;; Recovery: no completion event; projections stay
         ;; semantically Inbox/PENDING (in-progress is not a
         ;; terminal plan status).
         (define-values (oa pp) (recover-fresh! dir rec))
         (check-equal? oa 0 "in-progress wave never emits a completion event")
         (check-true (string-contains? (read-text (build-path dir ".planning" "PLAN.md"))
                                       "- [Inbox] W0")
                     "PLAN.md stays Inbox for in-progress")
         (check-true (string-contains? (read-text (build-path dir ".planning" "STATE.md"))
                                       "| W0 | E2E Wave Alpha | PENDING |")
                     "STATE.md stays PENDING for in-progress")
         ;; Second recovery is a no-op (first pass may normalize
         ;; trailing newlines; after that the set matches).
         (define-values (oa2 pp2) (recover-fresh! dir rec))
         (check-equal? oa2 0 "second recovery appends nothing")
         (check-equal? pp2 '() "second recovery is a no-op")
         (define durable2 (load-campaign-record dir (campaign-plan-id rec)))
         (check-eq? (wave-status* durable2 0) 'in-progress)
         (check-equal? (load-outbox dir (campaign-plan-id rec)) '()))
       (lambda () (cleanup! dir))))

    (test-case "crash after verifying persist: recovery never invents done"
      (define dir (make-e2e-project))
      (dynamic-wind void
                    (lambda ()
                      (define rec (migrate-campaign! dir))
                      (begin-wave-persisted! dir rec 0)
                      (set-campaign-wave-status! (wave* rec 0) 'verifying)
                      (persist-campaign! dir rec)
                      (define-values (oa pp) (recover-fresh! dir rec))
                      (check-equal? oa 0 "verifying is not done -> no invented event")
                      (define durable (load-campaign-record dir (campaign-plan-id rec)))
                      (check-eq? (wave-status* durable 0) 'verifying)
                      (check-equal? (load-outbox dir (campaign-plan-id rec)) '()))
                    (lambda () (cleanup! dir))))

    (test-case "crash after failed persist: FAILED projection repaired, no event"
      (define dir (make-e2e-project))
      (dynamic-wind void
                    (lambda ()
                      (define rec (migrate-campaign! dir))
                      (begin-wave-persisted! dir rec 0)
                      (set-campaign-wave-status! (wave* rec 0) 'failed)
                      (persist-campaign! dir rec)
                      ;; Crash before projection apply: PLAN.md still says Inbox.
                      (check-true (string-contains? (read-text (build-path dir ".planning" "PLAN.md"))
                                                    "- [Inbox] W0"))
                      (define-values (oa pp) (recover-fresh! dir rec))
                      (check-equal? oa 0 "failed wave emits no completion event")
                      (check-equal? (length pp) 3 "plan + state + wave doc repaired to FAILED")
                      (define durable (load-campaign-record dir (campaign-plan-id rec)))
                      (check-projections-agree! dir durable)
                      (check-equal? (load-outbox dir (campaign-plan-id rec)) '()))
                    (lambda () (cleanup! dir))))

    (test-case "crash after interrupted persist: durable-only, projections stay pending"
      (define dir (make-e2e-project))
      (dynamic-wind
       void
       (lambda ()
         (define rec (migrate-campaign! dir))
         (begin-wave-persisted! dir rec 0)
         (set-campaign-wave-status! (wave* rec 0) 'interrupted)
         (persist-campaign! dir rec)
         (define-values (oa pp) (recover-fresh! dir rec))
         (check-equal? oa 0 "interrupted wave emits no completion event")
         (define durable (load-campaign-record dir (campaign-plan-id rec)))
         (check-eq? (wave-status* durable 0) 'interrupted)
         (check-equal? (load-outbox dir (campaign-plan-id rec))
                       '()
                       "no phantom event for interrupted wave")
         ;; Projections: interrupted is not a terminal plan status; PLAN.md
         ;; keeps the Inbox marker (durable truth is the record, not the marker).
         (check-true (string-contains? (read-text (build-path dir ".planning" "PLAN.md"))
                                       "- [Inbox] W0"))
         ;; Restart retries the interrupted wave.
         (define r (run-campaign! dir durable #:runner (lambda (_) 'ok) #:verifier approve-wave))
         (check-eq? (campaign-result-status r) 'campaign-complete)
         (check-eq? (wave-status* (load-campaign-record dir (campaign-plan-id rec)) 0) 'done))
       (lambda () (cleanup! dir))))

    (test-case "crash after done commit before outbox: reconcile rebuilds event + projections"
      (define dir (make-e2e-project))
      (dynamic-wind
       void
       (lambda ()
         (define rec (migrate-campaign! dir))
         (begin-wave-persisted! dir rec 0)
         (set-campaign-wave-status! (wave* rec 0) 'done)
         (persist-campaign! dir rec)
         (check-equal? (load-outbox dir (campaign-plan-id rec)) '() "crash before outbox append")
         (define-values (oa pp) (recover-fresh! dir rec))
         (check-equal? oa 1 "one done wave -> one event rebuilt")
         (check-equal? (length pp) 3 "plan + state + doc repaired to DONE")
         (define durable (load-campaign-record dir (campaign-plan-id rec)))
         (check-outbox-derived! dir durable)
         (check-projections-agree! dir durable)
         (define-values (oa2 pp2) (recover-fresh! dir rec))
         (check-equal? oa2 0 "idempotent")
         (check-equal? pp2 '() "idempotent"))
       (lambda () (cleanup! dir))))

    (test-case "crash after outbox before projections: no duplicate event"
      (define dir (make-e2e-project))
      (dynamic-wind
       void
       (lambda ()
         (define rec (migrate-campaign! dir))
         (begin-wave-persisted! dir rec 0)
         (set-campaign-wave-status! (wave* rec 0) 'done)
         (persist-campaign! dir rec)
         ;; outbox already carries the event (appended before the crash)
         (define durable (load-campaign-record dir (campaign-plan-id rec)))
         (reconcile-completion-outbox! dir durable)
         (check-equal? (count-completion-events dir durable) 1)
         (define-values (oa pp) (recover-fresh! dir rec))
         (check-equal? oa 0 "event already present — no duplicate")
         (check-equal? (length pp) 3)
         (check-equal? (count-completion-events dir durable) 1 "exactly one event after recovery"))
       (lambda () (cleanup! dir))))

    ;; ------------------------------------------------------------
    ;; No duplicate external effect across crash + restart
    ;; ------------------------------------------------------------

    (test-case "publishing an outbox event through github-port is exactly-once across restart"
      (define dir (make-e2e-project))
      (dynamic-wind
       void
       (lambda ()
         ;; Drive one wave to done through the production path.
         (define rec (migrate-campaign! dir))
         (define r
           (run-campaign! dir
                          rec
                          #:runner (lambda (idx) (if (= idx 0) 'ok 'error))
                          #:verifier approve-wave))
         (check-eq? (campaign-result-status r) 'wave-failed)
         (define durable (load-or-migrate-campaign! dir))
         (recover-fresh! dir durable)
         (define event-id (car (load-outbox dir (campaign-plan-id durable))))
         (check-regexp-match #rx"^campaign/[0-9a-f]+/wave/0/attempt/[0-9a-z-]+/completed$" event-id)
         ;; Publish the event through the W4 github-port (live path, fake adapter).
         (define-values (adapter state) (make-fake-github-adapter))
         (define port1 (make-github-port adapter #:dry-run? #f))
         (define c1
           (gsd-github-command 'issue-create
                               event-id
                               (hash 'title "completion notice" 'dedup-key event-id)
                               #f))
         (define res1 ((gsd-github-port-execute port1) c1))
         (check-not-false (gsd-github-command-result-external-id res1))
         (check-equal? (fake-github-call-count state 'create-issue!) 1 "one external issue")
         ;; Retry within the same process: journal replay, adapter untouched.
         (define res2 ((gsd-github-port-execute port1) c1))
         (check-true (gsd-github-command-result-already-done? res2))
         (check-equal? (fake-github-call-count state 'create-issue!) 1 "journal replay: no dup")
         ;; CRASH: a fresh port over the SAME external state (new process).
         (define port2 (make-github-port adapter #:dry-run? #f))
         (define res3 ((gsd-github-port-execute port2) c1))
         (check-true (gsd-github-command-result-already-done? res3)
                     "external dedup finds the existing issue after restart")
         (check-equal? (fake-github-call-count state 'create-issue!)
                       1
                       "cross-restart: exactly one external effect")
         ;; And the outbox itself still carries exactly one event.
         (check-equal? (count-completion-events dir durable) 1))
       (lambda () (cleanup! dir))))))

(void (run-tests all-tests))
