#lang racket/base

;; @speed fast  ;; @suite extensions

;; tests/test-gsd-atomic-projection-transaction.rkt — v0.99.90 W2 (#9233)
;;
;; The completion transaction must behave as ONE logical operation: the
;; durable campaign record is the commit point; the completion outbox and the
;; PLAN/STATE/wave-doc projections are DERIVED files that may lag after a
;; crash but must never lead (no phantom completion event for a wave whose
;; durable status is not 'done = no invented DONE, no skipped wave).
;;
;; Failure-injection matrix: for every crash window we construct the exact
;; intermediate state the transaction leaves behind, then run the recovery
;; path (retry and/or the reconcile entry points) and assert convergence to
;; the SAME canonical final state as a clean completion — idempotently.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/port
         racket/string
         (only-in "../extensions/gsd/campaign-state.rkt"
                  make-campaign-manifest
                  make-campaign-wave-descriptor
                  make-campaign-wave
                  make-campaign-record
                  campaign-plan-id
                  campaign-record-waves
                  campaign-wave-index
                  campaign-wave-status
                  campaign-wave-current-attempt
                  campaign-attempt-id
                  campaign-attempt-fence-token
                  set-campaign-wave-status!
                  set-campaign-fence-token!
                  begin-attempt!
                  migrate-campaign!)
         (only-in "../extensions/gsd/campaign-repository.rkt" persist-campaign! load-campaign-record)
         (only-in "../extensions/gsd/wave-completion.rkt"
                  try-complete-wave!
                  completion-result-status
                  make-event-id
                  load-outbox
                  count-completion-events
                  reconcile-completion-outbox!)
         (only-in "../extensions/gsd/wave-docs.rkt" plan-slug-map)
         (only-in "../extensions/gsd/projection-effects.rkt" reconcile-projections-from-waves!))

;; ============================================================
;; Fixture helpers
;; ============================================================

(define (read-text p)
  (call-with-input-file p port->string))

(define (wave* rec idx)
  (for/first ([w (campaign-record-waves rec)]
              #:when (= (campaign-wave-index w) idx))
    w))

(define (make-fixture)
  ;; 2-wave campaign with PLAN.md + STATE.md + wave docs, durable record
  ;; persisted with W0 'verifying (fence 1, attempt A) and W1 'pending.
  (define dir (make-temporary-file "gsd-atomic-~a" 'directory))
  (make-directory* (build-path dir ".planning" "waves"))
  (call-with-output-file
   (build-path dir ".planning" "PLAN.md")
   (lambda (out)
     (display
      "# Plan: Test Campaign\n\n## Waves\n\n- [Inbox] W0: Wave 0 → waves/W0-wave.md\n- [Inbox] W1: Wave 1 → waves/W1-wave.md"
      out))
   #:exists 'truncate)
  (call-with-output-file
   (build-path dir ".planning" "STATE.md")
   (lambda (out)
     (display
      "# State: Test Campaign\n\n| Wave | Title | Status |\n|---|---|---|\n| W0 | Wave 0 | PENDING |\n| W1 | Wave 1 | PENDING |"
      out))
   #:exists 'truncate)
  (call-with-output-file (build-path dir ".planning" "waves" "W0-wave.md")
                         (lambda (out) (display "# Wave 0\nStatus: Inbox\n\nAlpha body.\n" out))
                         #:exists 'truncate)
  (call-with-output-file (build-path dir ".planning" "waves" "W1-wave.md")
                         (lambda (out) (display "# Wave 1\nStatus: Inbox\n\nBeta body.\n" out))
                         #:exists 'truncate)
  (define rec (migrate-campaign! dir))
  (set-campaign-fence-token! rec 1)
  (begin-attempt! rec 0 1)
  (set-campaign-wave-status! (wave* rec 0) 'verifying)
  (persist-campaign! dir rec)
  (values dir rec))

(define (cleanup dir)
  (delete-directory/files dir #:must-exist? #f))

(define (complete! dir rec idx)
  (define attempt (campaign-wave-current-attempt (wave* rec idx)))
  (try-complete-wave! dir
                      rec
                      idx
                      #:verifier-approve? #t
                      #:expected-attempt-id (campaign-attempt-id attempt)
                      #:expected-fence-token (campaign-attempt-fence-token attempt)))

;; The canonical final state after a clean approval of wave 0.
(define (check-canonical-done! dir rec)
  (define durable (load-campaign-record dir (campaign-plan-id rec)))
  (check-eq? (campaign-wave-status (wave* durable 0)) 'done "durable W0 done")
  (define event-id
    (make-event-id (campaign-plan-id rec)
                   0
                   (campaign-attempt-id (campaign-wave-current-attempt (wave* rec 0)))))
  (check-equal? (load-outbox dir (campaign-plan-id rec))
                (list event-id)
                "exactly one completion event in outbox")
  (check-true (string-contains? (read-text (build-path dir ".planning" "PLAN.md"))
                                "- [DONE] W0: Wave 0")
              "PLAN.md W0 marker DONE")
  (check-true (string-contains? (read-text (build-path dir ".planning" "STATE.md"))
                                "| W0 | Wave 0 | DONE |")
              "STATE.md W0 row DONE")
  (check-true (string-prefix? (read-text (build-path dir ".planning" "waves" "W0-wave.md"))
                              "# Wave 0\nStatus: DONE")
              "W0 wave doc DONE")
  ;; No skipped wave: W1 untouched.
  (check-eq? (campaign-wave-status (wave* durable 1)) 'pending "W1 still pending")
  (check-true (string-contains? (read-text (build-path dir ".planning" "PLAN.md"))
                                "- [Inbox] W1: Wave 1")
              "PLAN.md W1 marker still Inbox")
  (check-true (string-contains? (read-text (build-path dir ".planning" "STATE.md"))
                                "| W1 | Wave 1 | PENDING |")
              "STATE.md W1 row still PENDING")
  durable)

;; Recovery entry points, mirroring run-campaign!.
(define (recover! dir rec)
  (define durable (load-campaign-record dir (campaign-plan-id rec)))
  (define outbox-added (reconcile-completion-outbox! dir durable))
  (define projection-added
    (reconcile-projections-from-waves! dir
                                       (for/list ([w (campaign-record-waves durable)])
                                         (cons (campaign-wave-index w) (campaign-wave-status w)))
                                       (plan-slug-map dir)))
  (values outbox-added projection-added))

;; ============================================================
;; Tests
;; ============================================================

(define all-tests
  (test-suite "atomic projection transaction (W2 #9233)"

    (test-case "clean approval converges to canonical state"
      (define-values (dir rec) (make-fixture))
      (dynamic-wind void
                    (lambda ()
                      (define result (complete! dir rec 0))
                      (check-eq? (completion-result-status result) 'done)
                      (check-canonical-done! dir rec))
                    (lambda () (cleanup dir))))

    (test-case "crash before any write: retry converges, no partial tracking"
      ;; Window W0 — the transaction never ran. Durable still 'verifying.
      (define-values (dir rec) (make-fixture))
      (dynamic-wind void
                    (lambda ()
                      (define durable (load-campaign-record dir (campaign-plan-id rec)))
                      (check-eq? (campaign-wave-status (wave* durable 0)) 'verifying)
                      (check-equal? (load-outbox dir (campaign-plan-id rec)) '())
                      ;; Retry completes idempotently.
                      (define result (complete! dir rec 0))
                      (check-eq? (completion-result-status result) 'done)
                      (check-canonical-done! dir rec))
                    (lambda () (cleanup dir))))

    (test-case "crash after durable commit, before outbox: reconcile rebuilds event"
      ;; Window W1 — durable W0=done committed, outbox empty, projections stale.
      (define-values (dir rec) (make-fixture))
      (dynamic-wind
       void
       (lambda ()
         ;; Construct the intermediate state: durable commit only.
         (define durable (load-campaign-record dir (campaign-plan-id rec)))
         (set-campaign-wave-status! (wave* durable 0) 'done)
         (persist-campaign! dir durable)
         (check-equal? (load-outbox dir (campaign-plan-id rec)) '() "crash left no outbox event")
         (check-true (string-contains? (read-text (build-path dir ".planning" "PLAN.md"))
                                       "- [Inbox] W0")
                     "crash left projections stale")
         ;; Recovery rebuilds outbox + projections.
         (define-values (outbox-added projection-added) (recover! dir rec))
         (check-equal? outbox-added 1 "exactly one outbox event rebuilt")
         (check-equal? (length projection-added) 3 "plan + doc + state repaired")
         (check-canonical-done! dir rec)
         ;; Idempotent second recovery: no writes.
         (define-values (outbox-added-2 projection-added-2) (recover! dir rec))
         (check-equal? outbox-added-2 0)
         (check-equal? projection-added-2 '()))
       (lambda () (cleanup dir))))

    (test-case "crash after outbox, before projections: reconciles converge, no dup"
      ;; Window W2 — durable W0=done + outbox event present, projections stale.
      (define-values (dir rec) (make-fixture))
      (dynamic-wind
       void
       (lambda ()
         (define durable (load-campaign-record dir (campaign-plan-id rec)))
         (set-campaign-wave-status! (wave* durable 0) 'done)
         (persist-campaign! dir durable)
         ;; outbox already carries the event (appended before the crash)
         (define event-id
           (make-event-id (campaign-plan-id rec)
                          0
                          (campaign-attempt-id (campaign-wave-current-attempt (wave* durable 0)))))
         (define outbox-path
           (build-path dir
                       ".planning"
                       "campaigns"
                       (string-append (campaign-plan-id rec) ".outbox.rktd")))
         (make-directory* (build-path dir ".planning" "campaigns"))
         (call-with-output-file outbox-path
                                (lambda (out) (write (list event-id) out))
                                #:exists 'truncate)
         (define-values (outbox-added projection-added) (recover! dir rec))
         (check-equal? outbox-added 0 "event already present — no duplicate")
         (check-equal? (length projection-added) 3)
         (check-canonical-done! dir rec)
         (check-equal? (count-completion-events dir rec) 1 "exactly one event after recovery"))
       (lambda () (cleanup dir))))

    (test-case "crash mid projection apply (partial files): reconcile converges"
      ;; Window W3 — durable done + outbox present, PLAN.md already new but
      ;; STATE.md + wave doc stale (crash between renames).
      (define-values (dir rec) (make-fixture))
      (dynamic-wind
       void
       (lambda ()
         (define durable (load-campaign-record dir (campaign-plan-id rec)))
         (set-campaign-wave-status! (wave* durable 0) 'done)
         (persist-campaign! dir durable)
         ;; partially applied: only PLAN.md marker flipped
         (define plan-path (build-path dir ".planning" "PLAN.md"))
         (define plan-text* (read-text plan-path))
         (call-with-output-file
          plan-path
          (lambda (out)
            (display (string-replace plan-text* "- [Inbox] W0: Wave 0" "- [DONE] W0: Wave 0") out))
          #:exists 'truncate)
         (define-values (outbox-added projection-added) (recover! dir rec))
         (check-equal? outbox-added 1 "durable done with no outbox event — rebuilt")
         (check-equal? (length projection-added) 2 "state + doc repaired; plan already matches")
         (check-canonical-done! dir rec))
       (lambda () (cleanup dir))))

    (test-case "phantom prevention: reconcile never invents events for non-done waves"
      ;; A durable record with W0 'verifying / 'failed / 'pending must NOT get a
      ;; completion event — the outbox may only ever lag, never lead.
      (define-values (dir rec) (make-fixture))
      (dynamic-wind void
                    (lambda ()
                      (for ([status '(verifying failed pending)])
                        (define durable (load-campaign-record dir (campaign-plan-id rec)))
                        (set-campaign-wave-status! (wave* durable 0) status)
                        (persist-campaign! dir durable)
                        (define added (reconcile-completion-outbox! dir durable))
                        (check-equal? added 0 (format "no event invented for ~a" status))
                        (check-equal? (load-outbox dir (campaign-plan-id rec))
                                      '()
                                      (format "outbox empty for ~a" status))))
                    (lambda () (cleanup dir))))

    (test-case "recovery after crash then retry: second completion returns already-done"
      ;; Complete once (clean), then crash-window state with no outbox event,
      ;; recover, then a re-driven completion must not duplicate the event.
      (define-values (dir rec) (make-fixture))
      (dynamic-wind
       void
       (lambda ()
         (define r1 (complete! dir rec 0))
         (check-eq? (completion-result-status r1) 'done)
         ;; simulate the crash window: remove the outbox event only
         (define outbox-path
           (build-path dir
                       ".planning"
                       "campaigns"
                       (string-append (campaign-plan-id rec) ".outbox.rktd")))
         (call-with-output-file outbox-path (lambda (out) (write '() out)) #:exists 'truncate)
         (recover! dir rec)
         (check-equal? (count-completion-events dir rec) 1)
         (define r2 (complete! dir rec 0))
         (check-eq? (completion-result-status r2) 'already-done)
         (check-equal? (count-completion-events dir rec) 1 "no duplicate on re-completion"))
       (lambda () (cleanup dir))))))

(void (run-tests all-tests))
