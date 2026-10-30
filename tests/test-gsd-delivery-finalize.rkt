#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers extensions/gsd/delivery-finalize.rkt
;; D1 regression (v1.00.33 audit, §16): record-attempt-delivery-provenance!
;; is documented to return "the recorded (branch . head) or #f when no real
;; provenance exists". The verification-context side effect must not change
;; that contract — these tests pin the success and refusal return values.
;; record-wave-delivery! is best-effort persistence (warns without a campaign
;; record); the contract under test is the return value, not the side effect.
(require rackunit
         racket/file
         (only-in racket/string string-contains?)
         "../extensions/gsd/delivery-finalize.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/delivery-handoff.rkt"
         "../extensions/gsd/wave-completion.rkt"
         (only-in "../extensions/gsd/campaign-result.rkt"
                  campaign-result
                  campaign-result-status
                  campaign-result-message
                  campaign-result-diagnostics
                  campaign-diagnostics
                  campaign-result-with-diagnostics)
         (only-in "../extensions/gsd/campaign-state.rkt"
                  campaign-plan-id
                  migrate-campaign!
                  begin-attempt!
                  campaign-attempt-id
                  campaign-attempt-fence-token
                  campaign-record-waves
                  campaign-wave-current-attempt
                  campaign-wave-index
                  campaign-wave-status
                  make-campaign-cancellation
                  set-campaign-cancellation!
                  set-campaign-fence-token!
                  set-campaign-wave-delivery-branch!
                  set-campaign-wave-delivery-head-sha!
                  set-campaign-wave-status!)
         (only-in "../extensions/gsd/campaign-repository.rkt"
                  load-campaign-record
                  persist-campaign!)
         (only-in "../extensions/gsd/campaign-decision.rkt" campaign-decision)
         (only-in "../extensions/gsd/planning-paths.rkt" campaign-record-file))
(define plan (make-string 64 #\a))
(define head (make-string 40 #\b))
(define receipt
  (hasheq 'repo
          "/repo/q"
          'branch
          "campaign/w2"
          'head
          head
          'tree
          (make-string 40 #\c)
          'origin
          "https://github.com/example/q.git"
          'verified-at
          1
          'evidence
          "full Verify passed"
          'attempt-id
          "attempt-9"
          'attempt-fence
          3))
(define (with-root f)
  (define root (make-temporary-file "delivery-finalize-~a" 'directory))
  (dynamic-wind void (lambda () (f root)) (lambda () (delete-directory/files root))))
(module+ test
  (test-case "receipt-path success returns (branch . head) — not void (D1 regression)"
    (with-root (lambda (root)
                 (record-delivery-receipt! root plan 2 receipt)
                 (define result (record-attempt-delivery-provenance! root plan 2 "attempt-9" 3))
                 (check-true (pair? result) "documented contract: (branch . head) pair")
                 (check-equal? result (cons "campaign/w2" head)))))
  (test-case "refusal: receipt identity mismatch (attempt-id, fence) returns #f"
    (with-root (lambda (root)
                 (record-delivery-receipt! root plan 2 receipt)
                 (check-false (record-attempt-delivery-provenance! root plan 2 "attempt-8" 3))
                 (check-false (record-attempt-delivery-provenance! root plan 2 "attempt-9" 4)))))
  (test-case "refusal: no journal / no provenance at all returns #f"
    (with-root (lambda (root)
                 (check-false (record-attempt-delivery-provenance! root plan 2 "attempt-9" 3)))))
  (define vc
    (hasheq 'base
            (make-string 40 #\d)
            'merge-sha
            (make-string 40 #\e)
            'pr-head
            (make-string 40 #\f)
            'branch
            "binding/plan-w2"
            'repo-root
            "/repo/q"
            'verified-at
            1
            'snapshot-refs
            (list "origin/main" "origin/pr/9768")))
  (test-case "success with verification context: exact (branch . head) return, context persisted"
    (with-root
     (lambda (root)
       (record-delivery-receipt! root plan 2 receipt)
       (define result
         (record-attempt-delivery-provenance! root plan 2 "attempt-9" 3 #:verification-context vc))
       (check-equal? result (cons "campaign/w2" head))
       (check-equal? (hash-ref (load-delivery-journal root plan 2) 'verification-context #f) vc))))
  (test-case "refusal with verification context: #f return, context still persisted"
    (with-root
     (lambda (root)
       (record-delivery-receipt! root plan 2 receipt)
       (check-false
        (record-attempt-delivery-provenance! root plan 2 "attempt-8" 3 #:verification-context vc))
       (check-equal? (hash-ref (load-delivery-journal root plan 2) 'verification-context #f) vc))))
  (test-case "context persistence without provenance raises separately (no return value)"
    (with-root (lambda (root)
                 (check-exn (lambda (e)
                              (and (exn:fail? e)
                                   (regexp-match? #rx"missing verified provenance" (exn-message e))))
                            (lambda ()
                              (record-attempt-delivery-provenance! root
                                                                   plan
                                                                   2
                                                                   "attempt-9"
                                                                   3
                                                                   #:verification-context vc)))))))

;; ── W1: isolated effect-record-loss regression ────────────────────────
;; The post-lane-effect campaign read is fresh and possibly ABSENT. It is
;; classified honestly as a typed blocked stop ("campaign record disappeared"
;; — the orchestrator's existing typed-stop text) with no next launch and no
;; reuse of the caller's pre-effect record; it must never crash inside a raw
;; accessor on #f.
(module+ test
  (test-case "mid-effect campaign record loss yields a typed blocked stop, not a contract crash"
    (with-root
     (lambda (root)
       (make-directory* (build-path root ".planning" "waves"))
       (call-with-output-file
        (build-path root ".planning" "PLAN.md")
        (lambda (out)
          (displayln "# Plan: Record loss\n\n- [Inbox] W0: Evidence → waves/W0-e.md"))
        #:exists 'truncate)
       (call-with-output-file
        (build-path root ".planning" "waves" "W0-e.md")
        (lambda (out)
          (displayln "# Wave 0\nStatus: Inbox\n\n## Verify\n\nraco test .\n\n## Done\n\nmeasured"))
        #:exists 'truncate)
       (define rec (migrate-campaign! root))
       (define plan-id (campaign-plan-id rec))
       (persist-campaign! root rec)
       (check-true (file-exists? (campaign-record-file root plan-id)))
       (define effect-ran? (box #f))
       (define outcome
         (run-campaign-resume!
          root
          plan-id
          (campaign-decision 'resume 0 'coordinator-step "coordinator step" #f #f)
          "coordinator step"
          (lambda (dir p idx)
            (delete-file (campaign-record-file dir p))
            (set-box! effect-ran? #t)
            (cons 'ok "coordinator moved"))
          #:author-session-live? (lambda () #f)))
       (check-true (unbox effect-ran?) "the lane effect itself ran")
       (check-eq? (campaign-outcome-kind outcome) 'blocked "typed stop, never an exception")
       (check-equal? (campaign-outcome-reason outcome) "campaign record disappeared")))))

;; ── W4: the finalization's OWN non-DONE stops carry durable diagnostics ──
;; Each stop is enriched AT ITS CONSTRUCTION POINT with the actual completion
;; status / durable gate reason, a typed code, an explicit next action and
;; artifact references. The legacy status and message stay byte-identical;
;; the envelope rides the additive campaign-result subtype. The DONE branch
;; passes the continuation's result through UNWRAPPED: on-delivered may run
;; later campaign iterations, and their result identity is never re-tagged
;; with this finalization's decision.
(module+ test
  (define (write-w4-fixture! root)
    (make-directory* (build-path root ".planning" "waves"))
    (call-with-output-file
     (build-path root ".planning" "PLAN.md")
     (lambda (out) (displayln "# Plan: W4 stops\n\n- [Inbox] W0: Evidence → waves/W0-e.md" out))
     #:exists 'truncate)
    (call-with-output-file
     (build-path root ".planning" "waves" "W0-e.md")
     (lambda (out)
       (displayln "# Wave 0\nStatus: Inbox\n\n## Verify\n\nraco test .\n\n## Done\n\nmeasured" out))
     #:exists 'truncate))

  ;; One-wave campaign parked exactly where BUG-0077 parks it: durable
  ;; 'awaiting-delivery with a live current attempt/fence.
  (define (make-w4-parked-fixture)
    (define root (make-temporary-file "finalize-w4-~a" 'directory))
    (write-w4-fixture! root)
    (define rec (migrate-campaign! root))
    (set-campaign-fence-token! rec 1)
    (begin-attempt! rec 0 1)
    (set-campaign-wave-status! (car (campaign-record-waves rec)) 'verifying)
    (persist-campaign! root rec)
    (define attempt (campaign-wave-current-attempt (car (campaign-record-waves rec))))
    (try-complete-wave! root
                        rec
                        0
                        #:verifier-approve? #t
                        #:expected-attempt-id (campaign-attempt-id attempt)
                        #:expected-fence-token (campaign-attempt-fence-token attempt))
    (values root
            (campaign-plan-id rec)
            (load-campaign-record root (campaign-plan-id rec))
            attempt))

  (test-case "failed release gate: durable reason rides the stop, legacy text kept"
    (define-values (root plan-id rec attempt) (make-w4-parked-fixture))
    (define result
      (run-delivery-finalization root
                                 plan-id
                                 rec
                                 0
                                 (make-string 40 #\e)
                                 '()
                                 #:release-check (lambda () "Release object missing")))
    (check-eq? (campaign-result-status result) 'wave-failed)
    (check-equal? (campaign-result-message result) "release verification failed")
    (define d (campaign-result-diagnostics result))
    (check-equal? (hash-ref d 'code) 'delivery-finalization-failed)
    (check-true (string-contains? (hash-ref d 'detail)
                                  "release not verified: Release object missing")
                (format "detail: ~a" (hash-ref d 'detail)))
    (check-true (string-contains? (hash-ref d 'next-action) "fresh verified attempt"))
    (check-true (pair? (hash-ref d 'artifacts)))
    (check-eq? (campaign-wave-status (car (campaign-record-waves (load-campaign-record root plan-id))))
               'failed)
    (delete-directory/files root #:must-exist? #f))

  (test-case "stale completion: actual durable status named, legacy text kept"
    (define-values (root plan-id rec attempt) (make-w4-parked-fixture))
    (set-campaign-cancellation! rec (make-campaign-cancellation "stop" 1))
    (persist-campaign! root rec)
    (define result
      (run-delivery-finalization root plan-id rec 0 (make-string 40 #\e) '()))
    (check-eq? (campaign-result-status result) 'wave-cancelled)
    (check-equal? (campaign-result-message result) "stale delivery completion ignored")
    (define d (campaign-result-diagnostics result))
    (check-equal? (hash-ref d 'code) 'delivery-finalization-stale)
    (check-true (string-contains? (hash-ref d 'detail) "cancelled")
                (format "detail: ~a" (hash-ref d 'detail)))
    (delete-directory/files root #:must-exist? #f))

  (test-case "paused finalization: durable journal/handoff facts and explicit next action"
    (define-values (root plan-id rec attempt) (make-w4-parked-fixture))
    (define result
      (run-delivery-finalization root plan-id rec 0 (make-string 40 #\f) '()))
    (check-eq? (campaign-result-status result) 'wave-blocked)
    (check-equal? (campaign-result-message result)
                  "delivery provenance changed during finalization")
    (define d (campaign-result-diagnostics result))
    (check-equal? (hash-ref d 'code) 'delivery-finalization-pending)
    (check-true (string-contains? (hash-ref d 'detail) "finalization authorization refused")
                (format "detail: ~a" (hash-ref d 'detail)))
    (check-true (string-contains? (hash-ref d 'detail) "durable delivery journal: absent")
                (format "detail: ~a" (hash-ref d 'detail)))
    (check-true (string-contains? (hash-ref d 'detail) "delivery handoff: delivery-pending")
                (format "detail: ~a" (hash-ref d 'detail)))
    (check-true (string-contains? (hash-ref d 'next-action) "do not rerun implementation"))
    (check-true (string-contains? (hash-ref d 'next-action) "gsd-delivery.py"))
    (check-true (pair? (hash-ref d 'artifacts)) "the durable handoff record is referenced")
    (check-eq? (campaign-wave-status (car (campaign-record-waves (load-campaign-record root plan-id))))
               'awaiting-delivery
               "a paused finalization never moves the wave off its resumable state")
    (delete-directory/files root #:must-exist? #f))

  (test-case "done: the continuation's result identity is returned unwrapped"
    (define-values (root plan-id rec attempt) (make-w4-parked-fixture))
    (define head (make-string 40 #\a))
    (define merge-sha (make-string 40 #\c))
    (define wave (car (campaign-record-waves rec)))
    (set-campaign-wave-delivery-branch! wave "campaign/test")
    (set-campaign-wave-delivery-head-sha! wave head)
    (persist-campaign! root rec)
    (record-delivery-receipt!
     root
     plan-id
     0
     (hasheq 'repo
             "/repo"
             'branch
             "campaign/test"
             'head
             head
             'tree
             (make-string 40 #\b)
             'origin
             "https://github.com/example/q.git"
             'verified-at
             0
             'evidence
             "Verify passed"
             'attempt-id
             (campaign-attempt-id attempt)
             'attempt-fence
             (campaign-attempt-fence-token attempt)))
    (persist-delivery-handoff! root plan-id wave "test precondition")
    (reconcile-delivered-handoff! root plan-id 0 merge-sha)
    (update-delivery-journal! root plan-id 0 (hasheq 'stage "delivered"))
    (define later-result
      (campaign-result-with-diagnostics
       (campaign-result 'campaign-complete '() "a later iteration's result")
       (campaign-diagnostics 'later-iteration #:detail "never re-tagged")))
    (define result
      (run-delivery-finalization root
                                 plan-id
                                 (load-campaign-record root plan-id)
                                 0
                                 merge-sha
                                 '()
                                 #:on-delivered (lambda () later-result)))
    (check-true (eq? result later-result)
                "the later continuation result is passed through unwrapped")
    (check-eq? (campaign-wave-status (car (campaign-record-waves (load-campaign-record root plan-id))))
               'done)
    (delete-directory/files root #:must-exist? #f)))
