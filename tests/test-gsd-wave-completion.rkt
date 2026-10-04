#lang racket/base
;; @covers extensions/gsd/wave-completion.rkt

;; @speed fast  ;; @suite extensions
;; @boundary integration

;; tests/test-gsd-wave-completion.rkt — W1: Verifier-First Completion and Lifecycle Truth
;;
;; TDD red tests for:
;;   1. Verifier rejection cannot persist DONE.
;;   2. Verifier approval persists DONE + outbox event.
;;   3. /skip commits DEFERRED durably.
;;   4. Duplicate completion events are deduplicated by stable event ID.
;;   5. Doc existence never implies completion (GC-5 regression).

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         (only-in "../extensions/gsd/campaign-state.rkt"
                  make-campaign-manifest
                  make-campaign-wave-descriptor
                  make-campaign-wave
                  make-campaign-record
                  campaign-manifest-hash
                  campaign-plan-id
                  campaign-record-waves
                  campaign-wave-index
                  campaign-wave-status
                  campaign-wave-attempt-count
                  campaign-wave-current-attempt
                  campaign-attempt-id
                  campaign-attempt-fence-token
                  set-campaign-wave-status!
                  set-campaign-wave-delivery-branch!
                  set-campaign-wave-delivery-head-sha!
                  set-campaign-fence-token!
                  set-campaign-cancellation!
                  make-campaign-cancellation
                  begin-attempt!
                  select-next-actionable-wave
                  wave-failure-reason
                  attempt-failure-reason
                  stamp-wave-failure!
                  migrate-campaign!)
         (only-in "../extensions/gsd/campaign-repository.rkt" persist-campaign! load-campaign-record)
         (only-in "../extensions/gsd/delivery-handoff.rkt"
                  persist-delivery-handoff!
                  delivery-pending-wave
                  delivery-handoff-path
                  reconcile-delivered-handoff!)
         (only-in "../extensions/gsd/delivery-journal.rkt"
                  record-delivery-receipt!
                  update-delivery-journal!)
         (only-in "../extensions/gsd/wave-completion.rkt"
                  try-complete-wave!
                  skip-wave!
                  completion-result-status
                  completion-result-event-id
                  count-completion-events
                  make-event-id
                  load-outbox))

;; W3 exports under test (red-first: dynamic until the module provides them).
(define-runtime-path handoff-module "../extensions/gsd/delivery-handoff.rkt")
(define-runtime-path wave-completion-module "../extensions/gsd/wave-completion.rkt")
(define delivery-handoff-status (dynamic-require handoff-module 'delivery-handoff-status))
(define reconcile-completion-outbox!
  (dynamic-require wave-completion-module 'reconcile-completion-outbox!))
(define completion-outbox-invariant?
  (dynamic-require wave-completion-module 'completion-outbox-invariant?))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-tmp-campaign-dir n-waves)
  (define dir (make-temporary-file "wave-comp-~a" 'directory))
  (make-directory* (build-path dir ".planning" "waves"))
  (call-with-output-file (build-path dir ".planning" "PLAN.md")
                         (lambda (out)
                           (display "# Plan: Test Completion\n\n## Waves\n\n" out)
                           (for ([i (in-range n-waves)])
                             (fprintf out "- [Inbox] W~a: Wave ~a → waves/W~a-wave.md\n" i i i)))
                         #:exists 'truncate)
  ;; BUG-0052: every referenced wave doc must exist for campaign creation.
  (for ([i (in-range n-waves)])
    (call-with-output-file
     (build-path dir ".planning" "waves" (format "W~a-wave.md" i))
     (lambda (out) (fprintf out "# Wave ~a\n\nGoal: wave ~a\n\n## Verify\n\nraco test .\n" i i))
     #:exists 'truncate))
  dir)

(define (load-or-migrate dir)
  (migrate-campaign! dir))

(define (cleanup-tmp dir)
  (delete-directory/files dir #:must-exist? #f))

(define (wave* rec idx)
  (for/first ([w (campaign-record-waves rec)]
              #:when (= (campaign-wave-index w) idx))
    w))

(define (wave-status* rec idx)
  (campaign-wave-status (wave* rec idx)))

(define (complete-current! dir rec idx approve?)
  (define attempt (campaign-wave-current-attempt (wave* rec idx)))
  (try-complete-wave! dir
                      rec
                      idx
                      #:verifier-approve? approve?
                      #:expected-attempt-id (campaign-attempt-id attempt)
                      #:expected-fence-token (campaign-attempt-fence-token attempt)))

(define (seed-delivered-proof! dir rec idx #:merge-sha [merge-sha (make-string 40 #\a)])
  (define durable (load-campaign-record dir (campaign-plan-id rec)))
  (define pid (campaign-plan-id durable))
  (define w (wave* durable idx))
  (define attempt (campaign-wave-current-attempt w))
  (define branch (format "campaign/w~a" idx))
  (define head (make-string 40 #\b))
  (define tree (make-string 40 #\c))
  (set-campaign-wave-delivery-branch! w branch)
  (set-campaign-wave-delivery-head-sha! w head)
  (persist-campaign! dir durable)
  (define caller-wave (wave* rec idx))
  (when caller-wave
    (set-campaign-wave-delivery-branch! caller-wave branch)
    (set-campaign-wave-delivery-head-sha! caller-wave head))
  (record-delivery-receipt! dir
                            pid
                            idx
                            (hasheq 'repo
                                    "/repo"
                                    'branch
                                    branch
                                    'head
                                    head
                                    'tree
                                    tree
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
  (persist-delivery-handoff! dir pid w "fixture: current verified identity")
  (update-delivery-journal! dir pid idx (hasheq 'stage "delivered"))
  (reconcile-delivered-handoff! dir pid idx merge-sha)
  merge-sha)

(define (finalize-current! dir rec idx merge-sha)
  (define attempt (campaign-wave-current-attempt (wave* rec idx)))
  (try-complete-wave! dir
                      rec
                      idx
                      #:verifier-approve? #t
                      #:expected-attempt-id (campaign-attempt-id attempt)
                      #:expected-fence-token (campaign-attempt-fence-token attempt)
                      #:delivered-merge-sha merge-sha))

;; Seed a canonical "Status: Inbox" header under the H1 so the lockstep
;; assertions observe real projection writes instead of a missing line.
(define (seed-status! dir idx)
  (define p (build-path dir ".planning" "waves" (format "W~a-wave.md" idx)))
  (define lines (string-split (call-with-input-file p port->string) "\n"))
  (call-with-output-file
   p
   (lambda (out)
     (display (string-join (append (list (car lines) "Status: Inbox") (cdr lines)) "\n") out))
   #:exists 'truncate))

;; ============================================================
;; Test suites
;; ============================================================

(define verifier-first-suite
  (test-suite "verifier-first completion"

    (test-case "verifier rejection cannot persist DONE"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define result (complete-current! dir rec 0 #f))
      (check-eq? (wave-status* rec 0) 'failed "rejected verifier marks wave 'failed, not 'done")
      (check-eq? (completion-result-status result) 'failed)
      ;; Verify it was persisted
      (define loaded (load-campaign-record dir (campaign-plan-id rec)))
      (check-eq? (wave-status* loaded 0) 'failed)
      (cleanup-tmp dir))

    (test-case "verifier approval parks awaiting-delivery without DONE"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define result (complete-current! dir rec 0 #t))
      (check-eq? (wave-status* rec 0) 'awaiting-delivery)
      (check-eq? (completion-result-status result) 'awaiting-delivery)
      (check-false (completion-result-event-id result) "no completion event before delivery")
      (cleanup-tmp dir))

    (test-case "approval cannot complete a wave that is not VERIFYING"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (persist-campaign! dir rec)
      (define result
        (try-complete-wave! dir
                            rec
                            0
                            #:verifier-approve? #t
                            #:expected-attempt-id "missing"
                            #:expected-fence-token 0))
      (check-eq? (completion-result-status result) 'invalid-state)
      (check-eq? (wave-status* (load-campaign-record dir (campaign-plan-id rec)) 0) 'pending)
      (cleanup-tmp dir))

    (test-case "stale attempt cannot overwrite newer durable VERIFYING state"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define stale (load-campaign-record dir (campaign-plan-id rec)))
      (begin
        (set-campaign-fence-token! rec 2)
        (begin-attempt! rec 0 2))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define result (complete-current! dir stale 0 #t))
      (check-eq? (completion-result-status result) 'stale-attempt)
      (define durable (load-campaign-record dir (campaign-plan-id rec)))
      (check-eq? (wave-status* durable 0) 'verifying)
      (check-equal? (campaign-attempt-fence-token (campaign-wave-current-attempt (wave* durable 0)))
                    2)
      (cleanup-tmp dir))

    (test-case "second completion of already-DONE wave returns 'already-done"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (complete-current! dir rec 0 #t)
      (define merge-sha (seed-delivered-proof! dir rec 0))
      (finalize-current! dir rec 0 merge-sha)
      (define r2 (complete-current! dir rec 0 #t))
      (check-eq? (completion-result-status r2) 'already-done)
      (cleanup-tmp dir))))

(define skip-suite
  (test-suite "/skip lifecycle"

    (test-case "/skip commits DEFERRED durably"
      (define dir (make-tmp-campaign-dir 3))
      (define rec (load-or-migrate dir))
      (define result (skip-wave! dir rec 0))
      (check-eq? (completion-result-status result) 'deferred)
      (check-eq? (wave-status* rec 0) 'deferred)
      (check-equal? (select-next-actionable-wave rec) 1 "deferred wave is not selected; next wave is")
      ;; Persisted
      (define loaded (load-campaign-record dir (campaign-plan-id rec)))
      (check-eq? (wave-status* loaded 0) 'deferred)
      (cleanup-tmp dir))

    (test-case "/skip on already-done returns 'already-done"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (complete-current! dir rec 0 #t)
      (define merge-sha (seed-delivered-proof! dir rec 0))
      (finalize-current! dir rec 0 merge-sha)
      (define result (skip-wave! dir rec 0))
      (check-eq? (completion-result-status result) 'already-done)
      (cleanup-tmp dir))))

(define outbox-suite
  (test-suite "durable outbox deduplication"

    (test-case "exactly one completion event per wave/attempt"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (complete-current! dir rec 0 #t)
      (define merge-sha (seed-delivered-proof! dir rec 0))
      (define r1 (finalize-current! dir rec 0 merge-sha))
      (check-not-false (completion-result-event-id r1))
      (check-equal? (count-completion-events dir rec) 1 "exactly one event in outbox"))))

(define no-heuristic-suite
  (test-suite "no heuristic implies completion (GC-5)"

    (test-case "wave doc existence never implies DONE"
      (define dir (make-tmp-campaign-dir 2))
      (make-directory* (build-path dir ".planning" "waves"))
      (call-with-output-file (build-path dir ".planning" "waves" "W0-wave.md")
                             (lambda (out) (display "## Done!\nFully implemented.\n" out))
                             #:exists 'truncate)
      (define rec (load-or-migrate dir))
      (check-false (eq? (wave-status* rec 0) 'done) "doc existence does not infer completion"))))

;; ============================================================
;; W3 (verification-truth): durable failure reason on completion
;; ============================================================

(define failure-reason-suite
  (test-suite "durable failure reason on completion"
    (test-case "verifier rejection persists the verifier message durably"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define attempt (campaign-wave-current-attempt (wave* rec 0)))
      (define result
        (try-complete-wave! dir
                            rec
                            0
                            #:verifier-approve? #f
                            #:verifier-message "no wave target files changed: src/foo.rkt"
                            #:expected-attempt-id (campaign-attempt-id attempt)
                            #:expected-fence-token (campaign-attempt-fence-token attempt)))
      (check-eq? (completion-result-status result) 'failed)
      (define loaded (load-campaign-record dir (campaign-plan-id rec)))
      (check-equal? (wave-failure-reason (wave* loaded 0))
                    "no wave target files changed: src/foo.rkt")
      (check-equal? (attempt-failure-reason (campaign-wave-current-attempt (wave* loaded 0)))
                    "no wave target files changed: src/foo.rkt")
      (cleanup-tmp dir))

    (test-case "blank verifier message gets an honest named fallback reason"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define attempt (campaign-wave-current-attempt (wave* rec 0)))
      (define result
        (try-complete-wave! dir
                            rec
                            0
                            #:verifier-approve? #f
                            #:verifier-message ""
                            #:expected-attempt-id (campaign-attempt-id attempt)
                            #:expected-fence-token (campaign-attempt-fence-token attempt)))
      (check-eq? (completion-result-status result) 'failed)
      (define loaded (load-campaign-record dir (campaign-plan-id rec)))
      (define reason (wave-failure-reason (wave* loaded 0)))
      (check-true (and (string? reason)
                       (positive? (string-length reason))
                       (string-contains? reason "verifier rejected"))
                  (format "blank verdicts never persist as blank: ~s" reason))
      (cleanup-tmp dir))

    (test-case "release-gate failure persists the release reason durably"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define attempt (campaign-wave-current-attempt (wave* rec 0)))
      (define result
        (try-complete-wave! dir
                            rec
                            0
                            #:verifier-approve? #t
                            #:expected-attempt-id (campaign-attempt-id attempt)
                            #:expected-fence-token (campaign-attempt-fence-token attempt)
                            #:release-check (lambda () "no GitHub Release for v1.2.3")))
      (check-eq? (completion-result-status result) 'failed)
      (define loaded (load-campaign-record dir (campaign-plan-id rec)))
      (check-true (string-contains? (wave-failure-reason (wave* loaded 0)) "release not verified"))
      (cleanup-tmp dir))

    (test-case "approval clears a previously stamped failure reason"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (stamp-wave-failure! (wave* rec 0) "stale prior failure")
      (persist-campaign! dir rec)
      (define attempt (campaign-wave-current-attempt (wave* rec 0)))
      (define result
        (try-complete-wave! dir
                            rec
                            0
                            #:verifier-approve? #t
                            #:expected-attempt-id (campaign-attempt-id attempt)
                            #:expected-fence-token (campaign-attempt-fence-token attempt)))
      (check-eq? (completion-result-status result) 'awaiting-delivery)
      (define loaded (load-campaign-record dir (campaign-plan-id rec)))
      (check-equal? (wave-failure-reason (wave* loaded 0))
                    ""
                    "an approved awaiting-delivery wave carries no failure reason")
      (check-false (attempt-failure-reason (campaign-wave-current-attempt (wave* loaded 0))))
      (cleanup-tmp dir))))

;; ============================================================
;; Delivery-gated completion (this campaign's W3, register F9 + F10)
;; ============================================================

(define delivery-gate-suite
  (test-suite "delivery-gated completion"

    (test-case "BUG-0077 diagnostic: rejected verification remains a failure without delivery"
      ;; Discriminates the proposed 'delivery gate preempts rejection' cause.
      ;; A rejecting verifier must fail regardless of the proof mode.
      (for ([mode (in-list '(carry-forward require-delivered))])
        (define dir (make-tmp-campaign-dir 1))
        (define rec (load-or-migrate dir))
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1)
        (set-campaign-wave-status! (wave* rec 0) 'verifying)
        (persist-campaign! dir rec)
        (define attempt (campaign-wave-current-attempt (wave* rec 0)))
        (define result
          (try-complete-wave! dir
                              rec
                              0
                              #:verifier-approve? #f
                              #:verifier-message "verifier rejected"
                              #:expected-attempt-id (campaign-attempt-id attempt)
                              #:expected-fence-token (campaign-attempt-fence-token attempt)
                              #:delivery-proof mode))
        (check-eq? (completion-result-status result) 'failed (symbol->string mode))
        (check-eq? (wave-status* (load-campaign-record dir (campaign-plan-id rec)) 0) 'failed)
        (cleanup-tmp dir)))

    (test-case "BUG-0077: approval parks a durable awaiting-delivery attempt without DONE or outbox"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (set-campaign-fence-token! rec 1)
      (begin-attempt! rec 0 1)
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define attempt (campaign-wave-current-attempt (wave* rec 0)))
      (define result
        (try-complete-wave! dir
                            rec
                            0
                            #:verifier-approve? #t
                            #:expected-attempt-id (campaign-attempt-id attempt)
                            #:expected-fence-token (campaign-attempt-fence-token attempt)))
      (check-eq? (completion-result-status result) 'awaiting-delivery)
      (define durable (load-campaign-record dir (campaign-plan-id rec)))
      (check-eq? (wave-status* durable 0) 'awaiting-delivery)
      (check-equal? (count-completion-events dir durable) 0)
      (check-eq? (delivery-handoff-status dir (campaign-plan-id rec) 0) 'delivery-pending)
      (cleanup-tmp dir))

    (test-case "F9: approval is strict and parks awaiting-delivery before finalization"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (seed-status! dir 0)
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define attempt (campaign-wave-current-attempt (wave* rec 0)))
      (define result
        (try-complete-wave! dir
                            rec
                            0
                            #:verifier-approve? #t
                            #:expected-attempt-id (campaign-attempt-id attempt)
                            #:expected-fence-token (campaign-attempt-fence-token attempt)))
      (check-eq? (completion-result-status result) 'awaiting-delivery)
      (define durable (load-campaign-record dir (campaign-plan-id rec)))
      (check-eq? (wave-status* durable 0) 'awaiting-delivery "no premature DONE is persisted")
      (check-equal? (count-completion-events dir durable) 0 "no leading completion event")
      (check-true (string-contains? (call-with-input-file (build-path dir ".planning" "PLAN.md")
                                                          port->string)
                                    "[Inbox] W0")
                  "plan index bracket stays Inbox before finalization")
      (check-true
       (string-contains? (call-with-input-file (build-path dir ".planning" "waves" "W0-wave.md")
                                               port->string)
                         "Status: Inbox")
       "wave doc header stays Inbox before finalization")
      (check-eq? (delivery-handoff-status dir (campaign-plan-id rec) 0) 'delivery-pending)
      (cleanup-tmp dir))

    (test-case "F9: an authoritative delivered journal finalizes awaiting-delivery"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define pid (campaign-plan-id rec))
      (define wave (wave* rec 0))
      (define attempt (campaign-wave-current-attempt wave))
      (define parked
        (try-complete-wave! dir
                            rec
                            0
                            #:verifier-approve? #t
                            #:expected-attempt-id (campaign-attempt-id attempt)
                            #:expected-fence-token (campaign-attempt-fence-token attempt)))
      (check-eq? (completion-result-status parked) 'awaiting-delivery)
      (define merge-sha (seed-delivered-proof! dir rec 0))
      (check-eq? (delivery-handoff-status dir pid 0) 'delivered)
      (define result (finalize-current! dir rec 0 merge-sha))
      (check-eq? (completion-result-status result) 'done)
      (define durable (load-campaign-record dir pid))
      (check-eq? (wave-status* durable 0) 'done)
      ;; Lockstep: plan index bracket, wave doc header and journal agree.
      (check-true (string-contains? (call-with-input-file (build-path dir ".planning" "PLAN.md")
                                                          port->string)
                                    "[DONE] W0"))
      (check-true (string-contains?
                   (call-with-input-file (build-path dir ".planning" "waves" "W0-wave.md")
                                         port->string)
                   "Status: DONE"))
      (check-eq? (delivery-handoff-status dir pid 0) 'delivered)
      (check-eq? (completion-outbox-invariant? dir durable) 'ok)
      (cleanup-tmp dir))

    (test-case "BUG-0077: pending receipt cannot finalize DONE"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (set-campaign-fence-token! rec 1)
      (begin-attempt! rec 0 1)
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define attempt (campaign-wave-current-attempt (wave* rec 0)))
      (try-complete-wave! dir
                          rec
                          0
                          #:verifier-approve? #t
                          #:expected-attempt-id (campaign-attempt-id attempt)
                          #:expected-fence-token (campaign-attempt-fence-token attempt))
      (define merge-sha (seed-delivered-proof! dir rec 0))
      (update-delivery-journal! dir (campaign-plan-id rec) 0 (hasheq 'stage "context-ready"))
      (define result (finalize-current! dir rec 0 merge-sha))
      (check-eq? (completion-result-status result) 'delivery-pending-cannot-complete)
      (define durable (load-campaign-record dir (campaign-plan-id rec)))
      (check-eq? (wave-status* durable 0) 'awaiting-delivery)
      (check-equal? (count-completion-events dir durable) 0)
      (cleanup-tmp dir))

    (test-case "BUG-0077: finalization requires exact delivered merge SHA"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (set-campaign-fence-token! rec 1)
      (begin-attempt! rec 0 1)
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define attempt (campaign-wave-current-attempt (wave* rec 0)))
      (try-complete-wave! dir
                          rec
                          0
                          #:verifier-approve? #t
                          #:expected-attempt-id (campaign-attempt-id attempt)
                          #:expected-fence-token (campaign-attempt-fence-token attempt))
      (define merge-sha (seed-delivered-proof! dir rec 0 #:merge-sha (make-string 40 #\a)))
      (define refused (finalize-current! dir rec 0 (make-string 40 #\d)))
      (check-eq? (completion-result-status refused) 'delivery-pending-cannot-complete)
      (check-eq? (wave-status* (load-campaign-record dir (campaign-plan-id rec)) 0)
                 'awaiting-delivery)
      (define done (finalize-current! dir rec 0 merge-sha))
      (check-eq? (completion-result-status done) 'done)
      (cleanup-tmp dir))

    (test-case "repair receipt cannot finalize a different delivered handoff head or branch"
      (for ([field '(delivery-head-sha delivery-branch)]
            [wrong (list (make-string 40 #\d) "campaign/wrong")])
        (define dir (make-tmp-campaign-dir 1))
        (dynamic-wind
         void
         (lambda ()
           (define rec (load-or-migrate dir))
           (set-campaign-fence-token! rec 1)
           (begin-attempt! rec 0 1)
           (set-campaign-wave-status! (wave* rec 0) 'verifying)
           (persist-campaign! dir rec)
           (define attempt (campaign-wave-current-attempt (wave* rec 0)))
           (try-complete-wave! dir
                               rec
                               0
                               #:verifier-approve? #t
                               #:expected-attempt-id (campaign-attempt-id attempt)
                               #:expected-fence-token (campaign-attempt-fence-token attempt))
           (define merge-sha (seed-delivered-proof! dir rec 0))
           (define path (delivery-handoff-path dir (campaign-plan-id rec) 0))
           (define handoff (call-with-input-file path read))
           (call-with-output-file path
                                  (lambda (out) (write (hash-set handoff field wrong) out))
                                  #:exists 'truncate)
           (check-false (reconcile-delivered-handoff! dir (campaign-plan-id rec) 0 merge-sha))
           (check-eq? (completion-result-status (finalize-current! dir rec 0 merge-sha))
                      'delivery-pending-cannot-complete)
           (check-eq? (wave-status* (load-campaign-record dir (campaign-plan-id rec)) 0)
                      'awaiting-delivery)
           ;; A disputed DONE projection must not bypass the same journal/handoff gate.
           (set-campaign-wave-status! (wave* rec 0) 'done)
           (persist-campaign! dir rec)
           (check-not-false (delivery-pending-wave dir
                                                   (campaign-plan-id rec)
                                                   (campaign-record-waves rec)
                                                   (lambda (_b _p _w)
                                                     (hasheq 'status
                                                             "delivered"
                                                             'plan-id
                                                             (campaign-plan-id rec)
                                                             'wave
                                                             0
                                                             'merge-sha
                                                             merge-sha
                                                             'delivery-head-sha
                                                             (make-string 40 #\b)
                                                             'delivery-branch
                                                             "campaign/w0"
                                                             'attempt-id
                                                             (campaign-attempt-id attempt)
                                                             'attempt-fence
                                                             1
                                                             'binding-generation
                                                             0))
                                                   (make-hasheq))))
         (lambda () (cleanup-tmp dir)))))

    (test-case "authenticated old delivery proof cannot satisfy current receipt identity"
      (define dir (make-tmp-campaign-dir 1))
      (dynamic-wind
       void
       (lambda ()
         (define rec (load-or-migrate dir))
         (set-campaign-fence-token! rec 1)
         (begin-attempt! rec 0 1)
         (set-campaign-wave-status! (wave* rec 0) 'verifying)
         (persist-campaign! dir rec)
         (define attempt (campaign-wave-current-attempt (wave* rec 0)))
         (try-complete-wave! dir
                             rec
                             0
                             #:verifier-approve? #t
                             #:expected-attempt-id (campaign-attempt-id attempt)
                             #:expected-fence-token (campaign-attempt-fence-token attempt))
         (define merge-sha (seed-delivered-proof! dir rec 0))
         (define proof
           (hasheq 'status
                   "delivered"
                   'plan-id
                   (campaign-plan-id rec)
                   'wave
                   0
                   'merge-sha
                   merge-sha
                   'delivery-head-sha
                   (make-string 40 #\b)
                   'delivery-branch
                   "campaign/w0"
                   'attempt-id
                   (campaign-attempt-id attempt)
                   'attempt-fence
                   1
                   'binding-generation
                   0))
         (for ([key '(delivery-head-sha delivery-branch attempt-id attempt-fence binding-generation)]
               [wrong (list (make-string 40 #\d) "campaign/old" "attempt-old" 2 1)])
           (define verified (make-hasheq))
           (check-not-false (delivery-pending-wave dir
                                                   (campaign-plan-id rec)
                                                   (campaign-record-waves rec)
                                                   (lambda (_b _p _w) (hash-set proof key wrong))
                                                   verified))
           (check-equal? (hash-count verified) 0))
         (define verified (make-hasheq))
         (check-false (delivery-pending-wave dir
                                             (campaign-plan-id rec)
                                             (campaign-record-waves rec)
                                             (lambda (_b _p _w) proof)
                                             verified))
         (check-equal? (hash-ref verified 0) merge-sha)
         (set-campaign-wave-status! (wave* rec 0) 'done)
         (persist-campaign! dir rec)
         (define done-verified (make-hasheq))
         (check-false (delivery-pending-wave dir
                                             (campaign-plan-id rec)
                                             (campaign-record-waves rec)
                                             (lambda (_b _p _w) proof)
                                             done-verified))
         (check-equal? (hash-ref done-verified 0) merge-sha))
       (lambda () (cleanup-tmp dir))))

    (test-case "BUG-0077: cancelled awaiting-delivery cannot finalize"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (set-campaign-fence-token! rec 1)
      (begin-attempt! rec 0 1)
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define attempt (campaign-wave-current-attempt (wave* rec 0)))
      (try-complete-wave! dir
                          rec
                          0
                          #:verifier-approve? #t
                          #:expected-attempt-id (campaign-attempt-id attempt)
                          #:expected-fence-token (campaign-attempt-fence-token attempt))
      (define merge-sha (seed-delivered-proof! dir rec 0))
      (define durable (load-campaign-record dir (campaign-plan-id rec)))
      (set-campaign-cancellation! durable (make-campaign-cancellation "stop" 0))
      (persist-campaign! dir durable)
      (define result (finalize-current! dir rec 0 merge-sha))
      (check-eq? (completion-result-status result) 'cancelled)
      (check-eq? (wave-status* (load-campaign-record dir (campaign-plan-id rec)) 0)
                 'awaiting-delivery)
      (cleanup-tmp dir))

    (test-case "F9: default completion records the typed pending handoff"
      ;; A normal approval (delivery still pending) persists the typed handoff
      ;; before awaiting-delivery, so the v1.00.30 W4 incident state (DONE
      ;; without any journal) is impossible.
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define pid (campaign-plan-id rec))
      (define attempt (campaign-wave-current-attempt (wave* rec 0)))
      (define result
        (try-complete-wave! dir
                            rec
                            0
                            #:verifier-approve? #t
                            #:expected-attempt-id (campaign-attempt-id attempt)
                            #:expected-fence-token (campaign-attempt-fence-token attempt)))
      (check-eq? (completion-result-status result) 'awaiting-delivery)
      (check-eq? (delivery-handoff-status dir pid 0) 'delivery-pending)
      (define durable (load-campaign-record dir pid))
      (check-eq? (wave-status* durable 0) 'awaiting-delivery)
      (check-eq? (completion-outbox-invariant? dir durable) 'ok)
      (cleanup-tmp dir))

    (test-case "F10: rolling a done wave back leaves no leading completion event"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (begin
        (set-campaign-fence-token! rec 1)
        (begin-attempt! rec 0 1))
      (set-campaign-wave-status! (wave* rec 0) 'verifying)
      (persist-campaign! dir rec)
      (define pid (campaign-plan-id rec))
      (define attempt (campaign-wave-current-attempt (wave* rec 0)))
      (try-complete-wave! dir
                          rec
                          0
                          #:verifier-approve? #t
                          #:expected-attempt-id (campaign-attempt-id attempt)
                          #:expected-fence-token (campaign-attempt-fence-token attempt))
      (define merge-sha (seed-delivered-proof! dir rec 0))
      (finalize-current! dir rec 0 merge-sha)
      (define durable (load-campaign-record dir pid))
      (check-equal? (count-completion-events dir durable) 1)
      ;; Roll the done wave back to pending (the W4-era repair shape).
      (set-campaign-wave-status! (wave* durable 0) 'pending)
      (persist-campaign! dir durable)
      (define rolled (load-campaign-record dir pid))
      (check-eq? (completion-outbox-invariant? dir rolled)
                 'outbox-leads-record
                 "the derived outbox leads the rolled-back record")
      (check-eq? (reconcile-completion-outbox! dir rolled) 0 "nothing to append")
      (check-equal? (count-completion-events dir (load-campaign-record dir pid))
                    0
                    "the leading event was pruned")
      (check-eq? (completion-outbox-invariant? dir (load-campaign-record dir pid)) 'ok)
      (cleanup-tmp dir))

    (test-case "F10: reconcile drops an invented event for a never-done wave"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (persist-campaign! dir rec)
      (define pid (campaign-plan-id rec))
      (define fake-id (make-event-id pid 0 "attempt-fake"))
      ;; Forge a leading event through the same atomic writer the outbox uses.
      (define p (build-path dir ".planning" "campaigns" (string-append pid ".outbox.rktd")))
      (make-directory* (build-path dir ".planning" "campaigns"))
      (call-with-output-file p (lambda (out) (write (list fake-id) out)) #:exists 'truncate)
      (define durable (load-campaign-record dir pid))
      (check-eq? (completion-outbox-invariant? dir durable) 'outbox-leads-record)
      (check-eq? (reconcile-completion-outbox! dir durable) 0)
      (check-false
       (file-exists? (build-path dir ".planning" "campaigns" (string-append pid ".outbox.rktd")))
       "empty pruned outbox is removed")
      (cleanup-tmp dir))))

;; ============================================================
;; Runner
;; ============================================================

(define all-tests
  (test-suite "gsd-wave-completion (W1)"
    verifier-first-suite
    skip-suite
    outbox-suite
    no-heuristic-suite
    failure-reason-suite
    delivery-gate-suite))

(void (run-tests all-tests))
