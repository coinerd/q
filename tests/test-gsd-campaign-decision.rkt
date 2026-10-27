#lang racket/base

;; @speed fast
;; @suite extensions
;; @boundary integration

;; @covers extensions/gsd/campaign-decision.rkt
;; @covers extensions/gsd/go-orchestrator.rkt
;; @covers extensions/gsd/delivery-finalize.rkt
;; @covers extensions/gsd/delivery-handoff.rkt

;; tests/test-gsd-campaign-decision.rkt — W3 focused characterization.
;;
;; W3 replaces the three pre-W3 routing reconstructions (delivery-handoff
;; ready-for-run-checkpoint, delivery-finalize run-coordinator-checkpoint's
;; cond, go-orchestrator's `case` on their symbols) with ONE pure decision
;; (decide-campaign-action) plus ONE interpreter (run-campaign!). These tests
;; pin the decision matrix and the boundaries that must not regress:
;;
;;   * complete / launch / finalize / block / cancel precedence
;;   * every resume route, including the retained-supplemental block whose
;;     secondary seal runs before the typed denial and never launches work
;;   * a secondary seal failure cannot erase the primary cause
;;   * replay: a second /go after completion is idempotent and never reruns or
;;     duplicates implementation work, and no block leaves an outbox event
;;   * lease-busy diagnostics name the lock path, holder and live assessment
;;
;; Routing is asserted through typed kinds/routes/codes only. No assertion
;; chooses behaviour by matching human message text.

(require rackunit
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         racket/system
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
                  set-campaign-wave-status!
                  set-campaign-cancellation!
                  make-campaign-cancellation
                  set-campaign-fence-token!
                  set-campaign-record-execution-policy!
                  migrate-campaign!)
         (only-in "../extensions/gsd/campaign-repository.rkt"
                  persist-campaign!
                  load-campaign-record)
         (only-in "../extensions/gsd/campaign-decision.rkt"
                  campaign-views
                  campaign-delivery-view
                  campaign-lane-view
                  campaign-decision-kind
                  campaign-decision-route
                  campaign-decision-wave-index
                  campaign-decision-code
                  campaign-decision-message
                  campaign-decision-merge-sha
                  decide-campaign-action)
         (only-in "../extensions/gsd/delivery-finalize.rkt"
                  run-campaign-block!
                  run-delivery-finalization
                  campaign-outcome-kind
                  campaign-outcome-reason
                  campaign-outcome-typed)
         (only-in "../extensions/gsd/go-orchestrator.rkt"
                  run-campaign!
                  acquire-lease
                  release-lease!
                  campaign-lease?
                  campaign-result-status
                  campaign-result-message
                  campaign-result-completed-waves)
         (only-in "../extensions/gsd/campaign-result.rkt"
                  campaign-result-code
                  campaign-result-diagnostics
                  render-campaign-stop-text)
         (only-in "../extensions/gsd/delivery-handoff.rkt"
                  delivery-handoff-path
                  delivery-handoff-status)
         (only-in "../extensions/gsd/wave-completion.rkt" load-outbox make-event-id)
         (only-in "helpers/honest-delivery-fixture.rkt"
                  awaiting-test-merge-sha
                  seed-awaiting-delivery!
                  test-delivered-proof
                  test-delivered-reader))

(define-runtime-path campaign-lease-module "../extensions/gsd/campaign-lease.rkt")

;; ============================================================
;; Pure fixtures (no I/O)
;; ============================================================

(define PLAN-ID (make-string 64 #\a))
(define SHA-40 (make-string 40 #\b))

(define (wave idx status)
  (make-campaign-wave idx (format "w~a" idx) status 0 #f))

(define (record waves #:cancelled? [cancelled? #f])
  (make-campaign-record PLAN-ID
                       (make-campaign-manifest 1 "decision fixture" '() '() "constraints")
                       waves
                       (and cancelled? (make-campaign-cancellation "stop" 0))
                       0
                       'test
                       0
                       0))

(define (views rec
               #:pending [pending #f]
               #:awaiting [awaiting (hasheq)]
               #:stale? [stale? #f]
               #:lane [lane #f]
               #:message [message "pending delivery"]
               #:coordinator? [coordinator? #f])
  (campaign-views rec
                  '()
                  stale?
                  (campaign-delivery-view pending awaiting)
                  lane
                  message
                  coordinator?))

(define (lane idx
              #:author-pending? [author-pending? #f]
              #:resumable? [resumable? #f]
              #:retained? [retained? #f]
              #:author-repair? [author-repair? #f]
              #:author-running? [author-running? #f]
              #:review [review #f])
  (campaign-lane-view idx #f author-pending? resumable? retained? author-repair? author-running? review))

;; ============================================================
;; 1. Decision matrix — complete / launch / finalize
;; ============================================================

(test-case "all waves done and delivery-proven decides complete"
  (define d (decide-campaign-action (views (record (list (wave 0 'done) (wave 1 'deferred))))))
  (check-eq? (campaign-decision-kind d) 'complete)
  (check-eq? (campaign-decision-code d) 'campaign-complete)
  (check-eq? (campaign-decision-route d) 'none))

(test-case "an actionable wave decides exactly one launch"
  (define d (decide-campaign-action (views (record (list (wave 0 'done) (wave 1 'pending))))))
  (check-eq? (campaign-decision-kind d) 'launch)
  (check-eq? (campaign-decision-wave-index d) 1)
  (check-eq? (campaign-decision-code d) 'wave-launch))

(test-case "an awaiting-delivery wave with an authenticated proof decides finalize with that proof"
  (define d (decide-campaign-action
             (views (record (list (wave 0 'awaiting-delivery) (wave 1 'pending)))
                    #:awaiting (hash 0 SHA-40))))
  (check-eq? (campaign-decision-kind d) 'finalize)
  (check-eq? (campaign-decision-wave-index d) 0)
  (check-equal? (campaign-decision-merge-sha d) SHA-40)
  (check-eq? (campaign-decision-code d) 'delivery-ready))

(test-case "an awaiting-delivery wave WITHOUT a binding proof never finalizes or launches past it"
  (define d (decide-campaign-action
             (views (record (list (wave 0 'awaiting-delivery) (wave 1 'pending)))
                    #:pending (cons 0 "implementation PR pending")
                    #:lane (lane 0))))
  (check-eq? (campaign-decision-kind d) 'block)
  (check-eq? (campaign-decision-wave-index d) 0)
  (check-not-eq? (campaign-decision-kind d) 'launch))

;; ============================================================
;; 2. Decision matrix — cancel precedence
;; ============================================================

(test-case "durable cancellation wins over launch"
  (define d (decide-campaign-action
             (views (record (list (wave 0 'pending)) #:cancelled? #t)
                    #:pending (cons 0 "pending")
                    #:lane (lane 0))))
  (check-eq? (campaign-decision-kind d) 'cancel)
  (check-eq? (campaign-decision-code d) 'campaign-cancelled))

(test-case "a record that moved during the authenticated readback cancels with its own code"
  (define d (decide-campaign-action
             (views (record (list (wave 0 'pending))) #:stale? #t)))
  (check-eq? (campaign-decision-kind d) 'cancel)
  (check-eq? (campaign-decision-code d) 'campaign-stale-readback))

;; ============================================================
;; 3. Decision matrix — every resume route, and the retained block
;; ============================================================

(define (pending-decision lane #:coordinator? [coordinator? #t])
  (decide-campaign-action
   (views (record (list (wave 0 'done)))
          #:pending (cons 0 "no authenticated delivery proof")
          #:lane lane
          #:coordinator? coordinator?)))

(test-case "approved unlaunched supplemental task resumes the author lane"
  (define d (pending-decision (lane 0 #:author-pending? #t #:retained? #t)))
  (check-eq? (campaign-decision-kind d) 'resume)
  (check-eq? (campaign-decision-route d) 'supplemental-author))

(test-case "an active supplemental operation resumes the protected checkpoint"
  (define d (pending-decision (lane 0 #:resumable? #t #:retained? #t)))
  (check-eq? (campaign-decision-kind d) 'resume)
  (check-eq? (campaign-decision-route d) 'supplemental-checkpoint))

(test-case "a spent retained supplemental pointer BLOCKS, never the ordinary coordinator"
  ;; Both with and without a coordinator: the retained block owns the wave.
  (for ([coordinator? (list #t #f)])
    (define d (pending-decision (lane 0 #:retained? #t) #:coordinator? coordinator?))
    (check-eq? (campaign-decision-kind d) 'block)
    (check-eq? (campaign-decision-route d) 'supplemental-retained)
    (check-eq? (campaign-decision-code d) 'supplemental-retained)))

(test-case "durable author permission and publication recovery are distinct routes"
  (define permission (pending-decision (lane 0 #:author-repair? #t)))
  (check-eq? (campaign-decision-route permission) 'author-permission)
  (define recovery (pending-decision (lane 0 #:author-repair? #t #:author-running? #t)))
  (check-eq? (campaign-decision-route recovery) 'author-publication-recovery))

(test-case "a pending review rejection replays before the coordinator"
  (define d (pending-decision (lane 0 #:review "findings verbatim")))
  (check-eq? (campaign-decision-route d) 'review-replay))

(test-case "without any lane a wired coordinator is the route; without one the campaign blocks"
  (define coordinated (pending-decision #f #:coordinator? #t))
  (check-eq? (campaign-decision-kind coordinated) 'resume)
  (check-eq? (campaign-decision-route coordinated) 'coordinator-step)
  (define uncoordinated (pending-decision #f #:coordinator? #f))
  (check-eq? (campaign-decision-kind uncoordinated) 'block)
  (check-eq? (campaign-decision-route uncoordinated) 'none)
  (check-eq? (campaign-decision-code uncoordinated) 'delivery-pending))

;; ============================================================
;; 4. Block effect: secondary seal, primary cause, no launch
;; ============================================================

;; A fabricated block decision for a real (but empty) campaign root: the entry
;; seam has no supplemental business, so the primary cause must survive.
(define (call-with-empty-root proc)
  (define dir (make-temporary-file "campaign-decision-~a" 'directory))
  (dynamic-wind void (lambda () (proc dir)) (lambda () (delete-directory/files dir))))

(test-case "a retained block with no supplemental business preserves the primary cause"
  (call-with-empty-root
   (lambda (dir)
     (define decision
       (decide-campaign-action
        (views (record (list (wave 0 'done)))
               #:pending (cons 0 "no authenticated delivery proof")
               #:lane (lane 0 #:retained? #t))))
     (define outcome (run-campaign-block! dir PLAN-ID decision))
     (check-eq? (campaign-outcome-kind outcome) 'blocked)
     (check-false (campaign-outcome-typed outcome))
     (check-true (string? (campaign-outcome-reason outcome))))))

(test-case "a secondary seal failure cannot erase the primary cause or launch work"
  ;; An invalid campaign identity makes every supplemental probe/effect refuse;
  ;; the block must still return its primary reason (never an exception).
  (call-with-empty-root
   (lambda (dir)
     (define decision
       (decide-campaign-action
        (views (record (list (wave 0 'done)))
               #:pending (cons 0 "no authenticated delivery proof")
               #:lane (lane 0 #:retained? #t))))
     (define outcome
       (with-handlers ([exn:fail? (lambda (e) (error 'test "block raised: ~a" (exn-message e)))])
         (run-campaign-block! dir "not-a-plan-id" decision)))
     (check-eq? (campaign-outcome-kind outcome) 'blocked)
     (check-equal? (campaign-outcome-reason outcome)
                   (campaign-decision-message decision)))))

;; ============================================================
;; 5. Interpreter integration (real durable campaign)
;; ============================================================

;; A minimal two-wave campaign on disk (no receipts, no delivery proofs).
(define (call-with-campaign count proc)
  (define dir (make-temporary-file "campaign-decision-int-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (make-directory* (build-path dir ".planning/waves"))
     (call-with-output-file
      (build-path dir ".planning/PLAN.md")
      (lambda (out)
        (display "# Plan: Decision test\n\n## Waves\n\n" out)
        (for ([i (in-range count)])
          (fprintf out "- [Inbox] W~a: Test → waves/W~a-test.md\n" i i)
          (display-to-file "# Test\n\nGoal: test\n\n## Verify\n\nraco test .\n"
                           (build-path dir ".planning/waves" (format "W~a-test.md" i))))))
     (define rec (migrate-campaign! dir))
     ;; Legacy plain-runner fixture: shared checkout, no owned phases.
     (set-campaign-record-execution-policy! rec #f)
     (persist-campaign! dir rec)
     (proc dir rec))
   (lambda () (delete-directory/files dir))))

(define (pending-readable _base _plan _wave)
  (hasheq 'status "delivery-pending" 'reason "implementation PR pending"))

(test-case "a delivery-pending campaign blocks with the durable code and the handoff ledger"
  (call-with-campaign
   1
   (lambda (dir rec)
     (define plan (campaign-plan-id rec))
     (define result
       (run-campaign!
        dir
        rec
        #:runner (lambda (_) 'ok)
        #:verifier (lambda (_) #t)
        #:delivery-reader pending-readable))
     (check-eq? (campaign-result-status result) 'wave-blocked)
     (check-eq? (campaign-result-code result) 'delivery-pending)
     (check-true (hash? (campaign-result-diagnostics result)))
     (check-eq? (hash-ref (campaign-result-diagnostics result) 'route) 'none)
     ;; The pending handoff ledger was issued exactly once and never invents DONE.
     (check-eq? (delivery-handoff-status dir plan 0) 'delivery-pending)
     (check-equal? (campaign-result-completed-waves result) '())
     (check-equal? (load-outbox dir plan) '()))))

(test-case "a live coordinator is consulted once for the blocked wave and cannot claim progress"
  (call-with-campaign
   1
   (lambda (dir rec)
     (define calls 0)
     (define result
       (run-campaign!
        dir
        rec
        #:runner (lambda (_) 'ok)
        #:verifier (lambda (_) #t)
        #:delivery-reader pending-readable
        #:delivery-coordinator (lambda (_b _p _w)
                                 (set! calls (add1 calls))
                                 (cons 'ok "claimed progress"))))
     (check-equal? calls 1)
     (check-eq? (campaign-result-status result) 'wave-blocked)
     (check-eq? (campaign-result-code result) 'coordinator-step)
     ;; No durable journal movement means no advance (no-spin law preserved).
     (check-eq? (hash-ref (campaign-result-diagnostics result) 'route) 'coordinator-step))))

(test-case "a cancelled campaign short-circuits before any readback, launch, or coordinator"
  (call-with-campaign
   1
   (lambda (dir rec)
     (define plan (campaign-plan-id rec))
     (define latest (load-campaign-record dir plan))
     (set-campaign-cancellation! latest (make-campaign-cancellation "operator" 1))
     (persist-campaign! dir latest)
     (define readbacks 0)
     (define result
       (run-campaign!
        dir
        latest
        #:runner (lambda (_) (error 'test "cancelled campaign must not launch"))
        #:delivery-reader (lambda (_b _p _w)
                            (set! readbacks (add1 readbacks))
                            (hasheq 'status "delivery-pending" 'reason "implementation PR pending"))
        #:delivery-coordinator (lambda (_b _p _w)
                                 (error 'test "cancelled campaign must not coordinate"))))
     (check-eq? (campaign-result-status result) 'wave-cancelled)
     (check-eq? (campaign-result-code result) 'campaign-cancelled)
     (check-equal? readbacks 0))))

(test-case "replay: a completed campaign is idempotent and never reruns implementation"
  (call-with-campaign
   1
   (lambda (dir rec)
     (define plan (campaign-plan-id rec))
     ;; Mark the single wave done and give it an authenticated proof that is
     ;; valid for the historical no-journal fixture seam.
     (define done (load-campaign-record dir plan))
     (set-campaign-wave-status! (car (campaign-record-waves done)) 'done)
     (set-campaign-fence-token! done 7)
     (persist-campaign! dir done)
     (define delivered
       (lambda (_b p w)
         (hasheq 'status "delivered" 'plan-id p 'wave w 'merge-sha SHA-40)))
     (define runs 0)
     (define first
       (run-campaign! dir done
                      #:runner (lambda (_) (set! runs (add1 runs)) 'ok)
                      #:delivery-reader delivered))
     (check-eq? (campaign-result-status first) 'campaign-complete)
     ;; The wave was already durable DONE: nothing was re-completed in this run.
     (check-equal? (campaign-result-completed-waves first) '())
     (define outbox-after-first (load-outbox dir plan))
     (define second
       (run-campaign! dir
                      (load-campaign-record dir plan)
                      #:runner (lambda (_) (set! runs (add1 runs)) 'ok)
                      #:delivery-reader delivered))
     (check-eq? (campaign-result-status second) 'campaign-complete)
     (check-equal? runs 0 "implementation is never rerun after delivery")
     (check-equal? (load-outbox dir plan) outbox-after-first "replay adds no duplicate completion event"))))

(test-case "lease contention names the lock path, holder, and live assessment"
  (call-with-campaign
   1
   (lambda (dir rec)
     (define plan (campaign-plan-id rec))
     (define lease (acquire-lease dir plan #:session-id "holder-session"))
     (dynamic-wind
      void
      (lambda ()
        (define result (run-campaign! dir rec #:runner (lambda (_) (error 'test "must not run"))))
        (check-eq? (campaign-result-status result) 'busy)
        (check-eq? (campaign-result-code result) 'lease-busy)
        (define d (campaign-result-diagnostics result))
        (check-equal? (hash-ref d 'recorded-owner) "holder-session")
        (check-eq? (hash-ref d 'lease-state) 'held-live)
        (check-true (string? (hash-ref d 'lock-path)))
        (check-true (file-exists? (string->path (hash-ref d 'lock-path))))
        (check-true (string-contains? (render-campaign-stop-text result)
                                      "Next action: inspect the lock holder")))
      (lambda () (release-lease! lease))))))

;; ============================================================
;; 6. Crash-boundary and cross-process lease characterization
;;
;; These tests use only existing seams: the REAL OS advisory lock in a child
;; process, the durable parked-with-proof campaign state the production
;; finalize path consumes, and the existing run-delivery-finalization /
;; delivery-reader injection points. No production permission is widened and
;; no GitHub/network effect is reachable.
;; ============================================================

(test-case "a lease held by a child OS process is reacquired after the child exits (file persists)"
  (call-with-campaign
   1
   (lambda (dir rec)
     (define plan (campaign-plan-id rec))
     (define lock-path
       (build-path dir ".planning" "campaigns" (string-append plan ".lock")))
     ;; The child acquires the real OS lock, records the owner diagnostics, and
     ;; exits WITHOUT releasing. Process exit drops the advisory lock but the
     ;; stable lock FILE (holder evidence) remains on disk.
     (define child-code
       (format
        (string-append "(begin (require (file ~s)) "
                       "(unless (acquire-lease ~s ~s #:session-id \"child-holder\") (exit 2)) "
                       "(displayln 'acquired) (flush-output) (exit 0))")
        (path->string (path->complete-path campaign-lease-module))
        (path->string dir)
        plan))
     (define child-log (build-path dir "child-lease.stdout"))
     (define child-log-port (open-output-file child-log #:exists 'truncate))
     (define-values (child _child-in _child-out _child-err)
       (subprocess child-log-port #f #f (find-executable-path "racket") "-e" child-code))
     (subprocess-wait child)
     (close-output-port child-log-port)
     (define child-stdout (file->string child-log))
     (check-equal? (subprocess-status child) 0 (format "child failed: ~a" child-stdout))
     (check-true (string-contains? child-stdout "acquired"))
     (check-true (file-exists? lock-path)
                 "the diagnostic lock FILE survives the child process")
     (define recorded (call-with-input-file lock-path read))
     (check-equal? (hash-ref recorded 'owner #f) "child-holder")
     ;; The child is gone, so its OS lock is gone even though the file persists.
     (define parent-lease (acquire-lease dir plan #:session-id "parent-holder"))
     (check-true (campaign-lease? parent-lease)
                 "the parent reacquires while the old lock file still exists")
     (check-true (file-exists? lock-path) "reacquisition reuses the stable path")
     (release-lease! parent-lease))))

(test-case "crash after an authenticated proof before the finalize commit retries to one DONE/completion"
  (call-with-campaign
   1
   (lambda (dir rec)
     (define plan (campaign-plan-id rec))
     (define merge (awaiting-test-merge-sha))
     ;; Implementation verified and parked: awaiting-delivery, never DONE.
     (define parked-by-run
       (run-campaign! dir rec
                      #:runner (lambda (_) 'ok)
                      #:verifier (lambda (_) #t)
                      #:delivery-reader pending-readable))
     (check-eq? (campaign-result-status parked-by-run) 'wave-blocked)
     (check-equal? (load-outbox dir plan) '())
     ;; Operator completes the protected merge: the proof becomes durable. The
     ;; next /go authenticates it in the readback and then the process dies
     ;; before the finalize commit.
     (seed-awaiting-delivery! dir plan 0 #:merge-sha merge)
     (define before (load-campaign-record dir plan))
     (define attempt-id
       (campaign-attempt-id (campaign-wave-current-attempt (car (campaign-record-waves before)))))
     (check-eq? (campaign-wave-status (car (campaign-record-waves before))) 'awaiting-delivery)
     (check-equal? (load-outbox dir plan) '())
     (check-exn exn:fail?
                (lambda ()
                  (run-campaign! dir before
                                 #:runner (lambda (_) (error 'test "must not rerun"))
                                 #:delivery-reader
                                 (lambda (base plan-id wave)
                                   (define proof (test-delivered-proof base plan-id wave #:merge-sha merge))
                                   (when proof
                                     (error 'simulated-crash
                                            "process died after authenticating the proof"))
                                   proof))))
     (define after-crash (load-campaign-record dir plan))
     (check-eq? (campaign-wave-status (car (campaign-record-waves after-crash))) 'awaiting-delivery)
     (check-equal? (load-outbox dir plan) '() "the crash window commits no completion")
     ;; Retry: exactly one DONE and exactly one completion event, no rerun.
     (define runs 0)
     (define retry
       (run-campaign! dir after-crash
                      #:runner (lambda (_) (set! runs (add1 runs)) 'ok)
                      #:verifier (lambda (_) #t)
                      #:delivery-reader (test-delivered-reader #:merge-sha merge)))
     (check-eq? (campaign-result-status retry) 'campaign-complete)
     (check-equal? (campaign-result-completed-waves retry) (list 0))
     (check-equal? runs 0 "a verified implementation is never rerun by finalization")
     (check-equal? (load-outbox dir plan) (list (make-event-id plan 0 attempt-id)))
     (define done (load-campaign-record dir plan))
     (check-eq? (campaign-wave-status (car (campaign-record-waves done))) 'done))))

(test-case "crash after the DONE commit before the loop continues replays without rerun or duplicate event"
  (call-with-campaign
   1
   (lambda (dir rec)
     (define plan (campaign-plan-id rec))
     (define merge (awaiting-test-merge-sha))
     (run-campaign! dir rec
                    #:runner (lambda (_) 'ok)
                    #:verifier (lambda (_) #t)
                    #:delivery-reader pending-readable)
     (seed-awaiting-delivery! dir plan 0 #:merge-sha merge)
     (define parked (load-campaign-record dir plan))
     (define attempt-id
       (campaign-attempt-id (campaign-wave-current-attempt (car (campaign-record-waves parked)))))
     ;; The DONE commit and the outbox append happen, then the process dies
     ;; before the orchestrator loop resumes: #:on-delivered raises.
     (check-exn exn:fail?
                (lambda ()
                  (run-delivery-finalization
                   dir plan parked 0 merge '()
                   #:on-delivered (lambda () (error 'simulated-crash "after DONE commit")))))
     (define committed (load-campaign-record dir plan))
     (check-eq? (campaign-wave-status (car (campaign-record-waves committed))) 'done)
     (check-equal? (load-outbox dir plan) (list (make-event-id plan 0 attempt-id)))
     ;; Replay: no runner call and no duplicate completion event.
     (define runs 0)
     (define replay
       (run-campaign! dir committed
                      #:runner (lambda (_) (set! runs (add1 runs)) 'ok)
                      #:verifier (lambda (_) #t)
                      #:delivery-reader (test-delivered-reader #:merge-sha merge)))
     (check-eq? (campaign-result-status replay) 'campaign-complete)
     (check-equal? (campaign-result-completed-waves replay) '())
     (check-equal? runs 0)
     (check-equal? (load-outbox dir plan) (list (make-event-id plan 0 attempt-id))))))
