#lang racket/base

;; @speed fast  ;; @suite extensions

;; tests/test-gsd-wave-executor-isolation.rkt — W3 (#9234) orchestration tests
;;
;; The coordinator consumes ONE structured terminal outcome per runner
;; invocation. Deterministic fakes exercise: exactly-once completion, timeout
;; @boundary integration
;; → interrupted (no invented DONE, no outbox event), pending-tool
;; cancellation → interrupted (no event), and legacy symbol runner compat.

(require rackunit
         (only-in "helpers/honest-delivery-fixture.rkt" finalize-awaiting!)
         rackunit/text-ui
         racket/file
         racket/path
         racket/string
         (only-in "../extensions/gsd/wave-runner-port.rkt"
                  wave-execution-outcome
                  wave-execution-outcome-kind
                  make-wave-runner-port
                  gsd-wave-runner-port-cancel!
                  gsd-wave-runner-port-cancel-requested?)
         (only-in "../extensions/gsd/campaign-state.rkt"
                  make-campaign-manifest
                  make-campaign-wave-descriptor
                  make-campaign-wave
                  make-campaign-record
                  campaign-plan-id
                  campaign-record-waves
                  campaign-wave-index
                  campaign-wave-status
                  set-campaign-wave-status!
                  select-next-actionable-wave
                  migrate-campaign!)
         (only-in "../extensions/gsd/campaign-repository.rkt" persist-campaign! load-campaign-record)
         (only-in "../extensions/gsd/wave-completion.rkt" count-completion-events)
         (only-in "../extensions/gsd/go-orchestrator.rkt"
                  run-campaign-wave
                  run-campaign!
                  campaign-result-status
                  campaign-result-message)
         (only-in "../extensions/gsd/policy.rkt" current-gsd-wave-timeout-retries)
         (only-in "helpers/gsd-timeout-fake.rkt" with-deterministic-timeout))

;; ============================================================
;; Helpers
;; ============================================================

;; W4 deterministic timeout seam: campaign-level timeout cases run the real
;; executor on a fake timeline owned by tests/helpers/gsd-timeout-fake.rkt.
;; The fake clock advances only when the adapter waits, and a posted
;; cancellation is delivered immediately, so deadline expiry, cancellation
;; grace, and force-kill happen with zero wall-clock sleeps while every
;; production timeout/cancel/cleanup/outcome step still executes for real.
;; Empty stage list = every wait is a pure tick: the hung runner never
;; completes, so the deadline expires deterministically.

(define (make-tmp-campaign-dir n-waves)
  (define dir (make-temporary-file "exec-isol-~a" 'directory))
  (make-directory* (build-path dir ".planning" "waves"))
  (call-with-output-file (build-path dir ".planning" "PLAN.md")
                         (lambda (out)
                           (display "# Plan: Test Campaign\n\n## Waves\n\n" out)
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

(define (wave-status* rec idx)
  (for/first ([w (campaign-record-waves rec)]
              #:when (= (campaign-wave-index w) idx))
    (campaign-wave-status w)))

;; ============================================================
;; Suites
;; ============================================================

(define exactly-once-suite
  (test-suite "exactly-once completion"

    (test-case "structured done runner → exactly one completion event"
      ;; Single wave: the campaign must end by finalizing THIS wave's
      ;; delivery, never by starting a successor.
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (define result
        (run-campaign-wave dir
                           rec
                           0
                           #:runner (make-wave-runner-port
                                     (lambda (idx) (wave-execution-outcome 'done "wave finished")))
                           #:verifier (lambda (_) #t)))
      (check-eq? (campaign-result-status result) 'wave-awaiting-delivery)
      (check-eq? (wave-status* rec 0) 'awaiting-delivery)
      (check-equal? (count-completion-events dir rec)
                    0
                    "no completion event before authenticated delivery")
      (finalize-awaiting! dir rec)
      (check-equal? (count-completion-events dir rec) 1 "exactly one completion event per done wave")
      (cleanup-tmp dir))

    (test-case "re-execution of a done attempt is stale-ignored (no duplicate)"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (define runner (make-wave-runner-port (lambda (idx) (wave-execution-outcome 'done "ok"))))
      (define first (run-campaign-wave dir rec 0 #:runner runner #:verifier (lambda (_) #t)))
      (check-eq? (campaign-result-status first) 'wave-awaiting-delivery)
      (finalize-awaiting! dir rec)
      ;; second run with the same record: fence/attempt are stale
      (define second (run-campaign-wave dir rec 0 #:runner runner))
      (check-eq? (campaign-result-status second) 'wave-cancelled)
      (check-equal? (count-completion-events dir rec)
                    1
                    "duplicate attempt must not duplicate the completion event")
      (cleanup-tmp dir))

    (test-case "timeout never produces a completion event"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (define runner
        (make-wave-runner-port (lambda (idx)
                                 (sleep 30)
                                 (wave-execution-outcome 'done "too late"))))
      ;; The default timeout-retries (5) is a production policy for transient
      ;; session hangs; here the runner is deterministically hung, so retries
      ;; only re-pay the 1s deadline + 2s cancel grace for no new information.
      ;; Disable them: timeout semantics under test are retry-count-agnostic.
      (define result
        (with-deterministic-timeout
         '()
         (lambda ()
           (run-campaign-wave dir rec 0 #:runner runner #:timeout-sec 1 #:timeout-retries 0))))
      (check-eq? (campaign-result-status result) 'wave-cancelled)
      (check-eq? (wave-status* rec 0) 'interrupted)
      (check-equal? (count-completion-events dir rec) 0 "timed-out run must not invent a DONE")
      (cleanup-tmp dir))))

(define timeout-suite
  (test-suite "timeout / interrupt semantics"

    (test-case "timed-out runner → interrupted, campaign stops"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (define runner
        (make-wave-runner-port (lambda (idx)
                                 (sleep 30)
                                 (wave-execution-outcome 'done "late"))))
      (define result
        (with-deterministic-timeout
         '()
         (lambda ()
           (run-campaign-wave dir rec 0 #:runner runner #:timeout-sec 1 #:timeout-retries 0))))
      (check-eq? (campaign-result-status result) 'wave-cancelled)
      (check-true (string-contains? (campaign-result-message result) "exceeded"))
      (check-eq? (wave-status* rec 0) 'interrupted)
      (check-false (eq? (wave-status* rec 0) 'done) "timeout must not persist DONE")
      (cleanup-tmp dir))

    (test-case "missing timeout-sec → mandatory default deadline (never unbounded)"
      ;; follow-up regression: run-campaign-wave without
      ;; #:timeout-sec used to bind run-one to the RAW runner port, so a hung
      ;; runner blocked the campaign thread forever (the /go executor path
      ;; sat idle and never returned to the coordinator). The deadline is now
      ;; mandatory: absent the keyword the executor wraps with
      ;; current-gsd-wave-timeout-seconds (default 7200, guarded
      ;; positive-real — never #f). The deterministic fake expires even a
      ;; 7200 s deadline in pure ticks — zero wall-clock cost.
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (define runner
        (make-wave-runner-port (lambda (idx)
                                 (sleep 30)
                                 (wave-execution-outcome 'done "late"))))
      (define result
        (parameterize ([current-gsd-wave-timeout-retries 0])
          (with-deterministic-timeout '() (lambda () (run-campaign-wave dir rec 0 #:runner runner)))))
      (check-eq? (campaign-result-status result) 'wave-cancelled)
      (check-true (string-contains? (campaign-result-message result) "exceeded"))
      (check-eq? (wave-status* rec 0) 'interrupted)
      (check-false (eq? (wave-status* rec 0) 'done) "default-deadline timeout must not persist DONE")
      (cleanup-tmp dir))

    (test-case "interrupted outcome → interrupted"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (define result
        (run-campaign-wave dir
                           rec
                           0
                           #:runner (make-wave-runner-port
                                     (lambda (idx) (wave-execution-outcome 'interrupted "force")))))
      (check-eq? (campaign-result-status result) 'wave-cancelled)
      (check-eq? (wave-status* rec 0) 'interrupted)
      (cleanup-tmp dir))

    (test-case "cancelled outcome → interrupted"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (define result
        (run-campaign-wave dir
                           rec
                           0
                           #:runner (make-wave-runner-port
                                     (lambda (idx) (wave-execution-outcome 'cancelled "user")))))
      (check-eq? (campaign-result-status result) 'wave-cancelled)
      (check-eq? (wave-status* rec 0) 'interrupted)
      (check-equal? (count-completion-events dir rec) 0)
      (cleanup-tmp dir))))

(define pending-cancel-suite
  (test-suite "pending-tool cancellation"

    (test-case "runner polling cancel-requested? aborts mid-run → interrupted, no event"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (define started (make-semaphore 0))
      (define release (make-semaphore 0))
      (define cancelled? #f)
      (define port
        (make-wave-runner-port (lambda (idx)
                                 (semaphore-post started)
                                 (semaphore-wait release) ;; pending tool: polls its loop
                                 (if ((gsd-wave-runner-port-cancel-requested? port))
                                     (wave-execution-outcome 'cancelled "pending tool cancelled")
                                     (wave-execution-outcome 'done "completed")))
                               #:cancel! (lambda ()
                                           (set! cancelled? #t)
                                           (semaphore-post release))
                               #:cancel-requested? (lambda () cancelled?)))
      (define result-box (box #f))
      (define t
        (thread (lambda () (set-box! result-box (run-campaign-wave dir rec 0 #:runner port)))))
      (semaphore-wait started) ;; runner is mid-flight with a pending tool
      ;; campaign cancellation arrives while the tool is still pending
      ((gsd-wave-runner-port-cancel! port))
      (thread-wait t)
      (define result (unbox result-box))
      (check-eq? (campaign-result-status result) 'wave-cancelled)
      (check-eq? (wave-status* rec 0) 'interrupted)
      (check-equal? (count-completion-events dir rec)
                    0
                    "pending-tool cancellation must not invent a completion")
      (cleanup-tmp dir))

    (test-case "timeout adapter polls durable cancellation and invokes cancel once"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (define started (make-semaphore 0))
      (define release (make-semaphore 0))
      (define requested? (box #f))
      (define cancel-count (box 0))
      (define port
        (make-wave-runner-port (lambda (_idx)
                                 (semaphore-post started)
                                 (semaphore-wait release)
                                 (wave-execution-outcome 'done "must not win after cancellation"))
                               #:cancel! (lambda ()
                                           (set-box! cancel-count (add1 (unbox cancel-count)))
                                           (semaphore-post release))
                               #:cancel-requested? (lambda () (unbox requested?))))
      (define result-box (box #f))
      (define worker
        (thread (lambda ()
                  (set-box! result-box
                            (run-campaign-wave dir rec 0 #:runner port #:timeout-sec 10)))))
      (semaphore-wait started)
      (set-box! requested? #t)
      (thread-wait worker)
      (check-eq? (campaign-result-status (unbox result-box)) 'wave-cancelled)
      (check-equal? (unbox cancel-count) 1)
      (check-eq? (wave-status* rec 0) 'interrupted)
      (cleanup-tmp dir))))

(define compat-suite
  (test-suite "legacy symbol runners"

    (test-case "symbol runner 'ok still completes a wave"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (define result
        (run-campaign-wave dir rec 0 #:runner (lambda (_) 'ok) #:verifier (lambda (_) #t)))
      (check-eq? (campaign-result-status result) 'wave-awaiting-delivery)
      (check-eq? (wave-status* rec 0) 'awaiting-delivery)
      (cleanup-tmp dir))

    (test-case "symbol runner 'cancelled still interrupts"
      (define dir (make-tmp-campaign-dir 1))
      (define rec (load-or-migrate dir))
      (define result (run-campaign-wave dir rec 0 #:runner (lambda (_) 'cancelled)))
      (check-eq? (campaign-result-status result) 'wave-cancelled)
      (check-eq? (wave-status* rec 0) 'interrupted)
      (cleanup-tmp dir))

    (test-case "run-campaign! timeout applies to every wave"
      (define dir (make-tmp-campaign-dir 3))
      (define rec (load-or-migrate dir))
      (define result
        ;; Same retry rationale as above: the hung runner makes each retry
        ;; re-pay 1s deadline + 2s cancel grace, so pin the production
        ;; timeout-retry policy off for this deterministic scenario.
        (with-deterministic-timeout
         '()
         (lambda ()
           (parameterize ([current-gsd-wave-timeout-retries 0])
             (run-campaign! dir
                            rec
                            #:runner (make-wave-runner-port (lambda (idx)
                                                              (sleep 30)
                                                              (wave-execution-outcome 'done "late")))
                            #:timeout-sec 1)))))
      (check-eq? (campaign-result-status result) 'wave-cancelled)
      (check-eq? (wave-status* rec 0) 'interrupted)
      (check-equal? (count-completion-events dir rec) 0)
      (cleanup-tmp dir))))

;; ============================================================
;; BUG-0028 S1/S2 (W2): gsd.worktree-isolation settings wiring
;; ============================================================
;; Precedence (documented at resolve-worktree-isolation):
;;   explicit #:isolate? > gsd.worktree-isolation key > parameter default (OFF).
;; (a) key false/absent → shared checkout route (isolation stays OFF);
;; (b) key true → isolated route;
;; (c) explicit #:isolate? overrides the key in BOTH directions;
;; (d) banner names active worktree + resolved allowed roots.

(require (only-in "../extensions/gsd/wave-executor.rkt"
                  current-gsd-worktree-isolation
                  worktree-isolation-enabled?
                  resolve-worktree-isolation
                  apply-worktree-isolation-setting!
                  worktree-isolation-banner
                  tracker-arming-line)
         (only-in "../runtime/settings-core.rkt" q-settings)
         (only-in "../runtime/settings-query.rkt" gsd-tracker-live-binding))

(define settings-wiring-suite
  (test-suite "BUG-0028: gsd.worktree-isolation settings wiring"

    (test-case "(a) key false → shared checkout route (isolation OFF)"
      (parameterize ([current-gsd-worktree-isolation #f])
        (define settings (q-settings (hash) (hash) (hash 'gsd (hash 'worktree-isolation #f))))
        (check-false (resolve-worktree-isolation settings))
        (check-false (apply-worktree-isolation-setting! settings))
        (check-false (worktree-isolation-enabled?))))

    (test-case "(a-bis) key absent → shared checkout route (default OFF)"
      (parameterize ([current-gsd-worktree-isolation #f])
        (define settings (q-settings (hash) (hash) (hash)))
        (check-false (resolve-worktree-isolation settings))
        ;; settings unavailable (#f) ⇒ key absent ⇒ default OFF
        (check-false (resolve-worktree-isolation #f))))

    (test-case "(b) key true → isolated route (isolation ON)"
      (parameterize ([current-gsd-worktree-isolation #f])
        (define settings (q-settings (hash) (hash) (hash 'gsd (hash 'worktree-isolation #t))))
        (check-true (resolve-worktree-isolation settings))
        (check-true (apply-worktree-isolation-setting! settings))
        (check-true (worktree-isolation-enabled?))))

    (test-case "(c) explicit #:isolate? #f overrides key true"
      (parameterize ([current-gsd-worktree-isolation #f])
        (define settings (q-settings (hash) (hash) (hash 'gsd (hash 'worktree-isolation #t))))
        (check-false (resolve-worktree-isolation settings #:isolate? #f))
        (check-false (apply-worktree-isolation-setting! settings #:isolate? #f))
        (check-false (worktree-isolation-enabled?))))

    (test-case "(c-bis) explicit #:isolate? #t overrides key false"
      (parameterize ([current-gsd-worktree-isolation #f])
        (define settings (q-settings (hash) (hash) (hash 'gsd (hash 'worktree-isolation #f))))
        (check-true (resolve-worktree-isolation settings #:isolate? #t))
        (check-true (apply-worktree-isolation-setting! settings #:isolate? #t))))

    (test-case "(c-ter) 'auto means honor the settings key"
      (parameterize ([current-gsd-worktree-isolation #f])
        (define settings (q-settings (hash) (hash) (hash 'gsd (hash 'worktree-isolation #t))))
        (check-true (resolve-worktree-isolation settings #:isolate? 'auto))))

    (test-case "(d) banner names active worktree + resolved allowed roots"
      (define wt "/tmp/wt-demo")
      (define roots (list "/tmp/wt-demo" "/tmp/wt-demo/.planning"))
      (define banner (worktree-isolation-banner wt roots))
      (check-true (string-contains? banner "isolation ON")
                  (format "banner must state isolation ON: ~a" banner))
      (check-true (string-contains? banner wt) (format "banner must name active worktree: ~a" banner))
      (check-true (string-contains? banner "/tmp/wt-demo/.planning")
                  (format "banner must enumerate resolved roots: ~a" banner)))))

;; ============================================================
;; v1.00.33 W1 (BUG-0074 canary): strict gsd.tracker live binding
;; normalization + tracker-arming-line start diagnostic, per recovery
;; conclusion c1791527095625.0994 (A)/(B)/(C).
;;
;; Fail-closed: absent key, #f settings, live not exactly #t, or ANY
;; malformed field ⇒ #f (disarmed), never a partial binding. Strict
;; shape: repository "owner/repo", plan-id exactly 64 LOWERCASE hex
;; chars, nonnegative wave, positive issue-number, board-field
;; "Status", board-value "Done", four non-whitespace board IDs
;; (project-item-id project-id field-id option-id). The normalized
;; binding RETAINS repository (the live adapter wiring needs it) plus
;; the tracker-production-wiring.rkt required-binding keys.
;; The one-liner diagnostic is logged only when isolation is effective.
;; ============================================================

;; The campaign's real plan-id: 64 lowercase hex characters.
(define PLAN-ID-64 "79a69b427b40fb6c6b68b4fd1484a74551db182afa9b96f065353324af16b19e")

(define (live-tracker-settings [overrides '()])
  (define base
    (hash 'live
          #t
          'plan-id
          PLAN-ID-64
          'wave
          1
          'issue-number
          9807
          'repository
          "coinerd/q"
          'board-field
          "Status"
          'board-value
          "Done"
          'project-item-id
          "PVTI_item"
          'project-id
          "PVT_proj"
          'field-id
          "PVTF_field"
          'option-id
          "opt-done"))
  (define tracker
    (for/fold ([t base]) ([kv (in-list overrides)])
      (hash-set t (car kv) (cdr kv))))
  (q-settings (hash) (hash) (hash 'gsd (hash 'tracker tracker))))

;; Run proc under a child logger and return (list outcome info-lines),
;; so the gated executor-start diagnostic is observable from tests.
(define (with-arming-logs proc)
  (define logger (make-logger #f (current-logger)))
  (define receiver (make-log-receiver logger 'info))
  (define outcome
    (parameterize ([current-logger logger])
      (proc)))
  (define log-lines
    (let drain ([acc '()])
      (define v (sync/timeout 0.1 receiver))
      (if v
          (drain (cons (format "~a" (vector-ref v 1)) acc))
          (reverse acc))))
  (list outcome log-lines))

(define (apply-quietly! settings #:isolate? (override 'auto))
  (parameterize ([current-gsd-worktree-isolation #f])
    (apply-worktree-isolation-setting! settings #:isolate? override)))

(define tracker-binding-suite
  (test-suite "v1.00.33 W1: gsd.tracker live binding (strict, fail-closed)"

    (test-case "settings #f → #f (nothing could be loaded)"
      (check-false (gsd-tracker-live-binding #f)))

    (test-case "key absent → #f (disarmed)"
      (check-false (gsd-tracker-live-binding (q-settings (hash) (hash) (hash)))))

    (test-case "live not exactly #t → #f (explicit opt-in required)"
      (check-false (gsd-tracker-live-binding (live-tracker-settings '((live . #f)))))
      (check-false (gsd-tracker-live-binding (live-tracker-settings '((live . "true"))))))

    (test-case "valid live binding → normalized hash retaining repository + contract keys"
      (define binding (gsd-tracker-live-binding (live-tracker-settings)))
      (check-true (hash? binding))
      (check-equal? (hash-count binding)
                    10
                    "repository + plan/wave/issue/field/value + four IDs only")
      (check-equal? (hash-ref binding 'repository)
                    "coinerd/q"
                    "repository must be retained for the live adapter wiring")
      (check-equal? (hash-ref binding 'plan-id) PLAN-ID-64)
      (check-equal? (hash-ref binding 'wave) 1)
      (check-equal? (hash-ref binding 'issue-number) 9807)
      (check-equal? (hash-ref binding 'board-field) "Status")
      (check-equal? (hash-ref binding 'board-value) "Done")
      (check-equal? (hash-ref binding 'project-item-id) "PVTI_item")
      (check-equal? (hash-ref binding 'project-id) "PVT_proj")
      (check-equal? (hash-ref binding 'field-id) "PVTF_field")
      (check-equal? (hash-ref binding 'option-id) "opt-done"))

    (test-case "repository must be canonical owner/repo — malformed → #f"
      (check-false (gsd-tracker-live-binding (live-tracker-settings '((repository . #f)))))
      (check-false (gsd-tracker-live-binding (live-tracker-settings '((repository . "coinerd")))))
      (check-false (gsd-tracker-live-binding (live-tracker-settings '((repository . "a/b/c")))))
      (check-false (gsd-tracker-live-binding (live-tracker-settings '((repository .
                                                                                  "owner/../repo")))))
      (check-false (gsd-tracker-live-binding (live-tracker-settings '((repository . 42))))))

    (test-case "plan-id must be exactly 64 lowercase hex chars — otherwise #f"
      (check-false (gsd-tracker-live-binding (live-tracker-settings '((plan-id . "79a69b42")))))
      (check-false (gsd-tracker-live-binding
                    (live-tracker-settings `((plan-id . ,(substring PLAN-ID-64 0 63))))))
      (check-false (gsd-tracker-live-binding
                    (live-tracker-settings `((plan-id . ,(string-append PLAN-ID-64 "a"))))))
      (check-false (gsd-tracker-live-binding
                    (live-tracker-settings `((plan-id . ,(make-string 64 #\g))))))
      (check-false (gsd-tracker-live-binding
                    (live-tracker-settings `((plan-id . ,(make-string 64 #\A)))))))

    (test-case "board IDs must be non-whitespace — whitespace-only or embedded → #f"
      (check-false (gsd-tracker-live-binding (live-tracker-settings '((project-item-id . " ")))))
      (check-false (gsd-tracker-live-binding (live-tracker-settings '((project-id . "   ")))))
      (check-false (gsd-tracker-live-binding (live-tracker-settings '((field-id . "PVTF field")))))
      (check-false (gsd-tracker-live-binding (live-tracker-settings '((option-id . "\topt-done")))))
      (check-false (gsd-tracker-live-binding (live-tracker-settings '((option-id . "")))))
      (check-false (gsd-tracker-live-binding (live-tracker-settings '((project-id . 42))))))

    (test-case "wave/issue must be well-formed integers — otherwise #f"
      (check-false (gsd-tracker-live-binding (live-tracker-settings '((wave . -1)))))
      (check-false (gsd-tracker-live-binding (live-tracker-settings '((wave . "1")))))
      (check-false (gsd-tracker-live-binding (live-tracker-settings '((wave . 1.5)))))
      (check-false (gsd-tracker-live-binding (live-tracker-settings '((issue-number . 0)))))
      (check-false (gsd-tracker-live-binding (live-tracker-settings '((issue-number . -5)))))
      (check-false (gsd-tracker-live-binding (live-tracker-settings '((issue-number . "9807"))))))

    (test-case "board-field/board-value must be the exact Status/Done pair"
      (check-false (gsd-tracker-live-binding (live-tracker-settings '((board-field . "State")))))
      (check-false (gsd-tracker-live-binding (live-tracker-settings '((board-value . "Closed"))))))

    (test-case "tracker-arming-line names the armed binding incl. repository"
      (define line (tracker-arming-line (live-tracker-settings)))
      (check-true (string-contains? line "ARMED") (format "line must state ARMED: ~a" line))
      (check-true (string-contains? line PLAN-ID-64) (format "line must name the plan-id: ~a" line))
      (check-true (string-contains? line "coinerd/q")
                  (format "line must name the repository: ~a" line))
      (check-true (string-contains? line "9807") (format "line must name the issue: ~a" line)))

    (test-case "tracker-arming-line states OFF when absent or malformed (fail-closed)"
      (check-true (string-contains? (tracker-arming-line (q-settings (hash) (hash) (hash))) "OFF"))
      (check-true
       (string-contains? (tracker-arming-line (live-tracker-settings '((plan-id . "short")))) "OFF")))

    (test-case "arming diagnostic is logged ONLY when isolation is effective"
      (define tracker-hash
        (hash 'live
              #t
              'plan-id
              PLAN-ID-64
              'wave
              1
              'issue-number
              9807
              'repository
              "coinerd/q"
              'board-field
              "Status"
              'board-value
              "Done"
              'project-item-id
              "PVTI_item"
              'project-id
              "PVT_proj"
              'field-id
              "PVTF_field"
              'option-id
              "opt-done"))
      (define isolated-settings
        (q-settings (hash) (hash) (hash 'gsd (hash 'worktree-isolation #t 'tracker tracker-hash))))
      (define shared-settings
        (q-settings (hash) (hash) (hash 'gsd (hash 'worktree-isolation #f 'tracker tracker-hash))))
      (define (tracker-lines settings #:isolate? (override 'auto))
        (define result (with-arming-logs (lambda () (apply-quietly! settings #:isolate? override))))
        (filter (lambda (l) (string-contains? l "tracker live binding")) (cadr result)))
      ;; Key #t ⇒ effective ⇒ the arming line is emitted exactly once.
      (define armed (tracker-lines isolated-settings))
      (check-equal? (length armed) 1 "effective isolation ⇒ arming line logged once")
      (check-true (string-contains? (car armed) "ARMED"))
      ;; Key #f ⇒ not effective ⇒ silent.
      (check-equal? (tracker-lines shared-settings)
                    '()
                    "isolation not effective ⇒ arming line NOT logged")
      ;; Explicit #:isolate? #f overrides key #t ⇒ not effective ⇒ silent.
      (check-equal? (tracker-lines isolated-settings #:isolate? #f)
                    '()
                    "override to shared checkout ⇒ arming line NOT logged")
      ;; The gated log must not change the returned flag.
      (check-true (car (with-arming-logs (lambda () (apply-quietly! isolated-settings))))))))

(define all-suites
  (test-suite "wave executor isolation"
    exactly-once-suite
    timeout-suite
    pending-cancel-suite
    compat-suite
    settings-wiring-suite
    tracker-binding-suite))

(exit (if (zero? (run-tests all-suites)) 0 1))
