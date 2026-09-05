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
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (define result
        (run-campaign-wave dir
                           rec
                           0
                           #:runner (make-wave-runner-port
                                     (lambda (idx) (wave-execution-outcome 'done "wave finished")))
                           #:verifier (lambda (_) #t)))
      (check-eq? (campaign-result-status result) 'wave-done)
      (check-eq? (wave-status* rec 0) 'done)
      (check-equal? (count-completion-events dir rec) 1 "exactly one completion event per done wave")
      (cleanup-tmp dir))

    (test-case "re-execution of a done attempt is stale-ignored (no duplicate)"
      (define dir (make-tmp-campaign-dir 2))
      (define rec (load-or-migrate dir))
      (define runner (make-wave-runner-port (lambda (idx) (wave-execution-outcome 'done "ok"))))
      (define first (run-campaign-wave dir rec 0 #:runner runner #:verifier (lambda (_) #t)))
      (check-eq? (campaign-result-status first) 'wave-done)
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
      ;; v1.00.24 follow-up regression: run-campaign-wave without
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
      (check-eq? (campaign-result-status result) 'wave-done)
      (check-eq? (wave-status* rec 0) 'done)
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
                  worktree-isolation-banner)
         (only-in "../runtime/settings-core.rkt" q-settings))

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

(define all-suites
  (test-suite "wave executor isolation"
    exactly-once-suite
    timeout-suite
    pending-cancel-suite
    compat-suite
    settings-wiring-suite))

(exit (if (zero? (run-tests all-suites)) 0 1))
