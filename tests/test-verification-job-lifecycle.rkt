#lang racket/base

;; tests/test-verification-job-lifecycle.rkt — BUG-0053 / WP3.4
;;
;; Owned singleton verification jobs: one durable owner per verification
;; identity (campaign, wave, suite, checkout, profile), no duplicate
;; launches, attributable terminal states, stale-PID-safe reconciliation.
;;
;; TDD contract (wave W3 WP3.4):
;;   1. starting the same identity twice reuses the active job (no 2nd launch);
;;   2. replacement requires explicit cancel-and-reap first;
;;   3. ownership survives model turns (registry persists) but campaign
;;      cancellation cancels+reaps all jobs for that campaign;
;;   4. completed / failed / timed-out / cancelled / orphan-recovered are
;;      distinct terminal states (exit 124 => timed-out);
;;   5. status reads are structured records (no pgrep heuristics);
;;   6. a stale PID record cannot reclaim ownership without process
;;      identity/start-time verification (reconcile marks orphan-recovered).

(require rackunit
         racket/string
         (prefix-in vj: "../extensions/gsd/verification-job.rkt"))

;; ---------------------------------------------------------------- identity

(test-case "identity key covers campaign, wave, suite, checkout, profile"
  (define a (vj:verification-identity "camp-1" "W3" "fast" "/repo" "local"))
  (define b (vj:verification-identity "camp-1" "W3" "fast" "/repo" "local"))
  (define c (vj:verification-identity "camp-1" "W3" "broad" "/repo" "local"))
  (check-equal? (vj:verification-identity-key a) (vj:verification-identity-key b))
  (check-not-equal? (vj:verification-identity-key a) (vj:verification-identity-key c)))

;; ------------------------------------------------------------ singleton

(test-case "F/WP3.4-1: same identity twice reuses the active job (no duplicate launch)"
  (define reg (vj:make-verification-registry))
  (define ident (vj:verification-identity "camp-A" "W1" "fast" "/repo" "local"))
  (define r1 (vj:verification-start! reg ident "/bin/sh" '("-c" "sleep 2; exit 0")))
  (check-true (vj:start-result-started? r1) "first start must launch")
  (define r2 (vj:verification-start! reg ident "/bin/sh" '("-c" "sleep 2; exit 0")))
  (check-false (vj:start-result-started? r2) "second start must NOT launch a new process")
  (check-equal? (vj:start-result-job-id r2) (vj:start-result-job-id r1)
                "second start reuses the active job id")
  (check-equal? (vj:registry-active-count reg) 1)
  ;; reap for cleanup
  (vj:verification-cancel! reg (vj:start-result-job-id r1)))

(test-case "F/WP3.4-2: replacement requires explicit cancel-and-reap of prior job"
  (define reg (vj:make-verification-registry))
  (define ident (vj:verification-identity "camp-B" "W1" "fast" "/repo" "local"))
  (define r1 (vj:verification-start! reg ident "/bin/sh" '("-c" "sleep 2; exit 0")))
  (check-true (vj:start-result-started? r1))
  (define r2 (vj:verification-start! reg ident "/bin/sh" '("-c" "sleep 2; exit 0")))
  (check-equal? (vj:start-result-job-id r2) (vj:start-result-job-id r1))
  ;; cancel + reap the prior job, then a genuinely new start is permitted
  (define cancelled (vj:verification-cancel! reg (vj:start-result-job-id r1)))
  (check-equal? (vj:verification-job-state cancelled) 'cancelled)
  (check-true (vj:verification-job-reaped? cancelled) "cancel must reap the process")
  (define r3 (vj:verification-start! reg ident "/bin/sh" '("-c" "sleep 0.1; exit 0")))
  (check-true (vj:start-result-started? r3) "new start after cancel/reap launches fresh")
  (check-not-equal? (vj:start-result-job-id r3) (vj:start-result-job-id r1))
  (vj:verification-cancel! reg (vj:start-result-job-id r3)))

;; ------------------------------------------------- campaign cancellation

(test-case "F/WP3.4-3: campaign cancel reaps every owned job for that campaign"
  (define reg (vj:make-verification-registry))
  (define r1 (vj:verification-start! reg (vj:verification-identity "camp-C" "W1" "fast" "/repo" "local")
                                     "/bin/sh" '("-c" "sleep 5; exit 0")))
  (define r2 (vj:verification-start! reg (vj:verification-identity "camp-C" "W2" "broad" "/repo" "local")
                                     "/bin/sh" '("-c" "sleep 5; exit 0")))
  (define r3 (vj:verification-start! reg (vj:verification-identity "camp-D" "W1" "fast" "/repo" "local")
                                     "/bin/sh" '("-c" "sleep 5; exit 0")))
  (check-equal? (vj:registry-active-count reg) 3)
  (define reaped (vj:verification-cancel-campaign! reg "camp-C"))
  (check-equal? (length reaped) 2 "both camp-C jobs cancelled")
  (check-equal? (vj:registry-active-count reg) 1 "camp-D untouched")
  (for ([job reaped])
    (check-equal? (vj:verification-job-state job) 'cancelled))
  (vj:verification-cancel! reg (vj:start-result-job-id r3)))

;; ------------------------------------------------------- terminal states

(test-case "F/WP3.4-4a: success terminates as completed"
  (define reg (vj:make-verification-registry))
  (define r (vj:verification-start! reg (vj:verification-identity "camp-E" "W1" "fast" "/repo" "local")
                                    "/bin/sh" '("-c" "exit 0")))
  (define job (vj:verification-wait reg (vj:start-result-job-id r) 5000))
  (check-equal? (vj:verification-job-state job) 'completed)
  (check-equal? (vj:verification-job-exit-code job) 0))

(test-case "F/WP3.4-4b: nonzero exit terminates as failed with truthful code"
  (define reg (vj:make-verification-registry))
  (define r (vj:verification-start! reg (vj:verification-identity "camp-F" "W1" "fast" "/repo" "local")
                                    "/bin/sh" '("-c" "echo fake-status; exit 3")))
  (define job (vj:verification-wait reg (vj:start-result-job-id r) 5000))
  (check-equal? (vj:verification-job-state job) 'failed)
  (check-equal? (vj:verification-job-exit-code job) 3)
  (check-true (string-contains? (vj:verification-job-stdout job) "fake-status")
              "wrapper stdout is captured but printed status is not authoritative"))

(test-case "F/WP3.4-4c: exit 124 propagates as timed-out failure"
  (define reg (vj:make-verification-registry))
  (define r (vj:verification-start! reg (vj:verification-identity "camp-G" "W1" "fast" "/repo" "local")
                                    "/bin/sh" '("-c" "sleep 30; exit 0") #:timeout-ms 700))
  (define job (vj:verification-wait reg (vj:start-result-job-id r) 10000))
  (check-equal? (vj:verification-job-state job) 'timed-out)
  (check-equal? (vj:verification-job-exit-code job) 124)
  (check-true (vj:verification-job-reaped? job) "timeout must reap the process group"))

;; ---------------------------------------------------- structured status

(test-case "F/WP3.4-5: status is a structured record (id/pid/start/digest/log), no pgrep"
  (define reg (vj:make-verification-registry))
  (define r (vj:verification-start! reg (vj:verification-identity "camp-H" "W1" "fast" "/repo" "local")
                                    "/bin/sh" '("-c" "sleep 2; exit 0")
                                    #:log-path "/tmp/vj-test-H.log"))
  (define job (vj:verification-status reg (vj:start-result-job-id r)))
  (check-true (vj:verification-job? job))
  (check-equal? (vj:verification-job-identity-key job)
                (vj:verification-identity-key (vj:verification-identity "camp-H" "W1" "fast" "/repo" "local")))
  (check-true (real? (vj:verification-job-pid job)) "records live pid")
  (check-true (real? (vj:verification-job-start-ms job)) "records start time for reuse guard")
  (check-true (string? (vj:verification-job-command-digest job))
              "records command digest (sha256) for identity")
  (check-equal? (vj:verification-job-log-path job) "/tmp/vj-test-H.log")
  (check-equal? (vj:verification-job-state job) 'running)
  (vj:verification-cancel! reg (vj:start-result-job-id r)))

;; ------------------------------------------------- stale-PID reconciliation

(test-case "F/WP3.4-6: stale record (dead pid) reconciles to orphan-recovered, releases ownership"
  (define reg (vj:make-verification-registry))
  (define r (vj:verification-start! reg (vj:verification-identity "camp-I" "W1" "fast" "/repo" "local")
                                    "/bin/sh" '("-c" "sleep 2; exit 0")))
  (define id (vj:start-result-job-id r))
  ;; Simulate process death WITHOUT going through the registry (lost turn/crash):
  ;; forcibly kill the real child behind the registry's back.
  (define job (vj:verification-status reg id))
  (vj:simulate-lost-process! reg id) ; kills child out-of-band for the test
  ;; Reconciliation models a resume/restart after the loss, which is always
  ;; (far) later than the kill: a SIGKILL against a child still inside its
  ;; fork->exec window is observable as a live incarnation for a few ms.
  (sleep 0.3)
  (define rec (vj:verification-reconcile! reg id))
  (check-equal? (vj:verification-job-state rec) 'orphan-recovered
                "dead pid with live record must be reconciled, not left running")
  (check-true (vj:verification-job-reaped? rec))
  ;; ownership released: a fresh start for the same identity is now allowed
  (define r2 (vj:verification-start! reg (vj:verification-identity "camp-I" "W1" "fast" "/repo" "local")
                                     "/bin/sh" '("-c" "sleep 0.1; exit 0")))
  (check-true (vj:start-result-started? r2) "identity is free after orphan-recovery")
  (vj:verification-cancel! reg (vj:start-result-job-id r2)))

(test-case "F/WP3.4-6b: reconcile of a LIVE record keeps ownership (no false orphan)"
  (define reg (vj:make-verification-registry))
  (define r (vj:verification-start! reg (vj:verification-identity "camp-J" "W1" "fast" "/repo" "local")
                                    "/bin/sh" '("-c" "sleep 2; exit 0")))
  (define rec (vj:verification-reconcile! reg (vj:start-result-job-id r)))
  (check-equal? (vj:verification-job-state rec) 'running
                "live process must keep its ownership across reconcile")
  (vj:verification-cancel! reg (vj:start-result-job-id r)))
