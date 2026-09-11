#lang racket/base
;; @covers extensions/gsd/delivery-verifier.rkt
;; @speed slow
;; @suite extensions
;; @boundary e2e
;; @timeout 180  ;; per-owner cap: measured cold median ~25s (v1.00.29 W2 measurement), includes 2s-sleep attach test
;;
;; v1.00.29 W2 delivery-verifier boundary extraction — OWNER 3 of 3.
;;
;; E2E owner: the delivery verifier's EXECUTION PLANE and coordinator
;; composition — the claims that span more than the verifier's decision
;; function and more than the raw Git boundary:
;;   - make-delivery-verifier returns a working verifier callback
;;   - the coordinator (run-campaign-wave) maps approve/reject to
;;     wave-done / wave-failed campaign states
;;   - the owned-singleton verify lane: duplicate gate calls attach to the
;;     running registry job instead of launching twice
;;   - deadline + nonzero-failure truthfulness of the verify subprocess
;;     (exit 124 / state=timed-out, state=failed)
;;
;; Decision-logic claims over synthetic Git facts live in the decision owner
;; (tests/test-gsd-delivery-verifier-decision.rkt); required real-Git
;; fail-closed boundary canaries live in the contract owner
;; (tests/test-gsd-delivery-verifier-git-contract.rkt). Ownership map:
;; docs/reports/DELIVERY-VERIFIER-SPLIT-v1.00.29.md.
;;
;; Timing provenance: the stale whole-file `@timeout 300` / `~123s` note from
;; v1.00.28 (a2a10b9d) is superseded by per-owner caps derived from W0/W2
;; repeatable measurements. No global timeout increase.
;;
;; tests/test-gsd-delivery-verifier-e2e.rkt — e2e execution-plane + coordinator
;; composition suite for the /go delivery verifier.

(require rackunit
         rackunit/text-ui
         racket/string
         (only-in "helpers/private-fixture-templates.rkt" call-with-private-git-environment)
         (only-in "helpers/delivery-fixtures.rkt"
                  make-tmp-git-repo
                  write-wave-doc!
                  write-state!
                  load-plan*
                  make-git-branch!
                  make-git-file-change!
                  write-plan!
                  setup-standard-campaign!
                  cleanup-tmp)
         (only-in "../extensions/gsd/delivery-verifier.rkt"
                  run-delivery-verification
                  make-delivery-verifier
                  delivery-verification?
                  delivery-verification-approved?
                  delivery-verification-message
                  current-gsd-delivery-verify-timeout-sec)
         (only-in "../extensions/gsd/composition-root.rkt" current-gsd-verification-registry)
         (only-in "../extensions/gsd/verification-job.rkt"
                  make-verification-registry
                  registry-active-count)
         (only-in "../extensions/gsd/campaign-state.rkt" migrate-campaign!)
         (only-in "../extensions/gsd/go-orchestrator.rkt" run-campaign-wave campaign-result-status))

;; ============================================================
;; Tests (e2e execution plane + coordinator composition)
;; ============================================================

(define (e2e-suite)
  (test-suite "delivery-verifier e2e (execution plane + coordinator)"

    (test-case "make-delivery-verifier returns a working verifier callback"
      (define base (setup-standard-campaign!))
      (define plan (load-plan* base))
      (define verifier (make-delivery-verifier base plan))
      (define result (verifier 0))
      (check-true (delivery-verification? result))
      (check-true (delivery-verification-approved? result))
      (cleanup-tmp base))

    (test-case "coordinator marks wave done when structured verifier approves"
      (define base (setup-standard-campaign!))
      (define plan (load-plan* base))
      (define rec (migrate-campaign! base))
      (define result
        (run-campaign-wave base
                           rec
                           0
                           #:runner (lambda (_) 'ok)
                           #:verifier (make-delivery-verifier base plan)))
      (check-eq? (campaign-result-status result) 'wave-done)
      (cleanup-tmp base))

    (test-case "coordinator marks wave failed when structured verifier rejects"
      (define base (make-tmp-git-repo))
      (make-git-branch! base "feature/issue-99-wave") ; wrong branch
      (make-git-file-change! base)
      (write-plan! base 0 "Wave Zero" "zero")
      (write-wave-doc! base
                       0
                       "zero"
                       '("q/ui-core/preferences.rkt")
                       "raco make q/ui-core/preferences.rkt")
      (write-state! base 0 "42")
      (define plan (load-plan* base))
      (define rec (migrate-campaign! base))
      (define result
        (run-campaign-wave base
                           rec
                           0
                           #:runner (lambda (_) 'ok)
                           #:verifier (make-delivery-verifier base plan)))
      (check-eq? (campaign-result-status result) 'wave-failed)
      (cleanup-tmp base))

    (test-case "verify executes through the bound registry: duplicates attach, never launch twice"
      ;; The owned-singleton lane: while one declared verify is running, a
      ;; duplicate verifier call for the same wave+command+checkout attaches
      ;; to the SAME job instead of launching a second gate.
      (define base (setup-standard-campaign!))
      (define reg (make-verification-registry))
      (write-wave-doc! base 0 "zero" '("q/ui-core/preferences.rkt") "sleep 2; exit 0")
      (define plan (load-plan* base))
      (define first-result (box #f))
      (parameterize ([current-gsd-verification-registry reg])
        (define t
          (thread (lambda () (set-box! first-result (run-delivery-verification base plan 0)))))
        ;; Git evidence checks precede launch and can exceed a fixed 300ms on
        ;; loaded CI hosts. Poll for the owned start under a hard 5s bound.
        (let wait-for-owned-start ([remaining 250])
          (when (and (zero? (registry-active-count reg)) (not (thread-dead? t)) (> remaining 0))
            (sleep 0.02)
            (wait-for-owned-start (sub1 remaining))))
        (check-equal? (registry-active-count reg)
                      1
                      "declared verify runs as ONE owned job in the bound registry")
        ;; duplicate verifier call while the first verify is still running:
        ;; attaches to the running singleton — no second process launch
        (define second (run-delivery-verification base plan 0))
        (sync t)
        (check-equal? (registry-active-count reg)
                      0
                      "job is terminal after both callers' waits returned")
        (check-true (delivery-verification-approved? second) (delivery-verification-message second))
        (check-true (delivery-verification-approved? (unbox first-result))
                    "both attached callers observe the same approved job"))
      (cleanup-tmp base))

    (test-case "timed-out verify is a failure with truthful state and exit 124"
      ;; BUG-0057 class fix: a deadline-killed gate can never approve; the
      ;; verdict carries the attributable terminal state and exit 124.
      (define base (setup-standard-campaign!))
      (write-wave-doc! base 0 "zero" '("q/ui-core/preferences.rkt") "sleep 30; exit 0")
      (define plan (load-plan* base))
      (define result
        (parameterize ([current-gsd-verification-registry (make-verification-registry)]
                       [current-gsd-delivery-verify-timeout-sec 1])
          (run-delivery-verification base plan 0)))
      (check-false (delivery-verification-approved? result) "a timed-out gate must never approve")
      (define msg (delivery-verification-message result))
      (check-true (string-contains? msg "exit=124") msg)
      (check-true (string-contains? msg "state=timed-out") msg)
      (check-true (string-contains? msg "log=") msg)
      (cleanup-tmp base))

    (test-case "declared verify failing nonzero is a failure with attributable state"
      (define base (setup-standard-campaign!))
      (write-wave-doc! base 0 "zero" '("q/ui-core/preferences.rkt") "echo boom >&2; exit 3")
      (define plan (load-plan* base))
      (define result
        (parameterize ([current-gsd-verification-registry (make-verification-registry)])
          (run-delivery-verification base plan 0)))
      (check-false (delivery-verification-approved? result)
                   "a nonzero declared verify must fail delivery")
      (define msg (delivery-verification-message result))
      (check-true (string-contains? msg "exit=3") msg)
      (check-true (string-contains? msg "state=failed") msg)
      (check-true (string-contains? msg "log=") msg)
      (cleanup-tmp base))))

(module+ main
  (exit (call-with-private-git-environment (lambda () (run-tests (e2e-suite))))))
