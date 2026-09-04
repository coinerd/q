#lang racket/base
;; @covers extensions/gsd/campaign-state.rkt

;; @speed fast  ;; @suite extensions
;; @boundary integration

;; tests/test-gsd-campaign-state.rkt — W0: Durable campaign state,
;; identity, and reconstruction.
;;
;; TDD red tests for:
;;   1. Restart reconstruction selects the first unfinished wave.
;;   2. Stable plan identity under status-only changes; substantive
;;      wave/constraint/document edits change the manifest hash and
;;      pause the campaign (PLAN-CHANGED).
;;   3. Conflicting migration sources (PLAN vs STATE) fail closed.
;;   4. One-active-wave invariant.
;;   5. Exact canonical status mapping across all selectors.
;;   6. Durable cancellation/fencing fields persist across restart.

(require rackunit
         rackunit/text-ui
         racket/file
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         (only-in "../extensions/gsd/wave-executor.rkt"
                  wave-executor-statuses
                  wave-status-state
                  next-pending-wave
                  make-wave-executor-from-campaign))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-test-manifest n #:content-hashes [hashes #f])
  (define hs
    (or hashes
        (for/list ([i (in-range n)])
          (format "hash-~a" i))))
  (make-campaign-manifest 1
                          "Test Campaign"
                          '()
                          (for/list ([i (in-range n)])
                            (make-campaign-wave-descriptor i
                                                           (format "Wave ~a" i)
                                                           (format "waves/W~a-test.md" i)
                                                           (list-ref hs i)))
                          "constraints-hash"))

(define (make-test-record n)
  (define m (make-test-manifest n))
  (make-campaign-record (campaign-manifest-hash m)
                        m
                        (for/list ([i (in-range n)])
                          (make-campaign-wave i (format "Wave ~a" i) 'pending 0 #f))
                        #f
                        0
                        #f
                        (current-seconds)
                        (current-seconds)))

;; ============================================================
;; 1. Canonical status mapping (D4)
;; ============================================================

(define status-mapping-suite
  (test-suite "canonical status mapping"
    (test-case "exact canonical mapping for every selector"
      (check-eq? (canonical-wave-status "Inbox") 'pending)
      (check-eq? (canonical-wave-status "PENDING") 'pending)
      (check-eq? (canonical-wave-status "In-Progress") 'in-progress)
      (check-eq? (canonical-wave-status "VERIFYING") 'verifying)
      (check-eq? (canonical-wave-status "DONE") 'done)
      (check-eq? (canonical-wave-status "FAILED") 'failed)
      (check-eq? (canonical-wave-status "INTERRUPTED") 'interrupted)
      (check-eq? (canonical-wave-status "DEFERRED") 'deferred)
      (check-false (member 'rework canonical-wave-statuses) "no REWORK status exists")
      (check-equal? (length canonical-wave-statuses) 7 "exactly the seven canonical statuses"))

    (test-case "FAILED never counts as successful completion"
      (check-true (completed-status? 'done))
      (check-false (completed-status? 'failed))
      (check-false (completed-status? 'interrupted))
      (check-false (completed-status? 'deferred))
      (check-true (retryable-status? 'failed))
      (check-true (retryable-status? 'interrupted))
      (check-false (retryable-status? 'done)))

    (test-case "actionable selector matches required matrix"
      (check-true (actionable-status? 'pending))
      (check-true (actionable-status? 'in-progress))
      (check-true (actionable-status? 'verifying))
      (check-true (actionable-status? 'failed))
      (check-true (actionable-status? 'interrupted))
      (check-false (actionable-status? 'done))
      (check-false (actionable-status? 'deferred)))))

;; ============================================================
;; 2. Restart reconstruction selects first unfinished wave
;; ============================================================

(define reconstruction-suite
  (test-suite "restart reconstruction"
    (test-case "DONE W0 + PENDING W1 selects W1 (never restarts W0)"
      (define rec (make-test-record 2))
      (set-campaign-wave-status! (list-ref (campaign-record-waves rec) 0) 'done)
      (check-equal? (select-next-actionable-wave rec) 1)
      (check-false (restart-needed? rec 0)))

    (test-case "DEFERRED W0 + PENDING W1 selects W1"
      (define rec (make-test-record 2))
      (set-campaign-wave-status! (list-ref (campaign-record-waves rec) 0) 'deferred)
      (check-equal? (select-next-actionable-wave rec) 1))

    (test-case "FAILED W0 + PENDING W1 retries W0 and blocks W1"
      (define rec (make-test-record 2))
      (set-campaign-wave-status! (list-ref (campaign-record-waves rec) 0) 'failed)
      (check-equal? (select-next-actionable-wave rec) 0))

    (test-case "abandoned IN-PROGRESS W0 reruns W0"
      (define rec (make-test-record 2))
      (set-campaign-wave-status! (list-ref (campaign-record-waves rec) 0) 'in-progress)
      (check-equal? (select-next-actionable-wave rec) 0))

    (test-case "abandoned VERIFYING W0 still selects W0"
      (define rec (make-test-record 2))
      (set-campaign-wave-status! (list-ref (campaign-record-waves rec) 0) 'verifying)
      (check-equal? (select-next-actionable-wave rec) 0))

    (test-case "all DONE/DEFERRED selects nothing"
      (define rec (make-test-record 2))
      (set-campaign-wave-status! (list-ref (campaign-record-waves rec) 0) 'done)
      (set-campaign-wave-status! (list-ref (campaign-record-waves rec) 1) 'deferred)
      (check-false (select-next-actionable-wave rec)))

    (test-case "executor reconstruction from durable record (GC-2)"
      (define rec (make-test-record 2))
      (set-campaign-wave-status! (list-ref (campaign-record-waves rec) 0) 'done)
      (define exec (make-wave-executor-from-campaign rec))
      (check-eq? (wave-status-state (list-ref (wave-executor-statuses exec) 0)) 'completed)
      (check-eq? (wave-status-state (list-ref (wave-executor-statuses exec) 1)) 'pending)
      (check-equal? (next-pending-wave exec) 1 "restart never re-runs durably DONE W0"))))

;; ============================================================
;; 3. Stable plan identity (D2 / GC-14)
;; ============================================================

(define identity-suite
  (test-suite "stable plan identity"
    (test-case "status-only projection changes preserve plan-id"
      (define m (make-test-manifest 2))
      (define id1 (campaign-manifest-hash m))
      ;; Statuses live in the record, not the manifest — manifest unchanged.
      (define rec (make-campaign-record id1 m '() #f 0 #f 0 0))
      (check-equal? (campaign-plan-id rec) id1)
      (check-false (plan-changed? rec m) "status-only change is not a substantive plan change"))

    (test-case "substantive wave content edit changes manifest hash and pauses"
      (define m1 (make-test-manifest 2))
      (define rec (make-campaign-record (campaign-manifest-hash m1) m1 '() #f 0 #f 0 0))
      ;; New manifest with changed content hash for W0 (substantive edit).
      (define m2 (make-test-manifest 2 #:content-hashes '("hash-0-EDITED" "hash-1")))
      (check-not-equal? (campaign-manifest-hash m2)
                        (campaign-manifest-hash m1)
                        "substantive edit changes the manifest hash")
      (check-true (plan-changed? rec m2)
                  "campaign pauses PLAN-CHANGED when the on-disk manifest diverges"))

    (test-case "W5: status-header rewrite of wave docs preserves plan-id"
      ;; v0.99.90 W5 (#9236): the manifest hash (plan-id) must be stable
      ;; across projection updates. Wave docs carry a mutable "Status:"
      ;; header that completion/failure projections rewrite; hashing the raw
      ;; file would change the plan-id after every wave, so
      ;; load-or-migrate-campaign! would re-migrate and orphan the durable
      ;; record + outbox (Campaign Truth lost on restart).
      (define dir (make-temporary-file "campaign-identity-~a" 'directory))
      (make-directory (build-path dir ".planning"))
      (make-directory (build-path dir ".planning" "waves"))
      (call-with-output-file (build-path dir ".planning" "PLAN.md")
                             (lambda (out)
                               (write-string "# Plan: Identity\n\n## Waves\n" out)
                               (write-string "- [Inbox] W0: Zero → waves/W0-zero.md\n" out))
                             #:exists 'truncate)
      (call-with-output-file (build-path dir ".planning" "STATE.md")
                             (lambda (out)
                               (write-string "| Wave | Title | Status |\n|---|---|---|\n" out)
                               (write-string "| W0 | Zero | PENDING |\n" out))
                             #:exists 'truncate)
      (call-with-output-file (build-path dir ".planning" "waves" "W0-zero.md")
                             (lambda (out)
                               (write-string "# Wave 0\nStatus: Inbox\n\nSame immutable body.\n" out))
                             #:exists 'truncate)
      (define rec (migrate-campaign! dir))
      (define id-before (campaign-plan-id rec))
      ;; Simulate a completion projection: rewrite the wave doc Status header
      ;; (body unchanged) and the PLAN.md marker.
      (call-with-output-file (build-path dir ".planning" "waves" "W0-zero.md")
                             (lambda (out)
                               (write-string "# Wave 0\nStatus: DONE\n\nSame immutable body.\n" out))
                             #:exists 'truncate)
      (call-with-output-file (build-path dir ".planning" "PLAN.md")
                             (lambda (out)
                               (write-string "# Plan: Identity\n\n## Waves\n" out)
                               (write-string "- [DONE] W0: Zero → waves/W0-zero.md\n" out))
                             #:exists 'truncate)
      (call-with-output-file (build-path dir ".planning" "STATE.md")
                             (lambda (out)
                               (write-string "| Wave | Title | Status |\n|---|---|---|\n" out)
                               (write-string "| W0 | Zero | DONE |\n" out))
                             #:exists 'truncate)
      (define rec2 (migrate-campaign! dir))
      (check-equal? (campaign-plan-id rec2) id-before "status-header rewrite preserves plan-id"))

    (test-case "global constraints hash participates in identity"
      (define m1
        (make-campaign-manifest 1
                                "T"
                                '()
                                (list (make-campaign-wave-descriptor 0 "W" "w.md" "h"))
                                "C1"))
      (define m2
        (make-campaign-manifest 1
                                "T"
                                '()
                                (list (make-campaign-wave-descriptor 0 "W" "w.md" "h"))
                                "C2"))
      (check-not-equal? (campaign-manifest-hash m1) (campaign-manifest-hash m2)))))

;; ============================================================
;; 4. One-active-wave invariant
;; ============================================================

(define invariant-suite
  (test-suite "one-active-wave invariant"
    (test-case "at most one wave IN-PROGRESS or VERIFYING"
      (define rec (make-test-record 3))
      (set-campaign-wave-status! (list-ref (campaign-record-waves rec) 0) 'in-progress)
      (check-equal? (one-active-wave-violation rec) '())
      (set-campaign-wave-status! (list-ref (campaign-record-waves rec) 1) 'verifying)
      (check-equal? (length (one-active-wave-violation rec)) 2))

    (test-case "begin-attempt! rejects when another wave is active"
      (define rec (make-test-record 2))
      (set-campaign-wave-status! (list-ref (campaign-record-waves rec) 0) 'in-progress)
      (check-exn exn:fail?
                 (lambda () (begin-attempt! rec 1 1))
                 "cannot start W1 while W0 is active"))))

;; ============================================================
;; 5. Migration truth (D3)
;; ============================================================

(define migration-suite
  (test-suite "initial migration truth"
    (test-case "PLAN.md and STATE.md both present and agreeing seed record"
      (define dir (make-temporary-file "campaign-migrate-~a" 'directory))
      (make-directory (build-path dir ".planning"))
      (make-directory (build-path dir ".planning" "waves"))
      (call-with-output-file (build-path dir ".planning" "PLAN.md")
                             (lambda (out)
                               (write-string "# Plan: Agree\n\n## Waves\n" out)
                               (write-string "- [Inbox] W0: Zero → waves/W0-zero.md\n" out)
                               (write-string "- [DONE] W1: One → waves/W1-one.md\n" out))
                             #:exists 'truncate)
      ;; Referenced wave docs must exist (BUG-0052 hard failure otherwise).
      (for ([name (list "W0-zero.md" "W1-one.md")])
        (call-with-output-file (build-path dir ".planning" "waves" name)
                               (lambda (out) (fprintf out "# ~a\nStatus: Inbox\n\nBody.\n" name))
                               #:exists 'truncate))
      (call-with-output-file (build-path dir ".planning" "STATE.md")
                             (lambda (out)
                               (write-string "| Wave | Title | Status |\n|---|---|---|\n" out)
                               (write-string "| W0 | Zero | NOT STARTED |\n" out)
                               (write-string "| W1 | One | DONE |\n" out))
                             #:exists 'truncate)
      (define rec (migrate-campaign! dir))
      (check-eq? (campaign-record-provenance rec) 'plan-and-state)
      (check-eq? (campaign-wave-status (list-ref (campaign-record-waves rec) 0)) 'pending)
      (check-eq? (campaign-wave-status (list-ref (campaign-record-waves rec) 1)) 'done)
      (delete-directory/files dir #:must-exist? #f))

    (test-case "PLAN.md and STATE.md disagreeing fail closed"
      (define dir (make-temporary-file "campaign-migrate-conflict-~a" 'directory))
      (make-directory (build-path dir ".planning"))
      (make-directory (build-path dir ".planning" "waves"))
      (call-with-output-file (build-path dir ".planning" "PLAN.md")
                             (lambda (out)
                               (write-string "# Plan: Conflict\n\n## Waves\n" out)
                               (write-string "- [DONE] W0: Zero → waves/W0-zero.md\n" out))
                             #:exists 'truncate)
      (call-with-output-file (build-path dir ".planning" "STATE.md")
                             (lambda (out)
                               (write-string "| Wave | Title | Status |\n|---|---|---|\n" out)
                               (write-string "| W0 | Zero | NOT STARTED |\n" out))
                             #:exists 'truncate)
      (check-exn exn:fail:campaign-migration?
                 (lambda () (migrate-campaign! dir))
                 "PLAN/STATE conflict fails closed")
      (delete-directory/files dir #:must-exist? #f))

    (test-case "F-6: new campaign (different wave titles) auto-resolves from PLAN.md"
      (define dir (make-temporary-file "campaign-migrate-newcamp-~a" 'directory))
      (make-directory (build-path dir ".planning"))
      (make-directory (build-path dir ".planning" "waves"))
      ;; New PLAN.md has 3 waves with titles Alpha, Beta, Gamma
      (call-with-output-file (build-path dir ".planning" "PLAN.md")
                             (lambda (out)
                               (write-string "# Plan: New Campaign\n\n## Waves\n" out)
                               (write-string "- [Inbox] W0: Alpha → waves/W0-alpha.md\n" out)
                               (write-string "- [Inbox] W1: Beta → waves/W1-beta.md\n" out)
                               (write-string "- [Inbox] W2: Gamma → waves/W2-gamma.md\n" out))
                             #:exists 'truncate)
      ;; Referenced wave docs must exist: missing docs refuse campaign creation (BUG-0052).
      (for ([name (list "W0-alpha.md" "W1-beta.md" "W2-gamma.md")])
        (call-with-output-file (build-path dir ".planning" "waves" name)
                               (lambda (out) (fprintf out "# ~a\nStatus: Inbox\n\nBody.\n" name))
                               #:exists 'truncate))
      ;; Old STATE.md has 5 waves with different titles (stale campaign)
      (call-with-output-file (build-path dir ".planning" "STATE.md")
                             (lambda (out)
                               (write-string "| Wave | Title | Status |\n|---|---|---|\n" out)
                               (write-string "| W0 | OldZero | DONE |\n" out)
                               (write-string "| W1 | OldOne | DONE |\n" out)
                               (write-string "| W2 | OldTwo | FAILED |\n" out)
                               (write-string "| W3 | OldThree | PENDING |\n" out)
                               (write-string "| W4 | OldFour | PENDING |\n" out))
                             #:exists 'truncate)
      ;; Should NOT fail-closed; should auto-resolve from PLAN.md
      (define rec (migrate-campaign! dir))
      (check-eq? (campaign-record-provenance rec) 'plan-and-state)
      (check-equal? (length (campaign-record-waves rec)) 3 "3 waves from new PLAN.md")
      (check-equal? (campaign-wave-title (list-ref (campaign-record-waves rec) 0))
                    "Alpha"
                    "wave 0 title from PLAN.md")
      (check-equal? (campaign-wave-title (list-ref (campaign-record-waves rec) 2))
                    "Gamma"
                    "wave 2 title from PLAN.md")
      (check-eq? (campaign-wave-status (list-ref (campaign-record-waves rec) 0))
                 'pending
                 "all waves pending in new campaign")
      (delete-directory/files dir #:must-exist? #f))

    (test-case "F-6: new campaign (different wave count, same titles) auto-resolves"
      (define dir (make-temporary-file "campaign-migrate-newcount-~a" 'directory))
      (make-directory (build-path dir ".planning"))
      (make-directory (build-path dir ".planning" "waves"))
      ;; New PLAN.md has 2 waves
      (call-with-output-file (build-path dir ".planning" "PLAN.md")
                             (lambda (out)
                               (write-string "# Plan: Fewer Waves\n\n## Waves\n" out)
                               (write-string "- [Inbox] W0: Zero → waves/W0-zero.md\n" out)
                               (write-string "- [Inbox] W1: One → waves/W1-one.md\n" out))
                             #:exists 'truncate)
      ;; Referenced wave docs must exist (BUG-0052 hard failure otherwise).
      (for ([name (list "W0-zero.md" "W1-one.md")])
        (call-with-output-file (build-path dir ".planning" "waves" name)
                               (lambda (out) (fprintf out "# ~a\nStatus: Inbox\n\nBody.\n" name))
                               #:exists 'truncate))
      ;; Old STATE.md has 3 waves (different count)
      (call-with-output-file (build-path dir ".planning" "STATE.md")
                             (lambda (out)
                               (write-string "| Wave | Title | Status |\n|---|---|---|\n" out)
                               (write-string "| W0 | Zero | DONE |\n" out)
                               (write-string "| W1 | One | DONE |\n" out)
                               (write-string "| W2 | Two | PENDING |\n" out))
                             #:exists 'truncate)
      (define rec (migrate-campaign! dir))
      (check-equal? (length (campaign-record-waves rec)) 2 "2 waves from new PLAN.md")
      (check-eq? (campaign-wave-status (list-ref (campaign-record-waves rec) 0))
                 'pending
                 "new campaign resets statuses to pending")
      (delete-directory/files dir #:must-exist? #f))

    (test-case "exactly one durable source seeds with provenance"
      (define dir (make-temporary-file "campaign-migrate-plan-~a" 'directory))
      (make-directory (build-path dir ".planning"))
      (make-directory (build-path dir ".planning" "waves"))
      (call-with-output-file (build-path dir ".planning" "PLAN.md")
                             (lambda (out)
                               (write-string "# Plan: PlanOnly\n\n## Waves\n" out)
                               (write-string "- [Inbox] W0: Zero → waves/W0-zero.md\n" out))
                             #:exists 'truncate)
      ;; Referenced wave doc must exist (BUG-0052 hard failure otherwise).
      (call-with-output-file (build-path dir ".planning" "waves" "W0-zero.md")
                             (lambda (out)
                               (write-string "# W0-zero.md\nStatus: Inbox\n\nBody.\n" out))
                             #:exists 'truncate)
      (define rec (migrate-campaign! dir))
      (check-eq? (campaign-record-provenance rec) 'plan)
      (check-eq? (campaign-wave-status (list-ref (campaign-record-waves rec) 0)) 'pending)
      (delete-directory/files dir #:must-exist? #f))

    (test-case "wave doc existence never implies completion"
      (define dir (make-temporary-file "campaign-migrate-doc-~a" 'directory))
      (make-directory (build-path dir ".planning"))
      (make-directory (build-path dir ".planning" "waves"))
      ;; Wave doc file exists on disk with rich content…
      (call-with-output-file (build-path dir ".planning" "waves" "W0-zero.md")
                             (lambda (out)
                               (display "## Root Cause\n\nFully implemented wave doc.\n" out))
                             #:exists 'truncate)
      (call-with-output-file (build-path dir ".planning" "PLAN.md")
                             (lambda (out)
                               (write-string "# Plan: DocExists\n\n## Waves\n" out)
                               (write-string "- [Inbox] W0: Zero → waves/W0-zero.md\n" out))
                             #:exists 'truncate)
      (define rec (migrate-campaign! dir))
      (check-eq? (campaign-wave-status (list-ref (campaign-record-waves rec) 0))
                 'pending
                 "doc existence does not infer completion")
      (delete-directory/files dir #:must-exist? #f))))

;; ============================================================
;; 6. Durable persistence: cancellation + fencing (D5 / GC-8)
;; ============================================================

(define persistence-suite
  (test-suite "durable persistence"
    (test-case "cancellation and fencing fields persist across restart"
      (define dir (make-temporary-file "campaign-persist-~a" 'directory))
      (define rec (make-test-record 2))
      (set-campaign-cancellation! rec (make-campaign-cancellation "operator" 12345))
      (set-campaign-fence-token! rec 42)
      (persist-campaign! dir rec)
      (define loaded (load-campaign-record dir (campaign-plan-id rec)))
      (check-not-false loaded)
      (check-true (campaign-cancellation? (campaign-record-cancellation loaded)))
      (check-equal? (campaign-cancellation-reason (campaign-record-cancellation loaded)) "operator")
      (check-equal? (campaign-fence-token loaded) 42)
      (check-equal? (campaign-wave-status (list-ref (campaign-record-waves loaded) 0)) 'pending)
      (delete-directory/files dir #:must-exist? #f))

    (test-case "attempt and fence survive round-trip (atomic replace)"
      (define dir (make-temporary-file "campaign-attempt-~a" 'directory))
      (define rec (make-test-record 1))
      (begin-attempt! rec 0 7)
      (persist-campaign! dir rec)
      (define loaded (load-campaign-record dir (campaign-plan-id rec)))
      (define w (list-ref (campaign-record-waves loaded) 0))
      (check-eq? (campaign-wave-status w) 'in-progress)
      (check-equal? (campaign-wave-attempt-count w) 1)
      (check-equal? (campaign-attempt-fence-token (campaign-wave-current-attempt w)) 7)
      (delete-directory/files dir #:must-exist? #f))))

;; ============================================================
;; 7. v1.00.24 W3 (verification-truth): durable wave/attempt failure reason
;; ============================================================

(define failure-reason-suite
  (test-suite "durable failure reason"
    (test-case "fresh waves carry no failure reason"
      (define rec (make-test-record 2))
      (for ([w (campaign-record-waves rec)])
        (check-equal? (wave-failure-reason w) "" "no reason recorded by default")))

    (test-case "stamp-wave-failure! records the reason on wave and attempt"
      (define rec (make-test-record 1))
      (begin-attempt! rec 0 3)
      (define w (list-ref (campaign-record-waves rec) 0))
      (stamp-wave-failure! w "provider 500 after 5 retries")
      (check-equal? (wave-failure-reason w) "provider 500 after 5 retries")
      (define a (campaign-wave-current-attempt w))
      (check-not-false a)
      (check-equal? (attempt-failure-reason a) "provider 500 after 5 retries"))

    (test-case "blank and non-string reasons are ignored (never fake data)"
      (define rec (make-test-record 1))
      (begin-attempt! rec 0 3)
      (define w (list-ref (campaign-record-waves rec) 0))
      (stamp-wave-failure! w "")
      (stamp-wave-failure! w "   ")
      (stamp-wave-failure! w #f)
      (check-equal? (wave-failure-reason w) "")
      (check-false (attempt-failure-reason (campaign-wave-current-attempt w))))

    (test-case "clear-wave-failure! resets wave and attempt"
      (define rec (make-test-record 1))
      (begin-attempt! rec 0 3)
      (define w (list-ref (campaign-record-waves rec) 0))
      (stamp-wave-failure! w "timed out")
      (clear-wave-failure! w)
      (check-equal? (wave-failure-reason w) "")
      (check-false (attempt-failure-reason (campaign-wave-current-attempt w))))

    (test-case "stamping without a current attempt records the wave reason only"
      (define rec (make-test-record 1))
      (define w (list-ref (campaign-record-waves rec) 0))
      (stamp-wave-failure! w "verifier rejected")
      (check-equal? (wave-failure-reason w) "verifier rejected"))))

;; ============================================================
;; Runner
;; ============================================================

(define campaign-state-suite
  (test-suite "gsd-campaign-state"
    status-mapping-suite
    reconstruction-suite
    identity-suite
    invariant-suite
    migration-suite
    persistence-suite
    failure-reason-suite))

(void (run-tests campaign-state-suite))
