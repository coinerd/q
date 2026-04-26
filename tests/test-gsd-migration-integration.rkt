#lang racket

;; tests/test-gsd-migration-integration.rkt — Integration tests for GSD migration
;;
;; Wave 4c: Full lifecycle tests through the old API → new module delegation.

(require rackunit
         racket/file
         racket/hash
         (only-in "../tools/tool.rkt" tool-result-is-error?)
         "../extensions/gsd-planning.rkt"
         "../extensions/gsd/state-machine.rkt"
         "../extensions/gsd/plan-types.rkt"
         (only-in "../extensions/gsd/wave-executor.rkt"
                  make-wave-executor
                  wave-start!
                  wave-complete!
                  wave-fail!
                  wave-skip!
                  wave-executor-statuses
                  wave-status-state
                  all-waves-done?
                  [next-pending-wave exec-next-pending])
         "../extensions/gsd/plan-validator.rkt"
         (only-in "../extensions/gsd/core.rkt" gsd-write-guard))

;; ============================================================
;; Helper
;; ============================================================

(define (with-clean-state proc)
  (reset-all-gsd-state!)
  (define dir (make-temporary-file "~a-gsdtest" 'directory))
  (set-pinned-planning-dir! dir)
  (with-handlers ([exn:fail? (lambda (e)
                               (set-pinned-planning-dir! #f)
                               (delete-directory/files dir #:must-exist? #f)
                               (reset-all-gsd-state!)
                               (raise e))])
    (begin0 (proc dir)
      (set-pinned-planning-dir! #f)
      (delete-directory/files dir #:must-exist? #f)
      (reset-all-gsd-state!))))

;; ============================================================
;; Full lifecycle: /plan → write → /go → execute → verify
;; ============================================================

(test-case "full GSD lifecycle through old API"
  (with-clean-state (lambda (dir)
                      ;; Step 1: /plan → planning mode
                      (set-gsd-mode! 'planning)
                      (check-eq? (gsd-mode) 'planning)
                      (check-eq? (gsm-current) 'exploring)

                      ;; Step 2: Write PLAN.md → auto-transition to plan-written
                      (define plan-content
                        "# Plan\n\n## Wave 0: Fix\n- Files: fix.rkt\n- Verify: raco test fix\n")
                      (define args (hasheq 'artifact "PLAN" 'content plan-content))
                      (define write-result (handle-planning-write args))
                      (check-false (tool-result-is-error? write-result))
                      (check-eq? (gsd-mode) 'plan-written)
                      (check-eq? (gsm-current) 'plan-written)

                      ;; Step 3: /go → executing
                      (set-gsd-mode! 'executing)
                      (check-eq? (gsd-mode) 'executing)
                      (check-eq? (gsm-current) 'executing)

                      ;; Step 4: Reset
                      (reset-all-gsd-state!)
                      (check-eq? (gsd-mode) #f)
                      (check-eq? (gsm-current) 'idle))))

;; ============================================================
;; Wave failure + skip recovery
;; ============================================================

(test-case "wave failure recovery through new modules"
  (with-clean-state
   (lambda (dir)
     ;; Create a plan and executor
     (define plan
       (gsd-plan (list (gsd-wave 0 "Setup" 'pending "" '("a.rkt") '() "test a" '())
                       (gsd-wave 1 "Implement" 'pending "" '("b.rkt") '() "test b" '())
                       (gsd-wave 2 "Verify" 'pending "" '("c.rkt") '() "test c" '()))
                 ""
                 '()
                 '()))
     (define exec (make-wave-executor plan))

     ;; Start wave 0, fail it
     (wave-start! exec 0)
     (wave-fail! exec 0 "compile error")
     (check-eq? (wave-status-state (list-ref (wave-executor-statuses exec) 0)) 'failed)

     ;; Wave 1 still pending — proceed
     (check-equal? (exec-next-pending exec) 1)
     (wave-start! exec 1)
     (wave-complete! exec 1)

     ;; Wave 2 — skip it
     (wave-skip! exec 2)

     ;; All done
     (check-true (all-waves-done? exec)))))

;; ============================================================
;; Write-to-PLAN.md guard during executing
;; ============================================================

(test-case "write guard blocks PLAN.md during executing mode"
  (with-clean-state (lambda (dir)
                      ;; Transition to executing
                      (gsm-transition! 'exploring)
                      (gsm-transition! 'plan-written)
                      (gsm-transition! 'executing)

                      ;; Write guard should block PLAN.md
                      (define result (gsd-write-guard ".planning/PLAN.md" ".planning"))
                      (check-true (hash? result))
                      (check-equal? (hash-ref result 'blocked) #t)

                      ;; But other files should be fine
                      (check-true (gsd-write-guard "src/fix.rkt" ".planning")))))

(test-case "write guard blocks .. traversal to PLAN.md"
  (with-clean-state (lambda (dir)
                      (gsm-transition! 'exploring)
                      (gsm-transition! 'plan-written)
                      (gsm-transition! 'executing)

                      (define result (gsd-write-guard "src/../.planning/PLAN.md" ".planning"))
                      (check-true (hash? result))
                      (check-equal? (hash-ref result 'blocked) #t))))

;; ============================================================
;; State machine transitions across all paths
;; ============================================================

(test-case "state machine transition validation — all valid paths"
  (reset-gsm!)
  (check-eq? (gsm-current) 'idle)

  ;; idle → exploring
  (check-true (ok? (gsm-transition! 'exploring)))
  (check-eq? (gsm-current) 'exploring)

  ;; exploring → plan-written
  (check-true (ok? (gsm-transition! 'plan-written)))
  (check-eq? (gsm-current) 'plan-written)

  ;; plan-written → executing
  (check-true (ok? (gsm-transition! 'executing)))
  (check-eq? (gsm-current) 'executing)

  ;; executing → verifying
  (check-true (ok? (gsm-transition! 'verifying)))
  (check-eq? (gsm-current) 'verifying)

  ;; verifying → idle
  (check-true (ok? (gsm-transition! 'idle)))
  (check-eq? (gsm-current) 'idle))

(test-case "state machine rejects invalid transitions"
  (reset-gsm!)
  ;; idle → executing (invalid — must go through exploring → plan-written)
  (check-false (ok? (gsm-transition! 'executing)))
  (check-eq? (gsm-current) 'idle))

;; ============================================================
;; Plan validation integration
;; ============================================================

(test-case "plan validation rejects empty plan"
  (define plan (gsd-plan '() "" '() '()))
  (check-false (valid-plan->go? plan)))

(test-case "plan validation accepts well-formed plan"
  (define plan
    (gsd-plan (list (gsd-wave 0 "Fix" 'pending "root cause" '("a.rkt") '() "raco test a" '()))
              ""
              '()
              '()))
  (check-true (valid-plan->go? plan)))

;; ============================================================
;; Old API backward compatibility
;; ============================================================

(test-case "legacy API: gsd-mode/set-gsd-mode! round-trip"
  (reset-all-gsd-state!)
  (check-equal? (gsd-mode) #f)
  (set-gsd-mode! 'planning)
  (check-eq? (gsd-mode) 'planning)
  (set-gsd-mode! 'plan-written)
  (check-eq? (gsd-mode) 'plan-written)
  (set-gsd-mode! 'executing)
  (check-eq? (gsd-mode) 'executing)
  (set-gsd-mode! #f)
  (check-equal? (gsd-mode) #f))

(test-case "legacy API: budget functions still work"
  (reset-all-gsd-state!)
  (reset-go-budget!)
  (check-equal? (go-read-budget) GO-READ-BUDGET)
  (define after-dec (decrement-budget!))
  (check-equal? after-dec (sub1 GO-READ-BUDGET)))

(test-case "legacy API: read-count functions still work"
  (reset-all-gsd-state!)
  (clear-read-counts!)
  (check-equal? (get-read-count "foo.rkt") 0)
  (increment-read-count! "foo.rkt")
  (check-equal? (get-read-count "foo.rkt") 1))
