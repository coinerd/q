#lang racket

;; tests/test-gsd-planning-boundary.rkt — GSD planning/execution boundary tests
;;
;; Tests for v0.20.2 Wave 0: Server-side planning/execution boundary enforcement.
;; Covers:
;;   - gsd-mode state machine transitions
;;   - tool-call-pre hook blocks write tools after PLAN written
;;   - tool-call-pre hook blocks planning-write during /go
;;   - Mode resets on new /plan and /go invocations

(require rackunit
         racket/file
         racket/string
         "../extensions/gsd-planning.rkt"
         "../extensions/context.rkt"
         "../extensions/api.rkt"
         "../extensions/hooks.rkt"
         "../tools/tool.rkt"
         "../util/hook-types.rkt"
         "../agent/event-bus.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-temp-planning-dir)
  (define dir (make-temporary-file "gsd-boundary-test-~a" 'directory))
  dir)

(define (cleanup-temp-dir dir)
  (when (directory-exists? dir)
    (delete-directory/files dir)))

(define (with-temp-dir proc)
  (define dir (make-temp-planning-dir))
  (with-handlers ([exn:fail? (lambda (e)
                               (cleanup-temp-dir dir)
                               (raise e))])
    (begin0 (proc dir)
      (cleanup-temp-dir dir))))

(define (with-mode mode proc)
  (define saved (gsd-mode))
  (set-gsd-mode! mode)
  (with-handlers ([exn:fail? (lambda (e)
                               (set-gsd-mode! saved)
                               (raise e))])
    (begin0 (proc)
      (set-gsd-mode! saved))))

(define (with-pinned-dir dir proc)
  (define saved (pinned-planning-dir))
  (set-pinned-planning-dir! dir)
  (with-handlers ([exn:fail? (lambda (e)
                               (set-pinned-planning-dir! saved)
                               (raise e))])
    (begin0 (proc)
      (set-pinned-planning-dir! saved))))

;; ============================================================
;; gsd-mode state machine tests
;; ============================================================

(test-case "gsd-mode defaults to #f"
  (check-equal? (gsd-mode) #f))

(test-case "gsd-mode can be set to 'planning"
  (with-mode 'planning (lambda () (check-eq? (gsd-mode) 'planning))))

(test-case "gsd-mode can be set to 'plan-written"
  (with-mode 'plan-written (lambda () (check-eq? (gsd-mode) 'plan-written))))

(test-case "gsd-mode can be set to 'executing"
  (with-mode 'executing (lambda () (check-eq? (gsd-mode) 'executing))))

(test-case "gsd-mode can be reset to #f"
  (with-mode 'planning
             (lambda ()
               (set-gsd-mode! #f)
               (check-equal? (gsd-mode) #f))))

;; ============================================================
;; gsd-tool-guard tests (tool-call-pre hook)
;; ============================================================

(test-case "gsd-tool-guard passes when mode is #f (idle)"
  (with-mode #f
             (lambda ()
               (define payload (hasheq 'tool-name "edit"))
               (define result (gsd-tool-guard payload))
               (check-eq? (hook-result-action result) 'pass))))

(test-case "gsd-tool-guard passes when mode is 'planning"
  (with-mode 'planning
             (lambda ()
               (define payload (hasheq 'tool-name "edit"))
               (define result (gsd-tool-guard payload))
               (check-eq? (hook-result-action result) 'pass))))

(test-case "gsd-tool-guard blocks edit in 'plan-written mode"
  (with-mode 'plan-written
             (lambda ()
               (define payload (hasheq 'tool-name "edit"))
               (define result (gsd-tool-guard payload))
               (check-eq? (hook-result-action result) 'block)
               (check-true (string-contains? (hook-result-payload result) "/go")))))

(test-case "gsd-tool-guard blocks write in 'plan-written mode"
  (with-mode 'plan-written
             (lambda ()
               (define payload (hasheq 'tool-name "write"))
               (define result (gsd-tool-guard payload))
               (check-eq? (hook-result-action result) 'block))))

(test-case "gsd-tool-guard blocks bash in 'plan-written mode"
  (with-mode 'plan-written
             (lambda ()
               (define payload (hasheq 'tool-name "bash"))
               (define result (gsd-tool-guard payload))
               (check-eq? (hook-result-action result) 'block))))

(test-case "gsd-tool-guard allows read in 'plan-written mode"
  (with-mode 'plan-written
             (lambda ()
               (define payload (hasheq 'tool-name "read"))
               (define result (gsd-tool-guard payload))
               (check-eq? (hook-result-action result) 'pass))))

(test-case "gsd-tool-guard allows planning-read in 'plan-written mode"
  (with-mode 'plan-written
             (lambda ()
               (define payload (hasheq 'tool-name "planning-read"))
               (define result (gsd-tool-guard payload))
               (check-eq? (hook-result-action result) 'pass))))

(test-case "gsd-tool-guard allows grep in 'plan-written mode"
  (with-mode 'plan-written
             (lambda ()
               (define payload (hasheq 'tool-name "grep"))
               (define result (gsd-tool-guard payload))
               (check-eq? (hook-result-action result) 'pass))))

(test-case "gsd-tool-guard allows find in 'plan-written mode"
  (with-mode 'plan-written
             (lambda ()
               (define payload (hasheq 'tool-name "find"))
               (define result (gsd-tool-guard payload))
               (check-eq? (hook-result-action result) 'pass))))

(test-case "gsd-tool-guard blocks planning-write in 'executing mode"
  (with-mode 'executing
             (lambda ()
               (define payload (hasheq 'tool-name "planning-write"))
               (define result (gsd-tool-guard payload))
               (check-eq? (hook-result-action result) 'block)
               (check-true (string-contains? (hook-result-payload result) "Cannot update plan")))))

(test-case "gsd-tool-guard allows edit in 'executing mode"
  (with-mode 'executing
             (lambda ()
               (define payload (hasheq 'tool-name "edit"))
               (define result (gsd-tool-guard payload))
               (check-eq? (hook-result-action result) 'pass))))

(test-case "gsd-tool-guard allows write in 'executing mode"
  (with-mode 'executing
             (lambda ()
               (define payload (hasheq 'tool-name "write"))
               (define result (gsd-tool-guard payload))
               (check-eq? (hook-result-action result) 'pass))))

(test-case "gsd-tool-guard allows bash in 'executing mode"
  (with-mode 'executing
             (lambda ()
               (define payload (hasheq 'tool-name "bash"))
               (define result (gsd-tool-guard payload))
               (check-eq? (hook-result-action result) 'pass))))

(test-case "gsd-tool-guard allows planning-read in 'executing mode"
  (with-mode 'executing
             (lambda ()
               (define payload (hasheq 'tool-name "planning-read"))
               (define result (gsd-tool-guard payload))
               (check-eq? (hook-result-action result) 'pass))))

;; ============================================================
;; Mode transition via handle-planning-write tests
;; ============================================================

(test-case "planning-write PLAN transitions mode from 'planning to 'plan-written"
  (with-temp-dir
   (lambda (dir)
     (with-pinned-dir
      dir
      (lambda ()
        (with-mode
         'planning
         (lambda ()
           (define args (hasheq 'artifact "PLAN" 'content "# Test Plan"))
           (define result (handle-planning-write args))
           (check-false (tool-result-is-error? result) "planning-write should succeed")
           (check-eq? (gsd-mode) 'plan-written "mode should transition to plan-written"))))))))

(test-case "planning-write non-PLAN artifact does not change mode"
  (with-temp-dir
   (lambda (dir)
     (with-pinned-dir
      dir
      (lambda ()
        (with-mode 'planning
                   (lambda ()
                     (define args (hasheq 'artifact "STATE" 'content "# State"))
                     (define result (handle-planning-write args))
                     (check-false (tool-result-is-error? result))
                     (check-eq? (gsd-mode) 'planning "mode should remain planning"))))))))

(test-case "planning-write PLAN does not change mode when not in 'planning"
  (with-temp-dir (lambda (dir)
                   (with-pinned-dir dir
                                    (lambda ()
                                      ;; Mode is #f by default
                                      (define args (hasheq 'artifact "PLAN" 'content "# Plan"))
                                      (define result (handle-planning-write args))
                                      (check-false (tool-result-is-error? result))
                                      (check-equal? (gsd-mode) #f "mode should remain #f"))))))

;; ============================================================
;; Full workflow integration tests
;; ============================================================

(test-case "full workflow: /plan → write PLAN → blocked edit → /go → edit allowed"
  (with-temp-dir (lambda (dir)
                   (with-pinned-dir
                    dir
                    (lambda ()
                      ;; Step 1: /plan sets mode to 'planning
                      (set-gsd-mode! 'planning)
                      (check-eq? (gsd-mode) 'planning)

                      ;; Step 2: writing PLAN transitions to 'plan-written
                      (define write-result
                        (handle-planning-write (hasheq 'artifact "PLAN" 'content "# Plan")))
                      (check-false (tool-result-is-error? write-result))
                      (check-eq? (gsd-mode) 'plan-written)

                      ;; Step 3: edit is blocked in 'plan-written mode
                      (define guard-result (gsd-tool-guard (hasheq 'tool-name "edit")))
                      (check-eq? (hook-result-action guard-result) 'block)

                      ;; Step 4: /go sets mode to 'executing
                      (set-gsd-mode! 'executing)
                      (check-eq? (gsd-mode) 'executing)

                      ;; Step 5: edit is allowed in 'executing mode
                      (define guard-result2 (gsd-tool-guard (hasheq 'tool-name "edit")))
                      (check-eq? (hook-result-action guard-result2) 'pass)

                      ;; Step 6: planning-write is blocked in 'executing mode
                      (define guard-result3 (gsd-tool-guard (hasheq 'tool-name "planning-write")))
                      (check-eq? (hook-result-action guard-result3) 'block)

                      ;; Cleanup
                      (set-gsd-mode! #f))))))

(test-case "new /plan resets mode from 'plan-written"
  ;; Simulating starting a fresh planning session
  (with-mode 'plan-written
             (lambda ()
               (set-gsd-mode! 'planning)
               (check-eq? (gsd-mode) 'planning))))

(test-case "new /go resets mode from 'plan-written"
  (with-mode 'plan-written
             (lambda ()
               (set-gsd-mode! 'executing)
               (check-eq? (gsd-mode) 'executing))))
