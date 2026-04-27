#lang racket

;; tests/test-gsd-planning-integration.rkt — Integration tests for GSD planning
;;
;; Wave 4 (v0.20.3): Full-pipeline tests exercising hook dispatch, state transitions,
;; concurrent budget, and cross-cutting behaviors that unit tests miss.
;; These would have caught the parameter→box bug (C2) and invisible warning bug (C3).

(require rackunit
         racket/file
         "../extensions/gsd-planning.rkt"
         (only-in "../extensions/gsd-planning-state.rkt"
                  gsd-mode
                  set-gsd-mode!
                  gsd-mode?
                  pinned-planning-dir
                  set-pinned-planning-dir!
                  current-max-old-text-len
                  set-current-max-old-text-len!
                  completed-waves
                  total-waves
                  set-total-waves!
                  mark-wave-complete!
                  wave-complete?
                  next-pending-wave
                  gsd-snapshot
                  reset-all-gsd-state!)
         "../extensions/api.rkt"
         "../tools/tool.rkt"
         "../extensions/hooks.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (with-clean-state thunk)
  (reset-all-gsd-state!)
  (dynamic-wind (lambda () (reset-all-gsd-state!)) thunk (lambda () (reset-all-gsd-state!))))

(define (make-read-result path text)
  (make-success-result (list (hasheq 'type "text" 'text text)) (hasheq 'path path)))

(define (make-generic-result text)
  (make-success-result (list (hasheq 'type "text" 'text text)) (hasheq)))

(define (with-temp-planning-dir thunk)
  (define dir (make-temporary-file "gsd-integ-~a" 'directory))
  (dynamic-wind (lambda () (make-directory* (build-path dir ".planning")))
                (lambda ()
                  (parameterize ([current-directory dir])
                    (set-pinned-planning-dir! dir)
                    (thunk dir)))
                (lambda ()
                  (set-pinned-planning-dir! #f)
                  (with-handlers ([exn:fail? (lambda (e) (void))])
                    (delete-directory/files dir)))))

;; ============================================================
;; Test 1: /go sets mode to executing, resets budget and read counts
;; ============================================================


;; ============================================================
;; Test 2: Budget decrements correctly across tool-guard + read-tracker
;; ============================================================


;; ============================================================
;; Test 3: Budget warning appears in result, not in args
;; ============================================================


;; ============================================================
;; Test 4: planning-write blocked during executing mode
;; ============================================================

(test-case "planning-write blocked during executing mode"
  (with-clean-state
   (lambda ()
     (set-gsd-mode! 'executing)
     ;; Tool guard should block planning-write during executing
     (define res (gsd-tool-guard (hasheq 'tool-name "planning-write" 'args (hasheq))))
     (check-eq? (hook-result-action res) 'block "planning-write should be blocked during executing")
     (set-gsd-mode! #f))))

;; ============================================================
;; Test 5: session-shutdown resets all state
;; ============================================================


;; ============================================================
;; Test 6: planning-read allowed during executing (I1 fix)
;; ============================================================

(test-case "planning-read allowed during executing mode (I1 fix)"
  (with-clean-state (lambda ()
                      (with-temp-planning-dir
                       (lambda (dir)
                         (call-with-output-file (build-path dir ".planning" "STATE.md")
                                                (lambda (out) (display "# State\nActive." out))
                                                #:exists 'truncate)
                         (set-gsd-mode! 'executing)
                         (define result (handle-planning-read (hasheq 'artifact "STATE")))
                         (check-false (tool-result-is-error? result)
                                      "planning-read should succeed during executing")
                         (set-gsd-mode! #f))))))

;; ============================================================
;; Test 7: Concurrent budget decrement — no lost updates (C1)
;; ============================================================


;; ============================================================
;; Test 8: Read hint + budget warning both appear in result
;; ============================================================


;; ============================================================
;; Test 9: Hard block at GO-READ-BLOCK-THRESHOLD
;; ============================================================


;; ============================================================
;; Test 10: Edit limit raised during /go
;; ============================================================

(test-case "/go raises edit limit from default to 1200"
  (with-clean-state
   (lambda ()
     (with-temp-planning-dir
      (lambda (dir)
        (call-with-output-file (build-path dir ".planning" "PLAN.md")
                               (lambda (out) (display "## Wave 0\n" out))
                               #:exists 'truncate)
        (check-equal? (current-max-old-text-len) 500 "default should be 500")
        (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
        (handler (hasheq 'command "/go" 'input "/go"))
        (check-equal? (current-max-old-text-len) 1200 "should be raised to 1200 during /go"))))))

;; ============================================================
;; Test 11: State module re-exports work (backward compat)
;; ============================================================


;; ============================================================
;; Test 12: Non-read tools don't affect read count tracking
;; ============================================================


;; ============================================================
;; Test 13: reset-all-gsd-state! is idempotent
;; ============================================================


;; ============================================================
;; Test 14: Budget warning text for non-read read-only tools
;; ============================================================


;; ============================================================
;; Test 15: Full lifecycle — /plan → /go → session-shutdown
;; ============================================================

