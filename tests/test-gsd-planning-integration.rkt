#lang racket

;; BOUNDARY: integration

;; tests/test-gsd-planning-integration.rkt — Integration tests for GSD planning
;;
;; Wave 4 (v0.20.3): Full-pipeline tests exercising hook dispatch, state transitions,
;; concurrent budget, and cross-cutting behaviors that unit tests miss.
;; These would have caught the parameter→box bug (C2) and invisible warning bug (C3).

(require rackunit
         racket/file
         "../extensions/gsd-planning.rkt"
         (only-in "../extensions/gsd/state-machine.rkt"
                  gsm-current
                  gsm-reset!
                  gsm-transition-to!
                  gsm-transition!
                  gsm-completed-waves
                  gsm-total-waves
                  gsm-set-total-waves!
                  gsm-mark-wave-complete!
                  gsm-wave-complete?
                  gsm-next-pending-wave
                  gsm-current-wave
                  gsm-set-current-wave!
                  gsm-snapshot)
         (only-in "../extensions/gsd/session-state.rkt"
                  current-pinned-dir
                  set-pinned-dir!
                  current-edit-limit
                  set-edit-limit!)
         (only-in "../extensions/gsd/core.rkt" reset-all-gsd-state!)
         "../extensions/api.rkt"
         "../tools/tool.rkt"
         "../extensions/hooks.rkt")

;; Legacy mode wrappers (DEBT-01)
(define (gsd-mode)
  (let ([s (gsm-current)])
    (cond
      [(eq? s 'idle) #f]
      [(eq? s 'exploring) 'planning]
      [else s])))

(define (gsd-mode? v)
  (eq? (gsd-mode) v))

(define (set-gsd-mode! v)
  (cond
    [(not v) (gsm-reset!)]
    [(eq? v 'planning) (gsm-transition-to! 'exploring)]
    [(eq? v 'plan-written) (gsm-transition-to! 'plan-written)]
    [(eq? v 'executing) (gsm-transition-to! 'executing)]
    [else (gsm-transition! v)]))

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
                    (set-pinned-dir! dir)
                    (thunk dir)))
                (lambda ()
                  (set-pinned-dir! #f)
                  (with-handlers ([exn:fail? (lambda (e) (void))])
                    (delete-directory/files dir)))))

;; ============================================================

;; ============================================================
;; Test 1: planning-write blocked during executing mode
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
;; Test 2: planning-read allowed during executing mode
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
;; Test 3: /go raises edit limit from default to 1200
;; ============================================================

(test-case "/go raises edit limit from default to 1200"
  (with-clean-state
   (lambda ()
     (with-temp-planning-dir
      (lambda (dir)
        (call-with-output-file
         (build-path dir ".planning" "PLAN.md")
         (lambda (out) (display "## Wave 0: Test\n- File: q/test.rkt\n- Verify: raco test\n" out))
         #:exists 'truncate)
        (check-equal? (current-edit-limit) 500 "default should be 500")
        (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
        (handler (hasheq 'command "/go" 'input "/go"))
        (check-equal? (current-edit-limit) 1200 "should be raised to 1200 during /go"))))))

;; ============================================================
;; Test 4: reset-all-gsd-state! is idempotent
;; ============================================================

(test-case "reset-all-gsd-state! is idempotent"
  (with-clean-state (lambda ()
                      ;; Set some state
                      (set-gsd-mode! 'executing)
                      (gsm-set-total-waves! 5)
                      (gsm-mark-wave-complete! 1)
                      ;; Reset twice
                      (reset-all-gsd-state!)
                      (reset-all-gsd-state!)
                      ;; State should be clean
                      (check-false (gsd-mode))
                      (check-equal? (gsm-total-waves) 0)
                      (check-false (gsm-wave-complete? 1)))))
