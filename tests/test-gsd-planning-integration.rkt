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
                  go-read-budget
                  set-go-read-budget!
                  decrement-budget!
                  reset-go-budget!
                  GO-READ-BUDGET
                  read-counts
                  get-read-count
                  increment-read-count!
                  clear-read-counts!
                  current-max-old-text-len
                  set-current-max-old-text-len!
                  completed-waves
                  total-waves
                  set-total-waves!
                  mark-wave-complete!
                  wave-complete?
                  next-pending-wave
                  plan-tool-budget
                  decrement-plan-budget!
                  reset-plan-budget!
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

(test-case "/go sets executing mode and resets budget"
  (with-clean-state
   (lambda ()
     (with-temp-planning-dir
      (lambda (dir)
        ;; Pre-populate PLAN.md
        (call-with-output-file (build-path dir ".planning" "PLAN.md")
                               (lambda (out) (display "## Wave 0: Test\n- File: foo.rkt\n" out))
                               #:exists 'truncate)
        ;; Set some dirty state
        (set-gsd-mode! 'planning)
        (set-go-read-budget! 10)
        (increment-read-count! "/tmp/x.rkt")
        ;; Execute /go via the extension's handler
        (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
        (define result (handler (hasheq 'command "/go" 'input "/go")))
        ;; Verify mode changed
        (check-eq? (gsd-mode) 'executing)
        ;; Verify budget was reset to GO-READ-BUDGET (30)
        (check-equal? (go-read-budget) GO-READ-BUDGET)
        ;; Verify submit text contains implement prompt
        (check-true (string-contains? (hash-ref (hook-result-payload result) 'submit)
                                      "IMPLEMENT NOW")))))))

;; ============================================================
;; Test 2: Budget decrements correctly across tool-guard + read-tracker
;; ============================================================

(test-case "budget decrements correctly through tool-guard → read-tracker pipeline"
  (with-clean-state (lambda ()
                      (set-gsd-mode! 'executing)
                      (reset-go-budget!)
                      ;; Simulate 3 read calls through tool-guard (pre) then read-tracker (post)
                      (for ([i (in-range 3)])
                        (define payload (hasheq 'tool-name "read" 'args (hasheq)))
                        ;; Pre-hook: decrements budget
                        (define pre (gsd-tool-guard payload))
                        (check-eq? (hook-result-action pre) 'pass (format "call ~a should pass" i))
                        ;; Post-hook: tracks read
                        (define post-result (make-read-result "/tmp/test.rkt" "content"))
                        (gsd-read-tracker (hasheq 'tool-name "read" 'result post-result)))
                      ;; Budget should be 30 - 3 = 27
                      (check-equal? (go-read-budget) 27)
                      (set-gsd-mode! #f))))

;; ============================================================
;; Test 3: Budget warning appears in result, not in args
;; ============================================================

(test-case "budget warning in tool result content, not in tool call args"
  (with-clean-state
   (lambda ()
     (set-gsd-mode! 'executing)
     (reset-go-budget!)
     ;; Burn 25 calls via tool-guard
     (for ([_ (in-range 25)])
       (gsd-tool-guard (hasheq 'tool-name "read" 'args (hasheq))))
     ;; Budget should be 5 (warn threshold)
     (check-equal? (go-read-budget) 5)
     ;; Next call: tool-guard should PASS (not amend args with warning)
     (define pre (gsd-tool-guard (hasheq 'tool-name "read" 'args (hasheq))))
     (check-eq? (hook-result-action pre) 'pass "tool-guard should pass, not amend")
     ;; But read-tracker should inject warning into result
     (define post-result (make-read-result "/tmp/warn.rkt" "data"))
     (define post (gsd-read-tracker (hasheq 'tool-name "read" 'result post-result)))
     (check-eq? (hook-result-action post) 'amend "read-tracker should amend with warning")
     (define amended (hash-ref (hook-result-payload post) 'result))
     (define content (tool-result-content amended))
     (define all-text (apply string-append (map (lambda (p) (hash-ref p 'text "")) content)))
     (check-true (string-contains? all-text "BUDGET WARNING")
                 (format "warning should be in result, got: ~a" all-text))
     (set-gsd-mode! #f))))

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

(test-case "session-shutdown resets all state via gsd-session-cleanup"
  (with-clean-state
   (lambda ()
     ;; Set up dirty state
     (set-gsd-mode! 'executing)
     (reset-go-budget!)
     (increment-read-count! "/tmp/a.rkt")
     (increment-read-count! "/tmp/b.rkt")
     ;; Call cleanup
     (gsd-session-cleanup (hasheq 'session-id "test" 'duration 100))
     ;; Verify everything is reset
     (check-equal? (gsd-mode) #f "mode should be #f after cleanup")
     (check-false (go-read-budget) "budget should be #f after cleanup")
     (check-equal? (hash-count (read-counts)) 0 "read counts should be empty after cleanup"))))

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

(test-case "concurrent budget decrements are atomic (C1)"
  (with-clean-state (lambda ()
                      (set-gsd-mode! 'executing)
                      (reset-go-budget!)
                      ;; Fire 20 concurrent decrements from separate threads
                      (define sem (make-semaphore 0))
                      (define threads
                        (for/list ([_ (in-range 20)])
                          (thread (lambda ()
                                    (semaphore-wait sem)
                                    (decrement-budget!)))))
                      ;; Release all threads at once
                      (for ([_ (in-range 20)])
                        (semaphore-post sem))
                      ;; Wait for all threads
                      (for-each thread-wait threads)
                      ;; Budget should be exactly 30 - 20 = 10, no lost decrements
                      (check-equal? (go-read-budget) 10)
                      (set-gsd-mode! #f))))

;; ============================================================
;; Test 8: Read hint + budget warning both appear in result
;; ============================================================

(test-case "read hint and budget warning both injected when both thresholds met"
  (with-clean-state
   (lambda ()
     (set-gsd-mode! 'executing)
     (reset-go-budget!)
     ;; Burn 27 calls to get budget = 3 (below warn threshold)
     (for ([_ (in-range 27)])
       (gsd-tool-guard (hasheq 'tool-name "read" 'args (hasheq))))
     ;; Read same file 3 times to hit hint threshold
     (define result (make-read-result "/tmp/both.txt" "content"))
     (gsd-read-tracker (hasheq 'tool-name "read" 'result result))
     (gsd-read-tracker (hasheq 'tool-name "read" 'result result))
     (define post (gsd-read-tracker (hasheq 'tool-name "read" 'result result)))
     (check-eq? (hook-result-action post) 'amend)
     (define amended (hash-ref (hook-result-payload post) 'result))
     (define content (tool-result-content amended))
     (define all-text (apply string-append (map (lambda (p) (hash-ref p 'text "")) content)))
     (check-true (string-contains? all-text "SYSTEM NOTICE") "should contain read hint")
     (check-true (string-contains? all-text "SYSTEM NOTICE: BUDGET WARNING") "should contain budget warning")
     (set-gsd-mode! #f))))

;; ============================================================
;; Test 9: Hard block at GO-READ-BLOCK-THRESHOLD
;; ============================================================

(test-case "hard block when budget falls below block threshold"
  (with-clean-state (lambda ()
                      (set-gsd-mode! 'executing)
                      (reset-go-budget!)
                      ;; Burn 34 calls to get below block threshold (-4 < -3)
                      (for ([_ (in-range 34)])
                        (gsd-tool-guard (hasheq 'tool-name "read" 'args (hasheq))))
                      (check-true (< (go-read-budget) GO-READ-BLOCK-THRESHOLD))
                      ;; Next call should be blocked
                      (define res (gsd-tool-guard (hasheq 'tool-name "read" 'args (hasheq))))
                      (check-eq? (hook-result-action res) 'block)
                      (set-gsd-mode! #f))))

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

(test-case "state accessors available via gsd-planning re-export"
  (with-clean-state (lambda ()
                      ;; These are all re-exported from gsd-planning.rkt
                      (check-true (procedure? set-gsd-mode!))
                      (check-true (procedure? set-pinned-planning-dir!))
                      (check-true (procedure? set-go-read-budget!))
                      (check-true (procedure? set-current-max-old-text-len!))
                      (check-true (procedure? reset-all-gsd-state!))
                      (check-true (procedure? gsd-mode?))
                      (check-true (procedure? decrement-budget!)))))

;; ============================================================
;; Test 12: Non-read tools don't affect read count tracking
;; ============================================================

(test-case "edit tools pass through tool-guard without budget decrement"
  (with-clean-state (lambda ()
                      (set-gsd-mode! 'executing)
                      (reset-go-budget!)
                      ;; Edit tool should not affect budget
                      (for ([_ (in-range 5)])
                        (gsd-tool-guard (hasheq 'tool-name "edit" 'args (hasheq))))
                      (check-equal? (go-read-budget) 30 "edit calls should not decrement budget")
                      (set-gsd-mode! #f))))

;; ============================================================
;; Test 13: reset-all-gsd-state! is idempotent
;; ============================================================

(test-case "reset-all-gsd-state! is idempotent"
  (with-clean-state (lambda ()
                      (reset-all-gsd-state!)
                      (reset-all-gsd-state!)
                      (check-equal? (gsd-mode) #f)
                      (check-false (go-read-budget))
                      (check-equal? (hash-count (read-counts)) 0))))

;; ============================================================
;; Test 14: Budget warning text for non-read read-only tools
;; ============================================================

(test-case "budget warning injected for grep results when budget low"
  (with-clean-state (lambda ()
                      (set-gsd-mode! 'executing)
                      (reset-go-budget!)
                      ;; Burn 26 via tool-guard
                      (for ([_ (in-range 26)])
                        (gsd-tool-guard (hasheq 'tool-name "grep" 'args (hasheq))))
                      ;; grep result through read-tracker
                      (define result (make-generic-result "match found"))
                      (define post (gsd-read-tracker (hasheq 'tool-name "grep" 'result result)))
                      (check-eq? (hook-result-action post) 'amend "grep should get budget warning")
                      (define amended (hash-ref (hook-result-payload post) 'result))
                      (define content (tool-result-content amended))
                      (define all-text
                        (apply string-append (map (lambda (p) (hash-ref p 'text "")) content)))
                      (check-true (string-contains? all-text "BUDGET WARNING"))
                      (set-gsd-mode! #f))))

;; ============================================================
;; Test 15: Full lifecycle — /plan → /go → session-shutdown
;; ============================================================

(test-case "full lifecycle: /plan → /go → session-shutdown"
  (with-clean-state
   (lambda ()
     (with-temp-planning-dir
      (lambda (dir)
        ;; Write PLAN.md (simulating /plan)
        (call-with-output-file (build-path dir ".planning" "PLAN.md")
                               (lambda (out)
                                 (display "## Wave 0: Full lifecycle test\n- File: test.rkt\n" out))
                               #:exists 'truncate)
        ;; State should be clean
        (check-equal? (gsd-mode) #f)
        ;; Start /go
        (define handler (hash-ref (extension-hooks gsd-planning-extension) 'execute-command))
        (handler (hasheq 'command "/go" 'input "/go"))
        (check-eq? (gsd-mode) 'executing)
        (check-equal? (go-read-budget) GO-READ-BUDGET)
        ;; Simulate some work
        (gsd-tool-guard (hasheq 'tool-name "read" 'args (hasheq)))
        (gsd-tool-guard (hasheq 'tool-name "read" 'args (hasheq)))
        (check-equal? (go-read-budget) (- GO-READ-BUDGET 2))
        ;; Session shutdown
        (gsd-session-cleanup (hasheq 'session-id "lifecycle" 'duration 42))
        (check-equal? (gsd-mode) #f)
        (check-false (go-read-budget)))))))
