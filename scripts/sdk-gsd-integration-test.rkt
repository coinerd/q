#lang racket

;; SDK-level integration test for the GSD extension
;; Run from q/ directory: racket scripts/sdk-gsd-integration-test.rkt

(require "../extensions/gsd-planning.rkt"
         "../extensions/gsd-planning-state.rkt"
         "../extensions/gsd/state-machine.rkt"
         "../extensions/api.rkt"
         "../extensions/hooks.rkt"
         "../runtime/agent-session.rkt"
         "../agent/event-bus.rkt"
         "../tools/registry-defaults.rkt"
         "../tools/tool.rkt"
         (only-in "../interfaces/sdk.rkt" make-runtime open-session)
         "../runtime/settings.rkt"
         "../runtime/provider-factory.rkt"
         racket/file
         racket/port)

;; Helpers: make proper tool-results
(define (ok-result text [details (hasheq)])
  (make-tool-result (list (hasheq 'type "text" 'text text)) details #f))

(define (read-result text path)
  (ok-result text (hasheq 'path path)))

;; ============================================================
;; Setup
;; ============================================================
(define test-dir (make-temporary-file "sdk-gsd-test-~a" 'directory))
(define planning-dir (build-path test-dir ".planning"))
(make-directory* planning-dir)

(printf "=== SDK GSD Integration Test ===\n")
(printf "Test dir: ~a\n\n" test-dir)

(reset-all-gsd-state!)

(call-with-output-file (build-path planning-dir "PLAN.md")
                       (lambda (out)
                         (display (string-append "# Integration Test Plan\n\n"
                                                 "## Wave 0: Setup\n"
                                                 "- Create /tmp/sdk-test-output.txt\n"
                                                 "- File: /tmp/sdk-test-output.txt\n\n"
                                                 "## Wave 1: Verify\n"
                                                 "- Check file exists\n"
                                                 "- File: /tmp/sdk-test-output.txt\n")
                                  out))
                       #:exists 'truncate)
(call-with-output-file (build-path planning-dir "STATE.md")
                       (lambda (out) (display "# State\nW0: pending\n" out))
                       #:exists 'truncate)
(call-with-output-file (build-path planning-dir "VALIDATION.md")
                       (lambda (out) (display "# Validation\n- [ ] File exists\n" out))
                       #:exists 'truncate)

;; ============================================================
;; Test 1: Extension hooks
;; ============================================================
(printf "--- Test 1: Extension hooks ---\n")
(define hooks (extension-hooks gsd-planning-extension))
(for ([h '(register-tools register-shortcuts
                          execute-command
                          tool-call-pre
                          tool-result-post
                          session-shutdown)])
  (printf "  ~a: ~a\n" h (if (hash-has-key? hooks h) "OK" "MISSING")))
(printf "\n")

;; ============================================================
;; Test 2: State machine
;; ============================================================
(printf "--- Test 2: State machine ---\n")
(printf "  initial: ~a (gsm: ~a)\n" (gsd-mode) (gsm-current))
(set-gsd-mode! 'planning)
(printf "  planning: ~a (gsm: ~a)\n" (gsd-mode) (gsm-current))
(set-gsd-mode! 'executing)
(printf "  executing: ~a (gsm: ~a)\n" (gsd-mode) (gsm-current))
(reset-all-gsd-state!)
(printf "  reset: ~a (gsm: ~a)\n" (gsd-mode) (gsm-current))
(printf "\n")

;; ============================================================
;; Test 3: Tool guard
;; ============================================================
(printf "--- Test 3: Tool guard ---\n")
(set-pinned-planning-dir! test-dir)
(define guard-hook (hash-ref hooks 'tool-call-pre))

(set-gsd-mode! 'planning)
(printf "  planning+read: ~a\n"
        (hook-result-action
         (guard-hook (hasheq 'tool-name "read" 'arguments (hasheq 'path "foo.rkt")))))

(set-gsd-mode! 'plan-written)
(define ed-r (guard-hook (hasheq 'tool-name "edit" 'arguments (hasheq 'path "foo.rkt"))))
(printf "  plan-written+edit: ~a\n" (hook-result-action ed-r))

(set-gsd-mode! 'executing)
(define pw-r (guard-hook (hasheq 'tool-name "planning-write" 'arguments (hasheq 'artifact "PLAN"))))
(printf "  executing+planning-write: ~a\n" (hook-result-action pw-r))

(printf "  executing+edit: ~a\n"
        (hook-result-action
         (guard-hook (hasheq 'tool-name "edit" 'arguments (hasheq 'path "foo.rkt")))))

(reset-all-gsd-state!)
(printf "\n")

;; ============================================================
;; Test 4: /plan command
;; ============================================================
(printf "--- Test 4: /plan command ---\n")
(reset-all-gsd-state!)
(set-pinned-planning-dir! test-dir)
(define cmd-hook (hash-ref hooks 'execute-command))
(define plan-r (cmd-hook (hasheq 'command "/plan" 'input "/plan fix the bug")))
(printf "  action: ~a\n" (hook-result-action plan-r))
(when (eq? 'amend (hook-result-action plan-r))
  (define s (hash-ref (hook-result-payload plan-r) 'submit #f))
  (printf "  submit: ~a\n" (if s "present" "missing"))
  (when s
    (printf "  stale warning: ~a\n" (string-contains? s "existing PLAN.md"))
    (printf "  mode: ~a (gsm: ~a)\n" (gsd-mode) (gsm-current))))
(printf "\n")

;; ============================================================
;; Test 5: /go command
;; ============================================================
(printf "--- Test 5: /go command ---\n")
(reset-all-gsd-state!)
(set-pinned-planning-dir! test-dir)
(define go-r (cmd-hook (hasheq 'command "/go" 'input "/go")))
(printf "  action: ~a\n" (hook-result-action go-r))
(when (eq? 'amend (hook-result-action go-r))
  (define s (hash-ref (hook-result-payload go-r) 'submit #f))
  (when s
    (printf "  [gsd-planning]: ~a\n" (string-contains? s "[gsd-planning]"))
    (printf "  IMPLEMENT NOW: ~a\n" (string-contains? s "IMPLEMENT NOW"))
    (printf "  Wave 0: ~a\n" (string-contains? s "Wave 0"))
    (printf "  Wave 1: ~a\n" (string-contains? s "Wave 1"))
    (printf "  Error recovery: ~a\n" (string-contains? s "Failed waves do NOT block")))
  (printf "  total-waves: ~a\n" (total-waves))
  (printf "  mode: ~a (gsm: ~a)\n" (gsd-mode) (gsm-current)))
(printf "\n")

;; ============================================================
;; Test 6: /gsd status
;; ============================================================
(printf "--- Test 6: /gsd status ---\n")
(define gsd-r (cmd-hook (hasheq 'command "/gsd" 'input "/gsd")))
(printf "  action: ~a\n" (hook-result-action gsd-r))
(when (eq? 'amend (hook-result-action gsd-r))
  (define t (hash-ref (hook-result-payload gsd-r) 'text ""))
  (printf "  status (200): ~a\n" (substring t 0 (min 200 (string-length t)))))
(printf "\n")

;; ============================================================
;; Test 7: Bash detection
;; ============================================================
(printf "--- Test 7: Bash detection ---\n")
(reset-all-gsd-state!)
(set-gsd-mode! 'executing)
(set-pinned-planning-dir! test-dir)
(define result-hook (hash-ref hooks 'tool-result-post))

(define sed-r
  (result-hook (hasheq 'tool-name
                       "bash"
                       'arguments
                       (hasheq 'command "sed -n '10,20p' foo.rkt")
                       'result
                       (ok-result "line 10\n"))))
(printf "  sed -n: ~a\n" (hook-result-action sed-r))

(define git-r
  (result-hook (hasheq 'tool-name
                       "bash"
                       'arguments
                       (hasheq 'command "git status")
                       'result
                       (ok-result "nothing"))))
(printf "  git status: ~a\n" (hook-result-action git-r))
(reset-all-gsd-state!)
(printf "\n")

;; ============================================================
;; Test 8: Steering stall (with path in details)
;; ============================================================
(printf "--- Test 8: Steering stall ---\n")
(reset-all-gsd-state!)
(set-gsd-mode! 'executing)
(set-pinned-planning-dir! test-dir)

;; Feed 4 identical reads with proper result (path in details)
(for ([i (in-range 4)])
  (result-hook (hasheq 'tool-name
                       "read"
                       'arguments
                       (hasheq 'path "same.rkt")
                       'result
                       (read-result "file content" "same.rkt"))))

;; 5th read should trigger stall
(define stall-r
  (result-hook (hasheq 'tool-name
                       "read"
                       'arguments
                       (hasheq 'path "same.rkt")
                       'result
                       (read-result "file content" "same.rkt"))))
(printf "  After 5 identical reads: ~a\n" (hook-result-action stall-r))
(when (eq? 'amend (hook-result-action stall-r))
  (define t (hash-ref (hook-result-payload stall-r) 'text ""))
  (printf "  stall msg (first 120): ~a\n" (substring t 0 (min 120 (string-length t)))))
(reset-all-gsd-state!)
(printf "\n")

;; ============================================================
;; Test 9: Session shutdown
;; ============================================================
(printf "--- Test 9: Session shutdown ---\n")
(set-gsd-mode! 'executing)
(set-total-waves! 5)
(printf "  before: mode=~a waves=~a\n" (gsd-mode) (total-waves))
(define shutdown-hook (hash-ref hooks 'session-shutdown))
(shutdown-hook (hasheq))
(printf "  after: mode=~a waves=~a\n" (gsd-mode) (total-waves))
(printf "\n")

;; ============================================================
;; Test 10: Planning tools
;; ============================================================
(printf "--- Test 10: Planning tools ---\n")
(reset-all-gsd-state!)
(set-pinned-planning-dir! test-dir)
(define reg (make-tool-registry))
(register-default-tools! reg)
(define ctx
  (make-extension-ctx #:session-id "sdk-test"
                      #:session-dir test-dir
                      #:event-bus (make-event-bus)
                      #:extension-registry (make-extension-registry)
                      #:tool-registry reg
                      #:command-registry (box (hash))
                      #:working-directory test-dir))
(define reg-hook (hash-ref hooks 'register-tools))
(reg-hook ctx (hasheq))

(define pr (lookup-tool reg "planning-read"))
(define pw (lookup-tool reg "planning-write"))
(printf "  read STATE: ~a\n"
        (if (tool-result-is-error? ((tool-execute pr) (hasheq 'artifact "STATE") #f)) "ERR" "OK"))
(printf "  read PLAN: ~a\n"
        (if (tool-result-is-error? ((tool-execute pr) (hasheq 'artifact "PLAN") #f)) "ERR" "OK"))
(printf "  write VALIDATION: ~a\n"
        (if (tool-result-is-error? ((tool-execute pw) (hasheq 'artifact "VALIDATION" 'content "ok")
                                                      #f))
            "ERR"
            "OK"))

(set-gsd-mode! 'executing)
(define plan-block-r ((tool-execute pw) (hasheq 'artifact "PLAN" 'content "hax") #f))
(printf "  write PLAN (executing): ~a\n"
        (if (tool-result-is-error? plan-block-r) "BLOCKED OK" "ALLOWED FAIL"))
(printf "\n")

;; ============================================================
;; Test 11: SDK session lifecycle (basic smoke test)
;; ============================================================
(printf "--- Test 11: SDK session lifecycle ---\n")
(reset-all-gsd-state!)
(with-handlers ([exn:fail? (lambda (e) (printf "  Error: ~a\n" (exn-message e)))])
  (define settings (load-settings))
  (printf "  Settings: OK\n")
  (define provider (build-provider (hasheq) settings))
  (printf "  Provider: ~a\n" (object-name provider))
  (define sdk-dir (make-temporary-file "sdk-sess-~a" (quote directory)))
  (define rt
    (make-runtime #:provider provider
                  #:session-dir sdk-dir
                  #:register-default-tools? #t
                  #:auto-load-extensions? #f))
  (printf "  Runtime: OK\n")
  ;; open-session without ID creates new session
  (define sess (open-session rt))
  (printf "  Session opened: OK\n")
  ;; close-session! needs agent-session? not runtime?
  ;; For this smoke test, just verify we got this far
  (printf "  Closed: skipped (requires agent-session cast)\n")
  (delete-directory/files sdk-dir))
(printf "\n")

;; Cleanup
(reset-all-gsd-state!)
(delete-directory/files test-dir)
(printf "=== All 11 SDK integration tests completed ===\n")
