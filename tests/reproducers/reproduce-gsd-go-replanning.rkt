#!/usr/bin/env racket
#lang racket/base

;; @speed slow
;; @suite gsd
;; tests/reproducers/test-gsd-go-replanning.rkt
;;
;; Reproduce the bug where /go execution falls back into planning mode.

(require rackunit
         racket/port
         racket/file
         racket/string
         racket/list
         racket/runtime-path
         "../../llm/provider.rkt"
         "../../util/event/event-bus.rkt"
         (only-in "../../tools/tool.rkt" make-tool-registry tool-registry?)
         (only-in "../../tools/registry-defaults.rkt" register-default-tools!)
         (only-in "../../extensions/gsd/session-state.rkt" current-gsd-mode current-gsd-ctx)
         (only-in "../../extensions/gsd/state-machine.rkt"
                  gsm-ctx-current
                  gsm-ctx-reset!
                  gsm-ctx-transition-to!
                  gsm-ctx-transition!)
         (only-in "../../extensions/gsd/core.rkt" reset-all-gsd-state!)
         (only-in "../../extensions/api.rkt" make-extension-registry extension-registry?)
         (only-in "../../extensions/loader.rkt" load-extension!)
         (only-in "../../extensions/hooks.rkt" dispatch-hooks hook-result? hook-result-payload)
         (only-in "../../util/hook-types.rkt" hook-result-action))

(define-runtime-path gsd-extension-path "../../extensions/gsd-planning.rkt")

(define (setup)
  (reset-all-gsd-state!)
  (define bus (make-event-bus))
  (define reg (make-tool-registry))
  (register-default-tools! reg)
  (define ext-reg (make-extension-registry))
  (load-extension! ext-reg gsd-extension-path #:event-bus bus)
  (values bus reg ext-reg))

(define (write-test-plan content)
  (define plan-dir (build-path (current-directory) ".planning"))
  (make-directory* plan-dir)
  (call-with-output-file (build-path plan-dir "PLAN.md")
                         (lambda (out) (display content out))
                         #:exists 'replace))

(define (result-action result)
  (if (hook-result? result)
      (hook-result-action result)
      result))

(define (blocked? result)
  (eq? (result-action result) 'block))

(define (test-go-prompt)
  (displayln "=== Test 1: What prompt does /go produce? ===")
  (define-values (bus reg ext-reg) (setup))

  (write-test-plan
   (string-append "# Plan: Test\n\n"
                  "## Wave 0: Fix foo\n- File: foo.rkt\n- Verify: raco test foo.rkt\n\n"
                  "## Wave 1: Fix bar\n- File: bar.rkt\n- Verify: raco test bar.rkt\n"))

  (define payload (hasheq 'command "/go" 'input "/go"))
  (define result (dispatch-hooks 'execute-command payload ext-reg))

  (define p (hook-result-payload result))
  (define execution-text (and (hash? p) (hash-ref p 'new-session #f)))
  (define display-text (and (hash? p) (hash-ref p 'text #f)))
  (printf "Display text: ~a\n" display-text)
  (printf "Execution text length: ~a chars\n"
          (if execution-text
              (string-length execution-text)
              0))
  (check-true (string? execution-text))
  (check-true (string-contains? execution-text "EXECUTE the plan"))
  (check-true (string-contains? execution-text "do NOT explore"))
  (check-true (string-contains? execution-text "Read each target file BEFORE editing it"))
  (when execution-text
    (printf "Execution text first 600 chars:\n~a\n...\n"
            (substring execution-text 0 (min 600 (string-length execution-text))))
    (printf "Contains 'EXECUTE the plan': ~a\n" (string-contains? execution-text "EXECUTE the plan"))
    (printf "Contains 'do NOT explore': ~a\n" (string-contains? execution-text "do NOT explore"))
    (printf "Requires reading target files: ~a\n"
            (string-contains? execution-text "Read each target file BEFORE editing it"))
    (printf "Contains plan waves: ~a\n" (string-contains? execution-text "Wave 0: Fix foo"))))

(define (test-mode-transitions)
  (displayln "\n=== Test 2: GSD mode transitions ===")
  (define-values (bus reg ext-reg) (setup))

  (define ctx (current-gsd-ctx))
  (printf "Initial mode: ~a\n" (current-gsd-mode))
  (gsm-ctx-transition-to! ctx 'exploring)
  (printf "After transition to exploring: ~a\n" (current-gsd-mode))
  (gsm-ctx-transition-to! ctx 'plan-written)
  (printf "After transition to plan-written: ~a\n" (current-gsd-mode))
  (gsm-ctx-transition-to! ctx 'executing)
  (printf "After transition to executing: ~a\n" (current-gsd-mode))
  (check-equal? (current-gsd-mode) 'executing))

(define (test-tool-blocking)
  (displayln "\n=== Test 3: Tool blocking during executing ===")
  (define-values (bus reg ext-reg) (setup))
  (gsm-ctx-transition-to! (current-gsd-ctx) 'executing)

  ;; planning-write should be blocked
  (define pw-payload
    (hasheq 'tool-name "planning-write" 'tool-arguments (hash 'artifact "PLAN" 'content "# New")))
  (define pw-result (dispatch-hooks 'tool-call-pre pw-payload ext-reg))
  (check-true (blocked? pw-result))
  (printf "planning-write: action=~a ~a\n"
          (result-action pw-result)
          (if (blocked? pw-result)
              (format "BLOCKED: ~a" (hook-result-payload pw-result))
              "PASSED"))

  ;; write to /tmp should be ALLOWED
  (define write-payload
    (hasheq 'tool-name "write" 'tool-arguments (hash 'path "/tmp/test.txt" 'content "hello")))
  (define write-result (dispatch-hooks 'tool-call-pre write-payload ext-reg))
  (check-false (blocked? write-result))
  (printf "write /tmp/test.txt: action=~a ~a\n"
          (result-action write-result)
          (if (blocked? write-result)
              (format "BLOCKED: ~a" (hook-result-payload write-result))
              "ALLOWED"))

  ;; edit should be ALLOWED
  (define edit-payload
    (hasheq 'tool-name
            "edit"
            'tool-arguments
            (hash 'path "/tmp/test.txt" 'old_text "foo" 'new_text "bar")))
  (define edit-result (dispatch-hooks 'tool-call-pre edit-payload ext-reg))
  (printf "edit /tmp/test.txt: action=~a ~a\n"
          (result-action edit-result)
          (if (blocked? edit-result)
              (format "BLOCKED: ~a" (hook-result-payload edit-result))
              "ALLOWED"))

  ;; bash should be ALLOWED
  (define bash-payload (hasheq 'tool-name "bash" 'tool-arguments (hash 'command "ls /tmp")))
  (define bash-result (dispatch-hooks 'tool-call-pre bash-payload ext-reg))
  (printf "bash ls: action=~a ~a\n"
          (result-action bash-result)
          (if (blocked? bash-result)
              (format "BLOCKED: ~a" (hook-result-payload bash-result))
              "ALLOWED"))

  ;; read should pass (budget not yet set to a number)
  (define read-payload (hasheq 'tool-name "read" 'tool-arguments (hash 'path "/tmp/test.txt")))
  (define read-result (dispatch-hooks 'tool-call-pre read-payload ext-reg))
  (printf "read /tmp/test.txt (no budget set): action=~a ~a\n"
          (result-action read-result)
          (if (blocked? read-result)
              (format "BLOCKED: ~a" (hook-result-payload read-result))
              "ALLOWED")))

(define (test-write-bypass-plan)
  (displayln "\n=== Test 4: Can agent bypass planning-write guard via write tool? ===")
  (define-values (bus reg ext-reg) (setup))
  (gsm-ctx-transition-to! (current-gsd-ctx) 'executing)

  ;; Agent uses write tool to overwrite PLAN.md
  (define plan-path (build-path (current-directory) ".planning" "PLAN.md"))
  (define write-payload
    (hasheq 'tool-name
            "write"
            'tool-arguments
            (hash 'path plan-path 'content "# New plan via write!")))
  (define write-result (dispatch-hooks 'tool-call-pre write-payload ext-reg))
  (check-true (blocked? write-result))
  (define windows-write-result
    (dispatch-hooks 'tool-call-pre
                    (hasheq 'tool-name
                            "write"
                            'tool-arguments
                            (hash 'path "C:\\repo\\.planning\\PLAN.md" 'content "# bypass"))
                    ext-reg))
  (check-true (blocked? windows-write-result))
  (printf "write PLAN.md during executing: action=~a ~a\n"
          (result-action write-result)
          (if (blocked? write-result)
              "BLOCKED (good)"
              "ALLOWED — BUG: agent can rewrite PLAN.md via write tool!")))

;; Run all tests in an isolated project directory.
(define test-project-dir (make-temporary-file "q-gsd-replanning-~a" 'directory))
(dynamic-wind void
              (lambda ()
                (parameterize ([current-directory test-project-dir])
                  (test-case "go prompt is execution-oriented"
                    (test-go-prompt))
                  (test-case "gsd mode transitions reach executing"
                    (test-mode-transitions))
                  (test-case "executing mode blocks planning-write only"
                    (test-tool-blocking))
                  (test-case "generic write cannot bypass planning artifact guard"
                    (test-write-bypass-plan))
                  (displayln "\n=== All tests complete ===")))
              (lambda () (delete-directory/files test-project-dir)))
