#!/usr/bin/env racket
#lang racket/base

;; scripts/test-gsd-go-replanning.rkt
;;
;; Reproduce the bug where /go execution falls back into planning mode.

(require racket/port
         racket/string
         racket/list
         "../llm/provider.rkt"
         "../agent/event-bus.rkt"
         (only-in "../tools/tool.rkt" make-tool-registry tool-registry?)
         (only-in "../tools/registry-defaults.rkt" register-default-tools!)
         (only-in "../extensions/gsd-planning-state.rkt" gsd-mode set-gsd-mode! reset-all-gsd-state!)
         (only-in "../extensions/api.rkt" make-extension-registry extension-registry?)
         (only-in "../extensions/loader.rkt" load-extension! discover-extension-files)
         (only-in "../extensions/hooks.rkt" dispatch-hooks hook-result? hook-result-payload)
         (only-in "../util/hook-types.rkt" hook-result-action))

(define ext-dir (build-path (find-system-path 'home-dir) "src/q-agent/q/extensions"))

(define (setup)
  (reset-all-gsd-state!)
  (define bus (make-event-bus))
  (define reg (make-tool-registry))
  (register-default-tools! reg)
  (define ext-reg (make-extension-registry))
  (for ([ext-path (discover-extension-files (list ext-dir))])
    (load-extension! ext-reg (cdr ext-path) #:event-bus bus))
  (values bus reg ext-reg))

(define (write-test-plan content)
  (define plan-dir (build-path (find-system-path 'home-dir) "src/q-agent/q/.planning"))
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
   (string-append
    "# Plan: Test\n\n"
    "## Wave 1: Fix foo\n- old-text: def overview_page\n- new-text: def new_page\n\n"
    "## Wave 2: Fix bar\n- old-text: def timeline_page\n- new-text: def new_timeline\n"))

  (define payload (hasheq 'command "/go" 'input "/go"))
  (define result (dispatch-hooks 'execute-command payload ext-reg))

  (define p (hook-result-payload result))
  (define submit-text (and (hash? p) (hash-ref p 'submit #f)))
  (define display-text (and (hash? p) (hash-ref p 'text #f)))
  (printf "Display text: ~a\n" display-text)
  (printf "Submit text length: ~a chars\n"
          (if submit-text
              (string-length submit-text)
              0))
  (when submit-text
    (printf "Submit text first 600 chars:\n~a\n...\n"
            (substring submit-text 0 (min 600 (string-length submit-text))))
    (printf "Contains 'EXECUTE the plan': ~a\n" (string-contains? submit-text "EXECUTE the plan"))
    (printf "Contains 'do NOT explore': ~a\n" (string-contains? submit-text "do NOT explore"))
    (printf "Contains 'Do NOT run read-only tools': ~a\n"
            (string-contains? submit-text "Do NOT run read-only tools"))
    (printf "Contains plan waves: ~a\n" (string-contains? submit-text "Wave 1: Fix foo"))))

(define (test-mode-transitions)
  (displayln "\n=== Test 2: GSD mode transitions ===")
  (define-values (bus reg ext-reg) (setup))

  (printf "Initial mode: ~a\n" (gsd-mode))
  (set-gsd-mode! 'planning)
  (printf "After set planning: ~a\n" (gsd-mode))
  (set-gsd-mode! 'plan-written)
  (printf "After set plan-written: ~a\n" (gsd-mode))
  (set-gsd-mode! 'executing)
  (printf "After set executing: ~a\n" (gsd-mode)))

(define (test-tool-blocking)
  (displayln "\n=== Test 3: Tool blocking during executing ===")
  (define-values (bus reg ext-reg) (setup))
  (set-gsd-mode! 'executing)

  ;; planning-write should be blocked
  (define pw-payload
    (hasheq 'tool-name "planning-write" 'tool-arguments (hash 'artifact "PLAN" 'content "# New")))
  (define pw-result (dispatch-hooks 'tool-call-pre pw-payload ext-reg))
  (printf "planning-write: action=~a ~a\n"
          (result-action pw-result)
          (if (blocked? pw-result)
              (format "BLOCKED: ~a" (hook-result-payload pw-result))
              "PASSED"))

  ;; write to /tmp should be ALLOWED
  (define write-payload
    (hasheq 'tool-name "write" 'tool-arguments (hash 'path "/tmp/test.txt" 'content "hello")))
  (define write-result (dispatch-hooks 'tool-call-pre write-payload ext-reg))
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
  (set-gsd-mode! 'executing)

  ;; Agent uses write tool to overwrite PLAN.md
  (define plan-path "/home/user/src/q-agent/q/.planning/PLAN.md")
  (define write-payload
    (hasheq 'tool-name
            "write"
            'tool-arguments
            (hash 'path plan-path 'content "# New plan via write!")))
  (define write-result (dispatch-hooks 'tool-call-pre write-payload ext-reg))
  (printf "write PLAN.md during executing: action=~a ~a\n"
          (result-action write-result)
          (if (blocked? write-result)
              "BLOCKED (good)"
              "ALLOWED — BUG: agent can rewrite PLAN.md via write tool!")))

;; Run all tests
(test-go-prompt)
(test-mode-transitions)
(test-tool-blocking)
(test-write-bypass-plan)

(displayln "\n=== All tests complete ===")
