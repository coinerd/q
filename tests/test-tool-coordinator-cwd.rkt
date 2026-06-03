#lang racket

;; BOUNDARY: integration

;; test-tool-coordinator-cwd.rkt — BUG-4 regression tests
;; Validates that tool execution CWD prefers project dir over session dir.
;; Tests via bash tool which respects exec-ctx working-directory.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/port
         (only-in "../tools/tool.rkt"
                  make-tool
                  make-tool-call
                  make-success-result
                  tool-result-content
                  tool-result-is-error?)
         (only-in "../tools/exec-context.rkt" make-exec-context)
         (only-in "../tools/registry.rkt" make-tool-registry register-tool!)
         (only-in "../tools/scheduler.rkt" run-tool-batch scheduler-result-results)
         (only-in "../tools/builtins/bash.rkt" tool-bash)
         "helpers/fixtures.rkt")

(define (result-content-text r)
  (define parts (tool-result-content r))
  (string-join (for/list ([part (in-list parts)])
                 (cond
                   [(string? part) part]
                   [(hash? part) (hash-ref part 'text "")]
                   [else (~a part)]))
               ""))

(define (make-bash-tool)
  (make-tool "bash"
             "Execute a shell command"
             (hasheq 'type
                     "object"
                     'required
                     '("command")
                     'properties
                     (hasheq 'command (hasheq 'type "string" 'description "Shell command to run")))
             tool-bash))

(define coordinator-cwd-tests
  (test-suite "tool-coordinator-cwd"

    (test-case "bash tool uses exec-ctx working-directory"
      (with-temp-dir
       (lambda (dir)
         ;; Create a unique file in temp dir
         (call-with-output-file (build-path dir "unique-marker-cwd.txt")
                                (lambda (out) (display "test" out))
                                #:exists 'replace)
         ;; Register bash tool
         (define reg (make-tool-registry))
         (register-tool! reg (make-bash-tool))
         ;; Create exec-ctx pointing to our temp dir
         (define ctx (make-exec-context #:working-directory dir))
         ;; Run ls via bash — should list from our temp dir
         (define tc (make-tool-call "bash-1" "bash" (hasheq 'command "ls unique-marker-cwd.txt")))
         (define result (run-tool-batch (list tc) reg #:exec-context ctx))
         (define results (scheduler-result-results result))
         (define content (result-content-text (car results)))
         (check-true (string-contains? content "unique-marker-cwd.txt")
                     (format "Expected 'unique-marker-cwd.txt' in bash ls output, got: ~a"
                             content)))))

    (test-case "bash tool with current-directory succeeds"
      (define reg (make-tool-registry))
      (register-tool! reg (make-bash-tool))
      (define ctx (make-exec-context #:working-directory (current-directory)))
      (define tc (make-tool-call "bash-2" "bash" (hasheq 'command "echo ok")))
      (define result (run-tool-batch (list tc) reg #:exec-context ctx))
      (define results (scheduler-result-results result))
      (check-false (tool-result-is-error? (car results))))))

(run-tests coordinator-cwd-tests)
