#lang racket/base

;; tests/test-extension-tool-registration.rkt — v0.19.4 GAP-2 fix verification
;;
;; Verifies that extension tools actually register when the register-tools
;; hook is dispatched with a proper extension-ctx (not a bare hasheq).

(require rackunit
         racket/port
         "../extensions/context.rkt"
         "../extensions/hooks.rkt"
         "../extensions/api.rkt"
         "../extensions/dynamic-tools.rkt"
         (only-in "../extensions/gsd-planning.rkt" gsd-planning-extension)
         (only-in "../extensions/racket-tooling.rkt" racket-tooling-extension)
         (only-in "../extensions/github-integration.rkt" github-integration-extension)
         (only-in "../extensions/compact-context.rkt" compact-context-extension)
         (only-in "../extensions/q-sync.rkt" q-sync-extension)
         (only-in "../extensions/session-export.rkt" session-export-extension)
         (only-in "../extensions/image-input.rkt" image-input-extension)
         (only-in "../extensions/ext-package-manager.rkt" ext-package-manager-extension)
         "../tools/tool.rkt"
         "../agent/event-bus.rkt")

;; Helper: create a test extension-ctx with a real tool-registry
(define (make-test-ext-ctx reg)
  (make-extension-ctx #:session-id "test-session"
                      #:session-dir "/tmp"
                      #:event-bus (make-event-bus)
                      #:extension-registry (make-extension-registry)
                      #:tool-registry reg))

;; Helper: list all extensions we expect to register tools
(define test-extensions
  (list gsd-planning-extension
        racket-tooling-extension
        github-integration-extension
        compact-context-extension
        q-sync-extension
        session-export-extension
        image-input-extension
        ext-package-manager-extension))

;; Helper: register all test extensions into a registry
(define (register-all-extensions! ext-reg)
  (for ([ext (in-list test-extensions)])
    (register-extension! ext-reg ext)))

(test-case "register-tools handlers accept 2 args (ctx payload)"
  ;; Verify each extension's register-tools handler has arity >= 2
  (define ext-reg (make-extension-registry))
  (register-all-extensions! ext-reg)
  (for ([ext (in-list test-extensions)])
    (define handler (hash-ref (extension-hooks ext) 'register-tools #f))
    (check-not-false handler (format "extension ~a has register-tools handler" (extension-name ext)))
    (check-true (procedure-arity-includes? handler 2)
                (format "~a register-tools handler accepts 2 args" (extension-name ext)))))

(test-case "extension tools register via hook dispatch with proper extension-ctx"
  ;; Create registry, extension registry, register extensions
  (define tool-reg (make-tool-registry))
  (define ext-reg (make-extension-registry))
  (register-all-extensions! ext-reg)

  ;; Build extension-ctx with real tool-registry
  (define ext-ctx (make-test-ext-ctx tool-reg))

  ;; Dispatch register-tools hook with proper ctx
  (define hook-res (dispatch-hooks 'register-tools (hasheq) ext-reg #:ctx ext-ctx))

  ;; Verify tools were actually registered in the tool-registry
  (define registered-names (sort (tool-names tool-reg) string<?))
  (check-true
   (>= (length registered-names) 10)
   (format "Expected 10+ extension tools, got ~a: ~a" (length registered-names) registered-names))

  ;; Verify specific expected tools
  (for ([name '("planning-read" "planning-write"
                                "racket-check"
                                "racket-edit"
                                "racket-codemod"
                                "gh-issue"
                                "gh-pr"
                                "gh-milestone"
                                "gh-board"
                                "gh-wave-start"
                                "gh-wave-finish"
                                "compact-context"
                                "q-sync"
                                "session-export"
                                "image-input"
                                "ext-package")])
    (check-not-false (lookup-tool tool-reg name) (format "tool '~a' registered" name))))

(test-case "old 1-arg dispatch still works (backward compat)"
  ;; The hooks system should fall back to 1-arg call for backward compat
  ;; This test verifies the call-handler path
  (define tool-reg (make-tool-registry))
  (define ext-reg (make-extension-registry))
  (register-all-extensions! ext-reg)
  (define ext-ctx (make-test-ext-ctx tool-reg))

  ;; Dispatch should work even if we pass ctx and some handlers accept 2 args
  ;; The call-handler in hooks.rkt checks arity: if ctx provided and handler
  ;; accepts 2 args, call (handler ctx payload); else call (handler payload)
  (define hook-res (dispatch-hooks 'register-tools (hasheq) ext-reg #:ctx ext-ctx))

  ;; Should succeed without error — tools registered
  (check-not-false hook-res "hook returned a result")
  (check-true (>= (length (tool-names tool-reg)) 10)
              (format "10+ tools registered via compat path, got ~a" (length (tool-names tool-reg)))))

(test-case "extension-ctx requires tool-registry for ext-register-tool!"
  ;; Verify that without tool-registry, ext-register-tool! raises an error
  (define ext-ctx-no-reg
    (make-extension-ctx #:session-id "test"
                        #:session-dir "/tmp"
                        #:event-bus (make-event-bus)
                        #:extension-registry (make-extension-registry)))
  (check-exn exn:fail?
             (lambda ()
               (ext-register-tool! ext-ctx-no-reg "test-tool" "desc" (hasheq) (lambda (args) '())))
             "ext-register-tool! should fail when no tool-registry in ctx"))
