#lang racket

;; tests/test-extension-tool-dispatch.rkt — Integration test for extension tool
;; dispatch through the tool scheduler.
;;
;; Verifies that extension tool handlers registered via ext-register-tool!
;; work correctly when called through the scheduler (which passes 2 args:
;; args + exec-ctx). This prevents arity mismatch regressions like the one
;; fixed in v0.19.8 where 8/16 handlers took only 1 arg.

(require rackunit
         racket/hash
         racket/string
         racket/file
         "../extensions/context.rkt"
         "../extensions/dynamic-tools.rkt"
         "../extensions/loader.rkt"
         "../extensions/api.rkt"
         "../extensions/gsd-planning.rkt"
         "../agent/event-bus.rkt"
         "../tools/tool.rkt"
         "../tools/scheduler.rkt"
         "../runtime/settings.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (result-text result)
  (define c (tool-result-content result))
  (cond
    [(string? c) c]
    [(list? c)
     (string-join (for/list ([item (in-list c)]
                             #:when (hash? item))
                    (hash-ref item 'text "")))]
    [else (format "~a" c)]))

(define (make-test-ctx #:tool-registry [reg #f])
  (define r (or reg (make-tool-registry)))
  (make-extension-ctx #:session-id "test-dispatch"
                      #:session-dir "/tmp"
                      #:event-bus (make-event-bus)
                      #:extension-registry (make-extension-registry)
                      #:tool-registry r))

;; ============================================================
;; 1-arg handler (most common extension pattern)
;; ============================================================

(test-case "1-arg handler registered via ext-register-tool! works through scheduler"
  (define reg (make-tool-registry))
  (define ctx (make-test-ctx #:tool-registry reg))
  ;; Register a handler that takes only 1 arg — the common extension pattern
  (ext-register-tool!
   ctx
   "test-1arg"
   "Test tool with 1-arg handler"
   (hasheq 'type "object" 'properties (hasheq))
   (lambda (args) (make-success-result (list (hasheq 'type "text" 'text "1-arg handler succeeded")))))
  ;; Call through the scheduler, which passes (args exec-ctx)
  (define t (lookup-tool reg "test-1arg"))
  (check-not-false t "tool registered")
  ;; The scheduler always calls with 2 args — simulate that
  (define result ((tool-execute t) (hasheq) #f))
  (check-false (tool-result-is-error? result) "should not be an error")
  (check-true (string-contains? (result-text result) "1-arg handler succeeded")
              "result should contain success message"))

(test-case "1-arg handler works with exec-ctx struct"
  ;; Same test but with an actual exec-context, not just #f
  (define reg (make-tool-registry))
  (define ctx (make-test-ctx #:tool-registry reg))
  (ext-register-tool! ctx
                      "test-1arg-ctx"
                      "Test tool with 1-arg handler and exec-ctx"
                      (hasheq 'type "object" 'properties (hasheq))
                      (lambda (args)
                        (make-success-result
                         (list (hasheq 'type "text" 'text "1-arg with ctx succeeded")))))
  (define t (lookup-tool reg "test-1arg-ctx"))
  (define exec-ctx (make-exec-context))
  (define result ((tool-execute t) (hasheq) exec-ctx))
  (check-false (tool-result-is-error? result) "should not be an error with exec-ctx")
  (check-true (string-contains? (result-text result) "1-arg with ctx succeeded")))

;; ============================================================
;; 2-arg handler (defense-in-depth pattern)
;; ============================================================

(test-case "2-arg handler with [exec-ctx #f] still works through scheduler"
  (define reg (make-tool-registry))
  (define ctx (make-test-ctx #:tool-registry reg))
  (ext-register-tool!
   ctx
   "test-2arg"
   "Test tool with 2-arg handler"
   (hasheq 'type "object" 'properties (hasheq))
   (lambda (args [exec-ctx #f])
     (make-success-result
      (list (hasheq 'type "text" 'text (format "2-arg handler succeeded, exec-ctx=~a" exec-ctx))))))
  (define t (lookup-tool reg "test-2arg"))
  ;; Wrapped by dynamic-tools.rkt, so the outer lambda takes (args exec-ctx)
  ;; and calls the inner lambda with just (args). The inner lambda has [exec-ctx #f]
  ;; as defense-in-depth but the wrapper won't pass exec-ctx.
  (define result ((tool-execute t) (hasheq) #f))
  (check-false (tool-result-is-error? result))
  (check-true (string-contains? (result-text result) "2-arg handler succeeded")))

;; ============================================================
;; Actual gsd-planning extension tool dispatch
;; ============================================================

(test-case "planning-write tool dispatches through scheduler without arity error"
  (define reg (make-tool-registry))
  (define ctx (make-test-ctx #:tool-registry reg))
  ;; Load gsd-planning extension
  (define hooks (extension-hooks gsd-planning-extension))
  (define register-handler (hash-ref hooks 'register-tools))
  (register-handler ctx (hasheq))
  ;; Verify planning-write is registered
  (define pw (lookup-tool reg "planning-write"))
  (check-not-false pw "planning-write should be registered")
  ;; Call through scheduler interface (2 args)
  (define tmpdir (make-temporary-file "planning-test-~a" 'directory))
  (define result
    ((tool-execute pw)
     (hasheq 'artifact "PLAN" 'content "# Test Plan\n## Goal\nVerify dispatch" 'base_dir tmpdir)
     #f))
  (check-false (tool-result-is-error? result)
               (format "planning-write should succeed, got: ~a" (result-text result)))
  ;; Clean up
  (delete-directory/files tmpdir))

(test-case "planning-read tool dispatches through scheduler without arity error"
  (define reg (make-tool-registry))
  (define ctx (make-test-ctx #:tool-registry reg))
  (define hooks (extension-hooks gsd-planning-extension))
  (define register-handler (hash-ref hooks 'register-tools))
  (register-handler ctx (hasheq))
  (define pr (lookup-tool reg "planning-read"))
  (check-not-false pr "planning-read should be registered")
  (define tmpdir (make-temporary-file "planning-read-test-~a" 'directory))
  ;; planning-read looks for artifacts in .planning/ subdirectory
  (make-directory (build-path tmpdir ".planning"))
  (call-with-output-file (build-path tmpdir ".planning" "PLAN.md")
                         (lambda (out) (display "# Test Plan Content" out))
                         #:exists 'replace)
  (define result ((tool-execute pr) (hasheq 'artifact "PLAN" 'base_dir tmpdir) #f))
  (check-false (tool-result-is-error? result)
               (format "planning-read should succeed, got: ~a" (result-text result)))
  (delete-directory/files tmpdir))
