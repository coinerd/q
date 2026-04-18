#lang racket/base

;; examples/tests/test-examples.rkt — validation harness for extension examples
;;
;; Tests that every example extension:
;;   1. Loads without error
;;   2. Provides a valid the-extension binding
;;   3. Has the expected extension struct fields
;;   4. All declared hooks are registered hook points

(require rackunit
         rackunit/text-ui
         racket/runtime-path
         racket/list
         "../../extensions/api.rkt"
         "../../extensions/hooks.rkt"
         "../../util/hook-types.rkt")

;; ============================================================
;; Example definitions
;; ============================================================

(define-runtime-path examples-dir "../extensions")

;; List of all example extension files
(define example-files
  '("hello-world.rkt"
    "custom-tool.rkt"
    "custom-command.rkt"
    "context-enricher.rkt"
    "tool-guard.rkt"
    "response-logger.rkt"
    "session-lifecycle.rkt"
    "streaming-observer.rkt"
    "provider-hook.rkt"
    "multi-hook.rkt"
    "define-extension-dsl.rkt"
    "error-handler.rkt"
    ;; Wave 3 (#1215-#1218)
    "custom-provider.rkt"
    "session-state.rkt"
    "tui-widget.rkt"
    "keyboard-shortcut.rkt"))

;; ============================================================
;; Validation tests
;; ============================================================

(define-test-suite example-loading-tests
  ;; Test each example loads and provides a valid extension
  (for ([filename (in-list example-files)])
    (test-case (format "Loads without error: ~a" filename)
      (define path (build-path examples-dir filename))
      (check-true (file-exists? path)
                  (format "Example file exists: ~a" filename))
      (define ext
        (with-handlers ([exn:fail?
                         (lambda (e)
                           (fail (format "Load failed: ~a — ~a"
                                         filename (exn-message e))))])
          (dynamic-require path 'the-extension)))
      (when ext
        ;; Verify it's an extension struct
        (check-true (extension? ext)
                    (format "~a provides a valid extension struct" filename))
        ;; Verify required fields
        (check-true (string? (extension-name ext))
                    (format "~a: name is a string" filename))
        (check-true (string? (extension-version ext))
                    (format "~a: version is a string" filename))
        (check-true (string? (extension-api-version ext))
                    (format "~a: api-version is a string" filename))
        ;; Verify hooks is a hash
        (check-true (hash? (extension-hooks ext))
                    (format "~a: hooks is a hash" filename))))))

(define-test-suite example-hook-validation-tests
  ;; Test that all hook points used in examples are registered schemas
  (for ([filename (in-list example-files)])
    (test-case (format "All hooks are valid: ~a" filename)
      (define path (build-path examples-dir filename))
      (define ext
        (with-handlers ([exn:fail? (lambda (e) #f)])
          (dynamic-require path 'the-extension)))
      (when ext
        (define hooks (extension-hooks ext))
        (for ([(hook-point handler) (in-hash hooks)])
          (check-true (valid-hook-name? hook-point)
                      (format "~a: hook '~a is a registered hook point"
                              filename hook-point)))))))

(define-test-suite example-specific-tests
  ;; #1212: hello-world has turn-start hook
  (test-case "hello-world registers turn-start"
    (define ext (dynamic-require (build-path examples-dir "hello-world.rkt") 'the-extension))
    (check-not-false (hash-has-key? (extension-hooks ext) 'turn-start)))

  ;; #1213: custom-tool has register-tools hook
  (test-case "custom-tool registers register-tools"
    (define ext (dynamic-require (build-path examples-dir "custom-tool.rkt") 'the-extension))
    (check-not-false (hash-has-key? (extension-hooks ext) 'register-tools)))

  ;; #1214: custom-command has input hook
  (test-case "custom-command registers input"
    (define ext (dynamic-require (build-path examples-dir "custom-command.rkt") 'the-extension))
    (check-not-false (hash-has-key? (extension-hooks ext) 'input)))

  ;; #1215: custom-provider has extension.loaded hook
  (test-case "custom-provider registers extension.loaded"
    (define ext (dynamic-require (build-path examples-dir "custom-provider.rkt") 'the-extension))
    (check-not-false (hash-has-key? (extension-hooks ext) 'extension.loaded)))

  ;; #1216: session-state has session.loaded hook
  (test-case "session-state registers session.loaded"
    (define ext (dynamic-require (build-path examples-dir "session-state.rkt") 'the-extension))
    (check-not-false (hash-has-key? (extension-hooks ext) 'session.loaded)))

  ;; #1217: tui-widget has turn-start hook
  (test-case "tui-widget registers turn-start"
    (define ext (dynamic-require (build-path examples-dir "tui-widget.rkt") 'the-extension))
    (check-not-false (hash-has-key? (extension-hooks ext) 'turn-start)))

  ;; #1218: keyboard-shortcut has register-shortcuts hook
  (test-case "keyboard-shortcut registers register-shortcuts"
    (define ext (dynamic-require (build-path examples-dir "keyboard-shortcut.rkt") 'the-extension))
    (check-not-false (hash-has-key? (extension-hooks ext) 'register-shortcuts)))

  ;; Test that all 16 examples exist
  (test-case "all 16 examples exist"
    (check-equal? (length example-files) 16)))

(define-test-suite example-registration-tests
  ;; Test that examples can be registered in an extension registry
  (test-case "all examples register without error"
    (define reg (make-extension-registry))
    (define loaded-names '())
    (for ([filename (in-list example-files)])
      (define path (build-path examples-dir filename))
      (with-handlers ([exn:fail?
                       (lambda (e)
                         (fail (format "Registration failed: ~a — ~a"
                                              filename (exn-message e))))])
        (define ext (dynamic-require path 'the-extension))
        (when ext
          (register-extension! reg ext)
          (set! loaded-names (cons (extension-name ext) loaded-names)))))
    ;; Verify all 16 registered
    (define all-exts (list-extensions reg))
    (check-equal? (length all-exts) 16
                  "All 16 extensions should register successfully")
    ;; Verify no duplicate names
    (define names (map extension-name all-exts))
    (define unique-names (remove-duplicates names))
    (check-equal? (length names) (length unique-names)
                  "No duplicate extension names")))

;; ============================================================
;; Run all tests
;; ============================================================

(run-tests
 (make-test-suite "Extension Example Gallery Tests"
   (list example-loading-tests
         example-hook-validation-tests
         example-specific-tests
         example-registration-tests)))
