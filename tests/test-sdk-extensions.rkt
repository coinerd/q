#lang racket

;; tests/test-sdk-extensions.rkt — tests for v0.20.5 W3: Extension Pre-Registration
;;
;; Covers:
;;   - Extension tools appear in list-tools after open-session
;;   - Extension state (event bus) is initialized before first run-prompt!
;;   - register-session-extensions! is idempotent
;;   - Works with create-agent-session too

(require rackunit
         racket/file
         "../interfaces/sdk.rkt"
         "../extensions/api.rkt"
         "../extensions/gsd-planning.rkt"
         "../extensions/hooks.rkt"
         "../agent/event-bus.rkt"
         "../tools/tool.rkt"
         "helpers/mock-provider.rkt"
         "helpers/temp-fs.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-ext-runtime)
  (define tmp (make-temporary-file "/tmp/sdk-ext-test-~a" 'directory))
  (define prov (make-simple-mock-provider
                (list (hasheq 'role "assistant"
                              'content (list (hasheq 'type "text" 'text "ok"))))))
  (define ext-reg (make-extension-registry))
  (register-extension! ext-reg the-extension)
  (define bus (make-event-bus))
  (define rt (make-runtime #:provider prov
                           #:session-dir tmp
                           #:extension-registry ext-reg
                           #:event-bus bus
                           #:register-default-tools? #t))
  (values rt tmp ext-reg bus))

(define (cleanup-ext! tmp)
  (delete-directory/files tmp #:must-exist? #f))

;; ============================================================
;; Tests
;; ============================================================

(test-case "W3: extension tools available after open-session (before run-prompt!)"
  (define-values (rt tmp ext-reg bus) (make-ext-runtime))
  ;; Before open-session, the tool registry has only default tools
  (define tools-before (list-tools (runtime-config-tool-registry (runtime-rt-config rt))))
  ;; Open session — should trigger extension pre-registration
  (define opened (open-session rt))
  ;; After open-session, extension tools should be registered
  (define tools-after (list-tools (runtime-config-tool-registry (runtime-rt-config opened))))
  (check-true (> (length tools-after) (length tools-before))
              "extension tools should be added after open-session")
  ;; Specifically check for planning-read and planning-write
  (define tool-names (map tool-name tools-after))
  (check-not-false (member "planning-read" tool-names)
              "planning-read should be registered")
  (check-not-false (member "planning-write" tool-names)
              "planning-write should be registered")
  (cleanup-ext! tmp))

(test-case "W3: GSD event bus is initialized after open-session"
  (reset-all-gsd-state!)
  (define-values (rt tmp ext-reg bus) (make-ext-runtime))
  ;; Before open-session, gsd-event-bus should be #f
  (check-false (gsd-event-bus))
  ;; Open session — extension registration sets the event bus
  (define opened (open-session rt))
  ;; After open-session, gsd-event-bus should be the runtime's bus
  (check-eq? (gsd-event-bus) bus)
  (reset-all-gsd-state!)
  (cleanup-ext! tmp))

(test-case "W3: register-session-extensions! is idempotent"
  (reset-all-gsd-state!)
  (define-values (rt tmp ext-reg bus) (make-ext-runtime))
  (define opened (open-session rt))
  (define tools-1 (list-tools (runtime-config-tool-registry (runtime-rt-config opened))))
  ;; Open another session — tools should still work, no duplicates
  (define opened2 (open-session rt))
  (define tools-2 (list-tools (runtime-config-tool-registry (runtime-rt-config opened2))))
  (check-equal? (length tools-1) (length tools-2)
                "tool count should be stable after re-registration")
  (reset-all-gsd-state!)
  (cleanup-ext! tmp))

(test-case "W3: works with create-agent-session"
  (reset-all-gsd-state!)
  (define tmp (make-temporary-file "/tmp/sdk-ext-test-~a" 'directory))
  (define prov (make-simple-mock-provider
                (list (hasheq 'role "assistant"
                              'content (list (hasheq 'type "text" 'text "ok"))))))
  (define ext-reg (make-extension-registry))
  (register-extension! ext-reg the-extension)
  (define bus (make-event-bus))
  (define rt (create-agent-session #:provider prov
                                   #:session-dir tmp
                                   #:extension-registry ext-reg
                                   #:event-bus bus))
  ;; Extension tools should be registered
  (define tools (list-tools (runtime-config-tool-registry (runtime-rt-config rt))))
  (define tool-names (map tool-name tools))
  (check-not-false (member "planning-read" tool-names)
              "planning-read should be registered via create-agent-session")
  ;; Event bus should be set
  (check-eq? (gsd-event-bus) bus)
  (reset-all-gsd-state!)
  (delete-directory/files tmp #:must-exist? #f))

(test-case "W3: no crash when no extension registry"
  (define tmp (make-temporary-file "/tmp/sdk-ext-test-~a" 'directory))
  (define prov (make-simple-mock-provider
                (list (hasheq 'role "assistant"
                              'content (list (hasheq 'type "text" 'text "ok"))))))
  ;; No extension registry — should not crash
  (define rt (make-runtime #:provider prov
                           #:session-dir tmp
                           #:register-default-tools? #t))
  (define opened (open-session rt))
  (check-true (runtime? opened))
  (delete-directory/files tmp #:must-exist? #f))
