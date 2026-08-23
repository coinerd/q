#lang racket/base

;; tests/test-extension-context-characterization.rkt — v0.99.88 W0
;; @speed fast
;; @suite default
;;
;; Extension context characterization and re-baselining (roadmap v0.99.88 W0).
;; Pins the CURRENT boundary/behavior so W1–W3 can isolate the concrete
;; Runtime services behind a host protocol without breaking semantics:
;;
;;   CH1  session-type boundary: util/extension/extension-types.rkt has no
;; @boundary unit
;;        runtime/ import (MA-03 closure evidence).
;;   CH2  extension-ctx struct surface: 16 documented fields, transparent.
;;   CH3  ctx-* provider wrappers register/update/list/lookup/unregister.
;;   CH4  null-registry error cases (ctx without provider-registry).
;;   CH5  registry idempotency + two-contexts-one-registry behavior.
;;   CH6  construction-root contract: register-session-extensions! null path
;;        and gsd-ctx wiring expression used by the session setup root.

(require rackunit
         racket/file
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         "../extensions/api.rkt"
         "../extensions/context.rkt"
         "../llm/model.rkt"
         "../llm/provider.rkt"
         "../runtime/extension-setup.rkt"
         "../runtime/extension-host-adapter.rkt"
         "../runtime/provider/provider-registry.rkt"
         "../tools/tool.rkt"
         "../util/event/event-bus.rkt"
         (only-in "../extensions/gsd/session-state.rkt"
                  current-gsd-ctx
                  gsd-session-ctx?
                  make-gsd-context))

(define-runtime-path extension-types-path "../util/extension/extension-types.rkt")

(define-runtime-path context-path "../extensions/context.rkt")

;; q/ root as absolute path (raco test cwd is tests/)
(define q-root (simplify-path (build-path (path-only (syntax-source #'here)) "..")))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (read-source path)
  (call-with-input-file path (lambda (in) (port->string in))))

(define (make-test-provider)
  (make-mock-provider (make-model-response (list (hasheq 'type "text" 'text "mock"))
                                           (hasheq 'input-tokens 10 'output-tokens 20)
                                           "mock-model"
                                           'stop)
                      #:name "test-provider"))

(define (make-basic-ctx #:provider-registry [registry #f])
  ;; W2: ctx-* wrappers delegate to an injected neutral provider-host-service.
  ;; Tests that exercise the registry path wrap a concrete registry once via
  ;; the Runtime adapter (make-provider-host-service).
  (make-extension-ctx #:session-id "char-test"
                      #:session-dir "/tmp"
                      #:event-bus (make-event-bus)
                      #:extension-registry (make-extension-registry)
                      #:provider-registry (if registry
                                              (make-provider-host-service registry)
                                              #f)))

;; ---------------------------------------------------------------------------
;; CH1 — session-type boundary (MA-03 closure evidence)
;; ---------------------------------------------------------------------------

(module+ test
  (test-case "CH1: extension-types.rkt carries no runtime/ import"
    (define src (read-source extension-types-path))
    (for ([line (in-list (string-split src "\n"))]
          #:when (regexp-match? #rx"^\\s*\\(require" line))
      ;; Any runtime/ path form on a require line is a boundary violation;
      ;; matches ../runtime/ and absolute forms alike.
      (check-false (string-contains? line "runtime/")
                   (format "runtime import leaked into pure types: ~a" line)))))

;; ---------------------------------------------------------------------------
;; CH2 — extension-ctx struct surface
;; ---------------------------------------------------------------------------

(module+ test
  (test-case "CH2: extension-ctx exposes the 17 documented read-only fields"
    (define ctx (make-basic-ctx))
    ;; struct->vector = tag + 17 fields (v0.99.88 W3 added package-service)
    (define vec (struct->vector ctx))
    (check-equal? (vector-ref vec 0) 'struct:extension-ctx)
    (check-equal? (vector-length vec) 18)
    ;; spot-check accessors resolve to the struct
    (check-equal? (ctx-session-id ctx) "char-test")
    (check-equal? (ctx-gsd-ctx ctx) #f)
    (check-false (ctx-package-service ctx) "package-service defaults to #f")))

;; ---------------------------------------------------------------------------
;; CH3 — ctx-* provider wrappers against a real registry
;; ---------------------------------------------------------------------------

(module+ test
  (test-case "CH3: ctx wrappers register/list/lookup/unregister via real registry"
    (define reg (make-provider-registry))
    (define ctx (make-basic-ctx #:provider-registry reg))
    (check-equal? (ctx-list-providers ctx) '())
    (check-equal? (ctx-register-provider! ctx "openai" (make-test-provider)) 'registered)
    (check-equal? (length (ctx-list-providers ctx)) 1)
    (check-true (provider-info? (ctx-lookup-provider ctx "openai")))
    (check-false (ctx-lookup-provider ctx "nonexistent"))
    (ctx-unregister-provider! ctx "openai")
    (check-equal? (ctx-list-providers ctx) '())))

;; ---------------------------------------------------------------------------
;; CH4 — null-registry error cases
;; ---------------------------------------------------------------------------

(module+ test
  (test-case "CH4: ctx without provider-registry degrades safely"
    (define ctx (make-basic-ctx))
    (define reg-result (ctx-register-provider! ctx "openai" (make-test-provider)))
    (check-true (hash-ref reg-result 'error #f))
    (check-true (string? (hash-ref reg-result 'message #f)))
    (check-equal? (ctx-list-providers ctx) '())
    (check-false (ctx-lookup-provider ctx "openai"))
    ;; unregister with missing registry is a no-op (void)
    (check-equal? (ctx-unregister-provider! ctx "openai") (void)))

  (test-case "CH4b: session-less ctx (closed/null session state) is registry-safe"
    ;; A ctx built without session-dir/session-store models closed/null session
    ;; state; registry operations must not touch session state.
    (define reg (make-provider-registry))
    (define ctx
      (make-extension-ctx #:session-id "closed"
                          #:session-dir #f
                          #:event-bus (make-event-bus)
                          #:extension-registry (make-extension-registry)
                          #:session-store #f
                          #:provider-registry (make-provider-host-service reg)))
    (check-equal? (ctx-register-provider! ctx "openai" (make-test-provider)) 'registered)
    (check-equal? (length (ctx-list-providers ctx)) 1)
    (check-true (provider-info? (ctx-lookup-provider ctx "openai")))
    ;; and with no registry at all it still degrades safely
    (define bare
      (make-extension-ctx #:session-id "closed"
                          #:session-dir #f
                          #:event-bus (make-event-bus)
                          #:extension-registry (make-extension-registry)))
    (check-equal? (ctx-list-providers bare) '())
    (check-false (ctx-lookup-provider bare "openai")))

  (test-case "CH4c: closed session-store accessor stays #f (no hidden state)"
    (define bare
      (make-extension-ctx #:session-id "closed"
                          #:session-dir #f
                          #:event-bus (make-event-bus)
                          #:extension-registry (make-extension-registry)))
    (check-false (ctx-session-store bare))))

;; ---------------------------------------------------------------------------
;; CH5 — registry idempotency and shared-registry concurrency semantics
;; ---------------------------------------------------------------------------

(module+ test
  (test-case "CH5: duplicate registration updates; unknown unregister is no-op"
    (define reg (make-provider-registry))
    (define ctx (make-basic-ctx #:provider-registry reg))
    (check-equal? (ctx-register-provider! ctx "openai" (make-test-provider)) 'registered)
    (check-equal? (ctx-register-provider! ctx "openai" (make-test-provider)) 'updated)
    (check-equal? (length (ctx-list-providers ctx)) 1)
    (ctx-unregister-provider! ctx "does-not-exist") ; must not raise
    (check-equal? (length (ctx-list-providers ctx)) 1))

  (test-case "CH5b: two contexts sharing one registry see each other's registrations"
    (define reg (make-provider-registry))
    (define ctx-a (make-basic-ctx #:provider-registry reg))
    (define ctx-b (make-basic-ctx #:provider-registry reg))
    (ctx-register-provider! ctx-a "openai" (make-test-provider))
    (ctx-register-provider! ctx-b "anthropic" (make-test-provider))
    (check-equal? (length (ctx-list-providers ctx-a)) 2)
    (check-true (provider-info? (ctx-lookup-provider ctx-b "openai")))))

;; ---------------------------------------------------------------------------
;; CH6 — construction-root contract
;; ---------------------------------------------------------------------------

(module+ test
  (test-case "CH6: register-session-extensions! null path returns no tools"
    (check-equal? (register-session-extensions! (make-tool-registry) #f (make-event-bus) "sess-1")
                  '()))

  (test-case "CH6b: session setup ctx expression wires gsd-ctx (C-01 contract)"
    ;; This is exactly the ctx expression runtime/extension-setup.rkt builds;
    ;; pin that the field carries a gsd-ctx when the parameter is set.
    (define probe-ctx (make-gsd-context))
    (parameterize ([current-gsd-ctx probe-ctx])
      (define ctx
        (make-extension-ctx #:session-id "sess-1"
                            #:session-dir #f
                            #:event-bus (make-event-bus)
                            #:extension-registry (make-extension-registry)
                            #:tool-registry (make-tool-registry)
                            #:gsd-ctx (current-gsd-ctx)))
      (check-true (gsd-session-ctx? (ctx-gsd-ctx ctx)))
      (check-eq? (ctx-gsd-ctx ctx) probe-ctx))))

;; ---------------------------------------------------------------------------
;; CH7 — session-switch construction root (rebind/resume path)
;; ---------------------------------------------------------------------------

(module+ test
  (test-case "CH7: session-switch rebind ctx expression is a valid extension-ctx"
    ;; runtime/session/session-switch.rkt `make-rebind-ctx` builds the new
    ;; session ctx with exactly these kwargs for every switch reason
    ;; (new/resume/fork). Pin that the factory contract accepts them and the
    ;; resulting ctx carries the switch fields.
    (define bus (make-event-bus))
    (define ctx
      (make-extension-ctx #:session-id "switched-session"
                          #:session-dir "/tmp/switched"
                          #:event-bus bus
                          #:extension-registry (make-extension-registry)
                          #:model-name "model-x"
                          #:working-directory "/work"))
    (check-true (extension-ctx? ctx))
    (check-equal? (ctx-session-id ctx) "switched-session")
    (check-equal? (ctx-session-dir ctx) "/tmp/switched")
    (check-equal? (ctx-model ctx) "model-x")
    (check-equal? (ctx-cwd ctx) "/work")
    (check-eq? (ctx-event-bus ctx) bus)
    ;; resume keeps the same construction contract
    (define resumed
      (make-extension-ctx #:session-id "switched-session"
                          #:session-dir "/tmp/switched"
                          #:event-bus bus
                          #:extension-registry (make-extension-registry)))
    (check-equal? (ctx-session-id resumed) "switched-session")))

;; ---------------------------------------------------------------------------
;; CH8 — production consumer inventory of ctx-* provider wrappers
;; ---------------------------------------------------------------------------

(module+ test
  (test-case "CH8: ctx-* provider wrappers have zero production consumers"
    (define wrapper-names
      '("ctx-register-provider!" "ctx-unregister-provider!"
                                 "ctx-list-providers"
                                 "ctx-lookup-provider"))
    (define roots '("extensions" "runtime" "wiring" "agent" "tools" "tui" "interfaces"))
    (define violations
      (for*/list ([root (in-list roots)]
                  [f (in-directory (build-path q-root root))]
                  #:when (and (string-suffix? (path->string f) ".rkt")
                              (not (string-contains? (path->string f) "/compiled/")))
                  [src (in-value (read-source f))]
                  [w (in-list wrapper-names)]
                  #:when (and (string-contains? src w)
                              (not (string-contains? (path->string f) "context.rkt"))))
        (list (path->string f) w)))
    (check-equal? violations
                  '()
                  (format "ctx-* provider wrappers must have no production consumers: ~v"
                          violations))))
