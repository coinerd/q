#lang racket/base

;; tests/test-provider-registry-service-isolation.rkt — v0.99.88 W2
;; @speed fast
;; @suite default
;; @boundary unit
;;
;; Dual-run characterization (roadmap v0.99.88 W2 risk control):
;; the OLD direct registry path (concrete provider-registry calls) and the NEW
;; injected provider-host-service path (ctx-* facade delegating through the
;; neutral service) must produce IDENTICAL registration, lookup, errors, and
;; ordering. Also pins the compatibility facade surface.

(require rackunit
         racket/string
         "../extensions/context.rkt"
         "../llm/model.rkt"
         "../llm/provider.rkt"
         "../runtime/extension-host-adapter.rkt"
         (only-in "../runtime/provider/provider-registry.rkt"
                  make-provider-registry
                  register-provider!
                  unregister-provider!
                  list-providers
                  lookup-provider
                  provider-info?
                  provider-info-name)
         "../util/event/event-bus.rkt"
         "../util/extension/host-services.rkt")

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (make-test-provider)
  (make-mock-provider (make-model-response (list (hasheq 'type "text" 'text "mock"))
                                           (hasheq 'input-tokens 10 'output-tokens 20)
                                           "mock-model"
                                           'stop)
                      #:name "test-provider"))

;; A ctx whose injected registry value is a neutral provider-host-service
;; wrapping the given concrete registry (the W2 wiring pattern).
(define (make-ctx-with-service reg)
  (make-extension-ctx #:session-id "dual"
                      #:session-dir "/tmp"
                      #:event-bus (make-event-bus)
                      #:extension-registry #f
                      #:provider-registry (make-provider-host-service reg)))

(define (names infos)
  (map provider-info-name infos))

;; ---------------------------------------------------------------------------
;; D1 — registration parity
;; ---------------------------------------------------------------------------

(module+ test
  (test-case "D1: register via direct registry and via ctx service return identical results"
    (define p (make-test-provider))
    (define reg-direct (make-provider-registry))
    (define reg-svc (make-provider-registry))
    (define ctx (make-ctx-with-service reg-svc))
    (check-equal? (register-provider! reg-direct "openai" p) 'registered)
    (check-equal? (ctx-register-provider! ctx "openai" p) 'registered)
    (check-true (provider-info? (lookup-provider reg-direct "openai")))
    (check-true (provider-info? (lookup-provider reg-svc "openai")))
    (check-equal? (provider-info-name (lookup-provider reg-direct "openai"))
                  (provider-info-name (lookup-provider reg-svc "openai")))))

;; ---------------------------------------------------------------------------
;; D2 — re-registration (update) parity
;; ---------------------------------------------------------------------------

(module+ test
  (test-case "D2: re-registration returns 'updated on both paths"
    (define p (make-test-provider))
    (define reg-direct (make-provider-registry))
    (define reg-svc (make-provider-registry))
    (define ctx (make-ctx-with-service reg-svc))
    (register-provider! reg-direct "openai" p)
    (ctx-register-provider! ctx "openai" p)
    (check-equal? (register-provider! reg-direct "openai" p) 'updated)
    (check-equal? (ctx-register-provider! ctx "openai" p) 'updated)
    (check-equal? (length (list-providers reg-direct)) 1)
    (check-equal? (length (list-providers reg-svc)) 1)))

;; ---------------------------------------------------------------------------
;; D3 — lookup parity
;; ---------------------------------------------------------------------------

(module+ test
  (test-case "D3: lookup found/missing identical on both paths"
    (define p (make-test-provider))
    (define reg-direct (make-provider-registry))
    (define reg-svc (make-provider-registry))
    (define ctx (make-ctx-with-service reg-svc))
    (register-provider! reg-direct "openai" p)
    (ctx-register-provider! ctx "openai" p)
    (check-true (provider-info? (lookup-provider reg-direct "openai")))
    (check-true (provider-info? (ctx-lookup-provider ctx "openai")))
    (check-false (lookup-provider reg-direct "missing"))
    (check-false (ctx-lookup-provider ctx "missing"))))

;; ---------------------------------------------------------------------------
;; D4 — list ordering parity
;; ---------------------------------------------------------------------------

(module+ test
  (test-case "D4: list order preserved identically on both paths"
    (define reg-direct (make-provider-registry))
    (define reg-svc (make-provider-registry))
    (define ctx (make-ctx-with-service reg-svc))
    (for ([name (in-list '("alpha" "beta" "gamma" "delta"))])
      (register-provider! reg-direct name (make-test-provider))
      (ctx-register-provider! ctx name (make-test-provider)))
    (check-equal? (names (list-providers reg-direct))
                  (names (list-providers reg-svc))
                  "direct and adapter registries must agree on list order")
    (check-equal? (names (ctx-list-providers ctx))
                  (names (list-providers reg-svc))
                  "ctx facade must return exactly the registry's list order")
    (check-equal? (names (ctx-list-providers ctx)) (names (list-providers reg-direct)))))

;; ---------------------------------------------------------------------------
;; D5 — unregister parity
;; ---------------------------------------------------------------------------

(module+ test
  (test-case "D5: unregister and unknown-unregister no-op identical"
    (define reg-direct (make-provider-registry))
    (define reg-svc (make-provider-registry))
    (define ctx (make-ctx-with-service reg-svc))
    (register-provider! reg-direct "openai" (make-test-provider))
    (ctx-register-provider! ctx "openai" (make-test-provider))
    (unregister-provider! reg-direct "openai")
    (ctx-unregister-provider! ctx "openai")
    (check-false (lookup-provider reg-direct "openai"))
    (check-false (lookup-provider reg-svc "openai"))
    (check-equal? (ctx-list-providers ctx) '())
    ;; unknown unregister is a void no-op on both paths
    (check-equal? (unregister-provider! reg-direct "does-not-exist") (void))
    (check-equal? (ctx-unregister-provider! ctx "does-not-exist") (void))))

;; ---------------------------------------------------------------------------
;; D6 — error parity
;; ---------------------------------------------------------------------------

(module+ test
  (test-case "D6: invalid provider and non-string name raise contract errors on both paths"
    (define reg-direct (make-provider-registry))
    (define reg-svc (make-provider-registry))
    (define ctx (make-ctx-with-service reg-svc))
    (check-exn exn:fail:contract? (lambda () (register-provider! reg-direct "bad" 'not-a-provider)))
    (check-exn exn:fail:contract? (lambda () (ctx-register-provider! ctx "bad" 'not-a-provider)))
    (check-exn exn:fail:contract? (lambda () (register-provider! reg-direct 42 (make-test-provider))))
    (check-exn exn:fail:contract? (lambda () (ctx-register-provider! ctx 42 (make-test-provider)))))

  (test-case "D6b: invalid-provider validation semantics identical on both paths"
    (define reg-direct (make-provider-registry))
    (define reg-svc (make-provider-registry))
    (define ctx (make-ctx-with-service reg-svc))
    (define (msg thunk)
      (with-handlers ([exn:fail:contract? (lambda (e) (exn-message e))])
        (thunk)
        "no-exn"))
    (define direct-msg (msg (lambda () (register-provider! reg-direct "bad" 'not-a-provider))))
    (define svc-msg (msg (lambda () (ctx-register-provider! ctx "bad" 'not-a-provider))))
    ;; Same validation predicate (provider?), same given value, same error class.
    (check-true (and (string-contains? direct-msg "expected: provider?")
                     (string-contains? svc-msg "expected: provider?"))
                "both paths must validate provider?")
    (check-true (and (string-contains? direct-msg "given: 'not-a-provider")
                     (string-contains? svc-msg "given: 'not-a-provider"))
                "both paths must report the same offending value")
    ;; Boundary validation: the ctx facade contract fires before the registry
    ;; contract (same predicate, earlier blame). Root cause is identical.
    (check-true (string-contains? svc-msg "ctx-register-provider!")
                (format "facade contract must name the ctx operation: ~a" svc-msg))))

;; ---------------------------------------------------------------------------
;; D7 — compatibility facade preservation
;; ---------------------------------------------------------------------------

(module+ test
  (test-case "D7: injected service is exposed via ctx-provider-registry"
    (define reg (make-provider-registry))
    (define ctx (make-ctx-with-service reg))
    (check-true (provider-host-service? (ctx-provider-registry ctx)))
    ;; absent service degrades exactly like the historical null-registry path
    (define bare
      (make-extension-ctx #:session-id "bare"
                          #:session-dir "/tmp"
                          #:event-bus (make-event-bus)
                          #:extension-registry #f))
    (check-equal? (ctx-register-provider! bare "openai" (make-test-provider))
                  (hasheq 'error #t 'message "No provider-registry on context"))
    (check-equal? (ctx-list-providers bare) '())
    (check-false (ctx-lookup-provider bare "openai"))
    (check-equal? (ctx-unregister-provider! bare "openai") (void))
    ;; kwarg remains an any/c compatibility slot: a non-service value is stored
    ;; and returned verbatim (matches historical passthrough semantics)
    (define sym-ctx
      (make-extension-ctx #:session-id "sym"
                          #:session-dir "/tmp"
                          #:event-bus (make-event-bus)
                          #:extension-registry #f
                          #:provider-registry 'some-legacy-registry))
    (check-equal? (ctx-provider-registry sym-ctx) 'some-legacy-registry)))
