#lang racket/base

;; tests/test-extension-host-service-protocol.rkt — v0.99.88 W1
;; @speed fast
;; @suite default
;;
;; Neutral Extension Host Service Protocol (roadmap v0.99.88 W1, MA-03):
;;
;;   P1  Protocol surface: contracted struct accessors + capability metadata
;;       (name/contract/lifetime/owner for all four registry operations).
;;   P2  Neutral boundary: util/extension/host-services.rkt has NO runtime/,
;;       llm/, tools/, tui/, or extensions/ imports (module->imports).
;;   P3  Adapter contract: make-provider-host-service rejects non-registry.
;;   P4  Dual-run equivalence: direct registry calls and adapter capability
;;       calls produce identical registration, lookup, errors, and order.
;;   P5  Contract enforcement: neutral operation contracts blame the caller
;;       for domain violations (e.g. non-string provider name).
;;   P6  dynamic-require boundary: the neutral protocol loads in a fresh
;;       namespace without instantiating Runtime modules; the adapter is
;;       the module that legitimately imports the concrete registry.
;;   P7  Compatibility shim preserved: ctx-* facade signatures/behavior
;;       unchanged in W1 (delegation to the host service happens in W2).

(require rackunit
         racket/file
         racket/port
         racket/runtime-path
         racket/string
         "../extensions/context.rkt"
         "../llm/model.rkt"
         "../llm/provider.rkt"
         "../runtime/extension-host-adapter.rkt"
         "../runtime/provider/provider-registry.rkt"
         "../util/event/event-bus.rkt"
         "../util/extension/host-services.rkt")

(define-runtime-path host-services-path "../util/extension/host-services.rkt")

(define-runtime-path adapter-path "../runtime/extension-host-adapter.rkt")

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (make-test-provider)
  (make-mock-provider (make-model-response (list (hasheq 'type "text" 'text "mock"))
                                           (hasheq 'input-tokens 10 'output-tokens 20)
                                           "mock-model"
                                           'stop)
                      #:name "test-provider"))

(define (module-import-strings path)
  ;; Resolved import module paths of the file at PATH, as strings.
  (define resolved
    (with-handlers ([exn:fail? (lambda (_) '())])
      (module->imports `(file ,(path->string path)))))
  (for*/list ([phase+mods (in-list resolved)]
              [mod (in-list (cdr phase+mods))])
    (format "~s" mod)))

;; ---------------------------------------------------------------------------
;; P1 — protocol surface and capability metadata
;; ---------------------------------------------------------------------------

(module+ test
  (test-case "P1: protocol struct and capability metadata are complete"
    (check-true (procedure? provider-host-service?))
    ;; All four operations are contracted fields
    (define svc
      (provider-host-service (lambda (name inst #:config [cfg (hasheq)]) 'registered)
                             (lambda (name) (void))
                             (lambda () '())
                             (lambda (name) #f)))
    (check-true (provider-host-service? svc))
    (check-true (procedure? (provider-host-service-register-provider! svc)))
    (check-true (procedure? (provider-host-service-unregister-provider! svc)))
    (check-true (procedure? (provider-host-service-list-providers svc)))
    (check-true (procedure? (provider-host-service-lookup-provider svc)))
    ;; Capability metadata: 4 capabilities, each with name/lifetime/owner
    (check-equal? (length provider-registry-capabilities) 4)
    (for ([cap (in-list provider-registry-capabilities)])
      (check-true (host-capability-descriptor? cap))
      (check-true (symbol? (host-capability-descriptor-name cap)))
      (check-eq? (host-capability-descriptor-lifetime cap) 'session)
      (check-equal? (host-capability-descriptor-owner cap) "runtime")
      (define summary (host-capability-descriptor-summary cap))
      (check-true (and (string? summary) (positive? (string-length summary)))))
    (check-equal? (map host-capability-descriptor-name provider-registry-capabilities)
                  '(provider-registry.register! provider-registry.unregister!
                                                provider-registry.list
                                                provider-registry.lookup))))

;; ---------------------------------------------------------------------------
;; P2 — neutral boundary: no runtime/llm/tools/tui/extensions imports
;; ---------------------------------------------------------------------------

(module+ test
  (test-case "P2: host-services.rkt imports no runtime/llm/tools/tui/extensions module"
    (define imports (module-import-strings host-services-path))
    (check-false (null? imports) "module->imports must resolve for host-services.rkt")
    (for ([layer (in-list '("runtime/" "llm/" "tools/" "tui/" "extensions/"))])
      (check-false (for/or ([imp (in-list imports)])
                     (string-contains? imp layer))
                   (format "neutral protocol must not import ~a (got ~s)" layer imports)))))

;; ---------------------------------------------------------------------------
;; P3 — adapter construction contract
;; ---------------------------------------------------------------------------

(module+ test
  (test-case "P3: make-provider-host-service enforces provider-registry? contract"
    (define reg (make-provider-registry))
    (check-true (provider-host-service? (make-provider-host-service reg)))
    (check-exn exn:fail:contract?
               (lambda () (make-provider-host-service 'not-a-registry))
               "adapter must reject non-registry arguments")))

;; ---------------------------------------------------------------------------
;; P4 — dual-run equivalence: direct registry vs adapter capability
;; ---------------------------------------------------------------------------

(module+ test
  (test-case "P4: adapter path is behaviorally identical to direct registry path"
    (define reg-direct (make-provider-registry))
    (define reg-adapted (make-provider-registry))
    (define svc (make-provider-host-service reg-adapted))
    ;; register
    (define direct-reg-result
      (register-provider! reg-direct "openai" (make-test-provider) #:config (hasheq 'k "v")))
    (define adapted-reg-result
      ((provider-host-service-register-provider! svc) "openai"
                                                      (make-test-provider)
                                                      #:config (hasheq 'k "v")))
    (check-eq? direct-reg-result adapted-reg-result)
    (check-eq? adapted-reg-result 'registered)
    ;; re-register -> 'updated on both paths
    (check-eq? (register-provider! reg-direct "openai" (make-test-provider)) 'updated)
    (check-eq? ((provider-host-service-register-provider! svc) "openai" (make-test-provider))
               'updated)
    ;; list: same descriptors, same order
    (register-provider! reg-direct "gemini" (make-test-provider))
    ((provider-host-service-register-provider! svc) "gemini" (make-test-provider))
    (define direct-list (list-providers reg-direct))
    (define adapted-list ((provider-host-service-list-providers svc)))
    (check-equal? (map provider-info-name direct-list) (map provider-info-name adapted-list))
    (check-equal? (length adapted-list) 2)
    ;; lookup: present and absent
    (check-equal? (provider-info-name (lookup-provider reg-direct "openai"))
                  (provider-info-name ((provider-host-service-lookup-provider svc) "openai")))
    (check-false ((provider-host-service-lookup-provider svc) "missing"))
    (check-false (lookup-provider reg-direct "missing"))
    ;; unregister: both paths end empty
    (unregister-provider! reg-direct "openai")
    ((provider-host-service-unregister-provider! svc) "openai")
    (check-equal? (map provider-info-name (list-providers reg-direct))
                  (map provider-info-name ((provider-host-service-list-providers svc))))
    (check-equal? (length ((provider-host-service-list-providers svc))) 1)
    ;; unknown unregister is a no-op on both paths
    (check-equal? ((provider-host-service-unregister-provider! svc) "does-not-exist") (void))
    (check-equal? (unregister-provider! reg-direct "does-not-exist") (void)))

  (test-case "P4b: invalid provider instance raises exn:fail:contract through the adapter"
    (define svc (make-provider-host-service (make-provider-registry)))
    (check-exn exn:fail:contract?
               (lambda () ((provider-host-service-register-provider! svc) "bad" 'not-a-provider))
               "runtime validation must fire through the neutral capability")))

;; ---------------------------------------------------------------------------
;; P5 — neutral operation contracts blame the caller
;; ---------------------------------------------------------------------------

(module+ test
  (test-case "P5: neutral contracts reject domain violations at the boundary"
    (define svc (make-provider-host-service (make-provider-registry)))
    (check-exn exn:fail:contract?
               (lambda () ((provider-host-service-register-provider! svc) 42 (make-test-provider)))
               "non-string name must violate the register! contract")
    (check-exn exn:fail:contract?
               (lambda () ((provider-host-service-lookup-provider svc) 42))
               "non-string name must violate the lookup contract")
    (check-exn exn:fail:contract?
               (lambda () ((provider-host-service-unregister-provider! svc) 'symbol-name))
               "non-string name must violate the unregister! contract"))

  (test-case "P5b: contract blame lands on the extension caller"
    (define svc (make-provider-host-service (make-provider-registry)))
    (define (blame-message thunk)
      (with-handlers ([exn:fail:contract? (lambda (e) (exn-message e))]
                      [exn:fail? (lambda (e) "wrong-exn")])
        (thunk)
        "no-exn"))
    (define msg
      (blame-message (lambda ()
                       ((provider-host-service-register-provider! svc) 42 (make-test-provider)))))
    (check-true (string-contains? msg "blaming:")
                (format "violation must carry a blame party: ~a" msg))
    (check-true (string-contains? msg "test-extension-host-service-protocol.rkt")
                (format "blame must name the extension caller module: ~a" msg))
    (check-true (string-contains? msg "register-provider!")
                (format "blame must name the violated operation: ~a" msg))))

;; ---------------------------------------------------------------------------
;; P6 — dynamic-require boundary
;; ---------------------------------------------------------------------------

(module+ test
  (test-case "P6: neutral protocol dynamic-requires cleanly in a fresh namespace"
    (define ns (make-base-empty-namespace))
    (define-values (pred constructor)
      (parameterize ([current-namespace ns])
        (values (dynamic-require host-services-path 'provider-host-service?)
                (dynamic-require host-services-path 'provider-host-service))))
    (check-true (procedure? pred))
    (check-true (procedure? constructor))
    (define fresh-service
      (constructor (lambda (n i #:config [c (hasheq)]) 'registered)
                   (lambda (n) (void))
                   (lambda () '())
                   (lambda (n) #f)))
    (check-true (pred fresh-service))
    (check-false (pred 'not-a-service)))

  (test-case "P6b: the concrete registry import lives in the runtime adapter only"
    (define adapter-imports (module-import-strings adapter-path))
    (check-true (for/or ([imp (in-list adapter-imports)])
                  (string-contains? imp "provider/provider-registry.rkt"))
                "adapter must encapsulate the concrete registry import")
    ;; and the neutral protocol does not (P2 re-asserted at the module level)
    (define neutral-imports (module-import-strings host-services-path))
    (check-false (for/or ([imp (in-list neutral-imports)])
                   (string-contains? imp "provider-registry"))
                 "neutral protocol must not know the concrete registry module"))

  (test-case "P6c: Typed Racket client imports the neutral protocol as opaque"
    (define client-path (make-temporary-file "q-host-protocol-typed-~a.rkt"))
    (dynamic-wind
     void
     (lambda ()
       (call-with-output-file
        client-path
        #:exists 'truncate
        (lambda (out)
          (fprintf out
                   (string-append "#lang typed/racket/base\n"
                                  "(require/typed (file ~s)\n"
                                  "  [#:opaque ProviderHostService provider-host-service?]\n"
                                  "  [provider-registry-capabilities (Listof Any)])\n"
                                  "(provide typed-boundary-ok?)\n"
                                  "(: typed-boundary-ok? Boolean)\n"
                                  "(define typed-boundary-ok?\n"
                                  "  (and (= (length provider-registry-capabilities) 4) #t))\n")
                   (path->string host-services-path))))
       (check-true (parameterize ([use-compiled-file-paths '()])
                     (dynamic-require client-path 'typed-boundary-ok?))))
     (lambda ()
       (when (file-exists? client-path)
         (delete-file client-path))))))

;; ---------------------------------------------------------------------------
;; P7 — compatibility shim preserved in W1
;; ---------------------------------------------------------------------------

(module+ test
  (test-case "P7: ctx-* facade behavior is unchanged in W1"
    (define ctx
      (make-extension-ctx #:session-id "s"
                          #:session-dir #f
                          #:event-bus (make-event-bus)
                          #:extension-registry #f
                          #:provider-registry (make-provider-registry)))
    (check-equal? (ctx-register-provider! ctx "openai" (make-test-provider)) 'registered)
    (check-equal? (length (ctx-list-providers ctx)) 1)
    (check-true (provider-info? (ctx-lookup-provider ctx "openai")))
    (check-equal? (ctx-unregister-provider! ctx "openai") (void))
    (check-equal? (ctx-list-providers ctx) '())
    ;; null-registry degradation unchanged
    (define bare
      (make-extension-ctx #:session-id "s"
                          #:session-dir #f
                          #:event-bus (make-event-bus)
                          #:extension-registry #f))
    (check-equal? (ctx-register-provider! bare "openai" (make-test-provider))
                  (hasheq 'error #t 'message "No provider-registry on context"))
    (check-equal? (ctx-list-providers bare) '())
    (check-false (ctx-lookup-provider bare "openai"))))
