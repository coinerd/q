#lang racket

;; tests/test-wave4-sdk-expansion.rkt — tests for Wave 4 SDK Surface Expansion
;;
;; Covers:
;;   #1220: Provider Registration API (validation, convenience, duplicates)
;;   #1221: Provider Discovery and Selection (RPC methods, metadata)
;;   #1223: Session State Query API (messages, tokens, model)
;;   #1224: Project Context and Telemetry

(require rackunit
         rackunit/text-ui
         "../llm/provider.rkt"
         "../llm/model.rkt"
         "../runtime/provider-registry.rkt"
         "../extensions/api.rkt"
         "../extensions/context.rkt"
         "../extensions/hooks.rkt"
         "../extensions/telemetry.rkt"
         "../wiring/provider-rpc.rkt"
         "../agent/event-bus.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-mock-response)
  (make-model-response
   (list (hasheq 'type "text" 'text "mock response"))
   (hasheq 'input-tokens 10 'output-tokens 20)
   "mock-model"
   'stop))

(define (make-test-provider #:name [name "test-provider"])
  (make-mock-provider (make-mock-response) #:name name))

;; Real event bus and extension registry constructors
;; (previously used fake hasheq stubs — H1 fix)

;; ============================================================
;; #1220: Provider Registration API
;; ============================================================

(define-test-suite provider-registration-tests

  (test-case "register-provider! validates provider? — rejects non-provider"
    (define reg (make-provider-registry))
    (check-exn exn:fail:contract?
               (lambda () (register-provider! reg "bad" "not-a-provider")))
    (check-exn exn:fail:contract?
               (lambda () (register-provider! reg "bad" 42)))
    (check-exn exn:fail:contract?
               (lambda () (register-provider! reg "bad" (hasheq)))))

  (test-case "register-provider! accepts valid provider? instances"
    (define reg (make-provider-registry))
    (define p (make-test-provider))
    (check-not-exn (lambda () (register-provider! reg "valid" p))))

  (test-case "register-provider! returns 'registered for new provider"
    (define reg (make-provider-registry))
    (define p (make-test-provider))
    (define result (register-provider! reg "openai" p))
    (check-eq? result 'registered))

  (test-case "register-provider! returns 'updated for duplicate provider"
    (define reg (make-provider-registry))
    (define p1 (make-test-provider #:name "p1"))
    (define p2 (make-test-provider #:name "p2"))
    (define r1 (register-provider! reg "openai" p1))
    (define r2 (register-provider! reg "openai" p2))
    (check-eq? r1 'registered)
    (check-eq? r2 'updated))

  (test-case "register-provider! merges config on re-registration"
    (define reg (make-provider-registry))
    (define p (make-test-provider))
    (register-provider! reg "openai" p #:config (hasheq 'api-key "key1"))
    (register-provider! reg "openai" p #:config (hasheq 'timeout 30))
    (define info (lookup-provider reg "openai"))
    (check-equal? (hash-ref (provider-info-config info) 'api-key) "key1")
    (check-equal? (hash-ref (provider-info-config info) 'timeout) 30))

  (test-case "unregister-provider! removes provider and its models"
    (define reg (make-provider-registry))
    (define p (make-test-provider))
    (register-provider! reg "openai" p)
    (register-model! reg #:id "gpt-4" #:name "GPT-4" #:provider-name "openai")
    (unregister-provider! reg "openai")
    (check-false (lookup-provider reg "openai"))
    (check-equal? (length (list-models-for-provider reg "openai")) 0))

  (test-case "provider-metadata extracts info from provider-info"
    (define reg (make-provider-registry))
    (define p (make-test-provider))
    (register-provider! reg "openai" p #:config (hasheq 'region "us-east"))
    (define info (lookup-provider reg "openai"))
    (define meta (provider-metadata info))
    (check-equal? (hash-ref meta 'name) "openai")
    (check-true (hash-ref meta 'provider-valid?))
    (check-equal? (hash-ref (hash-ref meta 'config) 'region) "us-east"))

  (test-case "provider-summary returns metadata + models"
    (define reg (make-provider-registry))
    (define p (make-test-provider))
    (register-provider! reg "openai" p)
    (register-model! reg #:id "gpt-4" #:name "GPT-4" #:provider-name "openai"
                      #:context-window 128000 #:max-tokens 4096)
    (define summary (provider-summary reg "openai"))
    (check-true (hash? summary))
    (check-true (hash? (hash-ref summary 'info)))
    (check-equal? (length (hash-ref summary 'models)) 1)
    (check-equal? (hash-ref (car (hash-ref summary 'models)) 'id) "gpt-4"))

  (test-case "provider-summary returns #f for unknown provider"
    (define reg (make-provider-registry))
    (check-false (provider-summary reg "nonexistent")))

  ;; Integration: register -> list -> use -> unregister
  (test-case "full provider lifecycle: register -> list -> unregister"
    (define reg (make-provider-registry))
    (define p1 (make-test-provider #:name "openai"))
    (define p2 (make-test-provider #:name "anthropic"))
    (register-provider! reg "openai" p1)
    (register-provider! reg "anthropic" p2)
    (register-model! reg #:id "gpt-4" #:name "GPT-4" #:provider-name "openai")
    (register-model! reg #:id "claude-3" #:name "Claude 3" #:provider-name "anthropic")
    ;; List
    (check-equal? (length (list-providers reg)) 2)
    ;; Find
    (define found (find-model reg "gpt-4"))
    (check-true (registered-model? found))
    (check-equal? (registered-model-id found) "gpt-4")
    ;; Unregister one
    (unregister-provider! reg "openai")
    (check-equal? (length (list-providers reg)) 1)
    (check-false (find-model reg "gpt-4"))
    ;; Other still there
    (check-true (registered-model? (find-model reg "claude-3")))))

;; ============================================================
;; #1221: Provider Discovery and Selection (RPC)
;; ============================================================

(define-test-suite provider-rpc-tests

  (test-case "providers/list returns all registered providers"
    (define reg (make-provider-registry))
    (define p (make-test-provider))
    (register-provider! reg "test-provider" p)
    (define rpc (make-provider-rpc-handlers (hasheq 'provider-registry reg)))
    (define handler (hash-ref rpc 'providers/list))
    (define result (handler (hasheq)))
    (check-equal? (length (hash-ref result 'providers)) 1)
    (check-equal? (hash-ref (car (hash-ref result 'providers)) 'name) "test-provider"))

  (test-case "providers/list returns error without registry"
    (define rpc (make-provider-rpc-handlers (hasheq)))
    (define handler (hash-ref rpc 'providers/list))
    (define result (handler (hasheq)))
    (check-equal? (hash-ref result 'status) "error"))

  (test-case "providers/models lists all models"
    (define reg (make-provider-registry))
    (define p (make-test-provider))
    (register-provider! reg "openai" p)
    (register-model! reg #:id "gpt-4" #:name "GPT-4" #:provider-name "openai")
    (register-model! reg #:id "gpt-3.5" #:name "GPT-3.5" #:provider-name "openai")
    (define rpc (make-provider-rpc-handlers (hasheq 'provider-registry reg)))
    (define handler (hash-ref rpc 'providers/models))
    (define result (handler (hasheq)))
    (check-equal? (length (hash-ref result 'models)) 2))

  (test-case "providers/models filters by provider name"
    (define reg (make-provider-registry))
    (define p1 (make-test-provider #:name "openai"))
    (define p2 (make-test-provider #:name "anthropic"))
    (register-provider! reg "openai" p1)
    (register-provider! reg "anthropic" p2)
    (register-model! reg #:id "gpt-4" #:name "GPT-4" #:provider-name "openai")
    (register-model! reg #:id "claude-3" #:name "Claude 3" #:provider-name "anthropic")
    (define rpc (make-provider-rpc-handlers (hasheq 'provider-registry reg)))
    (define handler (hash-ref rpc 'providers/models))
    (define result (handler (hasheq 'provider "openai")))
    (check-equal? (length (hash-ref result 'models)) 1)
    (check-equal? (hash-ref (car (hash-ref result 'models)) 'id) "gpt-4"))

  (test-case "providers/find finds model by ID"
    (define reg (make-provider-registry))
    (define p (make-test-provider))
    (register-provider! reg "openai" p)
    (register-model! reg #:id "gpt-4" #:name "GPT-4" #:provider-name "openai")
    (define rpc (make-provider-rpc-handlers (hasheq 'provider-registry reg)))
    (define handler (hash-ref rpc 'providers/find))
    (define result (handler (hasheq 'query "gpt-4")))
    (check-true (hash-ref result 'found))
    (check-equal? (hash-ref (hash-ref result 'model) 'id) "gpt-4"))

  (test-case "providers/find returns not found for unknown model"
    (define reg (make-provider-registry))
    (define rpc (make-provider-rpc-handlers (hasheq 'provider-registry reg)))
    (define handler (hash-ref rpc 'providers/find))
    (define result (handler (hasheq 'query "nonexistent")))
    (check-false (hash-ref result 'found)))

  (test-case "providers/find returns error without query param"
    (define reg (make-provider-registry))
    (define rpc (make-provider-rpc-handlers (hasheq 'provider-registry reg)))
    (define handler (hash-ref rpc 'providers/find))
    (define result (handler (hasheq)))
    (check-equal? (hash-ref result 'status) "error")))

;; ============================================================
;; #1223: Session State Query API
;; ============================================================

(define-test-suite session-state-query-tests

  (test-case "ctx-session-messages returns messages when set"
    (define ctx (make-extension-ctx
                 #:session-id "test-123"
                 #:session-dir "/tmp"
                 #:event-bus (make-event-bus)
                 #:extension-registry (make-extension-registry)
                 #:session-messages (list (hasheq 'role "user" 'content "hello")
                                          (hasheq 'role "assistant" 'content "hi"))))
    (define msgs (ctx-session-messages ctx))
    (check-equal? (length msgs) 2)
    (check-equal? (hash-ref (car msgs) 'role) "user"))

  (test-case "ctx-session-messages returns empty list when not set"
    (define ctx (make-extension-ctx
                 #:session-id "test-123"
                 #:session-dir "/tmp"
                 #:event-bus (make-event-bus)
                 #:extension-registry (make-extension-registry)))
    (check-equal? (ctx-session-messages ctx) '()))

  (test-case "ctx-session-token-count returns usage when set"
    (define ctx (make-extension-ctx
                 #:session-id "test-123"
                 #:session-dir "/tmp"
                 #:event-bus (make-event-bus)
                 #:extension-registry (make-extension-registry)
                 #:session-token-usage (hasheq 'input-tokens 100 'output-tokens 50)))
    (define usage (ctx-session-token-count ctx))
    (check-equal? (hash-ref usage 'input-tokens) 100)
    (check-equal? (hash-ref usage 'output-tokens) 50))

  (test-case "ctx-session-token-count returns empty hash when not set"
    (define ctx (make-extension-ctx
                 #:session-id "test-123"
                 #:session-dir "/tmp"
                 #:event-bus (make-event-bus)
                 #:extension-registry (make-extension-registry)))
    (check-equal? (ctx-session-token-count ctx) (hasheq)))

  (test-case "ctx-session-model returns model name"
    (define ctx (make-extension-ctx
                 #:session-id "test-123"
                 #:session-dir "/tmp"
                 #:event-bus (make-event-bus)
                 #:extension-registry (make-extension-registry)
                 #:model-name "gpt-4o"))
    (check-equal? (ctx-session-model ctx) "gpt-4o"))

  (test-case "ctx-session-model returns #f when no model"
    (define ctx (make-extension-ctx
                 #:session-id "test-123"
                 #:session-dir "/tmp"
                 #:event-bus (make-event-bus)
                 #:extension-registry (make-extension-registry)))
    (check-false (ctx-session-model ctx)))

  (test-case "extension handler reads session state via ctx"
    (define received-messages (box #f))
    (define received-tokens (box #f))
    (define reg (make-extension-registry))
    (register-extension! reg
      (extension "state-reader" "1.0.0" "1"
                 (hasheq 'turn-start
                         (lambda (ctx payload)
                           (set-box! received-messages (ctx-session-messages ctx))
                           (set-box! received-tokens (ctx-session-token-count ctx))
                           (hook-pass payload)))))
    (define ctx (make-extension-ctx
                 #:session-id "test-123"
                 #:session-dir "/tmp"
                 #:event-bus (make-event-bus)
                 #:extension-registry reg
                 #:session-messages (list (hasheq 'role "user" 'content "test"))
                 #:session-token-usage (hasheq 'input-tokens 42)))
    (dispatch-hooks 'turn-start (hasheq 'turn 1) reg #:ctx ctx)
    (check-equal? (length (unbox received-messages)) 1)
    (check-equal? (hash-ref (unbox received-tokens) 'input-tokens) 42)))

;; ============================================================
;; #1224: Telemetry
;; ============================================================

(define-test-suite telemetry-tests

  (test-case "telemetry-start returns timer with operation and start-time"
    (define timer (telemetry-start "test-op"))
    (check-equal? (hash-ref timer 'operation) "test-op")
    (check-true (number? (hash-ref timer 'start-time))))

  (test-case "telemetry-end returns telemetry-event"
    (define timer (telemetry-start "test-op"))
    (define event (telemetry-end timer))
    (check-true (telemetry-event? event))
    (check-equal? (telemetry-event-operation event) "test-op")
    (check-true (>= (telemetry-event-elapsed-ms event) 0)))

  (test-case "telemetry-event captures metadata"
    (define timer (telemetry-start "test-op" #:metadata (hasheq 'key "value")))
    (define event (telemetry-end timer))
    (check-equal? (hash-ref (telemetry-event-metadata event) 'key) "value"))

  (test-case "telemetry-report groups by operation"
    (define events
      (for/list ([i (in-range 5)])
        (define timer (telemetry-start "op-a"))
        (telemetry-end timer)))
    (define report (telemetry-report events))
    (check-true (hash-has-key? report "op-a"))
    (define stats (hash-ref report "op-a"))
    (check-equal? (hash-ref stats 'count) 5))

  (test-case "telemetry-report handles multiple operations"
    (define events-a
      (for/list ([i (in-range 3)])
        (define timer (telemetry-start "op-a"))
        (telemetry-end timer)))
    (define events-b
      (for/list ([i (in-range 2)])
        (define timer (telemetry-start "op-b"))
        (telemetry-end timer)))
    (define report (telemetry-report (append events-a events-b)))
    (check-equal? (hash-ref (hash-ref report "op-a") 'count) 3)
    (check-equal? (hash-ref (hash-ref report "op-b") 'count) 2))

  (test-case "telemetry-summary formats event as string"
    (define timer (telemetry-start "my-tool"))
    (define event (telemetry-end timer))
    (define summary (telemetry-summary event))
    (check-true (string? summary))
    (check-true (string-contains? summary "my-tool"))
    (check-true (string-contains? summary "ms"))))

;; ============================================================
;; #1220: ctx convenience methods
;; ============================================================

(define-test-suite ctx-provider-convenience-tests

  (test-case "ctx-register-provider! registers via context"
    (define reg (make-provider-registry))
    (define ctx (make-extension-ctx
                 #:session-id "test"
                 #:session-dir "/tmp"
                 #:event-bus (make-event-bus)
                 #:extension-registry (make-extension-registry)
                 #:provider-registry reg))
    (define p (make-test-provider))
    (ctx-register-provider! ctx "openai" p)
    (check-true (provider-info? (lookup-provider reg "openai"))))

  (test-case "ctx-register-provider! returns error hash without registry"
    (define ctx (make-extension-ctx
                 #:session-id "test"
                 #:session-dir "/tmp"
                 #:event-bus (make-event-bus)
                 #:extension-registry (make-extension-registry)))
    (define result (ctx-register-provider! ctx "openai" (make-test-provider)))
    (check-true (hash-ref result 'error #f))
    (check-true (string? (hash-ref result 'message #f))))

  (test-case "ctx-unregister-provider! removes provider"
    (define reg (make-provider-registry))
    (define ctx (make-extension-ctx
                 #:session-id "test"
                 #:session-dir "/tmp"
                 #:event-bus (make-event-bus)
                 #:extension-registry (make-extension-registry)
                 #:provider-registry reg))
    (ctx-register-provider! ctx "openai" (make-test-provider))
    (ctx-unregister-provider! ctx "openai")
    (check-false (lookup-provider reg "openai")))

  (test-case "ctx-list-providers returns providers"
    (define reg (make-provider-registry))
    (define ctx (make-extension-ctx
                 #:session-id "test"
                 #:session-dir "/tmp"
                 #:event-bus (make-event-bus)
                 #:extension-registry (make-extension-registry)
                 #:provider-registry reg))
    (ctx-register-provider! ctx "openai" (make-test-provider))
    (ctx-register-provider! ctx "anthropic" (make-test-provider))
    (check-equal? (length (ctx-list-providers ctx)) 2))

  (test-case "ctx-list-providers returns empty without registry"
    (define ctx (make-extension-ctx
                 #:session-id "test"
                 #:session-dir "/tmp"
                 #:event-bus (make-event-bus)
                 #:extension-registry (make-extension-registry)))
    (check-equal? (ctx-list-providers ctx) '()))

  (test-case "ctx-lookup-provider finds provider by name"
    (define reg (make-provider-registry))
    (define ctx (make-extension-ctx
                 #:session-id "test"
                 #:session-dir "/tmp"
                 #:event-bus (make-event-bus)
                 #:extension-registry (make-extension-registry)
                 #:provider-registry reg))
    (ctx-register-provider! ctx "openai" (make-test-provider))
    (check-true (provider-info? (ctx-lookup-provider ctx "openai")))
    (check-false (ctx-lookup-provider ctx "nonexistent"))))

;; ============================================================
;; Run all tests
;; ============================================================

(run-tests
 (make-test-suite "Wave 4: SDK Surface Expansion Tests"
   (list provider-registration-tests
         provider-rpc-tests
         session-state-query-tests
         telemetry-tests
         ctx-provider-convenience-tests)))
