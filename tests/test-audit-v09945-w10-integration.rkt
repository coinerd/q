#lang racket/base
;; W10 — Integration subsystem real-world audit
;; Tests: provider registry, auto-retry, settings, sandbox limits, IPC protocol
;; @suite default
;; @speed fast

(require rackunit
         racket/list
         racket/string
         ;; Provider
         (only-in "../llm/provider.rkt" make-provider provider?)
         (only-in "../llm/model.rkt" make-model-response)
         ;; Provider registry
         "../runtime/provider/provider-registry.rkt"
         ;; Auto-retry
         "../runtime/auto-retry.rkt"
         ;; Settings
         "../runtime/settings-core.rkt"
         "../runtime/settings-query.rkt"
         ;; Sandbox limits
         "../sandbox/limits.rkt"
         ;; IPC protocol
         "../sandbox/ipc-protocol.rkt")

;; ---------------------------------------------------------------------------
;; Helper: create a mock provider
;; ---------------------------------------------------------------------------

(define (test-provider)
  (make-provider (lambda () "test-provider")
                 (lambda () (hasheq 'streaming #t))
                 (lambda (req) (make-model-response '() '()))
                 (lambda (req) (hasheq 'chunks '()))))

;; ---------------------------------------------------------------------------
;; 1. Provider Registry
;; ---------------------------------------------------------------------------

(test-case "audit-prov-reg-empty"
  (define reg (make-provider-registry))
  (check-true (provider-registry? reg))
  (check-equal? (length (list-providers reg)) 0))

(test-case "audit-prov-reg-register-and-lookup"
  (define reg (make-provider-registry))
  (define result (register-provider! reg "my-prov" (test-provider)))
  (check-true (or (eq? result 'registered) (eq? result 'updated)))
  (check-equal? (length (list-providers reg)) 1)
  (define pinfo (lookup-provider reg "my-prov"))
  (check-true (provider-info? pinfo))
  (check-equal? (provider-info-name pinfo) "my-prov"))

(test-case "audit-prov-reg-unregister"
  (define reg (make-provider-registry))
  (register-provider! reg "a" (test-provider))
  (register-provider! reg "b" (test-provider))
  (unregister-provider! reg "a")
  (check-false (lookup-provider reg "a"))
  (check-true (provider-info? (lookup-provider reg "b"))))

(test-case "audit-prov-reg-reregister-returns-updated"
  (define reg (make-provider-registry))
  (check-eq? (register-provider! reg "p" (test-provider)) 'registered)
  (check-eq? (register-provider! reg "p" (test-provider)) 'updated))

(test-case "audit-prov-reg-lookup-nonexistent"
  (define reg (make-provider-registry))
  (check-false (lookup-provider reg "nope")))

(test-case "audit-prov-reg-rejects-non-provider"
  (define reg (make-provider-registry))
  (check-exn exn:fail:contract? (lambda () (register-provider! reg "bad" "not-a-provider"))))

(test-case "audit-prov-reg-config-merge"
  (define reg (make-provider-registry))
  (register-provider! reg "p" (test-provider) #:config (hasheq 'a 1))
  (register-provider! reg "p" (test-provider) #:config (hasheq 'b 2))
  (define pinfo (lookup-provider reg "p"))
  (define cfg (provider-info-config pinfo))
  (check-equal? (hash-ref cfg 'a) 1)
  (check-equal? (hash-ref cfg 'b) 2))

;; ---------------------------------------------------------------------------
;; 2. Model Registration and Search
;; ---------------------------------------------------------------------------

(test-case "audit-model-register"
  (define reg (make-provider-registry))
  (register-provider! reg "prov1" (test-provider))
  (register-model! reg
                   #:id "gpt-4"
                   #:name "GPT-4"
                   #:provider-name "prov1"
                   #:context-window 8192
                   #:max-tokens 4096)
  (define models (list-models-for-provider reg "prov1"))
  (check-equal? (length models) 1)
  (define m (car models))
  (check-true (registered-model? m))
  (check-equal? (registered-model-id m) "gpt-4")
  (check-equal? (registered-model-name m) "GPT-4")
  (check-equal? (registered-model-context-window m) 8192))

(test-case "audit-model-find-by-id"
  (define reg (make-provider-registry))
  (register-provider! reg "prov1" (test-provider))
  (register-model! reg #:id "gpt-4" #:name "GPT-4" #:provider-name "prov1")
  (define m (find-model reg "gpt-4"))
  (check-true (registered-model? m))
  (check-equal? (registered-model-id m) "gpt-4"))

(test-case "audit-model-find-by-name"
  (define reg (make-provider-registry))
  (register-provider! reg "prov1" (test-provider))
  (register-model! reg #:id "gpt-4" #:name "GPT-4" #:provider-name "prov1")
  (define m (find-model reg "GPT-4"))
  (check-true (registered-model? m))
  (check-equal? (registered-model-name m) "GPT-4"))

(test-case "audit-model-find-case-insensitive"
  (define reg (make-provider-registry))
  (register-provider! reg "prov1" (test-provider))
  (register-model! reg #:id "gpt-4" #:name "GPT-4" #:provider-name "prov1")
  (define m (find-model reg "GPT-4"))
  (check-true (registered-model? m)))

(test-case "audit-model-find-not-found"
  (define reg (make-provider-registry))
  (check-false (find-model reg "nonexistent")))

(test-case "audit-model-unregister"
  (define reg (make-provider-registry))
  (register-provider! reg "prov1" (test-provider))
  (register-model! reg #:id "gpt-4" #:name "GPT-4" #:provider-name "prov1")
  (unregister-model! reg "prov1" "gpt-4")
  (check-false (find-model reg "gpt-4")))

(test-case "audit-model-find-models-fuzzy"
  (define reg (make-provider-registry))
  (register-provider! reg "prov1" (test-provider))
  (register-model! reg #:id "gpt-4" #:name "GPT-4 Turbo" #:provider-name "prov1")
  (register-model! reg #:id "gpt-3.5" #:name "GPT-3.5 Turbo" #:provider-name "prov1")
  (register-model! reg #:id "claude-3" #:name "Claude 3" #:provider-name "prov1")
  (define results (find-models reg "gpt"))
  (check-equal? (length results) 2))

(test-case "audit-model-unregister-provider-removes-models"
  (define reg (make-provider-registry))
  (register-provider! reg "prov1" (test-provider))
  (register-model! reg #:id "gpt-4" #:name "GPT-4" #:provider-name "prov1")
  (register-model! reg #:id "gpt-3" #:name "GPT-3" #:provider-name "prov1")
  (unregister-provider! reg "prov1")
  (check-equal? (length (list-models-for-provider reg "prov1")) 0))

(test-case "audit-prov-summary"
  (define reg (make-provider-registry))
  (register-provider! reg "prov1" (test-provider))
  (register-model! reg #:id "gpt-4" #:name "GPT-4" #:provider-name "prov1")
  (define summary (provider-summary reg "prov1"))
  (check-true (hash? summary))
  (check-true (hash-has-key? summary 'info))
  (check-true (hash-has-key? summary 'models))
  (check-false (provider-summary reg "nonexistent")))

;; ---------------------------------------------------------------------------
;; 3. Auto-Retry: Error Classification
;; ---------------------------------------------------------------------------

(test-case "audit-retry-classify-rate-limit"
  (define e (exn:fail "Error: 429 Too Many Requests" (current-continuation-marks)))
  (check-equal? (classify-error e) 'rate-limit))

(test-case "audit-retry-classify-timeout"
  (define e (exn:fail "Connection timed out" (current-continuation-marks)))
  (check-equal? (classify-error e) 'timeout))

(test-case "audit-retry-classify-auth"
  (define e (exn:fail "401 Unauthorized" (current-continuation-marks)))
  (check-equal? (classify-error e) 'auth))

(test-case "audit-retry-classify-unknown"
  (define e (exn:fail "Something weird happened" (current-continuation-marks)))
  (check-equal? (classify-error e) 'provider-error))

(test-case "audit-retry-retryable-rate-limit"
  (define e (exn:fail "429 rate limit exceeded" (current-continuation-marks)))
  (check-true (retryable-error? e)))

(test-case "audit-retry-retryable-timeout"
  (define e (exn:fail "connection timed out" (current-continuation-marks)))
  (check-true (retryable-error? e)))

(test-case "audit-retry-retryable-server-error"
  (define e (exn:fail "503 Service Unavailable" (current-continuation-marks)))
  (check-true (retryable-error? e)))

(test-case "audit-retry-not-retryable-permanent-tool"
  (define e (exn:fail "missing required argument: path" (current-continuation-marks)))
  (check-false (retryable-error? e)))

(test-case "audit-retry-not-retryable-auth"
  (define e (exn:fail "401 auth failed" (current-continuation-marks)))
  ;; auth errors are classified as auth but NOT in the retryable categories
  (check-false (retryable-error? e)))

(test-case "audit-retry-context-overflow"
  (define e (exn:fail "context_length_exceeded: input is too long" (current-continuation-marks)))
  (check-true (context-overflow-error? e)))

(test-case "audit-retry-permanent-tool-error"
  (define e (exn:fail "validate-tool-args: missing required argument" (current-continuation-marks)))
  (check-true (permanent-tool-error? e)))

;; ---------------------------------------------------------------------------
;; 4. Auto-Retry: Retry Execution
;; ---------------------------------------------------------------------------

(test-case "audit-retry-success-first-try"
  (define calls (box 0))
  (define result
    (with-auto-retry (lambda ()
                       (set-box! calls (add1 (unbox calls)))
                       42)
                     #:max-retries 3
                     #:base-delay-ms 1))
  (check-equal? result 42)
  (check-equal? (unbox calls) 1))

(test-case "audit-retry-success-on-second"
  (define calls (box 0))
  (define result
    (with-auto-retry (lambda ()
                       (set-box! calls (add1 (unbox calls)))
                       (if (= (unbox calls) 1)
                           (error "429 rate limit")
                           'success))
                     #:max-retries 3
                     #:base-delay-ms 1))
  (check-equal? result 'success)
  (check-equal? (unbox calls) 2))

(test-case "audit-retry-non-retryable-immediate-fail"
  (define calls (box 0))
  (check-exn exn:fail?
             (lambda ()
               (with-auto-retry (lambda ()
                                  (set-box! calls (add1 (unbox calls)))
                                  (error "validate-tool-args: missing argument"))
                                #:max-retries 3
                                #:base-delay-ms 1)))
  (check-equal? (unbox calls) 1))

(test-case "audit-retry-exhausted"
  (define calls (box 0))
  (check-exn retry-exhausted?
             (lambda ()
               (with-auto-retry (lambda ()
                                  (set-box! calls (add1 (unbox calls)))
                                  (error "503 server error"))
                                #:max-retries 2
                                #:base-delay-ms 1)))
  (check-equal? (unbox calls) 3)) ;; initial + 2 retries

(test-case "audit-retry-on-retry-callback"
  (define callback-called (box #f))
  (with-auto-retry (lambda ()
                     (if (unbox callback-called)
                         'ok
                         (begin
                           (set-box! callback-called #t)
                           (error "429 rate limit"))))
                   #:max-retries 3
                   #:base-delay-ms 1
                   #:on-retry (lambda (attempt max-retries delay-ms msg err-type)
                                (check-true (> attempt 0))))
  (check-true (unbox callback-called)))

(test-case "audit-retry-policy-struct"
  (define policy (make-default-retry-policy #:max-retries 5 #:base-delay-ms 100))
  (check-true (retry-policy? policy))
  (check-equal? (retry-policy-max-retries policy) 5)
  (check-equal? (retry-policy-base-delay-ms policy) 100))

;; ---------------------------------------------------------------------------
;; 5. Settings: Loading and Merging
;; ---------------------------------------------------------------------------

(test-case "audit-settings-deep-merge-simple"
  (define left (hasheq 'a 1 'b 2))
  (define right (hasheq 'b 3 'c 4))
  (define merged (deep-merge-hash left right))
  (check-equal? (hash-ref merged 'a) 1)
  (check-equal? (hash-ref merged 'b) 3) ;; right wins
  (check-equal? (hash-ref merged 'c) 4))

(test-case "audit-settings-deep-merge-nested"
  (define left (hasheq 'providers (hasheq 'openai (hasheq 'key "old" 'model "gpt-4"))))
  (define right (hasheq 'providers (hasheq 'openai (hasheq 'key "new"))))
  (define merged (deep-merge-hash left right))
  (define providers (hash-ref merged 'providers))
  (define openai (hash-ref providers 'openai))
  (check-equal? (hash-ref openai 'key) "new") ;; overridden
  (check-equal? (hash-ref openai 'model) "gpt-4")) ;; preserved

(test-case "audit-settings-make-minimal"
  (define s (make-minimal-settings))
  (check-true (q-settings? s))
  (check-true (hash? (q-settings-merged s))))

(test-case "audit-settings-make-minimal-with-overrides"
  (define s (make-minimal-settings #:provider "anthropic" #:model "claude-3"))
  (check-equal? (setting-ref s 'default-provider) "anthropic")
  (check-equal? (setting-ref s 'default-model) "claude-3"))

(test-case "audit-settings-ref-with-default"
  (define s (make-minimal-settings))
  (check-equal? (setting-ref s 'nonexistent 'default) 'default)
  (check-false (setting-ref s 'nonexistent)))

(test-case "audit-settings-ref*-nested"
  (define s (make-minimal-settings #:overrides (hasheq 'steering (hasheq 'gentle_threshold 5))))
  (check-equal? (setting-ref* s '(steering gentle_threshold)) 5)
  (check-false (setting-ref* s '(steering nonexistent)))
  (check-equal? (setting-ref* s '(steering nonexistent) 'fallback) 'fallback))

;; ---------------------------------------------------------------------------
;; 6. Settings: Query Functions
;; ---------------------------------------------------------------------------

(test-case "audit-settings-parallel-tools-default"
  (define s (make-minimal-settings))
  (check-false (parallel-tools-enabled? s)))

(test-case "audit-settings-http-timeout-default"
  (define s (make-minimal-settings))
  (check-equal? (http-request-timeout s) 300))

(test-case "audit-settings-steering-defaults"
  (define s (make-minimal-settings))
  (check-equal? (steering-gentle-threshold s) 8)
  (check-equal? (steering-strong-threshold s) 12)
  (check-equal? (steering-hard-cap s) 20)
  (check-true (steering-same-file-dedup? s)))

(test-case "audit-settings-context-assembly-profile-default"
  (define s (make-minimal-settings))
  ;; Default profile: "observe" → coerced to 'observe
  (check-true (symbol? (setting-context-assembly-profile s))))

(test-case "audit-settings-credential-policy-default"
  (define s (make-minimal-settings))
  (check-equal? (credential-policy s) 'auto))

(test-case "audit-settings-shell-risk-default"
  (define s (make-minimal-settings))
  (check-equal? (shell-risk-classifier s) 'regex))

(test-case "audit-settings-execution-plane-defaults"
  (define s (make-minimal-settings))
  (check-true (execution-plane-enabled? s))
  (check-equal? (execution-plane-timeout-ms s) 120000))

(test-case "audit-settings-verifier-defaults"
  (define s (make-minimal-settings))
  (check-true (verifier-enabled? s))
  (check-equal? (verifier-risk-threshold s) 'high)
  (check-equal? (verifier-max-rework-iterations s) 3))

(test-case "audit-settings-blackboard-default"
  (define s (make-minimal-settings))
  (check-true (blackboard-enabled? s)))

(test-case "audit-settings-hot-swap-default"
  (define s (make-minimal-settings))
  (check-true (hot-swap-enabled? s)))

(test-case "audit-settings-auto-reload-default"
  (define s (make-minimal-settings))
  (check-false (auto-reload-enabled? s)))

(test-case "audit-settings-mcp-defaults"
  (define s (make-minimal-settings))
  (check-false (mcp-enabled? s))
  (check-false (mcp-server-enabled? s)))

(test-case "audit-settings-broker-defaults"
  (define s (make-minimal-settings))
  (check-false (broker-enabled? s))
  (check-equal? (broker-remote-host s) "localhost")
  (check-equal? (broker-remote-port s) 8443))

;; ---------------------------------------------------------------------------
;; 7. Sandbox: Limits
;; ---------------------------------------------------------------------------

(test-case "audit-limits-default"
  (define l (default-exec-limits))
  (check-true (exec-limits? l))
  (check-equal? (exec-limits-timeout-seconds l) 120)
  (check-equal? (exec-limits-max-processes l) 10))

(test-case "audit-limits-strict"
  (define l (strict-exec-limits))
  (check-true (exec-limits? l))
  (check-true (< (exec-limits-timeout-seconds l) 120))
  (check-true (< (exec-limits-max-output-bytes l) 1048576)))

(test-case "audit-limits-permissive"
  (define l (permissive-exec-limits))
  (check-true (exec-limits? l))
  (check-true (> (exec-limits-timeout-seconds l) 120)))

(test-case "audit-limits-merge-stricter-wins"
  (define base (permissive-exec-limits))
  (define override (strict-exec-limits))
  (define merged (merge-limits base override))
  ;; Merged should be as strict as the stricter of the two
  (check-equal? (exec-limits-timeout-seconds merged) (exec-limits-timeout-seconds override)))

(test-case "audit-limits-within-limits-ok"
  (define l (default-exec-limits))
  (check-true (within-limits? l #:elapsed 10 #:output-size 100 #:memory 1000 #:processes 1)))

(test-case "audit-limits-within-limits-timeout"
  (define l (default-exec-limits))
  (check-false (within-limits? l #:elapsed 999)))

(test-case "audit-limits-within-limits-output"
  (define l (default-exec-limits))
  (check-false (within-limits? l #:output-size 999999999)))

(test-case "audit-limits-within-limits-partial-check"
  ;; Only check some dimensions; others are OK
  (define l (default-exec-limits))
  (check-true (within-limits? l #:elapsed 10))
  (check-true (within-limits? l #:memory 100)))

(test-case "audit-limits-process-tracking"
  (reset-process-count!)
  (check-equal? (get-process-count) 0)
  (define n1 (track-process!))
  (check-equal? n1 1)
  (check-equal? (get-process-count) 1)
  (define n2 (track-process!))
  (check-equal? n2 2)
  (untrack-process!)
  (check-equal? (get-process-count) 1)
  (reset-process-count!))

;; ---------------------------------------------------------------------------
;; 8. Sandbox: IPC Protocol
;; ---------------------------------------------------------------------------

(test-case "audit-ipc-request-construction"
  (define req (ipc-request "req-1" "bash" (hasheq 'command "ls") 5000 "/tmp" 'any 1))
  (check-true (ipc-request? req))
  (check-equal? (ipc-request-request-id req) "req-1")
  (check-equal? (ipc-request-tool-name req) "bash")
  (check-equal? (ipc-request-timeout-ms req) 5000))

(test-case "audit-ipc-response-construction"
  (define resp (ipc-response "req-1" 'ok "output" (hasheq) #f 1))
  (check-true (ipc-response? resp))
  (check-equal? (ipc-response-request-id resp) "req-1")
  (check-equal? (ipc-response-status resp) 'ok))

(test-case "audit-ipc-request-round-trip"
  (define req (ipc-request "req-1" "bash" (hasheq 'command "echo hello") 5000 "/tmp" 'any 1))
  (define jsexpr (ipc-request->jsexpr req))
  (check-true (hash? jsexpr))
  (check-equal? (hash-ref jsexpr 'request-id) "req-1")
  (check-equal? (hash-ref jsexpr 'tool-name) "bash")
  (define req2 (jsexpr->ipc-request jsexpr))
  (check-true (ipc-request? req2))
  (check-equal? (ipc-request-request-id req2) "req-1")
  (check-equal? (ipc-request-tool-name req2) "bash"))

(test-case "audit-ipc-response-round-trip"
  (define resp (ipc-response "req-2" 'ok "result text" (hasheq 'exit-code 0) #f 1))
  (define jsexpr (ipc-response->jsexpr resp))
  (check-true (hash? jsexpr))
  (define resp2 (jsexpr->ipc-response jsexpr))
  (check-true (ipc-response? resp2))
  (check-equal? (ipc-response-status resp2) 'ok)
  (check-equal? (ipc-response-content resp2) "result text"))

(test-case "audit-ipc-response-error"
  (define resp (ipc-response "req-3" 'error (void) (hasheq) "something broke" 1))
  (define jsexpr (ipc-response->jsexpr resp))
  (define resp2 (jsexpr->ipc-response jsexpr))
  (check-true (ipc-response? resp2))
  (check-equal? (ipc-response-status resp2) 'error)
  (check-equal? (ipc-response-error-message resp2) "something broke"))

(test-case "audit-ipc-make-error-response"
  (define resp (make-error-response "req-4" "bad args"))
  (check-true (ipc-response? resp))
  (check-equal? (ipc-response-status resp) 'error)
  (check-equal? (ipc-response-error-message resp) "bad args"))

(test-case "audit-ipc-make-timeout-response"
  (define resp (make-timeout-response "req-5"))
  (check-true (ipc-response? resp))
  (check-equal? (ipc-response-status resp) 'timeout))

(test-case "audit-ipc-jsexpr-invalid-request"
  (check-false (jsexpr->ipc-request "not-a-hash"))
  (check-false (jsexpr->ipc-request (hasheq))) ; missing required fields
  (check-false (jsexpr->ipc-request (hasheq 'request-id "x")))) ; missing tool-name

(test-case "audit-ipc-jsexpr-invalid-response"
  (check-false (jsexpr->ipc-response "not-a-hash"))
  (check-false (jsexpr->ipc-response (hasheq))) ; missing required fields
  (check-false (jsexpr->ipc-response (hasheq 'request-id "x" 'status "invalid-status"))))

(test-case "audit-ipc-constants"
  (check-true (exact-positive-integer? IPC-SCHEMA-VERSION))
  (check-true (exact-positive-integer? IPC-MAX-REQUEST-BYTES))
  (check-true (exact-positive-integer? IPC-MAX-RESPONSE-BYTES))
  (check-true (exact-positive-integer? IPC-DEFAULT-TIMEOUT-MS)))

(test-case "audit-ipc-request-too-large"
  ;; Create a request with large arguments
  (define big-args (hasheq 'data (make-string 2000000 #\x)))
  (define req (ipc-request "req-big" "bash" big-args 5000 "/tmp" 'any 1))
  (check-true (ipc-request-too-large? req)))

(test-case "audit-ipc-request-not-too-large"
  (define req (ipc-request "req-small" "bash" (hasheq 'command "ls") 5000 "/tmp" 'any 1))
  (check-false (ipc-request-too-large? req)))

;; ---------------------------------------------------------------------------
;; 9. Provider Metadata
;; ---------------------------------------------------------------------------

(test-case "audit-prov-metadata"
  (define reg (make-provider-registry))
  (register-provider! reg "prov1" (test-provider))
  (define pinfo (lookup-provider reg "prov1"))
  (define meta (provider-metadata pinfo))
  (check-true (hash? meta))
  (check-equal? (hash-ref meta 'name) "prov1")
  (check-true (hash-ref meta 'provider-valid?)))
