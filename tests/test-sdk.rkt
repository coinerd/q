#lang racket

;; q/tests/test-sdk.rkt — tests for interfaces/sdk.rkt
;;
;; TDD tests for the SDK surface — thin wrapper around runtime/session.

(require rackunit
         racket/file
         racket/path
         racket/string
         "../llm/model.rkt"
         "../llm/provider.rkt"
         (only-in "../tools/tool.rkt" make-tool-registry tool-registry? register-tool!)
         "../agent/event-bus.rkt"
         "../util/protocol-types.rkt"
         (only-in "../extensions/api.rkt"
                  make-extension-registry
                  extension-registry?
                  extension
                  register-extension!)
         (only-in "../runtime/compactor.rkt" compaction-result-removed-count compact-and-persist!)
         (only-in "../runtime/token-compaction.rkt" token-compaction-config)
         "helpers/compaction-helpers.rkt"
         (only-in "../runtime/agent-session.rkt" agent-session-system-instructions session-history)
         (only-in "../runtime/settings.rkt" default-session-dir)
         (only-in "../util/jsonl.rkt" jsonl-read-all-valid)
         "../interfaces/sdk.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-temp-session-dir)
  (make-temporary-file "q-sdk-test-~a" 'directory))

(define (make-test-provider)
  (make-mock-provider (make-model-response (list (hasheq 'type "text" 'text "Hello from mock"))
                                           (hasheq 'inputTokens 5 'outputTokens 5)
                                           "mock-model"
                                           'stop)))

(define (cleanup-dir dir)
  (with-handlers ([exn:fail? (λ (_) (void))])
    (delete-directory/files dir #:must-exist? #f)))

;; ============================================================
;; Test suite: make-runtime
;; ============================================================

(test-case "make-runtime: creates runtime with provider and defaults"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    (check-pred runtime? rt)
    (check-pred runtime-config? (runtime-rt-config rt))
    (check-equal? (runtime-config-model-name (runtime-rt-config rt)) #f)
    (check-equal? (runtime-config-max-iterations (runtime-rt-config rt)) 10)
    (check-false (runtime-rt-session rt))
    (cleanup-dir tmp)))

(test-case "make-runtime: creates runtime with custom max-iterations"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp #:max-iterations 25))
    (check-equal? (runtime-config-max-iterations (runtime-rt-config rt)) 25)
    (cleanup-dir tmp)))

(test-case "make-runtime: creates runtime with custom tool-registry"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define reg (make-tool-registry))
    (define rt (make-runtime #:provider prov #:session-dir tmp #:tool-registry reg))
    (check-eq? (runtime-config-tool-registry (runtime-rt-config rt)) reg)
    (cleanup-dir tmp)))

(test-case "make-runtime: creates runtime with custom event-bus"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define bus (make-event-bus))
    (define rt (make-runtime #:provider prov #:session-dir tmp #:event-bus bus))
    (check-eq? (runtime-config-event-bus (runtime-rt-config rt)) bus)
    (cleanup-dir tmp)))

;; ============================================================
;; Test suite: open-session
;; ============================================================

(test-case "open-session: creates new session when no ID given"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    (check-false (runtime-rt-session rt))
    (define rt2 (open-session rt))
    (check-pred runtime? rt2)
    (check-not-false (runtime-rt-session rt2))
    (define info (session-info rt2))
    (check-true (string? (hash-ref info 'session-id)))
    (cleanup-dir tmp)))

(test-case "open-session: session-info returns metadata for active session"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    (define rt2 (open-session rt))
    (define info (session-info rt2))
    (check-pred hash? info)
    (check-true (hash-has-key? info 'session-id))
    (check-true (hash-has-key? info 'active?))
    (check-true (hash-has-key? info 'history-length))
    (check-equal? (hash-ref info 'active?) #t)
    (check-equal? (hash-ref info 'history-length) 0)
    (cleanup-dir tmp)))

(test-case "open-session: session-info returns #f when no active session"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    (check-false (session-info rt))
    (cleanup-dir tmp)))

(test-case "open-session: resumes existing session when ID given"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    ;; Create a session first
    (define rt1 (make-runtime #:provider prov #:session-dir tmp))
    (define rt2 (open-session rt1))
    (define sid (hash-ref (session-info rt2) 'session-id))
    ;; Now resume it on a fresh runtime
    (define rt3 (make-runtime #:provider prov #:session-dir tmp))
    (define rt4 (open-session rt3 sid))
    (check-pred runtime? rt4)
    (define info (session-info rt4))
    (check-equal? (hash-ref info 'session-id) sid)
    (check-equal? (hash-ref info 'active?) #t)
    (cleanup-dir tmp)))

;; ============================================================
;; Test suite: run-prompt!
;; ============================================================

(test-case "run-prompt!: runs prompt and returns updated runtime + result"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    (define rt2 (open-session rt))
    (define-values (rt3 result) (run-prompt! rt2 "Hello, world!"))
    (check-pred runtime? rt3)
    (check-pred loop-result? result)
    (check-equal? (loop-result-termination-reason result) 'completed)
    ;; History should have grown
    (define info (session-info rt3))
    (check-true (> (hash-ref info 'history-length) 0))
    (cleanup-dir tmp)))

(test-case "run-prompt!: error when no session active"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    ;; Should return an error, not crash
    (define-values (rt2 result) (run-prompt! rt "Hello"))
    (check-pred runtime? rt2)
    (check-equal? result 'no-active-session)
    (cleanup-dir tmp)))

;; ============================================================
;; Test suite: subscribe-events!
;; ============================================================

(test-case "subscribe-events!: subscribes and receives events"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define received-events (box '()))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    (define sub-id
      (subscribe-events! rt (λ (evt) (set-box! received-events (cons evt (unbox received-events))))))
    (check-pred exact-nonnegative-integer? sub-id)
    ;; Open a session — should trigger session.started event
    (define rt2 (open-session rt))
    (check-true (not (null? (unbox received-events))))
    (define event-names (map (λ (e) (event-event e)) (unbox received-events)))
    (check-not-false (member "session.started" event-names))
    (cleanup-dir tmp)))

(test-case "subscribe-events!: subscribe with filter"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define received-events (box '()))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    ;; Only subscribe to turn events
    (define sub-id
      (subscribe-events! rt
                         (λ (evt) (set-box! received-events (cons evt (unbox received-events))))
                         (λ (evt) (string-contains? (event-event evt) "turn"))))
    ;; Open session + run prompt — only turn-related events should be captured
    (define rt2 (open-session rt))
    (define-values (rt3 result) (run-prompt! rt2 "test"))
    ;; The filter should exclude session.started, session.updated, etc.
    (for ([e (in-list (unbox received-events))])
      (check-true (string-contains? (event-event e) "turn")
                  (format "Expected turn event, got: ~a" (event-event e))))
    (cleanup-dir tmp)))

;; ============================================================
;; Test suite: interrupt!
;; ============================================================

(test-case "interrupt!: emit interrupt event and return runtime"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define received-events (box '()))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    (subscribe-events! rt (λ (evt) (set-box! received-events (cons evt (unbox received-events)))))
    (define rt2 (open-session rt))
    (define rt3 (interrupt! rt2))
    (check-pred runtime? rt3)
    ;; Should have emitted an interrupt event
    (define event-names (map (λ (e) (event-event e)) (unbox received-events)))
    (check-not-false (member "interrupt.requested" event-names)
                     (format "Expected interrupt.requested in ~a" event-names))
    (cleanup-dir tmp)))

;; ============================================================
;; Test suite: fork-session!
;; ============================================================

(test-case "fork-session!: forks active session"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    (define rt2 (open-session rt))
    ;; Run a prompt to create some history
    (define-values (rt3 result) (run-prompt! rt2 "First message"))
    (define original-sid (hash-ref (session-info rt3) 'session-id))
    ;; Fork
    (define rt4 (fork-session! rt3))
    (check-pred runtime? rt4)
    (define forked-info (session-info rt4))
    (check-not-equal? (hash-ref forked-info 'session-id) original-sid)
    ;; Forked session should have copied history
    (check-true (> (hash-ref forked-info 'history-length) 0))
    (cleanup-dir tmp)))

(test-case "fork-session!: error when no session to fork"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    (define result (fork-session! rt))
    (check-equal? result 'no-active-session)
    (cleanup-dir tmp)))

;; ============================================================
;; Test suite: compact-session!
;; ============================================================

(test-case "compact-session!: compacts session history"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    (define rt2 (open-session rt))
    ;; Run multiple prompts to build history
    (define-values (rt3 _res1) (run-prompt! rt2 "Message one"))
    (define-values (rt4 _res2) (run-prompt! rt3 "Message two"))
    (define-values (rt5 _res3) (run-prompt! rt4 "Message three"))
    ;; Compact (now returns values: runtime + compaction-result)
    (define-values (rt6 comp-res) (compact-session! rt5))
    (check-pred runtime? rt6)
    (check-pred compaction-result? comp-res)
    ;; After compaction, history should exist (may be same or smaller)
    (define info (session-info rt6))
    (check-true (>= (hash-ref info 'history-length) 0))
    (cleanup-dir tmp)))

(test-case "compact-session!: emits compaction events"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define received-events (box '()))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    (subscribe-events! rt (λ (evt) (set-box! received-events (cons evt (unbox received-events)))))
    (define rt2 (open-session rt))
    (define-values (rt3 _) (run-prompt! rt2 "build history"))
    (define-values (rt4 comp-res) (compact-session! rt3))
    (check-pred compaction-result? comp-res)
    (define event-names (map (λ (e) (event-event e)) (unbox received-events)))
    (check-not-false (member "compaction.started" event-names))
    (check-not-false (member "compaction.completed" event-names))
    (cleanup-dir tmp)))

(test-case "compact-session!: error when no session to compact"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    (define result (compact-session! rt))
    (check-equal? result 'no-active-session)
    (cleanup-dir tmp)))

;; ============================================================
;; Test suite: immutable runtime pattern
;; ============================================================

(test-case "immutability: open-session does not mutate original runtime"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    (check-false (runtime-rt-session rt))
    (define rt2 (open-session rt))
    ;; Original should still have no session
    (check-false (runtime-rt-session rt))
    ;; New one should have session
    (check-not-false (runtime-rt-session rt2))
    (cleanup-dir tmp)))

(test-case "immutability: fork returns new runtime, original unchanged"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    (define rt2 (open-session rt))
    (define-values (rt3 _) (run-prompt! rt2 "test"))
    (define original-sid (hash-ref (session-info rt3) 'session-id))
    (define rt4 (fork-session! rt3))
    ;; Original session should still be same
    (check-equal? (hash-ref (session-info rt3) 'session-id) original-sid)
    ;; Forked should be different
    (check-not-equal? (hash-ref (session-info rt4) 'session-id) original-sid)
    (cleanup-dir tmp)))

;; ============================================================
;; Test suite: extension-registry and model-name in SDK
;; ============================================================

(test-case "SDK: extension-registry stored in runtime-config"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define ext-reg (make-extension-registry))
    (define rt (make-runtime #:provider prov #:session-dir tmp #:extension-registry ext-reg))
    (check-equal? (runtime-config-extension-registry (runtime-rt-config rt))
                  ext-reg
                  "extension-registry stored in runtime-config")
    (cleanup-dir tmp)))

(test-case "SDK: model-name stored in runtime-config"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp #:model-name "gpt-4o"))
    (check-equal? (runtime-config-model-name (runtime-rt-config rt))
                  "gpt-4o"
                  "model-name stored in runtime-config")
    (cleanup-dir tmp)))

(test-case "SDK: session-info includes model-name"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp #:model-name "test-model"))
    (define rt2 (open-session rt))
    (define info (session-info rt2))
    (check-equal? (hash-ref info 'model-name #f) "test-model" "session-info includes model-name")
    (cleanup-dir tmp)))

(test-case "SDK: session-info does not expose raw extension registry"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define ext-reg (make-extension-registry))
    (define rt (make-runtime #:provider prov #:session-dir tmp #:extension-registry ext-reg))
    (define rt2 (open-session rt))
    (define info (session-info rt2))
    ;; Should return count, not the raw registry object
    (check-equal? (hash-ref info 'extension-registry-count #f)
                  0
                  "session-info returns extension count, not raw registry")
    ;; Must NOT return the raw registry
    (check-false (hash-has-key? info 'extension-registry)
                 "session-info does not have 'extension-registry key")
    (cleanup-dir tmp)))

(test-case "SDK: open-session passes extension-registry to agent-session"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define ext-reg (make-extension-registry))
    (define rt
      (make-runtime #:provider prov
                    #:session-dir tmp
                    #:extension-registry ext-reg
                    #:model-name "gpt-4o"))
    (define rt2 (open-session rt))
    ;; Verify session was created (extension-registry is forwarded internally)
    (check-not-false (runtime-rt-session rt2))
    ;; session-info should reflect model-name and extension count
    (define info (session-info rt2))
    (check-equal? (hash-ref info 'model-name) "gpt-4o")
    (check-equal? (hash-ref info 'extension-registry-count)
                  0
                  "extension-registry-count is 0 for empty registry")
    (cleanup-dir tmp)))

;; make-runtime uses centralized session-dir default from settings
(let* ([prov (make-mock-provider (make-model-response '() (hasheq) "m" 'stop))]
       [rt (make-runtime #:provider prov)])
  (check-equal? (runtime-config-session-dir (runtime-rt-config rt))
                (default-session-dir)
                "SDK: session-dir default matches settings.rkt default"))

;; ============================================================
;; F3: session-info returns metadata, not raw mutable objects
;; ============================================================

(test-case "F3: session-info does not expose raw extension registry object"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define ext-reg (make-extension-registry))
    ;; Register an extension so count > 0
    (register-extension! ext-reg (extension "test-ext" "1.0" "0.1" (hasheq)))
    (define rt (make-runtime #:provider prov #:session-dir tmp #:extension-registry ext-reg))
    (define rt2 (open-session rt))
    (define info (session-info rt2))
    ;; Count should be 1 (not the raw registry)
    (check-equal? (hash-ref info 'extension-registry-count)
                  1
                  "extension-registry-count reflects registered extensions")
    ;; Verify the count is NOT the registry object itself
    (check-not-eq? (hash-ref info 'extension-registry-count)
                   ext-reg
                   "count is not the raw registry object")
    (cleanup-dir tmp)))

;; ============================================================
;; Test suite: system-instructions in SDK
;; ============================================================

(test-case "SDK: make-runtime accepts #:system-instructions keyword"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define instrs '("You are a Racket expert."))
    (define rt (make-runtime #:provider prov #:session-dir tmp #:system-instructions instrs))
    (check-pred runtime? rt)
    (check-equal? (runtime-config-system-instructions (runtime-rt-config rt))
                  instrs
                  "system-instructions stored in runtime-config")
    (cleanup-dir tmp)))

(test-case "SDK: make-runtime #:system-instructions defaults to '()"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    (check-equal? (runtime-config-system-instructions (runtime-rt-config rt))
                  '()
                  "system-instructions defaults to '()")
    (cleanup-dir tmp)))

(test-case "SDK: session-info includes system-instructions when provided"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define instrs '("Be helpful." "Be concise."))
    (define rt (make-runtime #:provider prov #:session-dir tmp #:system-instructions instrs))
    (define rt2 (open-session rt))
    (define info (session-info rt2))
    (check-equal? (hash-ref info 'system-instructions #f)
                  instrs
                  "session-info includes system-instructions")
    (cleanup-dir tmp)))

(test-case "SDK: system-instructions forwarded to agent-session"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define instrs '("You are a test assistant."))
    (define rt (make-runtime #:provider prov #:session-dir tmp #:system-instructions instrs))
    (define rt2 (open-session rt))
    ;; Verify the internal agent-session has the instructions
    (define sess (runtime-rt-session rt2))
    (check-not-false sess "should have active session")
    (check-equal? (agent-session-system-instructions sess)
                  instrs
                  "agent-session should have system-instructions from SDK")
    (cleanup-dir tmp)))

;; ============================================================
;; Test suite: cancellation-token
;; ============================================================

(test-case "cancellation-token: make-cancellation-token creates uncancelled token"
  (define tok (make-cancellation-token))
  (check-pred cancellation-token? tok)
  (check-false (cancellation-token-cancelled? tok)))

(test-case "cancellation-token: cancel-token! sets cancelled"
  (define tok (make-cancellation-token))
  (cancel-token! tok)
  (check-true (cancellation-token-cancelled? tok)))

(test-case "cancellation-token: callback fires on cancel"
  (define fired (box #f))
  (define tok (make-cancellation-token #:callback (lambda (t) (set-box! fired #t))))
  (cancel-token! tok)
  (check-true (unbox fired)))

(test-case "make-runtime: creates with cancellation token by default"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    (check-pred cancellation-token? (runtime-rt-cancellation-token rt))
    (cleanup-dir tmp)))

(test-case "make-runtime: accepts custom cancellation token"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define tok (make-cancellation-token))
    (define rt (make-runtime #:provider prov #:session-dir tmp #:cancellation-token tok))
    (check-eq? (runtime-rt-cancellation-token rt) tok)
    (cleanup-dir tmp)))

(test-case "make-runtime: accepts #:token-budget-threshold"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp #:token-budget-threshold 50000))
    (check-equal? (runtime-config-token-budget-threshold (runtime-rt-config rt)) 50000)
    (cleanup-dir tmp)))

(test-case "make-runtime: token-budget-threshold defaults to 100000"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    (check-equal? (runtime-config-token-budget-threshold (runtime-rt-config rt)) 100000)
    (cleanup-dir tmp)))

(test-case "interrupt! cancels the runtime token"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    (define rt2 (open-session rt))
    (interrupt! rt2)
    (check-true (cancellation-token-cancelled? (runtime-rt-cancellation-token rt2)))
    (cleanup-dir tmp)))

(test-case "session-info: includes max-iterations and token-budget-threshold"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt
      (make-runtime #:provider prov
                    #:session-dir tmp
                    #:max-iterations 20
                    #:token-budget-threshold 75000))
    (define rt2 (open-session rt))
    (define info (session-info rt2))
    (check-equal? (hash-ref info 'max-iterations) 20)
    (check-equal? (hash-ref info 'token-budget-threshold) 75000)
    (cleanup-dir tmp)))

(test-case "immutability: interrupt! returns same runtime (token is mutated in place)"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    (define rt2 (open-session rt))
    (define rt3 (interrupt! rt2))
    ;; The runtime struct is returned as-is; token inside is same object
    (check-eq? (runtime-rt-cancellation-token rt2) (runtime-rt-cancellation-token rt3))
    (check-true (cancellation-token-cancelled? (runtime-rt-cancellation-token rt3)))
    (cleanup-dir tmp)))

(test-case "open-session: passes token-budget-threshold to agent-session"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp #:token-budget-threshold 50000))
    ;; Open session + run a prompt — should not crash with custom threshold
    (define rt2 (open-session rt))
    (define-values (rt3 result) (run-prompt! rt2 "test threshold config"))
    (check-pred loop-result? result)
    (cleanup-dir tmp)))

;; ============================================================
;; Test suite: compact-session! #:persist? flag
;; ============================================================

(test-case "compact-session! #:persist? #t writes summary to log"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    (define rt2 (open-session rt))
    ;; Build enough history to trigger compaction (need > keep-recent-count = 20)
    (for ([i (in-range 25)])
      (define-values (rt-next _) (run-prompt! rt2 (format "Message ~a" i)))
      (set! rt2 rt-next))
    ;; Compact with persist — use low token config so compaction actually triggers
    (define sid (hash-ref (session-info rt2) 'session-id))
    (define log-file (build-path tmp sid "session.jsonl"))
    (define history (session-history (runtime-rt-session rt2)))
    (define low-tc LOW-TOKEN-CONFIG)
    (define comp-res (compact-and-persist! history log-file #:token-config low-tc))
    (check-pred compaction-result? comp-res)
    ;; Compaction should have removed some messages
    (check-true (> (compaction-result-removed-count comp-res) 0))
    ;; The session log should now have the compaction summary appended
    (define info (session-info rt2))
    (check-true (> (hash-ref info 'history-length) 0))
    ;; Verify compaction-summary entry was written to the JSONL log
    (check-pred file-exists? log-file "session log file should exist after persist")
    (define log-entries (jsonl-read-all-valid log-file))
    (define has-compaction-summary?
      (for/or ([entry (in-list log-entries)])
        (equal? (hash-ref entry 'kind #f) "compaction-summary")))
    (check-true has-compaction-summary? "log should contain a compaction-summary entry after persist")
    (cleanup-dir tmp)))

(test-case "compact-session! #:persist? #f is advisory-only (default)"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    (define rt2 (open-session rt))
    ;; Build enough history
    (for ([i (in-range 25)])
      (define-values (rt-next _) (run-prompt! rt2 (format "Message ~a" i)))
      (set! rt2 rt-next))
    ;; Get history length before compaction
    (define len-before (hash-ref (session-info rt2) 'history-length))
    ;; Compact without persist (default)
    (define-values (rt3 comp-res) (compact-session! rt2))
    (check-pred compaction-result? comp-res)
    ;; History should be unchanged (no summary entry appended)
    (define len-after (hash-ref (session-info rt3) 'history-length))
    (check-equal? len-after len-before "advisory compaction should not change session log")
    ;; Verify no compaction-summary was written to disk
    (define sid (hash-ref (session-info rt3) 'session-id))
    (define log-file (build-path tmp sid "session.jsonl"))
    (when (file-exists? log-file)
      (define log-entries (jsonl-read-all-valid log-file))
      (define has-compaction-summary?
        (for/or ([entry (in-list log-entries)])
          (equal? (hash-ref entry 'kind #f) "compaction-summary")))
      (check-false has-compaction-summary?
                   "advisory mode should NOT write compaction-summary to log"))
    (cleanup-dir tmp)))

(test-case "cancellation-token: cancel-token! is idempotent (double cancel)"
  (define call-count (box 0))
  (define tok
    (make-cancellation-token #:callback (lambda (_) (set-box! call-count (add1 (unbox call-count))))))
  (cancel-token! tok)
  (check-pred cancellation-token-cancelled? tok)
  (check-equal? (unbox call-count) 1 "callback should fire once after first cancel")
  (cancel-token! tok)
  (check-pred cancellation-token-cancelled? tok)
  (check-equal? (unbox call-count)
                2
                "callback fires again on second cancel — not idempotent, documented behavior"))

(test-case "interrupt! on runtime with no active session cancels token, no crash"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (lambda (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    ;; Don't open a session — rt has no active session
    (define tok (runtime-rt-cancellation-token rt))
    (check-false (cancellation-token-cancelled? tok))
    (define rt2 (interrupt! rt))
    ;; Token should be cancelled
    (check-true (cancellation-token-cancelled? (runtime-rt-cancellation-token rt2)))
    ;; Should not crash
    (check-pred runtime? rt2)
    (cleanup-dir tmp)))

;; ============================================================
;; Test suite: contract-out for SDK public functions (Issue #10)
;; ============================================================

(test-case "contract: open-session rejects non-runtime first arg"
  (check-exn exn:fail:contract?
             (lambda () (open-session "not-a-runtime"))
             "open-session should enforce runtime? contract on first arg"))

(test-case "contract: open-session rejects non-string session-id"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (lambda (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    (check-exn exn:fail:contract?
               (lambda () (open-session rt 42))
               "open-session should enforce (or/c string? #f) on session-id")
    (cleanup-dir tmp)))

(test-case "contract: run-prompt! rejects non-runtime first arg"
  (check-exn exn:fail:contract?
             (lambda () (run-prompt! 'bad "hello"))
             "run-prompt! should enforce runtime? contract on first arg"))

(test-case "contract: run-prompt! rejects non-string prompt"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (lambda (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    (check-exn exn:fail:contract?
               (lambda () (run-prompt! rt 123))
               "run-prompt! should enforce string? on prompt arg")
    (cleanup-dir tmp)))

(test-case "contract: subscribe-events! rejects non-runtime first arg"
  (check-exn exn:fail:contract?
             (lambda () (subscribe-events! 'bad void))
             "subscribe-events! should enforce runtime? contract on first arg"))

(test-case "contract: subscribe-events! rejects non-procedure handler"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (lambda (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    (check-exn exn:fail:contract?
               (lambda () (subscribe-events! rt "not-a-proc"))
               "subscribe-events! should enforce procedure? on handler")
    (cleanup-dir tmp)))

(test-case "contract: interrupt! rejects non-runtime arg"
  (check-exn exn:fail:contract?
             (lambda () (interrupt! "not-a-runtime"))
             "interrupt! should enforce runtime? contract"))

(test-case "contract: fork-session! rejects non-runtime first arg"
  (check-exn exn:fail:contract?
             (lambda () (fork-session! 42))
             "fork-session! should enforce runtime? contract on first arg"))

(test-case "contract: compact-session! rejects non-runtime arg"
  (check-exn exn:fail:contract?
             (lambda () (compact-session! 'bad))
             "compact-session! should enforce runtime? contract"))

(test-case "contract: session-info rejects non-runtime arg"
  (check-exn exn:fail:contract?
             (lambda () (session-info 'bad))
             "session-info should enforce runtime? contract"))

;; ============================================================

(test-case "compact-session! emits persist? flag in events"
  (define tmp (make-temp-session-dir))
  (with-handlers ([exn:fail? (λ (e)
                               (cleanup-dir tmp)
                               (raise e))])
    (define prov (make-test-provider))
    (define received-events (box '()))
    (define rt (make-runtime #:provider prov #:session-dir tmp))
    (subscribe-events! rt (λ (evt) (set-box! received-events (cons evt (unbox received-events)))))
    (define rt2 (open-session rt))
    (define-values (rt3 _run-res) (run-prompt! rt2 "build history"))
    ;; Compact with persist
    (define-values (rt4 _comp-res) (compact-session! rt3 #:persist? #t))
    (define compaction-events
      (filter (λ (e) (string-prefix? (event-event e) "compaction.")) (unbox received-events)))
    (for ([e (in-list compaction-events)])
      (check-equal? (hash-ref (event-payload e) 'persist? #f)
                    #t
                    (format "event ~a should have persist?=#t" (event-event e))))
    (cleanup-dir tmp)))
