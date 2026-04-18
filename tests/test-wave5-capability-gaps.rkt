#lang racket

;; tests/test-wave5-capability-gaps.rkt — Wave 5 capability gaps tests
;;
;; Tests for v0.11.0 Wave 5 sub-issues:
;;   #1193: Subagent Spawning — spawn-subagent tool
;;   #1194: Process-Safe Settings Writes — lockfile
;;   #1195: Additional LLM Provider Adapters — Azure OpenAI
;;   #1196: SDK API Enrichment — convenience aliases

(require rackunit
         racket/string
         racket/file
         racket/port
         ;; #1193
         "../tools/builtins/spawn-subagent.rkt"
         "../tools/tool.rkt"
         "../tools/registry-defaults.rkt"
         ;; #1194
         "../util/lockfile.rkt"
         ;; #1195
         "../llm/azure-openai.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt"
         "../runtime/provider-factory.rkt"
         ;; #1196
         "../interfaces/sdk.rkt")

;; ============================================================
;; #1193: Subagent Spawning
;; ============================================================

(test-case "#1193: spawn-subagent tool is registered"
  (define registry (make-tool-registry))
  (register-default-tools! registry)
  (define tool (lookup-tool registry "spawn-subagent"))
  (check-not-false tool "spawn-subagent should be registered")
  (when tool
    (check-equal? (tool-name tool) "spawn-subagent")
    (check-true (string? (tool-description tool)))
    (check-true (hash? (tool-schema tool)))))

(test-case "#1193: spawn-subagent schema requires task"
  (define registry (make-tool-registry))
  (register-default-tools! registry)
  (define tool (lookup-tool registry "spawn-subagent"))
  (check-not-false tool)
  (when tool
    (define schema (tool-schema tool))
    (define required (hash-ref schema 'required '()))
    (check-not-false (member "task" required) "task should be required")
    (define props (hash-ref schema 'properties (hasheq)))
    (check-not-false (hash-ref props 'task #f))
    (check-not-false (hash-ref props 'role #f))
    (check-not-false (hash-ref props 'max-turns #f))
    (check-not-false (hash-ref props 'model #f))
    (check-not-false (hash-ref props 'tools #f))))

(test-case "#1193: spawn-subagent returns error without task"
  (define result (tool-spawn-subagent (hasheq)))
  (check-true (tool-result? result))
  (check-true (tool-result-is-error? result)))

(test-case "#1193: spawn-subagent executes with mock provider"
  (define result (tool-spawn-subagent (hasheq 'task "List all files")))
  (check-true (tool-result? result))
  ;; With mock provider it should succeed
  (check-false (tool-result-is-error? result)
               (format "expected success, got error: ~a" (tool-result-content result))))

(test-case "#1193: spawn-subagent respects max-turns parameter"
  (define result (tool-spawn-subagent (hasheq 'task "test task" 'max-turns 1)))
  (check-true (tool-result? result))
  (when (not (tool-result-is-error? result))
    (define details (tool-result-details result))
    (check-not-false details "should have details")
    (when (hash? details)
      (check-equal? (hash-ref details 'turns-used #f) 1))))

;; ============================================================
;; #1194: Process-Safe Settings Writes — Lockfile
;; ============================================================

(test-case "#1194: call-with-lock acquires and releases"
  (define tmp-dir (make-temporary-file "q-lock-test-~a" 'directory))
  (define target (build-path tmp-dir "test-file.txt"))
  (define result (call-with-lock target
                  (lambda () "locked-result")
                  #:timeout-ms 1000))
  (check-equal? result "locked-result")
  (delete-directory/files tmp-dir))

(test-case "#1194: with-lock-result returns ok on success"
  (define tmp-dir (make-temporary-file "q-lock-test-~a" 'directory))
  (define target (build-path tmp-dir "test.txt"))
  (define result (with-lock-result target
                   (lambda () 42)
                   #:timeout-ms 1000))
  (check-equal? (car result) 'ok)
  (check-equal? (cadr result) 42)
  (delete-directory/files tmp-dir))

(test-case "#1194: sequential locks on same target succeed"
  (define tmp-dir (make-temporary-file "q-lock-test-~a" 'directory))
  (define target (build-path tmp-dir "sequential.txt"))
  (define r1 (call-with-lock target (lambda () 1) #:timeout-ms 1000))
  (define r2 (call-with-lock target (lambda () 2) #:timeout-ms 1000))
  (check-equal? r1 1)
  (check-equal? r2 2)
  (delete-directory/files tmp-dir))

(test-case "#1194: lock protects file write"
  (define tmp-dir (make-temporary-file "q-lock-test-~a" 'directory))
  (define target (build-path tmp-dir "protected.txt"))
  (call-with-lock target
    (lambda ()
      (call-with-output-file target
        (lambda (out) (display "hello" out))
        #:exists 'replace)
      "written")
    #:timeout-ms 1000)
  (check-true (file-exists? target))
  (check-equal? (file->string target) "hello")
  (delete-directory/files tmp-dir))

;; ============================================================
;; #1195: Additional LLM Provider Adapters — Azure OpenAI
;; ============================================================

(test-case "#1195: make-azure-openai-provider requires api-key"
  (check-exn
   exn:fail?
   (lambda ()
     (make-azure-openai-provider (hasheq 'model "gpt-4"
                                          'base-url "https://test.openai.azure.com"
                                          'api-version "2024-02-15-preview")))))

(test-case "#1195: make-azure-openai-provider creates provider with valid config"
  (define provider
    (make-azure-openai-provider
     (hasheq 'api-key "test-key"
             'model "gpt-4"
             'base-url "https://test.openai.azure.com/openai/deployments/gpt4"
             'api-version "2024-02-15-preview")))
  (check-true (provider? provider))
  (check-equal? (provider-name provider) "Azure OpenAI"))

(test-case "#1195: Azure provider reports streaming capability"
  (define provider
    (make-azure-openai-provider
     (hasheq 'api-key "test-key"
             'model "gpt-4"
             'base-url "https://test.openai.azure.com")))
  (define caps (provider-capabilities provider))
  (check-true (hash? caps))
  (check-true (hash-ref caps 'streaming #f)))

(test-case "#1195: Azure provider is dispatched for 'azure' name"
  ;; Verify that create-provider-for-name handles "azure"
  (define config (hasheq 'api-key "test-key" 'model "gpt-4"))
  (define provider (create-provider-for-name "azure" #f "test-key" "gpt-4"))
  (check-true (provider? provider))
  (check-equal? (provider-name provider) "Azure OpenAI"))

;; ============================================================
;; #1196: SDK API Enrichment
;; ============================================================

(test-case "#1196: q:create-session is bound"
  (check-true (procedure? q:create-session)))

(test-case "#1196: q:session-send is bound"
  (check-true (procedure? q:session-send)))

(test-case "#1196: q:session-subscribe is bound"
  (check-true (procedure? q:session-subscribe)))

(test-case "#1196: q:session-interrupt is bound"
  (check-true (procedure? q:session-interrupt)))

(test-case "#1196: q:session-fork is bound"
  (check-true (procedure? q:session-fork)))

(test-case "#1196: q:session-compact is bound"
  (check-true (procedure? q:session-compact)))

(test-case "#1196: q:session-info is bound"
  (check-true (procedure? q:session-info)))

(test-case "#1196: enriched API creates sessions via q:create-session"
  ;; Create a runtime with mock provider
  (define mock-response
    (make-model-response (list (hasheq 'type "text" 'text "test"))
                         (hasheq 'prompt-tokens 0 'completion-tokens 0 'total-tokens 0)
                         "mock" 'stop))
  (define provider (make-mock-provider mock-response #:name "test-mock"))
  (define rt (q:create-session #:provider provider))
  (check-true (runtime? rt)))
