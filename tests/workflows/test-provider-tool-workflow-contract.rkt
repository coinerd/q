#lang racket/base

;; @speed slow  ;; @suite workflows

;; BOUNDARY: integration
;; @suite workflows
;; @boundary integration
;; @speed fast
;; @mutates none
;; tests/workflows/test-provider-tool-workflow-contract.rkt — Provider/tool workflow contracts
;;
;; Composes provider scenario DSL + tool-turn harness to verify
;; cross-boundary contracts: tool execution, result persistence,
;; blocked tool handling, and multi-tool ordering.

(require rackunit
         racket/list
         "../../llm/model.rkt"
         "../../llm/provider.rkt"
         "../../tools/tool.rkt"
         (only-in "../../tools/registry.rkt" with-registry-snapshot)
         "../helpers/provider-scenarios.rkt"
         "../helpers/tool-turn-scenarios.rkt")

;; ---------------------------------------------------------------------------
;; Provider asks for one tool → tool executes → provider answers
;; ---------------------------------------------------------------------------

(test-case "workflow: single tool roundtrip"
  (define sc (turn-scenario-tool-call "bash" #:result "file.txt"))
  (define prov (scenario->provider sc))
  (define reg (scenario->tool-registry sc))
  
  ;; Step 1: provider returns tool call
  (define resp1 (provider-send prov (make-model-request '() '() (hash))))
  (check-equal? (model-response-stop-reason resp1) 'tool-calls)
  (define tc (car (model-response-content resp1)))
  (check-equal? (hash-ref tc 'name) "bash")
  (check-true (hash-has-key? tc 'id))
  
  ;; Step 2: tool executes
  (define tools (turn-scenario-tools sc))
  (define tool-result ((tool-execute (car tools)) (hash) #f))
  (check-true (tool-result? tool-result))
  (check-false (tool-result-is-error? tool-result))
  (check-equal? (tool-result-content tool-result) "file.txt")
  
  ;; Step 3: provider returns final text
  (define resp2 (provider-send prov (make-model-request '() '() (hash))))
  (check-equal? (model-response-stop-reason resp2) 'stop))

;; ---------------------------------------------------------------------------
;; Blocked tool emits structured blocked result
;; ---------------------------------------------------------------------------

(test-case "workflow: blocked tool (tool not in registry)"
  (define sc (turn-scenario-blocked-tool "dangerous-tool"))
  (define prov (scenario->provider sc))
  (define reg (scenario->tool-registry sc))
  
  ;; Provider returns tool call for "dangerous-tool"
  (define resp1 (provider-send prov (make-model-request '() '() (hash))))
  (check-equal? (model-response-stop-reason resp1) 'tool-calls)
  (define tc (car (model-response-content resp1)))
  (check-equal? (hash-ref tc 'name) "dangerous-tool")
  
  ;; Tool not found in registry — lookup returns #f
  (check-false (with-registry-snapshot reg
                 (lambda (tools-hash)
                   (hash-ref tools-hash "dangerous-tool" #f)))))

;; ---------------------------------------------------------------------------
;; Multi-tool order remains stable
;; ---------------------------------------------------------------------------

(test-case "workflow: multi-tool ordering is stable"
  (define tool-names '("bash" "read" "grep"))
  (define sc (turn-scenario-multi-tool tool-names))
  (define prov (scenario->provider sc))
  (define reg (scenario->tool-registry sc))
  
  ;; Provider returns multi-tool call
  (define resp1 (provider-send prov (make-model-request '() '() (hash))))
  (check-equal? (model-response-stop-reason resp1) 'tool-calls)
  (define content (model-response-content resp1))
  (check-true (>= (length content) 2))
  
  ;; Tools execute in order
  (define tools (turn-scenario-tools sc))
  (define results
    (for/list ([t (in-list tools)])
      (define r ((tool-execute t) (hash) #f))
      (tool-result-content r)))
  (check-equal? results '("bash done" "read done" "grep done")))

;; ---------------------------------------------------------------------------
;; Tool result structure contract
;; ---------------------------------------------------------------------------

(test-case "workflow: tool result has required fields"
  (define sc (turn-scenario-tool-call "read" #:result "contents"))
  (define tools (turn-scenario-tools sc))
  (define result ((tool-execute (car tools)) (hash) #f))
  (check-true (tool-result? result))
  (check-true (or (string? (tool-result-content result))
                  (hash? (tool-result-content result))
                  (list? (tool-result-content result))))
  (check-true (or (hash? (tool-result-details result))
                  (not (tool-result-details result)))))

;; ---------------------------------------------------------------------------
;; Provider response structure contract
;; ---------------------------------------------------------------------------

(test-case "workflow: provider response has required fields"
  (define sc (turn-scenario-text "hello"))
  (define prov (scenario->provider sc))
  (define resp (provider-send prov (make-model-request '() '() (hash))))
  (check-true (model-response? resp))
  (check-true (list? (model-response-content resp)))
  (check-true (symbol? (model-response-stop-reason resp)))
  (check-true (hash? (model-response-usage resp)))
  (check-true (string? (model-response-model resp))))

(test-case "workflow: provider returns content with usage"
  (define-values (prov cap) (make-scenario-provider
                              (list (scenario-text "response text"))))
  (define resp (provider-send prov (make-model-request '() '() (hash))))
  (check-true (model-response? resp))
  (check-true (hash? (model-response-usage resp)))
  (check-true (>= (hash-ref (model-response-usage resp) 'total-tokens 0) 0)))

;; ---------------------------------------------------------------------------
;; Error scenario contract
;; ---------------------------------------------------------------------------

(test-case "workflow: error scenario raises exception"
  (define-values (prov cap) (make-scenario-provider
                              (list (scenario-error "API rate limit exceeded"))))
  (check-exn exn:fail?
             (lambda () (provider-send prov (make-model-request '() '() (hash))))))
