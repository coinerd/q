#lang racket/base

;; BOUNDARY: unit
;; @suite runtime
;; @boundary unit
;; @speed fast
;; @mutates none
;; Tests for tool-turn scenario harness (v0.83.5 W1)

(require rackunit
         racket/list
         "../llm/model.rkt"
         "../llm/provider.rkt"
                  (only-in "../tools/tool.rkt" tool-names tool-name tool-execute tool?)
         (only-in "../util/tool/tool-types.rkt" make-tool-result tool-result? tool-result-content tool-result-is-error?)
         "helpers/tool-turn-scenarios.rkt"
         "helpers/provider-scenarios.rkt")

;; ---------------------------------------------------------------------------
;; Mock tool construction
;; ---------------------------------------------------------------------------

(test-case "make-mock-tool: creates executable tool"
  (define t (make-mock-tool "test-tool"))
  (check-true (tool? t))
  (check-equal? (tool-name t) "test-tool"))

(test-case "make-mock-tool: custom handler"
  (define t (make-mock-tool "custom"
              #:handler (lambda (args ctx)
                          (make-tool-result "custom output" (hash) #f))))
  (define result ((tool-execute t) (hash) #f))
  (check-true (tool-result? result))
  (check-equal? (tool-result-content result) "custom output"))

(test-case "make-mock-tool-registry: registers tools"
  (define reg (make-mock-tool-registry
               (list (make-mock-tool "a") (make-mock-tool "b"))))
  (check-equal? (sort (tool-names reg) string<?) '("a" "b")))

;; ---------------------------------------------------------------------------
;; Scenario construction
;; ---------------------------------------------------------------------------

(test-case "turn-scenario-text: text-only scenario"
  (define sc (turn-scenario-text "Hello"))
  (check-equal? (turn-scenario-name sc) "text-only")
  (check-equal? (length (turn-scenario-provider-responses sc)) 1)
  (check-equal? (turn-scenario-tools sc) (list)))

(test-case "turn-scenario-tool-call: single tool scenario"
  (define sc (turn-scenario-tool-call "bash" #:result "file.txt"))
  (check-equal? (turn-scenario-name sc) "one-tool")
  (check-equal? (length (turn-scenario-provider-responses sc)) 2)
  (check-equal? (length (turn-scenario-tools sc)) 1))

(test-case "turn-scenario-multi-tool: multi-tool scenario"
  (define sc (turn-scenario-multi-tool '("bash" "read")))
  (check-equal? (length (turn-scenario-tools sc)) 2))

(test-case "turn-scenario-blocked-tool: blocked scenario"
  (define sc (turn-scenario-blocked-tool "dangerous-tool"))
  (check-equal? (turn-scenario-tools sc) (list))
  (check-equal? (length (turn-scenario-provider-responses sc)) 1))

(test-case "turn-scenario-malformed-tool: malformed scenario"
  (define sc (turn-scenario-malformed-tool))
  (check-equal? (turn-scenario-name sc) "malformed-tool"))

;; ---------------------------------------------------------------------------
;; Scenario -> provider / registry
;; ---------------------------------------------------------------------------

(test-case "scenario->provider: returns working provider"
  (define sc (turn-scenario-text "test"))
  (define prov (scenario->provider sc))
  (check-true (provider? prov))
  (define resp (provider-send prov (make-model-request '() '() (hash))))
  (check-equal? (hash-ref (car (model-response-content resp)) 'text) "test"))

(test-case "scenario->tool-registry: returns populated registry"
  (define sc (turn-scenario-tool-call "bash"))
  (define reg (scenario->tool-registry sc))
  (check-equal? (tool-names reg) '("bash")))

;; ---------------------------------------------------------------------------
;; Assertion helpers
;; ---------------------------------------------------------------------------

(test-case "check-tool-result-content: matches content"
  (define result (make-tool-result "ok" (hash) #f))
  (check-true (check-tool-result-content result "ok"))
  (check-false (check-tool-result-content result "fail")))

(test-case "check-event-order: validates event sequence"
  (check-true (check-event-order '(a b c) '(a b c)))
  (check-false (check-event-order '(a b) '(a b c)))
  (check-true (check-event-order
               (list (hash 'type 'response))
               '(response))))

;; ---------------------------------------------------------------------------
;; Tool execution through scenario tools
;; ---------------------------------------------------------------------------

(test-case "scenario tools execute and return results"
  (define sc (turn-scenario-tool-call "grep" #:result "found"))
  (define reg (scenario->tool-registry sc))
  (define tools (turn-scenario-tools sc))
  (check-true (not (null? tools)))
  (define result ((tool-execute (car tools)) (hash) #f))
  (check-true (tool-result? result))
  (check-equal? (tool-result-content result) "found"))

(test-case "multi-tool scenario: all tools execute"
  (define sc (turn-scenario-multi-tool '("bash" "read" "grep")))
  (define tools (turn-scenario-tools sc))
  (check-equal? (length tools) 3)
  (for ([t (in-list tools)])
    (define result ((tool-execute t) (hash) #f))
    (check-true (tool-result? result))
    (check-true (not (tool-result-is-error? result)))))

;; ---------------------------------------------------------------------------
;; Provider + tool integration
;; ---------------------------------------------------------------------------

(test-case "provider returns tool-call, tool executes"
  (define sc (turn-scenario-tool-call "read"))
  (define prov (scenario->provider sc))
  (define reg (scenario->tool-registry sc))
  ;; Step 1: provider returns tool-call response
  (define resp1 (provider-send prov (make-model-request '() '() (hash))))
  (check-equal? (model-response-stop-reason resp1) 'tool-calls)
  (define tc (car (model-response-content resp1)))
  (check-equal? (hash-ref tc 'name) "read")
  ;; Step 2: tool executes
  (define tools (turn-scenario-tools sc))
  (define tool-result ((tool-execute (car tools)) (hash) #f))
  (check-true (tool-result? tool-result))
  ;; Step 3: provider returns text after tool result
  (define resp2 (provider-send prov (make-model-request '() '() (hash))))
  (check-equal? (model-response-stop-reason resp2) 'stop))
