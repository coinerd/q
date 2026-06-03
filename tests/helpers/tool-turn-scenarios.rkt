#lang racket/base

;; BOUNDARY: unit
;; @suite runtime
;; @boundary unit
;; @speed fast
;; @mutates none
;; tests/helpers/tool-turn-scenarios.rkt — Focused tool-turn test scenarios
;;
;; Provides scenario constructors and assertions for testing tool-turn
;; interactions without requiring full agent-session scaffolding.

(require racket/list
         "../../llm/model.rkt"
         "../../llm/provider.rkt"
         (only-in "../../util/tool/tool-types.rkt" tool-result tool-result? tool-result-content tool-result-is-error? make-tool-result make-tool-call tool-call tool-call?)
         (only-in "../../tools/registry.rkt" make-tool-registry register-tool!)
         (only-in "../../tools/tool.rkt" make-tool tool-execute tool-name tool-names tool?)
         "provider-scenarios.rkt")


(provide make-mock-tool
         make-mock-tool-registry
         (struct-out turn-scenario)
         turn-scenario-text
         turn-scenario-tool-call
         turn-scenario-multi-tool
         turn-scenario-blocked-tool
         turn-scenario-malformed-tool
         scenario->provider
         scenario->tool-registry
         check-tool-result-content
         check-event-order)

;; ---------------------------------------------------------------------------
;; Mock tool
;; ---------------------------------------------------------------------------

(define (make-mock-tool name #:handler [handler #f])
  "Create a minimal tool that returns a fixed result or calls handler."
  (make-tool name
             (format "Mock ~a tool" name)
             (hash 'type "function"
                   'function (hash 'name name
                                   'parameters (hash 'type "object"
                                                     'properties (hash))))
             (or handler
                 (lambda (args ctx)
                   (make-tool-result (format "~a result" name) (hash) #f)))))

(define (make-mock-tool-registry tools)
  "Create a tool registry from a list of mock tools."
  (define reg (make-tool-registry))
  (for ([t (in-list tools)])
    (register-tool! reg t))
  reg)

;; ---------------------------------------------------------------------------
;; Turn scenario constructors
;; ---------------------------------------------------------------------------

(struct turn-scenario
  (name provider-responses tools expected-events)
  #:transparent)

(define (turn-scenario-text text)
  "A simple text turn with no tool calls."
  (turn-scenario
   "text-only"
   (list (scenario-text text))
   '()
   '(response)))

(define (turn-scenario-tool-call tool-name #:result [result "ok"])
  "A single tool-call turn."
  (turn-scenario
   "one-tool"
   (list
    (scenario-tool-call tool-name)
    (scenario-text (format "Tool ~a returned: ~a" tool-name result)))
   (list (make-mock-tool tool-name
                         #:handler (lambda (args ctx)
                                     (make-tool-result result (hash) #f))))
   '(tool-call tool-result response)))

(define (turn-scenario-multi-tool tool-names)
  "A multi-tool-call turn."
  (turn-scenario
   "multi-tool"
   (list
    (scenario-multi-tool (for/list ([n (in-list tool-names)])
                           (list n (hash))))
    (scenario-text "All tools completed"))
   (for/list ([n (in-list tool-names)])
     (make-mock-tool n
                     #:handler (lambda (args ctx)
                                 (make-tool-result (format "~a done" n) (hash) #f))))
   '(tool-call tool-call tool-result tool-result response)))

(define (turn-scenario-blocked-tool tool-name)
  "A blocked tool call (tool not found)."
  (turn-scenario
   "blocked-tool"
   (list (scenario-tool-call tool-name))
   '()  ;; no tools registered — call will be blocked
   '(tool-call tool-blocked response)))

(define (turn-scenario-malformed-tool)
  "A malformed tool call (missing name)."
  (turn-scenario
   "malformed-tool"
   (list (make-model-response
          (list (hash 'type "tool-call" 'id "tc-bad" 'name "" 'arguments (hash)))
          (hasheq 'prompt-tokens 5 'completion-tokens 3 'total-tokens 8)
          "scenario"
          'tool-calls))
   '()
   '(tool-call tool-error response)))

;; ---------------------------------------------------------------------------
;; Scenario -> provider/registry
;; ---------------------------------------------------------------------------

(define (scenario->provider scenario)
  "Extract a provider from a turn scenario."
  (define-values (prov cap) (make-scenario-provider (turn-scenario-provider-responses scenario)))
  prov)

(define (scenario->tool-registry scenario)
  "Extract a tool registry from a turn scenario."
  (make-mock-tool-registry (turn-scenario-tools scenario)))

;; ---------------------------------------------------------------------------
;; Assertion helpers
;; ---------------------------------------------------------------------------

(define (check-tool-result-content result expected-content)
  "Assert that a tool-result's content matches expected."
  (and (tool-result? result)
       (equal? (tool-result-content result) expected-content)))

(define (check-event-order events expected-types)
  "Check that a list of event type symbols matches the expected sequence."
  (define actual-types
    (for/list ([e (in-list events)])
      (if (hash? e)
          (hash-ref e 'type #f)
          e)))
  (equal? actual-types expected-types))
