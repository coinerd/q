#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: unit

;; W5 (#8769) — production lifecycle / provider provenance truth.
;; Red-first harness for provider-neutral telemetry, correlation, and the
;; event-struct completeness gates (tests 1, 2, 3, 4, 5, 6, 7, 8).

(require rackunit
         racket/match
         ;; --- Slice 1: provider telemetry core ---
         "../llm/provider-telemetry.rkt"
         (only-in "../llm/model.rkt" model-response-provenance)
         ;; --- Slice 2: adapters propagate native response identity ---
         (only-in "../llm/openai-compatible.rkt" openai-parse-response)
         (only-in "../llm/anthropic.rkt" anthropic-parse-response)
         (only-in "../llm/gemini.rkt" gemini-parse-response)
         ;; --- Slice 3: event-struct completeness ---
         (only-in "../agent/event-structs/session-events.rkt"
                  session-resumed-event
                  session-resumed-event-resumed-id)
         (only-in "../agent/event-structs/memory-events.rkt"
                  MEMORY-EVENT-SCHEMA-VERSION
                  mem-retrieval-performed-event
                  mem-retrieval-performed-event-fields
                  mem-retrieval-performed-event-result-id
                  mem-retrieval-performed-event-matched-item-ids
                  mem-retrieval-performed-event-presence)
         (only-in "../agent/event-structs/mas-events.rkt"
                  mas-spawn-approval-requested-event
                  mas-spawn-approval-requested-event-request-id
                  mas-spawn-approval-requested-event-commitment-digest
                  mas-spawn-approval-requested-event-presentation-digest
                  mas-spawn-approval-terminal-event
                  mas-spawn-approval-terminal-event-terminal-status
                  mas-child-terminal-event
                  mas-child-terminal-event-outcome
                  mas-child-terminal-event-result-digest)
         (only-in "../extensions/gsd/event-structs.rkt"
                  gsd-transition-succeeded-event
                  gsd-transition-succeeded-event-type
                  gsd-transition-succeeded-event-from
                  gsd-transition-succeeded-event-to)
         (only-in "../util/event/event-macro.rkt" lookup-event-schema-version)
         (only-in "../agent/event-structs/base.rkt" typed-event-session-id typed-event-type))

;; ============================================================
;; Test 1 — production v2 telemetry serializes the full identity chain
;; ============================================================

(test-case "telemetry serializes session/turn/q-request/provider/model/adapter identity"
  (define t
    (make-provider-telemetry #:session-id "sess-1"
                             #:turn-id "turn-1"
                             #:q-request-id "qrq-1"
                             #:adapter "anthropic"
                             #:configured-provider "anthropic"
                             #:configured-model "claude-3"
                             #:stage 'completed
                             #:native-request-id "req-abc"
                             #:native-response-id "msg-xyz"
                             #:native-model "claude-3-opus"))
  (define j (provider-telemetry->jsexpr t))
  (check-equal? (hash-ref j 'schemaVersion) 2)
  (check-equal? (hash-ref j 'sessionId) "sess-1")
  (check-equal? (hash-ref j 'turnId) "turn-1")
  (check-equal? (hash-ref j 'qRequestId) "qrq-1")
  (check-equal? (hash-ref j 'adapter) "anthropic")
  (check-equal? (hash-ref j 'configuredProvider) "anthropic")
  (check-equal? (hash-ref j 'configuredModel) "claude-3")
  (check-equal? (hash-ref j 'nativeRequestId) "req-abc")
  (check-equal? (hash-ref j 'nativeResponseId) "msg-xyz")
  (check-equal? (hash-ref j 'nativeModel) "claude-3-opus")
  ;; round-trip
  (check-equal? (provider-telemetry->jsexpr (jsexpr->provider-telemetry j)) j))

;; ============================================================
;; Test 3 — started/completed/terminal correlation; invented identity fails
;; ============================================================

(define (mk-started #:source [src 'provider-native] #:native-res [nr "real-resp-1"])
  (make-provider-telemetry #:session-id "sess-1"
                           #:turn-id "turn-1"
                           #:q-request-id "qrq-1"
                           #:adapter "openai-compatible"
                           #:configured-provider "openai"
                           #:configured-model "gpt-4"
                           #:stage 'started
                           #:source src
                           #:native-response-id nr))

(define (mk-completed #:source [src 'provider-native]
                      #:native-res [nr "real-resp-1"]
                      #:qreq [qreq "qrq-1"]
                      #:sess [sess "sess-1"]
                      #:turn [turn "turn-1"])
  (make-provider-telemetry #:session-id sess
                           #:turn-id turn
                           #:q-request-id qreq
                           #:adapter "openai-compatible"
                           #:configured-provider "openai"
                           #:configured-model "gpt-4"
                           #:stage 'completed
                           #:source src
                           #:native-response-id nr))

(define (mk-terminal)
  (make-provider-telemetry #:session-id "sess-1"
                           #:turn-id "turn-1"
                           #:q-request-id "qrq-1"
                           #:adapter "openai-compatible"
                           #:configured-provider "openai"
                           #:configured-model "gpt-4"
                           #:stage 'terminal
                           #:source 'provider-native
                           #:native-response-id "real-resp-1"))

(test-case "correlated provider-native started/completed/terminal chain is valid"
  (check-true (telemetry-chain-valid? (mk-started) (mk-completed) (mk-terminal)))
  (check-true (telemetry-chain-valid? (mk-started) (mk-completed))))

(test-case "mock source fails correlation"
  (check-false (telemetry-chain-valid? (mk-started) (mk-completed #:source 'mock))))

(test-case "locally-invented source fails correlation"
  (check-false (telemetry-chain-valid? (mk-started) (mk-completed #:source 'local-invented))))

(test-case "empty native response id fails correlation"
  (check-false (telemetry-chain-valid? (mk-started) (mk-completed #:native-res #f)))
  (check-false (telemetry-chain-valid? (mk-started) (mk-completed #:native-res ""))))

(test-case "mismatched session/turn/q-request fails correlation"
  (check-false (telemetry-chain-valid? (mk-started) (mk-completed #:sess "OTHER")))
  (check-false (telemetry-chain-valid? (mk-started) (mk-completed #:turn "OTHER")))
  (check-false (telemetry-chain-valid? (mk-started) (mk-completed #:qreq "OTHER"))))

(test-case "unrelated terminal fails correlation"
  (define bad-terminal
    (make-provider-telemetry #:session-id "sess-1"
                             #:turn-id "turn-1"
                             #:q-request-id "DIFFERENT"
                             #:adapter "openai-compatible"
                             #:configured-provider "openai"
                             #:configured-model "gpt-4"
                             #:stage 'terminal))
  (check-false (telemetry-chain-valid? (mk-started) (mk-completed) bad-terminal)))

;; ============================================================
;; Test 8 — hand-built / impossible fields reject
;; ============================================================

(test-case "telemetry source is constrained to the three allowed discriminators"
  ;; The contract accepts only 'provider-native 'mock 'local-invented.
  (check-exn exn:fail:contract?
             (lambda ()
               (make-provider-telemetry #:session-id "s"
                                        #:turn-id "t"
                                        #:q-request-id "q"
                                        #:adapter "a"
                                        #:configured-provider "p"
                                        #:configured-model "m"
                                        #:stage 'completed
                                        #:source 'bogus))))

(test-case "q-request-id generator yields distinct non-empty ids"
  (define a (gen-q-request-id))
  (define b (gen-q-request-id))
  (check-pred string? a)
  (check-pred (lambda (s) (> (string-length s) 0)) a)
  (check-false (equal? a b) "q-request ids must be distinct"))

;; ============================================================
;; Test 2 — adapters propagate provider-native response identity
;; ============================================================

(test-case "OpenAI-compatible adapter extracts native response id into provenance"
  (define raw
    (hasheq
     'id
     "chatcmpl-abc123"
     'model
     "gpt-4-turbo"
     'usage
     (hasheq)
     'choices
     (list (hasheq 'finish_reason "stop" 'message (hasheq 'role "assistant" 'content "hello")))))
  (define resp (openai-parse-response raw))
  (define prov (model-response-provenance resp))
  (check-pred hash? prov "provenance must be present")
  (check-equal? (hash-ref prov 'native-response-id) "chatcmpl-abc123")
  (check-equal? (hash-ref prov 'native-model) "gpt-4-turbo")
  (check-equal? (hash-ref prov 'adapter) "openai-compatible"))

(test-case "Anthropic adapter extracts native response id into provenance"
  (define raw
    (hasheq 'id
            "msg-01XyZ"
            'model
            "claude-3-opus"
            'usage
            (hasheq 'input_tokens 10 'output_tokens 5)
            'stop_reason
            "end_turn"
            'content
            (list (hasheq 'type "text" 'text "hello"))))
  (define resp (anthropic-parse-response raw))
  (define prov (model-response-provenance resp))
  (check-pred hash? prov)
  (check-equal? (hash-ref prov 'native-response-id) "msg-01XyZ")
  (check-equal? (hash-ref prov 'native-model) "claude-3-opus"))

(test-case "Gemini adapter extracts native response id into provenance"
  (define raw
    (hasheq
     'responseId
     "resp-abc-xyz"
     'modelVersion
     "gemini-1.5-pro"
     'usageMetadata
     (hasheq 'promptTokenCount 7 'candidatesTokenCount 3)
     'candidates
     (list (hasheq 'finishReason "STOP" 'content (hasheq 'parts (list (hasheq 'text "hello")))))))
  (define resp (gemini-parse-response raw))
  (define prov (model-response-provenance resp))
  (check-pred hash? prov)
  (check-equal? (hash-ref prov 'native-response-id) "resp-abc-xyz")
  (check-equal? (hash-ref prov 'native-model) "gemini-1.5-pro"))

(test-case "mock response without a native id yields no provenance"
  (define raw
    (hasheq 'model
            "unknown"
            'usage
            (hasheq)
            'choices
            (list (hasheq 'finish_reason "stop" 'message (hasheq 'role "assistant" 'content "hi")))))
  (define resp (openai-parse-response raw))
  (check-false (model-response-provenance resp) "empty native id must not invent provenance"))

;; ============================================================
;; Test 4 — session.resumed is a typed event carrying exact resumed id
;; ============================================================

(test-case "session-resumed typed event exists and carries the exact resumed session id"
  (define resumed "sess-RESUME-1")
  (define evt (session-resumed-event "session.resumed" 1000 resumed #f resumed))
  (check-equal? (typed-event-session-id evt) resumed)
  (check-equal? (session-resumed-event-resumed-id evt) resumed)
  (check-not-equal? (typed-event-type evt) "session.started"))

;; ============================================================
;; Test 5 — memory retrieval emits correlated result ids/presence
;; ============================================================

(test-case "memory retrieval event schema bumped to v2 and carries result-id/presence"
  (check-equal? MEMORY-EVENT-SCHEMA-VERSION 2)
  (check-true (not (not (memq 'result-id mem-retrieval-performed-event-fields))))
  (check-true (not (not (memq 'matched-item-ids mem-retrieval-performed-event-fields))))
  (check-true (not (not (memq 'presence mem-retrieval-performed-event-fields))))
  (define evt
    (mem-retrieval-performed-event "memory.retrieval.performed"
                                   1000
                                   "sess-1"
                                   "turn-1"
                                   "hello"
                                   2
                                   10
                                   'session
                                   45
                                   "mem-result-abc"
                                   '("mem-1" "mem-2")
                                   #t))
  (check-equal? (mem-retrieval-performed-event-result-id evt) "mem-result-abc")
  (check-equal? (mem-retrieval-performed-event-matched-item-ids evt) '("mem-1" "mem-2"))
  (check-true (mem-retrieval-performed-event-presence evt)))

;; ============================================================
;; Test 6 — MAS spawn approval / child events are typed and redacted
;; ============================================================

(test-case "MAS spawn approval events are typed and carry correlation without raw task"
  (define req "spawn-req-1")
  (define cd "abcd1234...")
  (define pd "efgh5678...")
  (define req-evt
    (mas-spawn-approval-requested-event "mas.spawn-approval.requested"
                                        1000
                                        "sess-1"
                                        "turn-1"
                                        req
                                        cd
                                        pd))
  (check-equal? (mas-spawn-approval-requested-event-request-id req-evt) req)
  (check-equal? (mas-spawn-approval-requested-event-commitment-digest req-evt) cd)
  (check-equal? (mas-spawn-approval-requested-event-presentation-digest req-evt) pd)
  (define term-evt
    (mas-spawn-approval-terminal-event "mas.spawn-approval.terminal"
                                       1000
                                       "sess-1"
                                       "turn-1"
                                       req
                                       cd
                                       pd
                                       "approved"))
  (check-equal? (mas-spawn-approval-terminal-event-terminal-status term-evt) "approved")
  (define child-evt
    (mas-child-terminal-event "mas.child.terminal"
                              1000
                              "sess-1"
                              "turn-1"
                              req
                              "success"
                              cd
                              "result-digest-xyz"))
  (check-equal? (mas-child-terminal-event-result-digest child-evt) "result-digest-xyz")
  (check-equal? (mas-child-terminal-event-outcome child-evt) "success"))

;; ============================================================
;; Test 7 — GSD wrapped events serialize through production constructors
;; ============================================================

(test-case "GSD transition event has explicit schema-version and serializes via constructor"
  (check-equal? (lookup-event-schema-version gsd-transition-succeeded-event-type) 1)
  (define evt
    (gsd-transition-succeeded-event "gsd.transition.succeeded"
                                    1000
                                    "sess-1"
                                    "turn-1"
                                    "plan"
                                    "execute"))
  (check-equal? (gsd-transition-succeeded-event-from evt) "plan")
  (check-equal? (gsd-transition-succeeded-event-to evt) "execute"))
