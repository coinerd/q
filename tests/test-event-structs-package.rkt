#lang racket

;; BOUNDARY: integration

;; tests/test-event-structs-package.rkt — verify event-structs sub-module decomposition
;;
;; Tests that:
;; 1. Each sub-module compiles and exports expected identifiers
;; 2. The façade re-exports everything
;; 3. Struct inheritance works across sub-module boundaries
;; 4. Backward compatibility: all previously-exported names remain accessible

(require rackunit
         rackunit/text-ui
         "../agent/event-structs.rkt"
         ;; Direct sub-module imports
         "../agent/event-structs/base.rkt"
         "../agent/event-structs/turn-events.rkt"
         "../agent/event-structs/message-events.rkt"
         "../agent/event-structs/tool-events.rkt"
         "../agent/event-structs/provider-events.rkt"
         "../agent/event-structs/session-events.rkt")

(define es-tests
  (test-suite "Event Structs Package Decomposition"

    (test-case "base: typed-event struct"
      (check-true (procedure? typed-event?))
      (define ev (typed-event "test" 1000 "sess" "turn"))
      (check-true (typed-event? ev))
      (check-equal? (typed-event-type ev) "test")
      (check-equal? (typed-event-session-id ev) "sess"))

    (test-case "turn-events: construction and inheritance"
      (define tse
        (make-turn-start-event #:session-id "s1"
                               #:turn-id "t1"
                               #:timestamp 1000
                               #:model "gpt-4"
                               #:provider "openai"))
      (check-true (turn-start-event? tse))
      (check-true (typed-event? tse))
      (check-equal? (typed-event-type tse) "turn.started")
      (check-equal? (turn-start-event-model tse) "gpt-4")

      (define tee
        (make-turn-end-event #:session-id "s1"
                             #:turn-id "t1"
                             #:timestamp 2000
                             #:reason "complete"
                             #:duration-ms 1000))
      (check-true (turn-end-event? tee))
      (check-equal? (turn-end-event-reason tee) "complete"))

    (test-case "message-events: construction and inheritance"
      (define mse
        (make-message-start-event #:session-id "s1"
                                  #:turn-id "t1"
                                  #:timestamp 1000
                                  #:role "assistant"
                                  #:model "gpt-4"))
      (check-true (message-start-event? mse))
      (check-true (typed-event? mse))
      (check-equal? (message-start-event-role mse) "assistant"))

    (test-case "tool-events: tool-call-event and per-tool inheritance"
      (define tce
        (make-tool-call-event #:session-id "s1"
                              #:turn-id "t1"
                              #:timestamp 1000
                              #:tool-name "bash"
                              #:arguments (hasheq 'cmd "ls")
                              #:tool-call-id "tc-1"))
      (check-true (tool-call-event? tce))
      (check-true (typed-event? tce))
      (check-equal? (tool-call-event-tool-name tce) "bash")

      ;; Per-tool: bash
      (define bce
        (make-bash-tool-call-event #:session-id "s1"
                                   #:turn-id "t1"
                                   #:timestamp 1000
                                   #:command "ls -la"
                                   #:timeout 30
                                   #:cwd "/tmp"
                                   #:tool-call-id "tc-2"))
      (check-true (bash-tool-call-event? bce))
      (check-true (tool-call-event? bce))
      (check-true (typed-event? bce))
      (check-equal? (bash-tool-call-event-command bce) "ls -la")

      ;; Per-tool: edit
      (define ece
        (make-edit-tool-call-event #:session-id "s1"
                                   #:turn-id "t1"
                                   #:timestamp 1000
                                   #:path "/tmp/test.rkt"
                                   #:edits '()
                                   #:tool-call-id "tc-3"))
      (check-true (edit-tool-call-event? ece))
      (check-equal? (edit-tool-call-event-path ece) "/tmp/test.rkt"))

    (test-case "tool-events: tool-result-event"
      (define tre
        (make-tool-result-event #:session-id "s1"
                                #:turn-id "t1"
                                #:timestamp 1000
                                #:tool-call-id "tc-1"
                                #:content "output"
                                #:is-error? #f))
      (check-true (tool-result-event? tre))
      (check-true (typed-event? tre))
      (check-false (tool-result-event-is-error? tre)))

    (test-case "provider-events: construction and inheritance"
      (define pre
        (make-provider-request-event #:session-id "s1"
                                     #:turn-id "t1"
                                     #:timestamp 1000
                                     #:model "gpt-4"
                                     #:provider "openai"))
      (check-true (provider-request-event? pre))
      (check-true (typed-event? pre))
      (check-equal? (provider-request-event-model pre) "gpt-4"))

    (test-case "session-events: session lifecycle"
      (define sse
        (make-session-start-event #:session-id "s1" #:turn-id "t1" #:timestamp 1000 #:model "gpt-4"))
      (check-true (session-start-event? sse))
      (check-true (typed-event? sse))
      (check-equal? (session-start-event-model sse) "gpt-4")

      (define ie
        (make-input-event #:session-id "s1"
                          #:turn-id "t1"
                          #:timestamp 1000
                          #:input-type "text"
                          #:content "hello"))
      (check-true (input-event? ie))
      (check-equal? (input-event-content ie) "hello")

      (define mse
        (make-model-select-event #:session-id "s1"
                                 #:turn-id "t1"
                                 #:timestamp 1000
                                 #:model "gpt-4"
                                 #:provider "openai"))
      (check-true (model-select-event? mse))

      (define ase
        (make-agent-start-event #:session-id "s1" #:turn-id "t1" #:timestamp 1000 #:model "gpt-4"))
      (check-true (agent-start-event? ase))
      (check-true (typed-event? ase))

      (define ce
        (make-context-event #:session-id "s1"
                            #:turn-id "t1"
                            #:timestamp 1000
                            #:token-count 1000
                            #:window-size 4000))
      (check-true (context-event? ce))
      (check-equal? (context-event-token-count ce) 1000))

    (test-case "façade re-exports all identifiers"
      ;; Verify that the façade provides everything needed
      (check-true (procedure? typed-event?))
      (check-true (procedure? turn-start-event?))
      (check-true (procedure? message-start-event?))
      (check-true (procedure? tool-call-event?))
      (check-true (procedure? provider-request-event?))
      (check-true (procedure? session-start-event?))
      (check-true (procedure? input-event?))
      (check-true (procedure? model-select-event?))
      (check-true (procedure? agent-start-event?))
      (check-true (procedure? context-event?)))))

(module+ main
  (run-tests es-tests))
