#lang racket/base

;; tests/test-event-types.rkt — TDD tests for typed event structs
;; Wave 0: Issues #1308 + #1311
;;
;; Tests typed event structs for all hook point categories:
;;   - Turn lifecycle (start/end)
;;   - Message lifecycle (start/update/end)
;;   - Tool execution lifecycle (start/update/end)
;;   - Tool call/result (generic + per-tool typed)
;;   - Provider request/response
;;   - Session lifecycle (start/shutdown)
;;   - Input events
;;   - Model selection
;;   - Agent lifecycle (start/end)
;;   - Context events
;;
;; All events must:
;;   - Construct with typed fields
;;   - Roundtrip through JSON (->jsexpr / jsexpr->)
;;   - Have correct type predicates

(require rackunit
         racket/match
         "../agent/event-types.rkt")

;; ============================================================
;; Helper: test JSON roundtrip
;; ============================================================

(define (check-roundtrip evt description)
  (define js (typed-event->jsexpr evt))
  (check-not-exn (lambda () (typed-event->jsexpr evt))
                 (format "~a: serialization succeeds" description))
  (define recovered (jsexpr->typed-event js))
  (check-not-exn (lambda () (jsexpr->typed-event js))
                 (format "~a: deserialization succeeds" description))
  (check-equal? (typed-event-type recovered)
                (typed-event-type evt)
                (format "~a: type preserved" description))
  ;; Verify key fields match
  (check-equal? (typed-event-session-id recovered)
                (typed-event-session-id evt)
                (format "~a: session-id preserved" description))
  (check-equal? (typed-event-turn-id recovered)
                (typed-event-turn-id evt)
                (format "~a: turn-id preserved" description))
  recovered)

;; ============================================================
;; Turn lifecycle events
;; ============================================================

(test-case "turn-start-event: construction and predicates"
  (define evt
    (make-turn-start-event #:session-id "s1"
                           #:turn-id "t1"
                           #:timestamp 1000
                           #:model "gpt-4"
                           #:provider "openai"))
  (check-equal? (typed-event-type evt) "turn-start")
  (check-equal? (turn-start-event-model evt) "gpt-4")
  (check-equal? (turn-start-event-provider evt) "openai")
  (check-true (turn-start-event? evt))
  (check-true (typed-event? evt)))

(test-case "turn-start-event: JSON roundtrip"
  (define evt
    (make-turn-start-event #:session-id "s1"
                           #:turn-id "t1"
                           #:timestamp 1000
                           #:model "gpt-4"
                           #:provider "openai"))
  (check-roundtrip evt "turn-start-event"))

(test-case "turn-end-event: construction and predicates"
  (define evt
    (make-turn-end-event #:session-id "s1"
                         #:turn-id "t1"
                         #:timestamp 2000
                         #:reason "completed"
                         #:duration-ms 1000))
  (check-equal? (typed-event-type evt) "turn-end")
  (check-equal? (turn-end-event-reason evt) "completed")
  (check-equal? (turn-end-event-duration-ms evt) 1000)
  (check-true (turn-end-event? evt)))

(test-case "turn-end-event: JSON roundtrip"
  (define evt
    (make-turn-end-event #:session-id "s1"
                         #:turn-id "t1"
                         #:timestamp 2000
                         #:reason "error"
                         #:duration-ms 500))
  (check-roundtrip evt "turn-end-event"))

;; ============================================================
;; Message lifecycle events
;; ============================================================

(test-case "message-start-event: construction and predicates"
  (define evt
    (make-message-start-event #:session-id "s1"
                              #:turn-id "t1"
                              #:timestamp 1000
                              #:role "assistant"
                              #:model "gpt-4"))
  (check-equal? (typed-event-type evt) "message-start")
  (check-equal? (message-start-event-role evt) "assistant")
  (check-true (message-start-event? evt)))

(test-case "message-start-event: JSON roundtrip"
  (define evt
    (make-message-start-event #:session-id "s1"
                              #:turn-id "t1"
                              #:timestamp 1000
                              #:role "user"
                              #:model "gpt-4"))
  (check-roundtrip evt "message-start-event"))

(test-case "message-update-event: construction"
  (define evt
    (make-message-update-event #:session-id "s1"
                               #:turn-id "t1"
                               #:timestamp 1000
                               #:content "hello"
                               #:delta "lo"))
  (check-equal? (typed-event-type evt) "message-update")
  (check-equal? (message-update-event-delta evt) "lo"))

(test-case "message-update-event: JSON roundtrip"
  (define evt
    (make-message-update-event #:session-id "s1"
                               #:turn-id "t1"
                               #:timestamp 1000
                               #:content "hello"
                               #:delta "lo"))
  (check-roundtrip evt "message-update-event"))

(test-case "message-end-event: construction"
  (define evt
    (make-message-end-event #:session-id "s1"
                            #:turn-id "t1"
                            #:timestamp 2000
                            #:role "assistant"
                            #:content-length 42))
  (check-equal? (typed-event-type evt) "message-end")
  (check-equal? (message-end-event-content-length evt) 42))

(test-case "message-end-event: JSON roundtrip"
  (define evt
    (make-message-end-event #:session-id "s1"
                            #:turn-id "t1"
                            #:timestamp 2000
                            #:role "assistant"
                            #:content-length 42))
  (check-roundtrip evt "message-end-event"))

;; ============================================================
;; Tool execution lifecycle events
;; ============================================================

(test-case "tool-execution-start-event: construction"
  (define evt
    (make-tool-execution-start-event #:session-id "s1"
                                     #:turn-id "t1"
                                     #:timestamp 1000
                                     #:tool-name "bash"
                                     #:tool-call-id "tc-1"))
  (check-equal? (typed-event-type evt) "tool-execution-start")
  (check-equal? (tool-execution-start-event-tool-name evt) "bash")
  (check-equal? (tool-execution-start-event-tool-call-id evt) "tc-1"))

(test-case "tool-execution-start-event: JSON roundtrip"
  (define evt
    (make-tool-execution-start-event #:session-id "s1"
                                     #:turn-id "t1"
                                     #:timestamp 1000
                                     #:tool-name "bash"
                                     #:tool-call-id "tc-1"))
  (check-roundtrip evt "tool-execution-start-event"))

(test-case "tool-execution-update-event: construction"
  (define evt
    (make-tool-execution-update-event #:session-id "s1"
                                      #:turn-id "t1"
                                      #:timestamp 1100
                                      #:tool-name "bash"
                                      #:progress "running..."))
  (check-equal? (typed-event-type evt) "tool-execution-update"))

(test-case "tool-execution-end-event: construction"
  (define evt
    (make-tool-execution-end-event #:session-id "s1"
                                   #:turn-id "t1"
                                   #:timestamp 2000
                                   #:tool-name "bash"
                                   #:duration-ms 1000
                                   #:result-summary "exit 0"))
  (check-equal? (typed-event-type evt) "tool-execution-end"))

;; ============================================================
;; Tool call/result events (generic)
;; ============================================================

(test-case "tool-call-event: construction"
  (define evt
    (make-tool-call-event #:session-id "s1"
                          #:turn-id "t1"
                          #:timestamp 1000
                          #:tool-name "bash"
                          #:arguments '#hasheq((command . "ls"))
                          #:tool-call-id "tc-1"))
  (check-equal? (typed-event-type evt) "tool-call")
  (check-equal? (tool-call-event-tool-name evt) "bash")
  (check-true (tool-call-event? evt)))

(test-case "tool-call-event: JSON roundtrip"
  (define evt
    (make-tool-call-event #:session-id "s1"
                          #:turn-id "t1"
                          #:timestamp 1000
                          #:tool-name "bash"
                          #:arguments '#hasheq((command . "ls"))
                          #:tool-call-id "tc-1"))
  (check-roundtrip evt "tool-call-event"))

(test-case "tool-result-event: construction"
  (define evt
    (make-tool-result-event #:session-id "s1"
                            #:turn-id "t1"
                            #:timestamp 2000
                            #:tool-call-id "tc-1"
                            #:content "file1.txt\nfile2.txt"
                            #:is-error? #f))
  (check-equal? (typed-event-type evt) "tool-result")
  (check-equal? (tool-result-event-tool-call-id evt) "tc-1")
  (check-false (tool-result-event-is-error? evt)))

(test-case "tool-result-event: JSON roundtrip"
  (define evt
    (make-tool-result-event #:session-id "s1"
                            #:turn-id "t1"
                            #:timestamp 2000
                            #:tool-call-id "tc-1"
                            #:content "output"
                            #:is-error? #t))
  (check-roundtrip evt "tool-result-event"))

;; ============================================================
;; Per-tool typed events (Issue #1311)
;; ============================================================

(test-case "bash-tool-call-event: inherits tool-call-event"
  (define evt
    (make-bash-tool-call-event #:session-id "s1"
                               #:turn-id "t1"
                               #:timestamp 1000
                               #:command "ls -la"
                               #:timeout 30
                               #:cwd "/tmp"
                               #:tool-call-id "tc-bash"))
  (check-equal? (typed-event-type evt) "bash-tool-call")
  (check-equal? (bash-tool-call-event-command evt) "ls -la")
  (check-equal? (bash-tool-call-event-timeout evt) 30)
  (check-equal? (bash-tool-call-event-cwd evt) "/tmp")
  (check-true (bash-tool-call-event? evt))
  ;; Per-tool events are also tool-call-events
  (check-true (tool-call-event? evt))
  (check-equal? (tool-call-event-tool-name evt) "bash"))

(test-case "bash-tool-call-event: JSON roundtrip"
  (define evt
    (make-bash-tool-call-event #:session-id "s1"
                               #:turn-id "t1"
                               #:timestamp 1000
                               #:command "ls"
                               #:timeout 30
                               #:cwd "/tmp"
                               #:tool-call-id "tc-1"))
  (check-roundtrip evt "bash-tool-call-event"))

(test-case "edit-tool-call-event: construction and inheritance"
  (define evt
    (make-edit-tool-call-event #:session-id "s1"
                               #:turn-id "t1"
                               #:timestamp 1000
                               #:path "test.rkt"
                               #:edits '(((oldText . "foo") (newText . "bar")))
                               #:tool-call-id "tc-edit"))
  (check-equal? (typed-event-type evt) "edit-tool-call")
  (check-equal? (edit-tool-call-event-path evt) "test.rkt")
  (check-true (tool-call-event? evt))
  (check-equal? (tool-call-event-tool-name evt) "edit"))

(test-case "edit-tool-call-event: JSON roundtrip"
  (define evt
    (make-edit-tool-call-event #:session-id "s1"
                               #:turn-id "t1"
                               #:timestamp 1000
                               #:path "test.rkt"
                               #:edits '(((oldText . "foo") (newText . "bar")))
                               #:tool-call-id "tc-edit"))
  (check-roundtrip evt "edit-tool-call-event"))

(test-case "write-tool-call-event: construction"
  (define evt
    (make-write-tool-call-event #:session-id "s1"
                                #:turn-id "t1"
                                #:timestamp 1000
                                #:path "out.rkt"
                                #:content "(#lang racket)"
                                #:tool-call-id "tc-write"))
  (check-equal? (typed-event-type evt) "write-tool-call")
  (check-equal? (write-tool-call-event-path evt) "out.rkt"))

(test-case "write-tool-call-event: JSON roundtrip"
  (define evt
    (make-write-tool-call-event #:session-id "s1"
                                #:turn-id "t1"
                                #:timestamp 1000
                                #:path "out.rkt"
                                #:content "(#lang racket)"
                                #:tool-call-id "tc-write"))
  (check-roundtrip evt "write-tool-call-event"))

(test-case "read-tool-call-event: construction"
  (define evt
    (make-read-tool-call-event #:session-id "s1"
                               #:turn-id "t1"
                               #:timestamp 1000
                               #:path "test.rkt"
                               #:offset 1
                               #:limit 50
                               #:tool-call-id "tc-read"))
  (check-equal? (typed-event-type evt) "read-tool-call")
  (check-equal? (read-tool-call-event-offset evt) 1)
  (check-equal? (read-tool-call-event-limit evt) 50)
  (check-true (tool-call-event? evt)))

(test-case "read-tool-call-event: JSON roundtrip"
  (define evt
    (make-read-tool-call-event #:session-id "s1"
                               #:turn-id "t1"
                               #:timestamp 1000
                               #:path "test.rkt"
                               #:offset 1
                               #:limit 50
                               #:tool-call-id "tc-read"))
  (check-roundtrip evt "read-tool-call-event"))

(test-case "grep-tool-call-event: construction"
  (define evt
    (make-grep-tool-call-event #:session-id "s1"
                               #:turn-id "t1"
                               #:timestamp 1000
                               #:pattern "TODO"
                               #:path "src/"
                               #:glob "*.rkt"
                               #:tool-call-id "tc-grep"))
  (check-equal? (typed-event-type evt) "grep-tool-call")
  (check-equal? (grep-tool-call-event-pattern evt) "TODO"))

(test-case "grep-tool-call-event: JSON roundtrip"
  (define evt
    (make-grep-tool-call-event #:session-id "s1"
                               #:turn-id "t1"
                               #:timestamp 1000
                               #:pattern "TODO"
                               #:path "src/"
                               #:glob "*.rkt"
                               #:tool-call-id "tc-grep"))
  (check-roundtrip evt "grep-tool-call-event"))

(test-case "find-tool-call-event: construction"
  (define evt
    (make-find-tool-call-event #:session-id "s1"
                               #:turn-id "t1"
                               #:timestamp 1000
                               #:pattern "test"
                               #:path "src/"
                               #:tool-call-id "tc-find"))
  (check-equal? (typed-event-type evt) "find-tool-call"))

(test-case "find-tool-call-event: JSON roundtrip"
  (define evt
    (make-find-tool-call-event #:session-id "s1"
                               #:turn-id "t1"
                               #:timestamp 1000
                               #:pattern "test"
                               #:path "src/"
                               #:tool-call-id "tc-find"))
  (check-roundtrip evt "find-tool-call-event"))

(test-case "custom-tool-call-event: fallback for unknown tools"
  (define evt
    (make-custom-tool-call-event #:session-id "s1"
                                 #:turn-id "t1"
                                 #:timestamp 1000
                                 #:tool-name "my-custom-tool"
                                 #:arguments '#hasheq((x . 1))
                                 #:tool-call-id "tc-custom"))
  (check-equal? (typed-event-type evt) "custom-tool-call")
  (check-equal? (tool-call-event-tool-name evt) "my-custom-tool")
  (check-true (tool-call-event? evt)))

(test-case "custom-tool-call-event: JSON roundtrip"
  (define evt
    (make-custom-tool-call-event #:session-id "s1"
                                 #:turn-id "t1"
                                 #:timestamp 1000
                                 #:tool-name "my-custom-tool"
                                 #:arguments '#hasheq((x . 1))
                                 #:tool-call-id "tc-custom"))
  (check-roundtrip evt "custom-tool-call-event"))

;; ============================================================
;; Provider events
;; ============================================================

(test-case "provider-request-event: construction and roundtrip"
  (define evt
    (make-provider-request-event #:session-id "s1"
                                 #:turn-id "t1"
                                 #:timestamp 1000
                                 #:model "gpt-4"
                                 #:provider "openai"))
  (check-equal? (typed-event-type evt) "provider-request")
  (check-equal? (provider-request-event-model evt) "gpt-4")
  (check-roundtrip evt "provider-request-event"))

(test-case "provider-response-event: construction and roundtrip"
  (define evt
    (make-provider-response-event #:session-id "s1"
                                  #:turn-id "t1"
                                  #:timestamp 2000
                                  #:model "gpt-4"
                                  #:provider "openai"
                                  #:latency-ms 1500))
  (check-equal? (typed-event-type evt) "provider-response")
  (check-equal? (provider-response-event-latency-ms evt) 1500)
  (check-roundtrip evt "provider-response-event"))

;; ============================================================
;; Session events
;; ============================================================

(test-case "session-start-event: construction and roundtrip"
  (define evt
    (make-session-start-event #:session-id "s1" #:turn-id #f #:timestamp 1000 #:model "gpt-4"))
  (check-equal? (typed-event-type evt) "session-start")
  (check-roundtrip evt "session-start-event"))

(test-case "session-shutdown-event: construction and roundtrip"
  (define evt
    (make-session-shutdown-event #:session-id "s1"
                                 #:turn-id #f
                                 #:timestamp 5000
                                 #:reason "user-exit"))
  (check-equal? (typed-event-type evt) "session-shutdown")
  (check-equal? (session-shutdown-event-reason evt) "user-exit")
  (check-roundtrip evt "session-shutdown-event"))

;; ============================================================
;; Input events
;; ============================================================

(test-case "input-event: construction and roundtrip"
  (define evt
    (make-input-event #:session-id "s1"
                      #:turn-id "t1"
                      #:timestamp 1000
                      #:input-type "text"
                      #:content "hello world"))
  (check-equal? (typed-event-type evt) "input")
  (check-equal? (input-event-input-type evt) "text")
  (check-roundtrip evt "input-event"))

;; ============================================================
;; Model events
;; ============================================================

(test-case "model-select-event: construction and roundtrip"
  (define evt
    (make-model-select-event #:session-id "s1"
                             #:turn-id "t1"
                             #:timestamp 1000
                             #:model "gpt-4"
                             #:provider "openai"))
  (check-equal? (typed-event-type evt) "model-select")
  (check-roundtrip evt "model-select-event"))

;; ============================================================
;; Agent events
;; ============================================================

(test-case "agent-start-event: construction and roundtrip"
  (define evt
    (make-agent-start-event #:session-id "s1" #:turn-id "t1" #:timestamp 1000 #:model "gpt-4"))
  (check-equal? (typed-event-type evt) "agent-start")
  (check-roundtrip evt "agent-start-event"))

(test-case "agent-end-event: construction and roundtrip"
  (define evt
    (make-agent-end-event #:session-id "s1"
                          #:turn-id "t1"
                          #:timestamp 5000
                          #:reason "completed"
                          #:duration-ms 4000))
  (check-equal? (typed-event-type evt) "agent-end")
  (check-equal? (agent-end-event-reason evt) "completed")
  (check-roundtrip evt "agent-end-event"))

;; ============================================================
;; Context events
;; ============================================================

(test-case "context-event: construction and roundtrip"
  (define evt
    (make-context-event #:session-id "s1"
                        #:turn-id "t1"
                        #:timestamp 1000
                        #:token-count 1000
                        #:window-size 4096))
  (check-equal? (typed-event-type evt) "context")
  (check-equal? (context-event-token-count evt) 1000)
  (check-roundtrip evt "context-event"))

;; ============================================================
;; Event type registry
;; ============================================================

(test-case "all-known-event-types returns comprehensive list"
  (define types (all-known-event-types))
  (check-not-false (member "turn-start" types))
  (check-not-false (member "turn-end" types))
  (check-not-false (member "message-start" types))
  (check-not-false (member "tool-call" types))
  (check-not-false (member "tool-result" types))
  (check-not-false (member "bash-tool-call" types))
  (check-not-false (member "edit-tool-call" types))
  (check-not-false (member "session-start" types))
  (check-not-false (member "session-shutdown" types))
  (check-not-false (member "agent-start" types))
  (check-not-false (member "agent-end" types))
  (check-not-false (member "input" types))
  (check-not-false (member "model-select" types))
  (check-not-false (member "context" types))
  (check-not-false (member "provider-request" types))
  (check-not-false (member "provider-response" types))
  ;; At least 20 types
  (check-true (>= (length types) 20)))

(test-case "event-name->tool-name maps correctly"
  (check-equal? (event-name->tool-name "bash-tool-call") "bash")
  (check-equal? (event-name->tool-name "edit-tool-call") "edit")
  (check-equal? (event-name->tool-name "write-tool-call") "write")
  (check-equal? (event-name->tool-name "read-tool-call") "read")
  (check-equal? (event-name->tool-name "grep-tool-call") "grep")
  (check-equal? (event-name->tool-name "find-tool-call") "find")
  (check-equal? (event-name->tool-name "custom-tool-call") #f)
  (check-equal? (event-name->tool-name "tool-call") #f))

;; ============================================================
;; Deserialization: unknown type handling
;; ============================================================

(test-case "jsexpr->typed-event: unknown type returns generic typed-event"
  (define js (hasheq 'type "unknown-event-type" 'timestamp 1000 'session-id "s1" 'turn-id "t1"))
  (define evt (jsexpr->typed-event js))
  (check-equal? (typed-event-type evt) "unknown-event-type")
  (check-true (typed-event? evt))
  ;; Unknown types don't match specific predicates
  (check-false (turn-start-event? evt)))
