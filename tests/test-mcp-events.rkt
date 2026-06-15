#lang racket

;; @speed fast  ;; @suite extensions

;; tests/test-mcp-events.rkt — W5 (v0.99.9) MCP Events + Blackboard Integration Tests
;;
;; Tests for:
;;   - mas-mcp-connected-event and mas-mcp-tool-called-event construction
;;   - Reducer handlers updating agent-activities
;;   - Subscriber recognizing the new MCP event types

(require rackunit
         rackunit/text-ui
         "../agent/event-structs/mas-events.rkt"
         (only-in "../agent/event-structs/base.rkt"
                  typed-event?
                  typed-event-type
                  typed-event-session-id)
         "../agent/blackboard.rkt"
         "../agent/blackboard-reducer.rkt"
         (only-in "../agent/blackboard-subscriber.rkt"
                  blackboard-relevant-event?
                  relevant-event-names))

;; Helper: create a GSD-wrapped event hash (matches emit-gsd-event! format)
(define (make-gsd-event name data)
  (hasheq 'event name 'data data 'timestamp 1000 'correlation-id #f))

(define suite
  (test-suite "MCP Events + Blackboard (v0.99.9 W5)"

    ;; ── mas-mcp-connected-event ──

    (test-case "mas-mcp-connected-event constructs with required fields"
      (define evt
        (make-mas-mcp-connected-event #:session-id "s1" #:turn-id "t1" #:server-name "mcp-server-1"))
      (check-pred typed-event? evt)
      (check-equal? (typed-event-type evt) "mas.mcp.connected")
      (check-equal? (typed-event-session-id evt) "s1"))

    (test-case "mas-mcp-connected-event stores server-name in field"
      (define evt
        (make-mas-mcp-connected-event #:session-id "s"
                                      #:turn-id "t"
                                      #:server-name "filesystem-server"))
      (check-equal? (mas-mcp-connected-event-server-name evt) "filesystem-server"))

    ;; ── mas-mcp-tool-called-event ──

    (test-case "mas-mcp-tool-called-event constructs with required fields"
      (define evt
        (make-mas-mcp-tool-called-event #:session-id "s1" #:turn-id "t1" #:tool-name "read_file"))
      (check-pred typed-event? evt)
      (check-equal? (typed-event-type evt) "mas.mcp.tool.called"))

    (test-case "mas-mcp-tool-called-event with optional server-name"
      (define evt
        (make-mas-mcp-tool-called-event #:session-id "s"
                                        #:turn-id "t"
                                        #:tool-name "read_file"
                                        #:server-name "filesystem-server"))
      (check-equal? (mas-mcp-tool-called-event-tool-name evt) "read_file")
      (check-equal? (mas-mcp-tool-called-event-server-name evt) "filesystem-server"))

    (test-case "mas-mcp-tool-called-event without server-name defaults to #f"
      (define evt
        (make-mas-mcp-tool-called-event #:session-id "s" #:turn-id "t" #:tool-name "list_dirs"))
      (check-false (mas-mcp-tool-called-event-server-name evt)))

    ;; ── Reducer: mas.mcp.connected ──

    (test-case "mas.mcp.connected → agent-activities appended"
      (define evt (make-gsd-event 'mas.mcp.connected (hasheq 'server-name "mcp-1")))
      (define result (apply-event empty-blackboard evt))
      (define activities (blackboard-state-agent-activities result))
      (check-equal? (length activities) 1)
      (check-equal? (hash-ref (car activities) 'action) 'mcp-connected)
      (check-equal? (hash-ref (car activities) 'agent-name) "mcp-1"))

    ;; ── Reducer: mas.mcp.tool.called ──

    (test-case "mas.mcp.tool.called → agent-activities appended"
      (define evt
        (make-gsd-event 'mas.mcp.tool.called
                        (hasheq 'tool-name "read_file" 'server-name "fs-server")))
      (define result (apply-event empty-blackboard evt))
      (define activities (blackboard-state-agent-activities result))
      (check-equal? (length activities) 1)
      (define act (car activities))
      (check-equal? (hash-ref act 'action) 'mcp-tool-called)
      (check-equal? (hash-ref act 'tool-name) "read_file"))

    (test-case "mas.mcp.tool.called without server-name defaults agent-name to mcp"
      (define evt (make-gsd-event 'mas.mcp.tool.called (hasheq 'tool-name "list_dirs")))
      (define result (apply-event empty-blackboard evt))
      (define activities (blackboard-state-agent-activities result))
      (check-equal? (hash-ref (car activities) 'agent-name) "mcp"))

    ;; ── Subscriber: relevant events ──

    (test-case "subscriber recognizes mas.mcp.connected as relevant"
      (check-not-false (member 'mas.mcp.connected relevant-event-names)))

    (test-case "subscriber recognizes mas.mcp.tool.called as relevant"
      (check-not-false (member 'mas.mcp.tool.called relevant-event-names)))

    (test-case "blackboard-relevant-event? returns #t for mas.mcp.connected hash"
      (define evt (hasheq 'event 'mas.mcp.connected 'data (hasheq) 'timestamp 1000))
      (check-true (blackboard-relevant-event? evt)))

    (test-case "blackboard-relevant-event? returns #t for mas.mcp.tool.called hash"
      (define evt (hasheq 'event 'mas.mcp.tool.called 'data (hasheq) 'timestamp 1000))
      (check-true (blackboard-relevant-event? evt)))))

(run-tests suite)
