#lang racket

(require rackunit
         racket/date
         "../agent/types.rkt")

;; ============================================================
;; Test suite: agent/types.rkt — canonical runtime structs
;; ============================================================

;; ------------------------------------------------------------
;; 1. Content-part constructors and predicates
;; ------------------------------------------------------------

(define text-part-1 (make-text-part "Hello, world!"))
(define tc-part-1 (make-tool-call-part "tc-1" "read" '#hash((path . "foo.rkt"))))
(define tr-part-1 (make-tool-result-part "tc-1" '("file contents here") #f))

(check-true (text-part? text-part-1) "text-part predicate")
(check-true (tool-call-part? tc-part-1) "tool-call-part predicate")
(check-true (tool-result-part? tr-part-1) "tool-result-part predicate")

(check-equal? (content-part-type text-part-1) "text")
(check-equal? (text-part-text text-part-1) "Hello, world!")

(check-equal? (content-part-type tc-part-1) "tool-call")
(check-equal? (tool-call-part-id tc-part-1) "tc-1")
(check-equal? (tool-call-part-name tc-part-1) "read")
(check-equal? (hash-ref (tool-call-part-arguments tc-part-1) 'path) "foo.rkt")

(check-equal? (content-part-type tr-part-1) "tool-result")
(check-equal? (tool-result-part-tool-call-id tr-part-1) "tc-1")
(check-equal? (tool-result-part-content tr-part-1) '("file contents here"))
(check-false (tool-result-part-is-error? tr-part-1))

;; error tool-result-part
(define tr-part-err (make-tool-result-part "tc-2" '("command failed") #t))
(check-true (tool-result-part-is-error? tr-part-err))

;; ------------------------------------------------------------
;; 2. Message struct
;; ------------------------------------------------------------

(define msg-1
  (make-message "msg-1" #f 'user 'message
                (list text-part-1)
                1775500000
                '#hash()))

(check-true (message? msg-1) "message predicate")
(check-equal? (message-id msg-1) "msg-1")
(check-false (message-parent-id msg-1))
(check-equal? (message-role msg-1) 'user)
(check-equal? (message-kind msg-1) 'message)
(check-equal? (message-content msg-1) (list text-part-1))
(check-equal? (message-timestamp msg-1) 1775500000)
(check-equal? (message-meta msg-1) '#hash())

;; Assistant message with tool call
(define msg-2
  (make-message "msg-2" "msg-1" 'assistant 'message
                (list (make-text-part "I will read the file.")
                      tc-part-1)
                1775500100
                '#hash((model . "openai-compatible/default")
                       (turnId . "turn-1"))))

(check-equal? (message-parent-id msg-2) "msg-1")
(check-equal? (message-role msg-2) 'assistant)
(check-equal? (length (message-content msg-2)) 2)

;; Tool-result message
(define msg-3
  (make-message "msg-3" "msg-2" 'tool 'tool-result
                (list tr-part-1)
                1775500200
                '#hash((turnId . "turn-1"))))

(check-equal? (message-kind msg-3) 'tool-result)
(check-equal? (message-role msg-3) 'tool)

;; Compaction-summary message
(define msg-compact
  (make-message "msg-c1" "msg-2" 'system 'compaction-summary
                (list (make-text-part "Summary of prior turns..."))
                1775500500
                '#hash((strategy . "summary-plus-recency"))))

(check-equal? (message-kind msg-compact) 'compaction-summary)

;; ------------------------------------------------------------
;; 3. Message JSON roundtrip
;; ------------------------------------------------------------

(define (roundtrip-message msg)
  (jsexpr->message (message->jsexpr msg)))

;; Simple text message
(define rt-msg-1 (roundtrip-message msg-1))
(check-equal? (message-id rt-msg-1) (message-id msg-1))
(check-equal? (message-parent-id rt-msg-1) (message-parent-id msg-1))
(check-equal? (message-role rt-msg-1) (message-role msg-1))
(check-equal? (message-kind rt-msg-1) (message-kind msg-1))
(check-equal? (message-timestamp rt-msg-1) (message-timestamp msg-1))
(check-equal? (message-meta rt-msg-1) (message-meta msg-1))
(check-equal? (length (message-content rt-msg-1)) 1)
(check-equal? (text-part-text (car (message-content rt-msg-1)))
              "Hello, world!")

;; Message with tool-call part
(define rt-msg-2 (roundtrip-message msg-2))
(check-equal? (length (message-content rt-msg-2)) 2)
(define rt-tc (cadr (message-content rt-msg-2)))
(check-true (tool-call-part? rt-tc))
(check-equal? (tool-call-part-name rt-tc) "read")

;; Message with tool-result part
(define rt-msg-3 (roundtrip-message msg-3))
(check-equal? (length (message-content rt-msg-3)) 1)
(define rt-tr (car (message-content rt-msg-3)))
(check-true (tool-result-part? rt-tr))
(check-equal? (tool-result-part-tool-call-id rt-tr) "tc-1")
(check-false (tool-result-part-is-error? rt-tr))

;; Message with #f parent-id stays #f after roundtrip
(check-false (message-parent-id rt-msg-1))

;; ------------------------------------------------------------
;; 4. message->jsexpr output structure
;; ------------------------------------------------------------

(define j1 (message->jsexpr msg-1))
(check-equal? (hash-ref j1 'id) "msg-1")
(check-equal? (hash-ref j1 'parentId) #f)  ; null in JSON
(check-equal? (hash-ref j1 'role) "user")
(check-equal? (hash-ref j1 'kind) "message")
(check-equal? (hash-ref j1 'timestamp) 1775500000)
(check-true (hash? (hash-ref j1 'meta)))
(check-true (list? (hash-ref j1 'content)))
(check-equal? (hash-ref (car (hash-ref j1 'content)) 'type) "text")

;; Verify tool-call serialization
(define j2 (message->jsexpr msg-2))
(define tc-in-j2 (cadr (hash-ref j2 'content)))
(check-equal? (hash-ref tc-in-j2 'type) "tool-call")
(check-equal? (hash-ref tc-in-j2 'id) "tc-1")
(check-equal? (hash-ref tc-in-j2 'name) "read")

;; ------------------------------------------------------------
;; 5. Event envelope struct
;; ------------------------------------------------------------

(define evt-1
  (make-event "turn.started" 1775500001 "sess-1" "turn-1"
              '#hash((model . "default"))))

(check-true (event? evt-1) "event predicate")
(check-equal? (event-version evt-1) 1)
(check-equal? (event-event evt-1) "turn.started")
(check-equal? (event-time evt-1) 1775500001)
(check-equal? (event-session-id evt-1) "sess-1")
(check-equal? (event-turn-id evt-1) "turn-1")
(check-equal? (hash-ref (event-payload evt-1) 'model) "default")

;; Event without turn-id
(define evt-2
  (make-event "session.started" 1775500000 "sess-1" #f '#hash()))

(check-false (event-turn-id evt-2))

;; Event with explicit version
(define evt-3
  (make-event "runtime.error" 1775500999 "sess-1" #f
              '#hash((message . "oops"))
              #:version 2))
(check-equal? (event-version evt-3) 2)

;; ------------------------------------------------------------
;; 6. Event JSON roundtrip
;; ------------------------------------------------------------

(define (roundtrip-event evt)
  (jsexpr->event (event->jsexpr evt)))

(define rt-evt-1 (roundtrip-event evt-1))
(check-equal? (event-version rt-evt-1) (event-version evt-1))
(check-equal? (event-event rt-evt-1) (event-event evt-1))
(check-equal? (event-time rt-evt-1) (event-time evt-1))
(check-equal? (event-session-id rt-evt-1) (event-session-id evt-1))
(check-equal? (event-turn-id rt-evt-1) (event-turn-id evt-1))
(check-equal? (hash-ref (event-payload rt-evt-1) 'model) "default")

;; Roundtrip with #f turnId
(define rt-evt-2 (roundtrip-event evt-2))
(check-false (event-turn-id rt-evt-2))

;; ------------------------------------------------------------
;; 7. event->jsexpr output structure
;; ------------------------------------------------------------

(define je1 (event->jsexpr evt-1))
(check-equal? (hash-ref je1 'version) 1)
(check-equal? (hash-ref je1 'event) "turn.started")
(check-equal? (hash-ref je1 'sessionId) "sess-1")
(check-equal? (hash-ref je1 'turnId) "turn-1")
(check-true (hash? (hash-ref je1 'payload)))

;; turnId #f -> JSON null
(define je2 (event->jsexpr evt-2))
(check-false (hash-ref je2 'turnId))

;; ------------------------------------------------------------
;; 8. Tool-call struct (standalone)
;; ------------------------------------------------------------

(define tc-1 (make-tool-call "tc-42" "bash" '#hash((command . "ls -la"))))
(check-true (tool-call? tc-1))
(check-equal? (tool-call-id tc-1) "tc-42")
(check-equal? (tool-call-name tc-1) "bash")
(check-equal? (hash-ref (tool-call-arguments tc-1) 'command) "ls -la")

;; ------------------------------------------------------------
;; 9. Tool-result struct (standalone)
;; ------------------------------------------------------------

(define tr-1 (make-tool-result '("line 1" "line 2") '#hash((exitCode . 0)) #f))
(check-true (tool-result? tr-1))
(check-equal? (tool-result-content tr-1) '("line 1" "line 2"))
(check-equal? (hash-ref (tool-result-details tr-1) 'exitCode) 0)
(check-false (tool-result-is-error? tr-1))

;; Error result
(define tr-err (make-tool-result '("command not found") '#hash((exitCode . 127)) #t))
(check-true (tool-result-is-error? tr-err))

;; ------------------------------------------------------------
;; 10. Loop-result struct
;; ------------------------------------------------------------

(define lr-1 (make-loop-result (list msg-1 msg-2 msg-3)
                               'turn-completed
                               '#hash((turnId . "turn-1")
                                      (tokensUsed . 1500))))
(check-true (loop-result? lr-1))
(check-equal? (length (loop-result-messages lr-1)) 3)
(check-equal? (loop-result-termination-reason lr-1) 'turn-completed)
(check-equal? (hash-ref (loop-result-metadata lr-1) 'turnId) "turn-1")

;; ------------------------------------------------------------
;; 11. Edge cases
;; ------------------------------------------------------------

;; Empty content list
(define msg-empty (make-message "m-e" #f 'system 'message '() 0 '#hash()))
(check-equal? (message-content (roundtrip-message msg-empty)) '())

;; Empty payload event
(define evt-empty (make-event "session.started" 0 "s" #f '#hash()))
(check-equal? (hash-keys (event-payload (roundtrip-event evt-empty))) '())

;; Bookmark message kind
(define msg-bm (make-message "bm-1" "msg-1" 'user 'bookmark
                             (list (make-text-part "checkpoint"))
                             1775501000 '#hash()))
(check-equal? (message-kind (roundtrip-message msg-bm)) 'bookmark)

;; All roles and kinds survive roundtrip
(for ([role '(system user assistant tool extension)])
  (for ([kind '(message tool-result compaction-summary bookmark metadata)])
    (define m (make-message "x" #f role kind '() 0 '#hash()))
    (check-equal? (message-role (roundtrip-message m)) role
                  (format "role ~a roundtrip" role))
    (check-equal? (message-kind (roundtrip-message m)) kind
                  (format "kind ~a roundtrip" kind))))

;; ------------------------------------------------------------
;; 12. Content-part serialization helpers
;; ------------------------------------------------------------

;; Verify jsexpr->content-part dispatches correctly on type
(check-true (text-part? (jsexpr->content-part '#hash((type . "text") (text . "hi")))))
(check-true (tool-call-part? (jsexpr->content-part '#hash((type . "tool-call")
                                                      (id . "tc")
                                                      (name . "bash")
                                                      (arguments . #hash())))))
(check-true (tool-result-part? (jsexpr->content-part '#hash((type . "tool-result")
                                                            (toolCallId . "tc")
                                                            (content . ("ok"))
                                                            (isError . #f)))))

;; Verify content-part->jsexpr produces correct structure
(check-equal? (hash-ref (content-part->jsexpr text-part-1) 'type) "text")
(check-equal? (hash-ref (content-part->jsexpr tc-part-1) 'type) "tool-call")
(check-equal? (hash-ref (content-part->jsexpr tr-part-1) 'type) "tool-result")

(println "All tests passed!")
