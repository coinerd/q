#lang racket

;; test-rpc-mode.rkt — tests for q/interfaces/rpc-mode.rkt
;;
;; Covers:
;;   - parse-rpc-request
;;   - rpc-response->json / rpc-notification->json
;;   - rpc-error helper
;;   - dispatch-rpc-request (routing, unknown method, handler errors)
;;   - run-rpc-loop (stdin/stdout JSONL loop with string ports)
;;   - event forwarding (event bus → notifications on output port)
;;   - ping/pong
;;   - shutdown
;;   - edge cases: empty lines, malformed JSON

(require rackunit
         racket/port
         racket/string
         json
         "../../q/agent/types.rkt"
         "../../q/agent/event-bus.rkt"
         "../../q/interfaces/rpc-mode.rkt")

;; ============================================================
;; 1. parse-rpc-request
;; ============================================================

(test-case "parse-rpc-request: valid request with all fields"
  (define json-str "{\"id\":\"req-1\",\"method\":\"prompt\",\"params\":{\"text\":\"hello\"}}")
  (define req (parse-rpc-request json-str))
  (check-not-false req)
  (check-equal? (rpc-request-id req) "req-1")
  (check-equal? (rpc-request-method req) 'prompt)
  (check-equal? (hash-ref (rpc-request-params req) 'text) "hello"))

(test-case "parse-rpc-request: method as symbol"
  (define json-str "{\"id\":\"r2\",\"method\":\"session.create\",\"params\":{}}")
  (define req (parse-rpc-request json-str))
  (check-equal? (rpc-request-method req) 'session.create))

(test-case "parse-rpc-request: missing id returns #f"
  (define json-str "{\"method\":\"ping\",\"params\":{}}")
  (define req (parse-rpc-request json-str))
  ;; Missing id should be rejected
  (check-false (and req (rpc-request-id req))))

(test-case "parse-rpc-request: missing method returns #f"
  (define json-str "{\"id\":\"r3\",\"params\":{}}")
  (check-false (parse-rpc-request json-str)))

(test-case "parse-rpc-request: missing params defaults to empty hash"
  (define json-str "{\"id\":\"r4\",\"method\":\"ping\"}")
  (define req (parse-rpc-request json-str))
  (check-not-false req)
  (check-equal? (rpc-request-params req) (make-immutable-hash)))

(test-case "parse-rpc-request: malformed JSON returns #f"
  (check-false (parse-rpc-request "{not json}"))
  (check-false (parse-rpc-request ""))
  (check-false (parse-rpc-request "   ")))

(test-case "parse-rpc-request: params not a hash defaults to empty"
  (define json-str "{\"id\":\"r5\",\"method\":\"ping\",\"params\":[1,2,3]}")
  (define req (parse-rpc-request json-str))
  ;; params not a hash should still parse (non-hash params become empty hash)
  (check-not-false req))

(test-case "parse-rpc-request: all supported method names"
  (for ([method-str '("session.create" "session.resume" "prompt"
                      "interrupt" "compact" "fork" "history"
                      "subscribe" "unsubscribe" "ping" "shutdown")])
    (define json-str (format "{\"id\":\"t\",\"method\":\"~a\",\"params\":{}}" method-str))
    (define req (parse-rpc-request json-str))
    (check-not-false req (format "method ~a should parse" method-str))
    (check-equal? (rpc-request-method req) (string->symbol method-str)
                  (format "method ~a should be symbol" method-str))))

;; ============================================================
;; 2. rpc-response->json
;; ============================================================

(test-case "rpc-response->json: success response"
  (define resp (rpc-response "req-1" (hasheq 'status "ok") #f))
  (define json-str (rpc-response->json resp))
  (define parsed (read-json (open-input-string json-str)))
  (check-equal? (hash-ref parsed 'id) "req-1")
  (check-equal? (hash-ref parsed 'result) (hasheq 'status "ok"))
  (check-false (hash-ref parsed 'error)))

(test-case "rpc-response->json: error response"
  (define err (hasheq 'code -32601 'message "Method not found"))
  (define resp (rpc-response "req-2" #f err))
  (define json-str (rpc-response->json resp))
  (define parsed (read-json (open-input-string json-str)))
  (check-equal? (hash-ref parsed 'id) "req-2")
  (check-false (hash-ref parsed 'result))
  (check-equal? (hash-ref (hash-ref parsed 'error) 'code) -32601)
  (check-equal? (hash-ref (hash-ref parsed 'error) 'message) "Method not found"))

(test-case "rpc-response->json: result is #t (boolean)"
  (define resp (rpc-response "req-3" #t #f))
  (define json-str (rpc-response->json resp))
  (define parsed (read-json (open-input-string json-str)))
  (check-equal? (hash-ref parsed 'result) #t))

;; ============================================================
;; 3. rpc-notification->json
;; ============================================================

(test-case "rpc-notification->json: basic notification"
  (define notif (rpc-notification 'model.stream.delta (hasheq 'text "hello")))
  (define json-str (rpc-notification->json notif))
  (define parsed (read-json (open-input-string json-str)))
  (check-equal? (hash-ref parsed 'jsonrpc) "2.0")
  (check-equal? (hash-ref parsed 'method) "model.stream.delta")
  (check-equal? (hash-ref (hash-ref parsed 'params) 'text) "hello"))

(test-case "rpc-notification->json: no id field present"
  (define notif (rpc-notification 'turn.completed (hasheq)))
  (define json-str (rpc-notification->json notif))
  (define parsed (read-json (open-input-string json-str)))
  (check-false (hash-has-key? parsed 'id)))

;; ============================================================
;; 4. rpc-error helper
;; ============================================================

(test-case "rpc-error: constructs error response"
  (define resp (rpc-error "e1" -32700 "Parse error"))
  (check-equal? (rpc-response-id resp) "e1")
  (check-false (rpc-response-result resp))
  (check-equal? (hash-ref (rpc-response-error resp) 'code) -32700)
  (check-equal? (hash-ref (rpc-response-error resp) 'message) "Parse error"))

;; ============================================================
;; 5. Standard error codes
;; ============================================================

(test-case "standard error codes are defined"
  (check-equal? RPC-ERROR-PARSE -32700)
  (check-equal? RPC-ERROR-INVALID-REQUEST -32600)
  (check-equal? RPC-ERROR-METHOD-NOT-FOUND -32601)
  (check-equal? RPC-ERROR-INVALID-PARAMS -32602)
  (check-equal? RPC-ERROR-INTERNAL -32603))

;; ============================================================
;; 6. dispatch-rpc-request
;; ============================================================

(define (test-handlers)
  (hasheq 'ping (λ (params) #t)
          'prompt (λ (params) (hasheq 'reply "echo"))
          'session.create (λ (params) (hasheq 'sessionId "s-1"))))

(test-case "dispatch-rpc-request: routes to correct handler"
  (define req (rpc-request "r1" 'ping (make-immutable-hash)))
  (define resp (dispatch-rpc-request req (test-handlers)))
  (check-equal? (rpc-response-id resp) "r1")
  (check-equal? (rpc-response-result resp) #t)
  (check-false (rpc-response-error resp)))

(test-case "dispatch-rpc-request: passes params to handler"
  (define req (rpc-request "r2" 'prompt (hasheq 'text "hi")))
  (define resp (dispatch-rpc-request req (test-handlers)))
  (check-equal? (rpc-response-id resp) "r2")
  (check-equal? (hash-ref (rpc-response-result resp) 'reply) "echo"))

(test-case "dispatch-rpc-request: unknown method returns METHOD-NOT-FOUND"
  (define req (rpc-request "r3" 'nonexistent (make-immutable-hash)))
  (define resp (dispatch-rpc-request req (test-handlers)))
  (check-equal? (rpc-response-id resp) "r3")
  (check-false (rpc-response-result resp))
  (check-equal? (hash-ref (rpc-response-error resp) 'code) RPC-ERROR-METHOD-NOT-FOUND))

(test-case "dispatch-rpc-request: handler exception becomes INTERNAL error"
  (define handlers (hasheq 'fail (λ (params) (error 'fail "boom"))))
  (define req (rpc-request "r4" 'fail (make-immutable-hash)))
  (define resp (dispatch-rpc-request req handlers))
  (check-equal? (rpc-response-id resp) "r4")
  (check-false (rpc-response-result resp))
  (check-equal? (hash-ref (rpc-response-error resp) 'code) RPC-ERROR-INTERNAL))

;; ============================================================
;; 7. run-rpc-loop
;; ============================================================

(test-case "run-rpc-loop: single ping request"
  (define input "{\"id\":\"p1\",\"method\":\"ping\",\"params\":{}}\n")
  (define in (open-input-string input))
  (define out (open-output-string))
  (define handlers (hasheq 'ping (λ (params) "pong")))
  (run-rpc-loop handlers #:input-port in #:output-port out)
  (define output (get-output-string out))
  (define parsed (read-json (open-input-string output)))
  (check-equal? (hash-ref parsed 'id) "p1")
  (check-equal? (hash-ref parsed 'result) "pong")
  (check-false (hash-ref parsed 'error)))

(test-case "run-rpc-loop: multiple requests"
  (define input (string-append
                 "{\"id\":\"a\",\"method\":\"ping\",\"params\":{}}\n"
                 "{\"id\":\"b\",\"method\":\"session.create\",\"params\":{}}\n"))
  (define in (open-input-string input))
  (define out (open-output-string))
  (define handlers (hasheq 'ping (λ (p) #t)
                           'session.create (λ (p) (hasheq 'sessionId "s1"))))
  (run-rpc-loop handlers #:input-port in #:output-port out)
  (define output (get-output-string out))
  (define lines (string-split output "\n" #:trim? #f))
  ;; Filter out empty trailing line
  (define json-lines (filter (λ (l) (> (string-length (string-trim l)) 0)) lines))
  (check-equal? (length json-lines) 2)
  (define resp1 (read-json (open-input-string (first json-lines))))
  (define resp2 (read-json (open-input-string (second json-lines))))
  (check-equal? (hash-ref resp1 'id) "a")
  (check-equal? (hash-ref resp1 'result) #t)
  (check-equal? (hash-ref resp2 'id) "b")
  (check-equal? (hash-ref (hash-ref resp2 'result) 'sessionId) "s1"))

(test-case "run-rpc-loop: empty lines are skipped"
  (define input "\n\n{\"id\":\"e1\",\"method\":\"ping\",\"params\":{}}\n\n")
  (define in (open-input-string input))
  (define out (open-output-string))
  (define handlers (hasheq 'ping (λ (p) #t)))
  (run-rpc-loop handlers #:input-port in #:output-port out)
  (define output (get-output-string out))
  (define json-lines (filter (λ (l) (> (string-length (string-trim l)) 0))
                             (string-split output "\n" #:trim? #f)))
  (check-equal? (length json-lines) 1))

(test-case "run-rpc-loop: malformed JSON returns PARSE error"
  (define input "not json\n")
  (define in (open-input-string input))
  (define out (open-output-string))
  (run-rpc-loop (hasheq) #:input-port in #:output-port out)
  (define output (get-output-string out))
  (define parsed (read-json (open-input-string output)))
  (check-equal? (hash-ref parsed 'id) #f)
  (check-equal? (hash-ref (hash-ref parsed 'error) 'code) RPC-ERROR-PARSE))

(test-case "run-rpc-loop: invalid request (missing method) returns error"
  (define input "{\"id\":\"x\"}\n")
  (define in (open-input-string input))
  (define out (open-output-string))
  (run-rpc-loop (hasheq) #:input-port in #:output-port out)
  (define output (get-output-string out))
  (define parsed (read-json (open-input-string output)))
  (check-equal? (hash-ref parsed 'id) "x")
  (check-equal? (hash-ref (hash-ref parsed 'error) 'code) RPC-ERROR-INVALID-REQUEST))

(test-case "run-rpc-loop: shutdown terminates the loop"
  ;; shutdown should be processed but no response is needed (or a success response)
  ;; Then the loop stops, so no more reads.
  (define input "{\"id\":\"s1\",\"method\":\"shutdown\",\"params\":{}}\n")
  (define in (open-input-string input))
  (define out (open-output-string))
  (define handlers (hasheq 'shutdown (λ (p) #t)))
  (run-rpc-loop handlers #:input-port in #:output-port out)
  (define output (get-output-string out))
  ;; shutdown should produce a success response and then stop
  (define parsed (read-json (open-input-string (string-trim output))))
  (check-equal? (hash-ref parsed 'id) "s1")
  (check-equal? (hash-ref parsed 'result) #t))

(test-case "run-rpc-loop: handler error returns INTERNAL error"
  (define input "{\"id\":\"err1\",\"method\":\"fail\",\"params\":{}}\n")
  (define in (open-input-string input))
  (define out (open-output-string))
  (define handlers (hasheq 'fail (λ (p) (error 'fail "crash"))))
  (run-rpc-loop handlers #:input-port in #:output-port out)
  (define output (get-output-string out))
  (define parsed (read-json (open-input-string (string-trim output))))
  (check-equal? (hash-ref parsed 'id) "err1")
  (check-equal? (hash-ref (hash-ref parsed 'error) 'code) RPC-ERROR-INTERNAL))

;; ============================================================
;; 8. Event forwarding
;; ============================================================

(test-case "start-rpc-event-forwarding!: events become notifications"
  (define bus (make-event-bus))
  (define out (open-output-string))
  (define sub (start-rpc-event-forwarding! bus out))
  ;; Publish an event
  (define evt (make-event "model.stream.delta" 1000 "sess-1" #f (hasheq 'text "world")))
  (publish! bus evt)
  ;; Check the output
  (define output (get-output-string out))
  (define parsed (read-json (open-input-string output)))
  (check-equal? (hash-ref parsed 'jsonrpc) "2.0")
  (check-equal? (hash-ref parsed 'method) "model.stream.delta")
  (check-equal? (hash-ref (hash-ref (hash-ref parsed 'params) 'payload) 'text) "world")
  ;; Cleanup
  (stop-rpc-event-forwarding! bus sub))

(test-case "start-rpc-event-forwarding!: multiple events produce multiple lines"
  (define bus (make-event-bus))
  (define out (open-output-string))
  (define sub (start-rpc-event-forwarding! bus out))
  (publish! bus (make-event "turn.started" 1000 "sess-1" #f (hasheq)))
  (publish! bus (make-event "turn.completed" 2000 "sess-1" #f (hasheq)))
  (define output (get-output-string out))
  (define lines (filter (λ (l) (> (string-length (string-trim l)) 0))
                        (string-split output "\n" #:trim? #f)))
  (check-equal? (length lines) 2)
  (define p1 (read-json (open-input-string (first lines))))
  (define p2 (read-json (open-input-string (second lines))))
  (check-equal? (hash-ref p1 'method) "turn.started")
  (check-equal? (hash-ref p2 'method) "turn.completed")
  (stop-rpc-event-forwarding! bus sub))

(test-case "stop-rpc-event-forwarding!: no more events after stop"
  (define bus (make-event-bus))
  (define out (open-output-string))
  (define sub (start-rpc-event-forwarding! bus out))
  ;; Publish one event
  (publish! bus (make-event "ping" 0 "s" #f (hasheq)))
  (define before-stop (get-output-string out))
  ;; Stop forwarding
  (stop-rpc-event-forwarding! bus sub)
  ;; Publish another — should NOT appear
  (publish! bus (make-event "pong" 1 "s" #f (hasheq)))
  (define after-stop (get-output-string out))
  (check-equal? after-stop before-stop))

;; ============================================================
;; 9. Integration: RPC loop with event forwarding
;; ============================================================

(test-case "run-rpc-loop with notification writer"
  ;; Test that we can combine the loop with event forwarding
  ;; by providing a custom notification-out port
  (define bus (make-event-bus))
  (define input "{\"id\":\"int1\",\"method\":\"ping\",\"params\":{}}\n")
  (define in (open-input-string input))
  (define rpc-out (open-output-string))
  (define notif-out (open-output-string))
  ;; Start event forwarding to notif-out
  (define sub (start-rpc-event-forwarding! bus notif-out))
  ;; Run the RPC loop
  (define handlers (hasheq 'ping (λ (p) #t)))
  (run-rpc-loop handlers #:input-port in #:output-port rpc-out)
  ;; Check RPC response
  (define rpc-output (get-output-string rpc-out))
  (define parsed (read-json (open-input-string (string-trim rpc-output))))
  (check-equal? (hash-ref parsed 'id) "int1")
  (check-equal? (hash-ref parsed 'result) #t)
  ;; No events published, so no notifications
  (check-equal? (string-length (get-output-string notif-out)) 0)
  (stop-rpc-event-forwarding! bus sub))

;; ============================================================
;; 10. Edge cases
;; ============================================================

(test-case "parse-rpc-request: method with dots and slashes"
  (define json-str "{\"id\":\"x\",\"method\":\"session.create\",\"params\":{}}")
  (define req (parse-rpc-request json-str))
  (check-equal? (rpc-request-method req) 'session.create))

(test-case "rpc-response->json: result is #f for error responses"
  (define resp (rpc-error "z1" -32603 "Internal error"))
  (define json-str (rpc-response->json resp))
  (define parsed (read-json (open-input-string json-str)))
  ;; JSON null becomes #f in Racket json library
  (check-false (hash-ref parsed 'result))
  (check-equal? (hash-ref (hash-ref parsed 'error) 'code) -32603))

(test-case "dispatch-rpc-request: empty handlers hash"
  (define req (rpc-request "r0" 'anything (make-immutable-hash)))
  (define resp (dispatch-rpc-request req (hasheq)))
  (check-equal? (hash-ref (rpc-response-error resp) 'code) RPC-ERROR-METHOD-NOT-FOUND))

(test-case "run-rpc-loop: empty input produces no output"
  (define in (open-input-string ""))
  (define out (open-output-string))
  (run-rpc-loop (hasheq) #:input-port in #:output-port out)
  (check-equal? (string-length (get-output-string out)) 0))

;; ============================================================
;; 11. Session management RPC methods (dispatch-level)
;; ============================================================
;; These test the dispatch pattern with mock handlers.
;; Full integration tests (with real sessions) belong in test-main.rkt.

(test-case "dispatch-rpc-request: session.open handler"
  (define sessions (make-hash))
  (define handlers
    (hasheq 'session.open
            (lambda (params)
              (define sid (format "sess-~a" (hash-count sessions)))
              (hash-set! sessions sid 'session)
              (hasheq 'sessionId sid))))
  (define req (rpc-request "so1" 'session.open (make-immutable-hash)))
  (define resp (dispatch-rpc-request req handlers))
  (check-false (rpc-response-error resp))
  (check-equal? (hash-ref (rpc-response-result resp) 'sessionId) "sess-0")
  (check-equal? (hash-count sessions) 1))

(test-case "dispatch-rpc-request: session.list handler"
  (define handlers
    (hasheq 'session.list
            (lambda (params)
              (hasheq 'sessions '("sess-a" "sess-b")))))
  (define req (rpc-request "sl1" 'session.list (make-immutable-hash)))
  (define resp (dispatch-rpc-request req handlers))
  (check-false (rpc-response-error resp))
  (check-equal? (hash-ref (rpc-response-result resp) 'sessions)
                '("sess-a" "sess-b")))

(test-case "dispatch-rpc-request: session.resume handler"
  (define handlers
    (hasheq 'session.resume
            (lambda (params)
              (define sid (hash-ref params 'sessionId))
              (hasheq 'sessionId sid 'status "resumed"))))
  (define req (rpc-request "sr1" 'session.resume (hasheq 'sessionId "sess-42")))
  (define resp (dispatch-rpc-request req handlers))
  (check-false (rpc-response-error resp))
  (check-equal? (hash-ref (rpc-response-result resp) 'sessionId) "sess-42")
  (check-equal? (hash-ref (rpc-response-result resp) 'status) "resumed"))

(test-case "dispatch-rpc-request: session.close handler"
  (define sessions (make-hash))
  (hash-set! sessions "sess-1" 'session)
  (define handlers
    (hasheq 'session.close
            (lambda (params)
              (define sid (hash-ref params 'sessionId))
              (unless (hash-has-key? sessions sid)
                (error 'session.close "not found"))
              (hash-remove! sessions sid)
              (hasheq 'sessionId sid 'status "closed"))))
  (define req (rpc-request "sc1" 'session.close (hasheq 'sessionId "sess-1")))
  (define resp (dispatch-rpc-request req handlers))
  (check-false (rpc-response-error resp))
  (check-equal? (hash-ref (rpc-response-result resp) 'sessionId) "sess-1")
  (check-equal? (hash-ref (rpc-response-result resp) 'status) "closed")
  (check-equal? (hash-count sessions) 0))

(test-case "dispatch-rpc-request: session.close missing sessionId -> error"
  (define handlers
    (hasheq 'session.close
            (lambda (params)
              (define sid (hash-ref params 'sessionId #f))
              (unless sid
                (error 'session.close "sessionId parameter required"))
              (hasheq 'sessionId sid 'status "closed"))))
  (define req (rpc-request "sc2" 'session.close (make-immutable-hash)))
  (define resp (dispatch-rpc-request req handlers))
  (check-false (rpc-response-result resp))
  (check-equal? (hash-ref (rpc-response-error resp) 'code) RPC-ERROR-INTERNAL))

(test-case "dispatch-rpc-request: session.close unknown session -> error"
  (define handlers
    (hasheq 'session.close
            (lambda (params)
              (define sid (hash-ref params 'sessionId))
              (error 'session.close "session not found: ~a" sid))))
  (define req (rpc-request "sc3" 'session.close (hasheq 'sessionId "no-such-session")))
  (define resp (dispatch-rpc-request req handlers))
  (check-false (rpc-response-result resp))
  (check-equal? (hash-ref (rpc-response-error resp) 'code) RPC-ERROR-INTERNAL))

(test-case "run-rpc-loop: session.open creates and returns session-id"
  (define sessions (make-hash))
  (define input "{\"id\":\"so2\",\"method\":\"session.open\",\"params\":{}}\n")
  (define in (open-input-string input))
  (define out (open-output-string))
  (define handlers
    (hasheq 'session.open
            (lambda (params)
              (define sid "mock-sess-1")
              (hash-set! sessions sid #t)
              (hasheq 'sessionId sid))))
  (run-rpc-loop handlers #:input-port in #:output-port out)
  (define output (get-output-string out))
  (define parsed (read-json (open-input-string (string-trim output))))
  (check-equal? (hash-ref parsed 'id) "so2")
  (check-equal? (hash-ref (hash-ref parsed 'result) 'sessionId) "mock-sess-1")
  (check-false (hash-ref parsed 'error)))

(test-case "run-rpc-loop: session.list returns sessions array"
  (define input "{\"id\":\"sl2\",\"method\":\"session.list\",\"params\":{}}\n")
  (define in (open-input-string input))
  (define out (open-output-string))
  (define handlers
    (hasheq 'session.list
            (lambda (params)
              (hasheq 'sessions '("aaa" "bbb")))))
  (run-rpc-loop handlers #:input-port in #:output-port out)
  (define output (get-output-string out))
  (define parsed (read-json (open-input-string (string-trim output))))
  (check-equal? (hash-ref parsed 'id) "sl2")
  (check-equal? (hash-ref (hash-ref parsed 'result) 'sessions) '("aaa" "bbb")))

(test-case "run-rpc-loop: session.resume with sessionId param"
  (define input "{\"id\":\"sr2\",\"method\":\"session.resume\",\"params\":{\"sessionId\":\"existing-sess\"}}\n")
  (define in (open-input-string input))
  (define out (open-output-string))
  (define handlers
    (hasheq 'session.resume
            (lambda (params)
              (hasheq 'sessionId (hash-ref params 'sessionId)
                      'status "resumed"))))
  (run-rpc-loop handlers #:input-port in #:output-port out)
  (define output (get-output-string out))
  (define parsed (read-json (open-input-string (string-trim output))))
  (check-equal? (hash-ref (hash-ref parsed 'result) 'status) "resumed")
  (check-equal? (hash-ref (hash-ref parsed 'result) 'sessionId) "existing-sess"))

(test-case "run-rpc-loop: session.close with sessionId param"
  (define input "{\"id\":\"sc4\",\"method\":\"session.close\",\"params\":{\"sessionId\":\"sess-x\"}}\n")
  (define in (open-input-string input))
  (define out (open-output-string))
  (define handlers
    (hasheq 'session.close
            (lambda (params)
              (hasheq 'sessionId (hash-ref params 'sessionId)
                      'status "closed"))))
  (run-rpc-loop handlers #:input-port in #:output-port out)
  (define output (get-output-string out))
  (define parsed (read-json (open-input-string (string-trim output))))
  (check-equal? (hash-ref (hash-ref parsed 'result) 'status) "closed")
  (check-equal? (hash-ref (hash-ref parsed 'result) 'sessionId) "sess-x"))

(test-case "parse-rpc-request: all session method names parse correctly"
  (for ([method-str '("session.open" "session.list" "session.resume" "session.close")])
    (define json-str (format "{\"id\":\"t\",\"method\":\"~a\",\"params\":{}}" method-str))
    (define req (parse-rpc-request json-str))
    (check-not-false req (format "method ~a should parse" method-str))
    (check-equal? (rpc-request-method req) (string->symbol method-str)
                  (format "method ~a should be correct symbol" method-str))))
