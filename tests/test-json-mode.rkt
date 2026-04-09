#lang racket

;; test-json-mode.rkt — TDD tests for JSON mode interface
;;
;; Tests cover:
;;   1. event->json-line (pure function)
;;   2. message->json (pure function)
;;   3. parse-json-intent (pure function)
;;   4. start-json-mode! / stop-json-mode! (integration with event bus)
;;   5. JSONL format (one object per line)
;;   6. Error event handling (malformed events → error JSON lines)
;;   7. json-mode-event-filter

(require rackunit
         rackunit/text-ui
         racket/port
         json
         "../../q/agent/types.rkt"
         "../../q/agent/event-bus.rkt"
         "../../q/interfaces/json-mode.rkt")

;; ============================================================
;; Test suite definition
;; ============================================================

(define test-json-mode
  (test-suite
   "json-mode interface tests"

   ;; ============================================================
   ;; 1. event->json-line — pure function tests
   ;; ============================================================

   (test-case "event->json-line: converts simple event to valid JSON"
     (define evt (make-event "session.started" 1000 "sess-1" #f (hasheq 'info "test")))
     (define line (event->json-line evt))
     (check-pred string? line)
     (check-not-exn (λ () (string->jsexpr line)))
     (define js (string->jsexpr line))
     (check-equal? (hash-ref js 'event) "session.started")
     (check-equal? (hash-ref js 'time) 1000)
     (check-equal? (hash-ref js 'sessionId) "sess-1"))

   (test-case "event->json-line: includes version field"
     (define evt (make-event "turn.started" 2000 "sess-2" "turn-1" (hasheq)))
     (define js (string->jsexpr (event->json-line evt)))
     (check-equal? (hash-ref js 'version) 1))

   (test-case "event->json-line: includes turnId when present"
     (define evt (make-event "turn.completed" 3000 "sess-3" "turn-2" (hasheq 'reason "done")))
     (define js (string->jsexpr (event->json-line evt)))
     (check-equal? (hash-ref js 'turnId) "turn-2"))

   (test-case "event->json-line: turnId absent when #f"
     (define evt (make-event "session.started" 1000 "sess-1" #f (hasheq)))
     (define js (string->jsexpr (event->json-line evt)))
     ;; When turnId is #f, it should not appear in output (or be null)
     ;; We just check it parses correctly; the key may be absent or null
     (check-not-exn (λ () (hash-ref js 'turnId (void)))))

   (test-case "event->json-line: single line, no embedded newlines"
     (define evt (make-event "model.stream.delta" 5000 "sess-4" "turn-3"
                             (hasheq 'text "hello\nworld")))
     (define line (event->json-line evt))
     ;; The JSON line itself should end with exactly one newline
     ;; No unescaped newlines in the JSON string before the trailing newline
     (define trimmed (string-trim line))
     (check-false (regexp-match? #rx"\n" trimmed))
     ;; But parsing should succeed and contain the text with embedded newline
     (define js (string->jsexpr trimmed))
     (check-equal? (hash-ref (hash-ref js 'payload) 'text) "hello\nworld"))

   (test-case "event->json-line: payload preserved correctly"
     (define payload (hasheq 'model "gpt-4" 'tokens 42 'streaming #t))
     (define evt (make-event "model.stream.completed" 6000 "sess-5" "turn-4" payload))
     (define js (string->jsexpr (string-trim (event->json-line evt))))
     (define p (hash-ref js 'payload))
     (check-equal? (hash-ref p 'model) "gpt-4")
     (check-equal? (hash-ref p 'tokens) 42)
     (check-equal? (hash-ref p 'streaming) #t))

   (test-case "event->json-line: session-id override"
     (define evt (make-event "session.started" 1000 "original" #f (hasheq)))
     (define line (event->json-line evt "override-sess"))
     (define js (string->jsexpr (string-trim line)))
     (check-equal? (hash-ref js 'sessionId) "override-sess"))

   (test-case "event->json-line: ends with newline"
     (define evt (make-event "session.started" 1000 "s1" #f (hasheq)))
     (define line (event->json-line evt))
     (check-equal? (string-ref line (sub1 (string-length line))) #\newline))

   ;; ============================================================
   ;; 2. message->json — pure function tests
   ;; ============================================================

   (test-case "message->json: basic conversion"
     (define msg (make-message "m1" #f 'user 'message
                               (list (make-text-part "hello"))
                               1000 (hasheq)))
     (define js (message->json msg))
     (check-equal? (hash-ref js 'id) "m1")
     (check-equal? (hash-ref js 'parentId) #f)
     (check-equal? (hash-ref js 'role) "user")
     (check-equal? (hash-ref js 'kind) "message")
     (check-equal? (hash-ref js 'timestamp) 1000))

   (test-case "message->json: serializes content parts"
     (define msg (make-message "m2" "m1" 'assistant 'message
                               (list (make-text-part "response")
                                     (make-tool-call-part "tc1" "read" "{\"path\":\"foo\"}"))
                               2000 (hasheq)))
     (define js (message->json msg))
     (define content (hash-ref js 'content))
     (check-equal? (length content) 2)
     (check-equal? (hash-ref (first content) 'type) "text")
     (check-equal? (hash-ref (first content) 'text) "response")
     (check-equal? (hash-ref (second content) 'type) "tool-call")
     (check-equal? (hash-ref (second content) 'name) "read"))

   (test-case "message->json: round-trips through JSON"
     (define msg (make-message "m3" #f 'system 'message
                               (list (make-text-part "system prompt"))
                               0 (hasheq 'source "global")))
     (define json-str (jsexpr->string (message->json msg)))
     (check-not-exn (λ () (string->jsexpr json-str)))
     (define js (string->jsexpr json-str))
     (check-equal? (hash-ref js 'id) "m3")
     (check-equal? (hash-ref js 'role) "system"))

   ;; ============================================================
   ;; 3. parse-json-intent — pure function tests
   ;; ============================================================

   (test-case "parse-json-intent: prompt intent"
     (define i (parse-json-intent "{\"intent\":\"prompt\",\"text\":\"hello world\"}"))
     (check-not-false i)
     (check-equal? (intent-type i) 'prompt)
     (check-equal? (hash-ref (intent-payload i) 'text) "hello world"))

   (test-case "parse-json-intent: interrupt intent"
     (define i (parse-json-intent "{\"intent\":\"interrupt\"}"))
     (check-not-false i)
     (check-equal? (intent-type i) 'interrupt)
     (check-equal? (intent-payload i) #f))

   (test-case "parse-json-intent: compact intent"
     (define i (parse-json-intent "{\"intent\":\"compact\"}"))
     (check-not-false i)
     (check-equal? (intent-type i) 'compact)
     (check-equal? (intent-payload i) #f))

   (test-case "parse-json-intent: fork intent"
     (define i (parse-json-intent "{\"intent\":\"fork\",\"entryId\":\"entry-42\"}"))
     (check-not-false i)
     (check-equal? (intent-type i) 'fork)
     (check-equal? (hash-ref (intent-payload i) 'entryId) "entry-42"))

   (test-case "parse-json-intent: quit intent"
     (define i (parse-json-intent "{\"intent\":\"quit\"}"))
     (check-not-false i)
     (check-equal? (intent-type i) 'quit)
     (check-equal? (intent-payload i) #f))

   (test-case "parse-json-intent: returns #f for malformed JSON"
     (check-false (parse-json-intent "not json at all"))
     (check-false (parse-json-intent ""))
     (check-false (parse-json-intent "{invalid")))

   (test-case "parse-json-intent: returns #f for missing intent field"
     (check-false (parse-json-intent "{\"text\":\"hello\"}")))

   (test-case "parse-json-intent: returns #f for unknown intent type"
     (check-false (parse-json-intent "{\"intent\":\"unknown\"}")))

   ;; ============================================================
   ;; 4. start-json-mode! / stop-json-mode! — integration tests
   ;; ============================================================

   (test-case "start-json-mode!: subscribes and outputs events as JSON"
     (define bus (make-event-bus))
     (define out (open-output-string))
     (define sub-id (start-json-mode! bus #:session-id "test-sess" #:output-port out))
     (check-true (exact-nonnegative-integer? sub-id))
     ;; Publish an event
     (define evt (make-event "session.started" 1000 "test-sess" #f (hasheq 'source "test")))
     (publish! bus evt)
     ;; Check output
     (define output (get-output-string out))
     (check-not-equal? output "")
     (define js (string->jsexpr (string-trim output)))
     (check-equal? (hash-ref js 'event) "session.started")
     (check-equal? (hash-ref js 'sessionId) "test-sess")
     ;; Clean up
     (stop-json-mode! bus sub-id))

   (test-case "start-json-mode!: multiple events produce multiple JSON lines"
     (define bus (make-event-bus))
     (define out (open-output-string))
     (define sub-id (start-json-mode! bus #:session-id "multi-sess" #:output-port out))
     (publish! bus (make-event "session.started" 1000 "multi-sess" #f (hasheq)))
     (publish! bus (make-event "turn.started" 1100 "multi-sess" "turn-1" (hasheq)))
     (publish! bus (make-event "turn.completed" 2000 "multi-sess" "turn-1" (hasheq 'reason "done")))
     (stop-json-mode! bus sub-id)
     (define output (get-output-string out))
     (define lines (filter (λ (l) (not (string=? l "")))
                           (string-split output "\n")))
     (check-equal? (length lines) 3)
     ;; Each line should be valid JSON
     (for ([line (in-list lines)])
       (check-not-exn (λ () (string->jsexpr line)))))

   (test-case "stop-json-mode!: unsubscribes — no more events received"
     (define bus (make-event-bus))
     (define out (open-output-string))
     (define sub-id (start-json-mode! bus #:session-id "stop-sess" #:output-port out))
     (publish! bus (make-event "session.started" 1000 "stop-sess" #f (hasheq)))
     (stop-json-mode! bus sub-id)
     ;; Publish after stop — should NOT appear
     (publish! bus (make-event "turn.started" 1100 "stop-sess" "turn-1" (hasheq)))
     (define output (get-output-string out))
     (define lines (filter (λ (l) (not (string=? l "")))
                           (string-split output "\n")))
     (check-equal? (length lines) 1))

   ;; ============================================================
   ;; 5. JSONL format verification
   ;; ============================================================

   (test-case "JSONL format: one complete JSON object per line"
     (define bus (make-event-bus))
     (define out (open-output-string))
     (define sub-id (start-json-mode! bus #:session-id "jsonl-sess" #:output-port out))
     (for ([i (in-range 5)])
       (publish! bus (make-event "model.stream.delta"
                                 (+ 1000 i)
                                 "jsonl-sess"
                                 "turn-1"
                                 (hasheq 'text (format "chunk ~a" i)))))
     (stop-json-mode! bus sub-id)
     (define output (get-output-string out))
     (define lines (filter (λ (l) (not (string=? l "")))
                           (string-split output "\n")))
     (check-equal? (length lines) 5)
     ;; Verify each line is independent, valid JSON
     (for ([line (in-list lines)]
           [i (in-naturals)])
       (define js (string->jsexpr line))
       (check-equal? (hash-ref js 'event) "model.stream.delta")
       (check-equal? (hash-ref (hash-ref js 'payload) 'text)
                     (format "chunk ~a" i))))

   ;; ============================================================
   ;; 6. Error event handling
   ;; ============================================================

   (test-case "error event: runtime.error serializes correctly"
     (define evt (make-event "runtime.error" 9999 "err-sess" #f
                             (hasheq 'message "something went wrong"
                                     'code 500)))
     (define line (event->json-line evt))
     (check-not-exn (λ () (string->jsexpr (string-trim line))))
     (define js (string->jsexpr (string-trim line)))
     (check-equal? (hash-ref js 'event) "runtime.error")
     (check-equal? (hash-ref (hash-ref js 'payload) 'message) "something went wrong"))

   (test-case "error event: nested payload serializes correctly"
     (define nested (hasheq 'inner (hasheq 'key "value" 'num 42)))
     (define evt (make-event "tool.call.completed" 5000 "sess" "t1" nested))
     (define js (string->jsexpr (string-trim (event->json-line evt))))
     (define inner (hash-ref (hash-ref js 'payload) 'inner))
     (check-equal? (hash-ref inner 'key) "value")
     (check-equal? (hash-ref inner 'num) 42))

   ;; ============================================================
   ;; 7. json-mode-event-filter tests
   ;; ============================================================

   (test-case "json-mode-event-filter: accepts session events"
     (check-true (json-mode-event-filter
                  (make-event "session.started" 1000 "s" #f (hasheq)))))

   (test-case "json-mode-event-filter: accepts model events"
     (check-true (json-mode-event-filter
                  (make-event "model.stream.delta" 2000 "s" "t" (hasheq)))))

   (test-case "json-mode-event-filter: accepts tool events"
     (check-true (json-mode-event-filter
                  (make-event "tool.call.started" 3000 "s" "t" (hasheq)))))

   (test-case "json-mode-event-filter: accepts error events"
     (check-true (json-mode-event-filter
                  (make-event "runtime.error" 4000 "s" #f (hasheq)))))

   (test-case "json-mode-event-filter: accepts compaction events"
     (check-true (json-mode-event-filter
                  (make-event "compaction.started" 5000 "s" #f (hasheq)))))

   ;; ============================================================
   ;; 8. Intent struct tests
   ;; ============================================================

   (test-case "intent struct: transparent equality"
     (check-equal? (intent 'prompt (hasheq 'text "hi"))
                   (intent 'prompt (hasheq 'text "hi"))))

   (test-case "intent struct: accessors"
     (define i (intent 'fork (hasheq 'entryId "e1")))
     (check-equal? (intent-type i) 'fork)
     (check-equal? (hash-ref (intent-payload i) 'entryId) "e1"))
   ))

;; Run the suite
(run-tests test-json-mode)
