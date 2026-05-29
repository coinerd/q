#lang racket

;; tests/test-gemini-stream.rkt — v0.70.8 W1
;; Extracted from test-gemini.rkt: streaming + parse-single-event tests

(require rackunit
         "../llm/model.rkt"
         "../llm/stream.rkt"
         "../llm/gemini.rkt")

;; ============================================================
;; 14. gemini-parse-stream-chunks — text deltas
;; ============================================================

(define stream-text-events
  (list
   (hash
    'candidates
    (list (hash 'content (hash 'role "model" 'parts (list (hash 'text "Hello"))) 'finishReason #f)))
   (hash
    'candidates
    (list
     (hash 'content (hash 'role "model" 'parts (list (hash 'text " world"))) 'finishReason "STOP"))
    'usageMetadata
    (hash 'promptTokenCount 10 'candidatesTokenCount 2 'totalTokenCount 12))))

(define stream-text-chunks (gemini-parse-stream-chunks stream-text-events))
(check-pred list? stream-text-chunks "stream parsing returns list")
(check-true (andmap stream-chunk? stream-text-chunks) "all results are stream-chunk?")

(define text-deltas (filter (lambda (c) (stream-chunk-delta-text c)) stream-text-chunks))
(check-equal? (length text-deltas) 2 "two text deltas")
(check-equal? (stream-chunk-delta-text (car text-deltas)) "Hello" "first text delta")
(check-equal? (stream-chunk-delta-text (cadr text-deltas)) " world" "second text delta")

;; ============================================================
;; 15. gemini-parse-stream-chunks — done with usage
;; ============================================================

(define stream-done-events
  (list
   (hash
    'candidates
    (list (hash 'content (hash 'role "model" 'parts (list (hash 'text "Hi"))) 'finishReason "STOP"))
    'usageMetadata
    (hash 'promptTokenCount 8 'candidatesTokenCount 5 'totalTokenCount 13))))

(define stream-done-chunks (gemini-parse-stream-chunks stream-done-events))
(define done-chunks (filter (lambda (c) (stream-chunk-done? c)) stream-done-chunks))
(check-equal? (length done-chunks) 1 "one done chunk")
(define done-chunk (car done-chunks))
(check-pred hash? (stream-chunk-usage done-chunk) "done chunk carries usage")

;; ============================================================
;; 16. gemini-parse-stream-chunks — tool use deltas
;; ============================================================

(define stream-tool-events
  (list (hash 'candidates
              (list (hash 'content
                          (hash 'role "model" 'parts (list (hash 'text "Running command...")))
                          'finishReason
                          #f)))
        (hash 'candidates
              (list (hash 'content
                          (hash 'role
                                "model"
                                'parts
                                (list (hash 'functionCall
                                            (hash 'name "bash" 'args (hash 'command "ls")))))
                          'finishReason
                          "STOP")))))

(define stream-tool-chunks (gemini-parse-stream-chunks stream-tool-events))
(check-pred list? stream-tool-chunks)
(define tool-stream-text (filter (lambda (c) (stream-chunk-delta-text c)) stream-tool-chunks))
(check-equal? (length tool-stream-text) 1 "one text delta before tool call")
(define tool-deltas (filter (lambda (c) (stream-chunk-delta-tool-call c)) stream-tool-chunks))
(check-equal? (length tool-deltas) 1 "one tool-call delta")
(check-equal? (hash-ref (hash-ref (stream-chunk-delta-tool-call (car tool-deltas)) 'function) 'name)
              "bash"
              "tool call name preserved in stream")
(define tool-done (filter (lambda (c) (stream-chunk-done? c)) stream-tool-chunks))
(check-equal? (length tool-done) 1 "stream ends with done chunk")

;; ============================================================
;; 29. Stream chunk — empty text parts are filtered
;; ============================================================

(define stream-empty-text-events
  (list
   (hash 'candidates
         (list (hash 'content (hash 'role "model" 'parts (list (hash 'text ""))) 'finishReason #f)))
   (hash 'candidates
         (list (hash 'content
                     (hash 'role "model" 'parts (list (hash 'text "Actual text")))
                     'finishReason
                     "STOP")))))

(define stream-empty-chunks (gemini-parse-stream-chunks stream-empty-text-events))
(define empty-text-deltas (filter (lambda (c) (stream-chunk-delta-text c)) stream-empty-chunks))
(check-equal? (length empty-text-deltas) 1 "empty text parts are filtered out")
(check-equal? (stream-chunk-delta-text (car empty-text-deltas))
              "Actual text"
              "only non-empty text preserved")

;; ============================================================
;; 34. Issue #110 — Gemini stream tool calls have unique IDs
;; ============================================================

(gemini-reset-tool-id-counter!)
(define stream-dual-tool-events
  (list
   (hash 'candidates
         (list (hash 'content
                     (hash 'role
                           "model"
                           'parts
                           (list (hash 'functionCall (hash 'name "bash" 'args (hash 'command "ls")))
                                 (hash 'functionCall (hash 'name "read" 'args (hash 'path "/tmp")))))
                     'finishReason
                     "STOP")))))

(define stream-dual-chunks (gemini-parse-stream-chunks stream-dual-tool-events))
(define stream-tool-deltas (filter (lambda (c) (stream-chunk-delta-tool-call c)) stream-dual-chunks))
(check-equal? (length stream-tool-deltas) 2 "gemini stream: 2 tool call deltas")
(define stc1 (stream-chunk-delta-tool-call (car stream-tool-deltas)))
(define stc2 (stream-chunk-delta-tool-call (cadr stream-tool-deltas)))
(check-true (> (string-length (hash-ref stc1 'id)) 0) "gemini stream tool call 1 has non-empty ID")
(check-true (> (string-length (hash-ref stc2 'id)) 0) "gemini stream tool call 2 has non-empty ID")
(check-not-equal? (hash-ref stc1 'id) (hash-ref stc2 'id) "gemini stream tool calls have unique IDs")

;; ============================================================
;; 35. Issue #109 — Gemini stream uses cons+reverse (not append)
;; ============================================================

(define gemini-large-events
  (for/list ([i (in-range 100)])
    (hash 'candidates
          (list (hash 'content
                      (hash 'role "model" 'parts (list (hash 'text (format "chunk~a " i))))
                      'finishReason
                      #f)))))

(define gemini-large-chunks (gemini-parse-stream-chunks gemini-large-events))
(define gemini-large-texts (filter (lambda (c) (stream-chunk-delta-text c)) gemini-large-chunks))
(check-equal? (length gemini-large-texts) 100 "100 text deltas preserved (cons+reverse correctness)")
(check-equal? (stream-chunk-delta-text (car gemini-large-texts)) "chunk0 " "first chunk is chunk0")
(check-equal? (stream-chunk-delta-text (list-ref gemini-large-texts 99))
              "chunk99 "
              "last chunk is chunk99")

;; ============================================================
;; 36. gemini-parse-single-event — text delta
;; ============================================================

(test-case "gemini-parse-single-event: text delta → list with one text chunk"
  (define event
    (hash
     'candidates
     (list (hash 'content (hash 'role "model" 'parts (list (hash 'text "Hello"))) 'finishReason #f))))
  (define chunks (gemini-parse-single-event event))
  (check-equal? (length chunks) 1)
  (check-equal? (stream-chunk-delta-text (car chunks)) "Hello")
  (check-false (stream-chunk-delta-tool-call (car chunks)))
  (check-false (stream-chunk-done? (car chunks))))

;; ============================================================
;; 37. gemini-parse-single-event — functionCall → tool-call chunk
;; ============================================================

(test-case "gemini-parse-single-event: functionCall → tool-call delta"
  (gemini-reset-tool-id-counter!)
  (define event
    (hash 'candidates
          (list (hash 'content
                      (hash 'role
                            "model"
                            'parts
                            (list (hash 'functionCall
                                        (hash 'name "bash" 'args (hash 'command "ls")))))
                      'finishReason
                      #f))))
  (define chunks (gemini-parse-single-event event))
  (check-equal? (length chunks) 1)
  (define tc (stream-chunk-delta-tool-call (car chunks)))
  (check-true (hash? tc))
  (check-true (> (string-length (hash-ref tc 'id)) 0))
  (check-equal? (hash-ref (hash-ref tc 'function) 'name) "bash"))

;; ============================================================
;; 38. gemini-parse-single-event — finish reason → done chunk
;; ============================================================

(test-case "gemini-parse-single-event: finishReason STOP → done chunk with usage"
  (define event
    (hash
     'candidates
     (list (hash 'content (hash 'role "model" 'parts (list (hash 'text "Hi"))) 'finishReason "STOP"))
     'usageMetadata
     (hash 'promptTokenCount 10 'candidatesTokenCount 5 'totalTokenCount 15)))
  (define chunks (gemini-parse-single-event event))
  (check-true (>= (length chunks) 2))
  (define done-chunk (last chunks))
  (check-true (stream-chunk-done? done-chunk))
  (check-equal? (hash-ref (stream-chunk-usage done-chunk) 'prompt_tokens) 10))

;; ============================================================
;; 39. gemini-parse-single-event — empty text filtered
;; ============================================================

(test-case "gemini-parse-single-event: empty text part produces no text chunk"
  (define event
    (hash 'candidates
          (list (hash 'content (hash 'role "model" 'parts (list (hash 'text ""))) 'finishReason #f))))
  (define chunks (gemini-parse-single-event event))
  (check-equal? chunks '() "empty text produces no chunks"))

;; ============================================================
;; 40. gemini-parse-single-event — usage-only event
;; ============================================================

(test-case "gemini-parse-single-event: usage-only event (no finishReason) → usage chunk"
  (define event
    (hash 'usageMetadata (hash 'promptTokenCount 42 'candidatesTokenCount 0 'totalTokenCount 42)))
  (define chunks (gemini-parse-single-event event))
  (check-equal? (length chunks) 1)
  (check-equal? (hash-ref (stream-chunk-usage (car chunks)) 'prompt_tokens) 42)
  (check-false (stream-chunk-done? (car chunks))))

;; ============================================================
;; Issue #443 — Gemini streaming tool calls: distinct indices per tool call
;; ============================================================

(test-case "Issue #443: streaming tool calls across separate SSE chunks get distinct indices"
  (gemini-reset-tool-id-counter!)
  (define stream-events
    (list (hash 'candidates
                (list (hash 'content
                            (hash 'role
                                  "model"
                                  'parts
                                  (list (hash 'functionCall
                                              (hash 'name "bash" 'args (hash 'command "ls")))))
                            'finishReason
                            #f)))
          (hash 'candidates
                (list (hash 'content
                            (hash 'role
                                  "model"
                                  'parts
                                  (list (hash 'functionCall
                                              (hash 'name "read" 'args (hash 'path "/tmp")))))
                            'finishReason
                            "STOP")))))
  (define chunks (gemini-parse-stream-chunks stream-events))
  (define tc-chunks (filter (lambda (c) (stream-chunk-delta-tool-call c)) chunks))
  (check-equal? (length tc-chunks) 2 "two tool-call deltas across two SSE chunks")
  (define tc1 (stream-chunk-delta-tool-call (car tc-chunks)))
  (define tc2 (stream-chunk-delta-tool-call (cadr tc-chunks)))
  (check-equal? (hash-ref tc1 'index) 0 "first tool call has index 0")
  (check-equal? (hash-ref tc2 'index) 1 "second tool call has index 1")
  (check-not-equal? (hash-ref tc1 'id)
                    (hash-ref tc2 'id)
                    "streaming tool calls across chunks have unique IDs")
  (check-equal? (hash-ref (hash-ref tc1 'function) 'name) "bash" "first tool call name preserved")
  (check-equal? (hash-ref (hash-ref tc2 'function) 'name) "read" "second tool call name preserved")
  (define accumulated (accumulate-tool-call-deltas tc-chunks))
  (check-equal? (length accumulated) 2 "accumulated tool calls produce 2 separate entries")
  (check-equal? (hash-ref (car accumulated) 'name) "bash" "first accumulated tool call is bash")
  (check-equal? (hash-ref (cadr accumulated) 'name) "read" "second accumulated tool call is read"))

(test-case "Issue #443: single SSE chunk with two functionCalls gets distinct indices"
  (gemini-reset-tool-id-counter!)
  (define event
    (hash 'candidates
          (list (hash 'content
                      (hash 'role
                            "model"
                            'parts
                            (list (hash 'functionCall (hash 'name "bash" 'args (hash 'command "ls")))
                                  (hash 'functionCall (hash 'name "read" 'args (hash 'path "/tmp")))))
                      'finishReason
                      "STOP"))))
  (define chunks (gemini-parse-single-event event))
  (define tc-chunks (filter (lambda (c) (stream-chunk-delta-tool-call c)) chunks))
  (check-equal? (length tc-chunks) 2 "two tool-call deltas in one event")
  (define tc1 (stream-chunk-delta-tool-call (car tc-chunks)))
  (define tc2 (stream-chunk-delta-tool-call (cadr tc-chunks)))
  (check-equal? (hash-ref tc1 'index) 0 "first tool call in same event: index 0")
  (check-equal? (hash-ref tc2 'index) 1 "second tool call in same event: index 1")
  (check-not-equal? (hash-ref tc1 'id) (hash-ref tc2 'id) "tool calls in same event have unique IDs"))

(println "All Gemini stream tests passed!")
