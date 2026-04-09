#lang racket

(require rackunit
         "../llm/stream.rkt"
         "../llm/model.rkt")

;; ============================================================
;; parse-sse-line tests
;; ============================================================

(check-equal? (parse-sse-line "") #f)
(check-equal? (parse-sse-line "  ") #f)
(check-equal? (parse-sse-line ": comment") #f)
(check-equal? (parse-sse-line "data: [DONE]") 'done)
(check-false (parse-sse-line "not a data line"))
(check-equal? (hash-ref (parse-sse-line "data: {\"id\":\"chatcmpl-1\",\"choices\":[{\"delta\":{\"content\":\"Hi\"}}]}") 'id) "chatcmpl-1")

;; ============================================================
;; normalize-openai-chunk tests
;; ============================================================

(define chunk1 (normalize-openai-chunk
  (hash 'id "chatcmpl-1" 'choices (list (hash 'delta (hash 'content "Hello") 'finish_reason 'null)) 'usage (hash))))
(check-true (stream-chunk? chunk1))
(check-equal? (stream-chunk-delta-text chunk1) "Hello")
(check-false (stream-chunk-delta-tool-call chunk1))

(define chunk2 (normalize-openai-chunk
  (hash 'id "chatcmpl-2" 'choices (list (hash 'delta (hash 'tool_calls (list (hash 'index 0 'id "call_1" 'function (hash 'name "bash" 'arguments "")))) 'finish_reason 'null)))))
(check-true (stream-chunk? chunk2))
(check-false (stream-chunk-delta-text chunk2))
(check-true (hash? (stream-chunk-delta-tool-call chunk2)))

(define chunk3 (normalize-openai-chunk
  (hash 'id "chatcmpl-3" 'choices (list (hash 'delta (hash) 'finish_reason "stop")) 'usage (hash 'prompt_tokens 10 'completion_tokens 5 'total_tokens 15))))
(check-false (stream-chunk-delta-text chunk3))
(check-true (stream-chunk-done? chunk3))

;; ============================================================
;; read-sse-chunks generator tests
;; ============================================================

;; Basic: two content chunks then [DONE]
(define sse-input "data: {\"id\":\"c1\",\"choices\":[{\"delta\":{\"content\":\"He\"}}]}\n\ndata: {\"id\":\"c2\",\"choices\":[{\"delta\":{\"content\":\"llo\"}}]}\n\ndata: [DONE]\n\n")
(define port (open-input-string sse-input))
(define gen (read-sse-chunks port))
(define c1 (gen))
(check-true (stream-chunk? c1))
(check-equal? (stream-chunk-delta-text c1) "He")
(define c2 (gen))
(check-true (stream-chunk? c2))
(check-equal? (stream-chunk-delta-text c2) "llo")
(define c3 (gen))
(check-false c3)  ; #f = stream done

;; Mixed lines (comments, blanks)
(define sse-input2 ": comment\n\ndata: {\"id\":\"c1\",\"choices\":[{\"delta\":{\"content\":\"A\"}}]}\n\n: another comment\n\ndata: [DONE]\n\n")
(define port2 (open-input-string sse-input2))
(define gen2 (read-sse-chunks port2))
(check-equal? (stream-chunk-delta-text (gen2)) "A")
(check-false (gen2))

;; Malformed line followed by valid data, then EOF
(define sse-input3 "data: bad json\n\ndata: {\"id\":\"c1\",\"choices\":[{\"delta\":{\"content\":\"B\"}}]}\n\n")
(define port3 (open-input-string sse-input3))
(define gen3 (read-sse-chunks port3))
(check-equal? (stream-chunk-delta-text (gen3)) "B")
(check-false (gen3))  ; EOF → #f

;; ============================================================
;; accumulate-tool-call-deltas edge case tests
;; ============================================================

;; Helper: build a stream-chunk with a tool-call delta
(define (make-tc-chunk index id name arguments)
  (stream-chunk
   #f  ; no delta-text
   (hash 'index index
         'id id
         'function (hash 'name name 'arguments arguments))
   #f  ; no usage
   #f)) ; not done

;; accumulate handles empty chunk list
(check-equal? (accumulate-tool-call-deltas '()) '())

;; accumulate handles single chunk
(let ([result (accumulate-tool-call-deltas
               (list (make-tc-chunk 0 "call_1" "bash" "ls")))])
  (check-equal? (length result) 1)
  (check-equal? (hash-ref (car result) 'id) "call_1")
  (check-equal? (hash-ref (car result) 'name) "bash")
  (check-equal? (hash-ref (car result) 'arguments) "ls"))

;; accumulate handles chunks with same index (overwrite)
;; Second chunk with same index provides new id/name — should overwrite
(let* ([ch1 (stream-chunk #f (hash 'index 0 'id "call_old" 'function (hash 'name "old_fn" 'arguments "arg1")) #f #f)]
       [ch2 (stream-chunk #f (hash 'index 0 'id "call_new" 'function (hash 'name "new_fn" 'arguments "arg2")) #f #f)]
       [result (accumulate-tool-call-deltas (list ch1 ch2))])
  (check-equal? (length result) 1)
  (check-equal? (hash-ref (car result) 'id) "call_new")
  (check-equal? (hash-ref (car result) 'name) "new_fn")
  ;; arguments are appended, not overwritten
  (check-equal? (hash-ref (car result) 'arguments) "arg1arg2"))

;; accumulate handles missing index field (default to 0)
(let* ([ch (stream-chunk #f (hash 'id "call_no_idx" 'function (hash 'name "fn" 'arguments "x")) #f #f)]
       [result (accumulate-tool-call-deltas (list ch))])
  (check-equal? (length result) 1)
  (check-equal? (hash-ref (car result) 'id) "call_no_idx")
  (check-equal? (hash-ref (car result) 'name) "fn")
  (check-equal? (hash-ref (car result) 'arguments) "x"))

;; accumulate handles partial function name across chunks
;; First chunk carries id+name, subsequent chunks carry only argument fragments
(let* ([ch1 (stream-chunk #f (hash 'index 0 'id "call_partial" 'function (hash 'name "read_file" 'arguments "{\"pat")) #f #f)]
       [ch2 (stream-chunk #f (hash 'index 0 'function (hash 'arguments "h\": \"")) #f #f)]
       [ch3 (stream-chunk #f (hash 'index 0 'function (hash 'arguments "/tmp/x\"}")) #f #f)]
       [result (accumulate-tool-call-deltas (list ch1 ch2 ch3))])
  (check-equal? (length result) 1)
  (check-equal? (hash-ref (car result) 'id) "call_partial")
  (check-equal? (hash-ref (car result) 'name) "read_file")
  (check-equal? (hash-ref (car result) 'arguments) "{\"path\": \"/tmp/x\"}"))

;; accumulate handles interleaved tool calls (index 0 and 1)
(let* ([ch-a1 (stream-chunk #f (hash 'index 0 'id "call_a" 'function (hash 'name "bash" 'arguments "ls ")) #f #f)]
       [ch-b1 (stream-chunk #f (hash 'index 1 'id "call_b" 'function (hash 'name "read" 'arguments "/et")) #f #f)]
       [ch-a2 (stream-chunk #f (hash 'index 0 'function (hash 'arguments "-la")) #f #f)]
       [ch-b2 (stream-chunk #f (hash 'index 1 'function (hash 'arguments "c/pa")) #f #f)]
       [ch-a3 (stream-chunk #f (hash 'index 0 'function (hash 'arguments "")) #f #f)]
       [ch-b3 (stream-chunk #f (hash 'index 1 'function (hash 'arguments "sswd")) #f #f)]
       [result (accumulate-tool-call-deltas (list ch-a1 ch-b1 ch-a2 ch-b2 ch-a3 ch-b3))])
  (check-equal? (length result) 2)
  ;; index 0 = bash
  (check-equal? (hash-ref (car result) 'id) "call_a")
  (check-equal? (hash-ref (car result) 'name) "bash")
  (check-equal? (hash-ref (car result) 'arguments) "ls -la")
  ;; index 1 = read
  (check-equal? (hash-ref (cadr result) 'id) "call_b")
  (check-equal? (hash-ref (cadr result) 'name) "read")
  (check-equal? (hash-ref (cadr result) 'arguments) "/etc/passwd"))

(displayln "All stream incremental tests passed")
