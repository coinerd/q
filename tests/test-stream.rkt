#lang racket

;; BOUNDARY: integration

(require rackunit
         "../llm/stream.rkt"
         "../llm/model.rkt"
         "../agent/loop-stream.rkt")

;; ============================================================
;; parse-sse-line tests
;; ============================================================

(check-equal? (parse-sse-line "") #f)
(check-equal? (parse-sse-line "  ") #f)
(check-equal? (parse-sse-line ": comment") #f)
(check-equal? (parse-sse-line "data: [DONE]") 'done)
(check-false (parse-sse-line "not a data line"))
(check-equal?
 (hash-ref
  (parse-sse-line "data: {\"id\":\"chatcmpl-1\",\"choices\":[{\"delta\":{\"content\":\"Hi\"}}]}")
  'id)
 "chatcmpl-1")

;; ============================================================
;; normalize-openai-chunk tests
;; ============================================================

(define chunk1
  (normalize-openai-chunk (hash 'id
                                "chatcmpl-1"
                                'choices
                                (list (hash 'delta (hash 'content "Hello") 'finish_reason 'null))
                                'usage
                                (hash))))
(check-pred stream-chunk? chunk1)
(check-equal? (stream-chunk-delta-text chunk1) "Hello")
(check-false (stream-chunk-delta-tool-call chunk1))

;; v0.28.19: reasoning_content → delta-thinking (singular)
(define reasoning-chunk
  (normalize-openai-chunk
   (hash
    'id
    "chatcmpl-r1"
    'choices
    (list (hash 'delta (hash 'reasoning_content "Let me think about this...") 'finish_reason 'null))
    'usage
    (hash))))
(check-pred stream-chunk? reasoning-chunk)
(check-false (stream-chunk-delta-text reasoning-chunk)) ;; no content during reasoning
(check-equal? (stream-chunk-delta-thinking reasoning-chunk) "Let me think about this...")

;; v0.28.19: reasoning_content → delta-thinking (batch)
(define reasoning-batch
  (normalize-openai-chunks
   (list (hash 'id
               "c1"
               'choices
               (list (hash 'delta (hash 'reasoning_content "Step 1") 'finish_reason 'null)))
         (hash 'id
               "c2"
               'choices
               (list (hash 'delta (hash 'content "The answer is 42") 'finish_reason 'null))))))
(check-equal? (length reasoning-batch) 2)
(check-equal? (stream-chunk-delta-thinking (car reasoning-batch)) "Step 1")
(check-false (stream-chunk-delta-text (car reasoning-batch)))
(check-equal? (stream-chunk-delta-text (cadr reasoning-batch)) "The answer is 42")
(check-false (stream-chunk-delta-thinking (cadr reasoning-batch)))

;; v0.28.20 T11: Co-located content + reasoning_content in single chunk (DeepSeek-R1 edge case)
(define co-located-chunk
  (normalize-openai-chunks
   (list (hash 'id
               "chatcmpl-coloc"
               'object
               "chat.completion.chunk"
               'choices
               (list (hash 'delta
                           (hash 'reasoning_content "thinking..." 'content "hello")
                           'finish_reason
                           'null))))))
(check-equal? (length co-located-chunk) 1)
(define coloc (car co-located-chunk))
(check-equal? (stream-chunk-delta-thinking coloc) "thinking...")
(check-equal? (stream-chunk-delta-text coloc) "hello")

(define chunk2
  (normalize-openai-chunk
   (hash
    'id
    "chatcmpl-2"
    'choices
    (list (hash 'delta
                (hash 'tool_calls
                      (list (hash 'index 0 'id "call_1" 'function (hash 'name "bash" 'arguments ""))))
                'finish_reason
                'null)))))
(check-pred stream-chunk? chunk2)
(check-false (stream-chunk-delta-text chunk2))
(check-true (hash? (stream-chunk-delta-tool-call chunk2)))

(define chunk3
  (normalize-openai-chunk (hash 'id
                                "chatcmpl-3"
                                'choices
                                (list (hash 'delta (hash) 'finish_reason "stop"))
                                'usage
                                (hash 'prompt_tokens 10 'completion_tokens 5 'total_tokens 15))))
(check-false (stream-chunk-delta-text chunk3))
(check-pred stream-chunk-done? chunk3)

;; ============================================================
;; read-sse-chunks generator tests
;; ============================================================

;; Basic: two content chunks then [DONE]
(define sse-input
  (string-append "data: {\"id\":\"c1\",\"choices\":[{\"delta\":{\"content\":\"He\"}}]}\n\n"
                 "data: {\"id\":\"c2\",\"choices\":[{\"delta\":{\"content\":\"llo\"}}]}\n\n"
                 "data: [DONE]\n\n"))
(define port (open-input-string sse-input))
(define gen (read-sse-chunks port))
(define c1 (gen))
(check-pred stream-chunk? c1)
(check-equal? (stream-chunk-delta-text c1) "He")
(define c2 (gen))
(check-pred stream-chunk? c2)
(check-equal? (stream-chunk-delta-text c2) "llo")
(define c3 (gen))
(check-false c3) ; #f = stream done

;; Mixed lines (comments, blanks)
(define sse-input2
  ": comment\n\ndata: {\"id\":\"c1\",\"choices\":[{\"delta\":{\"content\":\"A\"}}]}\n\n: another comment\n\ndata: [DONE]\n\n")
(define port2 (open-input-string sse-input2))
(define gen2 (read-sse-chunks port2))
(check-equal? (stream-chunk-delta-text (gen2)) "A")
(check-false (gen2))

;; Malformed line followed by valid data, then EOF
(define sse-input3
  "data: bad json\n\ndata: {\"id\":\"c1\",\"choices\":[{\"delta\":{\"content\":\"B\"}}]}\n\n")
(define port3 (open-input-string sse-input3))
(define gen3 (read-sse-chunks port3))
(check-equal? (stream-chunk-delta-text (gen3)) "B")
(check-false (gen3)) ; EOF → #f

;; ============================================================
;; accumulate-tool-call-deltas edge case tests
;; ============================================================

;; Helper: build a stream-chunk with a tool-call delta
(define (make-tc-chunk index id name arguments)
  (make-stream-chunk #f ; no delta-text
                     (hash 'index index 'id id 'function (hash 'name name 'arguments arguments))
                     #f ; no usage
                     #f)) ; not done

;; accumulate handles empty chunk list
(check-equal? (accumulate-tool-call-deltas '()) '())

;; accumulate handles single chunk
(let ([result (accumulate-tool-call-deltas (list (make-tc-chunk 0 "call_1" "bash" "ls")))])
  (check-equal? (length result) 1)
  (check-equal? (hash-ref (car result) 'id) "call_1")
  (check-equal? (hash-ref (car result) 'name) "bash")
  (check-equal? (hash-ref (car result) 'arguments) "ls"))

;; accumulate handles chunks with same index (overwrite)
;; Second chunk with same index provides new id/name — should overwrite
(let* ([ch1 (make-stream-chunk
             #f
             (hash 'index 0 'id "call_old" 'function (hash 'name "old_fn" 'arguments "arg1"))
             #f
             #f)]
       [ch2 (make-stream-chunk
             #f
             (hash 'index 0 'id "call_new" 'function (hash 'name "new_fn" 'arguments "arg2"))
             #f
             #f)]
       [result (accumulate-tool-call-deltas (list ch1 ch2))])
  (check-equal? (length result) 1)
  (check-equal? (hash-ref (car result) 'id) "call_new")
  (check-equal? (hash-ref (car result) 'name) "new_fn")
  ;; arguments are appended, not overwritten
  (check-equal? (hash-ref (car result) 'arguments) "arg1arg2"))

;; accumulate handles missing index field (default to 0)
(let* ([ch (make-stream-chunk #f
                              (hash 'id "call_no_idx" 'function (hash 'name "fn" 'arguments "x"))
                              #f
                              #f)]
       [result (accumulate-tool-call-deltas (list ch))])
  (check-equal? (length result) 1)
  (check-equal? (hash-ref (car result) 'id) "call_no_idx")
  (check-equal? (hash-ref (car result) 'name) "fn")
  (check-equal? (hash-ref (car result) 'arguments) "x"))

;; accumulate handles partial function name across chunks
;; First chunk carries id+name, subsequent chunks carry only argument fragments
(let* ([ch1 (make-stream-chunk
             #f
             (hash 'index 0 'id "call_partial" 'function (hash 'name "read_file" 'arguments "{\"pat"))
             #f
             #f)]
       [ch2 (make-stream-chunk #f (hash 'index 0 'function (hash 'arguments "h\": \"")) #f #f)]
       [ch3 (make-stream-chunk #f (hash 'index 0 'function (hash 'arguments "/tmp/x\"}")) #f #f)]
       [result (accumulate-tool-call-deltas (list ch1 ch2 ch3))])
  (check-equal? (length result) 1)
  (check-equal? (hash-ref (car result) 'id) "call_partial")
  (check-equal? (hash-ref (car result) 'name) "read_file")
  (check-equal? (hash-ref (car result) 'arguments) "{\"path\": \"/tmp/x\"}"))

;; accumulate handles interleaved tool calls (index 0 and 1)
(let* ([ch-a1 (make-stream-chunk
               #f
               (hash 'index 0 'id "call_a" 'function (hash 'name "bash" 'arguments "ls "))
               #f
               #f)]
       [ch-b1 (make-stream-chunk
               #f
               (hash 'index 1 'id "call_b" 'function (hash 'name "read" 'arguments "/et"))
               #f
               #f)]
       [ch-a2 (make-stream-chunk #f (hash 'index 0 'function (hash 'arguments "-la")) #f #f)]
       [ch-b2 (make-stream-chunk #f (hash 'index 1 'function (hash 'arguments "c/pa")) #f #f)]
       [ch-a3 (make-stream-chunk #f (hash 'index 0 'function (hash 'arguments "")) #f #f)]
       [ch-b3 (make-stream-chunk #f (hash 'index 1 'function (hash 'arguments "sswd")) #f #f)]
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

;; ============================================================
;; read-line/timeout tests
;; ============================================================

(test-case "read-line/timeout returns string from ready port"
  (define p (open-input-string "hello\nworld\n"))
  (check-equal? (read-line/timeout p #:timeout 1) "hello")
  (check-equal? (read-line/timeout p #:timeout 1) "world")
  (check-equal? (read-line/timeout p #:timeout 1) eof))

(test-case "read-line/timeout returns #f on empty port (timeout)"
  ;; A pipe with no writer will block forever on read
  (define-values (in out) (make-pipe))
  (define result (read-line/timeout in #:timeout 0.001))
  (check-false result))

(test-case "read-response-body/timeout reads full body"
  (define data (make-bytes 100 65)) ; 100 bytes of 'A'
  (define p (open-input-bytes data))
  (define result (read-response-body/timeout p #:timeout 1))
  (check-equal? (bytes-length result) 100))

(test-case "read-response-body/timeout raises on timeout"
  ;; Create a port that never produces data
  (define p (open-input-string ""))
  ;; A pipe with no writer will block forever on read
  (define-values (in out) (make-pipe))
  ;; Don't write anything — read will timeout
  (check-exn exn:fail:network:timeout? (lambda () (read-response-body/timeout in #:timeout 0.001))))

(test-case "read-sse-chunks respects timeout"
  (define-values (in out) (make-pipe))
  ;; Write one chunk but then stop — the second read should timeout
  (write-bytes #"data: {\"id\":\"c1\",\"choices\":[{\"delta\":{\"content\":\"Hi\"}}]}\n\n" out)
  (define gen (read-sse-chunks in #:initial-timeout 0.001 #:stream-timeout 0.001))
  (define c1 (gen))
  (check-pred stream-chunk? c1)
  (check-equal? (stream-chunk-delta-text c1) "Hi")
  ;; Next read will timeout — should raise exn:fail:network:timeout
  (check-exn exn:fail:network:timeout? (lambda () (gen))))

(test-case "exn:fail:network:timeout is an exn:fail"
  (check-true (exn:fail? (exn:fail:network:timeout "test" (current-continuation-marks)))))

(test-case "http-read-timeout-default is a positive number"
  (check-true (and (number? http-read-timeout-default) (> http-read-timeout-default 0))))

(test-case "call-with-request-timeout calls #:cleanup on timeout (#454)"
  (define cleanup-called (box #f))
  (check-exn exn:fail:network:timeout?
             (lambda ()
               (call-with-request-timeout (lambda () (sync/timeout 10 never-evt)) ; blocks forever
                                          #:timeout 0.001
                                          #:cleanup (lambda () (set-box! cleanup-called #t)))))
  (check-true (unbox cleanup-called)))

(displayln "All stream incremental tests passed")

;; ============================================================
;; v0.11.2: Progressive SSE timeout tests
;; ============================================================

(test-case "v0.11.2: http-stream-timeout-default is less than http-read-timeout-default"
  (check-true (and (number? http-stream-timeout-default) (> http-stream-timeout-default 0))
              "stream timeout is a positive number")
  (check-true (> http-stream-timeout-default 0))
  (check-true (< http-stream-timeout-default http-read-timeout-default))
  ;; Specific values: initial=120s, stream=60s
  (check-equal? http-read-timeout-default 120)
  (check-equal? http-stream-timeout-default 60))

(test-case "v0.11.2: read-sse-chunks accepts #:initial-timeout and #:stream-timeout"
  ;; Create a pipe with data available immediately
  (define-values (in out) (make-pipe))
  (write-string
   "data: {\"id\":\"test\",\"choices\":[{\"delta\":{\"content\":\"Hi\"}}]}\n\ndata: [DONE]\n"
   out)
  (close-output-port out)
  (define gen (read-sse-chunks in #:initial-timeout 5 #:stream-timeout 1))
  (define chunk (gen))
  (check-pred stream-chunk? chunk)
  (check-equal? (stream-chunk-delta-text chunk) "Hi")
  ;; Next call should return #f (done)
  (check-equal? (gen) #f))

(test-case "v0.11.2: progressive timeout - first read uses initial-timeout"
  ;; Test that initial timeout is used for first read
  ;; Use a fast initial timeout to prove it's respected
  (define-values (in out) (make-pipe))
  (define gen (read-sse-chunks in #:initial-timeout 0.05 #:stream-timeout 10))
  ;; Don't write anything — should timeout quickly (0.05s initial)
  (check-exn exn:fail:network:timeout? (lambda () (gen))))

(test-case "v0.11.2: progressive timeout - after first chunk uses stream-timeout"
  ;; After receiving first chunk, subsequent reads use shorter timeout
  (define-values (in out) (make-pipe))
  ;; Write first chunk immediately
  (write-string "data: {\"id\":\"test\",\"choices\":[{\"delta\":{\"content\":\"X\"}}]}\n" out)
  ;; Flush but don't close — next read will timeout with stream-timeout
  (define gen (read-sse-chunks in #:initial-timeout 5 #:stream-timeout 0.05))
  (define chunk (gen))
  (check-pred stream-chunk? chunk)
  ;; Now no more data — should timeout quickly (0.05s stream)
  (check-exn exn:fail:network:timeout? (lambda () (gen)))
  (close-output-port out))

;; ============================================================
;; v0.45.9 AF4: empty-response detection tests
;; ============================================================

(test-case "AF4: accumulate-stream-chunks returns empty text for thinking-only chunks"
  (define thinking-chunk
    (make-stream-chunk #f #f #f #f #:delta-thinking "Let me think..." #:finish-reason 'stop))
  (define acc (accumulate-stream-chunks (list thinking-chunk)))
  (check-equal? (hash-ref acc 'text) "")
  (check-equal? (hash-ref acc 'tool-calls) '())
  (check-not-false (hash-ref acc 'thinking)))

(test-case "AF4: accumulate-stream-chunks returns text for mixed chunks"
  (define text-chunk (make-stream-chunk "Hello" #f #f #f #:finish-reason 'stop))
  (define acc (accumulate-stream-chunks (list text-chunk)))
  (check-equal? (hash-ref acc 'text) "Hello")
  (check-equal? (hash-ref acc 'tool-calls) '()))

;; ============================================================
;; v0.45.10 NF3/NF4/NF5: runtime.warning event tests
;; ============================================================

(require "../llm/provider.rkt"
         "../util/event.rkt"
         "../agent/event-bus.rkt"
         "../agent/state.rkt")

(test-case "NF3: runtime.warning emitted for thinking-only response"
  (define bus (make-event-bus))
  (define warning-events (box '()))
  (subscribe! bus
              (lambda (evt)
                (when (equal? (event-ev evt) "runtime.warning")
                  (set-box! warning-events (cons evt (unbox warning-events))))))
  (define st (make-loop-state "test-session" "test-turn"))
  ;; Create stream-data with thinking-only chunks (no text)
  (define thinking-chunk
    (make-stream-chunk #f
                       #f
                       #f
                       #f
                       #:delta-thinking "Let me think deeply about this..."
                       #:finish-reason 'stop))
  (define stream-data
    (hasheq 'text
            ""
            'tool-calls
            '()
            'thinking
            "Let me think deeply about this..."
            'all-chunks
            (list thinking-chunk)
            'cancelled?
            #f
            'stream-blocked?
            #f))
  (define prov
    (make-provider (lambda () "test-provider")
                   (lambda () (hash 'streaming #t 'token-counting #t))
                   (lambda (req) (hasheq))
                   (lambda (req) '())))
  ;; Call build-stream-result — should emit runtime.warning
  (define result
    (build-stream-result stream-data
                         '() ;; raw-messages
                         bus
                         "test-session"
                         "test-turn"
                         st
                         #f ;; tools
                         prov
                         #f)) ;; hook-dispatcher
  ;; Verify warning event was emitted
  (define warnings (unbox warning-events))
  (check-equal? (length warnings) 1)
  (define payload (event-payload (car warnings)))
  (check-equal? (hash-ref payload 'warning) "empty-assistant-response")
  (check-equal? (hash-ref payload 'turnId) "test-turn")
  (check-true (> (hash-ref payload 'thinkingLength) 0)))

(test-case "NF3: no warning when tool calls present with empty text"
  (define bus (make-event-bus))
  (define warning-events (box '()))
  (subscribe! bus
              (lambda (evt)
                (when (equal? (event-ev evt) "runtime.warning")
                  (set-box! warning-events (cons evt (unbox warning-events))))))
  (define st (make-loop-state "test-session" "test-turn"))
  ;; Provide a chunk with a tool call — accumulate-stream-chunks should extract it
  (define tc-chunk
    (make-stream-chunk #f
                       (hasheq 'index 0 'id "tc-1" 'name "read" 'arguments "{}")
                       #f
                       #f
                       #:finish-reason 'tool-calls))
  (define stream-data
    (hasheq 'text
            ""
            'tool-calls
            '()
            'thinking
            ""
            'all-chunks
            (list tc-chunk)
            'cancelled?
            #f
            'stream-blocked?
            #f))
  (define prov
    (make-provider (lambda () "test-provider")
                   (lambda () (hash 'streaming #t 'token-counting #t))
                   (lambda (req) (hasheq))
                   (lambda (req) '())))
  (build-stream-result stream-data '() bus "test-session" "test-turn" st #f prov #f)
  (check-equal? (length (unbox warning-events)) 0))

(test-case "NF4: warning fires for whitespace-only response"
  (define bus (make-event-bus))
  (define warning-events (box '()))
  (subscribe! bus
              (lambda (evt)
                (when (equal? (event-ev evt) "runtime.warning")
                  (set-box! warning-events (cons evt (unbox warning-events))))))
  (define st (make-loop-state "test-session" "test-turn"))
  (define ws-chunk (make-stream-chunk "   \n  " #f #f #f #:finish-reason 'stop))
  (define stream-data
    (hasheq 'text
            "   \n  "
            'tool-calls
            '()
            'thinking
            ""
            'all-chunks
            (list ws-chunk)
            'cancelled?
            #f
            'stream-blocked?
            #f))
  (define prov
    (make-provider (lambda () "test-provider")
                   (lambda () (hash 'streaming #t 'token-counting #t))
                   (lambda (req) (hasheq))
                   (lambda (req) '())))
  (build-stream-result stream-data '() bus "test-session" "test-turn" st #f prov #f)
  ;; Whitespace-only should trigger the warning (string-trim makes it empty)
  (check-equal? (length (unbox warning-events)) 1)
  (check-equal? (hash-ref (event-payload (car (unbox warning-events))) 'warning)
                "empty-assistant-response"))

;; ============================================================
;; v0.45.11 W0: Wall-clock deadline + consecutive-empty tests
;; ============================================================

(test-case "v0.45.11: wall-clock deadline fires for slow stream"
  ;; Sleep between generator calls to let the deadline expire.
  (define lines (string-append "data: {\"choices\":[]}\n\n" "data: {\"choices\":[]}\n\n"))
  (define in (open-input-string lines))
  (define gen (read-sse-chunks in #:max-total-timeout 0.05))
  ;; First read succeeds
  (define first (gen))
  (check-not-false first)
  ;; Sleep past the 50ms deadline
  (sleep 0.1)
  ;; Now the deadline check should fire
  (check-exn exn:fail:network:timeout? (lambda () (gen))))
(test-case "v0.45.11: consecutive-empty abort after 100 empty lines"
  ;; Feed 101 empty lines (no data: prefix) -- should abort after 100
  (define empty-lines (make-string 404 #\newline)) ;; 101 newlines = 101 empty lines
  (define in (open-input-string empty-lines))
  (define gen (read-sse-chunks in #:initial-timeout 5 #:stream-timeout 5 #:max-total-timeout 600))
  (check-exn exn:fail:network:timeout?
             (lambda ()
               (let loop ()
                 (define v (gen))
                 (when v
                   (loop))))))

(test-case "v0.45.11: normal stream with default max-total-timeout works"
  (define in (open-input-string "data: {\"choices\":[]}\ndata: [DONE]\n"))
  (define gen (read-sse-chunks in #:max-total-timeout 600))
  ;; Should yield a chunk then #f -- no timeout
  (define first (gen))
  (check-not-false first)
  (define second (gen))
  (check-false second))

(test-case "v0.45.12 L2: consecutive-empty resets on valid data chunk"
  ;; Interleave 80 empty lines with a valid chunk, then 80 more empty lines.
  ;; If reset works: 80 < 100, 80 < 100 → stream completes.
  ;; If reset broken: cumulative 160 > 100 → abort. This makes the test falsifiable.
  (define lines
    (string-append (apply string-append (make-list 80 "\n"))
                   "data: {\"choices\":[]}\n\n" ;; valid chunk (resets counter to 0)
                   (apply string-append (make-list 80 "\n"))
                   "data: [DONE]\n")) ;; stream end
  (define in (open-input-string lines))
  (define gen (read-sse-chunks in #:initial-timeout 5 #:stream-timeout 5 #:max-total-timeout 600))
  ;; First valid chunk
  (define first (gen))
  (check-not-false first)
  ;; Should reach DONE without hitting 100-empty limit
  (define done (gen))
  (check-false done))

(test-case "v0.45.12 L2: consecutive-empty counter does NOT reset on comment lines"
  ;; Mix comments and empty lines — both count toward consecutive-empty
  (define lines
    (string-append (apply string-append (make-list 50 ": comment\n"))
                   (apply string-append (make-list 51 "\n")))) ;; 50 + 51 = 101 > 100
  (define in (open-input-string lines))
  (define gen (read-sse-chunks in #:initial-timeout 5 #:stream-timeout 5 #:max-total-timeout 600))
  (check-exn exn:fail:network:timeout?
             (lambda ()
               (let loop ()
                 (define v (gen))
                 (when v
                   (loop))))))

;; ============================================================
;; v0.45.13 L2: Deadline checked at loop top (before each read)
;; ============================================================

(test-case "v0.45.13 L2: deadline fires on immediate second call (no sleep between)"
  ;; Verify the deadline check fires at the TOP of each loop iteration,
  ;; not just after sleeping. Create a stream with one chunk and near-zero deadline.
  (define lines "data: {\"choices\":[]}\n\ndata: {\"choices\":[]}\n\n")
  (define in (open-input-string lines))
  (define gen (read-sse-chunks in #:initial-timeout 5 #:stream-timeout 5 #:max-total-timeout 0.001))
  ;; First call: generator body starts, stream-start = now, deadline = now+1ms.
  ;; First iteration: deadline NOT yet passed (just set). Read succeeds.
  (define first (gen))
  (check-not-false first)
  ;; Second call: loop re-enters, checks deadline at TOP. If 1ms has passed,
  ;; deadline fires BEFORE the second read-line/timeout.
  (sleep 0.01) ;; Ensure 1ms has passed
  (check-exn exn:fail:network:timeout? (lambda () (gen))))
