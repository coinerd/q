#lang racket/base

;; @speed fast
;; @suite default

;; test-loop-stream-pure.rkt — T2-6: Pure chunk classification tests
;; Tests classify-chunk and chunk-has-data? without mocks or providers.

(require rackunit
         racket/match
         "../llm/model.rkt"
         "../agent/loop-stream.rkt"
         "../agent/streaming-message.rkt")

;; Helper: make a stream-chunk with specific fields
(define (make-test-chunk #:delta-text [dt #f]
                         #:delta-thinking [dth #f]
                         #:delta-tool-call [tc #f]
                         #:done? [done #f]
                         #:usage [usage #f]
                         #:finish-reason [fr #f])
  (make-stream-chunk dt tc usage done #:delta-thinking dth #:finish-reason fr))

;; ============================================================
;; classify-chunk tests
;; ============================================================

(test-case "classify-chunk: text delta"
  (define chunk (make-test-chunk #:delta-text "hello"))
  (define result (classify-chunk chunk))
  (check-equal? (length result) 1)
  (check-equal? (car result) (cons 'text-delta "hello")))

(test-case "classify-chunk: thinking delta"
  (define chunk (make-test-chunk #:delta-thinking "reasoning..."))
  (define result (classify-chunk chunk))
  (check-equal? (length result) 1)
  (check-equal? (car result) (cons 'thinking-delta "reasoning...")))

(test-case "classify-chunk: tool-call delta"
  (define tc (hasheq 'id "tc-1" 'name "read" 'arguments "{}"))
  (define chunk (make-test-chunk #:delta-tool-call tc))
  (define result (classify-chunk chunk))
  (check-equal? (length result) 1)
  (check-equal? (caar result) 'tool-call-delta)
  (check-equal? (cdar result) tc))

(test-case "classify-chunk: done chunk"
  (define usage (hasheq 'prompt-tokens 10 'completion-tokens 5))
  (define chunk (make-test-chunk #:done? #t #:usage usage #:finish-reason "stop"))
  (define result (classify-chunk chunk))
  (check-not-false (assoc 'done result))
  (define done-data (cdr (assoc 'done result)))
  (check-equal? (hash-ref done-data 'usage) usage)
  (check-equal? (hash-ref done-data 'finish-reason) "stop"))

(test-case "classify-chunk: empty chunk returns '()"
  (define chunk (make-test-chunk))
  (check-equal? (classify-chunk chunk) '()))

(test-case "classify-chunk: multi-type chunk (text + thinking)"
  (define chunk (make-test-chunk #:delta-text "hi" #:delta-thinking "hmm"))
  (define result (classify-chunk chunk))
  (check-equal? (length result) 2)
  (check-equal? (car result) (cons 'text-delta "hi"))
  (check-equal? (cadr result) (cons 'thinking-delta "hmm")))

(test-case "classify-chunk: text + done combined"
  (define usage (hasheq 'total-tokens 42))
  (define chunk (make-test-chunk #:delta-text "final" #:done? #t #:usage usage))
  (define result (classify-chunk chunk))
  (check-equal? (length result) 2)
  (check-not-false (assoc 'text-delta result))
  (check-not-false (assoc 'done result)))

;; ============================================================
;; chunk-has-data? tests
;; ============================================================

(test-case "chunk-has-data?: #t for text chunk"
  (check-true (chunk-has-data? (make-test-chunk #:delta-text "x"))))

(test-case "chunk-has-data?: #t for done chunk"
  (check-true (chunk-has-data? (make-test-chunk #:done? #t))))

(test-case "chunk-has-data?: #f for empty chunk"
  (check-false (chunk-has-data? (make-test-chunk))))

(test-case "chunk-has-data?: #t for thinking chunk"
  (check-true (chunk-has-data? (make-test-chunk #:delta-thinking "..."))))

(test-case "chunk-has-data?: #t for tool-call chunk"
  (check-true (chunk-has-data? (make-test-chunk #:delta-tool-call (hasheq)))))

;; ============================================================
;; Streaming message FSM state tests (T2-7)
;; ============================================================

(test-case "streaming-message: initial FSM state is not-started"
  (define sm (make-streaming-message "msg-test"))
  (check-equal? (streaming-message-fsm-state sm) 'not-started)
  (check-false (streaming-message-message-started? sm))
  (check-false (streaming-message-cancelled? sm))
  (check-false (streaming-message-blocked? sm)))

(test-case "streaming-message: set-message-started! transitions to streaming"
  (define sm (make-streaming-message "msg-test"))
  (streaming-message-set-message-started! sm)
  (check-equal? (streaming-message-fsm-state sm) 'streaming)
  (check-true (streaming-message-message-started? sm)))

(test-case "streaming-message: set-cancelled! transitions to cancelled"
  (define sm (make-streaming-message "msg-test"))
  (streaming-message-set-cancelled! sm)
  (check-equal? (streaming-message-fsm-state sm) 'cancelled)
  (check-true (streaming-message-cancelled? sm)))

(test-case "streaming-message: set-blocked! transitions to blocked"
  (define sm (make-streaming-message "msg-test"))
  (streaming-message-set-blocked! sm)
  (check-equal? (streaming-message-fsm-state sm) 'blocked)
  (check-true (streaming-message-blocked? sm)))

(test-case "streaming-message: message-started? true in all non-initial states"
  (define sm (make-streaming-message "msg-test"))
  (streaming-message-set-message-started! sm)
  (check-true (streaming-message-message-started? sm))
  ;; After blocking, still "started"
  (streaming-message-set-blocked! sm)
  (check-true (streaming-message-message-started? sm)))

(test-case "streaming-message: accumulate text"
  (define sm (make-streaming-message "msg-test"))
  (streaming-message-append-text! sm "hello ")
  (streaming-message-append-text! sm "world")
  (check-equal? (streaming-message-text sm) "hello world"))

(test-case "streaming-message: accumulate tool calls"
  (define sm (make-streaming-message "msg-test"))
  (define tc (hasheq 'id "tc-1" 'name "read"))
  (streaming-message-append-tool-call! sm tc)
  (check-equal? (length (streaming-message-get-tool-calls sm)) 1)
  (check-equal? (streaming-message-get-tool-calls sm) (list tc)))

(test-case "streaming-message: ->hash includes FSM state data"
  (define sm (make-streaming-message "msg-test"))
  (streaming-message-set-message-started! sm)
  (define h (streaming-message->hash sm))
  (check-equal? (hash-ref h 'cancelled?) #f)
  (check-equal? (hash-ref h 'stream-blocked?) #f))
