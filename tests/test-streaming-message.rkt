#lang racket

;; tests/test-streaming-message.rkt — FEAT-71: Structured streaming accumulator

(require rackunit
         rackunit/text-ui
         "../agent/streaming-message.rkt"
         "../util/protocol-types.rkt")

(define streaming-tests
  (test-suite "streaming-message accumulator"

    ;; ============================================================
    ;; Construction
    ;; ============================================================
    (test-case "make-streaming-message creates empty accumulator"
      (define sm (make-streaming-message "msg-123"))
      (check-equal? (streaming-message-message-id sm) "msg-123")
      (check-equal? (streaming-message-text sm) "")
      (check-equal? (streaming-message-tool-calls sm) '())
      (check-equal? (streaming-message-chunks sm) '())
      (check-false (streaming-message-cancelled? sm))
      (check-false (streaming-message-blocked? sm))
      (check-false (streaming-message-message-started? sm)))

    ;; ============================================================
    ;; Text accumulation
    ;; ============================================================
    (test-case "append-text accumulates across calls"
      (define sm (make-streaming-message "msg-1"))
      (streaming-message-append-text! sm "Hello ")
      (streaming-message-append-text! sm "World")
      (check-equal? (streaming-message-text sm) "Hello World"))

    (test-case "append-text from empty starts with first text"
      (define sm (make-streaming-message "msg-2"))
      (streaming-message-append-text! sm "first")
      (check-equal? (streaming-message-text sm) "first"))

    ;; ============================================================
    ;; Tool call accumulation
    ;; ============================================================
    (test-case "append-tool-call accumulates tool calls"
      (define sm (make-streaming-message "msg-3"))
      (streaming-message-append-tool-call! sm (hasheq 'name "bash" 'arguments (hasheq)))
      (streaming-message-append-tool-call! sm (hasheq 'name "read" 'arguments (hasheq)))
      (define tcs (streaming-message-tool-calls sm))
      (check-equal? (length tcs) 2)
      (check-equal? (hash-ref (car tcs) 'name) "bash")
      (check-equal? (hash-ref (cadr tcs) 'name) "read"))

    ;; ============================================================
    ;; Chunk accumulation
    ;; ============================================================
    (test-case "append-chunk accumulates in reverse order (cons)"
      (define sm (make-streaming-message "msg-4"))
      (streaming-message-append-chunk! sm 'chunk-1)
      (streaming-message-append-chunk! sm 'chunk-2)
      (define chunks (streaming-message-chunks sm))
      ;; cons order: latest first
      (check-equal? chunks '(chunk-2 chunk-1)))

    ;; ============================================================
    ;; State flags
    ;; ============================================================
    (test-case "cancelled flag starts #f, set to #t"
      (define sm (make-streaming-message "msg-5"))
      (check-false (streaming-message-cancelled? sm))
      (streaming-message-set-cancelled! sm)
      (check-true (streaming-message-cancelled? sm)))

    (test-case "blocked flag starts #f, set to #t"
      (define sm (make-streaming-message "msg-6"))
      (check-false (streaming-message-blocked? sm))
      (streaming-message-set-blocked! sm)
      (check-true (streaming-message-blocked? sm)))

    (test-case "message-started flag starts #f, set to #t"
      (define sm (make-streaming-message "msg-7"))
      (check-false (streaming-message-message-started? sm))
      (streaming-message-set-message-started! sm)
      (check-true (streaming-message-message-started? sm)))

    ;; ============================================================
    ;; Finalization -> message
    ;; ============================================================
    (test-case "finalize with text produces message with text-part"
      (define sm (make-streaming-message "msg-8"))
      (streaming-message-append-text! sm "Hello")
      (define msg (streaming-message-finalize sm))
      (check-equal? (message-id msg) "msg-8")
      (check-equal? (message-role msg) 'assistant)
      (define content (message-content msg))
      (check > (length content) 0)
      (check-equal? (text-part-text (car content)) "Hello"))

    (test-case "finalize with tool calls produces message with tool-call-parts"
      (define sm (make-streaming-message "msg-9"))
      (streaming-message-append-tool-call!
       sm
       (hasheq 'id "tc-1" 'name "bash" 'arguments (hasheq 'cmd "ls")))
      (define msg (streaming-message-finalize sm))
      (define content (message-content msg))
      (check >= (length content) 1)
      (define tc-part (car (filter tool-call-part? content)))
      (check-equal? (tool-call-part-name tc-part) "bash"))

    (test-case "finalize with empty text and no tool calls produces empty content"
      (define sm (make-streaming-message "msg-10"))
      (define msg (streaming-message-finalize sm))
      (check-equal? (message-content msg) '()))

    ;; ============================================================
    ;; ->hash backward compat
    ;; ============================================================
    (test-case "->hash produces expected keys"
      (define sm (make-streaming-message "msg-11"))
      (streaming-message-append-text! sm "text")
      (streaming-message-set-cancelled! sm)
      (define h (streaming-message->hash sm))
      (check-equal? (hash-ref h 'text) "text")
      (check-equal? (hash-ref h 'tool-calls) '())
      (check-true (hash-ref h 'cancelled?))
      (check-false (hash-ref h 'stream-blocked?)))))

(run-tests streaming-tests)
