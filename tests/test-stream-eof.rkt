#lang racket

;; test-stream-eof.rkt — Tests for silent stream EOF detection (BUG-SILENT-STREAM-EOF)
;;
;; When the API closes the connection without sending a finish_reason chunk,
;; stream-from-provider should emit a synthetic model.stream.completed with
;; finish_reason "eof" and truncated? #t.

(require rackunit
         rackunit/text-ui
         racket/generator
         "../llm/model.rkt"
         "../llm/provider.rkt"
         "../agent/event-bus.rkt"
         "../agent/state.rkt"
         "../agent/loop.rkt"
         "../util/protocol-types.rkt")

;; ============================================================
;; Test helpers
;; ============================================================

;; Create a mock provider whose stream yields given chunks, then optionally #f
;; to simulate EOF without a done chunk (silent EOF), or with a done chunk (normal).
(define (make-eof-mock-provider chunks)
  (make-provider
   (lambda () "eof-test")
   (lambda () (hasheq 'streaming #t))
   (lambda (req) (error "not used"))
   (lambda (req)
     ;; Return a generator that yields each chunk then #f
     (generator ()
       (for ([ch (in-list chunks)])
         (yield ch))
       (yield #f)))))

;; Create a minimal loop-state for testing
(define (test-state)
  (make-loop-state "test-session" "test-turn"))

;; Capture events from the bus matching event-name
;; Returns a box of event payloads (not the full event struct)
(define (capture-events bus event-name)
  (define captured (box '()))
  (subscribe! bus
              (lambda (evt)
                (when (equal? (event-event evt) event-name)
                  (set-box! captured (append (unbox captured) (list (event-payload evt)))))))
  captured)

;; ============================================================
;; Test suite
;; ============================================================

(define stream-eof-tests
  (test-suite
   "Silent Stream EOF Detection (BUG-SILENT-STREAM-EOF)"

   ;; ── Case 1: Silent EOF — text chunks then #f, no done chunk ──
   (test-case "silent EOF emits synthetic model.stream.completed with finish_reason eof"
     (define bus (make-event-bus))
     (define captured (capture-events bus "model.stream.completed"))
     ;; 3 text chunks, no done chunk — generator yields #f after text
     (define chunks
       (list (make-stream-chunk "Hello " #f (hasheq) #f)
             (make-stream-chunk "World" #f (hasheq) #f)
             (make-stream-chunk "!" #f (hasheq) #f)))
     (define mock-prov (make-eof-mock-provider chunks))
     (define req (make-model-request '() #f (hasheq)))
     (define result (stream-from-provider mock-prov req bus "test-session" "turn-1" (test-state) #f #f))
     ;; Check the result has text
     (check-equal? (hash-ref result 'text) "Hello World!")
     ;; Check synthetic completion was emitted
     (sync-events bus)
     (define completions (unbox captured))
     (check = (length completions) 1 "expected exactly 1 model.stream.completed event")
     (when (>= (length completions) 1)
       (define comp (car completions))
       (check-equal? (hash-ref comp 'finish_reason #f) "eof")
       (check-true (hash-ref comp 'truncated? #f)
                   "truncated? should be #t for silent EOF")))

   ;; ── Case 2: Normal completion — done chunk present ──
   (test-case "normal completion does NOT emit synthetic model.stream.completed"
     (define bus (make-event-bus))
     (define captured (capture-events bus "model.stream.completed"))
     ;; Text chunk followed by done chunk
     (define chunks
       (list (make-stream-chunk "Hello" #f (hasheq) #f)
             (make-stream-chunk #f #f (hasheq 'prompt_tokens 10 'completion_tokens 5) #t
                                #:finish-reason "stop")))
     (define mock-prov (make-eof-mock-provider chunks))
     (define req (make-model-request '() #f (hasheq)))
     (define result (stream-from-provider mock-prov req bus "test-session" "turn-2" (test-state) #f #f))
     (sync-events bus)
     (define completions (unbox captured))
     ;; Should have exactly 1 from the normal done chunk, NOT 2
     (check = (length completions) 1 "normal path: exactly 1 completion event")
     (when (>= (length completions) 1)
       (define comp (car completions))
       (check-equal? (hash-ref comp 'finish_reason #f) "stop")
       ;; Normal completion should NOT have truncated?
       (check-false (hash-ref comp 'truncated? #f)
                    "normal completion should NOT have truncated?")))

   ;; ── Case 3: Empty stream — generator yields #f immediately ──
   (test-case "empty stream yields #f immediately — no synthetic event"
     (define bus (make-event-bus))
     (define captured (capture-events bus "model.stream.completed"))
     ;; No chunks at all
     (define mock-prov (make-eof-mock-provider '()))
     (define req (make-model-request '() #f (hasheq)))
     (define result (stream-from-provider mock-prov req bus "test-session" "turn-3" (test-state) #f #f))
     (sync-events bus)
     (define completions (unbox captured))
     ;; Empty stream: no message.start was emitted, so no synthetic event needed
     (check = (length completions) 0 "empty stream: no completion events"))

   ;; ── Case 4: Silent EOF emits message.end if message was started ──
   (test-case "silent EOF emits message.end when message was started"
     (define bus (make-event-bus))
     (define captured (capture-events bus "message.end"))
     ;; Text chunk triggers message.start, then EOF
     (define chunks
       (list (make-stream-chunk "Partial text" #f (hasheq) #f)))
     (define mock-prov (make-eof-mock-provider chunks))
     (define req (make-model-request '() #f (hasheq)))
     (define result (stream-from-provider mock-prov req bus "test-session" "turn-4" (test-state) #f #f))
     (sync-events bus)
     (define ends (unbox captured))
     ;; Should emit exactly 1 message.end (from synthetic EOF)
     (check >= (length ends) 1 "silent EOF should emit message.end when message was started"))

   ))

;; Helper to let async events propagate (event-bus is synchronous but
;; this ensures any deferred subscriptions fire)
(define (sync-events bus)
  ;; event-bus is synchronous in this implementation, so just a no-op
  (void))

;; Run
(run-tests stream-eof-tests)
