#lang racket

;; tests/test-trace-events.rkt — v0.15.0 Wave 1
;;
;; Tests that new/enriched events are emitted correctly.

(require rackunit
         racket/list
         racket/generator
         "../agent/event-bus.rkt"
         "../agent/state.rkt"
         "../agent/loop.rkt"
         "../llm/model.rkt"
         "../llm/stream.rkt"
         "../llm/provider.rkt"
         "../util/protocol-types.rkt"
         "../util/ids.rkt")

;; Helper: create event collector
(define (make-event-collector bus [filter-name #f])
  (define events '())
  (subscribe! bus
              (lambda (evt)
                (set! events (append events (list evt)))
                evt)
              #:filter (if filter-name
                           (lambda (evt) (equal? (event-ev evt) filter-name))
                           #f))
  (lambda () events))

(define (make-user-message text)
  (make-message (generate-id) #f 'user 'text (list (make-text-part text)) (now-seconds) (hasheq)))

;; Helper: make mock provider with specific chunks (including finish-reason)
(define (make-provider-with-chunks chunks name)
  (make-provider (lambda () name)
                 (lambda () (hasheq 'streaming #t))
                 (lambda (req) (error "not implemented"))
                 (lambda (req)
                   ;; Return list of chunks
                   chunks)))

;; ============================================================
;; stream-chunk finish-reason field
;; ============================================================

(test-case "stream-chunk stores finish-reason string"
  (define chunk (stream-chunk "hello" #f #f (hasheq) #t "stop"))
  (check-equal? (stream-chunk-finish-reason chunk) "stop"))

(test-case "stream-chunk finish-reason can be #f"
  (define chunk (stream-chunk "hi" #f #f (hasheq) #f #f))
  (check-false (stream-chunk-finish-reason chunk)))

(test-case "make-stream-chunk defaults finish-reason to #f"
  (define chunk (make-stream-chunk "text" #f (hasheq) #t))
  (check-false (stream-chunk-finish-reason chunk)))

;; ============================================================
;; model.stream.completed includes finish_reason
;; ============================================================

(test-case "model.stream.completed includes finish_reason from chunk"
  (define bus (make-event-bus))
  (define get-events (make-event-collector bus "model.stream.completed"))

  (define chunks
    (list (stream-chunk "hello" #f #f (hasheq) #f #f)
          (stream-chunk ""
                        #f
                        #f
                        (hasheq 'prompt_tokens 10 'completion_tokens 5 'total_tokens 15)
                        #t
                        "stop")))

  (define prov (make-provider-with-chunks chunks "test-model"))
  (define req (make-model-request (list (hasheq 'role "user" 'content "hi")) #f (hasheq)))
  (define state (make-loop-state "s1" "t1"))

  (define result (stream-from-provider prov req bus "s1" "t1" state #f #f))
  (define evts (get-events))
  (check >= (length evts) 1)
  (define payload (event-payload (car evts)))
  (check-equal? (hash-ref payload 'finish_reason #f) "stop"))

(test-case "model.stream.completed with 'length finish_reason"
  (define bus (make-event-bus))
  (define get-events (make-event-collector bus "model.stream.completed"))

  (define chunks
    (list (stream-chunk "text" #f #f (hasheq) #f #f) (stream-chunk "" #f #f (hasheq) #t "length")))

  (define prov (make-provider-with-chunks chunks "test-model"))
  (define req (make-model-request (list (hasheq 'role "user" 'content "hi")) #f (hasheq)))
  (define state (make-loop-state "s1" "t1"))

  (stream-from-provider prov req bus "s1" "t1" state #f #f)
  (define evts (get-events))
  (check >= (length evts) 1)
  (define payload (event-payload (car evts)))
  (check-equal? (hash-ref payload 'finish_reason #f) "length"))

(test-case "model.stream.completed with no finish_reason yields 'unknown'"
  (define bus (make-event-bus))
  (define get-events (make-event-collector bus "model.stream.completed"))

  ;; Chunk with done?=#t but finish-reason=#f
  (define chunks (list (stream-chunk "text" #f #f (hasheq) #t #f)))

  (define prov (make-provider-with-chunks chunks "test-model"))
  (define req (make-model-request (list (hasheq 'role "user" 'content "hi")) #f (hasheq)))
  (define state (make-loop-state "s1" "t1"))

  (stream-from-provider prov req bus "s1" "t1" state #f #f)
  (define evts (get-events))
  (check >= (length evts) 1)
  (define payload (event-payload (car evts)))
  (check-equal? (hash-ref payload 'finish_reason #f) "unknown"))

;; ============================================================
;; model.request.start includes model and max_tokens
;; ============================================================

(test-case "model.request.start enriched with model and settings"
  (define bus (make-event-bus))
  (define get-events (make-event-collector bus "model.request.started"))

  (define chunks (list (stream-chunk "hi" #f #f (hasheq) #t "stop")))

  (define prov (make-provider-with-chunks chunks "test-model"))
  (define settings (hasheq 'max-tokens 16384))

  (run-agent-turn (list (make-user-message "hello"))
                  prov
                  bus
                  #:session-id "s1"
                  #:turn-id "t1"
                  #:provider-settings settings)

  (define evts (get-events))
  (check >= (length evts) 1)
  (define payload (event-payload (car evts)))
  (check-equal? (hash-ref payload 'max_tokens #f) 16384)
  ;; v0.15.1: model field now uses (format "~a" (object-name provider))
  ;; which produces a string, not a symbol, for safe JSON serialization
  (check-equal? (hash-ref payload 'model #f) "provider"))

(test-case "model.request.start without provider-settings has #f max_tokens"
  (define bus (make-event-bus))
  (define get-events (make-event-collector bus "model.request.started"))

  (define chunks (list (stream-chunk "hi" #f #f (hasheq) #t "stop")))

  (define prov (make-provider-with-chunks chunks "mock"))

  (run-agent-turn (list (make-user-message "hello")) prov bus #:session-id "s1" #:turn-id "t1")

  (define evts (get-events))
  (check >= (length evts) 1)
  (define payload (event-payload (car evts)))
  (check-false (hash-ref payload 'max_tokens #f)))
