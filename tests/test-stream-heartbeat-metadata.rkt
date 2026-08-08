#lang racket

;; @speed fast
;; @suite default
;; BOUNDARY: integration
;;
;; tests/test-stream-heartbeat-metadata.rkt
;; v0.99.81 W1: SSE heartbeat/data/phase metadata on stream timeout.
;;
;; Verifies that timeout exceptions from stream-sse-events
;; carry truthful metadata about what the stream received before stalling:
;;   - received-heartbeats?  : did the peer send SSE comment (: ) lines?
;;   - received-any-data?    : did the peer send any data: chunk at all?
;;   - phase                 : 'initial | 'thinking | 'content

(require rackunit
         racket/port
         racket/generator
         "../llm/stream.rkt"
         "../llm/openai-compatible.rkt"
         "../llm/model.rkt")

;; Helper: run stream-sse-events against a pipe pre-loaded with lines.
;; The pipe output port stays open so the final read blocks → timeout.
;; Returns either the timeout exception or the list of yielded chunks.
(define (run-stream lines
                    #:initial-timeout [initial 0.4]
                    #:stream-timeout [stream 0.4]
                    #:max-total-timeout [max-total 5])
  (define-values (in out) (make-pipe))
  (for ([line (in-list lines)])
    (displayln line out))
  ;; Keep out open so reads block instead of returning EOF
  (define gen
    (stream-sse-events in
                       (lambda (parsed) (list (normalize-openai-chunk parsed)))
                       #:initial-timeout initial
                       #:stream-timeout stream
                       #:max-total-timeout max-total))
  (begin0 (with-handlers ([exn:fail:network:timeout? (lambda (e) e)])
            (let loop ([acc '()])
              (define chunk (gen))
              (if chunk
                  (loop (cons chunk acc))
                  (reverse acc))))
    (with-handlers ([exn:fail? (lambda (_) (void))])
      (close-output-port out))
    (with-handlers ([exn:fail? (lambda (_) (void))])
      (close-input-port in))))

;; Same as run-stream but uses stream-sse-events with OpenAI normalization.
(define (run-stream-sse events-raw
                        #:initial-timeout [initial 0.4]
                        #:stream-timeout [stream 0.4]
                        #:event->chunks
                        [event->chunks (lambda (evt) (list (normalize-openai-chunk evt)))])
  (define-values (in out) (make-pipe))
  (for ([line (in-list events-raw)])
    (displayln line out))
  (define gen
    (stream-sse-events in
                       event->chunks
                       #:initial-timeout initial
                       #:stream-timeout stream
                       #:max-total-timeout 5))
  (begin0 (with-handlers ([exn:fail:network:timeout? (lambda (e) e)])
            (let loop ([acc '()])
              (define chunk (gen))
              (if chunk
                  (loop (cons chunk acc))
                  (reverse acc))))
    (with-handlers ([exn:fail? (lambda (_) (void))])
      (close-output-port out))
    (with-handlers ([exn:fail? (lambda (_) (void))])
      (close-input-port in))))

;; ── PN-2 Red: heartbeat metadata ──

(test-case "heartbeat: timeout after keep-alive comments sets received-heartbeats?=#t"
  (define result (run-stream '(": keep-alive" ": ping")))
  (check-pred exn:fail:network:timeout:stream? result)
  (when (exn:fail:network:timeout:stream? result)
    (check-true (exn:fail:network:timeout:stream-received-heartbeats? result)
                "heartbeats were seen before timeout")
    (check-false (exn:fail:network:timeout:stream-received-any-data? result)
                 "no data chunks were received")
    (check-equal? (exn:fail:network:timeout:stream-phase result) 'initial)))

(test-case "heartbeat: timeout with no comments sets received-heartbeats?=#f"
  (define result (run-stream '("" "")))
  (check-pred exn:fail:network:timeout:stream? result)
  (when (exn:fail:network:timeout:stream? result)
    (check-false (exn:fail:network:timeout:stream-received-heartbeats? result)
                 "no comment/heartbeat lines were seen")))

;; ── PN-2 Red: data semantics ──

(test-case "data: any yielded data chunk sets received-any-data?=#t"
  (define lines (list "data: {\"id\":\"x\",\"choices\":[{\"delta\":{\"content\":\"\"}}]}"))
  (define result (run-stream lines))
  (check-pred exn:fail:network:timeout:stream? result)
  (when (exn:fail:network:timeout:stream? result)
    (check-true (exn:fail:network:timeout:stream-received-any-data? result)
                "a data chunk was yielded (even with empty content)")))

(test-case "data: text content chunk sets received-any-data?=#t"
  (define lines (list "data: {\"id\":\"x\",\"choices\":[{\"delta\":{\"content\":\"Hi\"}}]}"))
  (define result (run-stream lines))
  (check-pred exn:fail:network:timeout:stream? result)
  (when (exn:fail:network:timeout:stream? result)
    (check-true (exn:fail:network:timeout:stream-received-any-data? result))))

;; ── PN-2 Red: phase semantics ──

(test-case "phase: 'initial when only comments seen"
  (define result (run-stream '(": keep-alive")))
  (when (exn:fail:network:timeout:stream? result)
    (check-equal? (exn:fail:network:timeout:stream-phase result) 'initial)))

(test-case "phase: 'content after first text data chunk"
  (define lines (list "data: {\"id\":\"x\",\"choices\":[{\"delta\":{\"content\":\"Hi\"}}]}"))
  (define result (run-stream lines))
  (when (exn:fail:network:timeout:stream? result)
    (check-equal? (exn:fail:network:timeout:stream-phase result) 'content)))

(test-case "phase: 'thinking after data chunk with no text content"
  ;; Empty-content delta: data was received but no text → thinking phase
  (define lines (list "data: {\"id\":\"x\",\"choices\":[{\"delta\":{\"content\":\"\"}}]}"))
  (define result (run-stream lines))
  (when (exn:fail:network:timeout:stream? result)
    (check-equal? (exn:fail:network:timeout:stream-phase result) 'thinking)))

;; ── PN-2 Red: flood protection ──

(test-case "flood: heartbeats count toward consecutive-empty limit"
  ;; 105 heartbeat lines: flood guard fires at 100, not at infinity
  (define lines
    (for/list ([_ (in-range 105)])
      ": keep-alive"))
  (define result (run-stream lines #:initial-timeout 2 #:stream-timeout 2))
  (check-pred exn:fail:network:timeout? result)
  (when (exn:fail:network:timeout? result)
    (check-true (string-contains? (exn-message result) "consecutive empty")
                "flood guard must fire on heartbeat flood, not silently wait")
    (when (exn:fail:network:timeout:stream? result)
      (check-true (exn:fail:network:timeout:stream-received-heartbeats? result)))))

;; ── stream-sse-events metadata (provider-agnostic path) ──

(test-case "stream-sse-events: heartbeat metadata on timeout"
  (define result (run-stream-sse '(": keep-alive")))
  (check-pred exn:fail:network:timeout:stream? result)
  (when (exn:fail:network:timeout:stream? result)
    (check-true (exn:fail:network:timeout:stream-received-heartbeats? result))
    (check-false (exn:fail:network:timeout:stream-received-any-data? result))
    (check-equal? (exn:fail:network:timeout:stream-phase result) 'initial)))

(test-case "stream-sse-events: data + phase metadata on timeout"
  (define events (list "data: {\"id\":\"x\",\"choices\":[{\"delta\":{\"content\":\"Hello\"}}]}"))
  (define result (run-stream-sse events))
  (check-pred exn:fail:network:timeout:stream? result)
  (when (exn:fail:network:timeout:stream? result)
    (check-true (exn:fail:network:timeout:stream-received-any-data? result))
    (check-equal? (exn:fail:network:timeout:stream-phase result) 'content)))

;; ── PN-2: empty event->chunks must NOT set received-any-data? ──

(test-case "stream-sse-events: ping event with empty chunks keeps received-any-data?=#f"
  ;; An event that event->chunks maps to '() (e.g. Anthropic ping, message_start)
  ;; must not be counted as data — no chunk was yielded.
  (define events (list "data: {\"type\":\"ping\"}"))
  (define result (run-stream-sse events #:event->chunks (lambda (evt) '())))
  (check-pred exn:fail:network:timeout:stream? result)
  (when (exn:fail:network:timeout:stream? result)
    (check-false (exn:fail:network:timeout:stream-received-any-data? result)
                 "empty-chunks event must not count as data")
    (check-equal? (exn:fail:network:timeout:stream-phase result) 'initial)))

;; ── backward compatibility ──

(test-case "exn:fail:network:timeout:stream is also exn:fail:network:timeout?"
  (define result (run-stream '(": keep-alive")))
  (check-pred exn:fail:network:timeout? result)
  (check-pred exn:fail? result))

(test-case "plain exn:fail:network:timeout still works for request-level timeouts"
  ;; call-with-request-timeout raises plain exn:fail:network:timeout (no stream metadata)
  (define e
    (with-handlers ([exn:fail:network:timeout? (lambda (e) e)])
      (call-with-request-timeout (lambda () (sync/timeout 10 never-evt)) #:timeout 0.2)))
  (check-pred exn:fail:network:timeout? e)
  (check-false (exn:fail:network:timeout:stream? e) "request-level timeout is not a stream timeout"))
