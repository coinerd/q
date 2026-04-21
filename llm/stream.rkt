#lang racket/base

;; llm/stream.rkt — streamed chunk normalization
;;
;; Parses raw SSE text into intermediate jsexprs, then normalizes
;; provider-specific chunks (OpenAI format) into canonical stream-chunk
;; structs. Includes tool-call delta accumulation for partial JSON assembly.

(require racket/contract
         racket/string
         racket/generator
         racket/hash
         json
         "model.rkt"
         racket/port
         racket/match)

;; — SSE line-level helpers —
(provide parse-sse-lines
         parse-sse-line
         parse-sse-data-line
         sse-done?
         ;; — OpenAI chunk normalization —
         normalize-openai-chunks
         normalize-openai-chunk
         accumulate-tool-call-deltas
         ;; — Incremental SSE reading —
         read-sse-chunks
         ;; — Response body reading (bounded) —
         read-response-body
         read-response-body/timeout
         max-response-size
         ;; — Timeout infrastructure —
         read-line/timeout
         exn:fail:network:timeout
         exn:fail:network:timeout?
         http-read-timeout-default
         http-stream-timeout-default
         http-request-timeout-default
         current-http-request-timeout
         call-with-request-timeout)

;; ============================================================
;; Timeout configuration
;; ============================================================

;; Default HTTP read timeout in seconds.
;; When the network drops mid-stream, reads will timeout after this many seconds.
(define http-read-timeout-default 120)

;; Default overall HTTP request timeout in seconds.
;; Covers connection + full response reading.  Settable via settings.
(define http-request-timeout-default 600)

;; Parameter: overall HTTP request timeout for the current session.
;; Set by the runtime from settings; read by LLM providers.
(define current-http-request-timeout (make-parameter http-request-timeout-default))

;; call-with-request-timeout : thunk [#:timeout seconds #:cleanup thunk] -> any
;; Runs thunk in a separate thread with a channel for results;
;; kills the thread and raises exn:fail:network:timeout if the
;; overall timeout is exceeded.  Used by LLM providers to wrap
;; blocking http-sendrecv + body reads.
;; When #:cleanup is provided, it is called on timeout (e.g. to close ports).
(define (call-with-request-timeout thunk
                                   #:timeout [timeout-secs (current-http-request-timeout)]
                                   #:cleanup [cleanup-thunk (lambda () (void))])
  (define ch (make-channel))
  (define th
    (thread (lambda ()
              (with-handlers ([exn:fail? (lambda (e) (channel-put ch (cons 'exn e)))])
                (channel-put ch (cons 'val (thunk)))))))
  (define result (sync/timeout timeout-secs ch))
  (cond
    [(eq? result #f)
     (kill-thread th)
     (with-handlers ([exn:fail? void])
       (cleanup-thunk)) ; #454: close ports
     (raise (exn:fail:network:timeout (format "HTTP request timeout (~a seconds)" timeout-secs)
                                      (current-continuation-marks)))]
    [else
     (define tag (car result))
     (define payload (cdr result))
     (cond
       [(eq? tag 'exn) (raise payload)]
       [else payload])]))

;; Exception type for network read timeouts.
(struct exn:fail:network:timeout exn:fail () #:transparent)

;; ============================================================
;; Timeout-aware read helpers
;; ============================================================

;; read-line/timeout : input-port? [#:timeout seconds] -> (or/c string? eof?)
;; Like read-line but with a timeout. Returns #f on timeout (caller should raise).
(define (read-line/timeout port #:timeout [timeout-secs http-read-timeout-default])
  (define result (sync/timeout timeout-secs (read-line-evt port 'any)))
  (cond
    [(eq? result #f) #f] ; timeout
    [else result])) ; string or eof

;; read-response-body/timeout : input-port? [#:timeout seconds] -> bytes?
;; Like read-response-body but with a per-chunk read timeout.
;; Raises exn:fail:network:timeout on timeout.
(define (read-response-body/timeout port #:timeout [timeout-secs http-read-timeout-default])
  (define out (open-output-bytes))
  (define buf (make-bytes 8192))
  (define deadline (+ (current-inexact-milliseconds) (* timeout-secs 1000.0)))
  (let loop ([total 0])
    (define remaining (/ (- deadline (current-inexact-milliseconds)) 1000.0))
    (when (< remaining 0)
      (raise (exn:fail:network:timeout
              (format "HTTP read timeout (~a seconds) while reading response body" timeout-secs)
              (current-continuation-marks))))
    (define n (sync/timeout remaining (read-bytes-avail!-evt buf port)))
    (cond
      [(eq? n #f)
       (raise (exn:fail:network:timeout
               (format "HTTP read timeout (~a seconds) while reading response body" timeout-secs)
               (current-continuation-marks)))]
      [(eof-object? n) (get-output-bytes out)]
      [(> (+ total n) max-response-size)
       (raise (exn:fail "LLM response exceeds maximum size limit (10 MB)"
                        (current-continuation-marks)))]
      [else
       (write-bytes buf out 0 n)
       (loop (+ total n))])))

;; ============================================================
;; Bounded response body reading (SEC-10)
;; ============================================================

;; Maximum response body size: 10 MB
(define max-response-size (* 10 1024 1024))

;; Read from port into bytes with a size limit.
;; Raises exn:fail if the response exceeds max-response-size.
(define (read-response-body port)
  (define out (open-output-bytes))
  (define buf (make-bytes 8192))
  (let loop ([total 0])
    (define n (read-bytes-avail! buf port))
    (cond
      [(eof-object? n) (get-output-bytes out)]
      [(> (+ total n) max-response-size)
       (raise (exn:fail "LLM response exceeds maximum size limit (10 MB)"
                        (current-continuation-marks)))]
      [else
       (write-bytes buf out 0 n)
       (loop (+ total n))])))

;; ============================================================
;; parse-sse-lines
;; ============================================================

;; Parse a raw SSE text into a list of jsexpr hashes.
;; Filters out:
;;   - comment lines (starting with ':')
;;   - empty lines
;;   - [DONE] termination signals
;; Returns a list of parsed JSON hashes from 'data:' lines.
(define (parse-sse-lines text)
  (define lines (string-split text "\n"))
  (define results
    (for/fold ([acc '()]) ([line (in-list lines)])
      (define data-str (parse-sse-data-line line))
      (cond
        [(not data-str) acc] ; non-data line
        [(sse-done? data-str) acc] ; termination
        [else
         (with-handlers ([exn:fail? (lambda (e) acc)]) ; skip malformed
           (define parsed (string->jsexpr data-str))
           (cons parsed acc))])))
  (reverse results))

;; ============================================================
;; normalize-openai-chunks
;; ============================================================

;; Convert a list of OpenAI-format streaming response objects
;; into canonical stream-chunk structs.
(define (normalize-openai-chunks raw-chunks)
  (for/list ([chunk (in-list raw-chunks)])
    (define choices (hash-ref chunk 'choices '()))
    (define usage (hash-ref chunk 'usage #f))
    (define choice
      (if (null? choices)
          #f
          (car choices)))
    (define delta
      (if choice
          (hash-ref choice 'delta #f)
          #f))
    (define finish-reason
      (if choice
          (hash-ref choice 'finish_reason #f)
          #f))

    ;; Extract text delta
    (define delta-content
      (if delta
          (hash-ref delta 'content #f)
          #f))
    ;; JSON null is represented as 'null symbol in Racket - filter it out
    (define delta-text (if (string? delta-content) delta-content #f))

    ;; Extract tool-call delta
    (define delta-tool-call
      (if delta
          (let ([tcs (hash-ref delta 'tool_calls #f)])
            (if (and tcs (pair? tcs))
                (car tcs) ; first tool call delta
                #f))
          #f))

    (make-stream-chunk delta-text delta-tool-call usage (and (string? finish-reason) #t))))

;; ============================================================
;; accumulate-tool-call-deltas
;; ============================================================

;; Given a list of stream-chunks with tool-call deltas, accumulate
;; partial deltas into finalized tool call hashes.
;; Returns a list of (hash 'id ... 'name ... 'arguments ...) entries.
(define (accumulate-tool-call-deltas chunks)
  ;; We process chunks in order, grouping by index.
  ;; The first delta for a tool call carries the id and name.
  ;; Subsequent deltas only carry arguments fragments.
  ;; We use a mutable hash keyed by index to accumulate.
  (define groups (make-hash))

  (for ([ch (in-list chunks)])
    (define tc (stream-chunk-delta-tool-call ch))
    (when tc
      (define idx (hash-ref tc 'index 0))
      (define fn (hash-ref tc 'function (hash)))
      (define maybe-id (hash-ref tc 'id #f))
      (define maybe-name (hash-ref fn 'name #f))
      (define args-delta (hash-ref fn 'arguments ""))

      (cond
        [(hash-has-key? groups idx)
         ;; Accumulate into existing entry
         (define existing (hash-ref groups idx))
         (define prev-id (car existing))
         (define prev-name (cadr existing))
         (define prev-args (caddr existing))
         (hash-set! groups
                    idx
                    (list (or maybe-id prev-id)
                          (or maybe-name prev-name)
                          (string-append prev-args args-delta)))]
        ;; New tool call
        [else (hash-set! groups idx (list maybe-id maybe-name args-delta))])))

  ;; Build finalized tool calls in index order
  (define sorted-indices (sort (hash-keys groups) <))
  (for/list ([idx (in-list sorted-indices)])
    (define val (hash-ref groups idx))
    (hasheq 'id (car val) 'name (cadr val) 'arguments (caddr val))))

;; ============================================================
;; parse-sse-line (incremental)
;; ============================================================

;; parse-sse-line : string? -> (or/c hash? 'done #f)
;; Parse one SSE line. Returns jsexpr hash for "data: ..." lines,
;; 'done for [DONE], #f for empty lines, comments, or malformed data.
(define (parse-sse-line line)
  (define data-str (parse-sse-data-line line))
  (cond
    [(not data-str) #f]
    [(sse-done? data-str) 'done]
    [else
     (with-handlers ([exn:fail? (lambda (e) #f)])
       (string->jsexpr data-str))]))

;; ============================================================
;; parse-sse-data-line / sse-done?
;; ============================================================

;; parse-sse-data-line : string? -> (or/c string? #f)
;; Extract the data payload from an SSE `data: ...` line.
;; Returns the data string (after `data: `) for data lines,
;; or #f for non-data lines (empty lines, comments, event: lines, etc.).
(define (parse-sse-data-line line)
  (define trimmed (string-trim line))
  (cond
    [(string=? trimmed "") #f]
    [(string-prefix? trimmed ":") #f]
    [(string-prefix? trimmed "data: ") (substring trimmed 6)]
    [else #f]))

;; sse-done? : string? -> boolean?
;; Returns #t when the SSE data payload is the `[DONE]` termination signal.
(define (sse-done? data-str)
  (string=? data-str "[DONE]"))

;; ============================================================
;; normalize-openai-chunk (singular)
;; ============================================================

;; normalize-openai-chunk : hash? -> stream-chunk?
;; Normalize a single OpenAI-format streaming response object into a stream-chunk.
(define (normalize-openai-chunk raw)
  (define choices (hash-ref raw 'choices '()))
  (define usage (hash-ref raw 'usage #f))
  (define choice
    (if (null? choices)
        #f
        (car choices)))
  (define delta
    (if choice
        (hash-ref choice 'delta #f)
        #f))
  (define finish-reason
    (if choice
        (hash-ref choice 'finish_reason #f)
        #f))
  (define delta-content
    (if delta
        (hash-ref delta 'content #f)
        #f))
  (define delta-text (if (string? delta-content) delta-content #f))
  (define delta-tool-call
    (if delta
        (let ([tcs (hash-ref delta 'tool_calls #f)])
          (if (and tcs (pair? tcs))
              (car tcs)
              #f))
        #f))
  (make-stream-chunk delta-text delta-tool-call usage (and (string? finish-reason) #t)))

;; ============================================================
;; read-sse-chunks (incremental generator)
;; ============================================================

;; Default per-chunk timeout AFTER the first chunk has been received.
;; Once streaming has started, chunks should arrive quickly.
(define http-stream-timeout-default 60)

;; read-sse-chunks : input-port? [#:initial-timeout seconds] [#:stream-timeout seconds] -> generator?
;; Returns a generator that yields stream-chunk? values as they arrive from the port.
;; Yields #f when the stream is complete ([DONE] received or EOF).
;; Raises exn:fail:network:timeout on read timeout.
;; Uses #:initial-timeout for the first read (waiting for stream to start),
;; then #:stream-timeout for subsequent reads (chunks should arrive fast).
;; The port is NOT closed by this function — the caller is responsible.
(define (read-sse-chunks port
                         #:initial-timeout [initial-secs http-read-timeout-default]
                         #:stream-timeout [stream-secs http-stream-timeout-default])
  (generator ()
             (let loop ([first-read? #t])
               (define timeout-secs (if first-read? initial-secs stream-secs))
               (define line (read-line/timeout port #:timeout timeout-secs))
               (cond
                 [(eq? line #f)
                  ;; Timeout — raise clean error
                  (raise (exn:fail:network:timeout
                          (format "HTTP read timeout (~a seconds) waiting for SSE chunk" timeout-secs)
                          (current-continuation-marks)))]
                 [(eof-object? line) (yield #f)]
                 [else
                  (define parsed (parse-sse-line line))
                  (cond
                    [(eq? parsed 'done) (yield #f)]
                    [(hash? parsed)
                     (yield (normalize-openai-chunk parsed))
                     (loop #f)]
                    [else (loop #f)])]))))
