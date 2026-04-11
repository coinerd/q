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
         racket/port)

(provide
 parse-sse-lines
 parse-sse-line
 normalize-openai-chunks
 normalize-openai-chunk
 accumulate-tool-call-deltas
 read-sse-chunks
 read-response-body
 max-response-size)

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
    (for/fold ([acc '()])
              ([line (in-list lines)])
      (define trimmed (string-trim line))
      (cond
        [(string=? trimmed "") acc]                          ; empty line
        [(string-prefix? trimmed ":") acc]                   ; comment
        [(string-prefix? trimmed "data: ")
         (define data-str (substring trimmed 6))              ; after "data: "
         (cond
           [(string=? data-str "[DONE]") acc]                ; termination
           [else
            (with-handlers ([exn:fail? (lambda (e) acc)])    ; skip malformed
              (define parsed (string->jsexpr data-str))
              (cons parsed acc))])]
        [else acc])))
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
    (define choice (if (null? choices) #f (car choices)))
    (define delta (if choice (hash-ref choice 'delta #f) #f))
    (define finish-reason (if choice (hash-ref choice 'finish_reason #f) #f))

    ;; Extract text delta
    (define delta-content
      (if delta
          (hash-ref delta 'content #f)
          #f))
    ;; JSON null is represented as 'null symbol in Racket - filter it out
    (define delta-text
      (if (string? delta-content)
          delta-content
          #f))

    ;; Extract tool-call delta
    (define delta-tool-call
      (if delta
          (let ([tcs (hash-ref delta 'tool_calls #f)])
            (if (and tcs (pair? tcs))
                (car tcs)   ; first tool call delta
                #f))
          #f))

    (stream-chunk delta-text
                  delta-tool-call
                  usage
                  (and (string? finish-reason) #t))))

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
         (hash-set! groups idx
                    (list (or maybe-id prev-id)
                          (or maybe-name prev-name)
                          (string-append prev-args args-delta)))]
        [else
         ;; New tool call
         (hash-set! groups idx
                    (list maybe-id
                          maybe-name
                          args-delta))])))

  ;; Build finalized tool calls in index order
  (define sorted-indices (sort (hash-keys groups) <))
  (for/list ([idx (in-list sorted-indices)])
    (define val (hash-ref groups idx))
    (hash 'id (car val)
          'name (cadr val)
          'arguments (caddr val))))

;; ============================================================
;; parse-sse-line (incremental)
;; ============================================================

;; parse-sse-line : string? -> (or/c hash? 'done #f)
;; Parse one SSE line. Returns jsexpr hash for "data: ..." lines,
;; 'done for [DONE], #f for empty lines, comments, or malformed data.
(define (parse-sse-line line)
  (define trimmed (string-trim line))
  (cond
    [(string=? trimmed "") #f]
    [(string-prefix? trimmed ":") #f]
    [(string-prefix? trimmed "data: ")
     (define data-str (substring trimmed 6))
     (if (string=? data-str "[DONE]")
         'done
         (with-handlers ([exn:fail? (lambda (e) #f)])
           (string->jsexpr data-str)))]
    [else #f]))

;; ============================================================
;; normalize-openai-chunk (singular)
;; ============================================================

;; normalize-openai-chunk : hash? -> stream-chunk?
;; Normalize a single OpenAI-format streaming response object into a stream-chunk.
(define (normalize-openai-chunk raw)
  (define choices (hash-ref raw 'choices '()))
  (define usage (hash-ref raw 'usage #f))
  (define choice (if (null? choices) #f (car choices)))
  (define delta (if choice (hash-ref choice 'delta #f) #f))
  (define finish-reason (if choice (hash-ref choice 'finish_reason #f) #f))
  (define delta-content (if delta (hash-ref delta 'content #f) #f))
  (define delta-text (if (string? delta-content) delta-content #f))
  (define delta-tool-call
    (if delta
        (let ([tcs (hash-ref delta 'tool_calls #f)])
          (if (and tcs (pair? tcs)) (car tcs) #f))
        #f))
  (stream-chunk delta-text delta-tool-call usage (and (string? finish-reason) #t)))

;; ============================================================
;; read-sse-chunks (incremental generator)
;; ============================================================

;; read-sse-chunks : input-port? -> generator?
;; Returns a generator that yields stream-chunk? values as they arrive from the port.
;; Yields #f when the stream is complete ([DONE] received or EOF).
;; The port is NOT closed by this function — the caller is responsible.
(define (read-sse-chunks port)
  (generator ()
    (let loop ()
      (define line (read-line port))
      (cond
        [(eof-object? line) (yield #f)]
        [else
         (define parsed (parse-sse-line line))
         (cond
           [(eq? parsed 'done) (yield #f)]
           [(hash? parsed)
            (yield (normalize-openai-chunk parsed))
            (loop)]
           [else (loop)])]))))
