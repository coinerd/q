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
         "request-policy.rkt"
         racket/port
         racket/match)

;; Accumulator for partial tool-call deltas (I-09)
(struct tool-call-accum (id name arguments) #:transparent)

;; — SSE line-level helpers —
;; SSE parsing
(provide (contract-out [parse-sse-lines (-> string? (listof hash?))]
                       [parse-sse-line (-> string? (or/c hash? 'done #f))]
                       [parse-sse-data-line (-> string? (or/c string? #f))]
                       [sse-done? (-> string? boolean?)]
                       [accumulate-tool-call-deltas (-> list? (listof hash?))]
                       [stream-sse-events
                        (->* (input-port? procedure?)
                             (#:initial-timeout positive?
                                                #:stream-timeout positive?
                                                #:thinking-timeout positive?
                                                #:max-total-timeout positive?
                                                #:peer-close-probe-secs positive?)
                             generator?)]
                       [close-port-after-stream
                        (->* (generator? input-port?) (#:cleanup procedure?) generator?)]
                       ;; Response body reading
                       [read-response-body (-> input-port? bytes?)]
                       [read-response-body/timeout (->* (input-port?) (#:timeout positive?) bytes?)]
                       ;; Timeout-aware line reading
                       [read-line/timeout (->* (input-port?) (#:timeout positive?) any/c)]
                       ;; Timeout helpers
                       [effective-request-timeout-for (-> (or/c string? #f) positive?)]
                       [effective-sse-read-timeout-for (-> (or/c string? #f) (or/c positive? #f))]
                       [call-with-request-timeout
                        (->* (procedure?) (#:timeout positive? #:cleanup procedure?) any/c)]
                       ;; Phase timeout resolver (v1.00.12; BUG-0018 W1 cap)
                       [sse-phase-timeout-secs
                        (->* (#:request-timeout positive?)
                             (#:sse-read-override (or/c positive? #f)
                                                  #:thinking-gap-cap-override (or/c positive? #f))
                             (values positive? positive? positive?))])
         ;; Struct and predicates (direct export for match compatibility)
         tool-call-accum
         tool-call-accum?
         tool-call-accum-id
         tool-call-accum-name
         tool-call-accum-arguments
         ;; Constants
         max-response-size
         http-read-timeout-default
         http-stream-timeout-default
         http-request-timeout-default
         max-thinking-gap-secs
         ;; SSE line-level helper (BUG-0018 W1 keepalive tests)
         sse-comment-line?
         ;; Parameters
         current-http-request-timeout
         current-model-timeouts
         current-model-sse-read-timeouts
         ;; BUG-0018 W1 (re-export from request-policy.rkt)
         current-max-thinking-gap-secs
         ;; Mechanism observation seam (v1.00.13 W2)
         current-request-mechanism-observer
         ;; Exception struct
         exn:fail:network:timeout
         exn:fail:network:timeout?
         exn:fail:network:timeout:stream
         exn:fail:network:timeout:stream?
         exn:fail:network:timeout:stream-received-heartbeats?
         exn:fail:network:timeout:stream-received-any-data?
         exn:fail:network:timeout:stream-phase
         exn:fail:network:timeout:stream-output-chars
         ;; BUG-0019 W1: FIN/CLOSE-WAIT peer-close exception
         exn:fail:network:peer-closed
         exn:fail:network:peer-closed?
         exn:fail:network:peer-closed-phase
         exn:fail:network:peer-closed-data-received?
         exn:fail:network:peer-closed-content-chars
         exn:fail:network:peer-closed-elapsed-ms)

;; ============================================================
;; Timeout configuration
;; ============================================================

;; v1.00.13 W1 (#9461): the timeout-configuration OWNERSHIP moved to
;; llm/request-policy.rkt — constants, raw-config parameters, accessors, and
;; the v1.00.12 phase resolver all live there now (single-owner rule,
;; PLAN-v1.00.13 RL-1/RL-2/AC-1/AC-2). This mechanism module re-exports the
;; compatibility surface so existing requires keep working; no model/config
;; semantics remain here.
;;
;;   constants : http-read-timeout-default, http-request-timeout-default,
;;               http-stream-timeout-default, max-thinking-gap-secs
;;   params    : current-http-request-timeout, current-model-timeouts,
;;               current-model-sse-read-timeouts
;;   accessors : effective-request-timeout-for, effective-sse-read-timeout-for
;;   resolver  : sse-phase-timeout-secs (compatibility re-export only)

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
              ;; v0.99.78 FIX: catch exn:break from kill-thread silently. Without
              ;; this, a timeout while blocked in http-sendrecv (deepseek resets
              ;; the connection after long thinking pauses) leaked a full Racket
              ;; stack trace to stderr, corrupting the TUI status/prompt area.
              (with-handlers ([exn:break? (lambda (e) (void))]
                              [exn:fail? (lambda (e) (channel-put ch (cons 'exn e)))])
                (channel-put ch (cons 'val (thunk)))))))
  (define result (sync/timeout timeout-secs ch))
  (match result
    [#f
     ;; Close the owned response resource before interrupting the worker. Killing
     ;; first can race TLS teardown and leave the connection half-open.
     (with-handlers ([exn:fail? (lambda (e)
                                  (log-warning (format "llm/stream: cleanup error: ~a"
                                                       (exn-message e))))])
       (cleanup-thunk)) ; #454: close ports
     (kill-thread th)
     (raise (exn:fail:network:timeout (format "HTTP request timeout (~a seconds)" timeout-secs)
                                      (current-continuation-marks)))]
    [_
     (define tag (car result))
     (define payload (cdr result))
     (match tag
       ['exn (raise payload)]
       [_ payload])]))

;; Exception type for network read timeouts.
(struct exn:fail:network:timeout exn:fail () #:transparent)

;; v0.99.81 W1: Stream-level timeout with liveness metadata.
;; Subtype of exn:fail:network:timeout so existing handlers still match.
;; Carries truthful evidence about what the peer sent before stalling:
;;   - received-heartbeats?  : SSE comment lines (: ...) were seen
;;   - received-any-data?    : at least one data: chunk was yielded
;;   - phase                 : 'initial | 'thinking | 'content
;; Used by W2 circuit-breaker to distinguish a dead peer from a slow one.
(struct exn:fail:network:timeout:stream
        exn:fail:network:timeout
        (received-heartbeats? received-any-data? phase output-chars)
  #:transparent)

;; BUG-0019 W1: raised when the transport reports the peer is gone while the
;; SSE reader waits in an idle window — a TCP FIN without TLS close_notify
;; (GLM gateway signature) surfaces on Racket SSL ports as a network read
;; error rather than a clean EOF, so detection must not wait out the whole
;; phase window. Subtype of exn:fail:network:timeout so existing handlers
;; and the retryable/timeout-tier classification keep matching.
;; Fields (machine truth; message carries the human-readable suffix):
;;   phase          'initial | 'thinking | 'content at detection time
;;   data-received? at least one chunk was yielded before the close
;;   content-chars  visible content chars produced before the close
;;   elapsed-ms     wall-clock ms from stream start to detection
(struct exn:fail:network:peer-closed
        exn:fail:network:timeout
        (phase data-received? content-chars elapsed-ms)
  #:transparent)

;; sse-comment-line? : string? -> boolean?
;; Returns #t for SSE comment lines (starting with ':') used as heartbeats.
(define (sse-comment-line? line)
  (define trimmed (string-trim line))
  (and (positive? (string-length trimmed)) (char=? (string-ref trimmed 0) #\:)))

;; phase-from-state : boolean? boolean? -> (or/c 'initial 'thinking 'content)
;; Derives the stream phase from accumulated liveness flags.
(define (phase-from-state received-any-data? seen-content?)
  (cond
    [(not received-any-data?) 'initial]
    [(not seen-content?) 'thinking]
    [else 'content]))

;; ============================================================
;; Timeout-aware read helpers
;; ============================================================

;; read-line/timeout : input-port? [#:timeout seconds] -> (or/c string? eof?)
;; Like read-line but with a timeout. A timed-out response is no longer usable,
;; so close it before returning #f; callers must establish a new connection.
(define (read-line/timeout port #:timeout [timeout-secs http-read-timeout-default])
  (define result (sync/timeout timeout-secs (read-line-evt port 'any)))
  (match result
    [#f
     (unless (port-closed? port)
       (close-input-port port))
     #f]
    [_ result])) ; string or eof

;; read-line/nonblocking : input-port? -> (or/c string? #f)
;; Non-blocking read — returns the line if available, #f otherwise (port left open).
;; Unlike read-line/timeout, this never closes the port.
(define (read-line/nonblocking port)
  (define result (sync/timeout 0 (read-line-evt port 'any)))
  (and result (not (eof-object? result)) result))

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
         (with-handlers ([exn:fail? (lambda (e)
                                      (log-warning (format "llm/stream: malformed SSE data: ~a"
                                                           (exn-message e)))
                                      acc)]) ; skip malformed
           (define parsed (string->jsexpr data-str))
           (cons parsed acc))])))
  (reverse results))

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
         (define prev-id (tool-call-accum-id existing))
         (define prev-name (tool-call-accum-name existing))
         (define prev-args (tool-call-accum-arguments existing))
         (hash-set! groups
                    idx
                    (tool-call-accum (or maybe-id prev-id)
                                     (or maybe-name prev-name)
                                     (string-append prev-args args-delta)))]
        ;; New tool call
        [else (hash-set! groups idx (tool-call-accum maybe-id maybe-name args-delta))])))

  ;; Build finalized tool calls in index order
  (define sorted-indices (sort (hash-keys groups) <))
  (for/list ([idx (in-list sorted-indices)])
    (define val (hash-ref groups idx))
    (hasheq 'id
            (tool-call-accum-id val)
            'name
            (tool-call-accum-name val)
            'arguments
            (tool-call-accum-arguments val))))

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
     (with-handlers ([exn:fail? (lambda (e)
                                  (log-warning (format "llm/stream: parse error: ~a" (exn-message e)))
                                  #f)])
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
    [(string-prefix? trimmed "data:") (substring trimmed 5)]
    [else #f]))

;; sse-done? : string? -> boolean?
;; Returns #t when the SSE data payload is the `[DONE]` termination signal.
(define (sse-done? data-str)
  (string=? data-str "[DONE]"))

;; ============================================================
;; Default per-chunk stream timeout (used by stream-sse-events callers)
;; ============================================================

;; v1.00.13 W1 (#9461): the definition moved to llm/request-policy.rkt and is
;; re-exported above (compatibility). http-stream-timeout-default (60) and
;; max-thinking-gap-secs (300) are policy constants with one owner.

;; ============================================================
;; SSE phase-timeout resolver (v1.00.12, SS-1/SS-2/SS-3)
;; ============================================================

;; v1.00.13 W1 (#9461): sse-phase-timeout-secs moved to
;; llm/request-policy.rkt and is re-exported for compatibility. Prefer
;; resolve-request-network-policy / resolve-request-network-policy-for-model;
;; the compat surface exists for one release (see
;; tests/test-request-policy-architecture.rkt).

;; ============================================================
;; stream-sse-events: Provider-agnostic SSE event generator
;; ============================================================
;; remains open across yields and closes on normal termination, read failure,
;; cancellation, or collection after consumer abandonment.
;;
;; A dynamic-wind around `yield` is intentionally not used: a yield exits the
;; dynamic extent and would run the after-thunk, closing the live port after the
;; first chunk. A will associates cleanup with collection of the wrapper itself.
(define stream-port-will-executor (make-will-executor))
(void (thread (lambda ()
                (let loop ()
                  ;; A custom port can raise while closing. Isolate each action so one
                  ;; faulty finalizer cannot permanently disable cleanup for later streams.
                  (with-handlers ([exn? (lambda (e)
                                          (log-warning (format "llm/stream: finalizer error: ~a"
                                                               (exn-message e))))])
                    (will-execute stream-port-will-executor))
                  (loop)))))

(define (close-port-after-stream source
                                 port
                                 #:cleanup [resource-cleanup
                                            (lambda ()
                                              (unless (port-closed? port)
                                                (close-input-port port)))])
  (define cleanup-lock (make-semaphore 1))
  (define cleaned? (box #f))
  (define (cleanup!)
    (call-with-semaphore cleanup-lock
                         (lambda ()
                           (unless (unbox cleaned?)
                             (set-box! cleaned? #t)
                             (resource-cleanup)))))
  (define wrapped
    (generator ()
               (with-handlers ([exn:break? (lambda (e)
                                             (cleanup!)
                                             (raise e))]
                               [exn:fail? (lambda (e)
                                            (cleanup!)
                                            (raise e))])
                 (let loop ()
                   (define chunk (source))
                   (cond
                     [chunk
                      (yield chunk)
                      (loop)]
                     [else
                      (cleanup!)
                      (yield #f)])))))
  (will-register
   stream-port-will-executor
   wrapped
   (lambda (_ignored)
     (with-handlers ([exn? (lambda (e)
                             (log-warning (format "llm/stream: finalizer cleanup error: ~a"
                                                  (exn-message e))))])
       (cleanup!))))
  wrapped)

;; Provider-agnostic SSE event generator.
;; Takes a port and an event->chunks callback that converts parsed JSON events
;; into provider-specific chunks. Handles SSE lifecycle, timeouts, and
;; keep-alive protection. Returns a generator yielding chunks or #f when done.
;;
;; BUG-0018 W1 liveness semantics: the phase timeouts are per-read windows.
;; SSE comment/heartbeat lines (`: ...`) and zero-delta data chunks each
;; complete a successful read, so every such frame resets the idle clock — a
;; socket that is demonstrably alive (keepalive traffic flowing) is never
;; killed by the thinking/content idle window. Only true silence (no bytes at
;; all for the whole phase window) raises, and the total-duration budget
;; (#:max-total-timeout) still bounds heartbeat-only streams.
;; ============================================================
;; Mechanism observation seam (v1.00.13 W2, RL-10/AC-3)
;; ============================================================

;; When set, the shared stream/body mechanism invokes it with a hash of the
;; lifecycle arguments each adapter actually passed: stream-sse-events records
;; (kind stream initial thinking content total); make-provider-http-request
;; records (kind body-read read-timeout). This is how the cross-adapter
;; conformance harness (tests/test-provider-network-policy-conformance.rkt)
;; proves every adapter consumes the SAME resolved policy. Default #f = no
;; overhead. Injection seam only — never a data path.
(define current-request-mechanism-observer (make-parameter #f))

(define (stream-sse-events port
                           event->chunks
                           #:initial-timeout [initial-secs http-read-timeout-default]
                           #:stream-timeout [stream-secs http-stream-timeout-default]
                           #:thinking-timeout [thinking-secs stream-secs]
                           #:max-total-timeout [max-total-secs 600]
                           ;; BUG-0019 W1: probe cadence (policy-driven via
                           ;; request-policy's current-peer-close-probe-secs).
                           #:peer-close-probe-secs [probe-secs (current-peer-close-probe-secs)])
  (generator
   ()
   (define observer (current-request-mechanism-observer))
   (when observer
     (observer (hasheq 'kind
                       'stream
                       'initial
                       initial-secs
                       'thinking
                       thinking-secs
                       'content
                       stream-secs
                       'total
                       max-total-secs)))
   (define stream-start (current-inexact-milliseconds))
   (define deadline (+ stream-start (* max-total-secs 1000.0)))
   (define max-consecutive-empty 100)
   ;; v1.00.12 W2 (SS-5): diagnostic suffix on every stream-timeout
   ;; message. The struct fields remain the machine source of truth;
   ;; this string form exists for logs/UX and is regression-matched by
   ;; tests/test-sse-phase-timeout-bounds.rkt against
   ;; #rx"\\[phase=(initial|thinking|content) data-received=(yes|no) chars=[0-9]+\\]$".
   (define (timeout-msg base phase received-any-data? content-chars)
     (string-append base
                    (format " [phase=~a data-received=~a chars=~a]"
                            phase
                            (if received-any-data? "yes" "no")
                            content-chars)))
   ;; v0.99.84: Line buffer for aggressive socket draining (CLOSE-WAIT fix).
   ;; Ported from read-sse-chunks so all providers using stream-sse-events
   ;; benefit from the same CLOSE-WAIT prevention.
   (define line-buffer (box '()))
   (define (buf-pop!)
     (match (unbox line-buffer)
       ['() #f]
       [(cons l r)
        (set-box! line-buffer r)
        l]))
   (define (buf-push! lines)
     (set-box! line-buffer (append (unbox line-buffer) lines)))
   ;; ==========================================================
   ;; BUG-0019 W1: FIN/CLOSE-WAIT liveness watchdog
   ;; ============================================================
   ;; All port reads now flow through a byte-level pump that
   ;; slices each idle window into probe-interval chunks. At every
   ;; slice boundary (and once more when the window is exhausted)
   ;; a zero-timeout liveness check runs:
   ;;
   ;;   - a raised network read error => the peer is gone without a
   ;;     clean TLS shutdown => exn:fail:network:peer-closed
   ;;     immediately (ladder option 1);
   ;;   - eof => clean shutdown/close_notify => normal end-of-stream;
   ;;   - bytes => buffered into `pending-bytes` and assembled into
   ;;     lines (never lost, never re-read by the line layer);
   ;;   - #f (would block) => inconclusive: keep waiting. The
   ;;     watchdog NEVER fabricates death, so healthy slow-start or
   ;;     deep-think streams are untouched (BUG-0018 rule).
   (define chunk-buf (make-bytes 4096))
   (define pending-bytes (box #""))
   (define eof-seen? (box #f))
   (define (stash-bytes! n)
     (set-box! pending-bytes (bytes-append (unbox pending-bytes) (subbytes chunk-buf 0 n))))
   ;; Decode one complete line's bytes (strip optional \r).
   (define (decode-line! raw)
     (define len (bytes-length raw))
     (define stripped
       (if (and (> len 0) (= (bytes-ref raw (sub1 len)) 13))
           (subbytes raw 0 (sub1 len))
           raw))
     (with-handlers ([exn:fail? (lambda (_e) (bytes->string/latin-1 stripped))])
       (bytes->string/utf-8 stripped)))
   ;; Pop the next complete line from pending bytes, or #f.
   (define (pending-line!)
     (define b (unbox pending-bytes))
     (let find-nl ([i 0])
       (cond
         [(>= i (bytes-length b)) #f]
         [(= (bytes-ref b i) 10)
          (set-box! pending-bytes (subbytes b (add1 i)))
          (decode-line! (subbytes b 0 i))]
         [else (find-nl (add1 i))])))
   ;; Zero-timeout liveness probe (BUG-0019 ladder option 1).
   ;; Returns 'data | 'dead | 'clean-eof | 'inconclusive.
   (define (probe-port!)
     (with-handlers ([exn:fail:network? (lambda (_e) 'dead)])
       (define r (sync/timeout 0 (read-bytes-avail!-evt chunk-buf port)))
       (cond
         [(eof-object? r)
          (set-box! eof-seen? #t)
          'clean-eof]
         [(not r) 'inconclusive]
         [(and (real? r) (> r 0))
          (stash-bytes! r)
          'data]
         [else 'inconclusive])))
   (define (raise-peer-closed! phase data-received? content-chars)
     (define elapsed-ms (- (current-inexact-milliseconds) stream-start))
     (raise (exn:fail:network:peer-closed
             (string-append "Peer closed the connection mid-stream"
                            (format " [phase=~a data-received=~a chars=~a elapsed-ms=~a]"
                                    phase
                                    (if data-received? "yes" "no")
                                    content-chars
                                    (inexact->exact (floor elapsed-ms))))
             (current-continuation-marks)
             phase
             data-received?
             content-chars
             elapsed-ms)))
   ;; Wait up to wait-secs for the next complete SSE line.
   ;; Returns: a complete line string, the eof object (clean
   ;; end-of-stream), or 'timeout (idle window exhausted with the
   ;; peer silent). Raises exn:fail:network:peer-closed when the
   ;; transport reports death.
   (define (pump! wait-secs phase data-received? content-chars)
     (let loop ([remaining wait-secs])
       (or (pending-line!)
           (cond
             [(unbox eof-seen?) eof]
             [(<= remaining 0)
              ;; Window exhausted: one last free liveness check
              ;; before the caller raises the phase timeout.
              (case (probe-port!)
                [(dead) (raise-peer-closed! phase data-received? content-chars)]
                [else 'timeout])]
             [else
              (define slice-start-ms (current-inexact-milliseconds))
              (define r
                (with-handlers ([exn:fail:network?
                                 (lambda (_e)
                                   (raise-peer-closed! phase data-received? content-chars))])
                  (sync/timeout (min remaining probe-secs) (read-bytes-avail!-evt chunk-buf port))))
              (define consumed-secs (/ (- (current-inexact-milliseconds) slice-start-ms) 1000.0))
              (cond
                [(eof-object? r)
                 ;; Clean TLS shutdown (close_notify) or plain-pipe
                 ;; EOF: normal end-of-stream, NOT an error.
                 (set-box! eof-seen? #t)
                 (or (pending-line!) eof)]
                [(not r)
                 ;; Slice expired silently: probe for a dead
                 ;; transport before burning the next slice.
                 (case (probe-port!)
                   [(dead) (raise-peer-closed! phase data-received? content-chars)]
                   [(clean-eof)
                    (set-box! eof-seen? #t)
                    (or (pending-line!) eof)]
                   [else (loop (max 0 (- remaining (min consumed-secs remaining))))])]
                [else
                 (stash-bytes! r)
                 (loop (max 0 (- remaining (min consumed-secs remaining))))])]))))
   ;; v0.99.84 parity: after a successful line, opportunistically
   ;; drain already-buffered frames so a slow consumer cannot let
   ;; the socket drift into CLOSE-WAIT buildup.
   (define (drain-nonblocking!)
     (let drain ()
       (unless (unbox eof-seen?)
         (when (memq (probe-port!) '(data clean-eof))
           (let push ()
             (define l (pending-line!))
             (when l
               (buf-push! (list l))
               (push)))
           (drain)))))
   (let loop ([first-read? #t]
              [consecutive-empty 0]
              [seen-content? #f]
              [received-heartbeats? #f]
              [received-any-data? #f]
              [content-chars 0])
     (when (> (current-inexact-milliseconds) deadline)
       (raise (exn:fail:network:timeout:stream
               (timeout-msg (format "Stream exceeded maximum total duration (~a seconds)"
                                    max-total-secs)
                            (phase-from-state received-any-data? seen-content?)
                            received-any-data?
                            content-chars)
               (current-continuation-marks)
               received-heartbeats?
               received-any-data?
               (phase-from-state received-any-data? seen-content?)
               content-chars)))
     (when (>= consecutive-empty max-consecutive-empty)
       (raise (exn:fail:network:timeout:stream
               (timeout-msg (format "Stream exceeded ~a consecutive empty lines"
                                    max-consecutive-empty)
                            (phase-from-state received-any-data? seen-content?)
                            received-any-data?
                            content-chars)
               (current-continuation-marks)
               received-heartbeats?
               received-any-data?
               (phase-from-state received-any-data? seen-content?)
               content-chars)))
     ;; v0.99.65 W0: Phase-aware timeout (same as read-sse-chunks)
     (define timeout-secs
       (cond
         [first-read? initial-secs]
         [(not seen-content?) thinking-secs]
         [else stream-secs]))
     ;; v1.00.13 W4 (#9478, NP-7): a blocking read may never overshoot
     ;; the total wall-clock deadline by a full phase window — every
     ;; wait is capped at the remaining total budget (min of the
     ;; phase idle window and the remaining budget).
     (define remaining-total-secs (/ (- deadline (current-inexact-milliseconds)) 1000.0))
     (define wait-secs (min timeout-secs (max 0.05 remaining-total-secs)))
     ;; v0.99.84 / BUG-0019 W1: Check buffer first, then read
     ;; through the sliced watchdog pump (probe cadence =
     ;; #:peer-close-probe-secs).
     (define line
       (or (buf-pop!)
           (let ([l (pump! wait-secs
                           (phase-from-state received-any-data? seen-content?)
                           received-any-data?
                           content-chars)])
             (when (string? l)
               (drain-nonblocking!))
             l)))
     (cond
       [(eq? line 'timeout)
        (raise (exn:fail:network:timeout:stream
                (timeout-msg (format "HTTP read timeout (~a seconds) waiting for SSE chunk"
                                     timeout-secs)
                             (phase-from-state received-any-data? seen-content?)
                             received-any-data?
                             content-chars)
                (current-continuation-marks)
                received-heartbeats?
                received-any-data?
                (phase-from-state received-any-data? seen-content?)
                content-chars))]
       [(eof-object? line) (yield #f)]
       [else
        (define is-heartbeat? (sse-comment-line? line))
        (define parsed (parse-sse-line line))
        (cond
          [(eq? parsed 'done) (yield #f)]
          [(hash? parsed)
           (define chunks (event->chunks parsed))
           ;; v0.99.82 W1 NR-1: Detect content and accumulate char
           ;; count for mid-stream stall classification.
           (define-values (any-content event-chars)
             (for/fold ([ac #f]
                        [total 0])
                       ([ch (in-list chunks)])
               (define txt (and (stream-chunk? ch) (stream-chunk-delta-text ch)))
               (define len
                 (if (and (string? txt) (positive? (string-length txt)))
                     (string-length txt)
                     0))
               (values (or ac (> len 0)) (+ total len))))
           ;; v0.99.81 W1: only set received-any-data? when a chunk is
           ;; actually yielded (pair? check), not when event->chunks
           ;; returns '() for ping/keepalive events.
           (define yielded-any? (pair? chunks))
           (for ([ch (in-list chunks)])
             (yield ch))
           (loop #f
                 0
                 (or seen-content? any-content)
                 received-heartbeats?
                 (or received-any-data? yielded-any?)
                 (+ content-chars event-chars))]
          [else
           (loop #f
                 (add1 consecutive-empty)
                 seen-content?
                 (or received-heartbeats? is-heartbeat?)
                 received-any-data?
                 content-chars)])]))))
