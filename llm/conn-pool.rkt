#lang racket/base

;; llm/conn-pool.rkt — BUG-0019 W2: flag-gated connection pool for
;; openai-compatible SSE streams.
;;
;; DEFAULT OFF: `current-conn-pool` is a parameter defaulting to #f, and every
;; caller MUST treat #f as "disabled passthrough" — i.e. run the legacy
;; per-request `http-sendrecv` path verbatim. The pool only becomes active
;; when config wiring (wiring/mode-helpers.rkt, networking.pool.enabled=true)
;; installs one.
;;
;; Semantics (PLAN-v1.00.15 W2):
;;   - keyed by (host, port, tls?) — hosts compared case-insensitively;
;;   - PER-ENTRY CUSTODIAN: every pooled socket lives under its own custodian
;;     parented by the pool custodian, never under a request-scoped custodian,
;;     so request teardown cannot kill healthy pooled sockets;
;;   - idle TTL default 55 s (< typical server keep-alive window);
;;   - max-per-host default 4, enforced at check-in time (a surplus healthy
;;     connection is closed instead of pooled);
;;   - FAULT RULE: any error/timeout/peer-close marks the connection
;;     single-use — it is discarded on release and NEVER handed out again;
;;   - reuse requires deterministic framing: Content-Length bodies check in
;;     directly; CHUNKED bodies are decoded by make-chunked-input-port
;;     (BUG-0021) and check in only once their terminating 0-chunk has been
;;     consumed. EOF-delimited bodies remain single-use-on-success.
;;
;; Stats (pool-stats): hits / misses / evictions / fault-closes — exposed for
;; bake-period evidence.

(require racket/contract
         racket/match
         racket/string
         racket/port
         racket/tcp
         openssl)

(provide (contract-out
          (struct pool-stats
                  ([hits exact-nonnegative-integer?] [misses exact-nonnegative-integer?]
                                                     [evictions exact-nonnegative-integer?]
                                                     [fault-closes exact-nonnegative-integer?]))
          [make-conn-pool
           (->* ()
                (#:idle-ttl-secs positive?
                                 #:max-per-host exact-positive-integer?
                                 #:connect-timeout-secs positive?)
                conn-pool?)]
          [conn-pool? (-> any/c boolean?)]
          [current-conn-pool (parameter/c (or/c conn-pool? #f))]
          [pool-acquire!
           (-> conn-pool? string? exact-nonnegative-integer? boolean? pooled-connection?)]
          [pool-release! (->* (conn-pool? pooled-connection?) (#:outcome symbol?) void?)]
          [pool-evict-idle! (-> conn-pool? exact-nonnegative-integer?)]
          [pool-shutdown! (-> conn-pool? void?)]
          [pool-stats-for (-> conn-pool? pool-stats?)]
          [pooled-connection? (-> any/c boolean?)]
          [pooled-connection-host (-> pooled-connection? string?)]
          [pooled-connection-port-number (-> pooled-connection? exact-nonnegative-integer?)]
          [pooled-connection-tls? (-> pooled-connection? boolean?)]
          [pooled-connection-reusable? (-> pooled-connection? boolean?)]
          [pooled-connection-released? (-> pooled-connection? boolean?)]
          [mark-pool-fault! (-> pooled-connection? void?)]
          ;; Test/unit hook: force the reusable flag (mirrors what a parsed
          ;; Content-Length response head does inside pool-send-request!).
          [mark-pool-reusable! (-> pooled-connection? void?)]
          [make-chunked-input-port (-> input-port? pooled-connection? input-port?)]
          [pooled-connection-in (-> pooled-connection? input-port?)]
          [pool-send-request!
           (->* (pooled-connection? string?)
                (#:method string? #:headers (listof string?) #:data bytes? #:head-timeout positive?)
                (values string? (listof string?) input-port?))]))

;; ============================================================
;; Data types
;; ============================================================

;; Immutable snapshot of the four pool counters (BUG_PLAN W2).
(struct pool-stats (hits misses evictions fault-closes) #:transparent)

;; A pooled TCP/TLS connection. All ports live under `custodian`, which is a
;; child of the owning pool's custodian — deliberately NOT under any
;; request-scoped custodian (BUG_PLAN W2 risk note).
(struct pooled-connection
        (host port-number
              tls?
              custodian
              in
              out
              ;; reusable?-box: set by pool-send-request! when response framing
              ;; allows safe reuse (Content-Length). Any fault clears it.
              reusable?-box
              ;; released?-box: idempotence guard — a connection is released
              ;; (checked in OR discarded) exactly once; later releases no-op.
              released?-box
              last-used-ms)
  #:transparent)

;; Internal pool record. `stats-vec` is (vector hits misses evictions
;; fault-closes), mutated only while holding `lock`.
(struct conn-pool
        (lock table stats-vec idle-ttl-secs max-per-host connect-timeout-secs custodian closed?-box)
  #:transparent)

;; Disabled passthrough is the DEFAULT (flag off ⇒ behavior-neutral).
(define current-conn-pool (make-parameter #f))

;; Box-unwrapping predicates backing the contract exports.
(define (pooled-connection-reusable? entry)
  (unbox (pooled-connection-reusable?-box entry)))
(define (pooled-connection-released? entry)
  (unbox (pooled-connection-released?-box entry)))

;; stats vector indices
(define STAT-HIT 0)
(define STAT-MISS 1)
(define STAT-EVICT 2)
(define STAT-FAULT 3)

;; Pool key: case-insensitive host + port + tls flag.
(define (pool-key host port tls?)
  (list (string-downcase host) port tls?))

(define (make-conn-pool #:idle-ttl-secs [idle-ttl-secs 55]
                        #:max-per-host [max-per-host 4]
                        #:connect-timeout-secs [connect-timeout-secs 10])
  (conn-pool (make-semaphore 1)
             (make-hash)
             (vector 0 0 0 0)
             idle-ttl-secs
             max-per-host
             connect-timeout-secs
             (make-custodian)
             (box #f)))

;; ============================================================
;; Internal helpers — every table/stat mutation happens under `lock`
;; ============================================================

(define (bump-stat! pool idx)
  (define v (conn-pool-stats-vec pool))
  (vector-set! v idx (add1 (vector-ref v idx))))

;; Close a single connection via its own custodian. Never raises.
(define (discard-entry! entry)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning (format "llm/conn-pool: discard error: ~a"
                                                    (exn-message e))))])
    (custodian-shutdown-all (pooled-connection-custodian entry))))

;; Drop expired entries from one bucket. Returns how many were evicted.
;; Caller holds the lock.
(define (evict-expired-for-key! pool key)
  (define bucket (hash-ref (conn-pool-table pool) key '()))
  (define now (current-inexact-milliseconds))
  (define ttl-ms (* 1000 (conn-pool-idle-ttl-secs pool)))
  (define expired
    (for/list ([entry (in-list bucket)]
               #:when (> (- now (pooled-connection-last-used-ms entry)) ttl-ms))
      entry))
  (define keep
    (for/list ([entry (in-list bucket)]
               #:unless (member entry expired))
      entry))
  (for ([entry (in-list expired)])
    (discard-entry! entry)
    (bump-stat! pool STAT-EVICT))
  (if (null? keep)
      (hash-remove! (conn-pool-table pool) key)
      (hash-set! (conn-pool-table pool) key keep))
  (length expired))

;; Open a brand-new TCP/TLS connection under its own per-entry custodian,
;; bounded by the pool's connect timeout. Raises on failure; the failed
;; attempt's resources are always shut down.
(define (open-connection! pool host port tls?)
  (when (unbox (conn-pool-closed?-box pool))
    (raise (exn:fail "llm/conn-pool: pool already shut down" (current-continuation-marks))))
  (define cust (make-custodian (conn-pool-custodian pool)))
  (define ch (make-channel))
  (parameterize ([current-custodian cust])
    (thread (lambda ()
              (with-handlers ([exn:fail? (lambda (e) (channel-put ch (cons 'exn e)))])
                (define-values (in out)
                  (if tls?
                      (ssl-connect host port)
                      (tcp-connect host port)))
                (channel-put ch (cons 'val (cons in out)))))))
  (define result (sync/timeout (conn-pool-connect-timeout-secs pool) ch))
  (unless result
    ;; Connect attempt still running: tear down its whole subtree (kills the
    ;; worker thread and closes whatever it opened).
    (custodian-shutdown-all cust)
    (raise (exn:fail:network (format "llm/conn-pool: connect timeout (~as) to ~a:~a"
                                     (conn-pool-connect-timeout-secs pool)
                                     host
                                     port)
                             (current-continuation-marks))))
  (match result
    [(cons 'exn e)
     (custodian-shutdown-all cust)
     (raise e)]
    [(cons 'val (cons in out))
     (pooled-connection host
                        port
                        tls?
                        cust
                        in
                        out
                        ;; Fresh connections start non-reusable: only a parsed
                        ;; Content-Length response head flips this to #t.
                        (box #f)
                        (box #f)
                        (current-inexact-milliseconds))]))

;; ============================================================
;; Public API
;; ============================================================

;; Snapshot of the counters.
(define (pool-stats-for pool)
  (call-with-semaphore (conn-pool-lock pool)
                       (lambda ()
                         (define v (conn-pool-stats-vec pool))
                         (pool-stats (vector-ref v STAT-HIT)
                                     (vector-ref v STAT-MISS)
                                     (vector-ref v STAT-EVICT)
                                     (vector-ref v STAT-FAULT)))))

;; Acquire a healthy connection for (host, port, tls?): return a pooled idle
;; connection when one exists (hit), otherwise open a fresh one (miss).
;; Expired idle entries are evicted on the way through.
(define (pool-acquire! pool host port tls?)
  (define key (pool-key host port tls?))
  (define cached
    (call-with-semaphore (conn-pool-lock pool)
                         (lambda ()
                           (evict-expired-for-key! pool key)
                           (define bucket (hash-ref (conn-pool-table pool) key '()))
                           (cond
                             [(null? bucket)
                              (bump-stat! pool STAT-MISS)
                              #f]
                             [else
                              (define entry (car bucket))
                              (hash-set! (conn-pool-table pool) key (cdr bucket))
                              (bump-stat! pool STAT-HIT)
                              entry]))))
  (or cached (open-connection! pool host port tls?)))

;; Mark a connection as faulted: it will be discarded (never reused) even if
;; the caller later releases it with #:outcome 'ok. Called automatically when
;; pool-send-request! raises; stream adapters should call it whenever a read/
;; timeout/peer-close error touches the underlying socket.
(define (mark-pool-fault! entry)
  (set-box! (pooled-connection-reusable?-box entry) #f))

;; Test/unit hook: force the reusable flag (mirrors what a parsed
;; Content-Length response head does inside pool-send-request!). Not used by
;; the production adapters.
(define (mark-pool-reusable! entry)
  (set-box! (pooled-connection-reusable?-box entry) #t))

;; Release a connection after use.
;;   #:outcome 'ok    — stream completed cleanly; check in if framing allows.
;;   #:outcome 'fault — an error/timeout/peer-close was seen; discard.
;; Idempotent: exactly the first release decides the fate.
(define (pool-release! pool entry #:outcome [outcome 'ok])
  (call-with-semaphore
   (conn-pool-lock pool)
   (lambda ()
     (unless (unbox (pooled-connection-released?-box entry))
       (set-box! (pooled-connection-released?-box entry) #t)
       (define fault? (eq? outcome 'fault))
       (define reusable? (and (not fault?) (unbox (pooled-connection-reusable?-box entry))))
       (cond
         [(not reusable?)
          ;; FAULT RULE / non-reusable framing: single use, never handed out again.
          (discard-entry! entry)
          (bump-stat! pool (if fault? STAT-FAULT STAT-EVICT))]
         [else
          (define key
            (pool-key (pooled-connection-host entry)
                      (pooled-connection-port-number entry)
                      (pooled-connection-tls? entry)))
          (define bucket (hash-ref (conn-pool-table pool) key '()))
          (cond
            [(>= (length bucket) (conn-pool-max-per-host pool))
             ;; Cap enforced at check-in: surplus healthy connections close.
             (discard-entry! entry)
             (bump-stat! pool STAT-EVICT)]
            [else
             ;; Check in with FRESH boxes: the returned handle must not share
             ;; mutable state with the pooled copy, otherwise the pooled
             ;; entry would inherit released?#t (making the next release a
             ;; silent no-op) and fault marks would leak across generations.
             (hash-set! (conn-pool-table pool)
                        key
                        (cons (struct-copy pooled-connection
                                           entry
                                           [reusable?-box (box #t)]
                                           [released?-box (box #f)]
                                           [last-used-ms (current-inexact-milliseconds)])
                              bucket))])])))))

;; Evict all idle entries older than the pool TTL across every key. Returns
;; the number of entries evicted.
(define (pool-evict-idle! pool)
  (call-with-semaphore (conn-pool-lock pool)
                       (lambda ()
                         (for/sum ([key (in-hash-keys (conn-pool-table pool))])
                                  (evict-expired-for-key! pool key)))))

;; Shut the pool down: close every pooled connection and mark the pool closed
;; (further acquire attempts raise). Idempotent.
(define (pool-shutdown! pool)
  (call-with-semaphore (conn-pool-lock pool)
                       (lambda ()
                         (unless (unbox (conn-pool-closed?-box pool))
                           (set-box! (conn-pool-closed?-box pool) #t)
                           (for ([key (in-hash-keys (conn-pool-table pool))])
                             (for ([entry (in-list (hash-ref (conn-pool-table pool) key '()))])
                               (discard-entry! entry))
                             (hash-remove! (conn-pool-table pool) key)))))
  ;; Belt-and-braces: also tears down any connection lost outside the table
  ;; (e.g. checked out but abandoned mid-flight).
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning (format "llm/conn-pool: shutdown error: ~a"
                                                    (exn-message e))))])
    (custodian-shutdown-all (conn-pool-custodian pool))))

;; ============================================================
;; Minimal HTTP/1.1 client over pooled connections
;; ============================================================

;; Header names the pool owns itself; caller-supplied duplicates are dropped.
(define reserved-header-rx #rx"(?i:^(host|connection|content-length|transfer-encoding):)")

;; Read one CRLF/LF-terminated header line with a timeout. Returns the line
;; without terminator, or #f on EOF/timeout.
(define (read-head-line in timeout-secs)
  (define r (sync/timeout timeout-secs (read-line-evt in 'return-linefeed)))
  (and (string? r) r))

;; ── Chunked-transfer decoding (BUG-0021) ──────────────────────────────────

;; Wrap the raw socket of a chunked response in a decoding input port.
;; RFC 7230 §4.1: repeated `<hex-size>[;ext]CRLF <data> CRLF` chunks,
;; terminated by a `0` chunk followed by optional trailer lines and a blank
;; line. The wrapper yields ONLY the logical body bytes; consuming the
;; terminating blank line marks the pooled connection reusable (the wire is
;; back at a clean message boundary). Any framing anomaly (EOF mid-chunk,
;; bad CRLF) faults the entry instead.
(define (make-chunked-input-port in entry)
  (define state (box 'read-size)) ; 'read-size | 'read-data | 'read-crlf | 'trailer | 'done
  (define chunk-left (box 0))
  (define (fault!)
    (mark-pool-fault! entry)
    (set-box! state 'done))
  (define (finish-clean!)
    (set-box! (pooled-connection-reusable?-box entry) #t)
    (set-box! state 'done))
  (define (skip-to-state! next)
    (set-box! state next))
  (define (read-one-chunk-header!)
    (define line (read-line in 'return-linefeed))
    (cond
      [(eof-object? line)
       (fault!)
       'done]
      ;; tolerate stray CRLF between chunks
      [(equal? line "") 'again]
      [else
       (define n (string->number (string-trim (car (regexp-split #rx";" line))) 16))
       (cond
         [(not n)
          (fault!)
          'done]
         [(zero? n)
          (skip-to-state! 'trailer)
          'again]
         [else
          (set-box! chunk-left n)
          (skip-to-state! 'read-data)
          'proceed])]))
  (define (read! dest)
    (let loop ()
      (case (unbox state)
        [(done) eof]
        [(read-size)
         (define r (read-one-chunk-header!))
         (if (eq? r 'done)
             eof
             (loop))]
        [(read-data)
         (define want (min (unbox chunk-left) (bytes-length dest)))
         (define got (read-bytes-avail! dest in 0 want))
         (cond
           [(or (eof-object? got) (not got) (zero? got))
            (fault!)
            eof]
           [else
            (set-box! chunk-left (- (unbox chunk-left) got))
            (when (zero? (unbox chunk-left))
              (skip-to-state! 'read-crlf))
            got])]
        [(read-crlf)
         (define b1 (read-byte in))
         (define b2 (read-byte in))
         (cond
           [(and (equal? b1 13) (equal? b2 10))
            (skip-to-state! 'read-size)
            (loop)]
           [else
            ;; malformed chunk boundary: corrupt stream, do not trust/reuse
            (fault!)
            eof])]
        [(trailer)
         (define line (read-line in 'return-linefeed))
         (cond
           [(eof-object? line)
            (fault!)
            eof]
           [(equal? line "")
            (finish-clean!)
            eof]
           [else (loop)])])))
  (make-input-port (format "conn-pool-chunked:~a:~a"
                           (pooled-connection-host entry)
                           (pooled-connection-port-number entry))
                   read!
                   #f ; no peek support — SSE readers never peek
                   (lambda () (set-box! state 'done)))) ; close: pool owns the socket

;; Send an HTTP/1.1 request over a pooled connection and parse the response
;; head. Returns (values status-line header-lines body-port). The body port is
;; wrapped in a limited input port when Content-Length framing is present —
;; consuming it fully leaves the connection cleanly reusable. Any failure
;; marks the connection as faulted before re-raising.
(define (pool-send-request! entry
                            path-str
                            #:method [method "POST"]
                            #:headers [headers '()]
                            #:data [data #""]
                            #:head-timeout [head-timeout 10])
  (with-handlers ([exn:fail? (lambda (e)
                               (mark-pool-fault! entry)
                               (raise e))])
    (define out (pooled-connection-out entry))
    (define in (pooled-connection-in entry))
    ;; --- request head + body ---
    (fprintf out "~a ~a HTTP/1.1\r\n" method path-str)
    (fprintf out "Host: ~a\r\n" (pooled-connection-host entry))
    (fprintf out "Connection: keep-alive\r\n")
    (fprintf out "Content-Length: ~a\r\n" (bytes-length data))
    (for ([h (in-list headers)]
          #:unless (regexp-match? reserved-header-rx (string-trim h)))
      (display h out)
      (display "\r\n" out))
    (display "\r\n" out)
    (unless (zero? (bytes-length data))
      (write-bytes data out))
    (flush-output out)
    ;; --- response head ---
    (define status-line (read-head-line in head-timeout))
    (unless status-line
      (raise (exn:fail:network "llm/conn-pool: no response status line from peer"
                               (current-continuation-marks))))
    (define header-lines
      (let loop ([acc '()])
        (define line (read-head-line in head-timeout))
        (cond
          [(not line)
           (raise (exn:fail:network "llm/conn-pool: peer closed during response headers"
                                    (current-continuation-marks)))]
          [(string=? line "") (reverse acc)]
          [else (loop (cons line acc))])))
    ;; --- body framing / reuse decision ---
    (define chunked?
      (for/or ([h (in-list header-lines)])
        (regexp-match? #rx"(?i:^transfer-encoding:[^\r\n]*chunked)" h)))
    (define content-length
      (and (not chunked?)
           (for/or ([h (in-list header-lines)])
             (define m (regexp-match #rx"(?i:^content-length:[ \t]*([0-9]+))" h))
             (and m (string->number (cadr m))))))
    (cond
      [content-length
       ;; Deterministic framing: fully consuming the limited port leaves the
       ;; underlying connection cleanly reusable for the next request.
       (set-box! (pooled-connection-reusable?-box entry) #t)
       (values status-line header-lines (make-limited-input-port in content-length #f))]
      ;; BUG-0021: chunked bodies MUST be decoded before the SSE parser sees
      ;; them — returning the raw socket spliced hex chunk sizes into data
      ;; lines at every TCP write boundary ("model typos"). The de-chunking
      ;; wrapper consumes the framing; consuming its terminating 0-chunk +
      ;; trailers marks the connection reusable.
      [chunked? (values status-line header-lines (make-chunked-input-port in entry))]
      [else
       ;; EOF-delimited: connection state after an incompletely consumed body
       ;; is unknown → single-use-on-success (W2 limitation).
       (set-box! (pooled-connection-reusable?-box entry) #f)
       (values status-line header-lines in)])))
