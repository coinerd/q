#lang racket/base

;; tests/test-conn-pool.rkt — BUG-0019 W2 acceptance tests.
;;
;; Coverage (BUG-0019 wave W2; see .planning plan):
;;   1. acquire/release roundtrip: fresh connect is a miss; a connection
;;      whose response had Content-Length framing checks in and the next
;;      acquire is a HIT on the same socket
;;   2. TTL eviction: idle entries older than idle-ttl-secs are discarded
;;   3. max-per-host cap: surplus healthy connections close at check-in
;;   4. single-use-on-fault: faulted entries are never handed out again
;;   5. wiring: flag absent/false keeps current-conn-pool #f (passthrough);
;;      flag true installs a pool with configured bounds
;;   6. fd-leak stress: 100 sequential pooled request cycles + shutdown
;;      return the open-fd count to baseline
;;   7. BUG-0021: chunked-transfer decoding across hostile chunk splits
;;   8. BUG-0022 W1B: stale keep-alive reuse — a request that dies at the
;;      status-line read with ZERO response bytes on a REUSED entry is
;;      transparently retried exactly once on a fresh connection; fresh
;;      first-use failures and failures after partial response bytes stay
;;      surfaced (never retried); per-host idle-TTL overrides
;;      (networking.pool.host-idle-ttl) beat the global TTL.

(require rackunit
         racket/tcp
         racket/format
         racket/port
         racket/string
         (only-in json read-json)
         (only-in "../runtime/settings.rkt" q-settings)
         (only-in "../llm/conn-pool.rkt"
                  current-conn-pool
                  make-conn-pool
                  conn-pool?
                  pool-acquire!
                  pool-release!
                  pool-evict-idle!
                  pool-shutdown!
                  pool-stats-for
                  pool-stats-hits
                  pool-stats-misses
                  pool-stats-evictions
                  pool-stats-fault-closes
                  pool-stats-stale-reuse-retries
                  pool-send-request!
                  mark-pool-fault!
                  mark-pool-reusable!
                  make-chunked-input-port
                  pooled-connection-in)
         (only-in "../wiring/mode-helpers.rkt" wire-connection-pool!))

;; ---- tiny HTTP server standing in for a provider --------------------------
;; Serves up to `max-reqs-per-conn` keep-alive requests per accepted
;; connection, each answered with a Content-Length-framed body (deterministic
;; framing → the pool marks the connection reusable).

(define (make-test-server [max-reqs-per-conn 8])
  ;; Bind on a random high port, retrying on collision (same pattern as
  ;; tests/reproducers/mock-fin-server.rkt).
  (define-values (port-no l)
    (let retry ()
      (define candidate (+ 20000 (random 40000)))
      (with-handlers ([exn:fail:network? (lambda (_e) (retry))])
        (values candidate (tcp-listen candidate 16 #f "127.0.0.1")))))
  (define alive? (box #t))
  (define srv-cust (make-custodian))
  (parameterize ([current-custodian srv-cust])
    (thread (lambda ()
              (let accept-loop ()
                (with-handlers ([exn:fail? void])
                  (define-values (cin cout) (tcp-accept l))
                  (let serve ([n 0])
                    (when (< n max-reqs-per-conn)
                      (define req-line (read-line cin 'return-linefeed))
                      (unless (eof-object? req-line)
                        (let drain ()
                          (define hdr (read-line cin 'return-linefeed))
                          (unless (or (eof-object? hdr) (equal? hdr ""))
                            (drain)))
                        (display "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok" cout)
                        (flush-output cout)
                        (serve (add1 n)))))
                  (with-handlers ([exn:fail? void])
                    (close-input-port cin)
                    (close-output-port cout))
                  (when (unbox alive?)
                    (accept-loop)))))))
  (values port-no alive? srv-cust (lambda () (tcp-close l))))

(define (stop-test-server! alive?-box cust close-thunk)
  (set-box! alive?-box #f)
  (custodian-shutdown-all cust)
  (with-handlers ([exn:fail? void])
    (close-thunk)))

;; One GET roundtrip returning (list status-line body-string).
(define (get-via entry)
  (define-values (status headers body) (pool-send-request! entry "/" #:method "GET"))
  (list status (read-string 2 body)))

;; Count open file descriptors for this process (Linux).
(define (open-fd-count)
  (length (directory-list "/proc/self/fd")))

;; ---- 1. roundtrip ----------------------------------------------------------

(test-case "acquire/release roundtrip reuses the same connection"
  (define-values (pno alive? cust close!) (make-test-server))
  (dynamic-wind void
                (lambda ()
                  (define pool (make-conn-pool #:idle-ttl-secs 30 #:max-per-host 4))
                  ;; First acquire = fresh connect (miss).
                  (define e1 (pool-acquire! pool "127.0.0.1" pno #f))
                  (check-equal? (pool-stats-misses (pool-stats-for pool)) 1)
                  ;; Complete one Content-Length-framed exchange → reusable → checks in.
                  (check-equal? (get-via e1) '("HTTP/1.1 200 OK" "ok"))
                  (pool-release! pool e1)
                  ;; Second acquire = hit on the checked-in connection.
                  (define e2 (pool-acquire! pool "127.0.0.1" pno #f))
                  (define stats (pool-stats-for pool))
                  (check-equal? (pool-stats-hits stats) 1 "second acquire is a cache hit")
                  (check-equal? (get-via e2) '("HTTP/1.1 200 OK" "ok"))
                  (pool-release! pool e2)
                  (pool-shutdown! pool))
                (lambda () (stop-test-server! alive? cust close!))))

;; ---- 2. TTL eviction -------------------------------------------------------

(test-case "TTL eviction drops expired idle entries"
  (define-values (pno alive? cust close!) (make-test-server))
  (dynamic-wind void
                (lambda ()
                  (define pool (make-conn-pool #:idle-ttl-secs 1 #:max-per-host 4))
                  (define e1 (pool-acquire! pool "127.0.0.1" pno #f))
                  (mark-pool-reusable! e1)
                  (pool-release! pool e1)
                  (sleep 1.3)
                  (check-equal? (pool-evict-idle! pool) 1 "one expired idle entry evicted")
                  ;; Next acquire must be a fresh miss, not a stale hit.
                  (void (pool-acquire! pool "127.0.0.1" pno #f))
                  (define stats (pool-stats-for pool))
                  (check-equal? (pool-stats-hits stats) 0 "no hit after TTL expiry")
                  (check-equal? (pool-stats-misses stats) 2)
                  (pool-shutdown! pool))
                (lambda () (stop-test-server! alive? cust close!))))

;; ---- 3. max-per-host cap ---------------------------------------------------

(test-case "max-per-host cap closes surplus healthy connections at check-in"
  (define-values (pno alive? cust close!) (make-test-server))
  (dynamic-wind void
                (lambda ()
                  (define pool (make-conn-pool #:idle-ttl-secs 30 #:max-per-host 1))
                  ;; Two concurrent acquisitions = two live connections.
                  (define e1 (pool-acquire! pool "127.0.0.1" pno #f))
                  (define e2 (pool-acquire! pool "127.0.0.1" pno #f))
                  (mark-pool-reusable! e1)
                  (mark-pool-reusable! e2)
                  (pool-release! pool e1) ; checks in (bucket empty → kept)
                  (pool-release! pool e2) ; cap 1 exceeded → closed
                  (define stats (pool-stats-for pool))
                  (check-equal? (pool-stats-evictions stats) 1 "surplus healthy conn closed")
                  (pool-shutdown! pool))
                (lambda () (stop-test-server! alive? cust close!))))

;; ---- 4. single-use-on-fault -------------------------------------------------

(test-case "single-use-on-fault: faulted entry never handed out again"
  (define-values (pno alive? cust close!) (make-test-server))
  (dynamic-wind void
                (lambda ()
                  (define pool (make-conn-pool #:idle-ttl-secs 30 #:max-per-host 4))
                  (define e1 (pool-acquire! pool "127.0.0.1" pno #f))
                  (mark-pool-reusable! e1)
                  (mark-pool-fault! e1) ; error/timeout/peer-close seen
                  (pool-release! pool e1 #:outcome 'fault)
                  (define stats (pool-stats-for pool))
                  (check-equal? (pool-stats-fault-closes stats) 1 "faulted entry discarded")
                  ;; Next acquire cannot be a hit — nothing healthy was checked in.
                  (define e2 (pool-acquire! pool "127.0.0.1" pno #f))
                  (check-equal? (pool-stats-hits (pool-stats-for pool)) 0)
                  (check-not-eq? e2 e1)
                  (pool-release! pool e2)
                  (pool-shutdown! pool))
                (lambda () (stop-test-server! alive? cust close!))))

;; ---- 5. wiring --------------------------------------------------------------

(test-case "flag absent -> pooling stays disabled"
  (define s (q-settings (hash) (hash) (hash)))
  (parameterize ([current-conn-pool (make-conn-pool)])
    (wire-connection-pool! s)
    (check-false (current-conn-pool) "absent networking.pool.enabled resets to disabled")))

(test-case "flag false -> pooling stays disabled"
  (define s (q-settings (hash) (hash) (hasheq 'networking (hasheq 'pool (hasheq 'enabled #f)))))
  (wire-connection-pool! s)
  (check-false (current-conn-pool)))

(test-case "flag true -> pool installed with configured bounds"
  (define s
    (q-settings (hash)
                (hash)
                (hasheq 'networking
                        (hasheq 'pool (hasheq 'enabled #t 'idle-ttl-secs 7 'max-per-host 2)))))
  (wire-connection-pool! s)
  (check-pred conn-pool? (current-conn-pool))
  (pool-shutdown! (current-conn-pool))
  (current-conn-pool #f))

;; ---- 6. fd-leak stress ------------------------------------------------------

(test-case "100 sequential pooled cycles: fds return to baseline after shutdown"
  (define-values (pno alive? cust close!) (make-test-server))
  (dynamic-wind void
                (lambda ()
                  (collect-garbage)
                  (collect-garbage)
                  (define baseline (open-fd-count))
                  (define pool (make-conn-pool #:idle-ttl-secs 1 #:max-per-host 2))
                  (for ([_i (in-range 100)])
                    (define e (pool-acquire! pool "127.0.0.1" pno #f))
                    (with-handlers ([exn:fail? (lambda (_e) (pool-release! pool e #:outcome 'fault))])
                      (define-values (_s _h b) (pool-send-request! e "/" #:method "GET"))
                      (read-string 2 b)
                      (pool-release! pool e)))
                  (pool-shutdown! pool)
                  (collect-garbage)
                  (sleep 0.2)
                  (collect-garbage)
                  (check-true (<= (open-fd-count) (+ baseline 3))
                              "no fd leak: post-shutdown fds within noise of baseline"))
                (lambda () (stop-test-server! alive? cust close!))))

;; ---- 7. BUG-0021: chunked-transfer decoding -------------------------------

;; Server that answers each request with a CHUNKED body whose 7-byte chunks
;; split the logical payload mid-line and mid-token (worst case BUG-0021).
(define (make-chunked-test-server)
  (define-values (port-no l)
    (let retry ()
      (define candidate (+ 20000 (random 40000)))
      (with-handlers ([exn:fail:network? (lambda (_e) (retry))])
        (values candidate (tcp-listen candidate 16 #f "127.0.0.1")))))
  (define alive? (box #t))
  (define srv-cust (make-custodian))
  (parameterize ([current-custodian srv-cust])
    (thread (lambda ()
              (let accept-loop ()
                (with-handlers ([exn:fail? void])
                  (define-values (cin cout) (tcp-accept l))
                  ;; read request head, then emit chunked reply
                  (let drain ()
                    (define hdr (read-line cin 'return-linefeed))
                    (unless (or (eof-object? hdr) (equal? hdr ""))
                      (drain)))
                  (define body
                    (string-append
                     "data: {\"path\": \"/opt/ci/fixture/.planning/waves/W4.md\", "
                     "\"note\": \"chunk boundaries must never corrupt tool arguments\"}\r\n\r\n"
                     "data: [DONE]\r\n\r\n"))
                  (display "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n" cout)
                  (for ([chunk (in-list (for/list ([i (in-range 0 (string-length body) 7)])
                                          (substring body i (min (+ i 7) (string-length body)))))])
                    (fprintf cout "~x\r\n" (string-length chunk))
                    (display chunk cout)
                    (display "\r\n" cout)
                    (flush-output cout))
                  (display "0\r\n\r\n" cout)
                  (flush-output cout)
                  (with-handlers ([exn:fail? void])
                    (close-input-port cin)
                    (close-output-port cout))
                  (when (unbox alive?)
                    (accept-loop)))))))
  (values port-no alive? srv-cust (lambda () (tcp-close l))))

(test-case "BUG-0021: chunked bodies decode byte-identically across chunk splits"
  (define-values (pno alive? cust close!) (make-chunked-test-server))
  (dynamic-wind
   void
   (lambda ()
     (define pool (make-conn-pool #:idle-ttl-secs 30 #:max-per-host 2))
     (define e1 (pool-acquire! pool "127.0.0.1" pno #f))
     (define-values (_s _h decoded) (pool-send-request! e1 "/" #:method "GET"))
     ;; The de-chunked stream must equal the LOGICAL body — no hex sizes,
     ;; no spliced CRLFs, nothing lost at the 7-byte chunk borders.
     (define logical
       (string-append "data: {\"path\": \"/opt/ci/fixture/.planning/waves/W4.md\", "
                      "\"note\": \"chunk boundaries must never corrupt tool arguments\"}\r\n\r\n"
                      "data: [DONE]\r\n\r\n"))
     (check-equal? (port->string decoded) logical)
     ;; Consuming the terminating 0-chunk marks the entry reusable, so the
     ;; release checks it in and the next acquire is a HIT (same socket —
     ;; entries are struct-copies with fresh boxes, so compare the ports).
     (pool-release! pool e1)
     (define e2 (pool-acquire! pool "127.0.0.1" pno #f))
     (check-equal? (pool-stats-hits (pool-stats-for pool))
                   1
                   "cleanly terminated chunked body was checked in")
     (check-equal? (pooled-connection-in e2)
                   (pooled-connection-in e1)
                   "hit returned the same underlying socket")
     (pool-shutdown! pool))
   (lambda () (stop-test-server! alive? cust close!))))

(test-case "BUG-0021: SSE parse over de-chunked stream yields clean JSON lines"
  (define-values (pno alive? cust close!) (make-chunked-test-server))
  (dynamic-wind void
                (lambda ()
                  (define pool (make-conn-pool #:idle-ttl-secs 30 #:max-per-host 2))
                  (define e1 (pool-acquire! pool "127.0.0.1" pno #f))
                  (define-values (_s _h decoded) (pool-send-request! e1 "/" #:method "GET"))
                  (define data-lines
                    (for/list ([line (in-lines decoded)]
                               #:when (string-prefix? line "data: "))
                      (substring line 6)))
                  (check-equal? (length data-lines) 2)
                  ;; first line parses as JSON with the path intact (pre-fix this broke)
                  (define parsed (with-input-from-string (car data-lines) read-json))
                  (check-equal? (hash-ref parsed 'path) "/opt/ci/fixture/.planning/waves/W4.md")
                  (check-equal? (cadr data-lines) "[DONE]")
                  (pool-shutdown! pool))
                (lambda () (stop-test-server! alive? cust close!))))

;; ---- 8. BUG-0022 W1B: stale-reuse transparent retry ------------------------

;; Shared request-head reader for the W1B mock servers: consumes the request
;; line plus all headers (through the blank line), so a subsequent server-side
;; close has no unread data (clean FIN — no RST race against the client).
(define (drain-request! cin)
  (read-line cin 'return-linefeed)
  (let drain ()
    (define hdr (read-line cin 'return-linefeed))
    (unless (or (eof-object? hdr) (equal? hdr ""))
      (drain))))

(define (serve-content-length-ok! cout)
  (display "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok" cout)
  (flush-output cout))

;; Server emulating an aggressive keep-alive closer (the z.ai profile): the
;; FIRST accepted connection serves exactly one request and is then
;; server-closed while idle — but via a half-close (output closed, input held
;; ~3 s) so the pooled client's next write lands in the dead socket without
;; an RST and its status-line read deterministically sees the zero-byte EOF
;; (the exact live BUG-0022 signature). Every LATER connection serves normal
;; keep-alive requests.
(define (make-stale-after-first-server)
  (define-values (port-no l)
    (let retry ()
      (define candidate (+ 20000 (random 40000)))
      (with-handlers ([exn:fail:network? (lambda (_e) (retry))])
        (values candidate (tcp-listen candidate 16 #f "127.0.0.1")))))
  (define alive? (box #t))
  (define srv-cust (make-custodian))
  (parameterize ([current-custodian srv-cust])
    (thread (lambda ()
              (let accept-loop ([first-conn? #t])
                (with-handlers ([exn:fail? void])
                  (define-values (cin cout) (tcp-accept l))
                  (cond
                    [first-conn?
                     (drain-request! cin)
                     (serve-content-length-ok! cout)
                     ;; FIN while the entry sits idle in the pool. Keep the input
                     ;; side open briefly so the client's follow-up write into the
                     ;; dead socket yields a clean read-side EOF, not a reset.
                     (close-output-port cout)
                     (thread (lambda ()
                               (sleep 3)
                               (with-handlers ([exn:fail? void])
                                 (close-input-port cin))))
                     (accept-loop #f)]
                    [else
                     (let serve ([n 0])
                       (when (< n 8)
                         (drain-request! cin)
                         (serve-content-length-ok! cout)
                         (serve (add1 n))))
                     (with-handlers ([exn:fail? void])
                       (close-input-port cin)
                       (close-output-port cout))
                     (when (unbox alive?)
                       (accept-loop #f))]))))))
  (values port-no alive? srv-cust (lambda () (tcp-close l))))

(test-case "BUG-0022 W1B: stale reused entry transparently retried exactly once"
  (define-values (pno alive? cust close!) (make-stale-after-first-server))
  (dynamic-wind
   void
   (lambda ()
     (define pool (make-conn-pool #:idle-ttl-secs 30 #:max-per-host 4))
     (define e1 (pool-acquire! pool "127.0.0.1" pno #f))
     (check-equal? (get-via e1) '("HTTP/1.1 200 OK" "ok"))
     (pool-release! pool e1) ; checked in idle; server closes it right away
     ;; HIT on the now server-closed socket — handed out verbatim.
     (define e2 (pool-acquire! pool "127.0.0.1" pno #f))
     (check-equal? (pool-stats-hits (pool-stats-for pool)) 1)
     ;; The request write goes into the dead socket; the status
     ;; read sees EOF with zero response bytes. This call must
     ;; NOT raise: the pool faults the dead socket, dials a
     ;; fresh connection and replays the identical request once.
     (check-equal? (get-via e2)
                   '("HTTP/1.1 200 OK" "ok")
                   "stale keep-alive reuse is transparent to the caller")
     (define stats (pool-stats-for pool))
     (check-equal? (pool-stats-stale-reuse-retries stats) 1 "exactly one stale-reuse retry recorded")
     ;; The entry handle was spliced onto the fresh socket, so
     ;; releasing it checks in a LIVE connection — nothing was
     ;; fault-closed and the retry itself was invisible.
     (check-equal? (pool-stats-fault-closes stats)
                   0
                   "transparent retry faulted nothing the caller owns")
     (pool-release! pool e2)
     (check-equal? (pool-stats-fault-closes (pool-stats-for pool)) 0)
     (pool-shutdown! pool))
   (lambda () (stop-test-server! alive? cust close!))))

;; Server that drains each request head and closes the connection WITHOUT
;; answering — cleanly (no unread data ⇒ FIN, not RST), so the client's
;; status-line read deterministically sees a zero-byte EOF.
(define (make-silent-close-server)
  (define-values (port-no l)
    (let retry ()
      (define candidate (+ 20000 (random 40000)))
      (with-handlers ([exn:fail:network? (lambda (_e) (retry))])
        (values candidate (tcp-listen candidate 16 #f "127.0.0.1")))))
  (define alive? (box #t))
  (define srv-cust (make-custodian))
  (parameterize ([current-custodian srv-cust])
    (thread (lambda ()
              (let accept-loop ()
                (with-handlers ([exn:fail? void])
                  (define-values (cin cout) (tcp-accept l))
                  (drain-request! cin)
                  (with-handlers ([exn:fail? void])
                    (close-input-port cin)
                    (close-output-port cout))
                  (when (unbox alive?)
                    (accept-loop)))))))
  (values port-no alive? srv-cust (lambda () (tcp-close l))))

(test-case "BUG-0022 W1B: fresh first-use failure is NOT transparently retried"
  (define-values (pno alive? cust close!) (make-silent-close-server))
  (dynamic-wind void
                (lambda ()
                  (define pool (make-conn-pool #:idle-ttl-secs 30 #:max-per-host 4))
                  ;; Freshly dialed (miss): a zero-byte status-line failure is
                  ;; a genuine server-side problem and MUST stay surfaced.
                  (define e1 (pool-acquire! pool "127.0.0.1" pno #f))
                  (check-equal? (pool-stats-misses (pool-stats-for pool)) 1)
                  (define msg (box #f))
                  (with-handlers ([exn:fail:network? (lambda (e) (set-box! msg (exn-message e)))])
                    (void (get-via e1)))
                  (check-pred string? (unbox msg) "fresh-connection failure surfaced")
                  (check-true (string-contains? (unbox msg) "no response status line from peer"))
                  (check-equal? (pool-stats-stale-reuse-retries (pool-stats-for pool))
                                0
                                "fresh first-use failure never consumes the stale retry")
                  (pool-release! pool e1 #:outcome 'fault)
                  (pool-shutdown! pool))
                (lambda () (stop-test-server! alive? cust close!))))

;; Server whose SECOND request on each connection gets a PARTIAL status line
;; ("HTTP/1.1 20", no CRLF) followed by a clean close: the client receives
;; response bytes, so the failure is past the status-line read and must NOT
;; be transparently retried — retrying could duplicate a processed request.
(define (make-partial-status-server)
  (define-values (port-no l)
    (let retry ()
      (define candidate (+ 20000 (random 40000)))
      (with-handlers ([exn:fail:network? (lambda (_e) (retry))])
        (values candidate (tcp-listen candidate 16 #f "127.0.0.1")))))
  (define alive? (box #t))
  (define srv-cust (make-custodian))
  (parameterize ([current-custodian srv-cust])
    (thread (lambda ()
              (let accept-loop ()
                (with-handlers ([exn:fail? void])
                  (define-values (cin cout) (tcp-accept l))
                  (drain-request! cin)
                  (serve-content-length-ok! cout) ; request 1: clean keep-alive
                  (drain-request! cin) ; request 2 on the reused conn
                  (display "HTTP/1.1 20" cout) ; partial status line, no CRLF
                  (flush-output cout)
                  (with-handlers ([exn:fail? void])
                    (close-input-port cin)
                    (close-output-port cout))
                  (when (unbox alive?)
                    (accept-loop)))))))
  (values port-no alive? srv-cust (lambda () (tcp-close l))))

(test-case "BUG-0022 W1B: failure after partial response bytes is NOT retried"
  (define-values (pno alive? cust close!) (make-partial-status-server))
  (dynamic-wind void
                (lambda ()
                  (define pool (make-conn-pool #:idle-ttl-secs 30 #:max-per-host 4))
                  (define e1 (pool-acquire! pool "127.0.0.1" pno #f))
                  (check-equal? (get-via e1) '("HTTP/1.1 200 OK" "ok"))
                  (pool-release! pool e1)
                  ;; HIT → reused entry, but the server answers the second
                  ;; request with a truncated head: bytes WERE received, so
                  ;; this is a surfaced mid-response failure, not a stale
                  ;; keep-alive.
                  (define e2 (pool-acquire! pool "127.0.0.1" pno #f))
                  (check-equal? (pool-stats-hits (pool-stats-for pool)) 1)
                  (define msg (box #f))
                  (with-handlers ([exn:fail:network? (lambda (e) (set-box! msg (exn-message e)))])
                    (void (get-via e2)))
                  (check-pred string? (unbox msg) "partial-response failure surfaced")
                  (check-true (string-contains? (unbox msg) "peer closed during response headers"))
                  (check-equal? (pool-stats-stale-reuse-retries (pool-stats-for pool))
                                0
                                "post-byte failures never trigger the transparent retry")
                  (pool-release! pool e2 #:outcome 'fault)
                  (pool-shutdown! pool))
                (lambda () (stop-test-server! alive? cust close!))))

(test-case "BUG-0022 W1B: per-host idle-TTL override beats the global TTL"
  (define-values (pno alive? cust close!) (make-test-server))
  (dynamic-wind void
                (lambda ()
                  ;; Global TTL 30 s, but 127.0.0.1 pinned to 1 s (the
                  ;; api.z.ai profile); a non-positive override is dropped
                  ;; during normalization and must not poison the map.
                  (define pool
                    (make-conn-pool #:idle-ttl-secs 30
                                    #:max-per-host 4
                                    #:host-idle-ttl (hasheq "127.0.0.1" 1 "bad.example" 0)))
                  (define e1 (pool-acquire! pool "127.0.0.1" pno #f))
                  (mark-pool-reusable! e1)
                  (pool-release! pool e1)
                  (sleep 1.3)
                  (check-equal? (pool-evict-idle! pool)
                                1
                                "per-host override (1 s) evicts despite the 30 s global TTL")
                  (pool-shutdown! pool))
                (lambda () (stop-test-server! alive? cust close!))))

(test-case "wire-connection-pool!: networking.pool.host-idle-ttl installs overrides"
  (define-values (pno alive? cust close!) (make-test-server))
  (dynamic-wind
   void
   (lambda ()
     (define s
       (q-settings
        (hash)
        (hash)
        (hasheq
         'networking
         (hasheq 'pool
                 (hasheq 'enabled #t 'idle-ttl-secs 30 'host-idle-ttl (hasheq "127.0.0.1" 1))))))
     (wire-connection-pool! s)
     (define pool (current-conn-pool))
     (check-pred conn-pool? pool)
     (define e1 (pool-acquire! pool "127.0.0.1" pno #f))
     (mark-pool-reusable! e1)
     (pool-release! pool e1)
     (sleep 1.3)
     (check-equal? (pool-evict-idle! pool) 1 "wired host override pins 127.0.0.1 to a 1 s idle TTL")
     (pool-shutdown! pool)
     (current-conn-pool #f))
   (lambda () (stop-test-server! alive? cust close!))))
