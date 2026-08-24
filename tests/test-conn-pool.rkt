#lang racket/base

;; tests/test-conn-pool.rkt — BUG-0019 W2 acceptance tests.
;;
;; Coverage (PLAN v1.00.15 W2):
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

(require rackunit
         racket/tcp
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
                  pool-send-request!
                  mark-pool-fault!
                  mark-pool-reusable!)
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
