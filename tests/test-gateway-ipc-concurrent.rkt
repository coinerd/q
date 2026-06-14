#lang racket/base

;; tests/test-gateway-ipc-concurrent.rkt
;; v0.99.3 W0: Concurrent regression tests for C1-C4 fixes.

(require rackunit
         rackunit/text-ui
         racket/async-channel
         (only-in racket/list remove-duplicates)
         "../sandbox/ipc-protocol.rkt"
         "../sandbox/gateway-ipc.rkt")

(define mock-path "tests/mock-worker.rkt")

(define (start-mock [mode "echo"])
  (start-worker! "racket" (list "-tm" mock-path mode) #f))

(define (mk-req [id #f] [tool "echo"])
  (ipc-request (or id (generate-request-id)) tool (hasheq) 5000 #f 'any IPC-SCHEMA-VERSION))

(define suite
  (test-suite "Gateway IPC Concurrent (v0.99.3 C1-C4)"

    ;; C1: Unique request IDs under concurrency
    (test-case "C1: 1000 request-IDs across 10 threads are unique"
      (define ids (box '()))
      (define lock (make-semaphore 1))
      (for-each
       thread-wait
       (for/list ([_ (in-range 10)])
         (thread (lambda ()
                   (for ([_ (in-range 100)])
                     (define id (generate-request-id))
                     (call-with-semaphore lock (lambda () (set-box! ids (cons id (unbox ids))))))))))
      (check-equal? (length (unbox ids)) 1000)
      (check-equal? (length (remove-duplicates (unbox ids))) 1000))

    ;; C2: Drain thread survives timeout
    (test-case "C2: timeout doesn't block drain thread"
      (define gw #f)
      (dynamic-wind (lambda () (set! gw (start-mock "delay:2")))
                    (lambda ()
                      (sleep 0.2)
                      (define resp1 (send-request! gw (mk-req "timeout-req") 200))
                      (check-equal? (ipc-response-status resp1) 'timeout)
                      (define resp2 (send-request! gw (mk-req "second-req") 5000))
                      (check-equal? (ipc-response-status resp2) 'ok))
                    (lambda ()
                      (when (and gw (gateway-alive? gw))
                        (gateway-shutdown! gw)))))

    ;; C3: Concurrent writes don't corrupt
    (test-case "C3: 20 concurrent requests all correct"
      (define gw #f)
      (dynamic-wind (lambda () (set! gw (start-mock "echo")))
                    (lambda ()
                      (sleep 0.2)
                      (define results (box (make-hash)))
                      (define lock (make-semaphore 1))
                      (for-each thread-wait
                                (for/list ([i (in-range 20)])
                                  (thread (lambda ()
                                            (define req-id (format "c3-~a" i))
                                            (define resp (send-request! gw (mk-req req-id) 10000))
                                            (call-with-semaphore
                                             lock
                                             (lambda () (hash-set! (unbox results) req-id resp)))))))
                      (check-equal? (hash-count (unbox results)) 20)
                      (for ([(id resp) (in-hash (unbox results))])
                        (check-equal? (ipc-response-status resp) 'ok)
                        (check-equal? (ipc-response-request-id resp) id)))
                    (lambda ()
                      (when (and gw (gateway-alive? gw))
                        (gateway-shutdown! gw)))))

    ;; C4: Worker crash notifies pending requests
    (test-case "C4: crash notifies pending within 5s"
      (define gw #f)
      (dynamic-wind (lambda () (set! gw (start-mock "slow")))
                    (lambda ()
                      (sleep 0.2)
                      (define ch (make-async-channel))
                      (register-pending-request! gw "crash-pending" ch)
                      (subprocess-kill (gateway-worker-process gw) #t)
                      (define result (sync/timeout 5 ch))
                      (check-not-false result "should receive error within 5s"))
                    (lambda ()
                      (when (and gw (gateway-alive? gw))
                        (gateway-shutdown! gw)))))

    ;; C4b: crash mode worker
    (test-case "C4b: crash-mode worker triggers error or timeout"
      (define gw #f)
      (dynamic-wind (lambda () (set! gw (start-mock "crash")))
                    (lambda ()
                      (sleep 0.3)
                      (define resp (send-request! gw (mk-req "crash-mode") 5000))
                      (check-not-false (memq (ipc-response-status resp) '(timeout error))))
                    (lambda ()
                      (when (and gw (gateway-alive? gw))
                        (gateway-shutdown! gw)))))

    ;; L1: stderr capped
    (test-case "L1: stderr capped at 64KB"
      (define gw #f)
      (dynamic-wind (lambda () (set! gw (start-mock "stderr")))
                    (lambda ()
                      (for ([i (in-range 50)])
                        (with-handlers ([exn:fail? void])
                          (send-request! gw (mk-req (format "se-~a" i)) 5000)))
                      (check-true (<= (string-length (gateway-stderr gw)) 65536)))
                    (lambda ()
                      (when (and gw (gateway-alive? gw))
                        (gateway-shutdown! gw)))))

    ;; Stress test
    (test-case "stress: 50 concurrent round-trips"
      (define gw #f)
      (dynamic-wind
       (lambda () (set! gw (start-mock "echo")))
       (lambda ()
         (sleep 0.2)
         (define ok-count (box 0))
         (define lock (make-semaphore 1))
         (for-each thread-wait
                   (for/list ([t (in-range 10)])
                     (thread (lambda ()
                               (for ([r (in-range 5)])
                                 (define req-id (format "s-~a-~a" t r))
                                 (define resp (send-request! gw (mk-req req-id) 10000))
                                 (when (and (eq? (ipc-response-status resp) 'ok)
                                            (equal? (ipc-response-request-id resp) req-id))
                                   (call-with-semaphore
                                    lock
                                    (lambda () (set-box! ok-count (add1 (unbox ok-count)))))))))))
         (check-equal? (unbox ok-count) 50))
       (lambda ()
         (when (and gw (gateway-alive? gw))
           (gateway-shutdown! gw)))))

    ;; C3: stdin-write-lock field exists
    (test-case "C3: stdin-write-lock accessor exists"
      (check-true (procedure? gateway-worker-stdin-write-lock)))

    ;; L8: working-directory parameter
    (test-case "L8: start-worker! accepts working-directory"
      (define gw #f)
      (dynamic-wind (lambda ()
                      (set! gw (start-worker! "racket" (list "-tm" mock-path "echo") "/tmp")))
                    (lambda ()
                      (sleep 0.2)
                      (check-true (gateway-alive? gw)))
                    (lambda ()
                      (when (and gw (gateway-alive? gw))
                        (gateway-shutdown! gw)))))

    ;; C1: IDs have correct format
    (test-case "C1: request-IDs have req-TIMESTAMP-COUNTER format"
      (for ([_ (in-range 5)])
        (check-true (regexp-match? #rx"^req-[0-9.]+-[0-9]+$" (generate-request-id)))))

    ;; C2: async-channel is non-blocking
    (test-case "C2: async-channel-put does not block without receiver"
      (define ch (make-async-channel))
      (async-channel-put ch 'test)
      (check-equal? (async-channel-get ch) 'test))

    ;; C4: shutdown clears pending
    (test-case "C4: shutdown clears pending requests"
      (define gw #f)
      (define cleaned #f)
      (dynamic-wind (lambda () (set! gw (start-mock "echo")))
                    (lambda ()
                      (sleep 0.2)
                      (define ch (make-async-channel))
                      (register-pending-request! gw "pend-shutdown" ch)
                      (gateway-shutdown! gw)
                      (define result (sync/timeout 2 ch))
                      (set! cleaned (and result #t))
                      (set! gw #f))
                    (lambda ()
                      (when (and gw (gateway-alive? gw))
                        (gateway-shutdown! gw))))
      (check-true cleaned))))

(run-tests suite)
