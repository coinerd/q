#lang racket

;; @speed fast
;; @suite provider
;; @boundary unit

;; tests/test-stream-liveness-classification.rkt
;; v1.00.13 (RL-8): heartbeat-aware held-request classification.
;;
;;   zero data + zero heartbeat + initial timeout = held/dead peer (unchanged)
;;   heartbeat-only initial stall = live-but-no-content, NOT a held request
;;   mid-stream stalls remain retryable regardless of heartbeat state
;;
;; Split from tests/test-network-failure-context.rkt in W3; promoted green
;; with the W4 liveness + deadline completion (#9478). Also carries the W4
;; blocking-phase matrix: connect/TTFB bound, hard remaining-budget reads,
;; and cancellation responsiveness.

(require rackunit
         (only-in "../llm/stream.rkt"
                  exn:fail:network:timeout:stream?
                  exn:fail:network:timeout:stream-phase))

(define held-request? (dynamic-require '"../runtime/auto-retry.rkt" 'held-request?))

(define (stream-timeout heartbeats? data? phase chars)
  ((dynamic-require '"../llm/stream.rkt" 'exn:fail:network:timeout:stream)
   "HTTP read timeout"
   (current-continuation-marks)
   heartbeats?
   data?
   phase
   chars))

(test-case "zero-liveness initial stall remains a held request"
  (check-true (held-request? (stream-timeout #f #f 'initial 0))))

(test-case "heartbeat-only initial stall is live-but-no-content, not a held request"
  (check-false (held-request? (stream-timeout #t #f 'initial 0))
               "RL-8: heartbeat-only stream must not be classified as held request"))

(test-case "mid-stream stalls remain retryable regardless of heartbeat state"
  (check-false (held-request? (stream-timeout #f #t 'thinking 40)))
  (check-false (held-request? (stream-timeout #t #t 'content 900))))

;; ============================================================
;; W4 matrix: connect/TTFB, total deadline, cancellation
;; ============================================================

(require racket/tcp
         (only-in "../llm/request-policy.rkt"
                  resolve-request-network-policy
                  request-network-policy-connect-ttfb-secs
                  request-network-policy-request-budget-secs))

;; A local peer that accepts the request and NEVER sends response headers —
;; the established-but-silent connection of RL-4.
(define (with-silent-peer thunk)
  (define listener (tcp-listen 0 4 #t "127.0.0.1"))
  (define-values (_h p _rh _rp) (tcp-addresses listener #t))
  (define server
    (thread (lambda ()
              (with-handlers ([exn:fail? (lambda (_) (void))])
                (define-values (in out) (tcp-accept listener))
                ;; drain the request; never respond
                (let loop ()
                  (unless (eof-object? (read-line in 'any))
                    (loop)))))))
  (dynamic-wind (lambda () (void))
                (lambda () (thunk (format "http://127.0.0.1:~a" p)))
                (lambda ()
                  (tcp-close listener)
                  (unless (thread-dead? server)
                    (kill-thread server)))))

(test-case "connect/TTFB: silent peer bounded by the dedicated window"
  (define policy (resolve-request-network-policy #:request-timeout 900))
  ;; the bound is the policy value (min request 120) — NOT the 900 s budget
  (check-true (<= (request-network-policy-connect-ttfb-secs policy) 120))
  (check-true (< (request-network-policy-connect-ttfb-secs policy)
                 (request-network-policy-request-budget-secs policy)))
  (define ttfb (dynamic-require '"../llm/http-helpers.rkt" 'provider-sendrecv/ttfb-bounded))
  (define parse-url (dynamic-require '"../llm/http-helpers.rkt" 'parse-provider-url))
  (define seam (dynamic-require '"../llm/http-helpers.rkt" 'current-provider-http-sendrecv))
  (with-silent-peer
   (lambda (url)
     (define-values (h pth prt ssl?) (parse-url (string-append url "/x")))
     (define t0 (current-inexact-milliseconds))
     (define exn-raised
       (with-handlers ([exn:fail:network:timeout:stream? (lambda (e) e)])
         (ttfb 1
               (lambda ()
                 ((seam) h pth #:port prt #:ssl? ssl? #:method #"POST" #:headers '() #:data #"{}"))
               #:cleanup (lambda () (void)))
         #f))
     (define elapsed (/ (- (current-inexact-milliseconds) t0) 1000.0))
     (check-not-false exn-raised "silent peer must trip the connect/TTFB bound")
     (when exn-raised
       (check-eq? (exn:fail:network:timeout:stream-phase exn-raised) 'connect/ttfb))
     (check-true (< elapsed 5) "bounded well below the request budget"))))

(test-case "total deadline: a blocking read cannot overshoot by a phase window"
  ;; max-total 0.5 s with a 60 s phase window — the raise must arrive by the
  ;; TOTAL deadline (well under the phase window), proving the per-read wait
  ;; is capped at the remaining budget.
  (define listener (tcp-listen 0 4 #t "127.0.0.1"))
  (define-values (_h p _rh _rp) (tcp-addresses listener #t))
  (define server
    (thread (lambda ()
              (with-handlers ([exn:fail? (lambda (_) (void))])
                (define-values (in out) (tcp-accept listener))
                (void out)
                (let loop ()
                  (unless (eof-object? (read-line in 'any))
                    (loop)))))))
  (define-values (cin _cout) (tcp-connect "127.0.0.1" p))
  (define gen
    ((dynamic-require '"../llm/stream.rkt" 'stream-sse-events) cin
                                                               (lambda (parsed) '())
                                                               #:initial-timeout 60
                                                               #:thinking-timeout 60
                                                               #:stream-timeout 60
                                                               #:max-total-timeout 0.5))
  (define t0 (current-inexact-milliseconds))
  (define exn-raised
    (with-handlers ([exn:fail:network:timeout:stream? (lambda (e) e)])
      (gen)
      #f))
  (define elapsed (/ (- (current-inexact-milliseconds) t0) 1000.0))
  (check-not-false exn-raised "the total deadline must fire")
  (check-true (< elapsed 5)
              (format "total deadline must bound blocking reads (elapsed ~as, not 60 s phase window)"
                      elapsed))
  (unless (thread-dead? server)
    (kill-thread server)))

(test-case "cancellation: breaking the consumer is responsive, not a timeout"
  ;; A consumer thread blocked in the stream generator is broken; it must
  ;; unwind via break — never reclassified as a stream timeout.
  (define listener (tcp-listen 0 4 #t "127.0.0.1"))
  (define-values (_h p _rh _rp) (tcp-addresses listener #t))
  (define-values (cin _cout) (tcp-connect "127.0.0.1" p))
  (define-values (pin _pout) (tcp-accept listener))
  (void pin)
  (tcp-close listener)
  (define gen
    ((dynamic-require '"../llm/stream.rkt" 'close-port-after-stream)
     ((dynamic-require '"../llm/stream.rkt" 'stream-sse-events) cin
                                                                (lambda (parsed) '())
                                                                #:initial-timeout 30
                                                                #:max-total-timeout 30)
     cin))
  (define result (box 'pending))
  (define t
    (thread (lambda ()
              (with-handlers ([exn:break? (lambda (_) (set-box! result 'break))]
                              [exn:fail? (lambda (e) (set-box! result (list 'fail (exn-message e))))])
                (gen)
                (set-box! result 'completed)))))
  (sleep 0.3)
  (break-thread t)
  (thread-wait t)
  (check-eq? (unbox result) 'break "cancellation must surface as break, not timeout"))
