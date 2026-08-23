#lang racket/base

;; tests/test-stream-peer-close.rkt — BUG-0019 W1 acceptance rows.
;;
;; Rows (PLAN v1.00.15 W1):
;;   0. exports: exn:fail:network:peer-closed + policy probe cadence
;;   1. unclean close -> peer-closed raised fast even with a 900 s thinking
;;      window (mock server FIN without TLS close_notify)
;;   2. clean EOF (close_notify) -> normal end-of-stream, NOT an error
;;   3. heartbeat-alive -> never killed by the probe (BUG-0018 rule kept)
;;   4. true-silence -> existing timeout classification unchanged
;;   5. auto-retry: retryable/timeout-tier; one retry on peer-closed with
;;      chars>0; silent-overflow economics preserved for chars=0 thinking
;;
;; Live rows need the local mock TLS server (tests/reproducers/
;; mock-fin-server.rkt); row 5 is pure-unit.

(require rackunit
         openssl
         (only-in "../llm/model.rkt" make-stream-chunk)
         (only-in "../llm/stream.rkt"
                  stream-sse-events
                  exn:fail:network:peer-closed
                  exn:fail:network:peer-closed?
                  exn:fail:network:peer-closed-phase
                  exn:fail:network:peer-closed-data-received?
                  exn:fail:network:peer-closed-content-chars
                  exn:fail:network:peer-closed-elapsed-ms
                  exn:fail:network:timeout:stream?)
         (only-in "../llm/request-policy.rkt"
                  peer-close-probe-secs-default
                  current-peer-close-probe-secs
                  resolve-request-network-policy
                  request-network-policy-peer-close-probe-secs)
         (only-in "../runtime/auto-retry.rkt"
                  retryable-error?
                  timeout-error?
                  classify-error
                  with-auto-retry
                  silent-thinking-overflow?
                  retry-exhausted?)
         "reproducers/mock-fin-server.rkt")

;; ---------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------

(define peer-closed-suffix-rx
  #rx"\\[phase=[a-z]+ data-received=(yes|no) chars=[0-9]+ elapsed-ms=[0-9]+\\]$")

(define (make-peer-closed-exn #:phase [phase 'thinking]
                              #:data? [data? #t]
                              #:chars [chars 0]
                              #:message [message "Peer closed mid-stream"])
  (exn:fail:network:peer-closed message (current-continuation-marks) phase data? chars 1234.0))

;; Drain a generator to completion; returns 'clean-end when it yields #f,
;; 'timeout when the phase timeout raises, or the exception otherwise.
(define (drain-generator! gen)
  (with-handlers ([exn:fail? (lambda (e) e)])
    (let loop ()
      (define v (gen))
      (if v
          (loop)
          'clean-end))))

;; event->chunks that models GLM keepalive/ping events: a yielded-but-
;; zero-text chunk (received-any-data? = #t, visible chars = 0). This is
;; exactly the live bug signature: data-received=yes chars=0 thinking.
(define (fake-event->chunks _parsed)
  (list (make-stream-chunk "" #f #f #f)))

;; Run one streaming pass against a fresh mock-server connection.
(define (with-mock-stream! mode run!)
  (define srv (start-mock-fin-server mode))
  (define in #f)
  (define out #f)
  (dynamic-wind (lambda () (void))
                (lambda ()
                  (define-values (i o) (ssl-connect "127.0.0.1" (mock-fin-server-port srv)))
                  (set! in i)
                  (set! out o)
                  (run! i))
                (lambda ()
                  (when in
                    (with-handlers ([exn:fail? void])
                      (close-input-port in)))
                  (when out
                    (with-handlers ([exn:fail? void])
                      (close-output-port out)))
                  (stop-mock-server! srv))))

;; ---------------------------------------------------------------
;; Row 0 — exports and policy surface
;; ---------------------------------------------------------------

(test-case "W1 row 0a: exn:fail:network:peer-closed struct and accessors"
  (define e (make-peer-closed-exn))
  (check-pred exn:fail:network:peer-closed? e)
  (check-equal? (exn:fail:network:peer-closed-phase e) 'thinking)
  (check-true (exn:fail:network:peer-closed-data-received? e))
  (check-equal? (exn:fail:network:peer-closed-content-chars e) 0)
  (check-equal? (exn:fail:network:peer-closed-elapsed-ms e) 1234.0))

(test-case "W1 row 0b: SS-5-style suffix with elapsed-ms on the message"
  (define e
    (exn:fail:network:peer-closed
     (string-append
      "Peer closed the connection mid-stream"
      (format " [phase=~a data-received=~a chars=~a elapsed-ms=~a]" 'thinking "yes" 0 1234))
     (current-continuation-marks)
     'thinking
     #t
     0
     1234.0))
  (check-true (regexp-match? peer-closed-suffix-rx (exn-message e))
              (format "no elapsed-ms suffix on: ~a" (exn-message e))))

(test-case "W1 row 0c: policy exposes peer-close-probe-secs default 5"
  (check-equal? peer-close-probe-secs-default 5)
  (check-equal? (current-peer-close-probe-secs) 5)
  (define policy (resolve-request-network-policy #:request-timeout 600))
  (check-equal? (request-network-policy-peer-close-probe-secs policy) 5))

;; ---------------------------------------------------------------
;; Row 1 — unclean FIN fails fast even under a 900 s thinking window
;; ---------------------------------------------------------------

(test-case "W1 row 1: unclose FIN raises peer-closed well under the phase window"
  (with-mock-stream! 'unclean-close
                     (lambda (in)
                       (define t0 (current-inexact-milliseconds))
                       (define gen
                         (stream-sse-events in
                                            fake-event->chunks
                                            #:initial-timeout 2
                                            #:stream-timeout 2
                                            ;; The whole point: a huge thinking window must NOT
                                            ;; delay detection.
                                            #:thinking-timeout 900
                                            #:max-total-timeout 120
                                            #:peer-close-probe-secs 5))
                       (define outcome (drain-generator! gen))
                       (define elapsed-s (/ (- (current-inexact-milliseconds) t0) 1000.0))
                       (check-pred exn:fail:network:peer-closed? outcome)
                       (when (exn:fail:network:peer-closed? outcome)
                         (check-equal? (exn:fail:network:peer-closed-phase outcome) 'thinking)
                         (check-true (exn:fail:network:peer-closed-data-received? outcome))
                         (check-true (regexp-match? peer-closed-suffix-rx (exn-message outcome))
                                     (format "no suffix: ~a" (exn-message outcome))))
                       (check-true (< elapsed-s 30)
                                   (format "detection took ~as, expected < 30s" elapsed-s)))))

;; ---------------------------------------------------------------
;; Row 2 — clean close_notify ends the stream normally
;; ---------------------------------------------------------------

(test-case "W1 row 2: clean close (close_notify) yields normal end-of-stream"
  (with-mock-stream! 'clean-close
                     (lambda (in)
                       (define gen
                         (stream-sse-events in
                                            fake-event->chunks
                                            #:initial-timeout 5
                                            #:stream-timeout 5
                                            #:thinking-timeout 5
                                            #:max-total-timeout 60
                                            #:peer-close-probe-secs 5))
                       (define saw-data? #f)
                       (define outcome
                         (with-handlers ([exn:fail? (lambda (e) e)])
                           (let loop ()
                             (define v (gen))
                             (cond
                               [(not v) 'clean-end]
                               [else
                                (set! saw-data? #t)
                                (loop)]))))
                       (check-equal? outcome 'clean-end)
                       (check-true saw-data? "expected at least one chunk before the clean EOF"))))

;; ---------------------------------------------------------------
;; Row 3 — heartbeat-alive connection is never killed by the watchdog
;; ---------------------------------------------------------------

(test-case "W1 row 3: heartbeat traffic keeps the stream alive past idle slices"
  (with-mock-stream! 'heartbeat-alive
                     (lambda (in)
                       (define gen
                         (stream-sse-events in
                                            fake-event->chunks
                                            ;; Windows far shorter than the heartbeat span:
                                            ;; only live traffic keeps this alive.
                                            #:initial-timeout 0.05
                                            #:stream-timeout 0.05
                                            #:thinking-timeout 0.05
                                            #:max-total-timeout 30
                                            #:peer-close-probe-secs 5))
                       (define outcome (drain-generator! gen))
                       (check-equal? outcome 'clean-end)
                       (check-pred (lambda (x) (not (exn:fail? x))) outcome))))

;; ---------------------------------------------------------------
;; Row 4 — true silence keeps the timeout classification
;; ---------------------------------------------------------------

(test-case "W1 row 4: true silence still raises the plain phase timeout"
  (with-mock-stream! 'true-silence
                     (lambda (in)
                       (define gen
                         (stream-sse-events in
                                            fake-event->chunks
                                            #:initial-timeout 0.3
                                            #:stream-timeout 0.3
                                            #:thinking-timeout 0.3
                                            #:max-total-timeout 10
                                            #:peer-close-probe-secs 5))
                       (define outcome (drain-generator! gen))
                       (check-pred exn:fail:network:timeout:stream? outcome)
                       (check-false (exn:fail:network:peer-closed? outcome)))))

;; ---------------------------------------------------------------
;; Row 5 — auto-retry classification and economics (pure unit)
;; ---------------------------------------------------------------

(test-case "W1 row 5a: peer-closed classifies as retryable timeout tier"
  (define e (make-peer-closed-exn))
  (check-true (retryable-error? e))
  (check-true (timeout-error? e))
  (check-equal? (classify-error e) 'timeout)
  (check-true (silent-thinking-overflow? (make-peer-closed-exn #:chars 0)))
  (check-false (silent-thinking-overflow? (make-peer-closed-exn #:chars 42))))

(test-case "W1 row 5b: peer-closed with chars>0 gets retried once then succeeds"
  (define attempts (box 0))
  (define result
    (with-auto-retry (lambda ()
                       (set-box! attempts (add1 (unbox attempts)))
                       (if (= (unbox attempts) 1)
                           (raise (make-peer-closed-exn #:chars 250))
                           'ok))
                     #:max-retries 3
                     #:base-delay-ms 1
                     #:per-type-budgets (hash 'timeout 2)))
  (check-equal? result 'ok)
  (check-equal? (unbox attempts) 2))

(test-case "W1 row 5c: peer-closed chars=0 thinking keeps silent-overflow economics"
  (define attempts (box 0))
  (define circuit-breaks (box 0))
  (check-exn retry-exhausted?
             (lambda ()
               (with-auto-retry (lambda ()
                                  (set-box! attempts (add1 (unbox attempts)))
                                  (raise (make-peer-closed-exn #:chars 0)))
                                #:max-retries 3
                                #:base-delay-ms 1
                                #:per-type-budgets (hash 'timeout 4)
                                #:on-circuit-break
                                (lambda (kind _exn)
                                  (when (eq? kind 'silent-thinking-overflow)
                                    (set-box! circuit-breaks (add1 (unbox circuit-breaks))))))))
  ;; Exactly ONE retry, then break with guidance (v1.00.14 economics).
  (check-equal? (unbox attempts) 2)
  (check-equal? (unbox circuit-breaks) 1))
