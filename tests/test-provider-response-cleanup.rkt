#lang racket

;; @speed fast
;; @suite provider
;; @boundary unit

;; tests/test-provider-response-cleanup.rkt
;; v1.00.13 (RL-6): deterministic response-port ownership on the
;; non-streaming/eager-body path (`make-provider-http-request`).
;;
;; The HTTP boundary is injectable through `current-provider-http-sendrecv`
;; (llm/http-helpers.rkt, landed with the W3 lifecycle ownership): a
;; parameter procedure with the http-sendrecv signature returning
;; (values status-line headers response-port). Tests substitute a local
;; socket pair so `port-closed?` directly observes the lifecycle contract:
;; every opened response port is closed exactly once by the request boundary
;; on success, status-check failure, read timeout, request timeout, and
;; cancellation. GC remains a safety net, not the lifecycle.
;;
;; Committed red in W0 (#9454); green since W3 (#9473).

(require rackunit
         racket/tcp
         (only-in "../llm/http-helpers.rkt" make-provider-http-request)
         (only-in "../llm/provider-errors.rkt" provider-error?))

;; ————————————————————————————————————————————————————————————
;; Seam resolution (guarded: red until W3 lands)
;; ————————————————————————————————————————————————————————————

(define sendrecv-seam
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (dynamic-require '"../llm/http-helpers.rkt" 'current-provider-http-sendrecv)))

(define (seam-missing-error)
  (fail "current-provider-http-sendrecv (W3 #9473) not yet landed: cleanup contract is red"))

;; ————————————————————————————————————————————————————————————
;; Recording fake boundary: hands the request a real socket port
;; ————————————————————————————————————————————————————————————

(define client-ports (box '()))

;; make-fake-sendrecv : [#:status bytes?] [#:body bytes?] [#:stall-after bytes?]
;;   -> procedure (http-sendrecv signature)
;; The peer writes the payload, then either completes (EOF) or stalls without
;; closing. Every client port is recorded so tests can observe closed state.
(define (make-fake-sendrecv #:status [status #"HTTP/1.1 200 OK"]
                            #:body [body #"{}"]
                            #:stall-after [stall-prefix #f])
  (set-box! client-ports '())
  (define listener (tcp-listen 0 4 #t "127.0.0.1"))
  (define-values (_lh lport _rh _rp) (tcp-addresses listener #t))
  (define stalling? (and stall-prefix #t))
  (lambda (host path #:port p #:ssl? ssl? #:method m #:headers hs #:data d)
    (define-values (in _out) (tcp-connect "127.0.0.1" lport))
    (define-values (peer-in peer-out) (tcp-accept listener))
    (void peer-in)
    (display (if stalling? stall-prefix body) peer-out)
    (flush-output peer-out)
    (unless stalling?
      (close-output-port peer-out))
    (set-box! client-ports (cons in (unbox client-ports)))
    (values status '("Content-Type: application/json") in)))

;; The most recent response port handed to the request boundary.
(define (last-response-port)
  (and (pair? (unbox client-ports)) (car (unbox client-ports))))

(define (last-response-port-closed?)
  (define p (last-response-port))
  (and (input-port? p) (port-closed? p)))

;; status checker that raises provider-error for >= 400 like the real ones
(define (strict-status-checker sl rb)
  (define code
    (let ([m (regexp-match #rx#"HTTP/[^ ]+ ([0-9]+)" sl)])
      (if m
          (string->number (bytes->string/utf-8 (cadr m)))
          0)))
  (when (>= code 400)
    (raise ((dynamic-require '"../llm/provider-errors.rkt" 'provider-error)
            "fake 429"
            (current-continuation-marks)
            (hash)
            'rate-limit
            429))))

;; ————————————————————————————————————————————————————————————
;; Cleanup contract matrix
;; ————————————————————————————————————————————————————————————

(test-case "success: response port closed after a completed body read"
  (unless (procedure? sendrecv-seam)
    (seam-missing-error))
  (define fake (make-fake-sendrecv #:body #"{\"ok\":true}"))
  (define result
    (parameterize ([sendrecv-seam fake])
      (make-provider-http-request "http://fake/x"
                                  '("h: v")
                                  #"{}"
                                  #:status-checker (lambda (sl rb) (void)))))
  (check-not-false result)
  (check-true (last-response-port-closed?)))

(test-case "status-check failure: response port closed after the raise"
  (unless (procedure? sendrecv-seam)
    (seam-missing-error))
  (define fake (make-fake-sendrecv #:status #"HTTP/1.1 429 Too Many Requests"))
  (parameterize ([sendrecv-seam fake])
    (check-exn provider-error?
               (lambda ()
                 (make-provider-http-request "http://fake/x"
                                             '("h: v")
                                             #"{}"
                                             #:status-checker strict-status-checker)))
    (check-true (last-response-port-closed?))))

(test-case "read timeout: response port closed after body-read stall"
  (unless (procedure? sendrecv-seam)
    (seam-missing-error))
  (define fake (make-fake-sendrecv #:stall-after #"{\"partial"))
  (parameterize ([sendrecv-seam fake])
    (check-exn exn:fail?
               (lambda ()
                 (make-provider-http-request "http://fake/x"
                                             '("h: v")
                                             #"{}"
                                             #:read-timeout 0.3
                                             #:status-checker (lambda (sl rb) (void)))))
    (check-true (last-response-port-closed?))))

(test-case "request timeout: response port closed when the outer budget fires"
  (unless (procedure? sendrecv-seam)
    (seam-missing-error))
  (define fake (make-fake-sendrecv #:stall-after #"{\"partial"))
  (parameterize ([sendrecv-seam fake])
    (check-exn exn:fail?
               (lambda ()
                 (make-provider-http-request "http://fake/x"
                                             '("h: v")
                                             #"{}"
                                             #:timeout 0.3
                                             #:status-checker (lambda (sl rb) (void)))))
    (check-true (last-response-port-closed?))))

(test-case "cancellation: response port closed when the caller breaks the read"
  (unless (procedure? sendrecv-seam)
    (seam-missing-error))
  (define fake (make-fake-sendrecv #:stall-after #"{\"partial"))
  (parameterize ([sendrecv-seam fake])
    (define t
      (thread (lambda ()
                (with-handlers ([exn:break? (lambda (_) (void))]
                                [exn:fail? (lambda (_) (void))])
                  (make-provider-http-request "http://fake/x"
                                              '("h: v")
                                              #"{}"
                                              #:read-timeout 30
                                              #:status-checker (lambda (sl rb) (void)))))))
    (sleep 0.2)
    (break-thread t)
    (thread-wait t))
  (check-true (last-response-port-closed?)))

(test-case "cleanup is single-owner: a completed request survives GC pressure"
  (unless (procedure? sendrecv-seam)
    (seam-missing-error))
  (define fake (make-fake-sendrecv #:body #"{}"))
  (parameterize ([sendrecv-seam fake])
    (make-provider-http-request "http://fake/x"
                                '("h: v")
                                #"{}"
                                #:status-checker (lambda (sl rb) (void))))
  ;; forcing collection after an explicit close must not resurrect or crash
  (collect-garbage)
  (collect-garbage)
  (check-true (last-response-port-closed?)))
