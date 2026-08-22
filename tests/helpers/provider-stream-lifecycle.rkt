#lang racket

;; @speed fast  ;; @suite provider

(require rackunit
         racket/tcp
         "../../llm/model.rkt"
         "../../llm/provider.rkt"
         "../../llm/stream.rkt")

(provide check-stream-setup-timeout-closes-peer)

;; Exercise the real net/http-client boundary against a local peer that accepts
;; a request but never sends response headers. The provider must time out and
;; tear down every request-custodian resource, which the peer observes as EOF.
(define (check-stream-setup-timeout-closes-peer make-provider)
  (define listener (tcp-listen 0 4 #t "127.0.0.1"))
  (define-values (_local-host local-port _remote-host _remote-port) (tcp-addresses listener #t))
  (define request-received (make-semaphore 0))
  (define peer-closed (make-channel))
  (define server
    (thread (lambda ()
              (define-values (client-in client-out) (tcp-accept listener))
              (read-line client-in 'any)
              (define content-length
                (let loop ([length 0])
                  (define line (read-line client-in 'any))
                  (cond
                    [(or (eof-object? line) (string=? line "")) length]
                    [else
                     (define m (regexp-match #px"(?i:^Content-Length: *([0-9]+))" line))
                     (loop (if m
                               (string->number (cadr m))
                               length))])))
              (when (positive? content-length)
                (read-bytes content-length client-in))
              (semaphore-post request-received)
              (let loop ()
                (unless (eof-object? (read-byte client-in))
                  (loop)))
              (channel-put peer-closed #t)
              (close-input-port client-in)
              (close-output-port client-out))))
  (define provider (make-provider (format "http://127.0.0.1:~a" local-port)))
  (parameterize ([current-http-request-timeout 2]
                 [current-model-timeouts (hash "timeout-model" 2)])
    (check-exn exn:fail?
               (lambda ()
                 (provider-stream provider
                                  (make-model-request '() '() (hash 'model "timeout-model"))))))
  (check-not-false (sync/timeout 0 request-received)
                   "the local peer must receive the request before timeout")
  (check-true (sync/timeout 5 peer-closed) "the local peer must observe EOF after setup timeout")
  (tcp-close listener)
  (unless (thread-dead? server)
    (kill-thread server)))
