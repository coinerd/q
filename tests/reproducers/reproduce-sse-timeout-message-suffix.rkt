#!/usr/bin/env racket
#lang racket/base

;; @speed slow
;; @suite runtime
;; @boundary unit

;; tests/reproducers/reproduce-sse-timeout-message-suffix.rkt
;; SS-5 reproducer (v1.00.12 W0, #9428): `exn:fail:network:timeout:stream`
;; messages carry no phase/liveness suffix — live triage from the transcript
;; shows only "HTTP read timeout (600 seconds) waiting for SSE chunk" with no
;; indication of phase or whether any data had been received.
;;
;; Assertion-red until W2 (#9430) appends the stable suffix
;;   [phase=<p> data-received=<yes|no> chars=<n>]
;; to the three raise sites in `llm/stream.rkt`. This file lives under
;; tests/reproducers/ (kept out of the fast suite by the slow-speed
;; annotation) so the red state does not break CI; W2 moves these checks into
;; tests/test-sse-phase-timeout-bounds.rkt and deletes this reproducer.

(require rackunit
         racket/port
         (only-in "../../llm/stream.rkt"
                  stream-sse-events
                  exn:fail:network:timeout:stream?
                  exn:fail:network:timeout:stream-phase
                  exn:fail:network:timeout:stream-received-any-data?)
         (only-in "../../llm/model.rkt" make-stream-chunk))

(define suffix-rx #rx"\\[phase=(initial|thinking|content) data-received=(yes|no) chars=[0-9]+\\]$")

;; Pipe that yields one content chunk then stalls (writer left open).
(define (content-stall-port)
  (define-values (in out) (make-pipe))
  (write-bytes #"data: {\"id\":\"c1\",\"choices\":[{\"delta\":{\"content\":\"Hi\"}}]}\n\n" out)
  (values in out))

;; Pipe that never yields anything (writer left open).
(define (silent-port)
  (make-pipe))

;; Pull n values from gen, capturing a raised timeout:stream exception.
(define (pull-until-timeout gen n)
  (with-handlers ([exn:fail:network:timeout:stream? (lambda (e) e)])
    (let loop ([i 0])
      (when (< i n)
        (gen)
        (loop (add1 i))))
    #f))

(test-case "SS-5: initial-phase hold message carries phase/liveness suffix"
  (define-values (in out) (silent-port))
  (define exn
    (pull-until-timeout (stream-sse-events in
                                           (lambda (_parsed) '())
                                           #:initial-timeout 0.05
                                           #:stream-timeout 0.05
                                           #:thinking-timeout 0.05
                                           #:max-total-timeout 5)
                        1))
  (check-not-false exn)
  (when exn
    (check-true (regexp-match? suffix-rx (exn-message exn))
                (format "no suffix on initial-phase message: ~a" (exn-message exn)))
    (check-equal? (exn:fail:network:timeout:stream-phase exn) 'initial)))

(test-case "SS-5: content-phase stall message carries phase/liveness suffix"
  (define-values (in out) (content-stall-port))
  (define exn
    (pull-until-timeout (stream-sse-events in
                                           (lambda (_parsed) (list (make-stream-chunk "Hi" #f #f #f)))
                                           #:initial-timeout 0.5
                                           #:stream-timeout 0.05
                                           #:thinking-timeout 0.5
                                           #:max-total-timeout 5)
                        2))
  (check-not-false exn)
  (when exn
    (check-true (regexp-match? suffix-rx (exn-message exn))
                (format "no suffix on content-phase message: ~a" (exn-message exn)))
    (check-equal? (exn:fail:network:timeout:stream-phase exn) 'content)
    (check-true (exn:fail:network:timeout:stream-received-any-data? exn))))

(close-output-port (silent-port))
