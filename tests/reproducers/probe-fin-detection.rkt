#lang racket/base
;; @not-test #t
(require racket/port)

;; tests/reproducers/probe-fin-detection.rkt — BUG-0019 Step-C experiment.
;;
;; QUESTION: after a peer sends FIN without TLS close_notify (mock server
;; mode 'unclean-close), what does the client-side Racket SSL input port
;; actually report?
;;
;; METHOD: connect with ssl-connect, consume the initial frames, then poll
;; the port for ~8 s using `sync/timeout` + `read-bytes-avail!-evt` and
;; print every observed verdict:
;;
;;   BYTES:<n>       read-bytes-avail!-evt delivered n bytes
;;   EOF             port reports end-of-file (SSL layer)
;;   WOULD-BLOCK     event not ready (#f) — port silent
;;   NETWORK-ERROR   reading raised exn:fail:network
;;   OTHER-ERROR     reading raised some other exn:fail
;;
;; EXPECTED LADDER DECISION (from openssl/mzssl.rkt source analysis):
;;   - clean close_notify  -> SSL_ERROR_ZERO_RETURN -> eof        => row 2
;;                             (normal end-of-stream) semantics
;;   - unclean FIN         -> SSL_ERROR_SYSCALL/-1 -> mzssl raises
;;                             exn:fail:network                    => option 1
;;                             detects death as a raised network error,
;;                             converted to exn:fail:network:peer-closed.
;; If instead this probe observes plain EOF for the unclean case on this
;; platform, option 1 is inconclusive here and ladder options 2/3 must be
;; revisited (record the outcome in STATE-v1.00.15).
;;
;; Run: racket tests/reproducers/probe-fin-detection.rkt

(require openssl
         racket/match
         "mock-fin-server.rkt")

(define (verdict-label r)
  (cond
    [(eof-object? r) "EOF"]
    [(not r) "WOULD-BLOCK"]
    [(and (real? r) (> r 0)) (format "BYTES:~a" r)]
    [else (format "OTHER:~s" r)]))

;; Poll the port until `budget-secs` elapses, printing every verdict.
(define (poll-port! in budget-secs phase-tag)
  (define buf (make-bytes 256))
  (define deadline (+ (current-inexact-milliseconds) (* budget-secs 1000.0)))
  (let loop ([reads 0])
    (when (< (current-inexact-milliseconds) deadline)
      (define remaining-secs (/ (- deadline (current-inexact-milliseconds)) 1000.0))
      (define result
        (with-handlers ([exn:fail:network? (lambda (_e) 'network-error)]
                        [exn:fail? (lambda (_e) 'other-error)])
          (sync/timeout (min 1.0 remaining-secs) (read-bytes-avail!-evt buf in))))
      (define label
        (match result
          ['network-error "NETWORK-ERROR"]
          ['other-error "OTHER-ERROR"]
          [_ (verdict-label result)]))
      (eprintf "[~a read #~a] ~a\n" phase-tag reads label)
      ;; Stop early once the transport has reported *something* terminal.
      (unless (member label '("EOF" "NETWORK-ERROR" "OTHER-ERROR"))
        (loop (add1 reads))))))

(module+ main
  (eprintf "PROBE-C: starting mock FIN server (mode=unclean-close)\n")
  (define srv (start-mock-fin-server 'unclean-close))
  (dynamic-wind (lambda () (void))
                (lambda ()
                  (define-values (in out) (ssl-connect "127.0.0.1" (mock-fin-server-port srv)))
                  (dynamic-wind (lambda () (void))
                                (lambda ()
                                  (eprintf "PROBE-C: connected; consuming initial frames\n")
                                  ;; Phase 1: normal traffic — frames should arrive as bytes.
                                  (poll-port! in 3.0 "traffic")
                                  ;; Phase 2: after the server abandoned the socket — what does the
                                  ;; port report now? This is the decisive observation.
                                  (eprintf "PROBE-C: server has closed uncleanly by now\n")
                                  (poll-port! in 8.0 "post-FIN"))
                                (lambda ()
                                  (with-handlers ([exn:fail? void])
                                    (close-input-port in))
                                  (with-handlers ([exn:fail? void])
                                    (close-output-port out)))))
                (lambda () (stop-mock-server! srv)))
  (eprintf "PROBE-C: done — record the verdict in STATE-v1.00.15\n"))
