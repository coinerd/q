#lang racket/base
;; @not-test #t

;; tests/reproducers/mock-fin-server.rkt — BUG-0019 W0 reproducer server.
;;
;; A minimal local TLS server that speaks just enough HTTP/1.1+SSE to drive
;; `llm/stream.rkt`'s `stream-sse-events` in unit tests. Four modes:
;;
;;   'unclean-close   send HTTP 200 + partial SSE frames (incl. a heartbeat
;;                    comment), then close the socket WITHOUT a TLS
;;                    close_notify alert (via `ssl-abandon-port`) — the
;;                    GLM-5.3 gateway FIN signature from BUG-0019.
;;   'clean-close     same frames, then a proper TLS shutdown (close_notify
;;                    via ordinary port close) — must look like a normal
;;                    end-of-stream to the client.
;;   'heartbeat-alive initial data frame, then periodic `: hb-N` comment
;;                    lines, then `data: [DONE]` + clean close — proves the
;;                    BUG-0018 liveness rule survives the W1 watchdog.
;;   'true-silence    HTTP 200 headers only, then hold the socket open
;;                    without ever sending another byte — the classic idle
;;                    timeout path (classification must stay `timeout`).
;;
;; A self-signed certificate is generated at setup time with the `openssl`
;; binary (Racket's default client context does not verify server certs, so
;; tests can connect with plain `ssl-connect`).
;;
;; NOTE: the frames are deliberately sent without chunked encoding /
;; Content-Length; unit tests consume the raw SSL input port with
;; `stream-sse-events` directly, so only the SSE line syntax matters here.

(require openssl
         racket/file
         racket/match)

(provide mock-fin-server
         mock-fin-server?
         mock-fin-server-port
         mock-fin-server-mode
         start-mock-fin-server
         stop-mock-server!)

;; One running mock server instance.
(struct mock-fin-server (port listener thread stop-box cert-path key-path mode) #:transparent)

;; ---------------------------------------------------------------
;; Self-signed certificate generation (setup-time)
;; ---------------------------------------------------------------

(define (find-openssl!)
  (or (find-executable-path "openssl")
      (raise (exn:fail (string-append "mock-fin-server: the `openssl` binary is "
                                      "required to generate the self-signed test "
                                      "certificate")
                       (current-continuation-marks)))))

;; Generate a self-signed cert/key pair into `dir`; returns (values cert key).
(define (make-self-signed-cert! dir)
  (define cert-path (build-path dir "cert.pem"))
  (define key-path (build-path dir "key.pem"))
  (define openssl (find-openssl!))
  (define-values (proc stdout stdin stderr)
    (subprocess #f
                #f
                #f
                (path->string openssl)
                "req"
                "-x509"
                "-newkey"
                "rsa:2048"
                "-nodes"
                "-days"
                "2"
                "-subj"
                "/CN=localhost"
                "-keyout"
                (path->string key-path)
                "-out"
                (path->string cert-path)))
  (close-output-port stdin)
  (close-input-port stdout)
  (close-input-port stderr)
  (subprocess-wait proc)
  (unless (zero? (subprocess-status proc))
    (raise (exn:fail (format "mock-fin-server: openssl exited with status ~a"
                             (subprocess-status proc))
                     (current-continuation-marks))))
  (unless (and (file-exists? cert-path) (file-exists? key-path))
    (raise (exn:fail "mock-fin-server: openssl did not produce cert/key files"
                     (current-continuation-marks))))
  (values cert-path key-path))

;; ---------------------------------------------------------------
;; SSE fixtures shared by the modes
;; ---------------------------------------------------------------

(define http-response-headers
  (string-append "HTTP/1.1 200 OK\r\n"
                 "Content-Type: text/event-stream\r\n"
                 "Cache-Control: no-cache\r\n"
                 "\r\n"))

;; One partial content delta frame + a heartbeat comment, then nothing —
;; the mid-thinking FIN shape from the bug report.
(define partial-sse-frames
  (string-append "data: {\"id\":\"c1\",\"choices\":[{\"delta\":{\"content\":\"par\"}}]}"
                 "\n\n"
                 ": heartbeat-before-close\n\n"))

(define (write-headers! out)
  (display http-response-headers out)
  (flush-output out))

(define (write-partial-frames! out)
  (display partial-sse-frames out)
  (flush-output out))

;; ---------------------------------------------------------------
;; Connection serving, per mode
;; ---------------------------------------------------------------

(define (serve-connection mode in out stop-box)
  (match mode
    ['unclean-close
     ;; Partial SSE traffic, then TCP-level teardown with NO TLS
     ;; close_notify: `ssl-abandon-port` closes the output side while
     ;; suppressing the closing handshake (mzssl sets shutdown-on-close #f).
     (write-headers! out)
     (write-partial-frames! out)
     (sleep 0.05) ; give the frames time to reach the wire before the FIN
     (ssl-abandon-port out)
     (with-handlers ([exn:fail? void])
       (close-input-port in))]
    ['clean-close
     ;; Platform reality (recorded in STATE-v1.00.15): on this OpenSSL 3
     ;; stack a peer close NEVER reaches the client as a plain TLS EOF —
     ;; even a graceful server close surfaces as "unexpected eof while
     ;; reading". Clean end-of-stream is therefore signalled at the SSE
     ;; layer with `data: [DONE]`, exactly how real providers do it.
     (write-headers! out)
     (write-partial-frames! out)
     (sleep 0.05)
     (display "data: [DONE]\n\n" out)
     (flush-output out)
     (close-output-port out)
     ;; Give the closing handshake time to hit the wire before the
     ;; input side (and possibly the listener thread) goes away.
     (sleep 0.4)
     (with-handlers ([exn:fail? void])
       (close-input-port in))]
    ['heartbeat-alive
     ;; Alive-but-slow: periodic heartbeats reset the client's idle clock,
     ;; then a normal [DONE] + clean close ends the stream.
     (write-headers! out)
     (write-partial-frames! out)
     (let loop ([i 0])
       (cond
         [(or (unbox stop-box) (>= i 25))
          (display "data: [DONE]\n\n" out)
          (flush-output out)
          (close-output-port out)
          (with-handlers ([exn:fail? void])
            (close-input-port in))]
         [else
          (display (format ": hb-~a\n\n" i) out)
          (flush-output out)
          (sleep 0.03)
          (loop (add1 i))]))]
    ['true-silence
     ;; Headers only, then hold the connection open silently until stopped.
     (write-headers! out)
     (let hold ()
       (unless (unbox stop-box)
         (sleep 0.05)
         (hold)))
     (with-handlers ([exn:fail? void])
       (close-input-port in)
       (close-output-port out))]
    [else
     (raise (exn:fail (format "mock-fin-server: unknown mode ~s" mode)
                      (current-continuation-marks)))]))

;; ---------------------------------------------------------------
;; Listener lifecycle
;; ---------------------------------------------------------------

;; Bind a TLS listener on 127.0.0.1, retrying random high ports on collision.
;; Returns (values port listener).
(define (bind-tls-listener! ctx)
  (let loop ([tries 20])
    (define candidate (+ 20000 (random 40000)))
    (with-handlers ([exn:fail:network?
                     (lambda (_e)
                       (when (zero? tries)
                         (raise (exn:fail "mock-fin-server: could not bind a port after 20 tries"
                                          (current-continuation-marks))))
                       (loop (sub1 tries)))])
      (define listener (ssl-listen candidate 5 #t "127.0.0.1" ctx))
      (values candidate listener))))

;; Start a mock server. Returns a mock-fin-server handle; always pair with
;; `stop-mock-server!`.
(define (start-mock-fin-server mode #:cert-dir [cert-dir #f])
  (define dir (or cert-dir (make-temporary-file "q-mock-fin-~a" 'directory)))
  (define-values (cert-path key-path) (make-self-signed-cert! dir))
  (define ctx (ssl-make-server-context))
  (ssl-load-certificate-chain! ctx cert-path)
  (ssl-load-private-key! ctx key-path)
  (define-values (port listener) (bind-tls-listener! ctx))
  (define stop-box (box #f))
  (define accept-thread
    (thread
     (lambda ()
       (let accept ()
         (unless (unbox stop-box)
           (with-handlers ([exn:fail? (lambda (_e)
                                        ;; Listener closed or a broken handshake: stop accepting.
                                        (void))])
             (define-values (in out) (ssl-accept listener))
             (serve-connection mode in out stop-box))
           (accept))))))
  (mock-fin-server port listener thread stop-box cert-path key-path mode))

;; Stop a server started by `start-mock-fin-server` and remove its temp dir.
(define (stop-mock-server! srv)
  (set-box! (mock-fin-server-stop-box srv) #t)
  (define listener (mock-fin-server-listener srv))
  (with-handlers ([exn:fail? void])
    (ssl-close listener))
  (define th (mock-fin-server-thread srv))
  (with-handlers ([exn:fail? void])
    (kill-thread th))
  (with-handlers ([exn:fail? void])
    (delete-directory/files (parent-directory-or-self (mock-fin-server-cert-path srv))
                            #:must-exist? #f)))

;; The cert lives directly inside the server's own temp dir, and
;; start-mock-fin-server guarantees that directory is dedicated to this
;; server instance, so deleting the directory containing the cert removes
;; exactly what we created.
(define (parent-directory-or-self p)
  (or (parent-directory p) p))

;; Helper: parent directory of a path (via racket/base `split-path`).
(define (parent-directory p)
  (define-values (base _name _dir?) (split-path p))
  (if (eq? base 'relative) #f base))
