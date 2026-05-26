#lang racket/base

;; runtime/oauth-callback.rkt — OAuth2 HTTP callback server
;;
;; Starts a local HTTP server on an ephemeral port to receive the
;; OAuth2 authorization code redirect. Implements CSRF protection
;; via state parameter and PKCE (S256) for code verifier/challenge.

(require racket/contract
         racket/string
         racket/port
         racket/tcp
         file/sha1
         net/base64
         net/uri-codec
         "../util/errors.rkt")

(provide (contract-out
          [generate-pkce (-> (values string? string?))]
          [generate-state (-> string?)]
          [start-callback-server
           (->* ()
                (#:timeout exact-positive-integer?)
                (values exact-positive-integer? string? string? (-> (or/c string? #f))))])
         base64url-encode-bytes
         parse-query
         safe-decode
         hash-base64url)

;; ============================================================
;; PKCE (Proof Key for Code Exchange) — RFC 7636
;; ============================================================

;; Generate PKCE code verifier and S256 challenge.
(define (generate-pkce)
  (define verifier (random-base64url 32))
  (define challenge (hash-base64url verifier))
  (values verifier challenge))

;; Generate a random state parameter for CSRF protection.
(define (generate-state)
  (random-base64url 16))

;; ============================================================
;; Callback HTTP server
;; ============================================================

;; Start a one-shot callback server on an ephemeral port.
;; The server closes after the first callback (success or failure) or timeout.
;; Returns (values port state code-verifier get-code).
(define (start-callback-server #:timeout [timeout 120])
  (define state (generate-state))
  (define code-verifier (random-base64url 32))
  (define result-chan (make-channel))
  (define done? (box #f))
  (define result-put? (box #f))
  (define listener (tcp-listen 0 5 #t "127.0.0.1"))
  (define-values (_host port _rhost _rport) (tcp-addresses listener #t))

  ;; Serve in a background thread — one-shot: stop after first result
  (define server-thread
    (thread (lambda ()
              (with-handlers ([exn:fail? (lambda (e) (void))])
                (let loop ()
                  (define-values (in out) (tcp-accept listener))
                  (thread (lambda ()
                            (handle-connection in out state result-chan listener done? result-put?)))
                  (unless (unbox done?)
                    (loop)))))))

  ;; Auto-shutdown after timeout
  (thread (lambda ()
            (sync (alarm-evt (+ (current-inexact-milliseconds) (* timeout 1000))))
            (with-handlers ([exn:fail? (lambda (e) (void))])
              (set-box! done? #t)
              (unless (unbox result-put?)
                (channel-put result-chan #f))
              (tcp-close listener)
              (kill-thread server-thread))))

  (define (get-code)
    (channel-get result-chan))

  (values port state code-verifier get-code))

;; ============================================================
;; HTTP connection handling (minimal)
;; ============================================================

;; Handle a single HTTP connection on the callback port.
;; One-shot: closes listener and sets done? after first result.
(define (handle-connection in out expected-state result-chan listener done? result-put?)
  (define line
    (with-handlers ([exn:fail? (lambda (e) #f)])
      (read-line in)))
  (cond
    [(not line)
     (close-input-port in)
     (close-output-port out)]
    [(unbox done?)
     ;; Already handled; discard
     (close-input-port in)
     (close-output-port out)]
    [else
     (set-box! done? #t)
     (define code (extract-callback-code line expected-state))
     (cond
       [code
        (display "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n" out)
        (display "<html><body><h2>Authorization successful!</h2>" out)
        (display "<p>You can close this tab.</p></body></html>" out)]
       [else (fprintf out "HTTP/1.1 400 Bad Request\r\nContent-Type: text/plain\r\n\r\nOAuth error")])
     (set-box! result-put? #t)
     (channel-put result-chan code)
     (close-input-port in)
     (close-output-port out)
     (with-handlers ([exn:fail? (lambda (e) (void))])
       (tcp-close listener))]))

;; Extract the authorization code from the callback request line.
(define (extract-callback-code request-line expected-state)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (define parts (string-split (string-trim request-line) " "))
    (when (< (length parts) 2)
      (error "bad request"))
    (define uri (cadr parts))
    (define query (parse-query uri))
    (define received-state
      (cond
        [(assoc "state" query)
         =>
         cdr]
        [else ""]))
    (define code
      (cond
        [(assoc "code" query)
         =>
         cdr]
        [else ""]))
    (define error-param (assoc "error" query))
    (cond
      [error-param #f]
      [(not (equal? received-state expected-state)) #f]
      [(string=? code "") #f]
      [else code])))

;; Parse query string from URI with safe percent-decoding.
;; Keys are kept as strings (attacker-controlled) not symbols.
(define (parse-query uri)
  (define qmark (string-index-of uri #\?))
  (cond
    [(not qmark) '()]
    [else
     (define qs (substring uri (add1 qmark)))
     (define hash-idx (string-index-of qs #\#))
     (define clean
       (if hash-idx
           (substring qs 0 hash-idx)
           qs))
     (for/list ([pair (in-list (string-split clean "&"))])
       (define eq-idx (string-index-of pair #\=))
       (if eq-idx
           (cons (safe-decode (substring pair 0 eq-idx)) (safe-decode (substring pair (add1 eq-idx))))
           (cons (safe-decode pair) "")))]))

;; Safely decode a URI component. Falls back to raw string on decode error.
(define (safe-decode s)
  (with-handlers ([exn:fail? (lambda (e) s)])
    (uri-decode s)))

;; Find first index of char in string.
(define (string-index-of str ch)
  (for/or ([c (in-string str)]
           [i (in-naturals)])
    (and (char=? c ch) i)))

;; ============================================================
;; Helpers
;; ============================================================

;; Random base64url string of n bytes using CSPRNG (/dev/urandom).
(define (random-base64url n)
  (define bs (csprng-bytes n))
  (base64url-encode-bytes bs))

;; Read n cryptographically-secure random bytes from /dev/urandom.
(define (csprng-bytes n)
  (call-with-input-file "/dev/urandom" (lambda (p) (read-bytes n p))))

;; Base64url encode bytes using standard library.
(define (base64url-encode-bytes bs)
  (define b64 (base64-encode bs))
  (define s (string-trim (bytes->string/utf-8 b64)))
  (define clean (string-replace (string-replace s "+" "-") "/" "_"))
  (string-replace clean "=" ""))

;; RFC 7636 PKCE challenge: BASE64URL(SHA256(input))
;; Uses real SHA-256 via file/sha1 library.
(define (hash-base64url input)
  (define bs (string->bytes/utf-8 input))
  (define hashed (sha256-bytes bs))
  (base64url-encode-bytes hashed))
