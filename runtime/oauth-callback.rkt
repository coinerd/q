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
;; Uses a semaphore for atomic completion: only one result is ever delivered,
;; and listener/ports are closed before the result is put on the channel.
;; Returns (values port state code-verifier get-code).
(define (start-callback-server #:timeout [timeout 120])
  (define state (generate-state))
  (define code-verifier (random-base64url 32))
  (define result-chan (make-channel))
  ;; Atomic completion: semaphore-try-wait returns #f after first success,
  ;; preventing any second consumer from delivering a result.
  (define completion-sema (make-semaphore 1))
  (define listener (tcp-listen 0 5 #t "127.0.0.1"))
  (define-values (_host port _rhost _rport) (tcp-addresses listener #t))
  (define server-thread #f)

  ;; Atomic complete: try to be the one-and-only completer.
  ;; Returns #t if this thread won the race, #f if already completed.
  (define (try-complete! code)
    (and (semaphore-try-wait? completion-sema)
         (begin
           ;; Close listener FIRST, then deliver result — ensures no more connections
           (with-handlers ([exn:fail? (lambda (e) (void))])
             (tcp-close listener))
           (channel-put result-chan code)
           ;; Kill server accept loop
           (when server-thread
             (with-handlers ([exn:fail? (lambda (e) (void))])
               (kill-thread server-thread)))
           #t)))

  ;; Serve in a background thread — accept connections until the listener is
  ;; closed by try-complete! (or the timeout thread).  The loop terminates
  ;; because tcp-accept raises exn:fail after tcp-close, caught by the
  ;; enclosing with-handlers.
  (set! server-thread
        (thread (lambda ()
                  (with-handlers ([exn:fail? (lambda (e) (void))])
                    (let loop ()
                      (define-values (in out) (tcp-accept listener))
                      (thread (lambda () (handle-connection in out state try-complete!)))
                      (loop))))))

  ;; Auto-shutdown after timeout
  (thread (lambda ()
            (sync (alarm-evt (+ (current-inexact-milliseconds) (* timeout 1000))))
            (try-complete! #f)))

  (define (get-code)
    (channel-get result-chan))

  (values port state code-verifier get-code))

;; ============================================================
;; HTTP connection handling (minimal)
;; ============================================================

;; Handle a single HTTP connection on the callback port.
;; One-shot: uses try-complete! for atomic completion — only the first
;; handler to call try-complete! wins; all others just close their ports.
(define (handle-connection in out expected-state try-complete!)
  (define line
    (with-handlers ([exn:fail? (lambda (e) #f)])
      (read-line in)))
  (cond
    [(not line)
     (close-input-port in)
     (close-output-port out)]
    [else
     (define code (extract-callback-code line expected-state))
     ;; Send HTTP response first, then try atomic completion
     (cond
       [code
        (display "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n" out)
        (display "<html><body><h2>Authorization successful!</h2>" out)
        (display "<p>You can close this tab.</p></body></html>" out)]
       [else (fprintf out "HTTP/1.1 400 Bad Request\r\nContent-Type: text/plain\r\n\r\nOAuth error")])
     (close-input-port in)
     (close-output-port out)
     ;; Atomic: close listener + deliver result, or silently discard if already completed
     (try-complete! code)]))

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
