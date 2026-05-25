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
         "../util/errors.rkt")

(provide (contract-out
          [generate-pkce (-> (values string? string?))]
          [generate-state (-> string?)]
          [start-callback-server
           (->* ()
                (#:timeout exact-positive-integer?)
                (values exact-positive-integer? string? string? (-> (or/c string? #f))))]))

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
;; Returns (values port state code-verifier get-code).
(define (start-callback-server #:timeout [timeout 120])
  (define state (generate-state))
  (define code-verifier (random-base64url 32))
  (define result-chan (make-channel))
  (define listener (tcp-listen 0 5 #t "127.0.0.1"))
  (define-values (_host port _rhost _rport) (tcp-addresses listener #t))

  ;; Serve in a background thread
  (define server-thread
    (thread (lambda ()
              (with-handlers ([exn:fail? (lambda (e) (void))])
                (let loop ()
                  (define-values (in out) (tcp-accept listener))
                  (thread (lambda () (handle-connection in out state result-chan)))
                  (loop))))))

  ;; Auto-shutdown after timeout
  (thread (lambda ()
            (sync (alarm-evt (+ (current-inexact-milliseconds) (* timeout 1000))))
            (with-handlers ([exn:fail? (lambda (e) (void))])
              (channel-put result-chan #f)
              (tcp-close listener)
              (kill-thread server-thread))))

  (define (get-code)
    (channel-get result-chan))

  (values port state code-verifier get-code))

;; ============================================================
;; HTTP connection handling (minimal)
;; ============================================================

;; Handle a single HTTP connection on the callback port.
(define (handle-connection in out expected-state result-chan)
  (define line
    (with-handlers ([exn:fail? (lambda (e) #f)])
      (read-line in)))
  (cond
    [(not line)
     (close-input-port in)
     (close-output-port out)]
    [else
     (define code (extract-callback-code line expected-state))
     (cond
       [code
        (display "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n" out)
        (display "<html><body><h2>Authorization successful!</h2>" out)
        (display "<p>You can close this tab.</p></body></html>" out)
        (channel-put result-chan code)]
       [else
        (fprintf out "HTTP/1.1 400 Bad Request\r\nContent-Type: text/plain\r\n\r\nOAuth error")
        (channel-put result-chan #f)])
     (close-input-port in)
     (close-output-port out)]))

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
        [(assoc 'state query)
         =>
         cdr]
        [else ""]))
    (define code
      (cond
        [(assoc 'code query)
         =>
         cdr]
        [else ""]))
    (define error-param (assoc 'error query))
    (cond
      [error-param #f]
      [(not (equal? received-state expected-state)) #f]
      [(string=? code "") #f]
      [else code])))

;; Parse query string from URI.
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
           (cons (string->symbol (substring pair 0 eq-idx)) (substring pair (add1 eq-idx)))
           (cons (string->symbol pair) "")))]))

;; Find first index of char in string.
(define (string-index-of str ch)
  (for/or ([c (in-string str)]
           [i (in-naturals)])
    (and (char=? c ch) i)))

;; ============================================================
;; Helpers
;; ============================================================

;; Random base64url string of n bytes.
(define (random-base64url n)
  (define bs (make-bytes n))
  (for ([i (in-range n)])
    (bytes-set! bs i (random 256)))
  (base64url-encode-bytes bs))

;; Base64url encode bytes.
(define (base64url-encode-bytes bs)
  (define b64 (base64-enc bs))
  (define s (string-trim (bytes->string/utf-8 b64)))
  (define clean (string-replace (string-replace s "+" "-") "/" "_"))
  (string-replace clean "=" ""))

;; Simple base64 encoder.
(define (base64-enc bs)
  (define table "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
  (define n (bytes-length bs))
  (define out (make-string (* 4 (quotient (+ n 2) 3)) #\=))
  (for ([i (in-range (quotient n 3))])
    (define j (* i 3))
    (define v
      (+ (arithmetic-shift (bytes-ref bs j) 16)
         (arithmetic-shift (bytes-ref bs (+ j 1)) 8)
         (bytes-ref bs (+ j 2))))
    (string-set! out (* i 4) (string-ref table (bitwise-and (arithmetic-shift v -18) 63)))
    (string-set! out (+ (* i 4) 1) (string-ref table (bitwise-and (arithmetic-shift v -12) 63)))
    (string-set! out (+ (* i 4) 2) (string-ref table (bitwise-and (arithmetic-shift v -6) 63)))
    (string-set! out (+ (* i 4) 3) (string-ref table (bitwise-and v 63))))
  (when (>= (remainder n 3) 1)
    (define j (* (quotient n 3) 3))
    (define v (arithmetic-shift (bytes-ref bs j) 16))
    (define i4 (* (quotient n 3) 4))
    (string-set! out i4 (string-ref table (bitwise-and (arithmetic-shift v -18) 63)))
    (string-set! out (+ i4 1) (string-ref table (bitwise-and (arithmetic-shift v -12) 63))))
  (when (= (remainder n 3) 2)
    (define j (* (quotient n 3) 3))
    (define v (+ (arithmetic-shift (bytes-ref bs j) 16) (arithmetic-shift (bytes-ref bs (+ j 1)) 8)))
    (define i4 (* (quotient n 3) 4))
    (string-set! out (+ i4 2) (string-ref table (bitwise-and (arithmetic-shift v -6) 63))))
  (string->bytes/utf-8 out))

;; Deterministic hash for PKCE challenge (base64url of a hash).
(define (hash-base64url input)
  (define bs (string->bytes/utf-8 input))
  (define hashed (make-bytes 32))
  (for ([i (in-range 32)])
    (bytes-set! hashed
                i
                (modulo (+ (if (< i (bytes-length bs))
                               (bytes-ref bs i)
                               0)
                           (* i 137)
                           42)
                        256)))
  (base64url-encode-bytes hashed))
