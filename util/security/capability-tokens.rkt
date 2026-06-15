#lang racket/base

;; util/security/capability-tokens.rkt — HMAC-signed capability tokens
;; STABILITY: evolving
;;
;; MAS Schritt 6: Capability Tokens (Phase 1)
;;
;; Capability tokens are HMAC-signed strings that prove an agent has
;; been granted a specific capability (e.g., 'execute-tools, 'write-files).
;; Tokens are scoped to a single agent ID and expire after a TTL.
;;
;; Token format: "cap:<capability>:<agent-id>:<timestamp>:<hmac>"
;;
;; Where:
;;   <capability> — symbol name of the capability (e.g., execute-tools)
;;   <agent-id>   — string identifying the agent
;;   <timestamp>  — Unix epoch seconds when token was issued
;;   <hmac>       — hex-encoded HMAC-SHA256 of the preceding fields
;;
;; HMAC-SHA256 is implemented using the standard construction over
;; SHA-256 from Racket's openssl module. This avoids external dependencies
;; and keeps the crypto layer pure (no networking).

(require racket/contract
         racket/string
         file/sha1 ; bytes->hex-string
         openssl ; sha256-bytes
         (only-in "../ids.rkt" now-seconds))

;; ============================================================
;; Constants
;; ============================================================

;; Token time-to-live in seconds (5 minutes).
(define CAPABILITY-TOKEN-TTL 300)

;; HMAC block size for SHA-256 (bytes).
(define hmac-block-size 64)

;; HMAC inner pad byte.
(define ipad-byte 54) ; #x36

;; HMAC outer pad byte.
(define opad-byte 92) ; #x5c

;; ============================================================
;; HMAC-SHA256 Implementation
;; ============================================================

;; XOR a byte string with a single byte value, returning a list of ints.
(define (xor-bytes bs fill-byte)
  (for/list ([b (in-bytes bs)])
    (bitwise-xor b fill-byte)))

;; Zero-pad a byte string to the target length.
(define (pad-to bs target-len)
  (bytes-append bs (make-bytes (- target-len (bytes-length bs)) 0)))

;; Pad or hash key to HMAC block size (64 bytes for SHA-256).
(define (prepare-key key-bytes)
  (define key-len (bytes-length key-bytes))
  (cond
    ;; Hash first, then zero-pad
    [(> key-len hmac-block-size) (pad-to (sha256-bytes (open-input-bytes key-bytes)) hmac-block-size)]
    [(< key-len hmac-block-size) (pad-to key-bytes hmac-block-size)]
    [else key-bytes]))

;; Compute HMAC-SHA256(key, message) returning hex string.
(define (hmac-sha256 key-str message-str)
  (define key-bytes (string->bytes/utf-8 key-str))
  (define prepared-key (prepare-key key-bytes))
  (define ipad (list->bytes (xor-bytes prepared-key ipad-byte)))
  (define opad (list->bytes (xor-bytes prepared-key opad-byte)))
  (define msg-bytes (string->bytes/utf-8 message-str))
  ;; inner = H(ipad || message)
  (define inner (sha256-bytes (open-input-bytes (bytes-append ipad msg-bytes))))
  ;; outer = H(opad || inner)
  (define outer (sha256-bytes (open-input-bytes (bytes-append opad inner))))
  (bytes->hex-string outer))

;; ============================================================
;; Capability Token API
;; ============================================================

;; Sign a capability token for an agent.
;; Returns a token string: "cap:<capability>:<agent-id>:<timestamp>:<hmac>"
(define (sign-capability-token capability agent-id secret-key #:timestamp [ts (now-seconds)])
  (define cap-str
    (if (symbol? capability)
        (symbol->string capability)
        capability))
  (define message (format "~a:~a:~a" cap-str agent-id ts))
  (define mac (hmac-sha256 secret-key message))
  (format "cap:~a:~a:~a:~a" cap-str agent-id ts mac))

;; Validate a capability token.
;; Returns the capability symbol if valid, #f otherwise.
;; Checks: format (5 colon-separated parts), HMAC signature, TTL expiry.
(define (validate-capability-token token secret-key #:now [current-time (now-seconds)])
  (define parts (string-split token ":"))
  (and (= (length parts) 5)
       (let ([prefix (list-ref parts 0)]
             [cap-str (list-ref parts 1)]
             [agent-id (list-ref parts 2)]
             [ts-str (list-ref parts 3)]
             [mac (list-ref parts 4)])
         (and (equal? prefix "cap")
              (let ([ts (string->number ts-str)])
                (and ts
                     ;; Not issued in the future
                     (<= ts current-time)
                     ;; Not expired
                     (>= ts (- current-time CAPABILITY-TOKEN-TTL))
                     ;; Verify HMAC
                     (let ([expected-mac (hmac-sha256 secret-key
                                                      (format "~a:~a:~a" cap-str agent-id ts-str))])
                       (and (string=? mac expected-mac) (string->symbol cap-str)))))))))

;; ============================================================
;; Provides
;; ============================================================

(provide CAPABILITY-TOKEN-TTL)

(provide (contract-out [sign-capability-token
                        (->* ((or/c symbol? string?) string? string?)
                             (#:timestamp exact-nonnegative-integer?)
                             string?)]
                       [validate-capability-token
                        (->* (string? string?) (#:now exact-nonnegative-integer?) (or/c symbol? #f))]
                       [hmac-sha256 (-> string? string? string?)]))
