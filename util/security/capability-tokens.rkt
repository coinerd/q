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
;; Data
;; ============================================================

(struct capability-token-claims (capability agent-id timestamp) #:transparent)

;; ============================================================
;; Validation helpers
;; ============================================================

(define (non-empty-string? v)
  (and (string? v) (positive? (string-length v))))

(define (token-field-string? v)
  (and (non-empty-string? v) (not (regexp-match? #rx":" v))))

(define (capability-input? v)
  ;; F-06 (v0.99.11 W2): Both symbol and string inputs must pass the same
  ;; field validation after symbol->string conversion.
  (and (or (symbol? v) (string? v))
       (token-field-string? (if (symbol? v)
                                (symbol->string v)
                                v))))

(define (secret-key? v)
  (non-empty-string? v))

(define (capability->string capability)
  (if (symbol? capability)
      (symbol->string capability)
      capability))

(define (canonical-nonnegative-integer-string? s)
  (and (string? s)
       (regexp-match? #px"^(0|[1-9][0-9]*)$" s)
       (let ([n (string->number s)]) (exact-nonnegative-integer? n))))

(define (parse-token-timestamp ts-str)
  (and (canonical-nonnegative-integer-string? ts-str) (string->number ts-str)))

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

;; M1 (v0.99.10 W3): hardened MAC comparison.
(define (constant-time-string=? a b)
  (define a-bytes (string->bytes/utf-8 a))
  (define b-bytes (string->bytes/utf-8 b))
  (define a-len (bytes-length a-bytes))
  (define b-len (bytes-length b-bytes))
  (define max-len (max a-len b-len))
  (define diff
    (for/fold ([acc (bitwise-xor a-len b-len)]) ([i (in-range max-len)])
      (define av
        (if (< i a-len)
            (bytes-ref a-bytes i)
            0))
      (define bv
        (if (< i b-len)
            (bytes-ref b-bytes i)
            0))
      (bitwise-ior acc (bitwise-xor av bv))))
  (zero? diff))

;; ============================================================
;; Capability Token API
;; ============================================================

;; Sign a capability token for an agent.
;; Returns a token string: "cap:<capability>:<agent-id>:<timestamp>:<hmac>"
(define (sign-capability-token capability agent-id secret-key #:timestamp [ts (now-seconds)])
  (define cap-str (capability->string capability))
  (define message (format "~a:~a:~a" cap-str agent-id ts))
  (define mac (hmac-sha256 secret-key message))
  (format "cap:~a:~a:~a:~a" cap-str agent-id ts mac))

;; Validate a capability token and return explicit claims if valid.
;; Returns capability-token-claims or #f.
(define (validate-capability-token/claims token secret-key #:now [current-time (now-seconds)])
  ;; M2 / grammar hardening: keep empty fields so trailing colons are rejected.
  (define parts (string-split token ":" #:trim? #f))
  (and (= (length parts) 5)
       (let ([prefix (list-ref parts 0)]
             [cap-str (list-ref parts 1)]
             [agent-id (list-ref parts 2)]
             [ts-str (list-ref parts 3)]
             [mac (list-ref parts 4)])
         (and (equal? prefix "cap")
              (token-field-string? cap-str)
              (token-field-string? agent-id)
              (token-field-string? mac)
              (let ([ts (parse-token-timestamp ts-str)])
                (and ts
                     ;; Not issued in the future
                     (<= ts current-time)
                     ;; Not expired
                     (>= ts (- current-time CAPABILITY-TOKEN-TTL))
                     ;; Verify HMAC with hardened comparison.
                     (let ([expected-mac (hmac-sha256 secret-key
                                                      (format "~a:~a:~a" cap-str agent-id ts-str))])
                       (and (constant-time-string=? mac expected-mac)
                            (capability-token-claims (string->symbol cap-str) agent-id ts)))))))))

;; Validate a capability token.
;; Returns the capability symbol if valid, #f otherwise.
;; Backward-compatible wrapper around validate-capability-token/claims.
(define (validate-capability-token token secret-key #:now [current-time (now-seconds)])
  (define claims (validate-capability-token/claims token secret-key #:now current-time))
  (and claims (capability-token-claims-capability claims)))

;; Validate and enforce that the token belongs to expected-agent-id.
(define (validate-capability-token-for-agent token
                                             secret-key
                                             expected-agent-id
                                             #:now [current-time (now-seconds)])
  (define claims (validate-capability-token/claims token secret-key #:now current-time))
  (and claims
       (equal? (capability-token-claims-agent-id claims) expected-agent-id)
       (capability-token-claims-capability claims)))

;; ============================================================
;; Provides
;; ============================================================

(provide CAPABILITY-TOKEN-TTL)

(provide (struct-out capability-token-claims))

(provide (contract-out
          [sign-capability-token
           (->* (capability-input? token-field-string? secret-key?)
                (#:timestamp exact-nonnegative-integer?)
                string?)]
          [validate-capability-token
           (->* (string? secret-key?) (#:now exact-nonnegative-integer?) (or/c symbol? #f))]
          [validate-capability-token/claims
           (->* (string? secret-key?)
                (#:now exact-nonnegative-integer?)
                (or/c capability-token-claims? #f))]
          [validate-capability-token-for-agent
           (->* (string? secret-key? token-field-string?)
                (#:now exact-nonnegative-integer?)
                (or/c symbol? #f))]
          [hmac-sha256 (-> secret-key? string? string?)]
          [constant-time-string=? (-> string? string? boolean?)]))
