#lang racket/base

;;; util/ids.rkt — ULID-like ID generation and parsing
;;;
;;; Produces lexicographically sortable, globally unique string IDs.
;;; Format: 10-char timestamp (Crockford Base32) + 16-char random (Crockford Base32)
;;; Total: 26 characters.
;;;
;;; Within the same millisecond, the random portion is incremented
;;; to maintain sort order (ULID spec behavior).

(provide generate-id
         id?
         id->string
         string->id
         now-seconds)

;; ── Crockford Base32 encoding ──
;; Characters: 0-9, A-Z excluding I, L, O, U = 32 symbols

(define encoding-chars
  (string->list "0123456789ABCDEFGHJKMNPQRSTVWXYZ"))

(define encoding-table
  (for/hash ([c (in-list encoding-chars)]
             [i (in-naturals)])
    (values c i)))

(define (encode-base32 n digits)
  ;; Encode integer n into `digits` Base32 characters (most significant first)
  (list->string
   (for/list ([shift (in-range (* 5 (sub1 digits)) -1 -5)])
     (list-ref encoding-chars (bitwise-and (arithmetic-shift n (- shift)) #x1F)))))

(define (decode-base32 str)
  ;; Decode a Base32 string into an integer
  (for/fold ([acc 0])
            ([c (in-string str)])
    (bitwise-ior (arithmetic-shift acc 5)
                 (hash-ref encoding-table (char-upcase c)))))

(define (char-valid-base32? c)
  (hash-has-key? encoding-table (char-upcase c)))

;; ── ID constants ──

(define id-length 26)
(define ts-chars 10)
(define rnd-chars 16)

;; ── Monotonic state ──
;; Track last timestamp and random component to ensure monotonicity

(define last-ts (box 0))
(define last-rnd (box 0))

(define (generate-random-component)
  ;; Generate an 80-bit random number (16 base32 chars = 80 bits)
  (for/fold ([acc 0])
            ([_ (in-range rnd-chars)])
    (bitwise-ior (arithmetic-shift acc 5)
                 (random 32))))

(define (random-component->string n)
  (encode-base32 n rnd-chars))

(define (string->random-component s)
  (decode-base32 s))

(define (generate-id)
  ;; Generate a ULID-compatible ID with monotonic guarantee.
  ;; If the timestamp is the same as the previous call, increment the random part.
  ;; Otherwise, generate a fresh random part.
  (define ts (inexact->exact (truncate (current-inexact-milliseconds))))
  (define prev-ts (unbox last-ts))
  (define rnd
    (cond
      [(> ts prev-ts)
       ;; New millisecond: fresh random
       (generate-random-component)]
      [else
       ;; Same or earlier millisecond: increment previous random
       (define new-rnd (add1 (unbox last-rnd)))
       ;; Guard against overflow (80 bits)
       (if (> new-rnd (sub1 (expt 2 80)))
           (generate-random-component)
           new-rnd)]))
  (set-box! last-ts ts)
  (set-box! last-rnd rnd)
  (string-append (encode-base32 ts ts-chars)
                 (random-component->string rnd)))

;; ── Predicates and conversions ──

(define (id? v)
  ;; A valid ID is a string of exactly 26 Crockford-Base32 characters
  (and (string? v)
       (= (string-length v) id-length)
       (for/and ([c (in-string v)])
         (char-valid-base32? c))))

(define (id->string id)
  ;; IDs are already strings; validate and return
  (if (id? id)
      id
      (raise-argument-error 'id->string "id?" id)))

(define (string->id s)
  ;; Parse a string as an ID; returns #f if invalid
  (and (id? s) s))

;; ── Time helpers ──

(define (now-seconds)
  (inexact->exact (truncate (/ (current-inexact-milliseconds) 1000))))
