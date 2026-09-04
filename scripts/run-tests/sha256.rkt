#lang racket/base

;; Dependency-free SHA-256 (FIPS 180-4).
;;
;; This Racket installation exposes SHA-1 through `openssl` but no SHA-256
;; binding, and the `file/sha256` module assumed by earlier drafts does not
;; exist.  Since every digest here covers small inputs (manifests, test
;; files, allowlists), a straightforward exact-integer implementation is
;; more than fast enough and removes the external dependency entirely.

(provide sha256
         sha256-bytes
         sha256-hex
         bytes->hex-string)

(require racket/port
         racket/vector)

;; First 32 bits of the fractional parts of the cube roots of the first
;; 64 primes (FIPS 180-4 section 4.2.2).
(define K
  (list->vector
   '(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1
     #x923f82a4 #xab1c5ed5 #xd807aa98 #x12835b01 #x243185be #x550c7dc3
     #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174 #xe49b69c1 #xefbe4786
     #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
     #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147
     #x06ca6351 #x14292967 #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
     #x650a7354 #x766a0abb #x81c2c92e #x92722c85 #xa2bfe8a1 #xa81a664b
     #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
     #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a
     #x5b9cca4f #x682e6ff3 #x748f82ee #x78a5636f #x84c87814 #x8cc70208
     #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2)))

(define initial-h
  (vector #x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
          #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))

(define (u32 x) (bitwise-and x #xffffffff))
(define (shr x n) (arithmetic-shift x (- n)))
(define (rotr x n)
  (bitwise-ior (arithmetic-shift x (- n))
               (bitwise-and (arithmetic-shift x (- 32 n)) #xffffffff)))

(define (sha256-bytes bstr)
  (define len (bytes-length bstr))
  ;; Padding: message || 0x80 || zeros || 64-bit big-endian bit length.
  (define pad-len (- 64 (remainder (+ len 9) 64)))
  (define total (+ len 1 pad-len 8))
  (define msg (make-bytes total 0))
  (bytes-copy! msg 0 bstr)
  (bytes-set! msg len #x80)
  (define bitlen (* len 8))
  (for ([i (in-range 8)])
    (bytes-set! msg (+ len 1 pad-len i)
                (bitwise-and (arithmetic-shift bitlen (- (* i 8) 56)) #xff)))
  (define h (vector-copy initial-h))
  (define w (make-vector 64 0))
  (for ([off (in-range 0 total 64)])
    (for ([i (in-range 16)])
      (define base (+ off (* i 4)))
      (vector-set! w i
                   (+ (* (bytes-ref msg base) 16777216)
                      (* (bytes-ref msg (+ base 1)) 65536)
                      (* (bytes-ref msg (+ base 2)) 256)
                      (bytes-ref msg (+ base 3)))))
    (for ([i (in-range 16 64)])
      (define wi-15 (vector-ref w (- i 15)))
      (define wi-2 (vector-ref w (- i 2)))
      (define s0 (bitwise-xor (rotr wi-15 7) (rotr wi-15 18) (shr wi-15 3)))
      (define s1 (bitwise-xor (rotr wi-2 17) (rotr wi-2 19) (shr wi-2 10)))
      (vector-set! w i
                   (u32 (+ (vector-ref w (- i 16)) s0
                           (vector-ref w (- i 7)) s1))))
    (let compress ([a (vector-ref h 0)] [b (vector-ref h 1)]
                   [c (vector-ref h 2)] [d (vector-ref h 3)]
                   [e (vector-ref h 4)] [f (vector-ref h 5)]
                   [g (vector-ref h 6)] [hh (vector-ref h 7)]
                   [i 0])
      (if (= i 64)
          (begin
            (vector-set! h 0 (u32 (+ a (vector-ref h 0))))
            (vector-set! h 1 (u32 (+ b (vector-ref h 1))))
            (vector-set! h 2 (u32 (+ c (vector-ref h 2))))
            (vector-set! h 3 (u32 (+ d (vector-ref h 3))))
            (vector-set! h 4 (u32 (+ e (vector-ref h 4))))
            (vector-set! h 5 (u32 (+ f (vector-ref h 5))))
            (vector-set! h 6 (u32 (+ g (vector-ref h 6))))
            (vector-set! h 7 (u32 (+ hh (vector-ref h 7)))))
          (let* ([big-sigma-1 (bitwise-xor (rotr e 6) (rotr e 11) (rotr e 25))]
                 [ch (u32 (bitwise-xor (bitwise-and e f)
                                       (bitwise-and (bitwise-not e) g)))]
                 [t1 (u32 (+ hh big-sigma-1 ch
                             (vector-ref K i)
                             (vector-ref w i)))]
                 [big-sigma-0 (bitwise-xor (rotr a 2) (rotr a 13) (rotr a 22))]
                 [maj (u32 (bitwise-xor (bitwise-and a b)
                                        (bitwise-and a c)
                                        (bitwise-and b c)))]
                 [t2 (u32 (+ big-sigma-0 maj))])
            (compress (u32 (+ t1 t2)) a b c
                      (u32 (+ d t1)) e f g
                      (add1 i))))))
  (define out (make-bytes 32))
  (for ([i (in-range 8)])
    (define v (vector-ref h i))
    (for ([j (in-range 4)])
      (bytes-set! out (+ (* i 4) j)
                  (bitwise-and (arithmetic-shift v (- (* j 8) 24)) #xff))))
  out)

(define (sha256 in)
  (cond
    [(input-port? in) (sha256-bytes (port->bytes in))]
    [(bytes? in) (sha256-bytes in)]
    [else (raise-argument-error 'sha256 "(or/c input-port? bytes?)" in)]))

(define hex-digits "0123456789abcdef")

(define (bytes->hex-string bstr)
  (define out (make-string (* 2 (bytes-length bstr))))
  (for ([i (in-range (bytes-length bstr))])
    (define v (bytes-ref bstr i))
    (string-set! out (* 2 i) (string-ref hex-digits (arithmetic-shift v -4)))
    (string-set! out (add1 (* 2 i)) (string-ref hex-digits (bitwise-and v #x0f))))
  out)

(define (sha256-hex in)
  (bytes->hex-string (sha256 in)))
