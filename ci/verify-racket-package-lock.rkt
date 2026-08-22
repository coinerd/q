#lang racket/base

;; q/ci/verify-racket-package-lock.rkt — version-indexed lock verification
;; (v1.00.11 W1; schema-revision 2).
;;
;; Contract:
;;   * The lock (ci/racket-package-lock.rktd) carries one reviewed entry per
;;     supported runtime under `runtimes`, each binding a `racket-version`
;;     string to that runtime's reviewed `packages` table.
;;   * The requested runtime comes from (in precedence order):
;;       1. `--racket-version <v>` / `--racket-version=<v>` command-line flag
;;       2. `RACKET_LOCK_RACKET_VERSION` environment variable
;;       3. the running Racket's `(version)`
;;   * Exactly one entry matches — exact string equality, never a prefix and
;;     never a fallback to another runtime's entry. No match => nonzero exit.
;;   * On success we verify the selected entry's package checksums against
;;     the user package store (unchanged v1.00.10 semantics) and print the
;;     selected version plus a SHA-256 lock digest over the canonicalized
;;     selected entry, so callers (setup-racket action cache keys) derive
;;     runtime-scoped exact cache keys from it.

(require racket/format
         racket/hash
         racket/list
         racket/match
         racket/port
         racket/string
         racket/system)

(define lock-path "ci/racket-package-lock.rktd")

(define (fail fmt . args)
  (apply eprintf (string-append "racket package lock verification failed: " fmt "\n") args)
  (exit 1))

;; ------------------------------------------------------------
;; Pure-racket/base SHA-256 (FIPS 180-4) over bytes — same algorithm as
;; ci/prepared-environment/manifest.rkt, so this runs on any minimal Racket
;; distribution with no openssl/sha256 dependency.
;; ------------------------------------------------------------

(define (sha256-bytes->hex-string b)
  (define (u32 x) (bitwise-and x #xffffffff))
  (define (ror32 x n)
    (bitwise-ior (arithmetic-shift x (- n))
                 (bitwise-and (arithmetic-shift x (- 32 n)) #xffffffff)))
  (define k
    (list #x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
          #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
          #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
          #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
          #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
          #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
          #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
          #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))
  (define (word-add . xs) (u32 (apply + xs)))
  (define (pad! bs)
    ;; Append 0x80, then k zero bytes, then the 64-bit big-endian bit length,
    ;; choosing k in [0,63] so the total is a multiple of 64.
    (define len (bytes-length bs))
    (define pad-n (modulo (- 64 (modulo (+ len 9) 64)) 64))
    (bytes-append bs (bytes 128) (make-bytes pad-n 0)
                  (integer->integer-bytes (* len 8) 8 #f #t)))
  (define (process-block in h0)
    (define (at i) (integer-bytes->integer in #f #t (* 4 i) (* 4 (add1 i))))
    (define w (make-vector 64 0))
    (for ([i (in-range 16)])
      (vector-set! w i (at i)))
    (for ([i (in-range 16 64)])
      (define wi-15 (vector-ref w (- i 15)))
      (define wi-2 (vector-ref w (- i 2)))
      (define s0 (bitwise-xor (ror32 wi-15 7) (ror32 wi-15 18) (arithmetic-shift wi-15 -3)))
      (define s1 (bitwise-xor (ror32 wi-2 17) (ror32 wi-2 19) (arithmetic-shift wi-2 -10)))
      (vector-set! w i (word-add (vector-ref w (- i 16)) s0 (vector-ref w (- i 7)) s1)))
    (define a (vector-ref h0 0)) (define bb (vector-ref h0 1))
    (define c (vector-ref h0 2)) (define d (vector-ref h0 3))
    (define e (vector-ref h0 4)) (define f (vector-ref h0 5))
    (define g (vector-ref h0 6)) (define hh (vector-ref h0 7))
    (for ([i (in-range 64)])
      (define s1 (bitwise-xor (ror32 e 6) (ror32 e 11) (ror32 e 25)))
      (define ch (bitwise-ior (bitwise-and e f) (bitwise-and (bitwise-not e) g)))
      (define temp1 (word-add hh s1 ch (list-ref k i) (vector-ref w i)))
      (define s0 (bitwise-xor (ror32 a 2) (ror32 a 13) (ror32 a 22)))
      (define maj (bitwise-ior (bitwise-and a bb) (bitwise-and a c) (bitwise-and bb c)))
      (define temp2 (word-add s0 maj))
      (set! hh g) (set! g f) (set! f e) (set! e (word-add d temp1))
      (set! d c) (set! c bb) (set! bb a) (set! a (word-add temp1 temp2)))
    (vector (word-add (vector-ref h0 0) a)
            (word-add (vector-ref h0 1) bb)
            (word-add (vector-ref h0 2) c)
            (word-add (vector-ref h0 3) d)
            (word-add (vector-ref h0 4) e)
            (word-add (vector-ref h0 5) f)
            (word-add (vector-ref h0 6) g)
            (word-add (vector-ref h0 7) hh)))
  (define h (list->vector (list #x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                                #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19)))
  (define hex-digits "0123456789abcdef")
  (define (word->hex n)
    (define s (make-string 8 #\0))
    (let loop ([i 7] [v n])
      (when (>= i 0)
        (string-set! s i (string-ref hex-digits (bitwise-and v 15)))
        (loop (sub1 i) (arithmetic-shift v -4))))
    s)
  (define bytes-in (pad! b))
  (let loop ([off 0] [hv h])
    (if (>= off (bytes-length bytes-in))
        (apply string-append (for/list ([x (in-vector hv)]) (word->hex x)))
        (loop (+ off 64) (process-block (subbytes bytes-in off (+ off 64)) hv)))))

;; ------------------------------------------------------------
;; Requested runtime version
;; ------------------------------------------------------------

(define (requested-version)
  (define args (vector->list (current-command-line-arguments)))
  (define from-flag
    (let loop ([args args])
      (match args
        [(list-rest "--racket-version" v _rest) v]
        [(list-rest other _rest)
         #:when (string-prefix? other "--racket-version=")
         (substring other (string-length "--racket-version="))]
        [(list-rest _ rest) (loop rest)]
        [_ #f])))
  (define env-v (getenv "RACKET_LOCK_RACKET_VERSION"))
  (cond [(string? from-flag) from-flag]
        [(and (string? env-v) (non-empty-string? env-v)) env-v]
        [else (version)]))

;; ------------------------------------------------------------
;; Lock loading + entry selection (exact match, no fallback)
;; ------------------------------------------------------------

(define lock
  (with-handlers ([exn:fail? (lambda (e) (fail "cannot read ~a: ~a" lock-path (exn-message e)))])
    (call-with-input-file lock-path read)))

(unless (hash? lock) (fail "~a must contain a hash" lock-path))

(define schema-revision (hash-ref lock 'schema-revision #f))
(define runtimes (hash-ref lock 'runtimes #f))

(cond
  [(not (equal? schema-revision 2))
   (fail "unsupported lock schema-revision ~s (expected 2); the single-runtime schema-revision-1 lock was removed in v1.00.11 W1 — migrate to a version-indexed lock"
         schema-revision)]
  [(not (hash? runtimes))
   (fail "~a must define a `runtimes` hash (schema-revision 2)" lock-path)])

(for ([(v e) (in-hash runtimes)])
  (unless (and (hash? e)
               (string? (hash-ref e 'racket-version #f))
               (hash? (hash-ref e 'packages #f))
               (string=? (hash-ref e 'racket-version) v))
    (fail "runtimes entry ~s is malformed: needs racket-version matching its key and a packages hash" v)))

(define want-version (requested-version))
(define entry (hash-ref runtimes want-version #f))

(unless entry
  (fail "no lock entry for Racket version ~a; available entries: ~a (exact match required — no cross-runtime fallback)"
        want-version
        (string-join (sort (map (lambda (kv) (format "~a" (car kv))) (hash->list runtimes))
                           string<?)
                     ", ")))

(define expected-version (hash-ref entry 'racket-version))
(define expected-packages (hash-ref entry 'packages))

;; Canonicalized selected entry: version binding + packages sorted by name.
;; The digest is deterministic for identical lock content regardless of
;; in-memory hash iteration order.
(define canonical-entry
  (list 'racket-version expected-version
        'packages (sort (hash->list expected-packages) string<? #:key car)))

(define lock-digest
  (sha256-bytes->hex-string (string->bytes/utf-8 (format "~s" canonical-entry))))

;; ------------------------------------------------------------
;; Package-store verification of the SELECTED entry (v1.00.10 semantics)
;; ------------------------------------------------------------

(define raco-path
  (or (find-executable-path "raco")
      (fail "cannot locate raco on PATH")))

(define-values (proc stdout stdin stderr)
  (subprocess #f #f #f raco-path "pkg" "show"
              "--scope" "user" "--all" "--long" "--full-checksum"))
(close-output-port stdin)
(define output (port->string stdout))
(define errors (port->string stderr))
(subprocess-wait proc)
(unless (zero? (subprocess-status proc))
  (fail "raco pkg show failed: ~a" (string-trim errors)))

;; `raco pkg show --long --full-checksum` emits one package per line. Auto
;; dependencies have a trailing `*` in the package column; q itself has `#f`
;; because it is deliberately a checkout link and is verified separately.
(define package-rx #px"^([^[:space:]\\*]+)\\*?[[:space:]]+([0-9a-f]{40}|#f)[[:space:]]+")
(define actual
  (for/fold ([table (hash)]) ([line (in-list (string-split output "\n"))])
    (match (regexp-match package-rx line)
      [(list _ name checksum) (hash-set table name checksum)]
      [_ table])))

(for ([(name checksum) (in-hash expected-packages)])
  (define actual-checksum (hash-ref actual name #f))
  (unless actual-checksum
    (fail "locked package ~a is absent from the user package store" name))
  (unless (string=? actual-checksum checksum)
    (fail "locked package ~a has checksum ~a, expected ~a (runtime ~a)"
          name actual-checksum checksum expected-version)))

(unless (hash-has-key? actual "q")
  (fail "q is absent from the user package store"))

;; Machine-readable line for runtime-scoped exact cache-key derivation:
(printf "lock-ok runtime=~a schema-revision=~a lock-digest=~a packages=~a\n"
        expected-version schema-revision lock-digest (hash-count expected-packages))
(printf "Racket package lock verified: ~a external packages for Racket ~a\n"
        (hash-count expected-packages)
        expected-version)
