#lang racket/base

;; q/ci/prepared-environment/manifest.rkt — v1.00.11 W3
;; Repository-owned manifest contract for prepared Racket environment
;; artifacts (report-only pilot; NO consumer cutover in this wave).
;;
;; Two subcommands:
;;
;;   emit   — write manifest.json binding the artifact to its exact
;;            production tuple (schema revision, repository, Git SHA, OS,
;;            architecture, Racket version/variant/distribution, selected
;;            version-indexed lock digest, source digest, allowlisted
;;            path set).
;;
;;   verify — fail-closed validation BEFORE any artifact byte is moved into
;;            a live location: every expected field must match the
;;            consumer's tuple, the recorded allowlist must be a subset of
;;            the fixed known-safe prefix set, and EVERY path under the
;;            extracted artifact root must fall inside the allowlist
;;            (extra paths — .git, home-dir files, credentials — fail).
;;
;; Invariants (milestone v1.00.11 §2): I8 (hosted jobs share nothing but
;; validated artifacts), I9 (immutable, tuple-scoped artifacts),
;; I10 (allowlisted contents only). This script never mutates the root on
;; failure and exits nonzero on ANY mismatch.

(require racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/string
         json)

(define schema-revision-supported 1)

;; The fixed universe of artifact-safe top-level prefixes. The producer may
;; only allowlist entries from this set; the verifier enforces the same set
;; so a tampered manifest cannot widen its own allowlist.
(define known-safe-prefixes '("addon-store/" "q-compiled/" "manifest.json"))

(define (die fmt . args)
  (apply eprintf (string-append "prepared-environment manifest: " fmt "\n") args)
  (exit 1))

(define (sha256-file p)
  ;; Pure-racket/base SHA-256 (FIPS 180-4) so this contract runs on any
  ;; minimal Racket distribution — no openssl/sha256 dependency.
  (define (u32 x)
    (bitwise-and x #xffffffff))
  (define (ror32 x n)
    (bitwise-ior (arithmetic-shift x (- n)) (bitwise-and (arithmetic-shift x (- 32 n)) #xffffffff)))
  (define k
    (list #x428a2f98
          #x71374491
          #xb5c0fbcf
          #xe9b5dba5
          #x3956c25b
          #x59f111f1
          #x923f82a4
          #xab1c5ed5
          #xd807aa98
          #x12835b01
          #x243185be
          #x550c7dc3
          #x72be5d74
          #x80deb1fe
          #x9bdc06a7
          #xc19bf174
          #xe49b69c1
          #xefbe4786
          #x0fc19dc6
          #x240ca1cc
          #x2de92c6f
          #x4a7484aa
          #x5cb0a9dc
          #x76f988da
          #x983e5152
          #xa831c66d
          #xb00327c8
          #xbf597fc7
          #xc6e00bf3
          #xd5a79147
          #x06ca6351
          #x14292967
          #x27b70a85
          #x2e1b2138
          #x4d2c6dfc
          #x53380d13
          #x650a7354
          #x766a0abb
          #x81c2c92e
          #x92722c85
          #xa2bfe8a1
          #xa81a664b
          #xc24b8b70
          #xc76c51a3
          #xd192e819
          #xd6990624
          #xf40e3585
          #x106aa070
          #x19a4c116
          #x1e376c08
          #x2748774c
          #x34b0bcb5
          #x391c0cb3
          #x4ed8aa4a
          #x5b9cca4f
          #x682e6ff3
          #x748f82ee
          #x78a5636f
          #x84c87814
          #x8cc70208
          #x90befffa
          #xa4506ceb
          #xbef9a3f7
          #xc67178f2))
  (define (word-add . xs)
    (u32 (apply + xs)))
  (define (pad! bs)
    ;; Append 0x80, then k zero bytes, then the 64-bit big-endian bit length,
    ;; choosing k in [0,63] so the total is a multiple of 64.
    (define len (bytes-length bs))
    (define k (modulo (- 64 (modulo (+ len 9) 64)) 64))
    (bytes-append bs (bytes 128) (make-bytes k 0) (integer->integer-bytes (* len 8) 8 #f #t)))
  (define (process-block in h0)
    (define (at i)
      (integer-bytes->integer in #f #t (* 4 i) (* 4 (add1 i))))
    (define w (make-vector 64 0))
    (for ([i (in-range 16)])
      (vector-set! w i (at i)))
    (for ([i (in-range 16 64)])
      (define wi-15 (vector-ref w (- i 15)))
      (define wi-2 (vector-ref w (- i 2)))
      (define s0 (bitwise-xor (ror32 wi-15 7) (ror32 wi-15 18) (arithmetic-shift wi-15 -3)))
      (define s1 (bitwise-xor (ror32 wi-2 17) (ror32 wi-2 19) (arithmetic-shift wi-2 -10)))
      (vector-set! w i (word-add (vector-ref w (- i 16)) s0 (vector-ref w (- i 7)) s1)))
    (define a (vector-ref h0 0))
    (define b (vector-ref h0 1))
    (define c (vector-ref h0 2))
    (define d (vector-ref h0 3))
    (define e (vector-ref h0 4))
    (define f (vector-ref h0 5))
    (define g (vector-ref h0 6))
    (define hh (vector-ref h0 7))
    (for ([i (in-range 64)])
      (define s1 (bitwise-xor (ror32 e 6) (ror32 e 11) (ror32 e 25)))
      (define ch (bitwise-ior (bitwise-and e f) (bitwise-and (bitwise-not e) g)))
      (define temp1 (word-add hh s1 ch (list-ref k i) (vector-ref w i)))
      (define s0 (bitwise-xor (ror32 a 2) (ror32 a 13) (ror32 a 22)))
      (define maj (bitwise-ior (bitwise-and a b) (bitwise-and a c) (bitwise-and b c)))
      (define temp2 (word-add s0 maj))
      (set! hh g)
      (set! g f)
      (set! f e)
      (set! e (word-add d temp1))
      (set! d c)
      (set! c b)
      (set! b a)
      (set! a (word-add temp1 temp2)))
    (vector (word-add (vector-ref h0 0) a)
            (word-add (vector-ref h0 1) b)
            (word-add (vector-ref h0 2) c)
            (word-add (vector-ref h0 3) d)
            (word-add (vector-ref h0 4) e)
            (word-add (vector-ref h0 5) f)
            (word-add (vector-ref h0 6) g)
            (word-add (vector-ref h0 7) hh)))
  (define h
    (list->vector
     (list #x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19)))
  (define hex-digits "0123456789abcdef")
  (define (word->hex n)
    (define s (make-string 8 #\0))
    (let loop ([i 7]
               [v n])
      (when (>= i 0)
        (string-set! s i (string-ref hex-digits (bitwise-and v 15)))
        (loop (sub1 i) (arithmetic-shift v -4))))
    s)
  (define bytes-in (pad! (file->bytes p)))
  (let loop ([off 0]
             [hv h])
    (if (>= off (bytes-length bytes-in))
        (apply string-append
               (for/list ([x (in-vector hv)])
                 (word->hex x)))
        (loop (+ off 64) (process-block (subbytes bytes-in off (+ off 64)) hv)))))

;; path-in-allowlist? : path? (listof string?) -> boolean
;; True when the repo-relative POSIX path equals an allowlist entry or is
;; strictly under one of its directory prefixes ("dir/" semantics).
(define (path-in-allowlist? rel allowlist)
  (define s (path->string rel))
  (for/or ([prefix (in-list allowlist)])
    (or (string=? s prefix) (and (string-suffix? prefix "/") (string-prefix? s prefix)))))

(define (emit! out
               schema-revision
               repository
               git-sha
               os
               arch
               racket-version
               racket-variant
               racket-distribution
               lock-digest
               source-digest
               allowlist)
  (unless (= schema-revision schema-revision-supported)
    (die "unsupported schema revision ~a (supported: ~a)" schema-revision schema-revision-supported))
  ;; The producer may only allowlist known-safe prefixes (I10).
  (for ([p (in-list allowlist)])
    (unless (member p known-safe-prefixes)
      (die "refusing to allowlist non-safe path ~a (known-safe: ~a)" p known-safe-prefixes)))
  (define manifest
    (hasheq 'schema_revision
            schema-revision
            'repository
            repository
            'git_sha
            git-sha
            'os
            os
            'arch
            arch
            'racket_version
            racket-version
            'racket_variant
            racket-variant
            'racket_distribution
            racket-distribution
            'lock_digest
            lock-digest
            'source_digest
            source-digest
            'allowlisted_paths
            allowlist))
  (call-with-output-file out
                         #:exists 'replace
                         (lambda (p)
                           (write-json manifest p)
                           (newline p)))
  (printf "prepared-environment manifest written: ~a (git ~a, ~a/~a, Racket ~a, lock ~a)~n"
          out
          git-sha
          os
          arch
          racket-version
          lock-digest))

(define (verify! manifest-path
                 expect-repository
                 expect-git-sha
                 expect-os
                 expect-arch
                 expect-racket-version
                 expect-lock-digest
                 root)
  (define manifest
    (with-handlers ([exn:fail? (lambda (e)
                                 (die "cannot read manifest ~a: ~a"
                                      (path->string manifest-path)
                                      (exn-message e)))])
      (call-with-input-file manifest-path read-json)))
  (unless (hash? manifest)
    (die "manifest is not a JSON object"))

  (define (field name)
    (hash-ref manifest name (lambda () (die "manifest is missing required field ~a" name))))

  (define checks
    (list (list "schema_revision" (field 'schema_revision) schema-revision-supported)
          (list "repository" (field 'repository) expect-repository)
          (list "git_sha" (field 'git_sha) expect-git-sha)
          (list "os" (field 'os) expect-os)
          (list "arch" (field 'arch) expect-arch)
          (list "racket_version" (field 'racket_version) expect-racket-version)
          (list "lock_digest" (field 'lock_digest) expect-lock-digest)))
  (for ([c (in-list checks)])
    (match-define (list name actual expected) c)
    (unless (equal? actual expected)
      (die
       "manifest mismatch on ~a: artifact has ~s, consumer requires ~s — \
            refusing to restore (no rebuild, no silent repair)"
       name
       actual
       expected)))

  (define allowlist (field 'allowlisted_paths))
  (unless (and (list? allowlist) (andmap string? allowlist) (pair? allowlist))
    (die "manifest allowlisted_paths must be a non-empty list of strings"))
  (for ([p (in-list allowlist)])
    (unless (member p known-safe-prefixes)
      (die "manifest allowlists non-safe path ~s — possible tampering" p)))

  ;; Every extracted path must be inside the allowlist (I10). The root is
  ;; left untouched on failure.
  (define offenders (non-allowlisted-files root allowlist))
  (unless (null? offenders)
    (die "artifact contains ~a non-allowlisted path(s), e.g. ~a — refusing to restore"
         (length offenders)
         (take offenders (min 5 (length offenders)))))

  (printf
   "prepared-environment manifest VERIFIED: git ~a, ~a/~a, Racket ~a, lock ~a; \
           all extracted paths inside allowlist~n"
   expect-git-sha
   expect-os
   expect-arch
   expect-racket-version
   expect-lock-digest))

;; All FILES under root (relative POSIX strings) not covered by the
;; allowlist. Traversal starts from the absolute root so the produced
;; paths are stable on every hosted image. Only files are checked:
;; directory entries (including the root itself and bare "addon-store"
;; style names without a trailing slash) can never match a "dir/"
;; prefix entry, and directories carry no payload.
(define (non-allowlisted-files root allowlist)
  (define abs-root (simplify-path (path->complete-path root)))
  (unless (directory-exists? abs-root)
    (die "artifact root ~a does not exist" (path->string abs-root)))
  (define files (find-files file-exists? abs-root))
  (sort (for/list ([p (in-list files)]
                   #:unless (path-in-allowlist? (find-relative-path abs-root (simplify-path p))
                                                allowlist))
          (path->string (find-relative-path abs-root (simplify-path p))))
        string<?))
(module+ main
  (define argv (vector->list (current-command-line-arguments)))
  (match argv
    [(cons "emit" rest)
     (define out #f)
     (define repository #f)
     (define git-sha #f)
     (define os #f)
     (define arch #f)
     (define racket-version #f)
     (define racket-variant #f)
     (define racket-distribution #f)
     (define lock-digest #f)
     (define source-digest #f)
     (define allowlist '())
     (command-line
      #:program "manifest.rkt emit"
      #:argv rest
      #:once-each [("--out") v-out "manifest.json output path" (set! out v-out)]
      [("--repository") v-repo "expected repository" (set! repository v-repo)]
      [("--git-sha") v-sha "producer commit SHA" (set! git-sha v-sha)]
      [("--os") v-os "runner OS" (set! os v-os)]
      [("--arch") v-arch "architecture" (set! arch v-arch)]
      [("--racket-version") v-ver "Racket version" (set! racket-version v-ver)]
      [("--racket-variant") v-var "Racket variant" (set! racket-variant v-var)]
      [("--racket-distribution") v-dist "Racket distribution" (set! racket-distribution v-dist)]
      [("--lock-digest") v-lock "selected lock entry digest" (set! lock-digest v-lock)]
      [("--source-digest") v-src "compilation source digest" (set! source-digest v-src)]
      #:multi [("--allowlist")
               v-path
               "allowlisted artifact path prefix (repeatable)"
               (set! allowlist (cons v-path allowlist))]
      #:args ()
      (for ([required
             (in-list
              (list out repository git-sha os arch racket-version lock-digest source-digest))])
        (when (not required)
          (die "emit: missing required argument(s)")))
      (emit! (string->path out)
             schema-revision-supported
             repository
             git-sha
             os
             arch
             racket-version
             racket-variant
             racket-distribution
             lock-digest
             source-digest
             (reverse allowlist)))]
    [(cons "verify" rest)
     (define manifest-path #f)
     (define expect-repository #f)
     (define expect-git-sha #f)
     (define expect-os #f)
     (define expect-arch #f)
     (define expect-racket-version #f)
     (define expect-lock-digest #f)
     (define root #f)
     (command-line
      #:program "manifest.rkt verify"
      #:argv rest
      #:once-each [("--manifest") v-manifest "path to manifest.json" (set! manifest-path v-manifest)]
      [("--expect-repository") v-repo "consumer repository" (set! expect-repository v-repo)]
      [("--expect-git-sha") v-sha "consumer commit SHA" (set! expect-git-sha v-sha)]
      [("--expect-os") v-os "consumer runner OS" (set! expect-os v-os)]
      [("--expect-arch") v-arch "consumer architecture" (set! expect-arch v-arch)]
      [("--expect-racket-version") v-ver "consumer Racket version" (set! expect-racket-version v-ver)]
      [("--expect-lock-digest") v-lock "consumer lock digest" (set! expect-lock-digest v-lock)]
      [("--root") v-root "extracted artifact root" (set! root v-root)]
      #:args ()
      (for ([required (in-list (list manifest-path
                                     expect-repository
                                     expect-git-sha
                                     expect-os
                                     expect-arch
                                     expect-racket-version
                                     expect-lock-digest
                                     root))])
        (when (not required)
          (die "verify: missing required argument(s)")))
      (verify! (string->path manifest-path)
               expect-repository
               expect-git-sha
               expect-os
               expect-arch
               expect-racket-version
               expect-lock-digest
               (string->path root)))]
    [(cons "digest" rest)
     (define file #f)
     (command-line #:program "manifest.rkt digest"
                   #:argv rest
                   #:once-each [("--file") v-file "file to hash" (set! file v-file)]
                   #:args ()
                   (unless file
                     (die "digest: --file is required"))
                   (displayln (sha256-file (string->path file))))]
    [_
     (eprintf "usage: manifest.rkt emit|verify|digest ...~n")
     (exit 2)]))
