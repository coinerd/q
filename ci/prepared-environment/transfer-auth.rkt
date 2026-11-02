#lang racket/base

;; q/ci/prepared-environment/transfer-auth.rkt — W1 performance repair
;; round (open-gap closure, repair round 6), review-hardened.
;;
;; Two child-side primitives of the compiled-root launcher bootstrap:
;;
;;   1. authenticate-compiled-module — the transfer-module loader. The
;;      naive mechanism (hash the zo path, then dynamic-require the SAME
;;      path) authenticates one read and executes a SECOND one: between
;;      the canary's open and the module system's open, a same-user
;;      process can swap the bytes and the executed table is never the
;;      authenticated table (the in-process per-load guard has the same
;;      two-open shape and so does every hash-then-load canary). The
;;      repaired mechanism authenticates the EXACT BYTES EXECUTED: the
;;      zo is read exactly once, the bytes are digested against the
;;      baked canary, and the compiled form is read FROM THOSE BYTES in
;;      memory and evaluated — the module system never re-opens the
;;      path, so there is no second read to race. Mutation after
;;      verification fails closed at the digest check; a digest that
;;      authenticates non-compiled bytes fails closed at the
;;      compiled-expression check.
;;
;;   2. make-atomic-telemetry-writer — the child hit-counter writer. A
;;      truncate-in-place rewrite exposes partial/empty counter files to
;;      a concurrent aggregator read and to a killed child (timeout kill
;;      mid-write leaves the counter file truncated). The writer builds
;;      each counter file under a unique temporary name IN THE SAME
;;      DIRECTORY (same filesystem, so the final rename(2) is atomic)
;;      and renames it over the target: a reader observes either the
;;      previous complete datum or the new complete datum — never a
;;      partial one — and a killed child leaves the previous complete
;;      datum plus at worst an ignored temporary file. The temporary
;;      names deliberately do NOT match the child telemetry filename
;;      contract (q-compiled-root-hit*.rktd), so the aggregator ignores
;;      them as foreign files.
;;
;;   3. reserve-child-telemetry-target — the per-process counter-name
;;      reservation. The bootstrap used to name the counter file with
;;      (make-temporary-file "q-compiled-root-hit~a.rktd" ...) which
;;      CREATES an empty file matching the child telemetry filename
;;      contract BEFORE the first complete snapshot was renamed into
;;      place: a child killed in that startup window left the aggregator
;;      a malformed contract file to reject loudly (reviewer P2). The
;;      reservation computes a unique contract-matching NAME and creates
;;      NO file at that name: the contract filename materializes only
;;      when the writer's first atomic rename publishes a complete
;;      datum. The name's unique middle is claimed by an ATOMICALLY
;;      created probe under the contract-disjoint temporary prefix
;;      below, and the probe is KEPT for the reservation's lifetime —
;;      deleting it would release the claim (a name is claimable again
;;      exactly when no file exists at it), letting a concurrent process
;;      pinning the same clock readings win the same digits and collapse
;;      both children onto one counter target whose snapshots overwrite
;;      each other (review blocker: cross-process target collision). An
;;      interrupted startup leaves at worst a foreign temporary — never
;;      a contract file.
;;
;; This module is loaded by the child bootstrap BEFORE the compiled-file
;; roots are activated, together with the manifest digest module — the
;; bootstrap's trusted base. It is required by compiled-root.rkt so the
;; producer's raco make closure compiles it into every published root.

(require racket/file
         racket/path
         "manifest.rkt")

(provide authenticate-compiled-module
         make-atomic-telemetry-writer
         reserve-child-telemetry-target
         reservation-digits
         compiled-root-telemetry-tmp-prefix)

;; authenticate-compiled-module : path? hex-string? [string?] -> void?
;;   Read ZO-PATH exactly once, digest those bytes, fail closed on any
;;   mismatch, then declare+instantiate the compiled module carried by
;;   those same bytes in (current-namespace). The caller fetches its
;;   bindings from the registry by the module's declared name (e.g.
;;   (dynamic-require ''load-keys 'sym)) — no path-based load, no second
;;   open of ZO-PATH anywhere on the execution path.
(define (authenticate-compiled-module zo-path expected-digest [what "compiled load-key table"])
  (define bs (file->bytes zo-path))
  (unless (string=? (sha256-hex bs) expected-digest)
    (error 'compiled-root-toctou "~a mutated after verification: ~a" what zo-path))
  (define compiled
    (parameterize ([read-accept-compiled #t])
      (read (open-input-bytes bs))))
  (unless (compiled-expression? compiled)
    (error 'compiled-root-toctou "~a does not authenticate to a compiled module: ~a" what zo-path))
  ;; Executes EXACTLY the digested bytes: eval of the in-memory compiled
  ;; form is the same step the default file loader performs, minus the
  ;; second file open.
  (eval compiled)
  (void))

;; compiled-root-telemetry-tmp-prefix : string?
;;   The writer's temporary-name prefix AND the reservation claim
;;   probe's prefix. Deliberately disjoint from the child telemetry
;;   filename contract (q-compiled-root-hit*.rktd, see
;;   compiled-root.rkt): a temporary file abandoned by a killed child —
;;   or the reservation's own kept claim probe — is a foreign file to
;;   the aggregator and is
;;   ignored silently.
(define compiled-root-telemetry-tmp-prefix "q-compiled-root-tmp")

;; make-atomic-telemetry-writer : path? -> (bytes? exact-nonnegative-integer? exact-nonnegative-integer? . -> . void?)
;;   Build the counter writer for TARGET. The returned procedure writes
;;   (compiled-root-child-telemetry root-hits loads bytes-served) to a
;;   unique same-directory temporary file and atomically renames it over
;;   TARGET. Telemetry is best-effort (fail-open): any filesystem error
;;   — including a vanished hits directory — is swallowed; the per-load
;;   digest checks stay fail-closed regardless.
(define (make-atomic-telemetry-writer target)
  (define target-dir (path-only target))
  (lambda (hits loads bytes-served)
    (with-handlers ([exn:fail? (lambda (_) (void))])
      (define tmp
        (make-temporary-file (string-append compiled-root-telemetry-tmp-prefix "~a") #f target-dir))
      (dynamic-wind void
                    (lambda ()
                      (call-with-output-file tmp
                                             (lambda (out)
                                               (write (list 'compiled-root-child-telemetry
                                                            (cons 'root-hits hits)
                                                            (cons 'loads loads)
                                                            (cons 'bytes-served bytes-served))
                                                      out))
                                             #:exists 'truncate)
                      ;; same-directory rename(2): readers observe whole datums only
                      (rename-file-or-directory tmp target #t))
                    (lambda ()
                      (with-handlers ([exn:fail? (lambda (_) (void))])
                        (when (file-exists? tmp)
                          (delete-file tmp))))))))

;; reservation-digits : (parameter/c (-> string?))
;;   The candidate unique middle for a new reservation claim. The
;;   production default is make-temporary-file's own recipe —
;;   (current-seconds) concatenated with (current-inexact-milliseconds).
;;   Exposed as a parameter ONLY so the multiprocess collision
;;   regression can pin IDENTICAL clock readings in two real processes
;;   (the same-clock collapse the deleted-probe scheme lost to);
;;   production never rebinds it.
(define reservation-digits
  (make-parameter
   (lambda ()
     (format "~a~a" (current-seconds) (inexact->exact (truncate (current-inexact-milliseconds)))))))

;; reserve-child-telemetry-target : path? string? string? -> (or/c path? #f)
;;   Reserve the child's per-process counter-file name inside HITS-DIR:
;;   HIT-PREFIX + a unique middle + HIT-SUFFIX (the child telemetry
;;   filename contract). Creates NO file at the reserved name — the
;;   contract filename materializes only when the writer's first
;;   atomic rename publishes a complete datum (reviewer P2: an
;;   interrupted startup must leave no empty/malformed contract file).
;;   The middle is claimed ATOMICALLY cross-process: each candidate
;;   middle is materialized as a probe under the contract-disjoint
;;   compiled-root-telemetry-tmp-prefix by an exclusive create that
;;   either wins the name or loses it and retries with a fresh middle
;;   (the same arbitration discipline make-temporary-file applies to
;;   its own names). The winning probe is then KEPT: it IS the
;;   reservation, holding its middle for the claim's lifetime. (The
;;   probe used to be deleted after the digits were extracted;
;;   deletion releases the claim — a name is claimable again exactly
;;   when no file exists at it — and the fallback disambiguator, the
;;   per-process counter, starts at the SAME value in every process,
;;   so two same-clock processes collapsed onto one target: the
;;   review blocker's cross-process collision.) A kept probe is
;;   foreign evidence to the aggregator — ignored silently, exactly
;;   like an abandoned writer temporary — and the counter still
;;   disambiguates consecutive reservations within one process.
;;   Returns #f on any filesystem failure (unusable hits directory),
;;   exactly like the writer's fail-open discipline: telemetry is
;;   best-effort and must never break a child.
(define reservation-counter 0)
(define (reserve-child-telemetry-target hits-dir hit-prefix hit-suffix)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (set! reservation-counter (add1 reservation-counter))
    (let loop ([try 0])
      (define digits
        (if (zero? try)
            ((reservation-digits))
            (string-append ((reservation-digits)) "-" (number->string try))))
      (define probe (build-path hits-dir (string-append compiled-root-telemetry-tmp-prefix digits)))
      (with-handlers ([exn:fail:filesystem:exists? (lambda (_) (loop (add1 try)))])
        ;; THE claim: an atomic cross-process exclusive create. The
        ;; probe is created once and NEVER deleted — it holds the
        ;; middle for the reservation's lifetime. Deleting it would
        ;; release the claim (a name is claimable again exactly when no
        ;; file exists at it) and let a concurrent process pinning the
        ;; same clock readings collapse onto the same counter target.
        ;; The probe is contract-disjoint: foreign to the aggregator,
        ;; ignored silently, never telemetry, never an empty contract
        ;; file.
        (call-with-output-file probe void #:exists 'error)
        (build-path
         hits-dir
         (string-append hit-prefix digits "-" (number->string reservation-counter) hit-suffix))))))
