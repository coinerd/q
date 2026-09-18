#lang racket/base

;; q/ci/prepared-environment/compiled-root.rkt — v1.00.30 W3
;;
;; Trusted compiled-root prototype: relocation-safe producer and
;; consumer/resolver for an IMMUTABLE, read-only compiled root outside
;; the checkout (Racket 8.10 `current-compiled-file-roots` semantics).
;;
;; Proven semantics (probe-verified on Racket 8.10 in this wave):
;;   - A zo under an external root is used for a source S iff
;;     mtime(zo) >= mtime(S); the 'modify-seconds check gates on
;;     freshness. There is no mode that trusts stale root bytes.
;;   - For an absolute source S = /D0/.../Dn/name.rkt and an absolute
;;     root entry R, the default compiled-load handler consults exactly
;;       R/D0/.../Dn/compiled/name_rkt.zo
;;     i.e. reroot-path(S-dir, R) = R/<S-dir minus the leading slash>
;;     (the FULL absolute source-directory string is spliced under R)
;;     plus the "compiled" mode subdir and the MUNGED member name
;;     (path-add-extension(name.rkt, ".zo") = name_rkt.zo). Layouts
;;     that drop path components, insert the source file name as a
;;     directory, or use the un-munged source name are NOT consulted
;;     (probe-verified in all three negative directions).
;;   - Consequently a producer-built tree is relocatable only when the
;;     consumer's absolute source-directory string matches the
;;     producer's — never assume it. The job-local per-consumer mapping
;;     directory here materializes the required layout as symlinks from
;;     the consumer's absolute path structure into the immutable
;;     published payload, so the published root itself is never written.
;;
;; Safety contract implemented here:
;;   - Pure manifest validation BEFORE any compiled code runs
;;     (compiled-root-manifest.rkt): digests, trusted producer, exact
;;     executable, platform/ABI, locked dependency set, traversal gate.
;;   - Consumer source digests must match the manifest before the
;;     resolver forces source mtimes older than the root zo mtimes
;;     (mtime forcing only ever happens for byte-identical content).
;;   - TOCTOU canary: a current-load wrapper re-verifies each root
;;     member's digest at load time; mutation after validation fails
;;     closed instead of executing tampered bytecode.
;;   - Any validation failure resolves to a single EAGER fallback
;;     (raco make in the job-local checkout at the setup boundary,
;;     W2 containment semantics) or a hard failure — never producer-era
;;     code and never uncontrolled lazy compilation.
;;   - Publication is atomic (staging directory + rename) and the
;;     published root is made read-only; consumers never write to it.

(require racket/bool
         racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/port
         racket/string
         racket/system
         "compiled-root-manifest.rkt")

(provide (struct-out compiled-root)
         (struct-out compiled-root-telemetry)
         checkout-compilation-clean?
         build-compiled-root!
         publish-compiled-root!
         load-compiled-root-manifest
         resolve-compiled-root!
         compiled-root-lookup-directory
         compiled-root-activate-parameters
         install-compiled-root-toctou-guard!
         compiled-root-launch-arguments
         source->zo-name
         eager-fallback-compile!
         telemetry-snapshot
         reset-telemetry!
         with-compiled-root
         reason->string)

;; --------------------------------------------------------------- structs

;; A resolved, validated compiled root ready for activation.
(struct compiled-root
        (manifest ; validated manifest hash
         root-dir ; immutable published payload directory
         map-dir ; job-local per-consumer mapping directory
         checkout ; consumer checkout (current source of truth)
         reason) ; #t when resolved, else failure string
  #:transparent)

(struct compiled-root-telemetry
        (root-hits ; loads satisfied from the root
         fallbacks ; eager fallback activations
         loads ; module loads observed
         bytes-served ; root bytes loaded
         mechanism) ; exact lookup mechanism string
  #:transparent)

;; ---------------------------------------------------------------- state

(define telemetry (make-parameter (compiled-root-telemetry 0 0 0 0 "none")))

(define (telemetry-snapshot)
  (telemetry))

(define (reset-telemetry! [t (compiled-root-telemetry 0 0 0 0 "none")])
  (telemetry t))

(define (reason->string r)
  (if (eq? r #t)
      "ok"
      (format "~a" r)))

;; ------------------------------------------------------ producer helpers

;; checkout-compilation-clean?
;;   Reject dirty/untracked/generated compilation inputs at the producer
;;   boundary unless they are deliberately frozen by the caller. Runs
;;   `git status --porcelain` restricted to the declared source paths.
;; Each pathspec must be its own argument: a joined single argument is
;; matched literally by git and silently matches nothing (which would
;; accept dirty inputs — the exact bug class this gate exists for).
(define (checkout-compilation-clean? checkout source-rel-paths)
  (define out
    (with-output-to-string (lambda ()
                             (apply system*/exit-code
                                    (find-executable-path "git")
                                    "-C"
                                    (path->string (path->complete-path checkout))
                                    "status"
                                    "--porcelain"
                                    "--"
                                    source-rel-paths))))
  (string=? (string-trim out) ""))

;; Collect every zo raco make produced for the checkout (module +
;; dependency closure that lives inside the checkout tree), as
;; (list src-rel zo-rel) pairs. Excludes docs/ and any zo without a
;; sibling source file (generated artifacts without sources are not
;; rerootable identity-wise and are rejected).
(define (collect-checkout-compilation-pairs checkout)
  (define abs (path->complete-path checkout))
  (for/list ([zo (in-list (find-files (lambda (p) (regexp-match? #rx"\\.zo$" (path->string p))) abs))]
             #:unless (member "docs" (map path->string (explode-path (find-relative-path abs zo)))))
    (define rel (path->string (find-relative-path abs zo)))
    (match-define (list _ ... "compiled" name) (map path->string (explode-path zo)))
    (define src-name (regexp-replace #rx"_rkt[.]zo$" name ".rkt"))
    (define-values (compiled-dir _zo-file _zo-dir?) (split-path zo))
    (define-values (src-dir _f _d?) (split-path compiled-dir))
    (define src-abs (build-path src-dir src-name))
    (and (file-exists? src-abs)
         (list (path->string (find-relative-path abs src-abs))
               (path->string (find-relative-path abs zo))))))

;; build-compiled-root!
;;   Producer: eagerly compile the declared modules (plus their
;;   in-checkout dependency closure) with the CURRENT racket, harvest the
;;   produced zos, build the manifest, and return it. The payload staging
;;   directory mirrors checkout-relative paths so rel paths in the
;;   manifest are stable.
(define (build-compiled-root! #:checkout checkout
                              #:modules modules
                              #:staging-dir staging-dir
                              #:producer-label label
                              #:producer-trusted? [trusted? #t]
                              #:lockfile [lockfile #f])
  (define abs-checkout (simplify-path (path->complete-path checkout)))
  (unless (directory-exists? abs-checkout)
    (error 'build-compiled-root! "checkout missing: ~a" abs-checkout))
  ;; Reject any dirty/untracked/generated file in the source directories
  ;; of the declared compilation inputs, not only in the declared files
  ;; themselves (W3 plan: dirty/untracked inputs must fail the producer
  ;; gate unless deliberately frozen in).
  (define watched-dirs
    (remove-duplicates (for/list ([m (in-list modules)])
                         (define-values (base _n _d) (split-path (build-path abs-checkout m)))
                         (path->string (find-relative-path abs-checkout base)))))
  (unless (checkout-compilation-clean? abs-checkout (append modules watched-dirs))
    (error 'build-compiled-root!
           "refusing to compile: dirty/untracked compilation inputs (freeze or clean them first)"))
  ;; Eager, controlled compilation at the job setup boundary (raco make).
  (define ok?
    (zero? (apply system*/exit-code
                  (find-executable-path "raco")
                  "make"
                  "-j"
                  "1"
                  (for/list ([m (in-list modules)])
                    (build-path abs-checkout m)))))
  (unless ok?
    (error 'build-compiled-root! "raco make failed for ~a" modules))
  (define pairs (filter values (collect-checkout-compilation-pairs abs-checkout)))
  (unless (pair? pairs)
    (error 'build-compiled-root! "no compiled modules produced"))
  ;; Copy payload into staging at identical rel paths. Sources are copied
  ;; too: the manifest producer digests source bytes from the staging
  ;; tree, and the published payload stays self-describing.
  (make-directory* staging-dir)
  (for ([p (in-list pairs)])
    (match-define (list src-rel zo-rel) p)
    (define staging-zo (build-path staging-dir zo-rel))
    (make-directory* (path-only staging-zo))
    (copy-file (build-path abs-checkout zo-rel) staging-zo #t)
    (define staging-src (build-path staging-dir src-rel))
    (make-directory* (path-only staging-src))
    (copy-file (build-path abs-checkout src-rel) staging-src #t))
  (make-compiled-root-manifest #:sources (for/list ([p (in-list pairs)])
                                           (list (list-ref p 0) (list-ref p 1)))
                               #:root-dir staging-dir
                               #:racket-executable (find-executable-path "racket")
                               #:producer-label label
                               #:producer-trusted? trusted?
                               #:lockfile lockfile))

;; publish-compiled-root!
;;   Atomic publication: write manifest into staging, fsync-ish via file
;;   stream close, rename staging over the final path, then revoke write
;;   permission recursively. Readers either see the previous complete
;;   root or the new complete root — never a partial tar-like state.
(define (publish-compiled-root! staging-dir final-dir manifest)
  (call-with-output-file (build-path staging-dir "manifest.rktd")
                         #:exists 'replace
                         (lambda (o) (write manifest o)))
  (when (directory-exists? final-dir)
    (error 'publish-compiled-root! "final root already published: ~a" final-dir))
  (rename-file-or-directory staging-dir final-dir)
  ;; Immutability: revoke all write bits recursively (POSIX chmod; Racket
  ;; core has no in-process permission-mutation API — repo precedent:
  ;; scripts/pre-commit.rkt uses chmod via system).
  (case (system-type 'os)
    [(unix macosx)
     (unless (zero? (system*/exit-code (find-executable-path "chmod") "-R" "a-w" final-dir))
       (error 'publish-compiled-root! "failed to revoke write permissions: ~a" final-dir))]
    [else (error 'publish-compiled-root! "read-only roots unsupported on ~a" (system-type 'os))])
  final-dir)

(define (load-compiled-root-manifest final-dir)
  (define m-path (build-path final-dir "manifest.rktd"))
  (unless (file-exists? m-path)
    (error 'load-compiled-root-manifest "manifest missing in ~a (partial publication?)" final-dir))
  (call-with-input-file m-path read))

;; ------------------------------------------------------ consumer helpers

;; resolve-compiled-root!
;;   Pure validation first; then bind a per-consumer job-local mapping
;;   directory (symlinks into the immutable root) for the CURRENT
;;   checkout location. Source digests must match before any mtime is
;;   forced. Returns a compiled-root record; on ANY failure returns a
;;   record whose reason is the failure string (fail closed, no roots
;;   activated, caller falls back eagerly).

;; source->zo-name
;;   The member name the default compiled-load handler consults for a
;;   source file named SRC-NAME: path-add-extension(".zo") semantics,
;;   name.rkt -> name_rkt.zo (probe-verified lookup rule on 8.10; a
;;   link published under the un-munged source name is NOT consulted).
;;   W3 modules all end in .rkt; extensionless names fall back to
;;   name.zo.
(define (source->zo-name src-name)
  (define s
    (if (path? src-name)
        (path->string src-name)
        src-name))
  (string->path (if (regexp-match? #rx"[.][^.]+$" s)
                    (string-append (regexp-replace #rx"[.]([^.]*)$" s "_\\1") ".zo")
                    (string-append s ".zo"))))

(define (resolve-compiled-root! #:final-dir final-dir
                                #:checkout checkout
                                #:trusted-producer-labels labels
                                #:map-dir map-dir
                                #:expect-lockfile-digest [lock-digest #f])
  (with-handlers ([exn:fail? (lambda (e) (compiled-root #f #f #f checkout (exn-message e)))])
    (define manifest (load-compiled-root-manifest final-dir))
    (define validated
      (validate-compiled-root-manifest manifest
                                       #:root-dir final-dir
                                       #:racket-executable (find-executable-path "racket")
                                       #:trusted-producer-labels labels
                                       #:expect-lockfile-digest lock-digest))
    (define abs-checkout (simplify-path (path->complete-path checkout)))
    ;; Per-consumer mapping: for each entry, verify the consumer source
    ;; bytes, then map <abs consumer src>/compiled/<name>.zo into the
    ;; immutable payload zo.
    (make-directory* map-dir)
    (for ([e (in-list (hash-ref validated 'sources))])
      (define consumer-src (build-path abs-checkout (hash-ref e 'path)))
      (unless (file-exists? consumer-src)
        (error 'resolve-compiled-root! "consumer source missing: ~a" (hash-ref e 'path)))
      (define consumer-b (file->bytes consumer-src))
      (unless (equal? (sha256-bytes consumer-b) (hash-ref e 'digest))
        (error
         'resolve-compiled-root!
         "consumer source digest mismatch for ~a — current source changed since the producer build"
         (hash-ref e 'path)))
      ;; Digest-verified byte-identical source: satisfy the runtime
      ;; freshness gate by aging the SOURCE below the root zo mtime
      ;; (mtime(zo) >= mtime(src) semantics; content identity is proven
      ;; by digest, never by mtime).
      (define zo-mtime (hash-ref e 'zo-mtime-ms))
      (file-or-directory-modify-seconds consumer-src (- zo-mtime 10000))
      ;; Map the consumer's absolute source path into the payload. The
      ;; probe-verified lookup for source /D.../name.rkt under root R is
      ;; R/D.../compiled/name_rkt.zo: full absolute source directory
      ;; spliced under R, "compiled" mode subdir, MUNGED member name.
      ;; The payload member's basename IS the munged name (raco make
      ;; produced it for this source); the coherence gate below binds
      ;; that assumption instead of trusting it.
      (define src-name (file-name-from-path consumer-src))
      (define zo-name (source->zo-name src-name))
      (define payload-zo (build-path final-dir (hash-ref e 'zo)))
      (unless (equal? (file-name-from-path payload-zo) zo-name)
        (error 'resolve-compiled-root!
               "manifest member ~s is not the compiled form of source ~s"
               (hash-ref e 'zo)
               (hash-ref e 'path)))
      (define lookup-dir (build-path map-dir (substring (path->string consumer-src) 1) "compiled"))
      (make-directory* lookup-dir)
      (make-file-or-directory-link (path->string payload-zo) (build-path lookup-dir zo-name)))
    (compiled-root validated final-dir map-dir checkout #t)))

(define (compiled-root-lookup-directory root)
  (compiled-root-map-dir root))

;; compiled-root-activate-parameters
;;   Parameter values that route THIS racket's module lookups through
;;   the mapping directory. In-process activation returns three values.
(define (compiled-root-activate-parameters root)
  (unless (eq? (compiled-root-reason root) #t)
    (error 'compiled-root-activate-parameters "root not resolved"))
  (values (list (compiled-root-map-dir root)) 'modify-seconds))

;; install-compiled-root-toctou-guard!
;;   current-load wrapper: any module loaded from the published root is
;;   re-digested at load time. Mutation after validation (TOCTOU canary)
;;   fails closed here instead of executing tampered bytecode.
(define (install-compiled-root-toctou-guard! root)
  (define root-path
    (path->directory-path (simplify-path (path->complete-path (compiled-root-root-dir root)))))
  (define entries (hash-ref (compiled-root-manifest root) 'sources))
  (define by-zo
    (for/hash ([e (in-list entries)])
      (values (string->path (hash-ref e 'zo)) (hash-ref e 'zo-digest))))
  (define orig (current-load))
  (current-load
   (lambda (path modname)
     (when (and path (file-exists? path))
       ;; follow-links? = #t is load-bearing: root loads arrive via the
       ;; map-dir symlink; resolving it yields the published payload
       ;; member so the by-zo digest table matches (TOCTOU coverage and
       ;; root-hit telemetry both depend on this).
       (define p (simplify-path (path->complete-path path) #t #t))
       (define rel (find-relative-path root-path p))
       (define digest (hash-ref by-zo (string->path (path->string rel)) #f))
       (when digest
         (unless (string=? (sha256-bytes (file->bytes p)) digest)
           (error 'compiled-root-toctou "payload mutated after verification: ~a" path))
         (telemetry
          (match-let ([(compiled-root-telemetry h f l b m) (telemetry)])
            (compiled-root-telemetry (add1 h) f (add1 l) (+ b (bytes-length (file->bytes p))) m)))))
     (orig path modname))))

;; compiled-root-launch-arguments
;;   Subprocess inheritance: racket flags that point a CHILD process at
;;   the same mapping directory, before any -t/-l program arguments.
(define (compiled-root-launch-arguments root)
  (list
   "-e"
   (format
    "(begin (current-compiled-file-roots (list (string->path ~s))) (use-compiled-file-check 'modify-seconds))"
    (path->string (compiled-root-map-dir root)))))

;; eager-fallback-compile!
;;   The ONE eager current-source fallback: raco make in the job-local
;;   checkout (W2 containment boundary). Producer-era code is never
;;   executed; the current source is compiled and used.
(define (eager-fallback-compile! checkout modules)
  (define ok?
    (zero? (apply system*/exit-code
                  (find-executable-path "raco")
                  "make"
                  "-j"
                  "1"
                  (for/list ([m (in-list modules)])
                    (build-path checkout m)))))
  (unless ok?
    (error 'eager-fallback-compile! "eager fallback raco make failed"))
  (telemetry (match-let ([(compiled-root-telemetry h f l b m) (telemetry)])
               (compiled-root-telemetry h (add1 f) l b "eager-fallback(raco-make)")))
  #t)

;; with-compiled-root
;;   Consumer entry point: resolve the root; if resolved, activate
;;   parameters + TOCTOU guard and run thunk; if not, run the eager
;;   fallback and run thunk WITHOUT any root (current source semantics).
(define (with-compiled-root root checkout modules thunk)
  (if (eq? (compiled-root-reason root) #t)
      (parameterize ([current-compiled-file-roots (list (compiled-root-map-dir root))]
                     [use-compiled-file-check 'modify-seconds])
        (install-compiled-root-toctou-guard! root)
        (telemetry (match-let ([(compiled-root-telemetry h f l b _) (telemetry)])
                     (compiled-root-telemetry h f l b "external-compiled-root(map-dir symlinks)")))
        (thunk))
      (begin
        (eager-fallback-compile! checkout modules)
        (parameterize ([current-compiled-file-roots '()])
          (thunk)))))
