#lang racket/base

;; q/ci/prepared-environment/compiled-root.rkt — v1.00.30 W3
;;
;; Trusted compiled-root prototype: relocation-safe producer and
;; consumer/resolver for an IMMUTABLE, read-only compiled root outside
;; the checkout (Racket 8.10 `current-compiled-file-roots` semantics).
;;
;; Proven semantics (probe-verified on Racket 8.10 in this wave):
;;   - A zo under an explicit current-compiled-file-roots entry is
;;     consulted for a source S UNCONDITIONALLY: the 'modify-seconds
;;     freshness gate governs the source-ADJACENT default compiled
;;     directory, not external roots (probe-verified on 8.10). Identity
;;     is proven by digests, never by mtime. W1a-repair (coordinator
;;     trial c01 window-2, run 41271542715): root-on activation pins
;;     use-compiled-file-check to 'exists — mtime comparison is
;;     structurally wrong for restored roots (fresh checkout mtimes are
;;     always newer than producer-era zo mtimes), and freshness is
;;     established by the same-commit source-sha gate plus per-load
;;     content digests instead.
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
;;     member's digest at load time (loads arrive as CONSUMER SOURCE
;;     paths — zo discovery is internal to the module system — so the
;;     wrapper maps a source path back to its published payload member
;;     via the manifest and digests the bytes behind the mapping
;;     symlink); mutation after validation fails closed instead of
;;     executing tampered bytecode. The subprocess bootstrap derives
;;     the same digest table from the published manifest and verifies
;;     a launch-time baked manifest digest, for identical protection
;;     outside the process (O(1)-sized transfer, see
;;     compiled-root-launch-arguments).
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
         racket/runtime-path
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
         compiled-root-activation-mode
         compiled-root-load-keys
         consumer-source-bytes-match?
         package-link-points-at-checkout?
         install-compiled-root-toctou-guard!
         compiled-root-launch-arguments
         source->zo-name
         eager-fallback-compile!
         telemetry-snapshot
         reset-telemetry!
         with-compiled-root
         reason->string
         ;; v1.00.32 W2 repair (trial c01 window-2, run 41271542715):
         ;; mapped-child entry, child-tree hit counting + aggregation.
         compiled-root-script-entry-argument
         compiled-root-runner-entry-argument
         compiled-root-mapped-entry-argument
         compiled-root-mapped-child-arguments
         compiled-root-child-environment
         aggregate-compiled-root-child-telemetry)

(module+ test-helpers
  ;; W4: the activation-contract regression suite
  ;; (tests/test-compiled-root-workflow.rkt) imports exactly this
  ;; submodule so it pins the guarded-activation API surface without
  ;; reaching into module internals.
  (provide (struct-out compiled-root)
           (struct-out compiled-root-telemetry)
           compiled-root-activation-mode
           compiled-root-load-keys
           consumer-source-bytes-match?
           package-link-points-at-checkout?
           with-compiled-root
           eager-fallback-compile!
           telemetry-snapshot
           reset-telemetry!))

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

;; compiled-root-activation-mode
;;   W4 guarded-activation precedence, mirrored from the ci.yml lane
;;   condition so the precedence stays unit-testable:
;;     1. global kill switch ("off") wins over everything;
;;     2. the fast-lane opt-in switch must be exactly "on";
;;     3. the producer step must have succeeded — unknown outcomes
;;        (including missing outputs) fail closed to 'off, never to a
;;        lazy per-test compile;
;;     4. workflow_dispatch replays stay on the legacy eager path;
;;     5. everything else routes to 'auto (authenticated publication +
;;     verification BEFORE resolution; bounded eager fallback on miss).
;;   Global "off" is a hard rollback: no lane expression can override it.
(define (compiled-root-activation-mode #:global-prepared-artifact global
                                       #:lane-switch lane
                                       #:producer-result producer
                                       #:event-name event)
  (cond
    [(equal? global "off") 'off]
    [(not (equal? lane "on")) 'off]
    [(not (equal? producer "success")) 'off]
    [(equal? event "workflow_dispatch") 'off]
    [else 'auto]))

;; ------------------------------------------------------ producer helpers

;; checkout-compilation-clean?
;;   Reject dirty/untracked/generated compilation inputs at the producer
;;   boundary unless they are deliberately frozen by the caller. Runs
;;   `git status --porcelain` restricted to the declared source paths.
;; Each pathspec must be its own argument: a joined single argument is
;; matched literally by git and silently matches nothing (which would
;; accept dirty inputs — the exact bug class this gate exists for).
(define (checkout-compilation-clean? checkout source-rel-paths)
  (define out (open-output-string))
  (define code
    (parameterize ([current-output-port out])
      (apply system*/exit-code
             (find-executable-path "git")
             "-C"
             (path->string (path->complete-path checkout))
             "status"
             "--porcelain"
             "--"
             source-rel-paths)))
  ;; Fail closed: git failure (non-repository, bare host failure) must be
  ;; treated as UNVERIFIABLE, never as clean. A non-repo checkout previously
  ;; slipped through because only stdout emptiness was checked while the
  ;; fatal error went to stderr (W4 rollback-drill finding).
  (and (zero? code) (string=? (string-trim (get-output-string out)) "")))

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
  ;; Snapshot pre-existing compiled/ directories first: after harvesting
  ;; the payload, any compiled/ directory this build created is removed
  ;; again so the producer checkout returns to its clean pre-build state
  ;; (racomake residue must not turn a subsequent build into a
  ;; dirty-input rejection).
  (define pre-existing-compiled-dirs
    (for/list ([d (in-list (find-files directory-exists? abs-checkout))]
               #:when (equal? (file-name-from-path d) (string->path "compiled")))
      (simplify-path d)))
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
  ;; Restore the producer checkout: delete compiled/ dirs this build
  ;; created (pre-existing directories are never touched).
  ;; Each compiled/ directory may host several zos (module plus
  ;; in-checkout dependency closure). Delete every created directory
  ;; exactly once: a repeated delete-directory/files on an already
  ;; removed path raises exn:fail:filesystem and would abort every
  ;; multi-zo build.
  (for ([compiled-dir (in-list (remove-duplicates (for/list ([p (in-list pairs)])
                                                    (define-values (dir _zo-file _zo-dir?)
                                                      (split-path (build-path abs-checkout
                                                                              (list-ref p 1))))
                                                    (simplify-path dir))))])
    (unless (member compiled-dir pre-existing-compiled-dirs)
      (delete-directory/files compiled-dir)))
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
      ;; W4 (W3 review deferral): the resolver no longer MUTATES the
      ;; consumer checkout. Mtime aging (which also wrote seconds into
      ;; zo-mtime-ms fields) is removed: the W3 probe proved external
      ;; roots are consulted unconditionally per lookup, so freshness
      ;; is not load-bearing, and identity is proven by digests. As the
      ;; coherence belt, the consumer's on-disk byte count is compared
      ;; against the manifest entry at resolve time.
      (unless (consumer-source-bytes-match? consumer-src e)
        (error
         'resolve-compiled-root!
         "consumer source byte count mismatch for ~a — on-disk checkout diverges from the manifest"
         (hash-ref e 'path)))
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
      ;; W4 rollback-drill finding: map dirs can be re-entered (shard
      ;; retries, shared map dirs). Remapping must be idempotent: same
      ;; target => no-op; stale/different target => replace; regular
      ;; file at the link path => overwrite too (fail-closed integrity
      ;; is re-proven per run anyway).
      (define link-path (build-path lookup-dir zo-name))
      (define link-target (path->string payload-zo))
      (cond
        [(and (file-exists? link-path)
              (with-handlers ([exn:fail:filesystem? (lambda (_) #f)])
                (equal? (bytes->string/utf-8 (file->bytes link-path)) link-target)))
         (void)]
        [else
         (when (file-exists? link-path)
           (delete-file link-path))
         (make-file-or-directory-link link-target link-path)]))
    (compiled-root validated
                   (simplify-path (path->complete-path final-dir))
                   (simplify-path (path->complete-path map-dir))
                   (simplify-path (path->complete-path checkout))
                   #t)))

(define (compiled-root-lookup-directory root)
  (compiled-root-map-dir root))

;; compiled-root-activate-parameters
;;   Parameter values that route THIS racket's module lookups through
;;   the mapping directory. In-process activation returns three values.
;;   W1a-repair (coordinator trial c01 window-2, run 41271542715): the
;;   check mode is 'exists, never 'modify-seconds. Under
;;   'modify-seconds a compiled file older than its source is ignored,
;;   and restored roots can never satisfy that comparison BY DESIGN
;;   (fresh checkout mtimes are always newer than producer-era zo
;;   mtimes) — mtime comparison is structurally wrong for restored
;;   roots. Under root-on, freshness is established by the same-commit
;;   source-sha gate plus per-load content digests: any mutated zo
;;   fails closed via the sha256 check (install-compiled-root-toctou-guard!).
(define (compiled-root-activate-parameters root)
  (unless (eq? (compiled-root-reason root) #t)
    (error 'compiled-root-activate-parameters "root not resolved"))
  (values (list (compiled-root-map-dir root)) 'exists))

;; compiled-root-load-keys
;;   The load-path spellings the TOCTOU guard recognizes for a source.
;;   current-load may deliver the UNRESOLVED path (symlinked checkouts,
;;   macOS /tmp is itself a symlink) while the manifest digest table is
;;   keyed on the link-resolved form, so the guard must consult BOTH
;;   spellings. The result is always exactly two entries: the raw
;;   string and its link-blind simplified form (use-filesystem? #f so
;;   the mapping works for paths outside any real filesystem, too).
(define (compiled-root-load-keys source-path)
  (define raw
    (if (path? source-path)
        (path->string source-path)
        (~a source-path)))
  (define simplified (path->string (simplify-path raw #f)))
  (list raw simplified))

;; consumer-source-bytes-match?
;;   On-disk-vs-published coherence belt (W3 review deferral fix):
;;   sources[].bytes is compared against the CONSUMER's on-disk byte
;;   count at resolve time. Identity itself is proven by digests; the
;;   byte count is the cheap coherence check that catches truncated or
;;   partially-updated checkouts before any compiled code is consulted.
;;   Entries without a bytes field stay digest-governed.
(define (consumer-source-bytes-match? source-path manifest-entry)
  (define expected (hash-ref manifest-entry 'bytes #f))
  (or (not expected) (and (file-exists? source-path) (equal? (file-size source-path) expected))))

;; package-link-points-at-checkout?
;;   Link/collection identity gate for a restored addon store: the q
;;   package must be LINKED to THIS checkout before the cached store is
;;   consulted for changed q sources. Parses `raco pkg show`-style
;;   output for `source:` lines; a missing package entry or a link to a
;;   different checkout is a mismatch — never a silent pass. A trailing
;;   slash on the checkout path normalizes away.
(define (package-link-points-at-checkout? pkg-show-text checkout-path)
  (define checkout
    (regexp-replace #rx"/*$"
                    (if (path? checkout-path)
                        (path->string checkout-path)
                        (~a checkout-path))
                    ""))
  (and (positive? (string-length checkout))
       (for/or ([line (in-lines (open-input-string (~a pkg-show-text)))])
         (define m (regexp-match #px"^\\s*source:\\s*(.+)$" line))
         (and m (equal? (regexp-replace #rx"/*$" (string-trim (cadr m)) "") checkout)))))

;; install-compiled-root-toctou-guard!
;;   current-load wrapper that re-verifies a root member's digest at
;;   load time. Probe-verified on Racket 8.10: the module system hands
;;   current-load the CONSUMER SOURCE path — zo discovery through
;;   current-compiled-file-roots happens inside the default handler and
;;   is invisible to the wrapper (and, empirically, external roots are
;;   consulted without the source-freshness gate). The guard therefore
;;   maps an incoming source path back to its published payload member
;;   via the manifest and re-digests the actual bytes behind the
;;   consumer's mapping symlink before the module can execute.
;;   Mutation after validation (TOCTOU canary) fails closed here.
;;   Loads that DO arrive as published-root paths (direct zo requires)
;;   are covered by the secondary by-zo table.
(define (install-compiled-root-toctou-guard! root)
  (define root-path
    (path->directory-path (simplify-path (path->complete-path (compiled-root-root-dir root)))))
  (define checkout-dir
    (path->directory-path (simplify-path (path->complete-path (compiled-root-checkout root)))))
  (define entries (hash-ref (compiled-root-manifest root) 'sources))
  (define by-zo
    (for/hash ([e (in-list entries)])
      (values (string->path (hash-ref e 'zo)) (hash-ref e 'zo-digest))))
  (define final-dir (compiled-root-root-dir root))
  ;; consumer source path string -> (list zo-path-string zo-digest)
  ;; W4: key EVERY load-path spelling the guard may receive (raw +
  ;; link-blind simplified; see compiled-root-load-keys), not just the
  ;; symlink-resolved form.
  (define by-src
    (for*/hash ([e (in-list entries)]
                [k (in-list (compiled-root-load-keys
                             (path->string (simplify-path
                                            (path->complete-path (build-path checkout-dir
                                                                             (hash-ref e 'path)))
                                            #t))))])
      (values k
              (list (path->string (build-path final-dir (hash-ref e 'zo))) (hash-ref e 'zo-digest)))))
  (define orig (current-load))
  (current-load
   (lambda (path modname)
     (when (and path (file-exists? path))
       ;; follow-links? = #t is load-bearing: every mapped member arrives
       ;; through the map-dir symlink; resolving it yields the published
       ;; payload bytes the digest table is keyed on.
       (define p (simplify-path (path->complete-path path) #t))
       (define hit
         (or (for*/first ([k (in-list (compiled-root-load-keys path))]
                          [v (in-value (hash-ref by-src k #f))]
                          #:when v)
               v)
             (let ([rel (find-relative-path root-path p)])
               (define digest (hash-ref by-zo (string->path (path->string rel)) #f))
               (and digest (list (path->string (build-path final-dir (path->string rel))) digest)))))
       (when hit
         (match-define (list zo-path digest) hit)
         (define bs (file->bytes zo-path))
         (unless (string=? (sha256-bytes bs) digest)
           (error 'compiled-root-toctou "payload mutated after verification: ~a" path))
         (telemetry (match-let ([(compiled-root-telemetry h f l b m) (telemetry)])
                      (compiled-root-telemetry (add1 h) f (add1 l) (+ b (bytes-length bs)) m)))))
     (orig path modname))))

;; compiled-root-launch-arguments
;;   Subprocess inheritance: racket flags that point a CHILD process at
;;   the same mapping directory, before any -t/-l program arguments.
;;   The bootstrap also installs the same load-time digest guard as the
;;   in-process TOCTOU canary (child mutation-after-verification
;;   coverage), using the project-owned pure-Racket sha256 from the
;;   manifest module resolved via define-runtime-path.
;;   W1a-repair (coordinator trial c02, run 41248049747): the per-source
;;   load-key table used to be CONCATENATED INTO this single -e
;;   argument — one (cons <abs-src> (list <abs-zo> <digest>)) entry per
;;   manifest source, ~230 bytes each. With today's whole-checkout root
;;   (~5200+ members) that argv element reached ~1.2 MB and blew the
;;   Linux per-string exec limit (MAX_ARG_STRLEN = 32 pages = 128 KiB):
;;   every root-on shard died with "exec failed (Argument list too
;;   long; errno=7)" before running a single test. The transfer is
;;   O(1) now: the child re-reads the manifest.rktd ALREADY PUBLISHED
;;   inside the immutable root (the authoritative keys file — written
;;   into the staging tree before the atomic rename, read-only
;;   afterwards) and derives the identical load-key table at startup.
;;   The only baked-in payload is the manifest's own sha256 (64 hex
;;   chars): if the manifest mutates after resolve-time verification,
;;   the child fails closed AT STARTUP — the same
;;   mutation-after-verification canary the embedded per-source
;;   digests used to provide, now over the table's single source of
;;   truth. Marker checks, per-load fail-closed guard errors, loud
;;   eager fallback selection at the resolve boundary, and relocation
;;   independence (per-consumer map dir) are unchanged.
;;   The relative string form is used because define-runtime-path
;;   evaluates its template in the transformer phase where build-path
;;   is not bound on Racket 8.10; the result is resolved at load time
;;   relative to this module's source directory.
(define-runtime-path compiled-root-manifest-module-path "compiled-root-manifest.rkt")

(define (compiled-root-launch-arguments root)
  (define root-dir (path->string (simplify-path (path->complete-path (compiled-root-root-dir root)))))
  (define manifest-path (path->string (build-path root-dir "manifest.rktd")))
  (list "-e"
        (format
         (string-append
          "(begin"
          " (define sha256-file (dynamic-require (string->path ~s) 'sha256-file))"
          " (require racket/file)"
          " (define manifest-path (string->path ~s))"
          " (unless (string=? (sha256-file manifest-path) ~s)"
          "  (error 'compiled-root-toctou \"root manifest mutated after verification: ~~a\""
          "                            manifest-path))"
          " (current-compiled-file-roots (list (string->path ~s)))"
          ;; W1a-repair (trial c01 window-2, run 41271542715): 'exists,
          ;; never 'modify-seconds — mtime comparison is structurally
          ;; wrong for restored roots (fresh checkout mtimes are always
          ;; newer than producer-era zo mtimes, so every root member
          ;; would be judged stale). Freshness comes from the same-commit
          ;; source-sha gate; any mutated zo fails closed via the
          ;; per-load sha256 check below.
          " (use-compiled-file-check 'exists)"
          " (define checkout-dir (string->path ~s))"
          " (define root-dir (string->path ~s))"
          " (define tbl"
          "  (for/list ([e (in-list (hash-ref (file->value manifest-path) 'sources))])"
          "   (list"
          "    (path->string"
          "     (simplify-path"
          "      (path->complete-path (build-path checkout-dir (string->path (hash-ref e 'path))))"
          "      #t))"
          "    (path->string (build-path root-dir (string->path (hash-ref e 'zo))))"
          "    (hash-ref e 'zo-digest))))"
          ;; v1.00.32 W2 repair: hit counting. Q_COMPILED_ROOT_TELEMETRY_DIR
          ;; (exported by `launch`) turns every bootstrapped process in the
          ;; child tree into a counter: one temporary file per process,
          ;; rewritten on EVERY root-served load, so counts survive killed
          ;; children and parallel processes never race. Telemetry is
          ;; best-effort (fail-open): a counting failure must never break a
          ;; test execution — the per-load digest check above stays
          ;; fail-closed regardless.
          " (define q-tel-dir (getenv \"Q_COMPILED_ROOT_TELEMETRY_DIR\"))"
          " (define q-tel-file"
          "  (with-handlers ([exn:fail? (lambda (_) #f)])"
          "   (and q-tel-dir"
          "        (make-temporary-file \"q-compiled-root-hit~~a.rktd\" #f q-tel-dir))))"
          " (define q-hits 0)"
          " (define q-bytes 0)"
          " (define (q-count! b)"
          "  (set! q-hits (add1 q-hits))"
          "  (set! q-bytes (+ q-bytes b))"
          "  (when q-tel-file"
          "   (with-handlers ([exn:fail? (lambda (_) (void))])"
          "    (call-with-output-file"
          "     q-tel-file"
          "     (lambda (out)"
          "      (write (list 'compiled-root-child-telemetry"
          "                   (cons 'root-hits q-hits)"
          "                   (cons 'loads q-hits)"
          "                   (cons 'bytes-served q-bytes))"
          "             out))"
          "     #:exists 'truncate))))"
          " (define orig-load (current-load))"
          " (current-load"
          "  (lambda (path modname)"
          "   (define hit"
          "    (and path"
          "         (assoc (path->string (simplify-path (path->complete-path path) #t))"
          "                tbl)))"
          "   (when hit"
          "    (unless (string=? (sha256-file (cadr hit)) (caddr hit))"
          "     (error 'compiled-root-toctou \"payload mutated after verification: ~~a\" path))"
          "    (q-count! (file-size (cadr hit))))"
          "   (orig-load path modname))))")
         (path->string (simplify-path (path->complete-path compiled-root-manifest-module-path)))
         manifest-path
         (sha256-file manifest-path)
         (path->string (compiled-root-map-dir root))
         (path->string (path->directory-path
                        (simplify-path (path->complete-path (compiled-root-checkout root)))))
         root-dir)))

;; compiled-root-script-entry-argument
;;   W2 repair (trial c01 window-2, run 41271542715): a Racket init option
;;   such as the bootstrap's -e SUPPRESSES the implicit positional-file
;;   load, so the mapped child `racket -e <bootstrap> <script> <args…>`
;;   never loaded <script> at all — the root-on shard was a bootstrap-only
;;   process that exited 0 without executing a single test (the vacuous
;;   green behind zero-root-hits). This entry loads the script explicitly:
;;   module body first (root-served, TOCTOU-guarded), then the module's
;;   'main submodule when declared — the observable semantics of the plain
;;   `racket <script> <args…>` command line (probe-verified: body AND
;;   module+ main run, argv visible via current-command-line-arguments).
(define (compiled-root-script-entry-argument script-path)
  (list "-e"
        (format (string-append "(begin (define q-p (string->path ~s)) (dynamic-require q-p #f)"
                               " (define q-sm (list 'submod q-p 'main))"
                               " (when (module-declared? q-sm #t) (dynamic-require q-sm #f)))")
                (path->string script-path))))

;; compiled-root-runner-entry-argument
;;   The q test runner facade (scripts/run-tests.rkt) fires main under its
;;   own run-file direct-invocation guard, which cannot recognize a -e
;;   entry (find-system-path 'run-file stays the interpreter there), so
;;   the generic script entry would load it as a library and exit 0 —
;;   another silent vacuous green. The facade imports `main` from
;;   scripts/run-tests/runner.rkt; this entry calls it with the child argv
;;   after the root-served body load (probe-verified end to end).
(define (compiled-root-runner-entry-argument script-path)
  (list "-e"
        (format
         (string-append "(begin (define q-p (string->path ~s)) (dynamic-require q-p #f)"
                        " (define q-main"
                        " (namespace-variable-value 'main #t (lambda () #f) (module->namespace q-p)))"
                        " (when q-main (q-main (vector->list (current-command-line-arguments)))))")
         (path->string script-path))))

;; compiled-root-mapped-entry-argument
;;   Entry selection by script identity. The shard child the launcher
;;   exists to run is the q runner facade (the ci.yml invocation is pinned
;;   by tests/test-compiled-root-workflow.rkt, so a rename fails the
;;   contract loudly); any other script keeps the generic
;;   racket-<script> semantics. Never guess from file CONTENT.
(define q-runner-facade-suffix #rx"[/\\]scripts[/\\]run-tests[.]rkt$")
(define (compiled-root-mapped-entry-argument script-path)
  (if (regexp-match? q-runner-facade-suffix (path->string script-path))
      (compiled-root-runner-entry-argument script-path)
      (compiled-root-script-entry-argument script-path)))

;; compiled-root-mapped-child-arguments
;;   `launch -- racket <script> <args…>`: enter the script explicitly (see
;;   compiled-root-script-entry-argument); the script path is completed
;;   against the launcher's cwd, which the child inherits. Any other racket
;;   child shape (e.g. `racket -t <module>`) keeps today's verbatim flag
;;   prepend — the bootstrap's -e is processed first either way.
(define (compiled-root-mapped-child-arguments launch-args rest-args)
  (cond
    [(and (pair? rest-args) (string? (car rest-args)) (not (string-prefix? (car rest-args) "-")))
     (append
      launch-args
      (compiled-root-mapped-entry-argument (path->complete-path (string->path (car rest-args))))
      (cdr rest-args))]
    [else (append launch-args rest-args)]))

;; compiled-root-child-environment
;;   The child tree inherits the counting directory (the bootstrap reads
;;   it) and the flags file (the q test runner re-validates it to inject
;;   the same bootstrap into ITS racket children — the H2 layer). A fresh
;;   environment object: the parent process env is never mutated.
(define (compiled-root-child-environment flags-file hits-dir)
  (define src (current-environment-variables))
  (define dst (make-environment-variables))
  (for ([n (in-list (environment-variables-names src))])
    (environment-variables-set! dst n (environment-variables-ref src n)))
  (environment-variables-set! dst
                              #"Q_COMPILED_ROOT_TELEMETRY_DIR"
                              (string->bytes/utf-8 (path->string hits-dir)))
  (environment-variables-set! dst
                              #"Q_COMPILED_ROOT_FLAGS_FILE"
                              (string->bytes/utf-8 (path->string flags-file)))
  dst)

;; aggregate-compiled-root-child-telemetry
;;   Sum the per-process counter files every bootstrapped process in the
;;   child tree rewrote on each root-served load. Per-process temporary
;;   files cannot race; per-hit rewrites survive killed children (timeout
;;   kill). Unreadable or malformed files are skipped LOUDLY — telemetry
;;   never breaks execution, but it is never silently wrong either.
(define (aggregate-compiled-root-child-telemetry hits-dir)
  (for/fold ([hits 0]
             [loads 0]
             [served 0])
            ([f (in-list (directory-list hits-dir))])
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (eprintf "ci-compiled-root: ignoring unreadable child telemetry ~a: ~a\n"
                                (path->string f)
                                (exn-message e))
                       (values hits loads served))])
      (define datum (file->value (build-path hits-dir f)))
      (match datum
        [(list 'compiled-root-child-telemetry
               (cons 'root-hits h)
               (cons 'loads l)
               (cons 'bytes-served b))
         (values (+ hits h) (+ loads l) (+ served b))]
        [_
         (eprintf "ci-compiled-root: ignoring malformed child telemetry ~a\n" (path->string f))
         (values hits loads served)]))))

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
      ;; The [current-load (current-load)] frame scopes the guard's
      ;; imperative install to this dynamic extent (W3 review deferral,
      ;; fixed in W4): the TOCTOU wrapper is active for the thunk and
      ;; unwound on return — exceptions included.
      ;; W1a-repair (coordinator trial c01 window-2, run 41271542715):
      ;; 'exists, never 'modify-seconds — under root-on, freshness is
      ;; established by the same-commit source-sha gate plus per-load
      ;; content digests (any mutated zo fails closed via the sha256
      ;; check), while mtime comparison is structurally wrong for
      ;; restored roots: a fresh checkout's source mtimes are always
      ;; newer than producer-era root zo mtimes, so 'modify-seconds
      ;; judges every root member stale (zero-root-hits).
      (parameterize ([current-compiled-file-roots (list (compiled-root-map-dir root))]
                     [use-compiled-file-check 'exists]
                     [current-load (current-load)])
        (install-compiled-root-toctou-guard! root)
        (telemetry (match-let ([(compiled-root-telemetry h f l b _) (telemetry)])
                     (compiled-root-telemetry h f l b "external-compiled-root(map-dir symlinks)")))
        (thunk))
      (begin
        (eager-fallback-compile! checkout modules)
        (parameterize ([current-compiled-file-roots '()])
          (thunk)))))

;; --------------------------------------------------------- CI CLI (W4)
;;
;; module+ main — the CI-facing entry point for the guarded activation
;; pilot (issue #9690). Two subcommands:
;;
;;   resolve --root DIR --checkout DIR --trusted-label L
;;           --flags-file FILE [--map-dir DIR]
;;     Validate the published root against the CURRENT checkout
;;     (source digests + byte counts, q link/collection identity,
;;     trusted producer namespace) and write the per-shard flags file
;;     on success. Any validation failure exits NON-ZERO, loudly, and
;;     writes no flags file; the caller's continue-on-error gate then
;;     selects exactly ONE eager fallback compile.
;;
;;   launch --flags-file FILE [--telemetry-file FILE] -- CMD [ARGS...]
;;     Run CMD with the verified compiled-root mapping when (and only
;;     when) the flags file exists and is well-formed and
;;     Q_CI_COMPILED_ROOT is not "off"; otherwise run CMD unchanged
;;     (current-source semantics). Illegal Q_CI_COMPILED_ROOT values
;;     fail closed. Telemetry (mode, child exit code, wall clock, root
;;     hit counters) goes to --telemetry-file when given.
(module+ main
  (require racket/cmdline
           racket/file
           racket/list
           racket/match
           racket/string)

  (define (split-dashdash lst)
    (cond
      [(null? lst) (values '() '())]
      [(equal? (car lst) "--") (values '() (cdr lst))]
      [else
       (define-values (head tail) (split-dashdash (cdr lst)))
       (values (cons (car lst) head) tail)]))

  (define raw-argv (vector->list (current-command-line-arguments)))
  (define-values (flag-argv child-args) (split-dashdash raw-argv))
  ;; Same subcommand reorder as scripts/ci/compiled-root.rkt: command-line
  ;; stops flag parsing at the first non-flag token, so move the leading
  ;; subcommand to the end.
  (define ordered-argv
    (if (and (pair? flag-argv) (member (car flag-argv) '("resolve" "launch")))
        (append (cdr flag-argv) (list (car flag-argv)))
        flag-argv))

  (define root-dir (make-parameter #f))
  (define checkout-dir (make-parameter #f))
  (define trusted-labels (make-parameter '()))
  (define flags-file (make-parameter #f))
  (define map-dir-opt (make-parameter #f))
  (define telemetry-file (make-parameter #f))

  (define rest-args
    (command-line #:program "ci-compiled-root"
                  #:argv ordered-argv
                  #:once-each ["--root" d "published immutable compiled-root directory" (root-dir d)]
                  ["--checkout" d "consumer checkout directory" (checkout-dir d)]
                  ["--trusted-label"
                   l
                   "accepted producer label (repeatable)"
                   (trusted-labels (append (trusted-labels) (list l)))]
                  ["--flags-file"
                   f
                   "per-shard activation flags file (written by resolve, read by launch)"
                   (flags-file f)]
                  ["--map-dir" d "per-consumer read-only mapping directory" (map-dir-opt d)]
                  ["--telemetry-file" f "write launch telemetry to this file" (telemetry-file f)]
                  #:args rest
                  rest))

  (define cmd
    (cond
      [(and (pair? flag-argv) (member (car flag-argv) '("resolve" "launch"))) (car flag-argv)]
      [(and (pair? rest-args) (member (car rest-args) '("resolve" "launch"))) (car rest-args)]
      [else #f]))
  (unless cmd
    (error 'ci-compiled-root
           "usage: ci-compiled-root.rkt resolve|launch [options] [-- child args]; got: ~a"
           raw-argv))

  (define (require-opt! p name)
    (unless (p)
      (error 'ci-compiled-root "missing required option --~a" name))
    (p))

  (define (write-launch-telemetry! mode exit-code wall-ms served)
    (when (telemetry-file)
      (define t (telemetry-snapshot))
      (call-with-output-file
       (telemetry-file)
       (lambda (out)
         (write (list 'compiled-root-launch-telemetry
                      (cons 'mode mode)
                      (cons 'exit-code exit-code)
                      (cons 'wall-ms wall-ms)
                      (cons 'root-hits (+ (compiled-root-telemetry-root-hits t) (list-ref served 0)))
                      (cons 'fallbacks (compiled-root-telemetry-fallbacks t))
                      (cons 'loads (+ (compiled-root-telemetry-loads t) (list-ref served 1)))
                      (cons 'bytes-served
                            (+ (compiled-root-telemetry-bytes-served t) (list-ref served 2)))
                      ;; W2 repair: the mechanism IS the launch mode.
                      ;; The old structurally-constant "none" hid a
                      ;; mapped child whose counters could not report
                      ;; back and lied on every pass-through record.
                      (cons 'mechanism mode))
                out))
       #:exists 'truncate)))

  ;; The launch flags are racket VM flags (-e bootstrap). They are only
  ;; meaningful for a racket child process; anything else fails closed.
  (define (racket-child? argv0)
    (regexp-match? #rx"(^|/)racket$" argv0))

  ;; subprocess does not search PATH for path arguments: a bare "racket"
  ;; must be resolved via find-executable-path or it fails to exec.
  (define (resolve-child-program argv0)
    (or (and (not (string-contains? argv0 "/")) (find-executable-path argv0)) (string->path argv0)))

  (match cmd
    ["resolve"
     (define rd (require-opt! root-dir "root"))
     (define co (require-opt! checkout-dir "checkout"))
     (define ff (require-opt! flags-file "flags-file"))
     ;; The resolver API and the flags payload work in path values;
     ;; normalize CLI strings to complete paths once, here.
     (define rd-path (path->complete-path (string->path rd)))
     (define co-path (path->complete-path (string->path co)))
     (define ff-path (path->complete-path (string->path ff)))
     (when (null? (trusted-labels))
       (error 'ci-compiled-root "resolve requires at least one --trusted-label"))
     (define md
       ;; map-dir is stored in the root struct and consumed by
       ;; compiled-root-launch-arguments as a path value.
       (path->directory-path (path->complete-path (or (map-dir-opt)
                                                      (build-path (path-only ff-path)
                                                                  "compiled-root-map")))))
     (define root
       (resolve-compiled-root! #:final-dir rd-path
                               #:checkout co-path
                               #:trusted-producer-labels (trusted-labels)
                               #:map-dir md))
     (unless (eq? (compiled-root-reason root) #t)
       (eprintf "ci-compiled-root: resolution failed (~a)\n"
                (reason->string (compiled-root-reason root)))
       (exit 1))
     (define launch-args (compiled-root-launch-arguments root))
     (define ff-complete ff-path)
     (make-directory* (path-only ff-complete))
     (call-with-output-file ff-complete
                            (lambda (out)
                              (write (list 'compiled-root-flags
                                           (path->string (compiled-root-root-dir root))
                                           (path->string (compiled-root-map-dir root))
                                           (path->string (compiled-root-checkout root))
                                           launch-args)
                                     out))
                            #:exists 'truncate)
     (printf "ci-compiled-root: verified root resolved; flags written to ~a\n"
             (path->string ff-complete))]
    ["launch"
     (define ff (require-opt! flags-file "flags-file"))
     (when (null? child-args)
       (error 'ci-compiled-root "launch requires a child command after --"))
     ;; Global override precedence mirrors the run CLI: "off" wins over
     ;; everything and never touches the root mapping; "auto" (the ci.yml
     ;; lane value), "verify" and "resolve" use the resolved flags;
     ;; anything else fails closed.
     (define override (getenv "Q_CI_COMPILED_ROOT"))
     (when override
       (case (string->symbol override)
         [(off) (void)]
         [(auto verify resolve) (void)]
         [else
          (error 'ci-compiled-root
                 "illegal Q_CI_COMPILED_ROOT value ~s (off|auto|verify|resolve)"
                 override)]))
     (define flags
       (and override
            (not (equal? (string->symbol override) 'off))
            (file-exists? ff)
            (with-handlers ([exn:fail? (lambda (_) #f)])
              (file->value (path->complete-path ff)))))
     (define valid-flags?
       (and (list? flags)
            (= 5 (length flags))
            (eq? (list-ref flags 0) 'compiled-root-flags)
            (string? (list-ref flags 1))
            (string? (list-ref flags 2))
            (string? (list-ref flags 3))
            (and (list? (list-ref flags 4)) (andmap string? (list-ref flags 4)))))
     (define argv0 (first child-args))
     (define t0 (current-inexact-milliseconds))
     (define-values (exit-code mode served)
       (cond
         [(and valid-flags? (racket-child? argv0))
          ;; W2 repair (trial c01 window-2, run 41271542715): the mapped
          ;; child must actually ENTER the script — a Racket init option
          ;; (-e bootstrap) suppresses the implicit positional-file load,
          ;; so the pre-repair shard child was a bootstrap-only process
          ;; that exited 0 without running a single test — and every
          ;; bootstrapped process in the child tree counts its root-served
          ;; loads into a per-launch telemetry directory that this launcher
          ;; aggregates into the record below. Counts written by killed
          ;; children survive (per-hit rewrites), and telemetry failures
          ;; are loud but never fatal.
          (define hits-dir
            (with-handlers ([exn:fail? (lambda (_) #f)])
              (make-temporary-file "compiled-root-hits~a"
                                   'directory
                                   (path-only (path->complete-path ff)))))
          (unless hits-dir
            (eprintf "ci-compiled-root: cannot create the child telemetry directory;"
                     " root-hit counters stay 0\n"))
          (define child-env
            (and hits-dir (compiled-root-child-environment (path->complete-path ff) hits-dir)))
          (define code
            (parameterize ([current-environment-variables (or child-env
                                                              (current-environment-variables))])
              (apply system*/exit-code
                     (resolve-child-program argv0)
                     (compiled-root-mapped-child-arguments (list-ref flags 4) (rest child-args)))))
          (define served
            (if hits-dir
                (call-with-values (lambda () (aggregate-compiled-root-child-telemetry hits-dir)) list)
                (list 0 0 0)))
          (values code "verified-root-hit" served)]
         [else
          ;; Pass-through: no flags file (lane off or failed resolution —
          ;; the one eager gate already compiled), the global off
          ;; override won, or the child is not racket. Run the command
          ;; exactly as given, never silently mapping anything.
          (when valid-flags?
            (eprintf "ci-compiled-root: child is not racket (~s); running unmapped\n" argv0))
          (define code (apply system*/exit-code (resolve-child-program argv0) (rest child-args)))
          (values code
                  (cond
                    [(and override (equal? (string->symbol override) 'off)) "global-off"]
                    [(not valid-flags?) "no-flags-current-source"]
                    [else "unmapped-child"])
                  (list 0 0 0))]))
     (define wall-ms (inexact->exact (floor (- (current-inexact-milliseconds) t0))))
     (write-launch-telemetry! mode exit-code wall-ms served)
     (exit exit-code)]))
