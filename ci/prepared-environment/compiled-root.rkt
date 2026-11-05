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
;;     directory, not external roots. W1a-repair (coordinator trial c01
;;     window-2, run 41271542715): root-on activation pins
;;     use-compiled-file-check to 'exists — mtime comparison is
;;     structurally wrong for restored roots (fresh checkout mtimes are
;;     always newer than producer-era zo mtimes), and freshness is
;;     established by the same-commit source-sha gate plus per-load
;;     content digests instead.
;;     W1a install-map repair (PLAN-W1-INSTALL-MAP.md) re-probing,
;;     deterministic marker probes on 8.10 (pinned in
;;     tests/test-compiled-root.rkt):
;;     * CORRECTED invariant — the 'exists pin is GLOBAL: it governs
;;       EVERY compiled-file candidate, including the source-adjacent
;;       default 'same lookup. With the pin active a STALE
;;       source-adjacent zo is served without any freshness comparison
;;       (probe: a 10-seconds-older adjacent zo, distinct bytes, served
;;       under 'exists). Every pin site below therefore states the
;;       global effect; identity rests entirely on the resolve-time
;;       source-sha gate and the per-load content digests, never on
;;       mtime semantics.
;;     * CORRECTED invariant — 'modify-seconds gating of ROOT-DERIVED
;;       candidates is NOT reproducible in isolation on 8.10: an
;;       external-root candidate is served even with a strictly newer
;;       source, with or without its .dep sidecar (probe matrix). The
;;       historical zero-root-hits attribution to mtime comparison
;;       (trial c01 window-2) is superseded by the W2 analysis of the
;;       same trial (H0 bootstrap-only child / H1 no hit channel / H2
;;       non-inheriting grandchildren — see
;;       tests/test-compiled-root-runner-inheritance.rkt). The 'exists
;;       pin stays as defense in depth: no lookup freshness can ever
;;       gate a verified root member, whichever of the two readings of
;;       8.10 internals holds on a given shape.
;;     * Map-first: when several root entries carry a member, the FIRST
;;       current-compiled-file-roots entry wins (probe: root+same,
;;       per-zo root served, adjacent never consulted).
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
;;     symlink). Codex contract review qualification — the guarantee is
;;     NOT a blanket race immunity for ordinary payload members: the
;;     per-load canary is a hash-then-load (two-open) shape — it digests
;;     one open of the zo, then the module system's default handler
;;     re-opens the same path for the actual load — so a same-user
;;     process can in principle swap bytes between the two opens. The
;;     canary deterministically rejects any mutation that lands before
;;     its digest read (fail closed at the digest check), but executed
;;     bytes are not PROVABLY the digested bytes for ordinary payloads:
;;     that two-open window is a preexisting limitation, documented here
;;     and in the canonical report, never guarded away. The deterministic
;;     executed-equals-authenticated property exists ONLY for the child
;;     transfer module (authenticate-compiled-module reads the zo exactly
;;     once and evals the compiled form FROM THOSE BYTES —
;;     transfer-auth.rkt). A second preexisting mapping limitation: a
;;     load arriving under a path spelling outside the manifest's key
;;     set (e.g. an unresolved symlink spelling on symlinked checkouts)
;;     misses both key tables and passes through WITHOUT a digest check
;;     (the guard silently no-ops; the W3 deferred observation). The
;;     subprocess bootstrap derives the same digest table from the
;;     published manifest and verifies a launch-time baked manifest
;;     digest, for identical protection outside the process
;;     (O(1)-sized transfer, see compiled-root-launch-arguments); its
;;     per-load checks inherit the same two-open qualification.
;;   - Any validation failure resolves to a single EAGER fallback
;;     (raco make in the job-local checkout at the setup boundary,
;;     W2 containment semantics) or a hard failure — never producer-era
;;     code and never uncontrolled lazy compilation. W1a install-map
;;     repair: the fallback lane is PLAIN LEGACY Racket — default
;;     parameter values (current-compiled-file-roots '(same),
;;     use-compiled-file-check 'modify-seconds) — so the eager raco
;;     make output is usable; the pre-repair '() parameterize was
;;     no-compiled semantics and discarded the eager output.
;;   - Publication is atomic (staging directory + rename) and the
;;     published root is made read-only; consumers never write to it.
;;
;; W1a install map (PLAN-W1-INSTALL-MAP.md):
;;   - Launch roots become (list checkout-map install-map) — absolute
;;     paths, map-first order (first existing zo wins; the checkout
;;     payload shadows install for manifest-covered sources). No 'same,
;;     no relative entries, no defaults consulted. Any future append
;;     must use the RHS-before-assignment form
;;     (append (list ...) (current-compiled-file-roots)).
;;   - The install map is built at resolve time (job-local,
;;     deterministic, immutable, chmod a-w like the payload): every
;;     reachable installation zo is inventoried and mapped as a PER-ZO
;;     symlink under <install-map>/<abs-source-dir minus the leading
;;     slash>/compiled/<munged>.zo. Enumeration base: the primary
;;     library collection paths plus every existing collection LINK
;;     FILE (the primary paths alone miss every package collection —
;;     probed on 8.10); a zo is reachable when its un-munged sibling
;;     source exists. Whole-compiled-DIR symlinks were probe-validated
;;     to serve but are REJECTED: a directory link would expose
;;     non-inventoried members added to the installation after the
;;     inventory, while per-zo links can only ever serve inventoried
;;     bytes (fail-closed containment by construction).
;;   - Integrity: the map records a canonical inventory digest; per-load
;;     sha256 digest checks extend to install-served loads (same
;;     table/equal?-hash/first-match/canonicalization mechanics as the
;;     checkout table, libcrypto FFI fast path where available —
;;     measured ~1-2 ms per member, ~235 ms for a full 833-member
;;     inventory digest, NOT prohibitive); activation re-verifies every
;;     keyed mapped member and a missing member fails closed to the
;;     legacy fallback lane.
;;   - Residual risk: the installation is already the interpreter trust
;;     domain (its zos execute at boot); the digest checks make
;;     post-inventory mutation of install-served members fail closed,
;;     with the same hash-then-load two-open qualification as checkout
;;     members (see the TOCTOU canary paragraph above).

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
         "compiled-root-manifest.rkt"
         ;; W1 review round 6: required for its side effect on the
         ;; producer's raco make closure — the child bootstrap loads this
         ;; module (authenticate-compiled-module, the atomic telemetry
         ;; writer), so every published root must ship its compiled form.
         "transfer-auth.rkt")

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
         ;; W1a install-map repair: additive per-root counters (the
         ;; five-field compiled-root-telemetry envelope struct is
         ;; untouched) + the install map surface.
         telemetry-map-counters
         (struct-out compiled-root-map-counters)
         compiled-root-map-counters-filename?
         install-map-members-verified?
         aggregate-compiled-root-map-counters
         compiled-root-install-map-dir
         ;; W1a: exposed for hermetic fixture drivers in the test suite
         build-install-map!
         install-collection-roots
         with-compiled-root
         reason->string
         ;; v1.00.32 W2 repair (trial c01 window-2, run 41271542715):
         ;; mapped-child entry, child-tree hit counting + aggregation.
         compiled-root-script-entry-argument
         compiled-root-runner-entry-argument
         compiled-root-mapped-entry-argument
         compiled-root-mapped-child-arguments
         compiled-root-child-environment
         aggregate-compiled-root-child-telemetry
         ;; W1 performance repair round: pre-compiled load-key transfer
         ;; module (producer) + child telemetry filename contract.
         write-compiled-root-load-keys!
         compiled-root-child-telemetry-filename?
         compiled-root-telemetry-tmp-prefix
         ;; W1 repair round 7 (reviewer P2): the filename contract's two
         ;; halves, shared with the reservation (reserve-child-telemetry-
         ;; target) and pinned writer/reader-aligned in tests.
         compiled-root-child-telemetry-prefix
         compiled-root-child-telemetry-suffix)

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

;; W1a install-map repair: additive per-root counters. The five-field
;; envelope struct above and its semantics are UNTOUCHED (the
;; ci-recovery-trial-evidence@1 launcher mapping keeps reading exactly
;; its five fields; the collector maps datum pairs 1:1 and the assessor
;; reads named fields, so additive launch-record keys are verified
;; tolerated — pinned by tests). These counters travel separately:
;;   - in-process: the telemetry-map-counters parameter below;
;;   - children: a SECOND contract-disjoint counter file per process
;;     (q-compiled-root-map*.rktd — never a child-telemetry filename),
;;     written with the same atomic temp+rename discipline the
;;     transfer-auth writer uses, aggregated by
;;     aggregate-compiled-root-map-counters into the launch record's
;;     additive keys (checkout-map-hits/install-map-hits/unmapped-loads).
(struct compiled-root-map-counters
        (checkout-map ; loads served through the checkout map table
         install-map ; loads served through the install map table
         unmapped) ; loads of real files that hit neither table
  #:transparent)

(define compiled-root-map-counters-prefix "q-compiled-root-map")

;; compiled-root-map-counters-filename?
;;   The per-root counter filename contract: prefix + non-empty unique
;;   middle + the same .rktd suffix as the child telemetry contract —
;;   deliberately DISJOINT from that contract (a map-counter file is
;;   never a root-hits file and vice versa), so neither aggregator can
;;   mistake the other writer's files for its own telemetry.
(define (compiled-root-map-counters-filename? name)
  (define s
    (if (path? name)
        (path->string name)
        (~a name)))
  (and (string-prefix? s compiled-root-map-counters-prefix)
       (string-suffix? s compiled-root-child-telemetry-suffix)
       (> (string-length s)
          (+ (string-length compiled-root-map-counters-prefix)
             (string-length compiled-root-child-telemetry-suffix)))))

(define telemetry-map-counters (make-parameter (compiled-root-map-counters 0 0 0)))

;; ---------------------------------------------------------------- state

(define telemetry (make-parameter (compiled-root-telemetry 0 0 0 0 "none")))

(define (telemetry-snapshot)
  (telemetry))

(define (reset-telemetry! [t (compiled-root-telemetry 0 0 0 0 "none")])
  (telemetry t)
  ;; W1a: the additive per-root counters reset with the envelope struct
  (telemetry-map-counters (compiled-root-map-counters 0 0 0)))

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
  ;; W1 performance repair round: ship the load-key table as a COMPILED
  ;; module inside the root (see write-compiled-root-load-keys!) so
  ;; children require the zo instead of re-parsing manifest.rktd per
  ;; spawned process. Written and compiled in staging BEFORE the atomic
  ;; rename; the manifest binds its digests below.
  (define load-keys (write-compiled-root-load-keys! staging-dir pairs))
  (make-compiled-root-manifest #:sources (for/list ([p (in-list pairs)])
                                           (list (list-ref p 0) (list-ref p 1)))
                               #:root-dir staging-dir
                               #:racket-executable (find-executable-path "racket")
                               #:producer-label label
                               #:producer-trusted? trusted?
                               #:lockfile lockfile
                               #:load-keys load-keys))

;; write-compiled-root-load-keys!
;;   W1 performance repair round: the O(1)-argv transfer (PR #9878) left
;;   every spawned child re-parsing the full manifest.rktd to rebuild its
;;   load-key table — measured ~1.5 s of extra wall clock per child at
;;   trial scale (5,200 entries), which alone pushed the spawn-heavy test
;;   files over the runner's 2-min per-file cap (shard wall 3h13m vs
;;   ~25 min). This producer step ships the SAME table as a compiled
;;   module inside the root: children require the zo (an O(1) marshal
;;   load) instead of parsing the rktd — the parse cost moves to the
;;   producer, once. The module carries plain rel-path triples
;;   (src-rel zo-rel zo-digest): relocation stays a consumer-side
;;   completion, absolute paths are NEVER baked into the immutable root.
;;   The compiled form is produced here (raco make in staging, before the
;;   atomic rename) so a child never runs the table through the reader.
;;   Returns the manifest load-keys field: the digests bind the module
;;   into the manifest, whose own baked startup canary covers it.
(define (write-compiled-root-load-keys! staging-dir sources)
  (define triples
    (for/list ([s (in-list sources)])
      (list (list-ref s 0)
            (list-ref s 1)
            (sha256-file (build-path staging-dir (string->path (list-ref s 1)))))))
  (define lk-src (build-path staging-dir compiled-root-load-keys-source-name))
  (call-with-output-file lk-src
                         #:exists 'replace
                         (lambda (o)
                           (write `(module load-keys racket/base
                                     (provide compiled-root-load-key-table)
                                     (define compiled-root-load-key-table ',triples))
                                  o)))
  (unless (zero? (system*/exit-code (find-executable-path "raco") "make" "-j" "1" lk-src))
    (error 'write-compiled-root-load-keys!
           "raco make failed for the compiled load-key transfer module"))
  (define lk-zo (build-path staging-dir (string->path compiled-root-load-keys-zo-rel)))
  (unless (file-exists? lk-zo)
    (error 'write-compiled-root-load-keys! "compiled transfer module missing: ~a" lk-zo))
  (hasheq 'path
          compiled-root-load-keys-source-name
          'digest
          (sha256-file lk-src)
          'zo
          compiled-root-load-keys-zo-rel
          'zo-digest
          (sha256-file lk-zo)))

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

;; --------------------------------------------------- install map builder

;; install-collection-roots
;;   The enumeration base for the install map. The primary library
;;   collection paths (each element is a collection root) plus every
;;   root declared by an existing collection LINK FILE
;;   (current-library-collection-links): entries of the form
;;   (collection-or-root element ...) relative to the link file's
;;   directory, where element is a path element (bytes/symbol), and
;;   'root makes every directory ELEMENT of the target a collection
;;   root. Probed on 8.10: the primary paths alone miss every package
;;   collection (typed/racket lives under a pkgs root declared in the
;;   link file), so the link files are load-bearing for the
;;   "every reachable zo is inventoried" goal. Missing entries are
;;   skipped, never fatal: an uninstall-only partial environment must
;;   still yield a valid (possibly smaller) inventory.
(define (install-collection-roots)
  (define roots '())
  (for ([p (in-list (current-library-collection-paths))])
    (set! roots (cons p roots)))
  (for ([f (in-list (current-library-collection-links))]
        #:when (and (path? f) (file-exists? f)))
    (define entries
      (with-handlers ([exn:fail? (lambda (_) '())])
        (file->value f)))
    (when (and (list? entries) (andmap (lambda (e) (and (pair? e) (list? (cdr e)))) entries))
      (define base (path->directory-path (simplify-path (path->complete-path f))))
      (for ([e (in-list entries)])
        (define dir
          (with-handlers ([exn:fail? (lambda (_) #f)])
            (simplify-path (path->complete-path (apply build-path
                                                       base
                                                       (for/list ([c (in-list (cdr e))])
                                                         (if (bytes? c)
                                                             (bytes->path-element c)
                                                             c)))))))
        (when (and dir (directory-exists? dir))
          (if (eq? (car e) 'root)
              (for ([d (in-list (with-handlers ([exn:fail:filesystem? (lambda (_) '())])
                                  (directory-list dir)))]
                    #:when (directory-exists? (build-path dir d)))
                (set! roots (cons (simplify-path (path->complete-path (build-path dir d))) roots)))
              (set! roots (cons dir roots)))))))
  (remove-duplicates (reverse roots)))

;; install-reachable-zos
;;   Every reachable zo under ROOT: a compiled/ member whose un-munged
;;   sibling source exists (the source lives in the PARENT of the
;;   compiled/ directory). Returns (list source-abs-string zo-abs-string)
;;   pairs in sorted, deterministic order. A zo that is itself a symlink
;;   is skipped (its bytes are someone else's inventory); a disappearing
;;   path is skipped, never fatal.
(define (install-reachable-zos root)
  (define abs-root (simplify-path (path->complete-path root)))
  (define pairs '())
  (define (zo->src zo)
    (define name (path->string (file-name-from-path zo)))
    (define src-name
      (cond
        [(regexp-match? #rx"_rkt[.]zo$" name) (regexp-replace #rx"_rkt[.]zo$" name ".rkt")]
        [(regexp-match? #rx"[.]zo$" name) (regexp-replace #rx"[.]zo$" name "")]
        [else #f]))
    (and src-name
         (let ([p (build-path (path->directory-path (path-only zo)) 'up src-name)])
           (and (file-exists? p) p))))
  (define (walk! d)
    (define zos
      ;; fold-files raises PLAIN exn:fail for "path disappeared" (a root
      ;; or intermediate removed mid-walk), not an exn:fail:filesystem
      ;; subtype — probe-verified, so the catch must be exn:fail?.
      (with-handlers ([exn:fail? (lambda (_) '())])
        (find-files (lambda (p) (regexp-match? #rx"[.]zo$" (path->string p))) d)))
    (for ([p (in-list zos)]
          #:unless (link-exists? p)
          #:when (file-exists? p)
          #:when (zo->src p))
      (define src (zo->src p))
      (set! pairs
            (cons (list (path->string (simplify-path (path->complete-path src)))
                        (path->string (simplify-path (path->complete-path p))))
                  pairs))))
  (walk! abs-root)
  (sort (remove-duplicates pairs) string<? #:key car))

;; build-install-map!
;;   Resolve-time install-map materialization (job-local, deterministic,
;;   immutable): rebuild MAP-DIR from scratch, link every reachable zo
;;   under the probe-verified reroot layout, write the compiled install
;;   transfer module (same authenticate-then-eval discipline as the
;;   checkout table), revoke write permission, and return the inventory
;;   hash (map-dir, canonical inventory digest, member count, transfer
;;   field). Any failure raises — the resolver converts it to a
;;   fail-closed resolve.
(define (build-install-map! #:map-dir map-dir #:collection-roots roots)
  ;; fresh materialization: a previous job's links must never leak
  ;; non-inventoried members into this inventory
  (when (directory-exists? map-dir)
    (system*/exit-code (find-executable-path "chmod") "-R" "u+w" map-dir)
    (delete-directory/files map-dir))
  (make-directory* map-dir)
  (define members
    (for/list ([root (in-list roots)])
      (install-reachable-zos root)))
  (define pairs (sort (remove-duplicates (apply append members)) string<? #:key car))
  ;; inventory FIRST (digests before links): a zo that disappears between
  ;; inventory and linking makes the resolve fail closed at the link step
  (define inventoried
    (for/list ([p (in-list pairs)])
      (define zo (string->path (list-ref p 1)))
      (unless (file-exists? zo)
        (error 'build-install-map! "install zo disappeared during inventory: ~a" zo))
      (list (list-ref p 0) (list-ref p 1) (sha256-file zo) (file-size zo))))
  ;; per-zo symlinks under the PROBE-VERIFIED layout: for source
  ;; /A/B/mod.rkt the lookup is R/A/B/compiled/mod_rkt.zo (the SOURCE
  ;; DIRECTORY is spliced under R — path->directory-path on the FILE
  ;; path would keep the member name as a directory, the exact
  ;; wrong-layout shape that never serves)
  (define mapped
    (for/list ([m (in-list inventoried)])
      (define src (string->path (list-ref m 0)))
      (define src-dir (or (path-only src) (string->path "/")))
      (define lookup-dir (build-path map-dir (substring (path->string src-dir) 1) "compiled"))
      (make-directory* lookup-dir)
      (define link-path (build-path lookup-dir (source->zo-name (file-name-from-path src))))
      (when (or (file-exists? link-path) (link-exists? link-path))
        (delete-file link-path))
      (make-file-or-directory-link (list-ref m 1) link-path)
      ;; link verification: every keyed mapped member must resolve NOW
      (unless (file-exists? link-path)
        (error 'build-install-map! "mapped member missing after linking: ~a" link-path))
      (list (list-ref m 0) (path->string link-path) (list-ref m 2))))
  ;; canonical inventory digest: sorted "src\0zo-digest\n" frames
  (define inventory-digest
    (sha256-bytes (apply bytes-append
                         (for/list ([m (in-list inventoried)])
                           (bytes-append (string->bytes/utf-8 (list-ref m 0))
                                         #"\0"
                                         (string->bytes/utf-8 (list-ref m 2))
                                         #"\n")))))
  ;; the compiled install transfer module: children authenticate-then-eval
  ;; it and build their per-load table from it (O(1) marshal load, no
  ;; per-child re-derivation)
  (define lk-src (build-path map-dir "install-load-keys.rkt"))
  (call-with-output-file lk-src
                         #:exists 'truncate
                         (lambda (o)
                           (write `(module install-load-keys racket/base
                                     (provide compiled-root-install-key-table)
                                     (define compiled-root-install-key-table ',mapped))
                                  o)))
  (unless (zero? (system*/exit-code (find-executable-path "raco") "make" "-j" "1" lk-src))
    (error 'build-install-map! "raco make failed for the install transfer module"))
  (define lk-zo (build-path map-dir "compiled" "install-load-keys_rkt.zo"))
  (unless (file-exists? lk-zo)
    (error 'build-install-map! "compiled install transfer module missing: ~a" lk-zo))
  ;; immutability: revoke all write bits recursively (POSIX chmod)
  (unless (zero? (system*/exit-code (find-executable-path "chmod") "-R" "a-w" map-dir))
    (error 'build-install-map! "failed to revoke write permissions: ~a" map-dir))
  (hasheq 'map-dir
          (path->string (simplify-path (path->complete-path map-dir)))
          'digest
          inventory-digest
          'members
          (length inventoried)
          'mapped
          mapped
          'load-keys
          (hasheq 'path
                  "install-load-keys.rkt"
                  'digest
                  (sha256-file lk-src)
                  'zo
                  "compiled/install-load-keys_rkt.zo"
                  'zo-digest
                  (sha256-file lk-zo))))

;; compiled-root-install-map-dir
;;   The install map directory of a resolved root, or #f when the root
;;   carries no install map (resolve without #:install-map-dir).
(define (compiled-root-install-map-dir root)
  (define inv (hash-ref (compiled-root-manifest root) 'install-map #f))
  (and inv (hash-ref inv 'map-dir #f) (string->path (hash-ref inv 'map-dir))))

;; install-map-members-verified?
;;   Symlink verification at activation: every keyed mapped member must
;;   still resolve to real bytes, and the transfer module files must
;;   still match the inventory's recorded digests. A member missing at
;;   activation fails closed (the caller selects the eager fallback),
;;   never silently. Digest/bytes data only — nothing is executed here.
(define (install-map-members-verified? root)
  (define inv (hash-ref (compiled-root-manifest root) 'install-map #f))
  (or (not inv)
      (let ([map-dir (string->path (hash-ref inv 'map-dir))]
            [lk (hash-ref inv 'load-keys)])
        (and (file-exists? (build-path map-dir (string->path (hash-ref lk 'path))))
             (file-exists? (build-path map-dir (string->path (hash-ref lk 'zo))))
             (string=? (sha256-file (build-path map-dir (string->path (hash-ref lk 'path))))
                       (hash-ref lk 'digest))
             (string=? (sha256-file (build-path map-dir (string->path (hash-ref lk 'zo))))
                       (hash-ref lk 'zo-digest))
             (for/and ([e (in-list (hash-ref inv 'mapped '()))])
               (file-exists? (string->path (list-ref e 1))))))))

;; resolve-compiled-root!
;;   Pure validation first; then bind a per-consumer job-local mapping
;;   directory (symlinks into the immutable root) for the CURRENT
;;   checkout location. Source digests must match before any mtime is
;;   forced. Returns a compiled-root record; on ANY failure returns a
;;   record whose reason is the failure string (fail closed, no roots
;;   activated, caller falls back eagerly).
;;   W1a install-map repair: when #:install-map-dir is given, the
;;   installation's reachable zos are inventoried, digest-checked and
;;   mapped under the reroot layout; the validated manifest carries the
;;   consumer-local 'install-map inventory (map-dir, canonical digest,
;;   member count, transfer field). #:collection-roots overrides the
;;   enumeration base (hermetic tests); production defaults to the real
;;   collection search universe.

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
                                #:expect-lockfile-digest [lock-digest #f]
                                ;; W1a install-map repair: opt-in install
                                ;; map; absent (the default) keeps today's
                                ;; single-map behavior bit-for-bit.
                                #:install-map-dir [install-map-dir #f]
                                #:collection-roots [install-roots #f])
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
      ;; is re-proven per run anyway). Reviewer-blocker follow-up: the
      ;; same-target probe used to read the link target's FILE CONTENT
      ;; and UTF-8-decode it — payload members are binary zo, so every
      ;; re-entry over an existing link raised inside the probe and the
      ;; whole resolve failed. A retried shard could then never recover
      ;; on a shared map dir (its own stale-flags invalidation aside).
      ;; resolve-path compares identities, not data: a symlink resolves
      ;; to its target, a regular file resolves to itself — same-target
      ;; links are a no-op, everything else is replaced.
      (define link-path (build-path lookup-dir zo-name))
      (define link-target (path->string payload-zo))
      (define link-target-complete (simplify-path (path->complete-path payload-zo)))
      (cond
        [(and (file-exists? link-path)
              (with-handlers ([exn:fail? (lambda (_) #f)])
                (equal? (resolve-path link-path) link-target-complete)))
         (void)]
        [else
         ;; Reviewer blocker: file-exists? FOLLOWS links, so a stale
         ;; DANGLING link (its target vanished since a previous resolve —
         ;; rebuilt/republished payload, GCed temp tree) is invisible to
         ;; the probe, the delete was skipped, and the recreate raised
         ;; exn:fail:filesystem:exists on the existing link path; resolve
         ;; then failed on EVERY re-entry with a shared map dir and could
         ;; never recover. link-exists? probes the link itself, not its
         ;; target, and must gate the replacement delete as well.
         (when (or (file-exists? link-path) (link-exists? link-path))
           (delete-file link-path))
         (make-file-or-directory-link link-target link-path)]))
    ;; W1a install-map repair: build the install map AFTER the checkout
    ;; mapping verified — any failure here fails the whole resolve closed
    ;; (never a partially verified two-map activation).
    (define validated+
      (if install-map-dir
          (hash-set validated
                    'install-map
                    (build-install-map! #:map-dir install-map-dir
                                        #:collection-roots
                                        (or install-roots (install-collection-roots))))
          validated))
    (compiled-root validated+
                   (simplify-path (path->complete-path final-dir))
                   (simplify-path (path->complete-path map-dir))
                   (simplify-path (path->complete-path checkout))
                   #t)))

(define (compiled-root-lookup-directory root)
  (compiled-root-map-dir root))

;; compiled-root-activate-parameters
;;   Parameter values that route THIS racket's module lookups through
;;   the mapping directories. In-process activation returns two values.
;;   W1a-repair (coordinator trial c01 window-2, run 41271542715): the
;;   check mode is 'exists, never 'modify-seconds. Under
;;   'modify-seconds a compiled file older than its source is ignored,
;;   and restored roots can never satisfy that comparison BY DESIGN
;;   (fresh checkout mtimes are always newer than producer-era zo
;;   mtimes) — mtime comparison is structurally wrong for restored
;;   roots. Under root-on, freshness is established by the same-commit
;;   source-sha gate plus per-load content digests: any mutated zo
;;   fails closed via the sha256 check (install-compiled-root-toctou-guard!).
;;   W1a install-map repair — GLOBAL PIN EFFECT (probe-verified): pinning
;;   use-compiled-file-check to 'exists affects EVERY compiled-file
;;   candidate lookup in this process, INCLUDING source-adjacent default
;;   'same lookups: a stale source-adjacent zo is served without any
;;   freshness comparison while the pin is active. That is accepted at
;;   activation because the roots list excludes 'same entirely (no
;;   source-adjacent candidate is ever consulted under root-on) and
;;   identity is digest-proven; any future change that reintroduces
;;   'same alongside the pin must re-justify this note.
(define (compiled-root-activate-parameters root)
  (unless (eq? (compiled-root-reason root) #t)
    (error 'compiled-root-activate-parameters "root not resolved"))
  (define install-dir (compiled-root-install-map-dir root))
  (values (if install-dir
              (list (compiled-root-map-dir root) install-dir)
              (list (compiled-root-map-dir root)))
          'exists))

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
  ;; W1a install-map repair: the install table, same mechanics (equal?
  ;; keys over raw + link-blind simplified spellings; first-match).
  ;; Values keep the same (list zo-path digest) shape, where the zo path
  ;; is the MAPPED member inside the install map — digesting it follows
  ;; the symlink to the exact bytes the module system will serve.
  (define install-inv (hash-ref (compiled-root-manifest root) 'install-map #f))
  (define by-install-src
    (if install-inv
        (for*/hash ([e (in-list (hash-ref install-inv 'mapped))]
                    [k (in-list (compiled-root-load-keys (list-ref e 0)))])
          (values k (list (list-ref e 1) (list-ref e 2))))
        #f))
  ;; Probe-verified on 8.10 (W1a install-map repair): under root-on the
  ;; module system hands current-load the MAPPED ZO PATH for
  ;; install-served loads (unlike checkout-map members, which arrive as
  ;; consumer source paths), so the guard must also key the mapped
  ;; member paths — the digest checks the exact bytes behind the
  ;; serving symlink either way.
  (define by-install-zo
    (if install-inv
        (for*/hash ([e (in-list (hash-ref install-inv 'mapped))]
                    [k (in-list (compiled-root-load-keys (list-ref e 1)))])
          (values k (list (list-ref e 1) (list-ref e 2))))
        #f))
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
       ;; W1a install-map repair: the install table lookup (source-path
       ;; keys first, then the probe-verified mapped-zo-path spellings
       ;; the module system actually delivers under root-on), tried only
       ;; when the checkout tables miss.
       (define ihit
         (and (not hit)
              (or (and by-install-src
                       (for*/first ([k (in-list (compiled-root-load-keys path))]
                                    [v (in-value (hash-ref by-install-src k #f))]
                                    #:when v)
                         v))
                  (and by-install-zo
                       (for*/first ([k (in-list (compiled-root-load-keys path))]
                                    [v (in-value (hash-ref by-install-zo k #f))]
                                    #:when v)
                         v)))))
       (cond
         [hit
          (match-define (list zo-path digest) hit)
          (define bs (file->bytes zo-path))
          (unless (string=? (sha256-bytes bs) digest)
            (error 'compiled-root-toctou "payload mutated after verification: ~a" path))
          (telemetry (match-let ([(compiled-root-telemetry h f l b m) (telemetry)])
                       (compiled-root-telemetry (add1 h) f (add1 l) (+ b (bytes-length bs)) m)))
          (telemetry-map-counters
           (match-let ([(compiled-root-map-counters c i u) (telemetry-map-counters)])
             (compiled-root-map-counters (add1 c) i u)))]
         ;; W1a: install-served loads digest-check the mapped member's
         ;; actual bytes (the same hash-then-load canary as checkout
         ;; members) and count under the install-map counter.
         [ihit
          (match-define (list zo-path digest) ihit)
          (define bs (file->bytes zo-path))
          (unless (string=? (sha256-bytes bs) digest)
            (error 'compiled-root-toctou "payload mutated after verification: ~a" path))
          (telemetry (match-let ([(compiled-root-telemetry h f l b m) (telemetry)])
                       (compiled-root-telemetry (add1 h) f (add1 l) (+ b (bytes-length bs)) m)))
          (telemetry-map-counters
           (match-let ([(compiled-root-map-counters c i u) (telemetry-map-counters)])
             (compiled-root-map-counters c (add1 i) u)))]
         ;; Unmapped loads: a real file in neither table is served
         ;; (compiled) by the module system WITHOUT a digest check —
         ;; counted here, never silent.
         [else
          (telemetry-map-counters
           (match-let ([(compiled-root-map-counters c i u) (telemetry-map-counters)])
             (compiled-root-map-counters c i (add1 u))))]))
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
;;   afterwards). W1 performance repair round: the table no longer comes
;;   from re-parsing that rktd per spawned child (~1.5 s wall clock per
;;   child at trial scale, the 36/431 per-file 2-min timeouts behind the
;;   3h13m shard wall) — the producer ships it as a COMPILED module
;;   (load-keys.rkt next to manifest.rktd, raco-made in staging before
;;   the rename; see write-compiled-root-load-keys!) and children require
;;   the zo: an O(1) marshal load. Integrity is unchanged, fail-closed:
;;   the baked-in payload is the manifest's own sha256 PLUS the transfer
;;   module's zo sha256 (both 64 hex chars, both sourced from the
;;   manifest, which resolve-time validation digest-checks against the
;;   published bytes). If the manifest OR the transfer module mutates
;;   after resolve-time verification, the child fails closed AT STARTUP
;;   — the same mutation-after-verification canary the embedded
;;   per-source digests used to provide, now over the table's two-file
;;   source of truth. W1 review round 6 (TOCTOU): the child no longer
;;   hashes the zo PATH and then dynamic-requires the same path — that
;;   authenticates one open and executes a second one, and a same-user
;;   process can swap the bytes between the two opens (deterministically
;;   demonstrated by a current-load stand-in in the regression suite).
;;   authenticate-compiled-module (transfer-auth.rkt) reads the zo
;;   exactly once, digests those bytes against the baked canary, and
;;   evals the compiled form read FROM THOSE BYTES: the executed table
;;   IS the authenticated table, deterministically — there is no second
;;   open to race. Per-load zo digest checks are unchanged; the table's
;;   digests now root in an authenticated-and-executed module. Marker
;;   checks, per-load fail-closed guard errors, loud eager fallback
;;   selection at the resolve boundary, and relocation independence
;;   (per-consumer map dir; the transfer module stores REL-path
;;   triples) are unchanged.
;;   The relative string form is used because define-runtime-path
;;   evaluates its template in the transformer phase where build-path
;;   is not bound on Racket 8.10; the result is resolved at load time
;;   relative to this module's source directory.
(define-runtime-path compiled-root-manifest-module-path "compiled-root-manifest.rkt")
(define-runtime-path compiled-root-transfer-auth-module-path "transfer-auth.rkt")

(define (compiled-root-launch-arguments root)
  ;; Old roots (published before the compiled transfer module existed:
  ;; manifest with the load-keys field ABSENT — a present-but-false field
  ;; cannot reach this point, the presence-based shape gate rejects it at
  ;; validation) deliberately VALIDATE under the new reader — the field
  ;; is optional for backward acceptance — but they CANNOT LAUNCH: launch
  ;; arguments cannot be derived without the transfer module, so this
  ;; builder RAISES, naming the explicit rebuild requirement. Fail-closed
  ;; launch behavior by construction: the resolve CLI step invokes this
  ;; builder BEFORE writing the flags file, so an old root yields a
  ;; non-zero resolve and no newly written flags. Callers must gate launch
  ;; on successful resolve because a stale flags file may survive failure.
  ;; The remedy is an explicit rebuild with the current producer
  ;; (scripts/ci/compiled-root.rkt build); historic artifacts are never
  ;; rewritten to match the new field.
  (define root-dir (path->string (simplify-path (path->complete-path (compiled-root-root-dir root)))))
  (define manifest-path (path->string (build-path root-dir "manifest.rktd")))
  (define lk-info (hash-ref (compiled-root-manifest root) 'load-keys #f))
  (unless lk-info
    (error
     'compiled-root-launch-arguments
     "published root carries no compiled load-key transfer module ~
            (rebuild with the current producer)"))
  (define lk-zo-path (path->string (build-path root-dir (string->path (hash-ref lk-info 'zo)))))
  ;; W1a install-map repair: the install map inventory (resolve-time,
  ;; consumer-local). When present, the launch roots become (list
  ;; checkout-map install-map) — absolute paths, MAP-FIRST (probe: the
  ;; first entry wins) — and the child authenticates the install
  ;; transfer module exactly like the checkout one (baked zo digest,
  ;; authenticate-then-eval, table fetched by module name).
  (define install-inv (hash-ref (compiled-root-manifest root) 'install-map #f))
  (define install-dir (and install-inv (hash-ref install-inv 'map-dir)))
  (define install-lk (and install-inv (hash-ref install-inv 'load-keys)))
  (define il-zo-path
    (and install-lk
         (path->string (build-path (string->path install-dir)
                                   (string->path (hash-ref install-lk 'zo))))))
  ;; Segment 1: launch roots + install transfer table. The checkout-map
  ;; entry is FIRST in both shapes; the install entry is appended, never
  ;; prepended (any future append must use the RHS-before-assignment
  ;; form (append (list ...) (current-compiled-file-roots))).
  (define roots-install-segment
    (if install-dir
        (string-append
         (format "(current-compiled-file-roots (list (string->path ~s) (string->path ~s)))"
                 (path->string (compiled-root-map-dir root))
                 install-dir)
         (format " (define il-path (string->path ~s))" il-zo-path)
         (format " (auth! il-path ~s \"install load-key table\")" (hash-ref install-lk 'zo-digest))
         ;; absolute install keys — canonicalized exactly like the
         ;; checkout keys (simplify/complete #t), insert-if-absent keeps
         ;; the first match for any conflicting duplicate
         " (define itbl"
         "  (let ([ht (make-hash)])"
         "   (for ([e (in-list (dynamic-require ''install-load-keys"
         "                                     'compiled-root-install-key-table))])"
         "    (define k"
         "     (path->string"
         "      (simplify-path"
         "       (path->complete-path (string->path (car e)))"
         "       #t)))"
         "    (unless (hash-has-key? ht k)"
         "     (hash-set! ht k (list (cadr e) (caddr e)))))"
         "   ht))"
         ;; probe-verified on 8.10: under root-on the module system hands
         ;; current-load the MAPPED ZO PATH for install-served loads, so
         ;; the child keeps the same by-zo twin table the parent guard has
         " (define itbl-zo"
         "  (let ([ht (make-hash)])"
         "   (for ([e (in-list (dynamic-require ''install-load-keys"
         "                                     'compiled-root-install-key-table))])"
         "    (define k"
         "     (path->string"
         "      (simplify-path"
         "       (path->complete-path (string->path (cadr e)))"
         "       #t)))"
         "    (unless (hash-has-key? ht k)"
         "     (hash-set! ht k (list (cadr e) (caddr e)))))"
         "   ht))")
        (string-append (format "(current-compiled-file-roots (list (string->path ~s)))"
                               (path->string (compiled-root-map-dir root)))
                       " (define itbl #f)"
                       " (define itbl-zo #f)")))
  ;; Segment 2: per-root counters + the SECOND counter file. Contract:
  ;; q-compiled-root-map*.rktd — derived from the reserved hit-counter
  ;; name (its unique middle carries over, so cross-process uniqueness
  ;; is inherited), deliberately DISJOINT from the root-hits contract
  ;; (neither aggregator mistakes the other's files for its own
  ;; telemetry). Same atomic temp+rename discipline as the transfer-auth
  ;; writer: complete datums only, rewritten on EVERY classified load,
  ;; zero snapshot AT STARTUP (a zero-hit child leaves an aggregatable
  ;; file). Telemetry stays best-effort (fail-open); the per-load
  ;; digest checks above stay fail-closed regardless.
  (define map-counters-segment
    (string-append
     " (define q-hits 0)"
     " (define q-bytes 0)"
     " (define q-i-hits 0)"
     " (define q-i-bytes 0)"
     " (define q-unmapped 0)"
     " (define q-write-tel! (and q-tel-file (q-tel-writer q-tel-file)))"
     " (define q-map-file"
     "  (with-handlers ([exn:fail? (lambda (_) #f)])"
     "   (and q-tel-file"
     "        (string->path"
     "         (string-replace (path->string q-tel-file)"
     "                         \"q-compiled-root-hit\""
     "                         \"q-compiled-root-map\")))))"
     " (define q-write-map!"
     "  (and q-map-file"
     "       (let ([tmp (string->path (string-append (path->string q-map-file) \".part\"))])"
     "        (lambda (c i u)"
     "         (with-handlers ([exn:fail? void])"
     "          (call-with-output-file tmp"
     "                                 (lambda (out)"
     "                                   (write (list 'compiled-root-map-telemetry"
     "                                                (cons 'checkout-map-hits c)"
     "                                                (cons 'install-map-hits i)"
     "                                                (cons 'unmapped-loads u))"
     "                                          out))"
     "                                 #:exists 'truncate)"
     "          (rename-file-or-directory tmp q-map-file #t))))))"
     " (define (q-count! b)"
     "  (set! q-hits (add1 q-hits))"
     "  (set! q-bytes (+ q-bytes b))"
     "  (when q-write-tel! (q-write-tel! q-hits q-hits q-bytes))"
     "  (when q-write-map! (q-write-map! q-hits q-i-hits q-unmapped)))"
     " (define (q-count-install! b)"
     "  (set! q-i-hits (add1 q-i-hits))"
     "  (set! q-i-bytes (+ q-i-bytes b))"
     "  (when q-write-map! (q-write-map! q-hits q-i-hits q-unmapped)))"
     " (define (q-count-unmapped!)"
     "  (set! q-unmapped (add1 q-unmapped))"
     "  (when q-write-map! (q-write-map! q-hits q-i-hits q-unmapped)))"
     " (when q-write-tel! (q-write-tel! 0 0 0))"
     " (when q-write-map! (q-write-map! 0 0 0))"))
  (list
   "-e"
   (format
    (string-append
     "(begin"
     " (define sha256-file (dynamic-require (string->path ~s) 'sha256-file))"
     ;; W1 review round 6: authenticate-then-eval transfer loader (no
     ;; hash-path/require-path TOCTOU) + atomic telemetry writer.
     " (define auth! (dynamic-require (string->path ~s) 'authenticate-compiled-module))"
     " (define q-tel-writer (dynamic-require (string->path ~s) 'make-atomic-telemetry-writer))"
     ;; W1 repair round 7 (reviewer P2): the counter name is reserved
     ;; (a unique contract-matching name, NO file created) through the
     ;; same trusted transfer module — never via a bootstrap-side
     ;; make-temporary-file, which published an empty contract file
     ;; before the first complete snapshot.
     " (define q-tel-reserve (dynamic-require (string->path ~s) 'reserve-child-telemetry-target))"
     " (require racket/file)"
     ;; W1a install-map repair: string-replace derives the per-root
     ;; counter file name from the reserved hit-counter name below.
     " (require racket/string)"
     " (define manifest-path (string->path ~s))"
     " (unless (string=? (sha256-file manifest-path) ~s)"
     "  (error 'compiled-root-toctou \"root manifest mutated after verification: ~~a\""
     "                            manifest-path))"
     ;; W1 performance repair round: authenticate the transfer
     ;; module's bytes and execute EXACTLY those bytes (see
     ;; transfer-auth.rkt), then fetch the COMPILED table from the
     ;; declared module — the module system never re-opens lk-path.
     ;; use-compiled-file-check below is 'exists, so the zo inside
     ;; the immutable root is used regardless of mtimes.
     " (define lk-path (string->path ~s))"
     " (auth! lk-path ~s)"
     ;; W1a install-map repair: the launch roots + (when the resolve
     ;; carried an install map) the authenticated install transfer
     ;; table are spliced in here — see roots-install-segment below.
     "~a"
     ;; W1a-repair (trial c01 window-2, run 41271542715): 'exists,
     ;; never 'modify-seconds — mtime comparison is structurally
     ;; wrong for restored roots (fresh checkout mtimes are always
     ;; newer than producer-era zo mtimes, so every root member
     ;; would be judged stale). Freshness comes from the same-commit
     ;; source-sha gate; any mutated zo fails closed via the
     ;; per-load sha256 check below.
     ;; W1a install-map repair — GLOBAL PIN EFFECT (probe-verified):
     ;; this pin governs EVERY compiled-file candidate lookup in the
     ;; child, including source-adjacent default 'same lookups (a stale
     ;; adjacent zo is served without any freshness comparison while
     ;; the pin is active). The roots list excludes 'same entirely, so
     ;; no source-adjacent candidate is consulted under root-on and
     ;; identity stays digest-proven.
     " (use-compiled-file-check 'exists)"
     " (define checkout-dir (string->path ~s))"
     " (define root-dir (string->path ~s))"
     " (define tbl"
     ;; by NAME from the authenticated module — never (dynamic-require
     ;; lk-path ...), which would re-open the path after authentication.
     ;; W1a performance repair: the per-load lookup used to linear-scan
     ;; this table with assoc — a shaped miss measured ~300–320 µs at
     ;; production scale (5,200 entries) vs ~4.6–5.7 µs for a hash on
     ;; the same keys (local probes, not CI measurements). The table is
     ;; now an EQUAL?-based hash (make-hash; never hasheq/hasheqv —
     ;; table keys and arriving canonical paths are separately
     ;; allocated equal? strings), built insert-if-absent in manifest
     ;; order so conflicting duplicate canonical keys keep the assoc
     ;; FIRST match. Keys keep the identical simplify/complete
     ;; canonicalization; values keep the assoc entry shape (canonical
     ;; source, payload zo, digest) so the per-hit digest check and
     ;; telemetry below are untouched; the table is STILL keyed by
     ;; canonical SOURCE paths only — a direct payload-zo load misses it
     ;; exactly as before, so this repair adds no direct-zo protection.
     "  (let ([ht (make-hash)])"
     "   (for ([e (in-list (dynamic-require ''load-keys 'compiled-root-load-key-table))])"
     "    (define k"
     "     (path->string"
     "      (simplify-path"
     "       (path->complete-path (build-path checkout-dir (string->path (car e))))"
     "       #t)))"
     "    (unless (hash-has-key? ht k)"
     "     (hash-set!"
     "      ht"
     "      k"
     "      (list"
     "       k"
     "       (path->string (build-path root-dir (string->path (cadr e))))"
     "       (caddr e)))))"
     "   ht))"
     ;; v1.00.32 W2 repair: hit counting. Q_COMPILED_ROOT_TELEMETRY_DIR
     ;; (exported by `launch`) turns every bootstrapped process in the
     ;; child tree into a counter: one temporary file per process,
     ;; rewritten on EVERY root-served load, so counts survive killed
     ;; children and parallel processes never race. Telemetry is
     ;; best-effort (fail-open): a counting failure must never break a
     ;; test execution — the per-load digest check above stays
     ;; fail-closed regardless. W1 performance repair round: the
     ;; filename contract (prefix/suffix shared with the parent
     ;; aggregator) is passed in as an argument, and the zero
     ;; counters are written AT STARTUP — a zero-hit child leaves an
     ;; aggregatable file, never the empty file the parent used to
     ;; have to reject as malformed. W1 review round 6: the write is
     ;; atomic (unique same-directory temp + rename(2), see
     ;; transfer-auth.rkt) — a kill mid-write or a concurrent
     ;; aggregator read observes complete datums only, never the
     ;; partial/empty file truncate-in-place used to expose. W1
     ;; repair round 7 (reviewer P2): the counter NAME is reserved
     ;; through reserve-child-telemetry-target — a unique
     ;; contract-matching name and NO file — because the old
     ;; (make-temporary-file "q-compiled-root-hit~a.rktd" ...) created
     ;; an EMPTY contract-matching file BEFORE the first complete
     ;; snapshot, and a child killed in that startup window left the
     ;; aggregator a malformed "telemetry" file to reject loudly. The
     ;; reserved name materializes only when the first atomic rename
     ;; publishes the complete startup snapshot; an interrupted
     ;; startup leaves at worst a foreign temporary, never a contract
     ;; file.
     " (define q-tel-dir (getenv \"Q_COMPILED_ROOT_TELEMETRY_DIR\"))"
     " (define q-tel-file"
     "  (with-handlers ([exn:fail? (lambda (_) #f)])"
     "   (and q-tel-dir"
     "        (q-tel-reserve q-tel-dir ~s ~s))))"
     ;; W1a install-map repair: per-root counters + the SECOND counter
     ;; file are spliced in here — see map-counters-segment below.
     "~a"
     " (define orig-load (current-load))"
     " (current-load"
     "  (lambda (path modname)"
     ;; W1a install-map repair: first-match classification — the
     ;; checkout table, then the install table, then an unmapped load.
     ;; Same canonicalization for every lookup (simplify/complete #t).
     "   (define canon"
     "    (and path (path->string (simplify-path (path->complete-path path) #t))))"
     "   (define hit (and canon (hash-ref tbl canon #f)))"
     "   (define i-hit"
     "    (and canon (not hit) itbl (hash-ref itbl canon #f)))"
     "   (define iz-hit"
     "    (and canon (not hit) (not i-hit) itbl-zo (hash-ref itbl-zo canon #f)))"
     "   (cond"
     "    [hit"
     "     (unless (string=? (sha256-file (cadr hit)) (caddr hit))"
     "      (error 'compiled-root-toctou \"payload mutated after verification: ~~a\" path))"
     "     (q-count! (file-size (cadr hit)))]"
     "    [i-hit"
     "     (unless (string=? (sha256-file (car i-hit)) (cadr i-hit))"
     "      (error 'compiled-root-toctou \"payload mutated after verification: ~~a\" path))"
     "     (q-count-install! (file-size (car i-hit)))]"
     "    [iz-hit"
     "     (unless (string=? (sha256-file (car iz-hit)) (cadr iz-hit))"
     "      (error 'compiled-root-toctou \"payload mutated after verification: ~~a\" path))"
     "     (q-count-install! (file-size (car iz-hit)))]"
     "    [else"
     "     (when (and path (file-exists? path))"
     "      (q-count-unmapped!))])"
     "   (orig-load path modname))))")
    (path->string (simplify-path (path->complete-path compiled-root-manifest-module-path)))
    (path->string (simplify-path (path->complete-path compiled-root-transfer-auth-module-path)))
    (path->string (simplify-path (path->complete-path compiled-root-transfer-auth-module-path)))
    (path->string (simplify-path (path->complete-path compiled-root-transfer-auth-module-path)))
    manifest-path
    (sha256-file manifest-path)
    lk-zo-path
    (hash-ref lk-info 'zo-digest)
    roots-install-segment
    (path->string (path->directory-path (simplify-path (path->complete-path (compiled-root-checkout
                                                                             root)))))
    root-dir
    compiled-root-child-telemetry-prefix
    compiled-root-child-telemetry-suffix
    map-counters-segment)))

;; compiled-root-script-entry-argument
;;   W1a round 5 (trial trial-2026-11-01-smoke-3, treatment C, run
;;   41303344486): with ANY -e init option present — the bootstrap's own
;;   included — Racket does not load a positional script as the run-file,
;;   and arguments after the last -e expression are parsed as RACKET
;;   switches: the real ci.yml lane's
;;   `launch -- racket scripts/run-tests.rkt --suite fast …` died with
;;   `racket: bad switch: --suite` before the runner ever started (the
;;   W2 -e expression entry consumed the script into a second -e and
;;   appended the child args as bare switches; the generic variant
;;   silently swallowed them). The entry is therefore the SAME mechanism
;;   as the proven raco dispatch (`racket -N raco -l- raco/main.rkt …`)
;;   in the file-path grammar (-l rejects absolute file paths —
;;   (lib "/abs/p.rkt") is a bad module path, probe-verified on 8.10;
;;   -t is (require (file …))):
;;
;;     racket [-N <script>] -e <bootstrap> -t- <script> <args…>
;;
;;   Probe-verified on Racket 8.10: -e/-t- are processed in order (the
;;   bootstrap evaluates first), -t- loads the script as a module AND as
;;   the run-file (plain `racket <script>` identity: module body, then a
;;   declared 'main submodule), stops switch parsing, and routes the
;;   remaining arguments to the script's current-command-line-arguments;
;;   -N restores find-system-path 'run-file to the script, which the
;;   runner facade's direct-invocation guard requires.
(define (compiled-root-script-entry-argument script-path)
  (define p (path->string script-path))
  (list "-N" p "-t-" p))

;; compiled-root-runner-entry-argument
;;   The q test runner facade (scripts/run-tests.rkt) fires main under its
;;   own run-file direct-invocation guard. Under the unified -N/-t- entry
;;   the facade IS the run-file, so the facade's own guard invokes main
;;   exactly once with the routed child argv — the runner entry is the
;;   script entry now (the W2 -e expression had to call q-main manually
;;   because a -e entry leaves the run-file at the interpreter and can
;;   never route a command line; kept as a distinct export so the
;;   mapped-entry dispatch surface is unchanged).
(define (compiled-root-runner-entry-argument script-path)
  (compiled-root-script-entry-argument script-path))

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
;;   `launch -- racket <script> <args…>`: enter the script as the run-file
;;   (see compiled-root-script-entry-argument: -e <bootstrap> -N <script>
;;   -t- <script>); the script path is completed against the launcher's
;;   cwd, which the child inherits, and the remaining args are appended
;;   AFTER the script, where -t- routes them to the script's command line
;;   (never parsed as racket switches). Any other racket child shape (e.g.
;;   `racket -t <module>`) keeps today's verbatim flag prepend — the
;;   bootstrap's -e is processed first either way.
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

;; W1 performance repair round: the child telemetry filename contract.
;; Writer (the bootstrap's reserve-child-telemetry-target reservation,
;; via transfer-auth.rkt) and reader (the aggregator below) are aligned
;; on ONE format, defined here once: q-compiled-root-hit<unique>.rktd —
;; prefix and suffix ARE the contract; the middle is the per-process
;; unique suffix reserved through the contract-disjoint probe. The CI
;; shard logs previously flooded with "ignoring malformed child telemetry
;; q-compiled-root-hit<...>.rktd" lines (filename format mismatch) because
;; the aggregator parsed every file in the hits directory.
(define compiled-root-child-telemetry-prefix "q-compiled-root-hit")
(define compiled-root-child-telemetry-suffix ".rktd")

(define (compiled-root-child-telemetry-filename? name)
  (define s
    (if (path? name)
        (path->string name)
        (~a name)))
  (and (string-prefix? s compiled-root-child-telemetry-prefix)
       (string-suffix? s compiled-root-child-telemetry-suffix)
       (> (string-length s)
          (+ (string-length compiled-root-child-telemetry-prefix)
             (string-length compiled-root-child-telemetry-suffix)))))

;; aggregate-compiled-root-child-telemetry
;;   Sum the per-process counter files every bootstrapped process in the
;;   child tree rewrote on each root-served load. Per-process temporary
;;   files cannot race; per-hit rewrites survive killed children (timeout
;;   kill). Only files matching the child telemetry filename contract are
;;   telemetry: foreign files in the hits directory are not evidence and
;;   are ignored silently; a contract-named file that fails to parse IS
;;   evidence of a problem and is skipped LOUDLY — telemetry never breaks
;;   execution, but it is never silently wrong either.
(define (aggregate-compiled-root-child-telemetry hits-dir)
  (for/fold ([hits 0]
             [loads 0]
             [served 0])
            ([f (in-list (directory-list hits-dir))]
             #:when (compiled-root-child-telemetry-filename? f))
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

;; aggregate-compiled-root-map-counters
;;   W1a install-map repair: sum the per-root counter files every
;;   bootstrapped child wrote (contract q-compiled-root-map*.rktd,
;;   deliberately disjoint from the root-hits contract). Same loud-vs-
;;   silent split as the hit aggregator: foreign files are ignored
;;   silently; a contract-named file that fails to parse is skipped
;;   LOUDLY. Never fatal: telemetry must not break a test run, but it is
;;   never silently wrong either.
(define (aggregate-compiled-root-map-counters hits-dir)
  (for/fold ([checkout 0]
             [install 0]
             [unmapped 0])
            ([f (in-list (directory-list hits-dir))]
             #:when (compiled-root-map-counters-filename? f))
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (eprintf "ci-compiled-root: ignoring unreadable per-root counter ~a: ~a\n"
                                (path->string f)
                                (exn-message e))
                       (values checkout install unmapped))])
      (define datum (file->value (build-path hits-dir f)))
      (match datum
        [(list 'compiled-root-map-telemetry
               (cons 'checkout-map-hits c)
               (cons 'install-map-hits i)
               (cons 'unmapped-loads u))
         (values (+ checkout c) (+ install i) (+ unmapped u))]
        [_
         (eprintf "ci-compiled-root: ignoring malformed per-root counter ~a\n" (path->string f))
         (values checkout install unmapped)]))))

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
;;   W1a install-map repair: (a) activation re-verifies every keyed
;;   install-map member first — a missing member fails closed to the
;;   eager fallback, never silently; (b) the fallback lane is PLAIN
;;   LEGACY Racket: no parameterization at all, so the thunk runs with
;;   the ambient default parameter values ((current-compiled-file-roots)
;;   = '(same), (use-compiled-file-check) = 'modify-seconds) and the
;;   eager raco make output is usable — the pre-repair
;;   (current-compiled-file-roots '()) parameterize was no-compiled
;;   semantics and made the eager output unusable.
(define (with-compiled-root root checkout modules thunk)
  (if (and (eq? (compiled-root-reason root) #t)
           ;; W1a: symlink verification at activation (digest/bytes only,
           ;; nothing executed). A keyed mapped member missing NOW is the
           ;; fail-closed path to the legacy fallback lane.
           (install-map-members-verified? root))
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
      ;; W1a install-map repair — GLOBAL PIN EFFECT (probe-verified):
      ;; the 'exists pin governs EVERY compiled-file candidate lookup in
      ;; this dynamic extent, including source-adjacent default 'same
      ;; lookups; the roots list below excludes 'same entirely, so no
      ;; source-adjacent candidate is ever consulted under root-on and
      ;; identity stays digest-proven.
      (parameterize ([current-compiled-file-roots
                      (let ([install-dir (compiled-root-install-map-dir root)])
                        (if install-dir
                            (list (compiled-root-map-dir root) install-dir)
                            (list (compiled-root-map-dir root))))]
                     [use-compiled-file-check 'exists]
                     [current-load (current-load)])
        (install-compiled-root-toctou-guard! root)
        (telemetry (match-let ([(compiled-root-telemetry h f l b _) (telemetry)])
                     (compiled-root-telemetry h f l b "external-compiled-root(map-dir symlinks)")))
        (thunk))
      ;; W1a install-map repair: PLAIN LEGACY fallback — no
      ;; parameterization at all (the pre-repair '() parameterize was
      ;; no-compiled semantics and discarded the eager output; no 'exists
      ;; pin either — the ambient default check mode governs, exactly
      ;; like legacy Racket).
      (begin
        (eager-fallback-compile! checkout modules)
        (thunk))))

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

  ;; Parse errors are non-destructive: argv tokens cannot safely identify
  ;; either a command or a flags path until the parser has succeeded.
  ;; Callers must gate launch on a successful resolve, not file existence.

  (define root-dir (make-parameter #f))
  (define checkout-dir (make-parameter #f))
  (define trusted-labels (make-parameter '()))
  (define flags-file (make-parameter #f))
  (define map-dir-opt (make-parameter #f))
  ;; W1a install-map repair: opt-in install map; when given, the launch
  ;; roots become (list checkout-map install-map) — absolute, map-first.
  (define install-map-opt (make-parameter #f))
  ;; W1a install-map repair: hermetic install-map enumeration roots (the
  ;; CLI spelling of the resolver API's #:collection-roots). Absent, the
  ;; resolver enumerates the real collection search universe (production
  ;; behavior); present, exactly these roots are inventoried — the
  ;; controlled fixture-injection mechanism the suite uses to exercise
  ;; install-map members deterministically.
  (define collection-roots-opt (make-parameter '()))
  (define telemetry-file (make-parameter #f))

  (define rest-args
    (command-line
     #:program "ci-compiled-root"
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
     ["--install-map-dir"
      d
      "job-local install map directory (two-map launch roots)"
      (install-map-opt d)]
     ["--collection-root"
      d
      "install-map enumeration root (repeatable; default: the real collection search universe)"
      (collection-roots-opt (append (collection-roots-opt) (list d)))]
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

  (define (write-launch-telemetry! mode exit-code wall-ms served [map-counters (list 0 0 0)])
    (when (telemetry-file)
      (define t (telemetry-snapshot))
      (define mc (telemetry-map-counters))
      (call-with-output-file
       (telemetry-file)
       (lambda (out)
         (write
          (list 'compiled-root-launch-telemetry
                (cons 'mode mode)
                (cons 'exit-code exit-code)
                (cons 'wall-ms wall-ms)
                (cons 'root-hits (+ (compiled-root-telemetry-root-hits t) (list-ref served 0)))
                (cons 'fallbacks (compiled-root-telemetry-fallbacks t))
                (cons 'loads (+ (compiled-root-telemetry-loads t) (list-ref served 1)))
                (cons 'bytes-served (+ (compiled-root-telemetry-bytes-served t) (list-ref served 2)))
                ;; W2 repair: the mechanism IS the launch mode.
                ;; The old structurally-constant "none" hid a
                ;; mapped child whose counters could not report
                ;; back and lied on every pass-through record.
                (cons 'mechanism mode)
                ;; W1a install-map repair: ADDITIVE per-root keys.
                ;; In-process guard counters + child per-root counter
                ;; files. The collector maps datum pairs 1:1 and the
                ;; strict assessor reads named fields only — additive
                ;; keys verified tolerated (ci-recovery-trial-evidence@1
                ;; stays valid; pinned by the trial/evidence tests).
                (cons 'checkout-map-hits
                      (+ (compiled-root-map-counters-checkout-map mc) (list-ref map-counters 0)))
                (cons 'install-map-hits
                      (+ (compiled-root-map-counters-install-map mc) (list-ref map-counters 1)))
                (cons 'unmapped-loads
                      (+ (compiled-root-map-counters-unmapped mc) (list-ref map-counters 2))))
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
     ;; After parsing, attempt to invalidate the parser-bound flags path
     ;; on resolve failure. Parse errors do not enter this handler, and an
     ;; unlink error can leave valid stale flags behind. Such errors are
     ;; reported, not swallowed; callers must gate launch on resolve success.
     ;; The invalidation deletes through the RAW option string:
     ;; file-exists? and delete-file accept path strings, which keeps
     ;; the closure usable even when the complete-path conversion of the
     ;; flags path itself fails. Success behavior is unchanged: the file
     ;; is written only after the root verified and the launch arguments
     ;; were derived.
     ;;
     (define (invalidate-stale-flags!)
       (define ff (flags-file))
       (when (and ff (file-exists? ff))
         (delete-file ff)))
     (with-handlers ([exn:fail? (lambda (e)
                                  (invalidate-stale-flags!)
                                  (raise e))])
       (define ff (require-opt! flags-file "flags-file"))
       ;; The resolver API and the flags payload work in path values;
       ;; normalize CLI strings to complete paths once, here.
       (define ff-path (path->complete-path (string->path ff)))
       (define rd (require-opt! root-dir "root"))
       (define co (require-opt! checkout-dir "checkout"))
       (define rd-path (path->complete-path (string->path rd)))
       (define co-path (path->complete-path (string->path co)))
       (when (null? (trusted-labels))
         (error 'ci-compiled-root "resolve requires at least one --trusted-label"))
       (define md
         ;; map-dir is stored in the root struct and consumed by
         ;; compiled-root-launch-arguments as a path value.
         (path->directory-path (path->complete-path (or (map-dir-opt)
                                                        (build-path (path-only ff-path)
                                                                    "compiled-root-map")))))
       (define root
         (resolve-compiled-root!
          #:final-dir rd-path
          #:checkout co-path
          #:trusted-producer-labels (trusted-labels)
          #:map-dir md
          #:install-map-dir (let ([d (install-map-opt)])
                              (and d (path->directory-path (path->complete-path (string->path d)))))
          #:collection-roots
          (let ([ds (collection-roots-opt)])
            (and (not (null? ds))
                 (map (lambda (d) (path->directory-path (path->complete-path (string->path d))))
                      ds)))))
       (unless (eq? (compiled-root-reason root) #t)
         (invalidate-stale-flags!)
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
               (path->string ff-complete)))]
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
            (not (string=? ff ""))
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
     (define-values (exit-code mode served map-counters)
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
          ;; W1a install-map repair: per-root counter files (best-effort;
          ;; a missing hits dir keeps them 0, like the hit counters)
          (define map-counters
            (if hits-dir
                (call-with-values (lambda () (aggregate-compiled-root-map-counters hits-dir)) list)
                (list 0 0 0)))
          (values code "verified-root-hit" served map-counters)]
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
                  (list 0 0 0)
                  (list 0 0 0))]))
     (define wall-ms (inexact->exact (floor (- (current-inexact-milliseconds) t0))))
     (write-launch-telemetry! mode exit-code wall-ms served map-counters)
     (exit exit-code)]))
