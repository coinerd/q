#lang racket/base

;; q/scripts/run-tests/classify.rkt — File classification and discovery
;;
;; Facade module: re-exports metadata parsing (classify-metadata.rkt) and
;; suite classifiers (classify-filters.rkt), and provides shard distribution,
;; file collection, path utilities, and bytecode cleanup.
;; Extracted from run-tests.rkt (v0.96.16, AX1-2).
;; Further decomposed in v0.99.58 W3-1 (P3-CL).
;; STABILITY: internal (test runner infrastructure)

(require racket/string
         racket/path
         racket/file
         racket/list
         racket/system
         "classify-metadata.rkt"
         "classify-filters.rkt")

;; Re-export the full extracted API surface for backward compatibility.
(provide (all-from-out "classify-metadata.rkt")
         (all-from-out "classify-filters.rkt")
         ;; Shard support
         shard-files
         ;; Metadata classification provenance (W1)
         file-metadata-classification
         ;; Path utilities
         normalize-test-path
         ;; File collection
         collect-test-files
         ;; Repo surface restore
         repo-surface-files
         restore-repo-surfaces!
         ;; Bytecode cleanup
         clean-stale-bytecode!)

;; ============================================================
;; Shard support — select files by round-robin modulo
;; ============================================================

(define (shard-files files shard-index shard-total)
  (unless (and (integer? shard-total) (> shard-total 0))
    (raise-argument-error 'shard-files "positive integer" shard-total))
  (unless (and (integer? shard-index) (>= shard-index 0) (< shard-index shard-total))
    (raise-argument-error 'shard-files (format "integer in [0, ~a)" shard-total) shard-index))
  (for/list ([f (in-list files)]
             [i (in-naturals)]
             #:when (= (modulo i shard-total) shard-index))
    f))

;; ============================================================
;; Metadata classification provenance (W1)
;; ============================================================

;; All metadata parsing routes through the schema-aware parser in
;; classify-metadata.rkt (see `validate-file` there for schema v1 and the
;; report-only lint). Heuristic (filename/path-based) classification results
;; are labeled 'heuristic rather than 'explicit so downstream reports can
;; distinguish declarative metadata from heuristic fallbacks.
(define (file-metadata-classification f)
  (hash-ref (get-file-metadata f) 'classification 'heuristic))

;; ============================================================
;; Path utilities
;; ============================================================

(define (normalize-test-path f)
  (define s
    (if (path? f)
        (path->string f)
        f))
  (cond
    [(absolute-path? s) s]
    [(string-prefix? s "q/tests/") (substring s 2)]
    [(string-prefix? s "./q/tests/") (substring s 4)]
    [else s]))

;; ============================================================
;; File collection
;; ============================================================

;; ============================================================
;; File collection
;; ============================================================
;;
;; Default behavior (no root keywords): byte-for-byte the pre-W5 crawl of
;; the repository's own tests/ tree, in in-directory order.
;;
;; Explicit-root seam (v1.00.24 W5): `#:root` / `#:test-root` (aliases)
;; point collection at an explicit fixture root shaped like a repository
;; root: the walk visits `<root>/tests` recursively and returns
;; ROOT-RELATIVE normalized paths. Contract (pinned by
;; tests/test-run-tests-shard.rkt):
;;   - deterministic output: entries sorted lexicographically per level,
;;     pre-order (files and directories interleaved by name);
;;   - containment: no selected path may escape the root — directory
;;     symlinks are never descended (no symlink escape), file symlinks are
;;     resolved and rejected when their target lies outside the root, and
;;     `..` components in the root argument itself are normalized away;
;;   - metadata reads use the RESOLVED ABSOLUTE path of every file, so the
;;     metadata cache stays isolated between distinct roots (and between a
;;     fixture root and the repository);
;;   - selection mirrors the default-root rules: `.rkt` suffix, `compiled/`
;;     exclusion, `/helpers/` + `/fixtures/` support-module exclusion, and
;;     `@not-test` exclusion — repo-relative discovery ignore prefixes
;;     (tests/metadata-discovery/fixture/ et al.) do not apply inside an
;;     explicit root, because the caller owns that root;
;;   - a missing `<root>/tests` directory fails closed;
;;   - the shared metadata cache is NOT cleared: absolute-path keys cannot
;;     collide across roots.

(define (collect-test-files suite
                            #:extra-files [extra-files #f]
                            #:root [root #f]
                            #:test-root [test-root #f])
  (cond
    [(pair? extra-files) (map normalize-test-path extra-files)]
    [(and root test-root)
     (raise-argument-error 'collect-test-files
                           "only one of #:root/#:test-root"
                           (format "~s ~s" root test-root))]
    [(or root test-root) (collect-test-files/under suite (or root test-root))]
    [else (collect-test-files/default suite)]))

(define (collect-test-files/default suite)
  (define all-files
    (for/list ([f (in-directory (build-path base-dir "tests"))]
               #:when (and (file-exists? f)
                           (let* ([s (path->string f)]
                                  [rel (path->string (find-relative-path base-dir f))])
                             (and (string-suffix? s ".rkt")
                                  (not (string-contains? s "/compiled/"))
                                  ;; v1.00.11 hotfix: the discovery-parity fixture tree
                                  ;; (tests/metadata-discovery/fixture/) is frozen input
                                  ;; data for tests/ci/metadata-discovery-test.rkt, which
                                  ;; copies it into its own temp root before collecting.
                                  ;; The repo-root walk must never collect or execute it.
                                  (not (for/or ([prefix (in-list discovery-ignored-path-prefixes)])
                                         (string-prefix? rel prefix)))
                                  (not (support-test-module? s))
                                  (not (hash-ref (get-file-metadata rel) 'not-test? #f))))))
      (path->string (find-relative-path base-dir f))))
  (collect-classify-file-list suite all-files))

;; Suite classification over a collected path list. Shared by the default
;; repository branch and the explicit-root branch, so suite semantics are
;; identical regardless of the collection root.
(define (collect-classify-file-list suite all-files)
  (case suite
    [(all broad) all-files]
    [(fast) (filter (lambda (f) (and (not (slow-file? f)) (not (tui-file? f)))) all-files)]
    [(unit_fast unit-fast) (filter unit-fast-file? all-files)]
    [(slow) (filter slow-file? all-files)]
    [(tui) (filter tui-file? all-files)]
    [(smoke) (filter smoke-included? all-files)]
    [(release_smoke release-smoke) (filter release-smoke-included? all-files)]
    [(security) (filter security-file? all-files)]
    [(arch) (filter arch-file? all-files)]
    [(runtime) (filter runtime-file? all-files)]
    [(extensions) (filter extensions-file? all-files)]
    [(workflows) (filter workflows-file? all-files)]
    [(platform) (filter platform-file? all-files)]
    [(mutating) (filter mutating-file? all-files)]
    [else '("tests/")]))

(define (collect-test-files/under suite root-arg)
  ;; simplify-path + path->directory-path normalize away any `..` in the
  ;; caller-supplied root, so nothing below can select above the real root.
  (define root-path
    (simplify-path (path->directory-path (path->complete-path (if (path? root-arg)
                                                                  root-arg
                                                                  (string->path root-arg))
                                                              base-dir))
                   #f))
  (define root-str (path->string root-path))
  (unless (directory-exists? (build-path root-path "tests"))
    (raise-user-error 'collect-test-files "explicit test root has no tests/ directory: ~a" root-path))
  ;; Selection rules over the absolute fixture path. Metadata reads use the
  ;; absolute path, keeping cache entries keyed per root. The support-module
  ;; exclusion (`/helpers/`, `/fixtures/`) is applied to the ROOT-RELATIVE
  ;; path: support directories are a property of the collected tree, and an
  ;; ancestor of the root that merely happens to be named `fixtures` must
  ;; never exclude the entire tree.
  (define (select? fstr)
    (define rel-from-root
      (path->string (find-relative-path (simple-form-path root-path)
                                        (simple-form-path (string->path fstr)))))
    (and (string-suffix? fstr ".rkt")
         (not (string-contains? fstr "/compiled/"))
         (not (support-test-module? rel-from-root))
         (not (hash-ref (get-file-metadata fstr) 'not-test? #f))))
  ;; Containment: resolve symlinks and reject anything leaving the root.
  (define (contained-file? p)
    (define resolved
      (with-handlers ([exn:fail? (lambda (_) p)])
        (simplify-path (resolve-path p) #f)))
    (string-prefix? (path->string resolved) root-str))
  (define (walk! dir)
    (apply append
           (for/list ([entry (in-list (sort (map path->string (directory-list dir #:build? #t))
                                            string<?))])
             (define p (string->path entry))
             (cond
               [(directory-exists? p)
                ;; Containment first: descending is allowed only when the
                ;; directory's RESOLVED target stays under the root, so a
                ;; symlinked directory pointing outside is never descended.
                (define resolved
                  (with-handlers ([exn:fail? (lambda (_) #f)])
                    (path->string (resolve-path p))))
                (if (and resolved (string-prefix? resolved root-str))
                    (walk! p)
                    '())]
               [(and (file-exists? p) (contained-file? p))
                (define fstr (path->string p))
                (if (select? fstr)
                    (list fstr)
                    '())]
               [else '()]))))
  (define selected-abs (walk! (build-path root-path "tests")))
  ;; Classify ABSOLUTE path strings so per-file metadata (@speed, @tui, ...)
  ;; is read from each root's own files — the same contract select? obeys.
  ;; Only the RETURNED names are mapped back to root-relative form.
  (map (lambda (f)
         (normalize-test-path (path->string (find-relative-path (simple-form-path root-path)
                                                                (simple-form-path f)))))
       (collect-classify-file-list suite selected-abs)))

;; ============================================================
;; Repo surface restore (for --restore-surfaces mode)
;; ============================================================

(define repo-surface-files '("info.rkt" "README.md" "CHANGELOG.md"))

(define (restore-repo-surfaces! root)
  (for ([surface (in-list repo-surface-files)])
    (define path (build-path root surface))
    (when (file-exists? path)
      (define git-restore (format "cd ~a && git checkout -- ~a 2>/dev/null" root surface))
      (system git-restore))))

;; ============================================================
;; Bytecode cleanup — remove stale compiled/ artifacts
;; ============================================================

(define (compiled-zo-source-candidates compiled-dir zo)
  (define parent (path-only compiled-dir))
  (define base-path (file-name-from-path zo))
  (define base
    (if base-path
        (path->string base-path)
        ""))
  (define stem (regexp-replace #rx"\\.zo$" base ""))
  (filter values
          (list (and (regexp-match? #rx"_rkt$" stem)
                     (build-path parent (regexp-replace #rx"_rkt$" stem ".rkt")))
                (and (regexp-match? #rx"_rktl$" stem)
                     (build-path parent (regexp-replace #rx"_rktl$" stem ".rktl")))
                (and (regexp-match? #rx"_scrbl$" stem)
                     (build-path parent (regexp-replace #rx"_scrbl$" stem ".scrbl")))
                (path-replace-extension (path-replace-suffix zo "") #".rkt")
                (path-replace-extension (path-replace-suffix zo "") #".rktl"))))

(define (stale-compiled-zo? compiled-dir zo)
  (and (file-exists? zo)
       (string-suffix? (path->string zo) ".zo")
       (let* ([candidates (compiled-zo-source-candidates compiled-dir zo)]
              [existing-sources (filter file-exists? candidates)])
         (or (null? existing-sources)
             (for/or ([src (in-list existing-sources)])
               (> (file-or-directory-modify-seconds src) (file-or-directory-modify-seconds zo)))))))

(define (clean-stale-bytecode! root)
  (define cleaned 0)
  (for ([d (in-directory root)])
    (when (and (directory-exists? d) (equal? (path->string (file-name-from-path d)) "compiled"))
      (define zo-files (directory-list d #:build? #t))
      (define stale?
        (for/or ([zo (in-list zo-files)])
          (stale-compiled-zo? d zo)))
      (when stale?
        (delete-directory/files d)
        (set! cleaned (add1 cleaned)))))
  cleaned)
