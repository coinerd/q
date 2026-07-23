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
         racket/system
         "classify-metadata.rkt"
         "classify-filters.rkt")

;; Re-export the full extracted API surface for backward compatibility.
(provide (all-from-out "classify-metadata.rkt")
         (all-from-out "classify-filters.rkt")
         ;; Shard support
         shard-files
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

(define (collect-test-files suite #:extra-files [extra-files #f])
  (cond
    [(pair? extra-files) (map normalize-test-path extra-files)]
    [else
     (define all-files
       (for/list ([f (in-directory (build-path base-dir "tests"))]
                  #:when (and (file-exists? f)
                              (let* ([s (path->string f)]
                                     [rel (path->string (find-relative-path base-dir f))])
                                (and (string-suffix? s ".rkt")
                                     (not (string-contains? s "/compiled/"))
                                     (not (support-test-module? s))
                                     (not (hash-ref (get-file-metadata rel) 'not-test? #f))))))
         (path->string (find-relative-path base-dir f))))
     (case suite
       [(all broad) all-files]
       [(fast) (filter (lambda (f) (not (slow-file? f))) all-files)]
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
       [else '("tests/")])]))

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
