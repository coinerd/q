#lang racket

;; @speed slow
;; @suite default
;; @boundary integration

;;; tests/test-run-tests-repository-discovery.rkt — v1.00.24 W5
;;;
;;; The SINGLE scheduled smoke that owns REAL repository-scale test
;;; discovery (the L4 tier). All other runner unit tests
;;; (tests/test-run-tests-shard.rkt et al.) collect from hermetic
;;; fixture roots; this file exercises the production crawl itself and
;;; therefore runs in the slow suite only.
;;;
;;; Assertions are INVARIANT properties, not brittle exact counts:
;;;   - default collection is nonempty and path-normalized;
;;;   - fast/slow/tui selections are disjoint and contained in 'all';
;;;   - the real platform inventory is nonempty (moved here from the
;;;     fixture-root unit tests, which own only synthetic platforms);
;;;   - selected-path digests are deterministic, 64-hex, and
;;;     input-sensitive;
;;;   - the W5 #:root seam is default-call compatible: the default call is
;;;     unchanged, and pointing #:root at the repository root yields a
;;;     superset of the default discovery whose extras are exactly the
;;;     repo-relative discovery-ignore paths (a caller-owned root ignores
;;;     repository ignore rules).
;;;
;;; Exit discipline: run-tests reports a failure count; module main maps
;;; a nonzero count to a nonzero process exit so the runner's verdict
;;; (exit-code based) can fail this file.

(require rackunit
         rackunit/text-ui
         racket/format
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         (prefix-in classify: (file "../scripts/run-tests/classify.rkt"))
         (only-in (file "../scripts/run-tests/inventory.rkt") selected-paths-digest))

(define-runtime-path tests-dir ".")

;; Repository root = parent of tests/.
(define repo-root (simplify-path (build-path tests-dir "..")))

;; ---------------------------------------------------------------
;; Default discovery invariants (real repository)
;; ---------------------------------------------------------------

(define discovery-suite
  (test-suite "repository-scale discovery (L4 smoke)"

    (test-case "default repository discovery is nonempty and normalized"
      (define all-paths (classify:collect-test-files 'all))
      (check-true (pair? all-paths) "expected the real tests/ tree to yield tests")
      (for ([f (in-list all-paths)])
        (check-true (string? f))
        (check-false (string-prefix? f "/") "paths must be repo-relative")
        (check-true (string-suffix? f ".rkt") "only .rkt collected")
        (check-false (string-contains? f "/compiled/") "compiled/ must be excluded")
        (check-false (string-prefix? f "q/tests/") "paths must be tests-relative")))

    (test-case "discovery has no duplicates"
      (define all-paths (classify:collect-test-files 'all))
      (check-equal? (length all-paths) (length (remove-duplicates all-paths))))

    (test-case "speed-tier selections exclude each other where the runner requires it"
      ;; Suite axes are orthogonal: a file may be @suite tui AND @speed slow,
      ;; so slow∩tui legitimately overlaps. The invariant the runner needs is
      ;; that fast excludes slow-speed files and tui-bound files.
      (define fast-paths (classify:collect-test-files 'fast))
      (define slow-paths (classify:collect-test-files 'slow))
      (define tui-paths (classify:collect-test-files 'tui))
      (define (overlap? a b)
        (for/first ([x (in-list a)]
                    #:when (member x b))
          x))
      (check-false (overlap? fast-paths slow-paths))
      (check-false (overlap? fast-paths tui-paths)))

    (test-case "suite selections are contained in 'all"
      (define all-paths (classify:collect-test-files 'all))
      (for ([suite (in-list '(fast slow tui))])
        (for ([f (in-list (classify:collect-test-files suite))])
          (check-not-false (member f all-paths) (format "~a not in all" f)))))

    ;; -------------------------------------------------------------
    ;; Real platform inventory (responsibility moved from W5 unit tests)
    ;; -------------------------------------------------------------

    (test-case "real platform inventory is nonempty"
      (define platform-files (filter classify:platform-file? (classify:collect-test-files 'all)))
      (check-true (pair? platform-files) "expected real platform inventory to be nonempty")
      (check-true (andmap classify:platform-file? platform-files)))

    ;; -------------------------------------------------------------
    ;; Selected-path digest properties (real inventory)
    ;; -------------------------------------------------------------

    (test-case "selected-path digest is deterministic, 64-hex, input-sensitive"
      (define all-paths (classify:collect-test-files 'all))
      (define d1 (selected-paths-digest all-paths))
      (define d2 (selected-paths-digest all-paths))
      (check-equal? d1 d2 "digest must be deterministic")
      (check-true (string? d1))
      (check-equal? (string-length d1) 64 "digest must be sha256 hex")
      (check-true
       (and (andmap (λ (c)
                      (memv c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f)))
                    (string->list d1))
            #t)
       "digest must be lowercase hex")
      (check-not-equal? (selected-paths-digest (cons "tests/zzz-digest-probe.rkt" all-paths))
                        d1
                        "digest must change when the selected set changes"))

    ;; -------------------------------------------------------------
    ;; W5 seam: default-call compatibility + caller-owned-root semantics
    ;; -------------------------------------------------------------

    ;; Documented seam semantics: a caller-owned root does NOT apply the
    ;; repo-relative discovery ignore prefixes (tests/metadata-discovery/fixture/
    ;; is frozen input data whose exclusion belongs to the repository walk).
    ;; Therefore pointing #:root at the repository root yields the default
    ;; discovery PLUS extra paths that carry discovery-ignore prefixes —
    ;; never fewer than the default call.
    (test-case "explicit root = repository root is default-call compatible"
      (define default-all (classify:collect-test-files 'all))
      (define explicit-all (classify:collect-test-files 'all #:root repo-root))
      (define (ignored-prefix? f)
        (for/or ([prefix (in-list classify:discovery-ignored-path-prefixes)])
          (string-prefix? f prefix)))
      ;; Every default path must also be selected under an explicit repo root.
      (check-true (andmap (λ (f) (and (member f explicit-all) #t)) default-all)
                  "explicit repo-root discovery must be a superset of default")
      ;; Extra explicit-root paths must all be discovery-ignore paths
      ;; (the frozen fixture inputs the default walk skips).
      (define extras (filter (λ (f) (not (member f default-all))) explicit-all))
      (check-true (andmap ignored-prefix? extras)
                  "extra explicit-root paths must carry discovery-ignore prefixes")
      (for ([suite (in-list '(fast slow))])
        (define explicit-suite (classify:collect-test-files suite #:root repo-root))
        (check-true (andmap (λ (f) (and (member f explicit-suite) #t))
                            (classify:collect-test-files suite))
                    (format "suite ~a explicit repo-root must be a superset of default" suite)))
      ;; The default call itself must not have drifted: it never selects the
      ;; frozen fixture tree, matching the pre-W5 repository walk.
      (check-false (for/first ([f (in-list default-all)]
                               #:when (ignored-prefix? f))
                     f)
                   "default discovery must keep excluding the discovery-ignore prefixes"))))

(module+ main
  (define failures (run-tests discovery-suite))
  (exit (if (zero? failures) 0 1)))
