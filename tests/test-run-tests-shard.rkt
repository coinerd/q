#lang racket

;; @speed fast
;; @suite default
;; @boundary unit

;; BOUNDARY: unit

;; Test shard support and platform-cross classifier.
;; CI Acceleration W0 — PR gate ≤ 20 min.
;;
;; W5: collector/classifier/sharding assertions run ONLY on the
;; tiny explicit fixture tree tests/fixtures/run-tests-discovery/ via the
;; #:root/#:test-root seam — this file never crawls the live repository.
;; Real repository-scale discovery is owned by the scheduled L4 smoke in
;; tests/test-run-tests-repository-discovery.rkt.

(require rackunit
         rackunit/text-ui
         racket/port
         racket/runtime-path
         racket/string
         racket/file
         racket/path
         racket/system)

(require (only-in "../scripts/run-tests/classify.rkt" collect-test-files platform-file? shard-files)
         (only-in "../scripts/run-tests/cli.rkt" known-suites)
         (only-in "../scripts/run-tests/inventory.rkt" compute-inventory-hash selected-paths-digest)
         (only-in "../scripts/run-tests/classify-metadata.rkt" get-file-metadata))

(define-runtime-path fixture-root "../tests/fixtures/run-tests-discovery")
(define-runtime-path runner-script "../scripts/run-tests.rkt")

;; The exact expected collection of the fixture tree for suite 'all:
;; deterministic lexicographic per-level sort, support modules excluded
;; (/helpers/, /fixtures/), not-test marker excluded (zulu-not-test).
(define expected-all
  '("tests/alpha-heuristic-test.rkt" "tests/eps-mutating-probe.rkt"
                                     "tests/gamma-platform-test.rkt"
                                     "tests/iota-tui-named-test.rkt"
                                     "tests/nested/deep/zeta-deep-nested-test.rkt"
                                     "tests/nested/eta-nested-test.rkt"
                                     "tests/theta-slow-quietly-test.rkt"
                                     "tests/zeta-fast-test.rkt"))

;; Throwaway full copy of the fixture tree for escape/edge cases that
;; cannot be committed (symlinks, compiled/ bytecode, edited metadata).
(define (make-temp-dir! tag)
  (define base (find-system-path 'temp-dir))
  (let loop ([n 0])
    (define p (build-path base (format "~a-~a-~a" tag (current-milliseconds) n)))
    (with-handlers ([exn:fail:filesystem? (lambda (_) (loop (add1 n)))])
      (make-directory p)
      p)))

(define (copy-fixture-tree!)
  (define dst (build-path (make-temp-dir! "w5-discovery") "root"))
  (define fixture-root/simple (simple-form-path fixture-root))
  (for ([src (in-directory fixture-root)])
    (when (file-exists? src) ; skip directories (incl. symlinked dirs)
      (define rel (find-relative-path fixture-root/simple (simple-form-path src)))
      (define target (build-path dst rel))
      (make-directory* (path-only target))
      (copy-file src target)))
  dst)

;; `make-file-or-directory-link` argument behavior is fragile across
;; Racket versions (its "path already exists" error names the target);
;; shell out to `ln -s` for an unambiguous symlink.
(define (make-symlink! link-path target)
  (unless (zero? (system*/exit-code (find-executable-path "ln") "-s" target link-path))
    (error 'make-symlink! "ln -s failed: ~a -> ~a" link-path target)))

(define shard-suite
  (test-suite "Shard and platform tests"

    (test-case "shard 0/1 selects all files"
      (check-equal? (length (shard-files '("a" "b" "c" "d" "e") 0 1)) 5))

    (test-case "shard 0/3 selects correct subset"
      (check-equal? (shard-files '("a" "b" "c" "d" "e" "f") 0 3) '("a" "d")))

    (test-case "shard 1/3 selects correct subset"
      (check-equal? (shard-files '("a" "b" "c" "d" "e" "f") 1 3) '("b" "e")))

    (test-case "shard 2/3 selects correct subset"
      (check-equal? (shard-files '("a" "b" "c" "d" "e" "f") 2 3) '("c" "f")))

    (test-case "three shards union = full set"
      (define files '("a" "b" "c" "d" "e" "f" "g"))
      (define union
        (sort (append (shard-files files 0 3) (shard-files files 1 3) (shard-files files 2 3))
              string<?))
      (check-equal? union (sort files string<?)))

    (test-case "shard 0/1 = identity"
      (check-equal? (shard-files '("x" "y") 0 1) '("x" "y")))

    (test-case "shard-total 0 is rejected"
      (check-exn exn:fail? (lambda () (shard-files '("a") 0 0))))

    (test-case "shard-index >= shard-total is rejected"
      (check-exn exn:fail? (lambda () (shard-files '("a") 3 3))))

    (test-case "empty files with valid shard is empty"
      (check-equal? (shard-files '() 0 3) '()))

    (test-case "subprocess test is platform-file"
      (check-true (platform-file? "tests/test-subprocess.rkt")))

    (test-case "cwd-independence test is platform-file"
      (check-true (platform-file? "tests/test-cwd-independence.rkt")))

    (test-case "version test is platform-file (curated)"
      (check-true (platform-file? "tests/test-version.rkt")))

    (test-case "non-curated test is NOT platform-file"
      (check-false (platform-file? "tests/test-something-not-in-list.rkt")))

    (test-case "platform is a known suite"
      (check-true (and (member 'platform known-suites) #t)))

    ;; -------------------------------------------------------------------
    ;; W5: explicit-root discovery over the hermetic fixture tree.
    ;; -------------------------------------------------------------------

    (test-case "explicit root collection is deterministic and exact"
      (define a (collect-test-files 'all #:root fixture-root))
      (define b (collect-test-files 'all #:root fixture-root))
      (check-equal? a b)
      (check-equal? a expected-all))

    (test-case "#:root and #:test-root are aliases"
      (check-equal? (collect-test-files 'all #:root fixture-root)
                    (collect-test-files 'all #:test-root fixture-root)))

    (test-case "passing both root keywords is rejected"
      (check-exn exn:fail?
                 (lambda () (collect-test-files 'all #:root fixture-root #:test-root fixture-root))))

    (test-case "root with .. components normalizes to the same tree"
      ;; p/../p — same tree, but the root carries a `..` component that
      ;; must normalize away before containment checks.
      (define dotted (build-path fixture-root 'up (file-name-from-path fixture-root)))
      (check-equal? (collect-test-files 'all #:root dotted)
                    (collect-test-files 'all #:root fixture-root)))

    (test-case "suite partitions over the fixture root"
      (check-equal? (collect-test-files 'slow #:root fixture-root)
                    '("tests/theta-slow-quietly-test.rkt"))
      (check-equal? (collect-test-files 'tui #:root fixture-root) '("tests/iota-tui-named-test.rkt"))
      (check-equal? (collect-test-files 'platform #:root fixture-root)
                    '("tests/gamma-platform-test.rkt"))
      (check-equal? (collect-test-files 'unit_fast #:root fixture-root)
                    '("tests/zeta-fast-test.rkt")))

    (test-case "fast partition excludes metadata-slow and metadata-tui"
      (check-equal? (collect-test-files 'fast #:root fixture-root)
                    '("tests/alpha-heuristic-test.rkt" "tests/eps-mutating-probe.rkt"
                                                       "tests/gamma-platform-test.rkt"
                                                       "tests/nested/deep/zeta-deep-nested-test.rkt"
                                                       "tests/nested/eta-nested-test.rkt"
                                                       "tests/zeta-fast-test.rkt")))

    (test-case "heuristic selection of clean filename without metadata"
      ;; alpha-heuristic-test.rkt carries NO metadata tags; it is selected
      ;; for fast by the clean-filename heuristic alone.
      ;; NOTE: member returns the tail (a truthy list), not a boolean —
      ;; assert with check-not-false, never check-true.
      (check-not-false (member "tests/alpha-heuristic-test.rkt"
                               (collect-test-files 'fast #:root fixture-root))
                       "alpha heuristic file must be in fast"))

    (test-case "slow metadata wins over clean filename heuristics"
      ;; theta-slow-quietly-test.rkt: the name carries no slow pattern;
      ;; only its @speed slow metadata places it in the slow partition.
      (check-not-false (member "tests/theta-slow-quietly-test.rkt"
                               (collect-test-files 'slow #:root fixture-root)))
      (check-false (member "tests/theta-slow-quietly-test.rkt"
                           (collect-test-files 'fast #:root fixture-root))))

    (test-case "helpers/ and fixtures/ support modules are excluded"
      (define all (collect-test-files 'all #:root fixture-root))
      (check-false (member "tests/helpers/event-simulator.rkt" all))
      (check-false (member "tests/fixtures/data-fixture.rkt" all)))

    (test-case "@not-test files are excluded from every collection"
      (for ([suite '(all fast slow tui unit_fast platform)])
        (check-false (member "tests/zulu-not-test.rkt" (collect-test-files suite #:root fixture-root))
                     (format "zulu leaked into suite ~a" suite))))

    (test-case "@not-test and malformed metadata are inspectable via metadata API"
      (define zulu (build-path fixture-root "tests" "zulu-not-test.rkt"))
      (check-true (hash-ref (get-file-metadata zulu) 'not-test? #f)))

    (test-case "shard partition over a fixture-root collection is lossless"
      (define files (collect-test-files 'all #:root fixture-root))
      (define union
        (sort (append (shard-files files 0 3) (shard-files files 1 3) (shard-files files 2 3))
              string<?))
      (check-equal? union (sort files string<?))
      (check-equal? (length (shard-files files 0 4)) 2))

    (test-case "inventory digest over fixture collection is a stable SHA-256"
      (define files (collect-test-files 'all #:root fixture-root))
      (check-true (regexp-match? #px"^[0-9a-f]{64}$" (compute-inventory-hash files)))
      (check-equal? (compute-inventory-hash files) (selected-paths-digest files))
      (check-equal? (compute-inventory-hash files) (compute-inventory-hash (reverse files))))

    ;; -------------------------------------------------------------------
    ;; Containment and cache-isolation edge cases (throwaway temp copies).
    ;; -------------------------------------------------------------------

    (test-case "directory symlinks escaping the root are not descended"
      (define root (copy-fixture-tree!))
      (define outside (build-path (path-only root) "outside-dir"))
      (make-directory* outside)
      (with-output-to-file (build-path outside "escapee-test.rkt")
                           (lambda () (displayln "#lang racket"))
                           #:exists 'replace)
      (make-symlink! (build-path root "tests" "escape-link") outside)
      (define files (collect-test-files 'all #:root root))
      (check-false (for/or ([f (in-list files)])
                     (string-contains? f "escape-link"))
                   "symlinked directory outside the root must not be descended"))

    (test-case "file symlinks resolving outside the root are rejected"
      (define root (copy-fixture-tree!))
      (define outside (build-path (path-only root) "outside-dir"))
      (make-directory* outside)
      (with-output-to-file (build-path outside "outside-test.rkt")
                           (lambda () (displayln "#lang racket"))
                           #:exists 'replace)
      (make-symlink! (build-path root "tests" "outside-link-test.rkt")
                     (build-path outside "outside-test.rkt"))
      (define files (collect-test-files 'all #:root root))
      (check-false (for/or ([f (in-list files)])
                     (string-contains? f "outside-link-test.rkt"))
                   "symlinked file resolving outside the root must be rejected"))

    (test-case "compiled/ bytecode is never collected"
      (define root (copy-fixture-tree!))
      (make-directory* (build-path root "tests" "compiled"))
      (with-output-to-file (build-path root "tests" "compiled" "skip-me-test.rkt")
                           (lambda () (displayln "#lang racket"))
                           #:exists 'replace)
      (define files (collect-test-files 'all #:root root))
      (check-equal? files expected-all))

    (test-case "missing tests/ root fails closed"
      (define tmp (make-temp-dir! "w5-empty"))
      (check-exn exn:fail? (lambda () (collect-test-files 'all #:root tmp))))

    (test-case "metadata cache is isolated between distinct roots"
      (define root (copy-fixture-tree!))
      ;; Same relative filename, different metadata in the copy: the copy's
      ;; zeta-fast becomes @speed slow. Classification must follow each
      ;; root's own file, never a stale cross-root cache entry.
      (define zeta (build-path root "tests" "zeta-fast-test.rkt"))
      (define src (file->string zeta))
      (with-output-to-file zeta
                           (lambda () (display (string-replace src "@speed fast" "@speed slow")))
                           #:exists 'replace)
      ;; Suite 'slow selection falls back to speed-based inclusion, so
      ;; the copy selects BOTH theta (born slow) and the edited zeta
      ;; (now slow); the untouched fixture root still selects only theta.
      (check-equal? (collect-test-files 'slow #:root root)
                    '("tests/theta-slow-quietly-test.rkt" "tests/zeta-fast-test.rkt"))
      (check-equal? (collect-test-files 'slow #:root fixture-root)
                    '("tests/theta-slow-quietly-test.rkt")))

    ;; -------------------------------------------------------------------
    ;; W3: stable inventory identity for gate evidence.
    ;; equal-hash-code is randomized per Racket process; the selected
    ;; inventory identity must be the SHA-256 selected-path digest so that
    ;; recorded evidence can be re-derived and compared after the fact.
    ;; -------------------------------------------------------------------

    (test-case "inventory hash is a stable SHA-256 selected-path digest"
      (define files '("tests/a.rkt" "tests/b.rkt" "tests/c.rkt"))
      ;; Full 64-hex SHA-256, not a process-dependent hash code.
      (check-true (regexp-match? #px"^[0-9a-f]{64}$" (compute-inventory-hash files)))
      ;; Same canonical digest as the existing selected-path digest.
      (check-equal? (compute-inventory-hash files) (selected-paths-digest files))
      ;; Canonical over the sorted, de-duplicated path set.
      (check-equal? (compute-inventory-hash files) (compute-inventory-hash (reverse files)))
      (check-equal? (compute-inventory-hash files) (compute-inventory-hash (append files files)))
      (check-not-equal? (compute-inventory-hash files)
                        (compute-inventory-hash '("tests/a.rkt" "tests/b.rkt" "tests/d.rkt"))))

    (test-case "a shard's inventory digest never equals the full-suite digest"
      (define files
        (for/list ([i (in-range 6)])
          (format "tests/shard-fixture-~a.rkt" i)))
      (define full-digest (compute-inventory-hash files))
      (for ([idx (in-range 3)])
        (check-not-equal? (compute-inventory-hash (shard-files files idx 3)) full-digest)))

    (test-case "runner refuses --record-gate-evidence on sharded runs (fail closed)"
      (define racket-bin (find-executable-path "racket"))
      (define-values (sp out in err)
        (subprocess #f
                    #f
                    #f
                    racket-bin
                    runner-script
                    "--suite"
                    "smoke"
                    "--record-gate-evidence"
                    "--shard-index"
                    "0"
                    "--shard-total"
                    "2"))
      (close-output-port in)
      (define done (sync/timeout 600 sp))
      (unless done
        (subprocess-kill sp #t)
        (fail "runner did not exit before the 600s timeout"))
      (define stdout-text (port->string out))
      (define stderr-text (port->string err))
      (close-input-port out)
      (close-input-port err)
      (check-not-equal? (subprocess-status sp) 0)
      (check-true (string-contains? stderr-text "shard"))
      ;; Refusal happens before any run: no RUN-SUMMARY, no PASS record.
      (check-false (string-contains? stdout-text "RUN-SUMMARY")))))

(module+ main
  ;; Propagate rackunit's failure count to the process exit code so the
  ;; runner's exit-code-based verdict can fail this file.
  (exit (if (zero? (run-tests shard-suite)) 0 1)))
