;; @suite all
;; @speed fast
;; @boundary unit
;; @isolation process
;; @mutates temp
;; @requires fs

#lang racket/base

;; q/tests/ci/metadata-discovery-test.rkt — W0 discovery-parity fixture test.
;;
;; WAVE: W0 of docs/planning/PLAN-v1.00.11-TDD-CI-INTEGRITY-BASELINES.md.
;;
;; PURPOSE: pin the file-discovery contract of
;; q/scripts/run-tests/classify-metadata.rkt (base-dir resolution +
;; q-root-candidate?) and q/scripts/run-tests/classify.rkt
;; (`collect-test-files`) so that both invocation modes discover the
;; IDENTICAL normalized relative path list:
;;
;;   Mode A — "direct CLI from repo root": the discovery entry point is
;;   launched with a RELATIVE script path and cwd = the root, exactly like
;;   CI's `run: racket scripts/run-tests/classify-metadata.rkt --lint-metadata`.
;;
;;   Mode B — "clean-copy temp root mimicking the CI checkout": an
;;   identical copy of the tree is placed at a fresh temporary root and
;;   the entry point is launched FROM that root with an ABSOLUTE script
;;   path (CI checks out and runs from the checkout root).
;;
;; The frozen input tree is q/tests/metadata-discovery/fixture/ (see its
;; README.md for the per-path contract: ordinary files, nested and deeply
;; nested directories, generated/ content, compiled/ bytecode areas,
;; a symlink, an @not-test helper, and a path outside the discovery root).
;;
;; If the two modes ever diverge, this test fails with the exact divergent
;; paths (the defect fixture) instead of silently re-baselining. No
;; CI-only discovery branch is introduced by this wave.

(require rackunit
         racket/file
         racket/list
         racket/path
         racket/port
         racket/string
         racket/system)

;; ------------------------------------------------------------
;; Paths
;; ------------------------------------------------------------

;; NOTE: `find-system-path 'run-file` names the *racket/raco executable*,
;; not this test file, so we use the module's own source syntax instead
;; (works under both `racket -t` and `raco test`).
(define this-file
  (let ([src (syntax-source #'here)])
    (cond [(path? src) (simplify-path (path->complete-path src))]
          [else (simplify-path (find-system-path 'run-file))])))
(define repo-root
  (simplify-path (build-path (path-only this-file) 'up 'up)))
(define fixture-root (build-path repo-root "tests" "metadata-discovery" "fixture"))
(define classify-facade (build-path repo-root "scripts" "run-tests" "classify.rkt"))

;; ------------------------------------------------------------
;; Expected contract (mirrors fixture/README.md)
;; ------------------------------------------------------------

;; Files that MUST be discovered when symlinks are materialized as
;; ordinary files (checkout without symlink support). NOTE:
;; tests/generated/generated-test.rkt is NOT here — the current
;; discovery excludes generated/ trees (documented ignore contract),
;; which is exactly one of the sources of the local-vs-CI divergence
;; this fixture pins.
(define expected-flat
  '("tests/alpha-test.rkt"
    "tests/beta-plain.rkt"
    "tests/link-target.rkt"
    "tests/nested/deep/deep-test.rkt"
    "tests/nested/nested-test.rkt"))

;; The symlink entry is additionally discovered when the platform
;; preserves symlinks (sorted last: "symlinked-test.rkt" > "nested/...").
(define expected-with-symlink
  (append expected-flat '("tests/symlinked-test.rkt")))

;; Paths that MUST NOT appear in any mode.
(define forbidden
  '("tests/compiled/stray-test.rkt"   ; /compiled/ exclusion
     "tests/generated/generated-test.rkt" ; generated/ exclusion
     "tests/not-a-test-helper.rkt"     ; @not-test exclusion
     "outside/outside-test.rkt"))      ; outside the discovery root

;; ------------------------------------------------------------
;; Driver: launch the CURRENT discovery entry point
;; ------------------------------------------------------------

;; The driver script below is the file given to `racket`, so
;; (find-system-path 'orig-dir) inside classify-metadata.rkt resolves to
;; <root>/scripts, and resolve-base-dir walks its candidate list to <root>
;; — the same resolution the real scripts/run-tests.rkt entry performs.
;; The driver requires the UNMODIFIED repository facade by absolute path,
;; so the code under test is always the current tree's implementation.
(define (driver-script-content)
  (format
   "#lang racket/base~n(require (file ~s))~n(for ([f (in-list (sort (collect-test-files 'all) string<?))])~n  (displayln f))~n"
   (path->string classify-facade)))

(define (make-root! tag)
  (define root (make-temporary-file (format "q-w0-discovery-~a-~~a" tag) 'directory))
  (make-directory* (build-path root "scripts"))
  ;; NOTE: `outside` and `tests` are NOT pre-created — copy-directory/files
  ;; requires its destination to not already exist.
  ;; scripts/run-tests.rkt marker + discovery driver.
  (call-with-output-file (build-path root "scripts" "run-tests.rkt")
    #:exists 'replace
    (lambda (p) (display (driver-script-content) p)))
  ;; tests/ = verbatim copy of the frozen fixture tree.
  (copy-directory/files (build-path fixture-root "tests") (build-path root "tests"))
  ;; Path outside the discovery root: a q-root-shaped decoy is NOT needed
  ;; here; a plain sibling directory is sufficient to pin exclusion.
  (copy-directory/files (build-path fixture-root "outside") (build-path root "outside"))
  root)

;; invoke : root mode -> (listof string?)
;;   mode 'relative-cwd-root : `racket scripts/run-tests.rkt`, cwd = root (local CLI mode)
;;   mode 'absolute-cwd-root : `racket /abs/root/scripts/run-tests.rkt`, cwd = root (clean-copy CI mode)
(define (invoke root mode)
  (define racket-bin (or (find-executable-path "racket")
                          (error 'metadata-discovery-test "racket not on PATH")))
  (define-values (proc out in err)
    (let-values ([(cwd script-arg)
                  (case mode
                    [(relative-cwd-root)
                     (values root (build-path "scripts" "run-tests.rkt"))]
                    [(absolute-cwd-root)
                     (values root (build-path root "scripts" "run-tests.rkt"))]
                    [else (error 'invoke "unknown mode ~a" mode)])])
      (parameterize ([current-directory cwd])
        (subprocess #f #f #f racket-bin (path->string script-arg)))))
  (close-output-port in)
  (define stdout (port->string out))
  (define stderr (port->string err))
  (subprocess-wait proc)
  (define status (subprocess-status proc))
  (unless (zero? status)
    (error 'metadata-discovery-test
           "discovery driver failed (mode ~a): ~a~a"
           mode stdout stderr))
  (filter (lambda (l) (not (string=? l "")))
          (string-split stdout "\n")))

;; ------------------------------------------------------------
;; Parity test
;; ------------------------------------------------------------

(module+ test
  (define root-a (make-root! 'a))
  (define root-b (make-root! 'b))
  (dynamic-wind
    (lambda () (void))
    (lambda ()
      (define mode-a (invoke root-a 'relative-cwd-root))
      (define mode-b (invoke root-b 'absolute-cwd-root))

      ;; The parity contract: identical normalized relative path lists.
      (check-equal? mode-a mode-b
                    (format
                     "DISCOVERY DIVERGENCE (defect fixture) — mode A vs mode B differ.~nA-only: ~a~nB-only: ~a"
                     (string-join (remove* mode-b mode-a) ", ")
                     (string-join (remove* mode-a mode-b) ", ")))

      ;; The frozen-tree contract: exactly the expected entries (symlink
      ;; entry depends on platform symlink support).
      (define expected
        (if (link-exists? (build-path fixture-root "tests" "symlinked-test.rkt"))
            expected-with-symlink
            expected-flat))
      (check-equal? mode-a expected
                    (format "discovered list diverged from the fixture contract.~ngot:      ~a~nexpected: ~a"
                            mode-a expected))

      ;; Nothing forbidden is discovered in either mode.
      (for ([mode-name (in-list '(mode-a mode-b))]
            [paths (in-list (list mode-a mode-b))])
        (for ([bad (in-list forbidden)])
          (check-false (member bad paths)
                       (format "forbidden path ~a discovered in ~a" bad mode-name)))))
    (lambda ()
      (delete-directory/files root-a)
      (delete-directory/files root-b))))
