#lang racket/base

;; tests/test-runner-base-dir-resolution.rkt
;;
;; The test runner must measure the tree it was launched from.
;;
;; resolve-base-dir used to try the `q`-shaped candidates FIRST
;; (`<orig>/q`, then `<parent-of-orig>/q`) and only afterwards the directory
;; it was actually given. In this project's working area every git worktree
;; of the repo lives next to a `q/` clone, so a runner launched from
;; `<area>/wt-v10031-w7` resolved base-dir to `<area>/q` — a DIFFERENT
;; checkout, at a different commit, with a different q-version. The suite
;; then collected and executed the sibling clone's test files and printed a
;; RUN-SUMMARY (including its runner-version) for a tree nobody asked
;; about, while the tree under test was never executed. That silently
;; invalidates local gate evidence, which is exactly what the
;; evidence-identity guards exist to prevent.
;;
;; The launch tree (and its nearest q-root ancestor) must win; the
;; `q`-shaped candidates remain only as a fallback for the monorepo case
;; where the runner is launched from a directory that is not itself inside
;; a repository.
;;
;; Invocation note: these are top-level test-case forms, so running the file
;; directly (`racket tests/<file>.rkt`, how the suite runner executes it)
;; reports failures as rackunit FAILURE blocks and exits 0. That is expected:
;; the runner classifies a file by parsing the FAILURE block, not by exit code.
;; Do not "fix" this by wrapping the cases in a `run-tests` call — that is not
;; this repository's convention. The assertions are non-vacuous: mutating the
;; launch-tree expectation prints a FAILURE block, which is what the runner
;; acts on.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/string
         (only-in "../scripts/run-tests/classify-metadata.rkt" resolve-base-dir q-root-candidate?))

;; Materialize a minimal q-root: tests/ directory + scripts/run-tests.rkt.
(define (make-q-root! dir)
  (make-directory* (build-path dir "tests"))
  (make-directory* (build-path dir "scripts"))
  (call-with-output-file (build-path dir "scripts" "run-tests.rkt")
                         (lambda (out) (display "#lang racket\n" out))
                         #:exists 'truncate)
  (simplify-path dir))

;; Path identity that ignores simplify-path's trailing separator.
(define (same-path? a b)
  (define (norm p)
    (regexp-replace #rx"/+$" (path->string (simplify-path p)) ""))
  (string=? (norm a) (norm b)))

;; Call proc with a fresh empty directory; remove it afterwards.
(define (with-temp-dir proc)
  (define root (simplify-path (make-temporary-directory "q-base-dir-~a")))
  (dynamic-wind void
                (lambda () (proc root))
                (lambda ()
                  (when (directory-exists? root)
                    (delete-directory/files root #:must-exist? #f)))))

(test-case "a sibling q/ clone never steals the launch tree"
  (with-temp-dir (lambda (root)
                   (define clone (make-q-root! (build-path root "q")))
                   (define wt (make-q-root! (build-path root "wt-v10031-w7")))
                   (define resolved (resolve-base-dir wt))
                   (check-false (same-path? resolved clone)
                                "the sibling q/ clone must not be substituted for the launch tree")
                   (check-true (same-path? resolved wt)
                               "the launch tree wins over a sibling q/ clone"))))

(test-case "a nested launch directory resolves to its own repository root"
  (with-temp-dir (lambda (root)
                   (define clone (make-q-root! (build-path root "q")))
                   (define wt (make-q-root! (build-path root "wt-deep")))
                   (define nested (build-path wt "tests" "unit"))
                   (make-directory* nested)
                   (define resolved (resolve-base-dir nested))
                   (check-false (same-path? resolved clone)
                                "no sibling substitution from a nested directory")
                   (check-true (same-path? resolved wt)
                               "the nearest q-root ancestor of the launch directory is used"))))

(test-case "the monorepo fallback still resolves a sibling q/ when launched outside any repo"
  (with-temp-dir (lambda (root)
                   (define clone (make-q-root! (build-path root "q")))
                   (define outside (build-path root "elsewhere"))
                   (make-directory* outside)
                   (check-true (same-path? (resolve-base-dir outside) clone)
                               "a launch directory outside any repo still finds the q/ checkout"))))

(test-case "q-root-candidate? requires both the tests tree and the runner entry point"
  (with-temp-dir (lambda (root)
                   (define half (build-path root "half"))
                   (make-directory* (build-path half "tests"))
                   (check-false (q-root-candidate? half) "tests/ alone is not a q-root")
                   (make-q-root! half)
                   (check-true (q-root-candidate? half)
                               "tests/ plus scripts/run-tests.rkt is a q-root"))))
