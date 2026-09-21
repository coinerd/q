#lang racket/base

;; @speed fast
;; @suite workflows
;;
;; Metadata sits in the first lines on purpose: both metadata parsers only scan
;; the file's opening lines (scripts/test-metadata.rkt stops at line 30), and a
;; tag placed after the `require` block is silently… absent — the file then has
;; no tier at all and drops out of the ownership matrix (observed during this
;; wave: 997 rows did not grow by two).

;; tests/test-workflow-invocation-contract.rkt — v1.00.31 W1
;;
;; Repo-wide invocation contract (register F1).
;;
;; `tests/test-compiled-root-workflow.rkt` proves the *one* declaration that
;; broke v1.00.30 W4 is executable. This test proves the property holds for
;; every declaration in the repository — so the next lane cannot reintroduce
;; the same class of breakage in a workflow nobody re-read by hand.
;;
;; Two things are asserted that a "does it look right" review cannot:
;;
;; 1. Coverage. Every YAML file under `.github/` is scanned, and every
;;    extracted declaration is checked against the target script's own
;;    `command-line` option set — no exemptions, no ignore list. A
;;    declaration that cannot be checked is a failure, not a warning.
;; 2. Non-vacuity. A verifier that silently checks nothing looks exactly
;;    like a verifier that checks everything. The floors below (invocation
;;    count, strong-tier count, weak-tier count) make the tool prove it did
;;    real work; they were calibrated against the measured matrix, and they
;;    are the reason the two silent-inertness bugs found in this wave's own
;;    first draft would have been caught by CI instead of by reading.

(require json
         racket/file
         racket/format
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/system
         rackunit
         "../scripts/ci/invocation-contract.rkt")

(define-runtime-path here "test-workflow-invocation-contract.rkt")
(define repo-root (simplify-path (build-path (path-only here) "..")))

(define matrix (invocation-matrix repo-root))
(define declarations (extract-all-declared-invocations repo-root))
(define rows (hash-ref matrix 'invocations))
(define tier-counts (hash-ref matrix 'tier-counts))

(define (describe row)
  (format "~a:~a ~a" (hash-ref row 'source) (hash-ref row 'line) (hash-ref row 'target)))

(define (declare describe-row)
  (format "~a:~a" (hash-ref describe-row 'source) (hash-ref describe-row 'line)))

(test-case "the matrix covers every workflow and action file under .github/"
  (define expected (github-declaration-files repo-root))
  (check-true (>= (length expected) 14) (format "only ~a declaration files found" (length expected)))
  (check-equal? (hash-ref matrix 'declaration-files) expected))

(test-case "every declaration is verified and no declaration is exempt"
  ;; An entry in `unverifiable-targets` would mean the tool gave up on a
  ;; target, which is precisely the silent hole this wave exists to close.
  (check-equal? (hash-ref matrix 'unverifiable-targets) '())
  (check-equal? (hash-ref matrix 'errors) '())
  (for ([row (in-list rows)])
    (check-equal? (hash-ref row 'status) "ok" (describe row))
    (check-equal? (hash-ref row 'unknown-flags) '() (describe row))))

(test-case "the verification tiers did real work (anti-vacuity floors)"
  ;; Calibrated against the measured matrix; a regression that makes the
  ;; extractor inert (a `#lang` reader failure, a byte regexp with counted
  ;; repetition, a path resolved against the wrong root) collapses these to
  ;; zero while every individual check above stays green.
  (check-true (>= (hash-ref matrix 'invocation-count) 60)
              (format "invocation-count=~a" (hash-ref matrix 'invocation-count)))
  (check-true (>= (hash-ref tier-counts "option-set") 8)
              (format "option-set=~a" (hash-ref tier-counts "option-set")))
  (check-true (>= (hash-ref tier-counts "literal-vocabulary") 20)
              (format "literal-vocabulary=~a" (hash-ref tier-counts "literal-vocabulary"))))

(test-case "a declaration that names a Racket source file is never classified as not-a-script"
  ;; `not-a-script` is for probes and subcommands (`racket --version`,
  ;; `raco pkg install`). A declaration that names a `.rkt` file but is
  ;; filed as `not-a-script` would escape verification while looking tidy.
  (for ([d (in-list declarations)])
    (when (equal? (hash-ref d 'kind) "not-a-script")
      (for ([arg (in-list (hash-ref d 'argv))])
        (check-false (regexp-match? #px"\\.rktl?$" arg) (format "~a names ~a" (declare d) arg))))))

(test-case "the compiled-root producer declaration is verified against the CLI itself"
  ;; The F1 declaration, asserted from the extraction side: the flags come
  ;; from the action file, the option set comes from the script.
  (define row
    (for/first ([row (in-list rows)]
                #:when (equal? (hash-ref row 'target) "scripts/ci/compiled-root.rkt"))
      row))
  (check-true (hash? row) "no compiled-root declaration found in .github/")
  (check-equal? (hash-ref row 'status) "ok")
  (check-equal? (hash-ref row 'tier) "option-set")
  (check-true (and (member "--out" (hash-ref row 'flags)) #t))
  (check-true (and (member "--checkout" (hash-ref row 'flags)) #t))
  (check-true (and (member "--trusted-label" (hash-ref row 'flags)) #t)))

(test-case "the matrix is deterministic"
  ;; Same inputs, same report: a matrix whose ordering drifts would produce
  ;; unreproducible evidence artifacts.
  (check-equal? (invocation-matrix repo-root) matrix))

;; ------------------------------------------------------------- the emitter

;; The wave artifact is generated by `scripts/ci/invocation-contract.rkt`, so
;; the generator is exercised here rather than assumed. Both assertions below
;; were earned the hard way: hand-running the CLI during this wave surfaced a
;; relative root leaking absolute `/./`-style `source` paths (the artifact
;; stopped being reproducible) and a JSON writer that rejected the
;; extractor's own key shapes.
(define (emit-matrix! root)
  (define tmp (make-temporary-file "w1-matrix~a.json"))
  (define-values (code transcript)
    (parameterize ([current-directory repo-root])
      (values (system*/exit-code (find-executable-path "racket")
                                 "scripts/ci/invocation-contract.rkt"
                                 "--root"
                                 root
                                 "--out"
                                 tmp)
              "")))
  (check-equal? code 0 (format "emitter failed for root ~a: ~a" root transcript))
  (define text (file->string tmp))
  (delete-file tmp)
  text)

(define (json-ref json key)
  ;; `read-json` in this Racket yields symbol keys; accept either so the test
  ;; does not depend on that detail.
  (cond
    [(hash-has-key? json key) (hash-ref json key)]
    [(hash-has-key? json (string->symbol key)) (hash-ref json (string->symbol key))]
    [else (error 'json-ref "no key ~a" key)]))

(test-case "the emitted matrix is reproducible and matches the live matrix"
  (define from-absolute (emit-matrix! (path->string repo-root)))
  (define from-relative (emit-matrix! "."))
  (check-equal? from-relative from-absolute)
  (define parsed (with-input-from-string from-relative read-json))
  (check-equal? (json-ref parsed "schema") (hash-ref matrix 'schema))
  (check-equal? (json-ref parsed "invocation-count") (hash-ref matrix 'invocation-count))
  ;; The artifact's status tally must agree with the live matrix, so a stale
  ;; or hand-edited artifact cannot claim a greener result than the tool.
  (define status-counts (json-ref parsed "status-counts"))
  (check-equal? (apply + (hash-values status-counts)) (hash-ref matrix 'invocation-count))
  (check-equal? (hash-ref status-counts 'ok)
                (for/sum ([row (in-list rows)] #:when (equal? (hash-ref row 'status) "ok")) 1)))
