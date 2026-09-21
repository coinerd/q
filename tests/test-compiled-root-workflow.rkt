#lang racket/base

;; @speed slow
;; @suite workflows
;; @timeout 600

;; tests/test-compiled-root-workflow.rkt — v1.00.31 W1
;;
;; Workflow/action ↔ script invocation contract for the compiled-root
;; producer (register F1).
;;
;; The v1.00.30 W4 lane declared
;;   racket scripts/ci/compiled-root.rkt build --out … --checkout … --trusted-label …
;; while the CLI accepted no `--out` at all: it died with
;; `compiled-root: unknown switch: --out` (exit 1), the producer job failed
;; and the lane could never activate — yet `tests/test-compiled-root-workflow`
;; style substring assertions stayed green because they only looked for the
;; *string* of the invocation.
;;
;; This test closes that hole by EXTRACTING the declaration from the action
;; file and EXECUTING it verbatim against a scratch checkout. Nothing here
;; restates the flags by hand, so the declaration and the execution cannot
;; drift apart: change the action line and the executed command changes with
;; it. It also asserts the two failure shapes a substring check cannot see —
;; an unknown switch and a missing argument — so the red-first evidence that
;; the pre-fix CLI produced is retained as an executable expectation.

(require racket/file
         racket/format
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         rackunit
         "../scripts/ci/invocation-contract.rkt"
         "../ci/prepared-environment/compiled-root.rkt"
         "../ci/prepared-environment/compiled-root-manifest.rkt")

(define-runtime-path action-yml "../.github/actions/prepare-racket-environment/action.yml")
;; Repo root derived from this file's own location rather than from the
;; process cwd: `raco test` does not guarantee the cwd is the repo root, and
;; a contract test that silently checked the wrong directory would be worse
;; than no test.
(define repo-root (simplify-path (build-path (path-only action-yml) ".." ".." "..")))
(define producer-cli (build-path repo-root "scripts" "ci" "compiled-root.rkt"))

(define (run-producer #:cwd [cwd repo-root] . args)
  ;; Capture stdout+stderr together: an unknown switch is reported on
  ;; stderr and must be visible in the assertion, not swallowed.
  (define out (open-output-string))
  (define code
    (parameterize ([current-output-port out]
                   [current-error-port out]
                   [current-directory cwd])
      (apply system*/exit-code (find-executable-path "racket") (path->string producer-cli) args)))
  (values code (get-output-string out)))

;; ------------------------------------------------------------------ fixture

(define tmp-base (make-temporary-file "qcw-contract~a" 'directory))

(define (make-checkout! name)
  (define dir (build-path tmp-base name))
  (define module-dir (build-path dir "marker"))
  (make-directory* module-dir)
  (define (write-module! fname body)
    (call-with-output-file (build-path module-dir fname)
                           #:exists 'replace
                           (lambda (o) (display body o))))
  ;; Two modules, one requiring the other, so "derive the module list from
  ;; the checkout" is exercised rather than trivially satisfied.
  (write-module! "second.rkt" "#lang racket/base\n(provide second-value)\n(define second-value 41)\n")
  (write-module! "compiled-root-marker.rkt"
                 (string-append "#lang racket/base\n"
                                "(require \"second.rkt\")\n"
                                "(provide marker-result)\n"
                                "(define marker-result (add1 second-value))\n"))
  (define (git . args)
    (unless (zero?
             (apply system*/exit-code (find-executable-path "git") "-C" (path->string dir) args))
      (error 'fixture "git ~a failed" args)))
  (git "init" "-q")
  (git "config" "user.email" "ci@example.invalid")
  (git "config" "user.name" "CI Fixture")
  (git "add" ".")
  (git "commit" "-q" "-m" "fixture")
  dir)

(module+ test
  ;; ------------------------------------------------- the declared invocation

  (define declaration
    (for/first ([i (in-list (extract-declared-invocations action-yml))]
                #:when (equal? (hash-ref i 'target) "scripts/ci/compiled-root.rkt"))
      i))

  (test-case "the action declares exactly one compiled-root producer invocation"
    (check-true (hash? declaration))
    (check-equal? (length (for/list ([i (in-list (extract-declared-invocations action-yml))]
                                     #:when (equal? (hash-ref i 'target)
                                                    "scripts/ci/compiled-root.rkt"))
                            i))
                  1)
    (check-equal? (hash-ref declaration 'tool) "racket")
    (check-equal? (hash-ref declaration 'kind) "script"))

  (test-case "the declared flags are the CLI's own option set (this is what F1 broke)"
    ;; Red-first retention: before this wave the CLI's option set had no
    ;; `--out`, so this assertion is exactly the one F1 failed. It is a
    ;; statement about the SCRIPT, not about the action file.
    (define-values (switches declared?) (script-option-set producer-cli))
    (check-true declared?)
    (check-true (and (member "--out" switches) #t))
    (check-true (and (member "--checkout" switches) #t))
    (check-true (and (member "--trusted-label" switches) #t))
    (for ([f (in-list (hash-ref declaration 'flags))])
      (check-true (and (member f switches) #t) (format "declared flag ~a is not a CLI option" f))))

  (test-case "the invocation contract resolves the producer declaration"
    ;; Resolve against the repo root explicitly: the process cwd under
    ;; `raco test` is not guaranteed to be the repo root, and resolving
    ;; against the wrong directory reports `missing-target` for a declaration
    ;; that is actually fine.
    (define result (check-invocation declaration repo-root))
    (check-equal? (hash-ref result 'status) "ok")
    ;; The declaration must be verified against the target's own option set
    ;; (the strong tier), not merely against flag-looking literals: the weak
    ;; tier is what let F1's `--out` through unnoticed.
    (check-equal? (hash-ref result 'tier) "option-set"))

  ;; ---------------------------------------------- executing the declaration

  (define checkout (make-checkout! "producer-checkout"))
  (define stage (build-path tmp-base "stage"))
  (define published (build-path stage "q-compiled" "trusted-root"))

  (define (substitute token)
    ;; Only the two variables the declaration carries are bound here: the
    ;; command itself stays exactly as declared.
    (cond
      [(string=? token "$PWD") (path->string checkout)]
      [(string-prefix? token "$stage")
       (string-append (path->string stage) (substring token (string-length "$stage")))]
      [else token]))

  (test-case "the exact declared producer invocation executes and publishes a trusted root"
    ;; argv[0] is the declared target. The CLI this test runs IS that
    ;; script, so executing the tail honours the declaration exactly: the
    ;; flags below come from the action file, not from this test.
    (define argv (map substitute (hash-ref declaration 'argv)))
    (check-equal? (first argv) "scripts/ci/compiled-root.rkt")
    (define-values (code transcript) (apply run-producer (drop argv 1)))
    (check-equal? code 0 transcript)
    (check-true (file-exists? (build-path published "manifest.rktd"))
                (format "no published manifest at ~a" published))
    (define m (load-compiled-root-manifest published))
    (check-equal? (hash-ref m 'schema) manifest-schema)
    (check-equal? (hash-ref (hash-ref m 'producer) 'label) "q-trusted-producer")
    (check-true (hash-ref (hash-ref m 'producer) 'trusted))
    ;; The whole checkout was derived, not a hand-written module list.
    (check-true (>= (length (hash-ref m 'sources)) 2))
    ;; `#px`, not `#rx`: Racket's byte regexps silently ignore counted
    ;; repetitions, so `#rx"^[0-9a-f]{64}$"` matches nothing and this
    ;; assertion would have passed vacuously (it did, in the first run).
    (check-true (regexp-match? #px"^[0-9a-f]{64}$" (hash-ref (hash-ref m 'payload) 'digest))
                (format "payload digest: ~s" (hash-ref (hash-ref m 'payload) 'digest))))

  (test-case "the published root is read-only (immutability preserved)"
    (check-true (directory-exists? published))
    (for ([f (in-list (directory-list published))])
      (check-false (member 'write (file-or-directory-permissions (build-path published f))))
      (void)))

  (test-case "the staging tree is not left behind in the published artifact"
    ;; Publication is an atomic rename of a staging directory created next
    ;; to the root; a failed build must not leave it in the artifact.
    (define leftovers
      (for/list ([f (in-list (directory-list (build-path stage "q-compiled")))]
                 #:when (regexp-match? #rx"^compiled-root-stage" (path->string f)))
        f))
    (check-equal? leftovers '()))

  (test-case "the wave's own tooling contains no silently-inert matchers"
    ;; Both footguns below were found in this wave's own first draft, and
    ;; both failed *silently*: `file->list` raises on a `#lang` line (the
    ;; handler swallowed it, so the extractor honestly reported "no
    ;; declarations" while the strong verification tier verified nothing),
    ;; and `#rx` byte regexps ignore counted repetitions (so
    ;; `#rx"^[0-9a-f]{64}$"` never matches, making the digest assertion
    ;; vacuous). A contract tool that quietly checks nothing is worse than no
    ;; tool, so both shapes are forbidden mechanically rather than by memory.
    (for ([rel (in-list '("scripts/ci/invocation-contract.rkt" "scripts/ci/compiled-root.rkt"))])
      (define text (file->string (build-path repo-root rel)))
      (check-false (regexp-match? #px"#rx\"[^\"]*\\{[0-9]" text)
                   (format "~a: counted repetition inside a byte regexp (#rx)" rel))
      (check-false (regexp-match? #px"[( ]file->list[ )]" text)
                   (format "~a: reads forms with file->list (raises on #lang)" rel))))

  ;; ---------------------------------------------------- fail-closed shapes

  (test-case "an unknown switch fails (the pre-fix F1 shape, retained)"
    ;; `--outdir` is not a CLI option: this is the same failure class the
    ;; pre-fix CLI produced for `--out`, and the reason the substring
    ;; assertion could not see it.
    (define-values (code transcript)
      (run-producer "build"
                    "--outdir"
                    (path->string published)
                    "--checkout"
                    (path->string checkout)
                    "--trusted-label"
                    "q-trusted-producer"))
    (check-not-equal? code 0)
    (check-true (regexp-match? #rx"unknown switch" transcript) transcript))

  (test-case "a missing required argument fails"
    (define-values (code transcript)
      (run-producer "build" "--out" (path->string (build-path tmp-base "nowhere"))))
    (check-not-equal? code 0)
    (check-true (regexp-match? #rx"missing required option --checkout" transcript) transcript))

  (test-case "conflicting modes fail closed rather than guessing"
    (define-values (code transcript)
      (run-producer "build"
                    "--out"
                    (path->string (build-path tmp-base "conflict"))
                    "--checkout"
                    (path->string checkout)
                    "--module"
                    "marker/second.rkt"))
    (check-not-equal? code 0)
    (check-true (regexp-match? #rx"cannot be combined with --out" transcript) transcript))

  (test-case "the per-module contract still works unchanged"
    ;; The whole-checkout mode must not replace the documented per-module
    ;; build: existing callers keep working.
    (define single-stage (build-path tmp-base "per-module-stage"))
    (define single-final (build-path tmp-base "per-module-root"))
    (define-values (code transcript)
      (run-producer "build"
                    "--checkout"
                    (path->string checkout)
                    "--module"
                    "marker/second.rkt"
                    "--final-dir"
                    (path->string single-final)
                    "--staging-dir"
                    (path->string single-stage)
                    "--label"
                    "per-module-producer"))
    (check-equal? code 0 transcript)
    (define m (load-compiled-root-manifest single-final))
    (check-equal? (hash-ref (hash-ref m 'producer) 'label) "per-module-producer")))
