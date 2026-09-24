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
         "../ci/prepared-environment/compiled-root-manifest.rkt"
         (submod "../ci/prepared-environment/compiled-root.rkt" test-helpers))

;; Nested git commands must not inherit the caller's git plumbing. A git hook
;; (this repository's pre-commit hook, for instance) exports GIT_DIR/GIT_INDEX_FILE,
;; and `git -C <scratch fixture> init` then acts on the HOOK's repository: the
;; fixture's `git init` re-initialised the canonical checkout as bare once
;; (artifacts/wave-delivery-integrity/v1.00.31-w1/raw/hook-env-incident.txt), which
;; is why tests/test-compiled-root.rkt carries the same guard. It is applied to the
;; whole test rather than to the fixture alone because the producer shells out to
;; git as well (checkout cleanliness) and would inherit the same environment.
(define (git-plumbing-free-env)
  (define src (current-environment-variables))
  (define dst (make-environment-variables))
  (for ([name (in-list (environment-variables-names src))])
    (environment-variables-set! dst name (environment-variables-ref src name)))
  (for ([name (in-list '(#"GIT_DIR" #"GIT_INDEX_FILE"
                                    #"GIT_WORK_TREE"
                                    #"GIT_OBJECT_DIRECTORY"
                                    #"GIT_COMMON_DIR"
                                    #"GIT_PREFIX"
                                    #"GIT_ALTERNATE_OBJECT_DIRECTORIES"))])
    (environment-variables-set! dst name #f))
  dst)

(current-environment-variables (git-plumbing-free-env))

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
  ;; A deliberately NON-module .rkt input, exactly like this repository's test
  ;; fixtures do not compile it: `raco make` dies with "expected a `module`
  ;; declaration", which killed the first real whole-checkout run of this mode
  ;; (tests/metadata-discovery/fixture/tests/alpha-test.rkt). The producer must
  ;; recognise it, skip it, and say so.
  (call-with-output-file (build-path dir "marker" "not-a-module.rkt")
                         #:exists 'replace
                         (lambda (o) (display "(module+ test (void))\n" o)))
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
    (check-equal? (length (hash-ref m 'sources)) 2)
    ;; The non-module fixture is recognised as a non-compilation input and left
    ;; out instead of failing the lane (real-tree defect found by this wave's
    ;; first whole-checkout run; regression-guarded here).
    (check-true (regexp-match? #px"non-module .rkt input\\(s\\) skipped" transcript) transcript)
    (for ([entry (in-list (hash-ref m 'sources))])
      (check-false (string-contains? (hash-ref entry 'path) "not-a-module.rkt")
                   "a non-module fixture must not be published as a compiled module"))
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

;; ---------------------------------------------------------------------------
;; v1.00.30 W4 guarded compiled-root activation (issue #9690), repaired by
;; v1.00.31 W7.
;;
;; The blocked W4 wave carried these proofs and they are preserved verbatim
;; (path-adapated to the invocation the W1 contract pinned: the whole-checkout
;; root is published inside the already-contracted q-compiled/ prefix):
;; override precedence, fail-closed resolution, TOCTOU/current-load hygiene,
;; source byte counts, link/collection identity, and the lane's workflow-text
;; contract.

(define ci-yml (build-path repo-root ".github" "workflows" "ci.yml"))
(define prepare-action-yml action-yml)
(define restore-action-yml
  (build-path repo-root ".github" "actions" "restore-racket-environment" "action.yml"))
(define setup-racket-yml (build-path repo-root ".github" "actions" "setup-racket" "action.yml"))

(define (file-string path)
  (file->string path))

(define (string-index-of haystack needle)
  (let loop ([i 0])
    (cond
      [(> (+ i (string-length needle)) (string-length haystack)) -1]
      [(equal? (substring haystack i (+ i (string-length needle))) needle) i]
      [else (loop (add1 i))])))

;; ----------------------------------------------------- switch precedence

;; The activation switch is evaluated exactly as the ci.yml env expression
;; evaluates it: the global emergency switch RACKET_PREPARED_ARTIFACT=off
;; wins unconditionally; the dedicated lane switch RACKET_COMPILED_ROOT must
;; be explicitly 'on'; the producer must have succeeded in the same run; and
;; workflow_dispatch (version override) keeps the lane off. Windows/macOS/
;; cross-version lanes never opt in because the lane lives only on the ubuntu
;; fast shards.
(for ([row (in-list (list ; global off wins over every other combination
                     (list "off" "on" "success" "push" 'off)
                     (list "off" "on" "success" "workflow_dispatch" 'off)
                     (list "off" "off" "success" "push" 'off)
                     (list "off" "" "failure" "push" 'off)
                     ; lane switch off/absent keeps the lane off
                     (list "on" "off" "success" "push" 'off)
                     (list "on" "" "success" "push" 'off)
                     ; producer must have succeeded in THIS run
                     (list "on" "on" "failure" "push" 'off)
                     (list "on" "on" "skipped" "push" 'off)
                     (list "on" "on" "unknown" "push" 'off)
                     ; workflow_dispatch version override keeps it off
                     (list "on" "on" "success" "workflow_dispatch" 'off)
                     ; the one activation combination
                     (list "on" "on" "success" "push" 'auto)))]
      #:when #t)
  (define-values (global lane producer event expected) (apply values row))
  (check-equal?
   (compiled-root-activation-mode #:global-prepared-artifact global
                                  #:lane-switch lane
                                  #:producer-result producer
                                  #:event-name event)
   expected
   (format "precedence: global=~a lane=~a producer=~a event=~a" global lane producer event)))

;; ------------------------------------------------- TOCTOU guard keying

;; W3 review deferral: current-load may deliver an unresolved path while the
;; guard keyed only the symlink-resolved form (symlinked checkouts, macOS
;; /tmp). Both forms must be present in the key set.
(define (keys-of p)
  (compiled-root-load-keys p))

(check-equal? (length (keys-of "/some/checkout/main.rkt")) 2)
(check-true (boolean? (and (member "/some/checkout/main.rkt" (keys-of "/some/checkout/main.rkt")) #t))
            "unresolved form present")
(check-true (boolean? (and (member "/some/checkout/./nested/../main.rkt"
                                   (keys-of "/some/checkout/./nested/../main.rkt"))
                           #t))
            "unresolved simplified form present")

;; --------------------------------------------------- source byte-count

;; W3 review deferral: sources[].bytes was shape-validated only; the resolver
;; must compare the CONSUMER's on-disk byte count at resolve time (identity is
;; proven by digests; the byte count is the on-disk-vs-published coherence
;; belt).
(define tmp-parent (make-temporary-file "cr-bytes~a" 'directory))
(define tmp-src (build-path tmp-parent "mod.rkt"))
(displayln "(module mod racket/base)" (open-output-file tmp-src #:exists 'replace))
(check-true (consumer-source-bytes-match? tmp-src (hash 'bytes (file-size tmp-src))))
(check-false (consumer-source-bytes-match? tmp-src (hash 'bytes (add1 (file-size tmp-src)))))
(check-true (consumer-source-bytes-match? tmp-src (hash))
            "entries without a bytes field stay digest-governed")

;; ----------------------------------------------- link/collection identity

;; The cached addon store is not automatically trusted for changed q sources:
;; the consumer must verify the q package link points at THIS checkout before
;; the restored store is consulted.
(define ci-checkout-path (string-append "/home" "/ci" "/checkout"))
(check-true (package-link-points-at-checkout? (format "Package: q [installed]\n  source: ~a\n"
                                                      ci-checkout-path)
                                              ci-checkout-path))
(check-false (package-link-points-at-checkout?
              "Package: q [installed]\n  source: /some/other/checkout\n"
              ci-checkout-path))
(check-false (package-link-points-at-checkout? "" ci-checkout-path)
             "missing package entry is a mismatch, never a silent pass")
(define x-repo-path (string-append "/x" "/q-repo"))
(check-true (package-link-points-at-checkout? (format "Package: q\n  source: ~a\n" x-repo-path)
                                              (string-append x-repo-path "/"))
            "trailing-slash checkout normalizes")

;; ------------------------------------------- fail-closed fallback path

;; An unresolvable root (reason != #t) must select exactly ONE eager fallback
;; compile of the CURRENT source and must NOT install a current-load wrapper
;; (producer-era code never executes; no guard is active because no root is
;; active). The fallback compiles a tiny module in a disposable checkout.
(define fallback-checkout (make-temporary-file "cr-fallback~a" 'directory))
(define fallback-mod (build-path fallback-checkout "tiny.rkt"))
(with-output-to-file fallback-mod
                     (lambda () (displayln "#lang racket/base (provide answer) (define answer 42)")))
(define unresolved-root (compiled-root #f #f #f fallback-checkout "fixture-missing-manifest"))
(check-false (eq? (compiled-root-reason unresolved-root) #t))
(define load-before (current-load))
(with-compiled-root unresolved-root fallback-checkout (list "tiny.rkt") (lambda () (void)))
(check-eq? (current-load) load-before "no current-load wrapper survives a failed resolution")
(check-equal? (compiled-root-telemetry-mechanism (telemetry-snapshot)) "eager-fallback(raco-make)")
(check-equal? (compiled-root-telemetry-fallbacks (telemetry-snapshot))
              1
              "exactly ONE eager fallback compile")

;; ------------------------ current-load restoration, success path

;; W3 review deferral (W4): with-compiled-root installs the TOCTOU wrapper for
;; a RESOLVED root. The wrapper must be active during the thunk
;; (dynamic-extent hygiene) but must NOT survive the call — otherwise every
;; later load on this thread pays guard overhead.
(define ok-manifest (hasheq 'sources '()))
(define ok-root-dir (make-temporary-file "cr-ok-root~a" 'directory))
(define ok-map-dir (make-temporary-file "cr-ok-map~a" 'directory))
(define ok-checkout (make-temporary-file "cr-ok-co~a" 'directory))
(define ok-root (compiled-root ok-manifest ok-root-dir ok-map-dir ok-checkout #t))
(check-eq? (compiled-root-reason ok-root) #t)
(define load-before-ok (current-load))
(define wrapper-during-thunk? #f)
(with-compiled-root ok-root
                    ok-checkout
                    '()
                    (lambda ()
                      (set! wrapper-during-thunk? (not (eq? (current-load) load-before-ok)))))
(check-true wrapper-during-thunk? "TOCTOU guard is active during the thunk")
(check-eq? (current-load) load-before-ok "current-load is restored after with-compiled-root returns")

;; ----------------------------------------------- workflow text contracts

;; Lane exclusivity: only ci.yml may reference the compiled-root switch.
(define workflows-dir (build-path repo-root ".github" "workflows"))
(define workflow-files
  (for/list ([p (in-list (directory-list workflows-dir))]
             #:when (string-suffix? (path->string p) ".yml"))
    (build-path workflows-dir p)))
(for ([wf (in-list workflow-files)]
      #:unless (equal? wf ci-yml))
  (check-false (string-contains? (file->string wf) "RACKET_COMPILED_ROOT")
               (format "~a must not reference the compiled-root pilot switch" wf))
  (check-false (string-contains? (file->string wf) "compiled-root-fast")
               (format "~a must not consume the compiled-root artifact" wf)))

;; ci.yml: the lane expression encodes the full precedence in order (global
;; off wins BEFORE the lane switch is even consulted).
(define ci-text (file->string ci-yml))
(check-true (string-contains? ci-text "COMPILED_ROOT:")
            "test shards carry the compiled-root lane switch")
(check-true (< (string-index-of ci-text "vars.RACKET_PREPARED_ARTIFACT != 'off'")
               (string-index-of ci-text "vars.RACKET_COMPILED_ROOT == 'on'"))
            "global rollback switch is evaluated before the lane switch")
(check-true (string-contains? ci-text "vars.RACKET_COMPILED_ROOT == 'on'")
            "the dedicated lane switch must be explicitly on")
(check-true (string-contains? ci-text "needs.fast-env.result == 'success'")
            "a same-run successful producer is required (SAME-RUN SAME-HEAD)")
(check-true (string-contains? ci-text "--trusted-label q-trusted-producer")
            "consumers accept only the trusted producer namespace")
(check-true (string-contains? ci-text "steps.setup.outputs.prepared-env-root")
            "the verified root directory comes from the guarded restore")
(check-true (string-contains? ci-text "$RUNNER_TEMP/compiled-root-map")
            "the per-consumer mapping lives outside the checkout")
(check-true (string-contains? ci-text "--flags-file")
            "the shard launches through the verified resolution flags")
(check-true (string-contains? ci-text "steps.compiled-root.outcome")
            "activation failure selects the one eager compile gate, never lazy amplification")

;; Producer: the SAME prepared artifact carries the compiled root inside the
;; already-contracted q-compiled/ prefix (v1.00.31 W1 invocation contract).
(define prepare-text (file->string prepare-action-yml))
(check-true (string-contains? prepare-text "'q-compiled/'")
            "the trusted root joins the q-compiled/ allowlist")
(check-true (string-contains? prepare-text "q-compiled/trusted-root")
            "the producer publishes inside the contracted q-compiled/ prefix")
(check-true (string-contains? prepare-text "scripts/ci/compiled-root.rkt build")
            "the producer builds through the reviewed CLI")
(check-true (string-contains? prepare-text "q-trusted-producer")
            "the producer label is the trusted namespace")

;; Consumer restore: the verified root directory is exposed read-only; nothing
;; is materialized back INTO the checkout.
(define restore-text (file->string restore-action-yml))
(check-true (string-contains? restore-text "compiled-root-dir")
            "restore exports the verified compiled-root directory")
(check-false (string-contains? restore-text "cp -a \"$ROOT/compiled-root")
             "the compiled root is never copied into the checkout (read-only mount)")

;; setup-racket: purge invariant + fixture exclusions preserved, and the
;; verified root path is forwarded to the job.
(define setup-text (file->string setup-racket-yml))
(check-true (string-contains? setup-text "prepared-env-root")
            "setup-racket forwards the verified root directory")
(check-true (string-contains? setup-text "./tests/metadata-discovery/fixture/*")
            "the BUG-0065 purge keeps its frozen fixture exclusions")
(check-true
 (string-contains? setup-text
                   "prepared-env-root: ${{ steps.prepared-env.outputs.compiled-root-dir }}")
 "forwarding is bound to the restore action's output")

;; ---------------------------------------------------------------------------
;; v1.00.31 W7: the CLI `run` contract that the setup-racket pre-resolution step
;; declares. Independent review round 2 (finding 5) required committed coverage
;; for BOTH shapes: resolution-only (no --module) and module-bearing launch.

(module+ test
  (test-case "compiled-root run resolves and maps without a module (declared setup-racket step)"
    (define co (make-checkout! "run-no-module"))
    (define root (build-path tmp-base "run-no-module-root"))
    (define map (build-path tmp-base "run-no-module-map"))
    (define-values (bcode bout)
      (run-producer "build"
                    "--checkout"
                    (path->string co)
                    "--module"
                    "marker/compiled-root-marker.rkt"
                    "--final-dir"
                    (path->string root)
                    "--label"
                    "q-trusted-producer"))
    (check-equal? bcode 0 bout)
    (define-values (code transcript)
      (run-producer "run"
                    "--checkout"
                    (path->string co)
                    "--final-dir"
                    (path->string root)
                    "--map-dir"
                    (path->string map)
                    "--trusted-label"
                    "q-trusted-producer"))
    (check-equal? code 0 transcript)
    (check-true (regexp-match? #rx"verified root hit" transcript) transcript)
    (check-true (regexp-match? #rx"no --module to launch" transcript) transcript))

  (test-case "compiled-root run still launches a module through the verified root"
    (define co (make-checkout! "run-with-module"))
    (define root (build-path tmp-base "run-with-module-root"))
    (define map (build-path tmp-base "run-with-module-map"))
    (define-values (bcode bout)
      (run-producer "build"
                    "--checkout"
                    (path->string co)
                    "--module"
                    "marker/compiled-root-marker.rkt"
                    "--final-dir"
                    (path->string root)
                    "--label"
                    "q-trusted-producer"))
    (check-equal? bcode 0 bout)
    (define-values (code transcript)
      (run-producer "run"
                    "--checkout"
                    (path->string co)
                    "--final-dir"
                    (path->string root)
                    "--map-dir"
                    (path->string map)
                    "--trusted-label"
                    "q-trusted-producer"
                    "--module"
                    "marker/compiled-root-marker.rkt"))
    (check-equal? code 0 transcript)
    (check-true (regexp-match? #rx"verified root hit" transcript) transcript)))
