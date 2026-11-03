#lang racket/base

;; tests/test-compiled-root.rkt — v1.00.30 W3
;;
;; End-to-end trusted compiled-root matrix over REAL bytecode:
;; producer build (raco make, clean-checkout gate) -> atomic read-only
;; publication -> consumer resolution at a DIFFERENT absolute checkout
;; location (relocation is never assumed, only via per-consumer map
;; dirs) -> in-process activation with root-hit proof -> subprocess
;; inheritance via compiled-root-launch-arguments -> fail-closed
;; fallback on marker change / missing root -> TOCTOU mutation of the
;; published payload fails closed at load time.
;;
;; Runner metadata: heavy by design (raco make producer builds, git
;; subprocesses, ~30 test cases over real bytecode). Hard-capped by
;; @timeout; the pure/fast validation matrix lives in
;; tests/test-compiled-root-manifest.rkt.

;; @speed slow
;; @suite fast
;; @timeout 600

(require racket/cmdline
         racket/file
         racket/format
         racket/match
         racket/path
         racket/port
         racket/string
         racket/system
         rackunit
         "../ci/prepared-environment/compiled-root.rkt"
         "../ci/prepared-environment/compiled-root-manifest.rkt"
         "../ci/prepared-environment/transfer-auth.rkt"
         racket/runtime-path)

(module+ test
  (define tmp-base (make-temporary-file "qcr-e2e~a" 'directory))

  ;; Nested git commands must not inherit the caller's git plumbing. A git hook
  ;; (the repo's pre-commit hook, for instance) exports GIT_DIR/GIT_INDEX_FILE, so
  ;; a nested `git -C <scratch fixture> init/add` acts on the hook's repository and
  ;; dies with "must be run in a work tree". The fixture gets its own sanitized
  ;; copy of the environment rather than mutating the global one.
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

  ;; Apply the sanitized environment to the whole test, not just to the fixtures:
  ;; the engine under test also shells out to git (checkout cleanliness checks) and
  ;; inherits whatever environment this process was started with. Measured: with
  ;; GIT_DIR leaked, the fixtures passed but the CLI's git call failed, so a
  ;; fixture-only fix was not enough.
  (current-environment-variables (git-plumbing-free-env))

  ;; ---- fixture: a tiny git checkout containing one marker module ----
  (define (make-marker-source marker-symbol)
    (format
     "(module compiled-root-marker racket/base\n  (provide marker-result)\n  (define marker-result '~a))\n"
     marker-symbol))

  (define (make-checkout! name marker-symbol)
    (define dir (build-path tmp-base name))
    (define module-dir (build-path dir "marker"))
    (make-directory* module-dir)
    (with-output-to-file (build-path module-dir "compiled-root-marker.rkt")
                         #:exists 'replace
                         (lambda () (display (make-marker-source marker-symbol))))
    (define (git . args)
      (define code
        (parameterize ([current-environment-variables (git-plumbing-free-env)])
          (apply system*/exit-code (find-executable-path "git") "-C" (path->string dir) args)))
      (unless (zero? code)
        (error 'fixture "git ~a failed" args)))
    (git "init" "-q")
    (git "config" "user.email" "ci@example.invalid")
    (git "config" "user.name" "CI Fixture")
    (git "add" ".")
    (git "commit" "-q" "-m" "fixture")
    dir)

  (define (marker-abs-path checkout)
    (simplify-path (build-path checkout "marker" "compiled-root-marker.rkt")))

  ;; Producer checkout (committed, clean).
  (define checkout-a (make-checkout! "producer-checkout" 'producer-fixture))
  (define staging (build-path tmp-base "staging-root"))
  (define final-dir (build-path tmp-base "published-root"))
  ;; manifest produced in the first test case, reused by the publication case
  (define staged-manifest #f)

  ;; ------------------------------------------------- producer + publish

  (test-case "producer builds manifest from clean checkout"
    (define m
      (build-compiled-root! #:checkout checkout-a
                            #:modules (list "marker/compiled-root-marker.rkt")
                            #:staging-dir staging
                            #:producer-label "q-trusted-producer"))
    (set! staged-manifest m)
    (check-equal? (hash-ref m 'schema) "q-compiled-root-manifest-1")
    (check-true (pair? (hash-ref m 'sources)))
    (check-true (for/and ([e (in-list (hash-ref m 'sources))])
                  (file-exists? (build-path staging (hash-ref e 'zo))))))

  (test-case "dirty/untracked compilation input rejected at producer"
    (define checkout-dirty (make-checkout! "dirty-checkout" 'dirty-fixture))
    (with-output-to-file (build-path checkout-dirty "marker" "untracked-extra.rkt")
                         #:exists 'replace
                         (lambda () (display "(module untracked-extra racket/base)\n")))
    (check-exn #rx"dirty/untracked"
               (lambda ()
                 (build-compiled-root! #:checkout checkout-dirty
                                       #:modules (list "marker/compiled-root-marker.rkt")
                                       #:staging-dir (build-path tmp-base "staging-dirty")
                                       #:producer-label "q-trusted-producer"))))

  (test-case "publication is atomic and published root is read-only"
    (check-equal? (publish-compiled-root! staging final-dir staged-manifest) final-dir)
    ;; manifest landed in the published root
    (check-true (file-exists? (build-path final-dir "manifest.rktd")))
    ;; write revoked everywhere in the payload
    (check-true (for/and ([p (in-list (cons final-dir (find-files (lambda (_) #t) final-dir)))])
                  (not (member 'write (file-or-directory-permissions p)))))
    ;; second publication to the same final path fails closed
    (define staging2 (build-path tmp-base "staging-root-2"))
    (define manifest2
      (build-compiled-root! #:checkout checkout-a
                            #:modules (list "marker/compiled-root-marker.rkt")
                            #:staging-dir staging2
                            #:producer-label "q-trusted-producer"))
    (check-exn #rx"already published"
               (lambda () (publish-compiled-root! staging2 final-dir manifest2))))

  ;; -------------------------------------------- relocation (hard gate)

  ;; Consumer checkout B is at a DIFFERENT absolute path (same bytes as
  ;; the producer checkout). The root must be usable there only through
  ;; the per-consumer map dir; the consumer tree is never written to.
  (define checkout-b (make-checkout! "consumer-checkout-b" 'producer-fixture))
  (define map-dir-b (build-path tmp-base "map-b"))
  (define root-b
    (resolve-compiled-root! #:final-dir final-dir
                            #:checkout checkout-b
                            #:trusted-producer-labels '("q-trusted-producer")
                            #:map-dir map-dir-b))

  (test-case "root resolves for a relocated consumer checkout"
    (check-true (eq? (compiled-root-reason root-b) #t) (reason->string (compiled-root-reason root-b)))
    (check-equal? (compiled-root-lookup-directory root-b) map-dir-b))

  (test-case "in-process load is served by the root, not lazily compiled"
    (reset-telemetry!)
    (define hits-before (compiled-root-telemetry-root-hits (telemetry-snapshot)))
    (with-compiled-root
     root-b
     checkout-a
     (list "marker/compiled-root-marker.rkt")
     (lambda ()
       ;; absolute compiled-file roots map the source
       ;; relative to current-load-relative-directory;
       ;; dynamic-require does not set it, so pin it to
       ;; the consumer checkout prefix
       (parameterize ([current-load-relative-directory (path->directory-path checkout-b)])
         (check-equal? (dynamic-require (marker-abs-path checkout-b) 'marker-result)
                       'producer-fixture))))
    (check-true (> (compiled-root-telemetry-root-hits (telemetry-snapshot)) hits-before)
                "TOCTOU guard observed no root-served load")
    ;; consumer source was never (re)compiled beside itself
    (check-false (directory-exists? (build-path checkout-b "marker" "compiled"))))

  (test-case "subprocess inherits the root via launch arguments"
    (define child-path (build-path tmp-base "child-load-marker.rkt"))
    (define child-out (build-path tmp-base "child-result.txt"))
    (with-output-to-file
     child-path
     #:exists 'replace
     (lambda ()
       (printf "(define v (dynamic-require (string->path ~s) 'marker-result))\n"
               (path->string (marker-abs-path checkout-b)))
       (printf "(call-with-output-file ~s\n" (path->string child-out))
       (printf
        "  (lambda (o) (fprintf o \"marker=~~a roots=~~a\\n\" v (if (null? (current-compiled-file-roots)) \"none\" \"set\"))))\n")))
    (define exit-code
      (apply system*/exit-code
             (find-executable-path "racket")
             (append (compiled-root-launch-arguments root-b) (list "-f" (path->string child-path)))))
    (check-equal? exit-code 0)
    (define out (file->string child-out))
    (check-true (string-contains? out "marker=producer-fixture") out)
    (check-true (string-contains? out "roots=set") out)
    ;; child used the root: no compiled dir appeared in the consumer tree
    (check-false (directory-exists? (build-path checkout-b "marker" "compiled"))))

  ;; ------------------ fresh-clone mtime skew (W1a-repair trial pin)

  ;; W1a-repair (coordinator trial c01 window-2, run 41271542715): a
  ;; fresh shard clone's source mtimes are strictly newer than any
  ;; producer-era root zo. Root-on activation must still serve the root
  ;; on BOTH paths — the in-process parameters and the subprocess -e
  ;; bootstrap — because freshness is established by the same-commit
  ;; source-sha gate plus per-load content digests, never by mtime. A
  ;; lazily recompiled source-adjacent compiled/ dir is the observable
  ;; zero-root-hits failure signature this pin guards against.
  (test-case "fresh-clone consumer (sources newer than root zos) still gets root hits (W1a-repair)"
    (define (stamp-sources-newer! root)
      ;; make every manifest source in the consumer checkout strictly
      ;; newer than its published root zo (what a fresh clone looks like)
      (for ([e (in-list (hash-ref (compiled-root-manifest root) 'sources))])
        (define src (build-path (compiled-root-checkout root) (hash-ref e 'path)))
        (define zo-mtime
          (file-or-directory-modify-seconds (build-path (compiled-root-root-dir root)
                                                        (hash-ref e 'zo))))
        (file-or-directory-modify-seconds src (+ zo-mtime 60))))
    (define (assert-no-lazy-compile! checkout where)
      (check-false (directory-exists? (build-path checkout "marker" "compiled"))
                   (string-append where
                                  ": root zo was judged stale and the "
                                  "consumer tree was lazily recompiled")))

    ;; --- in-process activation path (with-compiled-root parameters)
    (define checkout-s1 (make-checkout! "consumer-fresh-clone-inproc" 'producer-fixture))
    (define map-dir-s1 (build-path tmp-base "map-s1"))
    (define root-s1
      (resolve-compiled-root! #:final-dir final-dir
                              #:checkout checkout-s1
                              #:trusted-producer-labels '("q-trusted-producer")
                              #:map-dir map-dir-s1))
    (check-true (eq? (compiled-root-reason root-s1) #t)
                (reason->string (compiled-root-reason root-s1)))
    (stamp-sources-newer! root-s1)
    (define s1-zo
      (build-path final-dir
                  (hash-ref (car (hash-ref (compiled-root-manifest root-s1) 'sources)) 'zo)))
    (check-true (> (file-or-directory-modify-seconds (marker-abs-path checkout-s1))
                   (file-or-directory-modify-seconds s1-zo))
                "fixture precondition: consumer source strictly newer than root zo")
    (reset-telemetry!)
    (define hits-before-s1 (compiled-root-telemetry-root-hits (telemetry-snapshot)))
    (with-compiled-root
     root-s1
     checkout-s1
     (list "marker/compiled-root-marker.rkt")
     (lambda ()
       (parameterize ([current-load-relative-directory (path->directory-path checkout-s1)])
         (check-equal? (dynamic-require (marker-abs-path checkout-s1) 'marker-result)
                       'producer-fixture))))
    (define tel-s1 (telemetry-snapshot))
    (check-true (> (compiled-root-telemetry-root-hits tel-s1) hits-before-s1)
                "in-process: TOCTOU guard observed no root-served load")
    (check-true (> (compiled-root-telemetry-loads tel-s1) 0) "in-process: no module load observed")
    (assert-no-lazy-compile! checkout-s1 "in-process")

    ;; --- subprocess activation path (bootstrap -e preamble)
    (define checkout-s2 (make-checkout! "consumer-fresh-clone-child" 'producer-fixture))
    (define map-dir-s2 (build-path tmp-base "map-s2"))
    (define root-s2
      (resolve-compiled-root! #:final-dir final-dir
                              #:checkout checkout-s2
                              #:trusted-producer-labels '("q-trusted-producer")
                              #:map-dir map-dir-s2))
    (check-true (eq? (compiled-root-reason root-s2) #t)
                (reason->string (compiled-root-reason root-s2)))
    (stamp-sources-newer! root-s2)
    (define child-path-s2 (build-path tmp-base "child-fresh-clone.rkt"))
    (define child-out-s2 (build-path tmp-base "child-fresh-clone-result.txt"))
    (with-output-to-file
     child-path-s2
     #:exists 'replace
     (lambda ()
       (printf "(define v (dynamic-require (string->path ~s) 'marker-result))\n"
               (path->string (marker-abs-path checkout-s2)))
       (printf "(call-with-output-file ~s\n" (path->string child-out-s2))
       (printf (string-append "  (lambda (o) (fprintf o \"marker=~~a roots=~~a\\n\" v\n"
                              "   (if (null? (current-compiled-file-roots))"
                              " \"none\" \"set\"))))\n"))))
    (define exit-code-s2
      (apply system*/exit-code
             (find-executable-path "racket")
             (append (compiled-root-launch-arguments root-s2)
                     (list "-f" (path->string child-path-s2)))))
    (check-equal? exit-code-s2 0)
    (define out-s2 (file->string child-out-s2))
    (check-true (string-contains? out-s2 "marker=producer-fixture") out-s2)
    (check-true (string-contains? out-s2 "roots=set") out-s2)
    (assert-no-lazy-compile! checkout-s2 "subprocess"))

  ;; -------------------------------------------------- fail-closed paths

  (test-case "marker change at consumer fails validation, fallback runs current source"
    (define checkout-f (make-checkout! "consumer-changed" 'consumer-changed))
    (define map-dir-f (build-path tmp-base "map-f"))
    (define root-f
      (resolve-compiled-root! #:final-dir final-dir
                              #:checkout checkout-f
                              #:trusted-producer-labels '("q-trusted-producer")
                              #:map-dir map-dir-f))
    (check-false (eq? (compiled-root-reason root-f) #t))
    (check-true (string-contains? (reason->string (compiled-root-reason root-f)) "digest mismatch"))
    (reset-telemetry!)
    (with-compiled-root root-f
                        checkout-f
                        (list "marker/compiled-root-marker.rkt")
                        (lambda ()
                          ;; eager fallback: the CURRENT consumer source ran, never
                          ;; producer-era bytecode carrying the stale marker value
                          (check-equal? (dynamic-require (marker-abs-path checkout-f) 'marker-result)
                                        'consumer-changed)))
    (check-true (>= (compiled-root-telemetry-fallbacks (telemetry-snapshot)) 1)))

  (test-case "missing published root fails closed, eager fallback still serves"
    (define checkout-e (make-checkout! "consumer-fallback" 'fallback-fixture))
    (define map-dir-e (build-path tmp-base "map-e"))
    (define root-e
      (resolve-compiled-root! #:final-dir (build-path tmp-base "no-such-root")
                              #:checkout checkout-e
                              #:trusted-producer-labels '("q-trusted-producer")
                              #:map-dir map-dir-e))
    (check-false (eq? (compiled-root-reason root-e) #t))
    (check-true (string-contains? (reason->string (compiled-root-reason root-e)) "manifest missing"))
    (with-compiled-root root-e
                        checkout-e
                        (list "marker/compiled-root-marker.rkt")
                        (lambda ()
                          (check-equal? (dynamic-require (marker-abs-path checkout-e) 'marker-result)
                                        'fallback-fixture))))

  (test-case "payload mutation after verification fails closed (TOCTOU)"
    (define checkout-c (make-checkout! "consumer-toctou" 'producer-fixture))
    (define map-dir-c (build-path tmp-base "map-c"))
    (define root-c
      (resolve-compiled-root! #:final-dir final-dir
                              #:checkout checkout-c
                              #:trusted-producer-labels '("q-trusted-producer")
                              #:map-dir map-dir-c))
    (check-true (eq? (compiled-root-reason root-c) #t))
    ;; make the published zo writable again and flip a byte AFTER the
    ;; resolve-time verification — the load-time guard must catch it
    (define zo-rel
      (for/first ([e (in-list (hash-ref (compiled-root-manifest root-c) 'sources))])
        (hash-ref e 'zo)))
    (define zo-abs (build-path final-dir zo-rel))
    (unless (zero? (system*/exit-code (find-executable-path "chmod") "u+w" zo-abs))
      (error 'toctou-test "could not chmod +w the published zo"))
    (define orig-bytes (file->bytes zo-abs))
    (with-output-to-file zo-abs
                         #:exists 'truncate/replace
                         (lambda ()
                           (define b (bytes-copy orig-bytes))
                           (bytes-set! b 0 (bitwise-xor (bytes-ref b 0) #xFF))
                           (write-bytes b)))
    (check-exn #rx"payload mutated after verification"
               (lambda ()
                 (with-compiled-root root-c
                                     checkout-c
                                     (list "marker/compiled-root-marker.rkt")
                                     (lambda ()
                                       (parameterize ([current-load-relative-directory
                                                       (path->directory-path checkout-c)])
                                         (dynamic-require (marker-abs-path checkout-c)
                                                          'marker-result))))))
    ;; restore for any later reader
    (with-output-to-file zo-abs #:exists 'truncate/replace (lambda () (write-bytes orig-bytes))))

  (test-case "manifest mutation after resolve fails the child launch closed (W1a-repair)"
    ;; W1a-repair: the child bootstrap no longer receives embedded
    ;; per-source digests (that transfer went E2BIG, trial c02); it
    ;; re-derives the table from the published manifest and verifies the
    ;; launch-time baked manifest digest instead. A manifest mutated
    ;; between resolve and launch must therefore kill the child at
    ;; startup — the same fail-closed canary the old embedded digests
    ;; provided. Whitespace-only mutation: the table itself would parse
    ;; identically, so only the digest canary can catch this.
    (define checkout-m (make-checkout! "consumer-manifest-canary" 'producer-fixture))
    (define map-dir-m (build-path tmp-base "map-m"))
    (define root-m
      (resolve-compiled-root! #:final-dir final-dir
                              #:checkout checkout-m
                              #:trusted-producer-labels '("q-trusted-producer")
                              #:map-dir map-dir-m))
    (check-true (eq? (compiled-root-reason root-m) #t))
    (define child-path-m (build-path tmp-base "child-manifest-canary.rkt"))
    (with-output-to-file child-path-m #:exists 'replace (lambda () (display "(void)\n")))
    (define launch-args-m (compiled-root-launch-arguments root-m))
    (define manifest-path-m (build-path final-dir "manifest.rktd"))
    (unless (zero? (system*/exit-code (find-executable-path "chmod") "u+w" manifest-path-m))
      (error 'canary-test "could not chmod +w the published manifest"))
    (define manifest-bytes-m (file->bytes manifest-path-m))
    (with-output-to-file manifest-path-m
                         #:exists 'truncate/replace
                         (lambda () (write-bytes (bytes-append manifest-bytes-m #" "))))
    (define exit-code-m
      (apply system*/exit-code
             (find-executable-path "racket")
             (append launch-args-m (list "-f" (path->string child-path-m)))))
    (check-false (zero? exit-code-m)
                 "child must fail closed when the manifest mutated after verification")
    ;; restore for any later reader
    (with-output-to-file manifest-path-m
                         #:exists 'truncate/replace
                         (lambda () (write-bytes manifest-bytes-m)))
    (system*/exit-code (find-executable-path "chmod") "a-w" manifest-path-m))

  ;; ----------- B1/B2 regression: producer CLI + shared compiled dirs

  ;; Fixture builder for checkouts whose module closure places several
  ;; zos in ONE compiled/ directory (the normal multi-module case).
  (define (make-lang-checkout! name files)
    (define dir (build-path tmp-base name))
    (for ([f (in-list files)])
      (define p (build-path dir (car f)))
      (make-directory* (path-only p))
      (with-output-to-file p #:exists 'replace (lambda () (display (cdr f)))))
    (define (git . args)
      (define code
        (parameterize ([current-environment-variables (git-plumbing-free-env)])
          (apply system*/exit-code (find-executable-path "git") "-C" (path->string dir) args)))
      (unless (zero? code)
        (error 'fixture "git ~a failed" args)))
    (git "init" "-q")
    (git "config" "user.email" "ci@example.invalid")
    (git "config" "user.name" "CI Fixture")
    (git "add" ".")
    (git "commit" "-q" "-m" "fixture")
    dir)

  (define checkout-multi
    (make-lang-checkout!
     "multi-checkout"
     (list (cons "multi/multi-b.rkt" "#lang racket/base\n(provide b)\n(define b 42)\n")
           (cons "multi/multi-a.rkt"
                 "#lang racket/base\n(provide a)\n(require \"multi-b.rkt\")\n(define a b)\n"))))

  ;; B2: the producer restores the checkout by deleting every compiled/
  ;; directory the build created. A per-zo delete loop removes a shared
  ;; directory repeatedly and raises exn:fail:filesystem, aborting the
  ;; producer for any module with an in-checkout dependency closure.
  (test-case "producer restores a shared compiled dir once for a multi-zo closure (B2)"
    (define m
      (build-compiled-root! #:checkout checkout-multi
                            #:modules (list "multi/multi-a.rkt")
                            #:staging-dir (build-path tmp-base "staging-multi")
                            #:producer-label "q-trusted-producer"))
    (check-equal? (hash-ref m 'schema) "q-compiled-root-manifest-1")
    (check-true (>= (length (hash-ref m 'sources)) 2)
                "module + in-checkout dependency closure must both be harvested")
    (check-false (directory-exists? (build-path checkout-multi "multi" "compiled"))
                 "producer must restore the checkout to its pre-build compiled state"))

  ;; B1: the documented producer entry point is the CLI 'build'
  ;; subcommand. It must publish a usable root end to end, not merely
  ;; exercise the in-process build helper.
  (define-runtime-path cli-script-path "../scripts/ci/compiled-root.rkt")

  (test-case "CLI 'build' subcommand publishes a compiled root (B1)"
    (define checkout-cli (make-checkout! "cli-checkout" 'cli-fixture))
    (define cli-final (build-path tmp-base "cli-published-root"))
    (define cli-staging (build-path tmp-base "cli-staging-root"))
    (define exit-code
      (apply system*/exit-code
             (find-executable-path "racket")
             (list (path->string cli-script-path)
                   "build"
                   "--checkout"
                   (path->string checkout-cli)
                   "--module"
                   "marker/compiled-root-marker.rkt"
                   "--staging-dir"
                   (path->string cli-staging)
                   "--final-dir"
                   (path->string cli-final)
                   "--label"
                   "q-trusted-producer")))
    (check-equal? exit-code 0)
    (check-true (file-exists? (build-path cli-final "manifest.rktd"))
                "CLI build must publish a manifest-bearing root")
    ;; W1 performance repair round: the published root must carry the
    ;; pre-compiled load-key transfer module (producer-side parse: children
    ;; require it instead of re-parsing the manifest per spawned process).
    (check-true (file-exists? (build-path cli-final "load-keys.rkt"))
                "published root must ship load-keys.rkt")
    (check-true (file-exists? (build-path cli-final "compiled" "load-keys_rkt.zo"))
                "load-keys.rkt must be published in compiled form")
    (define cli-manifest (file->value (build-path cli-final "manifest.rktd")))
    (check-true (hash-has-key? cli-manifest 'load-keys) "manifest must bind the transfer module")
    (check-equal? (hash-ref (hash-ref cli-manifest 'load-keys) 'zo) "compiled/load-keys_rkt.zo")
    (check-equal? (hash-ref (hash-ref cli-manifest 'load-keys) 'zo-digest)
                  (sha256-file (build-path cli-final "compiled" "load-keys_rkt.zo"))))

  ;; ------------------------------------------------------------ W1 repair
  ;; W1 performance repair round (open-gap closure, repair round 6).
  ;; PR #9878 fixed the E2BIG argv by having every child re-parse the full
  ;; manifest.rktd; at trial scale (5,200 entries) that is ~220 ms of pure
  ;; reader parse plus table completion PER spawned child, which alone
  ;; pushed 36/431 spawn-heavy test files over the runner's 2-min per-file
  ;; cap (shard wall 3h13m vs ~25 min). Red-first contract: N children over
  ;; a trial-scale root must complete with NO per-child manifest parse
  ;; cost — the delta against an identical 2-entry control root is bounded.
  (define repair-base (make-temporary-file "qcr-repair~a" 'directory))

  ;; Trial-scale manifest fixture (same entry shape as the e2big pin in
  ;; tests/test-compiled-root-manifest.rkt) plus a REAL compiled load-key
  ;; transfer module — the repair's own mechanism, hand-built so the
  ;; fixture does not depend on the producer for 5,200 fake members.
  (define (write-load-keys-fixture! root-dir triples)
    (define lk-src (build-path root-dir "load-keys.rkt"))
    (call-with-output-file lk-src
                           (lambda (o)
                             (write `(module load-keys racket/base
                                       (provide compiled-root-load-key-table)
                                       (define compiled-root-load-key-table ',triples))
                                    o)))
    (unless (zero? (system*/exit-code (find-executable-path "raco") "make" "-j" "1" lk-src))
      (error 'fixture "raco make failed for the load-key transfer module"))
    (hasheq 'path
            "load-keys.rkt"
            'digest
            (sha256-file lk-src)
            'zo
            "compiled/load-keys_rkt.zo"
            'zo-digest
            (sha256-file (build-path root-dir "compiled" "load-keys_rkt.zo"))))

  (define (make-synthetic-root! name entry-count)
    (define root-dir (build-path repair-base name))
    (make-directory* root-dir)
    (make-directory* (build-path repair-base (string-append name "-map")))
    (define entries
      (for/list ([i (in-range entry-count)])
        (hasheq 'path
                (format "agent/dir~a/module-~a.rkt" (modulo i 64) i)
                'digest
                (make-string 64 #\a)
                'bytes
                128
                'zo
                (format "agent/dir~a/compiled/module-~a_rkt.zo" (modulo i 64) i)
                'zo-digest
                (make-string 64 #\a)
                'zo-bytes
                4096
                'zo-mtime-ms
                0)))
    (define triples
      (for/list ([e (in-list entries)])
        (list (hash-ref e 'path) (hash-ref e 'zo) (hash-ref e 'zo-digest))))
    (define lk-info (write-load-keys-fixture! root-dir triples))
    (define manifest (hasheq 'schema manifest-schema 'sources entries 'load-keys lk-info))
    (call-with-output-file (build-path root-dir "manifest.rktd") (lambda (o) (write manifest o)))
    (compiled-root manifest
                   root-dir
                   (string->path (path->string (build-path repair-base (string-append name "-map"))))
                   checkout-b
                   #t))

  (define qcr-child-path (build-path repair-base "noop-child.rkt"))
  (call-with-output-file qcr-child-path
                         (lambda (o) (display "(module qcr-noop racket/base (void))\n" o)))

  ;; Run one bootstrapped child exactly like the mapped entry (W1a round 5):
  ;; racket <launch-args> -N <child> -t- <child>. Returns (values exit ms).
  (define (run-bootstrapped-child! root)
    (define argv
      (append (compiled-root-launch-arguments root)
              (list "-N" (path->string qcr-child-path) "-t-" (path->string qcr-child-path))))
    (define t0 (current-inexact-milliseconds))
    (define code (apply system*/exit-code (find-executable-path "racket") argv))
    (values code (- (current-inexact-milliseconds) t0)))

  (define big-root (make-synthetic-root! "big-root" 5200))
  (define small-root (make-synthetic-root! "small-root" 2))

  (test-case "spawn-heavy child transfer completes under a parse budget"
    ;; one warm-up child per root (OS caches), then the measured batches
    (letrec ([run-batch!
              (lambda (root n)
                (define t0 (current-inexact-milliseconds))
                (for ([_ (in-range n)])
                  (define-values (code ms) (run-bootstrapped-child! root))
                  (check-equal? code 0 (format "bootstrapped child failed (~a ms): exit ~a" ms code)))
                (- (current-inexact-milliseconds) t0))])
      (run-batch! big-root 1)
      (run-batch! small-root 1)
      (define big-ms (run-batch! big-root 6))
      (define small-ms (run-batch! small-root 6))
      ;; The per-child overhead of the trial-scale root over the control
      ;; root IS the manifest transfer cost. Measured on this machine:
      ;; RED pre-repair ~1556 ms/child (reader parse + megabyte canary
      ;; hashing per child; ~9.3 s delta for 6 children); GREEN
      ;; post-repair ~201 ms/child (compiled table unmarshal + rel-path
      ;; completion — irreducible without re-introducing parsing).
      ;; W1 review round 6: this timing bound is only the SECONDARY smoke
      ;; check (CI variance can wobble it); the DETERMINISTIC proof that
      ;; children never parse the manifest is the no-parse test below,
      ;; which serves an UNPARSEABLE manifest and requires the child to
      ;; run anyway. Budget: 4500 ms for the 6-child batch — ~2× below
      ;; the red measurement, ~3.5× above the green one, so slow shared
      ;; runners do not false-red the repair while an 8× regression
      ;; (parsing) still trips it.
      (check-true
       (<= (- big-ms small-ms) 4500)
       (format
        "per-child manifest transfer cost over budget: big=~a ms small=~a ms ~
                (delta ~a ms for 6 children) — children still re-parse the manifest"
        big-ms
        small-ms
        (- big-ms small-ms)))))

  (test-case "bootstrapped children never parse manifest.rktd (deterministic no-parse proof)"
    ;; W1 review round 6 finding: the timing budget only bounds COST — it
    ;; does not prove the mechanism. This test proves the absence of the
    ;; parse DETERMINISTICALLY: the published manifest is corrupted into
    ;; a byte string that cannot be read as a datum AT ALL, the launch
    ;; arguments are recomputed (baking the corrupted bytes' digest so
    ;; the startup canary passes — the canary pins bytes, it never reads
    ;; them as data), and the bootstrapped child must still complete and
    ;; run its script. The pre-repair transfer re-parsed manifest.rktd
    ;; per spawned child (file->value) and dies here: RED against it by
    ;; construction, with zero timing dependence.
    (define np-root (make-synthetic-root! "no-parse-root" 2))
    (define np-manifest-path (build-path (compiled-root-root-dir np-root) "manifest.rktd"))
    (with-output-to-file np-manifest-path
                         #:exists 'truncate/replace
                         (lambda ()
                           (write-bytes #") no rktd datum can start with an unbalanced close paren")))
    (define np-child (build-path repair-base "no-parse-child.rkt"))
    (define np-child-out (build-path repair-base "no-parse-child-out.txt"))
    (call-with-output-file np-child
                           (lambda (o)
                             (fprintf o "(module qcr-noparse racket/base\n")
                             (fprintf o "  (call-with-output-file ~s\n" (path->string np-child-out))
                             (fprintf o "    (lambda (out) (display \"ran\" out)))\n")
                             (fprintf o "  (void))\n")))
    (define np-code
      (apply system*/exit-code
             (find-executable-path "racket")
             (append (compiled-root-launch-arguments np-root)
                     (list "-N" (path->string np-child) "-t-" (path->string np-child)))))
    (check-equal? np-code
                  0
                  "child must run with an UNPARSEABLE manifest: the transfer never reads it as data")
    (check-equal? (file->string np-child-out) "ran" "the child must actually have executed"))

  ;; W1 review round 6: telemetry atomicity regressions. The counter
  ;; rewrite used to be truncate-in-place, exposing partial/empty files
  ;; to concurrent aggregator reads and to killed children. The repaired
  ;; writer (make-atomic-telemetry-writer, transfer-auth.rkt) builds each
  ;; datum under a unique same-directory temporary name and renames(2) it
  ;; over the target: readers observe complete datums only, at EVERY
  ;; instant (deterministic green, whatever the interleaving), and a kill
  ;; mid-write leaves the previous complete datum plus at worst an
  ;; ignored temporary.
  (test-case "child telemetry writes are atomic under a concurrent reader"
    (define at-dir (build-path repair-base "tel-atomic"))
    (make-directory* at-dir)
    (define at-target (build-path at-dir "q-compiled-root-hit111-222.rktd"))
    (define w (make-atomic-telemetry-writer at-target))
    (w 0 0 0) ; initial complete state: every later observation is a full datum
    (define anomalies 0)
    (define reads 0)
    (define stop? #f)
    (define reader
      (thread
       (lambda ()
         (let loop ()
           (with-handlers ([exn:fail? (lambda (_) (set! anomalies (add1 anomalies)))])
             (let ([v (file->value at-target)])
               (set! reads (add1 reads))
               (unless (and (list? v) (= 4 (length v)) (eq? (car v) 'compiled-root-child-telemetry))
                 (set! anomalies (add1 anomalies)))))
           (unless stop?
             (loop))))))
    (for ([i (in-range 1 2001)])
      (w i i (* 100 i)))
    (set! stop? #t)
    (thread-wait reader)
    (check-true (> reads 0) "concurrent reader made no progress")
    (check-equal? anomalies
                  0
                  (format "concurrent reader observed ~a partial/empty telemetry datum(s)" anomalies))
    ;; the temporary names the writer uses are foreign files to the
    ;; aggregator, never telemetry
    (define tmp-prefix-foreign?
      (compiled-root-child-telemetry-filename? (string-append compiled-root-telemetry-tmp-prefix
                                                              "1234")))
    (check-false tmp-prefix-foreign?
                 "atomic-writer temporaries must never match the telemetry contract")
    (check-equal? (file->value at-target)
                  (list 'compiled-root-child-telemetry
                        '(root-hits . 2000)
                        '(loads . 2000)
                        '(bytes-served . 200000))))

  (test-case "a mid-write kill leaves a complete datum; abandoned temporaries stay foreign"
    (define km-dir (build-path repair-base "tel-kill"))
    (make-directory* km-dir)
    (define km-target (build-path km-dir "q-compiled-root-hit333-444.rktd"))
    (define km-writer (make-atomic-telemetry-writer km-target))
    (km-writer 7 7 700)
    (define killer-cust (make-custodian))
    (parameterize ([current-custodian killer-cust])
      (thread (lambda ()
                (let loop ()
                  (km-writer 9 9 900)
                  (loop)))))
    ;; Kill while a write is observably IN FLIGHT: the atomic writer
    ;; builds each datum under a unique same-directory temporary
    ;; (compiled-root-telemetry-tmp-prefix) and renames(2)s it over the
    ;; target, so an existing temporary proves a write is mid-flight at
    ;; that instant. Bounded observation polling replaces the former
    ;; fixed 50 ms wall-clock sleep (v1.00.28 W1 wait-audit discipline:
    ;; tests carry no raw sleeps — the frozen v1.00.29-w1 wait-audit has
    ;; no row for this file); the ceiling bounds only pathological
    ;; scheduler starvation and never gates the assertions below, which
    ;; hold for every interleaving because each writer publication is
    ;; atomic.
    (let poll ([deadline (+ (current-inexact-milliseconds) 2000)])
      (unless (or (for/or ([f (in-list (directory-list km-dir))])
                    (string-prefix? (path->string f) compiled-root-telemetry-tmp-prefix))
                  (>= (current-inexact-milliseconds) deadline))
        (poll deadline)))
    (custodian-shutdown-all killer-cust)
    (define datum (file->value km-target))
    (check-true
     (and (list? datum) (= 4 (length datum)) (eq? (car datum) 'compiled-root-child-telemetry))
     (format "counter file must be a COMPLETE datum after a mid-write kill: ~s" datum))
    (define noise (open-output-string))
    (define-values (hits loads served)
      (parameterize ([current-error-port noise])
        (aggregate-compiled-root-child-telemetry km-dir)))
    (check-true (or (= hits 7) (= hits 9))
                (format "aggregate must reflect a complete counter file, got hits=~a" hits))
    (check-false (string-contains? (get-output-string noise) "ignoring")
                 (format "abandoned temporaries must be ignored silently: ~a"
                         (get-output-string noise))))

  (test-case "the child bootstrap wires the two review-round mechanisms in"
    ;; structural pins for the generated bootstrap: the transfer is
    ;; loaded through authenticate-compiled-module (exact-bytes-executed)
    ;; and telemetry goes through make-atomic-telemetry-writer — never an
    ;; in-place truncate of the counter file
    (define boot-expr (car (reverse (compiled-root-launch-arguments root-b))))
    (check-true (string-contains? boot-expr "authenticate-compiled-module")
                "bootstrap must authenticate-then-eval the transfer module")
    (check-true (string-contains? boot-expr "make-atomic-telemetry-writer")
                "bootstrap must route telemetry through make-atomic-telemetry-writer")
    (check-false (regexp-match? #rx"call-with-output-file[^\"]*q-tel-file" boot-expr)
                 "bootstrap must never write the counter file in place")
    ;; Reviewer P2 (W1 repair round 7): the counter NAME must be reserved
    ;; through reserve-child-telemetry-target — make-temporary-file in the
    ;; bootstrap created an EMPTY contract-matching file before the first
    ;; complete snapshot, which an interrupted startup left behind as
    ;; malformed telemetry
    (check-true (string-contains? boot-expr "reserve-child-telemetry-target")
                "bootstrap must reserve the counter name via reserve-child-telemetry-target")
    (check-false
     (string-contains? boot-expr "make-temporary-file")
     "bootstrap must never create a counter file itself: no contract filename before a complete snapshot")
    ;; the authenticated table is fetched by MODULE NAME from the registry:
    ;; a path-based dynamic-require would re-open lk-path after
    ;; authentication and resurrect the hash/require TOCTOU
    (check-true (string-contains? boot-expr "dynamic-require ''load-keys")
                "bootstrap must fetch the table from the authenticated module by name")
    (check-false (string-contains? boot-expr "(dynamic-require lk-path")
                 "bootstrap must never require the transfer module by path"))

  ;; W1 performance repair round: the child telemetry round trip. The
  ;; child bootstrap names its counter files and the parent aggregator
  ;; (aggregate-compiled-root-child-telemetry) must accept them. Two
  ;; defect classes are pinned:
  ;;   (a) a bootstrapped child with ZERO root-served loads (the common
  ;;       case for helper processes in spawn-heavy files) must leave NO
  ;;       file the aggregator reports as malformed — the CI shard log
  ;;       flooded with "ignoring malformed child telemetry
  ;;       q-compiled-root-hit<...>.rktd" lines (filename format mismatch);
  ;;   (b) hit-writing children must round-trip through the aggregator.
  (define (run-telemetry-child! root child-path tel-dir)
    (define env (make-environment-variables))
    (for ([n (in-list (environment-variables-names (current-environment-variables)))])
      (environment-variables-set! env
                                  n
                                  (environment-variables-ref (current-environment-variables) n)))
    (environment-variables-set! env
                                #"Q_COMPILED_ROOT_TELEMETRY_DIR"
                                (string->bytes/utf-8 (path->string tel-dir)))
    (define out (open-output-string))
    (define code
      (parameterize ([current-output-port out]
                     [current-error-port out]
                     [current-environment-variables env])
        (apply system*/exit-code
               (find-executable-path "racket")
               (append (compiled-root-launch-arguments root)
                       (list "-N" (path->string child-path) "-t-" (path->string child-path))))))
    (values code (get-output-string out)))

  (test-case "zero-hit bootstrapped child leaves no malformed telemetry noise"
    (define tel-dir (build-path tmp-base "tel-zero-hit"))
    (make-directory* tel-dir)
    (define zero-child (build-path tmp-base "zero-child.rkt"))
    (call-with-output-file zero-child
                           (lambda (o) (display "(module qcr-zero racket/base (void))\n" o)))
    (define-values (code transcript) (run-telemetry-child! root-b zero-child tel-dir))
    (check-equal? code 0 (format "zero-hit child failed: ~a" transcript))
    (define-values (hits loads served)
      (parameterize ([current-error-port (open-output-string)])
        (aggregate-compiled-root-child-telemetry tel-dir)))
    (check-equal? (list hits loads served) (list 0 0 0))
    ;; every CONTRACT file the writer left behind must be a parseable
    ;; telemetry datum (the writer reports its zero counters at startup;
    ;; it never leaves an empty contract file for the aggregator to
    ;; reject). Reservation claim probes are contract-disjoint foreign
    ;; files — not telemetry, never judged here (the aggregator ignores
    ;; them silently, pinned in the sibling suites).
    (for ([f (in-list (directory-list tel-dir))]
          #:when (compiled-root-child-telemetry-filename? f))
      (check-true (> (file-size (build-path tel-dir f)) 0)
                  (format "empty telemetry file ~a: the aggregator rejects it as malformed" f))
      (check-not-exn (lambda () (file->value (build-path tel-dir f)))
                     (format "telemetry file ~a is not a parseable datum" f)))
    (define noise (open-output-string))
    (parameterize ([current-error-port noise])
      (aggregate-compiled-root-child-telemetry tel-dir))
    (check-false (string-contains? (get-output-string noise) "ignoring")
                 (format "aggregator must accept writer-produced telemetry files: ~a"
                         (get-output-string noise))))

  (test-case "hit-writing child telemetry round-trips through the aggregator"
    (define tel-dir (build-path tmp-base "tel-hit"))
    (make-directory* tel-dir)
    (define hit-child (build-path tmp-base "hit-child.rkt"))
    (call-with-output-file
     hit-child
     (lambda (o)
       (fprintf o "(module qcr-hit racket/base\n")
       (fprintf o "  (require (file ~s))\n" (path->string (marker-abs-path checkout-b)))
       (fprintf o "  (void marker-result))\n")))
    (define-values (code transcript) (run-telemetry-child! root-b hit-child tel-dir))
    (check-equal? code 0 (format "hit child failed: ~a" transcript))
    (define noise (open-output-string))
    (define-values (hits loads served)
      (parameterize ([current-error-port noise])
        (aggregate-compiled-root-child-telemetry tel-dir)))
    (check-true (> hits 0) (format "aggregator counted no root hits: ~a" (get-output-string noise)))
    (check-true (> loads 0) (format "aggregator counted no loads: ~a" (get-output-string noise)))
    (check-true (> served 0)
                (format "aggregator counted no served bytes: ~a" (get-output-string noise)))
    (check-false (string-contains? (get-output-string noise) "ignoring")
                 (format "hit telemetry must be accepted, not skipped: ~a"
                         (get-output-string noise))))

  (test-case "transfer module mutation after verification fails the child launch closed"
    ;; the load-key transfer module carries its own startup canary: the
    ;; same mutation-after-verification fail-closed guarantee the manifest
    ;; digest provides, extended to the table the per-load digests root in
    (define lk-zo (build-path final-dir "compiled" "load-keys_rkt.zo"))
    (check-true (file-exists? lk-zo) "published root must ship the compiled transfer module")
    (unless (zero? (system*/exit-code (find-executable-path "chmod") "u+w" lk-zo))
      (error 'lk-canary-test "could not chmod +w the compiled transfer module"))
    (define lk-bytes (file->bytes lk-zo))
    (with-output-to-file lk-zo
                         #:exists 'truncate/replace
                         (lambda ()
                           (define b (bytes-copy lk-bytes))
                           (bytes-set! b 0 (bitwise-xor (bytes-ref b 0) #xFF))
                           (write-bytes b)))
    (define canary-child (build-path tmp-base "lk-canary-child.rkt"))
    (call-with-output-file canary-child
                           (lambda (o) (display "(module qcr-lkcanary racket/base (void))\n" o)))
    (define out (open-output-string))
    (define code
      (parameterize ([current-output-port out]
                     [current-error-port out])
        (apply system*/exit-code
               (find-executable-path "racket")
               (append (compiled-root-launch-arguments root-b)
                       (list "-N" (path->string canary-child) "-t-" (path->string canary-child))))))
    (check-false (zero? code) "child must fail closed when the transfer module mutated")
    (check-true (string-contains? (get-output-string out) "load-key table mutated")
                (format "failure must name the mutated transfer module: ~a" (get-output-string out)))
    ;; restore for any later reader
    (with-output-to-file lk-zo #:exists 'truncate/replace (lambda () (write-bytes lk-bytes)))
    (system*/exit-code (find-executable-path "chmod") "a-w" lk-zo))

  ;; ------------------ W1a follow-on: child bootstrap hash lookup
  ;;
  ;; Smoke-5 diagnosis (see PLAN-W1-ARGV-REVIEW-REPAIR.md, "Follow-on W1a
  ;; performance diagnosis"): the child bootstrap consulted its
  ;; canonical-source table with a linear `assoc` per guarded load — a
  ;; shaped miss measured ~300–320 µs at the 5,200-entry production
  ;; scale vs ~4.6–5.7 µs for an equal?-based hash on the same keys
  ;; (local synthetic probes; microbenchmarks, not CI measurements or
  ;; proof of the smoke's dominant cause). The repair replaces ONLY the
  ;; linear lookup table with an equal?-based hash preserving every
  ;; existing semantic:
  ;;   - equal? on canonical path strings (never hasheq/hasheqv; the
  ;;     table keys and the arriving canonical path are separately
  ;;     allocated equal? strings);
  ;;   - manifest-order FIRST match on conflicting duplicate canonical
  ;;     keys (the assoc rule); a plain immutable hash would keep the
  ;;     LAST entry instead;
  ;;   - identical simplify/complete canonicalization (links followed);
  ;;   - per-hit digest checks and telemetry counting unchanged;
  ;;   - the child table is STILL keyed by canonical SOURCE paths only:
  ;;     a direct payload-zo load misses it exactly as before — this
  ;;     repair adds no direct-zo protection (the parent keeps its two
  ;;     by-src/by-zo branches).
  ;; Detection does NOT rest on wall-clock alone: the mechanism pin
  ;; below asserts the generated bootstrap structurally, and the
  ;; semantic pins drive the REAL generated bootstrap end to end with
  ;; exact hit counts; only the scale test carries a secondary generous
  ;; timing budget (the parse-budget/no-parse two-layer pattern used
  ;; throughout this file).
  (define lookup-base (build-path repair-base "child-lookup"))
  (make-directory* lookup-base)
  ;; Real fixture modules the child actually loads: committed checkout,
  ;; raco-made zo payload bytes, matching digests — so guarded loads
  ;; HIT, pass the per-hit digest check, and leave exact, observable
  ;; telemetry (unlike make-synthetic-root! above, whose fake members
  ;; only measure startup cost of never-loading children).
  (define lookup-checkout
    (make-lang-checkout!
     "lookup-checkout"
     (for/list ([i (in-range 3)])
       (cons (format "lookup/look-~a.rkt" i)
             (format "#lang racket/base\n(provide look-~a)\n(define look-~a ~a)\n" i i i)))))
  ;; Symlink alias spelling the alias table entries below are declared
  ;; through: a REAL subdirectory plus a symlink to it, spelled with an
  ;; up-element detour. simplify-path follows links ONLY when an
  ;; up-element directly crosses them (probe-verified on this Racket:
  ;; a plain link element is never collapsed), so this detour is the
  ;; symlink alias whose normalization onto the canonical spelling the
  ;; child's canonicalization actually performs — the alias-detour
  ;; table entry then matches a consumer load of the REAL spelling.
  ;; (A plain link-dir spelling without the detour can never match:
  ;; its key stays link-spelled on both sides — the old fixture's
  ;; alias hit was an artifact of the masked canonical entry.)
  (make-directory* (build-path lookup-checkout "lookup" "real-sub"))
  (with-output-to-file (build-path lookup-checkout "lookup" "real-sub" "keep.txt")
                       #:exists 'replace
                       (lambda () (display "placeholder")))
  (make-file-or-directory-link (path->string (build-path lookup-checkout "lookup" "real-sub"))
                               (build-path lookup-checkout "lookup" "link-sub"))
  ;; raco make each fixture module and publish the zo bytes under the
  ;; payload layout of ROOT-DIR; returns (list rel-src zo-rel digest)
  ;; triples whose digests match the published bytes.
  (define (make-real-lookup-triples! root-dir module-indices)
    (for/list ([i (in-list module-indices)])
      (define rel-src (format "lookup/look-~a.rkt" i))
      (define abs-src (build-path lookup-checkout rel-src))
      (unless (zero? (system*/exit-code (find-executable-path "raco") "make" "-j" "1" abs-src))
        (error 'fixture "raco make failed for ~a" abs-src))
      (define zo-rel (format "lookup/compiled/look-~a_rkt.zo" i))
      (define payload-zo (build-path root-dir (string->path zo-rel)))
      (make-directory* (path-only payload-zo))
      (copy-file (build-path lookup-checkout (string->path zo-rel)) payload-zo #t)
      (list rel-src zo-rel (sha256-file payload-zo))))
  ;; Build a synthetic root from READY-MADE real triples plus arbitrary
  ;; extra triples (duplicate-key/collision fixtures) and filler entries
  ;; up to production scale. The transfer module is hand-built exactly
  ;; like write-load-keys-fixture! above: a REAL raco-made compiled
  ;; module, so the child bootstrap loads it through the ordinary
  ;; authenticate-then-eval path.
  (define (make-child-lookup-root! name real-triples extra-triples filler-count)
    (define root-dir (build-path lookup-base name))
    (make-directory* root-dir)
    (define all-triples
      (append real-triples
              extra-triples
              (for/list ([i (in-range filler-count)])
                (list (format "filler/dir~a/module-~a.rkt" (modulo i 64) i)
                      (format "filler/dir~a/compiled/module-~a_rkt.zo" (modulo i 64) i)
                      (make-string 64 #\a)))))
    (define lk-info (write-load-keys-fixture! root-dir all-triples))
    (define manifest-entries
      (for/list ([t (in-list real-triples)])
        (hasheq 'path
                (list-ref t 0)
                'digest
                (sha256-file (build-path lookup-checkout (string->path (list-ref t 0))))
                'bytes
                (file-size (build-path lookup-checkout (string->path (list-ref t 0))))
                'zo
                (list-ref t 1)
                'zo-digest
                (list-ref t 2)
                'zo-bytes
                (file-size (build-path root-dir (string->path (list-ref t 1))))
                'zo-mtime-ms
                0)))
    (define manifest (hasheq 'schema manifest-schema 'sources manifest-entries 'load-keys lk-info))
    (call-with-output-file (build-path root-dir "manifest.rktd") (lambda (o) (write manifest o)))
    (compiled-root manifest
                   root-dir
                   (string->path (path->string (build-path lookup-base (string-append name "-map"))))
                   lookup-checkout
                   #t))
  (define lookup-real (make-real-lookup-triples! (build-path lookup-base "lookup-root") (list 0 1 2)))
  (define look-1-triple (list-ref lookup-real 1))
  (define look-2-triple (list-ref lookup-real 2))
  ;; Production-scale lookup root: 1 canonical table member + 4 extra
  ;; entries (the look-2 alias-only symlink-detour declaration + the
  ;; look-0 canonical duplicate + the look-1 alias-canonical duplicate
  ;; pair) + 5,196 fillers = 5,201 table entries (~today's whole-
  ;; checkout root). All three payload members ARE published, but look-2's AND
  ;; look-1's CANONICAL spellings are deliberately ABSENT from the
  ;; table: look-2's only entry is the alias spelling, and look-1's
  ;; only entries are the alias-canonical duplicate pair. Any canonical
  ;; entry placed BEFORE such a fixture entry would pre-empt the
  ;; canonical key and mask the property under test (the exact trap
  ;; that made the old symlink test ineffective).
  (define lookup-root
    (make-child-lookup-root!
     "lookup-root"
     ;; canonical table entry: look-0 ONLY
     (list (list-ref lookup-real 0))
     ;; conflicting SECOND occurrence of look-0's canonical key: its
     ;; payload zo does not exist and its digest cannot match — a
     ;; last-match regression digests it and fails the child closed
     (list
      (list "lookup/look-0.rkt" "dup/compiled/missing_rkt.zo" (make-string 64 #\b))
      ;; look-2 ALIAS-ONLY entry, declared through the symlink
      ;; up-detour spelling (same payload member as the absent
      ;; canonical spelling; drives the symlink test below)
      (list "lookup/link-sub/../look-2.rkt" (list-ref look-2-triple 1) (list-ref look-2-triple 2))
      ;; conflicting ALIAS-CANONICAL duplicates of look-1: the FIRST
      ;; entry is the valid symlink-detour spelling, the SECOND an
      ;; invalid canonical duplicate. Normalization collides the
      ;; pair on look-1's canonical key and insert-if-absent must
      ;; keep the FIRST (valid) entry; a lost normalization leaves
      ;; the invalid duplicate owning the canonical key and the
      ;; real-spelling load digests the missing payload (child
      ;; fails closed); a last-match-wins hash overwrites the valid
      ;; alias entry (child fails closed). look-1 has NO other
      ;; canonical table entry that could pre-empt this pair.
      (list "lookup/link-sub/../look-1.rkt" (list-ref look-1-triple 1) (list-ref look-1-triple 2))
      (list "lookup/look-1.rkt" "dup2/compiled/missing_rkt.zo" (make-string 64 #\c)))
     5196))
  ;; 2-entry control root for the scale delta (constant child costs
  ;; cancel between the two runs; only table size differs).
  (define lookup-small (make-child-lookup-root! "lookup-small" '() '() 2))

  (test-case "child bootstrap builds an equal?-based hash lookup (deterministic mechanism pin)"
    (define boot-expr (car (reverse (compiled-root-launch-arguments lookup-root))))
    (check-false (string-contains? boot-expr "assoc")
                 "the child bootstrap must not linear-scan the table with assoc")
    (check-true (string-contains? boot-expr "(make-hash)")
                "the lookup table must be an equal?-based hash (make-hash)")
    (check-false (string-contains? boot-expr "(make-hasheq")
                 "the table equivalence must stay equal?, never eq?")
    (check-false (string-contains? boot-expr "(make-hasheqv")
                 "the table equivalence must stay equal?, never eqv?")
    (check-true (string-contains? boot-expr "(hash-ref tbl")
                "guarded loads must consult the hash, not a linear scan")
    (check-true
     (string-contains? boot-expr "(hash-has-key? ht k)")
     "conflicting duplicate canonical keys must keep the assoc first match (insert-if-absent)"))

  ;; exact child hit/byte counts through the REAL generated bootstrap:
  ;; one temporary telemetry directory per test case keeps the counts
  ;; independent
  (define (aggregate-child-telemetry tel-dir)
    (parameterize ([current-error-port (open-output-string)])
      (call-with-values (lambda () (aggregate-compiled-root-child-telemetry tel-dir)) list)))

  (test-case "child guarded loads hit on separately allocated equal? string keys"
    ;; the table keys arrive as zo-marshaled strings and the lookups as
    ;; fresh path->string allocations: an eq-keyed table (hasheq) would
    ;; miss them and report 0 hits; the equal?-based table must hit
    (define tel-dir (build-path lookup-base "tel-equal-keys"))
    (make-directory* tel-dir)
    (define child (build-path lookup-base "equal-keys-child.rkt"))
    (call-with-output-file
     child
     (lambda (o)
       (fprintf o "(module qcr-equal-keys racket/base\n")
       (fprintf o
                "  (require (file ~s))\n"
                (path->string (build-path lookup-checkout "lookup" "look-0.rkt")))
       (fprintf o
                "  (require (file ~s))\n"
                (path->string (build-path lookup-checkout "lookup" "look-1.rkt")))
       (fprintf o "  (void look-0 look-1))\n")))
    (define-values (code transcript) (run-telemetry-child! lookup-root child tel-dir))
    (check-equal? code 0 (format "hit child failed: ~a" transcript))
    (match-define (list hits loads served) (aggregate-child-telemetry tel-dir))
    (check-equal? hits 2 "each of the two guarded loads must hit the equal?-keyed table")
    (check-equal? loads 2 "hit counting must stay per guarded load")
    (check-equal?
     served
     (+ (file-size
         (build-path (compiled-root-root-dir lookup-root) "lookup" "compiled" "look-0_rkt.zo"))
        (file-size
         (build-path (compiled-root-root-dir lookup-root) "lookup" "compiled" "look-1_rkt.zo")))
     "served bytes must be the payload zo sizes of exactly the hit members"))

  (test-case "conflicting duplicate canonical keys select the assoc first match"
    ;; the transfer table carries look-0's canonical key TWICE: the
    ;; valid first entry and an invalid duplicate (nonexistent payload
    ;; zo, unmatchable digest). assoc semantics select the FIRST entry;
    ;; a hash built last-match-wins would digest the missing duplicate
    ;; payload and fail the child closed.
    (define tel-dir (build-path lookup-base "tel-dup-first"))
    (make-directory* tel-dir)
    (define child (build-path lookup-base "dup-first-child.rkt"))
    (call-with-output-file
     child
     (lambda (o)
       (fprintf o "(module qcr-dup-first racket/base\n")
       (fprintf o
                "  (require (file ~s))\n"
                (path->string (build-path lookup-checkout "lookup" "look-0.rkt")))
       (fprintf o "  (void look-0))\n")))
    (define-values (code transcript) (run-telemetry-child! lookup-root child tel-dir))
    (check-equal? code
                  0
                  (format "the child must run: first-match selection avoids the invalid duplicate: ~a"
                          transcript))
    (match-define (list hits loads served) (aggregate-child-telemetry tel-dir))
    (check-equal? hits 1 "exactly the guarded look-0 load must hit")
    (check-equal?
     served
     (file-size (build-path (compiled-root-root-dir lookup-root) "lookup" "compiled" "look-0_rkt.zo"))
     "the FIRST duplicate entry's payload must be the one digest-checked"))

  (test-case "conflicting alias-canonical duplicates select the first match after normalization"
    ;; the table declares look-1's canonical key TWICE: FIRST under the
    ;; symlink-detour alias spelling (valid payload), THEN under the
    ;; canonical spelling with a nonexistent payload zo and unmatchable
    ;; digest. Normalization collides the pair on look-1's canonical
    ;; key; insert-if-absent must then keep the FIRST (valid) entry.
    ;; Dually red: a lost normalization leaves the invalid duplicate
    ;; owning the canonical key and the real-spelling load digests the
    ;; missing payload (child fails closed); a last-match-wins hash
    ;; overwrites the valid alias entry with the invalid duplicate
    ;; (child fails closed). look-1 has NO earlier canonical table
    ;; entry that could pre-empt this pair (the masking trap that
    ;; neutralized the original symlink test). Ordered BEFORE the
    ;; payload-mutation test, which permanently corrupts look-1's
    ;; published payload.
    (define tel-dir (build-path lookup-base "tel-dup-alias-first"))
    (make-directory* tel-dir)
    (define child (build-path lookup-base "dup-alias-first-child.rkt"))
    (call-with-output-file
     child
     (lambda (o)
       (fprintf o "(module qcr-dup-alias-first racket/base\n")
       (fprintf o
                "  (require (file ~s))\n"
                (path->string (build-path lookup-checkout "lookup" "look-1.rkt")))
       (fprintf o "  (void look-1))\n")))
    (define-values (code transcript) (run-telemetry-child! lookup-root child tel-dir))
    (check-equal?
     code
     0
     (format "the child must run: the normalized FIRST match is the valid alias entry: ~a"
             transcript))
    (match-define (list hits loads served) (aggregate-child-telemetry tel-dir))
    (check-equal? hits 1 "exactly the guarded look-1 load must hit")
    (check-equal?
     served
     (file-size (build-path (compiled-root-root-dir lookup-root) "lookup" "compiled" "look-1_rkt.zo"))
     "the FIRST (alias-declared) duplicate entry's payload must be the one digest-checked"))

  (test-case "symlink alias spellings still hit (canonicalization preserved)"
    ;; EFFECTIVE alias regression (Codex review: the previous version
    ;; also kept look-2's canonical entry in the table, so the
    ;; real-spelling load hit it even with alias normalization removed —
    ;; the test could never go red). The table now carries look-2 ONLY
    ;; under the lookup/link-sub/../ symlink detour spelling; the child
    ;; loads the REAL spelling, so the hit is possible only if the
    ;; bootstrap's canonicalization normalizes the alias-detour key onto
    ;; the canonical real-spelling key (dropped normalization yields
    ;; 0 hits), and it must hit exactly once
    (define tel-dir (build-path lookup-base "tel-symlink"))
    (make-directory* tel-dir)
    (define child (build-path lookup-base "symlink-child.rkt"))
    (call-with-output-file
     child
     (lambda (o)
       (fprintf o "(module qcr-symlink racket/base\n")
       (fprintf o
                "  (require (file ~s))\n"
                (path->string (build-path lookup-checkout "lookup" "look-2.rkt")))
       (fprintf o "  (void look-2))\n")))
    (define-values (code transcript) (run-telemetry-child! lookup-root child tel-dir))
    (check-equal? code 0 (format "hit child failed: ~a" transcript))
    (match-define (list hits loads served) (aggregate-child-telemetry tel-dir))
    (check-equal? hits 1 "the real-spelling load must hit the symlink-declared entry")
    (check-equal?
     served
     (file-size (build-path (compiled-root-root-dir lookup-root) "lookup" "compiled" "look-2_rkt.zo"))
     "the alias entry must resolve to the same payload member"))

  (test-case "direct payload-zo loads miss the child table (source-only lookup preserved)"
    ;; the child table is keyed by canonical SOURCE paths only (the
    ;; parent keeps a separate by-zo table; the child never had one).
    ;; A direct load of a payload zo must NOT hit: no digest check, no
    ;; telemetry — this repair adds no direct-zo protection.
    (define tel-dir (build-path lookup-base "tel-direct-zo"))
    (make-directory* tel-dir)
    (define child (build-path lookup-base "direct-zo-child.rkt"))
    (call-with-output-file child
                           (lambda (o)
                             (fprintf o "(module qcr-direct-zo racket/base\n")
                             (fprintf o
                                      "  (require (file ~s))\n"
                                      (path->string (build-path (compiled-root-root-dir lookup-root)
                                                                "lookup"
                                                                "compiled"
                                                                "look-1_rkt.zo")))
                             (fprintf o "  (void))\n")))
    (define-values (code transcript) (run-telemetry-child! lookup-root child tel-dir))
    (check-equal? code 0 (format "a valid payload zo must load directly: ~a" transcript))
    (match-define (list hits loads served) (aggregate-child-telemetry tel-dir))
    (check-equal? (list hits loads served)
                  (list 0 0 0)
                  "direct zo loads must miss the canonical-source-only child table"))

  (test-case "payload mutation after bootstrap init fails a later child hit closed"
    ;; the table is built (and the transfer module authenticated) at
    ;; bootstrap; the per-hit digest check must still catch payload
    ;; bytes mutated afterwards — no weakening of the load-time canary
    (define child (build-path lookup-base "mutate-child.rkt"))
    (define look-1-zo
      (build-path (compiled-root-root-dir lookup-root) "lookup" "compiled" "look-1_rkt.zo"))
    (call-with-output-file
     child
     (lambda (o)
       (fprintf o "(module qcr-child-mutate racket/base\n")
       (fprintf o "  (require racket/file)\n")
       ;; runs AFTER the bootstrap initialized: flip the first payload
       ;; byte, then make a guarded load whose digest check must fail
       (fprintf o "  (define b (file->bytes (string->path ~s)))\n" (path->string look-1-zo))
       (fprintf o "  (bytes-set! b 0 (bitwise-xor (bytes-ref b 0) #xFF))\n")
       (fprintf o "  (call-with-output-file (string->path ~s)\n" (path->string look-1-zo))
       (fprintf o "    (lambda (out) (write-bytes b out)) #:exists 'truncate)\n")
       (fprintf o
                "  (dynamic-require (string->path ~s) 'look-1))\n"
                (path->string (build-path lookup-checkout "lookup" "look-1.rkt")))))
    (define tel-dir (build-path lookup-base "tel-mutate"))
    (make-directory* tel-dir)
    (define-values (code transcript) (run-telemetry-child! lookup-root child tel-dir))
    (check-false (zero? code)
                 (format "the later guarded hit must fail closed on mutated payload bytes: ~a"
                         transcript))
    (check-true (string-contains? transcript "payload mutated after verification")
                (format "the failure must name the load-time canary: ~a" transcript)))

  (test-case "scale: repeated guarded misses over the production-size table (real bootstrap)"
    ;; 40,000 guarded misses of a key absent from both tables, through
    ;; the REAL generated bootstrap. Linear assoc at 5,200 entries costs
    ;; ~300 µs per miss (~12 s for the batch, measured on this machine);
    ;; the equal?-based hash costs ~5 µs (~0.2 s). The 3 s budget is a
    ;; SECONDARY smoke check (CI variance can wobble it), ~4x below the
    ;; red measurement and ~60x above the green one; the deterministic
    ;; proof of the mechanism is the structural pin and the exact-count
    ;; semantic pins above. Constant per-load costs (canonicalization,
    ;; the orig-load miss) are identical for both roots and cancel in
    ;; the delta; warm-up children absorb OS cache effects.
    (define scale-child (build-path lookup-base "scale-child.rkt"))
    (define done-path (build-path lookup-base "scale-done.txt"))
    (define probe-path (build-path lookup-checkout "filler" "no-such-module.rkt"))
    (call-with-output-file
     scale-child
     (lambda (o)
       (fprintf o "(module qcr-lookup-scale racket/base\n")
       (fprintf o "  (define cl (current-load))\n")
       (fprintf o "  (define probe (string->path ~s))\n" (path->string probe-path))
       (fprintf o "  (define done 0)\n")
       (fprintf o "  (for ([_ (in-range 40000)])\n")
       (fprintf o "    (with-handlers ([exn:fail? void])\n")
       (fprintf o "      (cl probe 'qcr-scale-probe))\n")
       (fprintf o "    (set! done (add1 done)))\n")
       (fprintf o "  (call-with-output-file ~s\n" (path->string done-path))
       (fprintf o "    (lambda (out) (write done out)) #:exists 'truncate)\n")
       (fprintf o "  (void))\n")))
    (define (timed-scale-child! root)
      (define t0 (current-inexact-milliseconds))
      (define code
        (apply system*/exit-code
               (find-executable-path "racket")
               (append (compiled-root-launch-arguments root)
                       (list "-N" (path->string scale-child) "-t-" (path->string scale-child)))))
      (values code (- (current-inexact-milliseconds) t0)))
    (timed-scale-child! lookup-small)
    (timed-scale-child! lookup-root)
    (define-values (small-code small-ms) (timed-scale-child! lookup-small))
    (define-values (big-code big-ms) (timed-scale-child! lookup-root))
    (check-equal? small-code 0 "the small-root scale child must complete")
    (check-equal? big-code 0 "the production-size scale child must complete")
    (check-equal? (file->value done-path)
                  40000
                  "every guarded load must traverse the child wrapper end to end")
    (define delta (- big-ms small-ms))
    (check-true
     (<= delta 3000)
     (format (string-append "per-lookup table cost over budget: big=~a ms small=~a ms "
                            "(delta ~a ms for 40000 guarded misses over 5201 vs 2 entries) "
                            "— the child still linear-scans the canonical-source table")
             big-ms
             small-ms
             delta)))

  ;; Codex contract review: old roots (published before the compiled
  ;; load-key transfer module existed; manifest with the field absent)
  ;; deliberately VALIDATE under the new reader — the field is optional
  ;; for backward acceptance — but they CANNOT LAUNCH: launch arguments
  ;; cannot be derived without the transfer module, so the launch
  ;; boundary fails closed with an explicit rebuild requirement. The
  ;; resolve CLI step derives launch arguments BEFORE writing the flags
  ;; file, so an old root yields a non-zero resolve, no flags file, and
  ;; therefore no bootstrapped child — fail-closed by construction. The
  ;; remedy is an explicit rebuild with the current producer; historic
  ;; artifacts are never rewritten to match the new field.
  (define-runtime-path ci-cli-runtime-path "../ci/prepared-environment/compiled-root.rkt")
  ;; Materialize a genuine old-style artifact on a fresh copy (the
  ;; published root itself is never touched): strip the transfer
  ;; members and the manifest, then rebuild the manifest with the
  ;; pre-field algorithm (sources-only coverage, no field) — the exact
  ;; shape the pre-field producer published. One fixture recipe, three
  ;; test cases (validation/launch split, flags invalidation, launch
  ;; consumption), each with its own private old-dir copy.
  (define (materialize-old-root! name)
    (define old-dir (build-path tmp-base name))
    (when (directory-exists? old-dir)
      (delete-directory/files old-dir))
    (copy-directory/files final-dir old-dir)
    (unless (zero? (system*/exit-code (find-executable-path "chmod") "-R" "u+w" old-dir))
      (error 'old-root-fixture "could not chmod +w the old-root copy"))
    (delete-file (build-path old-dir "load-keys.rkt"))
    (delete-file (build-path old-dir "compiled" "load-keys_rkt.zo"))
    (define old-source-pairs
      (for/list ([e (in-list (hash-ref (file->value (build-path old-dir "manifest.rktd")) 'sources))])
        (list (hash-ref e 'path) (hash-ref e 'zo))))
    (delete-file (build-path old-dir "manifest.rktd"))
    (define old-manifest
      (make-compiled-root-manifest #:sources old-source-pairs
                                   #:root-dir old-dir
                                   #:racket-executable (find-executable-path "racket")
                                   #:producer-label "q-trusted-producer"))
    (check-false (hash-has-key? old-manifest 'load-keys)
                 "fixture precondition: the old-style manifest carries no field")
    (call-with-output-file (build-path old-dir "manifest.rktd") (lambda (o) (write old-manifest o)))
    (system*/exit-code (find-executable-path "chmod") "-R" "a-w" old-dir)
    old-dir)
  (test-case "old root (no load-keys field) validates but cannot launch — rebuild required"
    (define old-dir (materialize-old-root! "old-style-published-root"))
    ;; backward-compatible acceptance: the old artifact VALIDATES
    (define old-root
      (resolve-compiled-root! #:final-dir old-dir
                              #:checkout checkout-b
                              #:trusted-producer-labels '("q-trusted-producer")
                              #:map-dir (build-path tmp-base "map-old")))
    (check-true (eq? (compiled-root-reason old-root) #t)
                (reason->string (compiled-root-reason old-root)))
    ;; ... but it CANNOT LAUNCH: no launch arguments are ever derived
    ;; from a root that predates the transfer module — the failure names
    ;; the explicit rebuild requirement
    (check-exn #rx"no compiled load-key transfer module \\(rebuild with the current producer\\)"
               (lambda () (compiled-root-launch-arguments old-root)))
    ;; and the resolve CLI step fails closed the same way, BEFORE writing
    ;; a flags file: a non-zero resolve and no flags file mean no launch
    ;; step can ever bootstrap a child from an old root
    (define old-flags-dir (build-path tmp-base "old-root-flags"))
    (make-directory* old-flags-dir)
    (define old-flags-file (build-path old-flags-dir "flags.rktd"))
    (define resolve-code
      (system*/exit-code (find-executable-path "racket")
                         (path->string ci-cli-runtime-path)
                         "resolve"
                         "--root"
                         (path->string old-dir)
                         "--checkout"
                         (path->string checkout-b)
                         "--trusted-label"
                         "q-trusted-producer"
                         "--flags-file"
                         (path->string old-flags-file)))
    (check-false (zero? resolve-code) "resolve must fail closed for an old root")
    (check-false (file-exists? old-flags-file)
                 "resolve must not write a flags file for a root that cannot launch"))

  ;; Reviewer blocker (red-first regression, split into two test cases
  ;; so each concern fails independently): the check above proves only
  ;; that a failed resolve WRITES no flags. A flags file left on the
  ;; same path by an EARLIER successful resolve (retried shard, reused
  ;; runner temp directory, different root) survives the old-root
  ;; failure, and `launch` consumes any existing well-formed file —
  ;; bootstrapping children from a root this resolve just rejected.
  ;; This old-root failure invalidates the preexisting flags file;
  ;; a successful resolve still writes a well-formed, consumable file.
  ;; Parse and unlink failures have different behavior, covered below.
  (define (run-cli-resolve! flags-file root-arg)
    (system*/exit-code (find-executable-path "racket")
                       (path->string ci-cli-runtime-path)
                       "resolve"
                       "--root"
                       root-arg
                       "--checkout"
                       (path->string checkout-b)
                       "--trusted-label"
                       "q-trusted-producer"
                       "--flags-file"
                       (path->string flags-file)))
  (define (flags-well-formed? path)
    ;; mirrors the launch shape gate: tag + 3 string fields + a list
    ;; of launch-argument strings
    (define d (file->value path))
    (and (list? d)
         (= 5 (length d))
         (eq? (list-ref d 0) 'compiled-root-flags)
         (andmap string? (list (list-ref d 1) (list-ref d 2) (list-ref d 3)))
         (and (list? (list-ref d 4)) (andmap string? (list-ref d 4)))))
  ;; Linux procfs rejects unlink even for privileged test runners, unlike a
  ;; chmod-based fixture. This exercises the actual CLI's unlink error path;
  ;; /proc/version is not a valid flags file and does not establish caller gating.
  (when (file-exists? "/proc/version")
    (test-case "resolve reports an unlink failure"
      (define out (open-output-string))
      (define code
        (parameterize ([current-output-port out]
                       [current-error-port out])
          (run-cli-resolve! (string->path "/proc/version")
                            (path->string (build-path tmp-base "missing-root")))))
      (check-not-equal? code 0)
      (check-true (regexp-match? #rx"delete-file" (get-output-string out)) (get-output-string out))
      (check-true (file-exists? "/proc/version"))))

  (define-runtime-path ci-workflow-path "../.github/workflows/ci.yml")
  (test-case "unlink failure preserves valid stale flags but CI does not select them"
    (define dir (build-path tmp-base "unlink-valid-flags"))
    (make-directory* dir)
    (define flags (build-path dir "flags.rktd"))
    (check-equal? (run-cli-resolve! flags (path->string final-dir)) 0)
    (check-true (flags-well-formed? flags))
    (define original (file->bytes flags))
    (call-with-input-file
     flags
     (lambda (in)
       ;; Locate this process's open descriptor without relying on descriptor
       ;; numbering. A child can follow the parent's procfs link, but cannot
       ;; unlink it. Probe both properties: procfs may be absent or restricted.
       (define proc-self (string->path "/proc/self"))
       (define proc-fd
         (with-handlers ([exn:fail? (lambda (_) #f)])
           (define pid-path (resolve-path proc-self))
           (define fd-dir (build-path "/proc" pid-path "fd"))
           (for/first ([fd (in-list (directory-list fd-dir))]
                       #:when (with-handlers ([exn:fail? (lambda (_) #f)])
                                (equal? (resolve-path (build-path fd-dir fd)) flags)))
             (build-path fd-dir fd))))
       (define usable?
         (and proc-fd
              (with-handlers ([exn:fail? (lambda (_) #f)])
                (and (equal? (file->bytes proc-fd) original)
                     (with-handlers ([exn:fail:filesystem? (lambda (_) #t)])
                       (delete-file proc-fd)
                       #f)))))
       (if (not (eq? (system-type 'os*) 'linux))
           (eprintf "SKIP valid-stale unlink regression: Linux procfs required\n")
           (let ()
             (unless usable?
               (error 'unlink-test "Linux parent procfs fd is unavailable or unlinkable"))
             ;; Execute the actual CI run block, changing only fixture inputs.
             (define workflow (file->string ci-workflow-path))
             (define marker "      - name: Resolve the verified compiled root (W4 activation)")
             (define section (cadr (string-split workflow marker)))
             (define body (cadr (string-split section "        run: |\n")))
             (define block (car (string-split body "\n      #")))
             (define script
               (string-replace
                (string-replace
                 (string-replace
                  block
                  "flags=\"$RUNNER_TEMP/compiled-root-map/shard-${{ matrix.shard }}.rktd\""
                  (format "flags=~s" (path->string proc-fd)))
                 "ci/prepared-environment/compiled-root.rkt"
                 (path->string ci-cli-runtime-path))
                "$PWD"
                (path->string checkout-b)))
             (check-true (string-contains? block "set -euo pipefail"))
             (check-true (string-contains? block "echo \"flags-file=$flags\" >> \"$GITHUB_OUTPUT\""))
             (define script-path (build-path dir "ci-step.sh"))
             (define output-path (build-path dir "github-output"))
             (define log-path (build-path dir "resolve.log"))
             (call-with-output-file script-path (lambda (o) (display script o)))
             (call-with-output-file output-path void)
             (define env (git-plumbing-free-env))
             (environment-variables-set! env #"RUNNER_TEMP" (path->bytes dir))
             (environment-variables-set! env #"GITHUB_OUTPUT" (path->bytes output-path))
             (environment-variables-set! env
                                         #"VERIFIED_ROOT"
                                         (path->bytes (build-path dir "missing-root")))
             (define code
               (call-with-output-file log-path
                                      #:exists 'truncate
                                      (lambda (log)
                                        (parameterize ([current-environment-variables env]
                                                       [current-output-port log]
                                                       [current-error-port log])
                                          (system*/exit-code (find-executable-path "bash")
                                                             script-path)))))
             (check-not-equal? code 0)
             (check-regexp-match #rx"delete-file" (file->string log-path))
             (check-equal? (file->bytes proc-fd) original)
             (check-true (flags-well-formed? proc-fd))
             (check-equal? (file->string output-path) "" "failed CI step must publish no flags")
             ;; GitHub substitutes an absent step output as the empty string.
             (define tel (build-path dir "launch-tel.rktd"))
             (define child (build-path dir "child.rktd"))
             (environment-variables-set! env #"Q_CI_COMPILED_ROOT" #"auto")
             (environment-variables-set! env #"Q_COMPILED_ROOT_FLAGS_FILE" #f)
             (define launch-code
               (parameterize ([current-environment-variables env])
                 (system*/exit-code
                  (find-executable-path "racket")
                  ci-cli-runtime-path
                  "launch"
                  "--flags-file"
                  ""
                  "--telemetry-file"
                  tel
                  "--"
                  "racket"
                  "-e"
                  (format
                   "(call-with-output-file ~s (lambda (o) (write (getenv \"Q_COMPILED_ROOT_FLAGS_FILE\") o)))"
                   (path->string child)))))
             (check-equal? launch-code 0)
             (check-equal? (cdr (assq 'mode (cdr (file->value tel)))) "no-flags-current-source")
             (check-false (file->value child)))))))

  (test-case "a failed resolve invalidates a preexisting flags file (fail-closed)"
    (define old-dir (materialize-old-root! "old-style-root-invalidate"))
    (define flags-dir (build-path tmp-base "inval-flags"))
    (make-directory* flags-dir)
    (define flags-file (build-path flags-dir "flags.rktd"))
    ;; success behavior (must not regress): the verified root resolves
    ;; and writes a well-formed flags file on the shared path
    (check-true (zero? (run-cli-resolve! flags-file (path->string final-dir)))
                "success behavior: the verified root resolves and writes flags")
    (check-true (file-exists? flags-file)
                "success behavior: the flags file exists after a good resolve")
    (check-true (flags-well-formed? flags-file) "success behavior: the flags payload is well-formed")
    ;; the failing old-root resolve over the SAME path must invalidate
    ;; the preexisting file: no stale flags survive a resolve failure
    (check-false (zero? (run-cli-resolve! flags-file (path->string old-dir)))
                 "old-root resolve still fails closed")
    (check-false (file-exists? flags-file)
                 "a failed resolve must invalidate the preexisting flags file")
    ;; success behavior is preserved: a good resolve over the same path
    ;; rewrites a well-formed flags file after the invalidation
    (check-true (zero? (run-cli-resolve! flags-file (path->string final-dir)))
                "success behavior: a good resolve rewrites flags after invalidation")
    (check-true (and (file-exists? flags-file) (flags-well-formed? flags-file))
                "success behavior: the rewritten flags file is well-formed"))
  (test-case "launch cannot consume the flags a failed resolve left behind"
    (define old-dir (materialize-old-root! "old-style-root-launch"))
    (define flags-dir (build-path tmp-base "consume-flags"))
    (make-directory* flags-dir)
    (define flags-file (build-path flags-dir "flags.rktd"))
    (define launch-tel (build-path flags-dir "launch-tel.rktd"))
    (define child-probe (build-path flags-dir "child-probe.rktd"))
    ;; preexisting valid flags, then the failing old-root resolve over
    ;; the SAME path — whatever flags state the failure leaves, launch
    ;; must never map a child from it
    (check-true (zero? (run-cli-resolve! flags-file (path->string final-dir)))
                "precondition: a good resolve writes valid flags")
    (check-false (zero? (run-cli-resolve! flags-file (path->string old-dir)))
                 "precondition: the old-root resolve fails closed")
    ;; launch consumes flags only under an active Q_CI_COMPILED_ROOT
    ;; lane (auto/verify/resolve); run under the auto lane so the
    ;; consumption boundary is actually exercised, not skipped. The
    ;; probe result goes to a FILE — subprocess stdio does not inherit
    ;; parameterized ports. Discriminator: `launch` sets
    ;; Q_COMPILED_ROOT_FLAGS_FILE in the child environment only when it
    ;; maps through a verified flags file.
    (define launch-env (make-environment-variables))
    (for ([n (in-list (environment-variables-names (current-environment-variables)))])
      (environment-variables-set! launch-env
                                  n
                                  (environment-variables-ref (current-environment-variables) n)))
    (environment-variables-set! launch-env #"Q_CI_COMPILED_ROOT" #"auto")
    (define launch-code
      (parameterize ([current-environment-variables launch-env])
        (system*/exit-code
         (find-executable-path "racket")
         (path->string ci-cli-runtime-path)
         "launch"
         "--flags-file"
         (path->string flags-file)
         "--telemetry-file"
         (path->string launch-tel)
         "--"
         "racket"
         "-e"
         (format
          "(call-with-output-file ~s (lambda (o) (write (not (getenv \"Q_COMPILED_ROOT_FLAGS_FILE\")) o)))"
          (path->string child-probe)))))
    (check-true (zero? launch-code) "launch runs the child even after a failed resolve")
    (check-equal? (cdr (assq 'mode (cdr (file->value launch-tel))))
                  "no-flags-current-source"
                  "launch must pass through unmapped, never consume stale flags")
    (check-true (and (file-exists? child-probe) (eq? (file->value child-probe) #t))
                "the child must run unmapped (never bootstrapped from stale flags)"))

  ;; Reviewer blocker (red-first): the flags invalidation handler used to
  ;; be installed only AFTER the remaining required-option checks
  ;; (--root, --checkout) and the string->path/complete-path conversions
  ;; had run. A failure inside that window exited non-zero but left a
  ;; PREEXISTING flags file from an earlier resolve in place — and
  ;; `launch` consumes any existing well-formed file, so children would
  ;; still bootstrap from a root this resolve just rejected. Contract:
  ;; after successful parsing, resolve attempts to invalidate the bound
  ;; flags path on these failures. Parse failures do not invalidate it,
  ;; and an unlink failure can leave it intact. The conversion half of the window cannot be
  ;; triggered through exec argv (a conversion failure needs an
  ;; embedded-NUL path string, which argv cannot carry on this
  ;; platform); the fix moves the conversion inside the same handler the
  ;; probes below pin, so these missing-option cases exercise the shared
  ;; mechanism red-first.
  (test-case "required-option resolve failures invalidate a preexisting flags file"
    (define flags-dir (build-path tmp-base "inval-opt-flags"))
    (make-directory* flags-dir)
    (define flags-file (build-path flags-dir "flags.rktd"))
    ;; success behavior (must not regress): a good resolve writes a
    ;; well-formed flags file on the shared path
    (check-true (zero? (run-cli-resolve! flags-file (path->string final-dir)))
                "success behavior: a good resolve writes flags")
    (check-true (and (file-exists? flags-file) (flags-well-formed? flags-file))
                "success behavior: the flags payload is well-formed")
    ;; a resolve that fails on a missing --root AFTER the flags path is
    ;; known must still invalidate the preexisting file
    (check-false (zero? (apply system*/exit-code
                               (find-executable-path "racket")
                               (list (path->string ci-cli-runtime-path)
                                     "resolve"
                                     "--checkout"
                                     (path->string checkout-b)
                                     "--trusted-label"
                                     "q-trusted-producer"
                                     "--flags-file"
                                     (path->string flags-file))))
                 "resolve without --root must exit non-zero")
    (check-false (file-exists? flags-file)
                 "a resolve failing on missing --root must invalidate the preexisting flags file")
    ;; same contract for a missing --checkout
    (check-true (zero? (run-cli-resolve! flags-file (path->string final-dir)))
                "success behavior: a good resolve rewrites flags")
    (check-false (zero? (apply system*/exit-code
                               (find-executable-path "racket")
                               (list (path->string ci-cli-runtime-path)
                                     "resolve"
                                     "--root"
                                     (path->string final-dir)
                                     "--trusted-label"
                                     "q-trusted-producer"
                                     "--flags-file"
                                     (path->string flags-file))))
                 "resolve without --checkout must exit non-zero")
    (check-false (file-exists? flags-file)
                 "a resolve failing on missing --checkout must invalidate the preexisting flags file")
    ;; the contract composes: the next good resolve still succeeds
    (check-true (zero? (run-cli-resolve! flags-file (path->string final-dir)))
                "success behavior preserved after an invalidating failure")
    (check-true (and (file-exists? flags-file) (flags-well-formed? flags-file))
                "flags file well-formed after the recovery resolve"))

  ;; Parse failures leave paths untouched; only a caller gated on resolve
  ;; success can safely decide whether to pass the flags path to launch.
  (test-case "parse failures preserve preexisting flags without authorizing them"
    (define flags-dir (build-path tmp-base "parse-fail-flags"))
    (make-directory* flags-dir)
    (define flags-file (build-path flags-dir "flags.rktd"))
    ;; success behavior (must not regress): a good resolve writes a
    ;; well-formed flags file on the shared path
    (check-true (zero? (run-cli-resolve! flags-file (path->string final-dir)))
                "success behavior: a good resolve writes flags")
    (check-true (and (file-exists? flags-file) (flags-well-formed? flags-file))
                "success behavior: the flags payload is well-formed")
    ;; A failed parse is non-destructive, regardless of option order.
    (check-false (zero? (apply system*/exit-code
                               (find-executable-path "racket")
                               (list (path->string ci-cli-runtime-path)
                                     "resolve"
                                     "--root"
                                     (path->string final-dir)
                                     "--checkout"
                                     (path->string checkout-b)
                                     "--trusted-label"
                                     "q-trusted-producer"
                                     "--flags-file"
                                     (path->string flags-file)
                                     "--unknown-option")))
                 "resolve with a trailing unknown option must exit non-zero")
    (check-true (flags-well-formed? flags-file))
    ;; The unknown option may also precede the flags option.
    (check-true (zero? (run-cli-resolve! flags-file (path->string final-dir)))
                "success behavior: flags rewritten for the second scenario")
    (check-false (zero? (apply system*/exit-code
                               (find-executable-path "racket")
                               (list (path->string ci-cli-runtime-path)
                                     "resolve"
                                     "--root"
                                     (path->string final-dir)
                                     "--checkout"
                                     (path->string checkout-b)
                                     "--trusted-label"
                                     "q-trusted-producer"
                                     "--unknown-option"
                                     "--flags-file"
                                     (path->string flags-file))))
                 "resolve with a leading unknown option must exit non-zero")
    (check-true (flags-well-formed? flags-file))
    ;; the contract composes: the next good resolve still succeeds
    (check-true (zero? (run-cli-resolve! flags-file (path->string final-dir)))
                "success behavior preserved after a parse failure")
    (check-true (and (file-exists? flags-file) (flags-well-formed? flags-file))
                "flags file well-formed after the parse-failure recovery resolve"))

  ;; Direct CLI launch is not gated: explicitly pin the limitation.
  (test-case "direct launch can consume flags left by a failed parse"
    (define flags-dir (build-path tmp-base "parse-fail-launch-flags"))
    (make-directory* flags-dir)
    (define flags-file (build-path flags-dir "flags.rktd"))
    (define launch-tel (build-path flags-dir "launch-tel.rktd"))
    (define child-probe (build-path flags-dir "child-probe.rktd"))
    ;; Seed valid flags before the unsuccessful resolve.
    (check-true (zero? (run-cli-resolve! flags-file (path->string final-dir)))
                "precondition: a good resolve writes valid flags")
    (check-false (zero? (apply system*/exit-code
                               (find-executable-path "racket")
                               (list (path->string ci-cli-runtime-path)
                                     "resolve"
                                     "--root"
                                     (path->string final-dir)
                                     "--checkout"
                                     (path->string checkout-b)
                                     "--trusted-label"
                                     "q-trusted-producer"
                                     "--flags-file"
                                     (path->string flags-file)
                                     "--unknown-option")))
                 "precondition: the parse-failing resolve exits non-zero")
    ;; launch consumes flags only under an active Q_CI_COMPILED_ROOT
    ;; lane; run under the auto lane so the consumption boundary is
    ;; actually exercised, not skipped. Probe result goes to a FILE —
    ;; subprocess stdio does not inherit parameterized ports.
    (define launch-env (make-environment-variables))
    (for ([n (in-list (environment-variables-names (current-environment-variables)))])
      (environment-variables-set! launch-env
                                  n
                                  (environment-variables-ref (current-environment-variables) n)))
    (environment-variables-set! launch-env #"Q_CI_COMPILED_ROOT" #"auto")
    (define launch-code
      (parameterize ([current-environment-variables launch-env])
        (system*/exit-code
         (find-executable-path "racket")
         (path->string ci-cli-runtime-path)
         "launch"
         "--flags-file"
         (path->string flags-file)
         "--telemetry-file"
         (path->string launch-tel)
         "--"
         "racket"
         "-e"
         (format
          "(call-with-output-file ~s (lambda (o) (write (not (getenv \"Q_COMPILED_ROOT_FLAGS_FILE\")) o)))"
          (path->string child-probe)))))
    (check-true (zero? launch-code) "launch runs the child even after a failed-parse resolve")
    (check-equal? (cdr (assq 'mode (cdr (file->value launch-tel))))
                  "verified-root-hit"
                  "direct launch can consume stale flags; callers must gate it")
    (check-true (and (file-exists? child-probe) (eq? (file->value child-probe) #f))))

  ;; A value that looks like a command cannot authorize deletion.
  (test-case "an option value spelling launch does not trigger speculative deletion"
    (define flags-dir (build-path tmp-base "optval-resolve-flags"))
    (make-directory* flags-dir)
    (define flags-file (build-path flags-dir "flags.rktd"))
    (check-true (zero? (run-cli-resolve! flags-file (path->string final-dir)))
                "precondition: a good resolve writes valid flags")
    (check-false (zero? (apply system*/exit-code
                               (find-executable-path "racket")
                               (list (path->string ci-cli-runtime-path)
                                     "--root"
                                     "launch"
                                     "--flags-file"
                                     (path->string flags-file)
                                     "--unknown-option"
                                     "resolve")))
                 "the misparsed resolve-intent invocation must exit non-zero")
    (check-true (flags-well-formed? flags-file)))

  ;; Companion reviewer blocker (red-first), the mirror direction: in
  ;; `--flags-file F --unknown-option launch` the token "F" is the VALUE
  ;; of --flags-file and "launch" is the trailing command token (flags-
  ;; first order). The same first-non-flag-token heuristic saw the VALUE
  ;; first, found it unequal to "launch", and classified the invocation
  ;; as resolve intent — so the launch-side parse failure DELETED the
  ;; preexisting flags file, violating launch's read-only contract ("a
  ;; launch-side parse failure leaves the file untouched for the
  ;; caller's retry"). Contract: the switch + value pair is skipped, the
  ;; trailing "launch" governs, and the file survives the failed parse —
  ;; still well-formed and consumable by a corrected FLAGS-FIRST launch,
  ;; which must then map the child (verified-root-hit), proving flags-
  ;; first success behavior for both scanner consumers.
  (test-case "a launch-side parse failure leaves the flags file untouched (flags-first)"
    (define flags-dir (build-path tmp-base "optval-launch-flags"))
    (make-directory* flags-dir)
    (define flags-file (build-path flags-dir "flags.rktd"))
    (define launch-tel (build-path flags-dir "launch-tel.rktd"))
    (define child-probe (build-path flags-dir "child-probe.rktd"))
    (check-true (zero? (run-cli-resolve! flags-file (path->string final-dir)))
                "precondition: a good resolve writes valid flags")
    (check-false (zero? (apply system*/exit-code
                               (find-executable-path "racket")
                               (list (path->string ci-cli-runtime-path)
                                     "--flags-file"
                                     (path->string flags-file)
                                     "--unknown-option"
                                     "launch")))
                 "the misparsed launch-intent invocation must exit non-zero")
    (check-true (file-exists? flags-file)
                "a launch-side parse failure must leave the flags file untouched")
    ;; and the file survives INTACT: a corrected FLAGS-FIRST launch (all
    ;; options before the trailing command token) over the same path
    ;; still consumes it and maps the child
    (define launch-env (make-environment-variables))
    (for ([n (in-list (environment-variables-names (current-environment-variables)))])
      (environment-variables-set! launch-env
                                  n
                                  (environment-variables-ref (current-environment-variables) n)))
    (environment-variables-set! launch-env #"Q_CI_COMPILED_ROOT" #"auto")
    (define launch-code
      (parameterize ([current-environment-variables launch-env])
        (system*/exit-code
         (find-executable-path "racket")
         (path->string ci-cli-runtime-path)
         "--flags-file"
         (path->string flags-file)
         "--telemetry-file"
         (path->string launch-tel)
         "launch"
         "--"
         "racket"
         "-e"
         (format
          "(call-with-output-file ~s (lambda (o) (write (not (getenv \"Q_COMPILED_ROOT_FLAGS_FILE\")) o)))"
          (path->string child-probe)))))
    (check-true (zero? launch-code) "the corrected flags-first launch runs the child")
    (check-equal? (cdr (assq 'mode (cdr (file->value launch-tel))))
                  "verified-root-hit"
                  "flags-first launch must classify as launch intent and map the child")
    (check-true (and (file-exists? child-probe) (eq? (file->value child-probe) #f))
                "the child must run MAPPED (bootstrap environment present)"))

  ;; Exercise the real command-line parser and observe the filesystem, rather
  ;; than inferring bindings from token spelling. All fixtures start with two
  ;; distinct sentinels, including the exact switch-as-value binding case.
  (test-case "resolve argv parser binding and failure matrix"
    (define dir (build-path tmp-base "argv-matrix"))
    (make-directory* dir)
    (define a (build-path dir "a"))
    (define b (build-path dir "b"))
    (define as (path->string a))
    (define bs (path->string b))
    (define cases
      (list
       ;; A switch-looking operand is bound to --root; the next occurrence
       ;; binds the flags path, which the resolve failure must invalidate.
       (list "switch value binds exactly"
             (list "resolve" "--root" "--flags-file" "--flags-file" as)
             #f
             #t)
       (list "missing operand" (list "resolve" "--flags-file") #t #t)
       (list "duplicate flags" (list "resolve" "--flags-file" as "--flags-file" bs) #t #t)
       (list "unknown switch" (list "resolve" "--flags-file" as "--unknown") #t #t)
       (list "attached equals" (list "resolve" (string-append "--flags-file=" as)) #t #t)
       (list "option value is command" (list "--root" "resolve" "--flags-file" as "--unknown") #t #t)
       (list "positional stops options" (list "resolve" "extra" "--flags-file" as) #t #t)
       (list "separator hides flags" (list "resolve" "--" "--flags-file" as) #t #t)
       (list "launch parse failure" (list "launch" "--flags-file" as "--unknown") #t #t)))
    (for ([row (in-list cases)])
      (call-with-output-file a #:exists 'truncate/replace (lambda (o) (display "sentinel-a" o)))
      (call-with-output-file b #:exists 'truncate/replace (lambda (o) (display "sentinel-b" o)))
      (define code
        (apply system*/exit-code
               (find-executable-path "racket")
               (path->string ci-cli-runtime-path)
               (cadr row)))
      (check-not-equal? code 0 (car row))
      (for ([p (in-list (list a b))]
            [survives? (in-list (cddr row))]
            [content (in-list '("sentinel-a" "sentinel-b"))])
        (check-equal? (file-exists? p) survives? (car row))
        (when (file-exists? p)
          (check-equal? (file->string p) content (car row))))))

  (test-case "resolve parse failure never deletes an option operand"
    (define dir (build-path tmp-base "argv-victim"))
    (make-directory* dir)
    (define victim (build-path dir "victim"))
    (define flags (build-path dir "flags.rktd"))
    (call-with-output-file victim (lambda (out) (write-bytes #"victim sentinel\0" out)))
    (check-equal? (run-cli-resolve! flags (path->string final-dir)) 0)
    (define original (file->bytes flags))
    (define argv
      (list "resolve"
            "--root"
            "--flags-file"
            (path->string victim)
            "--flags-file"
            (path->string flags)
            "--unknown-option"))
    ;; Mirror the CLI's leading-command reorder and use the same Racket
    ;; command-line parser: the victim starts the positional tail, so the
    ;; later switch is not parsed as an option at all.
    (define bound-root #f)
    (define bound-flags #f)
    (define rest
      (command-line #:argv (append (cdr argv) (list (car argv)))
                    #:once-each ["--root" d "root" (set! bound-root d)]
                    ["--flags-file" f "flags" (set! bound-flags f)]
                    #:args args
                    args))
    (check-equal? bound-root "--flags-file")
    (check-false bound-flags)
    (check-equal?
     rest
     (list (path->string victim) "--flags-file" (path->string flags) "--unknown-option" "resolve"))
    (define log (build-path dir "resolve.log"))
    (define code
      (call-with-output-file log
                             #:exists 'truncate
                             (lambda (out)
                               (parameterize ([current-output-port out]
                                              [current-error-port out])
                                 (apply system*/exit-code
                                        (find-executable-path "racket")
                                        (path->string ci-cli-runtime-path)
                                        argv)))))
    (check-not-equal? code 0)
    (check-regexp-match #rx"missing required option --flags-file" (file->string log))
    (check-equal? (file->bytes victim) #"victim sentinel\0")
    (check-equal? (file->bytes flags) original)
    (check-true (flags-well-formed? flags))
    ;; Run the workflow's actual gate with only its resolve invocation and
    ;; fixture paths substituted. This tests the caller boundary, not GitHub
    ;; Actions execution or a claim that CI emits this malformed argv.
    (define workflow (file->string ci-workflow-path))
    (define section
      (cadr (string-split workflow
                          "      - name: Resolve the verified compiled root (W4 activation)")))
    (define block (car (string-split (cadr (string-split section "        run: |\n")) "\n      #")))
    (define invocation
      (string-append "racket ci/prepared-environment/compiled-root.rkt resolve \\\n"
                     "            --root \"$VERIFIED_ROOT\" \\\n"
                     "            --checkout \"$PWD\" \\\n"
                     "            --trusted-label q-trusted-producer \\\n"
                     "            --flags-file \"$flags\""))
    (check-true (string-contains? block invocation))
    (define replacement
      (string-join (map (lambda (s) (format "~s" s))
                        (append (list (path->string (find-executable-path "racket"))
                                      (path->string ci-cli-runtime-path))
                                argv))
                   " "))
    (define script (build-path dir "step.sh"))
    (define output (build-path dir "github-output"))
    (call-with-output-file
     script
     (lambda (out)
       (display
        (string-replace (string-replace block invocation replacement)
                        "flags=\"$RUNNER_TEMP/compiled-root-map/shard-${{ matrix.shard }}.rktd\""
                        (format "flags=~s" (path->string flags)))
        out)))
    (call-with-output-file output void)
    (define env (environment-variables-copy (current-environment-variables)))
    (environment-variables-set! env #"RUNNER_TEMP" (path->bytes dir))
    (environment-variables-set! env #"GITHUB_OUTPUT" (path->bytes output))
    (define step-log (build-path dir "step.log"))
    (define step-code
      (call-with-output-file step-log
                             #:exists 'truncate
                             (lambda (out)
                               (parameterize ([current-environment-variables env]
                                              [current-output-port out]
                                              [current-error-port out])
                                 (system*/exit-code (find-executable-path "bash") script)))))
    (check-not-equal? step-code 0)
    (check-regexp-match #rx"missing required option --flags-file" (file->string step-log))
    (check-equal? (file->string output) "" "failed workflow step publishes no flags path")
    (check-equal? (file->bytes flags) original))

  (test-case "plus unknown switch before launch preserves usable flags"
    (define dir (build-path tmp-base "argv-plus-launch"))
    (make-directory* dir)
    (define flags (build-path dir "flags.rktd"))
    (define tel (build-path dir "telemetry.rktd"))
    (check-equal? (run-cli-resolve! flags (path->string final-dir)) 0)
    (define before (file->bytes flags))
    (define code
      (system*/exit-code (find-executable-path "racket")
                         (path->string ci-cli-runtime-path)
                         "--flags-file"
                         (path->string flags)
                         "+unknown"
                         "launch"))
    (check-not-equal? code 0)
    (check-true (file-exists? flags) "failed launch must not delete flags")
    (when (file-exists? flags)
      (check-equal? (file->bytes flags) before)
      (define env (environment-variables-copy (current-environment-variables)))
      (environment-variables-set! env #"Q_CI_COMPILED_ROOT" #"auto")
      (check-equal? (parameterize ([current-environment-variables env])
                      (system*/exit-code (find-executable-path "racket")
                                         (path->string ci-cli-runtime-path)
                                         "launch"
                                         "--flags-file"
                                         (path->string flags)
                                         "--telemetry-file"
                                         (path->string tel)
                                         "--"
                                         "racket"
                                         "-e"
                                         "(void)"))
                    0)
      (check-equal? (cdr (assq 'mode (cdr (file->value tel)))) "verified-root-hit")))

  ;; Reviewer blocker (red-first): the stale-link replacement in the
  ;; resolver probed only file-exists?, which FOLLOWS links — a stale
  ;; DANGLING link (its target vanished since a previous resolve:
  ;; rebuilt/republished payload, GCed temp tree) is invisible to that
  ;; probe, so the delete was skipped and the recreate raised
  ;; exn:fail:filesystem:exists on the existing link path. resolve then
  ;; failed on EVERY re-entry with a shared map dir and could never
  ;; recover. The replacement delete must gate on link-exists? as well.
  (test-case "resolve replaces a dangling map symlink (link-exists? pin)"
    (define checkout-dl (make-checkout! "consumer-dangling" 'producer-fixture))
    (define map-dir-dl (build-path tmp-base "map-dangling"))
    (define src-dl (marker-abs-path checkout-dl))
    (define lookup-dir-dl (build-path map-dir-dl (substring (path->string src-dl) 1) "compiled"))
    (make-directory* lookup-dir-dl)
    (define link-path-dl (build-path lookup-dir-dl (source->zo-name (file-name-from-path src-dl))))
    ;; plant a stale DANGLING link exactly where the resolver maps: the
    ;; target path is deliberately nonexistent (a vanished payload)
    (make-file-or-directory-link (path->string (build-path tmp-base "vanished-root" "payload_rkt.zo"))
                                 link-path-dl)
    (check-true (link-exists? link-path-dl) "fixture: the planted link exists")
    (check-false (file-exists? link-path-dl)
                 "fixture precondition: the planted link dangles (file-exists? misses it)")
    (define root-dl
      (resolve-compiled-root! #:final-dir final-dir
                              #:checkout checkout-dl
                              #:trusted-producer-labels '("q-trusted-producer")
                              #:map-dir map-dir-dl))
    (check-true (eq? (compiled-root-reason root-dl) #t)
                (format "resolve must replace the dangling link, not fail: ~a"
                        (reason->string (compiled-root-reason root-dl))))
    (check-true (file-exists? link-path-dl) "the link path must serve the payload after the remap")
    (define payload-zo-dl
      (build-path final-dir
                  (hash-ref (car (hash-ref (compiled-root-manifest root-dl) 'sources)) 'zo)))
    (check-equal? (resolve-path link-path-dl)
                  (simplify-path (path->complete-path payload-zo-dl))
                  "the dangling link must be replaced with the correct payload target")))
