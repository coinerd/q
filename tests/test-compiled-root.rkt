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
  ;; ------------- W1a install-map repair: two verified map roots -----------
  ;;
  ;; PLAN-W1-INSTALL-MAP.md binding decisions, pinned red-first here:
  ;;   1. launch roots are exactly (list checkout-map install-map) —
  ;;      absolute paths, map-first; the -e bootstrap and the flags payload
  ;;      agree with the in-process activation values;
  ;;   2. the install map is a job-local, read-only, resolve-time
  ;;      materialization of the installation's source-adjacent compiled
  ;;      directories under the PROBE-VERIFIED reroot layout (R/<abs source
  ;;      dir minus the leading slash>/compiled/<munged>.zo, per-zo
  ;;      symlinks); a wrong-layout root (dropped path components) never
  ;;      serves;
  ;;   3. per-load sha256 digest checks extend to install-served loads;
  ;;      missing keyed members fail closed at activation;
  ;;   4. the fallback lane is PLAIN LEGACY Racket: default parameter
  ;;      values (no '() roots parameterize, no 'exists pin);
  ;;   5. per-root telemetry counters (checkout-map/install-map hits,
  ;;      unmapped loads) are additive; the five-field envelope struct and
  ;;      the launch record's existing fields are unchanged;
  ;;   6. direct-zo loads miss the guard tables and stale source-adjacent
  ;;      install zos are not served under activation (roots exclude
  ;;      'same); the 'exists pin's global effect is documented at the
  ;;      pin sites;
  ;;   7. an install-served closure loads fast relative to the same-tree
  ;;      from-source baseline (relative pin, never absolute wall clock).
  (define install-base (make-temporary-file "qcr-install~a" 'directory))

  ;; A fake installation collection with a two-module closure. Sources are
  ;; rewritten AFTER compilation, so the zo bytes carry INSTALL-MARKER
  ;; while the on-disk sources carry FINAL-MARKER: the loaded binding
  ;; identifies the serving origin deterministically (no timing, no
  ;; sleeps). Each case gets its OWN collection instance — module identity
  ;; in the test namespace's registry is keyed on the absolute source
  ;; path, so serving observations must never share paths.
  (define (make-install-coll! name install-marker final-marker)
    (define coll (build-path install-base name "mylib"))
    (make-directory* coll)
    (define beta (build-path coll "beta.rkt"))
    (define alpha (build-path coll "alpha.rkt"))
    (define (write-src! p marker requires?)
      (call-with-output-file p
                             #:exists 'truncate
                             (lambda (o)
                               (fprintf o
                                        (string-append "#lang racket/base\n"
                                                       (if requires? "(require \"beta.rkt\")\n" "")
                                                       "(provide iv bv)\n"
                                                       "(define bv '~a)\n"
                                                       "(define iv (list '~a bv))\n")
                                        marker
                                        marker))))
    (write-src! beta install-marker #f)
    (write-src! alpha install-marker #t)
    (unless (zero? (system*/exit-code (find-executable-path "racket")
                                      "-l"
                                      "raco"
                                      "-l-"
                                      "raco/main.rkt"
                                      "make"
                                      "-j"
                                      "1"
                                      alpha))
      (error 'fixture "raco make failed for the install closure ~a" alpha))
    (write-src! alpha final-marker #t)
    (write-src! beta final-marker #f)
    (list coll alpha beta))

  (define (install-zo coll src-name)
    (build-path coll "compiled" (source->zo-name (string->path (string-append src-name ".rkt")))))

  ;; Deterministic fixture freshness: make-install-coll! rewrites the
  ;; sources milliseconds AFTER raco make, so a rewrite that straddles a
  ;; clock second can flip a freshness-gated mapped load to source bytes
  ;; on the legacy lane (second-truncated mtimes). Stamp every fixture
  ;; source AT its install-map zo's mtime: src <= zo is fresh under ANY
  ;; reading of the loader's candidate selection, and the content is
  ;; untouched — the final-marker sources still discriminate served-zo
  ;; bytes from a fresh compilation.
  (define (stamp-install-fresh! colls)
    (for ([src (in-list (cdr colls))]
          [nm (in-list '("alpha" "beta"))])
      (file-or-directory-modify-seconds src
                                        (file-or-directory-modify-seconds (install-zo (car colls)
                                                                                      nm)))))

  ;; Resolve against the published checkout root with a REAL install map.
  ;; The collection roots are injected (the fixture's fake installation is
  ;; not on the real collection search path); the resolver's enumeration
  ;; layout and linking mechanism are the production ones.
  (define (resolve-with-install! name colls)
    (define imd (build-path install-base (string-append name "-install-map")))
    (define r
      (resolve-compiled-root! #:final-dir final-dir
                              #:checkout checkout-b
                              #:trusted-producer-labels '("q-trusted-producer")
                              #:map-dir (build-path install-base (string-append name "-cmap"))
                              #:install-map-dir imd
                              #:collection-roots (list (car colls))))
    (values r imd))

  ;; substring position helper for the bootstrap-string order pins
  (define (string-index-of haystack needle)
    (let loop ([i 0])
      (cond
        [(> (+ i (string-length needle)) (string-length haystack)) #f]
        [(equal? (substring haystack i (+ i (string-length needle))) needle) i]
        [else (loop (add1 i))])))

  (test-case "install map builds read-only per-zo symlinks under the real layout"
    (define colls (make-install-coll! "layout" 'install-layout 'source-layout))
    (define coll (list-ref colls 0))
    (define-values (r imd) (resolve-with-install! "layout" colls))
    (check-true (eq? (compiled-root-reason r) #t) (reason->string (compiled-root-reason r)))
    ;; per-zo links exist at R/<abs-source-dir-minus-slash>/compiled/<munged>
    (define lookup-dir (build-path imd (substring (path->string coll) 1) "compiled"))
    (for ([n (in-list '("alpha_rkt.zo" "beta_rkt.zo"))])
      (check-true (link-exists? (build-path lookup-dir n)) (format "missing link ~a" n))
      (check-true (file-exists? (build-path lookup-dir n)) (format "dangling link ~a" n)))
    ;; the map-owned entries are read-only, like the published payload
    ;; (LINK entries are names, not bytes: their targets live in the
    ;; installation and carry the installation's own permissions — the
    ;; map can only ever serve inventoried targets)
    (for ([p (in-list (cons imd (find-files (lambda (_) #t) imd)))]
          #:unless (link-exists? p))
      (check-false (member 'write (file-or-directory-permissions p))
                   (format "install map member is writable: ~a" p))))

  (test-case "roots shape pin: exactly two verified maps, absolute, map-first"
    (define colls (make-install-coll! "shape" 'install-shape 'source-shape))
    (define-values (r imd) (resolve-with-install! "shape" colls))
    (check-true (eq? (compiled-root-reason r) #t) (reason->string (compiled-root-reason r)))
    (define-values (roots mode) (compiled-root-activate-parameters r))
    (check-equal? mode 'exists)
    ;; exactly the two verified maps — never 'same, never defaults consulted
    (check-equal? roots (list (compiled-root-map-dir r) imd))
    (check-true (andmap absolute-path? roots) (format "non-absolute roots: ~s" roots))
    (check-equal? (list-ref roots 0) (compiled-root-map-dir r) "checkout map must be FIRST")
    ;; the -e bootstrap carries the same two roots in the same order
    (define boot (cadr (compiled-root-launch-arguments r)))
    (define cmap-at
      (string-index-of boot
                       (path->string (simplify-path (path->complete-path (compiled-root-map-dir
                                                                          r))))))
    (define imap-at (string-index-of boot (path->string (simplify-path (path->complete-path imd)))))
    (check-not-false cmap-at "bootstrap must mention the checkout map")
    (check-not-false imap-at "bootstrap must mention the install map")
    (check-true (< cmap-at imap-at) "map-first order must hold in the -e bootstrap")
    (check-false (string-contains? boot "'same") "bootstrap must never restore 'same"))

  (test-case "install map serves with the real reroot layout; wrong layout never serves"
    (define colls (make-install-coll! "serving" 'install-serving 'source-serving))
    (stamp-install-fresh! colls)
    (define alpha-src (list-ref colls 1))
    (define-values (r imd) (resolve-with-install! "serving" colls))
    (reset-telemetry!)
    (define hits-before (compiled-root-telemetry-root-hits (telemetry-snapshot)))
    (with-compiled-root
     r
     checkout-b
     (list "marker/compiled-root-marker.rkt")
     (lambda ()
       (parameterize ([current-load-relative-directory (path->directory-path checkout-b)])
         (check-equal? (dynamic-require alpha-src 'iv)
                       (list 'install-serving 'install-serving)
                       "the install map zo must serve the install-marker bytes"))))
    (define tel (telemetry-snapshot))
    (check-true (> (compiled-root-telemetry-root-hits tel) hits-before)
                "TOCTOU guard observed no root-served load")
    ;; NOTE: no "no adjacent compiled dir appeared" pin — unlike the
    ;; checkout fixture, the installation fixture's source-adjacent
    ;; compiled/ directory legitimately pre-exists (it is what the
    ;; inventory was built from); the marker equality above is the
    ;; serving discriminator.
    ;; WRONG layout: a root whose member is linked with the absolute path
    ;; prefix DROPPED is never consulted — the probe-verified negative
    ;; direction, pinned so the serving fixture can never regress to it
    ;; (the old /tmp/q-smoke6-probe fixture layout was wrong exactly here).
    (define wrong-colls (make-install-coll! "wronglayout" 'install-wrong 'source-wrong))
    (define wrong-root (build-path install-base "wrong-layout-root"))
    (define wrong-lookup (build-path wrong-root "mylib" "compiled"))
    (make-directory* wrong-lookup)
    (make-file-or-directory-link
     (path->string (simplify-path (path->complete-path (install-zo (list-ref wrong-colls 0)
                                                                   "alpha"))))
     (build-path wrong-lookup "alpha_rkt.zo"))
    (parameterize ([current-compiled-file-roots (list wrong-root)]
                   [use-compiled-file-check 'exists])
      (check-equal? (dynamic-require (list-ref wrong-colls 1) 'iv)
                    (list 'source-wrong 'source-wrong)
                    "a dropped-prefix layout member must NOT serve")))

  (test-case "install-member digest mutation fails closed at load time"
    (define colls (make-install-coll! "mutate" 'install-mutate 'source-mutate))
    (stamp-install-fresh! colls)
    (define alpha-src (list-ref colls 1))
    (define-values (r imd) (resolve-with-install! "mutate" colls))
    ;; flip a byte in the INSTALL zo (the link target) AFTER resolve-time
    ;; verification — the load-time canary must reject it
    (define zo (install-zo (list-ref colls 0) "alpha"))
    (unless (zero? (system*/exit-code (find-executable-path "chmod") "u+w" zo))
      (error 'fixture "could not chmod +w the install zo"))
    (define orig-bytes (file->bytes zo))
    (with-output-to-file zo
                         #:exists 'truncate/replace
                         (lambda ()
                           (define b (bytes-copy orig-bytes))
                           (bytes-set! b 0 (bitwise-xor (bytes-ref b 0) #xFF))
                           (write-bytes b)))
    (check-exn #rx"payload mutated after verification"
               (lambda ()
                 (with-compiled-root r
                                     checkout-b
                                     (list "marker/compiled-root-marker.rkt")
                                     (lambda () (dynamic-require alpha-src 'iv)))))
    ;; restore for later readers
    (with-output-to-file zo #:exists 'truncate/replace (lambda () (write-bytes orig-bytes)))
    (system*/exit-code (find-executable-path "chmod") "a-w" zo))

  (test-case "a removed install map member fails closed to the legacy fallback lane"
    (define colls (make-install-coll! "missing" 'install-missing 'source-missing))
    (define alpha-src (list-ref colls 1))
    (define beta-src (list-ref colls 2))
    (define coll (list-ref colls 0))
    (define-values (r imd) (resolve-with-install! "missing" colls))
    (define lookup-dir (build-path imd (substring (path->string coll) 1) "compiled"))
    ;; the map dir is read-only; deleting a LINK requires write access to
    ;; its containing directory — grant it for the removal only, then
    ;; observe the fail-closed activation on the now-missing member
    (unless (zero? (system*/exit-code (find-executable-path "chmod") "u+w" lookup-dir))
      (error 'fixture "could not chmod +w the install lookup dir"))
    (delete-file (build-path lookup-dir "alpha_rkt.zo"))
    ;; Deterministic legacy-lane freshness (B1): make-install-coll!
    ;; rewrites both sources milliseconds AFTER raco make, so with
    ;; second-truncated mtimes a rewrite that straddles a clock second
    ;; makes the source-adjacent zos look stale to the legacy lane's
    ;; 'modify-seconds freshness comparison (zo-mtime >= src-mtime) —
    ;; Racket would lazily recompile and serve 'source-missing bytes
    ;; instead of the zos' 'install-missing bytes (a fixture race, not a
    ;; behavior change). Stamp both sources AT the adjacent zo mtime —
    ;; the stamp-sources-newer! pattern from the fresh-clone pin above,
    ;; inverted (src <= zo keeps the zo fresh) — so the strict equality
    ;; below is deterministic. The stamp is reading-neutral under the
    ;; activation pin: per the header's corrected invariant (and the
    ;; fresh-clone pin in this same file), a candidate under the pinned
    ;; 'exists check is served regardless of mtimes, so the stamp only
    ;; removes legacy-lane nondeterminism. The equality is deliberately
    ;; NOT loosened to tolerate 'source-missing: serving source bytes on
    ;; this lane is exactly the regression the NEXT test-case pins red
    ;; (a fallback lane that parameterized roots to '() compiled and
    ;; then never consulted the eager output).
    (for ([src (in-list (list alpha-src beta-src))]
          [nm (in-list '("alpha" "beta"))])
      (file-or-directory-modify-seconds src (file-or-directory-modify-seconds (install-zo coll nm))))
    (reset-telemetry!)
    (with-compiled-root
     r
     checkout-b
     (list "marker/compiled-root-marker.rkt")
     (lambda ()
       (parameterize ([current-load-relative-directory (path->directory-path checkout-b)])
         ;; the activation REFUSED the root (a keyed mapped member is
         ;; missing); the thunk ran on the PLAIN LEGACY fallback lane, so
         ;; the load is served from the installation's own source-adjacent
         ;; compiled form — install-missing bytes, exactly like legacy
         ;; Racket serves a fresh source-adjacent zo. The FAIL-CLOSED
         ;; observable is the eager-fallback selection pinned below.
         (check-equal? (dynamic-require alpha-src 'iv)
                       (list 'install-missing 'install-missing)
                       "the refused activation must fall back to legacy serving"))))
    (check-true (>= (compiled-root-telemetry-fallbacks (telemetry-snapshot)) 1)
                "the missing-member activation failure must select the eager fallback"))

  (test-case "fallback lane restores plain legacy defaults (no '() roots, no 'exists pin)"
    ;; RED against the pre-repair implementation, which parameterized
    ;; roots to '() (no-compiled semantics) — the eager raco make output
    ;; of the fallback lane was compiled and then never consulted.
    (define unresolved-root (compiled-root #f #f #f checkout-b "fixture-unresolved"))
    (parameterize ([current-compiled-file-roots '(same)]
                   [use-compiled-file-check 'modify-seconds])
      (with-compiled-root unresolved-root
                          checkout-b
                          (list "marker/compiled-root-marker.rkt")
                          (lambda ()
                            (check-equal? (current-compiled-file-roots)
                                          '(same)
                                          "the fallback lane must keep the ambient legacy roots")
                            (check-equal? (use-compiled-file-check)
                                          'modify-seconds
                                          "the fallback lane must keep the ambient check mode"))))
    ;; the eager fallback compile's output is source-adjacent and the
    ;; legacy defaults consult it: the fallback lane behaves exactly like
    ;; legacy Racket
    (define fallback-src (build-path install-base "fallback-mod.rkt"))
    (call-with-output-file
     fallback-src
     #:exists 'truncate
     (lambda (o) (display "#lang racket/base\n(provide fv)\n(define fv 'fallback-v)\n" o)))
    (eager-fallback-compile! install-base (list "fallback-mod.rkt"))
    (check-true (file-exists? (build-path install-base "compiled" "fallback-mod_rkt.zo"))
                "eager fallback must compile source-adjacent")
    (parameterize ([current-compiled-file-roots '(same)]
                   [use-compiled-file-check 'modify-seconds])
      (with-compiled-root unresolved-root install-base (list "fallback-mod.rkt") void)
      ;; with the legacy defaults restored, the eager output is usable:
      ;; a fresh load consults the source-adjacent compiled form (identity
      ;; is the observable marker value; the parameters pin above carries
      ;; the contract)
      (check-equal? (dynamic-require fallback-src 'fv) 'fallback-v)))

  (test-case "per-root telemetry: new counters present, old fields unchanged"
    (define colls (make-install-coll! "telemetry" 'install-tel 'source-tel))
    (stamp-install-fresh! colls)
    (define alpha-src (list-ref colls 1))
    (define-values (r imd) (resolve-with-install! "telemetry" colls))
    ;; an unmapped probe source: a real file in neither table
    (define unmapped-src (build-path install-base "unmapped-probe.rkt"))
    (call-with-output-file unmapped-src
                           #:exists 'truncate
                           (lambda (o) (display "(module unmapped-probe racket/base (void))" o)))
    (reset-telemetry!)
    (with-compiled-root
     r
     checkout-b
     (list "marker/compiled-root-marker.rkt")
     (lambda ()
       (parameterize ([current-load-relative-directory (path->directory-path checkout-b)])
         ;; a FRESH namespace: the checkout marker was loaded earlier in
         ;; this file's registry, and a cached module never reaches
         ;; current-load (so it can never be counted)
         (parameterize ([current-namespace (make-base-namespace)])
           (check-equal? (dynamic-require (marker-abs-path checkout-b) 'marker-result)
                         'producer-fixture))
         (check-equal? (dynamic-require alpha-src 'iv) (list 'install-tel 'install-tel))
         (dynamic-require unmapped-src #f))))
    (define tel (telemetry-snapshot))
    ;; OLD fields unchanged: same struct, same semantics
    (check-true (> (compiled-root-telemetry-root-hits tel) 0) "no root hits counted")
    (check-true (> (compiled-root-telemetry-loads tel) 0) "no loads counted")
    (check-true (> (compiled-root-telemetry-bytes-served tel) 0) "no bytes counted")
    (check-equal? (compiled-root-telemetry-fallbacks tel) 0)
    (check-equal? (compiled-root-telemetry-mechanism tel) "external-compiled-root(map-dir symlinks)")
    ;; NEW per-root counters, additive
    (define mc (telemetry-map-counters))
    (check-true (>= (compiled-root-map-counters-checkout-map mc) 1)
                "checkout-map hits must be counted")
    (check-true (>= (compiled-root-map-counters-install-map mc) 2)
                "install-map hits must be counted (alpha + beta closure)")
    (check-true (>= (compiled-root-map-counters-unmapped mc) 1) "unmapped loads must be counted"))

  (test-case "direct-zo and stale-zo behaviors unchanged under activation"
    (define colls (make-install-coll! "stale" 'install-stale 'source-stale))
    (define alpha-src (list-ref colls 1))
    (define-values (r imd) (resolve-with-install! "stale" colls))
    (define lookup-dir (build-path imd (substring (path->string (list-ref colls 0)) 1) "compiled"))
    ;; direct zo load through the MAP path: BETA is the direct-load
    ;; member (no relative requires — a direct zo load resolves a
    ;; module's relative requires against the ZO's own path, where no
    ;; source tree exists). A mapped-path load IS a root-served load:
    ;; the guard digest-checks it (by-zo twin table) and counts it —
    ;; install members gain direct-load protection checkout members
    ;; deliberately never had (that checkout behavior is pinned
    ;; unchanged in the earlier lookup tests).
    (define imc-before (compiled-root-map-counters-install-map (telemetry-map-counters)))
    (with-compiled-root r
                        checkout-b
                        '()
                        (lambda ()
                          (parameterize ([current-compiled-file-roots (list (compiled-root-map-dir r)
                                                                            imd)]
                                         [use-compiled-file-check 'exists])
                            (dynamic-require (build-path lookup-dir "beta_rkt.zo") #f))))
    (define imc-after (compiled-root-map-counters-install-map (telemetry-map-counters)))
    (check-true (>= (- imc-after imc-before) 1)
                "the direct mapped-zo load must be digest-checked and counted")
    ;; stale-zo: under activation the roots EXCLUDE 'same, so the load
    ;; is served through the install-map member (the imd link), never
    ;; through a source-adjacent 'same candidate — the marker identity
    ;; below proves the map member serves
    (define stale-colls (make-install-coll! "stale2" 'install-stale2 'source-stale2))
    (define stale-alpha (list-ref stale-colls 1))
    (define-values (stale-r stale-imd) (resolve-with-install! "stale2x" stale-colls))
    ;; Deterministic serving premise (B1 twin, equal-mtime stamp): the
    ;; fixture's post-make source rewrites land milliseconds after the
    ;; zos, and a rewrite straddling a clock second could flip
    ;; legacy-lane serving to source bytes. Stamp both sources AT the
    ;; adjacent zo mtime so the equality below is deterministic. The
    ;; pin under test stays structural: under the activation 'exists
    ;; pin the map serves regardless of mtimes (header corrected
    ;; invariant; corroborated by the fresh-clone pin in this same
    ;; file, where sources are stamped +60 s NEWER and the root member
    ;; is still served with no lazy recompile), and the map serves
    ;; because roots exclude 'same — no mtime was aged to make it so.
    (for ([src (in-list (list stale-alpha (list-ref stale-colls 2)))]
          [nm (in-list '("alpha" "beta"))])
      (file-or-directory-modify-seconds
       src
       (file-or-directory-modify-seconds (install-zo (list-ref stale-colls 0) nm))))
    (with-compiled-root stale-r
                        checkout-b
                        '()
                        (lambda ()
                          (check-equal? (dynamic-require stale-alpha 'iv)
                                        (list 'install-stale2 'install-stale2)
                                        "the map member must serve, not any adjacent candidate"))))

  (test-case "install-served closure loads fast relative to the same-tree baseline"
    ;; bounded multi-module install closure (8-module chain): the
    ;; install-served child must beat the from-source baseline child for
    ;; the SAME tree. Relative pin only — never an absolute wall-clock
    ;; promise. The deterministic serving proof is the launcher record's
    ;; install-map-hits counter; the timing bound is the secondary smoke.
    (define pcoll (build-path install-base "chain" "mylib"))
    (make-directory* pcoll)
    (define (chain-src i)
      (format "#lang racket/base\n(provide c~a)\n~a(define c~a ~a)\n"
              i
              (if (zero? i)
                  ""
                  (format "(require \"chain-~a.rkt\")\n" (sub1 i)))
              i
              i))
    (for ([i (in-range 8)])
      (call-with-output-file (build-path pcoll (format "chain-~a.rkt" i))
                             #:exists 'truncate
                             (lambda (o) (display (chain-src i) o))))
    (unless (zero? (system*/exit-code (find-executable-path "racket")
                                      "-l"
                                      "raco"
                                      "-l-"
                                      "raco/main.rkt"
                                      "make"
                                      "-j"
                                      "1"
                                      (build-path pcoll "chain-7.rkt")))
      (error 'fixture "raco make failed for the chain closure"))
    (define chain-head (build-path pcoll "chain-7.rkt"))
    (define imd-chain (build-path install-base "chain-install-map"))
    (define chain-root
      (resolve-compiled-root! #:final-dir final-dir
                              #:checkout checkout-b
                              #:trusted-producer-labels '("q-trusted-producer")
                              #:map-dir (build-path install-base "chain-cmap")
                              #:install-map-dir imd-chain
                              #:collection-roots (list pcoll)))
    (check-true (eq? (compiled-root-reason chain-root) #t)
                (reason->string (compiled-root-reason chain-root)))
    (define (timed-child! roots-expr)
      (define out (build-path install-base "chain-child-result.txt"))
      (when (file-exists? out)
        (delete-file out))
      (define expr
        (string-append "(current-compiled-file-roots "
                       roots-expr
                       ") "
                       "(use-compiled-file-check 'exists) "
                       ;; the require is wrapped in (void ...): racket -e
                       ;; prints every non-void top-level result, and a
                       ;; bare (dynamic-require ... 'c7) would leak the
                       ;; loaded value (\"7\" per child) onto the suite's
                       ;; stdout
                       "(void (dynamic-require (string->path ~s) 'c7)) "
                       "(call-with-output-file ~s (lambda (o) (write 'ran o)))"))
      (define full (format expr (path->string chain-head) (path->string out)))
      (define t0 (current-inexact-milliseconds))
      (define code (system*/exit-code (find-executable-path "racket") "-e" full))
      (values code (- (current-inexact-milliseconds) t0) (and (file-exists? out) 'ran)))
    ;; warm-up children absorb OS cache effects (no sleeps, no measurement
    ;; before warm-up). Measured runs (min-of-3 per side) keep the relative
    ;; pin noise-robust on a loaded machine: a single pair can flip on
    ;; scheduler noise (measured 1235 vs 1231 ms), while the minimum of
    ;; repeated runs separates the map-served child from the from-source
    ;; baseline deterministically (no sleeps anywhere).
    (timed-child! "'()")
    (timed-child! (format "(list (string->path ~s))" (path->string imd-chain)))
    (define base-ms
      (for/fold ([min-ms +inf.0]) ([_ (in-range 3)])
        (define-values (base-code base-ms base-ran) (timed-child! "'()"))
        (unless (and (zero? base-code) (eq? base-ran 'ran))
          (error 'chain-timing "baseline child failed: exit ~a ran ~a" base-code base-ran))
        (min min-ms base-ms)))
    (define served-ms
      (for/fold ([min-ms +inf.0]) ([_ (in-range 3)])
        (define-values (served-code served-ms served-ran)
          (timed-child! (format "(list (string->path ~s))" (path->string imd-chain))))
        (unless (and (zero? served-code) (eq? served-ran 'ran))
          (error 'chain-timing "served child failed: exit ~a ran ~a" served-code served-ran))
        (min min-ms served-ms)))
    ;; Ratio bound with slack, not strict “must beat”: the 8-module toy
    ;; chain's compile-vs-serve delta is a few ms on this host, smaller
    ;; than min-of-3 scheduler noise (measured flips at 1170 vs 1139),
    ;; while the regression this pin guards against is the ~80 s
    ;; from-source closure compilation — orders of magnitude above the
    ;; bound either way. The mechanism itself is pinned structurally by
    ;; the serving/counter tests above.
    (check-true
     (<= served-ms (* base-ms 2.0))
     (format
      "install-served load must stay within 2x of the same-tree baseline: ~a ms served vs ~a ms baseline"
      (real->decimal-string served-ms 0)
      (real->decimal-string base-ms 0))))

  ;; ------------- Round-3: install-map enumeration loudness -------------
  ;;
  ;; PLAN-A-install-map-loudness.md (amended, review round 1), pinned
  ;; red-first here:
  ;;   1. install-collection-roots sets an install-enumeration-diag record
  ;;      (complete skip counts, capped detail lists): a skipped link
  ;;      file, a rejected entry and an unlistable 'root dir are counted;
  ;;      non-candidates (e.g. files under a 'root dir) are not skips;
  ;;   2. build-install-map! emits the enumeration line + skip details
  ;;      from that record, or the injected-roots line when the roots
  ;;      came in via #:collection-roots, plus one per-root walk line
  ;;      BEFORE the inventory loop — every new line carries the
  ;;      ci-compiled-root: prefix;
  ;;   3. a walk failure on a declared root is COUNTED-AND-LOUD: resolve
  ;;      still succeeds (a coverage gap, never a fatal resolve failure —
  ;;      the fail-closed integrity steps are untouched);
  ;;   4. resolve parameterizes the enumeration record away: injected
  ;;      roots never observe a stale record, nothing leaks outward.
  (define enum-base (make-temporary-file "qcr-enum~a" 'directory))

  ;; chmod 000 blocks non-root descent (uid-1000 CI runners); a root
  ;; runner would sail through, so probe the actual behavior once and
  ;; guard the permission fixtures on it (dynamic-wind restores the
  ;; permissions even on assertion failure).
  (define (chmod-blocks?)
    (define probe (make-temporary-file "qcr-chmodprobe~a" 'directory enum-base))
    (dynamic-wind (lambda () (system*/exit-code (find-executable-path "chmod") "000" probe))
                  (lambda ()
                    (with-handlers ([exn:fail? (lambda (_) #t)])
                      (directory-list probe)
                      #f))
                  (lambda ()
                    (system*/exit-code (find-executable-path "chmod") "u+w" probe)
                    (delete-directory probe))))

  (define (run-enumeration! paths links)
    (parameterize ([current-library-collection-paths paths]
                   [current-library-collection-links links])
      (define rs (install-collection-roots))
      (values rs (install-enumeration-diagnostics))))

  (define (dir-string-normalize s)
    (regexp-replace #rx"/+$" s ""))

  (define (root-string-matches? rs p)
    (define want (dir-string-normalize (path->string (simplify-path (path->complete-path p)))))
    (for/or ([r (in-list (map path->string rs))])
      (string=? (dir-string-normalize r) want)))

  (test-case "enumeration diagnostics on a healthy pair of primary roots"
    (define r0 (build-path enum-base "t0-r0"))
    (define r1 (build-path enum-base "t0-r1"))
    (make-directory* r0)
    (make-directory* r1)
    (define-values (rs diag) (run-enumeration! (list r0 r1) '()))
    (check-true (install-enumeration-diag? diag))
    (check-equal? (install-enumeration-diag-roots diag) 2)
    (check-equal? (install-enumeration-diag-primary-count diag) 2)
    (check-equal? (install-enumeration-diag-primary-missing diag) 0)
    (check-equal? (install-enumeration-diag-link-count diag) 0)
    (check-equal? (install-enumeration-diag-link-skip-count diag) 0)
    (check-equal? (install-enumeration-diag-entry-skip-count diag) 0)
    (check-equal? (install-enumeration-diag-root-skip-count diag) 0)
    (check-equal? (install-enumeration-diag-link-skips diag) '())
    (check-equal? (install-enumeration-diag-entry-skips diag) '())
    (check-equal? (install-enumeration-diag-root-skips diag) '())
    (check-equal? (length rs) 2))

  (test-case "a missing primary path is counted without being dropped"
    (define r0 (build-path enum-base "t0b-r0"))
    (define vanished (build-path enum-base "t0b-vanished"))
    (make-directory* r0)
    (define-values (rs diag) (run-enumeration! (list r0 vanished) '()))
    (check-equal? (install-enumeration-diag-primary-count diag) 2)
    (check-equal? (install-enumeration-diag-primary-missing diag) 1)
    ;; observation only: the missing path is still appended as a root
    (check-equal? (install-enumeration-diag-roots diag) 2)
    (check-true (root-string-matches? rs vanished) "the missing primary path must still be appended"))

  (test-case "enumeration counts corrupt, shape-rejected and missing link files"
    (define corrupt (build-path enum-base "t1-corrupt.rktd"))
    (call-with-output-file corrupt #:exists 'truncate (lambda (o) (display "(unclosed" o)))
    (define shape-bad (build-path enum-base "t1-shape.rktd"))
    (call-with-output-file shape-bad #:exists 'truncate (lambda (o) (write '(flat-symbol) o)))
    (define missing-link (build-path enum-base "t1-missing.rktd"))
    (define good-link (build-path enum-base "t1-good.rktd"))
    ;; the 'up element is the only spelling whose target exists under the
    ;; production base rule (base = the link file spelled as a directory,
    ;; so any ordinary element lands UNDER a file and can never exist);
    ;; it exercises the resolvable-entry branch without changing control
    ;; flow. Probe-verified on 8.10: real links.rktd entries are nested
    ;; (id (elem ...)) and every real entry contract-violates today —
    ;; which is exactly what the skip counts must make loud.
    (call-with-output-file good-link #:exists 'truncate (lambda (o) (write '((t1lib up)) o)))
    (define-values (rs diag) (run-enumeration! '() (list corrupt shape-bad missing-link good-link)))
    (check-equal? (install-enumeration-diag-link-count diag) 4)
    (check-equal? (install-enumeration-diag-link-skip-count diag) 3)
    (check-equal? (length (install-enumeration-diag-link-skips diag)) 3)
    (check-equal? (install-enumeration-diag-entry-skip-count diag) 0)
    ;; the resolvable entry's target ('up = the link file's own
    ;; directory) is still a declared root
    (check-true (root-string-matches? rs enum-base) "the valid link file's target must stay a root")
    (define (skip-reason p)
      (define hit
        (for/first ([s (in-list (install-enumeration-diag-link-skips diag))]
                    #:when (equal? (car s) p))
          (cadr s)))
      hit)
    (check-equal? (skip-reason missing-link) "missing or non-path link file")
    (check-equal? (skip-reason shape-bad) "shape rejected (not a list of (element ...) pairs)")
    ;; the accurate reason for a corrupt file is the actual read error,
    ;; never the shape-gate string (review blocker 1)
    (define corrupt-reason (skip-reason corrupt))
    (check-true (and (string? corrupt-reason)
                     (not (string=? corrupt-reason ""))
                     (not (string=? corrupt-reason
                                    "shape rejected (not a list of (element ...) pairs)")))
                (format "corrupt link reason: ~s" corrupt-reason)))

  (test-case "enumeration counts entry skips but keeps resolvable roots"
    (define link-t2 (build-path enum-base "t2.rktd"))
    (call-with-output-file link-t2
                           #:exists 'truncate
                           (lambda (o)
                             (write '((t2lib up) (t2lib bad-symbol-element)
                                                 (t2lib ("nested" "elements"))
                                                 (t2lib "t2-missing-dir"))
                                    o)))
    (define-values (rs diag) (run-enumeration! '() (list link-t2)))
    (check-equal? (install-enumeration-diag-entry-skip-count diag) 3)
    (check-equal? (length (install-enumeration-diag-entry-skips diag)) 3)
    (check-equal? (install-enumeration-diag-link-skip-count diag) 0)
    (check-equal? (install-enumeration-diag-roots diag) 1)
    ;; the 'up entry resolves to the link file's own directory: a real
    ;; declared root survives alongside the skips
    (check-true (root-string-matches? rs enum-base)
                "the resolvable root must survive the bad entries")
    ;; nested element list -> build-path contract violation (THE real
    ;; links.rktd shape, probe-verified): counted with the exception
    ;; message, the bad ENTRY as the path element
    (define nested-skip
      (for/first ([s (in-list (install-enumeration-diag-entry-skips diag))]
                  #:when (equal? (car s) (list 't2lib (list "nested" "elements"))))
        s))
    (check-not-false nested-skip)
    (check-true (and (string? (cadr nested-skip)) (not (string=? (cadr nested-skip) ""))))
    ;; symbol element -> build-path raise -> counted with the exception
    ;; message as the reason, the bad ENTRY as the path element
    (define raise-skip
      (for/first ([s (in-list (install-enumeration-diag-entry-skips diag))]
                  #:when (equal? (car s) (list 't2lib 'bad-symbol-element)))
        s))
    (check-not-false raise-skip)
    (check-true (and (string? (cadr raise-skip)) (not (string=? (cadr raise-skip) ""))))
    (check-false (string-prefix? (cadr raise-skip) "missing: "))
    ;; missing target dir -> counted with the LINK FILE as the path
    ;; element and the accurate "missing: <dir>" reason
    (define missing-skip
      (for/first ([s (in-list (install-enumeration-diag-entry-skips diag))]
                  #:when (equal? (car s) link-t2))
        s))
    (check-not-false missing-skip)
    (check-true (string-prefix? (cadr missing-skip) "missing: ")
                (format "missing-entry reason: ~s" (cadr missing-skip)))
    (check-true (string-contains? (cadr missing-skip) "t2-missing-dir")))

  ;; T3/T8 share the permission-fixture guard: on a root runner chmod 000
  ;; does not block, so the fixtures would observe nothing.
  (define enum-chmod-blocks? (chmod-blocks?))
  (unless enum-chmod-blocks?
    (eprintf
     "SKIP install-map enumeration permission fixtures: chmod 000 does not block this runner\n"))

  (when enum-chmod-blocks?
    (test-case "enumeration counts a chmod-000 root dir as a root-skip"
      ;; 'up lifts the target out from under the link file (see the T1
      ;; note): the 'root branch then lists the locked dir, whose
      ;; directory-list raises (probe-verified: chmod-000 blocks
      ;; directory-list)
      (define link-t3 (build-path enum-base "t3.rktd"))
      (define locked (build-path enum-base "t3-locked"))
      (make-directory* locked)
      (call-with-output-file link-t3
                             #:exists 'truncate
                             (lambda (o) (write '((root up "t3-locked")) o)))
      (dynamic-wind (lambda () (system*/exit-code (find-executable-path "chmod") "000" locked))
                    (lambda ()
                      (define-values (_rs diag) (run-enumeration! '() (list link-t3)))
                      (check-equal? (install-enumeration-diag-root-skip-count diag) 1)
                      (check-equal? (length (install-enumeration-diag-root-skips diag)) 1)
                      (define skip (car (install-enumeration-diag-root-skips diag)))
                      ;; subtype-agnostic: the reason is the actual (non-empty)
                      ;; filesystem exception message, the path is the locked root
                      (check-equal? (path->string (car skip))
                                    (path->string (simplify-path (path->complete-path locked))))
                      (check-true (and (string? (cadr skip)) (not (string=? (cadr skip) "")))))
                    (lambda () (system*/exit-code (find-executable-path "chmod") "u+w" locked)))))

  (test-case "install-walk-zos reports a stubbed walk failure without raising"
    (define-values (zos raised? msg)
      (parameterize ([install-walker (lambda (_)
                                       (raise (exn:fail "simulated walk failure"
                                                        (current-continuation-marks))))])
        (install-walk-zos (build-path enum-base "t4-root"))))
    (check-equal? zos '())
    (check-true raised?)
    (check-equal? msg "simulated walk failure"))

  (test-case "a stubbed walk failure still resolves (counted-and-loud, decision 6)"
    (define t5-root (build-path enum-base "t5-root"))
    (make-directory* t5-root)
    (define out (open-output-string))
    (define r5
      (parameterize ([current-error-port out]
                     [install-walker (lambda (_)
                                       (raise (exn:fail "simulated walk failure"
                                                        (current-continuation-marks))))])
        (resolve-compiled-root! #:final-dir final-dir
                                #:checkout checkout-b
                                #:trusted-producer-labels '("q-trusted-producer")
                                #:map-dir (build-path enum-base "t5-cmap")
                                #:install-map-dir (build-path enum-base "t5-install-map")
                                #:collection-roots (list t5-root))))
    (check-true (eq? (compiled-root-reason r5) #t) (reason->string (compiled-root-reason r5)))
    (define log (get-output-string out))
    (check-true (string-contains? log "0 members, 0 links, inventory e3b0c44298fc")
                "the empty-inventory totals line must stay byte-identical")
    (check-true (string-contains? log "walk-raised yes: simulated walk failure"))
    (check-true (string-contains? log "enumeration roots injected (#:collection-roots): 1 roots")))

  (test-case "resolve isolates a stale enumeration diagnostic (no leak)"
    (define t6-root (build-path enum-base "t6-root"))
    (make-directory* t6-root)
    (define stale-skip (list (list "stale-path" "stale reason")))
    (define stale (install-enumeration-diag 99 9 9 9 9 stale-skip 9 stale-skip 9 stale-skip))
    (define before (install-enumeration-diagnostics))
    (define out (open-output-string))
    (define r6
      (parameterize ([current-error-port out]
                     [install-enumeration-diagnostics stale])
        (resolve-compiled-root! #:final-dir final-dir
                                #:checkout checkout-b
                                #:trusted-producer-labels '("q-trusted-producer")
                                #:map-dir (build-path enum-base "t6-cmap")
                                #:install-map-dir (build-path enum-base "t6-install-map")
                                #:collection-roots (list t6-root))))
    (check-true (eq? (compiled-root-reason r6) #t) (reason->string (compiled-root-reason r6)))
    (define log (get-output-string out))
    (check-true (string-contains? log "enumeration roots injected (#:collection-roots): 1 roots"))
    (check-false (string-contains? log "install map enumeration")
                 "the stale record must not produce an enumeration line")
    (check-false (string-contains? log "link-skip") "the stale record must not be emitted")
    (check-false (string-contains? log "entry-skip") "the stale record must not be emitted")
    (check-false (string-contains? log "root-skip") "the stale record must not be emitted")
    (check-true (string-contains? log "walk-raised no"))
    (check-equal? (install-enumeration-diagnostics) before "no diagnostic may leak outward"))

  (test-case "build-install-map! emits the full enumeration line and skip details"
    (define t7-root (build-path enum-base "t7-root"))
    (make-directory* t7-root)
    (define synthetic
      (install-enumeration-diag 1
                                1
                                0
                                2
                                1
                                (list (list "qcr-link" "missing or non-path link file"))
                                0
                                '()
                                0
                                '()))
    (define out (open-output-string))
    (define inv
      (parameterize ([current-error-port out]
                     [install-enumeration-diagnostics synthetic])
        (build-install-map! #:map-dir (build-path enum-base "t7-install-map")
                            #:collection-roots (list t7-root))))
    (define log (get-output-string out))
    (check-equal? (hash-ref inv 'members) 0)
    (check-true
     (string-contains?
      log
      "install map enumeration: 1 roots, primary 1 (0 missing), 2 link files, 1 link skips, 0 entry skips, 0 root skips"))
    (check-true (string-contains? log "link-skip qcr-link: missing or non-path link file"))
    (check-true (string-contains? log "walk-raised no"))
    (check-true (string-contains? log "0 members, 0 links, inventory e3b0c44298fc")))

  (when enum-chmod-blocks?
    (test-case "the real walker reports (not swallows) a chmod-000 descent"
      (define locked-root (build-path enum-base "t8-locked"))
      (make-directory* locked-root)
      (dynamic-wind (lambda () (system*/exit-code (find-executable-path "chmod") "000" locked-root))
                    (lambda ()
                      (define-values (zos raised? msg) (install-walk-zos locked-root))
                      (check-equal? zos '())
                      (check-true raised?)
                      (check-true (and (string? msg) (not (string=? msg "")))))
                    (lambda ()
                      (system*/exit-code (find-executable-path "chmod") "u+w" locked-root)))))

  (test-case "CLI resolve+launch carry the install map: flags payload and record counters"
    (define colls (make-install-coll! "cli" 'install-cli 'source-cli))
    (stamp-install-fresh! colls)
    (define alpha-src (list-ref colls 1))
    (define imd (build-path install-base "cli-install-map"))
    (define cli-flags (build-path install-base "cli-flags.rktd"))
    (define cli-tel (build-path install-base "cli-tel.rktd"))
    (define resolve-code
      (system*/exit-code (find-executable-path "racket")
                         (path->string ci-cli-runtime-path)
                         "resolve"
                         "--root"
                         (path->string final-dir)
                         "--checkout"
                         (path->string checkout-b)
                         "--trusted-label"
                         "q-trusted-producer"
                         "--install-map-dir"
                         (path->string imd)
                         ;; hermetic enumeration: the fixture install coll
                         ;; is not on the child process's real collection
                         ;; search path, so the same controlled roots the
                         ;; in-process resolves use are injected here
                         ;; (the CLI spelling of #:collection-roots)
                         "--collection-root"
                         (path->string (list-ref colls 0))
                         "--flags-file"
                         (path->string cli-flags)))
    (check-equal? resolve-code 0 "resolve with --install-map-dir must succeed")
    (check-true (file-exists? cli-flags))
    (define flags (file->value cli-flags))
    (check-equal? (length flags) 5 "the flags payload shape is unchanged")
    (define launch-args (list-ref flags 4))
    (define boot (cadr launch-args))
    ;; the -e bootstrap and the flags payload agree with in-process values:
    ;; both maps, map-first, absolute
    (define cmap-at (string-index-of boot (list-ref flags 2)))
    (define imap-at (string-index-of boot (path->string (simplify-path (path->complete-path imd)))))
    (check-not-false cmap-at "flags bootstrap must mention the checkout map")
    (check-not-false imap-at "flags bootstrap must mention the install map")
    (check-true (< cmap-at imap-at) "map-first order must hold in the flags bootstrap")
    (check-false (string-contains? boot "'same") "flags bootstrap must never restore 'same")
    ;; launch a child that requires the install member + the checkout
    ;; marker; the record must carry the old fields AND the new keys
    (define child-marker (build-path install-base "cli-child-result.txt"))
    (when (file-exists? child-marker)
      (delete-file child-marker))
    (define child-env (make-environment-variables))
    (for ([n (in-list (environment-variables-names (current-environment-variables)))])
      (environment-variables-set! child-env
                                  n
                                  (environment-variables-ref (current-environment-variables) n)))
    (environment-variables-set! child-env #"Q_CI_COMPILED_ROOT" #"auto")
    (define child-expr
      (format
       "(begin (require racket/file) \
               (dynamic-require (string->path ~s) 'iv) \
               (dynamic-require (string->path ~s) 'marker-result) \
               (call-with-output-file ~s (lambda (o) (write 'ran o))))"
       (path->string alpha-src)
       (path->string (marker-abs-path checkout-b))
       (path->string child-marker)))
    (define launch-code
      (parameterize ([current-environment-variables child-env])
        (system*/exit-code (find-executable-path "racket")
                           (path->string ci-cli-runtime-path)
                           "launch"
                           "--flags-file"
                           (path->string cli-flags)
                           "--telemetry-file"
                           (path->string cli-tel)
                           "--"
                           "racket"
                           "-e"
                           child-expr)))
    (check-equal? launch-code 0)
    (check-equal? (file->value child-marker) 'ran "the mapped child must really run")
    (define tel (file->value cli-tel))
    (define (field k)
      (define pair (assq k (cdr tel)))
      (and pair (cdr pair)))
    ;; OLD fields unchanged
    (check-equal? (field 'mode) "verified-root-hit")
    (check-equal? (field 'exit-code) 0)
    (check-true (and (field 'root-hits)
                     (field 'fallbacks)
                     (field 'loads)
                     (field 'bytes-served)
                     (field 'wall-ms)
                     (field 'mechanism)
                     #t)
                "the launch record must keep every old field")
    ;; NEW additive keys
    (check-true (>= (field 'checkout-map-hits) 1) "checkout-map-hits missing/zero")
    (check-true (>= (field 'install-map-hits) 2) "install-map-hits missing/zero")
    (check-true (exact-nonnegative-integer? (field 'unmapped-loads)) "unmapped-loads key missing"))

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
                  "the dangling link must be replaced with the correct payload target"))

  ;; ------------- Step B: rerooted compiled-cache lane (install map) ------
  ;;
  ;; PLAN-B-install-cache-lane.md (red-first). A unix-style installation
  ;; declares its rerooted compiled cache in <prefix>/etc/racket/config.rktd
  ;; under 'compiled-file-roots; the classic enumeration (collection paths
  ;; plus link files) only reaches SOURCE-ADJACENT zos and misses the
  ;; rerooted cache entirely (CI smoke-10: ~6084 cache zos never
  ;; inventoried). Pre-edit probes (this machine, Racket 8.10,
  ;; /tmp/q-unixstyle-probe + the system ~/.racket install):
  ;;   P0: (find-system-path 'config-dir) IS <prefix>/etc/racket/ — the
  ;;       INSTALLATION config dir (absolute-installation? . #t), never
  ;;       ~/.config/racket (review blocker 1 refuted by probe; the
  ;;       install-config-dir seam keeps tests/overrides hermetic);
  ;;   P0b: file->value tolerates the leading ";; generated by ..." comment;
  ;;       'compiled-file-roots reads (same "<prefix>/lib/racket/compiled");
  ;;   P1: 6084 zos under <prefix>/lib/racket/compiled;
  ;;   P2: segment->source reconstruction spot-check 12/12, 0 sources
  ;;       missing. A putenv PLTCONFIGDIR override did NOT redirect
  ;;       find-system-path on this binary — the parameter seam is the
  ;;       authoritative override channel.
  (define cache-base (make-temporary-file "qcr-cache~a" 'directory))

  (define (write-coll-src! p marker requires?)
    (call-with-output-file p
                           #:exists 'truncate
                           (lambda (o)
                             (fprintf o
                                      (string-append "#lang racket/base\n"
                                                     (if requires? "(require \"beta.rkt\")\n" "")
                                                     "(provide iv bv)\n"
                                                     "(define bv '~a)\n"
                                                     "(define iv (list '~a bv))\n")
                                      marker
                                      marker))))

  (define (raco-make! alpha)
    (unless (zero? (system*/exit-code (find-executable-path "racket")
                                      "-l"
                                      "raco"
                                      "-l-"
                                      "raco/main.rkt"
                                      "make"
                                      "-j"
                                      "1"
                                      alpha))
      (error 'fixture "raco make failed for the cache closure ~a" alpha)))

  ;; move the source-adjacent compiled/ dir into the rerooted cache layout
  ;; <cache>/<ABS source dir minus the leading slash>/compiled — the real
  ;; unixstyle layout (probe P2: source /tmp/q-unixstyle-probe/share/.../
  ;; acks/acks.rkt lives at lib/racket/compiled/tmp/q-unixstyle-probe/
  ;; share/.../acks/compiled/acks_rkt.zo), exactly the reroot contract the
  ;; serving lookup and the segment reconstruction use
  (define (reroot-adjacent! prefix coll cache)
    (define rel-src (find-relative-path (string->path "/") (path->directory-path coll)))
    (make-directory* (path->directory-path (path-only (build-path cache rel-src "compiled"))))
    (rename-file-or-directory (build-path coll "compiled") (build-path cache rel-src "compiled")))

  (define (cache-config-path prefix)
    (build-path prefix "etc" "racket" "config.rktd"))

  (define (write-config-value! prefix v)
    (make-directory* (build-path prefix "etc" "racket"))
    (call-with-output-file (cache-config-path prefix)
                           #:exists 'truncate
                           (lambda (o)
                             (display ";; generated by test fixture\n" o)
                             (write v o))))

  (define (write-cache-config! prefix cache-roots)
    (write-config-value! prefix
                         (hash 'share-dir
                               (path->string (build-path prefix "share" "racket"))
                               'lib-dir
                               (path->string (build-path prefix "lib" "racket"))
                               'pkgs-dir
                               (path->string (build-path prefix "share" "racket" "pkgs"))
                               'bin-dir
                               (path->string (build-path prefix "bin"))
                               'doc-dir
                               (path->string (build-path prefix "share" "racket" "doc"))
                               'include-dir
                               (path->string (build-path prefix "include" "racket"))
                               'apps-dir
                               (path->string (build-path prefix "share" "applications"))
                               'man-dir
                               (path->string (build-path prefix "share" "man"))
                               'absolute-installation?
                               #t
                               'compiled-file-roots
                               (cons 'same (map path->string cache-roots)))))

  (define (write-raw-config! prefix text)
    (make-directory* (build-path prefix "etc" "racket"))
    (call-with-output-file (cache-config-path prefix)
                           #:exists 'truncate
                           (lambda (o) (display text o))))

  ;; Self-contained unix-style prefix: share/racket/collects/mylib sources
  ;; raco-made SOURCE-ADJACENT (make-install-coll! precedent), the adjacent
  ;; compiled/ dir RENAMED into <prefix>/lib/racket/compiled — real zo bytes
  ;; at the real reroot spelling. No sleeps, no mtimes read (the rename
  ;; preserves them and no pin consults them). The config carries the
  ;; installer shape: leading ;; comment, 8 absolute anchor keys under
  ;; <prefix>, absolute-installation? #t, 'compiled-file-roots (same
  ;; <cache>); the SOURCES live under the anchor prefix P (review change 8).
  (define (make-unixstyle-prefix! name marker)
    (define prefix (build-path cache-base name))
    (define coll (build-path prefix "share" "racket" "collects" "mylib"))
    (make-directory* coll)
    (define beta (build-path coll "beta.rkt"))
    (define alpha (build-path coll "alpha.rkt"))
    (write-coll-src! beta marker #f)
    (write-coll-src! alpha marker #t)
    (raco-make! alpha)
    (write-coll-src! alpha 'source-cache #t)
    (write-coll-src! beta 'source-cache #f)
    (define cache (build-path prefix "lib" "racket" "compiled"))
    (reroot-adjacent! prefix coll cache)
    (write-cache-config! prefix (list cache))
    (list prefix coll alpha beta cache))

  (define (unix-etc u)
    (build-path (list-ref u 0) "etc" "racket"))

  ;; T1/T6/T7 drive build-install-map! with an EXPLICIT 'config lane +
  ;; parameterized config dir + injected fixture classic roots (hermetic;
  ;; review blocker 2 adjudication)
  (define (build-cache-map! name u out)
    (parameterize ([current-error-port out]
                   [install-enumeration-diagnostics #f]
                   [install-config-dir (unix-etc u)])
      (build-install-map! #:map-dir (build-path cache-base (string-append name "-install-map"))
                          #:collection-roots (list (list-ref u 1))
                          #:install-cache-lane 'config)))

  ;; T4/T5 resolve WITHOUT #:collection-roots: the 'auto lane resolves to
  ;; 'config and the enumeration base comes from parameterized collection
  ;; paths/links (hermetic end-to-end)
  (define (resolve-with-cache-lane! name u cfg-dir out)
    (define imd (build-path cache-base (string-append name "-install-map")))
    (define r
      (parameterize ([current-error-port out]
                     [install-config-dir cfg-dir]
                     [current-library-collection-paths (list (list-ref u 1))]
                     [current-library-collection-links '()])
        (resolve-compiled-root! #:final-dir final-dir
                                #:checkout checkout-b
                                #:trusted-producer-labels '("q-trusted-producer")
                                #:map-dir (build-path cache-base (string-append name "-cmap"))
                                #:install-map-dir imd)))
    (values r imd))

  (test-case "cache lane inventories rerooted cache zos and logs its lines"
    (define u (make-unixstyle-prefix! "cache-t1" 'cache-t1))
    (define out (open-output-string))
    (define inv (build-cache-map! "cache-t1" u out))
    (check-equal? (hash-ref inv 'members) 2 "both cache zos must be inventoried")
    (check-true (and (string? (hash-ref inv 'digest)) (not (string=? (hash-ref inv 'digest) ""))))
    (define log (get-output-string out))
    (check-true
     (string-contains?
      log
      "install cache lane: 1 cache roots, 2 members, 0 walk skips, 0 outside prefix, config ok"))
    (check-true (string-contains? log "cache root ") (format "missing per-cache-root line: ~s" log))
    (check-true (string-contains? log "walk-raised no"))
    (check-false (string-contains? log "cache-config") "an ok config emits no cache-config line")
    (define diag (install-cache-lane-diagnostics))
    (check-true (install-cache-lane-diag? diag))
    (check-equal? (install-cache-lane-diag-config-state diag) 'ok)
    (check-equal? (install-cache-lane-diag-anchor-count diag) 8)
    (check-equal? (install-cache-lane-diag-root-count diag) 1)
    (check-equal? (install-cache-lane-diag-member-count diag) 2)
    (check-equal? (install-cache-lane-diag-walk-skip-count diag) 0)
    (check-equal? (install-cache-lane-diag-outside-count diag) 0))

  (test-case "cache lane counts a missing source loudly (reachability gate)"
    (define u (make-unixstyle-prefix! "cache-t2" 'cache-t2))
    (delete-file (list-ref u 2)) ; alpha.rkt — its cache zo stays behind
    (define out (open-output-string))
    (define inv (build-cache-map! "cache-t2" u out))
    (check-equal? (hash-ref inv 'members) 1 "only beta survives the reachability gate")
    (define log (get-output-string out))
    (check-true (string-contains? log "1 cache roots, 1 members, 1 walk skips"))
    (check-true (string-contains? log "source missing") (format "missing cache-skip detail: ~s" log))
    (define diag (install-cache-lane-diagnostics))
    (check-equal? (install-cache-lane-diag-walk-skip-count diag) 1)
    (check-equal? (cadr (car (install-cache-lane-diag-walk-skips diag))) "source missing"))

  (test-case "cache entries reconstructing outside the prefix are contained"
    (define u (make-unixstyle-prefix! "cache-t3" 'cache-t3))
    (define cache (list-ref u 4))
    ;; an outside SOURCE (exists under the temp base, outside P) + a cache
    ;; zo spelled to reconstruct into it: the zo's rel spelling is the
    ;; outside dir's absolute path minus the leading slash
    (define outside-src (build-path cache-base "cache-t3-outside-src" "evil.rkt"))
    (make-directory* (path-only outside-src))
    (call-with-output-file outside-src
                           #:exists 'truncate
                           (lambda (o) (display "#lang racket/base\n" o)))
    (define rel-outside
      (find-relative-path (string->path "/")
                          (path->directory-path (path->directory-path outside-src))))
    (define evil-dir (build-path cache rel-outside "compiled"))
    (make-directory* evil-dir)
    (call-with-output-file (build-path evil-dir "evil_rkt.zo")
                           #:exists 'truncate
                           (lambda (o) (display "not real bytecode" o)))
    (define out (open-output-string))
    (define inv (build-cache-map! "cache-t3" u out))
    (check-equal? (hash-ref inv 'members) 2 "the outside entry must not be inventoried")
    (define log (get-output-string out))
    (check-true (string-contains? log "1 walk skips, 1 outside prefix")
                (format "missing outside-prefix summary: ~s" log))
    (check-true (string-contains? log "outside install prefix"))
    (define diag (install-cache-lane-diagnostics))
    (check-equal? (install-cache-lane-diag-outside-count diag) 1))

  (test-case "config failures are fail-open-with-count: resolve succeeds, 0 members"
    (define u (make-unixstyle-prefix! "cache-t4" 'cache-t4))
    (define prefix (list-ref u 0))
    (define cache (list-ref u 4))
    (define (members-of r)
      (hash-ref (hash-ref (compiled-root-manifest r) 'install-map) 'members))
    ;; (a) missing config.rktd
    (define out-a (open-output-string))
    (define-values (r-a _a)
      (resolve-with-cache-lane! "cache-t4-missing"
                                u
                                (build-path cache-base "cache-t4-vanished-etc")
                                out-a))
    (check-true (eq? (compiled-root-reason r-a) #t) (reason->string (compiled-root-reason r-a)))
    (check-equal? (members-of r-a) 0)
    (check-true (string-contains? (get-output-string out-a) "cache-config missing"))
    ;; (b) corrupt config (unclosed paren)
    (write-raw-config! prefix "(unclosed")
    (define out-b (open-output-string))
    (define-values (r-b _b) (resolve-with-cache-lane! "cache-t4-corrupt" u (unix-etc u) out-b))
    (check-true (eq? (compiled-root-reason r-b) #t) (reason->string (compiled-root-reason r-b)))
    (check-equal? (members-of r-b) 0)
    (check-true (string-contains? (get-output-string out-b) "cache-config unreadable"))
    ;; (c) keyless hash
    (write-config-value! prefix (hash))
    (define out-c (open-output-string))
    (define-values (r-c _c) (resolve-with-cache-lane! "cache-t4-nokey" u (unix-etc u) out-c))
    (check-true (eq? (compiled-root-reason r-c) #t) (reason->string (compiled-root-reason r-c)))
    (check-equal? (members-of r-c) 0)
    (check-true (string-contains? (get-output-string out-c) "cache-config no-key"))
    ;; (d) anchor-less: the cache key present, no anchor keys (review
    ;; blocker 3 family: containment must not be vacuous)
    (write-config-value! prefix (hash 'compiled-file-roots (list 'same (path->string cache))))
    (define out-d (open-output-string))
    (define-values (r-d _d) (resolve-with-cache-lane! "cache-t4-noanchor" u (unix-etc u) out-d))
    (check-true (eq? (compiled-root-reason r-d) #t) (reason->string (compiled-root-reason r-d)))
    (check-equal? (members-of r-d) 0)
    (check-true (string-contains? (get-output-string out-d) "cache-config no-prefix"))
    ;; (e) zero-common-anchor: absolute anchors whose common run is just "/"
    (write-config-value! prefix
                         (hash 'share-dir
                               "/cache-disjoint-a"
                               'lib-dir
                               "/cache-disjoint-b"
                               'compiled-file-roots
                               (list 'same (path->string cache))))
    (define out-e (open-output-string))
    (define-values (r-e _e) (resolve-with-cache-lane! "cache-t4-noprefix" u (unix-etc u) out-e))
    (check-true (eq? (compiled-root-reason r-e) #t) (reason->string (compiled-root-reason r-e)))
    (check-equal? (members-of r-e) 0)
    (check-true (string-contains? (get-output-string out-e) "cache-config no-prefix"))
    (check-true (string-contains? (get-output-string out-e) "0 cache roots, 0 members")))

  (test-case "parameterized resolve takes the config lane and serves cache bytes"
    (define u (make-unixstyle-prefix! "cache-t5" 'cache-t5))
    (define out (open-output-string))
    (define-values (r _imd) (resolve-with-cache-lane! "cache-t5" u (unix-etc u) out))
    (check-true (eq? (compiled-root-reason r) #t) (reason->string (compiled-root-reason r)))
    (define inv (hash-ref (compiled-root-manifest r) 'install-map #f))
    (check-true (hash? inv) "the resolved root must carry the install map")
    (check-equal? (hash-ref inv 'members) 2)
    (check-true (string-contains? (get-output-string out)
                                  "install cache lane: 1 cache roots, 2 members, 0 walk skips"))
    (reset-telemetry!)
    (define hits-before (compiled-root-telemetry-root-hits (telemetry-snapshot)))
    (with-compiled-root
     r
     checkout-b
     (list "marker/compiled-root-marker.rkt")
     (lambda ()
       (parameterize ([current-load-relative-directory (path->directory-path checkout-b)])
         (check-equal? (dynamic-require (list-ref u 2) 'iv)
                       (list 'cache-t5 'cache-t5)
                       "the cache zo must serve the install-marker bytes"))))
    (check-true (> (compiled-root-telemetry-root-hits (telemetry-snapshot)) hits-before)
                "TOCTOU guard observed no cache-served load"))

  (test-case "merge: first cache root wins, classic wins, sorted, stable digest"
    (define u (make-unixstyle-prefix! "cache-t6" 'cache-t6a))
    (define prefix (list-ref u 0))
    (define cache1 (list-ref u 4))
    ;; a SECOND cache root with the same sources, DIFFERENT zo bytes
    (define cache2 (build-path prefix "lib" "racket" "cache2"))
    (write-coll-src! (list-ref u 3) 'cache-t6c2 #f)
    (write-coll-src! (list-ref u 2) 'cache-t6c2 #t)
    (raco-make! (list-ref u 2))
    (reroot-adjacent! prefix (list-ref u 1) cache2)
    ;; first-cache-root-wins across two roots sharing a source: the
    ;; inventory digest follows the FIRST root's zo bytes
    (write-cache-config! prefix (list cache1 cache2))
    (define inv-12 (build-cache-map! "cache-t6-12" u (open-output-string)))
    (check-equal? (hash-ref inv-12 'members) 2)
    (write-cache-config! prefix (list cache2 cache1))
    (define inv-21 (build-cache-map! "cache-t6-21" u (open-output-string)))
    (write-cache-config! prefix (list cache1))
    (define inv-1 (build-cache-map! "cache-t6-1" u (open-output-string)))
    (check-equal? (hash-ref inv-12 'digest)
                  (hash-ref inv-1 'digest)
                  "the first cache root must win a shared source")
    (check-not-equal? (hash-ref inv-21 'digest)
                      (hash-ref inv-1 'digest)
                      "reordering the cache roots must change the winner")
    ;; classic wins: re-make the closure source-adjacent AFTER the moves
    (write-cache-config! prefix (list cache1 cache2))
    (write-coll-src! (list-ref u 3) 'cache-t6classic #f)
    (write-coll-src! (list-ref u 2) 'cache-t6classic #t)
    (raco-make! (list-ref u 2))
    (define out-b (open-output-string))
    (define inv-b (build-cache-map! "cache-t6-classic" u out-b))
    (check-equal? (hash-ref inv-b 'members) 2)
    (check-true (string-contains? (get-output-string out-b) "duplicate source")
                "the cache duplicates of a classic member must be counted")
    ;; members sorted by source
    (define srcs (map car (hash-ref inv-b 'mapped)))
    (check-equal? srcs (sort srcs string<?))
    ;; two builds -> identical inventory digest
    (define inv-c (build-cache-map! "cache-t6-again" u (open-output-string)))
    (check-equal? (hash-ref inv-c 'digest) (hash-ref inv-b 'digest))
    ;; end to end: the classic member's bytes win the shared source
    (define out-w (open-output-string))
    (define-values (r-w _imw) (resolve-with-cache-lane! "cache-t6-win" u (unix-etc u) out-w))
    (check-true (eq? (compiled-root-reason r-w) #t) (reason->string (compiled-root-reason r-w)))
    (with-compiled-root
     r-w
     checkout-b
     (list "marker/compiled-root-marker.rkt")
     (lambda ()
       (parameterize ([current-load-relative-directory (path->directory-path checkout-b)])
         (check-equal? (dynamic-require (list-ref u 2) 'iv)
                       (list 'cache-t6classic 'cache-t6classic)
                       "the CLASSIC member must win a shared source")))))

  (test-case "install-cache-zo->src unit truth table + the non-_rkt walk skip"
    (define u (make-unixstyle-prefix! "cache-t7" 'cache-t7))
    (define cache (list-ref u 4))
    ;; _rkt munge -> .rkt source at the reroot spelling (fixture segments:
    ;; the cache member's rel spelling is the fixture's real absolute
    ;; source dir minus the leading slash, exactly like reroot-adjacent!)
    (define rel-mylib (find-relative-path (string->path "/") (path->directory-path (list-ref u 1))))
    (define fixture-compiled (build-path cache rel-mylib "compiled"))
    (define alpha-src (install-cache-zo->src cache (build-path fixture-compiled "alpha_rkt.zo")))
    (check-equal? alpha-src (list-ref u 2))
    ;; non-compiled parent -> #f
    (check-false (install-cache-zo->src cache
                                        (build-path fixture-compiled "notcompiled" "alpha_rkt.zo")))
    ;; 'up guard: an UNSIMPLIFIED zo spelling carrying a literal '..
    ;; element escapes the root -> #f (the guard precedes the compiled
    ;; check, so it rejects before any element indexing can raise)
    (check-false (install-cache-zo->src cache
                                        (build-path cache ".." "elsewhere" "compiled" "x_rkt.zo")))
    ;; 2 elements = empty remainder -> #f (counted skip at the walk level)
    (check-false (install-cache-zo->src cache (build-path cache "compiled" "x_rkt.zo")))
    ;; _scrbl: the derived name is extensionless — the reconstruction still
    ;; returns it, and the WALK counts it under the documented non-_rkt skip
    (define cache-coll-compiled (build-path cache rel-mylib "compiled"))
    (call-with-output-file (build-path cache-coll-compiled "doc_scrbl.zo")
                           #:exists 'truncate
                           (lambda (o) (display "bytes" o)))
    (define scrbl-src (install-cache-zo->src cache (build-path cache-coll-compiled "doc_scrbl.zo")))
    (check-equal? (path->string (file-name-from-path scrbl-src)) "doc_scrbl")
    (define out (open-output-string))
    (define inv (build-cache-map! "cache-t7" u out))
    (check-equal? (hash-ref inv 'members) 2 "the _scrbl member must not be inventoried")
    (check-true (string-contains? (get-output-string out) "not a module source (non-_rkt munge)")))

  (test-case "install-source-under-prefix? truth table (path algebra, no regexp)"
    (define u (make-unixstyle-prefix! "cache-t8" 'cache-t8))
    (define P (list-ref u 0))
    (check-true (install-source-under-prefix? P (build-path P "share" "x.rkt")))
    (check-true (install-source-under-prefix? P P) "p == P accepts")
    (check-false (install-source-under-prefix? P (simplify-path (build-path P 'up) #f))
                 "p == parent(P) rejects")
    (check-false
     (install-source-under-prefix? P (string->path (string-append (path->string P) "/../sib")))
     "the ../ spelling rejects")
    (check-true (install-source-under-prefix? (string->path (string-append (path->string P) "/"))
                                              (build-path P "lib" "y.rkt"))
                "a trailing-separator base accepts")
    (check-false (install-source-under-prefix? P (build-path cache-base "elsewhere.rkt")))
    (check-false (install-source-under-prefix? P "relative/x.rkt") "non-absolute input is #f"))

  (test-case "lane 'none emits nothing; reset clears the lane parameter"
    (define u (make-unixstyle-prefix! "cache-t9" 'cache-t9))
    (define out (open-output-string))
    (parameterize ([current-error-port out]
                   [install-enumeration-diagnostics #f])
      (build-install-map! #:map-dir (build-path cache-base "cache-t9-install-map")
                          #:collection-roots (list (list-ref u 1))))
    (check-false (string-contains? (get-output-string out) "install cache lane")
                 "'none mode must emit no cache-lane line")
    (install-cache-lane-diagnostics (install-cache-lane-diag 'ok "" 8 1 0 '() 0 '() 0 2))
    (reset-telemetry!)
    (check-false (install-cache-lane-diagnostics) "reset must clear the lane record")
    (check-false (install-enumeration-diagnostics))))
