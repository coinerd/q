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
;; subprocesses, ~15 test cases over real bytecode). Hard-capped by
;; @timeout; the pure/fast validation matrix lives in
;; tests/test-compiled-root-manifest.rkt.

;; @speed slow
;; @suite fast
;; @timeout 600

(require racket/file
         racket/format
         racket/match
         racket/path
         racket/port
         racket/string
         racket/system
         rackunit
         "../ci/prepared-environment/compiled-root.rkt"
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
                "CLI build must publish a manifest-bearing root")))
