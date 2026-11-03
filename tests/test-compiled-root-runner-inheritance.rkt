#lang racket/base

;; tests/test-compiled-root-runner-inheritance.rkt — v1.00.32 W2 repair
;;
;; Child-inheritance hit counting: a 2-file suite run through the REAL
;; runner under root-on — launched exactly the way ci.yml launches a shard
;; (`compiled-root.rkt launch --flags-file … --telemetry-file … -- racket
;; scripts/run-tests.rkt <files> --jobs N`) — must produce REAL root hits
;; in the launcher record.
;;
;; Confirmed mechanism (v1.00.32 W2 trial c01 window-2, run 41271542715):
;;   H0  the launcher's mapped child `racket -e <bootstrap> <script> <args>`
;;       never loads <script> at all (a Racket init option such as -e
;;       suppresses the implicit positional-file load), so the root-on
;;       shard was a bootstrap-only process: exit 0, zero tests executed,
;;       zero loads — the vacuous green behind zero-root-hits.
;;   H2  even with the runner entered, its per-file executions run in
;;       `raco test`/`racket` SUBPROCESSES that inherit neither
;;       current-compiled-file-roots nor the TOCTOU guard, so no test load
;;       can ever be root-served.
;;   H1  no channel reports child-process load hits back to the launcher's
;;       telemetry snapshot (root-hits/loads/bytes-served were structurally
;;       0 and mechanism structurally "none" in launch mode).
;;
;; This test pins the repaired contract end to end: the shard child really
;; runs the suite (non-vacuous pass summary), the per-file children resolve
;; through the trusted root, and their served loads are COUNTED in the
;; launcher telemetry (root-hits > 0, loads > 0, bytes-served > 0,
;; fallbacks = 0, honest mechanism).

;; @speed slow
;; @suite fast
;; @timeout 300

(require racket/file
         racket/format
         racket/path
         racket/port
         racket/string
         racket/system
         rackunit
         racket/runtime-path)

(define-runtime-path repo-root "..")
(define-runtime-path launcher-script "../ci/prepared-environment/compiled-root.rkt")
(define-runtime-path producer-script "../scripts/ci/compiled-root.rkt")
(define-runtime-path runner-script "../scripts/run-tests.rkt")

(module+ test
  ;; Nested git commands must not inherit the caller's git plumbing (see
  ;; tests/test-compiled-root.rkt for the incident note).
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

  (define tmp-base (make-temporary-file "qcr-inherit~a" 'directory))

  ;; ---- fixture: a tiny clean git checkout with a two-module closure ----
  (define co (build-path tmp-base "co"))
  (define marker-dir (build-path co "marker"))
  (make-directory* marker-dir)
  (call-with-output-file
   (build-path marker-dir "second.rkt")
   (lambda (o) (display "#lang racket/base\n(provide second-value)\n(define second-value 41)\n" o)))
  (call-with-output-file
   (build-path marker-dir "compiled-root-marker.rkt")
   (lambda (o)
     (display
      "#lang racket/base\n(require \"second.rkt\")\n(provide marker-result)\n(define marker-result (add1 second-value))\n"
      o)))
  (define (git . args)
    (define code (apply system*/exit-code (find-executable-path "git") "-C" (path->string co) args))
    (unless (zero? code)
      (error 'fixture "git ~a failed" args)))
  (git "init" "-q")
  (git "config" "user.email" "ci@example.invalid")
  (git "config" "user.name" "CI Fixture")
  (git "add" ".")
  (git "commit" "-q" "-m" "fixture")

  ;; ---- the 2-file suite: each file requires the closure via (file …) ----
  (define tdir (build-path tmp-base "tests"))
  (make-directory* tdir)
  ;; one paren-balanced format string per generated file (balance guard)
  (define test-file-body
    (string-append
     "#lang racket/base\n"
     "(module+ test\n  (require rackunit (file ~s))\n  (check-equal? marker-result 42))\n"
     "(module+ main (void))\n"))
  (define (make-test! name)
    (call-with-output-file
     (build-path tdir name)
     (lambda (o)
       (fprintf o test-file-body (path->string (build-path marker-dir "compiled-root-marker.rkt"))))))
  (make-test! "inheritance-t1.rkt")
  (make-test! "inheritance-t2.rkt")

  ;; ---- producer + resolve (the ci.yml invocation shapes) ----
  (define (sh env . argv)
    (define o (open-output-string))
    (define code
      (parameterize ([current-output-port o]
                     [current-error-port o]
                     [current-environment-variables env])
        (apply system*/exit-code
               (map (lambda (a)
                      (if (path? a)
                          (path->string a)
                          a))
                    argv))))
    (values code (get-output-string o)))

  (define-values (bcode btranscript)
    (sh (git-plumbing-free-env)
        (find-executable-path "racket")
        producer-script
        "build"
        "--checkout"
        co
        "--module"
        "marker/compiled-root-marker.rkt"
        "--final-dir"
        (build-path tmp-base "root")
        "--label"
        "q-trusted-producer"))
  (check-equal? bcode 0 btranscript)

  (define flags-file (build-path tmp-base "flags.rktd"))
  (define-values (rcode rtranscript)
    (sh (git-plumbing-free-env)
        (find-executable-path "racket")
        launcher-script
        "resolve"
        "--root"
        (build-path tmp-base "root")
        "--checkout"
        co
        "--trusted-label"
        "q-trusted-producer"
        "--flags-file"
        flags-file))
  (check-equal? rcode 0 rtranscript)
  (check-true (file-exists? flags-file) "resolve must write the flags file")

  ;; ---- the CI-shaped shard launch over the REAL runner ----
  ;; env: Q_CI_COMPILED_ROOT=auto exactly like the ci.yml shard step; cwd:
  ;; the consumer checkout exactly like the CI job.
  (define shard-env
    (let ([env (git-plumbing-free-env)])
      (environment-variables-set! env #"Q_CI_COMPILED_ROOT" #"auto")
      env))
  (define telemetry-file (build-path tmp-base "shard-0.launch.rktd"))
  (define-values (lcode ltranscript)
    (parameterize ([current-directory co])
      (sh shard-env
          (find-executable-path "racket")
          launcher-script
          "launch"
          "--flags-file"
          flags-file
          "--telemetry-file"
          telemetry-file
          "--"
          (find-executable-path "racket")
          runner-script
          (path->string (build-path tdir "inheritance-t1.rkt"))
          (path->string (build-path tdir "inheritance-t2.rkt"))
          "--jobs"
          "2"
          "--timeout"
          "120")))

  (test-case "the shard child really runs the suite (never a vacuous green)"
    (check-equal? lcode 0 ltranscript)
    ;; Both fixture files must have been executed AND passed: the runner
    ;; summary is the anti-vacuous-green witness (the pre-fix mapped child
    ;; exited 0 without ever starting the runner).
    (check-true (regexp-match? #px"Files:\\s+2 total, 2 passed" ltranscript)
                (format "runner summary must show 2 passed files; transcript:\n~a" ltranscript)))

  (test-case "child-inheritance hit counting lands in the launcher record"
    (check-true (file-exists? telemetry-file) "launcher must write telemetry")
    (define tel (file->value telemetry-file))
    (check-true (and (list? tel) (eq? (car tel) 'compiled-root-launch-telemetry)) (format "~s" tel))
    (define (field k)
      (define pair (assq k (cdr tel)))
      (and pair (cdr pair)))
    (check-equal? (field 'mode) "verified-root-hit" (format "~s" tel))
    (check-equal? (field 'exit-code) 0 (format "~s" tel))
    ;; THE contract: root-hits must reflect real served loads. The fixture
    ;; manifest does NOT contain the runner graph, so every counted hit can
    ;; only come from a test-child execution that inherited the activation.
    (check-true (> (field 'root-hits) 0)
                (format "zero-root-hits: launcher record must count child-served loads: ~s" tel))
    (check-true (> (field 'loads) 0) (format "loads=0 with a functioning root: ~s" tel))
    (check-true (> (field 'bytes-served) 0) (format "bytes-served=0 with a functioning root: ~s" tel))
    (check-equal? (field 'fallbacks) 0 (format "~s" tel))
    (check-false (equal? (field 'mechanism) "none")
                 (format "mechanism=none is a lie when the child ran mapped: ~s" tel))))

;; -----------------------------------------------------------------------
;; Runner-side helper contract (scripts/run-tests/compiled-root-child.rkt):
;; fail-closed prefix selection. Without a valid flags file every child
;; invocation must stay byte-identical to the legacy path (#f prefix).

(module+ test
  (require racket/file
           racket/path
           racket/string
           "../scripts/run-tests/compiled-root-child.rkt")

  (define tmpu (make-temporary-file "qcr-inherit-u~a" 'directory))
  (define (with-env flags-path thunk)
    (define env (make-environment-variables))
    (when flags-path
      (environment-variables-set! env #"Q_COMPILED_ROOT_FLAGS_FILE" (string->bytes/utf-8 flags-path)))
    (parameterize ([current-environment-variables env])
      (thunk)))

  (test-case "no flags env = fail closed to the legacy path"
    (with-env
     #f
     (lambda ()
       (check-false (compiled-root-child-prefix))
       (check-false (compiled-root-raco-child-args (string->path (string-append "/" "x/a-test.rkt"))))
       (check-false (compiled-root-bare-child-args (string->path (string-append "/" "x/b.rkt")))))))

  (test-case "malformed flags file = fail closed, loudly"
    (define bad (build-path tmpu "bad.rktd"))
    (call-with-output-file bad (lambda (o) (display "(not-flags 1 2)" o)))
    (with-env (path->string bad)
              (lambda ()
                (check-false (compiled-root-child-prefix))
                (check-false (compiled-root-raco-child-args
                              (string->path (string-append "/" "x/a-test.rkt")))))))

  (test-case "wrong-shape flags file = fail closed"
    (define wrong (build-path tmpu "wrong.rktd"))
    (call-with-output-file wrong (lambda (o) (write '(compiled-root-flags 1 2 3) o)))
    (with-env (path->string wrong) (lambda () (check-false (compiled-root-child-prefix)))))

  (test-case "valid flags yield the bootstrap prefix and child argv"
    (define good (build-path tmpu "good.rktd"))
    (define launch-args (list "-e" "(begin (void))"))
    (call-with-output-file good
                           (lambda (o)
                             (write (list 'compiled-root-flags "/root" "/map" "/co" launch-args) o)))
    (with-env (path->string good)
              (lambda ()
                (check-equal? (compiled-root-child-prefix) launch-args)
                ;; raco children: bootstrap + the raco launcher's exact dispatch
                (check-equal?
                 (compiled-root-raco-child-args (string->path "/co/tests/a-test.rkt"))
                 (append launch-args
                         (list "-N" "raco" "-l-" "raco/main.rkt" "test" "-t" "/co/tests/a-test.rkt")))
                ;; bare children: bootstrap + the -N/-t- run-file entry
                (define bare (compiled-root-bare-child-args (string->path "/co/tests/b.rkt")))
                (check-equal? bare
                              (append launch-args
                                      (list "-N" "/co/tests/b.rkt" "-t-" "/co/tests/b.rkt"))
                              (format "~s" bare))))))

;; -----------------------------------------------------------------------
;; W1a round 5 (trial trial-2026-11-01-smoke-3, treatment C, run
;; 41303344486): the bare-child entry form. With ANY -e init option
;; present (the bootstrap's own included) Racket does not load a
;; positional script as the run-file, and arguments after the last -e
;; expression are parsed as RACKET switches — the real ci.yml lane's
;; `launch -- racket scripts/run-tests.rkt --suite fast …` died with
;; `racket: bad switch: --suite` before the runner ever started.
;;
;; The repaired contract, probe-verified on Racket 8.10 and unified on
;; the same mechanism as the proven raco dispatch
;; (`racket -N raco -l- raco/main.rkt …`, here in the file-path grammar:
;; -l rejects absolute file paths — (lib "/abs/p.rkt") is a bad module
;; path — while -t is (require (file …))):
;;
;;   racket [-N <script>] -e <bootstrap> -t- <script> <args…>
;;
;;   (a) -e/-t- are processed in order, so the bootstrap evaluates first;
;;   (b) -t- loads the script as a module AND as the run-file (plain
;;       `racket <script>` identity — what the real facade's
;;       direct-invocation guard requires);
;;   (c) -t- stops switch parsing and routes the remaining arguments to
;;       the script's current-command-line-arguments.
(module+ test
  (require racket/file
           racket/path
           racket/string
           racket/system
           "../ci/prepared-environment/compiled-root.rkt"
           "../scripts/run-tests/compiled-root-child.rkt")

  (define fake-dir (make-temporary-file "qcr-inherit-fake~a" 'directory))
  (define fake-runner (build-path fake-dir "fake-runner.rkt"))
  ;; The fake runner mirrors the real facade's observable contract: write
  ;; a LOADED marker when the module body executes, echo the routed
  ;; command line into a second marker, and record the run-file identity
  ;; into a third (the direct-invocation guard in
  ;; scripts/run-tests/runner.rkt keys on exactly that identity).
  (call-with-output-file
   fake-runner
   (lambda (o)
     (display
      (string-append "#lang racket/base\n"
                     "(define q-dir (getenv \"FAKE_RUNNER_MARKER_DIR\"))\n"
                     "(when q-dir\n"
                     "  (call-with-output-file (build-path q-dir \"run-file\")\n"
                     "    (lambda (o) (write (path->string (find-system-path 'run-file)) o))\n"
                     "    #:exists 'truncate)\n"
                     "  (call-with-output-file (build-path q-dir \"loaded\")\n"
                     "    (lambda (o) (displayln \"loaded\" o))\n"
                     "    #:exists 'truncate)\n"
                     "  (call-with-output-file (build-path q-dir \"argv\")\n"
                     "    (lambda (o) (write (vector->list (current-command-line-arguments)) o))\n"
                     "    #:exists 'truncate))\n")
      o)))
  (define loaded-marker (build-path fake-dir "loaded"))
  (define argv-marker (build-path fake-dir "argv"))
  (define runfile-marker (build-path fake-dir "run-file"))

  ;; A minimal VALID flags payload; the bootstrap here is a no-op -e, the
  ;; exact option shape of the real bootstrap's launch-args.
  (define launch-args (list "-e" "(begin (void))"))
  (define good-flags (build-path fake-dir "good.rktd"))
  (call-with-output-file good-flags
                         (lambda (o)
                           (write (list 'compiled-root-flags "/root" "/map" "/co" launch-args) o)))

  ;; Execute `racket <argv…>` with the flags file and the marker directory
  ;; exported, the way the launcher exports them into a shard child.
  (define (clear-markers!)
    (for ([m (in-list (list loaded-marker argv-marker runfile-marker))])
      (when (file-exists? m)
        (delete-file m))))
  (define (racket-child argv)
    (define env (make-environment-variables))
    (for ([n (in-list (environment-variables-names (current-environment-variables)))])
      (environment-variables-set! env
                                  n
                                  (environment-variables-ref (current-environment-variables) n)))
    (environment-variables-set! env
                                #"Q_COMPILED_ROOT_FLAGS_FILE"
                                (string->bytes/utf-8 (path->string good-flags)))
    (environment-variables-set! env
                                #"FAKE_RUNNER_MARKER_DIR"
                                (string->bytes/utf-8 (path->string fake-dir)))
    (define o (open-output-string))
    (define code
      (parameterize ([current-output-port o]
                     [current-error-port o]
                     [current-environment-variables env])
        (apply system*/exit-code (find-executable-path "racket") argv)))
    (values code (get-output-string o)))

  (test-case "the ci.yml mapped bare child runs the script and routes its command line"
    ;; The exact shape of the failing smoke lane:
    ;;   launch -- racket <script> --suite fast --jobs 2
    (clear-markers!)
    (define mapped
      (compiled-root-mapped-child-arguments
       launch-args
       (list (path->string fake-runner) "--suite" "fast" "--jobs" "2")))
    (define-values (code transcript) (racket-child mapped))
    (check-equal? code
                  0
                  (format "the mapped bare child must not die on its own args:\n~a" transcript))
    ;; The loaded marker proves the script really executed.
    (check-true (file-exists? loaded-marker)
                (format "the mapped bare child never loaded the script:\n~a" transcript))
    ;; The script's command line is the routed argument vector — not
    ;; racket switches, not swallowed positionals.
    (check-equal? (file->value argv-marker)
                  (list "--suite" "fast" "--jobs" "2")
                  (format "argv routing: ~s" (file->value argv-marker)))
    (check-equal? (file->value runfile-marker)
                  (path->string fake-runner)
                  "the script must be the run-file, like plain `racket <script>`"))

  (test-case "the runner's per-file bare child loads the file as the run-file (empty argv)"
    (clear-markers!)
    (define bare
      (parameterize ([current-environment-variables
                      (let ([env (make-environment-variables)])
                        (environment-variables-set! env
                                                    #"Q_COMPILED_ROOT_FLAGS_FILE"
                                                    (string->bytes/utf-8 (path->string good-flags)))
                        env)])
        (compiled-root-bare-child-args fake-runner)))
    (check-true (and bare #t) "valid flags must yield bare-child argv")
    (define-values (code transcript) (racket-child bare))
    (check-equal? code 0 (format "per-file bare child failed:\n~a" transcript))
    (check-true (file-exists? loaded-marker)
                (format "the per-file bare child never loaded the file:\n~a" transcript))
    ;; Plain `racket <file>` identity: the file is the run-file (the
    ;; facade's direct-invocation guard depends on it) and the per-file
    ;; child keeps an empty command line.
    (check-equal? (file->value runfile-marker)
                  (path->string fake-runner)
                  (format "run-file must be the file itself, got ~s" (file->value runfile-marker)))
    (check-equal? (file->value argv-marker)
                  '()
                  (format "per-file children keep an empty argv, got ~s" (file->value argv-marker))))

  (test-case "the entry functions are unified on the -N/-t- run-file form"
    (define script (string->path (string-append "/" "co/scripts/tool.rkt")))
    (check-equal? (compiled-root-script-entry-argument script)
                  (list "-N"
                        (string-append "/" "co/scripts/tool.rkt")
                        "-t-"
                        (string-append "/" "co/scripts/tool.rkt")))
    ;; The facade needs no special entry anymore: under -N/-t- the facade
    ;; IS the run-file, so its own direct-invocation guard fires exactly
    ;; once with the routed argv.
    (check-equal? (compiled-root-runner-entry-argument script)
                  (compiled-root-script-entry-argument script)
                  "the runner entry must be the unified script entry")
    ;; And the raco path is untouched (probe-proven mechanism, kept).
    (check-equal?
     (parameterize ([current-environment-variables
                     (let ([env (make-environment-variables)])
                       (environment-variables-set! env
                                                   #"Q_COMPILED_ROOT_FLAGS_FILE"
                                                   (string->bytes/utf-8 (path->string good-flags)))
                       env)])
       (compiled-root-raco-child-args (string->path "/co/tests/x.rkt")))
     (append launch-args (list "-N" "raco" "-l-" "raco/main.rkt" "test" "-t" "/co/tests/x.rkt")))))
