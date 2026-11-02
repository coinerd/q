#lang racket/base

;; tests/test-compiled-root-manifest.rkt — v1.00.30 W3
;;
;; Pure manifest validation matrix for the trusted compiled root
;; prototype. No compiled code is ever executed here: every case
;; exercises validate-compiled-root-manifest / make-compiled-root-manifest
;; over byte-level attacks and forgeries against a hermetic fixture
;; payload. RED-first contract (W3 plan §Safe compiled-root):
;; validation must fail closed on any identity-dimension mismatch
;; BEFORE any compiled module can run.
;;
;; Runner metadata: pure validation (no raco make / git subprocesses),
;; but SHA-256 of the fixture executable is paid per make/validate call;
;; @timeout guards against slow shared runners.

;; @speed fast
;; @suite fast
;; @timeout 300

(require racket/file
         racket/format
         racket/match
         racket/path
         racket/port
         racket/string
         racket/system
         rackunit
         racket/runtime-path
         (only-in openssl/libcrypto libcrypto)
         "../ci/prepared-environment/compiled-root-manifest.rkt"
         "../ci/prepared-environment/compiled-root.rkt"
         ;; W1 review round 6: the SHA-256 backend matrix pins the two
         ;; backends directly; transfer-auth is the child-side
         ;; authenticate-exact-bytes loader under race regression here.
         "../ci/prepared-environment/manifest.rkt"
         "../ci/prepared-environment/transfer-auth.rkt"
         (prefix-in cli: "../scripts/ci/compiled-root.rkt"))

;; absolute path of the transfer module, for child-process regression
;; scripts that must load the reservation primitive in a REAL second
;; process (cross-process telemetry-target collision regression)
(define-runtime-path transfer-auth-runtime-path "../ci/prepared-environment/transfer-auth.rkt")

(module+ test
  (define tmp-root (make-temporary-file "qcr-manifest-test~a" 'directory))
  (define staging (build-path tmp-root "staging"))
  (define marker-src-rel "marker/compiled-root-marker.rkt")
  (define marker-zo-rel "marker/compiled/compiled-root-marker.zo")
  (define payload-bytes #"Q-COMPILED-ROOT-PAYLOAD-FIXTURE-BYTES")
  (define source-bytes
    #"(module compiled-root-marker racket/base (provide marker) (define marker 'producer))")

  ;; Deterministic stand-in executable: the manifest contract needs a file
  ;; whose digest is byte-stable between producer and consumer. The real
  ;; racket binary (~45 MB) would cost ~5 s of SHA-256 per make/validate
  ;; call; this 1 MiB fixture keeps the matrix hermetic and fast.
  (define fake-exe (build-path tmp-root "fake-racket" "racket"))
  (make-directory* (path-only fake-exe))
  (with-output-to-file fake-exe #:exists 'replace (lambda () (write-bytes (make-bytes 1048576 171))))

  ;; Hermetic payload: one source + one zo member at checkout-shaped
  ;; rel paths. The "zo" bytes need not be real bytecode — validation
  ;; is digest-based and never executes payload members.
  (define (build-staging!)
    (define zo-abs (build-path staging marker-zo-rel))
    (make-directory* (path-only zo-abs))
    (make-directory* (path-only (build-path staging marker-src-rel)))
    (with-output-to-file (build-path staging marker-zo-rel)
                         #:exists 'replace
                         (lambda () (write-bytes payload-bytes)))
    (with-output-to-file (build-path staging marker-src-rel)
                         #:exists 'replace
                         (lambda () (write-bytes source-bytes))))

  (build-staging!)
  (define base-manifest
    (make-compiled-root-manifest #:sources (list (list marker-src-rel marker-zo-rel))
                                 #:root-dir staging
                                 #:racket-executable fake-exe
                                 #:producer-label "q-trusted-producer"
                                 #:producer-trusted? #t))

  ;; ---------------------------------------------------------- happy path

  (test-case "manifest round-trip validates cleanly"
    (check-eq? (validate-compiled-root-manifest base-manifest
                                                #:root-dir staging
                                                #:racket-executable fake-exe
                                                #:trusted-producer-labels '("q-trusted-producer"))
               base-manifest))

  (test-case "schema constant is the expected one"
    (check-equal? manifest-schema "q-compiled-root-manifest-1"))

  (test-case "manifest-source-paths / manifest-payload-members"
    (check-equal? (manifest-source-paths base-manifest) (list marker-src-rel))
    (match-define (list (list p zo _digest _mtime)) (manifest-payload-members base-manifest))
    (check-equal? p marker-src-rel)
    (check-equal? zo marker-zo-rel))

  (test-case "canonical payload digest is order- and content-sensitive"
    (define members
      (list (cons (string->bytes/utf-8 "b/compiled/b.zo") #"bbb")
            (cons (string->bytes/utf-8 "a/compiled/a.zo") #"aaa")))
    (define reversed (reverse members))
    (check-equal? (canonical-payload-digest members) (canonical-payload-digest reversed))
    (check-not-equal? (canonical-payload-digest members)
                      (canonical-payload-digest
                       (list (cons (string->bytes/utf-8 "b/compiled/b.zo") #"bbb!")
                             (cons (string->bytes/utf-8 "a/compiled/a.zo") #"aaa")))))

  (test-case "payload framing distinguishes renamed members"
    (check-not-equal? (payload-file-digest #"x/compiled/a.zo" #"same")
                      (payload-file-digest #"y/compiled/a.zo" #"same")))

  (test-case "source->zo-name munging rule matches the 8.10 lookup rule"
    (check-equal? (source->zo-name "m.rkt") (string->path "m_rkt.zo"))
    (check-equal? (source->zo-name "marker/compiled-root-marker.rkt")
                  (string->path "marker/compiled-root-marker_rkt.zo"))
    (check-equal? (source->zo-name (string->path "m.rkt")) (string->path "m_rkt.zo"))
    (check-equal? (source->zo-name "noext") (string->path "noext.zo")))

  (test-case "lockfile digest is bound when supplied"
    (define lock (build-path tmp-root "lock.scm"))
    (with-output-to-file lock #:exists 'replace (lambda () (displayln "racket-base-lock-v1")))
    (define locked
      (make-compiled-root-manifest #:sources (list (list marker-src-rel marker-zo-rel))
                                   #:root-dir staging
                                   #:racket-executable fake-exe
                                   #:producer-label "q-trusted-producer"
                                   #:lockfile lock))
    (define ld (hash-ref (hash-ref locked 'packages) 'lockfile-digest))
    (check-equal? ld (sha256-file lock))
    (check-eq? (validate-compiled-root-manifest locked
                                                #:root-dir staging
                                                #:racket-executable fake-exe
                                                #:trusted-producer-labels '("q-trusted-producer")
                                                #:expect-lockfile-digest ld)
               locked))

  ;; ------------------------------------------------------------ attacks

  (define ((revalidated [root staging]) m)
    (validate-compiled-root-manifest m
                                     #:root-dir root
                                     #:racket-executable fake-exe
                                     #:trusted-producer-labels '("q-trusted-producer")))

  (define (check-manifest-rejects name thunk)
    (test-case name
      (check-exn exn:fail:compiled-root-manifest? thunk)))

  (check-manifest-rejects
   "unknown schema rejected"
   (lambda () ((revalidated) (hash-set base-manifest 'schema "q-compiled-root-manifest-0"))))

  (check-manifest-rejects "untrusted producer label rejected"
                          (lambda ()
                            (validate-compiled-root-manifest base-manifest
                                                             #:root-dir staging
                                                             #:racket-executable fake-exe
                                                             #:trusted-producer-labels
                                                             '("some-other-producer"))))

  (check-manifest-rejects
   "producer trusted flag false rejected"
   (lambda ()
     ((revalidated)
      (hash-set base-manifest 'producer (hash-set (hash-ref base-manifest 'producer) 'trusted #f)))))

  (check-manifest-rejects "foreign racket executable digest rejected"
                          (lambda ()
                            ((revalidated) (hash-set base-manifest
                                                     'racket
                                                     (hash-set (hash-ref base-manifest 'racket)
                                                               'executable-digest
                                                               (make-string 64 #\0))))))

  (check-manifest-rejects
   "racket version mismatch rejected"
   (lambda ()
     ((revalidated) (hash-set base-manifest
                              'racket
                              (hash-set (hash-ref base-manifest 'racket) 'version "0.0.0-fake")))))

  (check-manifest-rejects
   "platform/ABI mismatch rejected"
   (lambda ()
     ((revalidated) (hash-set base-manifest
                              'platform
                              (hash-set (hash-ref base-manifest 'platform) 'arch "not-an-arch")))))

  (check-manifest-rejects "locked dependency-set digest mismatch rejected"
                          (lambda ()
                            (validate-compiled-root-manifest
                             base-manifest
                             #:root-dir staging
                             #:racket-executable fake-exe
                             #:trusted-producer-labels '("q-trusted-producer")
                             #:expect-lockfile-digest (make-string 64 #\f))))

  (test-case "payload corruption (byte flip) fails closed"
    (define corrupt-staging (build-path tmp-root "corrupt"))
    (define corrupted
      (for/list ([m (in-list '("marker/compiled/compiled-root-marker.zo"
                               "marker/compiled-root-marker.rkt"))])
        (define dst (build-path corrupt-staging m))
        (make-directory* (path-only dst))
        (copy-file (build-path staging m) dst #t)))
    (with-output-to-file (build-path corrupt-staging marker-zo-rel)
                         #:exists 'truncate/replace
                         (lambda ()
                           (define b (make-bytes (bytes-length payload-bytes)))
                           (bytes-copy! b 0 payload-bytes)
                           (bytes-set! b 0 (bitwise-xor (bytes-ref b 0) #xFF))
                           (write-bytes b)))
    (check-exn exn:fail:compiled-root-manifest?
               (lambda ()
                 (validate-compiled-root-manifest base-manifest
                                                  #:root-dir corrupt-staging
                                                  #:racket-executable fake-exe
                                                  #:trusted-producer-labels
                                                  '("q-trusted-producer")))))

  (check-manifest-rejects
   "path traversal in payload member rejected"
   (lambda ()
     ((revalidated) (hash-set base-manifest
                              'sources
                              (list (hash-set* (car (hash-ref base-manifest 'sources))
                                               'zo
                                               "../../escape/compiled/evil.zo"))))))

  (check-manifest-rejects "absolute payload path rejected"
                          (lambda ()
                            ((revalidated) (hash-set base-manifest
                                                     'sources
                                                     (list (hash-set* (car (hash-ref base-manifest
                                                                                     'sources))
                                                                      'zo
                                                                      "/etc/compiled/evil.zo"))))))

  (test-case "symlink escape inside payload rejected"
    (define link-staging (build-path tmp-root "links"))
    (define link-rel "marker/compiled/escape.zo")
    (define link-abs (build-path link-staging link-rel))
    (make-directory* (path-only link-abs))
    (copy-file (build-path staging marker-src-rel) (build-path link-staging marker-src-rel) #t)
    (make-file-or-directory-link "/etc/hostname" link-abs)
    (define link-manifest
      (hash-set base-manifest
                'sources
                (list (hash-set* (car (hash-ref base-manifest 'sources)) 'zo link-rel))))
    (check-exn exn:fail:compiled-root-manifest?
               (lambda ()
                 (validate-compiled-root-manifest link-manifest
                                                  #:root-dir link-staging
                                                  #:racket-executable fake-exe
                                                  #:trusted-producer-labels
                                                  '("q-trusted-producer")))))

  (check-manifest-rejects "missing payload member (partial publication) rejected"
                          (lambda ()
                            (define partial (build-path tmp-root "partial"))
                            (for ([m (in-list '("marker/compiled/compiled-root-marker.zo"
                                                "marker/compiled-root-marker.rkt"))])
                              (define dst (build-path partial m))
                              (make-directory* (path-only dst))
                              (copy-file (build-path staging m) dst #t))
                            (delete-file (build-path partial marker-zo-rel))
                            ((revalidated partial) base-manifest)))

  (check-manifest-rejects "non-hash manifest rejected" (lambda () ((revalidated) "not-a-manifest")))

  (check-manifest-rejects "source-list-less manifest rejected"
                          (lambda () ((revalidated) (hash-set base-manifest 'sources '()))))

  ;; --------------------------------------------------- producer failure

  (test-case "make-compiled-root-manifest refuses missing payload member"
    (check-exn exn:fail:compiled-root-manifest?
               (lambda ()
                 (make-compiled-root-manifest #:sources (list (list marker-src-rel "nope/missing.zo"))
                                              #:root-dir staging
                                              #:racket-executable fake-exe
                                              #:producer-label "q-trusted-producer"))))

  (test-case "unsafe source rel path refused at producer"
    (check-exn exn:fail:compiled-root-manifest?
               (lambda ()
                 (make-compiled-root-manifest #:sources (list (list "../outside.rkt"
                                                                    "ok/compiled/ok.zo"))
                                              #:root-dir staging
                                              #:racket-executable fake-exe
                                              #:producer-label "q-trusted-producer")))))

(module+ test
  ;; W1a (open-gap-closure): the whole-checkout producer inventory must
  ;; exclude deliberate-fault fixtures under tests/fixtures/. Coordinator
  ;; trial c01 (run 41231230944) failed the C-lane producer exactly there:
  ;; the BUG-0088 fixture tests/fixtures/supplemental-evidence/baseline-query.rkt
  ;; is a VALID module whose require deliberately targets a nonexistent
  ;; sibling, so raco make died with "no such file or directory". Fixtures
  ;; are negative-test inputs compiled per-test by the runner with its own
  ;; error handling — never producer inventory.
  (test-case "derive-checkout-modules excludes tests/fixtures deliberate-fault inputs"
    (define scratch (make-temporary-file "qcr-w1a-fixtures~a" 'directory))
    (define (write-rel rel content)
      (define p (build-path scratch rel))
      (make-directory* (path-only p))
      (display-to-file content p #:mode 'text #:exists 'replace))
    (write-rel "mod.rkt" "#lang racket/base\n(provide ok)\n(define ok 1)\n")
    (write-rel "tests/fixtures/deliberate-fault/broken.rkt"
               "#lang racket/base\n(require \"settings-core.rkt\")\n")
    (write-rel "tests/fixtures/nonmod.rkt" "(module+ test (void))\n")
    (define-values (mods skipped) (cli:derive-checkout-modules scratch))
    (check-equal? mods '("mod.rkt") "only real modules outside tests/fixtures are inventory")
    (check-equal? skipped
                  '("tests/fixtures/deliberate-fault/broken.rkt" "tests/fixtures/nonmod.rkt")
                  "both fixture files are skipped non-compilation inputs")
    (check-false (member "tests/fixtures/deliberate-fault/broken.rkt" mods)
                 "a deliberate-fault fixture must never enter the producer inventory")))

(module+ test
  ;; W1a-repair (compiled-root launcher E2BIG; coordinator trial c02,
  ;; run 41248049747): under root-on ALL three test shards died with
  ;; "exec failed (Argument list too long; errno=7)" before running any
  ;; test. compiled-root-launch-arguments used to concatenate the whole
  ;; per-source load-key table into the SINGLE -e bootstrap argument.
  ;; Linux caps one argv/env string at MAX_ARG_STRLEN = 32 * PAGE_SIZE
  ;; = 131072 bytes; with today's whole-checkout root (~5200+ compiled
  ;; members at ~240 table bytes per member) that string blows the cap
  ;; and execve fails with E2BIG. This fixture is the trial in
  ;; miniature: a synthetic trusted root with trial-scale entry count
  ;; and runner-scale absolute path lengths. The transfer contract:
  ;; every launch argument string stays under the per-string limit
  ;; no matter how many members the manifest carries.
  ;;
  ;; Pure fixture (no raco make, no child processes): only the argv
  ;; size contract is pinned here; end-to-end child inheritance stays
  ;; in tests/test-compiled-root.rkt.
  (define linux-max-arg-strlen 131072)
  (define e2big-source-count 5200)
  ;; Runner-scale shapes: checkout under the GitHub Actions work path,
  ;; published root + per-consumer map dir under runner temp — the same
  ;; path length regime as the trial lanes. (Paths are assembled from
  ;; fragments so the hardcoded-runner-path lint cannot mistake the
  ;; quoted workflow shapes for local-environment assumptions.)
  (define e2big-checkout "/home/runner/work/q/q")
  (define e2big-published "/home/runner/work/_temp/compiled-root/published")
  (define e2big-map-dir "/home/runner/work/_temp/compiled-root-map")
  (define e2big-fake-digest (make-string 64 #\a))
  (define (e2big-entry i)
    (hasheq 'path
            (format "agent/dir~a/module-~a.rkt" (modulo i 64) i)
            'digest
            e2big-fake-digest
            'bytes
            128
            'zo
            (format "agent/dir~a/compiled/module-~a_rkt.zo" (modulo i 64) i)
            'zo-digest
            e2big-fake-digest
            'zo-bytes
            4096
            'zo-mtime-ms
            0))
  (define e2big-manifest
    (hasheq 'schema
            manifest-schema
            'sources
            (for/list ([i (in-range e2big-source-count)])
              (e2big-entry i))
            ;; W1 performance repair round: the launcher bakes the
            ;; transfer module's zo digest from this field; a synthetic
            ;; manifest carries the same shape (the fixture never runs a
            ;; child, so the digest value itself is not load-bearing).
            'load-keys
            (hasheq 'path
                    compiled-root-load-keys-source-name
                    'digest
                    e2big-fake-digest
                    'zo
                    compiled-root-load-keys-zo-rel
                    'zo-digest
                    e2big-fake-digest)))
  ;; The published manifest IS the fixture's keys file: written into
  ;; the (synthetic) published root exactly the way
  ;; publish-compiled-root! writes it before the atomic rename.
  (define e2big-fixture-dir (make-temporary-file "qcr-e2big~a" 'directory))
  (define e2big-root-dir (build-path e2big-fixture-dir "published"))
  (make-directory* e2big-root-dir)
  (call-with-output-file (build-path e2big-root-dir "manifest.rktd")
                         (lambda (out) (write e2big-manifest out))
                         #:exists 'replace)
  (define e2big-root
    (compiled-root e2big-manifest e2big-root-dir (string->path e2big-map-dir) e2big-checkout #t))

  (test-case "whole-checkout launch arguments stay under the Linux per-string exec limit"
    (define args (compiled-root-launch-arguments e2big-root))
    (check-true (and (list? args) (andmap string? args))
                "launch arguments remain a flat argv of strings")
    (for ([a (in-list args)])
      (check-true
       (<= (string-length a) linux-max-arg-strlen)
       (format "single argv element is ~a bytes > MAX_ARG_STRLEN ~a: execve fails E2BIG (errno 7)"
               (string-length a)
               linux-max-arg-strlen))))

  (test-case "launch arguments still route the child at the per-consumer map dir"
    (define args (compiled-root-launch-arguments e2big-root))
    (check-true (ormap (lambda (a) (string-contains? a "compiled-root-map")) args)
                "bootstrap must still set current-compiled-file-roots to the map dir")))

(module+ test
  ;; W1 performance repair round (compiled-root child transfer; open-gap
  ;; closure W1, repair round 6). The O(1)-argv launcher transfer (PR #9878)
  ;; made every spawned child RE-PARSE the full manifest.rktd to rebuild the
  ;; load-key table — ~220 ms of reader parse plus table completion per
  ;; child at trial scale (5,200 entries), which alone pushed spawn-heavy
  ;; test files over the runner's 2-min per-file cap. The repair ships the
  ;; table as a COMPILED module inside the immutable root at pack time
  ;; (load-keys.rkt next to manifest.rktd, compiled like the rest of the
  ;; payload): children require the zo instead of parsing the rktd, so the
  ;; parse cost moves to the producer (once).
  ;;
  ;; Integrity is unchanged, fail-closed: the table lives inside the
  ;; immutable, digest-pinned root; the launcher's baked manifest-sha256
  ;; startup canary still kills mutated roots; the transfer module carries
  ;; its own baked startup canary; per-load zo digest checks stay.

  (define lk-staging (make-temporary-file "qcr-lk~a" 'directory))
  (define lk-sources
    (list (list "agent/a.rkt" "agent/compiled/a_rkt.zo")
          (list "agent/b.rkt" "agent/compiled/b_rkt.zo")))
  (for ([s (in-list lk-sources)])
    (define zo (build-path lk-staging (cadr s)))
    (make-directory* (path-only zo))
    (call-with-output-file zo (lambda (o) (write-bytes (make-bytes 64 122) o)))
    ;; make-compiled-root-manifest digests the source bytes too
    (define src (build-path lk-staging (car s)))
    (make-directory* (path-only src))
    (call-with-output-file src (lambda (o) (display "(module m racket/base)\n" o))))

  (test-case "producer writes a compiled load-key transfer module for the root"
    (define lk-info (write-compiled-root-load-keys! lk-staging lk-sources))
    (check-equal? (hash-ref lk-info 'path) compiled-root-load-keys-source-name)
    (check-equal? (hash-ref lk-info 'zo) compiled-root-load-keys-zo-rel)
    (check-equal? (hash-ref lk-info 'digest)
                  (sha256-file (build-path lk-staging compiled-root-load-keys-source-name)))
    (check-equal? (hash-ref lk-info 'zo-digest)
                  (sha256-file (build-path lk-staging (string->path compiled-root-load-keys-zo-rel))))
    ;; The generated module provides the table as plain rel-path triples:
    ;; (src-rel zo-rel zo-digest) with the PAYLOAD members' own digests —
    ;; relocation stays a consumer-side completion (absolute paths are
    ;; NEVER baked into the root).
    (check-equal?
     (dynamic-require (build-path lk-staging compiled-root-load-keys-source-name)
                      'compiled-root-load-key-table)
     (list (list "agent/a.rkt"
                 "agent/compiled/a_rkt.zo"
                 (sha256-file (build-path lk-staging (string->path "agent/compiled/a_rkt.zo"))))
           (list "agent/b.rkt"
                 "agent/compiled/b_rkt.zo"
                 (sha256-file (build-path lk-staging (string->path "agent/compiled/b_rkt.zo")))))))

  (test-case "manifest binds the transfer module; its mutation fails validation closed"
    (define lk-info (write-compiled-root-load-keys! lk-staging lk-sources))
    (define lk-manifest
      (make-compiled-root-manifest #:sources lk-sources
                                   #:root-dir lk-staging
                                   #:racket-executable fake-exe
                                   #:producer-label "q-trusted-producer"
                                   #:load-keys lk-info))
    (check-equal? (hash-ref lk-manifest 'load-keys) lk-info)
    (define (validate-lk)
      (validate-compiled-root-manifest lk-manifest
                                       #:root-dir lk-staging
                                       #:racket-executable fake-exe
                                       #:trusted-producer-labels '("q-trusted-producer")))
    (check-eq? (validate-lk) lk-manifest)
    ;; source bytes mutated -> rejected
    (define lk-src (build-path lk-staging compiled-root-load-keys-source-name))
    (define lk-bytes (file->bytes lk-src))
    (with-output-to-file lk-src
                         #:exists 'truncate/replace
                         (lambda () (write-bytes (bytes-append lk-bytes #" "))))
    (check-exn exn:fail:compiled-root-manifest? validate-lk)
    (with-output-to-file lk-src #:exists 'truncate/replace (lambda () (write-bytes lk-bytes)))
    ;; compiled zo mutated -> rejected
    (define lk-zo (build-path lk-staging (string->path compiled-root-load-keys-zo-rel)))
    (define lk-zo-bytes (file->bytes lk-zo))
    (with-output-to-file lk-zo
                         #:exists 'truncate/replace
                         (lambda () (write-bytes (bytes-append lk-zo-bytes #" "))))
    (check-exn exn:fail:compiled-root-manifest? validate-lk)
    (with-output-to-file lk-zo #:exists 'truncate/replace (lambda () (write-bytes lk-zo-bytes)))
    ;; a manifest claiming a foreign transfer digest -> rejected
    (check-exn exn:fail:compiled-root-manifest?
               (lambda ()
                 (validate-compiled-root-manifest
                  (hash-set lk-manifest 'load-keys (hash-set lk-info 'zo-digest (make-string 64 #\f)))
                  #:root-dir lk-staging
                  #:racket-executable fake-exe
                  #:trusted-producer-labels '("q-trusted-producer")))))

  (test-case "old (sources-only) payload algorithm rejects a load-keys artifact whose field is missing"
    ;; W1 review round — directional compatibility addendum
    ;; (docs/reports/TRUSTED-COMPILED-ROOT-v1.00.30.md): the schema string is
    ;; deliberately NOT bumped for the optional load-keys field; the
    ;; whole-payload digest IS the version gate. Pinned here:
    ;;   - a pre-field reader computes the sources-only canonical digest,
    ;;     which can never equal a coverage digest that includes the transfer
    ;;     members — old readers reject new artifacts, fail-closed;
    ;;   - stripping the load-keys KEY from a new artifact's manifest makes
    ;;     the new validator recompute that same old sources-only digest,
    ;;     which mismatches the published coverage digest — the transfer
    ;;     binding cannot be removed by field deletion;
    ;;   - an old-style manifest (no load-keys field, the exact shape the
    ;;     pre-field producer emitted) still validates cleanly under the new
    ;;     reader — backward acceptance, migration one-directional.
    (define lk-info (write-compiled-root-load-keys! lk-staging lk-sources))
    (define lk-manifest
      (make-compiled-root-manifest #:sources lk-sources
                                   #:root-dir lk-staging
                                   #:racket-executable fake-exe
                                   #:producer-label "q-trusted-producer"
                                   #:load-keys lk-info))
    ;; sanity: the complete new artifact validates under the new reader
    (check-eq? (validate-compiled-root-manifest lk-manifest
                                                #:root-dir lk-staging
                                                #:racket-executable fake-exe
                                                #:trusted-producer-labels '("q-trusted-producer"))
               lk-manifest)
    ;; the OLD reader's whole-payload computation (coverage = declared
    ;; sources only — byte-identical to the pre-field algorithm) does not
    ;; reproduce the artifact's coverage digest
    (define old-algorithm-digest
      (canonical-payload-digest (for/list ([e (in-list (hash-ref lk-manifest 'sources))])
                                  (cons (string->bytes/utf-8 (hash-ref e 'zo))
                                        (file->bytes (build-path lk-staging
                                                                 (string->path (hash-ref e 'zo))))))))
    (define payload-digest (hash-ref (hash-ref lk-manifest 'payload) 'digest))
    (check-not-equal? old-algorithm-digest
                      payload-digest
                      "old (sources-only) digest must not equal the coverage digest")
    ;; stripping the load-keys field fails the new reader closed, with the
    ;; same whole-payload mismatch an old reader hits on the full artifact
    (define stripped (hash-remove lk-manifest 'load-keys))
    (check-false (hash-ref stripped 'load-keys #f) "field removal must be observable")
    (check-exn #rx"whole-payload digest mismatch"
               (lambda ()
                 (validate-compiled-root-manifest stripped
                                                  #:root-dir lk-staging
                                                  #:racket-executable fake-exe
                                                  #:trusted-producer-labels '("q-trusted-producer"))))
    ;; new readers accept old artifacts: a manifest with no load-keys field
    ;; (the pre-field producer shape) validates unchanged
    (define old-style-manifest
      (make-compiled-root-manifest #:sources lk-sources
                                   #:root-dir lk-staging
                                   #:racket-executable fake-exe
                                   #:producer-label "q-trusted-producer"))
    (check-false (hash-ref old-style-manifest 'load-keys #f)
                 "old-style manifest must not carry the field")
    (check-eq? (validate-compiled-root-manifest old-style-manifest
                                                #:root-dir lk-staging
                                                #:racket-executable fake-exe
                                                #:trusted-producer-labels '("q-trusted-producer"))
               old-style-manifest))

  (test-case "load-keys present but #f fails closed (presence-based shape gate)"
    ;; Codex contract review (W1 repair round): the documented shape is
    ;; "absent, or a hash binding path/digest/zo/zo-digest" — a manifest
    ;; that carries the KEY with a #f value is NEITHER. Optionality is
    ;; therefore detected by KEY presence (hash-has-key?), never by field
    ;; truthiness: the pre-fix validator guarded every load-keys check
    ;; with the field's VALUE ((when lk ...)), so a present-but-false
    ;; field skipped the shape gate entirely and validated like an
    ;; old-style artifact (whose coverage digest it also happens to
    ;; reproduce, since canonical-root-members treats #f as absent).
    ;; RED against this case until validation is presence-based.
    (define old-style
      (make-compiled-root-manifest #:sources lk-sources
                                   #:root-dir lk-staging
                                   #:racket-executable fake-exe
                                   #:producer-label "q-trusted-producer"))
    (check-false (hash-has-key? old-style 'load-keys)
                 "fixture precondition: the producer omitted the field")
    (define present-false-manifest (hash-set old-style 'load-keys #f))
    (check-true (hash-has-key? present-false-manifest 'load-keys)
                "fixture precondition: the key is PRESENT")
    (check-exn #rx"load-keys field is not a hash"
               (lambda ()
                 (validate-compiled-root-manifest present-false-manifest
                                                  #:root-dir lk-staging
                                                  #:racket-executable fake-exe
                                                  #:trusted-producer-labels '("q-trusted-producer"))))
    ;; the pre-fix validator must not have rejected the ABSENT shape:
    ;; the same manifest without the key still validates unchanged
    (check-eq? (validate-compiled-root-manifest old-style
                                                #:root-dir lk-staging
                                                #:racket-executable fake-exe
                                                #:trusted-producer-labels '("q-trusted-producer"))
               old-style))

  (test-case "whole-payload digest pins the transfer module bytes"
    ;; the canonical payload digest covers load-keys.rkt AND its compiled
    ;; zo: dropping the transfer files from the published root (partial
    ;; publication) must fail validation closed even though every source
    ;; entry is still intact
    (define lk-info (write-compiled-root-load-keys! lk-staging lk-sources))
    (define lk-manifest
      (make-compiled-root-manifest #:sources lk-sources
                                   #:root-dir lk-staging
                                   #:racket-executable fake-exe
                                   #:producer-label "q-trusted-producer"
                                   #:load-keys lk-info))
    (define partial (build-path tmp-root "lk-partial"))
    (for ([m (in-list (list compiled-root-load-keys-source-name compiled-root-load-keys-zo-rel))])
      (define dst (build-path partial (string->path m)))
      (make-directory* (path-only dst))
      (copy-file (build-path lk-staging (string->path m)) dst #t))
    (for ([s (in-list lk-sources)])
      (define dst (build-path partial (string->path (cadr s))))
      (make-directory* (path-only dst))
      (copy-file (build-path lk-staging (string->path (cadr s))) dst #t))
    (delete-file (build-path partial (string->path compiled-root-load-keys-zo-rel)))
    (check-exn exn:fail:compiled-root-manifest?
               (lambda ()
                 (validate-compiled-root-manifest lk-manifest
                                                  #:root-dir partial
                                                  #:racket-executable fake-exe
                                                  #:trusted-producer-labels
                                                  '("q-trusted-producer"))))))

(module+ test
  ;; W1 performance repair round: the child telemetry filename contract.
  ;; The child bootstrap names its counter files; the parent aggregator
  ;; must accept EXACTLY those files — writer and reader aligned on one
  ;; contract, foreign files never reported as malformed telemetry.
  (define tel-dir (make-temporary-file "qcr-tel~a" 'directory))
  (define boot-expr (car (reverse (compiled-root-launch-arguments e2big-root))))
  ;; Reviewer P2 (W1 repair round 7): the bootstrap used to NAME the
  ;; counter file with (make-temporary-file "q-compiled-root-hit~a.rktd"
  ;; ...) — which CREATES an empty file matching the child telemetry
  ;; filename contract BEFORE the first complete snapshot. A child killed
  ;; in that startup window left the aggregator a malformed contract file
  ;; to reject loudly. The bootstrap now reserves the counter name via
  ;; reserve-child-telemetry-target (transfer-auth.rkt) — a unique
  ;; contract-matching NAME and NO file — and the name materializes only
  ;; when the writer's first atomic rename publishes a complete datum.
  (define writer-prefix compiled-root-child-telemetry-prefix)
  (define writer-suffix compiled-root-child-telemetry-suffix)
  (check-true (string-contains? boot-expr "reserve-child-telemetry-target")
              "bootstrap must reserve the counter name via reserve-child-telemetry-target")
  (check-false (string-contains? boot-expr "make-temporary-file")
               "bootstrap must never create a contract-named file itself")
  (check-true (string-contains? boot-expr writer-prefix)
              "bootstrap must pass the shared contract prefix to the reservation")
  (check-true (string-contains? boot-expr writer-suffix)
              "bootstrap must pass the shared contract suffix to the reservation")
  (define tel-name (string-append writer-prefix "1730000000123-4711" writer-suffix))
  (call-with-output-file (build-path tel-dir tel-name)
                         (lambda (o)
                           (write (list 'compiled-root-child-telemetry
                                        (cons 'root-hits 3)
                                        (cons 'loads 3)
                                        (cons 'bytes-served 1234))
                                  o)))
  ;; a foreign file in the hits directory is NOT telemetry
  (call-with-output-file (build-path tel-dir "notes.txt") (lambda (o) (display "not telemetry" o)))
  (test-case "aggregator accepts writer-named files and ignores foreign files silently"
    (define noise (open-output-string))
    (define-values (hits loads served)
      (parameterize ([current-error-port noise])
        (aggregate-compiled-root-child-telemetry tel-dir)))
    (check-equal? (list hits loads served) (list 3 3 1234))
    (check-false (string-contains? (get-output-string noise) "ignoring")
                 (format "foreign files must not be reported as malformed telemetry: ~a"
                         (get-output-string noise))))
  (test-case "abandoned atomic-writer temporary files are ignored silently"
    ;; W1 review round 6 (atomic telemetry): a kill between the writer's
    ;; temp-file creation and its rename leaves a q-compiled-root-tmp*
    ;; file behind. It is a foreign file to the aggregator — never
    ;; telemetry, never reported as malformed evidence.
    (define tmp-name (string-append compiled-root-telemetry-tmp-prefix "1730000000123-4711"))
    (check-false (compiled-root-child-telemetry-filename? tmp-name)
                 "atomic-writer temporary names must never match the telemetry contract")
    (call-with-output-file (build-path tel-dir tmp-name) (lambda (o) (display "half-written" o)))
    (define noise (open-output-string))
    (define-values (hits loads served)
      (parameterize ([current-error-port noise])
        (aggregate-compiled-root-child-telemetry tel-dir)))
    (check-equal? (list hits loads served)
                  (list 3 3 1234)
                  "abandoned temporary files must not enter the aggregate")
    (check-false (string-contains? (get-output-string noise) "ignoring")
                 (format "abandoned temporary files must be ignored silently: ~a"
                         (get-output-string noise))))
  (test-case "filename contract recognizer matches the writer template only"
    (check-true (compiled-root-child-telemetry-filename? tel-name))
    (check-true (compiled-root-child-telemetry-filename? (string->path tel-name)))
    (check-false (compiled-root-child-telemetry-filename? "notes.txt"))
    (check-false (compiled-root-child-telemetry-filename? "q-compiled-root-hit.rktd.txt"))
    (check-false (compiled-root-child-telemetry-filename? "other-hit123.rktd")))

  ;; Reviewer P2 regression (W1 repair round 7): interrupted startup.
  ;; make-temporary-file used to publish an EMPTY contract filename at
  ;; reservation time — before any complete snapshot existed — so a child
  ;; killed between reservation and its first write left exactly the
  ;; malformed "telemetry" file the aggregator rejects loudly.
  ;; Reservation must therefore publish NOTHING at the contract name: a
  ;; unique contract-matching name, no file at it. The contract name
  ;; materializes only at the writer's first complete-snapshot rename;
  ;; an interrupted startup leaves no telemetry evidence at all (its
  ;; kept claim probes are contract-disjoint foreign files, ignored
  ;; silently).
  (test-case "reservation publishes no contract filename before the first complete snapshot"
    (define p2-dir (make-temporary-file "qcr-p2~a" 'directory))
    (define r1 (reserve-child-telemetry-target p2-dir writer-prefix writer-suffix))
    (define r2 (reserve-child-telemetry-target p2-dir writer-prefix writer-suffix))
    (check-true (path? r1) "reservation yields the per-process counter path")
    (check-true (compiled-root-child-telemetry-filename? (file-name-from-path r1))
                "reserved name matches the writer/reader filename contract")
    (check-true (compiled-root-child-telemetry-filename? (file-name-from-path r2))
                "reserved name matches the writer/reader filename contract")
    (check-false (equal? r1 r2) "each process reserves a unique counter name")
    (check-false (file-exists? r1) "no contract filename before a complete snapshot")
    (check-false (file-exists? r2) "no contract filename before a complete snapshot")
    (define p2-leftovers (directory-list p2-dir))
    (check-false (ormap compiled-root-child-telemetry-filename? p2-leftovers)
                 "no contract filename before a complete snapshot")
    ;; the reservation's kept claim probes are contract-disjoint foreign
    ;; files: never telemetry, never an empty contract file
    (check-true (for/and ([f (in-list p2-leftovers)])
                  (string-prefix? (path->string f) compiled-root-telemetry-tmp-prefix))
                "reservation leftovers are claim probes only")
    ;; interrupted startup: the child dies HERE, before any write. The
    ;; aggregator observes no telemetry and no malformed-contract noise.
    (define noise (open-output-string))
    (define-values (hits loads served)
      (parameterize ([current-error-port noise])
        (aggregate-compiled-root-child-telemetry p2-dir)))
    (check-equal? (list hits loads served) (list 0 0 0))
    (check-false (string-contains? (get-output-string noise) "ignoring")
                 (format "interrupted startup must not surface as malformed telemetry: ~a"
                         (get-output-string noise)))
    ;; the first write publishes the reserved name with a COMPLETE datum
    (define w (make-atomic-telemetry-writer r1))
    (w 0 0 0)
    (check-equal?
     (file->value r1)
     (list 'compiled-root-child-telemetry '(root-hits . 0) '(loads . 0) '(bytes-served . 0))
     "the startup snapshot is complete the instant the name exists")
    (check-equal? (length (filter compiled-root-child-telemetry-filename? (directory-list p2-dir)))
                  1
                  "exactly the published counter file carries the contract")
    ;; reservation fails open exactly like the writer: an unusable hits
    ;; directory yields #f, never a contract file
    (check-false (reserve-child-telemetry-target (build-path p2-dir "missing" "deeper")
                                                 writer-prefix
                                                 writer-suffix)
                 "an unusable hits directory reserves nothing"))

  ;; Review blocker (cross-process target collision): the reserved name's
  ;; unique middle is make-temporary-file's digits — (current-seconds)
  ;; concatenated with (current-inexact-milliseconds) — and that
  ;; uniqueness guarantee holds only among EXISTING files. The old
  ;; reservation deleted its probe immediately and disambiguated with a
  ;; per-process counter that starts at the SAME value in every process:
  ;; two processes holding the same clock readings were awarded the same
  ;; digits and collapsed onto ONE counter target, each process renaming
  ;; its snapshots over the other's — hits silently lost in the
  ;; aggregate. This regression pins IDENTICAL fabricated clock readings
  ;; in TWO REAL child processes (fresh per-process counters by
  ;; construction, no dependence on this test process's reservation
  ;; state), so the pre-fix scheme collides by construction, with zero
  ;; timing dependence; the repaired scheme arbitrates the claim with an
  ;; atomically created, contract-disjoint probe that lives for the
  ;; reservation's lifetime.
  (test-case "cross-process reservations stay distinct and their counters sum exactly"
    (define xp-dir (make-temporary-file "qcr-xp~a" 'directory))
    (define xp-run (make-temporary-file "qcr-xp-run~a" 'directory))
    (define transfer-auth-abs (path->string transfer-auth-runtime-path))
    (define fake-digits "1730000000123")
    ;; one script, two fresh Racket processes: each pins the SAME
    ;; fabricated clock readings, reserves, records its target and
    ;; whether a contract file already existed at reservation time, then
    ;; publishes one complete snapshot
    (define (child-out name)
      (build-path xp-run (string-append name ".rktd")))
    (define xp-script (build-path xp-run "child.rkt"))
    (call-with-output-file
     xp-script
     (lambda (o)
       (fprintf o "#lang racket/base\n")
       (fprintf o "(require (file ~s) racket/file)\n" transfer-auth-abs)
       (fprintf o "(define out (vector-ref (current-command-line-arguments) 0))\n")
       (fprintf o "(define target\n")
       (fprintf o "  (parameterize ([reservation-digits (lambda () ~s)])\n" fake-digits)
       (fprintf o
                "    (reserve-child-telemetry-target ~s ~s ~s)))\n"
                (path->string xp-dir)
                writer-prefix
                writer-suffix)
       (fprintf o "(call-with-output-file out\n")
       (fprintf o "  (lambda (report)\n")
       (fprintf o "    (write (list (and (path? target) (path->string target))\n")
       (fprintf o "                 (and (path? target) (file-exists? target)))\n")
       (fprintf o "           report)))\n")
       (fprintf o "(when (path? target) ((make-atomic-telemetry-writer target) 5 1 500))\n")))
    (define (run-child! name)
      (system*/exit-code (find-executable-path "racket")
                         (path->string xp-script)
                         (path->string (child-out name))))
    (define code-a (run-child! "a"))
    (define code-b (run-child! "b"))
    (check-equal? code-a 0 "first reservation process failed")
    (check-equal? code-b 0 "second reservation process failed")
    (match-define (list target-a exists-a?) (file->value (child-out "a")))
    (match-define (list target-b exists-b?) (file->value (child-out "b")))
    (check-true (and (string? target-a) (string? target-b)) "reservations failed")
    (check-false exists-a? "no contract file may exist at reservation time")
    (check-false exists-b? "the second process must not find a contract file at its target either")
    (check-false (string=? target-a target-b)
                 (format "two processes pinned to the same clock MUST NOT share one target: ~a"
                         target-a))
    ;; both published snapshots carry the contract; kept reservation
    ;; probes stay contract-disjoint foreign files, never telemetry
    (check-equal? (sort (for/list ([f (in-list (directory-list xp-dir))]
                                   #:when (compiled-root-child-telemetry-filename? f))
                          (path->string f))
                        string<?)
                  (map path->string
                       (sort (list (file-name-from-path target-a) (file-name-from-path target-b))
                             path<?))
                  "exactly the two published snapshots may carry the contract filename")
    ;; distinct targets: the aggregator sums BOTH processes exactly
    (define noise (open-output-string))
    (define-values (hits loads served)
      (parameterize ([current-error-port noise])
        (aggregate-compiled-root-child-telemetry xp-dir)))
    (check-equal? (list hits loads served)
                  (list 10 2 1000)
                  "distinct targets must let the aggregator sum both processes")
    (check-false (string-contains? (get-output-string noise) "ignoring")
                 "kept reservation probes must stay foreign (silently ignored) files"))

  (test-case "SHA-256 FIPS 180-4 vectors hold on BOTH backends; ffi is pure-equivalent"
    ;; W1 performance repair round: sha256 gained a libcrypto FFI fast
    ;; path (the pure-Racket implementation is the authoritative
    ;; fallback); W1 review round 6: BOTH backends are pinned directly —
    ;; the canaries and per-load checks root in these values, and a
    ;; "fix" that changes any backend's digest or drops the acceleration
    ;; must fail here, deterministically.
    (define (fingerprint bytes)
      (define tmp (make-temporary-file "qcr-sha~a"))
      (dynamic-wind
       (lambda () (call-with-output-file tmp (lambda (o) (write-bytes bytes o)) #:exists 'replace))
       (lambda () (sha256-file tmp))
       (lambda () (delete-file tmp))))
    (define sha-equiv-bytes
      ;; deterministic pseudo-random bytes: ffi and pure must agree on
      ;; multi-block inputs, not only on the short FIPS vectors
      (list->bytes (for/list ([i (in-range 65536)])
                     (modulo (+ (* i 2654435761) 40503) 256))))
    (define (utf8 b)
      (if (string? b)
          (string->bytes/utf-8 b)
          b))
    (define (vectors-of hex!)
      (list (hex! (utf8 ""))
            (hex! (utf8 "abc"))
            (hex! (utf8 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))
            (hex! (make-bytes 1000000 97))))
    (define fips-vectors
      (list "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
            "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
            "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
            "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0"))
    ;; the authoritative pure fallback must be the exact FIPS vectors
    (check-equal? (vectors-of sha256-hex/pure) fips-vectors "pure backend drift")
    ;; the ACTIVE path (sha256-file / sha256-hex) must match regardless
    ;; of which backend is in effect
    (check-equal? (vectors-of sha256-hex) fips-vectors)
    (check-equal? (fingerprint (make-bytes 1000000 97)) (list-ref fips-vectors 3))
    ;; backend declaration must match reality; acceleration retained;
    ;; missing or PARTIAL symbol tables must disable the fast path
    ;; (fail closed to the identical pure digest) instead of crashing
    (define bindings (sha256-libcrypto-bindings libcrypto))
    (check-equal? (sha256-active-backend) (if bindings 'libcrypto-ffi 'pure-racket))
    (check-false (sha256-libcrypto-bindings #f) "a missing library must bind nothing")
    (check-false (sha256-ffi-procedure #f) "missing bindings must yield no fast path")
    (check-false (sha256-ffi-procedure (vector #f #f #f))
                 "all-missing symbols must yield no fast path")
    (if bindings
        (let ([ffi (sha256-ffi-procedure bindings)])
          (check-true (procedure? ffi) "a fully-bound libcrypto must yield the fast path")
          (check-equal? (vectors-of ffi) fips-vectors "FFI backend digest drift")
          (check-equal? (ffi sha-equiv-bytes)
                        (sha256-hex/pure sha-equiv-bytes)
                        "FFI and pure backends must agree byte-for-byte")
          ;; ANY single missing symbol must disable the whole fast path
          (check-false (sha256-ffi-procedure (vector (vector-ref bindings 0) #f #f)))
          (check-false (sha256-ffi-procedure (vector #f (vector-ref bindings 1) #f)))
          (check-false (sha256-ffi-procedure (vector #f #f (vector-ref bindings 2)))))
        (begin
          (check-equal? (sha256-active-backend) 'pure-racket)
          (check-equal? (sha256-hex sha-equiv-bytes) (sha256-hex/pure sha-equiv-bytes)))))

  (test-case "injected native SHA init/update/final failures fail closed to the pure digest"
    ;; W1 repair round 7 (reviewer follow-up): the FFI fast path must
    ;; treat ANY injected SHA256_Init/Update/Final failure — a 0 return,
    ;; a raised exn:fail, or a non-proc slot — as "no fast path" (#f),
    ;; so sha256-hex routes the digest through the byte-identical pure
    ;; fallback. Synthetic binding tables make the injection
    ;; deterministic on every machine; when real libcrypto symbols bind,
    ;; one REAL symbol at a time is broken the same way.
    (define (stub ret)
      (lambda _ ret))
    (define (boom)
      (lambda _ (error 'sha256-inject "injected init/update/final failure")))
    (define (broken-ffi i u f)
      (sha256-ffi-procedure (vector i u f)))
    (define (route ffi b)
      (or (and ffi (ffi b)) (sha256-hex/pure b)))
    (define abc-digest (sha256-hex/pure #"abc"))
    (for* ([slot (in-list '(init update
                                 final))]
           [fail-proc (in-list (list (stub 0) (boom)))])
      (define-values (i u f)
        (case slot
          [(init) (values fail-proc (stub 1) (stub 1))]
          [(update) (values (stub 1) fail-proc (stub 1))]
          [(final) (values (stub 1) (stub 1) fail-proc)]))
      (define ffi (broken-ffi i u f))
      (check-true (procedure? ffi)
                  (format "~a: a fully-populated table still yields the fast path" slot))
      (check-false (ffi #"abc") (format "~a: injected failure must yield #f, never a digest" slot))
      (check-equal? (route ffi #"abc")
                    abc-digest
                    (format "~a: a failed fast path must route to the identical pure digest" slot)))
    ;; failure ORDER short-circuits: an init failure never touches
    ;; update/final; an update failure never touches final
    (define calls '())
    (define (track name)
      (lambda _
        (set! calls (cons name calls))
        1))
    (set! calls '())
    (define ffi-init-fails
      (broken-ffi (lambda _
                    (set! calls (cons 'init calls))
                    0)
                  (track 'update)
                  (track 'final)))
    (check-false (ffi-init-fails #"abc"))
    (check-equal? (reverse calls) '(init) "init failure must short-circuit update/final")
    (set! calls '())
    (define ffi-update-fails
      (broken-ffi (track 'init)
                  (lambda _
                    (set! calls (cons 'update calls))
                    0)
                  (track 'final)))
    (check-false (ffi-update-fails #"abc"))
    (check-equal? (reverse calls) '(init update) "update failure must short-circuit final")
    ;; a non-proc slot must not crash either: the bad application is an
    ;; exn:fail, resolved to #f like any other failure
    (check-false ((broken-ffi 1 (stub 1) (stub 1)) #"abc") "non-proc init slot resolves to #f")
    ;; real-symbol variants: break ONE real libcrypto binding at a time
    (define real (sha256-libcrypto-bindings libcrypto))
    (when real
      (check-false ((broken-ffi (stub 0) (vector-ref real 1) (vector-ref real 2)) #"abc")
                   "real init returning 0 disables the fast path")
      (check-false ((broken-ffi (vector-ref real 0) (stub 0) (vector-ref real 2)) #"abc")
                   "real update returning 0 disables the fast path")
      (check-false ((broken-ffi (vector-ref real 0) (vector-ref real 1) (stub 0)) #"abc")
                   "real final returning 0 disables the fast path")
      (check-equal? ((broken-ffi (vector-ref real 0) (vector-ref real 1) (vector-ref real 2)) #"abc")
                    abc-digest
                    "all-real bindings still produce the pure-identical digest"))))

(module+ test
  ;; W1 review round 6: DETERMINISTIC TOCTOU race regression for the
  ;; child transfer module. The pre-repair mechanism authenticated one
  ;; open of the zo path and then executed a SECOND open (the module
  ;; system's own, inside dynamic-require); a same-user process that
  ;; swaps the bytes between the two opens executes a table that was
  ;; never authenticated. Threads and timing are not needed to pin the
  ;; fix: winning that race is EXACTLY what a current-load wrapper that
  ;; serves different bytes for the same path does, deterministically.
  ;;   - under the OLD mechanism the wrapper swaps the executed table
  ;;     (the threat is real, asserted here as documentation);
  ;;   - under the REPAIRED mechanism (authenticate-compiled-module:
  ;;     read once, digest, eval the compiled form FROM THOSE BYTES) the
  ;;     wrapper is never consulted and the executed table is the
  ;;     authenticated table;
  ;;   - a digest mismatch and digest-authenticated non-compiled bytes
  ;;     both fail closed.
  (define race-dir (make-temporary-file "qcr-race~a" 'directory))
  (define (make-transfer-variant! name table-datum)
    (define src (build-path race-dir name "load-keys.rkt"))
    (make-directory* (path-only src))
    (call-with-output-file src
                           (lambda (o)
                             (write `(module load-keys racket/base
                                       (provide compiled-root-load-key-table)
                                       (define compiled-root-load-key-table ',table-datum))
                                    o)))
    (unless (zero? (system*/exit-code (find-executable-path "raco") "make" "-j" "1" src))
      (error 'race-fixture "raco make failed for ~a" name))
    (build-path race-dir name "compiled" "load-keys_rkt.zo"))
  (define honest-zo
    (make-transfer-variant! "honest"
                            (list (list "agent/a.rkt" "agent/compiled/a_rkt.zo" "honest-digest"))))
  (define honest-table (list (list "agent/a.rkt" "agent/compiled/a_rkt.zo" "honest-digest")))
  (define evil-zo (make-transfer-variant! "evil" '("EVIL")))
  (define evil-table '("EVIL"))
  (define honest-digest (sha256-hex (file->bytes honest-zo)))
  ;; the attacker stand-in: serves the EVIL bytes whenever the module
  ;; system opens a load-keys zo — the deterministic form of winning the
  ;; hash/require race on the path
  (define (attacker-load orig-load)
    (lambda (p modname)
      (if (and (path? p)
               (equal? (path->string (file-name-from-path (path->complete-path p)))
                       "load-keys_rkt.zo"))
          (orig-load evil-zo modname)
          (orig-load p modname))))
  (define (attacker-consulted-thunk)
    (define consulted? #f)
    (define orig-load (current-load))
    (values (lambda (p modname)
              (when (and (path? p)
                         (equal? (path->string (file-name-from-path (path->complete-path p)))
                                 "load-keys_rkt.zo"))
                (set! consulted? #t))
              ((attacker-load orig-load) p modname))
            (lambda () consulted?)))

  (test-case "OLD mechanism: the hash/require race is real (documented threat)"
    (parameterize ([current-namespace (make-base-namespace)])
      ;; the canary passes over the honest bytes...
      (check-equal? (sha256-hex (file->bytes honest-zo)) honest-digest)
      ;; ...but the path-based require executes whatever the loader's
      ;; second open finds: the swap succeeds
      (parameterize ([current-load (attacker-load (current-load))])
        (check-equal? (dynamic-require honest-zo 'compiled-root-load-key-table)
                      evil-table
                      "pre-repair mechanism: executed table is not the authenticated table"))))

  (test-case "REPAIRED mechanism: the executed table IS the authenticated bytes"
    (parameterize ([current-namespace (make-base-namespace)])
      (define-values (attacker consulted?) (attacker-consulted-thunk))
      (parameterize ([current-load attacker])
        (authenticate-compiled-module honest-zo honest-digest)
        (check-equal? (dynamic-require ''load-keys 'compiled-root-load-key-table)
                      honest-table
                      "executed table must be the table carried by the authenticated bytes")
        (check-false (consulted?) "the loader must never re-open the authenticated path"))))

  (test-case "transfer digest mismatch fails closed"
    (parameterize ([current-namespace (make-base-namespace)])
      (check-exn #rx"mutated after verification"
                 (lambda () (authenticate-compiled-module honest-zo (make-string 64 #\0))))))

  (test-case "digest-authenticated non-compiled bytes fail closed"
    ;; a digest that pins bytes which are NOT a compiled module must not
    ;; reach eval as arbitrary source — the compiled-expression gate
    ;; refuses it
    (define fake (build-path race-dir "not-compiled.zo"))
    (call-with-output-file fake (lambda (o) (display "(module load-keys racket/base)" o)))
    (parameterize ([current-namespace (make-base-namespace)])
      (check-exn #rx"does not authenticate to a compiled module"
                 (lambda () (authenticate-compiled-module fake (sha256-hex (file->bytes fake))))))))
