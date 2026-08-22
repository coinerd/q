;; @suite ci
;; @speed fast
;; @boundary unit
;; @isolation process
;; @mutates temp

#lang racket/base

;; q/tests/ci/verify-lock-selection-test.rkt — W1 required-green matrix for
;; the runtime-indexed lock gate in ci/verify-racket-package-lock.rkt.
;;
;; WAVE: W1 of docs/planning/PLAN-v1.00.11-TDD-CI-INTEGRITY-BASELINES.md
;; ("Make Racket cache and lock verification runtime-specific").
;;
;; HISTORY: W0 froze the red state (single-runtime lock rejected the 8.11
;; cross-version gate, CI run 32532206128). W1 replaced the lock with the
;; version-indexed schema-revision-2 lock and made the verifier select
;; exactly one entry by exact version match. This test flips the W0
;; known-red characterization into the required-green behavior matrix:
;;
;;   1. real lock + 8.10  -> accept  (exit 0, lock-ok runtime=8.10)
;;   2. real lock + 8.11  -> accept  (exit 0, lock-ok runtime=8.11)  [W0 red, now green]
;;   3. 8.10-only lock + 8.11 -> reject (no lock entry; no cross-runtime fallback)
;;   4. real lock + 8.12 (unlisted) -> reject
;;
;;   Plus: the two accepted selections emit DISTINCT lock digests, so no
;;   runtime's cache key can be satisfied by another runtime's entry (I3).
;;
;; HARNESS: only Racket 8.10 is installed here, so the requested runtime is
;; driven by the verifier's `--racket-version` flag (contract input #1).
;; The package-store stage after selection is environment-dependent, so a
;; stub `raco` is placed first on PATH reproducing the locked package table
;; (what `raco pkg show --long --full-checksum` would emit). The verifier
;; source itself is invoked unmodified from the repository.

(require rackunit
         racket/file
         racket/format
         racket/hash
         racket/list
         racket/match
         racket/path
         racket/port
         racket/string
         racket/system)

;; ------------------------------------------------------------
;; Locating the system under test
;; ------------------------------------------------------------

;; `find-system-path 'run-file` names the *racket/raco executable*, not this
;; test file, so use the module's own source syntax (works under both
;; `racket -t` and `raco test`).
(define this-file
  (let ([src (syntax-source #'here)])
    (cond [(path? src) (simplify-path (path->complete-path src))]
          [else (simplify-path (find-system-path 'run-file))])))
(define repo-root
  (simplify-path (build-path (path-only this-file) 'up 'up)))
(define verifier-path (build-path repo-root "ci" "verify-racket-package-lock.rkt"))
(define real-lock-path (build-path repo-root "ci" "racket-package-lock.rktd"))

;; ------------------------------------------------------------
;; The real schema-revision-2 lock must carry both reviewed runtimes
;; ------------------------------------------------------------

(define real-lock (call-with-input-file real-lock-path read))

(test-case
    "real lock is schema-revision 2 with reviewed 8.10 and 8.11 entries"
  (check-equal? (hash-ref real-lock 'schema-revision) 2)
  (define runtimes (hash-ref real-lock 'runtimes))
  (check-true (hash? runtimes) "runtimes must be a hash")
  (for ([(v e) (in-hash runtimes)])
    (check-pred hash? e)
    (check-equal? (hash-ref e 'racket-version) v)
    (check-pred hash? (hash-ref e 'packages))
    (check > (hash-count (hash-ref e 'packages)) 0)
    ;; Separate reviewed ENTRY per runtime — never a shared permissive lock.
    ;; The package tables themselves MAY be identical: the same reviewed
    ;; sources resolve under both runtimes (see the lock header), and
    ;; cross-runtime separation comes from the per-entry racket-version
    ;; binding plus the version-bound lock digest (asserted by I3 below).
    (for ([other '("8.10" "8.11")]
          #:unless (string=? other v))
      (check-not-equal? e
                        (hash-ref runtimes other #f)
                        (format "entries ~a/~a are the same entry" v other)))))

(define lock-8.10 (hash-ref (hash-ref real-lock 'runtimes) "8.10"))

;; A runtime that no lock in this repo has ever listed.
(define unlisted-runtime "8.12")

;; ------------------------------------------------------------
;; Harness: run the UNMODIFIED verifier with a chosen lock, requested
;; version, and stub package store
;; ------------------------------------------------------------

(define (write-executable! path content)
  (call-with-output-file path
    #:exists 'replace
    (lambda (p) (display content p)))
  (file-or-directory-permissions path #o755))

;; Stub `raco pkg show --scope user --all --long --full-checksum`: emit one
;; line per locked package plus the `q` checkout-link row. The verifier's
;; package-rx accepts `<name> <40-hex-checksum|#f> ...columns...`.
(define (make-raco-stub! dir packages)
  (define lines
    (for/list ([(name checksum) (in-hash packages)])
      (format "echo '~a ~a link x x'" name checksum)))
  (write-executable!
   (build-path dir "raco")
   (string-append
    "#!/bin/sh\n"
    "if [ \"$1\" = pkg ] && [ \"$2\" = show ]; then\n"
    (string-join (append lines (list "echo 'q #f checkout x x'")) "\n")
    "\nfi\nexit 0\n")))

(define (with-lock-root lock-sexpr thunk)
  (define root (make-temporary-file "q-w1-lockroot-~a" 'directory))
  (make-directory* (build-path root "ci"))
  (call-with-output-file (build-path root "ci" "racket-package-lock.rktd")
    (lambda (p) (write lock-sexpr p)))
  (begin0 (thunk root)
    (delete-directory/files root)))

;; run-verifier : hash? string? -> (list/c exact-nonnegative-integer?
;;                                          string? string?)
;; Runs the real verifier file as a subprocess with cwd = a temp root whose
;; ci/racket-package-lock.rktd is `lock-sexpr`, requesting `version` via the
;; `--racket-version` flag, with a stub raco (serving `packages` from the
;; lock's selected entry) first on PATH.
(define (run-verifier lock-sexpr version #:packages [packages #f])
  (define stub-dir (make-temporary-file "q-w1-racostub-~a" 'directory))
  (make-raco-stub! stub-dir
                   (or packages
                       (hash-ref (hash-ref (hash-ref lock-sexpr 'runtimes) version)
                                 'packages)))
  (define racket-bin (or (find-executable-path "racket")
                         (error 'verify-lock-selection-test "racket not on PATH")))
  (define saved-path (getenv "PATH"))
  (begin0
    (with-lock-root
     lock-sexpr
     (lambda (root)
       (parameterize ([current-directory root])
         (dynamic-wind
           (lambda () (putenv "PATH" (string-append (path->string stub-dir) ":" (or saved-path ""))))
           (lambda ()
             (define-values (proc out in err)
               (subprocess #f #f #f racket-bin (path->string verifier-path)
                           "--racket-version" version))
             (close-output-port in)
             (define stdout (port->string out))
             (define stderr (port->string err))
             (subprocess-wait proc)
             (list (subprocess-status proc) stdout stderr))
           (lambda () (putenv "PATH" (or saved-path "")))))))
    (delete-directory/files stub-dir)))

(define (lock-digest-of stdout)
  (match (regexp-match #px"lock-digest=([0-9a-f]{64})" stdout)
    [(list _ d) d]
    [_ #f]))

;; The runtime the CI cross-version gate runs under.
(define ci-cross-runtime "8.11")

(let ([result (run-verifier real-lock "8.10")])
  (test-case
      "matrix 1: real lock, requested 8.10 -> accepted"
    (check-equal? (first result) 0)
    (check-regexp-match #px"lock-ok runtime=8\\.10 schema-revision=2 lock-digest=[0-9a-f]{64}"
                        (second result))))

(let ([result (run-verifier real-lock ci-cross-runtime)])
  (test-case
      "matrix 2 (W0 known-red, now required-green): real lock, requested 8.11 -> accepted"
    (check-equal? (first result) 0)
    (check-regexp-match #px"lock-ok runtime=8\\.11 schema-revision=2 lock-digest=[0-9a-f]{64}"
                        (second result))))

(let* ([result-810 (run-verifier real-lock "8.10")]
       [result-811 (run-verifier real-lock ci-cross-runtime)])
  (test-case
      "I3: accepted 8.10 and 8.11 selections emit distinct lock digests"
    (check-equal? (first result-810) 0)
    (check-equal? (first result-811) 0)
    (define d810 (lock-digest-of (second result-810)))
    (define d811 (lock-digest-of (second result-811)))
    (check-pred string? d810)
    (check-pred string? d811)
    (check-not-equal? d810 d811)))

(let ([only-lock
       (hash 'schema-revision 2
             'runtimes (hash "8.10" lock-8.10))]
      [entry lock-8.10])
  (let ([result (run-verifier only-lock ci-cross-runtime
                              #:packages (hash-ref entry 'packages))])
    (test-case
        "matrix 3: lock with only an 8.10 entry, requested 8.11 -> rejected (no cross-runtime fallback)"
      (check-equal? (first result) 1)
      (check-regexp-match #px"no lock entry for Racket version 8\\.11" (third result))
      (check-regexp-match #px"8\\.10" (third result) "message lists the available entries"))))

(let ([result (run-verifier real-lock unlisted-runtime
                           #:packages (hash-ref lock-8.10 'packages))])
  (test-case
      "matrix 4: unlisted runtime 8.12 -> rejected"
    (check-equal? (first result) 1)
    (check-regexp-match #px"no lock entry for Racket version 8\\.12" (third result))))

(let ([legacy-lock #hash((schema-version . 1)
                         (racket-version . "8.10")
                         (packages . #hash()))])
  (let ([result (run-verifier legacy-lock "8.10"
                              #:packages (hash-ref lock-8.10 'packages))])
    (test-case
        "schema guard: schema-revision-1 lock -> rejected with migration message"
      (check-equal? (first result) 1)
      (check-regexp-match #px"schema-revision" (third result)))))
