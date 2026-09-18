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
         racket/system
         rackunit
         "../ci/prepared-environment/compiled-root-manifest.rkt"
         "../ci/prepared-environment/compiled-root.rkt")

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
