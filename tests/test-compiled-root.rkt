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
         "../ci/prepared-environment/compiled-root.rkt")

(module+ test
  (define tmp-base (make-temporary-file "qcr-e2e~a" 'directory))

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
      (unless (zero?
               (apply system*/exit-code (find-executable-path "git") "-C" (path->string dir) args))
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
    (with-output-to-file zo-abs #:exists 'truncate/replace (lambda () (write-bytes orig-bytes)))))
