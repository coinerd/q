#lang racket
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @timeout 600
(require rackunit
         json
         racket/system
         racket/port
         racket/string
         racket/path
         "../extensions/gsd/review-proof.rkt"
         "../extensions/gsd/composition-root.rkt"
         "../extensions/gsd/verification-job.rkt"
         "../util/json/checksum.rkt"
         racket/file
         "../scripts/gsd-evidence-probe-runner.rkt"
         "../extensions/gsd/evidence-repair-format.rkt"
         "../extensions/gsd/evidence-probe.rkt")
(define (fixture body)
  (define p (make-temporary-file "evidence-~a.rkt"))
  (call-with-output-file
   p
   #:exists 'truncate
   (lambda (out)
     (display (string-append
               "#lang racket/base\n(require rackunit)\n(provide evidence-suite)\n"
               body
               "\n(module+ test (require rackunit/text-ui) (exit (run-tests evidence-suite)))\n")
              out)))
  p)
(module+ test
  (define bad
    (fixture
     "(define evidence-suite (test-suite \"suite\" (test-case \"target\" (check-equal? 1 2))))"))
  (define broken (fixture "(define evidence-suite undefined-binding)"))
  (define marker (fixture "(displayln \"FAILURE target\")\n(define evidence-suite 7)"))
  (define good
    (fixture
     "(define evidence-suite (test-suite \"suite\" (test-case \"target\" (check-equal? 2 2))))"))
  (define setup
    (fixture
     "(define evidence-suite (test-suite \"suite\" #:before (lambda () (error 'setup \"broken\")) (test-case \"target\" (check-equal? 1 2))))"))
  (define unrelated
    (fixture
     "(define evidence-suite (test-suite \"suite\" (test-case \"other\" (check-equal? 1 2)) (test-case \"target\" (check-equal? 2 2))))"))
  (define shadow
    (fixture
     (string-append
      "(define-syntax-rule (check-equal? a b) (void))\n"
      "(define evidence-suite (test-suite \"suite\" (test-case \"target\" (check-equal? 1 2))))")))
  (check-false (hash-ref (collect-evidence-suite shadow) 'suite-completed))
  (delete-file shadow)
  (check-equal? (canonical-evidence-json (hasheq 'z 1 'a (list #t 'null)))
                "{\"a\":[true,null],\"z\":1}")
  (check-exn exn:fail? (lambda () (canonical-evidence-json 1.5)))
  (check-equal? (canonical-evidence-digest (hasheq 'x 1 'sha256 "ignored"))
                (canonical-evidence-digest (hasheq 'x 1)))
  (check-equal? (hash-ref (run-baseline-probe! #f #f "op" #:guard (lambda () #f)) 'code)
                "stale-guard")
  (check-exn exn:fail? (lambda () (authorize-evidence-registration #f #f #f "op")))
  ;; Actual owned registry jobs, detached ancestor overlay and offline replay.
  (define repo (make-temporary-file "probe-repo-~a" 'directory))
  (define proofs (make-temporary-file "probe-proof-~a" 'directory))
  (define (git! . args)
    (define out (open-output-string))
    (check-true (parameterize ([current-directory repo]
                               [current-output-port out])
                  (apply system* (find-executable-path "git") args)))
    (string-trim (get-output-string out)))
  (define (put path text)
    (display-to-file text (build-path repo path) #:exists 'truncate))
  (parameterize ([current-owned-proof-root (build-path proofs "owned")]
                 [current-gsd-verification-registry (make-verification-registry)])
    (git! "init" "-q")
    (git! "config" "user.name" "Probe")
    (git! "config" "user.email" "probe@invalid")
    (put "source.rkt" "#lang racket/base\n(provide answer)\n(define (answer) 0)\n")
    (git! "add" "source.rkt")
    (git! "commit" "-qm" "wrong baseline")
    (define baseline (git! "rev-parse" "HEAD"))
    (define baseline-tree (git! "rev-parse" "HEAD^{tree}"))
    (make-directory (build-path repo "tests"))
    (put
     "tests/probe.rkt"
     (string-append
      "#lang racket/base\n(require rackunit \"../source.rkt\")\n(provide evidence-suite)\n"
      "(define evidence-suite (test-suite \"suite\" (test-case \"target\" (check-equal? (answer) 1))))\n"
      "(module+ test (require rackunit/text-ui) (exit (run-tests evidence-suite)))\n"))
    (put "source.rkt" "#lang racket/base\n(provide answer)\n(define (answer) 1)\n")
    (git! "add" "source.rkt" "tests/probe.rkt")
    (git! "commit" "-qm" "candidate with regression")
    (define c
      (hasheq 'plan-id
              (make-string 64 #\a)
              'wave
              1
              'attempt-id
              "attempt"
              'attempt-fence
              0
              'receipt-generation
              0
              'receipt-sha256
              (make-string 64 #\b)
              'evidence-revision
              0
              'repo
              (path->string repo)
              'branch
              "master"
              'origin
              "local"
              'head
              (git! "rev-parse" "HEAD")
              'tree
              (git! "rev-parse" "HEAD^{tree}")
              'snapshot-sha256
              (make-string 64 #\c)
              'green-proof-sha256
              (make-string 64 #\d)))
    (define telemetry (collect-evidence-suite (build-path repo "tests/probe.rkt")))
    (define identity (car (hash-ref (car (hash-ref telemetry 'cases)) 'checks)))
    (define s
      (hasheq
       'kind
       "probe-spec/v1"
       'context
       c
       'baseline-head
       baseline
       'baseline-tree
       baseline-tree
       'focused-normalized-command
       "raco test tests/probe.rkt"
       'test-paths
       '("tests/probe.rkt")
       'test-files
       (list
        (hasheq 'path "tests/probe.rkt" 'sha256 (sha256-file (build-path repo "tests/probe.rkt"))))
       'harness-files
       '()
       'suite-export
       "evidence-suite"
       'target
       (hasheq 'test-path
               "tests/probe.rkt"
               'suite-path
               '("suite")
               'test-case-name
               "target"
               'check-name
               (hash-ref identity 'name)
               'check-location
               (hash-ref identity 'location))
       'runtime
       (hasheq 'racket-version (version) 'platform (symbol->string (system-type 'os)))
       'timeout-sec
       30))
    (check-exn exn:fail? (lambda () (prepare-baseline-probe! repo s "../escape")))
    (define p (prepare-baseline-probe! repo s "actual"))
    (define ref (run-baseline-probe! repo p "actual" #:guard (lambda () #t)))
    (check-true (hash-has-key? ref 'sha256) (format "~s" ref))
    (when (hash-has-key? ref 'sha256)
      (define m (validate-baseline-probe! repo ref c))
      (check-equal? (hash-ref m 'chronology) "posthoc-reproduction")
      (define token (authorize-evidence-registration repo c ref "actual"))
      (check-false (hash? token))
      (check-exn exn:fail?
                 (lambda () (consume-evidence-registration-authorization! ref repo c ref "actual")))
      (check-true (consume-evidence-registration-authorization! token repo c ref "actual"))
      (check-exn exn:fail?
                 (lambda () (consume-evidence-registration-authorization! token repo c ref "actual")))
      (define recovered
        (recover-evidence-registration-authorization repo c ref "actual" #:guard (lambda () #t)))
      (check-true (consume-evidence-registration-authorization! recovered repo c ref "actual"))
      (check-exn
       exn:fail?
       (lambda ()
         (recover-evidence-registration-authorization repo c ref "other" #:guard (lambda () #t))))
      (define exported (build-path proofs "portable"))
      (export-baseline-probe! repo ref c exported)
      (define candidate-clone (build-path proofs "candidate-clone"))
      (git! "clone" "--no-local" "--quiet" (path->string repo) (path->string candidate-clone))
      (delete-directory/files (hash-ref p 'baseline))
      (delete-directory/files (hash-ref p 'candidate))
      (define owner (build-path proofs "owned"))
      (define moved (build-path proofs "moved-owner"))
      (rename-file-or-directory owner moved)
      (check-equal? (validate-exported-baseline-probe! candidate-clone exported ref c) m)
      (check-exn
       exn:fail?
       (lambda ()
         (validate-exported-baseline-probe! candidate-clone exported ref (hash-set c 'wave 2))))
      (display-to-file "tamper" (build-path exported "objects.bundle") #:exists 'append)
      (check-exn exn:fail?
                 (lambda () (validate-exported-baseline-probe! candidate-clone exported ref c)))
      (rename-file-or-directory moved owner)
      (define raw-ref (hash-ref (car (hash-ref m 'results)) 'raw-log))
      (file-or-directory-permissions (hash-ref raw-ref 'path) #o644)
      (display-to-file "{}" (hash-ref raw-ref 'path) #:exists 'truncate)
      (check-exn exn:fail? (lambda () (validate-baseline-probe! repo ref c))))
    (define stale (prepare-baseline-probe! repo s "stale"))
    (define calls 0)
    (define stale-ref
      (run-baseline-probe! repo
                           stale
                           "stale"
                           #:guard (lambda ()
                                     (set! calls (add1 calls))
                                     (< calls 3))))
    (check-equal? (hash-ref stale-ref 'status) "blocked")
    (check-true (regexp-match? #rx"stale guard" (hash-ref stale-ref 'diagnostic)))
    ;; The PASS protocol comes from the actual adapter at this exact test
    ;; path/check location, not a hand-written approximation.  Only the imported
    ;; production function changes; the candidate still has the baseline bug.
    (for ([termination '("(exit 0)" "(kill-thread (current-thread))")]
          [serial (in-naturals)])
      (test-case (format "owned adapter rejects stdout/termination spoof ~a" serial)
        (define op (format "stdout-spoof-~a" serial))
        (check-true (hash-ref telemetry 'suite-completed))
        (check-equal? (hash-ref telemetry 'successes) 1)
        (put "source.rkt"
             (format (string-append "#lang racket/base\n(provide answer)\n"
                                    "(define (answer)\n"
                                    "  (call-with-output-file \"/dev/stdout\" #:exists 'append\n"
                                    "    (lambda (out) (display ~s out) (flush-output out)))\n"
                                    "  ~a\n"
                                    "  0)\n")
                     (jsexpr->string telemetry)
                     termination))
        (git! "add" "source.rkt")
        (git! "commit" "-qm" "unchanged bug with forged adapter output")
        ;; exit0 spoofs ordinary success; raco already detects test-thread kill.
        ;; The owned adapter must reject BOTH termination paths independently.
        ;; Baseline remains the genuine failing ancestor above.
        (define ordinary-success?
          (parameterize ([current-directory repo]
                         [current-output-port (open-output-nowhere)]
                         [current-error-port (open-output-nowhere)])
            (system* (find-executable-path "raco") "test" "tests/probe.rkt")))
        (check-equal? ordinary-success? (zero? serial))
        (define spoof-context
          (hash-set (hash-set c 'head (git! "rev-parse" "HEAD"))
                    'tree
                    (git! "rev-parse" "HEAD^{tree}")))
        (define spoof-spec (hash-set s 'context spoof-context))
        (define spoof-prepared (prepare-baseline-probe! repo spoof-spec op))
        (define spoof-ref (run-baseline-probe! repo spoof-prepared op #:guard (lambda () #t)))
        (check-equal? (hash-ref spoof-ref 'status #f)
                      "blocked"
                      "forged PASS telemetry must not become an owned evidence reference")
        (check-false (hash-has-key? spoof-ref 'sha256)
                     "no evidence authority may be published for the spoof")
        (check-exn exn:fail?
                   (lambda () (authorize-evidence-registration repo spoof-context spoof-ref op)))))
    (for ([variant '(compile setup unrelated)])
      (define body
        (case variant
          [(compile) "(define evidence-suite missing-binding)"]
          [(setup)
           (string-append
            "(define evidence-suite (test-suite \"suite\" #:before (lambda () (error 'setup \"broken\")) "
            "(test-case \"target\" (check-equal? (answer) 1))))")]
          [else
           (string-append
            "(define evidence-suite (test-suite \"suite\" (test-case \"other\" (check-equal? 1 2)) "
            "(test-case \"target\" (check-equal? (answer) 1))))")]))
      (put "tests/probe.rkt"
           (string-append
            "#lang racket/base\n(require rackunit \"../source.rkt\")\n(provide evidence-suite)\n"
            body
            "\n"))
      (git! "add" "tests/probe.rkt")
      (git! "commit" "-qm" (symbol->string variant))
      (define vc
        (hash-set (hash-set c 'head (git! "rev-parse" "HEAD"))
                  'tree
                  (git! "rev-parse" "HEAD^{tree}")))
      (define vs
        (hash-set (hash-set s 'context vc)
                  'test-files
                  (list (hasheq 'path
                                "tests/probe.rkt"
                                'sha256
                                (sha256-file (build-path repo "tests/probe.rkt"))))))
      (define vp (prepare-baseline-probe! repo vs (symbol->string variant)))
      (define vr (run-baseline-probe! repo vp (symbol->string variant) #:guard (lambda () #t)))
      (check-equal? (hash-ref vr 'status) "blocked")
      (check-exn exn:fail?
                 (lambda () (authorize-evidence-registration repo vc vr (symbol->string variant))))))
  (delete-directory/files repo)
  (delete-directory/files proofs)
  (dynamic-wind void
                (lambda ()
                  (check-equal? (hash-ref (collect-evidence-suite bad) 'failures) 1)
                  (check-false (hash-ref (collect-evidence-suite broken) 'compile-ok))
                  (check-false (hash-ref (collect-evidence-suite marker) 'suite-completed))
                  (define pass (collect-evidence-suite good))
                  (check-true (hash-ref pass 'suite-completed))
                  (check-equal? (hash-ref pass 'successes) 1)
                  (check-false (hash-ref (collect-evidence-suite setup) 'suite-completed))
                  (define failure (car (hash-ref (collect-evidence-suite unrelated) 'cases)))
                  (check-equal? (hash-ref failure 'test-case-name) "other")
                  (check-equal? (hash-ref failure 'suite-path) '("suite"))
                  (check-not-false (member "location"
                                           (map (lambda (i) (hash-ref i 'name))
                                                (hash-ref failure 'check-info)))))
                (lambda () (for-each delete-file (list bad broken marker good setup unrelated)))))
