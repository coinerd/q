#lang racket
;; @speed fast
;; @suite extensions
;; @boundary integration
(require rackunit
         "helpers/supplemental-evidence-fixture.rkt"
         "../extensions/gsd/evidence-discovery.rkt"
         "../extensions/gsd/evidence-repair-format.rkt"
         "../extensions/gsd/evidence-probe.rkt"
         "../extensions/gsd/review-proof.rkt"
         "../extensions/gsd/verification-job.rkt"
         "../util/json/checksum.rkt"
         "../extensions/gsd/composition-root.rkt")

(module+ test
  (test-case "supplemental authority: exact Git pins and helper bytes gate the owned overlay"
    (call-with-supplemental-fixture
     (lambda (repo baseline)
       (define helper (build-path repo fixture-helper-path))
       (define approved (list (hasheq 'path fixture-helper-path 'sha256 (sha256-file helper))))
       (define context (fixture-context repo))
       (define command "racket scripts/run-tests.rkt tests/probe.rkt")
       (define (discover [pins approved] [base baseline] [test fixture-test-path])
         (discover-evidence-probe-outcome repo
                                          context
                                          command
                                          #:harness-files pins
                                          #:baseline-head base
                                          #:test-path test))
       (define (refuses pins code phase path)
         (define outcome (discover pins))
         (check-equal? (hash-ref outcome 'status) "refused")
         (check-equal? (hash-ref outcome 'code) code)
         (check-equal? (hash-ref outcome 'phase) phase)
         (check-equal? (hash-ref outcome 'path) path))
       (define admitted (discover))
       (check-equal? (hash-ref admitted 'status) "admitted")
       (define spec (hash-ref admitted 'spec))
       (check-true (valid-probe-spec? spec))
       (check-equal? (hash-ref spec 'baseline-head) baseline)
       (check-equal? (hash-ref spec 'harness-files) approved)
       (refuses '() "helper-dependency-unapproved" "test-module" fixture-helper-path)
       (refuses (list (hash-set (car approved) 'sha256 (make-string 64 #\0)))
                "harness-digest-mismatch"
                "helper-manifest"
                fixture-helper-path)
       (check-equal? (hash-ref (discover approved (hash-ref context 'head)) 'status) "refused")
       (check-equal? (hash-ref (discover approved baseline "tests/absent.rkt") 'status) "refused")
       (define proofs (make-temporary-file "b4-supplemental-proof-~a" 'directory))
       (dynamic-wind
        void
        (lambda ()
          (parameterize ([current-owned-proof-root (build-path proofs "owned")]
                         [current-gsd-verification-registry (make-verification-registry)])
            (check-exn #rx"unapproved frozen regression coverage"
                       (lambda ()
                         (prepare-baseline-probe!
                          repo
                          (hash-set spec 'focused-normalized-command "racket tests/probe.rkt")
                          "bad-stage")))
            (define prepared
              (prepare-baseline-probe! repo (hash-set spec 'timeout-sec 120) "supplemental"))
            (for* ([role '(baseline candidate)]
                   [file (append (hash-ref spec 'test-files) approved)])
              (check-equal? (sha256-file (build-path (hash-ref prepared role) (hash-ref file 'path)))
                            (hash-ref file 'sha256)))
            ;; The base probe contract owns executed RED/GREEN and replay; the
            ;; deep supplemental owner runs the real settings closure twice.
            (define ref (run-baseline-probe! repo prepared "supplemental" #:guard (lambda () #f)))
            (check-equal? (hash-ref ref 'code) "stale-guard")
            (check-false (hash-has-key? ref 'sha256))
            (check-exn exn:fail?
                       (lambda ()
                         (authorize-evidence-registration repo context ref "supplemental")))))
        (lambda () (delete-directory/files proofs)))))))
