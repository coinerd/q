#lang racket
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @timeout 180
;; One owned pair and portable replay; the variant ladder stays in full.
(require rackunit
         "helpers/supplemental-evidence-fixture.rkt"
         "../extensions/gsd/composition-root.rkt"
         "../extensions/gsd/evidence-probe.rkt"
         "../extensions/gsd/review-proof.rkt"
         "../extensions/gsd/verification-job.rkt"
         "../scripts/gsd-evidence-probe-runner.rkt"
         "../util/json/checksum.rkt")

(module+ test
  (test-case "owned probe: strict ancestor, single-use registration, portable replay and tamper refusal"
    (define root (make-temporary-file "b4-probe-~a" 'directory))
    (define repo (build-path root "repo"))
    (define test "tests/probe.rkt")
    (define operation "b4-witness")
    (dynamic-wind
     void
     (lambda ()
       (make-directory repo)
       (define (git . args)
         (apply fixture-git repo args))
       (define (source! n)
         (fixture-put repo
                      "source.rkt"
                      (format "#lang racket/base\n(provide answer)\n(define answer ~a)\n" n)))
       (git "init" "-qb" "fixture")
       (git "config" "user.name" "Witness")
       (git "config" "user.email" "witness@invalid")
       (git "config" "commit.gpgsign" "false")
       (source! 0)
       (git "add" "source.rkt")
       (git "commit" "-qm" "baseline")
       (define baseline (git "rev-parse" "HEAD"))
       (define baseline-tree (git "rev-parse" "HEAD^{tree}"))
       (source! 1)
       (fixture-put
        repo
        test
        (string-append
         "#lang racket/base\n(require rackunit \"../source.rkt\")\n(provide evidence-suite)\n"
         "(define evidence-suite (test-suite \"suite\" (test-case \"target\" (check-equal? answer 1))))\n"
         "(module+ test (require rackunit/text-ui) (exit (run-tests evidence-suite)))\n"))
       (git "add" "source.rkt" test)
       (git "commit" "-qm" "candidate")
       (define context (fixture-context repo))
       (define path (build-path repo test))
       (define observed (collect-evidence-suite path))
       (check-true (hash-ref observed 'suite-completed))
       (check-equal? (hash-ref observed 'successes) 1)
       (define check (car (hash-ref (car (hash-ref observed 'cases)) 'checks)))
       (define spec
         (hasheq 'kind
                 "probe-spec/v1"
                 'context
                 context
                 'baseline-head
                 baseline
                 'baseline-tree
                 baseline-tree
                 'focused-normalized-command
                 "raco test tests/probe.rkt"
                 'test-paths
                 (list test)
                 'test-files
                 (list (hasheq 'path test 'sha256 (sha256-file path)))
                 'harness-files
                 '()
                 'suite-export
                 "evidence-suite"
                 'target
                 (hasheq 'test-path
                         test
                         'suite-path
                         '("suite")
                         'test-case-name
                         "target"
                         'check-name
                         (hash-ref check 'name)
                         'check-location
                         (hash-ref check 'location))
                 'runtime
                 (hasheq 'racket-version (version) 'platform (symbol->string (system-type 'os)))
                 'timeout-sec
                 30))
       (parameterize ([current-owned-proof-root (build-path root "owned")]
                      [current-gsd-verification-registry (make-verification-registry)])
         (check-exn exn:fail? (lambda () (prepare-baseline-probe! repo spec "../escape")))
         (check-exn exn:fail?
                    (lambda ()
                      (prepare-baseline-probe! repo
                                               (hash-set spec 'baseline-head (hash-ref context 'head))
                                               operation)))
         (check-equal? (hash-ref (run-baseline-probe! repo #f operation #:guard (lambda () #f)) 'code)
                       "stale-guard")
         (define prepared (prepare-baseline-probe! repo spec operation))
         (define ref (run-baseline-probe! repo prepared operation #:guard (lambda () #t)))
         (check-true (hash-has-key? ref 'sha256) (format "~s" ref))
         (define proof (validate-baseline-probe! repo ref context))
         (check-equal? (hash-ref proof 'chronology) "posthoc-reproduction")
         (check-exn exn:fail?
                    (lambda ()
                      (consume-evidence-registration-authorization! ref repo context ref operation)))
         (define token (authorize-evidence-registration repo context ref operation))
         (check-true (consume-evidence-registration-authorization! token repo context ref operation))
         (check-exn
          exn:fail?
          (lambda () (consume-evidence-registration-authorization! token repo context ref operation)))
         (check-exn exn:fail?
                    (lambda ()
                      (recover-evidence-registration-authorization repo
                                                                   context
                                                                   ref
                                                                   "other"
                                                                   #:guard (lambda () #t))))
         (define recovered
           (recover-evidence-registration-authorization repo
                                                        context
                                                        ref
                                                        operation
                                                        #:guard (lambda () #t)))
         (check-true
          (consume-evidence-registration-authorization! recovered repo context ref operation))
         (define bundle (build-path root "portable"))
         (define clone (build-path root "clone"))
         (export-baseline-probe! repo ref context bundle)
         (git "clone" "--no-local" "--quiet" (path->string repo) (path->string clone))
         (rename-file-or-directory (build-path root "owned") (build-path root "offline-owner"))
         (check-equal? (validate-exported-baseline-probe! clone bundle ref context) proof)
         (check-exn
          exn:fail?
          (lambda () (validate-exported-baseline-probe! clone bundle ref (hash-set context 'wave 2))))
         (display-to-file "tamper" (build-path bundle "objects.bundle") #:exists 'append)
         (check-exn exn:fail?
                    (lambda () (validate-exported-baseline-probe! clone bundle ref context)))
         (rename-file-or-directory (build-path root "offline-owner") (build-path root "owned"))
         (define raw (hash-ref (hash-ref (car (hash-ref proof 'results)) 'raw-log) 'path))
         (file-or-directory-permissions raw #o644)
         (display-to-file "{}" raw #:exists 'truncate)
         (check-exn exn:fail? (lambda () (validate-baseline-probe! repo ref context)))))
     (lambda () (delete-directory/files root))))
  (test-case "probe adapter refuses load, setup and stdout-only claims; reports the actual failing case"
    (define p (make-temporary-file "b4-probe-adapter-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (define (collect body)
         (define source (make-temporary-file "case-~a.rkt" #f p))
         (display-to-file
          (string-append
           "#lang racket/base\n(require rackunit)\n(provide evidence-suite)\n"
           body
           "\n(module+ test (require rackunit/text-ui) (exit (run-tests evidence-suite)))\n")
          source
          #:exists 'truncate)
         (parameterize ([current-output-port (open-output-nowhere)])
           (collect-evidence-suite source)))
       (check-false (hash-ref (collect "(define evidence-suite undefined-binding)") 'compile-ok))
       (check-false (hash-ref (collect "(displayln \"FAILURE target\")\n(define evidence-suite 7)")
                              'suite-completed))
       (check-false
        (hash-ref (collect (string-append "(define evidence-suite (test-suite \"suite\" "
                                          "#:before (lambda () (error 'setup \"broken\")) "
                                          "(test-case \"target\" (check-equal? 1 2))))"))
                  'suite-completed))
       (define report
         (collect (string-append "(define evidence-suite (test-suite \"suite\" "
                                 "(test-case \"other\" (check-equal? 1 2)) "
                                 "(test-case \"target\" (check-equal? 2 2))))")))
       (check-equal? (hash-ref report 'failures) 1)
       (check-equal? (hash-ref (car (hash-ref report 'cases)) 'test-case-name) "other"))
     (lambda () (delete-directory/files p)))))
