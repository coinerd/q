#lang racket
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 180
;; @covers extensions/gsd/red-checkpoint.rkt
(require rackunit
         racket/runtime-path
         "helpers/delivery-fixtures.rkt"
         "helpers/private-fixture-templates.rkt"
         "../extensions/gsd/plan-snapshot.rkt"
         "../extensions/gsd/review-proof.rkt"
         "../extensions/gsd/composition-root.rkt"
         "../extensions/gsd/verification-job.rkt")
(define-runtime-path engine "../extensions/gsd/red-checkpoint.rkt")
(define plan (make-string 64 #\a))
(module+ test
  (test-case "owned preimplementation RED needs no GREEN and freezes tests through implementation"
    (define capture #f)
    (define validate #f)
    (define green #f)
    (check-not-exn (lambda ()
                     (set! capture (dynamic-require engine 'capture-red-checkpoint!))
                     (set! validate (dynamic-require engine 'validate-red-checkpoint!))
                     (set! green (dynamic-require engine 'verify-red-checkpoint-green!))))
    (when (and capture validate)
      (define root (make-temporary-file "red-checkpoint-~a" 'directory))
      (define repo (build-path root "q"))
      (define live? (box #t))
      (define (guard)
        (unbox live?))
      (define (put path text)
        (display-to-file text (build-path repo path) #:exists 'truncate))
      (define (head)
        (define out (open-output-string))
        (parameterize ([current-directory repo]
                       [current-output-port out])
          (check-true (system* (find-executable-path "git") "rev-parse" "HEAD")))
        (string-trim (get-output-string out)))
      (dynamic-wind
       void
       (lambda ()
         (make-directory repo)
         (git-quiet! repo "init" "-q")
         (hermetic-identity! repo)
         (git-quiet! repo "remote" "add" "origin" "https://github.com/example/q.git")
         (put ".gitignore" "compiled/\n")
         (put "source.rkt" "#lang racket/base\n(provide answer)\n(define (answer) 0)\n")
         (git-quiet! repo "add" ".")
         (git-quiet! repo "commit" "-qm" "unfixed source")
         (git-quiet! repo "checkout" "-qb" "campaign/checkpoint")
         (define baseline (head))
         (make-directory* (build-path root ".planning" "waves"))
         (write-plan! root 0 "Checkpoint" "checkpoint")
         (write-wave-doc! root
                          0
                          "checkpoint"
                          '("source.rkt" "tests/probe.rkt")
                          "raco test tests/probe.rkt")
         (make-plan-snapshot! root plan (file->string (build-path root ".planning" "PLAN.md")))
         (make-directory (build-path repo "tests"))
         (define regression
           (string-append
            "#lang racket/base\n(require rackunit \"../source.rkt\")\n(provide evidence-suite)\n"
            "(define evidence-suite (test-suite \"suite\" (test-case \"target\" (check-equal? (answer) 1))))\n"
            "(module+ test (require rackunit/text-ui) (exit (run-tests evidence-suite)))\n"))
         (put "tests/probe.rkt" regression)
         (git-quiet! repo "add" ".")
         (git-quiet! repo "commit" "-qm" "regression only")
         (parameterize ([current-owned-proof-root (build-path root "owned-proofs")]
                        [current-gsd-verification-registry (make-verification-registry)])
           (define result (capture root plan 0 repo baseline "attempt-1" 1 #:guard guard))
           (check-equal? (hash-ref result 'status) "red-ready" (format "~s" result))
           (check-equal? (capture root plan 0 repo baseline "attempt-1" 1 #:guard guard) result)
           (check-true (hash? (validate root plan 0 repo "attempt-1" 1 #:guard guard)))
           (check-exn exn:fail? (lambda () (validate root plan 0 repo "attempt-2" 1 #:guard guard)))
           (put "source.rkt" "#lang racket/base\n(provide answer)\n(define (answer) 1)\n")
           (git-quiet! repo "add" ".")
           (git-quiet! repo "commit" "-qm" "implementation")
           (check-true (hash? (validate root plan 0 repo "attempt-1" 1 #:guard guard)))
           (define passed (green root plan 0 repo "attempt-1" 1 #:guard guard))
           (check-equal? (hash-ref passed 'status) "red-ready")
           (check-equal? (hash-ref (hash-ref passed 'green) 'status) "captured")
           (check-equal? (green root plan 0 repo "attempt-1" 1 #:guard guard) passed)
           (set-box! live? #f)
           (check-exn exn:fail? (lambda () (green root plan 0 repo "attempt-1" 1 #:guard guard)))
           (set-box! live? #t)
           (put "tests/probe.rkt" (string-append regression "; changed after RED\n"))
           (git-quiet! repo "add" ".")
           (git-quiet! repo "commit" "-qm" "test mutation must refuse")
           (check-exn exn:fail? (lambda () (green root plan 0 repo "attempt-1" 1 #:guard guard)))
           (check-exn exn:fail? (lambda () (validate root plan 0 repo "attempt-1" 1 #:guard guard)))
           (set-box! live? #f)
           (check-exn exn:fail? (lambda () (validate root plan 0 repo "attempt-1" 1 #:guard guard)))))
       (lambda () (delete-directory/files root))))))
