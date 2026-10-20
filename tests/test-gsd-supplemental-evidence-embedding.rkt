#lang racket/base
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 300
;; Separate trusted-owner and /go processes, isolated HOME; no live store access.
(require rackunit
         racket/file
         racket/runtime-path
         racket/system
         racket/port
         "helpers/supplemental-go-fixture.rkt"
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         (submod "../extensions/gsd/supplemental-evidence-authority.rkt" owner-frontend)
         "../extensions/gsd-planning/execution-policy.rkt"
         "../extensions/gsd/supplemental-evidence-controller.rkt")
(define-runtime-path self "test-gsd-supplemental-evidence-embedding.rkt")
(define-runtime-path cli "../scripts/gsd-evidence-replan.rkt")
(define (child-expression module)
  (format "(require (submod (file ~s) ~a))" (path->string self) module))
(module embedding-child racket/base
  (require rackunit
           "../extensions/gsd/command-handlers.rkt"
           "../extensions/gsd/go-orchestrator.rkt"
           "../extensions/gsd/campaign-repository.rkt"
           "../extensions/gsd/composition-root.rkt"
           "../extensions/gsd/review-proof.rkt"
           "../extensions/gsd/delivery-receipt.rkt"
           "../extensions/gsd/delivery-verifier.rkt"
           "../extensions/gsd/verification-job.rkt"
           "../extensions/gsd/supplemental-evidence-controller.rkt"
           "../extensions/gsd/supplemental-evidence-authority.rkt"
           "../extensions/gsd-planning/execution-policy.rkt")
  ;; Same production owner route used by /go, with a bounded fake model runner.
  ;; Stop at dispatch: this test must not publish or manufacture proofs.
  (define argv (current-command-line-arguments))
  (define root (vector-ref argv 0))
  (define plan (vector-ref argv 1))
  (define calls 0)
  (define observed-phase #f)
  (parameterize ([current-owned-proof-root (build-path root "owned-proofs")]
                 [current-gsd-verification-registry (make-verification-registry)]
                 [current-gsd-delivery-branch-context
                  (make-branch-delivery-context #:repo-root (build-path root "q")
                                                #:branch "campaign/w1"
                                                #:base-commit (vector-ref argv 2)
                                                #:worktree-path (build-path root "q"))])
    (void (run-campaign-wave root
                             (load-campaign-record root plan)
                             1
                             #:fence-token 8
                             #:timeout-sec 30
                             #:timeout-retries 0
                             #:no-change-retries 0
                             #:stall-soft-limit #f
                             #:stall-hard-limit #f
                             #:isolate? #f
                             #:advance-override? #t
                             #:runner (lambda (_)
                                        (set! observed-phase (current-gsd-author-phase))
                                        (set! calls (add1 calls))
                                        'ok)
                             #:verifier
                             (lambda (_) (error 'embedding "unexpected ordinary verify")))))
  (unless (and (hash? observed-phase) (eq? (hash-ref observed-phase 'phase #f) 'test-author))
    (error 'embedding "FAIL: /go did not admit the authenticated supplemental test-author lane"))
  (check-equal? calls 1 "fresh /go process must authenticate the owner's retained task")
  (parameterize ([current-supplemental-authority-root #f])
    (check-exn exn:fail? (lambda () (propose-retain! "task-proposal" (hasheq))))))
(module+ test
  (test-case "trusted provisioning is private, inert, and denied to author phases or explicit #f"
    (define home (make-temporary-file "supplemental-provision-~a" 'directory))
    (define root (build-path home "private"))
    (dynamic-wind void
                  (lambda ()
                    (parameterize ([current-supplemental-authority-root #f])
                      (check-exn exn:fail? initialize-supplemental-authority-root!))
                    (parameterize ([current-supplemental-authority-root root])
                      (parameterize ([current-gsd-author-phase (hasheq 'phase 'test-author)])
                        (check-exn #rx"author phase" initialize-supplemental-authority-root!))
                      (check-false (directory-exists? root))
                      (check-equal? (initialize-supplemental-authority-root!) root)
                      (check-equal? (file-or-directory-permissions root 'bits) #o700)
                      (check-equal? (directory-list root) '())
                      (check-equal? (initialize-supplemental-authority-root!) root)))
                  (lambda () (delete-directory/files home))))
  (test-case "owner CLI and fresh production /go route share the trusted user store"
    (define home (make-temporary-file "supplemental-embedding-home-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (call-with-supplemental-fixture
        (lambda (fx)
          ;; Seed only the sandbox home with real broker-issued fixture records.
          (make-directory* (build-path home ".q" "agent"))
          (copy-directory/files (supplemental-fixture-private fx)
                                (build-path home ".q" "agent" "supplemental-evidence"))
          (file-or-directory-permissions (build-path home ".q" "agent" "supplemental-evidence") #o700)
          (define pointer
            (load-supplemental-pointer (supplemental-fixture-root fx)
                                       (supplemental-fixture-plan fx)
                                       1))
          (define ref (hash-ref pointer 'task-proposal))
          (define env (environment-variables-copy (current-environment-variables)))
          (environment-variables-set! env #"HOME" (path->bytes home))
          (parameterize ([current-environment-variables env])
            (check-true (system* (find-executable-path "racket")
                                 "-e"
                                 (child-expression 'embedding-child)
                                 "--"
                                 (path->string (supplemental-fixture-root fx))
                                 (supplemental-fixture-plan fx)
                                 (hash-ref (supplemental-fixture-task-context fx) 'head)))
            (define output (open-output-string))
            (check-true (parameterize ([current-output-port output])
                          (system* (find-executable-path "racket")
                                   cli
                                   "--verb"
                                   "inspect"
                                   "--kind"
                                   "task-proposal"
                                   "--sha256"
                                   (hash-ref ref 'sha256)))
                        "owner CLI must use the same default store without a root flag")
            (check-regexp-match #rx"supplemental-proposal-store/v1" (get-output-string output))))))
     (lambda () (delete-directory/files home)))))
