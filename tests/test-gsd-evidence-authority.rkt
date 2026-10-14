#lang racket
(require rackunit json
         "helpers/delivery-fixtures.rkt" "helpers/private-fixture-templates.rkt"
         "../extensions/gsd/campaign-state.rkt" "../extensions/gsd/composition-root.rkt"
         "../extensions/gsd/verification-job.rkt" "../extensions/gsd/delivery-verifier.rkt"
         "../extensions/gsd/delivery-receipt.rkt" "../extensions/gsd/plan-snapshot.rkt"
         "../extensions/gsd/review-proof.rkt" "../extensions/gsd/evidence-readiness.rkt"
         "../extensions/gsd/evidence-probe.rkt" "../extensions/gsd/evidence-discovery.rkt" "../extensions/gsd/evidence-repair-format.rkt"
         "../extensions/gsd/evidence-repair-ledger.rkt" "../scripts/gsd-evidence-probe-runner.rkt"
         "../scripts/gsd-binding-data.rkt" "../scripts/gsd-gates/owned-audit.rkt"
         "../extensions/gsd/review-audit-format.rkt" "../util/json/checksum.rkt")
(module+ test
  (define root (make-temporary-file "authority-~a" 'directory))
  (define repo (build-path root "q"))
  (define plan (make-string 64 #\a))
  (define command "raco fmt -i source.rkt && raco make source.rkt && racket scripts/run-tests.rkt tests/probe.rkt && racket scripts/run-tests.rkt --suite fast && racket scripts/lint-all.rkt")
  (dynamic-wind void
    (lambda ()
      (make-directory repo)
      (git-quiet! repo "init" "-q") (hermetic-identity! repo)
      (git-quiet! repo "checkout" "-qb" "campaign/w2")
      (git-quiet! repo "remote" "add" "origin" "https://github.com/example/q.git")
      (define (put path text)
        (make-directory* (path-only (build-path repo path)))
        (display-to-file text (build-path repo path) #:exists 'truncate))
      (put ".gitignore" "compiled/\n.planning/\nproofs/\npublication/\nclone/\n")
      (put "source.rkt" "#lang racket/base\n(provide answer)\n(define (answer) 0)\n")
      (git-quiet! repo "add" "source.rkt" ".gitignore") (git-quiet! repo "commit" "-qm" "baseline")
      (define baseline (hash-ref (committed-delivery-snapshot repo) 'head))
      (define baseline-tree (hash-ref (committed-delivery-snapshot repo) 'tree))
      (put "source.rkt" "#lang racket/base\n(provide answer)\n(define (answer) 1)\n")
      (put "tests/probe.rkt" "#lang racket/base\n(require rackunit \"../source.rkt\")\n(provide evidence-suite)\n(define evidence-suite (test-suite \"suite\" (test-case \"target\" (check-equal? (answer) 1))))\n(module+ test (require rackunit/text-ui) (exit (run-tests evidence-suite)))\n")
      (put "scripts/run-tests.rkt" "#lang racket/base\n(require racket/runtime-path)\n(define-runtime-path test \"../tests/probe.rkt\")\n(dynamic-require `(submod ,test test) #f)\n")
      (put "scripts/lint-all.rkt" "#lang racket/base\n(require syntax/modread)\n(for ([p '(\"source.rkt\" \"tests/probe.rkt\")]) (call-with-input-file p (lambda (in) (with-module-reading-parameterization (lambda () (read-syntax p in))))))\n")
      ;; Run the actual formatter before freezing the clean candidate.
      (parameterize ([current-directory repo])
        (check-true (system* (find-executable-path "raco") "fmt" "-i" "source.rkt")))
      (git-quiet! repo "add" "source.rkt" "tests/probe.rkt" "scripts/run-tests.rkt" "scripts/lint-all.rkt")
      (git-quiet! repo "commit" "-qm" "candidate")
      (make-directory* (build-path root ".planning" "waves"))
      (write-plan! root 2 "Proof" "proof")
      (write-wave-doc! root 2 "proof" '("q/source.rkt") command)
      (make-plan-snapshot! root plan (file->string (build-path root ".planning" "PLAN.md")))
      (define rec (make-campaign-record plan (make-campaign-manifest 1 "proof" '() '() "constraints")
                   (list (make-campaign-wave 2 "proof" 'awaiting-delivery 1 (campaign-attempt "attempt-1" 7 0))) #f 7 'test 0 0))
      (parameterize ([current-owned-proof-root (build-path root "proofs")]
                     [current-gsd-verification-registry (make-verification-registry)]
                     [current-gsd-delivery-verify-timeout-sec 60])
        (define verification-result (verify-campaign-delivery root plan 2 repo
          (make-branch-delivery-context #:repo-root (hash-ref (committed-delivery-snapshot repo) 'repo)
            #:branch (hash-ref (committed-delivery-snapshot repo) 'branch)
            #:base-commit (hash-ref (committed-delivery-snapshot repo) 'head) #:worktree-path repo)
          (lambda (_) (define check (check-verify-command root 2 #f))
            (delivery-verification (cadr check) (list check) "actual miniature chain"))
          (lambda () rec) #:remote-published (lambda args #t)))
        (check-true (delivery-verification-approved? verification-result))
        (define context (current-evidence-context root plan 2))
        (define spec (discover-evidence-probe-spec repo context command))
        (check-true (hash? spec))
        (reserve-evidence-repair! root context (canonical-evidence-digest spec))
        (advance-evidence-repair! root context "repair-1" "reserved" (hasheq 'state "intent"))
        (advance-evidence-repair! root context "repair-1" "intent" (hasheq 'state "start"))
        (define prepared (prepare-baseline-probe! repo spec "repair-1"))
        (define probe (run-baseline-probe! repo prepared "repair-1" #:guard (lambda () #t)))
        (advance-evidence-repair! root context "repair-1" "start" (hasheq 'state "captured" 'probe probe))
        (register-owned-evidence! root plan 2 #:context context #:probe probe #:operation-id "repair-1"
          #:authorization (authorize-evidence-registration (hash-ref context 'repo) context probe "repair-1"))
        (define staging (build-path root "publication"))
        (define authority (owned-review-authority root plan 2 staging))
        ;; Portable validation uses a candidate clone, not the capture worktrees.
        (define clone (build-path root "clone"))
        (git-quiet! repo "clone" "--quiet" "--no-local" (path->string repo) (path->string clone))
        (copy-directory/files (build-path staging "docs") (build-path clone "docs"))
        (delete-directory/files (hash-ref prepared 'baseline))
        (delete-directory/files (hash-ref prepared 'candidate))
        (delete-directory/files (build-path root "proofs"))
        (define audited (validate-owned-authority clone authority))
        (check-equal? (hash-ref (hash-ref audited 'red-facts) 'origin) "posthoc-reproduction")
        (check-equal? (hash-ref authority 'evidence-revision) 1)
        (define binding (audit-binding (hash-set* (hash-ref authority 'expected)
          'generation (hash-ref authority 'generation) 'proof-sha256 (hash-ref (hash-ref authority 'reference) 'sha256)
          'content-digest "content" 'evidence-revision 1
          'evidence-registration-sha256 (hash-ref authority 'evidence-registration-sha256))))
        (define common (hasheq 'review-mode owned-audit-mode 'audit-binding binding
          'owned-proof (hash-ref authority 'reference) 'owned-red probe))
        (define evidence (hash-set* common 'implementation-sha (hash-ref context 'head)
          'content-digest "content" 'plan-id plan 'wave "W2" 'branch (hash-ref context 'branch)))
        (define review (hash-set common 'audit-ack (hasheq 'mode owned-audit-mode
          'verification-origin "audited-owned" 'source-read #t 'raw-output-read #t 'binding binding
          'red-ack (hasheq 'proof-sha256 (hash-ref probe 'sha256) 'raw-output-read #t))))
        (define red (hash-ref authority 'red))
        (define validation (for/fold ([v common]) ([(k fact) (in-hash (hash-ref authority 'green-gates))])
          (hash-set v k fact)))
        (define full-validation (hash-set validation 'red-first (hasheq 'command (hash-ref red 'command)
          'failure (hash-ref red 'failure-summary) 'result "passed" 'verification-origin "audited-owned"
          'owner-job-id (hash-ref red 'owner-job-id))))
        (define bound-authority (hash-set authority 'audit-binding binding))
        (check-equal? (validate-owned-audit clone evidence review full-validation bound-authority) '())
        (check-not-equal? (validate-owned-audit clone evidence review full-validation
          (hash-set bound-authority 'audit-binding (hash-set binding 'evidence-revision 0))) '())
        (for ([key '(evidence-revision evidence-registration-sha256)])
          (check-exn exn:fail? (lambda () (validate-owned-authority clone (hash-set authority key "tampered")))))
        (check-exn exn:fail? (lambda () (validate-owned-authority clone
          (hash-set authority 'red (hash-set (hash-ref authority 'red) 'owner-job-id "fabricated")))))
        (check-not-false (for/or ([p (hash-ref authority 'publication-files)]) (regexp-match? #rx"objects.bundle$" p)))))
    (lambda () (delete-directory/files root))))
