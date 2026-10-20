#lang racket
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 240
;; @covers extensions/gsd/supplemental-evidence-proof.rkt
;; Genuine fixture owned jobs + real interactive broker decisions. No live
;; campaign, network, or fabricated manifest is used as positive proof.
(require rackunit
         json
         "helpers/delivery-fixtures.rkt"
         "helpers/private-fixture-templates.rkt"
         "../runtime/approval/broker.rkt"
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         (submod "../extensions/gsd/supplemental-evidence-authority.rkt" owner-frontend)
         "../extensions/gsd/supplemental-evidence-format.rkt"
         "../extensions/gsd/supplemental-evidence-journal.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/supplemental-evidence-proof.rkt"
         "../extensions/gsd/effective-verify-binding.rkt"
         "../extensions/gsd/evidence-repair-format.rkt"
         "../extensions/gsd/review-proof.rkt"
         "../extensions/gsd/delivery-verifier.rkt"
         "../extensions/gsd/delivery-receipt.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/composition-root.rkt"
         "../extensions/gsd/verification-job.rkt"
         "../extensions/gsd/plan-snapshot.rkt"
         (only-in "../util/json/checksum.rkt" sha256-file sha256-string))
(define campaign-manifest (make-campaign-manifest 1 "fixture" '() '() "constraints"))
(define plan (campaign-manifest-hash campaign-manifest))
(define c0
  (string-join
   '("raco fmt -i runtime/local.rkt" "raco make runtime/local.rkt"
                                     "racket scripts/run-tests.rkt tests/test-existing.rkt"
                                     "racket scripts/run-tests.rkt --suite fast"
                                     "racket scripts/lint-all.rkt")
   " && "))
(define test-path "tests/test-witness.rkt")
(define helper-path "tests/helpers/witness.rkt")
(define c1 (make-effective-verify-command c0 test-path))
(define (put root relative text)
  (define p (build-path root relative))
  (make-directory* (path-only p))
  (display-to-file text p #:exists 'truncate))
(define (interactive-grant ref)
  (define digest (hash-ref ref 'sha256))
  (define id
    (register-approval-request-for-channel! (current-approval-channel) digest (hasheq 'proposal ref)))
  (unless (and id (approval-decide! id digest #t))
    (error 'fixture "broker decision failed"))
  (define-values (outcome grant) (approval-await-grant id digest 1000))
  (unless (eq? outcome 'approved)
    (error 'fixture "broker grant unavailable"))
  grant)
(define (approve ref)
  (approve-supplemental-proposal! ref (interactive-grant ref)))
(define (check-command root)
  (define check (check-verify-command root 1 #f))
  (delivery-verification (cadr check) (list check) (format "~a" check)))
(define (git-text cwd . args)
  (define out (open-output-string))
  (parameterize ([current-output-port out])
    (unless (zero? (apply system*/exit-code (find-executable-path "git") "-C" cwd args))
      (error 'fixture "git failed")))
  (string-trim (get-output-string out)))
(define (with-fixture proc #:jobs [jobs #f] #:passes? [passes? #t])
  (define root (make-temporary-file "supplemental-owned-~a" 'directory))
  (define repo (build-path root "q"))
  (define cwd (path->string repo))
  (define private (build-path root "private"))
  (dynamic-wind
   (lambda ()
     (when (current-approval-channel)
       (error 'fixture "active frontend"))
     (set-approval-channel! (make-approval-channel #:timeout-ms 1000)))
   (lambda ()
     (make-directory repo)
     (make-directory private #o700)
     (git-quiet! repo "init" "-q")
     (hermetic-identity! repo)
     (git-quiet! repo "checkout" "-qb" "campaign/w1")
     (git-quiet! repo "remote" "add" "origin" "https://github.com/example/q.git")
     (put repo ".gitignore" "compiled/\n")
     (put repo "runtime/local.rkt" "#lang racket/base\n(provide answer)\n(define answer 42)\n")
     (put repo
          "tests/test-existing.rkt"
          (string-append "#lang racket/base\n(require rackunit)\n(provide evidence-suite)\n"
                         "(define evidence-suite (test-suite \"existing\" (test-case \"arithmetic\" "
                         "(check-equal? (+ 20 22) 42))))\n"))
     (put
      repo
      "scripts/run-tests.rkt"
      (string-append
       "#lang racket/base\n(require rackunit/text-ui racket/file racket/list racket/string)\n"
       "(define args (vector->list (current-command-line-arguments)))\n"
       "(define paths (if (member args '((\"--suite\" \"fast\") (\"--suite\" \"fast\" \"--jobs\" \"2\")))\n"
       " (for/list ([p (directory-list \"tests\")] #:when (string-suffix? (path->string p) \".rkt\"))\n"
       "  (string-append \"tests/\" (path->string p))) args))\n"
       "(unless (pair? paths) (error 'runner \"no tests\"))\n"
       "(exit (for/sum ([path paths]) (run-tests (dynamic-require (path->complete-path path) 'evidence-suite))))\n"))
     (put repo
          "scripts/lint-all.rkt"
          (string-append
           "#lang racket/base\n(require racket/file racket/string)\n"
           "(for ([line (file->lines \"runtime/local.rkt\")])\n"
           " (when (regexp-match? #px\"[ \\t]+$\" line) (error 'lint \"trailing whitespace\")))\n"))
     ;; Canonicalize BEFORE committing; the later format gate must not dirty K.
     (parameterize ([current-directory repo])
       (unless (system* (find-executable-path "raco") "fmt" "-i" "runtime/local.rkt")
         (error 'fixture "formatter unavailable")))
     (git-quiet! repo
                 "add"
                 ".gitignore"
                 "runtime/local.rkt"
                 "tests/test-existing.rkt"
                 "scripts/run-tests.rkt"
                 "scripts/lint-all.rkt")
     (git-quiet! repo "commit" "-qm" "baseline")
     (make-directory* (build-path root ".planning" "waves"))
     (write-plan! root 1 "Proof" "proof")
     (write-wave-doc! root 1 "proof" '("q/runtime/local.rkt") c0)
     (make-plan-snapshot! root plan (file->string (build-path root ".planning" "PLAN.md")))
     (parameterize ([current-supplemental-authority-root private]
                    [current-owned-proof-root (build-path root "proofs")]
                    [current-gsd-verification-registry (make-verification-registry)]
                    [current-gsd-delivery-verify-timeout-sec 90]
                    [current-gsd-delivery-branch-context
                     (make-branch-delivery-context #:repo-root repo
                                                   #:branch "campaign/w1"
                                                   #:base-commit (git-text cwd "rev-parse" "HEAD")
                                                   #:worktree-path repo)])
       (define frozen (frozen-verify-binding root plan 1))
       (define before (committed-delivery-snapshot repo))
       (define-values (legacy-result old-capture)
         (call-with-owned-proof-capture root
                                        plan
                                        1
                                        before
                                        "old-attempt"
                                        7
                                        (lambda () (check-command root))
                                        #:cwd repo))
       (check-true (delivery-verification-approved? legacy-result)
                   (delivery-verification-message legacy-result))
       (unless old-capture
         (error 'fixture "real baseline capture failed"))
       (define old-ref (owned-proof-capture-reference old-capture))
       (define old
         (hash-set* before
                    'attempt-id
                    "old-attempt"
                    'attempt-fence
                    7
                    'verified-at
                    (current-seconds)
                    'evidence
                    "actual owned C0 fixture GREEN"
                    'owned-proof
                    old-ref))
       (record-delivery-receipt! root plan 1 old)
       (define journal-bytes (file->bytes (delivery-journal-path root plan 1)))
       (define context
         (hash-set* before
                    'repo
                    cwd
                    'plan-id
                    plan
                    'wave
                    1
                    'attempt-id
                    "old-attempt"
                    'attempt-fence
                    7
                    'receipt-generation
                    0
                    'receipt-sha256
                    (canonical-evidence-digest old)
                    'evidence-revision
                    0
                    'snapshot-sha256
                    (hash-ref frozen 'snapshot-sha256)
                    'green-proof-sha256
                    (hash-ref old-ref 'sha256)))
       (define task
         (seal-supplemental-record (hasheq 'kind
                                           "supplemental-evidence-task/v1"
                                           'context
                                           context
                                           'base-command
                                           c0
                                           'effective-command
                                           c1
                                           'test-path
                                           test-path
                                           'harness-path
                                           helper-path
                                           'owner-paths
                                           '()
                                           'baseline-head
                                           (hash-ref before 'head)
                                           'baseline-tree
                                           (hash-ref before 'tree)
                                           'control-plane-head
                                           (hash-ref before 'head)
                                           'bounds
                                           (hasheq 'model-dispatches
                                                   1
                                                   'verify-timeout-sec
                                                   120
                                                   'probe-timeout-sec
                                                   60
                                                   'deadline-sec
                                                   1800))))
       (define tref
         (propose-retain! "task-proposal" (hasheq 'task task 'expires-at (+ (current-seconds) 1200))))
       (define aref (approve tref))
       (start-supplemental-operation! aref (hasheq 'context context 'coordinator-fence 7))
       (define (step state facts)
         (define operation (load-supplemental-operation aref))
         (transition-supplemental-operation! aref
                                             (hash-ref operation 'revision)
                                             (hash-ref operation 'state)
                                             state
                                             facts))
       (define campaign
         (make-campaign-record
          plan
          campaign-manifest
          (list
           (make-campaign-wave 1 "proof" 'awaiting-delivery 1 (campaign-attempt "old-attempt" 7 0)))
          #f
          7
          'test
          0
          0))
       (set-campaign-fence-token! campaign 8)
       (begin-attempt! campaign 1 (campaign-fence-token campaign))
       (set-campaign-wave-status! (car (campaign-record-waves campaign)) 'verifying)
       (persist-campaign! root campaign)
       (step 'reserved
             (hasheq 'allocation
                     (hasheq 'attempt-id "attempt-2" 'attempt-fence 8 'coordinator-fence 8)))
       (step 'launched (hasheq 'dispatch-id "fixture-author-once"))
       (put repo
            helper-path
            (if passes?
                "#lang racket/base\n(provide witness)\n(define (witness) (= (+ 20 22) 42))\n"
                "#lang racket/base\n(provide witness)\n(define (witness) (= (+ 20 22) 43))\n"))
       (put repo
            test-path
            (string-append
             "#lang racket/base\n(require rackunit \"helpers/witness.rkt\")\n"
             "(provide evidence-suite)\n(define evidence-suite "
             "(test-suite \"witness\" (test-case \"new regression\" (check-true (witness)))))\n"))
       (git-quiet! repo "add" test-path helper-path)
       (git-quiet! repo "commit" "-qm" "new witness")
       (define source (committed-delivery-snapshot repo))
       (define manifest
         (for/list ([path (list test-path helper-path)])
           (hasheq 'path
                   path
                   'mode
                   "100644"
                   'blob
                   (git-text cwd "rev-parse" (string-append "HEAD:" path))
                   'sha256
                   (sha256-file (build-path repo path)))))
       (define candidate
         (seal-supplemental-record (hasheq 'kind
                                           "supplemental-evidence-candidate/v1"
                                           'context
                                           context
                                           'task-sha256
                                           (hash-ref task 'sha256)
                                           'task-proposal
                                           tref
                                           'task-approval
                                           aref
                                           'source
                                           source
                                           'attempt-id
                                           "attempt-2"
                                           'attempt-fence
                                           8
                                           'execution-cwd
                                           cwd
                                           'manifest
                                           manifest)))
       (define kref (propose-retain! "candidate-proposal" candidate (list tref aref)))
       (step 'candidate-sealed (hasheq 'candidate-ref kref))
       (define cap (approve kref))
       (step 'candidate-approved (hasheq 'candidate-approval-ref cap))
       (define contract
         (hasheq 'task-proposal
                 tref
                 'task-approval
                 aref
                 'candidate-proposal
                 kref
                 'candidate-approval
                 cap))
       (step 'verifying
             (hasheq 'verify-job
                     (hasheq 'execution-cwd
                             cwd
                             'operational-jobs
                             jobs
                             'command-sha256
                             (sha256-string
                              (string-join (list "/bin/sh" "-c" (apply-verify-jobs c1 jobs))
                                           "\u0")))))
       (parameterize ([current-gsd-delivery-verify-jobs jobs])
         (proc root repo frozen source old-ref journal-bytes contract step))))
   (lambda ()
     (clear-approval-channel!)
     (delete-directory/files root))))
(module+ test
  (test-case "private T/A/K/CAp authenticate a fresh complete C1 job; C0/history stay unchanged"
    (with-fixture
     (lambda (root repo frozen source old-ref journal-bytes contract step)
       (define (resolve [refs contract] [src source] [attempt "attempt-2"] [fence 8] [cwd repo])
         (resolve-supplemental-verify-binding refs
                                              frozen
                                              root
                                              plan
                                              1
                                              src
                                              attempt
                                              fence
                                              (path->string cwd)))
       (define effective (resolve))
       (check-equal? (hash-ref effective 'normalized-command) c0)
       (check-equal? (hash-ref effective 'effective-normalized-command) c1)
       ;; Caller-supplied descriptors are ignored; retained records are reloaded.
       (check-equal? (resolve (hash-set contract 'task-record (hasheq 'approved #t))) effective)
       (for ([bad (list (hash-remove contract 'task-approval)
                        (hash-set contract 'candidate-approval (hash-ref contract 'task-approval))
                        (hash-set contract
                                  'task-approval
                                  (hasheq 'kind "task-approval" 'sha256 (make-string 64 #\f))))])
         (check-exn exn:fail? (lambda () (resolve bad))))
       (check-exn exn:fail? (lambda () (resolve contract source "old-attempt" 7)))
       (check-exn exn:fail?
                  (lambda ()
                    (resolve-supplemental-verify-binding contract
                                                         (hash-set frozen 'normalized-command c1)
                                                         root
                                                         plan
                                                         1
                                                         source
                                                         "attempt-2"
                                                         8
                                                         (path->string repo))))
       (define actual (load-campaign-record root plan))
       (set-campaign-fence-token! actual 9)
       (persist-campaign! root actual)
       (check-exn exn:fail? (lambda () (resolve)))
       (set-campaign-fence-token! actual 8)
       (persist-campaign! root actual)
       (check-exn exn:fail?
                  (lambda () (resolve contract (hash-set source 'head (make-string 40 #\f)))))
       (check-exn exn:fail? (lambda () (resolve contract source "attempt-2" 8 root)))
       ;; A real verifier cannot use an explicit override, even if it exits 0.
       (check-exn exn:fail?
                  (lambda ()
                    (parameterize ([current-gsd-delivery-verify-command "true"])
                      (call-with-owned-proof-capture root
                                                     plan
                                                     1
                                                     source
                                                     "attempt-2"
                                                     8
                                                     (lambda () (check-command root))
                                                     #:cwd repo
                                                     #:supplemental-contract contract))))
       (define-values (result proposed capture)
         (capture-supplemental-delivery-verification root
                                                     plan
                                                     1
                                                     repo
                                                     "attempt-2"
                                                     8
                                                     contract
                                                     (lambda ()
                                                       (define result (check-command root))
                                                       (check-exn #rx"admission already consumed"
                                                                  (lambda () (check-command root)))
                                                       result)))
       (check-true (delivery-verification-approved? result))
       (define ref (hash-ref proposed 'owned-proof))
       (check-true (supplemental-owned-proof? ref))
       (check-false (valid-legacy-owned-proof-reference? ref))
       (check-false (consume-owned-proof-refresh! capture ref))
       (define m (load-receipt-owned-proof proposed root plan 1))
       (check-false (valid-legacy-owned-proof-manifest? m))
       (check-equal? (hash-ref m 'command) c1)
       (check-equal? (hash-ref m 'frozen-normalized-command) c0)
       (check-equal? (hash-ref m 'effective-normalized-command) c1)
       (check-equal? (hash-ref (hash-ref m 'job) 'state) "completed")
       (check-true (hash-ref (hash-ref m 'job) 'fresh))
       (check-true (hash-ref (hash-ref m 'job) 'reaped))
       (check-true (positive? (hash-ref (hash-ref m 'job) 'proc-start-ticks)))
       (check-false (receipt-owned-proof-blocker proposed root plan 1 #:command c1))
       (check-equal? (receipt-owned-proof-blocker proposed root plan 1 #:command c0)
                     'invalid-owned-proof)
       (check-equal? (frozen-verify-binding root plan 1) frozen)
       (check-equal? (file->bytes (delivery-journal-path root plan 1)) journal-bytes)
       (check-equal? (hash-ref (hash-ref (load-delivery-journal root plan 1) 'receipt) 'owned-proof)
                     old-ref)
       (check-true (valid-legacy-owned-proof-reference? old-ref))
       (define facts (supplemental-proof-offline-facts m ref frozen))
       (check-true (supplemental-proof-facts? facts))
       (check-equal? (supplemental-proof-facts-identity facts) contract)
       ;; Real retained bytes export and offline reload; portable facts still
       ;; cannot authorize a live capture or supersession.
       (define bundle (build-path root "bundle"))
       (export-owned-review-proof! (build-path (current-owned-proof-root) plan)
                                   ref
                                   (hash-ref ref 'binding)
                                   bundle
                                   #:command c1
                                   #:normalized-command c0)
       (check-equal? (load-owned-review-proof bundle
                                              ref
                                              (hash-ref ref 'binding)
                                              #:command c1
                                              #:normalized-command c0)
                     m)
       (for ([bad (list (hash-remove m 'supplemental-contract)
                        (hash-remove m 'effective-normalized-command)
                        (hash-set m 'effective-normalized-command c0)
                        (hash-set m 'frozen-normalized-command c1)
                        (hash-set m 'schema-version 1))])
         (check-exn exn:fail? (lambda () (supplemental-proof-offline-facts bad ref frozen))))
       ;; Advancing protected operation pins the exact retained GREEN reference.
       (step 'green-captured (hasheq 'green-proof-ref ref))
       (check-equal? (load-receipt-owned-proof proposed root plan 1) m)
       (check-exn exn:fail? (lambda () (resolve)))
       (put repo test-path "#lang racket/base\n(error 'dirty \"not authorized\")\n")
       (check-exn exn:fail? (lambda () (load-receipt-owned-proof proposed root plan 1))))))
  (test-case "approved operational jobs 2 survives the full owned C1 chain; revocation refuses readback"
    (with-fixture (lambda (root repo frozen source old-ref journal-bytes contract step)
                    (define-values (result receipt capture)
                      (capture-supplemental-delivery-verification root
                                                                  plan
                                                                  1
                                                                  repo
                                                                  "attempt-2"
                                                                  8
                                                                  contract
                                                                  (lambda () (check-command root))))
                    (check-true (delivery-verification-approved? result))
                    (define m (load-receipt-owned-proof receipt root plan 1))
                    (check-equal? (hash-ref m 'operational-jobs) 2)
                    (check-equal? (hash-ref m 'command) (apply-verify-jobs c1 2))
                    (check-equal? (hash-ref m 'frozen-normalized-command) c0)
                    (define revocation
                      (propose-retain! "revocation-proposal"
                                       (hasheq 'reason "fixture operator revoked candidate")
                                       (list (hash-ref contract 'candidate-approval))))
                    (revoke! revocation (interactive-grant revocation))
                    (check-exn exn:fail? (lambda () (load-receipt-owned-proof receipt root plan 1)))
                    (check-equal? (file->bytes (delivery-journal-path root plan 1)) journal-bytes))
                  #:jobs 2))
  (test-case "a genuine failed C1 job retains diagnostics but cannot mint supplemental GREEN"
    (with-fixture
     (lambda (root repo frozen source old-ref journal-bytes contract step)
       (check-exn #rx"stale/failed/override/drifted"
                  (lambda ()
                    (capture-supplemental-delivery-verification root
                                                                plan
                                                                1
                                                                repo
                                                                "attempt-2"
                                                                8
                                                                contract
                                                                (lambda () (check-command root)))))
       (define manifests
         (for/list ([path (directory-list (build-path (current-owned-proof-root) plan) #:build? #t)]
                    #:when (string-suffix? (path->string path) ".json"))
           (call-with-input-file path read-json)))
       (check-true (ormap (lambda (m)
                            (equal? (hash-ref m 'kind) "declared-verify-terminal-diagnostic"))
                          manifests))
       (check-false (ormap supplemental-owned-proof? manifests))
       (check-equal? (file->bytes (delivery-journal-path root plan 1)) journal-bytes))
     #:passes? #f)))
