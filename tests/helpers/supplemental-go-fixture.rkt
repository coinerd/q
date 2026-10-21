#lang racket/base
;; tests/helpers/supplemental-go-fixture.rkt — BUG-0088 C1 end-to-end fixture.
;;
;; A complete temp-home campaign: real git history (pre-fix baseline -> fixed
;; candidate), frozen C0 five-stage Verify, an authenticated old receipt, a
;; parked awaiting-delivery wave, a private 0700 authority root, and the REAL
;; interactive broker channel driven by a simulated trusted frontend. No live
;; campaign, no fabricated grants: approvals always pass through the real
;; register/decide/await broker flow and are consumed exactly once.
(require compiler/cm
         racket/file
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         "delivery-fixtures.rkt"
         "honest-delivery-fixture.rkt"
         "private-fixture-templates.rkt"
         "../../runtime/approval/broker.rkt"
         "../../extensions/gsd/campaign-state.rkt"
         "../../extensions/gsd/campaign-repository.rkt"
         "../../extensions/gsd/delivery-receipt.rkt"
         "../../extensions/gsd/delivery-journal.rkt"
         "../../extensions/gsd/delivery-verifier.rkt"
         "../../extensions/gsd/review-proof.rkt"
         "../../extensions/gsd/evidence-repair-format.rkt"
         "../../extensions/gsd/evidence-readiness.rkt"
         "../../extensions/gsd/evidence-author-journal.rkt"
         "../../extensions/gsd/author-scope.rkt"
         "../../extensions/gsd/effective-verify-binding.rkt"
         "../../extensions/gsd/supplemental-evidence-format.rkt"
         "../../extensions/gsd/supplemental-evidence-authority.rkt"
         (submod "../../extensions/gsd/supplemental-evidence-authority.rkt" owner-frontend)
         "../../extensions/gsd/supplemental-evidence-journal.rkt"
         "../../extensions/gsd/supplemental-evidence-controller.rkt"
         "../../extensions/gsd/plan-snapshot.rkt"
         "../../extensions/gsd/composition-root.rkt"
         "../../extensions/gsd/verification-job.rkt")
(provide (struct-out supplemental-fixture)
         call-with-supplemental-fixture
         fixture-interactive-grant
         fixture-approve!
         fixture-run-c1-verifier
         fixture-task-paths
         fixture-author-commit!
         fixture-finalize-predecessor!)

(struct supplemental-fixture
        (root repo
              private
              plan
              c0
              c1
              test-path
              harness-path
              owner-paths
              baseline-head
              baseline-tree
              task-context)
  #:transparent)

;; Anchor on the installed checkout so the fixture copies the REAL runner
;; closure regardless of the test process's working directory.
(define-runtime-path checkout-root "../..")

(define fixture-test-path "tests/test-witness.rkt")
(define fixture-harness-path "tests/helpers/witness.rkt")
(define fixture-owner-paths
  '("README.md" "reports/test-ownership-map.md"
                "reports/test-ownership-map.json"
                "artifacts/tier-ownership/v0.00.00-fixture/ownership-matrix.json"
                "artifacts/tier-ownership/v0.00.00-fixture/SHA256SUMS"))

(define (put repo relative text)
  (define p (build-path repo relative))
  (make-parent-directory* p)
  (display-to-file text p #:exists 'truncate))

;; The simulated trusted frontend: registers the exact proposal digest on the
;; REAL active broker channel with the retained record, decides once, awaits.
(define (fixture-interactive-grant ref [approved? #t])
  (define digest (hash-ref ref 'sha256))
  (define id
    (register-approval-request-for-channel!
     (current-approval-channel)
     digest
     (hasheq 'proposal ref 'record (load-supplemental-record ref))))
  (unless (and id (approval-decide! id digest approved?))
    (error 'fixture "broker decision failed"))
  (define-values (outcome grant) (approval-await-grant id digest 5000))
  (unless (eq? outcome 'approved)
    (error 'fixture "broker grant unavailable"))
  grant)
(define (fixture-approve! ref)
  (approve-supplemental-proposal! ref (fixture-interactive-grant ref)))

;; Real W0 receipt/handoff reconciliation, not a fabricated predecessor boolean.
(define (fixture-finalize-predecessor! fx)
  (define root (supplemental-fixture-root fx))
  (define plan (supplemental-fixture-plan fx))
  (define old (load-campaign-record root plan))
  (define (with-waves waves)
    (define rec
      (make-campaign-record plan
                            (campaign-record-manifest old)
                            waves
                            #f
                            (campaign-fence-token old)
                            'test
                            0
                            0))
    (set-campaign-record-plan-snapshot-path! rec (campaign-record-plan-snapshot-path old))
    (set-campaign-record-plan-snapshot-digest! rec (campaign-record-plan-snapshot-digest old))
    rec)
  (define predecessor
    (with-waves (list (make-campaign-wave 0
                                          "Predecessor"
                                          'awaiting-delivery
                                          1
                                          (campaign-attempt "predecessor" 7 0)))))
  (persist-campaign! root predecessor)
  (finalize-awaiting! root predecessor)
  (define ready (load-campaign-record root plan))
  (unless (eq? (campaign-wave-status (car (campaign-record-waves ready))) 'done)
    (error 'fixture "predecessor did not genuinely finalize"))
  (persist-campaign! root
                     (with-waves (append (campaign-record-waves ready) (campaign-record-waves old)))))
(define (fixture-task-paths fx)
  (list (supplemental-fixture-test-path fx) (supplemental-fixture-harness-path fx)))

;; The real frozen Verify entry the controller uses for G1 (no override).
(define (fixture-run-c1-verifier base wave)
  (check-verify-command base wave #f))

;; Simulated model author: writes ONLY the two approved files (the ordinary
;; hook guard would refuse anything else under current-gsd-author-phase).
(define (fixture-author-commit! repo)
  (put
   repo
   fixture-harness-path
   "#lang racket/base\n(require \"../../runtime/local.rkt\")\n(provide witness)\n(define (witness) (= answer 42))\n")
  (put repo
       fixture-test-path
       (string-append
        "#lang racket\n"
        ";; @speed fast\n;; @suite extensions\n;; @boundary unit\n;; @timeout 120\n"
        "(require rackunit \"helpers/witness.rkt\")\n(provide evidence-suite)\n"
        "(define evidence-suite (test-suite \"witness\" (test-case \"new regression\" "
        "(check-true (witness)))))\n"
        "(module+ test (require rackunit/text-ui) (exit (run-tests evidence-suite)))\n")))
(define (call-with-supplemental-fixture
         proc
         #:completed-author? [completed-author? #f]
         #:wave-index [wave 1]
         #:expires-in-sec [expires-in-sec 3600]
         #:proposal-control-plane-head [proposal-head #f]
         #:physical-worktree? [physical-worktree? #f]
         #:frozen-goal
         [frozen-goal
          "The exported answer from runtime/local.rkt is 42 on candidate; the pinned baseline answer is 7."])
  (define root (make-temporary-file "supplemental-go-~a" 'directory))
  (define repo (build-path root "q"))
  (define cwd (path->string repo))
  (define private (build-path root "private"))
  (when (current-approval-channel)
    (error 'fixture "active frontend"))
  (dynamic-wind
   (lambda ()
     (make-directory repo)
     (make-directory private #o700)
     (set-approval-channel! (make-approval-channel #:timeout-ms 5000)))
   (lambda ()
     (git-quiet! repo "init" "-q")
     (hermetic-identity! repo)
     (git-quiet! repo "checkout" "-qb" "campaign/w1")
     (git-quiet! repo "remote" "add" "origin" "https://github.com/example/q.git")
     (put repo ".gitignore" "compiled/\n")
     ;; Real installed runner closure: exact bytes so the focused-stage
     ;; dependency-closure authentication of the C1 probe is genuine.
     (for ([relative (installed-runner-closure)])
       (put repo relative (file->string (build-path checkout-root relative))))
     ;; Warm only the real runner's compilation cache. Four focused/full
     ;; subprocesses in the two authentic proof ladders otherwise compile the
     ;; same closure repeatedly. compiled/ is ignored; source pins are unchanged.
     (managed-compile-zo (build-path repo "scripts" "run-tests.rkt"))
     ;; Pre-fix baseline production fact, then the fix + existing witness.
     (put repo "runtime/local.rkt" "#lang racket/base\n(provide answer)\n(define answer 7)\n")
     (git-quiet! repo "add" "-A")
     (git-quiet! repo "commit" "-qm" "pre-fix baseline")
     (define baseline-head (git-text repo "rev-parse" "HEAD"))
     (define baseline-tree (git-text repo "rev-parse" "HEAD^{tree}"))
     (put repo "runtime/local.rkt" "#lang racket/base\n(provide answer)\n(define answer 42)\n")
     (put repo
          "tests/test-existing.rkt"
          (string-append
           "#lang racket/base\n"
           ";; @speed fast\n;; @suite extensions\n"
           "(module+ test (require rackunit \"../runtime/local.rkt\") (check-equal? answer 42))\n"))
     (put repo "README.md" "generated ownership placeholder\n")
     (put repo "reports/test-ownership-map.md" "generated\n")
     (put repo "reports/test-ownership-map.json" "{}\n")
     (put repo "artifacts/tier-ownership/v0.00.00-fixture/ownership-matrix.json" "{}\n")
     (put repo "artifacts/tier-ownership/v0.00.00-fixture/SHA256SUMS" "generated\n")
     (git-quiet! repo "add" "-A")
     (git-quiet! repo "commit" "-qm" "fix and existing evidence")
     ;; Canonicalize BEFORE the snapshot/journal so fmt never dirties K.
     (parameterize ([current-directory repo])
       (unless (system* (find-executable-path "raco") "fmt" "-i" "runtime/local.rkt")
         (error 'fixture "formatter unavailable")))
     (git-quiet! repo "add" "-A")
     (git-quiet! repo "commit" "-qm" "canonical formatting" "--allow-empty")
     (make-directory* (build-path root ".planning" "waves"))
     (define c0
       (string-join
        '("raco fmt -i runtime/local.rkt" "raco make runtime/local.rkt"
                                          "racket scripts/run-tests.rkt tests/test-existing.rkt"
                                          "racket scripts/run-tests.rkt --suite fast"
                                          "racket scripts/lint-all.rkt")
        " && "))
     (define c1 (make-effective-verify-command c0 fixture-test-path))
     (put repo
          "scripts/lint-all.rkt"
          (string-append "#lang racket/base\n(require racket/file)\n"
                         "(for ([line (file->lines \"runtime/local.rkt\")])\n"
                         " (when (regexp-match? #px\"[ \\t]+$\" line)\n"
                         "   (error 'lint \"trailing whitespace\")))\n"))
     (git-quiet! repo "add" "-A")
     (git-quiet! repo "commit" "-qm" "lint helper")
     (write-plan! root wave "Supplemental" "supplemental")
     (write-wave-doc! root wave "supplemental" '("q/runtime/local.rkt") c0)
     ;; Behavioral intent is authored BEFORE freezing/approval, never patched
     ;; into an already approved snapshot.
     (display-to-file
      (string-append
       "\n## Goal\n"
       frozen-goal
       "\n\n## Acceptance Criteria\n- The helper reads the exported production answer.\n"
       "- The witness expects answer 42, not baseline 7.\n")
      (build-path root ".planning" "waves" (format "W~a-supplemental.md" wave))
      #:exists 'append)
     (define plan (campaign-manifest-hash (make-campaign-manifest 1 "fixture" '() '() "constraints")))
     (make-plan-snapshot! root plan (file->string (build-path root ".planning" "PLAN.md")))
     ;; A real linked checkout preserves the logical repo in receipt identity,
     ;; while owned capture authenticates the distinct execution directory.
     (define logical-repo repo)
     (when physical-worktree?
       (git-quiet! repo "checkout" "--detach")
       (define physical (build-path root "wt-campaign"))
       (git-quiet! repo "worktree" "add" (path->string physical) "campaign/w1")
       (set! repo physical))
     (parameterize ([current-supplemental-authority-root private]
                    [current-owned-proof-root (build-path root "owned-proofs")]
                    [current-gsd-verification-registry (make-verification-registry)]
                    [current-gsd-delivery-branch-context
                     (make-branch-delivery-context #:repo-root logical-repo
                                                   #:branch "campaign/w1"
                                                   #:base-commit (git-text repo "rev-parse" "HEAD")
                                                   #:worktree-path repo)])
       ;; Authentic old receipt from a real owned C0 execution.
       (define before (committed-delivery-snapshot repo))
       ;; The owned capture below runs and authenticates the full real C0
       ;; ladder. Do not run a redundant uncaptured preflight ladder too.
       (define-values (_legacy old-capture)
         (call-with-owned-proof-capture root
                                        plan
                                        wave
                                        before
                                        "old-attempt"
                                        7
                                        (lambda () (fixture-run-c1-verifier root wave))
                                        #:cwd repo))
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
       (record-delivery-receipt! root plan wave old)
       ;; Parked awaiting-delivery wave with the old attempt (the W1 shape).
       (define campaign
         (make-campaign-record plan
                               (make-campaign-manifest 1 "fixture" '() '() "constraints")
                               (list (make-campaign-wave wave
                                                         "Supplemental"
                                                         'awaiting-delivery
                                                         1
                                                         (campaign-attempt "old-attempt" 7 0)))
                               #f
                               7
                               'test
                               0
                               0))
       ;; The campaign must carry the frozen snapshot binding: publication
       ;; boundaries re-verify the digest at every authorization phase.
       (set-campaign-record-plan-snapshot-path! campaign (path->string (snapshot-dir root plan)))
       (set-campaign-record-plan-snapshot-digest!
        campaign
        (snapshot-manifest-digest (load-snapshot-manifest root plan)))
       (persist-campaign! root campaign)
       ;; Optional actual historical author supersession, before approving T.
       (when completed-author?
         (reserve-evidence-author-repair! root
                                          plan
                                          wave
                                          (current-evidence-context root plan wave)
                                          (frozen-author-files root plan wave)
                                          #:guard (lambda () #t))
         (start-evidence-author-repair! root plan wave "attempt-2" 7 #:guard (lambda () #t))
         ;; Keep the already-canonical layout; the real successor Verify
         ;; still runs fmt and authenticates the committed result below.
         (put repo
              "runtime/local.rkt"
              (string-replace (file->string (build-path repo "runtime/local.rkt")) "42" "(+ 40 2)"))
         (git-quiet! repo "add" "runtime/local.rkt")
         (git-quiet! repo "commit" "-qm" "historical author successor")
         (define source (committed-delivery-snapshot repo))
         (define-values (_result capture)
           (call-with-owned-proof-capture root
                                          plan
                                          wave
                                          source
                                          "attempt-2"
                                          7
                                          (lambda () (fixture-run-c1-verifier root wave))
                                          #:cwd repo))
         (unless capture
           (error 'fixture "historical successor capture failed"))
         (reconcile-evidence-author-receipt!
          root
          plan
          wave
          (hash-set* source
                     'attempt-id
                     "attempt-2"
                     'attempt-fence
                     7
                     'verified-at
                     (current-seconds)
                     'evidence
                     "actual historical author GREEN"
                     'owned-proof
                     (owned-proof-capture-reference capture))
          #:guard (lambda () #t)
          #:head-ancestor? (lambda (a b) (equal? (git-text repo "merge-base" a b) a)))
         (set-campaign-wave-current-attempt! (car (campaign-record-waves campaign))
                                             (campaign-attempt "attempt-2" 7 0))
         (persist-campaign! root campaign))
       ;; Task proposal from authentic local facts + real operator approval.
       (define-values (tref task)
         (build-supplemental-task-proposal! root
                                            plan
                                            wave
                                            #:test-path fixture-test-path
                                            #:harness-path fixture-harness-path
                                            #:owner-paths fixture-owner-paths
                                            #:baseline-head baseline-head
                                            #:baseline-tree baseline-tree
                                            #:expires-in-sec expires-in-sec))
       ;; Model an already sealed proposal from a prior engineering install.
       ;; Change provenance BEFORE broker approval, never a retained record.
       (define approved-tref
         (if proposal-head
             (propose-retain! "task-proposal"
                              (hash-set (hash-ref (load-supplemental-record tref) 'payload)
                                        'task
                                        (seal-supplemental-record
                                         (hash-set task 'control-plane-head proposal-head))))
             tref))
       (define aref (fixture-approve! approved-tref))
       (unless (and (write-supplemental-pointer! root plan wave approved-tref aref))
         (error 'fixture "pointer write failed"))
       (proc (supplemental-fixture root
                                   repo
                                   private
                                   plan
                                   c0
                                   c1
                                   fixture-test-path
                                   fixture-harness-path
                                   fixture-owner-paths
                                   baseline-head
                                   baseline-tree
                                   (hash-ref task 'context)))))
   (lambda ()
     (clear-approval-channel!)
     (delete-directory/files root))))
(define (installed-runner-closure)
  ;; Same walk the runner-coverage authenticator performs over the installed
  ;; checkout; the fixture copies these bytes verbatim.
  (define repo-root (simplify-path checkout-root #f))
  (define seen (make-hash))
  (let visit ([path "scripts/run-tests.rkt"])
    (define full (simplify-path (path->complete-path path repo-root) #f))
    (define relative (path->string (find-relative-path repo-root full)))
    (when (and (not (string-prefix? relative "..")) (file-exists? full))
      (unless (hash-ref seen relative #f)
        (hash-set! seen relative #t)
        (define stx
          (parameterize ([read-accept-reader #t])
            (call-with-input-file full (lambda (in) (read-syntax full in)))))
        (define (dependency s)
          (cond
            [(string? s)
             (when (string-suffix? s ".rkt")
               (visit (path->string (build-path (path-only full) s))))]
            [(pair? s) (for-each dependency s)]))
        (define (walk s)
          (when (and (pair? s) (list? s))
            (when (eq? (car s) 'require)
              (for-each dependency (cdr s)))
            (when (and (eq? (car s) 'define-runtime-path)
                       (= 3 (length s))
                       (string? (caddr s))
                       (string-suffix? (caddr s) ".rkt"))
              (dependency (caddr s)))
            (for-each walk s)))
        (walk (syntax->datum stx)))))
  (sort (hash-keys seen) string<?))
(define (git-text repo . args)
  (define out (open-output-string))
  (parameterize ([current-directory repo]
                 [current-output-port out]
                 [current-error-port (open-output-nowhere)])
    (unless (zero? (apply system*/exit-code (find-executable-path "git") args))
      (error 'fixture "git failed: ~s" args)))
  (string-trim (get-output-string out)))
