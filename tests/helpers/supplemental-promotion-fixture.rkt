#lang racket/base
;; tests/helpers/supplemental-promotion-fixture.rkt — self-contained real
;; fixture for the BUG-0088 protected promotion ladder (plan Phase 0 step 3).
;;
;; Builds the SAME genuine ladder the publication suite exercises (real
;; private authority store, real interactive broker grants, real owned C0/G1
;; captures, real baseline-overlay RED/identical-candidate GREEN probe, real
;; S-authorized supersession with the injected remote publisher seam) and
;; drives it THROUGH certification: after this helper returns, the protected
;; supplemental operation is at "receipt-certified" over the superseded
;; receipt r2, which is exactly the entry state of
;; extensions/gsd/supplemental-evidence-promotion.rkt.
;;
;; No fake grants and no fabricated protected state: every authorization is a
;; genuine broker-approved private record and every effect goes through the
;; production modules. The remote push/readback effect is the existing
;; injected publisher seam (same as the publication suite).
;;
;; STABILITY: internal test-support module (tests/helpers/* is excluded from
;; test discovery, like private-fixture-templates.rkt).
(require racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         racket/system
         "private-fixture-templates.rkt"
         (only-in "delivery-fixtures.rkt" write-plan! write-wave-doc!)
         "../../runtime/approval/broker.rkt"
         "../../extensions/gsd/campaign-state.rkt"
         "../../extensions/gsd/campaign-repository.rkt"
         "../../extensions/gsd/composition-root.rkt"
         "../../extensions/gsd/delivery-verifier.rkt"
         "../../extensions/gsd/delivery-receipt.rkt"
         "../../extensions/gsd/delivery-journal.rkt"
         "../../extensions/gsd/delivery-publication.rkt"
         "../../extensions/gsd/effective-verify-binding.rkt"
         "../../extensions/gsd/evidence-probe.rkt"
         (only-in "../../extensions/gsd/evidence-repair-format.rkt" canonical-evidence-digest)
         "../../extensions/gsd/plan-snapshot.rkt"
         "../../extensions/gsd/review-proof.rkt"
         "../../extensions/gsd/supplemental-evidence-authority.rkt"
         (submod "../../extensions/gsd/supplemental-evidence-authority.rkt" owner-frontend)
         "../../extensions/gsd/supplemental-evidence-format.rkt"
         "../../extensions/gsd/supplemental-evidence-journal.rkt"
         "../../extensions/gsd/supplemental-evidence-proof.rkt"
         "../../extensions/gsd/supplemental-evidence-publication.rkt"
         "../../extensions/gsd/verification-job.rkt"
         (only-in "../../util/json/checksum.rkt" sha256-file sha256-string))

(define-runtime-path installed-root "../..")

(provide with-certified-promotion-fixture
         fixture-plan
         fixture-test-path
         fixture-helper-path
         fixture-effective-command
         fixture-interactive-grant
         fixture-approve!
         fixture-revoke-approval!)

;; Fixed single-wave campaign identity for every fixture instance.
(define campaign-manifest (make-campaign-manifest 1 "fixture" '() '() "constraints"))
(define fixture-plan (campaign-manifest-hash campaign-manifest))
(define fixture-test-path "tests/test-witness.rkt")
(define fixture-helper-path "tests/helpers/witness.rkt")
(define c0
  (string-join
   '("raco fmt -i runtime/local.rkt" "raco make runtime/local.rkt"
                                     "racket scripts/run-tests.rkt tests/test-existing.rkt"
                                     "racket scripts/run-tests.rkt --suite fast"
                                     "racket scripts/lint-all.rkt")
   " && "))
(define fixture-effective-command (make-effective-verify-command c0 fixture-test-path))

(define (put root relative text)
  (define p (build-path root relative))
  (make-directory* (path-only p))
  (display-to-file text p #:exists 'truncate))

(define (git-text cwd . args)
  (define out (open-output-string))
  (parameterize ([current-output-port out])
    (unless (zero? (apply system*/exit-code (find-executable-path "git") "-C" cwd args))
      (error 'fixture "git failed: ~a" args)))
  (string-trim (get-output-string out)))

;; A genuine interactive broker grant for the exact proposal digest; the
;; same channel mechanic the trusted frontend uses. No boolean/auto path.
(define (fixture-interactive-grant ref)
  (define digest (hash-ref ref 'sha256))
  (define id
    (register-approval-request-for-channel! (current-approval-channel) digest (hasheq 'proposal ref)))
  (unless (and id (approval-decide! id digest #t))
    (error 'fixture "broker decision failed"))
  (define-values (outcome grant) (approval-await-grant id digest 5000))
  (unless (eq? outcome 'approved)
    (error 'fixture "broker grant unavailable"))
  grant)

(define (fixture-approve! ref)
  (approve-supplemental-proposal! ref (fixture-interactive-grant ref)))

(define (fixture-revoke-approval! approval-ref)
  (define revocation
    (propose-retain! "revocation-proposal"
                     (hasheq 'reason "fixture operator revoked the supersession approval")
                     (list approval-ref)))
  (revoke! revocation (fixture-interactive-grant revocation)))

(define (check-command root)
  (define check (check-verify-command root 1 #f))
  (delivery-verification (cadr check) (list check) (format "~a" check)))

(define (stub-publisher)
  (lambda (_base _plan _wave snapshot _attempt-id _fence _coordinator-fence _evidence)
    (hasheq 'status "published" 'branch (hash-ref snapshot 'branch) 'head (hash-ref snapshot 'head))))

;; Copy the installed run-tests closure byte-for-byte so the fixture's
;; focused stage satisfies the installed-runner dependency-closure check.
(define (copy-runner-closure repo)
  (define seen (make-hash))
  (let visit ([p "scripts/run-tests.rkt"])
    (define full (simplify-path (path->complete-path p installed-root)))
    (define rel (path->string (find-relative-path (simplify-path installed-root) full)))
    (unless (hash-ref seen rel #f)
      (hash-set! seen rel #t)
      (define stx
        (parameterize ([read-accept-reader #t])
          (call-with-input-file full (lambda (in) (read-syntax full in)))))
      (let dep ([s (syntax->datum stx)])
        (when (and (pair? s) (list? s))
          (for ([item (in-list s)])
            (cond
              [(and (string? item)
                    (string-suffix? item ".rkt")
                    (relative-path? item)
                    (file-exists? (build-path (path-only full) item)))
               (visit (path->string (build-path (path-only full) item)))]
              [(list? item) (dep item)]))))))
  (for ([rel (in-hash-keys seen)])
    (define dst (build-path repo rel))
    (make-directory* (path-only dst))
    (copy-file (build-path installed-root rel) dst #t)))

(define (context-for receipt frozen owned-sha generation attempt-id fence)
  (hash-set* (hasheq 'repo
                     (hash-ref receipt 'repo)
                     'branch
                     (hash-ref receipt 'branch)
                     'origin
                     (hash-ref receipt 'origin)
                     'head
                     (hash-ref receipt 'head)
                     'tree
                     (hash-ref receipt 'tree)
                     'attempt-id
                     attempt-id
                     'attempt-fence
                     fence
                     'plan-id
                     fixture-plan
                     'wave
                     1
                     'receipt-generation
                     generation
                     'receipt-sha256
                     (canonical-evidence-digest receipt)
                     'evidence-revision
                     0
                     'snapshot-sha256
                     (hash-ref frozen 'snapshot-sha256)
                     'green-proof-sha256
                     owned-sha)
             'repo
             (hash-ref receipt 'repo)))

;; Drive the whole governed ladder to the certified promotion entry state,
;; then hand the immutable facts to proc. Everything is rebuilt per instance
;; in a private temp root and deleted on exit (normal or exceptional).
(define (with-certified-promotion-fixture proc)
  (define root (make-temporary-file "supplemental-promo-~a" 'directory))
  (define repo (build-path root "q"))
  (define cwd (path->string repo))
  (define private (build-path root "private"))
  (with-handlers ([void
                   (lambda (e)
                     (eprintf "fixture error raw: ~s\n" e)
                     (when (exn? e)
                       (for ([k (in-list (continuation-mark-set->context (exn-continuation-marks e)))]
                             [i (in-naturals)]
                             #:when (< i 12))
                         (eprintf "  at: ~s\n" k)))
                     (with-handlers ([exn:fail? void])
                       (delete-directory/files root))
                     (raise e))])
    (dynamic-wind
     (lambda ()
       (when (current-approval-channel)
         (error 'fixture "active frontend"))
       (set-approval-channel! (make-approval-channel #:timeout-ms 30000)))
     (lambda ()
       (make-directory repo)
       (make-directory private #o700)
       (git-text cwd "init" "-q")
       (hermetic-identity! repo)
       (git-text cwd "checkout" "-qb" "campaign/w1")
       (git-text cwd "remote" "add" "origin" "https://github.com/example/q.git")
       (put repo ".gitignore" "compiled/\n")
       (put repo "runtime/local.rkt" "#lang racket/base\n(provide answer)\n(define (answer) 41)\n")
       (put repo
            "tests/test-existing.rkt"
            (string-append
             ";; @speed fast\n#lang racket/base\n(require rackunit)\n(provide evidence-suite)\n"
             "(define evidence-suite (test-suite \"existing\" (test-case \"arithmetic\" "
             "(check-equal? (+ 20 22) 42))))\n"
             "(module+ test (require rackunit/text-ui) (exit (run-tests evidence-suite)))\n"))
       (put repo
            "scripts/lint-all.rkt"
            (string-append
             "#lang racket/base\n(require racket/file racket/string)\n"
             "(for ([line (file->lines \"runtime/local.rkt\")])\n"
             " (when (regexp-match? #px\"[ \\t]+$\" line) (error 'lint \"trailing whitespace\")))\n"))
       (copy-runner-closure repo)
       (parameterize ([current-directory repo])
         (unless (system* (find-executable-path "raco") "fmt" "-i" "runtime/local.rkt")
           (error 'fixture "formatter unavailable")))
       (git-text cwd
                 "add"
                 ".gitignore"
                 "runtime/local.rkt"
                 "tests/test-existing.rkt"
                 "scripts/lint-all.rkt"
                 "scripts/run-tests.rkt"
                 "scripts/run-tests"
                 "util")
       (git-text cwd "commit" "-qm" "baseline")
       (define baseline-head (git-text cwd "rev-parse" "HEAD"))
       (define baseline-tree (git-text cwd "rev-parse" "HEAD^{tree}"))
       (put repo "runtime/local.rkt" "#lang racket/base\n(provide answer)\n(define (answer) 42)\n")
       (parameterize ([current-directory repo])
         (unless (system* (find-executable-path "raco") "fmt" "-i" "runtime/local.rkt")
           (error 'fixture "formatter unavailable")))
       (git-text cwd "add" "runtime/local.rkt")
       (git-text cwd "commit" "-qm" "implement answer")
       (make-directory* (build-path root ".planning" "waves"))
       (write-plan! root 1 "Proof" "proof")
       (write-wave-doc! root 1 "proof" '("q/runtime/local.rkt") c0)
       (make-plan-snapshot! root fixture-plan (file->string (build-path root ".planning" "PLAN.md")))
       (parameterize ([current-supplemental-authority-root private]
                      [current-owned-proof-root (build-path root "proofs")]
                      [current-gsd-verification-registry (make-verification-registry)]
                      [current-gsd-delivery-verify-timeout-sec 120]
                      [current-gsd-delivery-branch-context
                       (make-branch-delivery-context #:repo-root repo
                                                     #:branch "campaign/w1"
                                                     #:base-commit baseline-head
                                                     #:worktree-path repo)])
         (define frozen (frozen-verify-binding root fixture-plan 1))
         (define rec
           (make-campaign-record
            fixture-plan
            campaign-manifest
            (list
             (make-campaign-wave 1 "proof" 'awaiting-delivery 1 (campaign-attempt "attempt-1" 7 0)))
            #f
            7
            'test
            0
            0))
         (set-campaign-record-plan-snapshot-path! rec (path->string (snapshot-dir root fixture-plan)))
         (set-campaign-record-plan-snapshot-digest! rec (hash-ref frozen 'snapshot-sha256))
         (persist-campaign! root rec)
         (define (read-current)
           (load-campaign-record root fixture-plan))
         (define before (committed-delivery-snapshot repo))
         ;; G0: real owned C0 GREEN at attempt-1/7.
         (define-values (g0-result g0-capture)
           (call-with-owned-proof-capture root
                                          fixture-plan
                                          1
                                          before
                                          "attempt-1"
                                          7
                                          (lambda () (check-command root))
                                          #:cwd repo))
         (unless (and (delivery-verification-approved? g0-result) g0-capture)
           (error 'fixture "baseline C0 capture failed"))
         (define ref1 (owned-proof-capture-reference g0-capture))
         (define r0
           (hash-set* before
                      'attempt-id
                      "attempt-1"
                      'attempt-fence
                      7
                      'verified-at
                      (current-seconds)
                      'evidence
                      "actual owned C0 fixture GREEN at attempt-1"
                      'owned-proof
                      ref1))
         (record-delivery-receipt! root fixture-plan 1 r0)
         (parameterize ([current-gsd-approved-head-publisher (stub-publisher)])
           (publish-approved-head! root
                                   fixture-plan
                                   1
                                   (for/hasheq ([k '(repo origin branch head tree)])
                                     (values k (hash-ref r0 k)))
                                   "attempt-1"
                                   7
                                   #f
                                   (hash-ref r0 'evidence)
                                   read-current))
         ;; Author-allowance supersession: r1 at attempt-2/8 (real capture) and
         ;; a durably completed (spent) evidence-author marker.
         (put
          repo
          "runtime/local.rkt"
          "#lang racket/base\n(provide answer)\n(define (answer) 42)\n;; author successor receipt commit\n")
         (parameterize ([current-directory repo])
           (unless (system* (find-executable-path "raco") "fmt" "-i" "runtime/local.rkt")
             (error 'fixture "formatter unavailable")))
         (git-text cwd "add" "runtime/local.rkt")
         (git-text cwd "commit" "-qm" "author successor receipt")
         (set-campaign-fence-token! rec 8)
         (begin-attempt! rec 1 8)
         (persist-campaign! root rec)
         (define before-2 (committed-delivery-snapshot repo))
         (define-values (g0b-result g0b-capture)
           (call-with-owned-proof-capture root
                                          fixture-plan
                                          1
                                          before-2
                                          "attempt-2"
                                          8
                                          (lambda () (check-command root))
                                          #:cwd repo))
         (unless (and (delivery-verification-approved? g0b-result) g0b-capture)
           (error 'fixture "second C0 capture failed"))
         (define ref2 (owned-proof-capture-reference g0b-capture))
         (define r1
           (hash-set* before-2
                      'attempt-id
                      "attempt-2"
                      'attempt-fence
                      8
                      'verified-at
                      (+ 5 (hash-ref r0 'verified-at))
                      'evidence
                      "actual owned C0 fixture GREEN at attempt-2"
                      'owned-proof
                      ref2))
         (define author-context (context-for r0 frozen (hash-ref ref1 'sha256) 0 "attempt-1" 7))
         (define marker
           (hasheq 'schema-version
                   1
                   'state
                   "running"
                   'context
                   author-context
                   'frozen-files
                   '("runtime/local.rkt")
                   'old-receipt
                   r0
                   'successor-attempt
                   "attempt-2"
                   'successor-fence
                   8
                   'successor-receipt-sha256
                   (canonical-evidence-digest r1)))
         (define journal-data
           (hasheq 'schema-version
                   1
                   'plan-id
                   fixture-plan
                   'wave
                   1
                   'stage
                   "context-ready"
                   'receipt
                   r1
                   'receipt-history
                   (list r0)
                   'evidence-author-repair
                   marker
                   'model-calls
                   0
                   'tokens
                   0
                   'cost
                   0
                   'usage-missing
                   #f))
         (define (base-journal receipt history)
           (hash-set* journal-data 'receipt receipt 'receipt-history history))
         (save! root fixture-plan 1 (base-journal r0 '()))
         (parameterize ([current-gsd-approved-head-publisher (stub-publisher)])
           (publish-approved-head! root
                                   fixture-plan
                                   1
                                   (for/hasheq ([k '(repo origin branch head tree)])
                                     (values k (hash-ref r1 k)))
                                   "attempt-2"
                                   8
                                   #f
                                   (hash-ref r1 'evidence)
                                   read-current
                                   #:owned-proof (hash-ref r1 'owned-proof)))
         (save! root
                fixture-plan
                1
                (hash-set (base-journal r1 (list r0))
                          'evidence-author-repair
                          (hash-set* marker
                                     'state
                                     "completed"
                                     'successor-receipt-sha256
                                     (canonical-evidence-digest r1))))
         ;; Supplemental private task T/A.
         (define context
           (hash-set (context-for r1 frozen (hash-ref ref2 'sha256) 1 "attempt-2" 8)
                     'repo
                     (path->string (simplify-path (path->complete-path repo)))))
         (define task
           (seal-supplemental-record (hasheq 'kind
                                             "supplemental-evidence-task/v1"
                                             'context
                                             context
                                             'base-command
                                             c0
                                             'effective-command
                                             fixture-effective-command
                                             'test-path
                                             fixture-test-path
                                             'harness-path
                                             fixture-helper-path
                                             'owner-paths
                                             '()
                                             'baseline-head
                                             baseline-head
                                             'baseline-tree
                                             baseline-tree
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
                                                     3600))))
         (define tref
           (propose-retain! "task-proposal"
                            (hasheq 'task task 'expires-at (+ (current-seconds) 2400))))
         (define aref (fixture-approve! tref))
         (start-supplemental-operation! aref (hasheq 'context context 'coordinator-fence 8))
         (define (step state facts)
           (define operation (load-supplemental-operation aref))
           (transition-supplemental-operation! aref
                                               (hash-ref operation 'revision)
                                               (hash-ref operation 'state)
                                               state
                                               facts))
         (step 'reserved
               (hasheq 'allocation
                       (hasheq 'attempt-id "attempt-3" 'attempt-fence 9 'coordinator-fence 9)))
         (step 'launched (hasheq 'dispatch-id "fixture-supplemental-author-once"))
         ;; Candidate K: the two-file witness commit.
         (put
          repo
          fixture-helper-path
          "#lang racket/base\n(require \"../../runtime/local.rkt\")\n(provide expected-answer)\n(define (expected-answer) 42)\n")
         (put repo
              fixture-test-path
              (string-append
               "#lang racket/base\n"
               "(require rackunit \"../runtime/local.rkt\" \"helpers/witness.rkt\")\n"
               "(provide evidence-suite)\n"
               "(define evidence-suite (test-suite \"supplemental witness\" "
               "(test-case \"the campaign answer is delivered\" "
               "(check-equal? (answer) (expected-answer)))))\n"
               "(module+ test (require rackunit/text-ui) (exit (run-tests evidence-suite)))\n"))
         (git-text cwd "add" fixture-test-path fixture-helper-path)
         (git-text cwd "commit" "-qm" "new supplemental witness")
         (set-campaign-fence-token! rec 9)
         (begin-attempt! rec 1 9)
         (set-campaign-wave-status! (car (campaign-record-waves rec)) 'verifying)
         (persist-campaign! root rec)
         (define source (committed-delivery-snapshot repo))
         (define manifest
           (for/list ([path (list fixture-test-path fixture-helper-path)])
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
                                             "attempt-3"
                                             'attempt-fence
                                             9
                                             'execution-cwd
                                             cwd
                                             'manifest
                                             manifest)))
         (define kref (propose-retain! "candidate-proposal" candidate (list tref aref)))
         (step 'candidate-sealed (hasheq 'candidate-ref kref))
         (define cap (fixture-approve! kref))
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
                               #f
                               'command-sha256
                               (sha256-string
                                (string-join (list "/bin/sh" "-c" fixture-effective-command)
                                             "\u0")))))
         ;; G1: the exact proposed receipt r*, captured through the private
         ;; resolver + protected operation.
         (define-values (g1-result r2 g1-capture)
           (capture-supplemental-delivery-verification root
                                                       fixture-plan
                                                       1
                                                       repo
                                                       "attempt-3"
                                                       9
                                                       contract
                                                       (lambda () (check-command root))))
         (unless (delivery-verification-approved? g1-result)
           (error 'fixture "supplemental C1 capture failed"))
         (step 'green-captured (hasheq 'green-proof-ref (hash-ref r2 'owned-proof)))
         ;; Staged wrapper with the exact predicted context E, derived from the
         ;; exact r*/G1 BEFORE the probe.
         (define staged
           (stage-supplemental-evidence! root
                                         fixture-plan
                                         1
                                         "attempt-3"
                                         9
                                         contract
                                         r2
                                         (hash-ref r2 'owned-proof)
                                         task
                                         candidate))
         ;; R1: fresh real owned baseline-overlay RED + identical-candidate GREEN.
         (define red-ref
           (run-staged-supplemental-probe! root
                                           fixture-plan
                                           1
                                           repo
                                           staged
                                           "repair-1"
                                           #:guard (lambda () #t)))
         (stage-supplemental-red! root fixture-plan 1 red-ref)
         (step 'probing (hasheq 'probe-job (hasheq 'execution-cwd cwd 'operation-id "repair-1")))
         (step 'probe-captured (hasheq 'probe-proof-ref red-ref))
         ;; S: sealed payload; operator approves via the authority owner
         ;; frontend with a REAL interactive broker grant.
         (define sref (seal-supplemental-supersession-proposal! root fixture-plan 1))
         (define sapref (fixture-approve! sref))
         (define staged-final
           (stage-supplemental-supersession-authorization! root fixture-plan 1 sref sapref))
         (step 'supersession-approved
               (hasheq 'supersession-proposal-ref sref 'supersession-approval-ref sapref))
         (define payload
           (load-supplemental-supersession-proposal!
            (hasheq 'kind "supersession-proposal" 'sha256 (hash-ref sref 'sha256))))
         ;; S-authorized publication: write-ahead intent -> push/readback ->
         ;; confirmed exact receipt -> journal CAS supersession (injected
         ;; remote publisher seam).
         (define journal-after-publish
           (parameterize ([current-gsd-approved-head-publisher (stub-publisher)])
             (publish-supplemental-supersession! root
                                                 fixture-plan
                                                 1
                                                 read-current
                                                 sapref
                                                 payload
                                                 (lambda () #t))))
         (unless (equal? (hash-ref journal-after-publish 'receipt) r2)
           (error 'fixture "the supplemental supersession did not certify r2"))
         ;; Protected operation advances through the publication tail to the
         ;; certified state promotion requires (the same idempotent at-target
         ;; transitions the publication tail performs; complete-supplemental-
         ;; supersession! itself is not exported).
         (supplemental-staged-probe-transition!
          aref
          "supersession-approved"
          "published"
          (hasheq
           'publication
           (hasheq 'status "confirmed" 'branch (hash-ref r2 'branch) 'head (hash-ref r2 'head))))
         (supplemental-staged-probe-transition! aref
                                                "published"
                                                "receipt-certified"
                                                (hasheq 'receipt (canonical-evidence-digest r2)))
         (define certified-operation (load-supplemental-operation aref))
         (unless (equal? (hash-ref certified-operation 'state) "receipt-certified")
           (error 'fixture
                  "fixture did not reach receipt-certified: ~a"
                  (hash-ref certified-operation 'state)))
         (proc (hasheq 'root
                       root
                       'repo
                       repo
                       'cwd
                       cwd
                       'frozen
                       frozen
                       'rec
                       rec
                       'read-current
                       read-current
                       'r0
                       r0
                       'r1
                       r1
                       'r2
                       r2
                       'contract
                       contract
                       'aref
                       aref
                       'step
                       step
                       'staged
                       staged-final
                       'context
                       (hash-ref (supplemental-staged-evidence-record staged-final) 'context)
                       'red-ref
                       red-ref
                       'sref
                       sref
                       'sapref
                       sapref
                       'payload
                       payload))))
     (lambda ()
       (clear-approval-channel!)
       (delete-directory/files root)))))
