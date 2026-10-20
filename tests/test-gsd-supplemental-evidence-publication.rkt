#lang racket
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 900
;; @covers extensions/gsd/supplemental-evidence-publication.rkt
;; @covers extensions/gsd/delivery-publication.rkt
;; @covers extensions/gsd/delivery-journal.rkt
;; BUG-0088 sections 7-8: exact S-authorized publication/supersession.
;; Self-contained fixture: temp q home, real private authority store, real
;; interactive broker grants, real owned C0/G1 jobs and a real owned
;; baseline-overlay RED/identical-candidate GREEN probe. The remote
;; push/readback effect is the existing injected publisher seam; every
;; authorization is a genuine broker-approved private record.
(require rackunit
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         racket/system
         "helpers/private-fixture-templates.rkt"
         (only-in "helpers/delivery-fixtures.rkt" write-plan! write-wave-doc!)
         "../runtime/approval/broker.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/composition-root.rkt"
         "../extensions/gsd/delivery-verifier.rkt"
         "../extensions/gsd/delivery-receipt.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/delivery-publication.rkt"
         "../extensions/gsd/effective-verify-binding.rkt"
         "../extensions/gsd/evidence-repair-format.rkt"
         "../extensions/gsd/evidence-probe.rkt"
         "../extensions/gsd/plan-snapshot.rkt"
         "../extensions/gsd/review-proof.rkt"
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         (submod "../extensions/gsd/supplemental-evidence-authority.rkt" owner-frontend)
         "../extensions/gsd/supplemental-evidence-format.rkt"
         "../extensions/gsd/supplemental-evidence-journal.rkt"
         "../extensions/gsd/supplemental-evidence-proof.rkt"
         "../extensions/gsd/supplemental-evidence-publication.rkt"
         "../extensions/gsd/verification-job.rkt"
         (only-in "../util/json/checksum.rkt" sha256-file sha256-string))
(define-runtime-path installed-root "..")
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
(define (git-text cwd . args)
  (define out (open-output-string))
  (parameterize ([current-output-port out])
    (unless (zero? (apply system*/exit-code (find-executable-path "git") "-C" cwd args))
      (error 'fixture "git failed: ~a" args)))
  (string-trim (get-output-string out)))
(define (interactive-grant ref)
  (define digest (hash-ref ref 'sha256))
  (define id
    (register-approval-request-for-channel! (current-approval-channel) digest (hasheq 'proposal ref)))
  (unless (and id (approval-decide! id digest #t))
    (error 'fixture "broker decision failed"))
  (define-values (outcome grant) (approval-await-grant id digest 5000))
  (unless (eq? outcome 'approved)
    (error 'fixture "broker grant unavailable"))
  grant)
(define (approve ref)
  (approve-supplemental-proposal! ref (interactive-grant ref)))
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
                    ;; Only actual module-path strings: data like the
                    ;; "run-tests.rkt" constant in runner.rkt is not a require.
                    ;; ../util requires are real closure dependencies and kept.
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
                     plan
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
(define (with-fixture proc)
  (define root (make-temporary-file "supplemental-pub-~a" 'directory))
  (eprintf "fixture: root created\n")
  (define repo (build-path root "q"))
  (define cwd (path->string repo))
  (define private (build-path root "private"))
  (with-handlers ([void (lambda (e)
                          (eprintf "fixture error raw: ~s\n" e)
                          (when (exn? e)
                            (for ([k (in-list (continuation-mark-set->context (exn-continuation-marks e)))]
                                  [i (in-naturals)]
                                  #:when (< i 12))
                              (eprintf "  at: ~s\n" k)))
                          (with-handlers ([exn:fail? void]) (delete-directory/files root))
                          (raise e))])
    (dynamic-wind
     (lambda ()
       (eprintf "fixture: pre\n")
       (when (current-approval-channel)
         (error 'fixture "active frontend"))
       (set-approval-channel! (make-approval-channel #:timeout-ms 30000)))
     (lambda ()
       (eprintf "fixture: body start\n")
       (define log-rc (make-log-receiver (current-logger) 'warning))
       (thread (lambda ()
                 (let loop ()
                   (define v (sync log-rc))
                   (eprintf "LOG: ~s\n"
                            (if (vector? v)
                                (vector-ref v 1)
                                v))
                   (loop))))
       (make-directory repo)
       (eprintf "fixture: repo dir\n")
       (make-directory private #o700)
       (eprintf "fixture: private dir\n")
       (git-text cwd "init" "-q")
       (eprintf "fixture: git init\n")
       (hermetic-identity! repo)
       (eprintf "fixture: identity\n")
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
       (make-plan-snapshot! root plan (file->string (build-path root ".planning" "PLAN.md")))
       (parameterize ([current-supplemental-authority-root private]
                      [current-owned-proof-root (build-path root "proofs")]
                      [current-gsd-verification-registry (make-verification-registry)]
                      [current-gsd-delivery-verify-timeout-sec 120]
                      [current-gsd-delivery-branch-context
                       (make-branch-delivery-context #:repo-root repo
                                                     #:branch "campaign/w1"
                                                     #:base-commit baseline-head
                                                     #:worktree-path repo)])
         (define frozen (frozen-verify-binding root plan 1))
         (define rec
           (make-campaign-record
            plan
            campaign-manifest
            (list
             (make-campaign-wave 1 "proof" 'awaiting-delivery 1 (campaign-attempt "attempt-1" 7 0)))
            #f
            7
            'test
            0
            0))
         (set-campaign-record-plan-snapshot-path! rec (path->string (snapshot-dir root plan)))
         (set-campaign-record-plan-snapshot-digest! rec (hash-ref frozen 'snapshot-sha256))
         (persist-campaign! root rec)
         (define (read-current)
           (load-campaign-record root plan))
         (define before (committed-delivery-snapshot repo))
         ;; G0: real owned C0 GREEN at attempt-1/7.
         (define-values (g0-result g0-capture)
           (call-with-owned-proof-capture root
                                          plan
                                          1
                                          before
                                          "attempt-1"
                                          7
                                          (lambda () (check-command root))
                                          #:cwd repo))
         (unless (and (delivery-verification-approved? g0-result) g0-capture)
           (eprintf "g0 result: ~s~ncapture: ~s~n" g0-result g0-capture)
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
         (record-delivery-receipt! root plan 1 r0)
         ;; Publish r0 through the ordinary route (confirmed prior publication).
         (parameterize ([current-gsd-approved-head-publisher (stub-publisher)])
           (publish-approved-head! root
                                   plan
                                   1
                                   (for/hasheq ([k '(repo origin branch head tree)])
                                     (values k (hash-ref r0 k)))
                                   "attempt-1"
                                   7
                                   #f
                                   (hash-ref r0 'evidence)
                                   read-current))
         ;; Author-allowance supersession: r1 at attempt-2/8 (real capture) and
         ;; a durably completed (spent) evidence-author marker. The successor
         ;; receipt needs a real descendant commit, and the marker must be
         ;; running while its successor publishes (the historical author flow).
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
                                          plan
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
                   plan
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
         ;; Historical order: the running marker is pinned to the current r0;
         ;; its successor publishes while running; then the allowance is spent.
         (save! root plan 1 (base-journal r0 '()))
         (check-equal? (hash-ref (load-delivery-journal root plan 1) 'receipt) r0)
         (parameterize ([current-gsd-approved-head-publisher (stub-publisher)])
           (publish-approved-head! root
                                   plan
                                   1
                                   (for/hasheq ([k '(repo origin branch head tree)])
                                     (values k (hash-ref r1 k)))
                                   "attempt-2"
                                   8
                                   #f
                                   (hash-ref r1 'evidence)
                                   read-current
                                   #:owned-proof (hash-ref r1 'owned-proof)))
         ;; The author allowance is spent only after its successor published.
         (save! root
                plan
                1
                (hash-set (base-journal r1 (list r0))
                          'evidence-author-repair
                          (hash-set* marker
                                     'state
                                     "completed"
                                     'successor-receipt-sha256
                                     (canonical-evidence-digest r1))))
         (check-equal? (hash-ref (load-delivery-journal root plan 1) 'receipt) r1)
         ;; Supplemental private task T/A.
         ;; The evidence context's repo is the exact execution checkout string
         ;; (no trailing slash), matching the resolver's cwd normalization.
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
                                             c1
                                             'test-path
                                             test-path
                                             'harness-path
                                             helper-path
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
         (define aref (approve tref))
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
          helper-path
          "#lang racket/base\n(require \"../../runtime/local.rkt\")\n(provide expected-answer)\n(define (expected-answer) 42)\n")
         (put repo
              test-path
              (string-append
               "#lang racket/base\n"
               "(require rackunit \"../runtime/local.rkt\" \"helpers/witness.rkt\")\n"
               "(provide evidence-suite)\n"
               "(define evidence-suite (test-suite \"supplemental witness\" "
               "(test-case \"the campaign answer is delivered\" "
               "(check-equal? (answer) (expected-answer)))))\n"
               "(module+ test (require rackunit/text-ui) (exit (run-tests evidence-suite)))\n"))
         (git-text cwd "add" test-path helper-path)
         (git-text cwd "commit" "-qm" "new supplemental witness")
         (set-campaign-fence-token! rec 9)
         (begin-attempt! rec 1 9)
         (set-campaign-wave-status! (car (campaign-record-waves rec)) 'verifying)
         (persist-campaign! root rec)
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
                                             "attempt-3"
                                             'attempt-fence
                                             9
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
                               #f
                               'command-sha256
                               (sha256-string (string-join (list "/bin/sh" "-c" c1) "\u0")))))
         ;; G1: the exact proposed receipt r*, captured through the private
         ;; resolver + protected operation.
         (define-values (g1-result r2 g1-capture)
           (capture-supplemental-delivery-verification root
                                                       plan
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
                                         plan
                                         1
                                         "attempt-3"
                                         9
                                         contract
                                         r2
                                         (hash-ref r2 'owned-proof)
                                         task
                                         candidate))
         (check-true (hash? (hash-ref (supplemental-staged-evidence-record staged) 'context)))
         ;; R1: fresh real owned baseline-overlay RED + identical-candidate GREEN.
         (define red-ref
           (run-staged-supplemental-probe! root plan 1 repo staged "repair-1" #:guard (lambda () #t)))
         (define staged-with-red (stage-supplemental-red! root plan 1 red-ref))
         (step 'probing (hasheq 'probe-job (hasheq 'execution-cwd cwd 'operation-id "repair-1")))
         (step 'probe-captured (hasheq 'probe-proof-ref red-ref))
         ;; S: sealed payload with the actual r*, old receipt sha/generation,
         ;; G1/R1/task refs, op id and snapshot; operator approves via the
         ;; authority owner frontend with a REAL interactive broker grant.
         (define sref (seal-supplemental-supersession-proposal! root plan 1))
         (define sapref (approve sref))
         (define staged-final
           (stage-supplemental-supersession-authorization! root plan 1 sref sapref))
         (step 'supersession-approved
               (hasheq 'supersession-proposal-ref sref 'supersession-approval-ref sapref))
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
                       'marker
                       marker
                       'contract
                       contract
                       'aref
                       aref
                       'step
                       step
                       'staged
                       staged-final
                       'red-ref
                       red-ref
                       'sref
                       sref
                       'sapref
                       sapref
                       'payload
                       (load-supplemental-supersession-proposal!
                        (hasheq 'kind "supersession-proposal" 'sha256 (hash-ref sref 'sha256)))))))
     (lambda ()
       (clear-approval-channel!)
       (delete-directory/files root)))))
(module+ test
  (test-case "the exact S-authorized receipt publishes with the existing intent/readback protocol and CAS-supersedes the journal"
    (with-fixture
     (lambda (env)
       (define root (hash-ref env 'root))
       (define repo (hash-ref env 'repo))
       (define r2 (hash-ref env 'r2))
       (define sapref (hash-ref env 'sapref))
       (define payload (hash-ref env 'payload))
       (define read-current (hash-ref env 'read-current))
       (define journal-before (load-delivery-journal root plan 1))
       ;; The legacy route refuses supplemental GREEN outright.
       (check-exn #rx"supplemental publication requires the exact supersession owner transition"
                  (lambda ()
                    (parameterize ([current-gsd-approved-head-publisher (stub-publisher)])
                      (publish-approved-head! root
                                              plan
                                              1
                                              (for/hasheq ([k '(repo origin branch head tree)])
                                                (values k (hash-ref r2 k)))
                                              "attempt-3"
                                              9
                                              #f
                                              (hash-ref r2 'evidence)
                                              read-current
                                              #:owned-proof (hash-ref r2 'owned-proof)))))
       ;; A fabricated/foreign S authorization refuses before any effect.
       (check-exn exn:fail?
                  (lambda ()
                    (publish-supplemental-supersession!
                     root
                     plan
                     1
                     read-current
                     (hasheq 'kind "supersession-approval" 'sha256 (make-string 64 #\f))
                     payload
                     (lambda () #t))))
       (check-equal? (hash-ref (load-delivery-journal root plan 1) 'receipt) (hash-ref env 'r1))
       ;; Happy path: write-ahead intent -> push/readback -> confirmed exact
       ;; receipt -> journal CAS supersession.
       (define journal-after
         (parameterize ([current-gsd-approved-head-publisher (stub-publisher)])
           (publish-supplemental-supersession! root
                                               plan
                                               1
                                               read-current
                                               sapref
                                               payload
                                               (lambda () #t))))
       (check-equal? (hash-ref journal-after 'receipt) r2)
       (check-equal? (hash-ref journal-after 'stage) "context-ready")
       (check-equal? (map (lambda (r) (hash-ref r 'attempt-id))
                          (hash-ref journal-after 'receipt-history))
                     '("attempt-1" "attempt-2"))
       ;; The spent author marker stayed byte-identical.
       (let ([actual (hash-ref journal-after 'evidence-author-repair)]
             [expected (hash-set* (hash-ref env 'marker)
                                  'state
                                  "completed"
                                  'successor-receipt-sha256
                                  (canonical-evidence-digest (hash-ref env 'r1)))])
         (let loop ([a actual] [e expected] [path '()])
           (cond
             [(and (hash? a) (hash? e))
              (for ([k (in-list (remove-duplicates (append (hash-keys a) (hash-keys e))))])
                (loop (hash-ref a k 'absent) (hash-ref e k 'absent) (append path (list k))))]
             [(not (equal? a e))
              (eprintf "MARKER-DIFF at ~s: actual=~s expected=~s\n" path
                       (if (hash? a) '<hash> a)
                       (if (hash? e) '<hash> e))]))
         (check-equal? actual expected))
       ;; The publication record preserves the EXACT r* bytes (timestamp
       ;; included) and grew its history by exactly one entry.
       (define publication (load-approved-publication root plan 1))
       (check-equal? (hash-ref publication 'status) "confirmed")
       (check-equal? (hash-ref publication 'receipt-candidate) r2)
       ;; r0->r1 (author successor) and r1->r2 (supplemental) each appended.
       (check-equal? (length (hash-ref publication 'publication-history)) 2)
       ;; The staged wrapper is NOT current readiness: the journal moved, so
       ;; a stale wrapper can never be re-published through this route.
       ;; Idempotent replay: no second history append, no journal rewrite.
       (define journal-bytes (file->bytes (delivery-journal-path root plan 1)))
       (define journal-replay
         (parameterize ([current-gsd-approved-head-publisher (stub-publisher)])
           (publish-supplemental-supersession! root
                                               plan
                                               1
                                               read-current
                                               sapref
                                               payload
                                               (lambda () #t))))
       (check-equal? (hash-ref journal-replay 'receipt) r2)
       (check-equal? (file->bytes (delivery-journal-path root plan 1)) journal-bytes)
       ;; Replay appends no third publication-history entry.
       (check-equal? (length (hash-ref (load-approved-publication root plan 1) 'publication-history))
                     2)
       (check-equal? (hash-ref (load-approved-publication root plan 1) 'receipt-candidate) r2)
       ;; Operation advanced through the publication tail.
       ((hash-ref env 'step)
        'published
        (hasheq 'publication
                (hasheq 'status "confirmed" 'head (hash-ref r2 'head) 'branch (hash-ref r2 'branch))))
       ((hash-ref env 'step) 'receipt-certified (hasheq 'receipt (canonical-evidence-digest r2)))
       ;; Direct journal CAS negatives: generation/digest late-cut guards.
       (check-exn #rx"generation advanced"
                  (lambda ()
                    (supersede-supplemental-receipt! root
                                                     plan
                                                     1
                                                     r2
                                                     #:expected-old-receipt-sha256
                                                     (hash-ref payload 'old-receipt-sha256)
                                                     #:expected-generation 0
                                                     #:expected-attempt-id "attempt-3"
                                                     #:expected-attempt-fence 9
                                                     #:head-ancestor? (lambda (_o _n) #t))))
       (check-exn #rx"not the S-authorized predecessor"
                  (lambda ()
                    (supersede-supplemental-receipt! root
                                                     plan
                                                     1
                                                     r2
                                                     #:expected-old-receipt-sha256
                                                     (make-string 64 #\a)
                                                     #:expected-generation 2
                                                     #:expected-attempt-id "attempt-3"
                                                     #:expected-attempt-fence 9
                                                     #:head-ancestor? (lambda (_o _n) #t))))))
    (test-case "an interrupted publication replays from the durable intent; revocation refuses any further replay"
      (with-fixture
       (lambda (env)
         (define root (hash-ref env 'root))
         (define sapref (hash-ref env 'sapref))
         (define payload (hash-ref env 'payload))
         (define read-current (hash-ref env 'read-current))
         ;; Crash cut: the intent is durably written, then the remote effect
         ;; fails. Nothing is certified; replay with a working publisher
         ;; completes exactly once.
         (define publisher (stub-publisher))
         (check-exn #rx"supplemental publication refused"
                    (lambda ()
                      (parameterize ([current-gsd-approved-head-publisher
                                      (lambda args
                                        (hasheq 'status "blocked" 'reason "remote readback failed"))])
                        (publish-supplemental-supersession! root
                                                            plan
                                                            1
                                                            read-current
                                                            sapref
                                                            payload
                                                            (lambda () #t)))))
         (define publication-after-crash (load-approved-publication root plan 1))
         (check-equal? (hash-ref publication-after-crash 'status) "intent")
         (check-equal? (hash-ref publication-after-crash 'receipt-candidate) (hash-ref env 'r2))
         (check-equal? (hash-ref (load-delivery-journal root plan 1) 'receipt) (hash-ref env 'r1))
         (define journal-after-replay
           (parameterize ([current-gsd-approved-head-publisher publisher])
             (publish-supplemental-supersession! root
                                                 plan
                                                 1
                                                 read-current
                                                 sapref
                                                 payload
                                                 (lambda () #t))))
         (check-equal? (hash-ref journal-after-replay 'receipt) (hash-ref env 'r2))
         ;; The crash cut appended the intent once; the replay confirmed the
         ;; SAME intent, so the history grew by exactly one entry overall.
         (check-equal? (length (hash-ref (load-approved-publication root plan 1)
                                         'publication-history))
                       2)
         ;; After revocation of the S approval, no further replay is possible.
         (define revocation
           (propose-retain! "revocation-proposal"
                            (hasheq 'reason "fixture operator revoked the supersession approval")
                            (list sapref)))
         (revoke! revocation (interactive-grant revocation))
         (check-exn
          #rx"supersession authorization changed|supersession approval is missing|supplemental publication identity changed"
          (lambda ()
            (parameterize ([current-gsd-approved-head-publisher publisher])
              (publish-supplemental-supersession! root
                                                  plan
                                                  1
                                                  read-current
                                                  sapref
                                                  payload
                                                  (lambda () #t))))))))))
