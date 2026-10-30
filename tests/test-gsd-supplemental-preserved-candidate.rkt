#lang racket
;; @speed slow
;; @suite extensions
;; @boundary unit
;; @timeout 600
;; Cold module-graph compilation is measured near five minutes, not a fast unit test.
;; Focused CORE-lane regressions for the optional preserved-candidate binding.
;; A genuine spent third-generation journal task is built through the real
;; authority/broker and journal transitions; the old candidate proposal and
;; approval are retained/approved through the real private store. The Git
;; fixture is a real temporary repository whose metadata descendant M changes
;; exactly the inherited owner paths. No live campaign, model, publication or
;; Verify is touched.
(require rackunit
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         racket/system
         (only-in file/sha1 bytes->hex-string)
         openssl
         (only-in "test-gsd-supplemental-evidence-journal.rkt"
                  with-fixture
                  fixture-root
                  fixture-workspace
                  fixture-grant
                  approve!
                  revoke-approval!)
         "../extensions/gsd/supplemental-evidence-format.rkt"
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         "../extensions/gsd/supplemental-evidence-journal.rkt"
         "../extensions/gsd/supplemental-disposition-policy.rkt"
         (submod "../extensions/gsd/supplemental-disposition-policy.rkt" test-support)
         (submod "../extensions/gsd/supplemental-disposition-policy.rkt" ancestry-support)
         "../extensions/gsd/supplemental-disposition-claim.rkt"
         "../extensions/gsd/supplemental-preserved-candidate.rkt"
         "../extensions/gsd/effective-verify-binding.rkt"
         "../extensions/gsd/evidence-readiness.rkt"
         "../extensions/gsd/review-proof.rkt"
         "../extensions/gsd/evidence-repair-format.rkt"
         (only-in "../util/json/checksum.rkt" sha256-string))
(define owner-paths
  '("README.md" "reports/test-ownership-map.md"
                "reports/test-ownership-map.json"
                "artifacts/tier-ownership/v0.00.00-fixture/ownership-matrix.json"
                "artifacts/tier-ownership/v0.00.00-fixture/SHA256SUMS"))
(define test-path "tests/test-preserved-probe.rkt")
(define harness-path "tests/helpers/preserved-harness.rkt")
(define model-source-path "runtime/model-source.rkt")
(define branch "campaign/w1")
(define origin "https://example.invalid/preserved.git")
(define-runtime-path candidate-module "../extensions/gsd/supplemental-preserved-candidate.rkt")
(define (hex n ch)
  (string->immutable-string (make-string n ch)))
(define (as-path-string p)
  (if (path? p)
      (path->string p)
      p))
(define (run-git repo . args)
  (define out (open-output-string))
  (define err (open-output-string))
  (define code
    (parameterize ([current-output-port out]
                   [current-error-port err])
      (apply system*/exit-code (find-executable-path "git") "-C" (as-path-string repo) args)))
  (unless (zero? code)
    (error 'preserved-test "git ~s failed: ~a" args (get-output-string err)))
  (string-trim (get-output-string out)))
(define (run-git-bytes repo . args)
  (define out (open-output-bytes))
  (define err (open-output-string))
  (define code
    (parameterize ([current-output-port out]
                   [current-error-port err])
      (apply system*/exit-code (find-executable-path "git") "-C" (as-path-string repo) args)))
  (unless (zero? code)
    (error 'preserved-test "git ~s failed: ~a" args (get-output-string err)))
  (get-output-bytes out))
;; Hash the EXACT Git blob bytes (one owner fixture blob is deliberately
;; non-UTF-8), never a decoded/re-encoded string.
(define (blob-sha repo oid)
  (bytes->hex-string
   (sha256-bytes (open-input-bytes (apply run-git-bytes repo (list "cat-file" "blob" oid))))))
(define (put repo relative text)
  (define p (build-path repo relative))
  (make-parent-directory* p)
  (display-to-file text p #:exists 'truncate))
(define (put-bytes repo relative bytes)
  (define p (build-path repo relative))
  (make-parent-directory* p)
  (display-to-file bytes p #:exists 'truncate))
(define (owner-text path version)
  (format "~a version ~a\n" path version))
;; A REAL two-worktree fixture: the LOGICAL candidate source (q) and the
;; PHYSICAL authenticated context (wt-campaign) are distinct paths that share
;; one git-common-dir. E_old, K and M are three distinct commits:
;;   E_old --test/harness--> K --owner metadata--> M
(define (make-repo)
  (define workspace (make-temporary-file "preserved-workspace-~a" 'directory))
  (define logical (build-path workspace "q"))
  (make-directory logical)
  (run-git logical "init" "-q")
  (run-git logical "config" "user.name" "Preserved Fixture")
  (run-git logical "config" "user.email" "preserved@invalid")
  (run-git logical "checkout" "-q" "-b" branch)
  (run-git logical "remote" "add" "origin" origin)
  (put logical model-source-path "#lang racket/base\n(provide answer)\n(define answer 42)\n")
  (put logical test-path "placeholder test\n")
  (put logical harness-path "placeholder harness\n")
  (for ([path (in-list owner-paths)])
    ;; One owner blob is deliberately non-UTF-8: manifest sha256 must be over
    ;; the raw Git object bytes, not a string round-trip.
    (if (equal? path (car owner-paths))
        (put-bytes logical path #"\377\376owner metadata old\n")
        (put logical path (owner-text path "old"))))
  (run-git logical "add" "-A")
  (run-git logical "commit" "-qm" "authenticated old E context")
  (define old-head (run-git logical "rev-parse" "HEAD"))
  (define old-tree (run-git logical "rev-parse" "HEAD^{tree}"))
  ;; Original candidate K: only the model-authorable test/harness paths change.
  (put logical test-path "candidate test\n")
  (put logical harness-path "candidate harness\n")
  (run-git logical "add" test-path harness-path)
  (run-git logical "commit" "-qm" "original K candidate")
  (define k-head (run-git logical "rev-parse" "HEAD"))
  (define k-tree (run-git logical "rev-parse" "HEAD^{tree}"))
  ;; The physical linked checkout is the authenticated context; the main repo
  ;; remains the detached LOGICAL candidate source at K.
  (define physical (build-path workspace "wt-campaign"))
  (run-git logical "checkout" "-q" "--detach")
  (run-git logical "worktree" "add" "-q" (path->string physical) branch)
  ;; Metadata descendant M: exactly the inherited owner paths change.
  (for ([path (in-list owner-paths)])
    (put physical path (owner-text path "new")))
  (run-git physical "add" "-A")
  (run-git physical "commit" "-qm" "metadata settlement M")
  (define m-head (run-git physical "rev-parse" "HEAD"))
  (define m-tree (run-git physical "rev-parse" "HEAD^{tree}"))
  (hasheq 'workspace
          workspace
          'logical
          logical
          'repo
          physical
          'old-head
          old-head
          'old-tree
          old-tree
          'k-head
          k-head
          'k-tree
          k-tree
          'm-head
          m-head
          'm-tree
          m-tree))
(define (snapshot repo head tree)
  (hasheq 'repo
          (path->string (simplify-path (path->complete-path repo) #f))
          'branch
          branch
          'origin
          origin
          'head
          head
          'tree
          tree))
(define (manifest-entry repo k-head path)
  (define oid (run-git repo "rev-parse" (format "~a:~a" k-head path)))
  (hasheq 'path path 'mode "100644" 'blob oid 'sha256 (blob-sha repo oid)))
(define (expected-manifest repo k-head)
  (for/list ([path (in-list (append (list test-path harness-path) owner-paths))])
    (manifest-entry repo k-head path)))
(define base-command
  (string-join
   '("raco fmt -i runtime/model-source.rkt" "raco make runtime/model-source.rkt"
                                            "racket scripts/run-tests.rkt tests/test-existing.rkt"
                                            "racket scripts/run-tests.rkt --suite fast"
                                            "racket scripts/lint-all.rkt")
   " && "))
(define (candidate-task repo ctx)
  (seal-supplemental-record (hasheq 'kind
                                    "supplemental-evidence-task/v1"
                                    'context
                                    ctx
                                    'base-command
                                    base-command
                                    'effective-command
                                    (make-effective-verify-command base-command test-path)
                                    'test-path
                                    test-path
                                    'harness-path
                                    harness-path
                                    'owner-paths
                                    owner-paths
                                    'baseline-head
                                    (hex 40 #\1)
                                    'baseline-tree
                                    (hex 40 #\2)
                                    'control-plane-head
                                    (hex 40 #\3)
                                    'bounds
                                    (hasheq 'model-dispatches
                                            1
                                            'verify-timeout-sec
                                            14400
                                            'probe-timeout-sec
                                            600
                                            'deadline-sec
                                            3600))))
(define (pointer p g)
  (disposition-pointer p g))
(define (put-json path value)
  (display-to-file (canonical-evidence-json value) path #:exists 'truncate)
  (file-or-directory-permissions path #o600))
(define (spend! grant base allocation)
  (start-supplemental-operation! grant base)
  (transition-supplemental-operation! grant
                                      0
                                      "proposal-approved"
                                      "reserved"
                                      (hasheq 'allocation allocation))
  (transition-supplemental-operation! grant
                                      1
                                      "reserved"
                                      "launched"
                                      (hasheq 'dispatch-id "fixture-spent-launch"))
  (transition-supplemental-operation! grant
                                      2
                                      "launched"
                                      "blocked"
                                      (hasheq 'block-reason "fixture spent"))
  (load-supplemental-operation grant))
(define (spend-late-genuine! grant base repo ctx snapshot allocation parent-proposal parent-approval)
  (define (step! from to facts)
    (define op (load-supplemental-operation grant))
    (transition-supplemental-operation! grant (hash-ref op 'revision) from to facts))
  (start-supplemental-operation! grant base)
  (step! "proposal-approved" "reserved" (hasheq 'allocation allocation))
  (step! "reserved" "launched" (hasheq 'dispatch-id "supplemental-author-attempt-6"))
  (define retained (load-supplemental-operation grant))
  (define task
    (hash-ref (hash-ref (load-supplemental-record (hash-ref retained 'proposal-ref)) 'payload) 'task))
  (define candidate
    (seal-supplemental-record (hasheq 'kind
                                      "supplemental-evidence-candidate/v1"
                                      'context
                                      ctx
                                      'task-sha256
                                      (hash-ref task 'sha256)
                                      'task-proposal
                                      parent-proposal
                                      'task-approval
                                      parent-approval
                                      'source
                                      snapshot
                                      'attempt-id
                                      (hash-ref allocation 'attempt-id)
                                      'attempt-fence
                                      (hash-ref allocation 'attempt-fence)
                                      'execution-cwd
                                      (hash-ref ctx 'repo)
                                      'manifest
                                      (expected-manifest repo (hash-ref snapshot 'head)))))
  (define kref
    (propose-retain! "candidate-proposal" candidate (list parent-proposal parent-approval)))
  (define capref (approve! kref))
  (step! "launched" "candidate-sealed" (hasheq 'candidate-ref kref))
  (step! "candidate-sealed" "candidate-approved" (hasheq 'candidate-approval-ref capref))
  (step! "candidate-approved"
         "verifying"
         (hasheq 'verify-job (hasheq 'id "fixture-verify-job" 'owner "fixture-controller")))
  (step! "verifying" "blocked" (hasheq 'block-reason "operator deadline expired"))
  (values (load-supplemental-operation grant) kref capref))
(define (child! parent old a)
  (define d (disposition-descriptor a old))
  (define t
    (seal-supplemental-record
     (hash-set* parent 'kind "supplemental-evidence-task/v3" 'disposition d)))
  (define p (propose-retain! "task-proposal" (hasheq 'task t 'expires-at (+ (current-seconds) 1700))))
  (define g (approve! p))
  (values p g t (pointer p g)))
(define (preserved-task base d)
  (seal-supplemental-record (hash-set* base 'disposition d)))
(define (with-preserved-fixture proc)
  (define repo-fixture (make-repo))
  (define workspace (hash-ref repo-fixture 'workspace))
  (define logical (hash-ref repo-fixture 'logical))
  (define repo (hash-ref repo-fixture 'repo))
  (define old-head (hash-ref repo-fixture 'old-head))
  (define old-tree (hash-ref repo-fixture 'old-tree))
  (define k-head (hash-ref repo-fixture 'k-head))
  (define k-tree (hash-ref repo-fixture 'k-tree))
  (define m-head (hash-ref repo-fixture 'm-head))
  (define m-tree (hash-ref repo-fixture 'm-tree))
  (define source (snapshot logical k-head k-tree))
  (define intended-m (snapshot logical m-head m-tree))
  (define ctx
    (hasheq 'plan-id
            (hex 64 #\a)
            'wave
            1
            'attempt-id
            "attempt-6"
            'attempt-fence
            9
            'receipt-generation
            3
            'receipt-sha256
            (hex 64 #\b)
            'evidence-revision
            0
            'repo
            (path->string (simplify-path (path->complete-path repo) #f))
            'branch
            branch
            'origin
            origin
            'head
            old-head
            'tree
            old-tree
            'snapshot-sha256
            (hex 64 #\e)
            'green-proof-sha256
            (hex 64 #\f)))
  (define base (hasheq 'context ctx 'coordinator-fence 9))
  (define allocation1 (hasheq 'attempt-id "attempt-4" 'attempt-fence 7 'coordinator-fence 7))
  (define allocation2 (hasheq 'attempt-id "attempt-5" 'attempt-fence 8 'coordinator-fence 8))
  (define allocation3 (hasheq 'attempt-id "attempt-6" 'attempt-fence 9 'coordinator-fence 9))
  (dynamic-wind
   void
   (lambda ()
     (with-fixture
      (lambda (f)
        (define root-grant (fixture-grant f))
        (define root-proposal (hash-ref (validate-supplemental-approval root-grant) 'proposal))
        (spend! root-grant base allocation1)
        (define-values (rp rt ro) (inspect-supplemental-recovery-parent root-grant))
        (define path (build-path (fixture-workspace f) "pointer.json"))
        (define old (pointer rp root-grant))
        (put-json path old)
        (define recovery
          (hasheq 'parent-proposal
                  rp
                  'parent-approval
                  root-grant
                  'terminal-sha256
                  (hash-ref ro 'sha256)
                  'pointer-sha256
                  (supplemental-record-digest old)
                  'allocation
                  allocation1))
        (define parent
          (seal-supplemental-record
           (hash-set* rt 'kind "supplemental-evidence-task/v2" 'recovery recovery)))
        (define pp
          (propose-retain! "task-proposal"
                           (hasheq 'task parent 'expires-at (+ (current-seconds) 1700))))
        (define pg (approve! pp))
        (define parent-pointer (pointer pp pg))
        (install-supplemental-recovery-claim! path old parent-pointer pp pg)
        (define po (spend! pg base allocation2))
        (define a1
          (seal-supplemental-record (hash-set* disposition-amendment-1
                                               'plan-id
                                               (hash-ref ctx 'plan-id)
                                               'wave
                                               1
                                               'root-proposal
                                               root-proposal
                                               'root-approval
                                               root-grant
                                               'parent-proposal
                                               pp
                                               'parent-approval
                                               pg
                                               'terminal-body-sha256
                                               (hash-ref po 'sha256)
                                               'terminal-record-sha256
                                               (sha256-string (canonical-evidence-json po))
                                               'snapshot-sha256
                                               (hash-ref ctx 'snapshot-sha256)
                                               'allocation
                                               allocation2)))
        (call-with-test-disposition-amendment
         a1
         (lambda ()
           (define-values (p1 g1 t1 target1) (child! parent parent-pointer a1))
           (install-supplemental-disposition-claim! path parent-pointer target1 p1 g1)
           (define t1-terminal (spend! g1 base allocation3))
           (define second
             (seal-supplemental-record
              (hash-set* a1
                         'v2-proposal
                         pp
                         'v2-approval
                         pg
                         'parent-proposal
                         p1
                         'parent-approval
                         g1
                         'terminal-body-sha256
                         (hash-ref t1-terminal 'sha256)
                         'terminal-record-sha256
                         (sha256-string (canonical-evidence-json t1-terminal))
                         'allocation
                         allocation3)))
           (call-with-test-disposition-amendment
            second
            (lambda ()
              (define-values (p2 g2 t2 target2) (child! t1 target1 second))
              (install-supplemental-disposition-claim! path target1 target2 p2 g2)
              (define-values (late-terminal kref capref)
                (spend-late-genuine! g2 base repo ctx source allocation3 p2 g2))
              (define third
                (seal-supplemental-record
                 (hash-set* second
                            'prior-parent-proposal
                            (hash-ref second 'parent-proposal)
                            'prior-parent-approval
                            (hash-ref second 'parent-approval)
                            'parent-proposal
                            p2
                            'parent-approval
                            g2
                            'terminal-body-sha256
                            (hash-ref late-terminal 'sha256)
                            'terminal-record-sha256
                            (sha256-string (canonical-evidence-json late-terminal))
                            'allocation
                            allocation3)))
              (call-with-test-disposition-amendment third
                                                    (lambda ()
                                                      (proc (hasheq 'f
                                                                    f
                                                                    'path
                                                                    path
                                                                    'workspace
                                                                    workspace
                                                                    'logical
                                                                    logical
                                                                    'repo
                                                                    repo
                                                                    'old-head
                                                                    old-head
                                                                    'old-tree
                                                                    old-tree
                                                                    'k-head
                                                                    k-head
                                                                    'k-tree
                                                                    k-tree
                                                                    'm-head
                                                                    m-head
                                                                    'm-tree
                                                                    m-tree
                                                                    'source
                                                                    source
                                                                    'intended-m
                                                                    intended-m
                                                                    'ctx
                                                                    ctx
                                                                    'parent-task
                                                                    t2
                                                                    'parent-pointer
                                                                    target2
                                                                    'amendment
                                                                    third
                                                                    'kref
                                                                    kref
                                                                    'capref
                                                                    capref)))
                                                    #:predecessor second))
            #:predecessor a1))))
      #:payload (hasheq 'task (candidate-task repo ctx) 'expires-at (+ (current-seconds) 1800))))
   (lambda () (delete-directory/files workspace))))
(define (owner-entries repo k-head m-head)
  (for/list ([path (in-list owner-paths)])
    (define before (run-git repo "rev-parse" (format "~a:~a" k-head path)))
    (define after (run-git repo "rev-parse" (format "~a:~a" m-head path)))
    (hasheq 'path
            path
            'before-blob
            before
            'before-sha256
            (blob-sha repo before)
            'after-blob
            after
            'after-sha256
            (blob-sha repo after))))
(define (make-binding fx)
  (define repo (hash-ref fx 'repo))
  (hasheq 'schema
          "supplemental-preserved-candidate/v1"
          'candidate
          (hash-ref fx 'kref)
          'candidate-approval
          (hash-ref fx 'capref)
          'original-source
          (hash-ref fx 'source)
          'descendant-source
          (hasheq 'head (hash-ref fx 'm-head) 'tree (hash-ref fx 'm-tree))
          'owner-paths
          (owner-entries repo (hash-ref fx 'k-head) (hash-ref fx 'm-head))))
(define (task-with-binding fx binding)
  (define third (hash-ref fx 'amendment))
  (define parent-pointer (hash-ref fx 'parent-pointer))
  (define descriptor (disposition-descriptor third parent-pointer #:preserved binding))
  (preserved-task (hash-ref fx 'parent-task) descriptor))
(define (expect-refusal pattern thunk)
  (check-exn pattern thunk))
;; Deterministic Git-call counting seam: a PATH shim executable appends one
;; byte per real `git` invocation and then execs the genuine git, so every
;; counted call keeps its exact behavior and refusal semantics. Counts are
;; process counts, never wall time, and the shim lives only inside the call.
(define (call-counting-git proc)
  (define real-git (find-executable-path "git"))
  (define shim-dir (make-temporary-file "preserved-git-shim-~a" 'directory))
  (define counter (make-temporary-file "preserved-git-count-~a"))
  (delete-file counter)
  (define shim (build-path shim-dir "git"))
  (call-with-output-file shim
                         (lambda (out)
                           (fprintf out
                                    "#!/bin/sh\nprintf x >> ~s\nexec ~s \"$@\"\n"
                                    (path->string counter)
                                    (path->string real-git)))
                         #:exists 'truncate)
  (file-or-directory-permissions shim #o755)
  (define shim-env (make-environment-variables))
  (for ([name (in-list (environment-variables-names (current-environment-variables)))])
    (environment-variables-set! shim-env
                                name
                                (environment-variables-ref (current-environment-variables) name)))
  (environment-variables-set! shim-env #"PATH" (string->bytes/utf-8 (path->string shim-dir)))
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-environment-variables shim-env])
                    (proc (lambda () (file-size counter)))))
                (lambda ()
                  (delete-directory/files shim-dir)
                  (when (file-exists? counter)
                    (delete-file counter)))))
(module+ test
  (test-case "ordinary current-evidence-context and receipt loader stay strict without a receipt"
    (define base (make-temporary-file "preserved-empty-~a" 'directory))
    (dynamic-wind void
                  (lambda ()
                    (parameterize ([current-owned-proof-root (build-path base "owner")])
                      (check-exn #rx"missing-owned-proof"
                                 (lambda ()
                                   (load-authenticated-evidence-receipt base (hex 64 #\a) 1)))
                      (check-exn #rx"missing-owned-proof"
                                 (lambda () (current-evidence-context base (hex 64 #\a) 1)))))
                  (lambda () (delete-directory/files base)))
    ;; The factored physical source gate is still strict: a clean HEAD equal
    ;; to the authenticated context passes, dirty/untracked/advanced HEAD
    ;; refuses. This is the gate preserved mode must NOT weaken.
    (define fx (make-repo))
    (dynamic-wind void
                  (lambda ()
                    (define repo (hash-ref fx 'repo))
                    (define ctx
                      (hasheq 'plan-id
                              (hex 64 #\a)
                              'wave
                              1
                              'attempt-id
                              "attempt-6"
                              'attempt-fence
                              9
                              'receipt-generation
                              3
                              'receipt-sha256
                              (hex 64 #\b)
                              'evidence-revision
                              0
                              'repo
                              (path->string (simplify-path (path->complete-path repo) #f))
                              'branch
                              branch
                              'origin
                              origin
                              'head
                              (hash-ref fx 'm-head)
                              'tree
                              (hash-ref fx 'm-tree)
                              'snapshot-sha256
                              (hex 64 #\e)
                              'green-proof-sha256
                              (hex 64 #\f)))
                    (check-true (valid-evidence-context? ctx))
                    (validate-current-evidence-source! ctx repo)
                    (put repo "untracked-scratch.txt" "x\n")
                    (check-exn #rx"stale-or-dirty-receipt"
                               (lambda () (validate-current-evidence-source! ctx repo)))
                    (delete-file (build-path repo "untracked-scratch.txt"))
                    (put repo (car owner-paths) "dirty tracked change\n")
                    (check-exn #rx"stale-or-dirty-receipt"
                               (lambda () (validate-current-evidence-source! ctx repo))))
                  (lambda () (delete-directory/files (hash-ref fx 'workspace)))))
  (test-case "preserved binding accepts only the exact K->M owner settlement"
    (with-preserved-fixture
     (lambda (fx)
       (define repo (hash-ref fx 'repo))
       (define logical (hash-ref fx 'logical))
       (define source (hash-ref fx 'source))
       (define ctx (hash-ref fx 'ctx))
       ;; REAL two-worktree layout: logical source and physical context differ
       ;; in path but share one git-common-dir; E_old != K != M.
       (check-not-equal? (hash-ref source 'repo) (hash-ref ctx 'repo))
       (check-equal?
        (run-git (hash-ref source 'repo) "rev-parse" "--path-format=absolute" "--git-common-dir")
        (run-git (hash-ref ctx 'repo) "rev-parse" "--path-format=absolute" "--git-common-dir"))
       (check-not-equal? (hash-ref fx 'old-head) (hash-ref fx 'k-head))
       (check-not-equal? (hash-ref fx 'k-head) (hash-ref fx 'm-head))
       (check-equal? (run-git repo "rev-parse" "HEAD") (hash-ref fx 'm-head))
       (define binding (make-binding fx))
       (define task (task-with-binding fx binding))
       (check-true (valid-preserved-candidate? binding))
       (check-true (valid-supplemental-task? task))
       (define admission (validate-preserved-candidate-claim-binding! task))
       (check-true (preserved-candidate-admission? admission))
       (check-equal? (preserved-candidate-admission-candidate admission) (hash-ref fx 'kref))
       (check-equal? (preserved-candidate-admission-candidate-approval admission)
                     (hash-ref fx 'capref))
       (check-equal? (preserved-candidate-admission-original-source admission) source)
       (check-equal? (preserved-candidate-admission-old-context admission) ctx)
       (check-equal? (preserved-candidate-admission-descendant-source admission)
                     (hasheq 'repo
                             (hash-ref source 'repo)
                             'branch
                             (hash-ref source 'branch)
                             'origin
                             (hash-ref source 'origin)
                             'head
                             (hash-ref fx 'm-head)
                             'tree
                             (hash-ref fx 'm-tree)))
       ;; The logical source is still exactly the candidate K identity.
       (check-equal? (run-git logical "rev-parse" (hash-ref source 'head)) (hash-ref fx 'k-head))
       ;; The claim owner revalidates the same binding on install, and the
       ;; descriptor base equality is unchanged by the optional key.
       (define p
         (propose-retain! "task-proposal" (hasheq 'task task 'expires-at (+ (current-seconds) 1700))))
       (define g (approve! p))
       (install-supplemental-disposition-claim! (hash-ref fx 'path)
                                                (hash-ref fx 'parent-pointer)
                                                (pointer p g)
                                                p
                                                g)
       (validate-disposition-claim! g p task)
       (define claim-path
         (build-path (fixture-root (hash-ref fx 'f))
                     (string-append "supplemental-disposition-"
                                    (hash-ref (hash-ref fx 'amendment) 'sha256)
                                    ".json")))
       (check-equal? (hash-ref (read-disposition-claim! claim-path) 'lineage)
                     (hash-ref task 'disposition))
       ;; The retained-receipt layer is genuinely required, not skipped.
       (expect-refusal #rx"missing-owned-proof"
                       (lambda ()
                         (validate-preserved-candidate! (fixture-root (hash-ref fx 'f))
                                                        (hash-ref (hash-ref fx 'ctx) 'plan-id)
                                                        1
                                                        task))))))
  (test-case "malformed metadata, wrong K/M, dirty source and revocation refuse"
    (with-preserved-fixture
     (lambda (fx)
       (define repo (hash-ref fx 'repo))
       (define binding (make-binding fx))
       (define owner-entries* (hash-ref binding 'owner-paths))
       (define (refuse-binding
                b
                (pattern #rx"preserved|owner|manifest|candidate|source|descendant|tree"))
         (expect-refusal pattern
                         (lambda ()
                           (validate-preserved-candidate-claim-binding! (task-with-binding fx b)))))
       ;; unknown binding field / schema / extra path / digest tamper
       (refuse-binding (hash-set binding 'extra #t))
       (refuse-binding (hash-set binding 'schema "supplemental-preserved-candidate/v0"))
       (refuse-binding (hash-set binding
                                 'owner-paths
                                 (append owner-entries*
                                         (list (hasheq 'path
                                                       test-path
                                                       'before-blob
                                                       (hash-ref (car owner-entries*) 'before-blob)
                                                       'before-sha256
                                                       (hash-ref (car owner-entries*) 'before-sha256)
                                                       'after-blob
                                                       (hash-ref (car owner-entries*) 'before-blob)
                                                       'after-sha256
                                                       (hash-ref (car owner-entries*)
                                                                 'before-sha256))))))
       (refuse-binding (hash-set binding 'owner-paths (cdr owner-entries*)))
       (refuse-binding (hash-set binding
                                 'owner-paths
                                 (cons (hash-set (car owner-entries*) 'after-sha256 (hex 64 #\0))
                                       (cdr owner-entries*))))
       (refuse-binding
        (hash-set binding
                  'original-source
                  (hash-set (hash-ref binding 'original-source) 'head (hash-ref fx 'm-head)))
        #rx"source|candidate|context")
       ;; M2 has M as its parent, not original K.
       (define m2
         (run-git repo
                  "commit-tree"
                  (string-append (hash-ref fx 'm-head) "^{tree}")
                  "-p"
                  (hash-ref fx 'm-head)
                  "-m"
                  "extra"))
       (refuse-binding
        (hash-set binding 'descendant-source (hasheq 'head m2 'tree (hash-ref fx 'm-tree)))
        #rx"single-parent|current source")
       ;; A non-owner path changed on a K-child commit refuses even when the
       ;; checkout is exactly that descendant.
       (run-git repo "checkout" "-q" "-b" "campaign/bad" (hash-ref fx 'k-head))
       (put repo model-source-path "#lang racket/base\n(provide answer)\n(define answer 41)\n")
       (run-git repo "add" "-A")
       (run-git repo "commit" "-qm" "bad non-owner change")
       (define bad-head (run-git repo "rev-parse" "HEAD"))
       (define bad-tree (run-git repo "rev-parse" "HEAD^{tree}"))
       (define bad-binding
         (hash-set binding 'descendant-source (hasheq 'head bad-head 'tree bad-tree)))
       (run-git repo "checkout" "-q" "-B" branch bad-head)
       (refuse-binding bad-binding #rx"owner-path|diff")
       (run-git repo "checkout" "-q" "-B" branch (hash-ref fx 'm-head))
       ;; Dirty/untracked current source refuses.
       (put repo "scratch-untracked.txt" "dirty\n")
       (refuse-binding binding #rx"dirty|untracked|current source")
       (delete-file (build-path repo "scratch-untracked.txt"))
       ;; Narrow malformed/missing metadata and path negatives.
       (refuse-binding (hash-remove binding 'owner-paths) #rx"preserved")
       (refuse-binding
        (hash-set binding 'original-source (hash-remove (hash-ref binding 'original-source) 'origin))
        #rx"preserved")
       (refuse-binding (hash-set binding
                                 'descendant-source
                                 (hash-remove (hash-ref binding 'descendant-source) 'tree))
                       #rx"preserved")
       (refuse-binding (hash-set binding
                                 'owner-paths
                                 (cons (hash-set (car owner-entries*) 'path "../escape")
                                       (cdr owner-entries*)))
                       #rx"preserved")
       (refuse-binding
        (hash-set binding
                  'owner-paths
                  (cons (hash-set (car owner-entries*) 'path (string-append "/" "absolute/path"))
                        (cdr owner-entries*)))
        #rx"preserved")
       ;; Missing manifest/owner path (structural path set) refuses.
       (refuse-binding (hash-set binding 'owner-paths (cdr owner-entries*))
                       #rx"owner|candidate|manifest")
       ;; A successful validation is never cached: the same binding must be
       ;; re-authenticated (and refuse) after the live approval is revoked.
       (check-true (preserved-candidate-admission? (validate-preserved-candidate-claim-binding!
                                                    (task-with-binding fx binding))))
       (revoke-approval! (hash-ref fx 'capref))
       (refuse-binding binding #rx"approval"))))
  (test-case "git facts are indexed once per tree and hashed once per unique blob per leaf invocation"
    ;; Deterministic count/equivalence regression (W3): one leaf admission
    ;; must enumerate each distinct old/K/M tree exactly once and hash each
    ;; unique raw blob exactly once, and the preserved claim owner (install +
    ;; final revalidation + explicit revalidation) must run exactly the leaf
    ;; validations with no extra Git work of its own, reusing the leaf's
    ;; self-authenticated ancestry instead of a second inspection. Exact
    ;; process counts; admission facts must stay identical.
    (define counts
      (call-counting-git
       (lambda (git-calls)
         (with-preserved-fixture
          (lambda (fx)
            ;; Fixture/binding construction itself runs git: snapshot the
            ;; counter first so both windows measure validation work only.
            (define binding (make-binding fx))
            (define task (task-with-binding fx binding))
            (define admission-window-start (git-calls))
            (define admission (validate-preserved-candidate-claim-binding! task))
            (check-true (preserved-candidate-admission? admission))
            (check-equal? (preserved-candidate-admission-parent-proposal admission)
                          (hash-ref (hash-ref fx 'parent-pointer) 'task-proposal))
            (check-equal? (preserved-candidate-admission-parent-task admission)
                          (hash-ref fx 'parent-task))
            (check-equal? (hash-ref (hash-ref (preserved-candidate-admission-terminal admission)
                                              'facts)
                                    'candidate-ref)
                          (hash-ref fx 'kref))
            (define after-leaf (git-calls))
            (define leaf-count (- after-leaf admission-window-start))
            (define p
              (propose-retain! "task-proposal"
                               (hasheq 'task task 'expires-at (+ (current-seconds) 1700))))
            (define g (approve! p))
            (install-supplemental-disposition-claim! (hash-ref fx 'path)
                                                     (hash-ref fx 'parent-pointer)
                                                     (pointer p g)
                                                     p
                                                     g)
            (validate-disposition-claim! g p task)
            (hasheq 'leaf leaf-count 'claim (- (git-calls) after-leaf)))))))
    (check-equal? (hash-ref counts 'leaf) 33)
    (check-equal? (hash-ref counts 'claim) 99))
  (test-case "module graph loads from a cold (no compiled cache) source"
    ;; The leaf's claim/readiness lazy loads must not create a static cycle.
    (define module-path (path->string candidate-module))
    (define code
      (format
       (string-append
        "(parameterize ([use-compiled-file-paths '()])"
        " (unless (and (procedure? (dynamic-require (string->path ~s) 'validate-preserved-candidate!))"
        " (procedure? (dynamic-require (string->path ~s) 'valid-preserved-candidate?)))"
        " (exit 1)))")
       module-path
       module-path))
    (check-equal? 0 (system*/exit-code (find-executable-path "racket") "-e" code))))
