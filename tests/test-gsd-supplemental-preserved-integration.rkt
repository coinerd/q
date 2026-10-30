#lang racket
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 3600
;; @covers extensions/gsd/supplemental-preserved-candidate.rkt
;; @covers extensions/gsd/supplemental-evidence-disposition.rkt
;; @covers extensions/gsd/supplemental-evidence-controller.rkt
;; @covers extensions/gsd/author-checkpoint.rkt
;; INTEGRATION lane for candidate-preserving recovery.
;;
;; A genuine temp campaign is built by the existing supplemental-go fixture:
;; real Git history, a real frozen Verify, a REAL owned C0 receipt, a real
;; private 0700 authority store and the real interactive broker. The spent
;; third-generation journal (root -> v2 recovery -> amendment-1 -> amendment-2
;; -> late amendment-3 terminal) is driven through the REAL journal
;; transitions and the real broker approvals. The metadata descendant M is a
;; real single-parent commit that changes exactly the inherited owner paths.
;;
;; Proved here: inert proposal; exact T approval/install with ONE claim; the
;; controller entry/source gate accepts only the authenticated E_old + K->M
;; pair; /go adopts M with ZERO model dispatch callbacks, consumes the fresh
;; permission once (launch-consumed), seals fresh K' for the new
;; allocation/source M and then waits at the human K' approval. BEYOND that
;; wait, the SAME chain resumes through the normal campaign entry with one
;; isolated genuine broker decision per owner gate: actual C1/G1 on M, the
;; staged wrapper, the real baseline-overlay R1, the typed S authorization
;; wait, then S approval and the protected publication/promotion to a
;; registered full delivery — zero model dispatch, with the immutable E_old
;; receipt context (attempt-2/fence-5) distinct from the parked parent
;; allocation (attempt-6/fence-9) and the fresh allocation (attempt-7/fence-10)
;; at every step. Tamper, source drift, cancellation, budget pause, fence
;; loss, an old-candidate revocation and a crash cut after launch all fail
;; closed with no ordinary fallback.
(require rackunit
         json
         racket/file
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         "helpers/supplemental-go-fixture.rkt"
         (only-in "test-gsd-supplemental-evidence-journal.rkt" revoke-approval!)
         "../runtime/approval/broker.rkt"
         (only-in "../extensions/gsd/supplemental-evidence-operator.rkt"
                  current-supplemental-operator-publisher)
         (only-in "../extensions/gsd/delivery-publication.rkt"
                  current-gsd-approved-head-publisher
                  publish-approved-head!)
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/delivery-receipt.rkt"
         (only-in "../extensions/gsd/delivery-journal.rkt" load-delivery-journal save!)
         (only-in "../extensions/gsd/delivery-verifier.rkt"
                  check-verify-command
                  current-gsd-delivery-branch-context
                  make-branch-delivery-context)
         (only-in "../extensions/gsd/review-proof.rkt"
                  call-with-owned-proof-capture
                  owned-proof-capture-reference)
         "../extensions/gsd/evidence-readiness.rkt"
         "../extensions/gsd/evidence-repair-format.rkt"
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         (submod "../extensions/gsd/supplemental-evidence-authority.rkt" owner-frontend)
         "../extensions/gsd/supplemental-evidence-format.rkt"
         "../extensions/gsd/supplemental-evidence-journal.rkt"
         "../extensions/gsd/supplemental-disposition-policy.rkt"
         (submod "../extensions/gsd/supplemental-disposition-policy.rkt" test-support)
         (submod "../extensions/gsd/supplemental-disposition-policy.rkt" ancestry-support)
         "../extensions/gsd/supplemental-disposition-claim.rkt"
         "../extensions/gsd/supplemental-evidence-controller.rkt"
         (only-in "../extensions/gsd/supplemental-preserved-candidate.rkt"
                  validate-preserved-candidate!)
         (only-in "../extensions/gsd/wave-runner-port.rkt" wave-execution-outcome-kind)
         (only-in "../extensions/gsd/campaign-result.rkt"
                  campaign-result-status
                  campaign-result-message)
         (only-in "../extensions/gsd/go-orchestrator.rkt" run-campaign!)
         (only-in "../extensions/gsd/author-checkpoint.rkt" resolve-supplemental-author-checkpoint!)
         (only-in "helpers/honest-delivery-fixture.rkt" test-delivered-reader)
         (only-in "../util/json/checksum.rkt" sha256-string))

(define branch "campaign/w1")
(define-runtime-path cli-module "../scripts/gsd-evidence-replan.rkt")
(define (hex n ch)
  (string->immutable-string (make-string n ch)))
(define (git-run repo raw? . args)
  (define out (open-output-string))
  (define err (open-output-string))
  (define code
    (parameterize ([current-output-port out]
                   [current-error-port err])
      (apply system*/exit-code (find-executable-path "git") "-C" (path->string repo) args)))
  (unless (zero? code)
    (error 'preserved-integration "git ~s failed: ~a" args (get-output-string err)))
  (if raw?
      (get-output-string out)
      (string-trim (get-output-string out))))
(define (git-text repo . args)
  (apply git-run repo #f args))
(define (git-raw repo . args)
  (apply git-run repo #t args))
(define (put repo relative text)
  (define p (build-path repo relative))
  (make-parent-directory* p)
  (display-to-file text p #:exists 'truncate))
(define (blob-sha repo oid)
  (sha256-string (git-raw repo "cat-file" "blob" oid)))
(define (candidate-record-files)
  ;; Retained proposals live flat in the private authority root, keyed by
  ;; digest. Read-only inspection for "did any fresh K' appear?".
  (define root (current-supplemental-authority-root))
  (for/list ([name (in-list (directory-list root))]
             #:when (and (regexp-match? #rx"[.]json$" (path->string name))
                         (file-exists? (build-path root name))))
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (call-with-input-file (build-path root name) read-json))))
(define (fresh-candidates-for task-proposal)
  (for/list ([record (in-list (candidate-record-files))]
             #:when (and (hash? record)
                         (equal? (hash-ref record 'kind #f) "candidate-proposal")
                         (equal? (hash-ref (hash-ref record 'payload (hasheq)) 'task-proposal #f)
                                 task-proposal)))
    record))
;; Deterministic Git-call counting seam (W3b): a PATH shim executable appends
;; one byte per real `git` invocation and then execs the genuine git, so every
;; counted call keeps its exact behavior and refusal semantics. Counts are
;; process counts, never wall time, and the shim lives only inside the call.
(define (call-counting-git proc)
  (define real-git (find-executable-path "git"))
  (define shim-dir (make-temporary-file "entry-view-git-shim-~a" 'directory))
  (define counter (make-temporary-file "entry-view-git-count-~a"))
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
                    (proc (lambda ()
                            (if (file-exists? counter)
                                (file-size counter)
                                0)))))
                (lambda ()
                  (delete-directory/files shim-dir)
                  (when (file-exists? counter)
                    (delete-file counter)))))

;; ---------------------------------------------------------------------------
;; Chain: genuine spent third-generation journal ancestry
;; ---------------------------------------------------------------------------
(define (allocation id fence)
  (hasheq 'attempt-id id 'attempt-fence fence 'coordinator-fence fence))
(define (base-binding ctx fence)
  (hasheq 'context ctx 'coordinator-fence fence))
(define (step! grant from to facts)
  (define operation (load-supplemental-operation grant))
  (transition-supplemental-operation! grant (hash-ref operation 'revision) from to facts))
(define (spend! grant base alloc)
  (start-supplemental-operation! grant base)
  (step! grant "proposal-approved" "reserved" (hasheq 'allocation alloc))
  (step! grant "reserved" "launched" (hasheq 'dispatch-id "fixture-spent-launch"))
  (step! grant "launched" "blocked" (hasheq 'block-reason "fixture spent"))
  (load-supplemental-operation grant))
(define (manifest-entry repo head path)
  (define oid (git-text repo "rev-parse" (format "~a:~a" head path)))
  (hasheq 'path path 'mode "100644" 'blob oid 'sha256 (blob-sha repo oid)))
(define (expected-manifest repo head paths)
  (for/list ([path (in-list paths)])
    (manifest-entry repo head path)))
(define (spend-late! grant base repo ctx source alloc parent-proposal parent-approval)
  (start-supplemental-operation! grant base)
  (step! grant "proposal-approved" "reserved" (hasheq 'allocation alloc))
  (step! grant "reserved" "launched" (hasheq 'dispatch-id "supplemental-author-attempt-6"))
  (define retained (load-supplemental-operation grant))
  (define task
    (hash-ref (hash-ref (load-supplemental-record (hash-ref retained 'proposal-ref)) 'payload) 'task))
  (define paths
    (append (list (hash-ref task 'test-path) (hash-ref task 'harness-path))
            (hash-ref task 'owner-paths)))
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
                                      source
                                      'attempt-id
                                      (hash-ref alloc 'attempt-id)
                                      'attempt-fence
                                      (hash-ref alloc 'attempt-fence)
                                      'execution-cwd
                                      (hash-ref ctx 'repo)
                                      'manifest
                                      (expected-manifest repo (hash-ref source 'head) paths))))
  (define kref
    (propose-retain! "candidate-proposal" candidate (list parent-proposal parent-approval)))
  (define capref (fixture-approve! kref))
  (step! grant "launched" "candidate-sealed" (hasheq 'candidate-ref kref))
  (step! grant "candidate-sealed" "candidate-approved" (hasheq 'candidate-approval-ref capref))
  (step! grant
         "candidate-approved"
         "verifying"
         (hasheq 'verify-job (hasheq 'id "fixture-verify-job" 'owner "fixture-controller")))
  (step! grant "verifying" "blocked" (hasheq 'block-reason "operator deadline expired"))
  (values (load-supplemental-operation grant) kref capref))
(define (child! parent old amendment)
  (define descriptor (disposition-descriptor amendment old))
  (define task
    (seal-supplemental-record
     (hash-set* parent 'kind "supplemental-evidence-task/v3" 'disposition descriptor)))
  (define p
    (propose-retain! "task-proposal" (hasheq 'task task 'expires-at (+ (current-seconds) 1700))))
  (define g (fixture-approve! p))
  (values p g task (disposition-pointer p g)))

(define (owner-entries repo k-head m-head)
  (define paths (string-split (git-text repo "diff" "--name-only" k-head m-head) "\n"))
  (for/list ([path (in-list paths)]
             #:unless (equal? path ""))
    (define before (git-text repo "rev-parse" (format "~a:~a" k-head path)))
    (define after (git-text repo "rev-parse" (format "~a:~a" m-head path)))
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
(define (commit-metadata! fx k)
  ;; M is a REAL single-parent descendant of K that changes exactly the
  ;; inherited owner paths and nothing else.
  (define repo (supplemental-fixture-repo fx))
  (for ([path (in-list (supplemental-fixture-owner-paths fx))])
    (put repo path (format "generated owner metadata for ~a\n" path)))
  (git-text repo "add" "-A")
  (git-text repo "commit" "-q" "-m" "metadata settlement M")
  (define m-head (git-text repo "rev-parse" "HEAD"))
  (define m-tree (git-text repo "rev-parse" "HEAD^{tree}"))
  (define changed (owner-entries repo (hash-ref k 'head) m-head))
  (unless (and (= (length changed) (length (supplemental-fixture-owner-paths fx)))
               (for/and ([entry (in-list changed)])
                 (member (hash-ref entry 'path) (supplemental-fixture-owner-paths fx))))
    (error 'preserved-integration "metadata descendant changed non-owner paths"))
  (hasheq 'head m-head 'tree m-tree))

;; ---------------------------------------------------------------------------
;; Candidate source K: the fixture's pre-author head does not yet contain the
;; approved test/harness paths. Capture a REAL owned receipt BEFORE the author
;; commit, then retain E_old unchanged while K adds exactly those two paths.
;; K must be a strict descendant of E_old, as in the live campaign.
;; The retained E_old receipt is attempt-2/fence-5 exactly like the live report,
;; so it is GENUINELY DISTINCT from the parked parent allocation
;; attempt-6/fence-9 (a fixture alias would let the regression pass by accident).
;; ---------------------------------------------------------------------------
(define (prepare-candidate-source! fx)
  (define root (supplemental-fixture-root fx))
  (define plan (supplemental-fixture-plan fx))
  (define repo (supplemental-fixture-repo fx))
  (define wave (hash-ref (supplemental-fixture-task-context fx) 'wave))
  (define pointer (load-supplemental-pointer root plan wave))
  (define old-task
    (hash-ref (hash-ref (load-supplemental-record (hash-ref pointer 'task-proposal)) 'payload) 'task))
  (define before (committed-delivery-snapshot repo))
  (define-values (_result capture)
    (parameterize ([current-gsd-delivery-branch-context
                    (make-branch-delivery-context #:repo-root repo
                                                  #:branch branch
                                                  #:base-commit (hash-ref before 'head)
                                                  #:worktree-path repo)])
      (call-with-owned-proof-capture root
                                     plan
                                     wave
                                     before
                                     "attempt-2"
                                     5
                                     (lambda () (check-verify-command root wave #f))
                                     #:cwd repo)))
  (unless capture
    (error 'preserved-integration "real candidate-source C0 capture failed"))
  (define receipt
    (hash-set* before
               'attempt-id
               "attempt-2"
               'attempt-fence
               5
               'verified-at
               (current-seconds)
               'evidence
               "actual owned C0 fixture GREEN for the original candidate source"
               'owned-proof
               (owned-proof-capture-reference capture)))
  (define journal (load-delivery-journal root plan wave))
  (define previous (hash-ref journal 'receipt))
  (save! root
         plan
         wave
         (hash-set* journal
                    'receipt
                    receipt
                    'receipt-history
                    (append (hash-ref journal 'receipt-history '()) (list previous))))
  (define ctx
    (authenticated-evidence-receipt-context (load-authenticated-evidence-receipt root plan wave)))
  (unless (equal? (hash-ref ctx 'head) (hash-ref before 'head))
    (error 'preserved-integration "derived old receipt context head mismatch"))
  ;; Retain the genuine PRIOR publication of the immutable E_old receipt
  ;; through the ordinary production API; only the remote effect is isolated
  ;; (the disposition-fixture precedent). The protected supplemental
  ;; supersession publication later requires exactly this retained confirmed
  ;; predecessor record, matched on the immutable receipt binding. The E_old
  ;; receipt is pinned to the historical attempt-2/fence-5 that captured it,
  ;; so publication authorization re-binds the wave record to exactly that
  ;; attempt for the ordinary publish and restores the parked predecessor
  ;; attempt afterwards; fences and every other record field stay untouched.
  (define restore-attempt
    (campaign-wave-current-attempt (wave-of (load-campaign-record root plan) wave)))
  (define (install-attempt! attempt)
    (mutate-campaign! root
                      plan
                      (lambda (rec) (set-campaign-wave-current-attempt! (wave-of rec wave) attempt))))
  (install-attempt! (campaign-attempt "attempt-2" 5 0))
  (parameterize ([current-gsd-approved-head-publisher
                  (lambda (_base _plan _wave source _attempt _fence _coord _evidence)
                    (hasheq 'status
                            "published"
                            'branch
                            (hash-ref source 'branch)
                            'head
                            (hash-ref source 'head)))])
    (publish-approved-head! root
                            plan
                            wave
                            (for/hasheq ([key '(repo origin branch head tree)])
                              (values key (hash-ref receipt key)))
                            (hash-ref receipt 'attempt-id)
                            (hash-ref receipt 'attempt-fence)
                            #f
                            (hash-ref receipt 'evidence)
                            (lambda () (load-campaign-record root plan))
                            #:owned-proof (hash-ref receipt 'owned-proof)))
  (install-attempt! restore-attempt)
  (fixture-author-commit! repo)
  (git-text repo "add" (supplemental-fixture-test-path fx) (supplemental-fixture-harness-path fx))
  (git-text repo "commit" "-q" "-m" "old candidate test evidence")
  (define candidate-source (committed-delivery-snapshot repo))
  (check-not-equal? (hash-ref candidate-source 'head) (hash-ref ctx 'head))
  (define root-task (seal-supplemental-record (hash-set old-task 'context ctx)))
  (define proposal
    (propose-retain! "task-proposal" (hasheq 'task root-task 'expires-at (+ (current-seconds) 1800))))
  (define grant (fixture-approve! proposal))
  ;; Public INERT pointer bytes for the root task (fixture setup only; the
  ;; production write path refuses to replace a different campaign proposal).
  (display-to-file
   (canonical-evidence-json (disposition-pointer proposal grant))
   (build-path root ".planning" "supplemental-evidence" (format "~a-w~a.json" plan wave))
   #:exists 'truncate)
  (hasheq 'ctx
          ctx
          'source
          (hasheq 'repo
                  (hash-ref ctx 'repo)
                  'branch
                  (hash-ref ctx 'branch)
                  'origin
                  (hash-ref ctx 'origin)
                  'head
                  (hash-ref candidate-source 'head)
                  'tree
                  (hash-ref candidate-source 'tree))
          'root-proposal
          proposal
          'root-grant
          grant))

(define (chain! fx proc)
  (define root (supplemental-fixture-root fx))
  (define plan (supplemental-fixture-plan fx))
  (define repo (supplemental-fixture-repo fx))
  (define prepared (prepare-candidate-source! fx))
  (define ctx (hash-ref prepared 'ctx))
  (define wave (hash-ref ctx 'wave))
  (define source (hash-ref prepared 'source))
  (define pointer0 (load-supplemental-pointer root plan wave))
  (define root-proposal (hash-ref pointer0 'task-proposal))
  (define root-grant (hash-ref pointer0 'task-approval))
  (define a1-alloc (allocation "attempt-4" 7))
  (define a2-alloc (allocation "attempt-5" 8))
  (define a3-alloc (allocation "attempt-6" 9))
  (spend! root-grant (base-binding ctx 7) a1-alloc)
  (define-values (rp _rt ro) (inspect-supplemental-recovery-parent root-grant))
  (define path (build-path root ".planning" "supplemental-evidence" (format "~a-w~a.json" plan wave)))
  (define old (disposition-pointer rp root-grant))
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
            a1-alloc))
  ;; The v2 parent task inherits the authentic v1 task exactly.
  (define parent-task (hash-ref (hash-ref (load-supplemental-record root-proposal) 'payload) 'task))
  (define parent
    (seal-supplemental-record
     (hash-set* parent-task 'kind "supplemental-evidence-task/v2" 'recovery recovery)))
  (define pp
    (propose-retain! "task-proposal" (hasheq 'task parent 'expires-at (+ (current-seconds) 1700))))
  (define pg (fixture-approve! pp))
  (define parent-pointer (disposition-pointer pp pg))
  (install-supplemental-recovery-claim! path old parent-pointer pp pg)
  (define po (spend! pg (base-binding ctx 7) a2-alloc))
  (define amendment-1
    (seal-supplemental-record (hash-set* disposition-amendment-1
                                         'plan-id
                                         plan
                                         'wave
                                         wave
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
                                         a2-alloc)))
  (call-with-test-disposition-amendment
   amendment-1
   (lambda ()
     (define-values (p1 g1 t1 target1) (child! parent parent-pointer amendment-1))
     (install-supplemental-disposition-claim! path parent-pointer target1 p1 g1)
     (define t1-terminal (spend! g1 (base-binding ctx 7) a3-alloc))
     (define amendment-2
       (seal-supplemental-record (hash-set* amendment-1
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
                                            a3-alloc)))
     (call-with-test-disposition-amendment
      amendment-2
      (lambda ()
        (define-values (p2 g2 t2 target2) (child! t1 target1 amendment-2))
        (install-supplemental-disposition-claim! path target1 target2 p2 g2)
        (define-values (late-terminal kref capref)
          (spend-late! g2 (base-binding ctx 7) repo ctx source a3-alloc p2 g2))
        (define amendment-3
          (seal-supplemental-record (hash-set* amendment-2
                                               'prior-parent-proposal
                                               (hash-ref amendment-2 'parent-proposal)
                                               'prior-parent-approval
                                               (hash-ref amendment-2 'parent-approval)
                                               'parent-proposal
                                               p2
                                               'parent-approval
                                               g2
                                               'terminal-body-sha256
                                               (hash-ref late-terminal 'sha256)
                                               'terminal-record-sha256
                                               (sha256-string (canonical-evidence-json late-terminal))
                                               'allocation
                                               a3-alloc)))
        (call-with-test-disposition-amendment
         amendment-3
         (lambda ()
           (define m (commit-metadata! fx source))
           (define binding
             (hasheq 'schema
                     "supplemental-preserved-candidate/v1"
                     'candidate
                     kref
                     'candidate-approval
                     capref
                     'original-source
                     source
                     'descendant-source
                     m
                     'owner-paths
                     (owner-entries repo (hash-ref source 'head) (hash-ref m 'head))))
           (proc (hasheq 'fx
                         fx
                         'root
                         root
                         'repo
                         repo
                         'plan
                         plan
                         'wave
                         wave
                         'ctx
                         ctx
                         'source
                         source
                         'm
                         m
                         'binding
                         binding
                         'parent-task
                         t2
                         'parent-pointer
                         target2
                         'amendment
                         amendment-3
                         'kref
                         kref
                         'capref
                         capref)))
         #:predecessor amendment-2))
      #:predecessor amendment-1))))

(define (with-chain proc)
  (call-with-supplemental-fixture (lambda (fx) (chain! fx proc)) #:wave-index 1))

;; ---------------------------------------------------------------------------
;; Campaign record retargeting (the amendment-3 allocation IS the live attempt
;; the controller must see at entry/adoption).
;; ---------------------------------------------------------------------------
(define (wave-of rec wave)
  (for/first ([w (in-list (campaign-record-waves rec))]
              #:when (= wave (campaign-wave-index w)))
    w))
(define (retarget-campaign! root
                            plan
                            wave
                            attempt-id
                            fence
                            #:status [status 'awaiting-delivery]
                            #:attempt-count [count #f])
  (define rec (load-campaign-record root plan))
  (define w (wave-of rec wave))
  (when count
    (set-campaign-wave-attempt-count! w count))
  (set-campaign-wave-status! w status)
  (set-campaign-wave-current-attempt! w (campaign-attempt attempt-id fence 0))
  (set-campaign-fence-token! rec fence)
  (set-campaign-cancellation! rec #f)
  (set-campaign-record-budget-pause! rec #f)
  (persist-campaign! root rec))
(define (mutate-campaign! root plan thunk)
  (define rec (load-campaign-record root plan))
  (thunk rec)
  (persist-campaign! root rec))
(define (clear-campaign! root plan attempt-id fence)
  (retarget-campaign! root plan 1 attempt-id fence))

(define (propose-preserved! base plan wave binding)
  (define pointer (load-supplemental-pointer base plan wave))
  (propose-supplemental-disposition! base
                                     plan
                                     wave
                                     (hash-ref (hash-ref pointer 'task-proposal) 'sha256)
                                     (+ (current-seconds) 1700)
                                     #:preserved-candidate binding))
(define (claim-path-for amendment)
  (build-path (current-supplemental-authority-root)
              (string-append "supplemental-disposition-" (hash-ref amendment 'sha256) ".json")))

(define (install-preserved! chain)
  (define root (hash-ref chain 'root))
  (define plan (hash-ref chain 'plan))
  (define wave (hash-ref chain 'wave))
  (define-values (p task) (propose-preserved! root plan wave (hash-ref chain 'binding)))
  (define g (fixture-approve! p))
  (install-supplemental-disposition! root plan wave p g)
  (hasheq 'proposal p 'approval g 'task task))

(define (attempt-adoption! chain)
  (define root (hash-ref chain 'root))
  (define repo (hash-ref chain 'repo))
  (define model-calls (box 0))
  (define outcome
    (parameterize ([current-directory repo])
      (run-supplemental-author-phases! root
                                       (hash-ref chain 'plan)
                                       (hash-ref chain 'wave)
                                       "attempt-7"
                                       11
                                       (lambda ()
                                         (set-box! model-calls (add1 (unbox model-calls)))
                                         'unexpected-model-author-response)
                                       #:guard (lambda () #t))))
  (values outcome (unbox model-calls)))
(define (prepare-go! chain)
  (retarget-campaign! (hash-ref chain 'root)
                      (hash-ref chain 'plan)
                      (hash-ref chain 'wave)
                      "attempt-7"
                      11
                      #:status 'failed))

(define (expect-refusal pattern thunk)
  (check-exn pattern thunk))

;; A parked checkpoint resolution refusal must be a pure denial: the wave stays
;; in the status the guard observed, no operation is initialized and no fresh
;; candidate appears.
(define (expect-stale-parked-refusal! root plan wave expected-status approval)
  (check-equal? (resolve-supplemental-author-checkpoint! root plan wave)
                "author-checkpoint: stale, cancelled or paused supplemental checkpoint")
  (define w (wave-of (load-campaign-record root plan) wave))
  (check-eq? (campaign-wave-status w) expected-status)
  (check-false (load-supplemental-operation approval)))

;; ---------------------------------------------------------------------------
;; Production /go entry from the parked parent (also usable as a standalone
;; focused case): the real resolver proves the fixed-a3
;; allocation attempt-6/fence-9, the orchestrator allocates a fresh monotonic
;; attempt-7/fence-10, the zero-model adoption seals fresh K' from M, and /go
;; stops at the HUMAN K' wait. The source runner must never run and the
;; immutable E_old receipt context stays attempt-2/fence-5.
;; ---------------------------------------------------------------------------
(define (full-campaign-case! chain #:installed [installed #f])
  (define root (hash-ref chain 'root))
  (define plan (hash-ref chain 'plan))
  (define wave (hash-ref chain 'wave))
  (define repo (hash-ref chain 'repo))
  (define ctx (hash-ref chain 'ctx))
  (define m (hash-ref chain 'm))
  (define installed*
    (or installed
        (begin
          (retarget-campaign! root plan wave "attempt-6" 9)
          (install-preserved! chain))))
  (define p (hash-ref installed* 'proposal))
  (define g (hash-ref installed* 'approval))
  (define task (hash-ref installed* 'task))
  (check-equal? (hash-ref ctx 'attempt-id) "attempt-2")
  (check-equal? (hash-ref ctx 'attempt-fence) 5)
  (retarget-campaign! root plan wave "attempt-6" 9 #:attempt-count 6)
  (define model-calls (box 0))
  (define channel (current-approval-channel))
  (define result
    (dynamic-wind void
                  (lambda ()
                    (set-approval-channel! #f)
                    (parameterize ([current-directory repo])
                      (run-campaign! root
                                     (load-campaign-record root plan)
                                     #:isolate? #f
                                     ;; The preserved entry re-authenticates the fixed-a3
                                     ;; terminal at every journal transition, so the owned
                                     ;; phase needs a realistic budget (never 120s).
                                     #:timeout-sec 600
                                     #:delivery-reader (test-delivered-reader)
                                     #:runner (lambda (_)
                                                (set-box! model-calls (add1 (unbox model-calls)))
                                                'unexpected-model-author-response)
                                     #:delivery-coordinator
                                     (lambda (_plan _idx)
                                       (error 'preserved-integration
                                              "ordinary coordinator forbidden"))
                                     #:verifier (lambda (_) #f))))
                  (lambda ()
                    (when channel
                      (set-approval-channel! channel)))))
  (check-eq? (campaign-result-status result) 'wave-blocked)
  (check-regexp-match #rx"operator candidate approval unavailable" (campaign-result-message result))
  (check-equal? (unbox model-calls) 0 "no model author callback may run")
  (define operation (load-supplemental-operation g))
  (check-equal? (hash-ref operation 'state) "candidate-sealed")
  (check-true (hash-ref operation 'launch-consumed))
  (define facts (hash-ref operation 'facts))
  (check-equal? (hash-ref facts 'dispatch-id) "adoption:attempt-7")
  (check-equal? (hash-ref facts 'dispatch-kind) "preserved-candidate-adoption")
  (check-equal? (hash-ref facts 'allocation) (allocation "attempt-7" 10))
  (define after (load-campaign-record root plan))
  (define parked (wave-of after wave))
  (check-eq? (campaign-wave-status parked) 'awaiting-delivery)
  (check-equal? (campaign-attempt-id (campaign-wave-current-attempt parked)) "attempt-7")
  (check-equal? (campaign-attempt-fence-token (campaign-wave-current-attempt parked)) 10)
  (check-equal? (campaign-fence-token after) 10)
  (define fresh-ref (hash-ref facts 'candidate-ref))
  (check-not-equal? (hash-ref fresh-ref 'sha256) (hash-ref (hash-ref chain 'kref) 'sha256))
  (define fresh (hash-ref (load-supplemental-record fresh-ref) 'payload))
  (check-equal? (hash-ref fresh 'task-proposal) p)
  (check-equal? (hash-ref fresh 'task-approval) g)
  (check-equal? (hash-ref fresh 'task-sha256) (hash-ref task 'sha256))
  (check-equal? (hash-ref fresh 'context) ctx)
  (check-equal? (hash-ref fresh 'source) (committed-delivery-snapshot repo))
  (check-equal? (hash-ref (hash-ref fresh 'source) 'head) (hash-ref m 'head))
  (check-equal? (hash-ref (hash-ref fresh 'source) 'tree) (hash-ref m 'tree))
  (check-equal? (hash-ref fresh 'execution-cwd) (hash-ref ctx 'repo))
  (check-equal? (map (lambda (e) (hash-ref e 'path)) (hash-ref fresh 'manifest))
                (append (list (supplemental-fixture-test-path (hash-ref chain 'fx))
                              (supplemental-fixture-harness-path (hash-ref chain 'fx)))
                        (supplemental-fixture-owner-paths (hash-ref chain 'fx))))
  ;; Fresh K' is NOT approved: only its own future broker decision can do that.
  (check-false (hash-ref facts 'candidate-approval-ref #f))
  ;; The stop is the HUMAN K' wait with zero model callbacks, and exactly one
  ;; fresh K' exists for the installed T.
  (check-equal? (unbox model-calls) 0)
  (check-equal? (hash-ref (load-supplemental-operation g) 'state) "candidate-sealed")
  (check-false
   (hash-ref (hash-ref (load-supplemental-operation g) 'facts) 'candidate-approval-ref #f))
  (check-equal? (length (fresh-candidates-for p)) 1)
  ;; ------------------------------------------------------------------
  ;; BEYOND the K' wait (same chain, same installed T — no second fixture):
  ;; the campaign resumes through the NORMAL run-campaign! entry, and every
  ;; owner gate is ONE isolated genuine broker decision made by the trusted
  ;; test frontend through the REAL active channel (register → decide →
  ;; await, exactly like any operator approval). Everything between the
  ;; decisions is the real protected ladder — actual C1/G1 on the descendant
  ;; M, the staged r*/E wrapper, the real baseline-overlay R1 probe, the
  ;; typed S authorization wait, then S approval and the protected
  ;; publication/promotion to a registered delivery. The model-author
  ;; callback and the ordinary coordinator stay forbidden; no prior GREEN,
  ;; approval or admission is ever reused; the immutable identities hold:
  ;; E_old receipt context attempt-2/fence-5, parked parent
  ;; attempt-6/fence-9, fresh allocation attempt-7/fence-10.
  ;; ------------------------------------------------------------------
  (define decisions (box '()))
  (define (genuine-decider! kind)
    ;; Test-support only: decide the exact registered digest for ONE proposal
    ;; kind; any other owner request stays genuinely undecided (typed wait).
    (lambda (event payload)
      (when (equal? event "supplemental-evidence.approval-requested")
        (define requested (hash-ref payload 'proposal-kind))
        (set-box! decisions (append (unbox decisions) (list requested)))
        (when (equal? kind requested)
          (unless (approval-decide! (hash-ref payload 'request-id)
                                    (hash-ref payload 'commitment-digest)
                                    #t)
            (error 'preserved-integration "genuine broker decision failed"))))))
  (define (resume-run! kind #:publish? [publish? #f])
    (parameterize ([current-supplemental-operator-publisher (genuine-decider! kind)]
                   [current-gsd-approved-head-publisher
                    (if publish?
                        (lambda (_base _plan _wave source _attempt _fence _coord _evidence)
                          (hasheq 'status
                                  "published"
                                  'branch
                                  (hash-ref source 'branch)
                                  'head
                                  (hash-ref source 'head)))
                        current-gsd-approved-head-publisher)]
                   [current-directory repo])
      (run-campaign! root
                     (load-campaign-record root plan)
                     #:isolate? #f
                     #:timeout-sec 600
                     #:delivery-reader (test-delivered-reader)
                     #:runner (lambda (_)
                                (set-box! model-calls (add1 (unbox model-calls)))
                                'unexpected-model-author-response)
                     #:delivery-coordinator
                     (lambda (_plan _idx)
                       (error 'preserved-integration "ordinary coordinator forbidden"))
                     #:verifier (lambda (_) #f))))
  ;; Phase 2 — ONLY the fresh K' is approved: real C1/G1, staged wrapper, the
  ;; real R1 baseline-overlay probe, then the typed S authorization wait.
  (define result2 (resume-run! "candidate-proposal"))
  (check-eq? (campaign-result-status result2) 'wave-blocked)
  (check-true (string-contains? (campaign-result-message result2)
                                "operator supersession authorization unavailable")
              (campaign-result-message result2))
  (check-equal? (unbox model-calls) 0 "zero model dispatch across real C1/G1/R1")
  (define op2 (load-supplemental-operation g))
  (check-equal? (hash-ref op2 'state) "probe-captured")
  (define facts2 (hash-ref op2 'facts))
  (define green2 (hash-ref facts2 'green-proof-ref #f))
  (define red2 (hash-ref facts2 'probe-proof-ref #f))
  (check-equal? (and (hash? green2) (hash-ref green2 'kind #f))
                "supplemental-declared-verify-green/v1")
  ;; The staged R1 is a content-addressed retained reference (sha256+path).
  (check-true (and (hash? red2)
                   (string? (hash-ref red2 'path #f))
                   (string? (hash-ref red2 'sha256 #f))
                   (regexp-match? #px"^[0-9a-f]{64}$" (hash-ref red2 'sha256))))
  (check-true (and (string? (hash-ref green2 'sha256 #f))
                   (regexp-match? #px"^[0-9a-f]{64}$" (hash-ref green2 'sha256))))
  (check-not-equal? (hash-ref green2 'sha256) (hash-ref red2 'sha256))
  (check-equal? (hash-ref facts2 'allocation) (allocation "attempt-7" 10))
  (check-equal? (unbox decisions)
                '("candidate-proposal" "supersession-proposal" "supersession-proposal"))
  (check-equal? (hash-ref ctx 'attempt-id) "attempt-2")
  (check-equal? (hash-ref ctx 'attempt-fence) 5)
  (define after2 (load-campaign-record root plan))
  (check-equal? (campaign-attempt-id (campaign-wave-current-attempt (wave-of after2 wave)))
                "attempt-7")
  (check-equal? (campaign-attempt-fence-token (campaign-wave-current-attempt (wave-of after2 wave)))
                10)
  (check-equal? (campaign-fence-token after2) 10)
  ;; Phase 3 — the ONE remaining owner gate: S authorization. The protected
  ;; publication tail (published → receipt-certified) and the promotion
  ;; (registered → completed) run through their existing production owners
  ;; with the same captured G1/R1 — never a recapture, never a re-approval.
  (define generation2 (length (hash-ref (load-delivery-journal root plan wave) 'receipt-history '())))
  (define result3 (resume-run! "supersession-proposal" #:publish? #t))
  (check-eq? (campaign-result-status result3) 'wave-blocked)
  ;; The truthful completed stop: readiness is owned by the registered
  ;; receipt, never claimed from the result alone.
  (check-true (string-prefix? (campaign-result-message result3)
                              "supplemental evidence task completed;")
              (campaign-result-message result3))
  (check-true (string-contains? (campaign-result-message result3)
                                "readiness owned by the registered receipt")
              (campaign-result-message result3))
  (check-equal? (unbox model-calls) 0 "zero model dispatch across publication/promotion")
  (define op3 (load-supplemental-operation g))
  (check-equal? (hash-ref op3 'state) "completed")
  (define facts3 (hash-ref op3 'facts))
  (check-equal? (hash-ref facts3 'green-proof-ref) green2 "captured G1 is never re-run")
  (check-equal? (hash-ref facts3 'probe-proof-ref) red2 "captured R1 is never re-run")
  (check-true (hash? (hash-ref facts3 'supersession-approval-ref #f)))
  (check-equal? (hash-ref facts3 'allocation) (allocation "attempt-7" 10))
  (check-equal?
   (unbox decisions)
   '("candidate-proposal" "supersession-proposal" "supersession-proposal" "supersession-proposal"))
  (define journal3 (load-delivery-journal root plan wave))
  (check-equal? (length (hash-ref journal3 'receipt-history '())) (add1 generation2))
  (define receipt3 (hash-ref journal3 'receipt))
  (check-equal? (hash-ref receipt3 'attempt-id) "attempt-7")
  (check-equal? (hash-ref receipt3 'attempt-fence) 10)
  (check-equal? (hash-ref receipt3 'head) (hash-ref m 'head))
  (define readiness3 (evidence-readiness root plan wave))
  (check-equal? (hash-ref readiness3 'status) "ready")
  (check-equal? (hash-ref readiness3 'code) "registered-probe-ready")
  (check-equal? (hash-ref ctx 'attempt-id) "attempt-2")
  (check-equal? (hash-ref ctx 'attempt-fence) 5)
  (define after3 (load-campaign-record root plan))
  (check-equal? (campaign-attempt-id (campaign-wave-current-attempt (wave-of after3 wave)))
                "attempt-7")
  (check-equal? (campaign-attempt-fence-token (campaign-wave-current-attempt (wave-of after3 wave)))
                10)
  (check-equal? (campaign-fence-token after3) 10))

;; ---------------------------------------------------------------------------
;; Success: inert proposal, exact T install (single claim), zero-model
;; adoption, fresh K', human K' wait.
;; ---------------------------------------------------------------------------
(define (success-case! chain)
  (define root (hash-ref chain 'root))
  (define plan (hash-ref chain 'plan))
  (define wave (hash-ref chain 'wave))
  (define repo (hash-ref chain 'repo))
  (define ctx (hash-ref chain 'ctx))
  (define binding (hash-ref chain 'binding))
  (define old-pointer (load-supplemental-pointer root plan wave))
  (define old-pointer-bytes
    (file->bytes
     (build-path root ".planning" "supplemental-evidence" (format "~a-w~a.json" plan wave))))
  (retarget-campaign! root plan wave "attempt-6" 9)
  ;; 0. The owner CLI accepts --preserved-candidate as inert DATA only: a
  ;; well-formed binding produces an inert v3 proposal (pointer unchanged),
  ;; and a malformed file refuses (exit 2) before any authority is touched.
  (define cli-binding (build-path root "preserved-candidate.json"))
  (display-to-file (canonical-evidence-json binding) cli-binding #:exists 'truncate)
  (define cli-common
    (list "--verb"
          "propose-disposition"
          "--authority-root"
          (path->string (current-supplemental-authority-root))
          "--campaign-root"
          (path->string root)
          "--plan"
          plan
          "--wave"
          (number->string wave)
          "--sha256"
          (hash-ref (hash-ref old-pointer 'task-proposal) 'sha256)
          "--expires-at"
          (number->string (+ (current-seconds) 1700))))
  (define (run-cli-subprocess! args)
    ;; The refusal path exits 2. Run the real script boundary as a child so the
    ;; exit can never reach the test process; the shape gate refuses before
    ;; any receipt/proof access.
    (parameterize ([current-output-port (open-output-nowhere)]
                   [current-error-port (open-output-nowhere)])
      (apply system*/exit-code (find-executable-path "racket") (path->string cli-module) args)))
  (define (run-cli-in-process! args)
    ;; The positive path shares THIS process's fixture parameterization (the
    ;; owned-proof root), exactly like the real /go process would.
    (define out (open-output-string))
    (parameterize ([current-command-line-arguments (list->vector args)]
                   [current-output-port out]
                   [current-error-port (open-output-nowhere)])
      (dynamic-require cli-module #f))
    (get-output-string out))
  (define cli-result
    (read-json (open-input-string (run-cli-in-process! (append cli-common
                                                               (list "--preserved-candidate"
                                                                     (path->string cli-binding)))))))
  (define cli-proposal
    (load-supplemental-record
     (hasheq 'kind "task-proposal" 'sha256 (string->immutable-string (hash-ref cli-result 'sha256)))))
  (check-equal? (load-supplemental-pointer root plan wave) old-pointer)
  (define cli-bad (build-path root "preserved-candidate-bad.json"))
  (display-to-file "{\"schema\":\"bogus\"}" cli-bad #:exists 'truncate)
  (check-equal? 2
                (run-cli-subprocess! (append cli-common
                                             (list "--preserved-candidate" (path->string cli-bad)))))
  ;; 1. Proposal is INERT: no pointer move, no claim, no operation.
  (define-values (p task) (propose-preserved! root plan wave binding))
  (check-equal? (hash-ref (hash-ref (hash-ref cli-proposal 'payload) 'task) 'disposition)
                (hash-ref task 'disposition))
  (check-equal? (load-supplemental-pointer root plan wave) old-pointer)
  (check-equal?
   (file->bytes
    (build-path root ".planning" "supplemental-evidence" (format "~a-w~a.json" plan wave)))
   old-pointer-bytes)
  (check-false (file-exists? (claim-path-for (hash-ref chain 'amendment))))
  (check-true (valid-supplemental-task? task))
  (check-equal? (hash-ref (hash-ref task 'disposition) 'preserved-candidate) binding)
  ;; 2. Exact approval + install: ONE permanent claim, pointer moves once.
  (define g (fixture-approve! p))
  (define installed (install-supplemental-disposition! root plan wave p g))
  (check-equal? installed (disposition-pointer p g))
  (check-equal? (load-supplemental-pointer root plan wave) (disposition-pointer p g))
  (define claim (claim-path-for (hash-ref chain 'amendment)))
  (check-true (file-exists? claim))
  (define claim-bytes (file->bytes claim))
  (check-equal? (hash-ref (read-disposition-claim! claim) 'lineage) (hash-ref task 'disposition))
  ;; Replay of the exact same install is idempotent (still exactly one claim).
  (install-supplemental-disposition! root plan wave p g)
  (check-equal? (file->bytes claim) claim-bytes)
  ;; A different task/v3 at the same fixed amendment cannot claim a fourth.
  (define forged (seal-supplemental-record (hash-set task 'control-plane-head (make-string 40 #\b))))
  (define fp
    (propose-retain! "task-proposal" (hasheq 'task forged 'expires-at (+ (current-seconds) 1700))))
  (define fg (fixture-approve! fp))
  (expect-refusal
   #rx"outside the fixed disposition amendment|already permanently claimed|claim|pointer"
   (lambda () (install-supplemental-disposition! root plan wave fp fg)))
  (check-equal? (file->bytes claim) claim-bytes)
  ;; The fused entry view carries the authenticated E_old context
  ;; (byte-equal, never M), the exact installed task and the fixed-a3 parked
  ;; parent allocation in ONE trusted load.
  (define entry-view
    (parameterize ([current-directory repo])
      (supplemental-approved-entry-view root plan wave)))
  (check-equal? (hash-ref entry-view 'context) ctx)
  (check-equal? (hash-ref entry-view 'task) task)
  (check-equal? (hash-ref entry-view 'parent-allocation) (allocation "attempt-6" 9))
  ;; 2b. PARKED RESOLUTION REGRESSION (red-first): the preserved entry is
  ;; parked on the authenticated fixed-a3 terminal allocation
  ;; attempt-6/fence-9, which is GENUINELY DISTINCT from the immutable E_old
  ;; receipt context attempt-2/fence-5. Every wrong parent identity, status,
  ;; cancellation or pause must refuse with the exact stale error and commit
  ;; nothing (no pending status, no operation).
  (check-equal? (hash-ref ctx 'attempt-id) "attempt-2")
  (check-equal? (hash-ref ctx 'attempt-fence) 5)
  (retarget-campaign! root plan wave "attempt-5" 9)
  (expect-stale-parked-refusal! root plan wave 'awaiting-delivery g)
  (retarget-campaign! root plan wave "attempt-6" 8)
  (expect-stale-parked-refusal! root plan wave 'awaiting-delivery g)
  (retarget-campaign! root plan wave "attempt-6" 9)
  (mutate-campaign! root plan (lambda (rec) (set-campaign-fence-token! rec 8)))
  (expect-stale-parked-refusal! root plan wave 'awaiting-delivery g)
  (retarget-campaign! root plan wave "attempt-6" 9)
  (mutate-campaign! root plan (lambda (rec) (set-campaign-fence-token! rec 10)))
  (expect-stale-parked-refusal! root plan wave 'awaiting-delivery g)
  (retarget-campaign! root plan wave "attempt-6" 9 #:status 'in-progress)
  (expect-stale-parked-refusal! root plan wave 'in-progress g)
  (retarget-campaign! root plan wave "attempt-6" 9)
  (mutate-campaign! root
                    plan
                    (lambda (rec)
                      (set-campaign-cancellation! rec (make-campaign-cancellation "stop" 0))))
  (expect-stale-parked-refusal! root plan wave 'awaiting-delivery g)
  (retarget-campaign! root plan wave "attempt-6" 9)
  (mutate-campaign!
   root
   plan
   (lambda (rec)
     (set-campaign-record-budget-pause!
      rec
      (campaign-budget-pause 'max-cost 1.0 2.0 "fixture ceiling crossed" (current-seconds)))))
  (expect-stale-parked-refusal! root plan wave 'awaiting-delivery g)
  (retarget-campaign! root plan wave "attempt-6" 9)
  ;; The GENUINE parked resolution accepts the fixed-a3 allocation and only
  ;; flips status to pending: it allocates nothing and never consumes T.
  (check-eq? (resolve-supplemental-author-checkpoint! root plan wave) #t)
  (check-eq? (campaign-wave-status (wave-of (load-campaign-record root plan) wave)) 'pending)
  (check-false (load-supplemental-operation g) "resolution never allocates")
  ;; 3. Adoption fail-closed while the fresh permission is NOT consumed.
  (prepare-go! chain)
  (define (no-operation!)
    (check-false (load-supplemental-operation g)))
  ;; cancellation
  (mutate-campaign! root
                    plan
                    (lambda (rec)
                      (set-campaign-cancellation! rec (make-campaign-cancellation "stop" 0))))
  (define-values (cancelled-outcome cancelled-calls) (attempt-adoption! chain))
  (check-eq? (wave-execution-outcome-kind cancelled-outcome) 'failed)
  (check-equal? cancelled-calls 0)
  (no-operation!)
  (clear-campaign! root plan "attempt-7" 11)
  ;; budget pause
  (mutate-campaign!
   root
   plan
   (lambda (rec)
     (set-campaign-record-budget-pause!
      rec
      (campaign-budget-pause 'max-cost 1.0 2.0 "fixture ceiling crossed" (current-seconds)))))
  (define-values (paused-outcome paused-calls) (attempt-adoption! chain))
  (check-eq? (wave-execution-outcome-kind paused-outcome) 'failed)
  (check-equal? paused-calls 0)
  (no-operation!)
  (clear-campaign! root plan "attempt-7" 11)
  ;; fence loss: the record names a different fence than the requested attempt
  (retarget-campaign! root plan wave "attempt-7" 12 #:status 'failed)
  (define-values (fenced-outcome fenced-calls) (attempt-adoption! chain))
  (check-eq? (wave-execution-outcome-kind fenced-outcome) 'failed)
  (check-equal? fenced-calls 0)
  (no-operation!)
  (clear-campaign! root plan "attempt-7" 11)
  ;; source drift: a non-owner production change on top of M
  (put repo "runtime/local.rkt" "#lang racket/base\n(provide answer)\n(define answer 0)\n")
  (git-text repo "add" "runtime/local.rkt")
  (git-text repo "commit" "-q" "-m" "unauthorized drift")
  (define-values (drift-outcome drift-calls) (attempt-adoption! chain))
  (check-eq? (wave-execution-outcome-kind drift-outcome) 'failed)
  (check-equal? drift-calls 0)
  (git-text repo "checkout" "-q" "-B" branch (hash-ref (hash-ref chain 'm) 'head))
  (no-operation!)
  ;; empty/untracked dirt also refuses
  (put repo "scratch.txt" "dirty\n")
  (define-values (dirty-outcome dirty-calls) (attempt-adoption! chain))
  (check-eq? (wave-execution-outcome-kind dirty-outcome) 'failed)
  (check-equal? dirty-calls 0)
  (delete-file (build-path repo "scratch.txt"))
  (no-operation!)
  ;; 4. Production /go entry from the parked parent, reusing the exact
  ;; installed T/proposal from this case.
  (full-campaign-case! chain #:installed (hasheq 'proposal p 'approval g 'task task)))

;; ---------------------------------------------------------------------------
;; Fail-closed proposal and entry: tamper/drift/cancel/pause/fence/revocation
;; ---------------------------------------------------------------------------
(define (tamper-case! chain)
  (define root (hash-ref chain 'root))
  (define plan (hash-ref chain 'plan))
  (define wave (hash-ref chain 'wave))
  (define repo (hash-ref chain 'repo))
  (define binding (hash-ref chain 'binding))
  (define pointer-before (load-supplemental-pointer root plan wave))
  (define (refuse! b pattern)
    (expect-refusal pattern (lambda () (propose-preserved! root plan wave b)))
    (check-equal? (load-supplemental-pointer root plan wave) pointer-before)
    (check-false (file-exists? (claim-path-for (hash-ref chain 'amendment)))))
  (retarget-campaign! root plan wave "attempt-6" 9)
  ;; malformed / unknown / digest-tampered bindings
  (refuse! (hash-set binding 'extra #t) #rx"preserved|malformed|exact")
  (refuse! (hash-set binding 'schema "supplemental-preserved-candidate/v0")
           #rx"preserved|malformed|exact")
  (refuse! (hash-set binding
                     'owner-paths
                     (cons (hash-set (car (hash-ref binding 'owner-paths)) 'after-sha256 (hex 64 #\0))
                           (cdr (hash-ref binding 'owner-paths))))
           #rx"owner|sha256|blob|preserved")
  ;; descendant that is NOT a single-parent K child on the owner-path set
  (put repo "runtime/local.rkt" "#lang racket/base\n(provide answer)\n(define answer 1)\n")
  (git-text repo "add" "runtime/local.rkt")
  (git-text repo "commit" "-q" "-m" "non-owner drift")
  (define drift-head (git-text repo "rev-parse" "HEAD"))
  (define drift-tree (git-text repo "rev-parse" "HEAD^{tree}"))
  (refuse! (hash-set binding 'descendant-source (hasheq 'head drift-head 'tree drift-tree))
           #rx"owner-path|diff|descendant|current source")
  (git-text repo "checkout" "-q" "-B" branch (hash-ref (hash-ref chain 'm) 'head))
  ;; dirty/untracked current source
  (put repo "scratch.txt" "dirty\n")
  (refuse! binding #rx"dirty|untracked|current source|retained clean source")
  (delete-file (build-path repo "scratch.txt"))
  ;; cancellation / pause / fence fail closed at the SAME entry gate
  (mutate-campaign! root
                    plan
                    (lambda (rec)
                      (set-campaign-cancellation! rec (make-campaign-cancellation "stop" 0))))
  (refuse! binding #rx"cancelled|unpaused|uncancelled|parent must own")
  (clear-campaign! root plan "attempt-6" 9)
  (mutate-campaign!
   root
   plan
   (lambda (rec)
     (set-campaign-record-budget-pause!
      rec
      (campaign-budget-pause 'max-cost 1.0 2.0 "fixture ceiling crossed" (current-seconds)))))
  (refuse! binding #rx"paused|unpaused|parent must own")
  (clear-campaign! root plan "attempt-6" 9)
  (retarget-campaign! root plan wave "attempt-6" 10)
  (refuse! binding #rx"attempt|fence|parent must own")
  (clear-campaign! root plan "attempt-6" 9)
  ;; Old candidate approval revocation is fail-closed even for a genuine
  ;; otherwise-valid binding (last: a revocation is permanent).
  (revoke-approval! (hash-ref chain 'capref))
  (refuse! binding #rx"revoked|approval|absent|foreign"))

;; ---------------------------------------------------------------------------
;; Crash cut after the honest adoption launch: no redispatch, no reallocation,
;; no fresh K'.
;; ---------------------------------------------------------------------------
(define (crash-cut-case! chain)
  (define root (hash-ref chain 'root))
  (define plan (hash-ref chain 'plan))
  (define wave (hash-ref chain 'wave))
  (define ctx (hash-ref chain 'ctx))
  (retarget-campaign! root plan wave "attempt-6" 9)
  (define installed (install-preserved! chain))
  (define g (hash-ref installed 'approval))
  (define p (hash-ref installed 'proposal))
  (check-equal? (length (fresh-candidates-for p)) 0)
  (prepare-go! chain)
  ;; Simulate a process cut AFTER the durable adoption launch, before sealing.
  (start-supplemental-operation! g (base-binding ctx 11))
  (transition-supplemental-operation! g
                                      0
                                      "proposal-approved"
                                      "reserved"
                                      (hasheq 'allocation (allocation "attempt-7" 11)))
  (transition-supplemental-operation!
   g
   1
   "reserved"
   "launched"
   (hasheq 'dispatch-id "adoption:attempt-7" 'dispatch-kind "preserved-candidate-adoption"))
  (define-values (kind detail)
    (parameterize ([current-directory (hash-ref chain 'repo)])
      (advance-supplemental-evidence! root plan wave)))
  (check-eq? kind 'failed)
  (check-regexp-match #rx"ambiguous|spent|blocked|no captured" detail)
  (define operation (load-supplemental-operation g))
  (check-equal? (hash-ref operation 'state) "blocked")
  (check-true (hash-ref operation 'launch-consumed))
  (check-false (hash-ref (hash-ref operation 'facts) 'candidate-ref #f))
  ;; Reentry cannot reallocate or redispatch, and no K' exists.
  (define-values (outcome model-calls) (attempt-adoption! chain))
  (check-eq? (wave-execution-outcome-kind outcome) 'failed)
  (check-equal? model-calls 0)
  (check-equal? (hash-ref (load-supplemental-operation g) 'sha256) (hash-ref operation 'sha256))
  (check-equal? (length (fresh-candidates-for p)) 0))

;; ---------------------------------------------------------------------------
;; Revocation of the fresh one-shot T authority: the parked resolver refuses
;; with no status commit and no operation, even for the genuine allocation.
;; ---------------------------------------------------------------------------
(define (parked-revocation-case! chain)
  (define root (hash-ref chain 'root))
  (define plan (hash-ref chain 'plan))
  (define wave (hash-ref chain 'wave))
  (retarget-campaign! root plan wave "attempt-6" 9)
  (define installed (install-preserved! chain))
  (define p (hash-ref installed 'proposal))
  (define g (hash-ref installed 'approval))
  ;; The genuine parked resolution works before revocation...
  (check-eq? (resolve-supplemental-author-checkpoint! root plan wave) #t)
  (check-eq? (campaign-wave-status (wave-of (load-campaign-record root plan) wave)) 'pending)
  (check-false (load-supplemental-operation g))
  ;; ...and a revoked T can never authorize a second park/commit.
  (retarget-campaign! root plan wave "attempt-6" 9)
  (revoke-approval! g)
  (check-regexp-match #rx"exact live task approval|revoked|absent|foreign"
                      (resolve-supplemental-author-checkpoint! root plan wave))
  (check-eq? (campaign-wave-status (wave-of (load-campaign-record root plan) wave))
             'awaiting-delivery)
  (check-false (with-handlers ([exn:fail? (lambda (_) #f)])
                 (load-supplemental-operation g)))
  (check-equal? (load-supplemental-pointer root plan wave) (disposition-pointer p g))
  (check-equal? (length (fresh-candidates-for p)) 0))

(module+ test
  (test-case "inert proposal, single exact T claim, zero-model K->M adoption, fresh K' then human K' wait"
    (with-chain success-case!))
  (test-case "tamper, source drift, cancellation, pause, fence loss and old K revocation fail closed"
    (with-chain tamper-case!))
  (test-case "crash cut after adoption launch neither reallocates nor reseals a conflicting K'"
    (with-chain crash-cut-case!))
  (test-case "revoked fresh T authority refuses the parked checkpoint without commit or allocation"
    (with-chain parked-revocation-case!))
  (test-case "parked preserved resolution loads the fused entry view exactly twice"
    ;; Deterministic Git-call counting regression (W3b): the parked
    ;; checkpoint fetches ONE fused authenticated entry view initially and
    ;; freshly again pre-commit. Each view load costs exactly TWO full leaf
    ;; admissions — the authenticated task load revalidates the fixed-a3
    ;; disposition claim through the journal, then the view runs its own
    ;; complete preserved admission (claim binding + retained E_old
    ;; receipt/context equality + parent allocation) — so a genuine resolve
    ;; costs exactly four admissions where the historical two-accessor
    ;; resolve cost eight (four accessor loads, two admissions each).
    ;; Process counts only; nothing is cached across calls: every later
    ;; load still costs a full fresh admission after an earlier one
    ;; succeeded.
    (with-chain
     (lambda (chain)
       (define root (hash-ref chain 'root))
       (define plan (hash-ref chain 'plan))
       (define wave (hash-ref chain 'wave))
       (define repo (hash-ref chain 'repo))
       (retarget-campaign! root plan wave "attempt-6" 9)
       (define installed (install-preserved! chain))
       (define g (hash-ref installed 'approval))
       (define task (hash-ref installed 'task))
       ;; One full leaf admission is the unit of view cost.
       (define leaf-calls
         (call-counting-git (lambda (git-calls)
                              (parameterize ([current-directory repo])
                                (validate-preserved-candidate! root plan wave task))
                              (git-calls))))
       (check-true (> leaf-calls 0))
       (define view1-calls
         (call-counting-git (lambda (git-calls)
                              (parameterize ([current-directory repo])
                                (supplemental-approved-entry-view root plan wave))
                              (git-calls))))
       (check-equal? view1-calls (* 2 leaf-calls))
       ;; The genuine parked resolution: exactly two view loads.
       (define resolve-calls
         (call-counting-git (lambda (git-calls)
                              (check-eq? (resolve-supplemental-author-checkpoint! root plan wave) #t)
                              (git-calls))))
       (printf "entry-view counts: leaf=~a view=~a resolve=~a (resolve = exactly 2 views)\n"
               leaf-calls
               view1-calls
               resolve-calls)
       (check-equal? resolve-calls (* 2 view1-calls))
       (check-eq? (campaign-wave-status (wave-of (load-campaign-record root plan) wave)) 'pending)
       ;; No cross-call cache: a later view load re-runs the whole admission
       ;; and still carries task/context/parent-allocation in one result.
       (define view-calls
         (call-counting-git (lambda (git-calls)
                              (define view
                                (parameterize ([current-directory repo])
                                  (supplemental-approved-entry-view root plan wave)))
                              (check-equal? (hash-ref view 'context) (hash-ref chain 'ctx))
                              (check-equal? (hash-ref view 'task) task)
                              (check-equal? (hash-ref view 'parent-allocation)
                                            (allocation "attempt-6" 9))
                              (git-calls))))
       (check-equal? view-calls view1-calls)
       (check-false (load-supplemental-operation g) "resolution never allocates")))))
