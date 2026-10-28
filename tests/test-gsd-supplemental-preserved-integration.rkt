#lang racket
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 900
;; @covers extensions/gsd/supplemental-preserved-candidate.rkt
;; @covers extensions/gsd/supplemental-evidence-disposition.rkt
;; @covers extensions/gsd/supplemental-evidence-controller.rkt
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
;; allocation/source M and then waits at the human K' approval. Tamper, source
;; drift, cancellation, budget pause, fence loss, an old-candidate revocation
;; and a crash cut after launch all fail closed with no ordinary fallback.
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
         (only-in "../extensions/gsd/wave-runner-port.rkt" wave-execution-outcome-kind)
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
                                     "candidate-attempt"
                                     6
                                     (lambda () (check-verify-command root wave #f))
                                     #:cwd repo)))
  (unless capture
    (error 'preserved-integration "real candidate-source C0 capture failed"))
  (define receipt
    (hash-set* before
               'attempt-id
               "candidate-attempt"
               'attempt-fence
               6
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
(define (retarget-campaign! root plan wave attempt-id fence #:status [status 'awaiting-delivery])
  (define rec (load-campaign-record root plan))
  (define w (wave-of rec wave))
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
  ;; Entry context is the authenticated E_old context (byte-equal), never M.
  (check-equal? (parameterize ([current-directory repo])
                  (supplemental-approved-entry-context root plan wave))
                ctx)
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
  ;; 4. Successful zero-model adoption on the exact attempt/source M.
  (define m (hash-ref chain 'm))
  (define-values (outcome model-calls) (attempt-adoption! chain))
  (check-eq? (wave-execution-outcome-kind outcome) 'done)
  (check-equal? model-calls 0 "no model author callback may run")
  (define operation (load-supplemental-operation g))
  (check-equal? (hash-ref operation 'state) "candidate-sealed")
  (check-true (hash-ref operation 'launch-consumed))
  (define facts (hash-ref operation 'facts))
  (check-equal? (hash-ref facts 'dispatch-id) "adoption:attempt-7")
  (check-equal? (hash-ref facts 'dispatch-kind) "preserved-candidate-adoption")
  (check-equal? (hash-ref facts 'allocation) (allocation "attempt-7" 11))
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
  ;; 5. /go continues to the HUMAN K' wait with zero model callbacks. The
  ;; channel is temporarily absent so the controller reports the honest typed
  ;; pause instead of blocking a test on an operator decision.
  (define channel (current-approval-channel))
  (dynamic-wind void
                (lambda ()
                  (set-approval-channel! #f)
                  (parameterize ([current-directory repo])
                    (define-values (kind detail) (advance-supplemental-evidence! root plan wave))
                    (check-eq? kind 'paused)
                    (check-regexp-match #rx"operator candidate approval unavailable" detail)))
                (lambda ()
                  (when channel
                    (set-approval-channel! channel))))
  (check-equal? model-calls 0)
  (check-equal? (hash-ref (load-supplemental-operation g) 'state) "candidate-sealed")
  (check-false
   (hash-ref (hash-ref (load-supplemental-operation g) 'facts) 'candidate-approval-ref #f))
  (check-equal? (length (fresh-candidates-for p)) 1))

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

(module+ test
  (test-case "inert proposal, single exact T claim, zero-model K->M adoption, fresh K' then human K' wait"
    (with-chain success-case!))
  (test-case "tamper, source drift, cancellation, pause, fence loss and old K revocation fail closed"
    (with-chain tamper-case!))
  (test-case "crash cut after adoption launch neither reallocates nor reseals a conflicting K'"
    (with-chain crash-cut-case!)))
