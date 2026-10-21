#lang racket/base
;; STABILITY: internal
;; BUG-0088 C1: owned supplemental-evidence orchestration (controller).
;;
;; This module drives the governed supplemental evidence task through the B1
;; proof APIs. It is owner-side trusted code inside the campaign process; it
;; NEVER grants authority itself. Every operator decision is requested
;; through supplemental-evidence-operator (the existing interactive broker
;; only) and consumed once via the authority owner-frontend submod with the
;; exact proposal digest. There is no boolean, stdin, policy or model path to
;; any approval here, and no readiness is ever asserted without the
;; underlying records: when a downstream publication/promotion module is not
;; installed, the controller reports the gap and pauses — never fakes
;; readiness.
;;
;; Durable state is the protected supplemental operation journal (A3) plus
;; the campaign record. A public INERT pointer file
;; (.planning/supplemental-evidence/<plan>-w<wave>.json) holds only the
;; {kind sha256} proposal/approval identities needed to FIND the private
;; operation; it confers no authority and is revalidated against the private
;; records on every use.
;;
;; State routing (never re-reserves, never redispatches):
;;   proposal-approved/reserved -> author lane: ONE test-author dispatch
;;       inside a real monotonic begin-attempt! allocation, launch permission
;;       durably consumed BEFORE the dispatch;
;;   launched..registered       -> checkpoint/entry resume WITHOUT a new
;;       attempt; pauses are typed stops at operator waits;
;;   terminal states            -> ordinary routing only.
(require json
         racket/file
         racket/list
         racket/match
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         "campaign-state.rkt"
         (only-in "campaign-result.rkt" campaign-result)
         (only-in "campaign-repository.rkt" load-campaign-record persist-campaign!)
         (only-in "delivery-journal.rkt" load-delivery-journal)
         (only-in "delivery-receipt.rkt"
                  committed-delivery-snapshot
                  current-wave-for-attempt
                  capture-supplemental-delivery-verification)
         (only-in "delivery-verifier.rkt"
                  check-verify-command
                  delivery-verification
                  current-gsd-delivery-branch-context
                  current-gsd-delivery-verify-jobs
                  current-gsd-delivery-verify-timeout-sec
                  make-branch-delivery-context)
         (only-in "review-proof-format.rkt" apply-verify-jobs)
         (only-in "review-proof.rkt" frozen-verify-binding load-receipt-owned-proof)
         (only-in "evidence-readiness.rkt" current-evidence-context evidence-readiness)
         (only-in "evidence-probe.rkt" run-staged-supplemental-probe! validate-baseline-probe!)
         (only-in "evidence-repair-format.rkt" canonical-evidence-json canonical-evidence-digest)
         "effective-verify-binding.rkt"
         "supplemental-evidence-format.rkt"
         "supplemental-evidence-authority.rkt"
         (submod "supplemental-evidence-authority.rkt" owner-frontend)
         "supplemental-evidence-journal.rkt"
         "supplemental-evidence-operator.rkt"
         (only-in "author-execution.rkt" run-supplemental-test-author!)
         (only-in "../gsd-planning/execution-policy.rkt" current-gsd-author-phase)
         (only-in "composition-root.rkt" current-gsd-verification-registry)
         (only-in "verification-job.rkt" make-verification-registry verification-close-registry!)
         (only-in "wave-executor.rkt" wave-worktree)
         "wave-runner-port.rkt"
         (only-in "../../util/json/checksum.rkt" sha256-file sha256-string))
(provide supplemental-pointer-path
         load-supplemental-pointer
         supplemental-pointer-retained?
         write-supplemental-pointer!
         build-supplemental-task-proposal!
         propose-supplemental-recovery!
         install-supplemental-recovery!
         supplemental-active-operation
         supplemental-author-pending?
         supplemental-approved-entry-context
         supplemental-operation-live?
         supplemental-resumable?
         supplemental-operation-summary
         run-supplemental-author-phases!
         advance-supplemental-evidence!
         supplemental-entry-outcome
         supplemental-verify-route!
         supplemental-checkpoint-step!
         retained-supplemental-worktree)

;; ----------------------------------------------------------------------
;; Small trusted-git fact helper (owner side only; never model reachability)
;; ----------------------------------------------------------------------
(define (git repo . args)
  (define out (open-output-string))
  (define err (open-output-string))
  (define code
    (parameterize ([current-directory repo]
                   [current-output-port out]
                   [current-error-port err])
      (apply system*/exit-code (find-executable-path "git") args)))
  (unless (zero? code)
    (error 'supplemental-evidence-controller "git fact failed (~a): ~a" args (get-output-string err)))
  (string-trim (get-output-string out)))
(define (git-ok? repo . args)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (apply git repo args)
    #t))
;; Directory-form comparison: both spellings normalize to one physical path.
(define (same-path? a b)
  (equal? (path->string (simplify-path (path->directory-path (path->complete-path a)) #f))
          (path->string (simplify-path (path->directory-path (path->complete-path b)) #f))))

;; The installed control-plane head, read from THIS checkout, never a literal.
(define-runtime-path control-source-root "../..")
(define (control-plane-head!)
  (git (simplify-path control-source-root #f) "rev-parse" "HEAD"))

(define (refuse message)
  (error 'supplemental-evidence-controller "~a" message))
(define (trim-message text [limit 400])
  (define one (string-join (string-split (format "~a" text)) " "))
  (if (> (string-length one) limit)
      (string-append (substring one 0 limit) "…")
      one))
(struct exn:supplemental-pause exn:fail ())
(define (raise-pause! message)
  (raise (exn:supplemental-pause message (current-continuation-marks))))

;; ----------------------------------------------------------------------
;; Public inert pointer: how the private operation is FOUND for a campaign.
;; Contains only immutable {kind sha256} references — never approval state.
;; ----------------------------------------------------------------------
(define (pointer-path base plan wave)
  (build-path base ".planning" "supplemental-evidence" (format "~a-w~a.json" plan wave)))
(define (supplemental-pointer-path base plan wave)
  (pointer-path base plan wave))
(define (valid-pointer? pointer)
  (and (hash? pointer)
       (immutable? pointer)
       (= 3 (hash-count pointer))
       (equal? (hash-ref pointer 'schema #f) "supplemental-pointer/v1")
       (supplemental-reference? (hash-ref pointer 'task-proposal #f))
       (equal? (hash-ref (hash-ref pointer 'task-proposal) 'kind) "task-proposal")
       (supplemental-reference? (hash-ref pointer 'task-approval #f))
       (equal? (hash-ref (hash-ref pointer 'task-approval) 'kind) "task-approval")))
(define (freeze-json v)
  (cond
    [(hash? v)
     (for/hasheq ([(k x) (in-hash v)])
       (values k (freeze-json x)))]
    [(list? v) (map freeze-json v)]
    [(string? v) (string->immutable-string v)]
    [else v]))
(define (load-supplemental-pointer base plan wave)
  (define path (pointer-path base plan wave))
  (and (file-exists? path)
       (not (link-exists? path))
       (let ([raw (with-handlers ([exn:fail? (lambda (_) #f)])
                    (call-with-input-file path read-json))])
         (and raw (hash? raw) (valid-pointer? (freeze-json raw)) (freeze-json raw)))))
(define (write-supplemental-pointer! base plan wave task-proposal-ref [task-approval-ref #f])
  (when (current-gsd-author-phase)
    (refuse "author phase cannot move the supplemental pointer"))
  (unless (and (supplemental-reference? task-proposal-ref)
               (equal? (hash-ref task-proposal-ref 'kind) "task-proposal")
               (hash-ref (load-supplemental-record task-proposal-ref) 'kind #f))
    (refuse "pointer requires a retained task proposal"))
  (when task-approval-ref
    (unless (and (supplemental-reference? task-approval-ref)
                 (equal? (hash-ref task-approval-ref 'kind) "task-approval")
                 (validate-supplemental-approval task-approval-ref task-proposal-ref))
      (refuse "pointer requires a live task approval for the exact proposal")))
  (define dir (build-path base ".planning" "supplemental-evidence"))
  (make-directory* dir)
  (define absent (hasheq 'kind "task-approval" 'sha256 (make-string 64 #\0)))
  (define record
    (hasheq 'schema
            "supplemental-pointer/v1"
            'task-proposal
            task-proposal-ref
            'task-approval
            (or task-approval-ref absent)))
  (define path (pointer-path base plan wave))
  (define temp (make-temporary-file ".supplemental-pointer-~a.tmp" #f dir))
  (dynamic-wind (lambda ()
                  (call-with-output-file temp
                                         (lambda (out) (display (canonical-evidence-json record) out))
                                         #:exists 'truncate)
                  (file-or-directory-permissions temp #o600))
                (lambda ()
                  (when (file-exists? path)
                    (define existing
                      (with-handlers ([exn:fail? (lambda (_) #f)])
                        (call-with-input-file path read-json)))
                    (unless (hash? existing)
                      (refuse "unreadable existing supplemental pointer"))
                    (unless (equal? (hash-ref existing 'task-proposal) task-proposal-ref)
                      (refuse "pointer already holds a different proposal for this campaign"))
                    (when task-approval-ref
                      (unless (or (equal? (hash-ref existing 'task-approval) absent)
                                  (equal? (hash-ref existing 'task-approval) task-approval-ref))
                        (refuse "pointer already holds a different task approval"))))
                  (rename-file-or-directory temp path #t))
                (lambda ()
                  (when (file-exists? temp)
                    (delete-file temp))))
  (load-supplemental-pointer base plan wave))

;; ----------------------------------------------------------------------
;; Task proposal construction from authentic local facts (owner CLI helper)
;; ----------------------------------------------------------------------
(define-runtime-path recovery-module "supplemental-evidence-recovery.rkt")
(define (propose-supplemental-recovery! base plan wave expected-parent-proposal-sha expires-at)
  ((dynamic-require recovery-module 'propose-supplemental-recovery!)
   base plan wave expected-parent-proposal-sha expires-at))
(define (install-supplemental-recovery! base plan wave new-proposal-ref new-approval-ref)
  ((dynamic-require recovery-module 'install-supplemental-recovery!)
   base plan wave new-proposal-ref new-approval-ref))
(define (hex40? v)
  (and (string? v) (= 40 (string-length v)) (regexp-match? #px"^[0-9a-f]+$" v)))
;; Aggregate ceilings are fixed by the approved engineering plan; task bounds
;; never exceed them and the owner enforces the deadline independently.
(define (fixed-bounds)
  (hasheq 'model-dispatches 1 'verify-timeout-sec 14400 'probe-timeout-sec 600 'deadline-sec 86400))
;; owner-paths are the enumerated owner-generated paths from the approved
;; engineering plan (generated README + ownership reports/matrix/checksums);
;; the operator supplies them explicitly so no live campaign identity is
;; hardcoded in this module.
(define (build-supplemental-task-proposal! base
                                           plan
                                           wave
                                           #:test-path test-path
                                           #:harness-path harness-path
                                           #:owner-paths owner-paths
                                           #:baseline-head baseline-head
                                           #:baseline-tree baseline-tree
                                           #:expires-in-sec [expires-in-sec #f])
  (when (current-gsd-author-phase)
    (refuse "author phase cannot propose a supplemental task"))
  (define readiness (evidence-readiness base plan wave))
  (unless (and (equal? (hash-ref readiness 'status #f) "repair-required")
               (equal? (hash-ref readiness 'code #f) "unsupported-harness"))
    (refuse
     (format "supplemental proposal requires authenticated unsupported-harness readiness, got ~a/~a"
             (hash-ref readiness 'status #f)
             (hash-ref readiness 'code #f))))
  (define context (current-evidence-context base plan wave))
  (define repo (hash-ref context 'repo))
  (define frozen (frozen-verify-binding base plan wave))
  (define c0 (hash-ref frozen 'normalized-command))
  (define c1 (make-effective-verify-command c0 test-path))
  (unless (and (hex40? baseline-head) (hex40? baseline-tree))
    (refuse "pinned baseline must be an exact 40-hex head and tree pair"))
  (unless (and (not (equal? baseline-head (hash-ref context 'head)))
               (git-ok? repo "merge-base" "--is-ancestor" baseline-head (hash-ref context 'head)))
    (refuse "pinned baseline must be a strict ancestor of the current head"))
  (unless (equal? (git repo "rev-parse" (string-append baseline-head "^{tree}")) baseline-tree)
    (refuse "pinned baseline tree does not match the repository"))
  (unless (for/or ([p (in-list (string-split
                                (git repo "diff" "--name-only" baseline-head (hash-ref context 'head))
                                "\n"))])
            (and (string-suffix? p ".rkt") (not (string-prefix? p "tests/"))))
    (refuse "pinned baseline has no real production difference against the current head"))
  (define bounds (fixed-bounds))
  (define expires-at (+ (current-seconds) (or expires-in-sec (hash-ref bounds 'deadline-sec))))
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
                                      harness-path
                                      'owner-paths
                                      owner-paths
                                      'baseline-head
                                      baseline-head
                                      'baseline-tree
                                      baseline-tree
                                      'control-plane-head
                                      (control-plane-head!)
                                      'bounds
                                      bounds)))
  (values (propose-retain! "task-proposal" (hasheq 'task task 'expires-at expires-at)) task))

;; ----------------------------------------------------------------------
;; Active operation discovery + lane predicates
;; ----------------------------------------------------------------------
;; Returns (values operation grant-ref task proposal-ref); #f/#f/#f/#f when
;; no live pointer/operation exists for this campaign. Returned values are
;; revalidated against private records and the current receipt on every call.
(define (supplemental-active-operation base plan wave)
  (define pointer (load-supplemental-pointer base plan wave))
  (if (not pointer)
      (values #f #f #f #f)
      (with-handlers ([exn:fail? (lambda (_) (values #f #f #f #f))])
        (define aref (hash-ref pointer 'task-approval))
        (define tref (hash-ref pointer 'task-proposal))
        (define operation (load-supplemental-operation aref))
        (unless operation
          (refuse "no protected supplemental operation for the pointer"))
        (define proposal (load-supplemental-record tref))
        (define task (hash-ref (hash-ref proposal 'payload) 'task))
        (define journal (load-delivery-journal base plan wave))
        (define receipt (and journal (hash-ref journal 'receipt #f)))
        (define context (hash-ref task 'context))
        ;; Authenticate the physical port through the exact predecessor
        ;; receipt pinned by T, not through CURRENT (which may already be
        ;; this operation's G1 while its private WAL still awaits reconcile).
        ;; Loading G1 here would circularly require a settled operation just
        ;; to route that operation to its authenticated reconcile path.
        (define history (hash-ref journal 'receipt-history '()))
        (define generation (hash-ref context 'receipt-generation))
        (define pinned-receipt
          (and (<= generation (length history))
               (if (= generation (length history)) receipt (list-ref history generation))))
        (define manifest
          (and pinned-receipt
               (equal? (canonical-evidence-digest pinned-receipt) (hash-ref context 'receipt-sha256))
               (equal? (hash-ref (hash-ref pinned-receipt 'owned-proof) 'sha256)
                       (hash-ref context 'green-proof-sha256))
               (load-receipt-owned-proof pinned-receipt base plan wave)))
        ;; Pre-supersession admission: the context names the CURRENT receipt.
        (define (admitted-context?)
          (and (= (hash-ref context 'receipt-generation)
                  (length (hash-ref journal 'receipt-history '())))
               (equal? (hash-ref context 'receipt-sha256) (canonical-evidence-digest receipt))
               (equal? (hash-ref context 'green-proof-sha256)
                       (hash-ref (hash-ref receipt 'owned-proof) 'sha256))))
        ;; After this task's OWN S-authorized supersession, the journal's
        ;; current receipt IS the task's product, so the pre-supersession
        ;; context can never equal it. Accept that superseded identity ONLY
        ;; through the operation's live bound supersession approval,
        ;; revalidated here against the retained exact S record and the
        ;; current journal: the S must have superseded exactly the admitted
        ;; context receipt/generation, belong to THIS task approval, and have
        ;; produced exactly the current receipt. Never a stored boolean.
        (define (superseded-context?)
          (define sapref (hash-ref (hash-ref operation 'facts) 'supersession-approval-ref #f))
          (define sloader (publication-api 'load-supplemental-supersession-proposal!))
          (and receipt
               sloader
               (supplemental-reference? sapref)
               (let* ([approval (with-handlers ([exn:fail? (lambda (_) #f)])
                                  (validate-supplemental-approval sapref))]
                      [sref (and approval (hash-ref approval 'proposal #f))]
                      [s (and sref
                              (equal? (hash-ref sref 'kind #f) "supersession-proposal")
                              (with-handlers ([exn:fail? (lambda (_) #f)])
                                (sloader sref)))])
                 (and s
                      (equal? (hash-ref s 'old-receipt-sha256) (hash-ref context 'receipt-sha256))
                      (equal? (hash-ref s 'old-generation) (hash-ref context 'receipt-generation))
                      (equal? (hash-ref s 'task-approval) aref)
                      (equal? (canonical-evidence-digest (hash-ref s 'receipt))
                              (canonical-evidence-digest receipt))))))
        (unless (and receipt
                     (equal? (hash-ref context 'plan-id) plan)
                     (= (hash-ref context 'wave) wave)
                     (or (admitted-context?) (superseded-context?))
                     manifest
                     (same-path? (hash-ref context 'repo) (hash-ref manifest 'execution-cwd))
                     (equal? (hash-ref context 'branch) (hash-ref receipt 'branch))
                     (equal? (hash-ref context 'origin) (hash-ref receipt 'origin)))
          (refuse "supplemental task context does not match this campaign's receipt"))
        (values operation aref task tref))))
(define (supplemental-author-pending? base plan wave)
  ;; A pending task is a VALIDATED live task approval whose protected record
  ;; is absent or not yet launched. The approval is revalidated here so a
  ;; stale/revoked pointer never selects the author lane.
  (define pointer (load-supplemental-pointer base plan wave))
  (and pointer
       (with-handlers ([exn:fail? (lambda (_) #f)])
         (and (validate-supplemental-approval (hash-ref pointer 'task-approval)
                                              (hash-ref pointer 'task-proposal))
              (let ([operation (load-supplemental-operation (hash-ref pointer 'task-approval))])
                (or (not operation)
                    (member (hash-ref operation 'state) '("proposal-approved" "reserved"))))))
       #t))
;; Pure admission for an unallocated parked entry. Never initialize an
;; operation here: a reserved allocation belongs to its original attempt and
;; cannot acquire a successor through this checkpoint after a crash.
;; The caller holds the authority lock through its campaign status commit.
(define (supplemental-approved-entry-context base plan wave)
  (define pointer (load-supplemental-pointer base plan wave))
  (unless (and pointer
               (validate-supplemental-approval (hash-ref pointer 'task-approval)
                                               (hash-ref pointer 'task-proposal)))
    (refuse "supplemental checkpoint requires the exact live task approval"))
  (define operation (inspect-supplemental-entry-operation (hash-ref pointer 'task-approval)))
  (unless (or (not operation) (equal? (hash-ref operation 'state) "proposal-approved"))
    (refuse "supplemental checkpoint cannot reallocate a reserved or spent task"))
  (define proposal (load-supplemental-record (hash-ref pointer 'task-proposal)))
  (define task (hash-ref (hash-ref proposal 'payload) 'task))
  (define context (hash-ref task 'context))
  ;; control-plane-head is sealed proposal-time provenance, not an execution
  ;; equality pin. Separately authorized engineering may upgrade the trusted
  ;; installed process without reapproving unchanged T. Caller-supplied code
  ;; is not admitted here; HEAD equality would not authenticate loaded bytes
  ;; (nor a dirty engine) within the same-uid/trusted-process boundary.
  ;; Live source/receipt context must still match exactly.
  (unless (equal? context (current-evidence-context base plan wave))
    (refuse "supplemental checkpoint task context drifted"))
  context)

;; The supplemental task OWNS the retained receipt checkout while a pointer
;; exists (even malformed), or its permanent private recovery claim survives:
;; delivery/receipt evidence must never be worktree-cleaned. This is deny-only;
;; pending/active routes still require full authentication, never claim alone.
(define (supplemental-pointer-retained? base plan wave)
  (define path (pointer-path base plan wave))
  (or (link-exists? path)
      (and (file-or-directory-type path #f) #t)
      (supplemental-recovery-pointer-claimed? path)))
(define (supplemental-operation-live? base plan wave)
  (define-values (operation _grant _task _tref) (supplemental-active-operation base plan wave))
  (and operation (not (member (hash-ref operation 'state) '("blocked" "revoked" "completed"))) #t))
;; Checkpoint/entry-routable: past the launch boundary, not terminal; resumed
;; WITHOUT a new attempt. A typed late-WAL pending view (BUG-0088 review B1)
;; is equally routable: resuming reconciles the SAME operation through the
;; journal's authenticated reconcile, never a relaunch or reallocation.
(define (supplemental-resumable? base plan wave)
  (define-values (operation _grant _task _tref) (supplemental-active-operation base plan wave))
  (and operation
       (or (member (hash-ref operation 'state)
                   '("launched" "candidate-sealed"
                                "candidate-approved"
                                "verifying"
                                "green-captured"
                                "probing"
                                "probe-captured"
                                "supersession-approved"
                                "published"
                                "receipt-certified"
                                "registered"))
           (member (hash-ref operation 'state)
                   '("recover-pending-verified" "recover-pending-published"
                                                "recover-pending-receipt-certified"
                                                "recover-pending-registration")))
       #t))
;; Public typed status for operator surfaces (no private material).
(define (supplemental-operation-summary base plan wave)
  (define-values (operation grant task _tref) (supplemental-active-operation base plan wave))
  (and operation
       (hasheq 'state
               (hash-ref operation 'state)
               'revision
               (hash-ref operation 'revision)
               'grant-sha256
               (and grant (hash-ref grant 'sha256))
               'task-sha256
               (and task (hash-ref task 'sha256))
               'deadline-at
               (hash-ref operation 'deadline-at)
               'launch-consumed
               (hash-ref operation 'launch-consumed)
               'facts-keys
               (sort (map symbol->string (hash-keys (hash-ref operation 'facts))) string<?))))

;; ----------------------------------------------------------------------
;; Shared journal/CAS and wave-status helpers
;; ----------------------------------------------------------------------
(define (step! grant operation new-state facts)
  (transition-supplemental-operation! grant
                                      (hash-ref operation 'revision)
                                      (hash-ref operation 'state)
                                      new-state
                                      facts))
(define (live-grant? grant)
  (define operation
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (load-supplemental-operation grant)))
  (and operation
       (not (member (hash-ref operation 'state) '("blocked" "revoked" "completed")))
       (< (current-seconds) (hash-ref operation 'deadline-at))))
(define (wave-for-attempt! base plan wave attempt-id fence status)
  (define rec (load-campaign-record base plan))
  (define w (and rec (current-wave-for-attempt rec wave fence attempt-id)))
  (unless (and rec w)
    (refuse "supplemental allocation is no longer the current campaign attempt"))
  (set-campaign-wave-status! w status)
  (persist-campaign! base rec)
  #t)
(define (park-wave! base plan wave attempt-id fence)
  (with-handlers ([exn:fail? void])
    (wave-for-attempt! base plan wave attempt-id fence 'awaiting-delivery)))
(define (allocation-of operation)
  (hash-ref (hash-ref operation 'facts) 'allocation #f))

;; Request an operator decision; ANY channel/transport failure is an honest
;; typed pause, never a failure and never a synthetic approval.
(define (request-operator-approval ref publisher what)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (values #f (format "operator ~a unavailable: ~a" what (trim-message e))))])
    (values (request-supplemental-approval! ref publisher) #f)))

;; ----------------------------------------------------------------------
;; Author lane: reservation + exactly ONE test-author dispatch + candidate seal
;; ----------------------------------------------------------------------
(define (task-write-paths task)
  (list (hash-ref task 'test-path) (hash-ref task 'harness-path)))
(define (task-expected-paths task)
  (append (task-write-paths task) (hash-ref task 'owner-paths)))
(define (manifest-entry repo path)
  (hasheq 'path
          path
          'mode
          "100644"
          'blob
          (git repo "rev-parse" (string-append "HEAD:" path))
          'sha256
          (sha256-file (build-path repo path))))
;; Every commit between the approved context head and the candidate must be
;; single-purpose: only exact task paths, additions/modifications only.
(define (validate-commit-chain! repo context task)
  (define old-head (hash-ref context 'head))
  (define head (git repo "rev-parse" "HEAD"))
  (unless (and (not (equal? old-head head))
               (git-ok? repo "merge-base" "--is-ancestor" old-head head)
               (git-ok? repo "merge-base" "--is-ancestor" (hash-ref task 'baseline-head) head))
    (refuse "candidate head is not a strict descendant of the approved context/baseline"))
  (define allowed (task-expected-paths task))
  (for ([line (in-list
               (string-split
                (git repo "log" "--format=%H" "--name-status" (string-append old-head ".." head))
                "\n"))]
        #:when (not (equal? line "")))
    (when (string-contains? line "\t")
      (match (string-split line "\t")
        [(list status path)
         (unless (and (member status '("A" "M")) (member path allowed))
           (refuse (format "intervening commit changed forbidden path ~s (~a)" path status)))]
        [other
         (refuse (format "intervening commit recorded an unsupported change entry: ~a" other))]))))
(define (run-supplemental-author-phases! base
                                         plan
                                         wave
                                         attempt-id
                                         fence
                                         run
                                         #:guard guard
                                         #:after-phase [after-phase void])
  (with-handlers ([exn:fail?
                   (lambda (e)
                     ;; A launch is durable: any admission refusal while the
                     ;; protected record is still `launched` spends the task.
                     (define-values (op g _task _tref) (supplemental-active-operation base plan wave))
                     (when (and op g (equal? (hash-ref op 'state) "launched"))
                       (with-handlers ([exn:fail? void])
                         (transition-supplemental-operation! g
                                                             (hash-ref op 'revision)
                                                             "launched"
                                                             "blocked"
                                                             (hasheq 'block-reason
                                                                     (trim-message e 200)))))
                     (wave-execution-outcome 'failed
                                             (string-append "Supplemental evidence task blocked: "
                                                            (trim-message e))))])
    (define repo (current-directory))
    (define pointer
      (or (load-supplemental-pointer base plan wave)
          (refuse "no supplemental pointer for this campaign")))
    (define grant (hash-ref pointer 'task-approval))
    (define tref (hash-ref pointer 'task-proposal))
    (define task (hash-ref (hash-ref (load-supplemental-record tref) 'payload) 'task))
    (define context (hash-ref task 'context))
    ;; The campaign identity guard alone may still name a failed attempt.
    ;; A terminal journal must also invalidate any retained author callback.
    (define (author-live?)
      (and (guard)
           (let ([op (with-handlers ([exn:fail? (lambda (_) #f)])
                       (load-supplemental-operation grant))])
             (and op (member (hash-ref op 'state) '("reserved" "launched")) #t))))
    (unless (same-path? (hash-ref context 'repo) repo)
      (refuse "supplemental task must run in its authenticated retained checkout"))
    (define source (committed-delivery-snapshot repo))
    (unless (and source
                 (for/and ([key '(branch origin head tree)])
                   (equal? (hash-ref source key) (hash-ref context key))))
      (refuse "retained checkout drifted from the approved task context"))
    ;; Initialize the protected operation under the live task approval; the
    ;; journal revalidates the operator authority at every access.
    (define operation
      (or (with-handlers ([exn:fail? (lambda (_) #f)])
            (load-supplemental-operation grant))
          (start-supplemental-operation! grant (hasheq 'context context 'coordinator-fence fence))))
    (unless operation
      (refuse "no live supplemental operation for this campaign"))
    (define state (hash-ref operation 'state))
    (unless (member state '("proposal-approved" "reserved"))
      (refuse "supplemental task already launched; ambiguous or completed launches never re-reserve"))
    (define operation*
      (if (equal? state "proposal-approved")
          (step! grant
                 operation
                 "reserved"
                 (hasheq
                  'allocation
                  (hasheq 'attempt-id attempt-id 'attempt-fence fence 'coordinator-fence fence)))
          (begin
            (unless (equal? (hash-ref (allocation-of operation) 'attempt-id #f) attempt-id)
              ;; A reserved allocation that lost its attempt (crash/restart)
              ;; can never be re-reserved: block the task durably and spend it.
              (with-handlers ([exn:fail? void])
                (transition-supplemental-operation!
                 grant
                 (hash-ref operation 'revision)
                 "reserved"
                 "blocked"
                 (hasheq 'block-reason "reserved allocation lost its attempt; task stays spent")))
              (refuse "bound allocation does not match the current attempt; the task stays spent"))
            operation)))
    ;; Durable launch intent BEFORE the one model dispatch: a crash or lost
    ;; response after this commit can never buy another dispatch.
    (unless (author-live?)
      (refuse "stale campaign authority before supplemental launch"))
    (define launched
      (step! grant
             operation*
             "launched"
             (hasheq 'dispatch-id (format "supplemental-author-~a" attempt-id))))
    (unless (author-live?)
      (refuse "stale campaign authority before model dispatch"))
    (define outcome
      (run-supplemental-test-author! repo (task-write-paths task) attempt-id run author-live? after-phase))
    (if (not (eq? (wave-execution-outcome-kind outcome) 'done))
        (begin
          ;; The launch was durably consumed: a phase that ends without an
          ;; authentic committed candidate (refusal/failure/cancellation)
          ;; spends the task. It is never silently retried and never
          ;; re-reserved; only owner governance may re-plan.
          (with-handlers ([exn:fail? void])
            (transition-supplemental-operation!
             grant
             (hash-ref launched 'revision)
             "launched"
             "blocked"
             (hasheq 'block-reason (trim-message (wave-execution-outcome-message outcome) 200))))
          outcome)
        (let ()
          (define committed (committed-delivery-snapshot repo))
          (unless committed
            (refuse "author phase left the retained checkout uncommitted"))
          (validate-commit-chain! repo context task)
          (for ([path (in-list (task-expected-paths task))])
            (unless (regexp-match? #px"^100644 blob [0-9a-f]{40}\t"
                                   (git repo "ls-tree" "HEAD" "--" path))
              (refuse (format "candidate path ~s is absent or not a regular committed blob" path))))
          (define manifest
            (for/list ([path (in-list (task-expected-paths task))])
              (manifest-entry repo path)))
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
                                              grant
                                              'source
                                              committed
                                              'attempt-id
                                              attempt-id
                                              'attempt-fence
                                              fence
                                              'execution-cwd
                                              (hash-ref context 'repo)
                                              'manifest
                                              manifest)))
          (define kref (propose-retain! "candidate-proposal" candidate (list tref grant)))
          (step! grant
                 (reload-for-step! grant operation*)
                 "candidate-sealed"
                 (hasheq 'candidate-ref kref))
          (wave-execution-outcome
           'done
           "supplemental test evidence committed and candidate sealed; operator candidate approval is next")))))

;; Reload the operation before a transition so the revision/state pair is the
;; durable truth, not a caller-held copy.
(define (reload-for-step! grant operation)
  (or (with-handlers ([exn:fail? (lambda (_) #f)])
        (load-supplemental-operation grant))
      operation))

;; ----------------------------------------------------------------------
;; Advance engine: continuous between operator waits, typed pauses at waits
;; ----------------------------------------------------------------------
(define (candidate-approval-step! operation grant publisher)
  (define kref (hash-ref (hash-ref operation 'facts) 'candidate-ref))
  (unless (and (supplemental-reference? kref) (equal? (hash-ref kref 'kind) "candidate-proposal"))
    (refuse "bound candidate reference is malformed"))
  (define-values (g err) (request-operator-approval kref publisher "candidate approval"))
  (cond
    [(not g) (values 'paused (or err "operator candidate approval unavailable"))]
    [else
     (with-handlers ([exn:fail?
                      (lambda (e)
                        (values 'paused (format "candidate approval refused: ~a" (trim-message e))))])
       (define cap (approve-supplemental-proposal! kref g))
       (unless (and (supplemental-reference? cap) (equal? (hash-ref cap 'kind) "candidate-approval"))
         (raise-pause!
          "candidate approval grant could not be consumed; operator decision required again"))
       (step! grant
              (reload-for-step! grant operation)
              "candidate-approved"
              (hasheq 'candidate-approval-ref cap))
       (values 'next #f))]))

;; Fresh admission only. A durable verifying state spends C1 even if the
;; process died before launch: a new registry cannot prove prior quiescence.
;; Resume must not enter this capture path again.
(define (verification-step! base plan wave operation grant task)
  (unless (equal? (hash-ref operation 'state) "candidate-approved")
    (raise-pause! "ambiguous supplemental verification admission; no C1 relaunch"))
  (define context (hash-ref task 'context))
  (define cwd (hash-ref context 'repo))
  (define c1 (hash-ref task 'effective-command))
  (define verify-bound (hash-ref (hash-ref task 'bounds) 'verify-timeout-sec))
  (define jobs (current-gsd-delivery-verify-jobs))
  (define command-sha
    (sha256-string (string-join (list "/bin/sh" "-c" (apply-verify-jobs c1 jobs)) "\u0")))
  (define intent (hasheq 'execution-cwd cwd 'operational-jobs jobs 'command-sha256 command-sha))
  (define operation* (step! grant operation "verifying" (hasheq 'verify-job intent)))
  (unless (equal? (hash-ref (hash-ref operation* 'facts) 'verify-job) intent)
    (raise-pause! "recorded verification intent differs from the current admission"))
  (define allocation (allocation-of operation*))
  (unless allocation
    (refuse "supplemental operation lost its allocation"))
  (wave-for-attempt! base
                     plan
                     wave
                     (hash-ref allocation 'attempt-id)
                     (hash-ref allocation 'attempt-fence)
                     'verifying)
  (define kref (hash-ref (hash-ref operation* 'facts) 'candidate-ref))
  (define cap-ref (hash-ref (hash-ref operation* 'facts) 'candidate-approval-ref))
  (define contract
    (hasheq 'task-proposal
            (hash-ref operation* 'proposal-ref)
            'task-approval
            grant
            'candidate-proposal
            kref
            'candidate-approval
            cap-ref))
  (define registry (make-verification-registry))
  (define-values (result proposed _capture)
    (parameterize ([current-gsd-verification-registry registry]
                   [current-gsd-delivery-verify-jobs jobs]
                   [current-gsd-delivery-verify-timeout-sec
                    (min (current-gsd-delivery-verify-timeout-sec) verify-bound)]
                   [current-gsd-delivery-branch-context
                    (make-branch-delivery-context #:repo-root (string->path cwd)
                                                  #:branch (hash-ref context 'branch)
                                                  #:base-commit (hash-ref context 'head)
                                                  #:worktree-path (string->path cwd))])
      (dynamic-wind void
                    (lambda ()
                      (capture-supplemental-delivery-verification
                       base
                       plan
                       wave
                       cwd
                       (hash-ref allocation 'attempt-id)
                       (hash-ref allocation 'attempt-fence)
                       contract
                       (lambda ()
                         ;; The owned capture needs the
                         ;; structured verifier outcome.
                         (define check (check-verify-command base wave #f))
                         (delivery-verification (cadr check) (list check) (format "~a" check)))))
                    (lambda ()
                      ;; Exact subprocess-group quiescence before releasing admission.
                      (verification-close-registry! registry)))))
  (define green-ref (hash-ref proposed 'owned-proof))
  ;; Stage the DISTINCT wrapper (T/A/K/G1 + the exact proposed receipt r* and
  ;; its predicted next-generation evidence context E) through the publication
  ;; wave's protected API BEFORE the green-captured journal transition: a late
  ;; WAL cut at that transition then recovers into a state whose staged r*/E
  ;; is already durable, so the authenticated same-operation resume continues
  ;; to R1 without re-running G1. Staging is monotonic/idempotent; r* is
  ;; constructed once here and never rebuilt; its digest never depends on R1.
  (define stage! (publication-api 'stage-supplemental-evidence!))
  (unless stage!
    (raise-pause!
     "supplemental GREEN captured; publication/supersession wave not installed; readiness NOT claimed"))
  (stage! base
          plan
          wave
          (hash-ref allocation 'attempt-id)
          (hash-ref allocation 'attempt-fence)
          contract
          proposed
          green-ref
          task
          (hash-ref (load-supplemental-record kref) 'payload))
  (step! grant operation* "green-captured" (hasheq 'green-proof-ref green-ref)))

(define (probe-step! base plan wave _prior-operation grant task)
  (define operation
    (or (with-handlers ([exn:fail? (lambda (_) #f)])
          (load-supplemental-operation grant))
        _prior-operation))
  (define context (hash-ref task 'context))
  (define repo (hash-ref context 'repo))
  (unless (member (hash-ref operation 'state) '("green-captured" "probing"))
    (refuse "staged probe requires captured GREEN"))
  (define load-staged (publication-api 'load-supplemental-staged-evidence))
  (define staged-record (publication-api 'supplemental-staged-evidence-record))
  (define staged (and load-staged (load-staged base plan wave)))
  (unless (and staged staged-record)
    (refuse "authenticated staged publication wrapper is required"))
  ;; R1 must name the predicted successor E, not the predecessor context
  ;; with only its source fields replaced. The staged API authenticates
  ;; T/A/K/G1 and exact r*/E without pretending that E is current yet.
  (define probe-context (hash-ref (staged-record staged) 'context))
  (define retained-ref (hash-ref (staged-record staged) 'red-proof-ref #f))
  (define ref
    (or retained-ref
        (begin
          (unless (equal? (hash-ref operation 'state) "green-captured")
            (refuse "ambiguous probe launch without retained R1; no relaunch"))
          (consume-staged-probe-launch! base plan wave)
          (run-staged-supplemental-probe! base
                                          plan
                                          wave
                                          repo
                                          staged
                                          "repair-1"
                                          #:guard (lambda () (live-grant? grant))))))
  (unless (and (hash? ref) (hash-ref ref 'sha256 #f))
    ;; Carry the probe's typed code/diagnostic: a blocked probe must be
    ;; diagnosable without touching the retained worker logs.
    (refuse (format "staged baseline overlay probe did not produce structured RED/GREEN: ~a/~a"
                    (hash-ref ref 'status "blocked")
                    (trim-message (or (hash-ref ref 'diagnostic #f) (hash-ref ref 'code #f) "unknown")
                                  300))))
  (define manifest (validate-baseline-probe! repo ref probe-context))
  (unless (equal? (hash-ref manifest 'relation) "baseline-with-test-overlay-v1")
    (refuse "unexpected staged probe relation"))
  ;; Pin R1 into the staged wrapper before recording the capture; the staged
  ;; R1 and the journal fact are one atomic step from the operator's view.
  (define stage-red! (publication-api 'stage-supplemental-red!))
  (unless stage-red!
    (refuse "publication/supersession wave not installed; R1 cannot be staged"))
  (stage-red! base plan wave ref)
  (when (equal? (hash-ref operation 'state) "green-captured")
    (step! grant
           operation
           "probing"
           (hasheq 'probe-job
                   (hasheq 'kind
                           "staged-baseline-overlay-v1"
                           'repo
                           repo
                           'baseline-head
                           (hash-ref task 'baseline-head)))))
  (step! grant (reload-for-step! grant operation) "probe-captured" (hasheq 'probe-proof-ref ref)))

(define (consume-staged-probe-launch! base plan wave)
  (define consume! (publication-api 'consume-supplemental-staged-probe!))
  (unless consume!
    (refuse "staged probe admission is unavailable"))
  (consume! base plan wave))

(define (supersession-step! base plan wave operation grant task publisher)
  (define facts (hash-ref operation 'facts))
  (unless (hash-ref facts 'probe-proof-ref #f)
    (refuse "supersession requires the captured staged probe"))
  ;; S is sealed from the staged wrapper by the publication wave; the
  ;; controller never constructs its own supersession payload.
  (define seal-S! (publication-api 'seal-supplemental-supersession-proposal!))
  (unless seal-S!
    (raise-pause!
     "publication/supersession wave not installed; S cannot be sealed; readiness NOT claimed"))
  (with-handlers ([exn:fail? (lambda (e)
                               (raise-pause! (format "supersession proposal sealing refused: ~a"
                                                     (trim-message e))))])
    (define sref (seal-S! base plan wave))
    (define-values (g err) (request-operator-approval sref publisher "supersession authorization"))
    (cond
      [(not g) (values 'paused (or err "operator supersession authorization unavailable"))]
      [else
       (with-handlers ([exn:fail? (lambda (e)
                                    (values 'paused
                                            (format "supersession authorization refused: ~a"
                                                    (trim-message e))))])
         (define cap (approve-supplemental-proposal! sref g))
         (unless (and (supplemental-reference? cap)
                      (equal? (hash-ref cap 'kind) "supersession-approval"))
           (raise-pause!
            "supersession grant could not be consumed; operator decision required again"))
         (define authorize! (publication-api 'stage-supplemental-supersession-authorization!))
         (unless authorize!
           (raise-pause!
            "publication/supersession wave not installed; S authorization cannot be staged"))
         (authorize! base plan wave sref cap)
         (step! grant
                (reload-for-step! grant operation)
                "supersession-approved"
                (hasheq 'supersession-approval-ref cap 'supersession-proposal-ref sref))
         (values 'next #f))])))

;; Downstream publication/certification/registration is the publication
;; wave's protected owner transition (replay-supplemental-supersession! ->
;; publish-supplemental-supersession!). Integrate via an explicit
;; dynamic-require-with-contract; when the route is absent or refuses,
;; report honestly and pause — never fake publication, certification,
;; registration or readiness.
(define-runtime-path publication-module "supplemental-evidence-publication.rkt")
(define (publication-api name)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define proc (dynamic-require publication-module name))
    (and (procedure? proc) proc)))
(define (publication-step! base plan wave operation grant task)
  (define facts (hash-ref operation 'facts))
  (define sapref (hash-ref facts 'supersession-approval-ref))
  (unless (supplemental-reference? sapref)
    (refuse "publication requires the bound supersession authorization"))
  (define replay! (publication-api 'replay-supplemental-supersession!))
  (unless replay!
    (raise-pause!
     "supersession approved; publication route not installed in this checkout; readiness NOT claimed"))
  (define (read-current)
    (load-campaign-record base plan))
  (with-handlers
      ([exn:fail?
        (lambda (e)
          (raise-pause!
           (format
            "supersession approved; protected publication did not complete (~a); readiness NOT claimed"
            (trim-message e))))])
    (define result (replay! base plan wave read-current sapref (lambda () (live-grant? grant))))
    ;; The publication wave owns the durable protected-operation tail
    ;; (supersession-approved -> published -> receipt-certified) as idempotent
    ;; at-target transitions inside the same S-authorized replay: a late WAL
    ;; cut inside the tail recovers through this same route without
    ;; republishing. The controller only verifies that the operation actually
    ;; moved past publication; readiness is never claimed from a result alone.
    (unless (and (hash? result) (hash-ref result 'publication #f))
      (raise-pause! "protected publication returned no publication record; readiness NOT claimed"))
    (define state
      (hash-ref (or (with-handlers ([exn:fail? (lambda (_) #f)])
                      (load-supplemental-operation grant))
                    operation)
                'state))
    (unless (member state '("published" "receipt-certified" "registered" "completed"))
      (raise-pause! (format "protected publication left the operation at ~a; readiness NOT claimed"
                            state)))))
;; Certified -> registered -> completed is the protected owner promotion
;; transition (P/capability/registration/readiness). The controller
;; integrates it like the publication route: when the module is absent or
;; refuses, report honestly and pause — never fake registration or readiness.
(define-runtime-path promotion-module "supplemental-evidence-promotion.rkt")
(define (promotion-api name)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define proc (dynamic-require promotion-module name))
    (and (procedure? proc) proc)))
(define (promotion-step! base plan wave operation grant task)
  (define facts (hash-ref operation 'facts))
  (define sapref (hash-ref facts 'supersession-approval-ref))
  (unless (supplemental-reference? sapref)
    (refuse "promotion requires the bound supersession authorization"))
  (define promote! (promotion-api 'promote-supplemental-evidence!))
  (unless promote!
    (raise-pause!
     "evidence certified; promotion/registration route not installed in this checkout; readiness NOT claimed"))
  (with-handlers
      ([exn:fail?
        (lambda (e)
          (raise-pause!
           (format
            "evidence certified; protected promotion did not complete (~a); readiness NOT claimed"
            (trim-message e))))])
    (define readiness (promote! base plan wave sapref (lambda () (live-grant? grant))))
    ;; The promotion transition itself admits readiness only after the
    ;; promoted registration validates against the current certified context;
    ;; the controller refuses to claim anything it did not observe.
    (unless (and (hash? readiness) (equal? (hash-ref readiness 'status #f) "ready"))
      (raise-pause! "protected promotion returned no ready readiness; readiness NOT claimed"))))

;; The advance loop: continuous between operator waits; every stop is typed.
;; Returns (values kind detail) with kind in 'paused | 'failed | 'completed.
(define (advance-supplemental-evidence! base
                                        plan
                                        wave
                                        #:publisher
                                        [publisher (current-supplemental-operator-publisher)])
  (define (blocked! grant operation reason)
    (with-handlers ([exn:fail? void])
      (transition-supplemental-operation! grant
                                          (hash-ref operation 'revision)
                                          (hash-ref operation 'state)
                                          "blocked"
                                          (hasheq 'block-reason (trim-message reason 200)))))
  (with-handlers ([exn:supplemental-pause? (lambda (e) (values 'paused (exn-message e)))]
                  [exn:fail? (lambda (e)
                               (define-values (operation grant task _t)
                                 (supplemental-active-operation base plan wave))
                               (when (and operation grant)
                                 (blocked! grant operation e))
                               (values 'failed (trim-message e)))])
    (let loop ([steps 0])
      (when (> steps 16)
        (raise-pause! "supplemental advance exceeded its bounded step count"))
      (define-values (operation grant task _tref) (supplemental-active-operation base plan wave))
      (unless operation
        (refuse "live supplemental operation vanished during advance"))
      (case (hash-ref operation 'state)
        [("proposal-approved" "reserved")
         (raise-pause!
          "supplemental task has not launched; it must run through the author lane attempt")]
        [("launched")
         (blocked! grant operation "launch consumed without captured author output; task stays spent")
         (values 'failed "ambiguous supplemental launch: no captured author output; task spent")]
        [("candidate-sealed")
         (define-values (kind detail) (candidate-approval-step! operation grant publisher))
         (if (eq? kind 'next)
             (loop (add1 steps))
             (values kind detail))]
        [("verifying")
         ;; Without a sealed GREEN transition there is no authenticated
         ;; recovery target here. The old process may have launched C1 (or
         ;; still be running it); matching intent is not launch permission.
         ;; Late GREEN WAL cuts retain their separate reconcile route below.
         (raise-pause!
          "ambiguous supplemental verification: admission already consumed; no C1 relaunch; owner recovery required")]
        [("candidate-approved")
         (begin
           ;; The loop call must stay OUTSIDE this handler: a pause raised by
           ;; a LATER state must not be mislabeled as a C1 verify failure.
           (with-handlers ([exn:fail? (lambda (e)
                                        (blocked! grant operation e)
                                        (values 'failed
                                                (format "supplemental C1 verify failed: ~a"
                                                        (trim-message e))))])
             (verification-step! base plan wave operation grant task))
           (loop (add1 steps)))]
        [("green-captured")
         (with-handlers ([exn:fail? (lambda (e)
                                      (blocked! grant operation e)
                                      (values 'failed
                                              (format "staged probe failed: ~a" (trim-message e))))])
           ;; R1 may run only after the staged wrapper (exact r*/E) exists.
           (define load-staged (publication-api 'load-supplemental-staged-evidence))
           (unless (and load-staged (load-staged base plan wave))
             (raise-pause!
              "supplemental GREEN captured; staged publication wrapper absent; readiness NOT claimed"))
           (probe-step! base plan wave operation grant task))
         ;; Loop OUTSIDE the probe handler (see the verify branch note).
         (loop (add1 steps))]
        [("probing")
         (begin
           (with-handlers ([exn:fail?
                            (lambda (e)
                              (blocked! grant operation e)
                              (values 'failed (format "staged probe failed: ~a" (trim-message e))))])
             (probe-step! base plan wave operation grant task))
           (loop (add1 steps)))]
        [("probe-captured")
         (define-values (kind detail)
           (supersession-step! base plan wave operation grant task publisher))
         (if (eq? kind 'next)
             (loop (add1 steps))
             (values kind detail))]
        [("supersession-approved")
         (publication-step! base plan wave operation grant task)
         (loop (add1 steps))]
        [("recover-pending-verified" "recover-pending-published"
                                     "recover-pending-receipt-certified"
                                     "recover-pending-registration")
         ;; Only the protected journal can install the exact sealed target.
         ;; It revalidates linked approvals under the shared owner lock.
         (reconcile-supplemental-operation! grant
                                            (hash-ref operation 'revision)
                                            (hash-ref operation 'state))
         (loop (add1 steps))]
        [("published")
         (publication-step! base plan wave operation grant task)
         (loop (add1 steps))]
        [("receipt-certified" "registered")
         (promotion-step! base plan wave operation grant task)
         (loop (add1 steps))]
        [("completed")
         (values 'completed
                 "supplemental evidence task completed; readiness owned by the registered receipt")]
        [else
         (values 'paused (format "supplemental operation is ~a" (hash-ref operation 'state)))]))))

;; ----------------------------------------------------------------------
;; Integration outcomes for the three admission seams
;; ----------------------------------------------------------------------
;; owned-entry-block seam: pre-attempt resume WITHOUT re-reserving. #f means
;; "no supplemental business here".
(define (supplemental-entry-outcome base plan wave)
  (cond
    [(supplemental-resumable? base plan wave)
     (let-values ([(kind detail) (advance-supplemental-evidence! base plan wave)])
       (campaign-result (case kind
                          [(failed) 'wave-failed]
                          [else 'wave-blocked])
                        '()
                        detail))]
    [(and (supplemental-pointer-retained? base plan wave)
          (not (supplemental-author-pending? base plan wave)))
     ;; Retention is ownership, NOT permission. Terminal, revoked, expired
     ;; or invalid tasks cannot release this checkout to ordinary authors.
     (campaign-result
      'wave-blocked
      '()
      "retained supplemental task is not launchable; ordinary author fallback refused")]
    [else #f]))
;; Orchestrator done-case seam: continues the flow after the author attempt
;; returns done (candidate approval, G1, R1, S, publication) and parks the
;; allocated attempt at every typed pause.
(define (supplemental-verify-route! base plan wave attempt-id fence)
  (define-values (kind detail) (advance-supplemental-evidence! base plan wave))
  (park-wave! base plan wave attempt-id fence)
  (campaign-result (case kind
                     [(failed) 'wave-failed]
                     [else 'wave-blocked])
                   '()
                   detail))
;; delivery-finalize checkpoint seam: one advance per checkpoint visit.
;; Returns 'advance only on durable private-journal progress; any pause or
;; failure is a typed blocked stop with its reason.
(define (supplemental-checkpoint-step! base plan wave)
  (define (snapshot)
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (load-supplemental-operation (hash-ref (load-supplemental-pointer base plan wave)
                                             'task-approval))))
  (define before (snapshot))
  (define-values (kind detail) (advance-supplemental-evidence! base plan wave))
  (case kind
    [(paused)
     (if (and before (not (equal? before (snapshot))))
         (values 'advance #f)
         (values 'blocked detail))]
    [(failed) (values 'blocked detail)]
    [else (values 'advance #f)]))

;; ----------------------------------------------------------------------
;; wave-scope seam: retain the SAME physical checkout the task context pins
;; ----------------------------------------------------------------------
(define (retained-supplemental-worktree root rec wave)
  (define plan (campaign-plan-id rec))
  (define pointer
    (or (load-supplemental-pointer root plan wave)
        (refuse "supplemental retention requires the campaign pointer")))
  (define task
    (hash-ref (hash-ref (load-supplemental-record (hash-ref pointer 'task-proposal)) 'payload) 'task))
  (let* ([context (hash-ref task 'context)]
         ;; The evidence context carries the exact execution checkout string
         ;; (no trailing separator); running the wave there must stringify
         ;; identically for the author's bound-cwd checks.
         [cwd (string->path (regexp-replace #px"/+$" (hash-ref context 'repo) ""))]
         [source (committed-delivery-snapshot cwd)])
    (unless (and source
                 (for/and ([key '(branch origin head tree)])
                   (equal? (hash-ref source key) (hash-ref context key))))
      (error 'supplemental-evidence-controller
             "exact retained supplemental checkout unavailable; no fallback or rewind"))
    (wave-worktree cwd cwd (hash-ref context 'branch) "origin/main" (build-path root ".planning"))))
