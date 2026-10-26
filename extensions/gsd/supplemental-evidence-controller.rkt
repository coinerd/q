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
         (submod "supplemental-author-contract.rkt" owner)
         (only-in "../gsd-planning/execution-policy.rkt" current-gsd-author-phase)
         (only-in "composition-root.rkt" current-gsd-verification-registry)
         (only-in "verification-job.rkt"
                  make-verification-registry
                  verification-close-registry!
                  verification-registry-incarnation
                  verification-job-command-digest
                  verification-job-exit-code
                  verification-job-id
                  verification-job-log-path
                  verification-job-reaped?
                  verification-job-state)
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
         propose-supplemental-disposition!
         install-supplemental-disposition!
         supplemental-active-operation
         supplemental-discover
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
;; ---- BUG-0101 secondary reporting --------------------------------------
;; The durable "blocked" transition is a SECONDARY outcome of a failure: it
;; is attempted only with the operation/attempt identity captured at entry,
;; persistence is claimed only when the CAS truly committed, and a CAS or
;; storage refusal is reported separately — never as a replacement for the
;; primary cause and never as a silently suppressed error.
(define (blocked-transition! grant operation reason)
  (with-handlers ([exn:fail? (lambda (e) (values #f (trim-message (exn-message e) 200)))])
    (transition-supplemental-operation! grant
                                        (hash-ref operation 'revision)
                                        (hash-ref operation 'state)
                                        "blocked"
                                        (hasheq 'block-reason
                                                (trim-message (if (exn:fail? reason)
                                                                  (exn-message reason)
                                                                  reason)
                                                              200)))
    (values #t #f)))
(define (with-secondary primary secondary)
  (if secondary
      (string-append primary " [blocked not persisted: " secondary "]")
      primary))

;; ---- BUG-0101 unit 2: terminal-observation loss hook -------------------
;; At every typed authority-loss/phase-failure path (the unit-1 secondary
;; sites), the controller BEST-EFFORT records the captured terminal job
;; evidence through the narrow journal writer, authenticating with the
;; CURRENT owner lease/fence (this process runs under the owner). The
;; observation is a SECONDARY outcome: it is never the primary cause, its
;; failure never blocks the typed failure return, and an observation that
;; could not durably record stays EXPLICITLY pending — reported distinctly
;; as "[terminal observation pending: …]" — never silently successful.
;; No captured registry evidence (author-lane dispatch losses, losses before
;; C1) records nothing and reports nothing: evidence is never fabricated.
(define (terminal-evidence registry rec)
  (hasheq 'state
          (verification-job-state rec)
          'exit-code
          (verification-job-exit-code rec)
          'reaped?
          (verification-job-reaped? rec)
          'log-path
          (verification-job-log-path rec)
          'captured-at
          (current-seconds)
          'identities
          (hasheq 'registry-incarnation
                  (symbol->string (verification-registry-incarnation registry))
                  'job-id
                  (symbol->string (verification-job-id rec))
                  'command-digest
                  (verification-job-command-digest rec))))
(define (observation-on-loss! base plan wave grant operation terminal-jobs)
  (define records (unbox terminal-jobs))
  (if (null? records)
      (values #f #f)
      (with-handlers ([exn:fail? (lambda (e) (values #f (trim-message (exn-message e) 200)))])
        (define allocation (allocation-of operation))
        ;; The C1 capture owns exactly one registry job; the primary (first)
        ;; captured record is the observation. Exact operation sha is read
        ;; fresh from the retained snapshot: a blocked transition may already
        ;; have advanced the revision captured at loop entry.
        (record-supplemental-terminal-observation!
         grant
         #:lease (supplemental-coordinator-lease base plan wave)
         #:operation-sha256 (supplemental-retained-operation-sha grant)
         #:attempt-id (hash-ref allocation 'attempt-id)
         #:attempt-fence (hash-ref allocation 'attempt-fence)
         #:coordinator-fence (hash-ref allocation 'coordinator-fence)
         #:terminal (car records)
         #:usage 'unknown)
        (values #t #f))))
(define (with-observation-note detail pending)
  (if pending
      (string-append detail " [terminal observation pending: " pending "]")
      detail))
;; ---- Amendment-3: the expired retained-operation seal --------------------
;; The typed retained-ineligible/expired entry path: a retained NON-terminal
;; operation whose task authority is SPENT (operator deadline expired or the
;; approval revoked) can never advance, and the ordinary blocked CAS can
;; never persist (every transition revalidates the live task grant). Here the
;; controller best-effort seals that operation blocked so the spent terminal
;; record finally exists: try the ordinary blocked transition FIRST, and ONLY
;; when it was refused by the spent task authority AND the CURRENT coordinator
;; lease is available, seal it through the journal's narrow terminalizer.
;; This is a SECONDARY outcome: the typed denial is never masked, and a seal
;; that did not happen stays an explicitly pending note. Idempotent: an
;; already-blocked retained operation adds no note and writes nothing.
(define expired-authority-refusals
  (list
   "task deadline expired"
   "approval requires a valid bounded task and unexpired operator expiry"
   "live authenticated task approval required"
   "linked private approval unavailable, revoked or expired"))
(define (expired-authority-refusal? secondary)
  (and (string? secondary)
       (for/or ([text (in-list expired-authority-refusals)])
         (string-contains? secondary text))))
(define (seal-expired-retained! base plan wave disc)
  (define grant (hash-ref disc 'grant-ref #f))
  (cond
    [(not (and grant
               (memq (hash-ref disc 'status #f) '(retained-ineligible revoked-expired))
               (memq (hash-ref disc 'cause #f) '(expired revoked))))
     #f]
    [else
     (with-handlers ([exn:fail?
                      (lambda (e)
                        (format " [expired operation seal pending: ~a]" (trim-message e 200)))])
       (define operation (supplemental-retained-operation grant))
       (cond
         [(not operation) #f]
         [(member (hash-ref operation 'state) '("blocked" "revoked" "completed")) #f]
         [else
          (define-values (persisted? secondary)
            (blocked-transition! grant operation "spent supplemental task authority"))
          (cond
            [persisted? " [expired retained operation sealed blocked]"]
            [(not (expired-authority-refusal? secondary))
             (format " [expired operation seal pending: ~a]" (or secondary "unknown refusal"))]
            [else
             (define lease (supplemental-coordinator-lease base plan wave))
             (unless lease
               (error 'supplemental-evidence-controller "current coordinator lease unavailable"))
             (define allocation (hash-ref (hash-ref operation 'facts) 'allocation))
             (terminalize-expired-supplemental-operation!
              grant
              #:lease lease
              #:operation-sha256 (hash-ref operation 'sha256)
              #:attempt-id (hash-ref allocation 'attempt-id)
              #:attempt-fence (hash-ref allocation 'attempt-fence)
              #:coordinator-fence (hash-ref allocation 'coordinator-fence))
             " [expired retained operation sealed blocked]"])]))]))
;; One typed-loss settlement: the unit-1 blocked transition (primary
;; secondary outcome) plus the unit-2 best-effort terminal observation.
;; Returns (values persisted? blocked-secondary observation-pending).
(define (settle-typed-loss! base
                            plan
                            wave
                            grant
                            operation
                            reason
                            #:terminal-jobs [terminal-jobs (box '())])
  (define-values (persisted? secondary) (blocked-transition! grant operation reason))
  (define-values (_observed? pending)
    (observation-on-loss! base plan wave grant operation terminal-jobs))
  (values persisted? secondary pending))

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
(define-runtime-path disposition-module "supplemental-evidence-disposition.rkt")
(define (propose-supplemental-disposition! base plan wave parent-sha expires-at)
  ((dynamic-require disposition-module 'propose-supplemental-disposition!) base
                                                                           plan
                                                                           wave
                                                                           parent-sha
                                                                           expires-at))
(define (install-supplemental-disposition! base plan wave proposal approval)
  ((dynamic-require disposition-module 'install-supplemental-disposition!) base
                                                                           plan
                                                                           wave
                                                                           proposal
                                                                           approval))
(define-runtime-path recovery-module "supplemental-evidence-recovery.rkt")
(define (propose-supplemental-recovery! base plan wave expected-parent-proposal-sha expires-at)
  ((dynamic-require recovery-module 'propose-supplemental-recovery!) base
                                                                     plan
                                                                     wave
                                                                     expected-parent-proposal-sha
                                                                     expires-at))
(define (install-supplemental-recovery! base plan wave new-proposal-ref new-approval-ref)
  ((dynamic-require recovery-module 'install-supplemental-recovery!) base
                                                                     plan
                                                                     wave
                                                                     new-proposal-ref
                                                                     new-approval-ref))
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
;; Typed discovery (BUG-0101/R101, security-contracts.md 3.1) + compatible
;; four-value adapter
;; ----------------------------------------------------------------------
;; One immutable tagged hash per campaign. 'status is exactly one of
;; 'absent | 'live | 'pending-wal | 'retained-ineligible | 'revoked-expired
;; | 'unreadable; 'cause is #f or the typed denial 'absent | 'unlaunched |
;; 'expired | 'revoked | 'malformed | 'inaccessible | 'context-mismatch;
;; plus 'message, safe identity ('grant-ref 'proposal-ref 'task
;; 'expires-at and the attempt/fence triple when bound), and the validated
;; record for live/pending views. Read-only and deny-only: discovery never
;; creates stores, never reconciles a WAL and never returns a capability.
;; A retained-but-ineligible authority is TYPED, never conflated with
;; absent, and an absent public pointer with a retained permanent claim
;; stays deny-only. The vocabulary is additive; the four-value API below
;; remains the compatibility surface for every existing consumer.
(define (supplemental-discover base plan wave)
  (define path (pointer-path base plan wave))
  (define pointer-present? (or (link-exists? path) (file-or-directory-type path #f)))
  (define pointer (and pointer-present? (load-supplemental-pointer base plan wave)))
  (define (result status
                  cause
                  message
                  #:view [view #f]
                  #:operation [operation #f]
                  #:grant-ref [grant-ref #f]
                  #:proposal-ref [proposal-ref #f]
                  #:task [task #f]
                  #:expires-at [expires-at #f]
                  #:intent-reconcilable? [reconcilable? #f]
                  #:intent-target [intent-target #f]
                  #:retained-claim? [claim? #f])
    (hasheq 'status
            status
            'cause
            cause
            'message
            message
            'operation
            (or operation (and view (hash-ref view 'operation #f)))
            'grant-ref
            (or grant-ref (and view (hash-ref view 'grant-ref #f)))
            'proposal-ref
            (or proposal-ref (and view (hash-ref view 'proposal-ref #f)))
            'task
            (or task (and view (hash-ref view 'task #f)))
            'expires-at
            (or expires-at (and view (hash-ref view 'expires-at #f)))
            'attempt-id
            (and view (hash-ref view 'attempt-id #f))
            'attempt-fence
            (and view (hash-ref view 'attempt-fence #f))
            'coordinator-fence
            (and view (hash-ref view 'coordinator-fence #f))
            'intent-reconcilable?
            (or reconcilable? (and view (hash-ref view 'intent-reconcilable? #f)))
            'intent-target
            (or intent-target (and view (hash-ref view 'intent-target #f)))
            'retained-claim?
            claim?))
  (cond
    [(not pointer-present?)
     ;; No public pointer. A retained permanent claim still owns the campaign
     ;; (deny-only) but never produces a live result here.
     (define claim?
       (with-handlers ([exn:fail? (lambda (_) #t)]) ; unreadable claim state denies
         (supplemental-recovery-pointer-claimed? path)))
     (result 'absent
             'absent
             (if claim?
                 "no public supplemental pointer; a retained claim still denies ordinary fallback"
                 "no public supplemental pointer for this campaign")
             #:retained-claim? claim?)]
    [(not pointer)
     ;; The pointer exists but cannot be authenticated: distinguish access
     ;; denial from a malformed public pointer. Both deny.
     (define-values (cause message)
       (with-handlers ([exn:fail:filesystem:errno?
                        (lambda _
                          (values 'inaccessible "public supplemental pointer is inaccessible"))]
                       [exn:fail? (lambda (e)
                                    (values 'malformed
                                            (format "malformed public supplemental pointer: ~a"
                                                    (trim-message e))))])
         (define raw (call-with-input-file path read-json))
         (unless (and (hash? raw) (valid-pointer? (freeze-json raw)))
           (error 'supplemental-evidence-controller "malformed pointer schema"))
         (values 'malformed "malformed public supplemental pointer")))
     (result (if (eq? cause 'inaccessible) 'unreadable 'retained-ineligible) cause message)]
    [else
     (define aref (hash-ref pointer 'task-approval))
     (define tref (hash-ref pointer 'task-proposal))
     (define view (supplemental-operation-view aref))
     (define status (hash-ref view 'status))
     (cond
       [(memq status '(absent retained-ineligible revoked-expired unreadable)) view]
       [else
        ;; live / pending-wal: authenticate the campaign context exactly
        ;; like the historical four-value discovery, with typed denials.
        (define-values (ok? cause message)
          (supplemental-context-admitted? base plan wave aref tref view))
        (if ok?
            view
            (result 'retained-ineligible cause message #:view view))])]))
;; Typed campaign-context authentication for a live/pending view: the exact
;; admission conjunction of the historical four-value discovery (same
;; checks, same order, same refusal text), reporting (values ok? cause
;; message). The pointer must also name exactly the approved proposal; a
;; drifted public pointer is a typed context denial, never an authority.
(define (supplemental-context-admitted? base plan wave aref tref view)
  ;; The typed outcome escapes as ONE list: escape continuations (call/ec)
  ;; never carry multiple values (Racket CS result arity).
  (apply
   values
   (call/ec
    (lambda (deny)
      (define task (hash-ref view 'task))
      (unless (equal? (hash-ref view 'proposal-ref #f) tref)
        (deny (list #f
                    'context-mismatch
                    "public pointer names a different task proposal than the approved task")))
      (define context (hash-ref task 'context))
      (define journal
        (with-handlers
            ([exn:fail? (lambda (e)
                          (deny (list #f
                                      (if (and (exn:fail:filesystem:errno? e)
                                               (pair? (exn:fail:filesystem:errno-errno e))
                                               (let ([code (car (exn:fail:filesystem:errno-errno e))])
                                                 (or (= code 13) (= code 1))))
                                          'inaccessible
                                          'malformed)
                                      (format "delivery journal unreadable: ~a" (trim-message e)))))])
          (or (load-delivery-journal base plan wave)
              (deny (list #f 'malformed "delivery journal is absent for this campaign")))))
      (define receipt (hash-ref journal 'receipt #f))
      ;; Authenticate the physical port through the exact predecessor
      ;; receipt pinned by T, not through CURRENT (which may already be
      ;; this operation's G1 while its private WAL still awaits reconcile).
      ;; Loading G1 here would circularly require a settled operation just
      ;; to route that operation to its authenticated reconcile path.
      (define history (hash-ref journal 'receipt-history '()))
      (define generation (hash-ref context 'receipt-generation))
      (define pinned-receipt
        (and (<= generation (length history))
             (if (= generation (length history))
                 receipt
                 (list-ref history generation))))
      (define manifest
        (with-handlers
            ([exn:fail:filesystem:errno?
              (lambda _ (deny (list #f 'inaccessible "campaign receipt evidence inaccessible")))]
             [exn:fail? (lambda (e)
                          ;; A tampered/unreadable owned proof can never
                          ;; authenticate the campaign context: typed denial.
                          (deny (list #f 'context-mismatch (trim-message (exn-message e)))))])
          (and pinned-receipt
               (equal? (canonical-evidence-digest pinned-receipt) (hash-ref context 'receipt-sha256))
               (equal? (hash-ref (hash-ref pinned-receipt 'owned-proof) 'sha256)
                       (hash-ref context 'green-proof-sha256))
               (load-receipt-owned-proof pinned-receipt base plan wave))))
      (define operation-facts
        (hash-ref (or (hash-ref view 'operation #f) (hasheq 'facts (hasheq))) 'facts (hasheq)))
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
        (define sapref (hash-ref operation-facts 'supersession-approval-ref #f))
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
        (deny (list #f
                    'context-mismatch
                    "supplemental task context does not match this campaign's receipt")))
      (list #t #f #f)))))
;; Four-value compatibility adapter (security-contracts.md 3.1). Live and
;; typed-pending results yield the original four values byte-identically;
;; every denial — including an absent public pointer retained by a
;; permanent claim — stays deny-only #f. An EARLY fail-closed write-ahead
;; cut keeps the journal's historical observe-and-settle contract HERE in
;; the adapter (never in discovery): one authorized load performs the
;; journal's blocked settlement exactly once and the typed view is re-read.
(define (supplemental-active-operation base plan wave)
  (define disc (supplemental-discover base plan wave))
  (define status (hash-ref disc 'status))
  (define (live-values d)
    (values (hash-ref d 'operation #f)
            (hash-ref d 'grant-ref #f)
            (hash-ref d 'task #f)
            (hash-ref d 'proposal-ref #f)))
  (cond
    [(eq? status 'live) (live-values disc)]
    [(and (eq? status 'pending-wal) (hash-ref disc 'intent-reconcilable? #f)) (live-values disc)]
    [(eq? status 'pending-wal)
     (with-handlers ([exn:fail? (lambda (_) (values #f #f #f #f))])
       (load-supplemental-operation (hash-ref disc 'grant-ref))
       (define settled (supplemental-discover base plan wave))
       (if (eq? (hash-ref settled 'status) 'live)
           (live-values settled)
           (values #f #f #f #f)))]
    [else (values #f #f #f #f)]))
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
  (define launch-identity (box #f))
  (with-handlers ([exn:fail? (lambda (e)
                               ;; Safe identity is the durably launched record captured
                               ;; at launch (BUG-0101): the handler never redisCOVERS
                               ;; live authority merely to report the original error,
                               ;; and a refused blocked transition is reported as a
                               ;; separate secondary outcome, never silently dropped.
                               (define id (unbox launch-identity))
                               (define primary
                                 (string-append "Supplemental evidence task blocked: "
                                                (trim-message (exn-message e))))
                               (define-values (persisted? secondary)
                                 (if id
                                     (blocked-transition! (car id) (cdr id) e)
                                     (values #f #f)))
                               (wave-execution-outcome 'failed
                                                       (if persisted?
                                                           primary
                                                           (with-secondary primary secondary))))])
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
    (define contract (make-supplemental-author-contract tref grant repo attempt-id base plan wave))
    (define launched
      (step! grant
             operation*
             "launched"
             (hasheq 'dispatch-id (format "supplemental-author-~a" attempt-id))))
    ;; Capture the durable launched identity (BUG-0101): the failure handler
    ;; above uses THIS record, never a rediscovery, to settle the task.
    (set-box! launch-identity (cons grant launched))
    (unless (author-live?)
      (refuse "stale campaign authority before model dispatch"))
    (define outcome
      (run-supplemental-test-author! repo contract attempt-id run author-live? after-phase))
    (if (not (eq? (wave-execution-outcome-kind outcome) 'done))
        (let-values ([(persisted? secondary)
                      ;; The launch was durably consumed: a phase that ends
                      ;; without an authentic committed candidate
                      ;; (refusal/failure/cancellation) spends the task. It
                      ;; is never silently retried and never re-reserved;
                      ;; only owner governance may re-plan. A refused
                      ;; blocked transition is a reported secondary, not a
                      ;; silent one.
                      (blocked-transition! grant launched (wave-execution-outcome-message outcome))])
          (wave-execution-outcome (wave-execution-outcome-kind outcome)
                                  (if persisted?
                                      (wave-execution-outcome-message outcome)
                                      (with-secondary (wave-execution-outcome-message outcome)
                                                      secondary))))
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
(define (verification-step! base plan wave operation grant task #:terminal-jobs [terminal-jobs #f])
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
                      ;; Exact subprocess-group quiescence before releasing admission,
                      ;; and BUG-0101 unit 2: capture the terminal registry records
                      ;; (first-terminal attribution, frozen by the registry) so a
                      ;; loss handler can observe them without touching the closed
                      ;; registry. Never a fabricated fact: a run with no jobs
                      ;; captures nothing.
                      (define terminal-records (verification-close-registry! registry))
                      (when terminal-jobs
                        (set-box! terminal-jobs
                                  (for/list ([rec (in-list terminal-records)])
                                    (terminal-evidence registry rec))))))))
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
;; BUG-0101 failure discipline:
;;   - phase-local handlers (C1/probe) return with the ORIGINAL cause; the
;;     loop is re-entered only after the explicit success value, or when the
;;     phase's own interrupted write left the journal's typed recoverable
;;     cut (that cut is reconciled in the same advance — never parked,
;;     never relaunched) — never blindly after a semantic failure;
;;   - a typed pause (exn:supplemental-pause) raised inside a phase passes
;;     through untouched and remains a pause, never a failure or a blocked
;;     transition;
;;   - safe operation/attempt identity is captured at each loop entry; the
;;     outer handler never redisCOVERS live authority merely to report the
;;     original error;
;;   - the durable blocked transition is a SECONDARY outcome: CAS/storage
;;     refusals are reported separately, never replacing the primary cause
;;     and never claiming blocked persisted when it was not;
;;   - retained-but-ineligible authority is typed in the failure message and
;;     never reported as vanished.
(define (advance-supplemental-evidence! base
                                        plan
                                        wave
                                        #:publisher
                                        [publisher (current-supplemental-operator-publisher)])
  (define identity (box #f)) ; (cons grant operation) captured at each loop entry
  (define terminal-jobs (box '())) ; captured C1 registry terminal evidence, reset per loop
  (with-handlers
      ([exn:supplemental-pause? (lambda (e) (values 'paused (exn-message e)))]
       [exn:fail?
        (lambda (e)
          (define primary (trim-message (exn-message e)))
          (define id (unbox identity))
          (define-values (persisted? secondary obs-pending)
            (if id
                (settle-typed-loss! base plan wave (car id) (cdr id) e #:terminal-jobs terminal-jobs)
                (values #f #f #f)))
          (values 'failed
                  (with-observation-note (if persisted?
                                             primary
                                             (with-secondary primary secondary))
                                         obs-pending)))])
    (let loop ([steps 0])
      (when (> steps 16)
        (raise-pause! "supplemental advance exceeded its bounded step count"))
      (set-box! terminal-jobs '())
      (define disc (supplemental-discover base plan wave))
      (define status (hash-ref disc 'status))
      (unless (memq status '(live pending-wal))
        ;; Retained-but-ineligible authority is never reported as vanished
        ;; (BUG-0101): the typed denial is the honest stop.
        (refuse (format "retained supplemental operation is not eligible (~a): ~a"
                        (hash-ref disc 'cause)
                        (hash-ref disc 'message))))
      (when (and (eq? status 'pending-wal) (not (hash-ref disc 'intent-reconcilable? #f)))
        ;; Fail-closed early-cut settlement: the mutation-authorized advance
        ;; observes the sealed intent through the journal's historical
        ;; load-and-settle exactly once (never a relaunch, never a retry);
        ;; discovery itself stays read-only.
        (load-supplemental-operation (hash-ref disc 'grant-ref))
        (set! disc (supplemental-discover base plan wave))
        (set! status (hash-ref disc 'status))
        (unless (memq status '(live pending-wal))
          (refuse (format "retained supplemental operation is not eligible (~a): ~a"
                          (hash-ref disc 'cause)
                          (hash-ref disc 'message)))))
      (define operation (hash-ref disc 'operation #f))
      (define grant (hash-ref disc 'grant-ref #f))
      (define task (hash-ref disc 'task #f))
      (unless operation
        (refuse "live supplemental operation vanished during advance"))
      (set-box! identity (cons grant operation))
      ;; Phase-local handler (BUG-0101): typed pauses pass through untouched;
      ;; any other failure returns with the ORIGINAL cause and a separately
      ;; reported blocked-transition outcome. ONE exception is routed back to
      ;; the loop: when THIS phase's write was interrupted leaving the
      ;; journal's own typed recoverable cut (pending-wal, reconcilable), the
      ;; same advance continues through the journal's authenticated
      ;; reconciliation of the SAME operation — never a relaunch, never a
      ;; redispatch, never a parked run (the cut is the journal's explicit
      ;; recoverable outcome, not a semantic phase failure). Every other
      ;; failure returns immediately; the loop is never re-entered blindly.
      (define (phase! run wrap)
        (with-handlers
            ([exn:supplemental-pause? (lambda (e) (raise e))]
             [exn:fail?
              (lambda (e)
                (define primary (wrap (trim-message (exn-message e))))
                (define-values (persisted? secondary obs-pending)
                  (settle-typed-loss! base plan wave grant operation e #:terminal-jobs terminal-jobs))
                (define detail
                  (with-observation-note (if persisted?
                                             primary
                                             (with-secondary primary secondary))
                                         obs-pending))
                (define post (supplemental-discover base plan wave))
                (if (and (eq? (hash-ref post 'status) 'pending-wal)
                         (hash-ref post 'intent-reconcilable? #f)
                         (equal? (hash-ref post 'grant-ref #f) grant))
                    (values 'recover detail)
                    (values 'failed detail)))])
          (values 'success (run))))
      (case (hash-ref operation 'state)
        [("proposal-approved" "reserved")
         (raise-pause!
          "supplemental task has not launched; it must run through the author lane attempt")]
        [("launched")
         (define reason "launch consumed without captured author output; task stays spent")
         (define primary "ambiguous supplemental launch: no captured author output; task spent")
         (define-values (persisted? secondary obs-pending)
           (settle-typed-loss! base plan wave grant operation reason #:terminal-jobs terminal-jobs))
         (values 'failed
                 (with-observation-note (if persisted?
                                            primary
                                            (with-secondary primary secondary))
                                        obs-pending))]
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
         (define-values (phase-kind detail)
           (phase!
            (lambda ()
              (verification-step! base plan wave operation grant task #:terminal-jobs terminal-jobs))
            (lambda (msg) (format "supplemental C1 verify failed: ~a" msg))))
         ;; Continuation is OUTSIDE the handler and only on explicit success
         ;; (or the journal's own typed recoverable cut): a pause raised by a
         ;; LATER state must not be mislabeled as a C1 verify failure, and a
         ;; semantic C1 failure never recurses the loop.
         (if (eq? phase-kind 'failed)
             (values 'failed detail)
             (loop (add1 steps)))]
        [("green-captured")
         (define-values (phase-kind detail)
           (phase!
            (lambda ()
              ;; R1 may run only after the staged wrapper (exact r*/E) exists.
              (define load-staged (publication-api 'load-supplemental-staged-evidence))
              (unless (and load-staged (load-staged base plan wave))
                (raise-pause!
                 "supplemental GREEN captured; staged publication wrapper absent; readiness NOT claimed"))
              (probe-step! base plan wave operation grant task))
            (lambda (msg) (format "staged probe failed: ~a" msg))))
         (if (eq? phase-kind 'failed)
             (values 'failed detail)
             (loop (add1 steps)))]
        [("probing")
         (define-values (phase-kind detail)
           (phase! (lambda () (probe-step! base plan wave operation grant task))
                   (lambda (msg) (format "staged probe failed: ~a" msg))))
         (if (eq? phase-kind 'failed)
             (values 'failed detail)
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
     ;; A spent NON-terminal retained operation is sealed blocked here
     ;; (secondary, best-effort, idempotent) so the spent terminal record
     ;; exists; the typed denial itself is unchanged.
     (define detail
       (let ([note (seal-expired-retained! base plan wave (supplemental-discover base plan wave))])
         (if note
             (string-append
              "retained supplemental task is not launchable; ordinary author fallback refused"
              note)
             "retained supplemental task is not launchable; ordinary author fallback refused")))
     (campaign-result 'wave-blocked '() detail)]
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
