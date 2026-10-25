#lang racket/base
;; STABILITY: internal
;; Protected supplemental operation storage (BUG-0088 A3), not a controller.
;;
;; start-supplemental-operation! grant-ref immutable-base-binding -> record
;;   Validates a private task-approval and task proposal. Proposal payload must be
;;   exactly {task: sealed supplemental task/v1, expires-at: Unix seconds}; expiry
;;   is operator-approved, positive, within the task deadline (at most one day).
;;   Base binding must equal task.context or contain that exact value at `context`;
;;   additional owner guards are pinned, not inferred as true by storage. The first
;;   base binding is permanent; identical initialization never resets it.
;; transition-supplemental-operation! grant-ref revision state new-state facts
;;   CAS under an OS file lock. States are strings or their corresponding symbols;
;;   facts are an immutable JSON hash, append-only, including nested values.
;; load-supplemental-operation grant-ref -> record or #f
;;   Also revalidates live authority and recovers interrupted writes fail-closed.
;;
;; The embedding must bind current-supplemental-authority-root, never a project
;; context root. Its authority validator is the sole approval source; there is no
;; caller guard, approved boolean, state import, or journal-root parameter.
;; The authority's authenticated transport enforces token expiry on issuance.
;; This journal requires an approved absolute operation expiry as well as the
;; exact finite task bounds, so retained approval references cannot live forever.
;;
;; CONTROLLER PRECONDITIONS / INTEGRATION STILL REQUIRED:
;; Hold the campaign lease; validate source/base/receipt/fences/cancellation and
;; allocate the attempt through the real monotonic owner API. Bind that result as
;; `allocation` at reserved. Commit launched BEFORE dispatching the model, only
;; when that CAS returned launched to this caller. Initialization/readback is NOT
;; a dispatch instruction. No transition permits a second allocation or launch.
;; The owner supplies dispatch IDs, authenticated live-job captures and immutable
;; proof/publication/receipt/registration descriptors. These facts and state names
;; DO NOT prove job ownership, subprocess quiescence, proof semantics, publication,
;; certification, readiness, or delivery. Those semantic checks and the actual
;; delivery-journal supersession/promotion remain the parent controller's job.
;;
;; Interrupted write-ahead intents recover to typed states, preserving all
;; bound facts and consuming launch permission. Never automatically replay an
;; effect. A post-commit exception is therefore an ambiguous effect, NOT retry
;; authority. Linux/Unix fsync + rename and OS advisory file locks protect
;; process crashes; same-uid hostile code / compromised trusted embedding are
;; not isolated here.
;;
;; BUG-0088 blockers fixed here:
;; - ONE shared root-private admission/revocation critical section: every
;;   journal admission/write revalidates authority and commits inside the SAME
;;   authority lock (extensions/gsd/supplemental-evidence-authority.rkt) that
;;   revocation takes. A revocation therefore can never interleave between an
;;   admission recheck and its durable commit.
;; - Typed recoverable pending states for LATE cuts: an interrupted intent
;;   whose target already holds fully bound captured facts (green-captured,
;;   published, receipt-certified, registered) recovers as an explicit typed
;;   pending view (recover-pending-verified / -published /
;;   -receipt-certified / -registration) instead of terminal blocked. The
;;   sealed intent stays on disk until reconcile-supplemental-operation!
;;   completes the SAME operation: authority-validated, exact expected
;;   pending state/revision (CAS), operator linked approvals (including the
;;   supersession approval) revalidated live, exact sealed intent record
;;   installed, no new facts, no caller guard/boolean, and never a relaunch,
;;   reallocation or re-dispatch. Early ambiguous model/job-consumed cuts
;;   (everything through verifying/probing admission and completion) remain
;;   fail-closed blocked. Storage facts stay descriptors, never proof.
(require racket/file
         racket/path
         "supplemental-evidence-store.rkt"
         "supplemental-evidence-format.rkt"
         "supplemental-evidence-authority.rkt"
         "supplemental-evidence-store-core.rkt"
         "supplemental-disposition-claim.rkt"
         (only-in "../gsd-planning/execution-policy.rkt" current-gsd-author-phase))
(provide start-supplemental-operation!
         transition-supplemental-operation!
         load-supplemental-operation
         supplemental-operation-view
         reconcile-supplemental-operation!
         recovery-pending-state-name
         current-supplemental-journal-crash-cut
         inspect-supplemental-entry-operation
         inspect-supplemental-recovery-parent
         inspect-supplemental-disposition-parent
         install-supplemental-disposition-claim!
         install-supplemental-recovery-claim!
         supplemental-recovery-pointer-claimed?)

;; Journal-local refusal identity for the WAL/transition checks that remain
;; here; the extracted storage core raises with the same source symbol.
(define (refuse message)
  (error 'supplemental-evidence-journal "~a" message))
;; Immutable lifecycle state tables and the storage/validation kernel now live
;; in the acyclic supplemental-evidence-store-core helper (BUG-0090), which is
;; also a direct dependency of the disposition claim: the journal -> claim ->
;; core graph is static and acyclic, so first use can no longer re-enter
;; partial instantiation through the old lazy dynamic-require seam.
;; Historical inspection is deliberately PURE: no operation lock and no WAL
;; reconciliation. The authority lock excludes all possible terminal writers.
(define (inspect-supplemental-entry-operation grant)
  (call-with-supplemental-authority-lock
   (lambda ()
     (define-values (proposal task expiry) (authenticated-task! grant))
     (define-values (_dir snapshot intent _lock) (paths (root!) grant))
     (when (present? intent)
       (refuse "entry cannot reconcile pending intent"))
     (and (present? snapshot) (validate-record! (read-sealed snapshot) grant proposal task expiry)))))
;; ---- BUG-0101/R101 typed discovery view (strictly READ-ONLY) ----------
;; One tagged, immutable view of a retained operation for the controller's
;; typed discovery (security-contracts.md 3.1). Contract:
;;   'status in 'absent | 'live | 'pending-wal | 'retained-ineligible
;;                    | 'revoked-expired | 'unreadable
;;   'cause  in #f | 'absent | 'unlaunched | 'expired | 'revoked
;;                  | 'malformed | 'inaccessible
;; plus 'message and safe identity only ('grant-ref 'proposal-ref 'task
;; 'expires-at, the validated sealed record when readable, and the
;; attempt/fence triple from the bound allocation fact when present).
;; This view NEVER creates stores (paths create? #f), NEVER reconciles or
;; settles a write-ahead intent (any sealed intent stays byte-identical on
;; disk), NEVER mints or returns a capability: ownership can only deny. The
;; fail-closed early-cut settlement that the historical load-on-observe
;; performed stays in the journal's write path (under-lock), not here. The
;; snapshot is taken inside the ONE shared admission/revocation section, so
;; a concurrent revocation can never split the view; the operation-local
;; write lock is deliberately not taken.
(define (fs-errno-code e)
  (and (exn:fail:filesystem:errno? e)
       (pair? (exn:fail:filesystem:errno-errno e))
       (eq? (cdr (exn:fail:filesystem:errno-errno e)) 'posix)
       (car (exn:fail:filesystem:errno-errno e))))
(define (view-storage-cause e [default 'malformed])
  ;; EACCES/EPERM (posix 13/1) is inaccessible; absence (ENOENT 2),
  ;; integrity refusals and every other read failure fail closed as
  ;; retained-malformed (DEFAULT), unless the layer is the authority root
  ;; itself, where a trust/mode failure denies as inaccessible.
  (let ([code (fs-errno-code e)])
    (cond
      [(and code (or (= code 13) (= code 1))) 'inaccessible]
      [code 'malformed]
      [else default])))
(define (storage-denial-status cause)
  (if (eq? cause 'inaccessible) 'unreadable 'retained-ineligible))
(define (supplemental-operation-view grant)
  (define (brief e)
    (define m (exn-message e))
    (string->immutable-string (substring m 0 (min 300 (string-length m)))))
  (define (finish status
                  cause
                  message
                  #:operation [operation #f]
                  #:proposal-ref [proposal-ref #f]
                  #:task [task #f]
                  #:expires-at [expires-at #f]
                  #:intent-reconcilable? [reconcilable? #f]
                  #:intent-target [intent-target #f])
    (define allocation
      (and operation
           (let ([a (hash-ref (hash-ref operation 'facts) 'allocation #f)])
             (and (exact-keys? a '(attempt-id attempt-fence coordinator-fence)) a))))
    (hasheq 'status
            status
            'cause
            cause
            'message
            message
            'operation
            operation
            'grant-ref
            grant
            'proposal-ref
            proposal-ref
            'task
            task
            'expires-at
            expires-at
            'intent-reconcilable?
            reconcilable?
            'intent-target
            intent-target
            'attempt-id
            (and allocation (hash-ref allocation 'attempt-id))
            'attempt-fence
            (and allocation (hash-ref allocation 'attempt-fence))
            'coordinator-fence
            (and allocation (hash-ref allocation 'coordinator-fence))))
  (call/ec
   (lambda (done)
     (when (current-gsd-author-phase)
       (done
        (finish 'unreadable 'inaccessible "author phase cannot inspect the supplemental journal")))
     (define root
       (with-handlers ([exn:fail?
                        (lambda (e)
                          (done (finish 'unreadable
                                        (view-storage-cause e 'inaccessible)
                                        (format "supplemental authority root inaccessible: ~a"
                                                (brief e)))))])
         (or (root! #t) (done (finish 'absent 'absent "no private supplemental authority root")))))
     (with-handlers ([exn:fail?
                      (lambda (e)
                        (done (finish 'unreadable
                                      (view-storage-cause e 'inaccessible)
                                      (format "supplemental authority section inaccessible: ~a"
                                              (brief e)))))])
       (call-with-supplemental-authority-lock
        (lambda ()
          ;; 1. The private approval record must be readable at all before
          ;; revocation can be distinguished from access denial.
          (with-handlers ([exn:fail? (lambda (e)
                                       (define cause (view-storage-cause e))
                                       (done (finish (storage-denial-status cause)
                                                     cause
                                                     (format "task approval record unreadable: ~a"
                                                             (brief e)))))])
            (load-supplemental-record grant))
          ;; 2. Live unrevoked approval. Revocation shares this section, so
          ;; a #f here is revoked/absent/foreign origin: typed denial.
          (define approval
            (or (validate-supplemental-approval grant)
                (done (finish
                       'revoked-expired
                       'revoked
                       "task approval is revoked, absent or foreign to this authority origin"))))
          (define proposal-ref (hash-ref approval 'proposal))
          (define proposal
            (with-handlers ([exn:fail? (lambda (e)
                                         (define cause (view-storage-cause e))
                                         (done (finish (storage-denial-status cause)
                                                       cause
                                                       (format "retained task proposal unreadable: ~a"
                                                               (brief e)))))])
              (load-supplemental-record proposal-ref)))
          (define payload (hash-ref proposal 'payload #f))
          (define task (and (hash? payload) (hash-ref payload 'task #f)))
          (define expires-at (and (hash? payload) (hash-ref payload 'expires-at #f)))
          (unless (and (equal? (hash-ref proposal-ref 'kind) "task-proposal")
                       (exact-keys? payload '(task expires-at))
                       (valid-supplemental-task? task)
                       (exact-positive-integer? expires-at))
            (done (finish 'retained-ineligible
                          'malformed
                          "retained task proposal is not a valid bounded task"
                          #:proposal-ref proposal-ref
                          #:task task
                          #:expires-at expires-at)))
          ;; 3. Operator expiry: the exact admission boundary. now >= expiry
          ;; is ALREADY expired (fail closed), matching the journal's
          ;; `(< (current-seconds) expires-at)` admission check exactly.
          (unless (< (current-seconds) expires-at)
            (done (finish 'retained-ineligible
                          'expired
                          (format "retained task approval expired at ~a" expires-at)
                          #:proposal-ref proposal-ref
                          #:task task
                          #:expires-at expires-at)))
          (unless (<= expires-at
                      (+ (current-seconds) (hash-ref (hash-ref task 'bounds) 'deadline-sec)))
            (done (finish 'retained-ineligible
                          'malformed
                          "retained task expiry exceeds the task deadline bound"
                          #:proposal-ref proposal-ref
                          #:task task
                          #:expires-at expires-at)))
          ;; 4. Storage, read-only, never created. A missing operations
          ;; directory is an UNLAUNCHED approved task, never a denial.
          (unless (present? (build-path root "supplemental-operations"))
            (done (finish 'absent
                          'unlaunched
                          "live approved task has no protected operation yet"
                          #:proposal-ref proposal-ref
                          #:task task
                          #:expires-at expires-at)))
          (define-values (dir snapshot intent _lock)
            (with-handlers ([exn:fail?
                             (lambda (e)
                               (when (= 2 (or (fs-errno-code e) -1))
                                 (done (finish 'absent
                                               'unlaunched
                                               "live approved task has no protected operation yet"
                                               #:proposal-ref proposal-ref
                                               #:task task
                                               #:expires-at expires-at)))
                               (raise e))])
              (paths root grant #f)))
          (define old
            (and (present? snapshot)
                 (with-handlers ([exn:fail?
                                  (lambda (e)
                                    (define cause (view-storage-cause e))
                                    (done (finish (storage-denial-status cause)
                                                  cause
                                                  (format "retained operation record unreadable: ~a"
                                                          (brief e))
                                                  #:proposal-ref proposal-ref
                                                  #:task task
                                                  #:expires-at expires-at)))])
                   (validate-record! (read-sealed snapshot) grant proposal-ref task expires-at))))
          (define wal
            (and (present? intent)
                 (with-handlers ([exn:fail?
                                  (lambda (e)
                                    (define cause (view-storage-cause e))
                                    (done (finish (storage-denial-status cause)
                                                  cause
                                                  (format "retained write-ahead intent unreadable: ~a"
                                                          (brief e))
                                                  #:operation old
                                                  #:proposal-ref proposal-ref
                                                  #:task task
                                                  #:expires-at expires-at)))])
                   (define w (read-sealed intent))
                   (unless (and (exact-keys? w '(schema from-revision from-state next sha256))
                                (equal? (hash-ref w 'schema) "supplemental-operation-intent/v1"))
                     (refuse "invalid write-ahead intent"))
                   w)))
          (define (intent-unreadable! operation)
            (done (finish 'retained-ineligible
                          'malformed
                          "write-ahead intent revision mismatch"
                          #:operation operation
                          #:proposal-ref proposal-ref
                          #:task task
                          #:expires-at expires-at)))
          (cond
            [(not wal)
             (unless old
               (done (finish 'absent
                             'unlaunched
                             "live approved task has no protected operation yet"
                             #:proposal-ref proposal-ref
                             #:task task
                             #:expires-at expires-at)))
             (finish 'live
                     #f
                     "authenticated retained operation"
                     #:operation old
                     #:proposal-ref proposal-ref
                     #:task task
                     #:expires-at expires-at)]
            [else
             ;; A pending view is DERIVED exactly like the journal's own
             ;; read-only recovery of a typed recoverable cut; nothing is
             ;; written and no intent is settled here.
             (define next
               (with-handlers ([exn:fail? (lambda (e) (intent-unreadable! old))])
                 (validate-record! (hash-ref wal 'next) grant proposal-ref task expires-at)))
             (define from-matches?
               (and (= (hash-ref wal 'from-revision)
                       (if old
                           (hash-ref old 'revision)
                           -1))
                    (equal? (hash-ref wal 'from-state)
                            (if old
                                (hash-ref old 'state)
                                "absent"))))
             (define from-extends?
               (and from-matches?
                    (= (hash-ref next 'revision) (add1 (hash-ref wal 'from-revision)))
                    (if old
                        (and (binding-extends? old next)
                             (transition-allowed? (hash-ref old 'state) (hash-ref next 'state)))
                        (equal? (hash-ref next 'state) "proposal-approved"))))
             (define target (hash-ref next 'state))
             (define (with-identity status
                                    cause
                                    message
                                    #:operation [operation #f]
                                    #:reconcilable? [reconcilable? #f])
               (finish status
                       cause
                       message
                       #:operation operation
                       #:proposal-ref proposal-ref
                       #:task task
                       #:expires-at expires-at
                       #:intent-reconcilable? reconcilable?
                       #:intent-target target))
             (cond
               [(recovery-pending-state-name target)
                (cond
                  [(and old (equal? old next))
                   (with-identity
                    'live
                    #f
                    "authenticated retained operation with a fenced reconciling intent")]
                  [from-extends?
                   (with-identity
                    'pending-wal
                    #f
                    "typed recoverable write-ahead cut awaits authenticated reconciliation"
                    #:operation (pending-view old next)
                    #:reconcilable? #t)]
                  [else (intent-unreadable! old)])]
               [else
                ;; Early ambiguous cut: the sealed intent stays on disk until
                ;; a mutation-authorized observation settles it fail-closed.
                (define already-blocked
                  (and old
                       (equal? (hash-ref old 'state) "blocked")
                       (equal? (hash-ref (hash-ref old 'facts) 'interrupted-intent #f)
                               (hash-ref wal 'sha256))))
                (cond
                  [(or already-blocked (and old (equal? old next)))
                   (with-identity 'live #f "authenticated retained operation with a fenced intent")]
                  [from-extends?
                   (with-identity
                    'pending-wal
                    #f
                    "fail-closed write-ahead cut awaits the journal's blocked settlement"
                    #:operation old)]
                  [else (intent-unreadable! old)])])])))))))
;; ---- BUG-0101/R101 typed discovery view (strictly READ-ONLY) ----------
;; BUG-0090: the disposition seam is now the static acyclic import above. The
;; claim and the shared storage core are instantiated with this module by the
;; module graph itself -- never lazily, mid-suite, on a worker thread -- and
;; the old `disposition-store-support` submodule back-edge is gone. The
;; journal re-exports the claim's disposition entry points unchanged.
(define (supplemental-recovery-pointer-claimed? path)
  ;; Explicit #f is outside the trusted feature embedding: it cannot discover
  ;; private ownership. Any configured-but-invalid/inaccessible root or claim
  ;; raises, NEVER becomes #f and ordinary fallback. No files/locks are created.
  (define root (and (current-supplemental-authority-root) (root! #t)))
  (define bound (canonical-pointer-path path))
  (and root
       (for/fold ([claimed? #f])
                 ([entry (in-list (directory-list root))]
                  #:when (regexp-match? #rx"^supplemental-(recovery|disposition)-"
                                        (path->string entry)))
         ;; Validate all candidates even after a match; ambiguity must stop.
         (define claim
           (if (regexp-match? #rx"^supplemental-disposition-" (path->string entry))
               (read-disposition-claim! (build-path root entry))
               (read-recovery-claim! (build-path root entry))))
         (or (equal? bound (hash-ref claim 'pointer-path)) claimed?))
       #t))
;; Caller holds campaign lease and has revalidated live context/source. Claim
;; precedes CAS and is itself the permanent replay intent; never initialize a
;; child operation here (operation -> authority is the only lock order).
(define (install-supplemental-recovery-claim! path old target proposal grant)
  (call-with-supplemental-authority-lock
   (lambda ()
     (unless (validate-supplemental-approval grant proposal)
       (refuse "exact live child approval required"))
     (define payload (hash-ref (load-supplemental-record proposal) 'payload))
     (define task (hash-ref payload 'task))
     (unless (and (exact-keys? payload '(task expires-at))
                  (valid-supplemental-task? task)
                  (equal? (hash-ref task 'kind) "supplemental-evidence-task/v2")
                  (< (current-seconds) (hash-ref payload 'expires-at))
                  (<= (hash-ref payload 'expires-at)
                      (+ (current-seconds) (hash-ref (hash-ref task 'bounds) 'deadline-sec))))
       (refuse "invalid or expired recovery child"))
     (define r (hash-ref task 'recovery))
     (unless (and (equal? (hash-ref old 'task-proposal) (hash-ref r 'parent-proposal))
                  (equal? (hash-ref old 'task-approval) (hash-ref r 'parent-approval))
                  (equal? (supplemental-record-digest old) (hash-ref r 'pointer-sha256))
                  (equal? target
                          (hasheq 'schema
                                  "supplemental-pointer/v1"
                                  'task-proposal
                                  proposal
                                  'task-approval
                                  grant)))
       (refuse "recovery expected pointer mismatch"))
     (define claim-file (claim-path (hash-ref r 'parent-approval)))
     (define claim
       (seal (hasheq 'schema
                     "supplemental-recovery-claim/v1"
                     'lineage
                     r
                     'pointer-path
                     (canonical-pointer-path path)
                     'target
                     target)))
     (when (present? claim-file)
       (unless (equal? (read-recovery-claim! claim-file) claim)
         (refuse "parent already permanently claimed by another child or approval")))
     (define current (read-pointer! path))
     (unless (or (equal? current old) (equal? current target))
       (refuse "active pointer CAS mismatch"))
     ;; Validate all inherited scope and terminal bindings before persisting.
     (define-values (p parent terminal)
       (inspect-supplemental-recovery-parent (hash-ref r 'parent-approval)))
     (unless (and (equal? p (hash-ref r 'parent-proposal))
                  (equal? (hash-ref terminal 'sha256) (hash-ref r 'terminal-sha256))
                  (equal? (hash-ref (hash-ref terminal 'facts) 'allocation) (hash-ref r 'allocation))
                  (for/and ([(key value) (in-hash parent)]
                            #:unless (memq key '(kind sha256 control-plane-head)))
                    (equal? value (hash-ref task key #f))))
       (refuse "child changed parent lineage or scope"))
     (unless (present? claim-file)
       (atomic-write! claim-file claim))
     (cut! 'recovery-claim-written)
     (unless (equal? current target)
       (atomic-write! path target))
     (cut! 'recovery-pointer-written)
     (validate-recovery-claim! grant proposal task)
     target)))
;; Admission wrapper (BUG-0090): the storage/authentication core lives in the
;; acyclic store-core helper shared with the claim module. This thin wrapper
;; adds the exact forward validation dispatch that must consult the claim
;; module (which this journal statically requires) for live admissions:
;; v3 tasks validate their permanent disposition claim, v2 recovery tasks
;; their permanent recovery claim. Historical authentication -- the only mode
;; the claim module itself uses -- is identical to the helper core.
(define (authenticated-task! grant [historical? #f])
  (define-values (proposal-ref task expiry) (authenticate-supplemental-task! grant historical?))
  (unless historical?
    (cond
      [(equal? (hash-ref task 'kind) "supplemental-evidence-task/v3")
       (validate-disposition-claim! grant proposal-ref task)]
      [(hash-has-key? task 'recovery) (validate-recovery-claim! grant proposal-ref task)]))
  (values proposal-ref task expiry))
(define (validate-linked-approvals! facts)
  (for ([keys '((candidate-approval-ref candidate-ref "candidate-approval" "candidate-proposal")
                (supersession-approval-ref supersession-proposal-ref
                                           "supersession-approval"
                                           "supersession-proposal"))])
    (define a-key (car keys))
    (when (hash-has-key? facts a-key)
      (define proposal (hash-ref facts (cadr keys) #f))
      (unless (and (supplemental-reference? proposal)
                   (equal? (hash-ref proposal 'kind) (cadddr keys)))
        (refuse "linked approval requires exact proposal reference"))
      (define approved (validate-supplemental-approval (hash-ref facts a-key) proposal))
      (unless (and (hash? approved)
                   (immutable? approved)
                   (equal? (hash-ref approved 'kind #f) (caddr keys))
                   (equal? (hash-ref approved 'proposal #f) proposal))
        (refuse "linked private approval unavailable, revoked or expired"))
      (load-supplemental-record proposal))))
;; Storage-only validation tables and the sealed operation-record validator
;; live in the acyclic store-core helper (BUG-0090) shared with the claim.
(define (under-lock grant proc)
  (define-values (proposal task expiry) (authenticated-task! grant))
  (define root (root!))
  (define-values (dir snapshot intent lock) (paths root grant))
  (call-with-semaphore (local-lock lock)
                       (lambda ()
                         (unless (present? lock)
                           (with-handlers ([exn:fail:filesystem:exists? void])
                             (call-with-output-file lock void #:exists 'error #:permissions #o600)))
                         (check-file! lock)
                         (call-with-file-lock/timeout
                          #f
                          'exclusive
                          (lambda ()
                            (check-file! lock)
                            (private-directory! dir)
                            ;; ONE shared admission/revocation critical section
                            ;; (BUG-0088): authority revalidation and the write
                            ;; itself hold the SAME root-private lock that
                            ;; revocation takes, so a revocation can never
                            ;; interleave between a recheck and its commit.
                            (call-with-supplemental-authority-lock
                             (lambda ()
                               ;; Approval may expire/revoke while waiting for another process.
                               (define-values (p t e) (authenticated-task! grant))
                               (unless (and (equal? p proposal) (equal? t task) (equal? e expiry))
                                 (refuse "authority changed while waiting"))
                               (proc snapshot intent proposal task expiry))))
                          (lambda () (refuse "operation lock busy"))
                          #:lock-file lock
                          #:max-delay 0.5))))
(define (commit! snapshot intent old next)
  (when (present? intent)
    (refuse "unrecovered write-ahead intent"))
  (define wal
    (seal (hasheq 'schema
                  "supplemental-operation-intent/v1"
                  'from-revision
                  (if old
                      (hash-ref old 'revision)
                      -1)
                  'from-state
                  (if old
                      (hash-ref old 'state)
                      "absent")
                  'next
                  next)))
  ;; Validate serialization size BEFORE making a pending intent.
  (bytes-for next)
  (atomic-write! intent wal)
  (cut! 'intent-written)
  (atomic-write! snapshot next)
  (cut! 'snapshot-written)
  (delete-file intent)
  (sync-directory! (path-only intent))
  (cut! 'intent-cleared)
  next)
(define (binding-extends? old next)
  (and (for/and ([(key value) (in-hash old)]
                 #:unless (member key '(revision state facts launch-consumed sha256)))
         (equal? value (hash-ref next key #f)))
       (for/and ([(key value) (in-hash (hash-ref old 'facts))])
         (equal? value (hash-ref (hash-ref next 'facts) key (lambda () (gensym)))))))
(define (pending-view old next)
  ;; Typed pending view of one interrupted recoverable write: the installed
  ;; snapshot state plus the typed recovery marker and the exact intent
  ;; digest. Derived deterministically on every read; never a committed fact
  ;; carrier. The sealed intent stays on disk until authenticated same-
  ;; operation reconciliation installs the exact interrupted record.
  (define target (hash-ref next 'state))
  (seal (hash-set* old
                   'state
                   (hash-ref recovery-pending-states target)
                   'facts
                   (hash-set* (hash-ref old 'facts)
                              'pending-recovery
                              target
                              'interrupted-intent
                              (hash-ref next 'sha256)))))
(define (recover/read! snapshot intent grant proposal task expiry)
  (define old
    (and (present? snapshot) (validate-record! (read-sealed snapshot) grant proposal task expiry)))
  (cond
    [(present? intent)
     (define wal (read-sealed intent))
     (unless (and (exact-keys? wal '(schema from-revision from-state next sha256))
                  (equal? (hash-ref wal 'schema) "supplemental-operation-intent/v1"))
       (refuse "invalid write-ahead intent"))
     (define next (validate-record! (hash-ref wal 'next) grant proposal task expiry))
     (define from-matches?
       (and (= (hash-ref wal 'from-revision)
               (if old
                   (hash-ref old 'revision)
                   -1))
            (equal? (hash-ref wal 'from-state)
                    (if old
                        (hash-ref old 'state)
                        "absent"))))
     (define from-extends?
       (and from-matches?
            (= (hash-ref next 'revision) (add1 (hash-ref wal 'from-revision)))
            (if old
                (and (binding-extends? old next)
                     (transition-allowed? (hash-ref old 'state) (hash-ref next 'state)))
                (equal? (hash-ref next 'state) "proposal-approved"))))
     (cond
       ;; Typed recoverable pending state (BUG-0088): a LATE cut whose target
       ;; already holds fully bound captured facts and a spent launch. The
       ;; sealed intent stays pending for the explicit authenticated
       ;; reconciliation; nothing is converted to blocked, relitigated or
       ;; relaunched here.
       [(recovery-pending-state-name (hash-ref next 'state))
        (cond
          [(and old (equal? old next)) old]
          [from-extends? (pending-view old next)]
          [else (refuse "write-ahead intent revision mismatch")])]
       ;; Early ambiguous cuts stay fail-closed blocked, consuming permission.
       [else
        (define already-blocked
          (and old
               (equal? (hash-ref old 'state) "blocked")
               (equal? (hash-ref (hash-ref old 'facts) 'interrupted-intent #f)
                       (hash-ref wal 'sha256))))
        (unless (or (and old (equal? old next)) from-extends? already-blocked)
          (refuse "write-ahead intent revision mismatch"))
        (unless already-blocked
          (atomic-write! snapshot
                         (seal (hash-set* next
                                          'revision
                                          (add1 (hash-ref next 'revision))
                                          'state
                                          "blocked"
                                          'launch-consumed
                                          #t
                                          'facts
                                          (hash-set* (hash-ref next 'facts)
                                                     'block-reason
                                                     "interrupted-transition"
                                                     'interrupted-intent
                                                     (hash-ref wal 'sha256))))))
        (cut! 'recovery-snapshot-written)
        (delete-file intent)
        (sync-directory! (path-only intent))
        (cut! 'recovery-intent-cleared)
        (if already-blocked
            old
            (read-sealed snapshot))])]
    [else old]))
(define (live-record! record)
  (when (>= (current-seconds) (hash-ref record 'deadline-at))
    (refuse "task deadline expired"))
  (validate-linked-approvals! (hash-ref record 'facts)))
(define (load-supplemental-operation grant)
  (under-lock grant
              (lambda (snapshot intent proposal task expiry)
                (recover/read! snapshot intent grant proposal task expiry))))
(define (start-supplemental-operation! grant base-binding)
  (define base (json-hash base-binding))
  (when (zero? (hash-count base))
    (refuse "empty base binding"))
  (under-lock
   grant
   (lambda (snapshot intent proposal task expiry)
     (unless (base-matches-task? base task)
       (refuse "base binding does not match approved task context"))
     (define old (recover/read! snapshot intent grant proposal task expiry))
     (cond
       [old
        (unless (equal? (hash-ref old 'base-binding) base)
          (refuse "grant already bound to a different base"))
        old]
       [else
        (define now (current-seconds))
        (commit! snapshot
                 intent
                 #f
                 (seal (hasheq 'schema
                               "supplemental-operation/v1"
                               'grant-ref
                               grant
                               'proposal-ref
                               proposal
                               'base-binding
                               base
                               'bounds
                               (hash-ref task 'bounds)
                               'created-at
                               now
                               'expires-at
                               expiry
                               'deadline-at
                               (min expiry (+ now (hash-ref (hash-ref task 'bounds) 'deadline-sec)))
                               'revision
                               0
                               'state
                               "proposal-approved"
                               'facts
                               (hasheq)
                               'launch-consumed
                               #f)))]))))
(define (transition-supplemental-operation! grant expected-revision expected-state new-state facts)
  (unless (exact-nonnegative-integer? expected-revision)
    (refuse "exact revision required"))
  (define from (state-string expected-state))
  (define to (state-string new-state))
  (when (member from pending-state-names)
    (refuse "pending recovery must reconcile before any transition"))
  (when (member to pending-state-names)
    (refuse "pending recovery states are installed only by reconciliation"))
  (define added (json-hash facts))
  (under-lock
   grant
   (lambda (snapshot intent proposal task expiry)
     (define old (recover/read! snapshot intent grant proposal task expiry))
     (unless old
       (refuse "operation must be initialized"))
     (unless (and (= expected-revision (hash-ref old 'revision)) (equal? from (hash-ref old 'state)))
       (refuse "stale operation revision or state"))
     (when (member from terminal)
       (refuse "terminal operation cannot transition"))
     (live-record! old)
     (unless (transition-allowed? from to)
       (refuse "forbidden operation transition (no redispatch or reallocation)"))
     (define combined
       (for/fold ([bound (hash-ref old 'facts)]) ([(key value) (in-hash added)])
         ;; Recovery-attested facts stay immutable once bound, as does the
         ;; base binding; a NEW typed fact (e.g. a blocked transition's own
         ;; reason) is appended exactly once and can never be rewritten.
         (when (or (hash-has-key? (hash-ref old 'base-binding) key)
                   (and (hash-has-key? bound key) (not (equal? value (hash-ref bound key)))))
           (refuse "bound fact/base binding cannot change"))
         (hash-set bound key value)))
     (define required (hash-ref required-facts (string->symbol to) #f))
     (when (and required (not (hash-has-key? combined required)))
       (refuse "missing transition fact"))
     (validate-linked-approvals! combined)
     (define next
       (seal (hash-set* old
                        'revision
                        (add1 expected-revision)
                        'state
                        to
                        'facts
                        combined
                        'launch-consumed
                        (or (hash-ref old 'launch-consumed)
                            (not (member to '("proposal-approved" "reserved")))))))
     ;; Recheck immediately before admitting the write, after all retained-record
     ;; reads. The parent must also serialize revocation with its campaign lease
     ;; and revalidate after every wait/before performing an external effect.
     (authenticated-task! grant)
     (live-record! next)
     (commit! snapshot intent old next))))

(define (reconcile-supplemental-operation! grant expected-revision expected-state)
  ;; Explicit AUTHENTICATED same-operation continuation of one typed
  ;; recoverable cut (BUG-0088). Authority is ONLY the live private task
  ;; approval (revalidated under the shared admission/revocation lock) plus
  ;; the sealed write-ahead intent itself; the target record's linked operator
  ;; approvals -- including the supersession approval where the plan requires
  ;; one -- are revalidated live. expected-revision/expected-state are an
  ;; exact CAS on the observed typed pending view. There is NO caller guard,
  ;; boolean, new fact, or reconstructed descriptor, and never a relaunch,
  ;; reallocation or re-dispatch: the exact interrupted record is installed
  ;; exactly once, and a second call finds nothing left to reconcile. Early
  ;; ambiguous cuts have no recoverable pending state and always refuse.
  (unless (exact-nonnegative-integer? expected-revision)
    (refuse "exact revision required"))
  (define expected (state-string expected-state))
  (under-lock
   grant
   (lambda (snapshot intent proposal task expiry)
     (unless (present? intent)
       (refuse "no pending recovery to reconcile"))
     (define raw
       (and (present? snapshot) (validate-record! (read-sealed snapshot) grant proposal task expiry)))
     (unless raw
       (refuse "no pending recovery to reconcile"))
     (define old (recover/read! snapshot intent grant proposal task expiry))
     (define wal (read-sealed intent))
     (define next (validate-record! (hash-ref wal 'next) grant proposal task expiry))
     (define pending-name (recovery-pending-state-name (hash-ref next 'state)))
     (unless pending-name
       (refuse "intent is not a typed recoverable cut"))
     ;; Same-operation binding: the sealed intent must extend exactly the
     ;; pre-cut snapshot into exactly the next lifecycle revision, or already
     ;; BE that installed record (mid-reconcile crash leftover).
     (unless (and (= (hash-ref next 'revision) (add1 (hash-ref wal 'from-revision)))
                  (or (and (= (hash-ref wal 'from-revision) (hash-ref raw 'revision))
                           (equal? (hash-ref wal 'from-state) (hash-ref raw 'state))
                           (binding-extends? raw next)
                           (transition-allowed? (hash-ref raw 'state) (hash-ref next 'state))
                           (not (member (hash-ref raw 'state) terminal)))
                      (equal? raw next)))
       (refuse "write-ahead intent revision mismatch"))
     ;; Exact expected view CAS: the unapplied pending view, the same pre-crash
     ;; expectations after a mid-reconcile crash, or the already-installed
     ;; state whose leftover intent still fences normal transitions.
     (unless (or (and (= expected-revision (hash-ref wal 'from-revision))
                      (equal? expected pending-name))
                 (and (equal? old next)
                      (= expected-revision (hash-ref next 'revision))
                      (equal? expected (hash-ref next 'state))))
       (refuse "stale operation revision or state"))
     ;; Live authority for the target state: task expiry plus operator
     ;; approvals, including the supersession approval where required.
     (live-record! next)
     (unless (equal? old next)
       (atomic-write! snapshot next)
       (cut! 'reconcile-snapshot-written))
     (delete-file intent)
     (sync-directory! (path-only intent))
     (cut! 'reconcile-intent-cleared)
     next)))
