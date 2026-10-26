#lang racket/base
;; STABILITY: internal
;; Known-source immutable storage/validation core extracted from the
;; supplemental journal (BUG-0090). Both the journal and the permanent
;; disposition claim require this module directly, so the old claim ->
;; journal `disposition-store-support` back-edge -- and with it the lazy
;; dynamic-require seam whose concurrent first use raced module
;; instantiation cold -- is gone. The forward v3 admission dispatch, which
;; must consult the claim module, stays in the journal: the journal
;; statically requires the claim, and this helper never imports either,
;; keeping the graph journal -> claim -> core acyclic.
;;
;; Everything here is storage-only validation: deliberately no approval
;; minting, revocation, operation inspection or reconciliation. Ownership
;; can only deny execution. Moved checks keep raising with the journal's
;; source symbol, exactly like supplemental-evidence-store.rkt, so error
;; identities are unchanged.
(require json
         racket/file
         racket/list
         racket/path
         "supplemental-evidence-store.rkt"
         "supplemental-evidence-format.rkt"
         "supplemental-evidence-authority.rkt"
         (only-in "../gsd-planning/execution-policy.rkt" current-gsd-author-phase))
(provide current-supplemental-journal-crash-cut
         cut!
         recovery-pending-state-name
         recovery-pending-states
         pending-state-names
         all-states
         terminal
         transition-allowed?
         state-string
         required-facts
         base-matches-task?
         claim-path
         canonical-pointer-path
         paths
         read-pointer!
         read-recovery-claim!
         validate-record!
         inspect-supplemental-recovery-parent
         validate-recovery-claim!
         authenticate-supplemental-task!
         observation-directory
         observation-paths
         observation-terminal-states
         validate-observation-record!)
;; Fault injection may only interrupt storage, never supply authority or facts.
(define current-supplemental-journal-crash-cut
  (make-parameter
   void
   (lambda (v)
     (unless (and (procedure? v) (procedure-arity-includes? v 1))
       (raise-argument-error 'current-supplemental-journal-crash-cut "(-> symbol? any)" v))
     v)))
(define (cut! name)
  ((current-supplemental-journal-crash-cut) name))
;; Kept symbol-compatible with supplemental-evidence-store for moved checks.
(define (refuse message)
  (error 'supplemental-evidence-journal "~a" message))
(define states
  '("proposal-approved" "reserved"
                        "launched"
                        "candidate-sealed"
                        "candidate-approved"
                        "verifying"
                        "green-captured"
                        "probing"
                        "probe-captured"
                        "supersession-approved"
                        "published"
                        "receipt-certified"
                        "registered"
                        "completed"))
(define terminal '("blocked" "revoked" "completed"))
;; Typed recoverable pending states (BUG-0088): late cuts whose target holds
;; fully bound captured facts recover as explicit pending views of the SAME
;; operation instead of terminal blocked. Early ambiguous model/job-consumed
;; cuts (reserved/launched through verifying/probing admission, sealing,
;; approvals and completion) stay fail-closed blocked.
(define recovery-pending-states
  (hash "green-captured"
        "recover-pending-verified"
        "published"
        "recover-pending-published"
        "receipt-certified"
        "recover-pending-receipt-certified"
        "registered"
        "recover-pending-registration"))
(define pending-state-names (hash-values recovery-pending-states))
(define (recovery-pending-state-name state)
  (hash-ref recovery-pending-states
            (if (symbol? state)
                (symbol->string state)
                state)
            #f))
(define all-states (append states '("blocked" "revoked") pending-state-names))
(define (transition-allowed? from to)
  (and (not (member from terminal))
       (or (member to '("blocked" "revoked"))
           (let ([tail (member from states)])
             (and tail (pair? (cdr tail)) (equal? to (cadr tail)))))))
(define (state-string s)
  (define v
    (if (symbol? s)
        (symbol->string s)
        s))
  (unless (member v all-states)
    (refuse "unknown operation state"))
  (string->immutable-string v))
(define (claim-path parent)
  (build-path (root!) (string-append "supplemental-recovery-" (hash-ref parent 'sha256) ".json")))
(define (canonical-pointer-path path)
  (path->string (simplify-path (path->complete-path path) #f)))
;; Storage-only validation: deliberately no approval, expiry, revocation,
;; operation inspection or reconciliation. Ownership can only deny execution.
(define (read-recovery-claim! path)
  (define claim (read-sealed path))
  (define r (hash-ref claim 'lineage #f))
  (define target (hash-ref claim 'target #f))
  (define bound (hash-ref claim 'pointer-path #f))
  (define (ref? value kind)
    (and (supplemental-reference? value) (equal? (hash-ref value 'kind) kind)))
  (unless (and
           (exact-keys? claim '(schema lineage pointer-path target sha256))
           (equal? (hash-ref claim 'schema) "supplemental-recovery-claim/v1")
           (exact-keys? r
                        '(parent-proposal parent-approval terminal-sha256 pointer-sha256 allocation))
           (ref? (hash-ref r 'parent-proposal) "task-proposal")
           (ref? (hash-ref r 'parent-approval) "task-approval")
           (for/and ([key '(terminal-sha256 pointer-sha256)])
             (define digest (hash-ref r key))
             (and (string? digest) (regexp-match? #px"^[0-9a-f]{64}$" digest)))
           (let ([a (hash-ref r 'allocation)])
             (and (exact-keys? a '(attempt-id attempt-fence coordinator-fence))
                  (string? (hash-ref a 'attempt-id))
                  (positive? (string-length (hash-ref a 'attempt-id)))
                  (exact-nonnegative-integer? (hash-ref a 'attempt-fence))
                  (exact-nonnegative-integer? (hash-ref a 'coordinator-fence))))
           (string? bound)
           (complete-path? bound)
           (equal? bound (canonical-pointer-path bound))
           (exact-keys? target '(schema task-proposal task-approval))
           (equal? (hash-ref target 'schema) "supplemental-pointer/v1")
           (ref? (hash-ref target 'task-proposal) "task-proposal")
           (ref? (hash-ref target 'task-approval) "task-approval")
           (equal? (file-name-from-path path)
                   (string->path (string-append "supplemental-recovery-"
                                                (hash-ref (hash-ref r 'parent-approval) 'sha256)
                                                ".json"))))
    (refuse "invalid permanent recovery claim"))
  claim)
(define (read-pointer! path)
  (check-file! path)
  (define bytes (file->bytes path))
  (define in (open-input-bytes bytes))
  (define value (freeze (read-json in)))
  (unless (and (eof-object? (peek-byte in)) (bytes=? bytes (bytes-for value)))
    (refuse "noncanonical recovery pointer"))
  value)
(define (validate-recovery-claim! grant proposal task)
  (define r (hash-ref task 'recovery))
  (define claim (read-recovery-claim! (claim-path (hash-ref r 'parent-approval))))
  (define target (hash-ref claim 'target))
  (unless (and (equal? (hash-ref claim 'schema) "supplemental-recovery-claim/v1")
               (equal? (hash-ref claim 'lineage) r)
               (equal? (hash-ref target 'task-proposal) proposal)
               (equal? (hash-ref target 'task-approval) grant)
               (equal? target (read-pointer! (hash-ref claim 'pointer-path))))
    (refuse "recovery child requires its permanent claim and exact active pointer"))
  ;; Revocation of the parent remains effective, even after migration.
  (define-values (_p parent terminal)
    (inspect-supplemental-recovery-parent (hash-ref r 'parent-approval)))
  (unless (and (equal? (hash-ref terminal 'proposal-ref) (hash-ref r 'parent-proposal))
               (equal? (hash-ref terminal 'sha256) (hash-ref r 'terminal-sha256))
               (equal? (hash-ref (hash-ref terminal 'facts) 'allocation) (hash-ref r 'allocation))
               (for/and ([(key value) (in-hash parent)]
                         #:unless (memq key '(kind sha256 control-plane-head)))
                 (equal? value (hash-ref task key #f))))
    (refuse "recovery lineage or inherited scope mismatch")))
;; Admission core: validates the live (or historical) private task approval
;; and returns its exact proposal/task/expiry triple. The forward validation
;; of v3 disposition and v2 recovery lineage -- the `historical?` dispatch the
;; journal's authenticated-task! wrapper performs -- is deliberately NOT here:
;; it requires the claim module, and a helper import of it would close the
;; cycle this module exists to break.
(define (authenticate-supplemental-task! grant [historical? #f])
  (when (current-gsd-author-phase)
    (refuse "author phase cannot operate supplemental journal"))
  (unless (supplemental-reference? grant)
    (refuse "invalid private approval reference"))
  (define approval (validate-supplemental-approval grant))
  (unless (and (hash? approval)
               (immutable? approval)
               (equal? (hash-ref approval 'kind #f) "task-approval")
               (supplemental-reference? (hash-ref approval 'proposal #f)))
    (refuse "live authenticated task approval required"))
  (define proposal-ref (hash-ref approval 'proposal))
  (define proposal (load-supplemental-record proposal-ref))
  (define payload (hash-ref proposal 'payload #f))
  (define task (and (hash? payload) (hash-ref payload 'task #f)))
  (define expiry (and (hash? payload) (hash-ref payload 'expires-at #f)))
  (unless (and (equal? (hash-ref proposal-ref 'kind) "task-proposal")
               (equal? (hash-ref proposal 'kind #f) "task-proposal")
               (exact-keys? payload '(task expires-at))
               (valid-supplemental-task? task)
               (exact-positive-integer? expiry)
               (or historical?
                   (and (< (current-seconds) expiry)
                        (<= expiry
                            (+ (current-seconds) (hash-ref (hash-ref task 'bounds) 'deadline-sec))))))
    (refuse "approval requires a valid bounded task and unexpired operator expiry"))
  (values proposal-ref task expiry))
(define (inspect-supplemental-recovery-parent grant)
  (call-with-supplemental-authority-lock
   (lambda ()
     (define-values (proposal task expiry) (authenticate-supplemental-task! grant #t))
     (unless (equal? (hash-ref task 'kind) "supplemental-evidence-task/v1")
       (refuse "recovery requires an original v1 parent; no grandchildren"))
     (define-values (_dir snapshot intent _lock) (paths (root!) grant #f))
     (when (present? intent)
       (refuse "parent has pending write-ahead intent"))
     (define record (validate-record! (read-sealed snapshot) grant proposal task expiry))
     (unless (and (equal? (hash-ref record 'state) "blocked")
                  (= (hash-ref record 'revision) 3)
                  (hash-ref record 'launch-consumed)
                  (exact-keys? (hash-ref record 'facts) '(allocation dispatch-id block-reason))
                  (for/and ([key '(dispatch-id block-reason)])
                    (define value (hash-ref (hash-ref record 'facts) key))
                    (and (string? value) (positive? (string-length value))))
                  (let ([a (hash-ref (hash-ref record 'facts) 'allocation)])
                    (and (exact-keys? a '(attempt-id attempt-fence coordinator-fence))
                         (string? (hash-ref a 'attempt-id))
                         (positive? (string-length (hash-ref a 'attempt-id)))
                         (exact-nonnegative-integer? (hash-ref a 'attempt-fence))
                         (exact-nonnegative-integer? (hash-ref a 'coordinator-fence)))))
       (refuse "parent is not an unambiguous spent blocked revision-3 launch"))
     (values proposal task record))))
(define required-facts
  (hasheq 'reserved
          'allocation
          'launched
          'dispatch-id
          'candidate-sealed
          'candidate-ref
          'candidate-approved
          'candidate-approval-ref
          'verifying
          'verify-job
          'green-captured
          'green-proof-ref
          'probing
          'probe-job
          'probe-captured
          'probe-proof-ref
          'supersession-approved
          'supersession-approval-ref
          'published
          'publication
          'receipt-certified
          'receipt
          'registered
          'registration
          'completed
          'readiness))
(define record-keys
  '(schema grant-ref
           proposal-ref
           base-binding
           bounds
           created-at
           expires-at
           deadline-at
           revision
           state
           facts
           launch-consumed
           sha256))
(define (base-matches-task? base task)
  (define approved (hash-ref task 'context))
  (or (equal? base approved) (equal? (hash-ref base 'context #f) approved)))
(define (validate-record! record grant proposal task expiry)
  (unless (and (exact-keys? record record-keys)
               (equal? (hash-ref record 'schema) "supplemental-operation/v1")
               (equal? (hash-ref record 'grant-ref) grant)
               (equal? (hash-ref record 'proposal-ref) proposal)
               (equal? (hash-ref record 'bounds) (hash-ref task 'bounds))
               (equal? record (seal record))
               (equal? (hash-ref record 'expires-at) expiry)
               (hash? (hash-ref record 'base-binding))
               (positive? (hash-count (hash-ref record 'base-binding)))
               (base-matches-task? (hash-ref record 'base-binding) task)
               (hash? (hash-ref record 'facts))
               (exact-nonnegative-integer? (hash-ref record 'revision))
               (member (hash-ref record 'state) all-states)
               (exact-positive-integer? (hash-ref record 'created-at))
               (= (hash-ref record 'deadline-at)
                  (min expiry
                       (+ (hash-ref record 'created-at)
                          (hash-ref (hash-ref task 'bounds) 'deadline-sec))))
               (boolean? (hash-ref record 'launch-consumed))
               (equal? (hash-ref record 'launch-consumed)
                       (not (member (hash-ref record 'state) '("proposal-approved" "reserved"))))
               (or (member (hash-ref record 'state) '("blocked" "revoked"))
                   (member (hash-ref record 'state) pending-state-names)
                   (= (hash-ref record 'revision) (index-of states (hash-ref record 'state))))
               (let ([key (hash-ref required-facts (string->symbol (hash-ref record 'state)) #f)])
                 (or (not key) (hash-has-key? (hash-ref record 'facts) key))))
    (refuse "invalid operation/base/grant binding"))
  record)
(define (paths root grant [create? #t])
  (define dir (build-path root "supplemental-operations"))
  (when (and create? (not (present? dir)))
    (with-handlers ([exn:fail:filesystem:exists? void])
      (make-directory dir #o700))
    (sync-directory! root))
  (private-directory! dir)
  (define name (hash-ref grant 'sha256))
  (values dir
          (build-path dir (string-append name ".json"))
          (build-path dir (string-append name ".intent.json"))
          (build-path dir (string-append name ".lock"))))
;; ---- BUG-0101 unit 2: terminal observation storage -----------------------
;; Private, sealed, append-only terminal observation records for a
;; supplemental operation that failed or lost authority. Pure storage and
;; structural validation only: NO authentication, NO authority and NO
;; eligibility decision lives here, and these records are never workflow
;; state -- the journal's authenticated writer is the only mutator, and the
;; operation snapshot/WAL/revision are never touched by this directory.
(define (observation-directory root)
  (build-path root "supplemental-terminal-observations"))
(define (observation-paths root grant [create? #t])
  (define dir (observation-directory root))
  (when (and create? (not (present? dir)))
    (with-handlers ([exn:fail:filesystem:exists? void])
      (make-directory dir #o700))
    (sync-directory! root))
  (private-directory! dir)
  (define name (hash-ref grant 'sha256))
  (values dir
          (build-path dir (string-append name ".json"))
          (build-path dir (string-append name ".lock"))))
;; Terminal registry job states only (verification-job.rkt vocabulary minus
;; 'running): a non-terminal observation is refused, never stored.
(define observation-terminal-states '(completed failed timed-out cancelled orphan-recovered))
(define (observation-digest? v)
  (and (string? v) (regexp-match? #px"^[0-9a-f]{64}$" v)))
(define (observation-usage-field-valid? key value)
  ;; Canonical private JSON admits exact integers only; usage is recorded in
  ;; whole observed units (token counts, integral cost units), never floats.
  (exact-nonnegative-integer? value))
(define (validate-observation-record! record grant)
  (unless (and
           (exact-keys?
            record
            '(schema grant-ref proposal-ref identity entries usage accounting-keys sha256))
           (equal? (hash-ref record 'schema) "supplemental-terminal-observation/v1")
           (equal? (hash-ref record 'grant-ref) grant)
           (supplemental-reference? (hash-ref record 'proposal-ref))
           (let ([i (hash-ref record 'identity)])
             (and (exact-keys? i '(attempt-id attempt-fence coordinator-fence))
                  (string? (hash-ref i 'attempt-id))
                  (positive? (string-length (hash-ref i 'attempt-id)))
                  (exact-nonnegative-integer? (hash-ref i 'attempt-fence))
                  (exact-nonnegative-integer? (hash-ref i 'coordinator-fence))))
           (let ([entries (hash-ref record 'entries)])
             (and
              (list? entries)
              (for/and ([e (in-list entries)])
                (and (exact-keys? e '(key operation-sha256 terminal usage-declared))
                     (observation-digest? (hash-ref e 'key))
                     (observation-digest? (hash-ref e 'operation-sha256))
                     (let ([t (hash-ref e 'terminal)])
                       (and (exact-keys? t '(state exit-code reaped? log-path captured-at identities))
                            (member (hash-ref t 'state)
                                    (map symbol->string observation-terminal-states))
                            (let ([x (hash-ref t 'exit-code)]) (or (not x) (exact-integer? x)))
                            (boolean? (hash-ref t 'reaped?))
                            (let ([l (hash-ref t 'log-path)])
                              (or (not l) (and (string? l) (positive? (string-length l)))))
                            (exact-positive-integer? (hash-ref t 'captured-at))
                            (let ([i2 (hash-ref t 'identities)])
                              (and (exact-keys? i2 '(registry-incarnation job-id command-digest))
                                   (for/and ([k '(registry-incarnation job-id command-digest)])
                                     (let ([v (hash-ref i2 k)])
                                       (and (string? v) (positive? (string-length v)))))))))
                     (let ([u (hash-ref e 'usage-declared)])
                       (or (not u)
                           (equal? u "unknown")
                           (and (hash? u)
                                (immutable? u)
                                (positive? (hash-count u))
                                (for/and ([k (in-hash-keys u)])
                                  (memq k '(input-tokens output-tokens total-tokens cost-usd)))
                                (for/and ([(k v) (in-hash u)])
                                  (observation-usage-field-valid? k v)))))))))
           (let ([u (hash-ref record 'usage)])
             (and (exact-keys? u '(coverage input-tokens output-tokens total-tokens cost-usd))
                  (member (hash-ref u 'coverage) '("none" "known" "unknown"))
                  (for/and ([k '(input-tokens output-tokens total-tokens cost-usd)])
                    (let ([v (hash-ref u k)]) (or (not v) (observation-usage-field-valid? k v))))))
           (let ([keys (hash-ref record 'accounting-keys)])
             (and (list? keys) (andmap observation-digest? keys))))
    (refuse "invalid terminal observation record"))
  record)
