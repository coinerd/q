#lang racket/base
;; Sole fixed owner disposition. Retention is inert; broker waits belong to UI.
(require racket/list
         racket/runtime-path
         racket/string
         racket/system
         "campaign-state.rkt"
         "campaign-repository.rkt"
         ;; W1: the SAME process-safe campaign lease owner the orchestrator
         ;; uses — imported directly instead of dynamically re-importing
         ;; go-orchestrator.rkt, which re-provides these exact bindings.
         ;; Same owner, same lock file, no orchestrator back-edge, no
         ;; recursive lease acquisition.
         (only-in "campaign-lease.rkt" acquire-lease release-lease!)
         "delivery-journal.rkt"
         (only-in "evidence-repair-format.rkt" canonical-evidence-json)
         (only-in "../../util/json/checksum.rkt" sha256-string)
         (only-in "delivery-receipt.rkt" committed-delivery-snapshot)
         (only-in "review-proof.rkt" load-receipt-owned-proof)
         (only-in "evidence-readiness.rkt"
                  current-evidence-context
                  load-authenticated-evidence-receipt
                  authenticated-evidence-receipt-context)
         "supplemental-evidence-authority.rkt"
         "supplemental-evidence-format.rkt"
         "supplemental-evidence-journal.rkt"
         "supplemental-disposition-decision.rkt"
         "supplemental-disposition-policy.rkt"
         (only-in "supplemental-preserved-candidate.rkt"
                  preserved-candidate-binding
                  validate-preserved-candidate!)
         (only-in "supplemental-disposition-claim.rkt" disposition-late-terminal?)
         (only-in "supplemental-evidence-controller.rkt"
                  load-supplemental-pointer
                  supplemental-pointer-path)
         (only-in "../gsd-planning/execution-policy.rkt" current-gsd-author-phase))
(provide propose-supplemental-disposition!
         install-supplemental-disposition!)
(define-runtime-path control-root "../..")
(define (refuse text)
  (error 'supplemental-evidence-disposition "~a" text))
(define (amendment)
  (disposition-amendment))
(define (under-lease base plan proc)
  (when (current-gsd-author-phase)
    (refuse "author phase cannot dispose tasks"))
  (define lease (acquire-lease base plan #:session-id "supplemental-owner-disposition"))
  (unless lease
    (refuse "campaign lease busy"))
  (dynamic-wind void proc (lambda () (release-lease! lease))))
(define (control-head)
  (define out (open-output-string))
  (unless (zero? (parameterize ([current-directory control-root]
                                [current-output-port out])
                   (system*/exit-code (find-executable-path "git") "rev-parse" "HEAD")))
    (refuse "cannot authenticate control head"))
  (string-trim (get-output-string out)))
(define (parent-pointer a)
  (disposition-pointer (hash-ref a 'parent-proposal) (hash-ref a 'parent-approval)))
;; Preserved mode authenticates the retained E_old receipt/context WITHOUT the
;; physical live-source gate: the live checkout is the metadata descendant M,
;; never E_old.head, and the preserved-candidate leaf proves that exact K->M
;; settlement separately. Ordinary mode below keeps current-evidence-context
;; and therefore its strict live-source equality unchanged.
(define (preserved-parent-context base plan wave)
  (authenticated-evidence-receipt-context (load-authenticated-evidence-receipt base plan wave)))
;; The eligibility gate is a pure decision over facts this adapter authenticated.
;; Stage E runs in two phases so a lineage refusal never triggers the context or
;; source reads that phase E5–E6 needs (the refusal order is part of the contract).
(define (amendment-view a)
  (disposition-amendment-view (hash-ref a 'plan-id)
                              (hash-ref a 'wave)
                              (hash-ref a 'snapshot-sha256)
                              (hash-ref a 'parent-proposal)
                              (hash-ref a 'parent-approval)
                              (hash-ref a 'terminal-body-sha256)
                              (hash-ref a 'terminal-record-sha256)
                              (hash-ref a 'allocation)))
(define (refuse-decision! decision)
  (when (eq? (disposition-decision-status decision) 'refuse)
    (refuse (disposition-decision-message decision))))
(define (eligible! base plan wave pointer #:preserved [preserved #f])
  (define a (amendment))
  ;; Phase E1–E4: facts the gate already required before any evidence read.
  (define-values (proposal task terminal)
    (inspect-supplemental-disposition-parent (hash-ref pointer 'task-approval)))
  (define rec (load-campaign-record base plan))
  (define w
    (and rec (findf (lambda (w) (= wave (campaign-wave-index w))) (campaign-record-waves rec))))
  (define attempt (and w (campaign-wave-current-attempt w)))
  (refuse-decision! (decide-disposition-lineage
                     (disposition-request-view plan wave pointer (parent-pointer a))
                     (amendment-view a)
                     (disposition-terminal-view proposal
                                                (hash-ref terminal 'sha256)
                                                (sha256-string (canonical-evidence-json terminal))
                                                (hash-ref (hash-ref terminal 'facts) 'allocation)
                                                (disposition-late-terminal? a terminal))
                     (disposition-campaign-view (and rec #t)
                                                (and w #t)
                                                (and attempt #t)
                                                (and w (campaign-wave-status w))
                                                (and rec (campaign-record-cancellation rec) #t)
                                                (and rec (campaign-record-budget-pause rec) #t)
                                                (and attempt (campaign-attempt-id attempt))
                                                (and attempt (campaign-attempt-fence-token attempt))
                                                (and rec (campaign-fence-token rec)))))
  ;; Phase E5–E6: authenticated reads happen only after lineage admitted the request.
  (define context (hash-ref task 'context))
  ;; Preserved mode authenticates the retained E_old receipt/context WITHOUT the
  ;; physical live-source gate: the live checkout is the metadata descendant M,
  ;; never E_old.head, and the preserved-candidate leaf proves that exact K->M
  ;; settlement separately. Ordinary mode below keeps current-evidence-context
  ;; and therefore its strict live-source equality unchanged.
  (define expected
    (if preserved
        (preserved-parent-context base plan wave)
        (current-evidence-context base plan wave)))
  (define descendant (and preserved (hash-ref preserved 'descendant-source)))
  (define source (committed-delivery-snapshot (hash-ref context 'repo)))
  (define journal (load-delivery-journal base plan wave))
  (define receipt (and journal (hash-ref journal 'receipt #f)))
  (define manifest (and receipt (load-receipt-owned-proof receipt base plan wave)))
  (define expected-head
    (if descendant
        (hash-ref descendant 'head)
        (hash-ref context 'head)))
  (define expected-tree
    (if descendant
        (hash-ref descendant 'tree)
        (hash-ref context 'tree)))
  (refuse-decision! (decide-disposition-evidence
                     (amendment-view a)
                     (disposition-context-view (hash-ref context 'plan-id)
                                               (hash-ref context 'wave)
                                               (hash-ref context 'snapshot-sha256)
                                               expected
                                               context)
                     (disposition-source-view
                      (and source #t)
                      (and manifest #t)
                      (and manifest (hash-ref manifest 'execution-cwd #f))
                      (hash-ref context 'repo)
                      (and source
                           (for/and ([key '(branch origin)])
                             (equal? (hash-ref source key) (hash-ref context key)))
                           #t)
                      (and source (equal? (hash-ref source 'head) expected-head) #t)
                      (and source (equal? (hash-ref source 'tree) expected-tree) #t))))
  (values proposal task terminal))
(define (propose-supplemental-disposition! base
                                           plan
                                           wave
                                           expected-parent-proposal-sha
                                           expires-at
                                           #:preserved-candidate [preserved-candidate #f])
  ;; Preserved mode is a structural binding only at proposal time: the caller
  ;; supplies the exact JSON object (never an authorization), and the leaf
  ;; helper below re-authenticates every source/K/terminal fact before any
  ;; inert bytes are retained. There is no file/boolean/policy substitute.
  (when preserved-candidate
    (unless (valid-preserved-candidate? preserved-candidate)
      (refuse "preserved candidate binding is malformed"))
    (unless (disposition-third-generation? (amendment))
      (refuse "preserved candidate requires the fixed third-generation amendment")))
  (under-lease
   base
   plan
   (lambda ()
     (call-with-supplemental-authority-lock
      (lambda ()
        (define pointer
          (or (load-supplemental-pointer base plan wave) (refuse "missing parent pointer")))
        (unless (equal? expected-parent-proposal-sha
                        (hash-ref (hash-ref pointer 'task-proposal) 'sha256))
          (refuse "expected parent proposal changed"))
        (define-values (proposal parent terminal)
          (eligible! base plan wave pointer #:preserved preserved-candidate))
        (define now (current-seconds))
        (unless (and (exact-positive-integer? expires-at)
                     (< now expires-at)
                     (<= expires-at (+ now (hash-ref (hash-ref parent 'bounds) 'deadline-sec))))
          (refuse "fresh bounded expiry required"))
        (define a (amendment))
        ;; The descriptor is built by the policy helper; preserved mode adds
        ;; exactly ONE optional key and never widens the amendment object.
        (define disposition
          (disposition-descriptor a (parent-pointer a) #:preserved preserved-candidate))
        (unless (equal? (supplemental-record-digest pointer) (hash-ref disposition 'pointer-sha256))
          (refuse "active pointer is not the fixed amendment predecessor"))
        (define task
          (seal-supplemental-record (hash-set* parent
                                               'kind
                                               "supplemental-evidence-task/v3"
                                               'control-plane-head
                                               (control-head)
                                               'disposition
                                               disposition)))
        (unless (valid-supplemental-task? task)
          (refuse "invalid disposition lineage"))
        (when preserved-candidate
          ;; All actual old-candidate/E_old/K->M authentication is the leaf
          ;; helper; a proposal that cannot authenticate is never retained.
          (validate-preserved-candidate! base plan wave task))
        (values (propose-retain! "task-proposal" (hasheq 'task task 'expires-at expires-at))
                task))))))
(define (install-supplemental-disposition! base plan wave new-proposal-ref new-approval-ref)
  (under-lease
   base
   plan
   (lambda ()
     (call-with-supplemental-authority-lock
      (lambda ()
        (unless (validate-supplemental-approval new-approval-ref new-proposal-ref)
          (refuse "exact genuine disposition task approval required"))
        (define task (hash-ref (hash-ref (load-supplemental-record new-proposal-ref) 'payload) 'task))
        (unless (and (valid-supplemental-task? task)
                     (equal? (hash-ref task 'kind) "supplemental-evidence-task/v3"))
          (refuse "disposition requires task/v3"))
        (define preserved (preserved-candidate-binding task))
        (define old (parent-pointer (amendment)))
        (eligible! base plan wave old #:preserved preserved)
        (when preserved
          ;; Recheck the full E_old/K->M settlement and the exact live source
          ;; immediately before the permanent claim/pointer mutation.
          (validate-preserved-candidate! base plan wave task))
        (define target
          (hasheq 'schema
                  "supplemental-pointer/v1"
                  'task-proposal
                  new-proposal-ref
                  'task-approval
                  new-approval-ref))
        (install-supplemental-disposition-claim! (supplemental-pointer-path base plan wave)
                                                 old
                                                 target
                                                 new-proposal-ref
                                                 new-approval-ref))))))
