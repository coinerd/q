#lang racket/base
;; STABILITY: internal
;; BUG-0088 section 8: the authenticated promotion protocol. This module IS
;; the protected owner transition: it seals the immutable owner-attested
;; promotion record P, records the journal's write-ahead promotion intent,
;; mints the ONE fresh opaque single-consumption registration capability
;; (only from the attested protected origin), drives the explicit
;; promoted-captured ledger admission, and consumes the capability through
;; the ordinary registrar's unchanged captured checks. Recovery is
;; full-revalidation-only: it never relaunches a probe or a model dispatch
;; and never appends a second registration.
(require racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         (only-in "../../util/json/checksum.rkt" sha256-string)
         (only-in "evidence-repair-format.rkt"
                  canonical-evidence-digest
                  evidence-context-digest
                  valid-evidence-context?)
         (only-in "supplemental-evidence-format.rkt" seal-supplemental-record valid-supplemental-record?)
         (only-in "supplemental-evidence-authority.rkt"
                  supplemental-reference?
                  load-supplemental-record
                  validate-supplemental-approval
                  call-with-supplemental-admission)
         (only-in "supplemental-evidence-journal.rkt" load-supplemental-operation reconcile-supplemental-operation!)
         (only-in "supplemental-evidence-publication.rkt"
                  load-supplemental-staged-evidence
                  supplemental-staged-evidence
                  supplemental-staged-evidence-record
                  supplemental-staged-probe-transition!)
         (only-in "delivery-journal.rkt" load-supplemental-promotion-intent record-supplemental-promotion-intent!)
         (only-in "review-proof.rkt" current-owned-proof-root)
         "evidence-probe.rkt"
         (submod "evidence-probe.rkt" protected-owner)
         (only-in "evidence-repair-ledger.rkt"
                  admit-promoted-captured-evidence!
                  load-evidence-repair
                  register-owned-evidence!)
         (only-in "evidence-readiness.rkt" current-evidence-context evidence-readiness))
(provide promote-supplemental-evidence!
         recover-supplemental-promotion!
         supplemental-promotion-record)
(define-runtime-path journal-module "delivery-journal.rkt")
(define (refuse message)
  (error 'supplemental-evidence-promotion "~a" message))
(define (text? s)
  (and (string? s) (positive? (string-length s))))
(define (hex? s n)
  (and (string? s) (= (string-length s) n) (regexp-match? #px"^[0-9a-f]+$" s)))
(define registration-operation-id "repair-1")

;; The immutable, owner-attested public audit descriptor P: it ties the
;; staged R1/G1/E/S identity to the ACTUAL certified receipt context and
;; generation and the ONE registration operation ID. P itself grants
;; nothing; only the capability minted from its attested journal intent is
;; registration authority.
(define (supplemental-promotion-record staged sproposal-ref sapref)
  (define record (supplemental-staged-evidence-record staged))
  (define E (hash-ref record 'context))
  (define red-ref (hash-ref record 'red-proof-ref #f))
  (unless (and (valid-evidence-context? E)
               (hash? red-ref)
               (hex? (hash-ref red-ref 'sha256 #f) 64)
               (text? (hash-ref red-ref 'path #f))
               (hash? (hash-ref record 'green-proof-ref #f)))
    (refuse "staged wrapper lacks a promotable R1/G1 context"))
  (define contract (hash-ref record 'contract))
  (seal-supplemental-record (hasheq 'kind
                                    "supplemental-evidence-promotion/v1"
                                    'context
                                    E
                                    'task-sha256
                                    (hash-ref record 'task-sha256)
                                    'task-proposal
                                    (hash-ref contract 'task-proposal)
                                    'task-approval
                                    (hash-ref contract 'task-approval)
                                    'candidate-proposal
                                    (hash-ref contract 'candidate-proposal)
                                    'candidate-approval
                                    (hash-ref contract 'candidate-approval)
                                    'supersession-proposal
                                    sproposal-ref
                                    'supersession-approval
                                    sapref
                                    'green-proof-ref
                                    (hash-ref record 'green-proof-ref)
                                    'red-proof-ref
                                    red-ref
                                    'receipt-sha256
                                    (hash-ref E 'receipt-sha256)
                                    'generation
                                    (hash-ref E 'receipt-generation)
                                    'operation-id
                                    registration-operation-id
                                    'staged-context-digest
                                    (evidence-context-digest E))))
(define (promote!/core base plan wave sapref live-guard)
  (unless (and (supplemental-reference? sapref) (equal? (hash-ref sapref 'kind) "supersession-approval"))
    (refuse "invalid supersession approval reference"))
  (unless (and (procedure? live-guard) (live-guard))
    (refuse "the campaign lease/live guard is stale"))
  (define staged (load-supplemental-staged-evidence base plan wave))
  (unless staged
    (refuse "no staged supplemental evidence"))
  (define record (supplemental-staged-evidence-record staged))
  (unless (equal? (hash-ref record 'supersession-approval #f) sapref)
    (refuse "the staged wrapper does not carry this S authorization"))
  ;; The approval record's own digest is NOT the proposal's digest: the
  ;; actual supersession-proposal reference is the one the staged wrapper
  ;; carries (the exact S the operator approved).
  (define sproposal-ref (hash-ref record 'supersession-proposal #f))
  (unless (and (supplemental-reference? sproposal-ref)
               (equal? (hash-ref sproposal-ref 'kind #f) "supersession-proposal"))
    (refuse "the staged wrapper lacks its supersession proposal reference"))
  (define refs (hash-ref record 'contract))
  (define grant-ref (hash-ref refs 'task-approval))
  ;; Live operator authorization before the admitted effect: the S approval
  ;; for the exact proposal bytes plus the T/A/K/CAp chain.
  (unless (validate-supplemental-approval sapref sproposal-ref)
    (refuse "the supersession approval is missing, revoked or foreign"))
  (unless (and (validate-supplemental-approval (hash-ref refs 'task-approval) (hash-ref refs 'task-proposal))
               (validate-supplemental-approval (hash-ref refs 'candidate-approval) (hash-ref refs 'candidate-proposal)))
    (refuse "the T/A/K/CAp authority is missing, revoked or foreign"))
  ;; The coordinated admission holds the shared owner critical section
  ;; around the whole promotion effect: a concurrent revocation can never
  ;; interleave with the attestation, capability mint, ledger admission or
  ;; registration. The nested authority lock (same root) is reentrant.
  (call-with-supplemental-admission
   (list grant-ref (hash-ref refs 'candidate-approval) sapref)
   (lambda ()
     (define (admit!)
       (unless (and (validate-supplemental-approval sapref sproposal-ref)
                    (validate-supplemental-approval (hash-ref refs 'task-approval) (hash-ref refs 'task-proposal))
                    (validate-supplemental-approval (hash-ref refs 'candidate-approval)
                                                    (hash-ref refs 'candidate-proposal)))
         (refuse "the private authority changed during the promotion"))
       (unless (live-guard)
         (refuse "the campaign lease/live guard went stale during the promotion"))
       (void))
     ;; 1. Recompute the ACTUAL current evidence context through its normal
     ;;    authenticated receipt path; require byte-canonical equality to the
     ;;    staged E pinned in R1. The original staged wrapper, R1 bytes, job
     ;;    facts and G1 stay immutable; P describes the transition without
     ;;    relabeling evidence.
     (define actual (current-evidence-context base plan wave))
     (define E (hash-ref record 'context))
     (unless (equal? actual E)
       (refuse "the certified current evidence context differs from the staged predicted context"))
     ;; 2. Validate the owned R1 origin against the current context (static
     ;;    + git revalidation; NO probe or model launch follows promotion).
     (define red-ref (hash-ref record 'red-proof-ref #f))
     (validate-baseline-probe! (hash-ref E 'repo) red-ref E)
     ;; 3. The protected operation must carry the certified receipt and be
     ;;    past publication; its facts may never name a different receipt.
     (admit!)
     (define observed (load-supplemental-operation grant-ref))
     (define operation
       (if (equal? (hash-ref observed 'state) "recover-pending-registration")
           (reconcile-supplemental-operation! grant-ref (hash-ref observed 'revision) (hash-ref observed 'state))
           observed))
     (unless (member (hash-ref operation 'state #f) '("receipt-certified" "registered" "completed"))
       (refuse (format "the protected operation is ~a; certification is required before promotion"
                       (hash-ref operation 'state #f))))
     (unless (equal? (hash-ref (hash-ref operation 'facts #f) 'receipt #f) (hash-ref E 'receipt-sha256))
       (refuse "the protected operation retained a different certified receipt"))
     ;; 4. Seal P and record the journal promotion intent BEFORE consumption.
     (define promotion (supplemental-promotion-record staged sproposal-ref sapref))
     (unless (valid-supplemental-record? promotion)
       (refuse "the sealed promotion record is invalid"))
     (define intent
       (hasheq 'schema-version
               1
               'plan-id
               plan
               'wave
               wave
               'generation
               (hash-ref E 'receipt-generation)
               'receipt-sha256
               (hash-ref E 'receipt-sha256)
               'context-digest
               (evidence-context-digest E)
               'operation-id
               registration-operation-id
               'promotion
               promotion
               'at
               (current-seconds)))
     (record-supplemental-promotion-intent! base plan wave intent)
     (admit!)
     ;; 5. Mint the fresh opaque single-consumption capability from the
     ;;    attested protected origin ONLY (the mint cross-checks the intent).
     (define capability
       (mint-promoted-registration-capability! base plan wave registration-operation-id E red-ref promotion))
     ;; 6. Explicit promoted-captured ledger admission in the ACTUAL
     ;;    generation (install-or-verify; the ordinary registrar's captured
     ;;    checks stay unchanged).
     (admit-promoted-captured-evidence! base E red-ref registration-operation-id capability)
     (admit!)
     ;; 7. Registration consumes the capability and appends exactly one
     ;;    matching registration; replay after a post-registration crash
     ;;    reads the identical existing registration.
     (define registration
       (register-owned-evidence! base
                                 plan
                                 wave
                                 #:context E
                                 #:probe red-ref
                                 #:authorization capability
                                 #:operation-id registration-operation-id
                                 #:promoted-capability capability))
     ;; 8. Protected operation transitions (idempotent at target AND at the
     ;;    terminal completed state, so a full recovery replay is a no-op),
     ;;    then readiness. Ready is admitted ONLY after the promoted
     ;;    registration validates against the current certified context.
     (define already-completed? (equal? (hash-ref (load-supplemental-operation grant-ref) 'state #f) "completed"))
     (unless already-completed?
       (supplemental-staged-probe-transition! grant-ref
                                              "receipt-certified"
                                              "registered"
                                              (hasheq 'registration (hash-ref registration 'sha256))))
     (define readiness (evidence-readiness base plan wave))
     (unless (and (equal? (hash-ref readiness 'status #f) "ready")
                  (equal? (hash-ref readiness 'code #f) "registered-probe-ready"))
       (refuse (format "readiness is not ready for the promoted registration: ~a" (hash-ref readiness 'code #f))))
     (unless already-completed?
       (supplemental-staged-probe-transition! grant-ref
                                              "registered"
                                              "completed"
                                              (hasheq 'readiness (hash-ref readiness 'code))))
     readiness)))
(define (promote-supplemental-evidence! base plan wave sapref live-guard)
  ;; The full protected owner transition, from certification to a promoted,
  ;; registered, ready evidence generation. Idempotent at every completed
  ;; step; a fresh capability is minted whenever an earlier one was lost.
  (promote!/core base plan wave sapref live-guard))
(define (recover-supplemental-promotion! base plan wave sapref live-guard)
  ;; Crash recovery: permitted ONLY from the protected origin (the journal's
  ;; attested promotion intent) plus full revalidation above. Never launches
  ;; a probe or a model dispatch; never appends a second registration.
  (unless (load-supplemental-promotion-intent base plan wave)
    (refuse "no durable promotion intent is recorded; recovery requires the protected origin"))
  (promote!/core base plan wave sapref live-guard))
