#lang racket
;; @speed slow
;; @suite extensions
;; @boundary integration
;; @mutates repo
;; @isolation temp-dir
;; @timeout 900
;; @covers extensions/gsd/supplemental-evidence-promotion.rkt
;; @covers extensions/gsd/delivery-journal.rkt
;; @covers extensions/gsd/evidence-probe.rkt
;; BUG-0088 section 8 / plan Phase 0 step 3: isolated fixture suite for the
;; protected owner promotion transition. The fixture drives the genuine
;; governed ladder (real private authority store, real interactive broker
;; grants, real owned C0/G1 captures, real baseline-overlay RED +
;; identical-candidate GREEN, real S-authorized supersession) all the way to
;; the certified entry state, then exercises the promotion surface on top:
;;
;;   - actual P issuance from the certified staged wrapper (sealed, pinned,
;;     deterministic, refused without a promotable R1/G1 context),
;;   - the promotion entry's authority gating refusals (shape, live guard,
;;     foreign S) with zero protected effects,
;;   - the single-consumption capability's forgery/refusal surface: the
;;     protected-owner mint refuses without the journal's attested promotion
;;     intent, and forged capability values can never be verified or consumed,
;;   - recovery requiring the durable attested intent (protected origin),
;;   - the REAL promotion happy path: full promote-supplemental-evidence!
;;     success with the write-ahead intent pinned to the exact E identity,
;;     exactly one ledger registration carrying the attested P provenance,
;;     readiness ready/registered-probe-ready and the operation completed,
;;   - single consumption of a fresh mint from the attested origin
;;     (verify -> #t, consume -> #t, verify -> #f, second consume refuses),
;;   - a LATE recovery replay after the wall clock advanced past the intent's
;;     write second: the replay rebuilds the intent with a fresh 'at, the
;;     immutable attested record stays intact, and no second registration is
;;     appended and no probe is relaunched,
;;   - a post-revocation refusal of the whole transition.
;;
;; No fake grants and no fabricated protected state are used anywhere in
;; this suite.
(require rackunit
         racket/file
         json
         "helpers/supplemental-promotion-fixture.rkt"
         "../extensions/gsd/delivery-journal.rkt"
         "../extensions/gsd/supplemental-evidence-journal.rkt"
         (only-in "../extensions/gsd/evidence-repair-format.rkt"
                  canonical-evidence-digest
                  evidence-context-digest)
         (only-in "../extensions/gsd/evidence-repair-ledger.rkt" load-evidence-repair)
         "../extensions/gsd/evidence-probe.rkt"
         (submod "../extensions/gsd/evidence-probe.rkt" protected-owner)
         (only-in "../extensions/gsd/evidence-readiness.rkt" current-evidence-context)
         (only-in "../extensions/gsd/supplemental-evidence-publication.rkt"
                  supplemental-staged-evidence
                  supplemental-staged-evidence-path
                  supplemental-staged-evidence-record)
         "../extensions/gsd/supplemental-evidence-format.rkt"
         (only-in "../extensions/gsd/supplemental-evidence-authority.rkt"
                  validate-supplemental-approval)
         "../extensions/gsd/supplemental-evidence-promotion.rkt")

(module+ test
  (test-case "the certified supplemental evidence promotion surface"
    (with-certified-promotion-fixture
     (lambda (env)
       (define root (hash-ref env 'root))
       (define plan fixture-plan)
       (define sapref (hash-ref env 'sapref))
       (define sref (hash-ref env 'sref))
       (define aref (hash-ref env 'aref))
       (define staged (hash-ref env 'staged))
       (define record (supplemental-staged-evidence-record staged))
       (define E (hash-ref env 'context))
       (define red-ref (hash-ref env 'red-ref))
       (define r2 (hash-ref env 'r2))

       (test-case "the fixture reached the certified promotion entry state"
         ;; The journal was CAS-superseded to the exact r* receipt at
         ;; generation 2, and the protected operation is receipt-certified
         ;; over that exact receipt: every precondition of
         ;; promote-supplemental-evidence! except the intent write itself.
         (define journal (load-delivery-journal root plan 1))
         (check-equal? (hash-ref journal 'receipt) r2)
         (check-equal? (length (hash-ref journal 'receipt-history '())) 2)
         (define operation (load-supplemental-operation aref))
         (check-equal? (hash-ref operation 'state) "receipt-certified")
         (check-equal? (hash-ref (hash-ref operation 'facts) 'receipt) (canonical-evidence-digest r2))
         ;; The staged wrapper carries exactly this S authorization, and the
         ;; live T/A/K/CAp authority validates against the private store.
         (check-equal? (hash-ref record 'supersession-approval) sapref)
         (define refs (hash-ref record 'contract))
         (check-true (and (validate-supplemental-approval sapref sref) #t))
         (check-true (and (validate-supplemental-approval (hash-ref refs 'task-approval)
                                                          (hash-ref refs 'task-proposal))
                          #t))
         (check-true (and (validate-supplemental-approval (hash-ref refs 'candidate-approval)
                                                          (hash-ref refs 'candidate-proposal))
                          #t))
         ;; The ACTUAL current evidence context equals the staged predicted
         ;; context E, and the owned R1 origin revalidates against it.
         (check-equal? (current-evidence-context root plan 1) E)
         (validate-baseline-probe! (hash-ref E 'repo) red-ref E)
         ;; The promotion ledger slot for generation 2 is still untouched and
         ;; no promotion intent exists yet.
         (define ledger (load-evidence-repair root plan 1 2))
         (check-equal? (hash-ref ledger 'operation) 'null)
         (check-equal? (hash-ref ledger 'registrations) '())
         (check-false (load-supplemental-promotion-intent root plan 1)))

       (test-case "P is issued sealed, pinned and deterministic from the certified staged wrapper"
         (define P1 (supplemental-promotion-record staged sref sapref))
         (check-true (valid-supplemental-record? P1))
         (check-equal? (hash-ref P1 'kind) "supplemental-evidence-promotion/v1")
         (check-equal? (hash-ref P1 'context) E)
         (check-equal? (hash-ref P1 'receipt-sha256) (hash-ref E 'receipt-sha256))
         (check-equal? (hash-ref P1 'generation) (hash-ref E 'receipt-generation))
         (check-equal? (hash-ref P1 'generation) 2)
         (check-equal? (hash-ref P1 'operation-id) "repair-1")
         (check-equal? (hash-ref P1 'staged-context-digest) (evidence-context-digest E))
         (check-equal? (hash-ref P1 'supersession-approval) sapref)
         (check-equal? (hash-ref P1 'red-proof-ref) red-ref)
         ;; Sealing the same certified inputs yields the identical identity.
         (check-equal? (hash-ref (supplemental-promotion-record staged sref sapref) 'sha256)
                       (hash-ref P1 'sha256))
         ;; A staged wrapper stripped of its R1/G1 origin refuses to seal.
         (check-exn #rx"staged wrapper lacks a promotable R1/G1 context"
                    (lambda ()
                      (supplemental-promotion-record
                       (supplemental-staged-evidence (hash-remove record 'red-proof-ref)
                                                     (supplemental-staged-evidence-path staged))
                       sref
                       sapref)))
         ;; P alone grants nothing: it is not a capability.
         (check-false (promoted-registration? P1)))

       (define journal-bytes (file->bytes (delivery-journal-path root plan 1)))

       (test-case "promotion authority gating refuses before any protected effect"
         (check-exn #rx"invalid supersession approval reference"
                    (lambda ()
                      (promote-supplemental-evidence!
                       root
                       plan
                       1
                       (hasheq 'kind "task-approval" 'sha256 (hash-ref sapref 'sha256))
                       (lambda () #t))))
         (check-exn #rx"the campaign lease/live guard is stale"
                    (lambda () (promote-supplemental-evidence! root plan 1 sapref (lambda () #f))))
         (define forged-digest (string->immutable-string (make-string 64 #\f)))
         (check-exn #rx"the staged wrapper does not carry this S authorization"
                    (lambda ()
                      (promote-supplemental-evidence!
                       root
                       plan
                       1
                       (hasheq 'kind "supersession-approval" 'sha256 forged-digest)
                       (lambda () #t))))
         ;; Zero effects: no write-ahead intent, journal byte-identical, the
         ;; protected operation and the ledger slot untouched.
         (check-false (file-exists? (supplemental-promotion-path root plan 1)))
         (check-equal? (file->bytes (delivery-journal-path root plan 1)) journal-bytes)
         (check-equal? (hash-ref (load-supplemental-operation aref) 'state) "receipt-certified")
         (check-equal? (hash-ref (load-evidence-repair root plan 1 2) 'registrations) '()))

       (test-case "the registration capability can only exist from the attested protected origin"
         ;; The mint requires the journal's durable attested promotion intent;
         ;; a sealed P (or any forged promotion hash) never substitutes for it.
         (define P1 (supplemental-promotion-record staged sref sapref))
         (check-exn #rx"no attested protected promotion origin for this operation"
                    (lambda ()
                      (mint-promoted-registration-capability! root plan 1 "repair-1" E red-ref P1)))
         ;; Forged capability values can never be verified or consumed: the
         ;; constructor is not exported, and the verifier/consumer refuse
         ;; anything not minted through the protected origin.
         (check-false (promoted-registration? (hasheq 'kind "capability")))
         (check-false
          (verify-promoted-registration-capability! (hasheq 'forged #t) E red-ref "repair-1"))
         (check-false (verify-promoted-registration-capability! #f E red-ref "repair-1"))
         (check-exn
          #rx"invalid, stale or double-consumed promotion capability"
          (lambda ()
            (consume-promoted-registration-capability! (hasheq 'forged #t) E red-ref "repair-1")))
         (check-exn #rx"invalid, stale or double-consumed promotion capability"
                    (lambda ()
                      (consume-promoted-registration-capability! "forged" E red-ref "repair-1"))))

       (test-case "certified recovery requires the durable attested protected origin"
         (check-exn
          #rx"no durable promotion intent is recorded; recovery requires the protected origin"
          (lambda () (recover-supplemental-promotion! root plan 1 sapref (lambda () #t))))
         (check-exn
          #rx"no durable promotion intent is recorded; recovery requires the protected origin"
          (lambda () (recover-supplemental-promotion! root plan 1 sapref (lambda () #f)))))

       ;; ============================================================
       ;; The REAL promotion transition: the documented happy path, the
       ;; single-consumption capability contract, and the late recovery
       ;; replay. No fake grants and no fabricated state: every assertion
       ;; below runs through the genuine production entry points on top of
       ;; the certified ladder above.
       (test-case "the full promotion transition registers the certified evidence exactly once"
         (define readiness (promote-supplemental-evidence! root plan 1 sapref (lambda () #t)))
         (check-equal? (hash-ref readiness 'status) "ready")
         (check-equal? (hash-ref readiness 'code) "registered-probe-ready")
         ;; Exactly one registration in the ACTUAL generation, bound to the
         ;; staged E, the owned R1 probe and "repair-1", carrying the
         ;; attested P provenance.
         (define P1 (supplemental-promotion-record staged sref sapref))
         (define ledger (load-evidence-repair root plan 1 2))
         (check-equal? (length (hash-ref ledger 'registrations)) 1)
         (define reg (car (hash-ref ledger 'registrations)))
         (check-equal? (hash-ref reg 'context) E)
         (check-equal? (hash-ref reg 'probe) red-ref)
         (check-equal? (hash-ref reg 'operation-id) "repair-1")
         (check-equal? (hash-ref readiness 'registration) reg)
         (check-equal?
          (hash-ref (hash-ref ledger 'operation) 'promoted)
          (hasheq 'promotion (hash-ref P1 'sha256) 'generation (hash-ref E 'receipt-generation)))
         ;; The durable write-ahead intent is pinned to the exact E identity.
         (define intent (load-supplemental-promotion-intent root plan 1))
         (check-true (and intent #t))
         (check-equal? (hash-ref intent 'generation) (hash-ref E 'receipt-generation))
         (check-equal? (hash-ref intent 'receipt-sha256) (hash-ref E 'receipt-sha256))
         (check-equal? (hash-ref intent 'context-digest) (evidence-context-digest E))
         (check-equal? (hash-ref (hash-ref intent 'promotion #f) 'sha256 #f) (hash-ref P1 'sha256))
         (check-equal? (hash-ref intent 'operation-id) "repair-1")
         ;; The protected operation completed through the readiness gate.
         (check-equal? (hash-ref (load-supplemental-operation aref) 'state) "completed"))

       (test-case "the write-ahead intent shape the promotion builds is a valid nine-field record"
         ;; promote!/core writes NINE fields: the eight attested fields plus
         ;; the 'at write stamp. The validator must accept exactly that shape
         ;; and refuse any tampering with an attested field.
         (define P1 (supplemental-promotion-record staged sref sapref))
         (define intent-as-built
           (hasheq 'schema-version
                   1
                   'plan-id
                   plan
                   'wave
                   1
                   'generation
                   (hash-ref E 'receipt-generation)
                   'receipt-sha256
                   (hash-ref E 'receipt-sha256)
                   'context-digest
                   (evidence-context-digest E)
                   'operation-id
                   "repair-1"
                   'promotion
                   P1
                   'at
                   (current-seconds)))
         (check-equal? (hash-count intent-as-built) 9)
         (check-true (valid-supplemental-promotion-intent? intent-as-built plan 1))
         ;; Shape tampering: an extra field and a promotion whose body no
         ;; longer matches its sealed digest are both refused.
         (check-false
          (valid-supplemental-promotion-intent? (hash-set intent-as-built 'extra 'field) plan 1))
         (check-false (valid-supplemental-promotion-intent?
                       (hash-set intent-as-built 'promotion (hash-set P1 'operation-id "repair-2"))
                       plan
                       1)))

       (test-case "a fresh mint from the attested origin is single-consumption"
         ;; The mint works now that the journal's attested intent is durable,
         ;; and the minted token is consumed exactly once.
         (define P1 (supplemental-promotion-record staged sref sapref))
         (define capability
           (mint-promoted-registration-capability! root plan 1 "repair-1" E red-ref P1))
         (check-true (verify-promoted-registration-capability! capability E red-ref "repair-1"))
         (check-true (consume-promoted-registration-capability! capability E red-ref "repair-1"))
         (check-false (verify-promoted-registration-capability! capability E red-ref "repair-1"))
         (check-exn #rx"invalid, stale or double-consumed promotion capability"
                    (lambda ()
                      (consume-promoted-registration-capability! capability E red-ref "repair-1")))
         (check-false (verify-promoted-registration-capability! capability E red-ref "repair-1"))
         ;; A non-minted value never verifies against the attested origin.
         (check-false (verify-promoted-registration-capability! (hasheq 'kind "capability")
                                                                E
                                                                red-ref
                                                                "repair-1")))

       (test-case "a late recovery replay keeps the attested intent and never appends"
         (define intent-path (supplemental-promotion-path root plan 1))
         (define prior-intent (call-with-input-file intent-path read-json))
         (define prior-at (hash-ref prior-intent 'at))
         (check-true (exact-nonnegative-integer? prior-at))
         (define reg-before (car (hash-ref (load-evidence-repair root plan 1 2) 'registrations)))
         ;; Advance the real wall clock into a LATER second, so the replay
         ;; rebuilds the write-ahead intent with a fresh 'at: the immutable
         ;; attested record must neither refuse the replay nor be overwritten.
         (let loop ()
           (when (<= (current-seconds) prior-at)
             (sleep 0.02)
             (loop)))
         (define readiness (recover-supplemental-promotion! root plan 1 sapref (lambda () #t)))
         (check-equal? (hash-ref readiness 'status) "ready")
         (check-equal? (hash-ref readiness 'code) "registered-probe-ready")
         ;; The identical single registration: no second append, and the R1
         ;; red proof is unchanged (the probe is never relaunched).
         (define ledger-after (load-evidence-repair root plan 1 2))
         (check-equal? (length (hash-ref ledger-after 'registrations)) 1)
         (check-equal? (car (hash-ref ledger-after 'registrations)) reg-before)
         (check-equal? (hash-ref readiness 'registration) reg-before)
         (check-equal? (hash-ref (car (hash-ref ledger-after 'registrations)) 'probe) red-ref)
         ;; The durable attested record itself did not move byte-for-byte.
         (define replayed-intent (call-with-input-file intent-path read-json))
         (check-equal? replayed-intent prior-intent)
         (check-equal? (hash-ref replayed-intent 'at) prior-at)
         (check-true (valid-supplemental-promotion-intent? replayed-intent plan 1))
         (check-equal? (hash-ref (load-supplemental-operation aref) 'state) "completed"))

       (test-case "after revoking the S approval the whole transition refuses"
         (define intent-path (supplemental-promotion-path root plan 1))
         (define intent-bytes-before (file->bytes intent-path))
         (fixture-revoke-approval! sapref)
         (check-false (validate-supplemental-approval sapref sref))
         ;; Even with the attested promotion intent durably recorded, a
         ;; revoked S authorization refuses the whole transition: the broker
         ;; authority stays live at every use and is never bypassed.
         (check-exn #rx"the supersession approval is missing, revoked or foreign"
                    (lambda () (promote-supplemental-evidence! root plan 1 sapref (lambda () #t))))
         (check-exn #rx"the supersession approval is missing, revoked or foreign"
                    (lambda () (recover-supplemental-promotion! root plan 1 sapref (lambda () #t))))
         ;; Revocation touched only the private authority store.
         (check-equal? (file->bytes (delivery-journal-path root plan 1)) journal-bytes)
         (check-equal? (file->bytes intent-path) intent-bytes-before))))))
