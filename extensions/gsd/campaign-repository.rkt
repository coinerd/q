#lang racket/base

;; extensions/gsd/campaign-repository.rkt — Campaign .rktd storage boundary
;;
;; v0.99.90 W1 (#9232): encapsulate durable campaign persistence/migration
;; behind one fail-closed repository boundary.
;;
;; Ownership:
;;   - serialization (campaign-record <-> datum) — moved from campaign-state
;;   - fail-closed validation: schema version, plan-id identity (plan-id must
;;     equal the manifest hash), fencing tokens, attempt ids, canonical status
;;   - atomic replace: same-directory tmp + rename; tmp cleaned up on failure
;;   - path containment: plan-id is the ONLY input-derived path component and
;;     must be 64 lowercase hex (SHA-256), so traversal/absolute/separable
;;     plan-ids are rejected before any filesystem touch
;;   - no-follow: symlinked .rktd targets rejected on read and write
;;   - backward-compatible loading: legacy schema-version-1 fixtures load
;;   - load-or-migrate-campaign!: migrate-seed then load-or-return (boundary
;;     composition; a corrupted durable record fails closed, never silently
;;     re-migrated)
;;
;; campaign-state.rkt keeps the records, identity, selection, attempts, and
;; the PLAN/STATE seeding truth (migrate-campaign!); this module owns .rktd.

(require racket/file
         racket/path
         racket/match
         racket/format
         racket/string
         racket/contract
         "campaign-state.rkt")

;; ============================================================
;; Public API
;; ============================================================

(provide (contract-out [persist-campaign! (-> path-string? campaign-record? void?)]
                       [load-campaign-record (-> path-string? string? (or/c #f campaign-record?))]
                       [load-or-migrate-campaign! (-> path-string? campaign-record?)]
                       [validate-campaign-record! (-> campaign-record? void?)]
                       [exn:fail:campaign-corrupt? (-> any/c boolean?)]))

;; Raised when a durable .rktd file is corrupt, malformed, unsupported,
;; outside the containment policy, or a symlink (fail-closed boundary).
(struct exn:fail:campaign-corrupt exn:fail () #:transparent)

;; ============================================================
;; Validation — fail closed (schema, identity, fencing, attempts)
;; ============================================================

(define CURRENT-SCHEMA 1)

;; NOTE: this environment's regexp engine does not support {n} bounds, so
;; plan-id validation is a plain string check (no regexp): 64 lowercase hex.
(define hex-chars "0123456789abcdef")

(define (valid-plan-id? pid)
  (and (string? pid)
       (= (string-length pid) 64)
       (for/and ([ch (in-string pid)])
         (string-contains? hex-chars (string ch)))))

(define (corrupt! fmt . args)
  (raise (exn:fail:campaign-corrupt (apply format fmt args) (current-continuation-marks))))

(define (validate-plan-id! pid)
  (unless (valid-plan-id? pid)
    (corrupt! "invalid plan-id ~s (must be 64 lowercase hex chars)" pid)))

(define (validate-campaign-record! rec)
  (unless (campaign-record? rec)
    (corrupt! "not a campaign record: ~s" rec))
  (define pid (campaign-plan-id rec))
  (validate-plan-id! pid)
  (define m (campaign-record-manifest rec))
  (define manifest-hash (campaign-manifest-hash m))
  (unless (string=? pid manifest-hash)
    (corrupt! "plan-id ~s does not match manifest hash ~s" pid manifest-hash))
  (define sv (campaign-manifest-schema-version m))
  (unless (and (exact-nonnegative-integer? sv) (<= 1 sv CURRENT-SCHEMA))
    (corrupt! "unsupported schema version ~s (current ~a)" sv CURRENT-SCHEMA))
  (unless (string? (campaign-manifest-title m))
    (corrupt! "manifest title must be a string"))
  ;; Manifest inner structure (MINOR-2): dependencies, descriptors,
  ;; constraints-hash are validated directly; plan-id==manifest-hash remains
  ;; the identity backstop for accidental corruption.
  (define deps (campaign-manifest-dependencies m))
  (unless (and (list? deps) (andmap string? deps))
    (corrupt! "manifest dependencies must be a list of strings"))
  (unless (string? (campaign-manifest-constraints-hash m))
    (corrupt! "manifest constraints-hash must be a string"))
  (define seen-desc (make-hasheq))
  (for ([wd (campaign-manifest-waves m)])
    (unless (and (campaign-wave-descriptor? wd)
                 (string? (campaign-wave-descriptor-title wd))
                 (string? (campaign-wave-descriptor-doc-path wd))
                 (string? (campaign-wave-descriptor-content-hash wd)))
      (corrupt! "wave descriptor must have string title/doc-path/content-hash"))
    (define wd-idx (campaign-wave-descriptor-index wd))
    (unless (and (exact-nonnegative-integer? wd-idx) (< wd-idx 1000))
      (corrupt! "wave descriptor index must be a non-negative integer < 1000"))
    (when (hash-has-key? seen-desc wd-idx)
      (corrupt! "duplicate wave descriptor index ~s" wd-idx))
    (hash-set! seen-desc wd-idx #t))
  (define fence (campaign-fence-token rec))
  (unless (or (not fence) (exact-nonnegative-integer? fence))
    (corrupt! "fence token must be a non-negative integer, got ~s" fence))
  (define prov (campaign-record-provenance rec))
  (unless (or (not prov) (string? prov) (symbol? prov))
    (corrupt! "provenance must be a string or symbol, got ~s" prov))
  (unless (exact-integer? (campaign-record-created-at rec))
    (corrupt! "created-at must be an integer"))
  (unless (exact-integer? (campaign-record-updated-at rec))
    (corrupt! "updated-at must be an integer"))
  (define cancellation (campaign-record-cancellation rec))
  (when cancellation
    (unless (campaign-cancellation? cancellation)
      (corrupt! "cancellation field must be a campaign-cancellation or #f"))
    (unless (string? (campaign-cancellation-reason cancellation))
      (corrupt! "cancellation reason must be a string"))
    (unless (exact-integer? (campaign-cancellation-timestamp cancellation))
      (corrupt! "cancellation timestamp must be an integer")))
  (define waves (campaign-record-waves rec))
  (unless (list? waves)
    (corrupt! "waves must be a list"))
  (define seen (make-hasheq))
  (for ([w waves])
    (unless (campaign-wave? w)
      (corrupt! "wave must be a campaign-wave, got ~s" w))
    (define idx (campaign-wave-index w))
    (unless (and (exact-nonnegative-integer? idx) (< idx 1000))
      (corrupt! "wave index must be a non-negative integer < 1000, got ~s" idx))
    (when (hash-has-key? seen idx)
      (corrupt! "duplicate wave index ~s" idx))
    (hash-set! seen idx #t)
    (unless (string? (campaign-wave-title w))
      (corrupt! "wave title must be a string"))
    (unless (memq (campaign-wave-status w) CANONICAL-WAVE-STATUSES)
      (corrupt! "non-canonical wave status ~s" (campaign-wave-status w)))
    (unless (exact-nonnegative-integer? (campaign-wave-attempt-count w))
      (corrupt! "attempt count must be a non-negative integer"))
    ;; v1.00.17 W7: delivery provenance fields are strings ("" = unrecorded).
    (unless (string? (campaign-wave-delivery-branch w))
      (corrupt! "delivery-branch must be a string"))
    (unless (string? (campaign-wave-delivery-head-sha w))
      (corrupt! "delivery-head-sha must be a string"))
    ;; v1.00.18 (BUG-0024 W3): infra-retry hand-off context ("" = none).
    (unless (string? (campaign-wave-attempt-context w))
      (corrupt! "attempt-context must be a string"))
    (define attempt (campaign-wave-current-attempt w))
    (when attempt
      (unless (campaign-attempt? attempt)
        (corrupt! "current-attempt must be a campaign-attempt or #f"))
      (unless (and (string? (campaign-attempt-id attempt))
                   (positive? (string-length (campaign-attempt-id attempt))))
        (corrupt! "attempt id must be a non-empty string"))
      (define afence (campaign-attempt-fence-token attempt))
      (unless (or (not afence) (exact-nonnegative-integer? afence))
        (corrupt! "attempt fence token must be a non-negative integer"))
      (unless (exact-integer? (campaign-attempt-started-at attempt))
        (corrupt! "attempt started-at must be an integer"))))
  (void))

;; ============================================================
;; Serialization (campaign-record <-> datum)
;; ============================================================

(define (record->datum rec)
  ;; v1.00.19 W3 (BUG-0031): the trailing 3 fields are the build identity
  ;; (build-version / main-head-sha / stale-override). Every wave report and
  ;; evidence write rewrites this datum, so the identity travels with every
  ;; piece of recorded campaign evidence. #f values are written explicitly so
  ;; the datum always self-describes its producer (or its pre-W3 origin).
  (list 'campaign-record
        (campaign-plan-id rec)
        (manifest->datum (campaign-record-manifest rec))
        (map wave->datum (campaign-record-waves rec))
        (and (campaign-record-cancellation rec)
             (list 'cancellation
                   (campaign-cancellation-reason (campaign-record-cancellation rec))
                   (campaign-cancellation-timestamp (campaign-record-cancellation rec))))
        (campaign-fence-token rec)
        (campaign-record-provenance rec)
        (campaign-record-created-at rec)
        (campaign-record-updated-at rec)
        (campaign-record-build-version rec)
        (campaign-record-main-head-sha rec)
        (campaign-record-stale-override rec)))

(define (manifest->datum m)
  (list 'manifest
        (campaign-manifest-schema-version m)
        (campaign-manifest-title m)
        (campaign-manifest-dependencies m)
        (for/list ([w (campaign-manifest-waves m)])
          (list (campaign-wave-descriptor-index w)
                (campaign-wave-descriptor-title w)
                (campaign-wave-descriptor-doc-path w)
                (campaign-wave-descriptor-content-hash w)))
        (campaign-manifest-constraints-hash m)))

(define (wave->datum w)
  (list (campaign-wave-index w)
        (campaign-wave-title w)
        (campaign-wave-status w)
        (campaign-wave-attempt-count w)
        (and (campaign-wave-current-attempt w)
             (list (campaign-attempt-id (campaign-wave-current-attempt w))
                   (campaign-attempt-fence-token (campaign-wave-current-attempt w))
                   (campaign-attempt-started-at (campaign-wave-current-attempt w))))
        (campaign-wave-delivery-branch w)
        (campaign-wave-delivery-head-sha w)
        ;; v1.00.18 (BUG-0024 W3): 8th field is the infra-retry hand-off
        ;; context ("" when none). Legacy 7/5-field records load as "".
        (campaign-wave-attempt-context w)))

(define (datum->manifest d)
  (match d
    [(list 'manifest sv title deps wds ch)
     (make-campaign-manifest sv
                             title
                             deps
                             (for/list ([wd wds])
                               (make-campaign-wave-descriptor (list-ref wd 0)
                                                              (list-ref wd 1)
                                                              (list-ref wd 2)
                                                              (list-ref wd 3)))
                             ch)]))

(define (datum->wave d)
  (match d
    ;; v1.00.18 (BUG-0024 W3): 8-field form carries attempt-context.
    [(list idx title status acct attempt branch head-sha attempt-context)
     (define w
       (make-campaign-wave idx
                           title
                           status
                           acct
                           (and attempt
                                (match attempt
                                  [(list aid fence started) (campaign-attempt aid fence started)]))))
     (set-campaign-wave-delivery-branch! w branch)
     (set-campaign-wave-delivery-head-sha! w head-sha)
     (set-campaign-wave-attempt-context! w (if (string? attempt-context) attempt-context ""))
     w]
    ;; v1.00.17 W7: 7-field form carries delivery-branch/head-sha.
    [(list idx title status acct attempt branch head-sha)
     (define w
       (make-campaign-wave idx
                           title
                           status
                           acct
                           (and attempt
                                (match attempt
                                  [(list aid fence started) (campaign-attempt aid fence started)]))))
     (set-campaign-wave-delivery-branch! w branch)
     (set-campaign-wave-delivery-head-sha! w head-sha)
     w]
    ;; Legacy 5-field records (pre-W7) load with "" delivery fields.
    [(list idx title status acct attempt)
     (make-campaign-wave idx
                         title
                         status
                         acct
                         (and attempt
                              (match attempt
                                [(list aid fence started) (campaign-attempt aid fence started)])))]))

(define (datum->record d)
  (match d
    ;; v1.00.19 W3 (BUG-0031): 11-field form carries the build identity.
    [(list 'campaign-record
           pid
           m
           waves
           cancellation
           fence
           prov
           created
           updated
           build-version
           main-head-sha
           stale-override)
     (define rec
       (make-campaign-record pid
                             (datum->manifest m)
                             (map datum->wave waves)
                             (and cancellation
                                  (match cancellation
                                    [(list 'cancellation r t) (make-campaign-cancellation r t)]))
                             fence
                             prov
                             created
                             updated))
     (set-campaign-record-build-version! rec (if (string? build-version) build-version #f))
     (set-campaign-record-main-head-sha! rec (if (string? main-head-sha) main-head-sha #f))
     (set-campaign-record-stale-override! rec (if (boolean? stale-override) stale-override #f))
     rec]
    ;; Legacy 8-field records (pre-v1.00.19) MUST still load: missing build
    ;; identity is treated as absent (#f), never as corruption.
    [(list 'campaign-record pid m waves cancellation fence prov created updated)
     (make-campaign-record pid
                           (datum->manifest m)
                           (map datum->wave waves)
                           (and cancellation
                                (match cancellation
                                  [(list 'cancellation r t) (make-campaign-cancellation r t)]))
                           fence
                           prov
                           created
                           updated)]))

;; ============================================================
;; File lifecycle — atomic replace, containment, no-follow
;; ============================================================

(define (campaigns-dir-of base-dir)
  (build-path base-dir ".planning" "campaigns"))

;; True when p is a symbolic link (file-or-directory-type does not follow
;; links). Used for no-follow containment on read and write.
(define (symlink-target? p)
  (eq? (file-or-directory-type p) 'link))

(define (persist-campaign! base-dir rec)
  (validate-campaign-record! rec)
  (define dir (campaigns-dir-of base-dir))
  (make-directory* dir)
  (define target (build-path dir (string-append (campaign-plan-id rec) ".rktd")))
  (when (symlink-target? target)
    (corrupt! "refusing to overwrite symlinked campaign file ~a" target))
  (define tmp (build-path dir (format ".tmp-~a-~a" (campaign-plan-id rec) (random 1000000))))
  (define committed? #f)
  (dynamic-wind
   void
   (lambda ()
     (call-with-output-file tmp (lambda (out) (write (record->datum rec) out)) #:exists 'error)
     (rename-file-or-directory tmp target #t)
     (set! committed? #t))
   (lambda ()
     (unless committed?
       (when (file-exists? tmp)
         (delete-file tmp)))))
  (void))

(define (load-campaign-record base-dir plan-id)
  (validate-plan-id! plan-id)
  (define target (build-path (campaigns-dir-of base-dir) (string-append plan-id ".rktd")))
  (cond
    ;; no-follow first: file-exists? FOLLOWS links, so a dangling symlink
    ;; would otherwise be misreported as "no durable record" (MINOR-1).
    [(symlink-target? target) (corrupt! "refusing to read symlinked campaign file ~a" target)]
    [(not (file-exists? target)) #f]
    [else
     (define datum
       (with-handlers ([exn:fail?
                        (lambda (e)
                          (corrupt! "unreadable campaign file ~a: ~a" target (exn-message e)))])
         (call-with-input-file target read)))
     (define rec
       (with-handlers ([exn:fail?
                        (lambda (e)
                          (corrupt! "malformed campaign datum in ~a: ~a" target (exn-message e)))])
         (datum->record datum)))
     (validate-campaign-record! rec)
     rec]))

;; Boundary composition: seed from PLAN/STATE when no durable record exists,
;; otherwise return the persisted record. A corrupted durable record fails
;; closed — it is never silently re-migrated.
(define (load-or-migrate-campaign! base-dir)
  (define migrated (migrate-campaign! base-dir))
  (or (load-campaign-record base-dir (campaign-plan-id migrated))
      (begin
        (persist-campaign! base-dir migrated)
        migrated)))
