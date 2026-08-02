#lang racket/base

;; extensions/gsd/campaign-state.rkt — Durable Campaign State, Identity, and Reconstruction
;;
;; v0.99.80 W0: GC-2, GC-3, GC-8, GC-10, GC-14
;;
;; D2 — Stable plan identity and authoritative campaign record:
;;   immutable manifest (schema version, normalized title, dependency metadata,
;;   ordered wave descriptors with doc-path + content hashes, global immutable
;;   constraints hash); `plan-id` = SHA-256 of the manifest.  Mutable status
;;   markers, timestamps, campaign IDs, and evidence are excluded, so
;;   status-only projection changes preserve identity.
;;
;; D3 — Initial migration truth: no campaign record -> parse validated PLAN.md,
;;   then STATE table; ignore in-memory executor state; PLAN==STATE seeds with
;;   provenance; both-conflict fails closed; never infer completion from
;;   docs/edits/mode/prior-wave completion.
;;
;; D4 — Canonical status mapping: PENDING/IN-PROGRESS/VERIFYING/DONE/FAILED/
;;   INTERRUPTED/DEFERRED.  No REWORK status; FAILED never counts as successful
;;   completion; INTERRUPTED retries with a new attempt.
;;
;; D5 — Fencing: every transition requires the current fencing token and
;;   attempt ID; cancellation is a durable record field.

(require racket/file
         racket/string
         racket/port
         racket/path
         racket/list
         racket/match
         racket/format
         (only-in "wave-docs.rkt"
                  parse-plan-index
                  wave-index-entry-idx
                  wave-index-entry-title
                  wave-index-entry-slug
                  wave-index-entry-status)
         (only-in "../../util/json/checksum.rkt" sha256-string))

;; ============================================================
;; Records
;; ============================================================

;; Immutable manifest — status/timestamps/evidence excluded (D2).
(struct campaign-manifest (schema-version title dependencies waves constraints-hash)
  #:transparent
  #:constructor-name make-campaign-manifest)

;; Ordered wave descriptor: index, title, doc path, content hash.
(struct campaign-wave-descriptor (index title doc-path content-hash)
  #:transparent
  #:constructor-name make-campaign-wave-descriptor)

;; Mutable per-wave projection of the durable campaign record.
(struct campaign-wave (index title status attempt-count current-attempt)
  #:transparent
  #:mutable
  #:constructor-name make-campaign-wave)

;; A single run attempt; fence-token guards stale results.
(struct campaign-attempt (id fence-token started-at) #:transparent)

;; Durable cancellation request (D5).
(struct campaign-cancellation (reason timestamp)
  #:transparent
  #:constructor-name make-campaign-cancellation)

;; Authoritative campaign record (D2).  plan-id == manifest hash.
;; #:mutable only for cancellation and fence-token (D5 restart safety).
(struct campaign-record
        (plan-id manifest waves cancellation fence-token provenance created-at updated-at)
  #:transparent
  #:mutable
  #:constructor-name make-campaign-record)

;; Raised when migration sources conflict (D3 fail-closed).
(struct exn:fail:campaign-migration exn:fail () #:transparent)

;; Aliases for names used in tests and external code — the struct field
;; accessors are campaign-record-plan-id / campaign-record-fence-token but
;; the API contract uses the shorter campaign-plan-id / campaign-fence-token.
(define campaign-plan-id campaign-record-plan-id)
(define campaign-fence-token campaign-record-fence-token)
(define (set-campaign-cancellation! rec val)
  (set-campaign-record-cancellation! rec val))
(define (set-campaign-fence-token! rec val)
  (set-campaign-record-fence-token! rec val))

;; ============================================================
;; Canonical status mapping (D4)
;; ============================================================

(define CANONICAL-WAVE-STATUSES '(pending in-progress verifying done failed interrupted deferred))

(define canonical-wave-statuses CANONICAL-WAVE-STATUSES)

(define (canonical-wave-status s)
  (define up
    (string-upcase (if (symbol? s)
                       (symbol->string s)
                       s)))
  (cond
    [(or (string=? up "INBOX") (string=? up "PENDING") (string=? up "NOT STARTED")) 'pending]
    [(or (string=? up "IN-PROGRESS") (string=? up "IN PROGRESS")) 'in-progress]
    [(string=? up "VERIFYING") 'verifying]
    [(or (string=? up "DONE") (string=? up "COMPLETED")) 'done]
    [(string=? up "FAILED") 'failed]
    [(string=? up "INTERRUPTED") 'interrupted]
    [(string=? up "DEFERRED") 'deferred]
    [else 'pending]))

(define (completed-status? s)
  (eq? s 'done))

(define (retryable-status? s)
  (and (memq s '(failed interrupted)) #t))

(define (actionable-status? s)
  (and (memq s '(pending in-progress verifying failed interrupted)) #t))

;; ============================================================
;; Manifest identity (D2)
;; ============================================================

(define (manifest->canonical-string m)
  (format "~s"
          (list (campaign-manifest-schema-version m)
                (campaign-manifest-title m)
                (campaign-manifest-dependencies m)
                (for/list ([w (campaign-manifest-waves m)])
                  (list (campaign-wave-descriptor-index w)
                        (campaign-wave-descriptor-title w)
                        (campaign-wave-descriptor-doc-path w)
                        (campaign-wave-descriptor-content-hash w)))
                (campaign-manifest-constraints-hash m))))

(define (campaign-manifest-hash m)
  (sha256-string (manifest->canonical-string m)))

(define (plan-changed? rec manifest)
  (not (string=? (campaign-manifest-hash manifest) (campaign-plan-id rec))))

;; ============================================================
;; Selection / reconciliation (D4, one-active-wave invariant)
;; ============================================================

(define (select-next-actionable-wave rec)
  (for/first ([w (campaign-record-waves rec)]
              #:when (actionable-status? (campaign-wave-status w)))
    (campaign-wave-index w)))

(define (restart-needed? rec wave-idx)
  (for/or ([w (campaign-record-waves rec)]
           #:when (= (campaign-wave-index w) wave-idx))
    (actionable-status? (campaign-wave-status w))))

(define (one-active-wave-violation rec)
  (define active
    (for/list ([w (campaign-record-waves rec)]
               #:when (memq (campaign-wave-status w) '(in-progress verifying)))
      (campaign-wave-index w)))
  (if (> (length active) 1)
      active
      '()))

;; ============================================================
;; Attempt lifecycle (D5 fencing)
;; ============================================================

(define (begin-attempt! rec wave-idx fence-token)
  (define waves (campaign-record-waves rec))
  (define active-others
    (for/list ([w waves]
               #:when (and (memq (campaign-wave-status w) '(in-progress verifying))
                           (not (= (campaign-wave-index w) wave-idx))))
      (campaign-wave-index w)))
  (when (pair? active-others)
    (error 'begin-attempt!
           "wave(s) ~a active; cannot begin wave ~a (one-active-wave invariant)"
           active-others
           wave-idx))
  (for ([w waves]
        #:when (= (campaign-wave-index w) wave-idx))
    (define new-count (add1 (campaign-wave-attempt-count w)))
    (set-campaign-wave-status! w 'in-progress)
    (set-campaign-wave-attempt-count! w new-count)
    (set-campaign-wave-current-attempt!
     w
     (campaign-attempt (format "attempt-~a" new-count) fence-token (current-seconds)))))

;; ============================================================
;; Initial migration truth (D3)
;; ============================================================

(define plan-title-rx #rx"^# +Plan: +(.*)$")
(define state-row-rx #rx"^\\| *W([0-9]+) *\\| *([^|]+) *\\| *([^|]+) *\\|")

(define (extract-plan-title text)
  (for/first ([line (string-split text "\n")]
              #:when (regexp-match plan-title-rx line))
    (string-trim (cadr (regexp-match plan-title-rx line)))))

(define (state-rows text)
  (for/list ([line (string-split text "\n")]
             #:when (regexp-match state-row-rx line))
    (define m (regexp-match state-row-rx line))
    (list (string->number (cadr m)) (canonical-wave-status (string-trim (list-ref m 3))))))

(define (plan-rows text)
  (for/list ([e (parse-plan-index text)])
    (list (wave-index-entry-idx e) (canonical-wave-status (wave-index-entry-status e)))))

(define (wave-doc-content-hash base-dir idx slug)
  (define p (build-path base-dir ".planning" "waves" (format "W~a-~a.md" idx slug)))
  (if (file-exists? p)
      (sha256-string (call-with-input-file p port->string))
      (sha256-string "")))

(define (seed-record base-dir plan-text provenance)
  (define entries (parse-plan-index plan-text))
  (define title (or (extract-plan-title plan-text) "Plan"))
  (define descriptors
    (for/list ([e entries])
      (make-campaign-wave-descriptor
       (wave-index-entry-idx e)
       (wave-index-entry-title e)
       (format "waves/W~a-~a.md" (wave-index-entry-idx e) (wave-index-entry-slug e))
       (wave-doc-content-hash base-dir (wave-index-entry-idx e) (wave-index-entry-slug e)))))
  (define waves
    (for/list ([e entries])
      (make-campaign-wave (wave-index-entry-idx e)
                          (wave-index-entry-title e)
                          (canonical-wave-status (wave-index-entry-status e))
                          0
                          #f)))
  (define m
    (make-campaign-manifest 1
                            title
                            '()
                            descriptors
                            (sha256-string "immutable-global-constraints-v1")))
  (make-campaign-record (campaign-manifest-hash m)
                        m
                        waves
                        #f
                        0
                        provenance
                        (current-seconds)
                        (current-seconds)))

(define (migrate-campaign! base-dir)
  (define plan-path (build-path base-dir ".planning" "PLAN.md"))
  (define state-path (build-path base-dir ".planning" "STATE.md"))
  (define plan-present? (file-exists? plan-path))
  (define state-present? (file-exists? state-path))
  (cond
    [(and plan-present? state-present?)
     (define plan-text (call-with-input-file plan-path port->string))
     (define state-text (call-with-input-file state-path port->string))
     (define plan-rows* (plan-rows plan-text))
     (define state-rows* (state-rows state-text))
     (if (equal? plan-rows* state-rows*)
         (seed-record base-dir plan-text 'plan-and-state)
         (raise
          (exn:fail:campaign-migration
           (format "PLAN.md and STATE.md disagree on wave statuses: ~a vs ~a" plan-rows* state-rows*)
           (current-continuation-marks))))]
    [plan-present? (seed-record base-dir (call-with-input-file plan-path port->string) 'plan)]
    [state-present? (seed-record base-dir "" 'state)]
    [else
     (raise (exn:fail:campaign-migration
             "no durable plan source: neither .planning/PLAN.md nor .planning/STATE.md exists"
             (current-continuation-marks)))]))

;; ============================================================
;; Atomic persistence (D2)
;; ============================================================

(define (record->datum rec)
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
        (campaign-record-updated-at rec)))

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
                   (campaign-attempt-started-at (campaign-wave-current-attempt w))))))

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

(define (persist-campaign! base-dir rec)
  (define campaigns-dir (build-path base-dir ".planning" "campaigns"))
  (make-directory* campaigns-dir)
  (define target (build-path campaigns-dir (string-append (campaign-plan-id rec) ".rktd")))
  (define tmp
    (build-path campaigns-dir (format ".tmp-~a-~a" (campaign-plan-id rec) (random 1000000))))
  (call-with-output-file tmp #:exists 'truncate (lambda (out) (write (record->datum rec) out)))
  (rename-file-or-directory tmp target #t))

(define (load-campaign-record base-dir plan-id)
  (define target (build-path base-dir ".planning" "campaigns" (string-append plan-id ".rktd")))
  (if (file-exists? target)
      (datum->record (call-with-input-file target read))
      #f))

;; ============================================================
;; Provide
;; ============================================================

(provide make-campaign-manifest
         campaign-manifest?
         campaign-manifest-schema-version
         campaign-manifest-title
         campaign-manifest-dependencies
         campaign-manifest-waves
         campaign-manifest-constraints-hash
         make-campaign-wave-descriptor
         campaign-wave-descriptor?
         campaign-wave-descriptor-index
         campaign-wave-descriptor-title
         campaign-wave-descriptor-doc-path
         campaign-wave-descriptor-content-hash
         make-campaign-wave
         campaign-wave?
         campaign-wave-index
         campaign-wave-title
         campaign-wave-status
         campaign-wave-attempt-count
         campaign-wave-current-attempt
         set-campaign-wave-status!
         set-campaign-wave-attempt-count!
         set-campaign-wave-current-attempt!
         campaign-attempt
         campaign-attempt?
         campaign-attempt-id
         campaign-attempt-fence-token
         campaign-attempt-started-at
         make-campaign-cancellation
         campaign-cancellation?
         campaign-cancellation-reason
         campaign-cancellation-timestamp
         make-campaign-record
         campaign-record?
         campaign-plan-id
         campaign-record-manifest
         campaign-record-waves
         campaign-record-cancellation
         campaign-fence-token
         campaign-record-provenance
         campaign-record-created-at
         campaign-record-updated-at
         set-campaign-cancellation!
         set-campaign-fence-token!
         exn:fail:campaign-migration
         exn:fail:campaign-migration?
         canonical-wave-status
         canonical-wave-statuses
         completed-status?
         retryable-status?
         actionable-status?
         campaign-manifest-hash
         plan-changed?
         select-next-actionable-wave
         restart-needed?
         one-active-wave-violation
         begin-attempt!
         migrate-campaign!
         persist-campaign!
         load-campaign-record)
