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
;;
;; v0.99.85 W3: Add contract-out for make-campaign-wave* validated constructor.

(require racket/file
         racket/string
         racket/port
         racket/path
         racket/list
         racket/format
         racket/contract
         (only-in "wave-docs.rkt"
                  parse-plan-index
                  wave-index-entry-idx
                  wave-index-entry-title
                  wave-index-entry-slug
                  wave-index-entry-status)
         (only-in "../../util/json/checksum.rkt" sha256-string))

;; ============================================================
;; Public API with contracts (§24)
;; ============================================================

(provide (contract-out
          [make-campaign-wave*
           (->i ([index (lambda (i) (and (exact-nonnegative-integer? i) (< i 1000)))]
                 [title string?]
                 [status (lambda (s) (and (symbol? s) (memq s CANONICAL-WAVE-STATUSES)))]
                 [attempt-count exact-nonnegative-integer?]
                 [current-attempt (lambda (a) (or (not a) (campaign-attempt? a)))])
                (lambda (index title status attempt-count current-attempt) campaign-wave?))]
          [canonical-wave-status (-> (or/c string? symbol?) symbol?)]
          [completed-status? (-> symbol? boolean?)]
          [retryable-status? (-> symbol? boolean?)]
          [actionable-status? (-> symbol? boolean?)]
          [campaign-manifest-hash (-> campaign-manifest? string?)]
          [plan-changed? (-> campaign-record? campaign-manifest? boolean?)]
          [select-next-actionable-wave (-> campaign-record? (or/c #f exact-nonnegative-integer?))]
          [restart-needed? (-> campaign-record? exact-nonnegative-integer? boolean?)]
          [one-active-wave-violation (-> campaign-record? list?)]
          [begin-attempt!
           (-> campaign-record?
               exact-nonnegative-integer?
               (or/c #f exact-nonnegative-integer?)
               void?)]
          [migrate-campaign! (-> path-string? campaign-record?)]
          [make-campaign-manifest
           (-> exact-nonnegative-integer?
               string?
               (listof string?)
               (listof campaign-wave-descriptor?)
               string?
               campaign-manifest?)]
          [make-campaign-wave-descriptor
           (-> exact-nonnegative-integer? string? string? string? campaign-wave-descriptor?)]
          [campaign-wave-descriptor-index (-> campaign-wave-descriptor? exact-nonnegative-integer?)]
          [campaign-wave-descriptor-title (-> campaign-wave-descriptor? string?)]
          [campaign-wave-descriptor-doc-path (-> campaign-wave-descriptor? string?)]
          [campaign-wave-descriptor-content-hash (-> campaign-wave-descriptor? string?)]
          [campaign-wave-descriptor? (-> any/c boolean?)]
          [make-campaign-wave
           (-> exact-nonnegative-integer?
               string?
               symbol?
               exact-nonnegative-integer?
               (or/c #f campaign-attempt?)
               campaign-wave?)]
          [make-campaign-record
           (-> string?
               campaign-manifest?
               (listof campaign-wave?)
               (or/c #f campaign-cancellation?)
               (or/c #f exact-nonnegative-integer?)
               (or/c #f string? symbol?)
               exact-integer?
               exact-integer?
               campaign-record?)]
          [make-campaign-cancellation (-> string? exact-integer? campaign-cancellation?)]
          [campaign-attempt
           (-> string? (or/c #f exact-nonnegative-integer?) exact-integer? campaign-attempt?)]
          [campaign-attempt? (-> any/c boolean?)]
          [campaign-wave? (-> any/c boolean?)]
          [campaign-record? (-> any/c boolean?)]
          [campaign-cancellation? (-> any/c boolean?)]
          [campaign-cancellation-reason (-> campaign-cancellation? string?)]
          [campaign-cancellation-timestamp (-> campaign-cancellation? exact-integer?)]
          [campaign-plan-id (-> campaign-record? string?)]
          [campaign-fence-token (-> campaign-record? (or/c #f exact-nonnegative-integer?))]
          [set-campaign-cancellation! (-> campaign-record? any/c void?)]
          [set-campaign-fence-token! (-> campaign-record? exact-nonnegative-integer? void?)]
          [campaign-record-waves (-> campaign-record? (listof campaign-wave?))]
          [campaign-record-manifest (-> campaign-record? campaign-manifest?)]
          [campaign-record-cancellation (-> campaign-record? (or/c #f campaign-cancellation?))]
          [campaign-record-provenance (-> campaign-record? (or/c #f string? symbol?))]
          [campaign-record-created-at (-> campaign-record? exact-integer?)]
          [campaign-record-updated-at (-> campaign-record? exact-integer?)]
          [campaign-wave-index (-> campaign-wave? exact-nonnegative-integer?)]
          [campaign-wave-title (-> campaign-wave? string?)]
          [campaign-wave-status (-> campaign-wave? symbol?)]
          [campaign-wave-attempt-count (-> campaign-wave? exact-nonnegative-integer?)]
          [campaign-wave-current-attempt (-> campaign-wave? (or/c #f campaign-attempt?))]
          [set-campaign-wave-status! (-> campaign-wave? symbol? void?)]
          [set-campaign-wave-attempt-count! (-> campaign-wave? exact-nonnegative-integer? void?)]
          [set-campaign-wave-current-attempt! (-> campaign-wave? (or/c #f campaign-attempt?) void?)]
          [campaign-attempt-id (-> campaign-attempt? string?)]
          [campaign-attempt-fence-token (-> campaign-attempt? (or/c #f exact-nonnegative-integer?))]
          [campaign-attempt-started-at (-> campaign-attempt? exact-integer?)]
          [campaign-manifest-schema-version (-> campaign-manifest? exact-nonnegative-integer?)]
          [campaign-manifest-title (-> campaign-manifest? string?)]
          [campaign-manifest-dependencies (-> campaign-manifest? (listof string?))]
          [campaign-manifest-waves (-> campaign-manifest? (listof campaign-wave-descriptor?))]
          [campaign-manifest-constraints-hash (-> campaign-manifest? string?)]
          [exn:fail:campaign-migration? (-> any/c boolean?)]
          [CANONICAL-WAVE-STATUSES (listof symbol?)]
          [canonical-wave-statuses (listof symbol?)]))

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

;; Validated constructor for public use — enforces domain constraints per §24.
(define (make-campaign-wave* index title status attempt-count current-attempt)
  (unless (and (exact-nonnegative-integer? index) (< index 1000))
    (raise-argument-error 'make-campaign-wave* "non-negative integer < 1000" index))
  (unless (string? title)
    (raise-argument-error 'make-campaign-wave* "string?" title))
  (unless (and (symbol? status) (memq status CANONICAL-WAVE-STATUSES))
    (raise-argument-error 'make-campaign-wave* (format "one of ~s" CANONICAL-WAVE-STATUSES) status))
  (unless (exact-nonnegative-integer? attempt-count)
    (raise-argument-error 'make-campaign-wave* "non-negative integer" attempt-count))
  (make-campaign-wave index title status attempt-count current-attempt))

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

;; F-6: Wave identities (idx + title) used to detect new campaigns.
;; When PLAN.md and STATE.md disagree on rows, we check whether the
;; wave *identities* differ (new campaign) vs just statuses (corruption).
(define (plan-wave-identities text)
  (for/list ([e (parse-plan-index text)])
    (cons (wave-index-entry-idx e) (wave-index-entry-title e))))

(define (state-wave-identities text)
  (for/list ([line (string-split text "\n")]
             #:when (regexp-match state-row-rx line))
    (define m (regexp-match state-row-rx line))
    (cons (string->number (cadr m)) (string-trim (list-ref m 2)))))

(define (wave-doc-content-hash base-dir idx slug)
  (define p (build-path base-dir ".planning" "waves" (format "W~a-~a.md" idx slug)))
  (if (file-exists? p)
      (sha256-string (strip-wave-doc-status (call-with-input-file p port->string)))
      (sha256-string "")))

;; v0.99.90 W5 (#9236): the manifest hash (plan-id) must be STABLE across
;; projection updates. Wave docs carry a mutable "Status:" header that the
;; completion/failure projections rewrite (Inbox -> DONE/FAILED); hashing the
;; raw file would change the plan-id after every wave, so
;; load-or-migrate-campaign! would re-migrate and orphan the durable record
;; and its outbox (Campaign Truth lost on restart). Hash only the doc body.
(define wave-doc-status-header-rx #rx"^# Wave [0-9]+\nStatus: [^\n]+\n\n")

(define (strip-wave-doc-status text)
  (define m (regexp-match wave-doc-status-header-rx text))
  (if m
      (substring text (string-length (car m)))
      text))

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
     (cond
       [(equal? plan-rows* state-rows*) (seed-record base-dir plan-text 'plan-and-state)]
       ;; F-6: If wave identities differ (different titles or counts), this is a
       ;; new campaign. Auto-resolve by re-seeding from PLAN.md rather than
       ;; failing closed. This prevents /go from crashing after /plan rewrites
       ;; PLAN.md with a new campaign while STATE.md still has the old one.
       [(not (equal? (plan-wave-identities plan-text) (state-wave-identities state-text)))
        (seed-record base-dir plan-text 'plan-and-state)]
       ;; Same wave identities but different statuses — potential corruption.
       ;; Keep fail-closed for safety (D3 invariant).
       [else
        (raise
         (exn:fail:campaign-migration
          (format "PLAN.md and STATE.md disagree on wave statuses: ~a vs ~a" plan-rows* state-rows*)
          (current-continuation-marks)))])]
    [plan-present? (seed-record base-dir (call-with-input-file plan-path port->string) 'plan)]
    [state-present? (seed-record base-dir "" 'state)]
    [else
     (raise (exn:fail:campaign-migration
             "no durable plan source: neither .planning/PLAN.md nor .planning/STATE.md exists"
             (current-continuation-marks)))]))

;; ============================================================
;; v0.99.90 W1 (#9232): .rktd persistence moved to campaign-repository.rkt
;; (fail-closed validation, atomic replace, path containment, no-follow,
;; backward-compatible load, and load-or-migrate-campaign!).
