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
          ;; v1.00.22 W5 (BUG-0039): usage accounting & budget ceilings
          [usage-datum
           (-> (or/c #f number?)
               (or/c #f number?)
               (or/c #f number?)
               (or/c #f (and/c real? positive?))
               any/c
               usage-datum?)]
          [usage-datum? (-> any/c boolean?)]
          [usage-datum-input-tokens (-> usage-datum? (or/c #f number?))]
          [usage-datum-output-tokens (-> usage-datum? (or/c #f number?))]
          [usage-datum-total-tokens (-> usage-datum? (or/c #f number?))]
          [usage-datum-cost-usd (-> usage-datum? (or/c #f (and/c real? positive?)))]
          [usage-datum-estimated? (-> usage-datum? any/c)]
          [usage-summary? (-> any/c boolean?)]
          [usage-summary-input-tokens (-> usage-summary? (or/c #f number?))]
          [usage-summary-output-tokens (-> usage-summary? (or/c #f number?))]
          [usage-summary-total-tokens (-> usage-summary? (or/c #f number?))]
          [usage-summary-cost-usd (-> usage-summary? (or/c #f real?))]
          [usage-summary-attempts-with-usage (-> usage-summary? exact-nonnegative-integer?)]
          [usage-summary-missing-attempts (-> usage-summary? exact-nonnegative-integer?)]
          [stamp-wave-usage!
           (-> campaign-record?
               exact-nonnegative-integer?
               (or/c #f usage-datum? 'usage-missing)
               void?)]
          [wave-usage-summary (-> campaign-wave? usage-summary?)]
          [wave-usage-input-tokens (-> campaign-wave? (or/c #f number?))]
          [wave-usage-output-tokens (-> campaign-wave? (or/c #f number?))]
          [wave-usage-total-tokens (-> campaign-wave? (or/c #f number?))]
          [wave-usage-cost-usd (-> campaign-wave? (or/c #f real?))]
          [wave-usage-source (-> campaign-wave? symbol?)]
          [wave-usage-missing-attempts (-> campaign-wave? exact-nonnegative-integer?)]
          [campaign-usage-summary (-> campaign-record? usage-summary?)]
          [campaign-budget-pause
           (-> (or/c 'max-cost 'max-tokens)
               real?
               real?
               string?
               exact-integer?
               campaign-budget-pause?)]
          [campaign-budget-pause? (-> any/c boolean?)]
          [campaign-budget-pause-kind (-> campaign-budget-pause? (or/c 'max-cost 'max-tokens))]
          [campaign-budget-pause-ceiling (-> campaign-budget-pause? real?)]
          [campaign-budget-pause-observed (-> campaign-budget-pause? real?)]
          [campaign-budget-pause-message (-> campaign-budget-pause? string?)]
          [campaign-budget-pause-timestamp (-> campaign-budget-pause? exact-integer?)]
          [budget-pause-violation?
           (-> campaign-record?
               (or/c #f (and/c real? positive?))
               (or/c #f exact-positive-integer?)
               (or/c #f campaign-budget-pause?))]
          [budget-pause-still-violated?
           (-> campaign-budget-pause?
               (or/c #f (and/c real? positive?))
               (or/c #f exact-positive-integer?)
               boolean?)]
          [pause-campaign-for-budget! (-> campaign-record? campaign-budget-pause? void?)]
          [clear-budget-pause! (-> campaign-record? void?)]
          [campaign-record-budget-pause (-> campaign-record? (or/c #f campaign-budget-pause?))]
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
          [campaign-record-build-version (-> campaign-record? (or/c #f string?))]
          [campaign-record-main-head-sha (-> campaign-record? (or/c #f string?))]
          [campaign-record-stale-override (-> campaign-record? any/c)]
          [set-campaign-record-build-version! (-> campaign-record? (or/c #f string?) void?)]
          [set-campaign-record-main-head-sha! (-> campaign-record? (or/c #f string?) void?)]
          [set-campaign-record-stale-override! (-> campaign-record? any/c void?)]
          [campaign-wave-index (-> campaign-wave? exact-nonnegative-integer?)]
          [campaign-wave-title (-> campaign-wave? string?)]
          [campaign-wave-status (-> campaign-wave? symbol?)]
          [campaign-wave-attempt-count (-> campaign-wave? exact-nonnegative-integer?)]
          [campaign-wave-current-attempt (-> campaign-wave? (or/c #f campaign-attempt?))]
          [set-campaign-wave-status! (-> campaign-wave? symbol? void?)]
          [set-campaign-wave-attempt-count! (-> campaign-wave? exact-nonnegative-integer? void?)]
          [set-campaign-wave-current-attempt! (-> campaign-wave? (or/c #f campaign-attempt?) void?)]
          [campaign-wave-delivery-branch (-> campaign-wave? string?)]
          [campaign-wave-delivery-head-sha (-> campaign-wave? string?)]
          [set-campaign-wave-delivery-branch! (-> campaign-wave? string? void?)]
          [set-campaign-wave-delivery-head-sha! (-> campaign-wave? string? void?)]
          [campaign-wave-attempt-context (-> campaign-wave? string?)]
          [set-campaign-wave-attempt-context! (-> campaign-wave? string? void?)]
          ;; v1.00.21 W5 (BUG-0029): attempt-artifact ledger.
          [wave-artifact-ledger (-> campaign-wave? list?)]
          [set-campaign-wave-artifact-ledger! (-> campaign-wave? list? void?)]
          [make-campaign-artifact-entry
           (-> (and/c string? (lambda (s) (positive? (string-length s))))
               string?
               string?
               string?
               campaign-artifact-entry?)]
          [campaign-artifact-entry? (-> any/c boolean?)]
          [campaign-artifact-entry-attempt-id (-> campaign-artifact-entry? string?)]
          [campaign-artifact-entry-branch (-> campaign-artifact-entry? string?)]
          [campaign-artifact-entry-worktree-path (-> campaign-artifact-entry? string?)]
          [campaign-artifact-entry-base-sha (-> campaign-artifact-entry? string?)]
          [campaign-artifact-entry-terminal-status (-> campaign-artifact-entry? symbol?)]
          [campaign-artifact-entry-merge-status (-> campaign-artifact-entry? symbol?)]
          [campaign-artifact-entry-teardown-status (-> campaign-artifact-entry? symbol?)]
          [set-campaign-artifact-entry-terminal-status! (-> campaign-artifact-entry? symbol? void?)]
          [set-campaign-artifact-entry-merge-status! (-> campaign-artifact-entry? symbol? void?)]
          [set-campaign-artifact-entry-teardown-status! (-> campaign-artifact-entry? symbol? void?)]
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

;; v1.00.21 W5 (BUG-0029): attempt-artifact ledger entry. Every attempt
;; that creates durable artifacts (delivery branch + per-wave worktree)
;; gets one entry at attempt START; terminal transitions (success/failure/
;; cancel/interrupt) update it; teardown results are noted at release.
;; Base fields are set at construction; the trailing fields are mutated
;; by the orchestrator lifecycle (never by deserialization directly).
;; terminal-status : symbol — 'running (not yet terminal) or one of
;;                   'success 'failed 'interrupted 'cancelled 'superseded
;; merge-status    : symbol — 'undetermined or a locally-determined
;;                   verdict ('merged-into-base / 'unmerged)
;; teardown-status : symbol — 'pending, 'removed,
;;                   'worktree-remove-failed, 'branch-delete-failed,
;;                   'worktree-removed-branch-kept ...
(struct campaign-artifact-entry
        (attempt-id branch
                    worktree-path
                    base-sha
                    [terminal-status #:auto]
                    [merge-status #:auto]
                    [teardown-status #:auto])
  #:transparent
  #:mutable
  #:constructor-name raw-make-campaign-artifact-entry
  #:auto-value 'running)

;; Validated constructor — enforces the start-time invariants and the
;; per-field defaults for the lifecycle fields.
(define (make-campaign-artifact-entry attempt-id branch worktree-path base-sha)
  (unless (and (string? attempt-id) (positive? (string-length attempt-id)))
    (raise-argument-error 'make-campaign-artifact-entry "non-empty string" attempt-id))
  (unless (string? branch)
    (raise-argument-error 'make-campaign-artifact-entry "string?" branch))
  (unless (string? worktree-path)
    (raise-argument-error 'make-campaign-artifact-entry "string?" worktree-path))
  (unless (string? base-sha)
    (raise-argument-error 'make-campaign-artifact-entry "string?" base-sha))
  (define e (raw-make-campaign-artifact-entry attempt-id branch worktree-path base-sha))
  (set-campaign-artifact-entry-merge-status! e 'undetermined)
  (set-campaign-artifact-entry-teardown-status! e 'pending)
  e)

;; Mutable per-wave projection of the durable campaign record.
;; v1.00.17 W7 (#9512b): delivery-branch / delivery-head-sha record the
;; branch the wave's changes live on and the head SHA at approval time.
;; #:auto (default "") keeps the 5-arg constructor unchanged, so legacy
;; campaign records on disk remain loadable.
;; v1.00.21 W5 (BUG-0029): artifact-ledger — the per-wave list of
;; campaign-artifact-entry values (one per attempt that created durable
;; artifacts). The struct-level #:auto-value is "" (a single shared
;; default); ALL reads go through wave-artifact-ledger, which normalizes
;; the sentinel to '(), so absent/legacy records behave exactly like an
;; empty ledger. Serialization of pre-W5 records tolerates the missing
;; 9th field (same tolerance rule as attempt-context / delivery fields).
(struct campaign-wave
        (index title
               status
               attempt-count
               current-attempt
               [delivery-branch #:auto]
               [delivery-head-sha #:auto]
               ;; v1.00.18 (BUG-0024 W3): durable hand-off context captured
               ;; from the prior executor session when it died on an infra
               ;; failure ("" = none). Consumed by the next attempt's prompt.
               [attempt-context #:auto]
               [artifact-ledger #:auto]
               ;; v1.00.22 W5 (BUG-0039): cumulative per-wave usage
               ;; (sum over stamped attempts). The struct-level shared
               ;; #:auto-value "" applies; ALL reads go through the
               ;; wave-usage-* normalizing accessors below ("" → #f),
               ;; mirroring the wave-artifact-ledger pattern.
               [usage-input-tokens #:auto]
               [usage-output-tokens #:auto]
               [usage-total-tokens #:auto]
               [usage-cost-usd #:auto]
               [usage-source #:auto]
               [usage-missing-attempts #:auto])
  #:transparent
  #:mutable
  #:constructor-name make-campaign-wave
  #:auto-value "")

;; Normalizing accessor: the sentinel default "" reads as '()' so callers
;; never see the auto-value leak of the struct definition.
(define (wave-artifact-ledger w)
  (define ledger (campaign-wave-artifact-ledger w))
  (if (list? ledger)
      ledger
      '()))

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
;; v1.00.22 W5 (BUG-0039): per-attempt cost/token telemetry. Fields are
;; stamped at attempt boundaries from loop-result usage metadata when
;; present. usage-source : #f (unstamped) | 'provider | 'provider-estimated
;; | 'usage-missing. When usage is absent the source is 'usage-missing and
;; the token/cost fields stay #f — honest accounting, NEVER faked zeros.
;; #:auto keeps the 3-arg constructor (and every existing caller) valid;
;; legacy records deserialize with #f usage (absent ≠ zero).
(struct campaign-attempt
        (id fence-token
            started-at
            [input-tokens #:auto #:mutable]
            [output-tokens #:auto #:mutable]
            [total-tokens #:auto #:mutable]
            [cost-usd #:auto #:mutable]
            [usage-source #:auto #:mutable])
  #:transparent
  #:auto-value #f)

;; Durable cancellation request (D5).
(struct campaign-cancellation (reason timestamp)
  #:transparent
  #:constructor-name make-campaign-cancellation)

;; Authoritative campaign record (D2).  plan-id == manifest hash.
;; #:mutable only for cancellation and fence-token (D5 restart safety).
;; v1.00.19 W3 (BUG-0031): build identity is recorded at campaign start and
;; rides the record through every wave report/evidence write (the record is
;; the durable evidence store — each persist rewrites it wholesale).
;;   build-version : exact (q-version) string of the RUNNING process — this
;;                   is what actually produced the evidence, not what is on
;;                   disk at analysis time.
;;   main-head-sha : origin/main HEAD at campaign start (best-effort; #f
;;                   outside a work tree / offline — must never fail a run).
;;   stale-override: #f, or #t when the operator bypassed the freshness
;;                   refusal with an explicit `allow-stale`.
;; #:auto keeps the 8-arg constructor unchanged, so legacy campaign records
;; on disk (and every existing caller) remain valid; pre-v1.00.19 records
;; deserialize with #f identity (absent ≠ corrupt).
(struct campaign-record
        (plan-id manifest
                 waves
                 cancellation
                 fence-token
                 provenance
                 created-at
                 updated-at
                 [build-version #:auto]
                 [main-head-sha #:auto]
                 [stale-override #:auto]
                 ;; v1.00.22 W5 (BUG-0039): durable budget pause. Set when
                 ;; cumulative spend crossed gsd.campaign.max-cost /
                 ;; gsd.campaign.max-tokens; cleared when the operator raises
                 ;; the ceiling and resumes. #f (default) = no pause.
                 [budget-pause #:auto])
  #:transparent
  #:mutable
  #:auto-value #f
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
;; v1.00.22 W5 (BUG-0039): usage accounting & budget ceilings
;; ============================================================

;; A single usage observation extracted from loop-result metadata.
;;   input/output/total-tokens : number or #f (#f = not reported)
;;   cost-usd                  : positive real or #f (#f = unknown — the
;;                               provider did not report a cost)
;;   estimated?                : #t when tokens were locally estimated
(struct usage-datum (input-tokens output-tokens total-tokens cost-usd estimated?) #:transparent)

;; Aggregated spend for a wave or the whole campaign.
;;   missing-attempts       : attempts that ended with usage-missing
;;   attempts-with-usage    : attempts that reported real usage
;;   cost-usd               : known-cost sum, or #f when ANY counted
;;                            attempt had unknown cost (never fake zeros)
(struct usage-summary
        (input-tokens output-tokens total-tokens cost-usd attempts-with-usage missing-attempts)
  #:transparent)

;; Durable budget pause (see campaign-record budget-pause field).
;;   kind     : 'max-cost | 'max-tokens
;;   ceiling  : the crossed ceiling value
;;   observed : the spend that crossed it
;;   message  : operator-facing text naming the ceiling + how to raise it
(struct campaign-budget-pause (kind ceiling observed message timestamp) #:transparent)

;; Normalizing accessors: struct #:auto-value "" must never leak.
(define (wave-usage-field w raw)
  (and (number? raw) raw))

(define (wave-usage-input-tokens w)
  (wave-usage-field w (campaign-wave-usage-input-tokens w)))
(define (wave-usage-output-tokens w)
  (wave-usage-field w (campaign-wave-usage-output-tokens w)))
(define (wave-usage-total-tokens w)
  (wave-usage-field w (campaign-wave-usage-total-tokens w)))
(define (wave-usage-cost-usd w)
  (define c (campaign-wave-usage-cost-usd w))
  (and (real? c) c))
(define (wave-usage-source w)
  (define s (campaign-wave-usage-source w))
  (if (symbol? s) s 'none))
(define (wave-usage-missing-attempts w)
  (define n (campaign-wave-usage-missing-attempts w))
  (if (exact-nonnegative-integer? n) n 0))

(define (valid-token-value v)
  (and (number? v) (exact-nonnegative-integer? v) v))
(define (valid-cost-value v)
  (and (real? v) (not (negative? v)) v))

;; Stamp attempt + cumulative wave usage from one boundary observation.
;;   usage = usage-datum  → real usage recorded (source 'provider or
;;                          'provider-estimated when the datum is estimated)
;;   usage = #f | 'usage-missing → the attempt is recorded usage-missing
;;           with NO token/cost values (honest accounting: absent ≠ zero).
;; The current attempt must exist (begin-attempt! ran). Mutates rec in
;; place; the CALLER persists. Idempotence: stamping twice would double —
;; the orchestrator stamps exactly once per attempt boundary.
(define (stamp-wave-usage! rec wave-idx usage)
  (for ([w (campaign-record-waves rec)]
        #:when (= (campaign-wave-index w) wave-idx))
    (define attempt (campaign-wave-current-attempt w))
    (cond
      [(not attempt) (void)]
      [(usage-datum? usage)
       (define src (if (usage-datum-estimated? usage) 'provider-estimated 'provider))
       (set-campaign-attempt-input-tokens! attempt
                                           (valid-token-value (usage-datum-input-tokens usage)))
       (set-campaign-attempt-output-tokens! attempt
                                            (valid-token-value (usage-datum-output-tokens usage)))
       (set-campaign-attempt-total-tokens! attempt
                                           (valid-token-value (usage-datum-total-tokens usage)))
       (set-campaign-attempt-cost-usd! attempt (valid-cost-value (usage-datum-cost-usd usage)))
       (set-campaign-attempt-usage-source! attempt src)
       ;; cumulative wave totals: #f (unset) + n = n; #f (unknown) + n = #f
       (define (accumulate old new)
         (cond
           [(not (number? old)) new]
           [(not (number? new)) old]
           [else (+ old new)]))
       (define (accumulate-cost old new)
         (cond
           [(not (real? old)) (valid-cost-value new)]
           [(valid-cost-value new) (+ old new)]
           [else old]))
       (set-campaign-wave-usage-input-tokens!
        w
        (accumulate (wave-usage-input-tokens w) (valid-token-value (usage-datum-input-tokens usage))))
       (set-campaign-wave-usage-output-tokens!
        w
        (accumulate (wave-usage-output-tokens w)
                    (valid-token-value (usage-datum-output-tokens usage))))
       (set-campaign-wave-usage-total-tokens!
        w
        (accumulate (wave-usage-total-tokens w) (valid-token-value (usage-datum-total-tokens usage))))
       (set-campaign-wave-usage-cost-usd!
        w
        (accumulate-cost (wave-usage-cost-usd w) (valid-cost-value (usage-datum-cost-usd usage))))
       (set-campaign-wave-usage-source! w
                                        (let ([prior (wave-usage-source w)])
                                          (cond
                                            [(or (eq? prior 'none) (not prior)) src]
                                            [(eq? prior src) src]
                                            [else 'mixed])))
       (set-campaign-wave-usage-missing-attempts! w (wave-usage-missing-attempts w))]
      [else
       ;; usage absent → distinct usage-missing marker, never zeros
       (set-campaign-attempt-usage-source! attempt 'usage-missing)
       (set-campaign-wave-usage-missing-attempts! w (add1 (wave-usage-missing-attempts w)))
       (when (eq? (wave-usage-source w) 'none)
         (set-campaign-wave-usage-source! w 'usage-missing))]))
  (set-campaign-record-updated-at! rec (current-seconds)))

;; Per-wave spend summary.
(define (wave-usage-summary w)
  (define in (wave-usage-input-tokens w))
  (define out (wave-usage-output-tokens w))
  (define tot (wave-usage-total-tokens w))
  (usage-summary in
                 out
                 tot
                 (wave-usage-cost-usd w)
                 (if (or in out tot) 1 0)
                 (wave-usage-missing-attempts w)))

;; Whole-campaign spend summary (sums over waves).
(define (campaign-usage-summary rec)
  (for/fold ([in 0]
             [out 0]
             [tot 0]
             [cost-known 0.0]
             [cost-fully-known? #t]
             [with-usage 0]
             [missing 0])
            ([w (campaign-record-waves rec)])
    (define s (wave-usage-summary w))
    (values (+ in (or (usage-summary-input-tokens s) 0))
            (+ out (or (usage-summary-output-tokens s) 0))
            (+ tot (or (usage-summary-total-tokens s) 0))
            (+ cost-known (or (usage-summary-cost-usd s) 0.0))
            (and cost-fully-known? (real? (usage-summary-cost-usd s)))
            (+ with-usage (usage-summary-attempts-with-usage s))
            (+ missing (usage-summary-missing-attempts s)))))

;; Operator-facing pause message: names the ceiling and how to raise it.
(define (budget-pause-message kind ceiling observed)
  (case kind
    [(max-cost)
     (format
      "campaign paused: budget ceiling gsd.campaign.max-cost = $~a crossed (spent $~a so far). Raise gsd.campaign.max-cost in your project/user settings and resume with /go to continue — nothing is dropped."
      (~r ceiling #:precision '(= 2))
      (~r observed #:precision '(= 2)))]
    [else
     (format
      "campaign paused: token ceiling gsd.campaign.max-tokens = ~a crossed (used ~a tokens so far). Raise gsd.campaign.max-tokens in your project/user settings and resume with /go to continue — nothing is dropped."
      ceiling
      observed)]))

;; Pure ceiling check against the durable cumulative totals.
;; Returns #f (within budget) or a campaign-budget-pause.
;;   max-cost   : positive real or #f (disabled / unknown cost never trips)
;;   max-tokens : positive integer or #f (disabled)
(define (budget-pause-violation? rec max-cost max-tokens)
  (define-values (in out tot cost-known cost-fully-known? with-usage missing)
    (campaign-usage-summary rec))
  (cond
    [(and (real? max-cost)
          (positive? max-cost)
          cost-fully-known?
          (real? cost-known)
          (> cost-known max-cost))
     (campaign-budget-pause 'max-cost
                            max-cost
                            cost-known
                            (budget-pause-message 'max-cost max-cost cost-known)
                            (current-seconds))]
    [(and (exact-positive-integer? max-tokens) (> tot max-tokens))
     (campaign-budget-pause 'max-tokens
                            max-tokens
                            tot
                            (budget-pause-message 'max-tokens max-tokens tot)
                            (current-seconds))]
    [else #f]))

;; Persist a budget pause on the record (durable — survives restarts).
(define (pause-campaign-for-budget! rec pause)
  (unless (campaign-budget-pause? pause)
    (raise-argument-error 'pause-campaign-for-budget! "campaign-budget-pause?" pause))
  (set-campaign-record-budget-pause! rec pause)
  (set-campaign-record-updated-at! rec (current-seconds)))

;; Is a previously-durable pause still in force under the CURRENT ceilings?
;; Raising (or removing) the ceiling clears it; resume then continues.
(define (budget-pause-still-violated? pause max-cost max-tokens)
  (and
   (campaign-budget-pause? pause)
   (case (campaign-budget-pause-kind pause)
     [(max-cost)
      (and (real? max-cost) (positive? max-cost) (> (campaign-budget-pause-observed pause) max-cost))]
     [else
      (and (exact-positive-integer? max-tokens)
           (> (campaign-budget-pause-observed pause) max-tokens))])))

(define (clear-budget-pause! rec)
  (set-campaign-record-budget-pause! rec #f)
  (set-campaign-record-updated-at! rec (current-seconds)))

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
