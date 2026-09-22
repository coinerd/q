#lang racket/base

;; extensions/gsd/wave-completion.rkt — Verifier-First Completion and Lifecycle Truth
;;
;; v0.99.80 W1: GC-3, GC-4, GC-5, GC-6, GC-11, GC-12
;;
;; Verifier-first: no production path may persist DONE before evidence is
;; verified. The completion service validates campaign/wave/attempt/fence
;; and runs verification before committing DONE to the durable record.
;;
;; Durable outbox: completion events have stable IDs and are deduplicated.
;; Crashes before publication publish later; crashes after publication can
;; redeliver but dedupe suppresses duplicate effects (D6).

(require racket/file
         racket/list
         racket/path
         racket/format
         racket/match
         racket/port
         racket/string
         "campaign-state.rkt"
         "campaign-repository.rkt"
         ;; GSD tracking files — update PLAN.md + wave docs on completion
         (only-in "wave-docs.rkt" wave-slug)
         (only-in "wave-status.rkt" STATUS-DONE STATUS-FAILED)
         (only-in "delivery-handoff.rkt" delivery-handoff-status persist-delivery-handoff!)
         "projection-effects.rkt")

;; ============================================================
;; Completion result
;; ============================================================

(struct completion-result (status event-id) #:transparent)

;; ============================================================
;; Stable event ID (D6)
;; ============================================================

(define (make-event-id plan-id wave-idx attempt-id)
  (format "campaign/~a/wave/~a/attempt/~a/completed" plan-id wave-idx attempt-id))

;; ============================================================
;; Verifier-first wave completion (GC-4)
;; ============================================================

;; Update STATE.md table row for a wave (e.g. "| W1 | ... | PENDING |" → "| W1 | ... | DONE |")
(define (update-state-table! base-dir wave-idx new-status)
  (define state-path (build-path base-dir ".planning" "STATE.md"))
  (when (file-exists? state-path)
    (define content (call-with-input-file state-path port->string))
    (define lines (string-split content "\n"))
    (define prefix (format "| W~a |" wave-idx))
    (define new-lines
      (for/list ([line lines])
        (if (string-prefix? line prefix)
            ;; Format: | W0 | Title | Status |
            ;; Split with #:trim? #f keeps empty edge strings:
            ;;   ["" " W0 " " Title " " Status " ""]
            ;; Replace parts[3] (Status field)
            (let* ([parts (string-split line "|" #:trim? #f)])
              (if (>= (length parts) 5)
                  (string-join (list (list-ref parts 0)
                                     (list-ref parts 1)
                                     (list-ref parts 2)
                                     (string-append " " new-status " ")
                                     (list-ref parts 4))
                               "|")
                  line))
            line)))
    (call-with-output-file state-path
                           (lambda (out) (display (string-join new-lines "\n") out))
                           #:exists 'truncate)))

;; Try to complete a wave. The verifier must approve before DONE is persisted.
;; On rejection, the wave is marked 'failed — DONE is never written.
;; On approval, DONE is persisted + outbox event appended atomically.
;;
;; BUG-0051 (W6): a release wave additionally requires an external GitHub
;; Release-object check. `release-check` (when provided) is a thunk returning
;; #f when the release is verified or a failure-reason string when not. When it
;; returns a reason, completion FAILS with "release not verified: …" — a
;; release wave can never be marked DONE without a verified Release object
;; (closing the v1.00.21 false-completion class).
;; Register F9 verdict: does the requested delivery proof authorize DONE?
;;   'carry-forward      — authorize; the typed pending handoff witness is
;;                         persisted atomically with the durable DONE
;;   'require-delivered  — authorize only when the journal says 'delivered
(define (delivery-proof-verdict base-dir rec wave-idx mode)
  (case mode
    [(carry-forward) 'ok]
    [(require-delivered)
     (define pid (campaign-plan-id rec))
     (if (and (regexp-match? #px"^[0-9a-f]{64}$" pid)
              (eq? (delivery-handoff-status base-dir pid wave-idx) 'delivered))
         'ok
         'refused)]
    [else (raise-argument-error 'try-complete-wave! "(or 'carry-forward 'require-delivered)" mode)]))

(define (try-complete-wave! base-dir
                            rec
                            wave-idx
                            #:verifier-approve? approve?
                            #:verifier-message [verifier-message ""]
                            #:expected-attempt-id expected-attempt-id
                            #:expected-fence-token expected-fence-token
                            #:release-check [release-check #f]
                            #:delivery-proof [delivery-proof 'carry-forward])
  ;; Resolve the release gate ONCE (before any mutation): a string means the
  ;; release is not verified (that string is the failure reason); #f/void means
  ;; either no release check configured or the release verified cleanly.
  (define release-reason
    (and release-check
         (let ([r (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
                    (release-check))])
           (and (string? r) (positive? (string-length (string-trim r))) r))))
  (define release-gate-ok? (not release-reason))
  ;; Completion is a durable compare-and-set boundary. Never trust only the
  ;; caller's in-memory record: reload the authoritative projection and require
  ;; the exact VERIFYING attempt/fence that the verifier observed.
  (define durable (load-campaign-record base-dir (campaign-plan-id rec)))
  (define wave
    (and durable
         (for/first ([w (campaign-record-waves durable)]
                     #:when (= (campaign-wave-index w) wave-idx))
           w)))
  (define attempt (and wave (campaign-wave-current-attempt wave)))
  (define caller-wave
    (for/first ([w (campaign-record-waves rec)]
                #:when (= (campaign-wave-index w) wave-idx))
      w))
  (define attempt-current?
    (and attempt
         (= (campaign-fence-token durable) expected-fence-token)
         (= (campaign-attempt-fence-token attempt) expected-fence-token)
         (equal? (campaign-attempt-id attempt) expected-attempt-id)))
  (cond
    [(not wave) (completion-result 'invalid-wave #f)]
    [(eq? (campaign-wave-status wave) 'done) (completion-result 'already-done #f)]
    [(eq? (campaign-wave-status wave) 'deferred) (completion-result 'already-done #f)]
    [(not (eq? (campaign-wave-status wave) 'verifying)) (completion-result 'invalid-state #f)]
    [(not attempt-current?) (completion-result 'stale-attempt #f)]
    ;; Register F9 (v1.00.31 W3): the wave-completion path refuses to mark a
    ;; wave 'done while its delivery journal for that wave is not 'delivered,
    ;; unless a typed carry-forward handoff is recorded with the DONE. The
    ;; v1.00.30 W4 incident state ([DONE] + Status: DONE with delivery-pending
    ;; and NO journal witness) is unreachable from here on.
    [(and approve?
          release-gate-ok?
          (eq? (delivery-proof-verdict base-dir durable wave-idx delivery-proof) 'refused))
     (completion-result 'delivery-pending-cannot-complete #f)]
    [(not approve?)
     ;; v1.00.24 W3 (verification-truth): durable failure reason FIRST —
     ;; the retry prompt reads wave-failure-reason / attempt-failure-reason
     ;; from the campaign record, so the reason is stamped BEFORE the
     ;; FAILED persist (and therefore before any projection/notification).
     ;; Blank verifier verdicts get an honest named fallback instead of an
     ;; actionable-looking blank.
     (define rejection-reason
       (if (and (string? verifier-message) (positive? (string-length (string-trim verifier-message))))
           verifier-message
           "verifier rejected: no verifier message recorded"))
     (stamp-wave-failure! wave rejection-reason)
     (set-campaign-wave-status! wave 'failed)
     (persist-campaign! base-dir durable)
     (when caller-wave
       (stamp-wave-failure! caller-wave rejection-reason)
       (set-campaign-wave-status! caller-wave 'failed))
     ;; Update GSD tracking files (PLAN.md + wave doc + STATE.md) through the
     ;; atomic projection shell — a crash cannot leave partial tracking.
     (apply-wave-status-projections! base-dir
                                     wave-idx
                                     STATUS-FAILED
                                     (lambda (idx) (wave-slug base-dir idx)))
     ;; Retry-with-adaptation: persist the verifier's failure reason into the
     ;; wave doc so the follow-up wave run sees why the previous attempt
     ;; failed and can adapt instead of repeating the same mistake.
     (record-wave-failure! base-dir wave-idx (lambda (idx) (wave-slug base-dir idx)) rejection-reason)
     (completion-result 'failed #f)]
    ;; BUG-0051: a release wave whose GitHub Release object is missing/draft
    ;; fails completion with a named reason — the verifier approved the code
    ;; delivery, but the release was never published.
    [(not release-gate-ok?)
     (define release-failure-message (format "release not verified: ~a" release-reason))
     (stamp-wave-failure! wave release-failure-message)
     (set-campaign-wave-status! wave 'failed)
     (persist-campaign! base-dir durable)
     (when caller-wave
       (stamp-wave-failure! caller-wave release-failure-message)
       (set-campaign-wave-status! caller-wave 'failed))
     (apply-wave-status-projections! base-dir
                                     wave-idx
                                     STATUS-FAILED
                                     (lambda (idx) (wave-slug base-dir idx)))
     (record-wave-failure! base-dir
                           wave-idx
                           (lambda (idx) (wave-slug base-dir idx))
                           release-failure-message)
     (completion-result 'failed #f)]
    [else
     ;; v1.00.24 W3 (verification-truth): success clears the durable failure
     ;; reason — a completed wave carries none (same lifecycle rule as the
     ;; BUG-0024 attempt-context hand-off clear).
     (clear-wave-failure! wave)
     (set-campaign-wave-status! wave 'done)
     (define event-id
       (make-event-id (campaign-plan-id durable) wave-idx (campaign-attempt-id attempt)))
     ;; v0.99.90 W2 (#9233): the durable record is the transaction COMMIT
     ;; POINT — persist it FIRST. The completion outbox and the
     ;; PLAN/STATE/wave-doc projections are DERIVED files: they may lag after
     ;; a crash (reconcile-completion-outbox! / reconcile-projections-from-waves!
     ;; rebuild them) but must never lead — a crash between the durable commit
     ;; and the outbox append leaves NO phantom completion event, so a later
     ;; outbox publication can never emit an invented DONE for a wave whose
     ;; durable status is still 'verifying.
     ;; F9: the typed carry-forward handoff witness is persisted BEFORE the
     ;; durable commit so a crash can never leave DONE without its journal.
     (define pid (campaign-plan-id durable))
     (when (and (regexp-match? #px"^[0-9a-f]{64}$" pid)
                (not (eq? (delivery-handoff-status base-dir pid wave-idx) 'delivered)))
       (persist-delivery-handoff! base-dir
                                  pid
                                  wave
                                  "verified; delivery pending (typed carry-forward handoff)"))
     (persist-campaign! base-dir durable)
     (append-completion-event! base-dir durable event-id)
     (when caller-wave
       (clear-wave-failure! caller-wave)
       (set-campaign-wave-status! caller-wave 'done))
     ;; Update GSD tracking files (PLAN.md + wave doc + STATE.md) through the
     ;; atomic projection shell — a crash cannot leave partial tracking.
     (apply-wave-status-projections! base-dir
                                     wave-idx
                                     STATUS-DONE
                                     (lambda (idx) (wave-slug base-dir idx)))
     (completion-result 'done event-id)]))
;; ============================================================

;; Retry-with-adaptation: persist the failure reason into the wave document so
;; a follow-up run of a FAILED wave sees why the previous attempt failed. The
;; retry prompt (build-single-wave-prompt) reads the wave doc, so appending a
;; "## Last Failure" section makes the reason part of the next run's context.
;;
;; Idempotent: replaces any existing "## Last Failure" section rather than
;; stacking repeated failures. When the reason is empty/non-informative, the
;; wave doc is left untouched (the FAILED status projection already ran).
(define (record-wave-failure! base-dir wave-idx slug-of reason)
  (define slug (and slug-of (slug-of wave-idx)))
  (define doc-path
    (and slug (build-path base-dir ".planning" "waves" (format "W~a-~a.md" wave-idx slug))))
  (cond
    [(or (not doc-path) (not (file-exists? doc-path))) (void)]
    [(or (not (string? reason)) (string=? (string-trim reason) "")) (void)]
    [else
     (define text (call-with-input-file doc-path port->string))
     ;; Replace an existing "## Last Failure" section (everything from its
     ;; heading to the next top-level heading or end-of-file); otherwise
     ;; append a new section at the end of the document. Line-based to avoid
     ;; Racket regexp flag-group quirks with (?ms) and \z.
     (define section (string-append "## Last Failure\n" (string-trim reason) "\n"))
     (define lines (string-split text "\n"))
     (define heading-idx
       (for/first ([i (in-naturals)]
                   [l (in-list lines)]
                   #:when (string=? (string-trim l) "## Last Failure"))
         i))
     (define new-text
       (if (not heading-idx)
           (string-append (string-trim text #:right? #t) "\n\n" section)
           ;; drop the old section: keep lines before the heading, then find
           ;; the next top-level heading and keep from there onward.
           (let-values ([(before _drop after) (split-lines-at-section lines heading-idx)])
             (string-join (append before (list "" section) after) "\n"))))
     (atomic-write-file! doc-path new-text)]))

;; Split lines at a "## Last Failure" section: returns (values before
;; heading body after-next-heading). heading body is discarded.
(define (split-lines-at-section lines heading-idx)
  (define n (length lines))
  (define next-heading
    (for/first ([i (in-range (add1 heading-idx) n)]
                #:when (and (>= (string-length (string-trim (list-ref lines i))) 2)
                            (string-prefix? (string-trim (list-ref lines i)) "##")))
      i))
  (values (take lines heading-idx)
          '()
          (if next-heading
              (drop lines next-heading)
              '())))
;; ============================================================

(define (skip-wave! base-dir rec wave-idx)
  (define waves (campaign-record-waves rec))
  (define wave
    (for/first ([w waves]
                #:when (= (campaign-wave-index w) wave-idx))
      w))
  (cond
    [(not wave) (completion-result 'invalid-wave #f)]
    [(memq (campaign-wave-status wave) '(done deferred)) (completion-result 'already-done #f)]
    [else
     (set-campaign-wave-status! wave 'deferred)
     (persist-campaign! base-dir rec)
     (completion-result 'deferred #f)]))

;; ============================================================
;; Durable completion outbox (D6, GC-12)
;; ============================================================

(define (outbox-path base-dir plan-id)
  (build-path base-dir ".planning" "campaigns" (string-append plan-id ".outbox.rktd")))

(define (load-outbox base-dir plan-id)
  (define p (outbox-path base-dir plan-id))
  (if (file-exists? p)
      (call-with-input-file p read)
      '()))

;; Append event ID to outbox with deduplication.
;; Uses atomic write (write-to-tmp + rename) for crash safety.
(define (append-completion-event! base-dir rec event-id)
  (define plan-id (campaign-plan-id rec))
  (define p (outbox-path base-dir plan-id))
  (define existing (load-outbox base-dir plan-id))
  ;; Dedup: skip if event-id already present
  (unless (member event-id existing)
    (define updated (append existing (list event-id)))
    (define dir (path-only p))
    (make-directory* dir)
    (define tmp (path-replace-extension p ".tmp"))
    (call-with-output-file tmp #:exists 'truncate (lambda (out) (write updated out)))
    (rename-file-or-directory tmp p #t)))

(define (count-completion-events base-dir rec)
  (length (load-outbox base-dir (campaign-plan-id rec))))

;; W3 register F10: the outbox is derived and may only LAG the durable
;; commit, never lead. reconcile-completion-outbox! is therefore two-way:
;; events whose wave is no longer 'done are dropped (a rollback must not
;; leave a leading completion event), and events missing for done waves are
;; appended. Returns the number of events appended (idempotent, atomic).
(define (event-id-wave-idx id)
  (define m (regexp-match #px"^campaign/[^/]+/wave/([0-9]+)/" (format "~a" id)))
  (and m (string->number (second m))))

(define (done-wave-idxes rec)
  (for/list ([w (in-list (campaign-record-waves rec))]
             #:when (eq? (campaign-wave-status w) 'done))
    (campaign-wave-index w)))

(define (write-outbox! base-dir plan-id ids)
  (define p (outbox-path base-dir plan-id))
  (cond
    [(null? ids)
     (when (file-exists? p)
       (delete-file p))]
    [else
     (define dir (path-only p))
     (make-directory* dir)
     (define tmp (path-replace-extension p ".tmp"))
     (call-with-output-file tmp #:exists 'truncate (lambda (out) (write ids out)))
     (rename-file-or-directory tmp p #t)]))

(define (reconcile-completion-outbox! base-dir rec)
  (define pid (campaign-plan-id rec))
  (define done-idx (done-wave-idxes rec))
  (define existing (load-outbox base-dir pid))
  (define justified
    (for/list ([id (in-list existing)]
               #:when (member (event-id-wave-idx id) done-idx))
      id))
  (define missing
    (for/list ([w (in-list (campaign-record-waves rec))]
               #:when (eq? (campaign-wave-status w) 'done)
               #:do [(define attempt (campaign-wave-current-attempt w))]
               #:when attempt
               #:do [(define id
                       (make-event-id pid (campaign-wave-index w) (campaign-attempt-id attempt)))]
               #:unless (member id existing))
      id))
  (when (or (pair? missing) (not (equal? justified existing)))
    (write-outbox! base-dir pid (append justified missing)))
  (length missing))

;; The typed invariant: 'ok exactly when every outbox event belongs to a
;; wave that is durably 'done — 'outbox-leads-record otherwise.
(define (completion-outbox-invariant? base-dir rec)
  (define done-idx (done-wave-idxes rec))
  (if (for/and ([id (in-list (load-outbox base-dir (campaign-plan-id rec)))]
                #:when (not (member (event-id-wave-idx id) done-idx)))
        #f)
      'ok
      'outbox-leads-record))

;; ============================================================
;; Path helper
;; ============================================================

(define (path-only p)
  (define-values (base name must-be-dir?) (split-path p))
  (if (path? base)
      base
      (current-directory)))

;; ============================================================
;; Provide
;; ============================================================

(provide try-complete-wave!
         record-wave-failure!
         skip-wave!
         update-state-table!
         completion-result
         completion-result-status
         completion-result-event-id
         load-outbox
         count-completion-events
         make-event-id
         reconcile-completion-outbox!
         completion-outbox-invariant?)
