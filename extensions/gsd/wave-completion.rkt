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
(define (try-complete-wave! base-dir
                            rec
                            wave-idx
                            #:verifier-approve? approve?
                            #:verifier-message [verifier-message ""]
                            #:expected-attempt-id expected-attempt-id
                            #:expected-fence-token expected-fence-token)
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
    [(not approve?)
     (set-campaign-wave-status! wave 'failed)
     (persist-campaign! base-dir durable)
     (when caller-wave
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
     (record-wave-failure! base-dir wave-idx (lambda (idx) (wave-slug base-dir idx)) verifier-message)
     (completion-result 'failed #f)]
    [else
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
     (persist-campaign! base-dir durable)
     (append-completion-event! base-dir durable event-id)
     (when caller-wave
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

;; v0.99.90 W2 (#9233): rebuild missing completion outbox events from the
;; authoritative durable record. Every durable 'done wave must have exactly its
;; stable completion event-id in the outbox; dedup (append-completion-event!
;; skips present ids) makes this idempotent, and non-done waves NEVER get an
;; event — the outbox is derived, it may only lag the durable commit, never
;; lead (no invented DONE). Returns the number of events appended.
(define (reconcile-completion-outbox! base-dir rec)
  (define pid (campaign-plan-id rec))
  (define existing (load-outbox base-dir pid))
  (define missing
    (for/list ([w (campaign-record-waves rec)]
               #:when (eq? (campaign-wave-status w) 'done))
      (define attempt (campaign-wave-current-attempt w))
      (and attempt
           (let ([id (make-event-id pid (campaign-wave-index w) (campaign-attempt-id attempt))])
             (and (not (member id existing)) id)))))
  (for ([id (filter (lambda (x) x) missing)])
    (append-completion-event! base-dir rec id))
  (length (filter (lambda (x) x) missing)))

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
         reconcile-completion-outbox!)
