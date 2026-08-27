#lang racket/base

;; campaign-budgets.rkt — GSD campaign cost/token accounting + ceilings
;; (BUG-0042, v1.00.22 W7; originally v1.00.22 W5 / BUG-0039).
;;
;; Extracted VERBATIM from go-orchestrator.rkt (behavior-preserving
;; decomposition). The orchestrator keeps the loop; this module owns:
;;   - the in-process usage observation parameter (runner lambda side)
;;   - raw loop-result -> usage datum extraction (honest accounting:
;;     absent metadata is 'usage-missing, never fake zeros)
;;   - durable per-attempt usage stamping at attempt boundaries
;;   - ceiling resolution (gsd.campaign.max-cost / max-tokens) and the
;;     durable budget pause/clear lifecycle
;; go-orchestrator re-provides these names for compatibility with
;; existing importers; new code should import this module directly.

(require racket/base
         (only-in "../../util/loop-result.rkt" loop-result? loop-result-metadata)
         "campaign-state.rkt"
         (only-in "campaign-repository.rkt" load-campaign-record persist-campaign!)
         (only-in "notify.rkt" notify-terminal-transition*!)
         (only-in "../../runtime/settings.rkt" load-settings)
         (only-in "../../runtime/settings-query.rkt" gsd-campaign-max-cost gsd-campaign-max-tokens))

(provide current-campaign-usage-observation
         loop-result->usage-datum
         record-usage-observation!
         take-usage-observation!
         stamp-observed-usage!
         resolve-campaign-budget
         pause-campaign-if-over-budget!
         resume-after-budget-pause!
         load-project-settings-silently)

;; ============================================================
;; v1.00.22 W5 (BUG-0039): campaign cost/token accounting + ceilings.
;;
;; Provider usage metadata rides loop-result's 'usage field but is
;; STRIPPED at the wave-runner-port boundary (wave-execution-outcome is
;; exactly (kind message)). The in-process default-runner path observes
;; the raw loop-result inside the runner lambda — parameterized over a
;; box in execute-campaign-request!'s extent — and the parent retry-loop
;; stamps it onto the durable record at ATTEMPT boundaries (never
;; mid-tool-call). Honest accounting: absent metadata is recorded
;; distinctly as 'usage-missing, never fake zeros (campaign-state's
;; stamp-wave-usage!). Ceilings gsd.campaign.max-cost /
;; gsd.campaign.max-tokens cross → durable pause with a named reason;
;; raising the ceiling + /go resume clears it and continues cleanly.
;; ============================================================

(define current-campaign-usage-observation
  (make-parameter #f)) ; box of (cons wave-index usage-datum|#f) | #f

;; Extract an honest usage datum from a raw loop-result, or #f when the
;; provider reported nothing. Tolerant of any metadata shape — junk is
;; treated as absent (usage-missing), never coerced to zeros.
(define (loop-result->usage-datum result)
  (and (loop-result? result)
       (let ([u (hash-ref (loop-result-metadata result) 'usage #f)])
         (and (hash? u)
              (let ()
                (define (num key)
                  (define v (hash-ref u key #f))
                  (cond
                    [(and (real? v) (not (negative? v))) v]
                    [(string? v)
                     (define n (string->number v))
                     (and (real? n) (not (negative? n)) n)]
                    [else #f]))
                (define in (num 'prompt_tokens))
                (define out (num 'completion_tokens))
                (define tot (or (num 'total_tokens) (and (or in out) (+ (or in 0) (or out 0)))))
                (define cost (num 'cost))
                (and (or in out tot cost)
                     (usage-datum in out tot cost (and (hash-ref u 'estimated? #f) #t))))))))

;; Runner-lambda side: observe BEFORE outcome conversion strips metadata.
(define (record-usage-observation! wave-index run-result)
  (define b (current-campaign-usage-observation))
  (when (box? b)
    (set-box! b (cons wave-index (loop-result->usage-datum run-result)))))

;; Parent side: drain + reset after an attempt (next attempt starts clean).
(define (take-usage-observation!)
  (define b (current-campaign-usage-observation))
  (and (box? b)
       (let ([v (unbox b)])
         (set-box! b #f)
         v)))

;; Durable per-attempt stamp: load fresh → stamp (datum, or usage-missing
;; when nothing was observed) → persist. Best-effort at attempt
;; boundaries; a stamp failure only logs (campaign liveness over
;; telemetry purity). Idempotent across restarts: stamping a second
;; observation REPLACES the attempt's numbers and re-accumulates the
;; wave totals from the per-attempt fields — no double counting.
(define (stamp-observed-usage! base-dir plan-id wave-index observation)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning "campaign usage stamp failed: ~a" (exn-message e)))])
    (define rec (load-campaign-record base-dir plan-id))
    (when rec
      (stamp-wave-usage! rec wave-index (and observation (cdr observation)))
      (persist-campaign! base-dir rec))))

;; Resolve the campaign ceilings from project/user settings.
;; (cons max-cost max-tokens) — each side #f when unset/invalid.
(define (resolve-campaign-budget base-dir)
  (define s (load-project-settings-silently base-dir))
  (define (pos-real v)
    (and (real? v) (positive? v) v))
  (define (pos-int v)
    (and (real? v) (>= (floor v) 1) (inexact->exact (floor v))))
  (cons (pos-real (gsd-campaign-max-cost s)) (pos-int (gsd-campaign-max-tokens s))))

;; Durable ceiling check at an attempt boundary. Returns the pause
;; message string when the campaign is now paused, #f otherwise
;; (within budget, ceilings unset, or the pause could not persist —
;; checked again at the next boundary in the latter case).
(define (pause-campaign-if-over-budget! base-dir plan-id)
  (define budget (resolve-campaign-budget base-dir))
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning "campaign budget check failed: ~a" (exn-message e))
                               #f)])
    (define rec (load-campaign-record base-dir plan-id))
    (cond
      [(not rec) #f]
      ;; Already paused (e.g. resumed with the ceiling untouched):
      ;; surface the existing named reason.
      [(campaign-record-budget-pause rec)
       =>
       campaign-budget-pause-message]
      [else
       (define pause (budget-pause-violation? rec (car budget) (cdr budget)))
       (and pause
            (begin
              (pause-campaign-for-budget! rec pause)
              (persist-campaign! base-dir rec)
              (log-info "campaign ~a paused by budget ceiling (~a)"
                        plan-id
                        (campaign-budget-pause-kind pause))
              (notify-terminal-transition*!
               plan-id
               #f
               'budget-pause
               #:reason (campaign-budget-pause-message pause)
               #:spend (let ([observed (campaign-budget-pause-observed pause)])
                         (and (pair? observed) (number? (car observed)) (car observed))))
              (campaign-budget-pause-message pause)))])))

;; Durable resume gate for run-campaign!'s loop: a paused campaign stays
;; paused while the CURRENT ceiling is still crossed; a raised (or
;; removed) ceiling clears the pause and returns the cleared record so
;; the loop continues cleanly (nothing dropped, nothing re-counted).
;; Returns (values proceed? refreshed-record-or-#f reason-or-#f).
(define (resume-after-budget-pause! base-dir plan-id rec)
  (define pause (and rec (campaign-record-budget-pause rec)))
  (cond
    [(not pause) (values #t #f #f)]
    [(budget-pause-still-violated? pause
                                   (car (resolve-campaign-budget base-dir))
                                   (cdr (resolve-campaign-budget base-dir)))
     (values #f #f (campaign-budget-pause-message pause))]
    [else
     (with-handlers ([exn:fail? (lambda (e)
                                  (log-warning "budget-pause clear failed: ~a" (exn-message e))
                                  (values #f #f (campaign-budget-pause-message pause)))])
       (define fresh (load-campaign-record base-dir plan-id))
       (cond
         [(not fresh) (values #f #f "campaign record disappeared")]
         [(not (campaign-record-budget-pause fresh)) (values #t fresh #f)]
         [else
          (clear-budget-pause! fresh)
          (persist-campaign! base-dir fresh)
          (log-info "campaign ~a budget pause cleared (ceiling raised); resuming" plan-id)
          (values #t fresh #f)]))]))

;; BUG-0028 S1 (v1.00.19 W2): best-effort project-settings load for the
;; gsd.worktree-isolation wiring at the composition root. NEVER raises —
;; settings unavailable means the key is absent, which resolves to the
;; current-gsd-worktree-isolation default (OFF). (Moved verbatim from
;; go-orchestrator.rkt in v1.00.22 W7.)
(define (load-project-settings-silently base-dir)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (load-settings base-dir)))
