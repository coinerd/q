#lang racket/base

;; @suite gsd
;; @speed fast
;; @boundary pure
;; tests/test-campaign-cost-tracking.rkt
;; BUG-0039 characterization pin (v1.00.21 W0) — FLIPPED by W5 (v1.00.22).
;;
;; The W0 pin asserted cost tracking existed nowhere. W5 landed it:
;;   - attempt transitions stamp token/cost fields from loop-result
;;     usage metadata (campaign-state stamp-wave-usage!),
;;   - absent metadata is recorded distinctly as 'usage-missing —
;;     never faked zeros,
;;   - gsd.campaign.max-cost / max-tokens ceilings pause the campaign
;;     durably with a named reason and are resumable.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         racket/path
         racket/runtime-path
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt")

(define-runtime-path settings-query-src "../runtime/settings-query.rkt")
(define-runtime-path go-orchestrator-src "../extensions/gsd/go-orchestrator.rkt")

(define (make-temp-campaign)
  (define dir (make-temporary-file "w5-cost~a" 'directory))
  (make-directory* (build-path dir ".planning" "waves"))
  (with-output-to-file (build-path dir ".planning" "PLAN.md")
                       (lambda ()
                         (displayln "# Plan: Cost tracking")
                         (newline)
                         (displayln "- [Inbox] W0: Cost tracking → waves/W0-cost.md"))
                       #:exists 'replace)
  (with-output-to-file (build-path dir ".planning" "waves" "W0-cost.md")
                       (lambda ()
                         (displayln "# Wave 0: Cost tracking")
                         (displayln "Status: Inbox")
                         (newline)
                         (displayln "## Goal")
                         (newline)
                         (displayln "Run one synthetic attempt."))
                       #:exists 'replace)
  dir)

(define (persisted-rktd dir)
  (string-join (for/list ([f (in-list (find-files (lambda (p)
                                                    (regexp-match? #rx"\\.rktd$" (path->string p)))
                                                  dir))])
                 (file->string f))
               " "))

(define campaign-cost-suite
  (test-suite "BUG-0039 flipped: usage metadata flows into the durable record (W5)"

    (test-case "synthetic usage metadata lands in attempt/wave/campaign fields"
      (define dir (make-temp-campaign))
      (dynamic-wind void
                    (lambda ()
                      (define rec (load-or-migrate-campaign! dir))
                      (begin-attempt! rec 0 42)
                      (stamp-wave-usage! rec 0 (usage-datum 100 50 150 0.25 #f))
                      (define w0 (car (campaign-record-waves rec)))
                      (define attempt (campaign-wave-current-attempt w0))
                      (check-not-false attempt "an attempt was begun")
                      (check-eq? (campaign-attempt-usage-source attempt) 'provider)
                      (check-equal? (wave-usage-input-tokens w0) 100)
                      (check-equal? (wave-usage-output-tokens w0) 50)
                      (check-equal? (wave-usage-total-tokens w0) 150)
                      (check-equal? (wave-usage-cost-usd w0) 0.25)
                      (define total (campaign-usage-summary rec))
                      (check-equal? (usage-summary-total-tokens total) 150)
                      (check-equal? (usage-summary-cost-usd total) 0.25)
                      (check-equal? (usage-summary-attempts-with-usage total) 1)
                      (check-equal? (usage-summary-missing-attempts total) 0)
                      ;; persisted record carries the cost/token data durably
                      (persist-campaign! dir rec)
                      (define persisted (persisted-rktd dir))
                      (check-true (string-contains? persisted "provider")
                                  "persisted record contains the usage source stamp")
                      (check-true (string-contains? persisted "0.25")
                                  "persisted record contains the cost datum")
                      ;; reload restores the accounting
                      (define reloaded (load-or-migrate-campaign! dir))
                      (check-equal? (usage-summary-cost-usd (campaign-usage-summary reloaded))
                                    0.25
                                    "usage survives persist/reload"))
                    (lambda () (delete-directory/files dir #:must-exist? #f))))

    (test-case "absent metadata records 'usage-missing — never zeros"
      (define dir (make-temp-campaign))
      (dynamic-wind void
                    (lambda ()
                      (define rec (load-or-migrate-campaign! dir))
                      (begin-attempt! rec 0 7)
                      (stamp-wave-usage! rec 0 'usage-missing)
                      (define w0 (car (campaign-record-waves rec)))
                      (check-eq? (campaign-attempt-usage-source (campaign-wave-current-attempt w0))
                                 'usage-missing)
                      (check-false (wave-usage-total-tokens w0) "missing usage is #f, not a faked 0")
                      (check-false (wave-usage-cost-usd w0) "missing cost is #f, not a faked 0")
                      (check-equal? (wave-usage-missing-attempts w0) 1)
                      (define total (campaign-usage-summary rec))
                      (check-equal? (usage-summary-missing-attempts total) 1)
                      (check-equal? (usage-summary-total-tokens total)
                                    0
                                    "known token sum is 0 — honesty rides missing-attempts")
                      (persist-campaign! dir rec)
                      (check-true (string-contains? (persisted-rktd dir) "usage-missing")
                                  "usage-missing is durable, distinct from zero"))
                    (lambda () (delete-directory/files dir #:must-exist? #f))))

    (test-case "max-cost crossing → durable pause with named reason, resumable"
      (define dir (make-temp-campaign))
      (dynamic-wind
       void
       (lambda ()
         (define rec (load-or-migrate-campaign! dir))
         (begin-attempt! rec 0 42)
         (stamp-wave-usage! rec 0 (usage-datum 1000 1000 2000 5.0 #f))
         (define pause (budget-pause-violation? rec 4.0 #f))
         (check-not-false pause "cost ceiling crossed")
         (check-eq? (campaign-budget-pause-kind pause) 'max-cost)
         (define msg (campaign-budget-pause-message pause))
         (check-true (string-contains? msg "max-cost") "pause reason names the ceiling")
         (check-true (string-contains? (string-downcase msg) "raise")
                     "pause reason says how to raise it")
         ;; within-budget check is clean under both ceilings
         (check-false (budget-pause-violation? rec 50.0 100000) "raised ceilings do not trip")
         ;; token ceiling trips distinctly when cost ceiling is clear
         (define tok-pause (budget-pause-violation? rec 50.0 1500))
         (check-not-false tok-pause "token ceiling crossed")
         (check-eq? (campaign-budget-pause-kind tok-pause) 'max-tokens)
         ;; durable: pause persists, then raising the ceiling resumes cleanly
         (pause-campaign-for-budget! rec pause)
         (persist-campaign! dir rec)
         (define reloaded (load-or-migrate-campaign! dir))
         (check-not-false (campaign-record-budget-pause reloaded) "pause is durable")
         (check-true (budget-pause-still-violated? pause 4.0 #f) "still blocked at the old ceiling")
         (check-false (budget-pause-still-violated? pause 10.0 #f) "raised ceiling unblocks")
         (clear-budget-pause! reloaded)
         (check-false (campaign-record-budget-pause reloaded) "pause cleared — resumable")
         ;; nothing dropped or double-counted by the pause cycle
         (check-equal? (usage-summary-cost-usd (campaign-usage-summary reloaded)) 5.0)
         (check-equal? (usage-summary-attempts-with-usage (campaign-usage-summary reloaded)) 1))
       (lambda () (delete-directory/files dir #:must-exist? #f))))

    ;; ── Flipped absent-seam markers ───────────────────────────────

    (test-case "gsd.campaign.max-cost is wired through settings and the orchestrator"
      (check-true (string-contains? (file->string settings-query-src) "gsd-campaign-max-cost")
                  "settings accessor for gsd.campaign.max-cost exists")
      (check-true (string-contains? (file->string settings-query-src) "gsd-campaign-max-tokens")
                  "settings accessor for gsd.campaign.max-tokens exists")
      (check-true (string-contains? (file->string go-orchestrator-src) "max-cost")
                  "orchestrator reads gsd.campaign.max-cost")
      (check-true (string-contains? (file->string go-orchestrator-src) "usage-missing")
                  "orchestrator stamps absent usage as usage-missing"))))

(module+ main
  (exit (run-tests campaign-cost-suite)))
