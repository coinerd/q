#lang racket/base

;; @suite gsd
;; @speed fast
;; @boundary pure
;; tests/test-campaign-cost-tracking.rkt
;; BUG-0039 characterization pin (v1.00.21 W0; FLIPPED by W5).
;;
;; TODAY campaign cost/token consumption is tracked nowhere: attempt
;; transitions stamp no token/cost fields, the persisted campaign
;; record carries none, and gsd.campaign.max-cost is consumed by no
;; code path. Every assertion below PASSES against today's red
;; behavior; W5 flips them once cost tracking lands.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         racket/path
         racket/struct
         racket/runtime-path
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt")

(define-runtime-path settings-query-src "../runtime/settings-query.rkt")
(define-runtime-path go-orchestrator-src "../extensions/gsd/go-orchestrator.rkt")
(define-runtime-path wave-executor-src "../extensions/gsd/wave-executor.rkt")

;; Distinctive field-shaped markers only. The bare word "cost" is
;; deliberately excluded: fixture prose (e.g. a wave titled "Cost
;; characterization") legitimately contains it. What we pin is the
;; absence of cost/token DATA FIELDS, which would appear under names
;; shaped like these if W5's tracking existed today.
(define COST-MARKERS
  '("max-cost" "cost-usd"
               "input-token"
               "output-token"
               "total-token"
               "tokens-used"
               "token-count"
               "usage-tokens"
               "total-usage"
               "usd"))

(define (contains-any? s markers)
  (for/or ([m (in-list markers)])
    (string-contains? (string-downcase s) m)))

(define (make-temp-campaign)
  (define dir (make-temporary-file "w0-cost~a" 'directory))
  (make-directory* (build-path dir ".planning" "waves"))
  (with-output-to-file (build-path dir ".planning" "PLAN.md")
                       (lambda ()
                         (displayln "# Plan: Cost characterization")
                         (newline)
                         (displayln "- [Inbox] W0: Cost characterization → waves/W0-cost.md"))
                       #:exists 'replace)
  (with-output-to-file (build-path dir ".planning" "waves" "W0-cost.md")
                       (lambda ()
                         (displayln "# Wave 0: Cost characterization")
                         (displayln "Status: Inbox")
                         (newline)
                         (displayln "## Goal")
                         (newline)
                         (displayln "Run one synthetic attempt."))
                       #:exists 'replace)
  dir)

;; ── Behavioral probe: synthetic attempt transition ────────────

(define campaign-cost-suite
  (test-suite "BUG-0039 characterization: attempt transitions stamp no cost data (W0 pin; W5 flips)"
    (test-case "attempt transition stamps no token/cost fields"
      (define dir (make-temp-campaign))
      (dynamic-wind
       void
       (lambda ()
         (define rec (load-or-migrate-campaign! dir))
         (begin-attempt! rec 0 42)
         ;; TODAY the in-memory record carries no cost/token data at all
         (define repr (format "~s" rec))
         (check-false (contains-any? repr COST-MARKERS)
                      "no token/cost fields are stamped on attempt transition")
         ;; the attempt struct field set is exactly (id fence-token started-at)
         (define w0 (car (campaign-record-waves rec)))
         (define attempt (campaign-wave-current-attempt w0))
         (check-not-false attempt "an attempt was begun")
         (check-equal? (length (struct->list attempt))
                       3
                       "TODAY the attempt record has exactly 3 fields — none cost/token")
         ;; persisted record is equally cost-free
         (persist-campaign! dir rec)
         (define persisted
           (string-join (for/list ([f (in-list (find-files (lambda (p)
                                                             (regexp-match? #rx"\\.rktd$"
                                                                            (path->string p)))
                                                           dir))])
                          (file->string f))
                        " "))
         (check-false (contains-any? persisted COST-MARKERS)
                      "persisted campaign record contains no token/cost fields"))
       (lambda () (delete-directory/files dir #:must-exist? #f))))

    ;; ── Absent-seam markers ───────────────────────────────────────

    (test-case "gsd.campaign.max-cost is consumed nowhere"
      (check-false (string-contains? (file->string settings-query-src) "max-cost")
                   "no settings accessor for gsd.campaign.max-cost")
      (check-false (string-contains? (file->string settings-query-src) "gsd-campaign")
                   "no gsd.campaign.* accessor family exists")
      (check-false (string-contains? (file->string go-orchestrator-src) "max-cost")
                   "orchestrator never reads gsd.campaign.max-cost")
      (check-false (string-contains? (file->string wave-executor-src) "max-cost")
                   "wave executor never reads gsd.campaign.max-cost"))

    (test-case "no cost/budget/token key under gsd.campaign is read by the orchestrator"
      ;; gsd.campaign.* TODAY carries only the infra-retry telemetry event —
      ;; no cost, budget, or token key exists anywhere in the campaign path.
      (check-false (regexp-match? #rx"gsd\\.campaign\\.[a-z-]*(cost|budget|token)[a-z-]*"
                                  (file->string go-orchestrator-src))))))

(module+ main
  (exit (run-tests campaign-cost-suite)))
