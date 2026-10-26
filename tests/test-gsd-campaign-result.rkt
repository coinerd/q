#lang racket
;; @covers extensions/gsd/campaign-result.rkt
;; @speed fast
;; @suite extensions
;; @boundary unit
;;
;; tests/test-gsd-campaign-result.rkt — W0 of the GSD simplification plan:
;; additive structured campaign diagnostics with NO behavior or authority
;; change. Pins:
;;   1. legacy 3-argument constructor + status/completed-waves/message
;;      accessors keep working unchanged (all existing call sites preserved);
;;   2. legacy results expose a stable default code derived from status;
;;   3. the optional diagnostics envelope (code/summary/detail/next-action/
;;      artifacts + extra structured fields) attaches without mutation, via
;;      the lean helper constructor (campaign-result-with-diagnostics);
;;   4. the single pure /go stop renderer preserves the legacy text
;;      byte-for-byte and lets explicit detail/next-action survive;
;;   5. the coordinator-boundary reconstruction (campaign-result-with-completed)
;;      propagates diagnostics instead of dropping them;
;;   6. diagnostics never participate in control flow: attaching them never
;;      changes status (the routing vocabulary).

(require rackunit
         racket/string
         (only-in "../extensions/gsd/campaign-result.rkt"
                  campaign-result
                  campaign-result?
                  campaign-result-status
                  campaign-result-completed-waves
                  campaign-result-message
                  campaign-result-diagnostics
                  campaign-result-code
                  campaign-diagnostics
                  campaign-result-with-diagnostics
                  campaign-result-with-diagnostic
                  campaign-result-with-completed
                  render-campaign-stop-text))

;; Local convenience: a legacy result carrying an attached envelope.
(define (diagnosed status completed message d)
  (campaign-result-with-diagnostics (campaign-result status completed message) d))

(module+ test
  ;; ── 1. Legacy constructor compatibility ──────────────────────────────────
  (test-case "legacy 3-argument constructor and accessors are preserved"
    (define r (campaign-result 'wave-failed '(2 1) "verifier rejected: bad metadata"))
    (check-true (campaign-result? r))
    (check-eq? (campaign-result-status r) 'wave-failed)
    (check-equal? (campaign-result-completed-waves r) '(2 1))
    (check-equal? (campaign-result-message r) "verifier rejected: bad metadata")
    (check-false (campaign-result-diagnostics r) "legacy results carry no envelope"))

  (test-case "diagnosed subtype preserves legacy match patterns"
    (define r (diagnosed 'wave-blocked '(1) "blocked" (campaign-diagnostics 'lease-busy)))
    (match r
      [(campaign-result status completed message)
       (check-eq? status 'wave-blocked)
       (check-equal? completed '(1))
       (check-equal? message "blocked")])
    (check-false (equal? r (campaign-result 'wave-blocked '(1) "blocked"))
                 "diagnosed values are distinct; compare legacy fields through accessors"))

  (test-case "legacy construction is stable under repeated calls"
    (check-equal? (campaign-result 'busy '() "campaign lease held by another process")
                  (campaign-result 'busy '() "campaign lease held by another process")))

  ;; ── 2. Default stable code derived from status ───────────────────────────
  (test-case "default stable code is derived from the status vocabulary"
    (for ([status '(error busy
                          wave-failed
                          wave-cancelled
                          wave-blocked
                          meta-fix
                          wave-awaiting-delivery
                          campaign-complete)])
      (define r (campaign-result status '() "stop"))
      (check-eq? (campaign-result-code r) status)
      (check-eq? (campaign-result-code r) (campaign-result-code r))))

  (test-case "explicit diagnostic code wins over the derived default"
    (define r
      (diagnosed 'wave-blocked '() "blocked" (campaign-diagnostics 'wave-predecessor-unproven)))
    (check-eq? (campaign-result-status r) 'wave-blocked)
    (check-eq? (campaign-result-code r) 'wave-predecessor-unproven)
    (check-eq? (campaign-result-code (campaign-result 'wave-blocked '() "blocked")) 'wave-blocked))

  ;; ── 3. Diagnostics envelope constructor ──────────────────────────────────
  (test-case "campaign-diagnostics builds the canonical envelope"
    (define d
      (campaign-diagnostics 'budget-pause
                            #:summary "spend ceiling crossed"
                            #:detail "observed 42 of 40"
                            #:next-action "raise the ceiling, then re-run /go"
                            #:artifacts (list ".planning/campaigns/x.rktd")))
    (check-eq? (hash-ref d 'code) 'budget-pause)
    (check-equal? (hash-ref d 'summary) "spend ceiling crossed")
    (check-equal? (hash-ref d 'detail) "observed 42 of 40")
    (check-equal? (hash-ref d 'next-action) "raise the ceiling, then re-run /go")
    (check-equal? (hash-ref d 'artifacts) (list ".planning/campaigns/x.rktd"))
    (check-eq? (hash-ref d 'extra-absent 'absent) 'absent "only documented keys exist"))

  (test-case "extra structured fields are allowed"
    (define d (campaign-diagnostics 'lease-busy #:extra (hasheq 'owner-session "sess-1" 'pid 4242)))
    (check-equal? (hash-ref d 'owner-session) "sess-1")
    (check-equal? (hash-ref d 'pid) 4242)
    (check-false (hash-ref d 'summary #f) "optional fields default to #f"))

  ;; ── 4. Immutability / no mutation ────────────────────────────────────────
  (test-case "attaching a diagnostics envelope never mutates the original"
    (define base (campaign-result 'wave-failed '(2) "runner failed"))
    (define envelope (campaign-diagnostics 'runner-failed #:detail "attempt 3 exhausted"))
    (define attached (campaign-result-with-diagnostics base envelope))
    (check-false (campaign-result-diagnostics base) "original result untouched")
    (check-not-eq? attached base "a NEW value is returned")
    (check-equal? (campaign-result-status attached) 'wave-failed)
    (check-equal? (campaign-result-completed-waves attached) '(2))
    (check-equal? (campaign-result-message attached) "runner failed")
    (check-equal? (campaign-result-diagnostics attached) envelope))

  (test-case "attaching one key merges without mutating the existing envelope"
    (define envelope (campaign-diagnostics 'wave-blocked #:next-action "bind the merge SHA"))
    (define r0
      (campaign-result-with-diagnostics (campaign-result 'wave-blocked '() "blocked") envelope))
    (define r1 (campaign-result-with-diagnostic r0 'code 'wave-predecessor-unproven))
    (check-equal? (campaign-result-diagnostics r0) envelope "original result untouched")
    (check-eq? (hash-ref (campaign-result-diagnostics r1) 'code) 'wave-predecessor-unproven)
    (check-equal? (hash-ref (campaign-result-diagnostics r1) 'next-action) "bind the merge SHA")
    (check-eq? (hash-ref envelope 'code) 'wave-blocked))

  ;; ── 5. Diagnostics are never control flow ────────────────────────────────
  (test-case "attaching diagnostics never changes the routing status"
    (define r (campaign-result 'wave-failed '() "boom"))
    (check-eq? (campaign-result-status (campaign-result-with-diagnostic r 'code 'x)) 'wave-failed)
    (check-eq? (campaign-result-status (diagnosed 'wave-failed '() "boom" (hasheq 'code 'x)))
               'wave-failed))

  ;; ── 6. The single pure /go stop renderer ─────────────────────────────────
  (test-case "renderer preserves the legacy stop text byte-for-byte"
    (check-equal?
     (render-campaign-stop-text (campaign-result 'wave-failed '() "verifier rejected: bad metadata"))
     "[ERROR] /go campaign stopped: wave-failed (verifier rejected: bad metadata)")
    (check-equal? (render-campaign-stop-text (campaign-result 'busy '() ""))
                  "[ERROR] /go campaign stopped: busy")
    (check-equal? (render-campaign-stop-text (campaign-result 'busy '() #f))
                  "[ERROR] /go campaign stopped: busy"))

  (test-case "an explicit distinct code is visible without changing legacy text"
    (define legacy (campaign-result 'wave-cancelled '(3) "campaign cancellation requested"))
    (define decorated
      (diagnosed 'wave-cancelled
                 '(3)
                 "campaign cancellation requested"
                 (campaign-diagnostics 'campaign-cancelled)))
    (check-equal? (render-campaign-stop-text legacy)
                  "[ERROR] /go campaign stopped: wave-cancelled (campaign cancellation requested)")
    (check-true (string-contains? (render-campaign-stop-text decorated) "[campaign-cancelled]")))

  (test-case "renderer keeps explicit detail and next-action in the stop text"
    (define r
      (diagnosed 'wave-blocked
                 '()
                 "wave 2 blocked: predecessor has no merge SHA"
                 (campaign-diagnostics 'wave-predecessor-unproven
                                       #:detail "wave 1 evidence trio is unbound"
                                       #:next-action "bind the squash-merge SHA, then re-run /go")))
    (check-equal? (render-campaign-stop-text r)
                  (string-append
                   "[ERROR] /go campaign stopped: wave-blocked [wave-predecessor-unproven] "
                   "(wave 2 blocked: predecessor has no merge SHA)"
                   "\nDetail: wave 1 evidence trio is unbound"
                   "\nNext action: bind the squash-merge SHA, then re-run /go")))

  (test-case "renderer keeps a lone next-action or a lone detail"
    (define only-action
      (diagnosed 'wave-blocked
                 '()
                 "blocked"
                 (campaign-diagnostics 'wave-blocked #:next-action "inspect the evidence trio")))
    (check-equal?
     (render-campaign-stop-text only-action)
     "[ERROR] /go campaign stopped: wave-blocked (blocked)\nNext action: inspect the evidence trio")
    (define only-detail
      (diagnosed 'wave-blocked
                 '()
                 "blocked"
                 (campaign-diagnostics 'wave-blocked #:detail "predecessor proof missing")))
    (check-equal?
     (render-campaign-stop-text only-detail)
     "[ERROR] /go campaign stopped: wave-blocked (blocked)\nDetail: predecessor proof missing"))

  (test-case "renderer uses summary fallback and shows artifacts"
    (define r
      (diagnosed 'error
                 '()
                 42
                 (campaign-diagnostics 'corrupt-record
                                       #:summary "campaign record is invalid"
                                       #:artifacts '("campaign.rktd"))))
    (check-equal? (render-campaign-stop-text r)
                  (string-append "[ERROR] /go campaign stopped: error [corrupt-record] "
                                 "(campaign record is invalid)\nArtifacts: campaign.rktd")))

  (test-case "renderer is deterministic"
    (define r (diagnosed 'busy '() "held" (campaign-diagnostics 'lease-busy #:next-action "wait")))
    (check-equal? (render-campaign-stop-text r) (render-campaign-stop-text r)))

  ;; ── 7. Propagation through the coordinator reconstruction ────────────────
  (test-case "coordinator reconstruction propagates diagnostics"
    (define wave-result
      (diagnosed 'wave-failed
                 '(9)
                 "runner failed"
                 (campaign-diagnostics 'runner-failed
                                       #:detail "attempt-3 exhausted the stall budget"
                                       #:next-action "inspect the campaign record")))
    (define propagated (campaign-result-with-completed wave-result '(1 2)))
    (check-eq? (campaign-result-status propagated) 'wave-failed)
    (check-equal? (campaign-result-message propagated) "runner failed")
    (check-equal? (campaign-result-completed-waves propagated) '(1 2))
    (check-equal? (campaign-result-diagnostics propagated) (campaign-result-diagnostics wave-result))
    (check-eq? (campaign-result-code propagated) 'runner-failed)
    (check-equal? (campaign-result-completed-waves wave-result) '(9) "original result untouched")
    (check-true (string-contains? (render-campaign-stop-text propagated)
                                  "Next action: inspect the campaign record")))

  (test-case "coordinator reconstruction keeps legacy results legacy"
    (define propagated
      (campaign-result-with-completed
       (campaign-result 'wave-cancelled '(3) "campaign cancellation requested")
       '(1)))
    (check-false (campaign-result-diagnostics propagated))
    (check-eq? (campaign-result-status propagated) 'wave-cancelled)
    (check-equal? (campaign-result-completed-waves propagated) '(1))
    (check-equal? (render-campaign-stop-text propagated)
                  "[ERROR] /go campaign stopped: wave-cancelled (campaign cancellation requested)")
    (define blocked
      (diagnosed 'wave-blocked
                 '()
                 "advance blocked"
                 (campaign-diagnostics 'wave-predecessor-unproven
                                       #:next-action "bind the merge SHA")))
    (define blocked-propagated (campaign-result-with-completed blocked '()))
    (check-eq? (campaign-result-status blocked-propagated) 'wave-blocked)
    (check-eq? (campaign-result-code blocked-propagated) 'wave-predecessor-unproven)
    (check-true (string-contains? (render-campaign-stop-text blocked-propagated)
                                  "Next action: bind the merge SHA"))))
