#lang racket/base

;; @covers extensions/gsd/delivery-outcome.rkt
;; @speed fast
;; @suite unit-fast
;; @boundary unit

;; W3 pure-production pilot (#9951): ONE table-driven owner for the pure
;; typed-stop classification `delivery-stop-outcome`. Production calls it
;; from run-delivery-coordinator! for every typed controller stop — the two
;; verbatim inline `case` blocks (first ladder action after a cleared
;; preflight, and every ordinary ladder action) plus the refused-preflight
;; mapping were removed by this extraction. The authenticated 'ok journal
;; advance (fence recheck + durable eligibility blocker + journal write)
;; stays at the effect boundary and is NOT duplicated here; it remains
;; covered by test-gsd-delivery-coordinator.rkt. Pure inputs only: a kind
;; symbol and an immutable payload hash — no filesystem, network, process,
;; clock or parameter access in the decision.
(require rackunit
         (only-in racket/string string-prefix?)
         (only-in "../extensions/gsd/delivery-outcome.rkt"
                  delivery-outcome
                  delivery-outcome-data
                  delivery-outcome-kind
                  delivery-outcome-message
                  delivery-stop-outcome))

;; The typed-stop vocabulary: every non-'ok delivery-effect-result kind maps
;; to exactly one outcome shape. (kind payload expected-outcome) rows; the
;; expected value is built with the same delivery-outcome constructor the
;; production accessors read, so the table pins the exact (kind message) or
;; (kind message data) shape — no message parsing anywhere.
(define rows
  ;; 'ok never classifies: the caller owns the authenticated advance.
  (list (list 'ok (hasheq 'stage "implementation-review") #f)
        ;; awaiting-review: operator text travels in the payload 'reason.
        (list 'awaiting-review
              (hasheq 'reason "no genuine independent reviewer")
              (delivery-outcome 'awaiting-review "no genuine independent reviewer"))
        ;; review-rejected: an explicit head-bound REQUEST_CHANGES keeps its
        ;; typed payload (findings etc.) alongside the exact receipt message.
        (list 'review-rejected
              (hasheq 'attempt-id "attempt-1" 'findings (list "gate X regressed"))
              (delivery-outcome
               'review-rejected
               "review-rejected: {\"attempt-id\":\"attempt-1\",\"findings\":[\"gate X regressed\"]}"
               (hasheq 'attempt-id "attempt-1" 'findings (list "gate X regressed"))))
        ;; review-infra: transport/timeout/stale/malformed reviewer failure.
        (list 'review-infra
              (hasheq 'reason "reviewer session timed out")
              (delivery-outcome 'review-infra "reviewer session timed out"))
        ;; retryable: typed retryable delivery failure (e.g. CI pending).
        (list 'retryable
              (hasheq 'reason "CI pending on the verified head")
              (delivery-outcome 'retryable "CI pending on the verified head"))
        ;; blocked: the controller refused the effect.
        (list 'blocked
              (hasheq 'reason "preflight refused by the handoff seam")
              (delivery-outcome 'blocked "preflight refused by the handoff seam"))))

(test-case "every typed stop classifies to its exact outcome; 'ok is never classified"
  (for ([row (in-list rows)])
    (define kind (car row))
    (define data (cadr row))
    (define expected (caddr row))
    (check-equal? (delivery-stop-outcome kind data) expected (format "kind: ~a" kind))))

(test-case "default fallback reasons are supplied when the payload carries none"
  (check-equal? (delivery-outcome-message (delivery-stop-outcome 'awaiting-review (hasheq)))
                "awaiting genuine independent review")
  (check-equal? (delivery-outcome-message (delivery-stop-outcome 'review-infra (hasheq)))
                "review infrastructure failure")
  (check-equal? (delivery-outcome-message (delivery-stop-outcome 'retryable (hasheq)))
                "retryable delivery failure")
  (check-equal? (delivery-outcome-message (delivery-stop-outcome 'blocked (hasheq)))
                "controller refused the effect"))

(test-case "review-rejected carries the payload object, never a re-parse of the message"
  (define payload (hasheq 'head (make-string 40 #\a) 'findings (list "f1" "f2")))
  (define outcome (delivery-stop-outcome 'review-rejected payload))
  (check-eq? (delivery-outcome-data outcome) payload)
  (check-equal? (delivery-outcome-kind outcome) 'review-rejected)
  (check-true (string-prefix? (delivery-outcome-message outcome) "review-rejected: ")))

(test-case "preflight path parameterization replaces every default reason"
  (define stopped (list 'awaiting-review 'review-infra 'retryable))
  (for ([kind (in-list stopped)])
    (check-equal?
     (delivery-outcome-message
      (delivery-stop-outcome kind (hasheq) #:reason-fallback "delivery preflight refused"))
     "delivery preflight refused"))
  (check-equal? (delivery-outcome-message (delivery-stop-outcome 'awaiting-review
                                                                 (hasheq 'reason "explicit reason")
                                                                 #:reason-fallback
                                                                 "delivery preflight refused"))
                "explicit reason"))

(test-case "blocked-message carries the pre-composed preflight text verbatim"
  (check-equal? (delivery-outcome-message
                 (delivery-stop-outcome 'blocked
                                        (hasheq 'reason "branch not published")
                                        #:reason-fallback "delivery preflight refused"
                                        #:blocked-message "delivery preflight: branch not published"))
                "delivery preflight: branch not published")
  (check-equal?
   (delivery-outcome-message (delivery-stop-outcome 'blocked
                                                    (hasheq)
                                                    #:reason-fallback "delivery preflight refused"
                                                    #:blocked-message
                                                    "delivery preflight: delivery preflight refused"))
   "delivery preflight: delivery preflight refused"))

(test-case "the classification is a pure function of its immutable inputs"
  (define data (hasheq 'reason "CI pending"))
  (check-equal? (delivery-stop-outcome 'retryable data) (delivery-stop-outcome 'retryable data))
  (check-equal? (delivery-stop-outcome 'retryable data) (delivery-outcome 'retryable "CI pending")))
