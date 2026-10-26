#lang racket/base
;; @covers extensions/gsd/wave-status.rkt

;; @speed fast  ;; @suite extensions
;; @boundary unit

;; BOUNDARY: integration

;; test-gsd-wave-status.rkt — Tests for canonical wave status constants
;; v0.32.5 W0: Test scaffolding

(require rackunit
         "../extensions/gsd/wave-status.rkt")

(test-case "STATUS constants are correct"
  (check-equal? STATUS-INBOX "Inbox")
  (check-equal? STATUS-IN-PROGRESS "In-Progress")
  (check-equal? STATUS-DONE "DONE")
  (check-equal? STATUS-DEFERRED "DEFERRED")
  (check-equal? STATUS-FAILED "FAILED"))

(test-case "ALL-STATUSES contains all 5 statuses"
  (check-equal? (length ALL-STATUSES) 5)
  (for ([s ALL-STATUSES])
    (check-true (wave-status-string? s))))

(test-case "wave-status-string? recognizes all statuses"
  (check-true (wave-status-string? "Inbox"))
  (check-true (wave-status-string? "DONE"))
  (check-false (wave-status-string? "unknown"))
  (check-false (wave-status-string? 42)))

(test-case "terminal-status? identifies DONE and DEFERRED"
  (check-true (terminal-status? STATUS-DONE))
  (check-true (terminal-status? STATUS-DEFERRED))
  (check-false (terminal-status? STATUS-INBOX))
  (check-false (terminal-status? STATUS-IN-PROGRESS))
  (check-false (terminal-status? STATUS-FAILED)))

(test-case "done-or-deferred? is alias for terminal-status?"
  (check-true (done-or-deferred? STATUS-DONE))
  (check-true (done-or-deferred? STATUS-DEFERRED))
  (check-false (done-or-deferred? STATUS-INBOX)))

(test-case "active-status? returns true for non-terminal"
  (check-true (active-status? STATUS-INBOX))
  (check-true (active-status? STATUS-IN-PROGRESS))
  (check-true (active-status? STATUS-FAILED))
  (check-false (active-status? STATUS-DONE))
  (check-false (active-status? STATUS-DEFERRED)))

(test-case "normalize-status! returns canonical form"
  (check-equal? (normalize-status! "DONE") STATUS-DONE)
  (check-equal? (normalize-status! "Done") STATUS-DONE)
  (check-equal? (normalize-status! "done") STATUS-DONE)
  (check-equal? (normalize-status! "DEFERRED") STATUS-DEFERRED)
  (check-equal? (normalize-status! "deferred") STATUS-DEFERRED)
  (check-equal? (normalize-status! "FAILED") STATUS-FAILED)
  (check-equal? (normalize-status! "INBOX") STATUS-INBOX)
  (check-equal? (normalize-status! "IN-PROGRESS") STATUS-IN-PROGRESS)
  (check-false (normalize-status! "unknown")))

;; ============================================================
;; GSD-SIMPLIFICATION W1: canonical + marker vocabulary ownership
;; ============================================================

(test-case "canonical durable vocabulary is exactly the eight campaign statuses"
  (check-equal? CANONICAL-WAVE-STATUSES
                '(pending in-progress verifying awaiting-delivery done failed interrupted deferred))
  (check-equal? canonical-wave-statuses CANONICAL-WAVE-STATUSES)
  (check-equal? (length CANONICAL-WAVE-STATUSES) 8))

(test-case "legacy spellings keep their historical canonical meaning"
  (check-eq? (wave-status-symbol "Inbox") 'pending)
  (check-eq? (wave-status-symbol "PENDING") 'pending)
  (check-eq? (wave-status-symbol "NOT STARTED") 'pending)
  (check-eq? (wave-status-symbol "In-Progress") 'in-progress)
  (check-eq? (wave-status-symbol "IN PROGRESS") 'in-progress)
  (check-eq? (wave-status-symbol "VERIFYING") 'verifying)
  (check-eq? (wave-status-symbol "AWAITING-DELIVERY") 'awaiting-delivery)
  (check-eq? (wave-status-symbol "DONE") 'done)
  (check-eq? (wave-status-symbol "COMPLETED") 'done)
  (check-eq? (wave-status-symbol "FAILED") 'failed)
  (check-eq? (wave-status-symbol "INTERRUPTED") 'interrupted)
  (check-eq? (wave-status-symbol "DEFERRED") 'deferred)
  (check-eq? (wave-status-symbol 'Done) 'done)
  (check-true (wave-status-recognized? "COMPLETED"))
  (check-true (wave-status-recognized? " in progress "))
  (check-false (wave-status-recognized? "CANCELLED"))
  (check-false (wave-status-recognized? "unknown")))

(test-case "unknown input keeps the pre-W1 pending fallback (no silent new meaning)"
  (check-eq? (wave-status-symbol "CANCELLED") 'pending)
  (check-eq? (wave-status-symbol "REWORK") 'pending)
  (check-eq? (wave-status-symbol "") 'pending)
  (check-false (wave-status-canonical "CANCELLED")))

(test-case "DEFERRED is disjoint from DONE/CANCELLED status semantics"
  ;; Deferred is its own status: never completed, actionable, or retryable.
  (check-true (deferred-status? 'deferred))
  (check-false (done-status? 'deferred))
  (check-false (completed-status? 'deferred))
  (check-false (actionable-status? 'deferred))
  (check-false (retryable-status? 'deferred))
  ;; DONE is not deferred and not retryable; both are terminal for campaign
  ;; completion, which does not MERGE them.
  (check-true (done-status? 'done))
  (check-true (completed-status? 'done))
  (check-false (deferred-status? 'done))
  (check-false (retryable-status? 'done))
  (check-true (terminal-wave-status? 'done))
  (check-true (terminal-wave-status? 'deferred))
  (check-false (terminal-wave-status? 'failed))
  (check-false (terminal-wave-status? 'awaiting-delivery))
  ;; Existing display grouping is a grouping only.
  (check-true (done-or-deferred? "DONE"))
  (check-true (done-or-deferred? "DEFERRED"))
  (check-false (done-or-deferred? "COMPLETED")))

(test-case "marker vocabulary retains canonical and legacy spellings verbatim"
  (check-equal? (wave-status-marker STATUS-INBOX) "[Inbox]")
  (check-equal? (wave-status-marker STATUS-IN-PROGRESS) "[In-Progress]")
  (check-equal? (wave-status-marker STATUS-DONE) "[DONE]")
  (check-equal? (wave-status-marker STATUS-DEFERRED) "[DEFERRED]")
  (check-equal? (wave-status-marker STATUS-FAILED) "[FAILED]")
  (check-equal? (wave-status-marker "COMPLETED") "[COMPLETED]")
  (check-equal? (wave-status-marker "CANCELLED") "[CANCELLED]")
  ;; Unknown string / symbol / other keep the historical fallbacks.
  (check-equal? (wave-status-marker "WEIRD") "[WEIRD]")
  ;; Symbol spellings are stringified verbatim (no case folding), exactly as
  ;; before the consolidation; canonical writers pass display strings.
  (check-equal? (wave-status-marker 'done) "[done]")
  (check-equal? (wave-status-marker 'DONE) "[DONE]")
  (check-equal? (wave-status-marker 42) "[Inbox]")
  (check-equal? (length WAVE-STATUS-MARKERS) 7)
  (for ([pair (in-list WAVE-STATUS-MARKERS)])
    (check-true (string? (car pair)))
    (check-equal? (string-ref (cdr pair) 0) #\[)
    (check-equal? (string-ref (cdr pair) (sub1 (string-length (cdr pair)))) #\])))
