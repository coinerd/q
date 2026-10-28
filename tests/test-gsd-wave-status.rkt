#lang racket/base
;; @covers extensions/gsd/wave-status.rkt

;; @speed fast  ;; @suite extensions
;; @boundary unit

;; BOUNDARY: integration

;; test-gsd-wave-status.rkt — Tests for canonical wave status constants
;; v0.32.5 W0: Test scaffolding

(require rackunit
         racket/string
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

(test-case "unknown input is a TYPED rejection, never a pending fallback (W5, R-019)"
  (for ([bad (in-list '("CANCELLED" "REWORK" "" "nonsense"))])
    (define ex
      (with-handlers ([exn:fail:wave-status? values])
        (wave-status-symbol bad)
        #f))
    (check-not-false ex (format "~s must be rejected, not coerced" bad))
    (when ex
      (check-equal? (exn:fail:wave-status-value ex) bad)
      (check-true (list? (exn:fail:wave-status-accepted ex)))
      (check-not-false (member "PENDING" (exn:fail:wave-status-accepted ex)))
      (check-not-false (member "COMPLETED" (exn:fail:wave-status-accepted ex)))
      (check-not-false (member "IN PROGRESS" (exn:fail:wave-status-accepted ex)))
      (check-false (exn:fail:wave-status-context ex))
      (check-true (string-contains? (exn-message ex) "unknown wave status"))
      (check-true (string-contains? (exn-message ex) "defaulting to `pending`"))))
  ;; ...also for symbol input outside the vocabulary.
  (check-true (with-handlers ([exn:fail:wave-status? (lambda (e) #t)])
                (wave-status-symbol 'rework)
                #f))
  ;; The pure lookup still answers "not recognized" without raising.
  (check-false (wave-status-canonical "CANCELLED"))
  (check-false (wave-status-canonical "rework")))

(test-case "comparison keys describe an unknown status explicitly (never as pending)"
  (for ([bad (in-list '("CANCELLED" "REWORK" ""))])
    (define key (wave-status-comparison-key bad))
    (check-not-eq? key 'pending)
    (check-equal? (car key) 'unknown)
    (check-equal? (cadr key) (string-upcase (string-trim bad))))
  ;; Same claim compares equal; a different claim does not.
  (check-equal? (wave-status-comparison-key "CANCELLED")
                (wave-status-comparison-key "cancelled"))
  (check-not-equal? (wave-status-comparison-key "CANCELLED")
                    (wave-status-comparison-key "PENDING"))
  ;; Known spellings compare by canonical symbol, exactly as before.
  (check-eq? (wave-status-comparison-key "COMPLETED") 'done)
  (check-eq? (wave-status-comparison-key "Inbox") 'pending))

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
