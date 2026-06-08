#lang racket/base

;; @speed fast  ;; @suite extensions

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
