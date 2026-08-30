#lang racket/base

;; tests/test-go-orchestrator-wave-status.rkt — Wave-status transition pins
;;
;; W4 BUG-0048 context: pins the canonical status grammar that
;; scripts/validate-plan.rkt, the shared kernel in wave-executor.rkt,
;; and /go's coordinator all rely on. Restored after the file was lost
;; (never committed) in a previous attempt — committed immediately.
;; Tags: @fast @isolated

(require rackunit
         (file "../extensions/gsd/wave-status.rkt")) ; repo-relative, robust under raco test

(test-case "canonical constants"
  (check-equal? STATUS-INBOX "Inbox")
  (check-equal? STATUS-IN-PROGRESS "In-Progress")
  (check-equal? STATUS-DONE "DONE")
  (check-equal? STATUS-DEFERRED "DEFERRED")
  (check-equal? STATUS-FAILED "FAILED"))

(test-case "ALL-STATUSES completeness"
  (check-equal? (length ALL-STATUSES) 5)
  (for ([s ALL-STATUSES])
    (check-true (wave-status-string? s))))

(test-case "wave-status-string?"
  (check-true (wave-status-string? "DONE"))
  (check-true (wave-status-string? "Inbox"))
  (check-false (wave-status-string? "done")) ; case-sensitive recognition
  (check-false (wave-status-string? "completed")) ; plan-types vocabulary is NOT canonical here
  (check-false (wave-status-string? 'done))
  (check-false (wave-status-string? 42)))

(test-case "terminal-status? accepts case variants"
  (check-true (terminal-status? "DONE"))
  (check-true (terminal-status? "done"))
  (check-true (terminal-status? "DEFERRED"))
  (check-true (terminal-status? "deferred"))
  (check-false (terminal-status? "In-Progress"))
  (check-false (terminal-status? "FAILED")) ; FAILED is NOT terminal — retryable
  (check-false (terminal-status? "Inbox")))

(test-case "done-or-deferred? aliases terminal-status?"
  (for ([s (list "DONE" "deferred" "DEFERRED" "done")])
    (check-true (done-or-deferred? s) s))
  (for ([s (list "FAILED" "In-Progress" "Inbox" "garbage")])
    (check-false (done-or-deferred? s) s)))

(test-case "active-status? is the complement"
  (for ([s (list "FAILED" "In-Progress" "Inbox")])
    (check-true (active-status? s) s))
  (for ([s (list "DONE" "DEFERRED" "done" "deferred")])
    (check-false (active-status? s) s)))

(test-case "normalize-status! canonicalizes case variants"
  (check-equal? (normalize-status! "done") STATUS-DONE)
  (check-equal? (normalize-status! "Done") STATUS-DONE)
  (check-equal? (normalize-status! "IN-BOX") #f) ; hyphenation matters
  (check-equal? (normalize-status! "Inbox") STATUS-INBOX)
  (check-equal? (normalize-status! "in-progress") STATUS-IN-PROGRESS)
  (check-equal? (normalize-status! "IN-PROGRESS") STATUS-IN-PROGRESS)
  (check-equal? (normalize-status! "failed") STATUS-FAILED)
  (check-equal? (normalize-status! "deferred") STATUS-DEFERRED)
  (check-false (normalize-status! "complete"))
  (check-false (normalize-status! "")))
