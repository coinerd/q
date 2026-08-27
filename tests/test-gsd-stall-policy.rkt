#lang racket/base

;; tests/test-gsd-stall-policy.rkt — BUG-0042 (v1.00.21 W7)
;;
;; CHARACTERIZATION tests for the stall-policy seam. Written BEFORE the
;; extraction from extensions/gsd/go-orchestrator.rkt into
;; extensions/gsd/stall-policy.rkt and run unchanged AFTER it: the
;; extracted module must reproduce every pinned output byte-for-byte
;; (same expectations file, no edits between the two runs). Imports go
;; through go-orchestrator.rkt, which re-exports the extracted seam
;; after the move (compat shim — no mass test edits).

(require rackunit
         rackunit/text-ui
         racket/string
         (only-in "../extensions/gsd/go-orchestrator.rkt"
                  stall-cause-message?
                  stall-steering-message
                  stall-hard-failure-message
                  wave-failure-notification-kind
                  no-change-rejection?
                  no-change-target-files
                  gsd-stall-exn?
                  make-gsd-stall-exn))

(define suite
  (test-suite "stall-policy characterization (BUG-0042)"

    (test-case "hard-failure message: exact format, no stall-tool, no targets, no recent tools"
      (check-equal? (stall-hard-failure-message 5 3 '() #f '())
                    (string-append
                     "mutation-stall watchdog: attempt terminated after 5 mutation-free "
                     "calls (limit 3). Target files: (none recorded). Recent tools: (none recorded). "
                     "The attempt will be re-attempted automatically with its prior "
                     "context preserved — resume implementation from recorded state.")))

    (test-case "hard-failure message: exact format with targets, stall-tool and recent tools"
      (check-equal?
       (stall-hard-failure-message 12 8 '("src/a.rkt" "src/b.rkt") 'read '(read bash grep))
       (string-append "mutation-stall watchdog: attempt terminated after 12 mutation-free "
                      "calls (limit 8) — repeating 'read'. Target files: src/a.rkt, src/b.rkt. "
                      "Recent tools: read, bash, grep. The attempt will be re-attempted "
                      "automatically with its prior context preserved — resume implementation "
                      "from recorded state.")))

    (test-case "hard-failure message: exact format with targets, no stall-tool"
      (check-equal? (stall-hard-failure-message 4 2 '("x.rkt") #f '(edit))
                    (string-append
                     "mutation-stall watchdog: attempt terminated after 4 mutation-free "
                     "calls (limit 2). Target files: x.rkt. Recent tools: edit. "
                     "The attempt will be re-attempted automatically with its prior "
                     "context preserved — resume implementation from recorded state.")))

    (test-case "stall-cause-message?: prefix classification (canonical prefix)"
      (check-true (stall-cause-message? "mutation-stall watchdog: anything at all"))
      (check-true (stall-cause-message? "mutation-stall watchdog:"))
      (check-true (stall-cause-message? (stall-hard-failure-message 5 3 '() #f '())))
      (check-false (stall-cause-message? "mutation-stall watchdog"))
      (check-false (stall-cause-message? "mutation-stall watchdogX: nope"))
      (check-false (stall-cause-message? "read timeout"))
      (check-false (stall-cause-message? "verifier rejected"))
      (check-false (stall-cause-message? ""))
      (check-false (stall-cause-message? #f))
      (check-false (stall-cause-message? 42)))

    (test-case "steering message: soft-limit wrapper around the re-anchor prompt"
      (define msg (stall-steering-message 9 "W3" "camp-1" "W3: do the thing" '("f1.rkt" "f2.rkt")))
      (check-true (string-contains? msg "[MUTATION-STALL WATCHDOG — SOFT LIMIT REACHED]"))
      (check-true (string-contains?
                   msg
                   (string-append "You have made 9 calls without any edit. "
                                  "Wave targets: f1.rkt, f2.rkt. Begin the first edit now.")))
      (check-true (string-contains? msg "W3: do the thing"))
      (check-true (string-contains? msg "camp-1"))
      (check-true (string-suffix? msg "Begin the first edit now."))
      ;; Characterization: the embedded re-anchor prompt carries its own
      ;; status line; observed behavior — it DOES travel in the message body.
      (check-true
       (string-contains? msg "(no edit has been made yet — this session has only read/explored)")))

    (test-case "steering message: no recorded targets degrade to (none recorded)"
      (define msg (stall-steering-message 3 "W0" "c" "W0: task" '()))
      (check-true (string-contains? msg "Wave targets: (none recorded). Begin the first edit now.")))

    (test-case "no-change-rejection?: prefix classification"
      (check-true (no-change-rejection? "no wave target files changed: f1.rkt, f2.rkt"))
      (check-true (no-change-rejection? "no wave target files changed"))
      (check-false (no-change-rejection? "no wave target files change"))
      (check-false (no-change-rejection? "verifier rejected"))
      (check-false (no-change-rejection? ""))
      (check-false (no-change-rejection? #f)))

    (test-case "no-change-target-files: parses the declared target list"
      (check-equal? (no-change-target-files "no wave target files changed: f1.rkt, f2.rkt")
                    '("f1.rkt" "f2.rkt"))
      (check-equal? (no-change-target-files "no wave target files changed: solo.rkt") '("solo.rkt"))
      (check-equal? (no-change-target-files "no wave target files changed") '())
      (check-equal? (no-change-target-files "verifier rejected") '()))

    (test-case "gsd-stall-exn: exn:fail subtype carrying the explicit cause"
      (define e (make-gsd-stall-exn "mutation-stall watchdog: boom"))
      (check-true (gsd-stall-exn? e))
      (check-true (exn:fail? e))
      (check-equal? (exn-message e) "mutation-stall watchdog: boom")
      (check-true (gsd-stall-exn? (make-gsd-stall-exn "x"))))

    (test-case "wave-failure-notification-kind: stall cause is its own notification kind"
      (check-eq? (wave-failure-notification-kind (stall-hard-failure-message 5 3 '() #f '()))
                 'stall-terminal)
      (check-eq? (wave-failure-notification-kind "mutation-stall watchdog: prefix only")
                 'stall-terminal)
      (check-eq? (wave-failure-notification-kind "verifier rejected") 'wave-failed)
      (check-eq? (wave-failure-notification-kind "") 'wave-failed)
      (check-eq? (wave-failure-notification-kind #f) 'wave-failed)
      (check-eq? (wave-failure-notification-kind 42) 'wave-failed))))

(module+ main
  (exit (run-tests suite)))
(module+ test
  (void (run-tests suite)))
