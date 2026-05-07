#lang racket/base

;; tests/tui/test-gsd-mode-decoupling.rkt — Test TUI→GSD circular import fix
;;
;; Verifies that state-events.rkt uses an injectable parameter for GSD mode
;; instead of directly importing from extensions/gsd/state-machine.rkt.

(require rackunit
         racket/string
         "../../tui/state-events.rkt"
         "../../tui/state-types.rkt"
         "../../util/protocol-types.rkt")

;; Helper to make a test event
(define (make-test-event ev-type
                         payload
                         #:time [time 1000]
                         #:session-id [session-id "test-session"]
                         #:turn-id [turn-id "turn-1"])
  (event 1 ev-type time session-id turn-id payload))

;; ============================================================
;; 1. Parameter defaults to 'idle
;; ============================================================

(test-case "current-gsd-mode-query defaults to idle"
  (check-eq? ((current-gsd-mode-query)) 'idle))

;; ============================================================
;; 2. Parameter can be overridden
;; ============================================================

(test-case "current-gsd-mode-query can be set to executing"
  (parameterize ([current-gsd-mode-query (lambda () 'executing)])
    (check-eq? ((current-gsd-mode-query)) 'executing)))

(test-case "current-gsd-mode-query can be set to exploring"
  (parameterize ([current-gsd-mode-query (lambda () 'exploring)])
    (check-eq? ((current-gsd-mode-query)) 'exploring)))

;; ============================================================
;; 3. Event processing uses parameter (iteration.soft-warning)
;; ============================================================

(test-case "iteration.soft-warning uses default mode (idle→exploring)"
  (define state (initial-ui-state))
  (define evt (make-test-event "iteration.soft-warning" (hasheq 'iteration 3 'remaining 5)))
  (define new-state (apply-event-to-state state evt))
  ;; Should contain "exploring" in the transcript
  (define entries (ui-state-transcript new-state))
  (check-true (not (null? entries)))
  (define text (transcript-entry-text (car entries)))
  (check-true (string-contains? text "exploring") (format "Expected 'exploring' in: ~a" text)))

(test-case "iteration.soft-warning uses executing mode"
  (parameterize ([current-gsd-mode-query (lambda () 'executing)])
    (define state (initial-ui-state))
    (define evt (make-test-event "iteration.soft-warning" (hasheq 'iteration 3 'remaining 5)))
    (define new-state (apply-event-to-state state evt))
    (define entries (ui-state-transcript new-state))
    (check-true (not (null? entries)))
    (define text (transcript-entry-text (car entries)))
    (check-true (string-contains? text "executing") (format "Expected 'executing' in: ~a" text))))

;; ============================================================
;; 4. Event processing uses parameter (exploration.progress)
;; ============================================================

(test-case "exploration.progress uses exploring by default"
  (define state (initial-ui-state))
  (define evt
    (make-test-event "exploration.progress"
                     (hasheq 'consecutive-tools 3 'tool-names '("read" "edit" "bash"))))
  (define new-state (apply-event-to-state state evt))
  (define entries (ui-state-transcript new-state))
  (check-true (not (null? entries)))
  (define text (transcript-entry-text (car entries)))
  (check-true (string-contains? text "exploring") (format "Expected 'exploring' in: ~a" text)))

(test-case "exploration.progress uses executing when set"
  (parameterize ([current-gsd-mode-query (lambda () 'executing)])
    (define state (initial-ui-state))
    (define evt
      (make-test-event "exploration.progress"
                       (hasheq 'consecutive-tools 2 'tool-names '("read" "write"))))
    (define new-state (apply-event-to-state state evt))
    (define entries (ui-state-transcript new-state))
    (check-true (not (null? entries)))
    (define text (transcript-entry-text (car entries)))
    (check-true (string-contains? text "executing") (format "Expected 'executing' in: ~a" text))))
