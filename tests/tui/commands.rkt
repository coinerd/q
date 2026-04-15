#lang racket

;; tests/tui/commands.rkt — Tests for tui/commands module
;;
;; Tests for /help command formatting and other command behaviors.

(require rackunit
         rackunit/text-ui
         "../../../q/tui/commands.rkt"
         "../../../q/tui/state.rkt"
         "../../../q/tui/render.rkt"
         "../../../q/util/protocol-types.rkt")

;; Helper to make a simple cmd-ctx with a fresh state
(define (make-test-cctx)
  (cmd-ctx (box (initial-ui-state))
           (box #t) ;; running
           #f ;; event-bus
           #f ;; session-dir
           (box #f) ;; needs-redraw
           #f)) ;; model-registry-box

(define commands-tests
  (test-suite "TUI Commands"

    ;; ============================================================
    ;; /help command produces formatted reference table
    ;; ============================================================

    (test-case "/help produces multiple transcript entries with descriptions"
      (define cctx (make-test-cctx))
      (process-slash-command cctx 'help)
      (define state (unbox (cmd-ctx-state-box cctx)))
      (define transcript (ui-state-transcript state))
      ;; Should have 16 entries: header + 15 commands (including /tree and /name)
      (check-equal? (length transcript) 16 "/help produces 16 entries (header + 15 commands)")
      ;; First entry is the header
      (check-true (string-contains? (transcript-entry-text (first transcript)) "Commands:")
                  "first entry is the Commands: header")
      ;; All entries should be 'system kind
      (for ([entry (in-list transcript)])
        (check-equal? (transcript-entry-kind entry)
                      'system
                      (format "entry '~a' is system kind" (transcript-entry-text entry)))))

    (test-case "/help entries contain command names and descriptions"
      (define cctx (make-test-cctx))
      (process-slash-command cctx 'help)
      (define state (unbox (cmd-ctx-state-box cctx)))
      (define transcript (ui-state-transcript state))
      ;; Check that key commands appear with descriptions
      (define all-text (string-join (map transcript-entry-text transcript) "\n"))
      (check-true (string-contains? all-text "/help") "contains /help")
      (check-true (string-contains? all-text "/clear") "contains /clear")
      (check-true (string-contains? all-text "/compact") "contains /compact")
      (check-true (string-contains? all-text "/interrupt") "contains /interrupt")
      (check-true (string-contains? all-text "/quit") "contains /quit")
      (check-true (string-contains? all-text "/branches") "contains /branches")
      (check-true (string-contains? all-text "/leaves") "contains /leaves")
      (check-true (string-contains? all-text "/children") "contains /children")
      (check-true (string-contains? all-text "/switch") "contains /switch")
      (check-true (string-contains? all-text "/model") "contains /model")
      (check-true (string-contains? all-text "/history") "contains /history")
      (check-true (string-contains? all-text "/fork") "contains /fork")
      (check-true (string-contains? all-text "/sessions") "contains /sessions")
      ;; Check that descriptions are present
      (check-true (string-contains? all-text "Show help") "has /help description")
      (check-true (string-contains? all-text "Clear transcript") "has /clear description")
      (check-true (string-contains? all-text "Exit session") "has /quit description"))

    (test-case "/help each command line has both name and description"
      (define cctx (make-test-cctx))
      (process-slash-command cctx 'help)
      (define state (unbox (cmd-ctx-state-box cctx)))
      (define transcript (ui-state-transcript state))
      ;; Skip header (first entry), check each command entry has format "  /cmd  Description"
      (for ([entry (in-list (cdr transcript))])
        (define text (transcript-entry-text entry))
        (check-true (string-prefix? text "  /") (format "command entry starts with '  /': ~a" text))
        ;; Each entry should contain at least two words after the /
        (check-true (> (length (string-split text)) 2)
                    (format "command entry has name + description: ~a" text))))

    (test-case "/help returns 'continue"
      (define cctx (make-test-cctx))
      (define result (process-slash-command cctx 'help))
      (check-equal? result 'continue "/help returns 'continue"))

    ;; ============================================================
    ;; /clear command
    ;; ============================================================

    (test-case "/clear empties transcript"
      (define cctx (make-test-cctx))
      ;; Add an entry first
      (set-box! (cmd-ctx-state-box cctx)
                (add-transcript-entry (unbox (cmd-ctx-state-box cctx))
                                      (make-entry 'user "hello" 0 (hash))))
      (check-equal? (length (ui-state-transcript (unbox (cmd-ctx-state-box cctx)))) 1)
      (process-slash-command cctx 'clear)
      (check-equal? (length (ui-state-transcript (unbox (cmd-ctx-state-box cctx))))
                    0
                    "/clear empties transcript"))

    ;; ============================================================
    ;; /quit command
    ;; ============================================================

    (test-case "/quit returns 'quit"
      (define cctx (make-test-cctx))
      (define result (process-slash-command cctx 'quit))
      (check-equal? result 'quit "/quit returns 'quit")
      (check-false (unbox (cmd-ctx-running-box cctx)) "running set to false"))

    ;; ============================================================
    ;; unknown command
    ;; ============================================================

    (test-case "unknown command shows error"
      (define cctx (make-test-cctx))
      (process-slash-command cctx 'unknown)
      (define state (unbox (cmd-ctx-state-box cctx)))
      (define transcript (ui-state-transcript state))
      (check-equal? (length transcript) 1 "unknown: adds error entry")
      (check-equal? (transcript-entry-kind (first transcript)) 'error "unknown: entry is error kind")
      (check-true (string-contains? (transcript-entry-text (first transcript)) "Unknown")
                  "unknown: mentions Unknown"))))

(run-tests commands-tests)
