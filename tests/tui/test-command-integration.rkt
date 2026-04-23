#lang racket

;; tests/tui/test-command-integration.rkt — TUI command integration tests
;;
;; Tests that TUI slash commands correctly interact with session state,
;; transcript, rendering, and the command parsing pipeline.
;;
;; Issue #608: TUI command integration tests

(require rackunit
         rackunit/text-ui
         "../../../q/tui/commands.rkt"
         "../../../q/tui/command-parse.rkt"
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
           #f ;; model-registry-box
           (box #f) ;; last-prompt-box
           #f ;; session-runner
           (box ""))) ;; input-text-box

;; Helper: get transcript text entries from cctx state
(define (get-transcript-texts cctx)
  (define state (unbox (cmd-ctx-state-box cctx)))
  (map transcript-entry-text (ui-state-transcript state)))

;; Helper: check if any transcript entry contains a string
(define (transcript-contains? cctx substr)
  (ormap (lambda (t) (string-contains? t substr)) (get-transcript-texts cctx)))

;; ============================================================
;; Command parsing
;; ============================================================

(test-case "cmd-parse: parse-command-name extracts command from slash prefix"
  (check-equal? (parse-command-name "/help") 'help)
  (check-equal? (parse-command-name "/clear") 'clear)
  (check-equal? (parse-command-name "/quit") 'quit)
  (check-equal? (parse-command-name "/unknown-xyz") 'unknown))

(test-case "cmd-parse: parse-command-name handles edge cases"
  ;; Single "/" returns 'unknown (has slash but no command)
  (check-equal? (parse-command-name "/") 'unknown)
  (check-equal? (parse-command-name "") #f)
  (check-equal? (parse-command-name "no-slash") #f))

(test-case "cmd-parse: resolve-command-name maps aliases"
  ;; "/q" should resolve to 'quit
  (check-equal? (resolve-command-name "/q") 'quit)
  ;; "/h" should resolve to 'help
  (check-equal? (resolve-command-name "/h") 'help))

;; ============================================================
;; /help integration
;; ============================================================

(test-case "cmd-integ: /help triggers needs-redraw"
  (define cctx (make-test-cctx))
  (process-slash-command cctx 'help)
  (check-true (unbox (cmd-ctx-needs-redraw-box cctx)) "/help should set needs-redraw"))

(test-case "cmd-integ: /help transcript entries are 'system kind"
  (define cctx (make-test-cctx))
  (process-slash-command cctx 'help)
  (define state (unbox (cmd-ctx-state-box cctx)))
  (define transcript (ui-state-transcript state))
  (for ([entry (in-list transcript)])
    (check-equal? (transcript-entry-kind entry)
                  'system
                  (format "entry '~a...' should be 'system kind"
                          (substring (transcript-entry-text entry)
                                     0
                                     (min 30 (string-length (transcript-entry-text entry))))))))

;; ============================================================
;; /clear integration
;; ============================================================

(test-case "cmd-integ: /clear removes all transcript entries"
  (define cctx (make-test-cctx))
  ;; Add some transcript entries first
  (define state-box (cmd-ctx-state-box cctx))
  (set-box! state-box
            (struct-copy ui-state
                         (unbox state-box)
                         [transcript
                          (list (transcript-entry 'user "test msg" 0 (hash) #f)
                                (transcript-entry 'assistant "response" 0 (hash) #f))]))
  (process-slash-command cctx 'clear)
  (define state (unbox state-box))
  (check-equal? (length (ui-state-transcript state)) 0 "/clear should empty transcript"))

(test-case "cmd-integ: /clear triggers needs-redraw"
  (define cctx (make-test-cctx))
  (process-slash-command cctx 'clear)
  (check-true (unbox (cmd-ctx-needs-redraw-box cctx)) "/clear should set needs-redraw"))

(test-case "cmd-integ: /clear returns 'continue (doesn't quit)"
  (define cctx (make-test-cctx))
  (define result (process-slash-command cctx 'clear))
  (check-equal? result 'continue "/clear should return 'continue"))

;; ============================================================
;; /quit integration
;; ============================================================

(test-case "cmd-integ: /quit sets running to #f"
  (define cctx (make-test-cctx))
  (check-true (unbox (cmd-ctx-running-box cctx)))
  (process-slash-command cctx 'quit)
  (check-false (unbox (cmd-ctx-running-box cctx)) "/quit should set running to #f"))

(test-case "cmd-integ: /quit returns 'quit"
  (define cctx (make-test-cctx))
  (define result (process-slash-command cctx 'quit))
  (check-equal? result 'quit))

;; ============================================================
;; Unknown commands
;; ============================================================

(test-case "cmd-integ: unknown command shows error in transcript"
  (define cctx (make-test-cctx))
  (process-slash-command cctx 'unknown)
  (check-true (transcript-contains? cctx "Unknown command")
              "unknown command should show error message"))

(test-case "cmd-integ: unknown command still returns 'continue"
  (define cctx (make-test-cctx))
  (define result (process-slash-command cctx 'nonexistent))
  (check-equal? result 'continue "unknown command should return 'continue"))

;; ============================================================
;; Command table completeness
;; ============================================================

(test-case "cmd-integ: all expected commands are in command table"
  (define ct (make-command-table))
  ;; Core commands (note: table keys include the "/" prefix)
  (check-not-false (hash-ref ct "/help" #f) "help in command table")
  (check-not-false (hash-ref ct "/clear" #f) "clear in command table")
  (check-not-false (hash-ref ct "/quit" #f) "quit in command table"))

;; ============================================================
;; State isolation between commands
;; ============================================================

(test-case "cmd-integ: commands don't interfere with each other"
  (define cctx (make-test-cctx))
  ;; Run /help
  (process-slash-command cctx 'help)
  (define state-after-help (unbox (cmd-ctx-state-box cctx)))
  (define help-entry-count (length (ui-state-transcript state-after-help)))
  ;; Run /clear
  (process-slash-command cctx 'clear)
  (define state-after-clear (unbox (cmd-ctx-state-box cctx)))
  (check-equal? (length (ui-state-transcript state-after-clear))
                0
                "/clear should clear /help entries")
  ;; Run /help again — should add entries again
  (process-slash-command cctx 'help)
  (define state-final (unbox (cmd-ctx-state-box cctx)))
  (check-equal? (length (ui-state-transcript state-final))
                help-entry-count
                "/help after /clear should re-add entries"))

;; ============================================================
;; Transcript entry rendering
;; ============================================================

(test-case "cmd-integ: transcript entries can be formatted by render pipeline"
  (define cctx (make-test-cctx))
  (process-slash-command cctx 'help)
  (define state (unbox (cmd-ctx-state-box cctx)))
  (define transcript (ui-state-transcript state))
  ;; Each transcript entry should be renderable by format-entry
  (for ([entry (in-list transcript)])
    (define lines (format-entry entry 80))
    (check-true (list? lines) "format-entry should return a list of styled-lines")
    (check > (length lines) 0 "format-entry should return at least one line")))

;; ============================================================
;; /retry command
;; ============================================================

(test-case "cmd-integ: /retry parses from /retry text"
  (define result (parse-command-name "/retry"))
  (check-equal? result 'retry))

(test-case "cmd-integ: /r parses as retry alias"
  (define result (parse-command-name "/r"))
  (check-equal? result 'retry))

(test-case "cmd-integ: /retry with no last prompt shows error"
  (define cctx (make-test-cctx))
  (process-slash-command cctx 'retry)
  (define state (unbox (cmd-ctx-state-box cctx)))
  (define entries (ui-state-transcript state))
  (check-equal? (length entries) 1)
  (check-equal? (transcript-entry-kind (first entries)) 'error))
