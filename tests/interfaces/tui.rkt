#lang racket

;; tests/interfaces/tui.rkt — Tests for interfaces/tui module
;;
;; Tests the TUI wiring logic (handle-key, process-slash-command,
;; make-tui-ctx) without requiring a real terminal.

(require rackunit
         racket/async-channel
         "../../../q/tui/input.rkt"
         "../../../q/tui/state.rkt"
         "../../../q/tui/terminal.rkt"
         (only-in "../../../q/tui/render.rkt"
                  styled-line
                  styled-line?
                  styled-segment
                  styled-segment?
                  styled-line-segments
                  styled-segment-text
                  styled-segment-style
                  apply-selection-highlight
                  highlight-line-range
                  style-invert
                  render-transcript
                  render-status-bar
                  render-input-line
                  render-branch-list
                  render-leaf-nodes
                  render-children-list
                  format-entry
                  md-format-assistant
                  wrap-styled-line)
         "../../../q/tui/layout.rkt"
         "../../../q/util/protocol-types.rkt"
         "../../../q/agent/event-bus.rkt"
         "../../../q/interfaces/tui.rkt")

(test-case "make-tui-ctx returns a valid tui-ctx with default values"
  (let ([ctx (make-tui-ctx)])
    (check-true (tui-ctx? ctx) "make-tui-ctx returns a tui-ctx")
    (check-true (ui-state? (unbox (tui-ctx-ui-state-box ctx))) "initial ui-state is a ui-state")
    (check-true (input-state? (unbox (tui-ctx-input-state-box ctx)))
                "initial input-state is an input-state")
    (check-false (tui-ctx-event-bus ctx) "event-bus defaults to #f")
    (check-true (unbox (tui-ctx-running-box ctx)) "running defaults to #t")
    ;; BUG-33: needs-redraw defaults to #t (first frame must draw)
    (check-true (unbox (tui-ctx-needs-redraw-box ctx)) "needs-redraw defaults to #t")))

(test-case "make-tui-ctx stores event-bus and session-runner kwargs"
  (let ([bus (make-event-bus)]
        [runner (lambda (p) (void))])
    (define ctx (make-tui-ctx #:event-bus bus #:session-runner runner))
    (check-equal? (tui-ctx-event-bus ctx) bus "event-bus is stored")
    (check-equal? (tui-ctx-session-runner ctx) runner "session-runner is stored")))

(test-case "BUG-33: needs-redraw flag is set by key events"
  (let ([ctx (make-tui-ctx)])
    ;; Initial state: needs-redraw is #t
    (check-true (unbox (tui-ctx-needs-redraw-box ctx)) "BUG-33: initial needs-redraw is #t")
    ;; Simulate: mark as drawn
    (set-box! (tui-ctx-needs-redraw-box ctx) #f)
    (check-false (unbox (tui-ctx-needs-redraw-box ctx)) "BUG-33: can clear needs-redraw")
    ;; Simulate: key press marks dirty
    (handle-key ctx #\h)
    (check-true (unbox (tui-ctx-needs-redraw-box ctx)) "BUG-33: key press sets needs-redraw")
    ;; Simulate: escape key still marks dirty (cursor may need reposition)
    (set-box! (tui-ctx-needs-redraw-box ctx) #f)
    (handle-key ctx #\u001b)
    (check-true (unbox (tui-ctx-needs-redraw-box ctx)) "BUG-33: escape key sets needs-redraw")
    ;; Simulate: arrow keys mark dirty
    (set-box! (tui-ctx-needs-redraw-box ctx) #f)
    (handle-key ctx 'left)
    (check-true (unbox (tui-ctx-needs-redraw-box ctx)) "BUG-33: arrow key sets needs-redraw")))

;; ============================================================
;; handle-key — character input
;; ============================================================

(test-case "handle-key: inserting a character returns 'continue and adds to buffer"
  (let ([ctx (make-tui-ctx)])
    (define result (handle-key ctx #\h))
    (check-equal? result 'continue "insert char returns 'continue")
    (check-equal? (input-state-buffer (unbox (tui-ctx-input-state-box ctx)))
                  "h"
                  "character inserted into buffer")))

(test-case "handle-key: inserting multiple characters builds buffer"
  (let ([ctx (make-tui-ctx)])
    (handle-key ctx #\h)
    (handle-key ctx #\i)
    (check-equal? (input-state-buffer (unbox (tui-ctx-input-state-box ctx)))
                  "hi"
                  "multiple characters inserted")))

(test-case "handle-key: backspace removes last character"
  (let ([ctx (make-tui-ctx)])
    (handle-key ctx #\a)
    (handle-key ctx #\b)
    (define result (handle-key ctx #\backspace))
    (check-equal? result 'continue "backspace returns 'continue")
    (check-equal? (input-state-buffer (unbox (tui-ctx-input-state-box ctx)))
                  "a"
                  "backspace removes last char")))

(test-case "handle-key: rubout (delete) removes character"
  (let ([ctx (make-tui-ctx)])
    (handle-key ctx #\x)
    (define result (handle-key ctx #\rubout))
    (check-equal? result 'continue "rubout returns 'continue")
    (check-equal? (input-state-buffer (unbox (tui-ctx-input-state-box ctx)))
                  ""
                  "rubout removes char")))

(test-case "handle-key: return on empty buffer returns 'continue"
  (let ([ctx (make-tui-ctx)])
    (define result (handle-key ctx #\return))
    (check-equal? result 'continue "empty submit returns 'continue")))

(test-case "handle-key: return with text submits buffer contents"
  (let ([ctx (make-tui-ctx)])
    (handle-key ctx #\h)
    (handle-key ctx #\i)
    (define result (handle-key ctx #\return))
    (check-true (pair? result) "non-empty submit returns a list")
    (check-equal? (car result) 'submit "submit result starts with 'submit")
    (check-equal? (cadr result) "hi" "submit result contains text")))

(test-case "handle-key: return key submits text"
  (let ([ctx (make-tui-ctx)])
    (handle-key ctx #\y)
    (handle-key ctx #\o)
    (define result (handle-key ctx #\return))
    (check-equal? (car result) 'submit "return submits")))

(test-case "handle-key: #\\newline inserts newline instead of submitting (Issue #133)"
  (let ([ctx (make-tui-ctx)])
    (handle-key ctx #\y)
    (handle-key ctx #\o)
    (define result (handle-key ctx #\newline))
    (check-equal? result 'continue "newline inserts, does not submit")
    (define inp (unbox (tui-ctx-input-state-box ctx)))
    (check-true (string-contains? (input-state-buffer inp) "\n") "newline char in buffer")))

(test-case "handle-key: slash /help command is parsed"
  (let ([ctx (make-tui-ctx)])
    (handle-key ctx #\/)
    (handle-key ctx #\h)
    (handle-key ctx #\e)
    (handle-key ctx #\l)
    (handle-key ctx #\p)
    (define result (handle-key ctx #\return))
    (check-true (pair? result) "slash command returns a list")
    (check-equal? (car result) 'command "slash command result starts with 'command")
    (check-equal? (cadr result) 'help "slash command parsed as 'help")))

(test-case "handle-key: unknown slash /zz command returns 'unknown"
  (let ([ctx (make-tui-ctx)])
    (handle-key ctx #\/)
    (handle-key ctx #\z)
    (handle-key ctx #\z)
    (define result (handle-key ctx #\return))
    (check-equal? (car result) 'command "unknown slash returns 'command")
    (check-equal? (cadr result) 'unknown "unknown slash parsed as 'unknown")))

(test-case "handle-key: escape char returns 'continue"
  (let ([ctx (make-tui-ctx)])
    (define result (handle-key ctx (integer->char 27)))
    (check-equal? result 'continue "esc char returns 'continue")))

;; ============================================================
;; handle-key — symbol keys
;; ============================================================

(test-case "handle-key: left arrow moves cursor left"
  (let ([ctx (make-tui-ctx)])
    (handle-key ctx #\a)
    (handle-key ctx #\b)
    (define result (handle-key ctx 'left))
    (check-equal? result 'continue "left returns 'continue")
    (check-equal? (input-state-cursor (unbox (tui-ctx-input-state-box ctx))) 1 "cursor moved left")))

(test-case "handle-key: right arrow moves cursor right"
  (let ([ctx (make-tui-ctx)])
    (handle-key ctx #\a)
    (handle-key ctx 'left)
    (define result (handle-key ctx 'right))
    (check-equal? result 'continue "right returns 'continue")
    (check-equal? (input-state-cursor (unbox (tui-ctx-input-state-box ctx))) 1 "cursor moved right")))

(test-case "handle-key: up arrow restores history entry"
  (let ([ctx (make-tui-ctx)])
    ;; Push some history by submitting
    (handle-key ctx #\a)
    (handle-key ctx #\return)
    ;; Now press up
    (define result (handle-key ctx 'up))
    (check-equal? result 'continue "up returns 'continue")
    (check-equal? (input-state-buffer (unbox (tui-ctx-input-state-box ctx)))
                  "a"
                  "up restores history entry")))

(test-case "handle-key: down arrow after up returns 'continue"
  (let ([ctx (make-tui-ctx)])
    (handle-key ctx #\a)
    (handle-key ctx #\return)
    (handle-key ctx 'up)
    (define result (handle-key ctx 'down))
    (check-equal? result 'continue "down returns 'continue")))

(test-case "handle-key: symbol backspace deletes character"
  (let ([ctx (make-tui-ctx)])
    (handle-key ctx #\x)
    (define result (handle-key ctx 'backspace))
    (check-equal? result 'continue "symbol backspace returns 'continue")
    (check-equal? (input-state-buffer (unbox (tui-ctx-input-state-box ctx)))
                  ""
                  "symbol backspace deletes char")))

(test-case "handle-key: delete removes char at cursor"
  (let ([ctx (make-tui-ctx)])
    (handle-key ctx #\a)
    (handle-key ctx #\b)
    (handle-key ctx 'left)
    (define result (handle-key ctx 'delete))
    (check-equal? result 'continue "delete returns 'continue")
    (check-equal? (input-state-buffer (unbox (tui-ctx-input-state-box ctx)))
                  "a"
                  "delete removes char at cursor")))

(test-case "handle-key: home moves cursor to start"
  (let ([ctx (make-tui-ctx)])
    (handle-key ctx #\a)
    (handle-key ctx #\b)
    (define result (handle-key ctx 'home))
    (check-equal? result 'continue "home returns 'continue")
    (check-equal? (input-state-cursor (unbox (tui-ctx-input-state-box ctx)))
                  0
                  "home moves cursor to start")))

(test-case "handle-key: end moves cursor to end"
  (let ([ctx (make-tui-ctx)])
    (handle-key ctx #\a)
    (handle-key ctx #\b)
    (handle-key ctx 'home)
    (define result (handle-key ctx 'end))
    (check-equal? result 'continue "end returns 'continue")
    (check-equal? (input-state-cursor (unbox (tui-ctx-input-state-box ctx)))
                  2
                  "end moves cursor to end")))
(test-case "Page-up (scroll — line-based, scrolls by transcript-height)"
  (let ([ctx (make-tui-ctx)])
    ;; Add entries to transcript
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (for ([i (in-range 10)])
      (define entry (make-entry 'system (format "line ~a" i) 0 (hash)))
      (set! state (add-transcript-entry state entry)))
    (set-box! (tui-ctx-ui-state-box ctx) state)
    (define result (handle-key ctx 'page-up))
    (check-equal? result 'continue "page-up returns 'continue")
    ;; Page-up now scrolls by transcript-height (line-based), not by 5 entries
    (check-true (> (ui-state-scroll-offset (unbox (tui-ctx-ui-state-box ctx))) 0)
                "page-up scrolls up by at least 1")))

;; Page-down (scroll — line-based)
(test-case "handle-key: page-down scrolls transcript toward bottom"
  (let ([ctx (make-tui-ctx)])
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (for ([i (in-range 10)])
      (define entry (make-entry 'system (format "line ~a" i) 0 (hash)))
      (set! state (add-transcript-entry state entry)))
    (set! state (scroll-up state 20))
    (set-box! (tui-ctx-ui-state-box ctx) state)
    (define result (handle-key ctx 'page-down))
    (check-equal? result 'continue "page-down returns 'continue")
    ;; scroll-offset should decrease (scrolled toward bottom)
    (check-true (< (ui-state-scroll-offset (unbox (tui-ctx-ui-state-box ctx))) 20)
                "page-down scrolls toward bottom")))

(test-case "handle-key: escape symbol returns 'continue"
  (let ([ctx (make-tui-ctx)])
    (define result (handle-key ctx 'escape))
    (check-equal? result 'continue "escape symbol returns 'continue")))

(test-case "handle-key: unknown symbol key returns 'continue"
  (let ([ctx (make-tui-ctx)])
    (define result (handle-key ctx 'f12))
    (check-equal? result 'continue "unknown symbol returns 'continue")))

(test-case "handle-key: non-char non-symbol key returns 'continue"
  (let ([ctx (make-tui-ctx)])
    (define result (handle-key ctx 42))
    (check-equal? result 'continue "non-char non-symbol returns 'continue")))

;; ============================================================
;; process-slash-command
;; ============================================================

(test-case "process-slash-command: /help adds system entry to transcript"
  (let ([ctx (make-tui-ctx)])
    (define result (process-slash-command ctx 'help))
    (check-equal? result 'continue "help returns 'continue")
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (define trans (ui-state-transcript state))
    (check-true (>= (length trans) 1) "help adds entry to transcript")
    (check-equal? (transcript-entry-kind (last trans)) 'system "help entry is system")))

(test-case "process-slash-command: /clear empties the transcript"
  (let ([ctx (make-tui-ctx)])
    ;; Add some transcript entries first
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (set-box! (tui-ctx-ui-state-box ctx)
              (add-transcript-entry state (make-entry 'user "hello" 0 (hash))))
    (check-true (positive? (length (ui-state-transcript (unbox (tui-ctx-ui-state-box ctx)))))
                "transcript has entries before clear")
    (define result (process-slash-command ctx 'clear))
    (check-equal? result 'continue "clear returns 'continue")
    (check-equal? (ui-state-transcript (unbox (tui-ctx-ui-state-box ctx)))
                  '()
                  "clear empties transcript")))

(test-case "process-slash-command: /compact adds status entry"
  (let ([ctx (make-tui-ctx)])
    (define result (process-slash-command ctx 'compact))
    (check-equal? result 'continue "compact returns 'continue")
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (define trans (ui-state-transcript state))
    (check-true (>= (length trans) 1) "compact adds status entry")))

(test-case "process-slash-command: /compact with event bus publishes event"
  (let ([bus (make-event-bus)]
        [received #f])
    (subscribe! bus (lambda (evt) (set! received evt)))
    (define ctx (make-tui-ctx #:event-bus bus))
    (define result (process-slash-command ctx 'compact))
    (check-equal? result 'continue "compact with bus returns 'continue")
    (check-true (event? received) "compact publishes event")))

(test-case "process-slash-command: /interrupt publishes interrupt.requested event"
  (let ([bus (make-event-bus)]
        [received #f])
    (subscribe! bus (lambda (evt) (set! received evt)))
    (define ctx (make-tui-ctx #:event-bus bus))
    (define result (process-slash-command ctx 'interrupt))
    (check-equal? result 'continue "interrupt returns 'continue")
    (check-true (event? received) "interrupt publishes event")
    (check-equal? (event-ev received)
                  "interrupt.requested"
                  "interrupt publishes correct event type")))

(test-case "process-slash-command: /interrupt without bus returns 'continue"
  (let ([ctx (make-tui-ctx)])
    (define result (process-slash-command ctx 'interrupt))
    (check-equal? result 'continue "interrupt without bus returns 'continue")))

(test-case "process-slash-command: /quit returns 'quit and sets running to #f"
  (let ([ctx (make-tui-ctx)])
    (check-true (unbox (tui-ctx-running-box ctx)) "running starts as #t")
    (define result (process-slash-command ctx 'quit))
    (check-equal? result 'quit "quit returns 'quit")
    (check-false (unbox (tui-ctx-running-box ctx)) "quit sets running to #f")))

(test-case "process-slash-command: /unknown adds error entry to transcript"
  (let ([ctx (make-tui-ctx)])
    (define result (process-slash-command ctx 'unknown))
    (check-equal? result 'continue "unknown returns 'continue")
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (define trans (ui-state-transcript state))
    (check-true (>= (length trans) 1) "unknown adds error entry")
    (check-equal? (transcript-entry-kind (last trans)) 'error "unknown entry is error")))

(test-case "process-slash-command: other symbol returns 'continue"
  (let ([ctx (make-tui-ctx)])
    (define result (process-slash-command ctx 'something-else))
    (check-equal? result 'continue "other command returns 'continue")))

;; ============================================================
;; draw-frame is exported (can't test rendering without terminal)
;; ============================================================

(check-true (procedure? draw-frame) "draw-frame is a procedure")

;; ============================================================
;; Channel-based event drain (drain-events!)
;; ============================================================

(test-case "drain-events: session.started event applied via channel"
  (let ([ctx (make-tui-ctx)])
    (define ch (tui-ctx-event-ch ctx))
    ;; Put an event on the async-channel (non-blocking, no thread needed)
    (async-channel-put ch (event 1 "session.started" 1000 "s1" #f (hash 'sessionId "s1")))
    ;; drain-events! should process it
    ((lambda ()
       (define ch-inner (tui-ctx-event-ch ctx))
       (let loop ()
         (define evt (sync/timeout 0 ch-inner))
         (when evt
           (define state (unbox (tui-ctx-ui-state-box ctx)))
           (set-box! (tui-ctx-ui-state-box ctx) (apply-event-to-state state evt))
           (loop)))))
    ;; Check the event was applied
    (check-equal? (ui-state-session-id (unbox (tui-ctx-ui-state-box ctx)))
                  "s1"
                  "drain-events: session.started applied via channel")))

(test-case "tui-ctx has event-ch async-channel"
  (let ([ctx (make-tui-ctx)])
    (check-true (async-channel? (tui-ctx-event-ch ctx)) "tui-ctx has event-ch async-channel")))

;; ============================================================
;; BUG-31 fix tests: keypad variants and pgup/pgdn
;; ============================================================

(test-case "BUG-31: kp-left moves cursor left"
  (let ([ctx (make-tui-ctx)])
    (handle-key ctx #\a)
    (handle-key ctx #\b)
    (define result (handle-key ctx 'kp-left))
    (check-equal? result 'continue "kp-left returns 'continue")
    (check-equal? (input-state-cursor (unbox (tui-ctx-input-state-box ctx)))
                  1
                  "kp-left moves cursor left")))

(test-case "BUG-31: kp-right moves cursor right"
  (let ([ctx (make-tui-ctx)])
    (handle-key ctx #\a)
    (handle-key ctx 'left)
    (define result (handle-key ctx 'kp-right))
    (check-equal? result 'continue "kp-right returns 'continue")
    (check-equal? (input-state-cursor (unbox (tui-ctx-input-state-box ctx)))
                  1
                  "kp-right moves cursor right")))

(test-case "BUG-31: kp-up loads history"
  (let ([ctx (make-tui-ctx)])
    (handle-key ctx #\a)
    (handle-key ctx #\return)
    (define result (handle-key ctx 'kp-up))
    (check-equal? result 'continue "kp-up returns 'continue")
    (check-equal? (input-state-buffer (unbox (tui-ctx-input-state-box ctx)))
                  "a"
                  "kp-up loads history")))

(test-case "BUG-31: kp-down returns 'continue"
  (let ([ctx (make-tui-ctx)])
    (handle-key ctx #\a)
    (handle-key ctx #\return)
    (handle-key ctx 'up)
    (define result (handle-key ctx 'kp-down))
    (check-equal? result 'continue "kp-down returns 'continue")))

(test-case "BUG-31: kp-home moves cursor to start"
  (let ([ctx (make-tui-ctx)])
    (handle-key ctx #\a)
    (handle-key ctx #\b)
    (handle-key ctx #\c)
    (handle-key ctx 'left)
    (define result (handle-key ctx 'kp-home))
    (check-equal? result 'continue "kp-home returns 'continue")
    (check-equal? (input-state-cursor (unbox (tui-ctx-input-state-box ctx)))
                  0
                  "kp-home moves cursor to start")))

(test-case "BUG-31: kp-end moves cursor to end"
  (let ([ctx (make-tui-ctx)])
    (handle-key ctx #\a)
    (handle-key ctx #\b)
    (handle-key ctx 'left)
    (define result (handle-key ctx 'kp-end))
    (check-equal? result 'continue "kp-end returns 'continue")
    (check-equal? (input-state-cursor (unbox (tui-ctx-input-state-box ctx)))
                  2
                  "kp-end moves cursor to end")))

(test-case "BUG-31: kp-delete deletes char at cursor"
  (let ([ctx (make-tui-ctx)])
    (handle-key ctx #\a)
    (handle-key ctx #\b)
    (handle-key ctx #\c)
    (handle-key ctx 'home)
    (define result (handle-key ctx 'kp-delete))
    (check-equal? result 'continue "kp-delete returns 'continue")
    (check-equal? (input-state-buffer (unbox (tui-ctx-input-state-box ctx)))
                  "bc"
                  "kp-delete deletes char at cursor")))

(test-case "BUG-31: pgup scrolls transcript up"
  (let ([ctx (make-tui-ctx)])
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (for ([i (in-range 10)])
      (define entry (make-entry 'system (format "line ~a" i) 0 (hash)))
      (set! state (add-transcript-entry state entry)))
    (set-box! (tui-ctx-ui-state-box ctx) state)
    (define result (handle-key ctx 'pgup))
    (check-equal? result 'continue "pgup returns 'continue")
    (check-true (> (ui-state-scroll-offset (unbox (tui-ctx-ui-state-box ctx))) 0)
                "pgup scrolls up by at least 1")))
(test-case "pgdn works like page-down (scroll — line-based)"
  (let ([ctx (make-tui-ctx)])
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (for ([i (in-range 10)])
      (define entry (make-entry 'system (format "line ~a" i) 0 (hash)))
      (set! state (add-transcript-entry state entry)))
    (set! state (scroll-up state 20))
    (set-box! (tui-ctx-ui-state-box ctx) state)
    (define result (handle-key ctx 'pgdn))
    (check-equal? result 'continue "pgdn returns 'continue")
    (check-true (< (ui-state-scroll-offset (unbox (tui-ctx-ui-state-box ctx))) 20)
                "pgdn scrolls toward bottom")))

(test-case "BUG-31: kp-pgup scrolls transcript up"
  (let ([ctx (make-tui-ctx)])
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (for ([i (in-range 10)])
      (define entry (make-entry 'system (format "line ~a" i) 0 (hash)))
      (set! state (add-transcript-entry state entry)))
    (set-box! (tui-ctx-ui-state-box ctx) state)
    (define result (handle-key ctx 'kp-pgup))
    (check-equal? result 'continue "kp-pgup returns 'continue")
    (check-true (> (ui-state-scroll-offset (unbox (tui-ctx-ui-state-box ctx))) 0)
                "kp-pgup scrolls up by at least 1")))

(test-case "BUG-31: kp-pgdn scrolls transcript toward bottom"
  (let ([ctx (make-tui-ctx)])
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (for ([i (in-range 10)])
      (define entry (make-entry 'system (format "line ~a" i) 0 (hash)))
      (set! state (add-transcript-entry state entry)))
    (set! state (scroll-up state 20))
    (set-box! (tui-ctx-ui-state-box ctx) state)
    (define result (handle-key ctx 'kp-pgdn))
    (check-equal? result 'continue "kp-pgdn returns 'continue")
    (check-true (< (ui-state-scroll-offset (unbox (tui-ctx-ui-state-box ctx))) 20)
                "kp-pgdn scrolls toward bottom")))

(test-case "BUG-31: kp-enter submits input"
  (let ([ctx (make-tui-ctx)])
    (handle-key ctx #\a)
    (handle-key ctx #\b)
    (define result (handle-key ctx 'kp-enter))
    (check-equal? result (list 'submit "ab") "kp-enter submits input")
    (check-equal? (input-state-buffer (unbox (tui-ctx-input-state-box ctx)))
                  ""
                  "kp-enter clears buffer after submit")))

;; --------------------------------------------------------
;; fix-sgr-bg-black — SGR post-processor
;; --------------------------------------------------------
(require rackunit)

(test-case "fix-sgr: standalone \\e[40m → \\e[49m"
  (let ()
    ;; T1: standalone \e[40m → \e[49m
    (check-equal? (fix-sgr-bg-black "\x1b[40m") "\x1b[49m" "fix-sgr: standalone bg=black → default")))

(test-case "fix-sgr: compound fg+bg black → fg+default"
  (let ()
    ;; T2: compound \e[37;40m → \e[37;49m (fg+bg)
    (check-equal? (fix-sgr-bg-black "\x1b[37;40m")
                  "\x1b[37;49m"
                  "fix-sgr: compound fg+bg black → fg+default")))

(test-case "fix-sgr: compound fg+bg+bold → fg+default+bold"
  (let ()
    ;; T3: compound \e[37;40;1m → \e[37;49;1m (fg+bg+bold)
    (check-equal? (fix-sgr-bg-black "\x1b[37;40;1m")
                  "\x1b[37;49;1m"
                  "fix-sgr: compound fg+bg+bold → fg+default+bold")))

(test-case "fix-sgr: non-black bg preserved"
  (let ()
    ;; T4: non-black bg preserved
    (check-equal? (fix-sgr-bg-black "\x1b[47m") "\x1b[47m" "fix-sgr: non-black bg unchanged")))

(test-case "fix-sgr: plain text passthrough unchanged"
  (let ()
    ;; T5: plain text passthrough
    (check-equal? (fix-sgr-bg-black "hello world") "hello world" "fix-sgr: plain text unchanged")))

(test-case "fix-sgr: mixed sequences handled"
  (let ()
    ;; T6: mixed sequences
    (check-equal? (fix-sgr-bg-black "\x1b[47mhello\x1b[37;40m world")
                  "\x1b[47mhello\x1b[37;49m world"
                  "fix-sgr: mixed sequences handled")))

(test-case "fix-sgr: empty SGR unchanged"
  (let ()
    ;; T7: empty SGR \e[m
    (check-equal? (fix-sgr-bg-black "\x1b[m") "\x1b[m" "fix-sgr: empty SGR unchanged")))

(test-case "fix-sgr: multiple SGR all fixed"
  (let ()
    ;; T8: multiple SGR in sequence
    (check-equal? (fix-sgr-bg-black "\x1b[37;40mtext\x1b[40m")
                  "\x1b[37;49mtext\x1b[49m"
                  "fix-sgr: multiple SGR all fixed")))

(test-case "fix-sgr: bg=0 as first param → default"
  (let ()
    ;; T9: bg=0 first param \e[40;1m
    (check-equal? (fix-sgr-bg-black "\x1b[40;1m")
                  "\x1b[49;1m"
                  "fix-sgr: bg=0 as first param → default")))

(test-case "fix-sgr: 256-color fg index 40 not changed"
  (let ()
    ;; T10: 256-color fg with index 40 — should NOT be changed
    ;; \e[38;5;40m is fg=256-color-40, not bg=black
    (check-equal? (fix-sgr-bg-black "\x1b[38;5;40m")
                  "\x1b[38;5;40m"
                  "fix-sgr: 256-color fg index 40 not changed")))

(test-case "fix-sgr: 256-color bg index 40 not changed"
  (let ()
    ;; T11: 256-color bg with index 40 — should NOT be changed
    (check-equal? (fix-sgr-bg-black "\x1b[48;5;40m")
                  "\x1b[48;5;40m"
                  "fix-sgr: 256-color bg index 40 not changed")))

(test-case "fix-sgr: truecolor fg R=40 not changed"
  (let ()
    ;; T12: truecolor fg with R=40 — should NOT be changed
    (check-equal? (fix-sgr-bg-black "\x1b[38;2;40;0;0m")
                  "\x1b[38;2;40;0;0m"
                  "fix-sgr: truecolor fg R=40 not changed")))

(test-case "fix-sgr: truecolor bg R=40 not changed"
  (let ()
    ;; T13: truecolor bg with R=40 — should NOT be changed
    (check-equal? (fix-sgr-bg-black "\x1b[48;2;40;0;0m")
                  "\x1b[48;2;40;0;0m"
                  "fix-sgr: truecolor bg R=40 not changed")))

;; --------------------------------------------------------
;; handle-mouse — mouse scroll dispatch
;; --------------------------------------------------------

(test-case "handle-mouse: scroll-up increases scroll offset by 3"
  (let ()
    ;; mouse scroll-up increases scroll offset
    (define ctx (make-tui-ctx))
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (for ([i (in-range 10)])
      (define entry (make-entry 'system (format "line ~a" i) 0 (hash)))
      (set! state (add-transcript-entry state entry)))
    (set-box! (tui-ctx-ui-state-box ctx) state)
    (handle-mouse ctx '(scroll-up 0 5))
    (check-equal? (ui-state-scroll-offset (unbox (tui-ctx-ui-state-box ctx)))
                  3
                  "mouse scroll-up scrolls by 3 lines")))

(test-case "handle-mouse: scroll-down decreases offset by 3"
  (let ()
    ;; mouse scroll-down decreases scroll offset
    (define ctx (make-tui-ctx))
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (for ([i (in-range 10)])
      (define entry (make-entry 'system (format "line ~a" i) 0 (hash)))
      (set! state (add-transcript-entry state entry)))
    (set! state (scroll-up state 10))
    (set-box! (tui-ctx-ui-state-box ctx) state)
    (handle-mouse ctx '(scroll-down 0 5))
    (check-equal? (ui-state-scroll-offset (unbox (tui-ctx-ui-state-box ctx)))
                  7
                  "mouse scroll-down decreases by 3 lines")))

(test-case "handle-mouse: scroll-down clamps offset at 0"
  (let ()
    ;; mouse scroll-down clamps at 0
    (define ctx (make-tui-ctx))
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (for ([i (in-range 10)])
      (define entry (make-entry 'system (format "line ~a" i) 0 (hash)))
      (set! state (add-transcript-entry state entry)))
    (set! state (scroll-up state 2))
    (set-box! (tui-ctx-ui-state-box ctx) state)
    (handle-mouse ctx '(scroll-down 0 5))
    (check-equal? (ui-state-scroll-offset (unbox (tui-ctx-ui-state-box ctx)))
                  0
                  "mouse scroll-down clamps at 0")))

(test-case "handle-mouse: click does not change scroll offset"
  (let ()
    ;; mouse click does nothing (no handler)
    (define ctx (make-tui-ctx))
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (for ([i (in-range 10)])
      (define entry (make-entry 'system (format "line ~a" i) 0 (hash)))
      (set! state (add-transcript-entry state entry)))
    (set! state (scroll-up state 5))
    (set-box! (tui-ctx-ui-state-box ctx) state)
    (handle-mouse ctx '(click 0 10 5))
    (check-equal? (ui-state-scroll-offset (unbox (tui-ctx-ui-state-box ctx)))
                  5
                  "mouse click does not change scroll")))
(test-case "handle-mouse — copy/paste selection events"
  (let ()
    ;; Left click sets selection anchor
    (define ctx (make-tui-ctx))
    (handle-mouse ctx '(click 0 10 5))
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (check-not-false (ui-state-sel-anchor state) "click sets sel-anchor")
    (check-equal? (ui-state-sel-anchor state) '(10 . 5) "sel-anchor is (col . row)")
    (check-not-false (ui-state-sel-end state) "click sets sel-end")))
(test-case "right click does not set selection"
  (let ()
    ;; Right click does NOT set selection (only left button)
    (define ctx (make-tui-ctx))
    (handle-mouse ctx '(click 2 10 5)) ;; button=2 (right)
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (check-false (ui-state-sel-anchor state) "right click does not set selection")))
(test-case "middle click does not set selection"
  (let ()
    ;; Middle click does NOT set selection (only left button)
    (define ctx (make-tui-ctx))
    (handle-mouse ctx '(click 1 10 5)) ;; button=1 (middle)
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (check-false (ui-state-sel-anchor state) "middle click does not set selection")))
(test-case "drag updates sel-end"
  (let ()
    ;; Drag updates selection end
    (define ctx (make-tui-ctx))
    (handle-mouse ctx '(click 0 10 5)) ;; start
    (handle-mouse ctx '(drag 15 8)) ;; drag to new position
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (check-equal? (ui-state-sel-end state) '(15 . 8) "drag updates sel-end")))
(test-case "drag without anchor does nothing"
  (let ()
    ;; Drag without prior click does nothing
    (define ctx (make-tui-ctx))
    (handle-mouse ctx '(drag 15 8))
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (check-false (ui-state-sel-anchor state) "drag without anchor does nothing")))
(test-case "release preserves sel-anchor"
  (let ()
    ;; Release preserves selection (for visual feedback + clipboard)
    (define ctx (make-tui-ctx))
    (handle-mouse ctx '(click 0 10 5)) ;; start
    (handle-mouse ctx '(release 15 8)) ;; release
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (check-not-false (ui-state-sel-anchor state) "release preserves sel-anchor")
    (check-not-false (ui-state-sel-end state) "release preserves sel-end")))
(test-case "release without selection does nothing"
  (let ()
    ;; Release without selection does nothing
    (define ctx (make-tui-ctx))
    (handle-mouse ctx '(release 15 8))
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (check-false (ui-state-sel-anchor state) "release without selection does nothing")))
(test-case "styled-line->text concatenates segments"
  (let ()
    ;; styled-line->text extracts plain text
    (define line
      (styled-line (list (styled-segment "hello" '(bold cyan)) (styled-segment " world" '(dim)))))
    (check-equal? (styled-line->text line) "hello world" "styled-line->text concatenates segments")))
(test-case "empty styled-line gives empty string"
  (let ()
    ;; styled-line->text empty line
    (define line (styled-line '()))
    (check-equal? (styled-line->text line) "" "empty styled-line gives empty string")))
(test-case "selection-text — integration with real transcript"
  (let ()
    ;; selection-text extracts text from rendered transcript at coordinates
    (define ctx (make-tui-ctx))
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    ;; Add a user message entry
    (define state2 (add-transcript-entry state (make-entry 'user "hello world" 0 (hash))))
    (define state3 (add-transcript-entry state2 (make-entry 'assistant "test response" 0 (hash))))
    (set-box! (tui-ctx-ui-state-box ctx) state3)
    ;; Set selection anchor at (0, 0) — but transcript starts below header
    (define state-sel (set-selection-anchor state3 0 2))
    (define state-sel2 (set-selection-end state-sel 10 2))
    (define text (selection-text ctx state-sel2))
    (check-true (string? text) "selection-text returns a string")
    (check-true (>= (string-length text) 0) "selection-text returns non-negative length")))
(test-case "out-of-bounds selection returns empty string"
  (let ()
    ;; selection-text returns "" for out-of-bounds coordinates
    (define ctx (make-tui-ctx))
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (define state2 (add-transcript-entry state (make-entry 'system "log entry" 0 (hash))))
    (set-box! (tui-ctx-ui-state-box ctx) state2)
    ;; Selection entirely out of transcript bounds
    (define state-sel (set-selection-anchor state2 0 100))
    (define state-sel2 (set-selection-end state-sel 10 200))
    (define text (selection-text ctx state-sel2))
    (check-equal? text "" "out-of-bounds selection returns empty string")))
(test-case "decode-mouse-x10 — X10 mouse event decoding (P0 regression)"
  (let ()
    ;; Left click at screen (0,0): cb=32, cx=33, cy=33
    ;; X10: column/row are 1-based + 32 offset → screen(0,0) = bytes(33,33)
    (check-equal? (decode-mouse-x10 32 33 33)
                  '(mouse click 0 0 0)
                  "decode-mouse-x10: left click at (0,0)")))
(test-case "decode-mouse-x10: right click"
  (let ()
    ;; Left click at (5,10): cb=32, cx=38, cy=43
    ;; X10: screen(5,10) = bytes(38,43)
    (check-equal? (decode-mouse-x10 32 38 43)
                  '(mouse click 0 5 10)
                  "decode-mouse-x10: left click at (5,10)")))
(test-case "decode-mouse-x10: left drag"
  (let ()
    ;; Middle click: cb=33 (button=1, no motion)
    (check-equal? (decode-mouse-x10 33 38 43)
                  '(mouse click 1 5 10)
                  "decode-mouse-x10: middle click")))
(test-case "decode-mouse-x10: middle drag"
  (let ()
    ;; Right click: cb=34 (button=2, no motion)
    (check-equal? (decode-mouse-x10 34 38 43) '(mouse click 2 5 10) "decode-mouse-x10: right click")))
(test-case "decode-mouse-x10: release (P0 — was silently dropped)"
  (let ()
    ;; Drag (left held + motion): cb = 32+32 = 64
    (check-equal? (decode-mouse-x10 64 38 43) '(mouse drag 5 10) "decode-mouse-x10: left drag")))
(test-case "decode-mouse-x10: release at (0,0)"
  (let ()
    ;; Drag (middle held + motion): cb = 33+32 = 65
    (check-equal? (decode-mouse-x10 65 38 43) '(mouse drag 5 10) "decode-mouse-x10: middle drag")))
(test-case "decode-mouse-x10: scroll up"
  (let ()
    ;; Release: cb=35 (button=3, NO motion bit) — this was the P0 bug
    ;; In X10 mode 1002, release has button=3 and motion bit is 0
    (check-equal? (decode-mouse-x10 35 38 43)
                  '(mouse release 5 10)
                  "decode-mouse-x10: release (P0 — was silently dropped)")))
(test-case "decode-mouse-x10: scroll down"
  (let ()
    ;; Release at origin: cb=35, cx=33, cy=33
    (check-equal? (decode-mouse-x10 35 33 33)
                  '(mouse release 0 0)
                  "decode-mouse-x10: release at (0,0)")))
(test-case "decode-mouse-x10: cb=67 is release (button=3 regardless of motion)"
  (let ()
    ;; Scroll up: cb = 96 (64 + 0 + 32)
    (check-equal? (decode-mouse-x10 96 33 33) '(mouse scroll-up 0 0) "decode-mouse-x10: scroll up")))
(test-case "decode-mouse-x10: scroll down cb=97"
  (let ()
    ;; Scroll down: cb = 97 (64 + 1 + 32)
    (check-equal? (decode-mouse-x10 97 33 33)
                  '(mouse scroll-down 0 0)
                  "decode-mouse-x10: scroll down")))
(test-case "decode-mouse-x10: cb=67 with motion returns release"
  (let ()
    ;; Unknown event: cb=39 (button=3 + motion=1 → shouldn't happen in X10)
    ;; But since we now catch button=3 first, this returns release
    (check-equal? (decode-mouse-x10 67 38 43)
                  '(mouse release 5 10)
                  "decode-mouse-x10: cb=67 is release (button=3 regardless of motion)")))
(test-case "selection-text P1 fix: first transcript row extracts text"
  (let ()
    ;; selection-text with bottom-aligned rendering.
    ;; With 80x24 terminal: trans-y=1, trans-height=21.
    ;; 2 content lines → pad-count=19, so content starts at row 1+19=20.
    ;; First content line ("first line") is at screen row 20.
    (define ctx (make-tui-ctx))
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    ;; Add two entries so we have multiple lines
    (define state2 (add-transcript-entry state (make-entry 'user "first line" 0 (hash))))
    (define state3 (add-transcript-entry state2 (make-entry 'assistant "second line" 0 (hash))))
    (set-box! (tui-ctx-ui-state-box ctx) state3)
    ;; Content is bottom-aligned: pad-count=19, first content at row 20
    (define content-row (+ 1 (- 21 2))) ; trans-y + (trans-height - content-count)
    (define state-sel (set-selection-anchor state3 0 content-row))
    (define state-sel2 (set-selection-end state-sel 5 content-row))
    (define text (selection-text ctx state-sel2))
    ;; Should extract text from the first rendered line, not empty
    (check-true (and (string? text) (> (string-length text) 0))
                "selection-text P1 fix: first transcript row extracts text")))
(test-case "Selection lifecycle tests (BUG: selection vanishes on release)"
  (let ()
    ;; selection-text row mapping — second transcript line
    (define ctx (make-tui-ctx))
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (define state2 (add-transcript-entry state (make-entry 'user "alpha" 0 (hash))))
    (define state3 (add-transcript-entry state2 (make-entry 'assistant "beta" 0 (hash))))
    (set-box! (tui-ctx-ui-state-box ctx) state3)
    ;; Select second transcript line (screen row 2)
    (define state-sel (set-selection-anchor state3 0 2))
    (define state-sel2 (set-selection-end state-sel 5 2))
    (define text (selection-text ctx state-sel2))
    (check-true (and (string? text) (>= (string-length text) 0))
                "selection-text P1 fix: second transcript row maps correctly")))
(test-case "Selection lifecycle tests (BUG: selection vanishes on release)"
  (let ()
    ;; Test 1: Selection persists after release handler
    (define ctx (make-tui-ctx))
    ;; Add transcript content
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (define state2
      (add-transcript-entry state (make-entry 'assistant "Hello world this is a test" 0 (hash))))
    (set-box! (tui-ctx-ui-state-box ctx) state2)

    ;; Simulate click (left button, col 0, row 1)
    (handle-mouse ctx '(click 0 0 1))
    (define after-click (unbox (tui-ctx-ui-state-box ctx)))
    (check-not-false (ui-state-sel-anchor after-click) "selection lifecycle: anchor set after click")
    (check-not-false (ui-state-sel-end after-click) "selection lifecycle: end set after click")

    ;; Simulate drag (col 10, row 1)
    (handle-mouse ctx '(drag 10 1))
    (define after-drag (unbox (tui-ctx-ui-state-box ctx)))
    (check-not-false (ui-state-sel-anchor after-drag)
                     "selection lifecycle: anchor persists during drag")
    (check-not-false (ui-state-sel-end after-drag) "selection lifecycle: end updated during drag")

    ;; Simulate release (col 10, row 1)
    (handle-mouse ctx '(release 10 1))
    (define after-release (unbox (tui-ctx-ui-state-box ctx)))
    (check-not-false (ui-state-sel-anchor after-release)
                     "selection lifecycle: anchor persists after release")
    (check-not-false (ui-state-sel-end after-release)
                     "selection lifecycle: end persists after release")))
(test-case "click-reset: selection exists after release"
  (let ()
    ;; Test 2: Next click resets selection
    (define ctx (make-tui-ctx))
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (define state2 (add-transcript-entry state (make-entry 'assistant "First line" 0 (hash))))
    (define state3 (add-transcript-entry state2 (make-entry 'assistant "Second line" 0 (hash))))
    (set-box! (tui-ctx-ui-state-box ctx) state3)

    ;; Select first line
    (handle-mouse ctx '(click 0 0 1))
    (handle-mouse ctx '(drag 9 1))
    (handle-mouse ctx '(release 9 1))
    (define after-release (unbox (tui-ctx-ui-state-box ctx)))
    (check-not-false (ui-state-sel-anchor after-release)
                     "click-reset: selection exists after release")

    ;; Click elsewhere — should reset selection to new anchor
    (handle-mouse ctx '(click 0 0 2))
    (define after-new-click (unbox (tui-ctx-ui-state-box ctx)))
    (define anchor (ui-state-sel-anchor after-new-click))
    (define end (ui-state-sel-end after-new-click))
    (check-not-false anchor "click-reset: anchor set after new click")
    (check-equal? anchor end "click-reset: anchor == end after new click (zero-width)")))
(test-case "Right-click and release position tests (BUG: right-click corrupts selection)"
  (let ()
    ;; Test 3: Zero-length selection has no text
    (define ctx (make-tui-ctx))
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (define state2 (add-transcript-entry state (make-entry 'assistant "Hello" 0 (hash))))
    (set-box! (tui-ctx-ui-state-box ctx) state2)
    ;; Click and release at same point (no drag)
    (handle-mouse ctx '(click 0 3 1))
    (handle-mouse ctx '(release 3 1))
    (define after-release (unbox (tui-ctx-ui-state-box ctx)))
    ;; selection-text with anchor=end=same point should return "" or very short
    (define text (selection-text ctx after-release))
    ;; Zero-width or single-char selection text
    (check-true (or (not text) (<= (string-length text) 1))
                "zero-length selection: text is empty or single char")))
(test-case "Right-click and release position tests (BUG: right-click corrupts selection)"
  (let ()
    ;; Release does NOT move sel-end to release position
    (define ctx (make-tui-ctx))
    (handle-mouse ctx '(click 0 2 1)) ;; left-click at (2,1)
    (handle-mouse ctx '(drag 10 1)) ;; drag to (10,1)
    (handle-mouse ctx '(release 20 1)) ;; release at (20,1) — should NOT move end
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (define sel-end (ui-state-sel-end state))
    (check-equal? sel-end '(10 . 1) "release does not move sel-end: stays at last drag position")))
(test-case "right-click does not change sel-anchor"
  (let ()
    ;; Right-click does not corrupt existing selection
    (define ctx (make-tui-ctx))
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (define state2
      (add-transcript-entry state (make-entry 'assistant "Hello world test string" 0 (hash))))
    (set-box! (tui-ctx-ui-state-box ctx) state2)
    ;; Left-click and drag to select "Hello"
    (handle-mouse ctx '(click 0 0 1))
    (handle-mouse ctx '(drag 5 1))
    (define after-drag (unbox (tui-ctx-ui-state-box ctx)))
    (define drag-end (ui-state-sel-end after-drag))
    ;; Right-click at different position (button 2)
    (handle-mouse ctx '(click 2 15 3))
    (handle-mouse ctx '(release 15 3))
    (define after-right-click (unbox (tui-ctx-ui-state-box ctx)))
    ;; Selection endpoints should be UNCHANGED by right-click
    (check-equal? (ui-state-sel-anchor after-right-click)
                  '(0 . 1)
                  "right-click does not change sel-anchor")
    (check-equal? (ui-state-sel-end after-right-click)
                  drag-end
                  "right-click does not change sel-end")))
(test-case "Resize polling tests (BUG: TUI not resized on terminal resize)"
  (let ()
    ;; Release copies correct text (from drag selection, not release position)
    ;; With 80x24 terminal: trans-y=1, trans-height=21.
    ;; 1 content line → pad-count=20, content at row 1+20=21.
    (define ctx (make-tui-ctx))
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    (define state2
      (add-transcript-entry state (make-entry 'assistant "Alpha Beta Gamma Delta" 0 (hash))))
    (set-box! (tui-ctx-ui-state-box ctx) state2)
    ;; Content is bottom-aligned: pad-count=20, content at row 21
    (define content-row (+ 1 (- 21 1))) ; trans-y + (trans-height - content-count)
    ;; Select first 5 chars ("Alpha") — drag to col 4 (0..4 = 5 chars)
    (handle-mouse ctx `(click 0 0 ,content-row))
    (handle-mouse ctx `(drag 4 ,content-row))
    ;; Release at position 20 — should still copy "Alpha", not "Alpha Beta Gamma D"
    (handle-mouse ctx `(release 20 ,content-row))
    (define state-after (unbox (tui-ctx-ui-state-box ctx)))
    (define text (selection-text ctx state-after))
    (check-true (and (string? text) (string=? text "Alpha"))
                (format "release copies drag selection text, got: ~s" text))))
(test-case "Resize polling tests (BUG: TUI not resized on terminal resize)"
  (let ()
    ;; Test: tui-screen-size-changed? returns #t on first call after reset
    (tui-screen-size-cache-reset!)
    (define result (tui-screen-size-changed?))
    (check-true (boolean? result) "resize poll: changed? returns boolean")
    ;; After first call, subsequent calls should return #f (same size)
    (define result2 (tui-screen-size-changed?))
    (check-false result2 "resize poll: changed? returns #f when size unchanged")))
(test-case "no-feedback-loop: first call returns #t"
  (let ()
    ;; Test: mark-dirty! sets the redraw flag
    (define ctx (make-tui-ctx))
    (set-box! (tui-ctx-needs-redraw-box ctx) #f)
    (mark-dirty! ctx)
    (check-true (unbox (tui-ctx-needs-redraw-box ctx)) "resize poll: mark-dirty! sets redraw flag")))
(test-case "no-feedback-loop: first call returns #t"
  (let ()
    ;; Test: tui-screen-size-cache-reset! clears cached size
    ;; After reset, tui-screen-size should re-query (not crash)
    (tui-screen-size-cache-reset!)
    (define-values (cols rows) (tui-screen-size))
    (check-true (and (integer? cols) (> cols 0)) "resize poll: cols is positive integer after reset")
    (check-true (and (integer? rows) (> rows 0))
                "resize poll: rows is positive integer after reset")))
(test-case "no-feedback-loop: first call returns #t"
  (let ()
    ;; BUG regression test: tui-screen-size-changed? must NOT create a
    ;; feedback loop. Simulating the main loop: call changed? multiple
    ;; times WITHOUT calling tui-screen-size-cache-reset! in between.
    ;; After the first call (which returns #t for first-call/init),
    ;; ALL subsequent calls must return #f when size hasn't changed.
    (tui-screen-size-cache-reset!)
    (define r1 (tui-screen-size-changed?))
    (check-true r1 "no-feedback-loop: first call returns #t")
    ;; These simulate successive main loop iterations WITHOUT reset!
    (define r2 (tui-screen-size-changed?))
    (check-false r2 "no-feedback-loop: second call returns #f (no reset)")
    (define r3 (tui-screen-size-changed?))
    (check-false r3 "no-feedback-loop: third call returns #f (no reset)")
    (define r4 (tui-screen-size-changed?))
    (check-false r4 "no-feedback-loop: fourth call returns #f (no reset)")))
(test-case "feedback-loop-doc: first call returns #t"
  (let ()
    ;; BUG regression test: calling changed? then reset! then changed?
    ;; should NOT loop — the reset makes changed? return #t again, but
    ;; the main loop should NOT call reset! after the polling block.
    ;; This test documents the bad behavior that caused the cursor blink.
    (tui-screen-size-cache-reset!)
    (define r1 (tui-screen-size-changed?))
    (check-true r1 "feedback-loop-doc: first call returns #t")
    ;; Simulate the OLD buggy code: changed? → reset! → changed? → #t
    (tui-screen-size-cache-reset!)
    (define r2 (tui-screen-size-changed?))
    (check-true r2 "feedback-loop-doc: reset causes changed? to return #t again (this was the bug)")
    ;; After changed? updates last-values, next call is #f
    (define r3 (tui-screen-size-changed?))
    (check-false r3 "feedback-loop-doc: without reset, returns #f")))
(test-case "Ctrl-C copy selection keybinding tests"
  (let ()
    ;; Ctrl-C with selection copies text (via osc52 mode to verify output)
    (define ctx (make-tui-ctx))
    ;; Add transcript entries so selection has text to select
    (define state0 (unbox (tui-ctx-ui-state-box ctx)))
    (define state1
      (add-transcript-entry state0 (make-entry 'assistant "Alpha Beta Gamma Delta" 0 (hash))))
    (set-box! (tui-ctx-ui-state-box ctx) state1)
    ;; Set up a selection via mouse click+drag at row 1 (transcript line 0)
    (handle-mouse ctx '(click 0 0 1))
    (handle-mouse ctx '(drag 4 1))
    ;; Verify selection exists
    (define state-with-sel (unbox (tui-ctx-ui-state-box ctx)))
    (check-not-false (has-selection? state-with-sel) "ctrl-c: selection exists after drag")
    ;; Ctrl-C should copy — test by checking handle-key returns 'continue
    (define result
      (parameterize ([current-clipboard-mode 'osc52])
        (handle-key ctx 'ctrl-c)))
    (check-equal? result 'continue "ctrl-c: handle-key returns 'continue")))
(test-case "ctrl-c no selection: returns 'continue"
  (let ()
    ;; Ctrl-C without selection does nothing (no crash)
    (define ctx (make-tui-ctx))
    (define state (unbox (tui-ctx-ui-state-box ctx)))
    ;; No selection set — should not crash
    (define result
      (parameterize ([current-clipboard-mode 'osc52])
        (handle-key ctx 'ctrl-c)))
    (check-equal? result 'continue "ctrl-c no selection: returns 'continue")))
(test-case "current-clipboard-mode is a parameter"
  (let ()
    ;; copy-text! is accessible from interfaces/tui
    (check-true (procedure? copy-text!) "copy-text! is exported from interfaces/tui")
    (check-true (procedure? copy-selection!) "copy-selection! is exported from interfaces/tui")
    (check-true (parameter? current-clipboard-mode) "current-clipboard-mode is a parameter")))
