#lang racket

;; tests/tui/input.rkt — Tests for tui/input module

(require rackunit
         rackunit/text-ui
         "../../../q/tui/input.rkt"
         "../../../q/tui/char-width.rkt")

(define input-tests
  (test-suite
   "TUI Input"

   ;; ============================================================
   ;; initial-input-state
   ;; ============================================================
   (test-case "initial input state has empty buffer"
     (let ([st (initial-input-state)])
       (check-equal? (input-state-buffer st) "")
       (check-equal? (input-state-cursor st) 0)
       (check-equal? (input-state-history st) '())
       (check-false (input-state-history-idx st))
       (check-false (input-state-saved-text st))))

   ;; ============================================================
   ;; input-insert-char
   ;; ============================================================
   (test-case "insert single char at cursor"
     (let ([st (input-insert-char (initial-input-state) #\h)])
       (check-equal? (input-state-buffer st) "h")
       (check-equal? (input-state-cursor st) 1)))

   (test-case "insert multiple chars builds string"
     (let* ([st0 (initial-input-state)]
            [st1 (input-insert-char st0 #\h)]
            [st2 (input-insert-char st1 #\e)]
            [st3 (input-insert-char st2 #\l)]
            [st4 (input-insert-char st3 #\l)]
            [st5 (input-insert-char st4 #\o)])
       (check-equal? (input-state-buffer st5) "hello")
       (check-equal? (input-state-cursor st5) 5)))

   (test-case "insert char in middle of buffer"
     (let* ([st (struct-copy input-state (initial-input-state) [buffer "hllo"] [cursor 1])]
            [st2 (input-insert-char st #\e)])
       (check-equal? (input-state-buffer st2) "hello")
       (check-equal? (input-state-cursor st2) 2)))

   (test-case "insert char at beginning of buffer"
     (let* ([st (struct-copy input-state (initial-input-state) [buffer "ello"] [cursor 0])]
            [st2 (input-insert-char st #\h)])
       (check-equal? (input-state-buffer st2) "hello")
       (check-equal? (input-state-cursor st2) 1)))

   ;; ============================================================
   ;; input-backspace
   ;; ============================================================
   (test-case "backspace at beginning does nothing"
     (let ([st2 (input-backspace (initial-input-state))])
       (check-equal? (input-state-buffer st2) "")
       (check-equal? (input-state-cursor st2) 0)))

   (test-case "backspace removes char before cursor at end"
     (let* ([st (struct-copy input-state (initial-input-state) [buffer "hello"] [cursor 5])]
            [st2 (input-backspace st)])
       (check-equal? (input-state-buffer st2) "hell")
       (check-equal? (input-state-cursor st2) 4)))

   (test-case "backspace removes char before cursor in middle"
     (let* ([st (struct-copy input-state (initial-input-state) [buffer "hello"] [cursor 3])]
            [st2 (input-backspace st)])
       (check-equal? (input-state-buffer st2) "helo")
       (check-equal? (input-state-cursor st2) 2)))

   ;; ============================================================
   ;; input-delete
   ;; ============================================================
   (test-case "delete at end does nothing"
     (let* ([st (struct-copy input-state (initial-input-state) [buffer "hi"] [cursor 2])]
            [st2 (input-delete st)])
       (check-equal? (input-state-buffer st2) "hi")
       (check-equal? (input-state-cursor st2) 2)))

   (test-case "delete removes char after cursor"
     (let* ([st (struct-copy input-state (initial-input-state) [buffer "hello"] [cursor 1])]
            [st2 (input-delete st)])
       (check-equal? (input-state-buffer st2) "hllo")
       (check-equal? (input-state-cursor st2) 1)))

   (test-case "delete removes char at beginning"
     (let* ([st (struct-copy input-state (initial-input-state) [buffer "abc"] [cursor 0])]
            [st2 (input-delete st)])
       (check-equal? (input-state-buffer st2) "bc")
       (check-equal? (input-state-cursor st2) 0)))

   ;; ============================================================
   ;; input-cursor-left, input-cursor-right
   ;; ============================================================
   (test-case "cursor left decrements position"
     (let* ([st (struct-copy input-state (initial-input-state) [buffer "abc"] [cursor 2])])
       (check-equal? (input-state-cursor (input-cursor-left st)) 1)))

   (test-case "cursor left at start stays at zero"
     (let* ([st (struct-copy input-state (initial-input-state) [buffer "abc"] [cursor 0])])
       (check-equal? (input-state-cursor (input-cursor-left st)) 0)))

   (test-case "cursor right increments position"
     (let* ([st (struct-copy input-state (initial-input-state) [buffer "abc"] [cursor 1])])
       (check-equal? (input-state-cursor (input-cursor-right st)) 2)))

   (test-case "cursor right at end stays at end"
     (let* ([st (struct-copy input-state (initial-input-state) [buffer "abc"] [cursor 3])])
       (check-equal? (input-state-cursor (input-cursor-right st)) 3)))

   ;; ============================================================
   ;; input-home, input-end
   ;; ============================================================
   (test-case "home moves cursor to start"
     (let* ([st (struct-copy input-state (initial-input-state) [buffer "hello"] [cursor 3])])
       (check-equal? (input-state-cursor (input-home st)) 0)))

   (test-case "end moves cursor to buffer length"
     (let* ([st (struct-copy input-state (initial-input-state) [buffer "hello"] [cursor 0])])
       (check-equal? (input-state-cursor (input-end st)) 5)))

   ;; ============================================================
   ;; input-clear
   ;; ============================================================
   (test-case "clear empties buffer and resets cursor"
     (let* ([st (struct-copy input-state (initial-input-state) [buffer "hello"] [cursor 3])]
            [st2 (input-clear st)])
       (check-equal? (input-state-buffer st2) "")
       (check-equal? (input-state-cursor st2) 0)))

   ;; ============================================================
   ;; input-history-push
   ;; ============================================================
   (test-case "history push adds entry"
     (check-equal? (input-state-history (input-history-push (initial-input-state) "hello"))
                   '("hello")))

   (test-case "history push ignores empty string"
     (check-equal? (input-state-history (input-history-push (initial-input-state) "")) '()))

   (test-case "history push deduplicates consecutive entries"
     (let* ([st1 (input-history-push (initial-input-state) "hello")]
            [st2 (input-history-push st1 "hello")])
       (check-equal? (input-state-history st2) '("hello"))))

   (test-case "history push allows non-consecutive repeats"
     (let* ([st1 (input-history-push (initial-input-state) "hello")]
            [st2 (input-history-push st1 "world")]
            [st3 (input-history-push st2 "hello")])
       (check-equal? (input-state-history st3) '("hello" "world" "hello"))))

   ;; ============================================================
   ;; input-history-up
   ;; ============================================================
   (test-case "history up on empty history does nothing"
     (let ([st2 (input-history-up (initial-input-state))])
       (check-equal? (input-state-buffer st2) "")))

   (test-case "history up loads newest entry"
     (let* ([st1 (input-history-push (initial-input-state) "first")]
            [st2 (input-history-push st1 "second")]
            [st3 (input-history-up st2)])
       (check-equal? (input-state-buffer st3) "second")
       (check-equal? (input-state-history-idx st3) 1)))

   (test-case "history up navigates to older entry"
     (let* ([st1 (input-history-push (initial-input-state) "first")]
            [st2 (input-history-push st1 "second")]
            [st3 (input-history-up st2)]
            [st4 (input-history-up st3)])
       (check-equal? (input-state-buffer st4) "first")
       (check-equal? (input-state-history-idx st4) 0)))

   (test-case "history up at oldest stays"
     (let* ([st1 (input-history-push (initial-input-state) "only")]
            [st2 (input-history-up st1)]
            [st3 (input-history-up st2)])
       (check-equal? (input-state-buffer st3) "only")
       (check-equal? (input-state-history-idx st3) 0)))

   (test-case "history up saves current text"
     (let* ([st1 (input-history-push (initial-input-state) "prev1")]
            [st2 (struct-copy input-state st1 [buffer "typing..."] [cursor 10] [history-idx #f] [saved-text #f])]
            [st3 (input-history-up st2)])
       (check-equal? (input-state-saved-text st3) "typing...")))

   ;; ============================================================
   ;; input-history-down
   ;; ============================================================
   (test-case "history down when not browsing does nothing"
     (check-equal? (input-state-buffer (input-history-down (initial-input-state))) ""))

   (test-case "history down navigates to newer entry"
     (let* ([st1 (input-history-push (initial-input-state) "first")]
            [st2 (input-history-push st1 "second")]
            [st3 (input-history-up st2)]   ; idx=1
            [st4 (input-history-up st3)]   ; idx=0
            [st5 (input-history-down st4)])
       (check-equal? (input-state-buffer st5) "second")
       (check-equal? (input-state-history-idx st5) 1)))

   (test-case "history down exits browsing at end and restores empty"
     (let* ([st1 (input-history-push (initial-input-state) "only")]
            [st2 (input-history-up st1)]
            [st3 (input-history-down st2)])
       (check-false (input-state-history-idx st3))
       (check-equal? (input-state-buffer st3) "")))

   (test-case "history down restores saved text"
     (let* ([st1 (input-history-push (initial-input-state) "hist1")]
            [st2 (struct-copy input-state st1 [buffer "my draft"] [cursor 8] [history-idx #f] [saved-text #f])]
            [st3 (input-history-up st2)]
            [st4 (input-history-down st3)])
       (check-equal? (input-state-buffer st4) "my draft")
       (check-false (input-state-history-idx st4))))

   ;; ============================================================
   ;; input-submit
   ;; ============================================================
   (test-case "submit returns text and resets state"
     (let ()
       (define-values (text st2) (input-submit (struct-copy input-state (initial-input-state) [buffer "hello world"] [cursor 11])))
       (check-equal? text "hello world")
       (check-equal? (input-state-buffer st2) "")
       (check-equal? (input-state-cursor st2) 0)
       (check-equal? (input-state-history st2) '("hello world"))))

   (test-case "submit on empty buffer returns #f"
     (let ()
       (define-values (text st2) (input-submit (initial-input-state)))
       (check-false text)))

   (test-case "submit on whitespace-only buffer returns #f"
     (let ()
       (define-values (text st2) (input-submit (struct-copy input-state (initial-input-state) [buffer "   "] [cursor 3])))
       (check-false text)))

   (test-case "submit trims whitespace from text"
     (let ()
       (define-values (text st2) (input-submit (struct-copy input-state (initial-input-state) [buffer "  hello  "] [cursor 9])))
       (check-equal? text "hello")))

   ;; ============================================================
   ;; input-current-text, input-at-beginning?, input-at-end?, input-empty?
   ;; ============================================================
   (test-case "input-current-text returns buffer contents"
     (check-equal? (input-current-text (struct-copy input-state (initial-input-state) [buffer "abc"])) "abc"))

   (test-case "input-at-beginning? checks cursor at start"
     (check-true (input-at-beginning? (initial-input-state)))
     (check-false (input-at-beginning? (struct-copy input-state (initial-input-state) [cursor 1]))))

   (test-case "input-at-end? checks cursor at buffer length"
     (check-true (input-at-end? (initial-input-state)))
     (check-true (input-at-end? (struct-copy input-state (initial-input-state) [buffer "ab"] [cursor 2])))
     (check-false (input-at-end? (struct-copy input-state (initial-input-state) [buffer "ab"] [cursor 1]))))

   (test-case "input-empty? checks buffer is empty"
     (check-true (input-empty? (initial-input-state)))
     (check-false (input-empty? (struct-copy input-state (initial-input-state) [buffer "x"]))))

   ;; ============================================================
   ;; input-slash-command
   ;; ============================================================
   (test-case "input-slash-command detects slash commands"
     (check-true (input-slash-command "/help"))
     (check-true (input-slash-command "/quit"))
     (check-false (input-slash-command "hello"))
     (check-false (input-slash-command "")))

   ;; ============================================================
   ;; parse-tui-slash-command
   ;; ============================================================
   (test-case "parse-tui-slash-command parses simple commands"
     (check-equal? (parse-tui-slash-command "/help") 'help)
     (check-equal? (parse-tui-slash-command "/h") 'help)
     (check-equal? (parse-tui-slash-command "/?") 'help)
     (check-equal? (parse-tui-slash-command "/clear") 'clear)
     (check-equal? (parse-tui-slash-command "/cls") 'clear)
     (check-equal? (parse-tui-slash-command "/compact") 'compact)
     (check-equal? (parse-tui-slash-command "/interrupt") 'interrupt)
     (check-equal? (parse-tui-slash-command "/stop") 'interrupt)
     (check-equal? (parse-tui-slash-command "/cancel") 'interrupt)
     (check-equal? (parse-tui-slash-command "/quit") 'quit)
     (check-equal? (parse-tui-slash-command "/exit") 'quit)
     (check-equal? (parse-tui-slash-command "/q") 'quit))

   (test-case "parse-tui-slash-command parses branch inspection commands"
     (check-equal? (parse-tui-slash-command "/branches") 'branches)
     (check-equal? (parse-tui-slash-command "/leaves") 'leaves)
     (check-equal? (parse-tui-slash-command "/switch branch-123") '(switch "branch-123"))
     (check-equal? (parse-tui-slash-command "/children node-456") '(children "node-456")))

   (test-case "parse-tui-slash-command returns error for commands missing args"
     (check-equal? (parse-tui-slash-command "/switch")
                   '(switch-error "Usage: /switch <branch-id>"))
     (check-equal? (parse-tui-slash-command "/children")
                   '(children-error "Usage: /children <node-id>")))

   (test-case "parse-tui-slash-command handles unknown and non-slash input"
     (check-equal? (parse-tui-slash-command "/foo") 'unknown)
     (check-false (parse-tui-slash-command "hello"))
     (check-false (parse-tui-slash-command ""))
     (check-equal? (parse-tui-slash-command "  /help  ") 'help)
     (check-equal? (parse-tui-slash-command "  /branches  ") 'branches)
     (check-equal? (parse-tui-slash-command "/switch  my-branch  ") '(switch "my-branch")))

   ;; ============================================================
   ;; parse-mouse-event
   ;; ============================================================
   (test-case "parse-mouse-event recognizes left click"
     ;; ESC [ M cb cx cy — left click = button 0, cb=32+0=32, col=5+33=38, row=3+33=36
     (let ([ev (parse-mouse-event (bytes #x1b #x5b #x4d 32 38 36))])
       (check-not-false ev)
       (check-equal? (mouse-event-type ev) 'mouse-click)
       (check-equal? (mouse-event-button ev) 0)
       (check-equal? (mouse-event-x ev) 5)
       (check-equal? (mouse-event-y ev) 3)))

   (test-case "parse-mouse-event recognizes right click"
     ;; Right click = button 2, cb=32+2=34
     (let ([ev (parse-mouse-event (bytes #x1b #x5b #x4d 34 33 33))])
       (check-not-false ev)
       (check-equal? (mouse-event-type ev) 'mouse-click)
       (check-equal? (mouse-event-button ev) 2)
       (check-equal? (mouse-event-x ev) 0)
       (check-equal? (mouse-event-y ev) 0)))

   (test-case "parse-mouse-event recognizes scroll up"
     ;; Scroll up: cb=32+0+64=96, cx=10+33=43, cy=5+33=38
     (let ([ev (parse-mouse-event (bytes #x1b #x5b #x4d 96 43 38))])
       (check-not-false ev)
       (check-equal? (mouse-event-type ev) 'mouse-scroll-up)
       (check-equal? (mouse-event-x ev) 10)
       (check-equal? (mouse-event-y ev) 5)))

   (test-case "parse-mouse-event recognizes scroll down"
     ;; Scroll down: cb=32+1+64=97, cx=10+33=43, cy=5+33=38
     (let ([ev (parse-mouse-event (bytes #x1b #x5b #x4d 97 43 38))])
       (check-not-false ev)
       (check-equal? (mouse-event-type ev) 'mouse-scroll-down)
       (check-equal? (mouse-event-x ev) 10)
       (check-equal? (mouse-event-y ev) 5)))

   (test-case "parse-mouse-event returns #f for non-mouse sequence"
     ;; Random bytes that don't start with ESC [ M
     (check-false (parse-mouse-event (bytes #x41 #x42 #x43)))
     ;; Too short
     (check-false (parse-mouse-event (bytes #x1b #x5b #x4d 32)))
     ;; Empty
     (check-false (parse-mouse-event (bytes)))
     ;; Wrong input type
     (check-false (parse-mouse-event "not bytes")))

   (test-case "parse-mouse-event accepts list of integers"
     ;; Same as left click test but with list input
     (let ([ev (parse-mouse-event (list #x1b #x5b #x4d 32 38 36))])
       (check-not-false ev)
       (check-equal? (mouse-event-type ev) 'mouse-click)
       (check-equal? (mouse-event-x ev) 5)
       (check-equal? (mouse-event-y ev) 3)))

   ;; ============================================================
   ;; Horizontal scroll tests (BUG-31 RC-3)
   ;; ============================================================

   (test-case "input-visible-window: short text fits in window"
     (define st (input-state "hello" 3 '() #f #f 0 '() '() '()))
     (define-values (vis offset cursor-col) (input-visible-window st 80))
     (check-equal? vis "hello")
     (check-equal? offset 0)
     (check-equal? cursor-col (+ INPUT-PROMPT-WIDTH 3)))

   (test-case "input-visible-window: cursor at end fits"
     (define st (input-state "hello" 5 '() #f #f 0 '() '() '()))
     (define-values (vis offset cursor-col) (input-visible-window st 80))
     (check-equal? vis "hello")
     (check-equal? offset 0)
     (check-equal? cursor-col (+ INPUT-PROMPT-WIDTH 5)))

   (test-case "input-visible-window: long text scrolls right"
     ;; 30 chars, 20-col terminal -> visible width = 16
     (define buf (make-string 30 #\x))
     (define st (input-state buf 30 '() #f #f 0 '() '() '()))
     (define-values (vis offset cursor-col) (input-visible-window st 20))
     (check-equal? offset 14)  ;; 30 - 17 + 1
     (check-equal? (string-length vis) 16)
     (check-equal? cursor-col (+ INPUT-PROMPT-WIDTH (- 30 offset))))

   (test-case "input-visible-window: scroll follows cursor left"
     ;; Start scrolled to end, cursor moves to beginning
     (define buf (make-string 30 #\x))
     (define st (input-state buf 0 '() #f #f 20 '() '() '()))  ;; cursor=0, offset=20
     (define-values (vis offset cursor-col) (input-visible-window st 20))
     (check-equal? offset 0)  ;; offset adjusts to show cursor at 0
     (check-equal? cursor-col INPUT-PROMPT-WIDTH))

   (test-case "input-visible-window: cursor in middle of long text"
     (define buf (make-string 50 #\x))
     (define st (input-state buf 25 '() #f #f 0 '() '() '()))  ;; cursor=25
     (define-values (vis offset cursor-col) (input-visible-window st 20))
     ;; Visible width = 17, cursor at 25
     ;; offset should be 25-17+1 = 9
     (check-equal? offset 9)
     (check-equal? (string-length vis) 17)
     (check-true (>= cursor-col INPUT-PROMPT-WIDTH))
     (check-true (< cursor-col 20)))

   (test-case "input-visible-window: empty buffer"
     (define st (initial-input-state))
     (define-values (vis offset cursor-col) (input-visible-window st 80))
     (check-equal? vis "")
     (check-equal? offset 0)
     (check-equal? cursor-col INPUT-PROMPT-WIDTH))

   (test-case "input-visible-window: very narrow terminal"
     (define buf "abc")
     (define st (input-state buf 1 '() #f #f 0 '() '() '()))
     (define-values (vis offset cursor-col) (input-visible-window st 5))
     ;; Width 5, prompt 4 -> visible = 1 char
     (check-true (>= (string-length vis) 1))
     (check-true (>= cursor-col INPUT-PROMPT-WIDTH)))

   (test-case "input-state scroll-offset preserved after cursor-right"
     (define st (input-state "hello" 3 '() #f #f 0 '() '() '()))
     (define moved (input-cursor-right st))
     (check-equal? (input-state-cursor moved) 4)
     (check-equal? (input-state-scroll-offset moved) 0))

   (test-case "input-state scroll-offset preserved after cursor-left"
     (define st (input-state "hello" 3 '() #f #f 0 '() '() '()))
     (define moved (input-cursor-left st))
     (check-equal? (input-state-cursor moved) 2)
     (check-equal? (input-state-scroll-offset moved) 0))

   ;; ============================================================
   ;; BUG-41: Non-ASCII input tests
   ;; ============================================================

   (test-case "input-insert-char: umlaut"
     (let* ([st (input-insert-char (initial-input-state) #\ä)])
       (check-equal? (input-state-buffer st) "ä")
       (check-equal? (input-state-cursor st) 1)))

   (test-case "input-insert-char: multiple umlauts"
     (let* ([st0 (initial-input-state)]
            [st1 (input-insert-char st0 #\ä)]
            [st2 (input-insert-char st1 #\ö)]
            [st3 (input-insert-char st2 #\ü)])
       (check-equal? (input-state-buffer st3) "äöü")
       (check-equal? (input-state-cursor st3) 3)))

   (test-case "input-backspace: umlaut"
     (let* ([st0 (initial-input-state)]
            [st1 (input-insert-char st0 #\ä)]
            [st2 (input-backspace st1)])
       (check-equal? (input-state-buffer st2) "")
       (check-equal? (input-state-cursor st2) 0)))

   ;; ============================================================
   ;; input-insert-newline (Issue #133)
   ;; ============================================================

   (test-case "input-insert-newline: inserts newline at cursor position"
     (let* ([st (struct-copy input-state (initial-input-state)
                             [buffer "hello"]
                             [cursor 2])]
            [st2 (input-insert-newline st)])
       (check-equal? (input-state-buffer st2) "he\nllo")
       (check-equal? (input-state-cursor st2) 3)))

   (test-case "input-insert-newline: at end of buffer"
     (let* ([st (struct-copy input-state (initial-input-state)
                             [buffer "hello"]
                             [cursor 5])]
            [st2 (input-insert-newline st)])
       (check-equal? (input-state-buffer st2) "hello\n")
       (check-equal? (input-state-cursor st2) 6)))

   (test-case "input-insert-newline: at beginning of buffer"
     (let* ([st (struct-copy input-state (initial-input-state)
                             [buffer "hello"]
                             [cursor 0])]
            [st2 (input-insert-newline st)])
       (check-equal? (input-state-buffer st2) "\nhello")
       (check-equal? (input-state-cursor st2) 1)))

   (test-case "input-insert-newline: clears history-idx"
     (let* ([st0 (input-history-push (initial-input-state) "old")]
            [st1 (input-history-up st0)]
            [st2 (input-insert-newline st1)])
       (check-false (input-state-history-idx st2)
                    "insert-newline clears history browsing")))

   ;; ============================================================
   ;; CJK/wide-char visible window tests (Wave 1: #369)
   ;; ============================================================

   (test-case "input-visible-window: CJK text fits in window"
     (define st (input-state "你好" 2 '() #f #f 0 '() '() '()))
     (define-values (vis offset cursor-col) (input-visible-window st 80))
     (check-equal? vis "你好")
     (check-equal? offset 0)
     ;; cursor at char 2, visible width from 0 to 2 = 4 cols
     (check-equal? cursor-col (+ INPUT-PROMPT-WIDTH 4)))

   (test-case "input-visible-window: CJK text scrolls"
     ;; 10 CJK chars = 20 visible cols, terminal 12 cols
     (define buf "一二三四五六七八九十")
     (define st (input-state buf 10 '() #f #f 0 '() '() '()))
     (define-values (vis offset cursor-col) (input-visible-window st 12))
     (check-true (> offset 0) "offset should be positive to show cursor")
     (check-true (< (string-visible-width vis) 20) "visible text < full width")
     (check-true (and (>= cursor-col INPUT-PROMPT-WIDTH)
                      (< cursor-col 12))
                (format "cursor-col ~a should be in prompt..cols" cursor-col)))

   (test-case "input-visible-window: mixed ASCII+CJK"
     ;; hello你好 = 5+4 = 9 visible cols
     (define st (input-state "hello你好" 7 '() #f #f 0 '() '() '()))
     (define-values (vis offset cursor-col) (input-visible-window st 20))
     (check-equal? vis "hello你好")
     (check-equal? offset 0)
     ;; cursor at char 7, visible width from 0 to 7 = 5+4 = 9 cols
     (check-equal? cursor-col (+ INPUT-PROMPT-WIDTH 9)))

   (test-case "input-visible-window: cursor before wide char"
     ;; cursor at 0, buffer starts with CJK
     (define st (input-state "你好世界" 0 '() #f #f 0 '() '() '()))
     (define-values (vis offset cursor-col) (input-visible-window st 80))
     (check-equal? offset 0)
     (check-equal? cursor-col INPUT-PROMPT-WIDTH))

   (test-case "input-visible-window: long CJK scrolls when cursor at end"
     (define buf "一二三四五六七八九十")
     (define st (input-state buf 10 '() #f #f 0 '() '() '()))
     (define-values (vis offset cursor-col) (input-visible-window st 10))
     (check-true (> offset 0) "should scroll right")
     (check-true (< cursor-col 10) "cursor should be within terminal width"))

   ;; ============================================================
   ;; Undo / Redo (Issue #396)
   ;; ============================================================

   (test-case "undo restores previous state"
     (define st0 (initial-input-state))
     (define st1 (input-insert-char st0 #\h))
     (define st2 (input-insert-char st1 #\i))
     (check-equal? (input-state-buffer st2) "hi")
     (define st3 (input-undo st2))
     (check-equal? (input-state-buffer st3) "h")
     (check-equal? (input-state-cursor st3) 1))

   (test-case "undo twice restores earlier state"
     (define st0 (initial-input-state))
     (define st1 (input-insert-char st0 #\a))
     (define st2 (input-insert-char st1 #\b))
     (define st3 (input-insert-char st2 #\c))
     (define st4 (input-undo (input-undo st3)))
     (check-equal? (input-state-buffer st4) "a"))

   (test-case "redo restores undone state"
     (define st0 (initial-input-state))
     (define st1 (input-insert-char st0 #\x))
     (define st2 (input-insert-char st1 #\y))
     (define st3 (input-undo st2))
     (check-equal? (input-state-buffer st3) "x")
     (define st4 (input-redo st3))
     (check-equal? (input-state-buffer st4) "xy"))

   (test-case "new edit clears redo stack"
     (define st0 (initial-input-state))
     (define st1 (input-insert-char st0 #\a))
     (define st2 (input-insert-char st1 #\b))
     (define st3 (input-undo st2))
     (define st4 (input-insert-char st3 #\c))
     (define st5 (input-redo st4))
     ;; redo stack was cleared by new edit, so redo does nothing
     (check-equal? (input-state-buffer st5) "ac"))

   (test-case "undo on empty stack does nothing"
     (define st (input-undo (initial-input-state)))
     (check-equal? (input-state-buffer st) ""))

   (test-case "redo on empty stack does nothing"
     (define st (input-redo (initial-input-state)))
     (check-equal? (input-state-buffer st) ""))

   ;; ============================================================
   ;; Kill ring (Issue #397)
   ;; ============================================================

   (test-case "kill-to-end removes text after cursor"
     (define st (struct-copy input-state (initial-input-state) [buffer "hello"] [cursor 2]))
     (define st2 (input-kill-to-end st))
     (check-equal? (input-state-buffer st2) "he")
     (check-equal? (input-state-kill-ring st2) '("llo")))

   (test-case "kill-to-beginning removes text before cursor"
     (define st (struct-copy input-state (initial-input-state) [buffer "hello"] [cursor 3]))
     (define st2 (input-kill-to-beginning st))
     (check-equal? (input-state-buffer st2) "lo")
     (check-equal? (input-state-cursor st2) 0)
     (check-equal? (input-state-kill-ring st2) '("hel")))

   (test-case "kill-word-backward removes previous word"
     (define st (struct-copy input-state (initial-input-state) [buffer "hello world"] [cursor 11]))
     (define st2 (input-kill-word-backward st))
     (check-equal? (input-state-buffer st2) "hello ")
     (check-equal? (input-state-kill-ring st2) '("world")))

   (test-case "yank inserts top of kill ring"
     ;; Kill from middle
     (define st (struct-copy input-state (initial-input-state) [buffer "hello world"] [cursor 5]))
     (define st2 (input-kill-to-end st))
     (check-equal? (input-state-buffer st2) "hello")
     (check-equal? (input-state-kill-ring st2) '(" world"))
     ;; Yank at end
     (define st3 (input-yank st2))
     (check-equal? (input-state-buffer st3) "hello world"))

   (test-case "yank on empty kill ring does nothing"
     (define st (input-yank (initial-input-state)))
     (check-equal? (input-state-buffer st) ""))

   ;; ============================================================
   ;; Word navigation (Issue #398)
   ;; ============================================================

   (test-case "cursor-word-left skips word"
     (define st (struct-copy input-state (initial-input-state) [buffer "hello world"] [cursor 11]))
     (define st2 (input-cursor-word-left st))
     (check-equal? (input-state-cursor st2) 6))

   (test-case "cursor-word-left from middle of word"
     (define st (struct-copy input-state (initial-input-state) [buffer "hello world"] [cursor 8]))
     (define st2 (input-cursor-word-left st))
     (check-equal? (input-state-cursor st2) 6))

   (test-case "cursor-word-right skips word"
     (define st (struct-copy input-state (initial-input-state) [buffer "hello world"] [cursor 0]))
     (define st2 (input-cursor-word-right st))
     (check-equal? (input-state-cursor st2) 5))

   (test-case "cursor-word-right from space"
     (define st (struct-copy input-state (initial-input-state) [buffer "hello world"] [cursor 5]))
     (define st2 (input-cursor-word-right st))
     (check-equal? (input-state-cursor st2) 11))

   (test-case "cursor-word-left at beginning does nothing"
     (define st (struct-copy input-state (initial-input-state) [buffer "hello"] [cursor 0]))
     (check-equal? (input-state-cursor (input-cursor-word-left st)) 0))

   (test-case "cursor-word-right at end does nothing"
     (define st (struct-copy input-state (initial-input-state) [buffer "hello"] [cursor 5]))
     (check-equal? (input-state-cursor (input-cursor-word-right st)) 5))

   ;; ============================================================
   ;; Insert string / paste (Issue #399)
   ;; ============================================================

   (test-case "input-insert-string inserts at cursor"
     (define st (struct-copy input-state (initial-input-state) [buffer "hello"] [cursor 5]))
     (define st2 (input-insert-string st " world"))
     (check-equal? (input-state-buffer st2) "hello world")
     (check-equal? (input-state-cursor st2) 11))

   (test-case "input-insert-string in middle"
     (define st (struct-copy input-state (initial-input-state) [buffer "helo"] [cursor 2]))
     (define st2 (input-insert-string st "l"))
     (check-equal? (input-state-buffer st2) "hello")
     (check-equal? (input-state-cursor st2) 3))

   (test-case "input-insert-string empty does nothing"
     (define st (struct-copy input-state (initial-input-state) [buffer "hello"] [cursor 3]))
     (define st2 (input-insert-string st ""))
     (check-equal? (input-state-buffer st2) "hello")
     (check-equal? (input-state-cursor st2) 3))
   ))

(run-tests input-tests)
