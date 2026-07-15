#lang racket/base

;; @speed fast
;; @suite default
;; tests/test-audit-v09945-w8-tui.rkt — TUI subsystem real-world audit
;;
;; Audit of the TUI subsystem covering:
;;   1. State types (ui-state, streaming-state, selection-state, cache-state, overlay)
;;   2. Transcript entries (construction, ID assignment, kinds)
;;   3. Render cache (set, ref, clear, invalidate, prune, width)
;;   4. Streaming state (busy, status, phase transitions, clear)
;;   5. Char-width (ASCII, CJK, combining marks, zero-width, graphemes)
;;   6. Input state (buffer, cursor, undo/redo, kill-ring, scroll)
;;   7. Mouse events (X10 parsing, decode)
;;   8. Command parsing (tokenize, lookup, validate-args, aliases)
;;   9. Command palette (registry, filter, resolve, completion)
;;  10. Scrollback (serialization, round-trip, max entries)
;;  11. Clipboard (OSC 52 encoding, backend detection)
;;  12. Approval channel (make, set, clear, permissive default)
;;  13. Cell buffer (create, set, get, clear, resize)
;;  14. Frame diff (same-length, grow, shrink, full redraw)

(require rackunit
         racket/list
         racket/string
         json
         racket/file
         racket/set
         ;; State types
         "../tui/state-types.rkt"
         ;; Char width
         "../tui/char-width.rkt"
         ;; Input state types
         (only-in "../tui/input/state-types.rkt"
                  input-state
                  input-state?
                  initial-input-state
                  input-state-buffer
                  input-state-cursor
                  input-state-history
                  input-state-history-idx
                  input-state-saved-text
                  input-state-scroll-offset
                  input-state-undo-stack
                  input-state-redo-stack
                  input-state-kill-ring
                  push-undo
                  push-kill
                  strip-for-undo
                  parse-mouse-event
                  decode-mouse-x10
                  mouse-event
                  mouse-event?
                  mouse-event-type
                  mouse-event-button
                  mouse-event-x
                  mouse-event-y
                  normalize-selection-range
                  MAX-UNDO-STACK
                  MAX-KILL-RING
                  INPUT-PROMPT-WIDTH
                  cursor-marker-string
                  strip-cursor-markers
                  has-cursor-markers?
                  insert-cursor-marker)
         ;; Command parsing
         "../tui/command-parse.rkt"
         ;; Command palette
         (only-in "../tui/palette.rkt"
                  make-command-registry
                  lookup-command
                  resolve-command
                  all-commands
                  commands-by-category
                  filter-commands
                  render-palette-overlay
                  complete-command
                  commands-from-hashes
                  merge-extension-commands
                  cmd-entry
                  cmd-entry?
                  cmd-entry-name
                  cmd-entry-summary
                  cmd-entry-category
                  cmd-entry-aliases
                  cmd-entry-args-spec)
         ;; Scrollback
         (only-in "../tui/scrollback.rkt"
                  transcript-entry->jsexpr
                  jsexpr->transcript-entry
                  save-scrollback
                  load-scrollback
                  reset-scrollback-id-counter!)
         ;; Clipboard
         (only-in "../tui/clipboard.rkt"
                  current-clipboard-mode
                  copy-text!
                  clipboard-backend-available?
                  osc-52-copy
                  detect-clipboard-tool)
         ;; Approval channel
         (only-in "../tui/approval-channel.rkt"
                  approval-channel?
                  make-approval-channel
                  approval-channel-timeout-ms
                  set-approval-channel!
                  clear-approval-channel!
                  set-headless-approval-mode!
                  current-approval-channel
                  approval-await-result
                  approval-put!
                  DEFAULT-APPROVAL-TIMEOUT-MS)
         ;; Cell buffer
         (only-in "../tui/cell-buffer.rkt"
                  cell-buffer
                  cell-buffer?
                  make-cell-buffer
                  cell-buffer-cols
                  cell-buffer-rows
                  cell-buffer-ref
                  cell-buffer-set!
                  cell-buffer-clear!
                  cell-buffer-dirty?)
         ;; Frame diff
         (only-in "../tui/frame-diff.rkt"
                  diff-cmd
                  diff-cmd?
                  diff-cmd-type
                  diff-cmd-row
                  diff-cmd-content
                  diff-frames
                  frame->string-lines)
         ;; Event registry
         (only-in "../tui/state-events/registry.rkt"
                  apply-event-to-state
                  register-event-reducer!
                  call-with-test-registry
                  event-reducer-registered?
                  current-event-reducers
                  get-event-reducers)
         ;; Event struct (needed by registry)
         (only-in "../util/event/event.rkt" event event-ev event?))

;; ---------------------------------------------------------------------------
;; 1. State Types
;; ---------------------------------------------------------------------------

(test-case "audit-state-ui-state-defaults"
  (define s (initial-ui-state))
  (check-true (ui-state? s))
  (check-equal? (ui-state-transcript s) '())
  (check-equal? (ui-state-scroll-offset s) 0)
  (check-false (ui-state-session-id s))
  (check-false (ui-state-model-name s))
  (check-equal? (ui-state-mode s) 'chat)
  (check-false (ui-state-current-branch s))
  (check-equal? (ui-state-visible-branches s) '())
  (check-false (ui-state-active-overlay s))
  (check-false (ui-state-queue-counts s))
  (check-equal? (ui-state-next-entry-id s) 0)
  (check-false (ui-state-focused-component s)))

(test-case "audit-state-ui-state-keyword-args"
  (define s (initial-ui-state #:session-id "test-session" #:model-name "claude" #:mode 'single))
  (check-equal? (ui-state-session-id s) "test-session")
  (check-equal? (ui-state-model-name s) "claude")
  (check-equal? (ui-state-mode s) 'single))

(test-case "audit-state-streaming-state-defaults"
  (define s (initial-ui-state))
  (check-false (ui-state-busy? s))
  (check-false (ui-state-status-message s))
  (check-false (ui-state-pending-tool-name s))
  (check-false (ui-state-streaming-text s))
  (check-false (ui-state-streaming-thinking s))
  (check-false (ui-state-busy-since s))
  (check-equal? (ui-state-streaming-phase s) 'idle)
  (check-false (ui-state-last-delta-ms s)))

(test-case "audit-state-selection-state"
  (define sel (selection-state (cons 0 0) (cons 10 5)))
  (check-true (selection-state? sel))
  (check-equal? (selection-state-anchor sel) (cons 0 0))
  (check-equal? (selection-state-end sel) (cons 10 5))
  (define sel2 (selection-state #f #f))
  (check-true (selection-state? sel2))
  (check-false (selection-state-anchor sel2)))

(test-case "audit-state-overlay-state"
  (define ov (overlay-state 'command-palette '() "" 'top-left #f #f 0 'extra-data))
  (check-true (overlay-state? ov))
  (check-equal? (overlay-state-type ov) 'command-palette)
  (check-equal? (overlay-state-anchor ov) 'top-left)
  (check-equal? (overlay-state-extra ov) 'extra-data))

(test-case "audit-state-cache-state"
  (define cs (cache-state (hash 1 'cached) (cons 80 24)))
  (check-true (cache-state? cs))
  (check-true (hash? (cache-state-entries cs)))
  (check-equal? (cache-state-width cs) (cons 80 24)))

(test-case "audit-state-branch-info"
  (define bi (branch-info "node-1" #f 'user #t #t))
  (check-true (branch-info? bi))
  (check-equal? (branch-info-id bi) "node-1")
  (check-false (branch-info-parent-id bi))
  (check-equal? (branch-info-role bi) 'user)
  (check-true (branch-info-leaf? bi))
  (check-true (branch-info-active? bi)))

;; ---------------------------------------------------------------------------
;; 2. Transcript Entries
;; ---------------------------------------------------------------------------

(test-case "audit-entry-make-entry"
  (define e (make-entry 'assistant "Hello" 12345 (hash 'model "claude")))
  (check-true (transcript-entry? e))
  (check-equal? (transcript-entry-kind e) 'assistant)
  (check-equal? (transcript-entry-text e) "Hello")
  (check-equal? (transcript-entry-timestamp e) 12345)
  (check-false (transcript-entry-id e)))

(test-case "audit-entry-system-and-error"
  (define se (make-system-entry "System message"))
  (check-equal? (transcript-entry-kind se) 'system)
  (check-true (string? (transcript-entry-text se)))
  (check-true (> (transcript-entry-timestamp se) 0))
  (define ee (make-error-entry "Error occurred"))
  (check-equal? (transcript-entry-kind ee) 'error))

(test-case "audit-entry-id-assignment"
  (define s (initial-ui-state))
  (define e (make-entry 'user "test" 0 (hash)))
  (define-values (e2 s2) (assign-entry-id e s))
  (check-equal? (transcript-entry-id e2) 0)
  (check-equal? (ui-state-next-entry-id s2) 1)
  (define-values (e3 s3) (assign-entry-id e s2))
  (check-equal? (transcript-entry-id e3) 1)
  (check-equal? (ui-state-next-entry-id s3) 2))

(test-case "audit-entry-id-monotonic"
  (define s0 (initial-ui-state))
  (define e (make-entry 'system "x" 0 (hash)))
  (define-values (_1 s1) (assign-entry-id e s0))
  (define-values (_2 s2) (assign-entry-id e s1))
  (define-values (_3 s3) (assign-entry-id e s2))
  (check-equal? (ui-state-next-entry-id s3) 3))

;; ---------------------------------------------------------------------------
;; 3. Render Cache
;; ---------------------------------------------------------------------------

(test-case "audit-cache-set-and-ref"
  (define s (initial-ui-state))
  ;; styled-line? = a list where each element is a pair: '((text . "line1"))
  (define lines (list (list (cons 'text "line1"))))
  (define s2 (rendered-cache-set s 0 lines))
  (check-equal? (rendered-cache-ref s2 0) lines))

(test-case "audit-cache-invalidate"
  (define s (initial-ui-state))
  (define lines (list (list (cons 'text "cached"))))
  (define s2 (rendered-cache-set s 0 lines))
  (check-equal? (rendered-cache-ref s2 0) lines)
  (define s3 (rendered-cache-invalidate-entry s2 0))
  (check-false (rendered-cache-ref s3 0)))

(test-case "audit-cache-clear"
  (define s (initial-ui-state))
  (define s2 (rendered-cache-set s 0 (list (list (cons 'text "a")))))
  (define s3 (rendered-cache-set s2 1 (list (list (cons 'text "b")))))
  (define s4 (rendered-cache-clear s3))
  (check-false (rendered-cache-ref s4 0))
  (check-false (rendered-cache-ref s4 1)))

(test-case "audit-cache-width"
  (define s (initial-ui-state))
  ;; width is natural? (width in columns)
  (check-false (rendered-cache-width-valid? s 80))
  (define s2 (rendered-cache-set-width s 80))
  (check-true (rendered-cache-width-valid? s2 80))
  (check-false (rendered-cache-width-valid? s2 100)))

(test-case "audit-cache-prune-at-max"
  (define s0 (initial-ui-state))
  ;; Add 101 entries, verify only 100 are kept (pruning oldest)
  (define s-final
    (for/fold ([s s0]) ([i (in-range 101)])
      (rendered-cache-set s i (list (list (cons 'text (format "line~a" i)))))))
  (define cache (ui-state-rendered-cache s-final))
  (check-true (<= (hash-count cache) 100))
  ;; Entry 0 should be pruned (oldest)
  (check-false (hash-ref cache 0 #f))
  ;; Entry 100 should exist (newest)
  (check-not-false (hash-ref cache 100 #f)))

;; ---------------------------------------------------------------------------
;; 4. Streaming State
;; ---------------------------------------------------------------------------

(test-case "audit-streaming-set-busy"
  (define s (initial-ui-state))
  (check-false (ui-state-busy? s))
  (define s2 (set-busy s #t))
  (check-true (ui-state-busy? s2))
  ;; busy-since should be set when transitioning to busy
  (check-true (number? (ui-state-busy-since s2))))

(test-case "audit-streaming-set-busy-clears-busy-since"
  (define s (initial-ui-state))
  (define s2 (set-busy s #t))
  (define s3 (set-busy s2 #f))
  (check-false (ui-state-busy? s3))
  ;; busy-since should be cleared when busy is set to #f
  (check-false (ui-state-busy-since s3)))

(test-case "audit-streaming-set-status-message"
  (define s (initial-ui-state))
  (check-false (ui-state-status-message s))
  (define s2 (set-status-message s "Thinking..."))
  (check-equal? (ui-state-status-message s2) "Thinking..."))

(test-case "audit-streaming-set-pending-tool"
  (define s (initial-ui-state))
  (define s2 (set-pending-tool-name s "bash"))
  (check-equal? (ui-state-pending-tool-name s2) "bash"))

(test-case "audit-streaming-set-streaming-text"
  (define s (initial-ui-state))
  (define s2 (set-streaming-text s "Hello, world!"))
  (check-equal? (ui-state-streaming-text s2) "Hello, world!"))

(test-case "audit-streaming-set-phase"
  (define s (initial-ui-state))
  (check-equal? (ui-state-streaming-phase s) 'idle)
  (define s2 (set-streaming-phase s 'thinking))
  (check-equal? (ui-state-streaming-phase s2) 'thinking))

(test-case "audit-streaming-clear"
  (define s (initial-ui-state))
  (define s2 (set-streaming-text s "text"))
  (define s3 (set-streaming-thinking s "thoughts"))
  (define s4 (clear-streaming s3))
  (check-false (ui-state-streaming-text s4))
  (check-false (ui-state-streaming-thinking s4)))

(test-case "audit-streaming-set-last-delta"
  (define s (initial-ui-state))
  (define s2 (set-last-delta-ms s 12345.6))
  (check-equal? (ui-state-last-delta-ms s2) 12345.6))

(test-case "audit-streaming-update-streaming-fn"
  (define s (initial-ui-state))
  ;; update-streaming applies a function to the streaming-state
  (define s2 (update-streaming s (lambda (st) (struct-copy streaming-state st [busy? #t]))))
  (check-true (ui-state-busy? s2)))

;; ---------------------------------------------------------------------------
;; 5. Char Width
;; ---------------------------------------------------------------------------

(test-case "audit-char-width-ascii"
  (check-equal? (char-width #\a) 1)
  (check-equal? (char-width #\Z) 1)
  (check-equal? (char-width #\0) 1)
  (check-equal? (char-width #\space) 1)
  (check-equal? (char-width #\!) 1))

(test-case "audit-char-width-control"
  (check-equal? (char-width #\nul) 0)
  (check-equal? (char-width #\tab) 0)
  (check-equal? (char-width #\return) 0)
  (check-equal? (char-width #\u007F) 0)
  (check-equal? (char-width #\u0080) 0))

(test-case "audit-char-width-cjk"
  (check-equal? (char-width #\中) 2)
  (check-equal? (char-width #\文) 2)
  (check-equal? (char-width #\日) 2)
  (check-equal? (char-width #\本) 2)
  (check-equal? (char-width #\語) 2))

(test-case "audit-char-width-combining"
  (check-equal? (char-width #\u0300) 0) ; Combining grave accent
  (check-equal? (char-width #\u0301) 0) ; Combining acute accent
  (check-equal? (char-width #\u0302) 0)) ; Combining circumflex

(test-case "audit-char-width-zero-width"
  (check-equal? (char-width #\u200B) 0) ; Zero Width Space
  (check-equal? (char-width #\u200C) 0) ; Zero Width Non-Joiner
  (check-equal? (char-width #\u200D) 0) ; Zero Width Joiner
  (check-equal? (char-width #\uFE0F) 0)) ; Variation Selector-16

(test-case "audit-char-width-emoji"
  (check-equal? (char-width #\u2600) 2) ; ☀ Sun
  (check-equal? (char-width #\u2601) 2) ; ☁ Cloud
  (check-equal? (char-width #\u2700) 2) ; ✁ Scissors
  (check-equal? (char-width #\u2714) 2)) ; ✔ Heavy check mark

(test-case "audit-string-visible-width"
  (check-equal? (string-visible-width "hello") 5)
  (check-equal? (string-visible-width "") 0)
  (check-equal? (string-visible-width "中文") 4)
  (check-equal? (string-visible-width "a中") 3)
  (check-equal? (string-visible-width "hello\n") 5)) ; \n is control, width 0

(test-case "audit-visible-width-alias"
  (check-equal? (visible-width "test") 4)
  ;; Verify they're the same function
  (check-equal? (visible-width "中文") (string-visible-width "中文")))

(test-case "audit-truncate-to-visible-width"
  (check-equal? (truncate-to-visible-width "hello world" 5) "hello")
  (check-equal? (truncate-to-visible-width "abc" 10) "abc")
  (check-equal? (truncate-to-visible-width "中文test" 4) "中文")
  (check-equal? (truncate-to-visible-width "a中文" 2) "a")
  (check-equal? (truncate-to-visible-width "" 5) ""))

(test-case "audit-display-col-to-string-offset"
  (check-equal? (display-col->string-offset "hello" 3) 3)
  (check-equal? (display-col->string-offset "中文test" 0) 0)
  (check-equal? (display-col->string-offset "中文test" 2) 1)
  (check-equal? (display-col->string-offset "中文test" 4) 2)
  ;; Column past end returns string-length
  (check-equal? (display-col->string-offset "ab" 100) 2))

(test-case "audit-grapheme-count"
  (check-equal? (grapheme-count "hello") 5)
  (check-equal? (grapheme-count "") 0)
  (check-equal? (grapheme-count "中文") 2)
  ;; Combining mark doesn't add a grapheme
  (check-equal? (grapheme-count "e\u0301") 1))

(test-case "audit-substring-by-graphemes"
  (check-equal? (substring-by-graphemes "hello" 1 3) "el")
  (check-equal? (substring-by-graphemes "hello" 0) "hello")
  (check-equal? (substring-by-graphemes "中文" 0 1) "中"))

;; ---------------------------------------------------------------------------
;; 6. Input State
;; ---------------------------------------------------------------------------

(test-case "audit-input-initial-state"
  (define st (initial-input-state))
  (check-true (input-state? st))
  (check-equal? (input-state-buffer st) "")
  (check-equal? (input-state-cursor st) 0)
  (check-equal? (input-state-history st) '())
  (check-false (input-state-history-idx st))
  (check-false (input-state-saved-text st))
  (check-equal? (input-state-scroll-offset st) 0)
  (check-equal? (input-state-undo-stack st) '())
  (check-equal? (input-state-redo-stack st) '())
  (check-equal? (input-state-kill-ring st) '()))

(test-case "audit-input-state-construction"
  (define st (input-state "hello" 3 '("a" "b") #f #f 0 '() '() '()))
  (check-equal? (input-state-buffer st) "hello")
  (check-equal? (input-state-cursor st) 3)
  (check-equal? (input-state-history st) '("a" "b")))

(test-case "audit-input-constants"
  (check-true (> MAX-UNDO-STACK 0))
  (check-true (> MAX-KILL-RING 0))
  (check-true (> INPUT-PROMPT-WIDTH 0)))

(test-case "audit-input-push-undo"
  (define st1 (input-state "abc" 1 '() #f #f 0 '() '() '()))
  (define st2 (input-state "abcd" 2 '() #f #f 0 '() '() '()))
  (define st3 (push-undo st1 st2))
  ;; undo-stack should have st1 (stripped)
  (check-equal? (length (input-state-undo-stack st3)) 1)
  ;; redo-stack should be cleared
  (check-equal? (input-state-redo-stack st3) '()))

(test-case "audit-input-strip-for-undo"
  (define st (input-state "text" 2 '("hist") 1 "saved" 5 (list 'undo) (list 'redo) (list "kill")))
  (define stripped (strip-for-undo st))
  ;; Buffer and cursor preserved
  (check-equal? (input-state-buffer stripped) "text")
  (check-equal? (input-state-cursor stripped) 2)
  ;; History/undo/redo/kill cleared
  (check-equal? (input-state-history stripped) '())
  (check-false (input-state-history-idx stripped))
  (check-false (input-state-saved-text stripped))
  (check-equal? (input-state-undo-stack stripped) '())
  (check-equal? (input-state-redo-stack stripped) '())
  (check-equal? (input-state-kill-ring stripped) '()))

(test-case "audit-input-push-kill"
  (define st (initial-input-state))
  (define st2 (push-kill st "killed text"))
  (check-equal? (input-state-kill-ring st2) '("killed text"))
  ;; Pushing empty string should be a no-op
  (define st3 (push-kill st2 ""))
  (check-equal? (input-state-kill-ring st3) '("killed text")))

(test-case "audit-input-cursor-markers"
  (define marker (cursor-marker-string))
  (check-true (string? marker))
  (check-true (> (string-length marker) 0))
  ;; Insert and strip
  (define marked (insert-cursor-marker "hello" 2))
  (check-true (has-cursor-markers? marked))
  (define stripped (strip-cursor-markers marked))
  (check-false (has-cursor-markers? stripped))
  (check-equal? stripped "hello"))

;; ---------------------------------------------------------------------------
;; 7. Mouse Events
;; ---------------------------------------------------------------------------

(test-case "audit-mouse-parse-x10-click"
  ;; X10 protocol: ESC [ M cb cx cy
  ;; cb=32+button (button 0 = left click, no modifiers)
  ;; cx=33+x (x=0), cy=33+y (y=0)
  (define evt (parse-mouse-event (list #x1b #x5b #x4d 32 33 33)))
  (check-true (mouse-event? evt))
  (check-equal? (mouse-event-type evt) 'mouse-click)
  (check-equal? (mouse-event-button evt) 0)
  (check-equal? (mouse-event-x evt) 0)
  (check-equal? (mouse-event-y evt) 0))

(test-case "audit-mouse-parse-x10-scroll-up"
  ;; cb=96 means scroll up (32+64: button 0 + scroll bit)
  (define evt (parse-mouse-event (list #x1b #x5b #x4d 96 33 33)))
  (check-true (mouse-event? evt))
  (check-equal? (mouse-event-type evt) 'mouse-scroll-up))

(test-case "audit-mouse-parse-x10-scroll-down"
  ;; cb=97 means scroll down (32+64+1)
  (define evt (parse-mouse-event (list #x1b #x5b #x4d 97 33 33)))
  (check-true (mouse-event? evt))
  (check-equal? (mouse-event-type evt) 'mouse-scroll-down))

(test-case "audit-mouse-parse-invalid"
  (check-false (parse-mouse-event (list 0 1 2)))
  (check-false (parse-mouse-event "not-bytes"))
  (check-false (parse-mouse-event (list #x1b #x5b #x4d 32))) ; Too few bytes

  ;; Decode mouse x10 directly
  (define evt (decode-mouse-x10 32 33 33))
  (check-true (mouse-event? evt))
  (check-equal? (mouse-event-type evt) 'mouse-click)
  (check-equal? (mouse-event-button evt) 0))

(test-case "audit-mouse-decode-release"
  (define evt (decode-mouse-x10 35 33 33))
  (check-true (mouse-event? evt))
  (check-equal? (mouse-event-type evt) 'mouse-release))

(test-case "audit-mouse-decode-scroll"
  ;; cb=96 → scroll up (32+64, button 0)
  (define evt (decode-mouse-x10 96 34 35))
  (check-true (mouse-event? evt))
  (check-equal? (mouse-event-type evt) 'mouse-scroll-up))

(test-case "audit-mouse-normalize-selection"
  ;; anchor < end (row-major order)
  (define-values (sc sr ec er) (normalize-selection-range (cons 0 0) (cons 10 5)))
  (check-equal? sc 0)
  (check-equal? sr 0)
  (check-equal? ec 10)
  (check-equal? er 5)
  ;; Reversed order → normalized
  (define-values (sc2 sr2 ec2 er2) (normalize-selection-range (cons 10 5) (cons 0 0)))
  (check-equal? sc2 0)
  (check-equal? sr2 0)
  (check-equal? ec2 10)
  (check-equal? er2 5)
  ;; Same row, different col
  (define-values (sc3 sr3 ec3 er3) (normalize-selection-range (cons 5 3) (cons 2 3)))
  (check-equal? sc3 2)
  (check-equal? ec3 5)
  (check-equal? sr3 3)
  (check-equal? er3 3))

;; ---------------------------------------------------------------------------
;; 8. Command Parsing
;; ---------------------------------------------------------------------------

(test-case "audit-cmd-parse-tokenize-contract-remediated"
  ;; FINDING remediation: tokenize has a precise two-value contract.
  ;; Commands return (values cmd args); non-commands return (values #f '()).
  (define-values (empty-cmd empty-args) (tokenize ""))
  (check-false empty-cmd)
  (check-equal? empty-args '())
  (define-values (text-cmd text-args) (tokenize "hello"))
  (check-false text-cmd)
  (check-equal? text-args '())
  (define-values (help-cmd help-args) (tokenize "/help"))
  (check-equal? help-cmd "/help")
  (check-equal? help-args '())
  (define result (parse-command-name "/help"))
  (check-true (parsed-command? result))
  (check-equal? (parsed-command-canonical-name result) 'help))

(test-case "audit-cmd-parse-with-args-indirect"
  ;; Test tokenize indirectly through parse-command-name
  (define result (parse-command-name "/switch branch-1"))
  (check-true (parsed-command? result))
  (check-equal? (parsed-command-canonical-name result) 'switch)
  (check-equal? (parsed-command-args result) '("branch-1")))

(test-case "audit-cmd-parse-lookup"
  (define table (make-command-table))
  (check-not-false (lookup-command-entry "/help" table))
  (check-not-false (lookup-command-entry "/h" table))
  (check-not-false (lookup-command-entry "/?" table))
  (check-false (lookup-command-entry "/nonexistent" table)))

(test-case "audit-cmd-parse-validate-args-none"
  (define table (make-command-table))
  (define entry (lookup-command-entry "/help" table))
  (define result (validate-args entry '()))
  (check-true (parsed-command? result))
  (check-equal? (parsed-command-canonical-name result) 'help)
  (check-equal? (parsed-command-arg-kind result) 'none))

(test-case "audit-cmd-parse-validate-args-required"
  (define table (make-command-table))
  (define entry (lookup-command-entry "/switch" table))
  (define result (validate-args entry '("branch-1")))
  (check-true (parsed-command? result))
  (check-equal? (parsed-command-canonical-name result) 'switch)
  (check-equal? (parsed-command-args result) '("branch-1"))
  (check-equal? (parsed-command-arg-kind result) 'required))

(test-case "audit-cmd-parse-validate-args-required-missing"
  (define table (make-command-table))
  (define entry (lookup-command-entry "/switch" table))
  (define result (validate-args entry '()))
  (check-true (parsed-command? result))
  (check-equal? (parsed-command-arg-kind result) 'error))

(test-case "audit-cmd-parse-validate-args-optional"
  (define table (make-command-table))
  (define entry (lookup-command-entry "/model" table))
  ;; Without args
  (define r1 (validate-args entry '()))
  (check-equal? (parsed-command-arg-kind r1) 'optional)
  (check-equal? (parsed-command-args r1) '())
  ;; With args
  (define r2 (validate-args entry '("sonnet")))
  (check-equal? (parsed-command-arg-kind r2) 'optional)
  (check-equal? (parsed-command-args r2) '("sonnet")))

(test-case "audit-cmd-parse-full"
  (define r1 (parse-command-name "/help"))
  (check-true (parsed-command? r1))
  (check-equal? (parsed-command-canonical-name r1) 'help)
  (define r2 (parse-command-name "/h"))
  (check-true (parsed-command? r2))
  (check-equal? (parsed-command-canonical-name r2) 'help)
  (define r3 (parse-command-name "hello"))
  (check-false r3)
  (define r4 (parse-command-name "/unknown"))
  (check-equal? r4 'unknown))

(test-case "audit-cmd-parse-resolve-name"
  (check-equal? (resolve-command-name "/help") 'help)
  (check-equal? (resolve-command-name "/h") 'help)
  (check-equal? (resolve-command-name "/quit") 'quit)
  (check-equal? (resolve-command-name "/q") 'quit)
  (check-false (resolve-command-name "/nonexistent")))

;; ---------------------------------------------------------------------------
;; 9. Command Palette
;; ---------------------------------------------------------------------------

(test-case "audit-palette-registry"
  (define reg (make-command-registry))
  (check-true (hash? reg))
  (check-not-false (lookup-command reg "/help"))
  (check-not-false (lookup-command reg "/h"))
  (check-not-false (lookup-command reg "/quit"))
  (check-not-false (lookup-command reg "/clear")))

(test-case "audit-palette-all-commands"
  (define reg (make-command-registry))
  (define cmds (all-commands reg))
  (check-true (> (length cmds) 10))
  ;; Verify sorted by name
  (check-true (string<=? (cmd-entry-name (car cmds)) (cmd-entry-name (cadr cmds)))))

(test-case "audit-palette-by-category"
  (define reg (make-command-registry))
  (define session-cmds (commands-by-category reg 'session))
  (check-true (> (length session-cmds) 0))
  (for ([c (in-list session-cmds)])
    (check-equal? (cmd-entry-category c) 'session)))

(test-case "audit-palette-filter"
  (define reg (make-command-registry))
  (define matches (filter-commands reg "/s"))
  (check-true (> (length matches) 0))
  ;; All should start with "/s"
  (for ([c (in-list matches)])
    (check-true (string-prefix? (cmd-entry-name c) "/s"))))

(test-case "audit-palette-resolve"
  (define reg (make-command-registry))
  (define-values (cmd args) (resolve-command reg "/switch my-branch"))
  (check-true (cmd-entry? cmd))
  (check-equal? args '("my-branch"))
  (define-values (cmd2 args2) (resolve-command reg "not-a-command"))
  (check-false cmd2)
  (check-false args2))

(test-case "audit-palette-complete"
  (define reg (make-command-registry))
  (define completions (complete-command reg "/s"))
  (check-true (> (length completions) 0))
  (for ([name (in-list completions)])
    (check-true (string-prefix? name "/s"))))

(test-case "audit-palette-from-hashes"
  (define hashes
    (list (hash 'name
                "/custom"
                'summary
                "Custom command"
                'category
                'general
                'args-spec
                '()
                'aliases
                '("c"))))
  (define cmds (commands-from-hashes hashes))
  (check-equal? (length cmds) 1)
  (check-equal? (cmd-entry-name (car cmds)) "/custom"))

(test-case "audit-palette-merge-extension"
  (define reg (make-command-registry))
  (define ext-cmd (list (cmd-entry "/ext" "Extension cmd" 'general '() '())))
  (define merged (merge-extension-commands reg ext-cmd))
  (check-not-false (lookup-command merged "/ext")))

(test-case "audit-palette-render-overlay"
  (define reg (make-command-registry))
  (define cmds (filter-commands reg "/h"))
  (define lines (render-palette-overlay "/h" cmds 80))
  (check-true (list? lines))
  (check-true (> (length lines) 0)))

;; ---------------------------------------------------------------------------
;; 10. Scrollback
;; ---------------------------------------------------------------------------

(test-case "audit-scrollback-entry-to-jsexpr"
  (define e (transcript-entry 'assistant "Hello world" 12345 (hash 'model "claude") 42))
  (define js (transcript-entry->jsexpr e))
  (check-true (hash? js))
  (check-equal? (hash-ref js 'kind) "assistant")
  (check-equal? (hash-ref js 'text) "Hello world")
  (check-equal? (hash-ref js 'timestamp) 12345)
  (check-equal? (hash-ref js 'id) 42))

(test-case "audit-scrollback-jsexpr-to-entry"
  (define js (hasheq 'kind "user" 'text "Test message" 'timestamp 99999 'id 5 'meta (hasheq)))
  (reset-scrollback-id-counter!)
  (define e (jsexpr->transcript-entry js))
  (check-true (transcript-entry? e))
  (check-equal? (transcript-entry-kind e) 'user)
  (check-equal? (transcript-entry-text e) "Test message")
  (check-equal? (transcript-entry-timestamp e) 99999))

(test-case "audit-scrollback-round-trip"
  (reset-scrollback-id-counter!)
  (define entries
    (list (transcript-entry 'assistant "Hello" 1000 (hash) #f)
          (transcript-entry 'user "World" 2000 (hash) #f)
          (transcript-entry 'system "System" 3000 (hash 'level 'info) #f)))
  (define jsexprs (map transcript-entry->jsexpr entries))
  (define restored (map jsexpr->transcript-entry jsexprs))
  (check-equal? (length restored) 3)
  (check-equal? (transcript-entry-kind (car restored)) 'assistant)
  (check-equal? (transcript-entry-text (cadr restored)) "World")
  (check-equal? (transcript-entry-kind (caddr restored)) 'system))

(test-case "audit-scrollback-save-load"
  (reset-scrollback-id-counter!)
  (define tmp-dir (make-temporary-file "scrollback-test-~a" 'directory))
  (define tmp-path (build-path tmp-dir "scrollback.jsonl"))
  (define entries (list (transcript-entry 'assistant "Test" 1000 (hash) #f)))
  (save-scrollback entries tmp-path)
  (define loaded (load-scrollback tmp-path))
  (check-equal? (length loaded) 1)
  (check-equal? (transcript-entry-kind (car loaded)) 'assistant)
  (check-equal? (transcript-entry-text (car loaded)) "Test")
  ;; Cleanup
  (delete-directory/files tmp-dir))

(test-case "audit-scrollback-load-nonexistent"
  (define loaded (load-scrollback "/nonexistent/path/scrollback.jsonl"))
  (check-equal? loaded '()))

;; ---------------------------------------------------------------------------
;; 11. Clipboard
;; ---------------------------------------------------------------------------

(test-case "audit-clipboard-backend-available"
  ;; clipboard-backend-available? always returns #t (OSC 52 always available)
  (check-true (clipboard-backend-available?)))

(test-case "audit-clipboard-osc52-encoding"
  ;; Test that OSC 52 produces the correct escape sequence
  (define out (open-output-string))
  (osc-52-copy "hello" out)
  (define result (get-output-string out))
  ;; Should start with ESC ] 52 ; c ;
  (check-true (string-prefix? result "\x1b]52;c;"))
  ;; Should end with ESC \
  (check-true (string-suffix? result "\x1b\\"))
  ;; Base64 of "hello" is "aGVsbG8="
  (check-true (string-contains? result "aGVsbG8=")))

(test-case "audit-clipboard-mode-off"
  (define old-mode (current-clipboard-mode))
  (current-clipboard-mode 'off)
  (check-equal? (copy-text! "test") 'disabled)
  (current-clipboard-mode old-mode))

(test-case "audit-clipboard-mode-osc52"
  (define old-mode (current-clipboard-mode))
  (current-clipboard-mode 'osc52)
  (define out (open-output-string))
  (check-equal? (copy-text! "test" out) 'ok-osc52)
  (current-clipboard-mode old-mode))

(test-case "audit-clipboard-detect-tool"
  ;; detect-clipboard-tool returns (list path symbol) or #f
  (define tool (detect-clipboard-tool))
  (check-true (or (not tool) (and (list? tool) (= (length tool) 2)))))

;; ---------------------------------------------------------------------------
;; 12. Approval Channel
;; ---------------------------------------------------------------------------

(test-case "audit-approval-make-channel"
  (define ch (make-approval-channel))
  (check-true (approval-channel? ch))
  (check-equal? (approval-channel-timeout-ms ch) DEFAULT-APPROVAL-TIMEOUT-MS))

(test-case "audit-approval-custom-timeout"
  (define ch (make-approval-channel #:timeout-ms 5000))
  (check-equal? (approval-channel-timeout-ms ch) 5000))

(test-case "audit-approval-explicit-headless-policy"
  (set-headless-approval-mode!)
  (check-true (approval-await-result))
  (set-approval-channel! (make-approval-channel))
  (clear-approval-channel!)
  (check-false (approval-await-result)))

(test-case "audit-approval-set-and-clear"
  (clear-approval-channel!)
  (check-false (current-approval-channel))
  (define ch (make-approval-channel))
  (set-approval-channel! ch)
  (check-equal? (current-approval-channel) ch)
  (clear-approval-channel!)
  (check-false (current-approval-channel)))

(test-case "audit-approval-put-and-await"
  (clear-approval-channel!)
  (define ch (make-approval-channel #:timeout-ms 5000))
  (set-approval-channel! ch)
  ;; Put approval result before awaiting
  (approval-put! #t)
  (check-equal? (approval-await-result) #t)
  ;; Test denial
  (approval-put! #f)
  (check-equal? (approval-await-result) #f)
  (clear-approval-channel!))

(test-case "audit-approval-default-timeout"
  (check-true (> DEFAULT-APPROVAL-TIMEOUT-MS 0))
  (check-true (exact-integer? DEFAULT-APPROVAL-TIMEOUT-MS)))

;; ---------------------------------------------------------------------------
;; 13. Cell Buffer
;; ---------------------------------------------------------------------------

(test-case "audit-cell-buffer-create"
  (define buf (make-cell-buffer 80 24))
  (check-true (cell-buffer? buf))
  (check-equal? (cell-buffer-cols buf) 80)
  (check-equal? (cell-buffer-rows buf) 24)
  (check-true (cell-buffer-dirty? buf)))

(test-case "audit-cell-buffer-default-cell"
  (define buf (make-cell-buffer 10 5))
  (define cell (cell-buffer-ref buf 0 0))
  (check-equal? (vector-ref cell 0) #\space) ; default char
  (check-equal? (vector-ref cell 1) 7) ; default fg (white)
  (check-equal? (vector-ref cell 2) 0)) ; default bg (black)

(test-case "audit-cell-buffer-set"
  (define buf (make-cell-buffer 10 5))
  (cell-buffer-set! buf 3 2 #:char #\X #:fg 1 #:bold #t)
  (define cell (cell-buffer-ref buf 3 2))
  (check-equal? (vector-ref cell 0) #\X)
  (check-equal? (vector-ref cell 1) 1)
  (check-true (vector-ref cell 3))) ; bold

(test-case "audit-cell-buffer-clear"
  (define buf (make-cell-buffer 10 5))
  (cell-buffer-set! buf 0 0 #:char #\A)
  (cell-buffer-clear! buf)
  (define cell (cell-buffer-ref buf 0 0))
  (check-equal? (vector-ref cell 0) #\space))

;; ---------------------------------------------------------------------------
;; 14. Frame Diff
;; ---------------------------------------------------------------------------

(test-case "audit-frame-diff-full-redraw"
  (define result (diff-frames #f '("line1")))
  (check-equal? (length result) 1)
  (check-equal? (diff-cmd-type (car result)) 'full))

(test-case "audit-frame-diff-empty-prev"
  (define result (diff-frames '() '("line1")))
  (check-equal? (length result) 1)
  (check-equal? (diff-cmd-type (car result)) 'full))

(test-case "audit-frame-diff-same-length-changed"
  (define prev '("aaa" "bbb" "ccc"))
  (define curr '("aaa" "BBB" "ccc"))
  (define result (diff-frames prev curr))
  (check-equal? (length result) 1)
  (check-equal? (diff-cmd-type (car result)) 'write)
  (check-equal? (diff-cmd-row (car result)) 1)
  (check-equal? (diff-cmd-content (car result)) "BBB"))

(test-case "audit-frame-diff-same-content"
  (define prev '("aaa" "bbb"))
  (define curr '("aaa" "bbb"))
  (define result (diff-frames prev curr))
  (check-equal? result '()))

(test-case "audit-frame-diff-grow"
  (define prev '("aaa" "bbb"))
  (define curr '("aaa" "bbb" "ccc"))
  (define result (diff-frames prev curr))
  ;; Only the new line should be a write
  (check-equal? (length result) 1)
  (check-equal? (diff-cmd-type (car result)) 'write)
  (check-equal? (diff-cmd-row (car result)) 2))

(test-case "audit-frame-diff-shrink"
  (define prev '("aaa" "bbb" "ccc"))
  (define curr '("aaa" "bbb"))
  (define result (diff-frames prev curr))
  ;; Should have a clear-from command at row 2
  (check-true (> (length result) 0))
  (define clear-cmd (findf (lambda (c) (eq? (diff-cmd-type c) 'clear-from)) result))
  (check-not-false clear-cmd)
  (check-equal? (diff-cmd-row clear-cmd) 2))

(test-case "audit-frame-diff-multi-change"
  (define prev '("aaa" "bbb" "ccc" "ddd"))
  (define curr '("AAA" "bbb" "CCC" "ddd"))
  (define result (diff-frames prev curr))
  (check-equal? (length result) 2)
  (check-equal? (diff-cmd-row (car result)) 0)
  (check-equal? (diff-cmd-row (cadr result)) 2))

(test-case "audit-frame-to-string-lines"
  (define lines '("a" "b" "c"))
  (check-equal? (frame->string-lines lines) lines))

;; ---------------------------------------------------------------------------
;; 15. Event Reducer Registry
;; ---------------------------------------------------------------------------

(test-case "audit-event-registry-test-isolation"
  ;; call-with-test-registry provides a fresh hash for test isolation
  (call-with-test-registry (lambda ()
                             (check-false (event-reducer-registered? "test-event"))
                             (register-event-reducer! "test-event"
                                                      (lambda (state evt) (cons 'handled state)))
                             (check-true (event-reducer-registered? "test-event"))))
  ;; After the thunk, the global registry should NOT have our test event
  (check-false (event-reducer-registered? "test-event")))

(test-case "audit-event-registry-apply-no-handler"
  ;; When no handler is registered, state is returned unchanged
  (call-with-test-registry (lambda ()
                             (define state 'test-state)
                             ;; event struct: version ev time session-id turn-id payload
                             (define evt (event 1 'unknown-event 0 #f #f 'data))
                             (check-equal? (apply-event-to-state state evt) state))))

(test-case "audit-event-registry-write-once"
  ;; Registering the same type twice should be a no-op (write-once)
  (call-with-test-registry (lambda ()
                             (register-event-reducer! "test-event" (lambda (s e) 'first))
                             (register-event-reducer! "test-event" (lambda (s e) 'second))
                             ;; First handler should still be active
                             (define state 'initial)
                             ;; event-ev must be a string to match registry keys
                             (define evt (event 1 "test-event" 0 #f #f 'data))
                             (check-equal? (apply-event-to-state state evt) 'first))))

(test-case "audit-event-registry-apply-with-handler"
  (call-with-test-registry (lambda ()
                             (register-event-reducer! "increment" (lambda (state evt) (add1 state)))
                             (define evt (event 1 "increment" 0 #f #f 'data))
                             (check-equal? (apply-event-to-state 41 evt) 42))))
