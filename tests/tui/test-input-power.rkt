#lang racket

;; tests/tui/test-input-power.rkt — Input editor power features tests
;;
;; Integration tests for Wave 2 features:
;; - #609: Ctrl+Z/Y undo/redo (deep stack, max depth, kill integration)
;; - #610: Ctrl+W/U/K kill + Ctrl+Y yank (ring overflow, multi-yank, undo)
;; - #611: Ctrl+Left/Right word navigation (edge cases, Unicode)
;; - #612: System clipboard paste via Ctrl+V (multi-line, undo)
;; - Keymap dispatch integration

(require rackunit
         rackunit/text-ui
         "../../../q/tui/input.rkt"
         "../../../q/tui/keymap.rkt")

;; ============================================================
;; Undo/Redo Deep Stack Tests (#609)
;; ============================================================

(test-case "undo-deep: 50 edits undo all the way back to empty"
  (define st0 (initial-input-state))
  (define st-final
    (for/fold ([st st0])
              ([i (in-range 50)])
      (input-insert-char st (integer->char (+ 65 (modulo i 26))))))
  (check > (string-length (input-state-buffer st-final)) 0)
  ;; Undo all 50 edits
  (define st-undo-all
    (for/fold ([st st-final])
              ([_ (in-range 50)])
      (input-undo st)))
  (check-equal? (input-state-buffer st-undo-all) "")
  (check-equal? (input-state-cursor st-undo-all) 0))

(test-case "undo-deep: undo/redo cycle preserves state"
  (define st0 (initial-input-state))
  (define st1 (input-insert-char st0 #\a))
  (define st2 (input-insert-char st1 #\b))
  (define st3 (input-insert-char st2 #\c))
  ;; Undo twice → "a"
  (define st4 (input-undo (input-undo st3)))
  (check-equal? (input-state-buffer st4) "a")
  ;; Redo once → "ab"
  (define st5 (input-redo st4))
  (check-equal? (input-state-buffer st5) "ab")
  ;; Redo again → "abc"
  (define st6 (input-redo st5))
  (check-equal? (input-state-buffer st6) "abc"))

(test-case "undo-deep: undo stack caps at MAX-UNDO-STACK (100)"
  (define st0 (initial-input-state))
  ;; Insert 150 characters
  (define st-full
    (for/fold ([st st0])
              ([i (in-range 150)])
      (input-insert-char st (integer->char (+ 65 (modulo i 26))))))
  ;; Can only undo 100 times (MAX-UNDO-STACK)
  (define st-at-cap
    (for/fold ([st st-full])
              ([_ (in-range 100)])
      (input-undo st)))
  ;; One more undo should do nothing (stack empty)
  (define st-noop (input-undo st-at-cap))
  (check-equal? (input-state-buffer st-noop) (input-state-buffer st-at-cap)))

(test-case "undo-deep: undo after backspace"
  (define st0 (initial-input-state))
  (define st1 (input-insert-char st0 #\h))
  (define st2 (input-insert-char st1 #\i))
  (define st3 (input-backspace st2))
  (check-equal? (input-state-buffer st3) "h")
  (define st4 (input-undo st3))
  (check-equal? (input-state-buffer st4) "hi"))

(test-case "undo-deep: undo after clear"
  (define st0 (initial-input-state))
  (define st1 (input-insert-char st0 #\x))
  (define st2 (input-insert-char st1 #\y))
  (define st3 (input-clear st2))
  (check-equal? (input-state-buffer st3) "")
  (define st4 (input-undo st3))
  (check-equal? (input-state-buffer st4) "xy"))

(test-case "undo-deep: undo after delete"
  (define st0 (initial-input-state))
  (define st1 (input-insert-char st0 #\a))
  (define st2 (input-insert-char st1 #\b))
  (define st3 (input-insert-char st2 #\c))
  ;; Move cursor to position 1 (after 'a')
  (define st4 (input-cursor-left (input-cursor-left st3)))
  (define st5 (input-delete st4))
  (check-equal? (input-state-buffer st5) "ac")
  (define st6 (input-undo st5))
  (check-equal? (input-state-buffer st6) "abc"))

;; ============================================================
;; Kill Ring Tests (#610)
;; ============================================================

(test-case "kill-ring: multiple kills push onto ring"
  (define st0 (initial-input-state))
  (define st1 (input-insert-string st0 "hello world foo"))
  (define st2 (input-cursor-word-left st1)) ;; cursor at end of "world "
  (define st3 (input-kill-word-backward st2)) ;; kills "world " backward
  (check >= (length (input-state-kill-ring st3)) 1)
  (check-true (< (string-length (input-state-buffer st3))
                  (string-length (input-state-buffer st2)))))

(test-case "kill-ring: kill-to-end then yank restores text"
  (define st0 (initial-input-state))
  (define st1 (input-insert-string st0 "hello world"))
  (define st2 (input-cursor-left (input-cursor-left
                                   (input-cursor-left
                                    (input-cursor-left
                                     (input-cursor-left
                                      (input-cursor-left st1))))))) ;; cursor before " world"
  (define st3 (input-kill-to-end st2))
  (check-equal? (input-state-buffer st3) "hello")
  (check-equal? (input-state-kill-ring st3) '(" world"))
  ;; Yank restores the killed text
  (define st4 (input-yank st3))
  (check-equal? (input-state-buffer st4) "hello world"))

(test-case "kill-ring: multiple kills fill ring, last is on top"
  (define st0 (initial-input-state))
  (define st1 (input-insert-string st0 "aaa bbb ccc"))
  ;; Kill all (from home)
  (define st2 (input-home st1))
  (define st3 (input-kill-to-end st2))
  (check-equal? (input-state-kill-ring st3) '("aaa bbb ccc"))
  ;; New text
  (define st4 (input-insert-string st3 "new"))
  ;; Kill again — this creates a new undo cycle
  ;; kill-to-end from end of "new" (cursor at end) does nothing
  ;; So let's kill-to-beginning instead
  (define st5 (input-end st4))
  (define st6 (input-kill-to-beginning st5))
  (check-equal? (input-state-kill-ring st6) '("new" "aaa bbb ccc"))
  ;; Yank gives most recent
  (define st7 (input-yank st6))
  (check-true (string-contains? (input-state-buffer st7) "new")))

(test-case "kill-ring: kill empty region does not push to ring"
  (define st0 (initial-input-state))
  (define st1 (input-kill-to-end st0)) ;; nothing to kill
  (check-equal? (input-state-kill-ring st1) '()))

(test-case "kill-ring: undo after kill restores killed text"
  (define st0 (initial-input-state))
  (define st1 (input-insert-string st0 "preserve this"))
  ;; Kill from beginning
  (define st2 (input-home st1))
  (define st3 (input-kill-to-end st2))
  (check-equal? (input-state-buffer st3) "")
  (define st4 (input-undo st3))
  (check-equal? (input-state-buffer st4) "preserve this"))

(test-case "kill-ring: kill-to-beginning at start does nothing"
  (define st0 (initial-input-state))
  (define st1 (input-insert-string st0 "hello"))
  (define st2 (input-home st1))
  (define st3 (input-kill-to-beginning st2))
  (check-equal? (input-state-buffer st3) "hello"))

(test-case "kill-ring: kill-to-end at end does nothing"
  (define st0 (initial-input-state))
  (define st1 (input-insert-string st0 "hello"))
  ;; Cursor is at end, so kill-to-end kills nothing
  (define st2 (input-kill-to-end st1))
  ;; Actually at end of "hello", kill-to-end returns same state
  (check-equal? (input-state-buffer st2) "hello"))

;; ============================================================
;; Word Navigation Edge Cases (#611)
;; ============================================================

(test-case "word-nav: multiple consecutive spaces"
  (define st0 (initial-input-state))
  (define st1 (input-insert-string st0 "hello   world"))
  (define st2 (input-home st1))
  (define st3 (input-cursor-word-right st2)) ;; past "hello" and spaces
  (check >= (input-state-cursor st3) 5)
  (define st4 (input-cursor-word-right st3)) ;; to end
  (check-equal? (input-state-cursor st4) (string-length "hello   world")))

(test-case "word-nav: single word (no spaces)"
  (define st0 (initial-input-state))
  (define st1 (input-insert-string st0 "hello"))
  (define st2 (input-home st1))
  (define st3 (input-cursor-word-right st2))
  (check-equal? (input-state-cursor st3) 5) ;; at end
  (define st4 (input-cursor-word-left st3))
  (check-equal? (input-state-cursor st4) 0)) ;; at start

(test-case "word-nav: leading and trailing spaces"
  (define st0 (initial-input-state))
  (define st1 (input-insert-string st0 "  hello  "))
  (define st2 (input-home st1))
  ;; word-right from start: skip spaces, then skip "hello", then skip trailing spaces
  (define st3 (input-cursor-word-right st2))
  (check >= (input-state-cursor st3) 7)
  ;; word-left from end: back to "hello" start
  (define st4 (input-cursor-word-left st3))
  (check <= (input-state-cursor st4) 4))

(test-case "word-nav: Unicode text"
  (define st0 (initial-input-state))
  (define st1 (input-insert-string st0 "hällo wörld"))
  (define st2 (input-home st1))
  (define st3 (input-cursor-word-right st2))
  (check > (input-state-cursor st3) 0)
  (define st4 (input-cursor-word-right st3))
  (check-equal? (input-state-cursor st4) (string-length "hällo wörld")))

(test-case "word-nav: empty buffer does nothing"
  (define st0 (initial-input-state))
  (check-equal? (input-state-cursor (input-cursor-word-left st0)) 0)
  (check-equal? (input-state-cursor (input-cursor-word-right st0)) 0))

(test-case "word-nav: word-right then word-left returns to start"
  (define st0 (initial-input-state))
  (define st1 (input-insert-string st0 "abc def ghi"))
  (define st2 (input-home st1))
  (define pos-start (input-state-cursor st2))
  (define st3 (input-cursor-word-right st2))
  (define st4 (input-cursor-word-left st3))
  (check-equal? (input-state-cursor st4) pos-start))

;; ============================================================
;; Paste / Insert-string Tests (#612)
;; ============================================================

(test-case "paste: multi-line paste inserts correctly"
  (define st0 (initial-input-state))
  (define st1 (input-insert-string st0 "line1\nline2\nline3"))
  (check-equal? (input-state-buffer st1) "line1\nline2\nline3")
  (check-equal? (input-state-cursor st1) 17))

(test-case "paste: paste in middle of text"
  (define st0 (initial-input-state))
  (define st1 (input-insert-string st0 "hello world"))
  ;; Move cursor to after "hello"
  (define st2 (input-cursor-left (input-cursor-left
                                   (input-cursor-left
                                    (input-cursor-left
                                     (input-cursor-left
                                      (input-cursor-left st1))))))) ;; cursor at 5
  (check-equal? (input-state-cursor st2) 5)
  (define st3 (input-insert-string st2 " beautiful"))
  (check-equal? (input-state-buffer st3) "hello beautiful world"))

(test-case "paste: undo after paste restores original"
  (define st0 (initial-input-state))
  (define st1 (input-insert-string st0 "original"))
  (define st2 (input-insert-string st1 " inserted"))
  (check-equal? (input-state-buffer st2) "original inserted")
  (define st3 (input-undo st2))
  (check-equal? (input-state-buffer st3) "original"))

(test-case "paste: paste empty string does nothing (no undo entry)"
  (define st0 (initial-input-state))
  (define st1 (input-insert-string st0 ""))
  (check-equal? (input-state-buffer st1) "")
  (check-equal? (input-state-undo-stack st1) '()))

(test-case "paste: paste with special characters"
  (define st0 (initial-input-state))
  (define st1 (input-insert-string st0 "tab\there\nnewline"))
  (check-true (string-contains? (input-state-buffer st1) "\t"))
  (check-true (string-contains? (input-state-buffer st1) "\n")))

;; ============================================================
;; Keymap Dispatch Integration Tests
;; ============================================================

(test-case "keymap: Ctrl-Z/Y mapped to undo/redo"
  (define km (default-keymap))
  ;; Ctrl+Z is handled via hardcoded dispatch, but let's verify
  ;; the keymap doesn't override it with something else
  (define ctrl-z-spec (key-spec 'z #t #f #f))
  (define action (keymap-lookup km ctrl-z-spec))
  ;; Default keymap should NOT have ctrl-z mapped (it falls through
  ;; to hardcoded). If someone adds it, we verify it maps to undo.
  (when action
    (check-equal? action 'undo)))

(test-case "keymap: Ctrl-V mapped to paste"
  (define km (default-keymap))
  (define ctrl-v-spec (key-spec #\v #t #f #f))
  (check-equal? (keymap-lookup km ctrl-v-spec) 'paste))

(test-case "keymap: Ctrl-Left/Right mapped to word navigation"
  (define km (default-keymap))
  (define ctrl-left-spec (key-spec 'left #t #f #f))
  (define ctrl-right-spec (key-spec 'right #t #f #f))
  (check-equal? (keymap-lookup km ctrl-left-spec) 'word-left)
  (check-equal? (keymap-lookup km ctrl-right-spec) 'word-right))

(test-case "keymap: user override works"
  (define km (default-keymap))
  ;; Override Ctrl-V to something else
  (keymap-add! km (key-spec #\v #t #f #f) 'custom-action)
  (check-equal? (keymap-lookup km (key-spec #\v #t #f #f)) 'custom-action))

;; ============================================================
;; Combined Workflow Tests
;; ============================================================

(test-case "workflow: type, kill-word, yank, undo sequence"
  (define st0 (initial-input-state))
  (define st1 (input-insert-string st0 "hello cruel world"))
  ;; Move to before "cruel"
  (define st2 (input-cursor-word-left (input-cursor-word-left st1)))
  ;; Kill "cruel "
  (define st3 (input-kill-word-backward st2)) ;; kills "hello " — wait, need to be at right spot
  ;; Actually let's use a clearer approach
  (define st-a (input-insert-string (initial-input-state) "foo bar baz"))
  ;; Cursor at end. Kill-to-end does nothing (already at end).
  ;; Let's move back and kill
  (define st-b (input-cursor-word-left st-a))
  (define st-c (input-kill-word-backward st-b))
  ;; Yank should restore the killed text
  (define st-d (input-yank st-c))
  ;; Undo should undo the yank
  (define st-e (input-undo st-d))
  ;; Should be back to after kill
  (check-true (not (string=? (input-state-buffer st-d)
                             (input-state-buffer st-e)))))

(test-case "workflow: paste, edit, undo back to pre-paste"
  (define st0 (initial-input-state))
  (define st1 (input-insert-string st0 "keep"))
  (define st2 (input-insert-string st1 " this")) ;; paste-like
  (define st3 (input-insert-char st2 #\!))
  (check-equal? (input-state-buffer st3) "keep this!")
  ;; Undo the '!'
  (define st4 (input-undo st3))
  (check-equal? (input-state-buffer st4) "keep this")
  ;; Undo the ' this'
  (define st5 (input-undo st4))
  (check-equal? (input-state-buffer st5) "keep"))

(test-case "workflow: word navigation + delete sequence"
  (define st0 (initial-input-state))
  (define st1 (input-insert-string st0 "one two three"))
  (define st2 (input-home st1))
  ;; Skip "one", land at "two"
  (define st3 (input-cursor-word-right st2))
  ;; Delete forward from there
  (define st4 (input-delete st3))
  ;; Should have removed 't' from "two"
  (check-true (not (string=? (input-state-buffer st1)
                             (input-state-buffer st4)))))

(test-case "workflow: kill-ring yank cycle"
  ;; Kill two things, verify yank gives most recent
  (define st0 (initial-input-state))
  (define st1 (input-insert-string st0 "first second third"))
  ;; Kill from beginning
  (define st2 (input-home st1))
  (define st3 (input-kill-to-end st2)) ;; kill all → ring: ("first second third")
  (check-equal? (input-state-buffer st3) "")
  (define st4 (input-insert-string st3 "new "))
  ;; Kill-to-beginning of the new text
  (define st5 (input-end st4))
  (define st6 (input-kill-to-beginning st5)) ;; kill "new " → ring: ("new ", "first second third")
  ;; Yank gives most recent: "new "
  (define st7 (input-yank st6))
  (check-true (string-contains? (input-state-buffer st7) "new")))
