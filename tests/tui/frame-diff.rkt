#lang racket/base

(require rackunit
         "../../tui/frame-diff.rkt")

(define empty-frame '())
(define frame-3 '("line0" "line1" "line2"))
(define frame-3-mod '("line0" "CHANGED" "line2"))
(define frame-3-all-mod '("a" "b" "c"))
(define frame-4 '("line0" "line1" "line2" "line3"))

;; Empty prev → full redraw
(check-equal? (diff-frames empty-frame frame-3)
              (list (diff-cmd 'full 0 #f))
              "empty prev yields full redraw")

;; Same frame → empty diffs
(check-equal? (diff-frames frame-3 frame-3) '() "identical frames yield no diffs")

;; One line changed → one write cmd
(check-equal? (diff-frames frame-3 frame-3-mod)
              (list (diff-cmd 'write 1 "CHANGED"))
              "single changed line yields one write cmd")

;; Multiple lines changed → multiple write cmds in order
(check-equal? (diff-frames frame-3 frame-3-all-mod)
              (list (diff-cmd 'write 0 "a") (diff-cmd 'write 1 "b") (diff-cmd 'write 2 "c"))
              "all lines changed yields write cmds for all rows")

;; Different length (curr longer) — incremental append
(check-equal? (diff-frames frame-3 frame-4)
              (list (diff-cmd 'write 3 "line3"))
              "longer frame yields append write cmd")

;; Different length (curr shorter) — incremental clear
(check-equal? (diff-frames frame-4 frame-3)
              (list (diff-cmd 'clear-from 3 #f))
              "shorter frame yields clear-from cmd")

;; frame->string-lines is identity
(check-equal? (frame->string-lines frame-3) frame-3 "frame->string-lines returns the frame unchanged")

;; #f prev (first render) → full redraw (Issue #390)
(check-equal? (diff-frames #f frame-3)
              (list (diff-cmd 'full 0 #f))
              "#f prev yields full redraw (first render)")
(check-equal? (diff-frames #f empty-frame)
              (list (diff-cmd 'full 0 #f))
              "#f prev with empty current yields full redraw")

;; ──────────────────────────────
;; Incremental resize tests (#466)
;; ──────────────────────────────

;; Append + change in common region
(define frame-3-mod-append '("line0" "CHANGED" "line2" "extra"))
(check-equal? (diff-frames frame-3 frame-3-mod-append)
              (list (diff-cmd 'write 1 "CHANGED")
                    (diff-cmd 'write 3 "extra"))
              "changed + appended yields write for both")

;; Shrink with change in common region
(define frame-4-mod-shrink '("line0" "DIFF" "line2"))
(check-equal? (diff-frames frame-4 frame-4-mod-shrink)
              (list (diff-cmd 'write 1 "DIFF")
                    (diff-cmd 'clear-from 3 #f))
              "changed + shrunk yields write + clear-from")

;; Grow by multiple lines
(define frame-6 '("a" "b" "c" "d" "e" "f"))
(check-equal? (diff-frames frame-3 frame-6)
              (list (diff-cmd 'write 0 "a")
                    (diff-cmd 'write 1 "b")
                    (diff-cmd 'write 2 "c")
                    (diff-cmd 'write 3 "d")
                    (diff-cmd 'write 4 "e")
                    (diff-cmd 'write 5 "f"))
              "grow by multiple lines yields all writes")

;; ──────────────────────────────
;; ANSI content in frames (#488)
;; ──────────────────────────────

;; Style-only changes: same text, different ANSI codes → detected as changed
(define ansi-frame-a '("\x1b[1;31merror\x1b[0m" "plain" "\x1b[36minfo\x1b[0m"))
(define ansi-frame-b '("\x1b[1;32merror\x1b[0m" "plain" "\x1b[36minfo\x1b[0m"))

(check-equal? (diff-frames ansi-frame-a ansi-frame-b)
              (list (diff-cmd 'write 0 "\x1b[1;32merror\x1b[0m"))
              "style-only change detected (red→green)")

;; ANSI frames identical → no diff
(check-equal? (diff-frames ansi-frame-a ansi-frame-a)
              '()
              "identical ANSI frames yield no diff")

;; Bright-color ANSI content
(define ansi-bright '("\x1b[90mmuted\x1b[0m"))
(define ansi-plain '("muted"))
(check-equal? (diff-frames ansi-bright ansi-plain)
              (list (diff-cmd 'write 0 "muted"))
              "bright-color ANSI vs plain text detected")
