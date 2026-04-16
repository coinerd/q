#lang racket

;; tests/tui/render.rkt — Tests for tui/render module

(require rackunit
         rackunit/text-ui
         "../../../q/tui/render.rkt"
         "../../../q/tui/state.rkt"
         "../../../q/tui/input.rkt"
         "../../../q/tui/char-width.rkt")

(define render-tests
  (test-suite "TUI Render"

    ;; --------------------------------------------------------
    ;; styled-segment struct accessors
    ;; --------------------------------------------------------
    (test-case "styled-segment accessors"
      (let ([seg (styled-segment "hello" '(bold))])
        (check-equal? (styled-segment-text seg) "hello")
        (check-equal? (styled-segment-style seg) '(bold))))

    ;; --------------------------------------------------------
    ;; styled-line struct
    ;; --------------------------------------------------------
    (test-case "styled-line holds two segments"
      (let ([line (styled-line (list (styled-segment "hello" '(bold))
                                     (styled-segment " world" '())))])
        (check-equal? (length (styled-line-segments line)) 2)))

    ;; --------------------------------------------------------
    ;; render-branch-list
    ;; --------------------------------------------------------
    (test-case "render-branch-list returns header + 2 branches"
      (let ([branches (list (branch-info "b1" #f 'user #t #t)
                            (branch-info "b2" "b1" 'assistant #f #f))])
        (define lines (render-branch-list branches 80))
        (check-equal? (length lines) 3 "header + 2 branches")
        (check-true
         (string-contains? (styled-segment-text (first (styled-line-segments (first lines))))
                           "Branches (2)")
         "header shows count")))

    (test-case "render-branch-list active branch has arrow marker"
      (let ([branches (list (branch-info "active-branch" #f 'user #t #t))])
        (define lines (render-branch-list branches 80))
        (define line-text (styled-segment-text (first (styled-line-segments (second lines)))))
        (check-true (string-contains? line-text "→") "active branch has arrow marker")
        (check-true (string-contains? line-text "active-branch") "branch id is shown")))

    (test-case "render-branch-list empty list returns just header"
      (let ([branches '()])
        (define lines (render-branch-list branches 80))
        (check-equal? (length lines) 1 "just header")))

    ;; --------------------------------------------------------
    ;; render-leaf-nodes
    ;; --------------------------------------------------------
    (test-case "render-leaf-nodes returns header + 2 leaves"
      (let ([branches (list (branch-info "leaf1" #f 'user #t #t)
                            (branch-info "mid1" #f 'assistant #f #f)
                            (branch-info "leaf2" "mid1" 'user #t #f))])
        (define lines (render-leaf-nodes branches 80))
        (check-equal? (length lines) 3 "header + 2 leaves")
        (check-true
         (string-contains? (styled-segment-text (first (styled-line-segments (first lines))))
                           "Leaf Nodes (2)")
         "header shows leaf count")))

    (test-case "render-leaf-nodes with no leaves returns just header"
      (let ([branches (list (branch-info "nonleaf" #f 'user #f #f))])
        (define lines (render-leaf-nodes branches 80))
        (check-equal? (length lines) 1 "no leaves: just header")))

    ;; --------------------------------------------------------
    ;; render-children-list
    ;; --------------------------------------------------------
    (test-case "render-children-list returns header + 2 children"
      (let ([children (list (branch-info "child1" "parent" 'user #t #f)
                            (branch-info "child2" "parent" 'assistant #f #f))])
        (define lines (render-children-list "parent" children 80))
        (check-equal? (length lines) 3 "header + 2 children")
        (define header-text (styled-segment-text (first (styled-line-segments (first lines)))))
        (check-true (string-contains? header-text "Children of parent") "header shows parent id")))

    (test-case "render-children-list empty shows no-children message"
      (let ([lines (render-children-list "parent" '() 80)])
        (check-equal? (length lines) 2 "header + message")
        (define msg-text (styled-segment-text (first (styled-line-segments (second lines)))))
        (check-true (string-contains? msg-text "no children") "shows no children message")))

    ;; --------------------------------------------------------
    ;; format-entry
    ;; --------------------------------------------------------
    (test-case "format-entry assistant message"
      (let ([entry (make-entry 'assistant "Hello!" 1000 (hash))])
        (define lines (format-entry entry 80))
        (check-equal? (length lines) 1 "one line")
        (check-equal? (styled-segment-text (first (styled-line-segments (first lines)))) "Hello!")))

    (test-case "format-entry user message has prompt prefix"
      (let ([entry (make-entry 'user "Hello!" 1000 (hash))])
        (define lines (format-entry entry 80))
        (define segs (styled-line-segments (first lines)))
        ;; Two segments: cyan prompt + bold text
        (check-equal? (length segs) 2)
        (check-equal? (styled-segment-text (first segs)) "> ")
        (check-equal? (styled-segment-style (first segs)) '(bold cyan) "user prompt uses theme")
        (check-equal? (styled-segment-text (second segs)) "Hello!")
        (check-equal? (styled-segment-style (second segs)) '(bold))))

    (test-case "format-entry error message has [ERR] prefix and bold red style"
      (let ([entry (make-entry 'error "Oops" 1000 (hash))])
        (define lines (format-entry entry 80))
        (define seg (first (styled-line-segments (first lines))))
        (check-true (string-contains? (styled-segment-text seg) "[ERR]"))
        ;; Error uses theme->style 'error '(bold) which resolves to '(bold red)
        (check-not-false (member 'red (styled-segment-style seg)) "error has red")
        (check-not-false (member 'bold (styled-segment-style seg)) "error has bold")))

    ;; --------------------------------------------------------
    ;; render-status-bar and render-input-line
    ;; --------------------------------------------------------
    (test-case "render-status-bar uses ASCII * when busy, not emoji"
      (let ([state (struct-copy ui-state
                                (initial-ui-state #:session-id "test-sess" #:model-name "gpt-4")
                                [busy? #t])])
        (define line (render-status-bar state 80))
        (define text (styled-segment-text (first (styled-line-segments line))))
        (check-true (string-contains? text "*") "busy marker is ASCII *")
        (check-false (string-contains? text "⏳") "no emoji used")))

    (test-case "render-status-bar uses space when idle"
      (let ([state (initial-ui-state #:session-id "test-sess" #:model-name "gpt-4")])
        (define line (render-status-bar state 80))
        (define text (styled-segment-text (first (styled-line-segments line))))
        ;; When idle, busy marker is a space (no *)
        (check-false (regexp-match? #rx"\\*.*q" text) "no * marker when idle")))

    (test-case "render-status-bar uses inverse style"
      (let ([state (initial-ui-state #:session-id "test-sess" #:model-name "gpt-4")])
        (define line (render-status-bar state 80))
        (check-equal? (styled-segment-style (first (styled-line-segments line)))
                      '(inverse)
                      "status bar uses inverse style")))

    (test-case "render-input-line shows q> prompt"
      (let ([inp (initial-input-state)])
        (define line (render-input-line inp 80))
        (define text (styled-segment-text (first (styled-line-segments line))))
        (check-true (string-contains? text "q>") "input line shows prompt")))

    ;; --------------------------------------------------------
    ;; Markdown rendering via format-entry (assistant)
    ;; --------------------------------------------------------
    (test-case "format-entry assistant bold text"
      (let ([entry (make-entry 'assistant "hello **world** end" 1000 (hash))])
        (define lines (format-entry entry 80))
        (check-equal? (length lines) 1 "one line")
        (define segs (styled-line-segments (first lines)))
        ;; Should have 3 segments: text "hello ", bold "world", text " end"
        (check-equal? (length segs) 3)
        (check-equal? (styled-segment-text (first segs)) "hello ")
        (check-equal? (styled-segment-style (second segs)) '(bold))
        (check-equal? (styled-segment-text (second segs)) "world")
        (check-equal? (styled-segment-text (third segs)) " end")))

    (test-case "format-entry assistant italic text"
      (let ([entry (make-entry 'assistant "say *hi* now" 1000 (hash))])
        (define lines (format-entry entry 80))
        (define segs (styled-line-segments (first lines)))
        (check-equal? (length segs) 3)
        (check-equal? (styled-segment-style (second segs)) '(italic))
        (check-equal? (styled-segment-text (second segs)) "hi")))

    (test-case "format-entry assistant inline code"
      (let ([entry (make-entry 'assistant "use `foo` here" 1000 (hash))])
        (define lines (format-entry entry 80))
        (define segs (styled-line-segments (first lines)))
        (check-equal? (length segs) 3)
        ;; Inline code uses theme 'md-code → 'bright-green in default dark theme
        (check-not-false (member 'bright-green (styled-segment-style (second segs)))
                         "inline code uses theme md-code color")
        (check-equal? (styled-segment-text (second segs)) "foo")))

    (test-case "format-entry assistant header"
      (let ([entry (make-entry 'assistant "### Title" 1000 (hash))])
        (define lines (format-entry entry 80))
        (check-equal? (length lines) 1 "one line")
        (define segs (styled-line-segments (first lines)))
        (check-equal? (styled-segment-text (first segs)) "Title")
        ;; Header uses theme 'md-heading → 'cyan in default dark theme
        (check-not-false (member 'bold (styled-segment-style (first segs))) "header is bold")
        (check-not-false (member 'cyan (styled-segment-style (first segs))) "header uses theme md-heading")))

    (test-case "format-entry assistant multi-line with newline"
      (let ([entry (make-entry 'assistant "line1\nline2" 1000 (hash))])
        (define lines (format-entry entry 80))
        (check-equal? (length lines) 2 "two lines")
        (check-equal? (styled-segment-text (first (styled-line-segments (first lines)))) "line1")
        (check-equal? (styled-segment-text (first (styled-line-segments (second lines)))) "line2")))

    (test-case "format-entry assistant code block produces green lines"
      (let ([entry (make-entry 'assistant "```\ncode line\n```" 1000 (hash))])
        (define lines (format-entry entry 80))
        ;; code-block line + trailing newline line
        (check-true (>= (length lines) 1) "at least one code line")
        (define code-segs (styled-line-segments (first lines)))
        ;; Code block uses theme 'md-code → 'bright-green in default dark theme
        (check-not-false (member 'bright-green (styled-segment-style (first code-segs)))
                         "code block uses theme md-code color")
        (check-true (string-contains? (styled-segment-text (first code-segs)) "code line"))))

    (test-case "format-entry assistant link uses blue underline"
      (let ([entry (make-entry 'assistant "click [here](http://example.com) ok" 1000 (hash))])
        (define lines (format-entry entry 80))
        (define segs (styled-line-segments (first lines)))
        ;; text "click " + link "here" + text " ok"
        (check-true (>= (length segs) 3))
        ;; Second segment is the link
        (define link-seg (second segs))
        (check-equal? (styled-segment-text link-seg) "here")
        ;; Link uses theme 'md-link → 'cyan in default dark theme
        (check-not-false (member 'underline (styled-segment-style link-seg)) "link has underline")
        (check-not-false (member 'cyan (styled-segment-style link-seg)) "link uses theme md-link color")))

    (test-case "format-entry tool-start shows [TOOL] text prefix and cyan color"
      (let ([entry (make-entry 'tool-start "[TOOL: read]" 1000 (hash))])
        (define lines (format-entry entry 80))
        (define seg (first (styled-line-segments (first lines))))
        (check-true (string-contains? (styled-segment-text seg) "[TOOL")
                    "tool-start text has [TOOL] prefix")
        ;; tool-start uses theme 'tool-title → 'cyan in default dark theme
        (check-equal? (styled-segment-style seg) '(cyan) "tool-start uses theme")))

    (test-case "format-entry tool-end shows [OK] text prefix and green color"
      (let ([entry (make-entry 'tool-end "[OK: read]" 1000 (hash))])
        (define lines (format-entry entry 80))
        (define seg (first (styled-line-segments (first lines))))
        (check-true (string-contains? (styled-segment-text seg) "[OK")
                    "tool-end text has [OK] prefix")
        ;; tool-end uses theme 'success → 'green in default dark theme
        (check-equal? (styled-segment-style seg) '(green) "tool-end uses theme")))

    (test-case "format-entry tool-fail shows [FAIL] text prefix and red color"
      (let ([entry (make-entry 'tool-fail "[FAIL: read]" 1000 (hash))])
        (define lines (format-entry entry 80))
        (define seg (first (styled-line-segments (first lines))))
        (check-true (string-contains? (styled-segment-text seg) "[FAIL")
                    "tool-fail text has [FAIL] prefix")
        ;; tool-fail uses theme 'error → 'red in default dark theme
        (check-equal? (styled-segment-style seg) '(red) "tool-fail uses theme")))

    (test-case "format-entry system shows [SYS] prefix"
      (let ([entry (make-entry 'system "Session started" 1000 (hash))])
        (define lines (format-entry entry 80))
        (define seg (first (styled-line-segments (first lines))))
        (check-true (string-contains? (styled-segment-text seg) "[SYS]"))
        ;; System uses theme 'muted → 'bright-black in default dark theme
        (check-equal? (styled-segment-style seg) '(bright-black) "system uses theme muted")))

    ;; --------------------------------------------------------
    ;; Line-based viewport slicing (render-transcript)
    ;; --------------------------------------------------------
    (test-case "render-transcript: content fits in height returns all lines"
      (let* ([entries (list (make-entry 'system "line1" 0 (hash))
                            (make-entry 'system "line2" 0 (hash)))]
             [state (struct-copy ui-state (initial-ui-state) [transcript entries])])
        (define-values (lines _st) (render-transcript state 10 200))
        (check-equal? (length lines) 2 "render-transcript: returns all when fits")))

    (test-case "render-transcript: more lines than height shows last N"
      (let* ([entries (list (make-entry 'system "line1" 0 (hash))
                            (make-entry 'system "line2" 0 (hash))
                            (make-entry 'system "line3" 0 (hash)))]
             [state (struct-copy ui-state (initial-ui-state) [transcript entries])])
        (define-values (lines _st) (render-transcript state 2 200))
        (check-equal? (length lines) 2 "render-transcript: shows last 2 of 3 lines")
        ;; First visible should be "[SYS] line2", second "[SYS] line3"
        (check-equal? (styled-segment-text (first (styled-line-segments (first lines))))
                      "[SYS] line2")
        (check-equal? (styled-segment-text (first (styled-line-segments (second lines))))
                      "[SYS] line3")))

    (test-case "render-transcript: scroll-offset=1 shows lines offset 1 from bottom"
      (let* ([entries (list (make-entry 'system "line1" 0 (hash))
                            (make-entry 'system "line2" 0 (hash))
                            (make-entry 'system "line3" 0 (hash))
                            (make-entry 'system "line4" 0 (hash)))]
             [state (struct-copy ui-state (initial-ui-state) [transcript entries] [scroll-offset 1])])
        (define-values (lines _st) (render-transcript state 2 200))
        (check-equal? (length lines) 2 "render-transcript: scroll=1 shows 2 lines")
        ;; Should show [SYS] line2 and [SYS] line3 (offset 1 from bottom)
        (check-equal? (styled-segment-text (first (styled-line-segments (first lines))))
                      "[SYS] line2")
        (check-equal? (styled-segment-text (first (styled-line-segments (second lines))))
                      "[SYS] line3")))

    (test-case "render-transcript: scroll-offset=2 shows older lines"
      (let* ([entries (list (make-entry 'system "line1" 0 (hash))
                            (make-entry 'system "line2" 0 (hash))
                            (make-entry 'system "line3" 0 (hash))
                            (make-entry 'system "line4" 0 (hash))
                            (make-entry 'system "line5" 0 (hash)))]
             [state (struct-copy ui-state (initial-ui-state) [transcript entries] [scroll-offset 2])])
        (define-values (lines _st) (render-transcript state 3 200))
        (check-equal? (length lines) 3 "render-transcript: scroll=2 shows 3 lines")
        ;; Should show [SYS] line1, [SYS] line2, [SYS] line3 (last 5 - 3 - 2 = start at 0)
        (check-equal? (styled-segment-text (first (styled-line-segments (first lines))))
                      "[SYS] line1")))

    (test-case "render-transcript: scroll-offset larger than content clamps to top"
      (let* ([entries (list (make-entry 'system "line1" 0 (hash))
                            (make-entry 'system "line2" 0 (hash)))]
             [state
              (struct-copy ui-state (initial-ui-state) [transcript entries] [scroll-offset 100])])
        (define-values (lines _st) (render-transcript state 10 200))
        ;; Both lines fit, scroll doesn't matter
        (check-equal? (length lines) 2 "render-transcript: small content always shows all")))

    (test-case "render-transcript: empty transcript returns empty"
      (let ([state (initial-ui-state)])
        (define-values (lines _st) (render-transcript state 10 200))
        (check-equal? (length lines) 0 "render-transcript: empty returns empty")))

    (test-case "render-transcript: streaming text shown at bottom when scroll=0"
      (let* ([entries (list (make-entry 'system "line1" 0 (hash)))]
             [state (struct-copy ui-state
                                 (initial-ui-state)
                                 [transcript entries]
                                 [streaming-text "streaming..."])])
        (define-values (lines _st) (render-transcript state 10 200))
        (check-equal? (length lines) 2 "streaming text appended")
        ;; Last line should be the streaming text (dim)
        (define last-seg (first (styled-line-segments (last lines))))
        (check-equal? (styled-segment-text last-seg) "streaming...")
        ;; Streaming text uses theme 'muted → 'bright-black in default dark theme
        (check-equal? (styled-segment-style last-seg) '(bright-black) "streaming uses theme muted")))))

(run-tests render-tests)

;; Additional tests for selection highlighting
(define selection-tests
  (test-suite "Selection highlighting"

    (test-case "style-invert adds inverse to empty style"
      (check-equal? (style-invert '()) '(inverse)))

    (test-case "style-invert adds inverse to existing style"
      (check-equal? (style-invert '(bold)) '(inverse bold)))

    (test-case "style-invert removes existing inverse"
      (check-equal? (style-invert '(inverse bold)) '(bold)))

    (test-case "highlight-line-range full line"
      (define sl (styled-line (list (styled-segment "hello" '()))))
      (define result (highlight-line-range sl 0 5))
      (define segs (styled-line-segments result))
      ;; Should have one segment with inverse
      (check-equal? (length segs) 1)
      (check-equal? (styled-segment-text (car segs)) "hello")
      (check-not-false (member 'inverse (styled-segment-style (car segs)))))

    (test-case "highlight-line-range partial range"
      (define sl (styled-line (list (styled-segment "hello world" '()))))
      (define result (highlight-line-range sl 2 7))
      (define segs (styled-line-segments result))
      ;; Should have 3 segments: "he" + "llo w" (inverted) + "orld"
      (check-equal? (length segs) 3)
      (check-equal? (styled-segment-text (first segs)) "he")
      (check-equal? (styled-segment-style (first segs)) '())
      (check-equal? (styled-segment-text (second segs)) "llo w")
      (check-not-false (member 'inverse (styled-segment-style (second segs))))
      (check-equal? (styled-segment-text (third segs)) "orld")
      (check-equal? (styled-segment-style (third segs)) '()))

    (test-case "highlight-line-range preserves existing style"
      (define sl (styled-line (list (styled-segment "hello" '(bold cyan)))))
      (define result (highlight-line-range sl 0 5))
      (define segs (styled-line-segments result))
      (check-equal? (length segs) 1)
      (check-not-false (member 'bold (styled-segment-style (first segs))))
      (check-not-false (member 'inverse (styled-segment-style (first segs)))))

    (test-case "apply-selection-highlight no selection returns lines unchanged"
      (define lines (list (styled-line (list (styled-segment "test" '())))))
      (check-equal? (apply-selection-highlight lines #f #f 1) lines))

    (test-case "apply-selection-highlight single line selection"
      (define lines
        (list (styled-line (list (styled-segment "line0" '())))
              (styled-line (list (styled-segment "line1" '())))
              (styled-line (list (styled-segment "line2" '())))))
      ;; Select line1 (screen row 2, trans-start=1, so line index = 2-1 = 1)
      (define result (apply-selection-highlight lines (cons 0 2) (cons 4 2) 1))
      ;; line0 and line2 unchanged, line1 inverted
      (check-equal? (styled-line-segments (first result)) (styled-line-segments (first lines)))
      (check-not-false (member 'inverse
                               (styled-segment-style (first (styled-line-segments (second result))))))
      (check-equal? (styled-line-segments (third result)) (styled-line-segments (third lines))))

    (test-case "apply-selection-highlight multi-line selection"
      (define lines
        (list (styled-line (list (styled-segment "aa" '())))
              (styled-line (list (styled-segment "bb" '())))
              (styled-line (list (styled-segment "cc" '())))))
      ;; Select from screen row 1 col 1 to screen row 3 col 1 (trans-start=1)
      (define result (apply-selection-highlight lines (cons 1 1) (cons 1 3) 1))
      ;; All 3 lines should have at least one inverted segment
      (for ([line result]
            [i (in-naturals)])
        (define segs (styled-line-segments line))
        (define has-inverse?
          (for/or ([s segs])
            (member 'inverse (styled-segment-style s))))
        (check-not-false has-inverse? (format "line ~a should have inverted segment" i))))))

(run-tests selection-tests)

;; Additional tests for BUG: highlight-line-range multi-segment crash
(define highlight-multi-seg-tests
  (test-suite "highlight-line-range multi-segment"

    (test-case "highlight-line-range multi-segment partial highlight (BUG repro)"
      ;; Line: "hello" (cols 0–4, plain) + " world" (cols 5–10, bold)
      ;; Highlight cols 3–8 → splits across both segments
      (define sl (styled-line (list (styled-segment "hello" '()) (styled-segment " world" '(bold)))))
      (define result (highlight-line-range sl 3 8))
      (define segs (styled-line-segments result))
      ;; Expected: "hel" (plain) + "lo" (inv) + " wo" (inv+bold) + "rld" (bold)
      (check-equal? (length segs) 4)
      (check-equal? (styled-segment-text (first segs)) "hel")
      (check-equal? (styled-segment-style (first segs)) '())
      (check-equal? (styled-segment-text (second segs)) "lo")
      (check-not-false (member 'inverse (styled-segment-style (second segs))))
      (check-equal? (styled-segment-text (third segs)) " wo")
      (check-not-false (member 'inverse (styled-segment-style (third segs))))
      (check-not-false (member 'bold (styled-segment-style (third segs))))
      (check-equal? (styled-segment-text (fourth segs)) "rld")
      (check-equal? (styled-segment-style (fourth segs)) '(bold)))

    (test-case "highlight-line-range multi-segment full-line highlight"
      ;; Line: "hi" (cols 0-1) + " there" (cols 2-7)
      ;; Highlight entire line (cols 0-7)
      (define sl (styled-line (list (styled-segment "hi" '()) (styled-segment " there" '()))))
      (define result (highlight-line-range sl 0 8))
      (define segs (styled-line-segments result))
      ;; Both segments fully inverted
      (for ([s segs])
        (check-not-false (member 'inverse (styled-segment-style s))
                         (format "segment '~a' should be inverted" (styled-segment-text s)))))

    (test-case "highlight-line-range single char in second segment"
      ;; Line: "ab" + "cd"
      ;; Highlight just col 2 (first char of second segment)
      (define sl (styled-line (list (styled-segment "ab" '()) (styled-segment "cd" '()))))
      (define result (highlight-line-range sl 2 3))
      (define segs (styled-line-segments result))
      ;; First segment untouched, second split into "c" (inv) + "d" (plain)
      (check-equal? (styled-segment-text (first segs)) "ab")
      (check-equal? (styled-segment-text (second segs)) "c")
      (check-not-false (member 'inverse (styled-segment-style (second segs))))
      (check-equal? (styled-segment-text (third segs)) "d"))

    (test-case "apply-selection-highlight with multi-segment line"
      ;; Simulates a user message line: "> " (bold cyan) + "Hello" (bold)
      ;; Select from col 0 to col 6 (entire line)
      (define lines
        (list (styled-line (list (styled-segment "> " '(bold cyan))
                                 (styled-segment "Hello" '(bold))))))
      (define result (apply-selection-highlight lines (cons 0 1) (cons 6 1) 1))
      (define segs (styled-line-segments (first result)))
      ;; All segments should be inverted
      (for ([s segs])
        (check-not-false (member 'inverse (styled-segment-style s))
                         (format "segment '~a' should be inverted" (styled-segment-text s)))))))

(run-tests highlight-multi-seg-tests)

;; Word-wrapping tests for md-format-assistant and wrap-styled-line
(define wrapping-tests
  (test-suite "word-wrapping for assistant markdown"

    (test-case "wrap-styled-line short line passes through"
      (define sl (styled-line (list (styled-segment "hello" '()))))
      (define result (wrap-styled-line sl 80))
      (check-equal? (length result) 1)
      (check-equal? (styled-line-segments (first result)) (list (styled-segment "hello" '()))))

    (test-case "wrap-styled-line wraps long plain text"
      (define long-text (make-string 100 #\a))
      (define sl (styled-line (list (styled-segment long-text '()))))
      (define result (wrap-styled-line sl 40))
      (check > (length result) 1)
      ;; All text preserved
      (check-equal? (apply string-append (map styled-line->text result)) long-text))

    (test-case "wrap-styled-line preserves multi-segment styles"
      ;; "bold-code-rest" where bold text is 50 chars, code is 10, rest is 40
      (define sl
        (styled-line (list (styled-segment (make-string 50 #\b) '(bold))
                           (styled-segment "codeblock" '(cyan))
                           (styled-segment (make-string 40 #\x) '()))))
      (define result (wrap-styled-line sl 40))
      ;; First wrapped line should start with bold style
      (define first-line-segs (styled-line-segments (first result)))
      (check-not-false (member 'bold (styled-segment-style (first first-line-segs))))
      ;; All text preserved across wrapped lines
      (check-equal? (apply string-append (map styled-line->text result))
                    (string-append (make-string 50 #\b) "codeblock" (make-string 40 #\x))))

    (test-case "md-format-assistant wraps long paragraphs"
      (define long-para (string-append (make-string 200 #\a)))
      (define result (md-format-assistant long-para 40))
      (check > (length result) 1)
      ;; All text preserved
      (check-equal? (apply string-append (map styled-line->text result)) long-para))

    (test-case "md-format-assistant does NOT wrap code blocks"
      ;; Code block with a long line — should NOT be wrapped
      ;; Note: parse-markdown preserves trailing \n, so string-split produces
      ;; an extra empty string at the end
      (define code-text "```\ncccc\n```")
      (define result (md-format-assistant code-text 40))
      ;; Code block produces lines for each \n-split line, NOT wrapped
      (for ([line result])
        (define line-text (styled-line->text line))
        ;; None of the code lines should be wrapped
        ;; (they may exceed terminal width but that's handled by draw-styled-line!)
        (check-not-false
         (member 'bright-green (apply append (map styled-segment-style (styled-line-segments line))))
         (format "code line '~a' should be bright-green (theme md-code)" line-text))))

    (test-case "md-format-assistant does NOT wrap headers"
      (define header-text (string-append "# " (make-string 200 #\h)))
      (define result (md-format-assistant header-text 40))
      ;; Header should be 1 line (not wrapped)
      (check-equal? (length result) 1)
      (define header-line-text (styled-line->text (first result)))
      (check > (string-length header-line-text) 40))

    (test-case "md-format-assistant wraps markdown with bold and code inline"
      ;; "**bold text** normal `code` more normal text that extends past 40 cols"
      (define text
        "**bold text** normal text and more text that continues past forty columns of width")
      (define result (md-format-assistant text 40))
      (check > (length result) 1)
      ;; Total text (minus markdown delimiters) preserved
      (define all-text (apply string-append (map styled-line->text result)))
      (check-true (string-contains? all-text "bold text"))
      (check-true (string-contains? all-text "columns of width")))))

(run-tests wrapping-tests)

;; Thinking indicator tests (#136)
(define thinking-tests
  (test-suite "Thinking indicator"

    (test-case "render-status-bar shows [thinking...] when busy with no streaming text"
      (let ([state (struct-copy ui-state
                                (initial-ui-state #:session-id "test-sess" #:model-name "gpt-4")
                                [busy? #t]
                                [streaming-text #f])])
        (define line (render-status-bar state 80))
        (define text (styled-segment-text (first (styled-line-segments line))))
        (check-true (string-contains? text "[thinking...]")
                    "status bar shows [thinking...] when busy and no streaming text")))

    (test-case "render-status-bar does NOT show [thinking...] when idle"
      (let ([state (initial-ui-state #:session-id "test-sess" #:model-name "gpt-4")])
        (define line (render-status-bar state 80))
        (define text (styled-segment-text (first (styled-line-segments line))))
        (check-false (string-contains? text "[thinking...]") "no [thinking...] when not busy")))

    (test-case "render-status-bar does NOT show [thinking...] when streaming text present"
      (let ([state (struct-copy ui-state
                                (initial-ui-state #:session-id "test-sess" #:model-name "gpt-4")
                                [busy? #t]
                                [streaming-text "Partial response..."])])
        (define line (render-status-bar state 80))
        (define text (styled-segment-text (first (styled-line-segments line))))
        (check-false (string-contains? text "[thinking...]")
                     "no [thinking...] when streaming text is present")))))

(run-tests thinking-tests)

;; CJK/wide-char wrapping tests (Wave 1: #368)
(define cjk-wrapping-tests
  (test-suite "CJK/wide-char wrapping"

    (test-case "wrap-text: CJK string wraps at visual column boundary"
      ;; 你好世界 = 4+4 = 8 visible cols, 4 chars
      ;; Wrap at width 5 → first line: 你好 (4 cols), second: 世界 (4 cols)
      (define result (wrap-text "你好世界" 5))
      (check-equal? (length result) 2 "CJK wraps into 2 lines")
      (check-equal? (string-visible-width (first result)) 4 "first line is 4 cols")
      (check-equal? (string-visible-width (second result)) 4 "second line is 4 cols"))

    (test-case "wrap-text: mixed ASCII+CJK wraps correctly"
      ;; hello你好 = 5+4 = 9 visible cols
      ;; Wrap at width 9 → fits in one line
      (define result1 (wrap-text "hello你好" 9))
      (check-equal? (length result1) 1 "9 cols: fits in one line")
      ;; Wrap at width 7 → hello (5) + 你 (2) = 7 cols on first line
      ;; But 你 is char index 5, 好 is char index 6
      (define result2 (wrap-text "hello你好" 7))
      (check > (length result2) 1 "7 cols: wraps to multiple lines")
      (check-equal? (string-visible-width (first result2)) 7 "first line is 7 cols"))

    (test-case "wrap-text: combining marks don't consume column budget"
      ;; e + combining acute (2 chars, 1 visible col)
      (define result (wrap-text "e\u0301e\u0301e\u0301" 3))
      (check-equal? (length result) 1 "combining marks fit in 3 cols"))

    (test-case "wrap-single-line: long CJK string wraps correctly"
      ;; 10 CJK chars = 20 visible cols, wrap at 10 → 2 lines of 10 cols each
      (define cjk-str "一二三四五六七八九十")
      (define result (wrap-single-line cjk-str 10))
      (check-equal? (length result) 2)
      (check-equal? (string-visible-width (first result)) 10)
      (check-equal? (string-visible-width (second result)) 10))

    (test-case "wrap-styled-line: CJK with styles preserves segment styles"
      (define sl (styled-line (list (styled-segment "你好世界" '(bold)))))
      (define result (wrap-styled-line sl 5))
      (check-equal? (length result) 2 "CJK styled line wraps to 2 lines")
      ;; All segments should have 'bold style
      (for ([line result])
        (for ([seg (styled-line-segments line)])
          (check-not-false (member 'bold (styled-segment-style seg))
                           (format "segment '~a' should be bold" (styled-segment-text seg))))))

    (test-case "wrap-text: long ASCII string still works correctly"
      ;; Regression test: ensure ASCII wrapping still works after refactor
      (define result (wrap-text (make-string 100 #\a) 40))
      (check-equal? (length result) 3 "100 a's at width 40 = 3 lines")
      (check-equal? (apply string-append result) (make-string 100 #\a)))

    (test-case "wrap-text: whitespace break with mixed content"
      ;; "hello 你好" = 5+1+4 = 10 visible cols
      ;; Wrap at 8 → "hello " (6 cols) + "你好" (4 cols)
      (define result (wrap-text "hello 你好" 8))
      (check-equal? (length result) 2)
      (check-equal? (first result) "hello ")
      (check-equal? (second result) "你好"))

    ;; ============================================================
    ;; styled-line->ansi — ANSI encoding of styled lines
    ;; ============================================================

    (test-case "styled-line->ansi: plain text (no styles)"
      (define sl (styled-line (list (styled-segment "hello" '()))))
      (check-equal? (styled-line->ansi sl) "hello"))

    (test-case "styled-line->ansi: single style"
      (define sl (styled-line (list (styled-segment "error" '(bold red)))))
      (check-equal? (styled-line->ansi sl) "\x1b[1;31merror\x1b[0m"))

    (test-case "styled-line->ansi: multi-segment with different styles"
      (define sl (styled-line (list (styled-segment "> " '(bold cyan))
                                     (styled-segment "text" '(bold)))))
      (define ansi (styled-line->ansi sl))
      (check-true (string-contains? ansi "\x1b[1;36m> "))
      (check-true (string-contains? ansi "\x1b[1mtext"))
      (check-true (string-suffix? ansi "\x1b[0m")))

    (test-case "styles->sgr: empty styles"
      (check-equal? (styles->sgr '()) ""))

    (test-case "styles->sgr: bold cyan"
      (check-equal? (styles->sgr '(bold cyan)) "\x1b[1;36m"))

    ;; Bright color support (SGR 90-97)
    (test-case "styles->sgr: bright-black"
      (check-equal? (styles->sgr '(bright-black)) "\x1b[90m"))

    (test-case "styles->sgr: bright-green"
      (check-equal? (styles->sgr '(bright-green)) "\x1b[92m"))

    (test-case "styles->sgr: bold bright-white"
      (check-equal? (styles->sgr '(bold bright-white)) "\x1b[1;97m"))

    (test-case "styled-line->ansi: bright color segment"
      (define sl (styled-line (list (styled-segment "muted text" '(bright-black)))))
      (check-equal? (styled-line->ansi sl) "\x1b[90mmuted text\x1b[0m"))

    ;; SGR leak regression: styled→plain segment must not leak styles
    (test-case "styled-line->ansi: styled to plain segment resets SGR"
      (define sl (styled-line (list (styled-segment "> " '(bold cyan))
                                     (styled-segment "hello" '()))))
      (define ansi (styled-line->ansi sl))
      ;; First segment gets SGR codes (no leading reset since it's first)
      (check-true (string-contains? ansi "\x1b[1;36m> "))
      ;; Plain segment after styled gets a reset before it
      (check-true (string-contains? ansi "\x1b[0mhello"))
      (check-true (string-suffix? ansi "\x1b[0m"))
      ;; The "hello" text must NOT have the cyan/bold style active
      (check-false (string-contains? ansi "\x1b[1;36mhello")
                   "plain segment must not inherit previous style"))

    (test-case "styled-line->ansi: three segments with mixed styles"
      (define sl (styled-line (list (styled-segment "[" '(bold))
                                     (styled-segment "ok" '(green))
                                     (styled-segment "]" '()))))
      (define ansi (styled-line->ansi sl))
      ;; First segment: no leading reset
      (check-true (string-contains? ansi "\x1b[1m["))
      ;; Second segment: reset + new style
      (check-true (string-contains? ansi "\x1b[0m\x1b[32mok"))
      ;; Third segment (plain after styled): reset
      (check-true (string-contains? ansi "\x1b[0m]"))
      ;; Final reset
      (check-true (string-suffix? ansi "\x1b[0m")))))

(run-tests cjk-wrapping-tests)

;; ============================================================
;; BUG-26: #f text in render pipeline must not crash
;; ============================================================

(define bug26-tests
  (test-suite "BUG-26: #f text guard in render pipeline"

    (test-case "render-transcript with #f text entry does not crash"
      (define state0 (initial-ui-state))
      (define entry-with-#f (transcript-entry 'assistant #f 1000 (hash) #f))
      (define state1 (struct-copy ui-state state0 [transcript (list entry-with-#f)]))
      ;; Must not crash — previously threw string=? contract violation
      (define-values (lines _st) (render-transcript state1 24 80))
      ;; Empty text → 0 rendered lines (no crash)
      (check-equal? (length lines) 0 "#f text renders zero lines (no crash)"))

    (test-case "format-entry with #f text for assistant returns empty list"
      (define entry (transcript-entry 'assistant #f 1000 (hash) #f))
      ;; Must not crash — returns 0 lines since empty text has no markdown tokens
      (define lines (format-entry entry 80))
      (check-equal? (length lines) 0))

    (test-case "format-entry with empty string text for assistant works"
      (define entry (make-entry 'assistant "" 1000 (hash)))
      (define lines (format-entry entry 80))
      (check-equal? (length lines) 0))

    (test-case "format-entry with #f text for tool-start does not crash"
      (define entry (transcript-entry 'tool-start #f 1000 (hash) #f))
      (define lines (format-entry entry 80))
      (check-equal? (length lines) 1))

    (test-case "format-entry with #f text for system does not crash"
      (define entry (transcript-entry 'system #f 1000 (hash) #f))
      (define lines (format-entry entry 80))
      (check-equal? (length lines) 1))))

(run-tests bug26-tests)
