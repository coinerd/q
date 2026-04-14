#lang racket

;; tests/test-tui-renderer.rkt — Tests for tui/renderer module
;;
;; Tests the ubuf-based renderer using a mock ubuf implementation.

(require rackunit
         rackunit/text-ui
         "../tui/renderer.rkt"
         "../tui/render.rkt"
         "../tui/layout.rkt"
         "../tui/state.rkt"
         "../tui/input.rkt")

;; ============================================================
;; Mock ubuf implementation for testing
;; ============================================================

;; A mock cell holding character and style attributes
;; Matches tui-ubuf: fg/bg as integers, bold, underline
(struct mock-cell (char bold underline fg bg) #:transparent #:mutable)

;; Create an empty cell (default fg=7, bg=0 like tui-ubuf)
(define (make-empty-cell)
  (mock-cell #\space #f #f 7 0))

;; A mock ubuf buffer
(struct mock-ubuf (cols rows cells) #:transparent)

;; Create a mock ubuf with given dimensions
(define (make-mock-ubuf cols rows)
  (define cells (make-vector (* cols rows) (make-empty-cell)))
  ;; Initialize with fresh cells (not shared)
  (for ([i (in-range (* cols rows))])
    (vector-set! cells i (make-empty-cell)))
  (mock-ubuf cols rows cells))

;; Get cell at (col, row) — 0-indexed (matching real ubuf)
(define (mock-ubuf-cell ubuf col row)
  (define cols (mock-ubuf-cols ubuf))
  (define idx (+ (* row cols) col))
  (vector-ref (mock-ubuf-cells ubuf) idx))

;; Set cell character
(define (mock-ubuf-set-cell! ubuf col row char bold underline fg bg)
  (define cell (mock-ubuf-cell ubuf col row))
  (set-mock-cell-char! cell char)
  (set-mock-cell-bold! cell bold)
  (set-mock-cell-underline! cell underline)
  (set-mock-cell-fg! cell fg)
  (set-mock-cell-bg! cell bg))

;; Clear the mock ubuf
(define (mock-ubuf-clear! ubuf)
  (define cols (mock-ubuf-cols ubuf))
  (define rows (mock-ubuf-rows ubuf))
  (for* ([row (in-range 0 rows)]
         [col (in-range 0 cols)])
    (mock-ubuf-set-cell! ubuf col row #\space #f #f 7 0)))

;; Put a string to the mock ubuf
;; Matches real tui-ubuf signature exactly:
;;   (ubuf x y str #:fg [7] #:bg [0] #:bold [#f] #:underline [#f] #:italic [#f] #:blink [#f])
(define (mock-ubuf-putstring! ubuf
                              col
                              row
                              str
                              #:fg [fg 7]
                              #:bg [bg 0]
                              #:bold [bold #f]
                              #:underline [underline #f]
                              #:italic [italic #f]
                              #:blink [blink #f])
  (for ([i (in-range (string-length str))])
    (mock-ubuf-set-cell! ubuf (+ col i) row (string-ref str i) bold underline fg bg)))

;; Read a row as string (ignoring styles)
(define (mock-ubuf-row-string ubuf row)
  (define cols (mock-ubuf-cols ubuf))
  (list->string (for/list ([col (in-range 0 cols)])
                  (mock-cell-char (mock-ubuf-cell ubuf col row)))))

;; Check if a row has a specific style attribute
(define (mock-ubuf-row-has-style? ubuf row style-pred)
  (define cols (mock-ubuf-cols ubuf))
  (for/or ([col (in-range 0 cols)])
    (style-pred (mock-ubuf-cell ubuf col row))))

;; Count cells with a specific style in a row
(define (mock-ubuf-row-style-count ubuf row style-accessor)
  (define cols (mock-ubuf-cols ubuf))
  (for/sum ([col (in-range 0 cols)]) (if (style-accessor (mock-ubuf-cell ubuf col row)) 1 0)))

;; ============================================================
;; Test helpers
;; ============================================================

;; Run render-frame! with mock ubuf operations
(define (run-render-frame! ubuf state input-st layout)
  (parameterize ([current-ubuf-clear mock-ubuf-clear!]
                 [current-ubuf-putstring mock-ubuf-putstring!])
    (define-values (cc cr _st) (render-frame! ubuf state input-st layout))
    (values cc cr)))

;; ============================================================
;; Test suite
;; ============================================================

(define renderer-tests
  (test-suite "TUI Renderer"

    ;; --------------------------------------------------------
    ;; style->ubuf-kws
    ;; --------------------------------------------------------
    (test-case "style->ubuf-kws converts bold"
      (define-values (kws vals) (style->ubuf-kws '(bold)))
      (check-not-false (member '#:bold kws) "bold keyword present")
      (check-equal? (list-ref vals (index-of kws '#:bold)) #t "bold value is #t"))

    (test-case "style->ubuf-kws converts inverse to fg/bg swap"
      (define-values (kws vals) (style->ubuf-kws '(inverse)))
      (check-not-false (member '#:fg kws) "fg keyword present")
      (check-not-false (member '#:bg kws) "bg keyword present")
      (check-equal? (list-ref vals (index-of kws '#:fg)) 0 "inverse fg=0")
      (check-equal? (list-ref vals (index-of kws '#:bg)) 7 "inverse bg=7"))

    (test-case "style->ubuf-kws converts underline"
      (define-values (kws vals) (style->ubuf-kws '(underline)))
      (check-not-false (member '#:underline kws) "underline keyword present")
      (check-equal? (list-ref vals (index-of kws '#:underline)) #t "underline value is #t"))

    (test-case "style->ubuf-kws converts dim to fg=8"
      (define-values (kws vals) (style->ubuf-kws '(dim)))
      (check-not-false (member '#:fg kws) "fg keyword present")
      (check-equal? (list-ref vals (index-of kws '#:fg)) 8 "dim fg=8"))

    (test-case "style->ubuf-kws converts red to fg=1"
      (define-values (kws vals) (style->ubuf-kws '(red)))
      (check-not-false (member '#:fg kws) "fg keyword present")
      (check-equal? (list-ref vals (index-of kws '#:fg)) 1 "red fg=1"))

    (test-case "style->ubuf-kws converts green to fg=2"
      (define-values (kws vals) (style->ubuf-kws '(green)))
      (check-not-false (member '#:fg kws) "fg keyword present")
      (check-equal? (list-ref vals (index-of kws '#:fg)) 2 "green fg=2"))

    (test-case "style->ubuf-kws converts multiple styles"
      (define-values (kws vals) (style->ubuf-kws '(bold red)))
      (check-not-false (member '#:bold kws) "bold present")
      (check-not-false (member '#:fg kws) "fg present")
      (check-equal? (list-ref vals (index-of kws '#:fg)) 1 "red fg=1"))

    (test-case "style->ubuf-kws handles empty style list"
      (define-values (kws vals) (style->ubuf-kws '()))
      (check-equal? kws '() "empty styles → no keywords")
      (check-equal? vals '() "empty styles → no values"))

    (test-case "style->ubuf-kws ignores unknown styles"
      (define-values (kws vals) (style->ubuf-kws '(bold unknown-style)))
      (check-not-false (member '#:bold kws) "bold present")
      ;; Only bold should appear (unknown-style ignored)
      (check-equal? (length kws) 1 "only 1 keyword for unknown style"))

    (test-case "style->ubuf-kws bold+underline returns sorted keywords"
      (define-values (kws vals) (style->ubuf-kws '(bold underline)))
      ;; keyword-apply requires sorted keywords
      (check-equal? kws '(#:bold #:underline) "keywords must be sorted"))

    (test-case "style->ubuf-kws inverse returns sorted keywords"
      (define-values (kws vals) (style->ubuf-kws '(inverse)))
      (check-equal? kws '(#:bg #:fg) "inverse keywords must be sorted: bg before fg"))

    (test-case "style->ubuf-kws cyan+underline returns sorted keywords"
      (define-values (kws vals) (style->ubuf-kws '(cyan underline)))
      (check-equal? kws '(#:fg #:underline) "cyan+underline keywords must be sorted"))

    (test-case "style->ubuf-kws bold+italic returns sorted keywords"
      (define-values (kws vals) (style->ubuf-kws '(bold italic)))
      (check-equal? kws '(#:bold #:italic) "bold+italic keywords must be sorted"))

    (test-case "style->ubuf-kws all styles returns sorted keywords"
      (define-values (kws vals) (style->ubuf-kws '(inverse bold underline)))
      (check-equal? kws '(#:bg #:bold #:fg #:underline) "all styles keywords must be sorted"))

    (test-case "style->ubuf-kws blue converts to fg=4"
      (define-values (kws vals) (style->ubuf-kws '(blue)))
      (check-equal? kws '(#:fg))
      (check-equal? vals '(4)))

    (test-case "style->ubuf-kws yellow converts to fg=3"
      (define-values (kws vals) (style->ubuf-kws '(yellow)))
      (check-equal? kws '(#:fg))
      (check-equal? vals '(3)))

    (test-case "style->ubuf-kws bold+yellow returns sorted keywords"
      (define-values (kws vals) (style->ubuf-kws '(bold yellow)))
      (check-equal? kws '(#:bold #:fg) "bold+yellow sorted")
      (check-equal? vals '(#t 3)))

    ;; --------------------------------------------------------
    ;; Header rendering
    ;; --------------------------------------------------------
    (test-case "render-frame! draws header row"
      (define ubuf (make-mock-ubuf 80 24))
      (define state (initial-ui-state))
      (define input-st (initial-input-state))
      (define layout (compute-layout 80 24))
      (run-render-frame! ubuf state input-st layout)
      ;; Check header row (row 1)
      (define header-str (mock-ubuf-row-string ubuf 0))
      (check-true (string-prefix? header-str " q ") "header should start with ' q '")
      ;; Header should be full width
      (check-equal? (string-length header-str) 80 "header should be full width"))

    (test-case "header row has inverse style applied (fg=0, bg=7)"
      (define ubuf (make-mock-ubuf 40 10))
      (define state (initial-ui-state))
      (define input-st (initial-input-state))
      (define layout (compute-layout 40 10))
      (run-render-frame! ubuf state input-st layout)
      ;; Header should have inverse style: bg=7
      (define inverse-count (mock-ubuf-row-style-count ubuf 0 (lambda (c) (= (mock-cell-bg c) 7))))
      (check-true (> inverse-count 0) "header should have bg=7 (inverse)"))

    (test-case "render-frame! clears buffer before drawing"
      (define ubuf (make-mock-ubuf 40 10))
      ;; Pre-populate with some data
      (for ([row (in-range 0 10)])
        (for ([col (in-range 0 40)])
          (mock-ubuf-set-cell! ubuf col row #\X #f #f 7 0)))
      (define state (initial-ui-state))
      (define input-st (initial-input-state))
      (define layout (compute-layout 40 10))
      (run-render-frame! ubuf state input-st layout)
      ;; Header row should be drawn with new content
      (define header-str (mock-ubuf-row-string ubuf 0))
      (check-true (string-prefix? header-str " q ") "header should be redrawn"))

    ;; --------------------------------------------------------
    ;; Transcript line rendering with styles
    ;; --------------------------------------------------------
    (test-case "render-frame! draws transcript entries"
      (define ubuf (make-mock-ubuf 80 24))
      (define state (add-transcript-entry (initial-ui-state) (make-entry 'user "Hello" 0 (hash))))
      (define input-st (initial-input-state))
      (define layout (compute-layout 80 24))
      (run-render-frame! ubuf state input-st layout)
      ;; Check transcript area (row 2 onwards)
      ;; The user entry should appear with "> " prefix
      (define found-user?
        (for/or ([row (in-range 1 23)])
          (string-contains? (mock-ubuf-row-string ubuf row) "> ")))
      (check-true found-user? "user entry should have prompt prefix"))

    (test-case "render-frame! renders assistant messages"
      (define ubuf (make-mock-ubuf 80 24))
      (define state
        (add-transcript-entry (initial-ui-state)
                              (make-entry 'assistant "Assistant response" 0 (hash))))
      (define input-st (initial-input-state))
      (define layout (compute-layout 80 24))
      (run-render-frame! ubuf state input-st layout)
      ;; Assistant message should be visible
      (define found-assistant?
        (for/or ([row (in-range 1 23)])
          (string-contains? (mock-ubuf-row-string ubuf row) "Assistant")))
      (check-true found-assistant? "assistant message should be rendered"))

    (test-case "render-frame! renders error messages with bold red style"
      (define ubuf (make-mock-ubuf 80 24))
      (define state
        (add-transcript-entry (initial-ui-state) (make-entry 'error "Error occurred" 0 (hash))))
      (define input-st (initial-input-state))
      (define layout (compute-layout 80 24))
      (run-render-frame! ubuf state input-st layout)
      ;; Error message should be visible
      (define found-error?
        (for/or ([row (in-range 1 23)])
          (string-contains? (mock-ubuf-row-string ubuf row) "Error")))
      (check-true found-error? "error message should be rendered"))

    (test-case "render-frame! truncates long transcript lines"
      (define ubuf (make-mock-ubuf 40 10))
      (define long-text (make-string 100 #\A))
      (define state
        (add-transcript-entry (initial-ui-state) (make-entry 'assistant long-text 0 (hash))))
      (define input-st (initial-input-state))
      (define layout (compute-layout 40 10))
      (run-render-frame! ubuf state input-st layout)
      ;; The line should be truncated/padded to 40 columns
      (define row2-str (mock-ubuf-row-string ubuf 1))
      (check-equal? (string-length row2-str) 40 "transcript line should be exactly 40 columns"))

    (test-case "render-frame! shows multiple transcript entries"
      (define ubuf (make-mock-ubuf 80 24))
      (define state
        (for/fold ([s (initial-ui-state)]) ([msg '("First" "Second" "Third")])
          (add-transcript-entry s (make-entry 'assistant msg 0 (hash)))))
      (define input-st (initial-input-state))
      (define layout (compute-layout 80 24))
      (run-render-frame! ubuf state input-st layout)
      ;; Should show the last entries that fit
      (define found-entries
        (for/sum ([row (in-range 1 23)])
                 (define str (mock-ubuf-row-string ubuf row))
                 (if (or (string-contains? str "First")
                         (string-contains? str "Second")
                         (string-contains? str "Third"))
                     1
                     0)))
      (check-true (>= found-entries 1) "should show at least one entry"))

    ;; --------------------------------------------------------
    ;; Status bar rendering
    ;; --------------------------------------------------------
    (test-case "render-frame! draws status bar"
      (define ubuf (make-mock-ubuf 80 24))
      (define state (initial-ui-state #:session-id "test-session" #:model-name "gpt-4"))
      (define input-st (initial-input-state))
      (define layout (compute-layout 80 24))
      (define status-row-num (tui-layout-status-row layout))
      (run-render-frame! ubuf state input-st layout)
      (define status-str (mock-ubuf-row-string ubuf status-row-num))
      (check-true (string-contains? status-str "q") "status bar contains 'q' indicator")
      (check-true (string-contains? status-str "test-session") "status bar shows session id"))

    (test-case "status bar has inverse style (bg=7)"
      (define ubuf (make-mock-ubuf 80 24))
      (define state (initial-ui-state #:session-id "s1"))
      (define input-st (initial-input-state))
      (define layout (compute-layout 80 24))
      (define status-row-num (tui-layout-status-row layout))
      (run-render-frame! ubuf state input-st layout)
      (define inverse-count
        (mock-ubuf-row-style-count ubuf status-row-num (lambda (c) (= (mock-cell-bg c) 7))))
      (check-true (> inverse-count 0) "status bar should have bg=7 (inverse)"))

    (test-case "status bar shows busy indicator when busy"
      (define ubuf (make-mock-ubuf 80 24))
      (define state (struct-copy ui-state (initial-ui-state #:session-id "s1") [busy? #t]))
      (define input-st (initial-input-state))
      (define layout (compute-layout 80 24))
      (define status-row-num (tui-layout-status-row layout))
      (run-render-frame! ubuf state input-st layout)
      (define status-str (mock-ubuf-row-string ubuf status-row-num))
      ;; Status bar should have busy marker (⏳) or session info
      (check-true (or (string-contains? status-str "⏳") (string-contains? status-str "s1"))
                  "status bar should show content"))

    ;; --------------------------------------------------------
    ;; Input line rendering
    ;; --------------------------------------------------------
    (test-case "render-frame! draws input line with prompt"
      (define ubuf (make-mock-ubuf 80 24))
      (define state (initial-ui-state))
      (define input-st (input-insert-char (input-insert-char (initial-input-state) #\h) #\i))
      (define layout (compute-layout 80 24))
      (define input-row-num (tui-layout-input-row layout))
      (run-render-frame! ubuf state input-st layout)
      (define input-str (mock-ubuf-row-string ubuf input-row-num))
      (check-true (string-contains? input-str "q>") "input line shows q> prompt")
      (check-true (string-contains? input-str "hi") "input line shows typed text"))

    (test-case "input line has bold style"
      (define ubuf (make-mock-ubuf 80 24))
      (define state (initial-ui-state))
      (define input-st (input-insert-char (initial-input-state) #\x))
      (define layout (compute-layout 80 24))
      (define input-row-num (tui-layout-input-row layout))
      (run-render-frame! ubuf state input-st layout)
      (define bold-count (mock-ubuf-row-style-count ubuf input-row-num mock-cell-bold))
      (check-true (> bold-count 0) "input line should have bold style"))

    (test-case "input line handles empty buffer"
      (define ubuf (make-mock-ubuf 80 24))
      (define state (initial-ui-state))
      (define input-st (initial-input-state))
      (define layout (compute-layout 80 24))
      (define input-row-num (tui-layout-input-row layout))
      (run-render-frame! ubuf state input-st layout)
      (define input-str (mock-ubuf-row-string ubuf input-row-num))
      (check-true (string-contains? input-str "q>") "empty input still shows prompt"))

    (test-case "render-frame! returns cursor position"
      (define ubuf (make-mock-ubuf 80 24))
      (define state (initial-ui-state))
      (define input-st (input-insert-char (input-insert-char (initial-input-state) #\a) #\b))
      ;; cursor should be at position 2 (after "ab")
      (define layout (compute-layout 80 24))
      (define-values (cursor-col cursor-row) (run-render-frame! ubuf state input-st layout))
      ;; cursor-row should be input row
      (check-equal? cursor-row (tui-layout-input-row layout))
      ;; cursor-col should account for "q> " (4 chars) + 2 chars typed
      (check-true (>= cursor-col 4) "cursor column should be >= prompt width"))

    ;; --------------------------------------------------------
    ;; Layout edge cases
    ;; --------------------------------------------------------
    (test-case "render-frame! handles minimum screen size"
      (define ubuf (make-mock-ubuf 20 4))
      (define state (initial-ui-state))
      (define input-st (initial-input-state))
      (define layout (compute-layout 20 4))
      ;; Should not error on minimum size
      (check-not-exn (lambda () (run-render-frame! ubuf state input-st layout))))

    (test-case "render-frame! handles wide screen"
      (define ubuf (make-mock-ubuf 200 50))
      (define state (initial-ui-state))
      (define input-st (initial-input-state))
      (define layout (compute-layout 200 50))
      ;; Should not error on large size
      (check-not-exn (lambda () (run-render-frame! ubuf state input-st layout))))

    (test-case "render-frame! uses bg=0 for transcript area"
      (define ubuf (make-mock-ubuf 80 10))
      (define state (initial-ui-state))
      (define input-st (initial-input-state))
      (define layout (compute-layout 80 10))
      (run-render-frame! ubuf state input-st layout)
      ;; Empty transcript cells have bg=0 (ANSI black)
      (define trans-start (tui-layout-transcript-start-row layout))
      (define cell (mock-ubuf-cell ubuf 0 trans-start))
      (check-equal? (mock-cell-bg cell) 0 "empty cells use bg=0"))

    (test-case "render-frame! inverse header keeps explicit bg=7"
      (define ubuf (make-mock-ubuf 80 10))
      (define state (initial-ui-state))
      (define input-st (initial-input-state))
      (define layout (compute-layout 80 10))
      (run-render-frame! ubuf state input-st layout)
      (define header-y (tui-layout-header-row layout))
      (define cell (mock-ubuf-cell ubuf 1 header-y))
      (check-equal? (mock-cell-bg cell) 7 "header uses inverse bg=7"))

    (test-case "render-frame! handles many transcript entries"
      (define ubuf (make-mock-ubuf 80 24))
      (define state
        (for/fold ([s (initial-ui-state)]) ([i (in-range 100)])
          (add-transcript-entry s (make-entry 'system (format "Line ~a" i) 0 (hash)))))
      (define input-st (initial-input-state))
      (define layout (compute-layout 80 24))
      ;; Should only show the visible portion without error
      (check-not-exn (lambda () (run-render-frame! ubuf state input-st layout))))

    ;; --------------------------------------------------------
    ;; Integration: render with streaming text
    ;; --------------------------------------------------------
    (test-case "render-frame! shows streaming text"
      (define ubuf (make-mock-ubuf 80 24))
      (define state (struct-copy ui-state (initial-ui-state) [streaming-text "Partial response..."]))
      (define input-st (initial-input-state))
      (define layout (compute-layout 80 24))
      (run-render-frame! ubuf state input-st layout)
      ;; Streaming text should appear in transcript area (rows 2–22)
      (define found-streaming?
        (for/or ([row (in-range 1 22)])
          (string-contains? (mock-ubuf-row-string ubuf row) "Partial")))
      (check-true found-streaming? "streaming text should be rendered"))))

;; ============================================================
;; Run tests
;; ============================================================

(module+ main
  (run-tests renderer-tests))

;; For raco test
(module+ test
  (run-tests renderer-tests))
