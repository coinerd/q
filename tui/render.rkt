#lang racket/base

(require racket/string
         racket/list
         racket/function
         "state.rkt"
         "input.rkt"
         "char-width.rkt"
         "theme.rkt"
         "../util/markdown.rkt"
         (only-in "../extensions/custom-renderer-registry.rkt" lookup-custom-renderer-for-tool))

;; Types
(provide (struct-out styled-line)
         (struct-out styled-segment)

         ;; Selection highlighting
         apply-selection-highlight
         highlight-line-range
         style-invert

         ;; Frame rendering
         render-transcript
         render-status-bar
         render-input-line
         render-branch-list
         render-leaf-nodes
         render-children-list

         ;; Entry formatting
         format-entry
         md-format-assistant

         ;; Helpers
         styled-line->text
         styled-line->ansi
         styles->sgr
         wrap-styled-line

         ;; FEAT-69: markdown rendering internals
         theme->style
         md-token->segment
         wrap-text
         wrap-single-line)

;; A styled segment (part of a line)
(struct styled-segment
        (text ; string
         style ; (listof symbol) — 'bold 'italic 'inverse 'underline 'dim
         ;   'red 'green 'yellow 'blue 'cyan 'magenta 'white
         )
  #:transparent)

;; A styled line (one terminal row)
(struct styled-line
        (segments ; (listof styled-segment)
         )
  #:transparent)

;; Convenience: make a plain line
(define (plain-line text)
  (styled-line (list (styled-segment text '()))))

;; Resolve a theme field to a style list for styled-segment.
;; Returns a list of symbols suitable for the style field.
;; For example: (theme->style 'accent) => '(cyan)
;;              (theme->style 'accent '(bold)) => '(bold cyan)
(define (theme->style field [modifiers '()])
  (define color (theme-ref field))
  (if color
      (append modifiers (list color))
      modifiers))

;; Format a transcript entry into styled lines
(define (format-entry entry [width 200])
  ;; Returns (listof styled-line) — usually 1 line, but could be multi-line
  (define kind (transcript-entry-kind entry))
  ;; BUG-26 fix: guard #f text — replace with empty string
  (define raw-text (or (transcript-entry-text entry) ""))
  ;; BUG-NEWLINE-BLEED fix: sanitize newlines in tool results/errors
  ;; Newlines in styled-line text cause terminal row overflow.
  ;; Replace with ⏎ visual indicator.
  (define text
    (if (memq kind '(tool-end tool-fail tool-start))
        (string-replace raw-text "\n" " \u23ce ")
        raw-text))
  ;; BUG-37 fix: skip whitespace-only assistant entries
  (case kind
    [(assistant)
     (if (string=? (string-trim text) "")
         '() ;; empty/whitespace → no lines
         (md-format-assistant text width))]
    [(tool-start)
     (define tool-name (hash-ref (transcript-entry-meta entry) 'tool-name #f))
     (define custom-render (and tool-name (lookup-custom-renderer-for-tool tool-name 'call)))
     (if custom-render
         (custom-render (hash-ref (transcript-entry-meta entry) 'args text))
         (list (styled-line (list (styled-segment text (theme->style 'tool-title))))))]
    [(tool-end)
     (define tool-name (hash-ref (transcript-entry-meta entry) 'tool-name #f))
     (define custom-render (and tool-name (lookup-custom-renderer-for-tool tool-name 'result)))
     (if custom-render
         (custom-render text)
         (list (styled-line (list (styled-segment text (theme->style 'success))))))]
    [(tool-fail)
     (define tool-name (hash-ref (transcript-entry-meta entry) 'tool-name #f))
     (define custom-render (and tool-name (lookup-custom-renderer-for-tool tool-name 'result)))
     (if custom-render
         (custom-render text)
         (list (styled-line (list (styled-segment text (theme->style 'error))))))]
    [(system)
     (list (styled-line (list (styled-segment (string-append "[SYS] " text) (theme->style 'muted)))))]
    [(error)
     (list (styled-line (list (styled-segment (string-append "[ERR] " text)
                                              (theme->style 'error '(bold))))))]
    [(user)
     ;; Split into themed prompt + bold text
     (list (styled-line (list (styled-segment "> " (theme->style 'input-prompt '(bold)))
                              (styled-segment text '(bold)))))]
    [else (list (plain-line text))]))

;; Convert a single md-token to a styled-segment
;; Matches pi's dark theme color scheme:
;;   bold → bold, italic → italic, code → cyan (teal),
;;   links → blue+underline, text → plain
(define (md-token->segment tok)
  (define content (md-token-content tok))
  (case (md-token-type tok)
    [(text) (styled-segment content '())]
    [(bold) (styled-segment content '(bold))]
    [(italic) (styled-segment content '(italic))]
    [(strikethrough) (styled-segment content '(dim))]
    [(code) (styled-segment content (theme->style 'md-code))]
    [(link) (styled-segment (cdr content) (theme->style 'md-link '(underline)))]
    [else (styled-segment (format "~a" content) '())]))

;; Extract plain text from a styled-line (local helper)
(define (styled-line->text sl)
  (apply string-append (map styled-segment-text (styled-line-segments sl))))

;; Convert a styled-line to an ANSI-encoded string.
;; Each segment gets its SGR codes; a final reset is appended.
(define (styled-line->ansi sl)
  ;; Emit reset before each segment so styles don't leak between them.
  ;; This means every segment starts with \x1b[0m (except the very first
  ;; one if it has no previous styled segment to reset from). We use a
  ;; simple rule: reset before every segment after the first styled one.
  (define seen-styled? (box #f))
  (define parts
    (for/list ([seg (in-list (styled-line-segments sl))])
      (define text (string-replace (styled-segment-text seg) "\n" " "))
      (define styles (styled-segment-style seg))
      (cond
        [(null? styles)
         ;; Plain segment: reset if a previous segment had styles
         (if (unbox seen-styled?)
             (string-append "\x1b[0m" text)
             text)]
        [else
         ;; Styled segment: reset before (if not first) + SGR + text
         (define reset-prefix (if (unbox seen-styled?) "\x1b[0m" ""))
         (set-box! seen-styled? #t)
         (string-append reset-prefix (styles->sgr styles) text)])))
  (define content (apply string-append parts))
  ;; Append SGR reset if any segment had styles
  (if (unbox seen-styled?)
      (string-append content "\x1b[0m")
      content))

;; Convert a list of style symbols to an SGR escape sequence.
;; e.g. '(bold cyan) → "\x1b[1;36m"
(define (styles->sgr styles)
  (define codes
    (for/list ([s (in-list styles)])
      (case s
        [(bold) 1]
        [(italic) 3]
        [(underline) 4]
        [(inverse) 7]
        [(dim) 2]
        [(black) 30]
        [(red) 31]
        [(green) 32]
        [(yellow) 33]
        [(blue) 34]
        [(magenta) 35]
        [(cyan) 36]
        [(white) 37]
        [(bright-black) 90]
        [(bright-red) 91]
        [(bright-green) 92]
        [(bright-yellow) 93]
        [(bright-blue) 94]
        [(bright-magenta) 95]
        [(bright-cyan) 96]
        [(bright-white) 97]
        [else #f])))
  (define valid (filter values codes))
  (if (null? valid)
      ""
      (format "\x1b[~am" (string-join (map number->string valid) ";"))))

;; Wrap a styled-line to a given width, returning a list of styled-lines.
;; Preserves segment styles by tracking character-to-segment mapping.
(define (wrap-styled-line sl width)
  (define text (styled-line->text sl))
  (if (<= (string-visible-width text) width)
      (list sl)
      (let* ([segs (styled-line-segments sl)]
             ;; Build char-style table: for each character, which style?
             [char-styles (for*/vector ([seg (in-list segs)]
                                        [_ (in-string (styled-segment-text seg))])
                            (styled-segment-style seg))]
             ;; Wrap the plain text into string lines
             [wrapped-lines (wrap-text text width)])
        ;; Rebuild styled-lines from wrapped text + char-style table
        (let loop ([lines wrapped-lines]
                   [char-pos 0]
                   [acc '()])
          (if (null? lines)
              (reverse acc)
              (let* ([line-text (car lines)]
                     [line-len (string-length line-text)])
                ;; Build segments by grouping consecutive chars with same style
                (define new-segs
                  (let seg-loop ([i 0]
                                 [seg-acc '()])
                    (cond
                      [(>= i line-len) (reverse seg-acc)]
                      [else
                       (define style (vector-ref char-styles (+ char-pos i)))
                       (let run-loop ([j (add1 i)])
                         (cond
                           [(>= j line-len)
                            (seg-loop line-len
                                      (cons (styled-segment (substring line-text i line-len) style)
                                            seg-acc))]
                           [(equal? (vector-ref char-styles (+ char-pos j)) style)
                            (run-loop (add1 j))]
                           [else
                            (seg-loop j
                                      (cons (styled-segment (substring line-text i j) style)
                                            seg-acc))]))])))
                (loop (cdr lines) (+ char-pos line-len) (cons (styled-line new-segs) acc))))))))

;; Format assistant markdown text into styled-lines
(define (md-format-assistant text width)
  (define tokens (parse-markdown text))
  ;; Process tokens: code-blocks, headers, and newlines create their own lines,
  ;; inline tokens (text/bold/italic/code/link) are collected into lines
  (define (flush-current lines current-segs)
    (if (null? current-segs)
        lines
        (let* ([raw-line (styled-line current-segs)]
               ;; Wrap inline text to width (code blocks and headers are NOT wrapped)
               [wrapped (wrap-styled-line raw-line width)])
          (append lines wrapped))))
  (define-values (lines current-segs)
    (for/fold ([lines '()]
               [current-segs '()])
              ([tok (in-list tokens)])
      (case (md-token-type tok)
        ;; Flush current line, start a new one
        [(newline) (values (flush-current lines current-segs) '())]
        [(code-block)
         ;; Flush current inline segments as a line
         (define prev-lines (flush-current lines current-segs))
         ;; Create code block lines — green like pi's mdCodeBlock
         (define code (cdr (md-token-content tok)))
         (define code-lines
           (for/list ([line (string-split code "\n" #:trim? #f)])
             (styled-line (list (styled-segment (string-append "  " line) (theme->style 'md-code))))))
         (values (append prev-lines code-lines) '())]
        [(header)
         (define prev-lines (flush-current lines current-segs))
         (define header-text (cdr (md-token-content tok)))
         ;; Heading uses md-heading theme color
         (values (append prev-lines
                         (list (styled-line (list (styled-segment header-text
                                                                  (theme->style 'md-heading
                                                                                '(bold)))))))
                 '())]
        [(hr)
         (define prev-lines (flush-current lines current-segs))
         ;; Horizontal rule — dim line
         (values (append prev-lines
                         (list (styled-line (list (styled-segment (make-string (max width 20) #\u2500)
                                                                  (theme->style 'muted))))))
                 '())]
        [(blockquote)
         (define prev-lines (flush-current lines current-segs))
         (define bq-content (md-token-content tok))
         ;; Blockquote: prefix with > and dim styling
         (define depth (car bq-content))
         (define inner-tokens (cdr bq-content))
         (define prefix (make-string depth #\>))
         (define inner-segs
           (for/list ([t (in-list inner-tokens)])
             (md-token->segment t)))
         (define bq-line
           (styled-line (cons (styled-segment (string-append prefix " ") (theme->style 'muted))
                              inner-segs)))
         (values (append prev-lines (list bq-line)) '())]
        [(unordered-list)
         (define prev-lines (flush-current lines current-segs))
         (define ul-content (md-token-content tok))
         (define indent (car ul-content))
         (define inner-tokens (cdr ul-content))
         (define bullet-prefix (string-append (make-string (* indent 2) #\space) "\u2022 "))
         (define inner-segs
           (for/list ([t (in-list inner-tokens)])
             (md-token->segment t)))
         (define ul-line (styled-line (cons (styled-segment bullet-prefix '()) inner-segs)))
         (values (append prev-lines (list ul-line)) '())]
        [(ordered-list)
         (define prev-lines (flush-current lines current-segs))
         (define ol-content (md-token-content tok))
         (define indent (car ol-content))
         (define num (cadr ol-content))
         (define inner-tokens (cddr ol-content))
         (define ol-prefix (string-append (make-string (* indent 2) #\space) (format "~a. " num)))
         (define inner-segs
           (for/list ([t (in-list inner-tokens)])
             (md-token->segment t)))
         (define ol-line (styled-line (cons (styled-segment ol-prefix '()) inner-segs)))
         (values (append prev-lines (list ol-line)) '())]
        [else
         ;; Inline token — accumulate segments
         (define seg (md-token->segment tok))
         (values lines (append current-segs (list seg)))])))
  ;; Flush remaining inline segments
  (flush-current lines current-segs))

;; Wrap text to a given width, returning list of strings.
;; Uses visible column width (CJK-aware) instead of string-length.
(define (wrap-text text max-width)
  (if (<= max-width 0)
      (list text)
      (let ([lines (string-split text "\n")])
        (apply append
               (for/list ([line (in-list lines)])
                 (wrap-single-line line max-width))))))

(define (wrap-single-line line max-width)
  (if (<= (string-visible-width line) max-width)
      (list line)
      (let loop ([remaining line]
                 [acc '()])
        (if (<= (string-visible-width remaining) max-width)
            (reverse (cons remaining acc))
            (let ([break-pos (find-break-pos remaining max-width)])
              (loop (substring remaining break-pos) (cons (substring remaining 0 break-pos) acc)))))))

;; Find a good break position using visible column width (prefer space).
;; Returns a character index (not a column index) into `text`.
(define (find-break-pos text max-width)
  ;; Walk through the string by character, accumulating visible width.
  ;; Find the last whitespace before visible width exceeds max-width.
  (let loop ([i 0]
             [col 0]
             [last-space-idx #f])
    (cond
      [(>= i (string-length text)) (string-length text)]
      ;; Exceeded width — break at last space or current position
      [(>= col max-width) (or last-space-idx i)]
      [else
       (define c (string-ref text i))
       (define w (char-width c))
       (define new-col (+ col w))
       (cond
         ;; Char would exceed max-width on its own
         [(> new-col max-width) (or last-space-idx i)]
         [(char-whitespace? c) (loop (add1 i) new-col (add1 i))]
         [else (loop (add1 i) new-col last-space-idx)])])))

;; Invert a style (add 'inverse if not present, remove if present)
(define (style-invert style)
  (if (member 'inverse style)
      (filter (lambda (s) (not (eq? s 'inverse))) style)
      (cons 'inverse style)))

;; Apply selection highlight to a list of rendered lines.
;; sel-anchor and sel-end are (cons col row) in screen coordinates,
;; where row is 0-based (row 0 = header, row 1 = first transcript line).
;; trans-start is the 0-based row where transcript starts (typically 1).
;; pad-count is the number of blank rows at top of transcript area
;; (BUG-57 fix: content is pushed down by pad-count rows when sparse).
;; Returns new list of styled-lines with 'inverse on selected ranges.
(define (apply-selection-highlight lines sel-anchor sel-end trans-start [pad-count 0])
  (if (not (and sel-anchor sel-end))
      lines ;; no selection — pass through
      (let ()
        ;; Normalize: ensure anchor <= end
        (define-values (start-col start-row end-col end-row)
          (normalize-selection-range sel-anchor sel-end))
        ;; Convert screen rows to line indices (BUG-57: subtract pad-count)
        (define sel-start-idx (max 0 (- start-row trans-start pad-count)))
        (define sel-end-idx (- end-row trans-start pad-count))
        (for/list ([line (in-list lines)]
                   [i (in-naturals)])
          (cond
            [(< i sel-start-idx) line]
            [(> i sel-end-idx) line]
            [else
             ;; This line is in the selection range — apply inverse.
             ;; Mouse X/Y gives display columns; convert to string offsets
             ;; so CJK characters (width 2) get correct selection bounds.
             (define line-text
               (apply string-append (map styled-segment-text (styled-line-segments line))))
             (define line-len (string-length line-text))
             (define col-start
               (if (= i sel-start-idx)
                   (min (display-col->string-offset line-text start-col) line-len)
                   0))
             (define col-end
               (if (= i sel-end-idx)
                   (min (display-col->string-offset line-text (add1 end-col)) line-len)
                   line-len))
             (if (>= col-start col-end)
                 line ;; selection range empty on this line
                 (highlight-line-range line col-start col-end))])))))

;; Apply inverse highlight to a column range within a styled-line.
;; Splits segments as needed to highlight exactly [col-start, col-end).
(define (highlight-line-range sl col-start col-end)
  (define segments (styled-line-segments sl))
  (styled-line
   (let loop ([segs segments]
              [pos 0]
              [acc '()])
     (if (null? segs)
         (reverse acc)
         (let* ([seg (car segs)]
                [text (styled-segment-text seg)]
                [style (styled-segment-style seg)]
                [seg-start pos]
                [seg-end (+ pos (string-length text))])
           (cond
             ;; Segment entirely before highlight
             [(<= seg-end col-start) (loop (cdr segs) seg-end (cons seg acc))]
             ;; Segment entirely after highlight
             [(>= seg-start col-end) (loop (cdr segs) seg-end (cons seg acc))]
             [else
              ;; Segment overlaps highlight range — split into up to 3 parts
              (let* ([before-len (max 0 (- col-start seg-start))]
                     [highlight-len (- (min seg-end col-end) (max seg-start col-start))]
                     [after-len (max 0 (- seg-end col-end))]
                     [non-empty
                      (filter
                       identity
                       (list
                        (and (> before-len 0) (styled-segment (substring text 0 before-len) style))
                        (and (> highlight-len 0)
                             (styled-segment (substring text before-len (+ before-len highlight-len))
                                             (style-invert style)))
                        (and (> after-len 0)
                             (styled-segment (substring text (+ before-len highlight-len)) style))))])
                (loop (cdr segs) seg-end (append (reverse non-empty) acc)))]))))))

;; Render the visible portion of the transcript
;; width is used for text wrapping
;; scroll-offset is line-based: 0 = bottom, positive = scrolled up
;; Returns (listof styled-line)
(define (render-transcript state transcript-height [width 200])
  (define entries (transcript-entries state))
  (define scroll-offset (ui-state-scroll-offset state))
  ;; Check width validity — flush cache if terminal resized
  (define state0
    (if (rendered-cache-width-valid? state width)
        state
        (rendered-cache-set-width (rendered-cache-clear state) width)))
  ;; Format entries using cache where possible
  ;; BUG-36 fix: cons lines in reverse order, then reverse at the end
  (define-values (formatted-lines state1)
    (let loop ([entries entries]
               [rev-lines '()]
               [st state0])
      (if (null? entries)
          (values (reverse rev-lines) st)
          (let* ([e (car entries)]
                 [eid (transcript-entry-id e)]
                 [cached (and eid (rendered-cache-ref st eid))]
                 [entry-text (transcript-entry-text e)]
                 [cache-valid? (and cached
                                    (string? entry-text)
                                    (> (string-length entry-text) 0)
                                    (> (length cached) 0)
                                    (let* ([first-line (styled-line->text (car cached))]
                                           [prefix-len (min 20 (string-length entry-text))]
                                           [prefix (substring entry-text 0 prefix-len)])
                                      (string-contains? first-line prefix)))])
            (if cache-valid?
                (loop (cdr entries) (foldl (lambda (l acc) (cons l acc)) rev-lines cached) st)
                (let ([fmt (format-entry e width)])
                  (loop (cdr entries)
                        (foldl (lambda (l acc) (cons l acc)) rev-lines fmt)
                        (if eid
                            (rendered-cache-set st eid fmt)
                            st))))))))
  ;; If streaming, append the partial streaming text (not cached)
  (define streaming (ui-state-streaming-text state))
  (define with-streaming
    (if (and streaming (not (string=? streaming "")))
        (append formatted-lines
                (for/list ([line (in-list (wrap-text streaming width))])
                  (styled-line (list (styled-segment line (theme->style 'muted))))))
        formatted-lines))
  ;; If thinking, append the thinking block (not cached)
  (define thinking-text (ui-state-streaming-thinking state))
  (define all-lines
    (if (and thinking-text (not (string=? thinking-text "")))
        (append with-streaming
                (list (styled-line (list (styled-segment "  ┄ thinking "
                                                         (theme->style 'muted '(dim))))))
                (for/list ([line (in-list (wrap-text thinking-text (max 1 (- width 4))))])
                  (styled-line (list (styled-segment (string-append "  │ " line)
                                                     (theme->style 'muted '(dim))))))
                (list (styled-line (list (styled-segment "  ┄" (theme->style 'muted '(dim)))))))
        with-streaming))
  (define total (length all-lines))
  (define sliced-lines
    (if (<= total transcript-height)
        all-lines
        (let* ([start (max 0 (- total transcript-height scroll-offset))]
               [available (drop all-lines start)])
          (take available (min transcript-height (length available))))))
  (values sliced-lines state1))

;; Render the status bar (bottom area above input)
;; Returns styled-line
(define (render-status-bar state width)
  (define session (ui-session-label state))
  (define model (ui-model-label state))
  (define status (ui-status-text state))
  (define busy? (ui-busy? state))
  (define streaming-text (ui-state-streaming-text state))
  (define streaming-thinking (ui-state-streaming-thinking state))
  (define thinking? (and busy? (not streaming-text) (not streaming-thinking)))
  (define actively-thinking? (and streaming-thinking (not (string=? streaming-thinking ""))))
  (define busy-marker (if busy? "*" " "))
  (define scroll-indicator (if (> (ui-state-scroll-offset state) 0) " ↑" ""))
  (define thinking-indicator
    (cond
      [(or actively-thinking? thinking?) " [thinking...]"]
      [else ""]))
  ;; BUG-55: Show mock provider warning in status bar
  (define mock-warning (if (ui-state-mock-provider? state) " [No API key]" ""))
  ;; v0.19.12 W1: Show context token count in status bar
  (define ctx-tok (ui-state-context-tokens state))
  (define tok-str
    (if ctx-tok
        (format " ~aK" (quotient (max 1 ctx-tok) 1000))
        ""))
  (define left (format " ~a q | ~a | ~a " busy-marker session model))
  (define right
    (format "~a~a~a~a~a " status mock-warning tok-str thinking-indicator scroll-indicator))
  ;; Pad to width
  (define pad-len (max 0 (- width (string-visible-width left) (string-visible-width right))))
  (define padded (string-append left (make-string pad-len #\ ) right))
  (styled-line (list (styled-segment padded '(inverse)))))

;; Render the input line
;; Returns styled-line with themed prompt + bold text
;; Returns styled-line
(define (render-input-line input-st width)
  (define-values (visible-text scroll-offset cursor-col) (input-visible-window input-st width))
  (define prompt-str "q> ")
  (define pad-len
    (max 0 (- width (string-visible-width prompt-str) (string-visible-width visible-text))))
  (styled-line (list (styled-segment prompt-str (theme->style 'input-prompt '(bold)))
                     (styled-segment (string-append visible-text (make-string pad-len #\space))
                                     '(bold)))))

;; Render a list of branch-info structs
;; Returns (listof styled-line)
(define (render-branch-list branches [width 200])
  (define header
    (styled-line (list (styled-segment (format "  Branches (~a):" (length branches))
                                       '(bold underline)))))
  (define branch-lines
    (for/list ([b (in-list branches)]
               [i (in-naturals 1)])
      (define prefix (if (branch-info-active? b) "  → " "    "))
      (define id (branch-info-id b))
      (define role-str (symbol->string (branch-info-role b)))
      (define leaf-indicator (if (branch-info-leaf? b) " [leaf]" ""))
      (define line-text (format "~a~a. ~a (~a)~a" prefix i id role-str leaf-indicator))
      (define style
        (if (branch-info-active? b)
            (theme->style 'success '(bold))
            (theme->style 'muted)))
      (styled-line (list (styled-segment line-text style)))))
  (cons header branch-lines))

;; Render leaf nodes list
;; Returns (listof styled-line)
(define (render-leaf-nodes branches [width 200])
  (define leaves (filter branch-info-leaf? branches))
  (define header
    (styled-line (list (styled-segment (format "  Leaf Nodes (~a):" (length leaves))
                                       '(bold underline)))))
  (define leaf-lines
    (for/list ([b (in-list leaves)]
               [i (in-naturals 1)])
      (define prefix (if (branch-info-active? b) "  → " "    "))
      (define id (branch-info-id b))
      (define role-str (symbol->string (branch-info-role b)))
      (define line-text (format "~a~a. ~a (~a)" prefix i id role-str))
      (define style
        (if (branch-info-active? b)
            (theme->style 'success '(bold))
            (theme->style 'muted)))
      (styled-line (list (styled-segment line-text style)))))
  (cons header leaf-lines))

;; Render children of a specific node
;; Returns (listof styled-line)
(define (render-children-list parent-id children [width 200])
  (define header
    (styled-line (list (styled-segment (format "  Children of ~a (~a):" parent-id (length children))
                                       '(bold underline)))))
  (if (null? children)
      (list header (styled-line (list (styled-segment "    (no children)" (theme->style 'muted)))))
      (cons header
            (for/list ([b (in-list children)]
                       [i (in-naturals 1)])
              (define id (branch-info-id b))
              (define role-str (symbol->string (branch-info-role b)))
              (define leaf-indicator (if (branch-info-leaf? b) " [leaf]" ""))
              (define line-text (format "    ~a. ~a (~a)~a" i id role-str leaf-indicator))
              (styled-line (list (styled-segment line-text (theme->style 'muted))))))))
