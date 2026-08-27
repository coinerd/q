#lang racket/base

;; q/tui/render/message-layout.rkt — message layout algorithms (pure)
;;
;; Entry formatting, markdown rendering, styled-line construction.

(require racket/contract
         racket/match
         racket/string
         racket/list
         racket/function
         "../state-types.rkt"
         "../input.rkt"
         "../char-width.rkt"
         "../theme.rkt"
         "../../util/markdown.rkt"
         (only-in "../../util/markdown.rkt" md-token-content md-token-type)
         "../../extensions/custom-renderer-registry.rkt"
         "../../ui-core/disclosure-state.rkt")

(provide styled-line
         styled-line?
         styled-line-segments
         styled-segment
         styled-segment?
         styled-segment-text
         styled-segment-style
         (contract-out
          [plain-line (-> string? styled-line?)]
          [theme->style (->* (any/c) [(listof symbol?)] (listof symbol?))]
          [format-entry (->* (any/c) [exact-nonnegative-integer? any/c] (listof styled-line?))]
          [md-format-assistant (-> string? exact-nonnegative-integer? (listof styled-line?))]
          [table-token->styled-lines (-> any/c exact-nonnegative-integer? (listof styled-line?))]
          [md-token->segment (-> any/c styled-segment?)]
          [styled-line->text (-> styled-line? string?)]
          [styled-line->ansi (-> styled-line? string?)]
          [styles->sgr (-> (listof symbol?) string?)]
          [wrap-styled-line (-> styled-line? exact-nonnegative-integer? (listof styled-line?))]
          [wrap-text (-> string? exact-nonnegative-integer? (listof string?))]
          [wrap-single-line (-> string? exact-nonnegative-integer? (listof string?))]
          [find-break-pos
           (-> string?
               exact-nonnegative-integer?
               exact-nonnegative-integer?
               exact-nonnegative-integer?)]
          [lookup-custom-renderer-for-tool (-> string? symbol? any/c)]))

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

(define (plain-line text)
  (styled-line (list (styled-segment text '()))))

;; Resolve a theme field to a style list.
(define (theme->style field [modifiers '()])
  (define color (theme-ref field))
  (if color
      (append modifiers (list color))
      modifiers))

;; Format a transcript entry into styled lines.
(define (format-entry entry [width 200] [disclosure #f])
  (define kind (transcript-entry-kind entry))
  (define raw-text (or (transcript-entry-text entry) ""))

  (case kind
    [(user)
     (list (styled-line (list (styled-segment "> " '(bold cyan)) (styled-segment raw-text '(bold)))))]
    [(assistant) (md-format-assistant raw-text width)]
    [(system)
     (list (styled-line (list (styled-segment (format "[SYS] ~a" raw-text) '(bright-black)))))]
    ;; BUG-0043 (W2): campaign wave failures (stall kills, infra failures)
    ;; render as a distinct red error line — visually separated from both
    ;; assistant output and dim [SYS] notices. The text is the outcome
    ;; message verbatim; the entry kind (not a marker string) carries the
    ;; error semantics.
    [(system-error)
     (list (styled-line (list (styled-segment (format "[SYS] [ERROR] ~a" raw-text) '(bold red)))))]
    [(tool tool-result)
     (define tool-name (hash-ref (transcript-entry-meta entry) 'name #f))
     (define renderer (and tool-name (lookup-custom-renderer-for-tool tool-name 'call)))
     (define text
       (cond
         [renderer (renderer (hash-ref (transcript-entry-meta entry) 'args entry))]
         [tool-name (format "  [~a] ~a" tool-name raw-text)]
         [else (format "  [tool] ~a" raw-text)]))
     (list (styled-line (list (styled-segment text (theme->style 'tool)))))]
    [(tool-start)
     (define tool-name (hash-ref (transcript-entry-meta entry) 'name "tool"))
     (define renderer (lookup-custom-renderer-for-tool tool-name 'call))
     (if renderer
         (renderer (hash-ref (transcript-entry-meta entry) 'args entry))
         (let ([sanitized (string-replace raw-text "\n" " ")])
           (list (styled-line (list (styled-segment (format "[TOOL] ~a: ~a" tool-name sanitized)
                                                    '(cyan)))))))]
    [(tool-end)
     (define tool-name (hash-ref (transcript-entry-meta entry) 'name "tool"))
     (define renderer (lookup-custom-renderer-for-tool tool-name 'result))
     (if renderer
         (renderer raw-text)
         (let ([sanitized (string-replace raw-text "\n" " ")])
           (list (styled-line (list (styled-segment (format "[OK] ~a: ~a" tool-name sanitized)
                                                    '(green)))))))]
    [(tool-fail)
     (define tool-name (hash-ref (transcript-entry-meta entry) 'name "tool"))
     (define sanitized (string-replace raw-text "\n" " "))
     ;; v1.00.00 W2 (BUG-0002): route the [FAIL] line through the same
     ;; wrap-styled-line algorithm the assistant path uses (see md-format-assistant),
     ;; so long failure payloads wrap instead of being clipped at terminal width.
     ;; Continuation lines keep the red style and carry no repeated prefix,
     ;; matching the wrapped-assistant convention.
     (define fail-line
       (styled-line (list (styled-segment (format "[FAIL] ~a: ~a" tool-name sanitized) '(red)))))
     (wrap-styled-line fail-line width)]
    [(error) (list (styled-line (list (styled-segment (format "[ERR] ~a" raw-text) '(bold red)))))]
    [(thinking)
     ;; v0.99.96 W2: Honor disclosure state — collapsed shows preview, expanded shows full body.
     ;; Pure: uses only disclosure-state.rkt helpers, no terminal/render side effects.
     (define artifact-id (hash-ref (transcript-entry-meta entry) 'artifact-id #f))
     (define base-style '(dim italic cyan))
     (cond
       ;; No disclosure state available (e.g. legacy callers): fall back to 3-line truncation.
       [(not disclosure)
        (define max-lines 3)
        (define lines (string-split raw-text "\n"))
        (define visible-lines (take lines (min max-lines (length lines))))
        (define truncated? (> (length lines) max-lines))
        (append (for/list ([l (in-list visible-lines)])
                  (styled-line (list (styled-segment (format "── [thinking] ~a" l) base-style))))
                (if truncated?
                    (list (styled-line (list (styled-segment (format "... ~a more lines"
                                                                     (- (length lines) max-lines))
                                                             base-style))))
                    '()))]
       ;; Expanded: render full body as normal scrollable transcript content (no modal).
       [(and artifact-id (disclosure-expanded? disclosure artifact-id))
        (for/list ([l (in-list (string-split raw-text "\n"))])
          (styled-line (list (styled-segment (format "── [thinking] ~a" l) base-style))))]
       ;; Collapsed: show a single useful preview line.
       [else
        (define total (length (string-split raw-text "\n")))
        (define preview (make-collapsed-preview raw-text 3 total))
        (list (styled-line (list (styled-segment (format "── [thinking] ~a" preview) base-style))))])]
    [else (list (styled-line (list (styled-segment raw-text '()))))]))

;; Convert a markdown token to a list of styled segments.
(define (md-token->segments tok)
  (define type (md-token-type tok))
  (define content (md-token-content tok))
  (match type
    ['text (list (styled-segment content '()))]
    ['bold (list (styled-segment content '(bold)))]
    ['italic (list (styled-segment content '(italic)))]
    ['code (list (styled-segment content (theme->style 'md-code)))]
    ['header
     (define hstyle (theme->style 'md-heading '(bold)))
     (list (styled-segment (cdr content) hstyle))]
    ['code-block
     (define lang (car content))
     (define code-text (cdr content))
     (define code-lines (string-split code-text "\n"))
     (append (if (and lang (not (string=? lang "")))
                 (list (styled-segment (format "  ~a" lang) '(dim)))
                 '())
             (for/list ([cl (in-list code-lines)])
               (styled-segment (format "  ~a" cl) (theme->style 'md-code '(dim)))))]
    ['link (list (styled-segment (cdr content) (theme->style 'md-link '(underline))))]
    ['hr (list (styled-segment (make-string 40 #\-) '(dim)))]
    ['unordered-list
     (define prefix (format "~a- " (make-string (* (car content) 2) #\space)))
     (cons (styled-segment prefix '()) (apply append (map md-token->segments (cdr content))))]
    ['ordered-list
     (define indent (car content))
     (define num (cadr content))
     (define prefix (format "~a~a. " (make-string (* indent 2) #\space) num))
     (cons (styled-segment prefix '()) (apply append (map md-token->segments (cddr content))))]
    ['blockquote
     (define prefix (format "~a> " (make-string (car content) #\space)))
     (cons (styled-segment prefix '(dim)) (apply append (map md-token->segments (cdr content))))]
    ['newline (list (styled-segment "\n" '()))]
    [_ (list (styled-segment (format "~a" content) '()))]))

;; Backward compat wrapper
(define (md-token->segment tok)
  (define segs (md-token->segments tok))
  (if (= (length segs) 1)
      (car segs)
      (styled-segment (string-join (map styled-segment-text segs) "")
                      (styled-segment-style (car segs)))))

;; Styled line to plain text.
(define (styled-line->text sl)
  (string-join (map styled-segment-text (styled-line-segments sl)) ""))

;; Styled line to ANSI string.
;; Algorithm: emit reset between different-styled segments, final reset only if any styled.
(define (styled-line->ansi sl)
  (define segs (styled-line-segments sl))
  (cond
    [(null? segs) ""]
    [else
     (define-values (rev-parts any-styled? _)
       (for/fold ([acc '()]
                  [saw-styled? #f]
                  [prev-styled? #f])
                 ([seg (in-list segs)])
         (define txt (styled-segment-text seg))
         (define sty (styled-segment-style seg))
         (define styled? (not (null? sty)))
         (define part
           (cond
             [styled?
              (define reset (if prev-styled? "\x1b[0m" ""))
              (string-append reset (styles->sgr sty) txt)]
             [prev-styled? (string-append "\x1b[0m" txt)]
             [else txt]))
         (values (cons part acc) (or saw-styled? styled?) styled?)))
     (string-append (string-join (reverse rev-parts) "") (if any-styled? "\x1b[0m" ""))]))

;; Convert style list to SGR escape sequence.
(define (styles->sgr styles)
  (define codes
    (for/list ([s (in-list styles)])
      (case s
        [(bold) "1"]
        [(dim) "2"]
        [(italic) "3"]
        [(underline) "4"]
        [(inverse) "7"]
        [(black) "30"]
        [(red) "31"]
        [(green) "32"]
        [(yellow) "33"]
        [(blue) "34"]
        [(magenta) "35"]
        [(cyan) "36"]
        [(white) "37"]
        [(bright-black) "90"]
        [(bright-red) "91"]
        [(bright-green) "92"]
        [(bright-yellow) "93"]
        [(bright-blue) "94"]
        [(bright-magenta) "95"]
        [(bright-cyan) "96"]
        [(bright-white) "97"]
        [else #f])))
  (define filtered (filter values codes))
  (if (null? filtered)
      ""
      (format "\x1b[~am" (string-join filtered ";"))))

;; Wrap a styled line to fit within width columns.
(define (wrap-styled-line sl width)
  (define text (styled-line->text sl))
  (define w (string-visible-width text))
  (if (<= w width)
      (list sl)
      ;; Segment-level wrapping: emit accumulated line when next segment would exceed.
      ;; Uses forward-ordered accumulator: all results appended in left-to-right order.
      (let loop ([segs (styled-line-segments sl)]
                 [col 0]
                 [acc-segs '()]
                 [result-lines '()])
        (cond
          [(null? segs)
           ;; Emit final accumulated line (if non-empty)
           (define last-line (styled-line (reverse acc-segs)))
           (define last-text (styled-line->text last-line))
           (if (string=? last-text "")
               result-lines
               (append result-lines (list last-line)))]
          [else
           (define seg (car segs))
           (define raw-text (styled-segment-text seg))
           ;; Strip leading whitespace from segments starting a new line (col=0, no accumulated segments)
           (define seg-text
             (if (and (null? acc-segs) (= col 0))
                 (string-trim raw-text #:left? #t #:right? #f)
                 raw-text))
           (define seg-width (string-visible-width seg-text))
           (define new-col (+ col seg-width))
           (cond
             [(> new-col width)
              (cond
                [(null? acc-segs)
                 ;; Single segment wider than width: split into width-sized sub-lines
                 (define sub-lines (wrap-single-line seg-text width))
                 (define seg-style (styled-segment-style seg))
                 (define sub-styled
                   (for/list ([l (in-list sub-lines)])
                     (styled-line (list (styled-segment l seg-style)))))
                 ;; Emit sub-lines in order, then continue with remaining segments
                 (loop (cdr segs) 0 '() (append result-lines sub-styled))]
                [else
                 ;; Fill the remaining columns with as much of the overflowing
                 ;; segment as fits. Moving the whole segment to the next row
                 ;; underfills lines at markdown style boundaries.
                 (define remaining-width (max 0 (- width col)))
                 (define break-pos
                   (and (> remaining-width 0)
                        (find-boundary-fill-break-pos seg-text remaining-width)))
                 (cond
                   [(and break-pos (> break-pos 0))
                    (define prefix (substring seg-text 0 break-pos))
                    (define suffix (substring seg-text break-pos))
                    (define seg-style (styled-segment-style seg))
                    (define current-line
                      (styled-line (reverse (cons (styled-segment prefix seg-style) acc-segs))))
                    (define remaining-segs
                      (if (string=? suffix "")
                          (cdr segs)
                          (cons (styled-segment suffix seg-style) (cdr segs))))
                    (loop remaining-segs 0 '() (append result-lines (list current-line)))]
                   [else
                    ;; No part of the segment can fit; retry it on a new line.
                    (define current-line (styled-line (reverse acc-segs)))
                    (loop (cons seg (cdr segs)) 0 '() (append result-lines (list current-line)))])])]
             [else
              ;; Strip leading whitespace only on first pass; use original segment for accumulation
              (define clean-seg
                (if (and (null? acc-segs) (= col 0) (not (string=? seg-text raw-text)))
                    (styled-segment seg-text (styled-segment-style seg))
                    seg))
              (loop (cdr segs) new-col (cons clean-seg acc-segs) result-lines)])]))))

;; Format assistant text with markdown rendering.
;; Preserves per-token styles instead of flattening.
;; BUG-0004: 'table tokens are laid out width-aware (table-token->styled-lines);
;; every other token flows through the original flatten+wrap pipeline unchanged,
;; so non-table output is byte-identical to the previous implementation.
(define (md-format-assistant text width)
  (if (or (not text) (string=? (string-trim text) ""))
      (quote ())
      (render-md-tokens (parse-markdown text) width)))

;; Render a token list: table tokens get the width-aware table layout, all
;; other tokens are batched into runs through the pre-existing pipeline.
(define (render-md-tokens tokens width)
  (let loop ([remaining tokens]
             [plain-run (quote ())]
             [out (quote ())])
    (cond
      [(null? remaining) (append out (render-plain-md-run (reverse plain-run) width))]
      [(eq? (md-token-type (car remaining)) 'table)
       (loop (cdr remaining)
             (quote ())
             (append out
                     (render-plain-md-run (reverse plain-run) width)
                     (table-token->styled-lines (car remaining) width)))]
      [else (loop (cdr remaining) (cons (car remaining) plain-run) out)])))

;; Original flatten+wrap pipeline over a run of non-table tokens.
(define (render-plain-md-run tokens width)
  (if (null? tokens)
      (quote ())
      (let* ([all-segments (apply append (map md-token->segments tokens))]
             [line-groups (split-segments-on-newline all-segments)])
        (apply
         append
         (for/list ([group (in-list line-groups)])
           (define non-empty (filter-not (lambda (s) (string=? (styled-segment-text s) "")) group))
           (cond
             [(null? non-empty) (list (styled-line (list (styled-segment "" (quote ())))))]
             [else
              (define line (styled-line non-empty))
              ;; Wrap if line exceeds width
              ;; Skip wrapping for headers (detected by single segment with heading style)
              (define line-text (styled-line->text line))
              (define exceeds? (> (string-visible-width line-text) width))
              (define is-header?
                (and (= (length non-empty) 1)
                     (let ([sty (styled-segment-style (car non-empty))])
                       (and (member 'bold sty) (> (length sty) 1))))) ;; heading has bold + color
              (if (and exceeds? (not is-header?))
                  (wrap-styled-line line width)
                  (list line))]))))))

;; ============================================================
;; GFM table rendering (BUG-0004)
;; ============================================================

;; Shrink column widths (widest first) until the table fits `avail`
;; characters; every column keeps at least 1 character. avail >= ncols
;; is guaranteed by the caller, so the loop always terminates.
(define (clamp-table-widths widths avail)
  (cond
    [(null? widths) widths]
    [(<= (apply + widths) avail) widths]
    [else
     (define i (index-of widths (apply max widths)))
     (define reduced (list-set widths i (max 1 (sub1 (list-ref widths i)))))
     (if (equal? reduced widths)
         widths
         (clamp-table-widths reduced avail))]))

;; Width-aware GFM table -> styled-lines. Column widths come from cell content,
;; clamped so the whole table (cells + two-space gutters) fits `width`;
;; over-wide cells wrap within their column via wrap-styled-line -- the same
;; wrapper used for ordinary markdown text, no duplicated wrap logic.
(define (table-token->styled-lines tok width)
  (define content (md-token-content tok))
  (define header (car content))
  (define alignments (cadr content))
  (define rows (caddr content))
  (define ncols (length header))
  (define gutters (max 0 (* 2 (sub1 ncols))))
  (define avail (max ncols (- width gutters)))
  (define widths (clamp-table-widths (table-column-widths (cons header rows)) avail))
  (define header-style (theme->style 'md-heading '(bold)))
  ;; A cell becomes its own wrapped line list (plain text) within its column.
  (define (cell-text-lines text w)
    (if (<= (string-length text) w)
        (list text)
        (map styled-line->text
             (wrap-styled-line (styled-line (list (styled-segment text (quote ())))) (max 1 w)))))
  ;; Emit one visual line per wrapped row line: padded cells + gutters.
  (define (emit-row cells style)
    (define cell-lines
      (for/list ([c (in-list cells)]
                 [w (in-list widths)])
        (cell-text-lines c w)))
    (define row-height
      (if (null? cell-lines)
          1
          (apply max (map length cell-lines))))
    (for/list ([idx (in-range row-height)])
      (define segs
        (for/fold ([acc (quote ())])
                  ([ls (in-list cell-lines)]
                   [w (in-list widths)]
                   [a (in-list alignments)]
                   [i (in-naturals)])
          (define txt
            (if (< idx (length ls))
                (list-ref ls idx)
                ""))
          (define gutter
            (if (zero? i)
                (quote ())
                (list (styled-segment "  " style))))
          (append acc gutter (list (styled-segment (table-pad-cell txt w a) style)))))
      (define non-empty (filter (lambda (sg) (not (string=? (styled-segment-text sg) ""))) segs))
      (styled-line (if (null? non-empty)
                       (list (styled-segment "" (quote ())))
                       non-empty))))
  (define delim-segs
    (for/fold ([acc (quote ())])
              ([w (in-list widths)]
               [a (in-list alignments)]
               [i (in-naturals)])
      (define gutter
        (if (zero? i)
            (quote ())
            (list (styled-segment "  " '(dim)))))
      (append acc gutter (list (styled-segment (table-delimiter-cell-text w a) '(dim))))))
  (append (emit-row header header-style)
          (list (styled-line delim-segs))
          (append-map (lambda (r) (emit-row r (quote ()))) rows)))

;; Split segments on newline markers.
(define (split-segments-on-newline segs)
  (let loop ([remaining segs]
             [current (quote ())]
             [groups (quote ())])
    (cond
      [(null? remaining)
       (define final (reverse current))
       (reverse (if (null? final)
                    groups
                    (cons final groups)))]
      [else
       (define seg (car remaining))
       (define txt (styled-segment-text seg))
       (cond
         [(string=? txt "\n") (loop (cdr remaining) (quote ()) (cons (reverse current) groups))]
         [(string-contains? txt "\n")
          (define parts (string-split txt "\n"))
          (define style (styled-segment-style seg))
          (define sub-segs
            (apply append
                   (for/list ([p (in-list parts)]
                              [i (in-naturals)])
                     (if (= i (sub1 (length parts)))
                         (list (styled-segment p style))
                         (list (styled-segment p style) (styled-segment "\n" (quote ())))))))
          (loop (append sub-segs (cdr remaining)) current groups)]
         [else (loop (cdr remaining) (cons seg current) groups)])])))

;; Wrap text to max-width columns.
(define (wrap-text text max-width)
  (define lines (string-split text "\n"))
  (apply append (map (lambda (l) (wrap-single-line l max-width)) lines)))

;; Wrap a single line to max-width columns.
(define (wrap-single-line line max-width)
  (if (<= (string-visible-width line) max-width)
      (list line)
      (let loop ([pos 0]
                 [acc '()])
        (cond
          [(>= pos (string-length line)) (reverse acc)]
          [else
           (define break (find-break-pos line pos max-width))
           (define chunk (substring line pos break))
           (if (>= break (string-length line))
               (reverse (cons chunk acc))
               (loop break (cons chunk acc)))]))))

;; Find break position starting from `pos` within `max-width` columns.
;; Checks if adding next char would exceed budget before including it (CJK-safe).
;; Prefers breaking at whitespace when possible (word-breaking).
(define (find-break-pos text start-pos max-width)
  (define len (string-length text))
  (let loop ([i start-pos]
             [col 0]
             [last-space-pos #f]
             [last-space-col #f])
    (cond
      [(>= i len) len]
      [else
       (define c (string-ref text i))
       (define w (char-width c))
       (define next-col (+ col w))
       (cond
         [(> next-col max-width)
          ;; Would exceed: prefer last space break point, else hard break here
          (if last-space-pos
              (add1 last-space-pos) ;; break after the space
              i)]
         [(char-whitespace? c) (loop (add1 i) next-col i col)]
         [else (loop (add1 i) next-col last-space-pos last-space-col)])])))

;; Like find-break-pos for filling the remainder of a partially accumulated
;; row, but do not treat leading whitespace as the preferred break. This keeps
;; short words such as " in" attached when they fit exactly at a segment
;; boundary.
(define (find-boundary-fill-break-pos text max-width)
  (define len (string-length text))
  (define starts-with-space? (and (> len 0) (char-whitespace? (string-ref text 0))))
  (let loop ([i 0]
             [col 0]
             [last-space-pos #f]
             [seen-nonspace? #f])
    (cond
      [(>= i len) len]
      [else
       (define c (string-ref text i))
       (define w (char-width c))
       (define next-col (+ col w))
       (cond
         [(> next-col max-width)
          (cond
            [(and starts-with-space? (char-whitespace? c) seen-nonspace?) i]
            [last-space-pos (add1 last-space-pos)]
            [else #f])]
         [(char-whitespace? c) (loop (add1 i) next-col (and seen-nonspace? i) seen-nonspace?)]
         [else (loop (add1 i) next-col last-space-pos #t)])])))
