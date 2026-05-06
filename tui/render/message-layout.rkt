#lang racket/base

;; q/tui/render/message-layout.rkt — message layout algorithms (pure)
;;
;; Entry formatting, markdown rendering, styled-line construction.

(require racket/match
         racket/string
         racket/list
         racket/function
         "../state.rkt"
         "../input.rkt"
         "../char-width.rkt"
         "../theme.rkt"
         "../../util/markdown.rkt"
         (only-in "../../util/markdown.rkt" md-token-content md-token-type)
         "../../extensions/custom-renderer-registry.rkt")

(provide (struct-out styled-line)
         (struct-out styled-segment)
         plain-line
         theme->style
         format-entry
         md-format-assistant
         md-token->segment
         styled-line->text
         styled-line->ansi
         styles->sgr
         wrap-styled-line
         wrap-text
         wrap-single-line
         find-break-pos
         lookup-custom-renderer-for-tool)

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
(define (format-entry entry [width 200])
  (define kind (transcript-entry-kind entry))
  (define raw-text (or (transcript-entry-text entry) ""))

  (case kind
    [(user)
     (list (styled-line (list (styled-segment "> " '(bold cyan)) (styled-segment raw-text '(bold)))))]
    [(assistant) (md-format-assistant raw-text width)]
    [(system)
     (list (styled-line (list (styled-segment (format "[SYS] ~a" raw-text) '(bright-black)))))]
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
     (list (styled-line (list (styled-segment (format "[FAIL] ~a: ~a" tool-name sanitized) '(red)))))]
    [(error) (list (styled-line (list (styled-segment (format "[ERR] ~a" raw-text) '(bold red)))))]
    [(thinking)
     ;; v0.28.21 W1: Distinct thinking rendering with truncation + separator
     (define max-lines 3)
     (define lines (string-split raw-text "\n"))
     (define visible-lines (take lines (min max-lines (length lines))))
     (define truncated? (> (length lines) max-lines))
     (define base-style '(dim italic cyan))
     (append (for/list ([l (in-list visible-lines)])
               (styled-line (list (styled-segment (format "── [thinking] ~a" l) base-style))))
             (if truncated?
                 (list (styled-line (list (styled-segment (format "... ~a more lines"
                                                                  (- (length lines) max-lines))
                                                          base-style))))
                 '()))]
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
      ;; Segment-level wrapping: emit accumulated line when next segment would exceed
      (let loop ([segs (styled-line-segments sl)]
                 [col 0]
                 [acc-segs '()]
                 [lines '()])
        (cond
          [(null? segs)
           (define last-line (styled-line (reverse acc-segs)))
           (define last-text (styled-line->text last-line))
           (cond
             [(string=? last-text "")
              (if (null? lines)
                  (quote ())
                  (reverse lines))]
             [(null? lines) (list last-line)]
             [else (reverse (cons last-line lines))])]
          [else
           (define seg (car segs))
           (define seg-text (styled-segment-text seg))
           (define seg-width (string-visible-width seg-text))
           (define new-col (+ col seg-width))
           (cond
             [(> new-col width)
              (cond
                [(null? acc-segs)
                 ;; Single segment wider than width: split into width-sized sub-segments
                 (define sub-lines (wrap-single-line seg-text width))
                 (define seg-style (styled-segment-style seg))
                 (define sub-segs
                   (for/list ([l (in-list sub-lines)])
                     (styled-line (list (styled-segment l seg-style)))))
                 (define remaining-lines (loop (cdr segs) 0 '() lines))
                 (append sub-segs remaining-lines)]
                [else
                 ;; Emit current line, put overflowing segment on new line
                 (define current-line (styled-line (reverse acc-segs)))
                 (loop (cons seg (cdr segs)) 0 '() (cons current-line lines))])]
             [else (loop (cdr segs) new-col (cons seg acc-segs) lines)])]))))

;; Format assistant text with markdown rendering.
;; Preserves per-token styles instead of flattening.
(define (md-format-assistant text width)
  (if (or (not text) (string=? (string-trim text) ""))
      (quote ())
      (let* ([tokens (parse-markdown text)]
             [all-segments (apply append (map md-token->segments tokens))]
             [line-groups (split-segments-on-newline all-segments)])
        (apply
         append
         (for/list ([group (in-list line-groups)])
           (define non-empty (filter (lambda (s) (not (string=? (styled-segment-text s) ""))) group))
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
