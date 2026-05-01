#lang racket/base

;; q/tui/render/message-layout.rkt — message layout algorithms (pure)
;;
;; Entry formatting, markdown rendering, styled-line construction.

(require racket/string
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
        (text   ; string
         style  ; (listof symbol) — 'bold 'italic 'inverse 'underline 'dim
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
     (list (styled-line
            (list (styled-segment "> " '(bold cyan))
                  (styled-segment raw-text '(bold)))))]
    [(assistant)
     (md-format-assistant raw-text width)]
    [(system)
     (list (styled-line
            (list (styled-segment (format "[SYS] ~a" raw-text)
                                  '(bright-black)))))]
    [(tool tool-result)
     (define tool-name (hash-ref (transcript-entry-meta entry) 'tool-name #f))
     (define renderer (and tool-name (lookup-custom-renderer-for-tool tool-name)))
     (define text
       (cond
         [renderer (renderer entry width)]
         [tool-name (format "  [~a] ~a" tool-name raw-text)]
         [else (format "  [tool] ~a" raw-text)]))
     (list (styled-line (list (styled-segment text (theme->style 'tool)))))]
    [(tool-start)
     (define tool-name (hash-ref (transcript-entry-meta entry) 'tool-name "tool"))
     (define sanitized (string-replace raw-text "\n" " "))
     (list (styled-line
            (list (styled-segment (format "[TOOL] ~a: ~a" tool-name sanitized) '(cyan)))))]
    [(tool-end)
     (define tool-name (hash-ref (transcript-entry-meta entry) 'tool-name "tool"))
     (define sanitized (string-replace raw-text "\n" " "))
     (list (styled-line
            (list (styled-segment (format "[OK] ~a: ~a" tool-name sanitized) '(green)))))]
    [(tool-fail)
     (define tool-name (hash-ref (transcript-entry-meta entry) 'tool-name "tool"))
     (define sanitized (string-replace raw-text "\n" " "))
     (list (styled-line
            (list (styled-segment (format "[FAIL] ~a: ~a" tool-name sanitized) '(red)))))]
    [(error)
     (list (styled-line
            (list (styled-segment (format "[ERR] ~a" raw-text)
                                  '(bold red)))))]
    [else
     (list (styled-line
            (list (styled-segment raw-text '()))))]))

;; Convert a markdown token to a styled segment.
(define (md-token->segment tok)
  (define text (md-token-content tok))
  (define type (md-token-type tok))
  (define style
    (case type
      [(heading) (theme->style 'accent '(bold))]
      [(bold) '(bold)]
      [(italic) '(italic)]
      [(code) (theme->style 'code)]
      [(code-block) (theme->style 'code-block '(dim))]
      [(link) (theme->style 'link '(underline))]
      [(list-item) '()]
      [(blockquote) '(dim)]
      [else '()]))
  (styled-segment text style))

;; Styled line to plain text.
(define (styled-line->text sl)
  (string-join (map styled-segment-text (styled-line-segments sl)) ""))

;; Styled line to ANSI string.
(define (styled-line->ansi sl)
  (define segs (styled-line-segments sl))
  (if (null? segs)
      ""
      (string-append
       (string-join (map (lambda (seg)
                           (define txt (styled-segment-text seg))
                           (define sty (styled-segment-style seg))
                           (if (null? sty)
                               txt
                               (string-append (styles->sgr sty) txt "\x1b[0m")))
                         segs)
                    "")
       "\x1b[0m")))

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
      ;; Simple wrapping: break on width boundaries
      (let loop ([segs (styled-line-segments sl)]
                 [col 0]
                 [acc-segs '()]
                 [lines '()])
        (cond
          [(null? segs)
           (define last-line (styled-line (reverse acc-segs)))
           (if (null? lines)
               (list last-line)
               (reverse (cons last-line lines)))]
          [else
           (define seg (car segs))
           (define seg-text (styled-segment-text seg))
           (define seg-width (string-visible-width seg-text))
           (define new-col (+ col seg-width))
           (cond
             [(> new-col width)
              ;; Break needed — for now, just add segment anyway
              (loop (cdr segs) new-col (cons seg acc-segs) lines)]
             [else
              (loop (cdr segs) new-col (cons seg acc-segs) lines)])]))))

;; Format assistant text with markdown rendering.
(define (md-format-assistant text width)
  (define tokens (parse-markdown text))
  (define segments (map md-token->segment tokens))
  (define plain (string-join (map styled-segment-text segments) ""))
  (define wrapped-lines (wrap-text plain width))
  (for/list ([line (in-list wrapped-lines)])
    (styled-line (list (styled-segment line (theme->style 'assistant))))))

;; Wrap text to max-width columns.
(define (wrap-text text max-width)
  (define lines (string-split text "\n"))
  (apply append (map (lambda (l) (wrap-single-line l max-width)) lines)))

;; Wrap a single line to max-width columns.
(define (wrap-single-line line max-width)
  (if (<= (string-visible-width line) max-width)
      (list line)
      (let loop ([pos 0] [acc '()])
        (cond
          [(>= pos (string-length line)) (reverse acc)]
          [else
           (define break (find-break-pos line pos max-width))
           (define chunk (substring line pos break))
           (if (>= break (string-length line))
               (reverse (cons chunk acc))
               (loop break (cons chunk acc)))]))))

;; Find break position starting from `pos` within `max-width` columns.
(define (find-break-pos text start-pos max-width)
  (define len (string-length text))
  (let loop ([i start-pos] [col 0])
    (cond
      [(>= i len) len]
      [(>= col max-width) i]
      [else (loop (add1 i) (+ col (char-width (string-ref text i))))])))
