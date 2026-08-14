#lang racket/base

;; q/util/markdown.rkt — Token-based markdown parser
;;
;; Shared parser that produces structured md-token values from markdown text.
;; Both CLI (→ ANSI strings) and TUI (→ styled-segments) can consume these tokens.
;;
;; Parsing order (matches CLI):
;;   1. Fenced code blocks (```...```)
;;   2. Headers (### ...)
;;   3. Inline: code (`...`), bold (**...**), italic (*...*), links ([text](url))

(require racket/contract
         racket/string
         racket/match
         racket/list)

(provide md-token
         md-token?
         md-token-type
         md-token-content
         (contract-out [parse-markdown (-> string? (listof md-token?))]
                       [parse-line (-> string? (listof md-token?))]
                       [parse-inline-markdown (-> string? (listof md-token?))]
                       [markdown-table->plain-lines
                        (-> list? (listof string?))])
         ;; GFM table helpers (BUG-0004) — used by renderers and tests.
         table-delimiter-row?
         pipe-row?
         split-table-row
         table-alignment-from-cell
         normalize-table-row
         table-column-widths
         table-pad-cell
         table-delimiter-cell-text
         try-parse-table)

;; ============================================================
;; Core struct — token model with selective nesting
;; ============================================================
;;
;; Block-level tokens (blockquote, unordered-list, ordered-list) contain
;; nested token lists from inline parsing. Their content field is:
;;   blockquote     → (cons depth (listof md-token))
;;   unordered-list → (cons indent (listof md-token))
;;   ordered-list   → (cons indent (cons number (listof md-token)))
;;
;; All other tokens carry flat content (strings, pairs, #t).
;; Consumers should handle nested token lists for these three types.

(struct md-token (type content) #:transparent)
;; type : 'text | 'bold | 'italic | 'code | 'code-block | 'header | 'link | 'newline
;;        | 'unordered-list | 'ordered-list | 'blockquote | 'hr | 'strikethrough
;;        | 'table
;; content : any
;;   'text       → string (literal text)
;;   'bold       → string (bold text content)
;;   'italic     → string (italic text content)
;;   'code       → string (inline code text)
;;   'code-block → (cons language-string code-string)
;;   'header     → (cons level-number header-text-string)
;;   'link       → (cons url-string link-text-string)
;;   'newline    → string "\n" (line separator)
;;   'unordered-list → (cons indent-level (listof md-token))
;;   'ordered-list   → (cons indent-level (cons number (listof md-token)))
;;   'blockquote    → (cons depth (listof md-token))
;;   'hr            → #t
;;   'strikethrough → string (struck-through text)
;;   'table         → (list header-cells alignments rows)
;;                    header-cells : (listof string)
;;                    alignments   : (listof (or/c 'left 'center 'right))
;;                    rows         : (listof (listof string)) — each row padded
;;                                   /truncated to (length header-cells)

;; ============================================================
;; Block-level parsing
;; ============================================================

;; Parse a complete markdown string into a flat list of md-token values.
;; Processes fenced code blocks first (they span lines and must not be
;; inline-parsed), then splits the remaining text into lines for header
;; and inline parsing.

(define (parse-markdown text)
  (if (string=? text "")
      '()
      (parse-code-blocks text)))

;; For code blocks we cannot use (?s) flag (not supported in Racket #px).
;; Instead we use a manual approach: scan for triple-backtick boundaries.
(define code-block-rx (regexp "```([a-zA-Z0-9_-]*)[\n]"))

(define (parse-code-blocks text)
  ;; Manual approach: split on triple-backtick boundaries.
  ;; Uses cons + reverse for O(n) accumulation.
  (define len (string-length text))
  (define result-acc '())
  (define pos 0)
  (let loop ()
    (define open-pos (find-triple-backtick text pos))
    (match open-pos
      [#f
       ;; No more code blocks — emit remaining text
       (when (< pos len)
         (set! result-acc (foldl cons result-acc (parse-regular-text (substring text pos len)))))]
      [_
       ;; Emit text before the code block
       (when (> open-pos pos)
         (set! result-acc (foldl cons result-acc (parse-regular-text (substring text pos open-pos)))))
       ;; Find the language tag (rest of the opening line)
       (define after-open (+ open-pos 3)) ; skip ```
       (define newline-pos (find-char text after-open #\newline))
       (define lang
         (if (and newline-pos (> newline-pos after-open))
             (string-trim (substring text after-open newline-pos))
             ""))
       ;; Find closing ```
       (define code-start
         (if newline-pos
             (add1 newline-pos)
             after-open))
       (define close-pos (find-triple-backtick text code-start))
       (cond
         [(not close-pos)
          ;; Unclosed code block — emit opening fence as text
          (set! result-acc (cons (md-token 'text (substring text open-pos len)) result-acc))
          (set! pos len)]
         [else
          (define code (substring text code-start close-pos))
          ;; Consume trailing newline after ``` if present
          (define after-close (+ close-pos 3)) ; skip closing ```
          (define trailing-nl?
            (and (< after-close len) (char=? (string-ref text after-close) #\newline)))
          (set! result-acc
                (cons (md-token 'newline "\n")
                      (cons (md-token 'code-block (cons (if (string=? lang "") #f lang) code))
                            result-acc)))
          (set! pos
                (if trailing-nl?
                    (add1 after-close)
                    after-close))
          (loop)])]))
  ;; Filter out empty text tokens
  (filter (lambda (t) (not (and (eq? (md-token-type t) 'text) (string=? (md-token-content t) ""))))
          (reverse result-acc)))

;; Find the start of a triple-backtick sequence (```) at or after pos
(define (find-triple-backtick text pos)
  (define len (string-length text))
  (let loop ([i pos])
    (cond
      [(> (+ i 3) len) #f]
      [(and (char=? (string-ref text i) #\`)
            (char=? (string-ref text (add1 i)) #\`)
            (char=? (string-ref text (+ i 2)) #\`))
       i]
      [else (loop (add1 i))])))

;; Parse regular (non-code-block) text: split into lines, detect GFM tables
;; (BUG-0004), then parse each remaining line (headers, lists, inline).
;; Emit 'newline tokens between logical units (lines / whole tables).
(define (parse-regular-text text)
  (if (string=? text "")
      '()
      (parse-line-units (string-split text "\n" #:trim? #f))))

;; Parse lines into token "units" — one unit per ordinary line, one unit per
;; GFM table — joined by single 'newline tokens (not after the last unit).
;; Byte-compatible with the previous per-line map for all non-table input.
(define (parse-line-units lines)
  (define units (scan-lines-for-tables lines))
  (cond
    [(null? units) '()]
    [(null? (cdr units)) (car units)]
    [else
     (append* (for/list ([unit (in-list units)]
                         [i (in-naturals)])
                (if (< i (sub1 (length units)))
                    (append unit (list (md-token 'newline "\n")))
                    unit)))]))

;; Walk lines left-to-right; where a GFM table starts (header row followed by
;; a delimiter row with matching cell count) consume header+delimiter+body
;; rows into a single 'table token. All other lines parse exactly as before.
(define (scan-lines-for-tables lines)
  (define n (length lines))
  (let loop ([i 0]
             [acc '()])
    (cond
      [(>= i n) (reverse acc)]
      [(try-parse-table lines i)
       => (lambda (t)
            (apply (lambda (header alignments rows next-i)
                     (loop next-i
                           (cons (list (md-token 'table (list header alignments rows)))
                                 acc)))
                   t))]
      [else (loop (add1 i)
                  (cons (parse-line (list-ref lines i)) acc))])))

;; ============================================================
;; GFM tables (BUG-0004)
;; ============================================================

;; Private-use sentinel guarding escaped pipes (\|) during cell splitting.
(define table-escape-sentinel "\uE000")

(define table-delimiter-cell-rx #px"^:?-+:?$")

;; Split a table row into trimmed cell strings. Leading/trailing pipes are
;; consumed; interior empty cells are preserved; escaped pipes (\|) stay
;; inside their cell instead of splitting it.
(define (split-table-row line)
  (define cleaned (string-replace line "\\|" table-escape-sentinel))
  (define parts (string-split cleaned "|"))
  (define without-leading
    (if (and (>= (length parts) 2)
             (string=? (string-trim (car parts)) ""))
        (cdr parts)
        parts))
  (define without-trailing
    (if (and (>= (length without-leading) 2)
             (string=? (string-trim (last without-leading)) ""))
        (reverse (cdr (reverse without-leading)))
        without-leading))
  (map (lambda (c) (string-trim (string-replace c table-escape-sentinel "|")))
       without-trailing))

;; A delimiter row: contains a pipe and every cell is :?-+:? (e.g. |---|:--:|---:|)
(define (table-delimiter-row? line)
  (and (string-contains? line "|")
       (let ([cells (split-table-row line)])
         (and (pair? cells)
              (andmap (lambda (c) (regexp-match? table-delimiter-cell-rx c))
                      cells)))))

;; Any table row candidate: contains a pipe but is not itself a delimiter row.
(define (pipe-row? line)
  (and (string-contains? line "|")
       (not (table-delimiter-row? line))))

;; Alignment spec from a delimiter cell: ":--"→left, "--:"→right, ":-:"→center.
(define (table-alignment-from-cell cell)
  (define len (string-length cell))
  (define left? (and (> len 0) (char=? (string-ref cell 0) #\:)))
  (define right? (and (> len 0) (char=? (string-ref cell (sub1 len)) #\:)))
  (cond [(and left? right?) 'center]
        [right? 'right]
        [else 'left]))

;; Pad a row with "" (or truncate it) to exactly ncols cells.
(define (normalize-table-row cells ncols)
  (cond [(= (length cells) ncols) cells]
        [(> (length cells) ncols) (take cells ncols)]
        [else (append cells (make-list (- ncols (length cells)) ""))]))

;; Try to parse a GFM table starting at lines[i]. Requires lines[i] to be a
;; header row and lines[i+1] a delimiter row with matching cell count (GFM).
;; Body rows: subsequent pipe rows. Returns
;;   (list header-cells alignments rows next-unconsumed-index)
;; or #f when no table starts at i.
(define (try-parse-table lines i)
  (cond
    [(or (>= (add1 i) (length lines))
         (not (table-delimiter-row? (list-ref lines (add1 i))))
         (not (pipe-row? (list-ref lines i))))
     #f]
    [else
     (define header (split-table-row (list-ref lines i)))
     (define delim (split-table-row (list-ref lines (add1 i))))
     (and (= (length header) (length delim))
          (let* ([alignments (map table-alignment-from-cell delim)]
                 [ncols (length header)])
            (let grab ([j (+ i 2)]
                       [acc '()])
              (if (and (< j (length lines)) (pipe-row? (list-ref lines j)))
                  (grab (add1 j)
                        (cons (normalize-table-row (split-table-row (list-ref lines j))
                                                   ncols)
                              acc))
                  (list header alignments (reverse acc) j)))))]))

;; ============================================================
;; Plain-text table layout (CLI / GUI fallback)
;; ============================================================

;; Natural column width = longest cell (header or body) per column.
(define (table-column-widths rows)
  (define ncols (if (null? rows) 0 (length (car rows))))
  (for/list ([i (in-range ncols)])
    (for/fold ([w 0])
              ([row (in-list rows)])
      (max w (if (< i (length row)) (string-length (list-ref row i)) 0)))))

;; Pad cell text to width w honoring alignment ('left default).
(define (table-pad-cell text w align)
  (define len (string-length text))
  (cond
    [(>= len w) text]
    [(eq? align 'right) (string-append (make-string (- w len) #\space) text)]
    [(eq? align 'center)
     (define pad (- w len))
     (define l (quotient pad 2))
     (string-append (make-string l #\space)
                    text
                    (make-string (- pad l) #\space))]
    [else (string-append text (make-string (- w len) #\space))]))

;; Alignment-aware delimiter cell text: ":--" / "--:" / ":-:" / "---".
(define (table-delimiter-cell-text w align)
  (define n (max 1 w))
  (define dashes (make-string n #\-))
  (case align
    [(center) (if (= n 1) ":" (string-append ":" (substring dashes 1 (sub1 n)) ":"))]
    [(right) (string-append (substring dashes 1 n) ":")]
    [(left) (string-append ":" (substring dashes 1 n))]
    [else dashes]))

;; Render a 'table token's content (list header alignments rows) as aligned
;; plain-text lines: padded cells, two-space gutters, alignment-aware
;; delimiter row under the header. For renderers without styled lines.
(define (markdown-table->plain-lines content)
  (define header (car content))
  (define alignments (cadr content))
  (define rows (caddr content))
  (define widths (table-column-widths (cons header rows)))
  (define (row->line cells)
    (string-join (for/list ([c (in-list cells)]
                            [w (in-list widths)]
                            [a (in-list alignments)])
                   (table-pad-cell c w a))
                 "  "))
  (cons (row->line header)
        (cons (string-join (for/list ([w (in-list widths)]
                                      [a (in-list alignments)])
                             (table-delimiter-cell-text w a))
                           "  ")
              (map row->line rows))))

;; Parse a single line: check for header, blockquote, list, hr, then inline parse.
(define (parse-line line)
  (cond
    ;; Horizontal rule: 3+ hyphens/asterisks/underscores with optional spaces
    [(or (regexp-match? #px"^[ \t]*-[- \t]*-[- \t]*-[ \t]*$" line)
         (regexp-match? #px"^[ \t]*[*][*][*][* \t]*$" line)
         (regexp-match? #px"^[ \t]*___+[_ \t]*$" line))
     (list (md-token 'hr #t))]
    ;; Header: # heading
    [(regexp-match-positions #px"^(#{1,6})[ \t]+(.+)$" line)
     =>
     (lambda (m)
       (define hashes (substring line (car (cadr m)) (cdr (cadr m))))
       (define header-text (substring line (car (caddr m)) (cdr (caddr m))))
       (list (md-token 'header (cons (string-length hashes) header-text))))]
    ;; Blockquote: > text
    [(regexp-match-positions #px"^[ \t]*(>+)[ \t]?(.*)$" line)
     =>
     (lambda (m)
       (define depth (string-length (substring line (car (cadr m)) (cdr (cadr m)))))
       (define quoted-text (substring line (car (caddr m)) (cdr (caddr m))))
       ;; Parse inline within the quoted text
       (define inner (parse-inline-markdown quoted-text))
       (list (md-token 'blockquote (cons depth inner))))]
    ;; Unordered list: [-*+] text
    [(regexp-match-positions #px"^([ \t]*)([-*+])[ \t]+(.+)$" line)
     =>
     (lambda (m)
       (define indent (quotient (string-length (substring line (car (cadr m)) (cdr (cadr m)))) 2))
       (define list-text (substring line (car (cadddr m)) (cdr (cadddr m))))
       (define inner (parse-inline-markdown list-text))
       (list (md-token 'unordered-list (cons indent inner))))]
    ;; Ordered list: N. text
    [(regexp-match-positions #px"^([ \t]*)([0-9]+)[.][ \t]+(.+)$" line)
     =>
     (lambda (m)
       (define indent (quotient (string-length (substring line (car (cadr m)) (cdr (cadr m)))) 2))
       (define num (string->number (substring line (car (caddr m)) (cdr (caddr m)))))
       (define list-text (substring line (car (cadddr m)) (cdr (cadddr m))))
       (define inner (parse-inline-markdown list-text))
       (list (md-token 'ordered-list (cons indent (cons num inner)))))]
    [else (parse-inline-markdown line)]))

;; ============================================================
;; Inline parsing
;; ============================================================

;; Parse inline markdown constructs within a single line.
;; Returns a flat list of md-token values.
;; Processing order: inline code → bold → italic → links.
;; Uses position-based scanning to handle mixed content correctly.

(define (parse-inline-markdown text)
  (define len (string-length text))
  (match len
    [0 '()]
    [_
     ;; Collect all matches with their positions, then process left-to-right
     ;; by repeatedly scanning from the current position.
     (let loop ([pos 0]
                [acc '()])
       (cond
         [(>= pos len) (reverse acc)]
         [else
          ;; Find the first inline construct starting at pos
          (define result (find-first-inline text pos))
          (match result
            [#f
             ;; No more constructs — emit remaining text
             (if (= pos len)
                 (reverse acc)
                 (reverse (cons (md-token 'text (substring text pos len)) acc)))]
            [_
             (match-define (list type start end content) result)
             ;; Emit text before the match (if any)
             (define new-acc
               (if (> start pos)
                   (cons (md-token 'text (substring text pos start)) acc)
                   acc))
             ;; Emit the matched token
             (define new-acc2 (cons (md-token type content) new-acc))
             (loop end new-acc2)])]))]))

;; Find the first inline markdown construct starting at or after `pos`.
;; Returns (list type start end content) or #f.
;; Checks: inline code, bold, italic, links — returns the earliest match.
(define (find-first-inline text pos)
  (define len (string-length text))
  (define candidates
    (filter values
            ;; Inline code: `code`
            (list (find-inline-code text pos len)
                  ;; Bold: **text**
                  (find-bold text pos len)
                  ;; Italic: *text*
                  (find-italic text pos len)
                  ;; Strikethrough: ~~text~~
                  (find-strikethrough text pos len)
                  ;; Links: [text](url)
                  (find-link text pos len))))
  (if (null? candidates)
      #f
      ;; Return the one with the smallest start position
      ;; (and longest match at same position to prefer bold over italic)
      (argmin (lambda (c) (list-ref c 1)) candidates)))

;; Find inline code: `code` starting at or after pos
(define (find-inline-code text pos len)
  (define rx #px"`([^`]+)`")
  (define m (regexp-match-positions rx text pos))
  (and m
       (let ([start (caar m)]
             [end (cdar m)]
             [code-start (car (cadr m))]
             [code-end (cdr (cadr m))])
         (list 'code start end (substring text code-start code-end)))))

;; Find bold: **text** starting at or after pos
(define (find-bold text pos len)
  (define rx (regexp "[*][*](.+?)[*][*]"))
  (define m (regexp-match-positions rx text pos))
  (and m
       (let* ([start (caar m)]
              [end (cdar m)]
              [content-start (car (cadr m))]
              [content-end (cdr (cadr m))])
         ;; Make sure this is really bold (**, not a single *)
         (list 'bold start end (substring text content-start content-end)))))

;; Find italic: *text* starting at or after pos
;; Must not match ** (bold takes priority). We use a simple approach:
;; match * not preceded or followed by * (using char check, not lookbehind)
(define (find-italic text pos len)
  (let loop ([i pos])
    (cond
      [(>= i (- len 2)) #f]
      [(char=? (string-ref text i) #\*)
       ;; Check not preceded by * and not followed by *
       (define prev-ok? (or (= i 0) (not (char=? (string-ref text (sub1 i)) #\*))))
       (define next-ok? (not (char=? (string-ref text (add1 i)) #\*)))
       (cond
         [(and prev-ok? next-ok?)
          ;; Opening * found at i. Find closing * after i+1
          (define close-pos (find-closing-asterisk text (+ i 2) len))
          (if close-pos
              (list 'italic i (add1 close-pos) (substring text (add1 i) close-pos))
              (loop (add1 i)))]
         [else (loop (add1 i))])]
      [else (loop (add1 i))])))

;; Find a closing * that is not part of **
(define (find-closing-asterisk text pos len)
  (let loop ([i pos])
    (cond
      [(>= i len) #f]
      [(char=? (string-ref text i) #\*)
       ;; Check not followed by * (that would be bold closing)
       (define next-ok? (or (= i (sub1 len)) (not (char=? (string-ref text (add1 i)) #\*))))
       (if next-ok?
           i
           (loop (+ i 2)))] ; skip ** and keep looking
      [else (loop (add1 i))])))

;; Find strikethrough: ~~text~~ starting at or after pos
(define (find-strikethrough text pos len)
  (let loop ([i pos])
    (cond
      [(> (+ i 3) len) #f]
      [(and (char=? (string-ref text i) #\~) (char=? (string-ref text (+ i 1)) #\~))
       ;; Found ~~, look for closing ~~
       (define close-pos
         (let search ([j (+ i 2)])
           (cond
             [(>= (+ j 1) len) #f]
             [(and (char=? (string-ref text j) #\~) (char=? (string-ref text (+ j 1)) #\~)) j]
             [else (search (+ j 1))])))
       (if (and close-pos (> close-pos (+ i 2)))
           (list 'strikethrough i (+ close-pos 2) (substring text (+ i 2) close-pos))
           (loop (+ i 2)))]
      [else (loop (add1 i))])))

;; Find link: [text](url) starting at or after pos, char-by-char
(define (find-link text pos len)
  (let loop ([i pos])
    (cond
      [(>= i len) #f]
      [(char=? (string-ref text i) #\[)
       ;; Found [, look for ](
       (define close-bracket (find-char text (add1 i) #\]))
       (cond
         [(not close-bracket) (loop (add1 i))]
         [(and (< (add1 close-bracket) len) (char=? (string-ref text (add1 close-bracket)) #\())
          ;; Found ](, look for closing )
          (define close-paren (find-char text (+ close-bracket 2) #\)))
          (match close-paren
            [#f (loop (add1 i))]
            [_
             (define link-text (substring text (add1 i) close-bracket))
             (define url (substring text (+ close-bracket 2) close-paren))
             (list 'link i (add1 close-paren) (cons url link-text))])]
         [else (loop (add1 i))])]
      [else (loop (add1 i))])))

;; Find character c in text starting at pos, return position or #f
(define (find-char text pos c)
  (define len (string-length text))
  (let loop ([i pos])
    (cond
      [(>= i len) #f]
      [(char=? (string-ref text i) c) i]
      [else (loop (add1 i))])))
