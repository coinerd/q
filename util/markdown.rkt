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

(require racket/string
         racket/match
         racket/list)

(provide md-token
         md-token?
         md-token-type
         md-token-content
         parse-markdown
         parse-inline-markdown)

;; ============================================================
;; Core struct — flat token model (no nesting in first version)
;; ============================================================

(struct md-token (type content) #:transparent)
;; type : 'text | 'bold | 'italic | 'code | 'code-block | 'header | 'link | 'newline
;; content : any
;;   'text       → string (literal text)
;;   'bold       → string (bold text content)
;;   'italic     → string (italic text content)
;;   'code       → string (inline code text)
;;   'code-block → (cons language-string code-string)
;;   'header     → (cons level-number header-text-string)
;;   'link       → (cons url-string link-text-string)
;;   'newline    → string "\n" (line separator)

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
(define code-block-rx
  (regexp "```([a-zA-Z0-9_-]*)[\n]"))

(define (parse-code-blocks text)
  ;; Manual approach: split on triple-backtick boundaries.
  ;; Find opening ``` and closing ``` pairs.
  (define len (string-length text))
  (define result '())
  (define pos 0)
  (let loop ()
    (define open-pos (find-triple-backtick text pos))
    (cond
      [(not open-pos)
       ;; No more code blocks — emit remaining text
       (when (< pos len)
         (set! result (append result (parse-regular-text (substring text pos len)))))]
      [else
       ;; Emit text before the code block
       (when (> open-pos pos)
         (set! result (append result (parse-regular-text (substring text pos open-pos)))))
       ;; Find the language tag (rest of the opening line)
       (define after-open (+ open-pos 3)) ; skip ```
       (define newline-pos (find-char text after-open #\newline))
       (define lang (if (and newline-pos (> newline-pos after-open))
                        (string-trim (substring text after-open newline-pos))
                        ""))
       ;; Find closing ```
       (define code-start (if newline-pos (add1 newline-pos) after-open))
       (define close-pos (find-triple-backtick text code-start))
       (cond
         [(not close-pos)
          ;; Unclosed code block — emit opening fence as text
          (set! result (append result (list (md-token 'text (substring text open-pos len)))))
          (set! pos len)]
         [else
          (define code (substring text code-start close-pos))
          ;; Consume trailing newline after ``` if present
          (define after-close (+ close-pos 3)) ; skip closing ```
          (define trailing-nl? (and (< after-close len)
                                    (char=? (string-ref text after-close) #\newline)))
          (set! result
                (append result
                        (list (md-token 'code-block
                                        (cons (if (string=? lang "") #f lang) code))
                              ;; Newline separator after code block
                              (md-token 'newline "\n"))))
          (set! pos (if trailing-nl? (add1 after-close) after-close))
          (loop)])]))
  ;; Filter out empty text tokens
  (filter (lambda (t)
            (not (and (eq? (md-token-type t) 'text)
                      (string=? (md-token-content t) ""))))
          result))

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

;; Parse regular (non-code-block) text: split into lines, detect headers,
;; then inline-parse each line. Emit 'newline tokens between lines.
(define (parse-regular-text text)
  (if (string=? text "")
      '()
      (let ([lines (string-split text "\n" #:trim? #f)])
        ;; Interleave 'newline tokens between lines (not after the last)
        (define parsed-lines (map parse-line lines))
        (if (= (length parsed-lines) 1)
            (car parsed-lines)
            (append*
             (for/list ([line-tokens (in-list parsed-lines)]
                        [i (in-naturals)])
               (if (< i (sub1 (length parsed-lines)))
                   (append line-tokens (list (md-token 'newline "\n")))
                   line-tokens)))))))

;; Parse a single line: check for header, then inline parse.
(define (parse-line line)
  (define header-match (regexp-match-positions #px"^(#{1,6})[ \t]+(.+)$" line))
  (cond
    [header-match
     (define hashes (substring line (car (cadr header-match))
                               (cdr (cadr header-match))))
     (define header-text (substring line (car (caddr header-match))
                                     (cdr (caddr header-match))))
     (list (md-token 'header (cons (string-length hashes) header-text)))]
    [else
     (parse-inline-markdown line)]))

;; ============================================================
;; Inline parsing
;; ============================================================

;; Parse inline markdown constructs within a single line.
;; Returns a flat list of md-token values.
;; Processing order: inline code → bold → italic → links.
;; Uses position-based scanning to handle mixed content correctly.

(define (parse-inline-markdown text)
  (define len (string-length text))
  (cond
    [(= len 0) '()]
    [else
     ;; Collect all matches with their positions, then process left-to-right
     ;; by repeatedly scanning from the current position.
     (let loop ([pos 0] [acc '()])
       (cond
         [(>= pos len) (reverse acc)]
         [else
          ;; Find the first inline construct starting at pos
          (define result (find-first-inline text pos))
          (cond
            [(not result)
             ;; No more constructs — emit remaining text
             (if (= pos len)
                 (reverse acc)
                 (reverse (cons (md-token 'text (substring text pos len)) acc)))]
            [else
             (match-define (list type start end content) result)
             ;; Emit text before the match (if any)
             (define new-acc
               (if (> start pos)
                   (cons (md-token 'text (substring text pos start)) acc)
                   acc))
             ;; Emit the matched token
             (define new-acc2
               (cons (md-token type content) new-acc))
             (loop end new-acc2)])]))]))

;; Find the first inline markdown construct starting at or after `pos`.
;; Returns (list type start end content) or #f.
;; Checks: inline code, bold, italic, links — returns the earliest match.
(define (find-first-inline text pos)
  (define len (string-length text))
  (define candidates
    (filter
     values
     (list
      ;; Inline code: `code`
      (find-inline-code text pos len)
      ;; Bold: **text**
      (find-bold text pos len)
      ;; Italic: *text*
      (find-italic text pos len)
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
              (list 'italic i (add1 close-pos)
                    (substring text (add1 i) close-pos))
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
         [(and (< (add1 close-bracket) len)
               (char=? (string-ref text (add1 close-bracket)) #\())
          ;; Found ](, look for closing )
          (define close-paren (find-char text (+ close-bracket 2) #\)))
          (cond
            [(not close-paren) (loop (add1 i))]
            [else
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
