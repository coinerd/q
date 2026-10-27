#lang racket/base

;; tools/builtins/edit-mismatch.rkt — one readable, bounded edit-mismatch
;; diagnostic shared by the direct edit tool and the sandboxed worker.
;;
;; Before W2 both planes rendered the mismatch as raw `U+…` code points only,
;; which made an exact repair hard: no line/column, no visible whitespace, no
;; readable diff. The contract here is:
;;   - line/column of the first difference plus the (compatibility) offset,
;;   - a bounded, readable diff of the nearest region and the old-text line,
;;   - visible escaping for whitespace/control characters (tabs are `\t`,
;;     controls are `\u{…}`), printable/non-ASCII characters stay literal
;;     with their code point named so an em-dash/hyphen confusion is obvious,
;;   - the same bytes from both execution planes.
;;
;; Pure: no filesystem, no policy, no parameters.

(require racket/list
         racket/string)

(provide format-not-found-diagnostic
         visible-text
         char-description
         find-nearest-match
         first-differing-offset)

;; --------------------------------------------------
;; Escaping
;; --------------------------------------------------

(define (control-char? c)
  (or (< (char->integer c) 32) (= (char->integer c) 127)))

;; One-character visible rendering: printable text stays literal, whitespace
;; and control characters become unambiguous escape spellings.
(define (visible-char c)
  (case c
    [(#\space) " "]
    [(#\tab) "\\t"]
    [(#\newline) "\\n"]
    [(#\return) "\\r"]
    [else
     (if (control-char? c)
         (format "\\u{~X}" (char->integer c))
         (string c))]))

;; Whole-string visible rendering (used for indentation samples and bounded
;; diff lines). Non-printable characters never reach the terminal raw.
(define (visible-text s)
  (apply string-append
         (for/list ([c (in-string s)])
           (visible-char c))))

;; Human-readable description of the differing character, including its code
;; point. A literal character is quoted so spaces/tabs are visible too.
(define (char-description c)
  (define code (char->integer c))
  (cond
    [(char=? c #\space) "' ' (space, U+20)"]
    [(char=? c #\tab) "'\\t' (tab, U+9)"]
    [(char=? c #\newline) "'\\n' (newline, U+A)"]
    [(char=? c #\return) "'\\r' (carriage return, U+D)"]
    [(control-char? c) (format "'\\u{~X}' (control, U+~X)" code code)]
    [else (format "'~a' (U+~X)" (string c) code)]))

;; --------------------------------------------------
;; Nearest-match search (bounded longest-common-substring heuristic)
;; --------------------------------------------------

(define (longest-common-substring-len a b)
  (define la (string-length a))
  (define lb (string-length b))
  (cond
    [(or (zero? la) (zero? lb)) 0]
    [else
     (for/fold ([best 0]) ([i (in-range la)])
       (for/fold ([best best]) ([j (in-range lb)])
         (let loop ([di 0]
                    [dj 0]
                    [len 0])
           (cond
             [(or (>= (+ i di) la) (>= (+ j dj) lb)) (max best len)]
             [(char=? (string-ref a (+ i di)) (string-ref b (+ j dj)))
              (loop (add1 di) (add1 dj) (add1 len))]
             [else (max best len)]))))]))

(define (extract-search-key old-text)
  (define trimmed (string-trim old-text))
  (if (<= (string-length trimmed) 60)
      trimmed
      (substring trimmed 0 60)))

;; Returns (values nearest-line-text nearest-line-number) or (values #f #f).
(define (find-nearest-match content old-text)
  (define lines (string-split content "\n" #:trim? #f))
  (define key (extract-search-key old-text))
  (define key-len (string-length key))
  (if (zero? key-len)
      (values #f #f)
      (for/fold ([best-line #f]
                 [best-num #f]
                 [best-score 0]
                 #:result (if (> best-score (* key-len 0.4))
                              (values best-num best-line)
                              (values #f #f)))
                ([line (in-list lines)]
                 [idx (in-naturals 1)])
        (define trimmed (string-trim line))
        (define lcs (longest-common-substring-len key trimmed))
        (if (> lcs best-score)
            (values line idx lcs)
            (values best-line best-num best-score)))))

;; --------------------------------------------------
;; First difference
;; --------------------------------------------------

;; Returns (values offset content-char old-char). OFFSET is relative to the
;; compared region (the nearest region when one was found), preserving the
;; historical diagnostic contract.
(define (first-differing-offset a b)
  (define len (min (string-length a) (string-length b)))
  (let loop ([i 0])
    (cond
      [(>= i len)
       (if (= (string-length a) (string-length b))
           (values #f #f #f)
           (values i
                   (and (< i (string-length a)) (string-ref a i))
                   (and (< i (string-length b)) (string-ref b i))))]
      [(char=? (string-ref a i) (string-ref b i)) (loop (add1 i))]
      [else (values i (string-ref a i) (string-ref b i))])))

;; Line/column (1-based) for an offset inside TEXT.
(define (line+column text offset)
  (define prefix (substring text 0 (min offset (string-length text))))
  (define newlines (for/sum ([c (in-string prefix)]) (if (char=? c #\newline) 1 0)))
  (define last-newline
    (for/last ([c (in-string prefix)]
               [i (in-naturals)]
               #:when (char=? c #\newline))
      i))
  (values (add1 newlines)
          (if last-newline
              (- (string-length prefix) last-newline)
              (add1 (string-length prefix)))))

;; Bounded readable line around OFFSET: at most MAX-CHARS visible characters,
;; with unambiguous "…" markers when clipped. Control characters are escaped,
;; so the reported width is the visible width, not the raw width.
(define (bounded-readable-line text offset #:max-chars (max-chars 96))
  (define line-start
    (or (for/last ([c (in-string text)]
                   [i (in-naturals 0)]
                   #:when (and (< i (min offset (string-length text))) (char=? c #\newline)))
          i)
        -1))
  (define line-end
    (or (for/first ([c (in-string text)]
                    [i (in-naturals 0)]
                    #:when (and (>= i (min offset (string-length text))) (char=? c #\newline)))
          i)
        (string-length text)))
  (define raw-line (substring text (add1 line-start) line-end))
  (define column-offset (- (min offset (string-length text)) (add1 line-start)))
  (define half (quotient max-chars 2))
  (define start (max 0 (- column-offset half)))
  (define end (min (string-length raw-line) (+ start max-chars)))
  (define slice (substring raw-line start end))
  (string-append (if (zero? start) "" "…")
                 (visible-text slice)
                 (if (>= end (string-length raw-line)) "" "…")))

;; --------------------------------------------------
;; Diagnostic
;; --------------------------------------------------

(define MAX-WHITESPACE-REPORT-LINES 3)

(define (leading-space-count line)
  (for/fold ([count 0])
            ([c (in-string line)]
             #:break (not (char=? c #\space)))
    (add1 count)))

(define (leading-whitespace-prefix line [limit 16])
  (define chars
    (for/list ([c (in-string line)]
               #:break (not (or (char=? c #\space) (char=? c #\tab))))
      c))
  (define kept
    (if (> (length chars) limit)
        (take chars limit)
        chars))
  (string-append (list->string kept) (if (> (length chars) limit) "…" "")))

(define (whitespace-report comparison-lines old-lines line-num)
  (define pairs
    (for/list ([file-line (in-list comparison-lines)]
               [expected-line (in-list old-lines)]
               #:when (and (string? file-line)
                           (string? expected-line)
                           (not (equal? file-line expected-line))))
      (list file-line expected-line)))
  (define shown
    (if (<= (length pairs) MAX-WHITESPACE-REPORT-LINES)
        pairs
        (take pairs MAX-WHITESPACE-REPORT-LINES)))
  (if (null? shown)
      ""
      (string-append
       (format "Whitespace differences near line ~a:\n" line-num)
       (string-join (for/list ([pair (in-list shown)])
                      (define file-line (car pair))
                      (define expected-line (cadr pair))
                      (format "  file line has ~a leading spaces (~a), old-text line has ~a (~a)"
                              (leading-space-count file-line)
                              (visible-text (leading-whitespace-prefix file-line))
                              (leading-space-count expected-line)
                              (visible-text (leading-whitespace-prefix expected-line))))
                    "\n")
       "\n")))

(define (mismatch-hint old-text)
  (cond
    [(regexp-match? #rx"^ +" old-text) "Hint: old-text has leading whitespace — check indentation.\n"]
    [(and (> (string-length old-text) 200) (< (length (string-split old-text "\n")) 2))
     "Hint: old-text is very long and single-line — try a smaller unique snippet.\n"]
    [else ""]))

;; Whole diagnostic for a not-found edit. Both execution planes call this.
(define (format-not-found-diagnostic path-str old-text content)
  (define-values (nearest-num _nearest-line) (find-nearest-match content old-text))
  (define old-lines (string-split old-text "\n" #:trim? #f))
  (define content-lines (string-split content "\n" #:trim? #f))
  ;; Compare old-text against the nearest matching region, not the file start
  ;; (an indented snippet in a Racket file would otherwise "differ" from the
  ;; leading #lang at offset zero).
  (define comparison-lines
    (if nearest-num
        (let* ([region-start (sub1 nearest-num)]
               [region-end (min (length content-lines) (+ region-start (length old-lines)))])
          (for/list ([i (in-range region-start region-end)])
            (list-ref content-lines i)))
        content-lines))
  (define comparison-region (string-join comparison-lines "\n"))
  (define-values (diff-offset content-char old-char)
    (first-differing-offset comparison-region old-text))
  (define-values (diff-line-in-region diff-column)
    (if diff-offset
        (line+column comparison-region diff-offset)
        (values 1 1)))
  (define absolute-diff-line
    (if nearest-num
        (+ (sub1 nearest-num) diff-line-in-region)
        diff-line-in-region))
  (define base-line
    (string-append
     (format "old-text not found in ~a (appears 0 times). Read the file first to get exact text.\n"
             path-str)
     (if nearest-num
         (format "Nearest match at line ~a:\n  ~a | ~a\n"
                 nearest-num
                 nearest-num
                 (bounded-readable-line (list-ref content-lines (sub1 nearest-num)) 0))
         "")))
  (define diff-detail
    (cond
      [(and diff-offset content-char old-char)
       (string-append
        (format "First differing offset: ~a (line ~a, column ~a) — file ~a vs old-text ~a\n"
                diff-offset
                absolute-diff-line
                diff-column
                (char-description content-char)
                (char-description old-char))
        (format "  file:     ~a\n" (bounded-readable-line comparison-region diff-offset))
        (format "  old-text: ~a\n" (bounded-readable-line old-text diff-offset)))]
      [diff-offset
       (format (string-append "First differing offset: ~a (line ~a, column ~a) — nearest region has "
                              "~a chars, old-text has ~a chars\n")
               diff-offset
               absolute-diff-line
               diff-column
               (string-length comparison-region)
               (string-length old-text))]
      [else ""]))
  (string-append base-line
                 (if (string=? diff-detail "") "" diff-detail)
                 (if nearest-num
                     (whitespace-report comparison-lines old-lines nearest-num)
                     "")
                 (mismatch-hint old-text)))
